// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use fnv::{FnvHashMap, FnvHashSet};
use std::collections::hash_map::Entry;

use super::analyze::*;
use super::design_unit::AnalyzeDesignUnit;
use super::lock::AnalysisLock;
use super::lock::{AnalysisEntry, ReadGuard};
use super::region::{AnyDeclaration, Region, VisibleDeclaration};
use crate::ast::search::*;
use crate::ast::*;
use crate::data::*;
use crate::syntax::Symbols;
use parking_lot::RwLock;
use std::cell::RefCell;
use std::sync::Arc;

/// A design unit with design unit data
pub struct AnalysisData {
    pub diagnostics: Vec<Diagnostic>,
    pub root_region: Arc<Region<'static>>,
    pub region: Arc<Region<'static>>,
    pub has_circular_dependency: bool,
}

pub type UnitReadGuard<'a> = ReadGuard<'a, AnyDesignUnit, AnalysisData>;
struct LockedUnit {
    ident: Ident,
    unit_id: UnitId,
    pub unit: AnalysisLock<AnyDesignUnit, AnalysisData>,
}

impl HasUnitId for LockedUnit {
    fn unit_id(&self) -> &UnitId {
        &self.unit_id
    }
}

impl LockedUnit {
    fn new(library_name: &Symbol, unit: AnyDesignUnit) -> LockedUnit {
        let unit_id = match unit {
            AnyDesignUnit::Primary(ref unit) => {
                UnitId::primary(library_name, PrimaryKind::kind_of(unit), unit.name())
            }
            AnyDesignUnit::Secondary(ref unit) => UnitId::secondary(
                library_name,
                SecondaryKind::kind_of(unit),
                unit.primary_name(),
                unit.name(),
            ),
        };

        LockedUnit {
            ident: unit.ident().clone(),
            unit_id,
            unit: AnalysisLock::new(unit),
        }
    }
}

impl HasIdent for LockedUnit {
    fn ident(&self) -> &Ident {
        &self.ident
    }
}

struct Library {
    name: Symbol,
    units: FnvHashMap<UnitKey, LockedUnit>,
    units_by_source: FnvHashMap<Source, FnvHashSet<UnitId>>,

    /// Units removed since last analysis
    removed: FnvHashSet<UnitId>,
    /// Units added since last analysis
    added: FnvHashSet<UnitId>,

    /// Design units which were not added since they were duplicates
    /// They need to be kept for later refresh which might make them not duplicates
    duplicates: Vec<(SrcPos, LockedUnit)>,
}

impl<'a> Library {
    fn new(name: Symbol) -> Library {
        Library {
            name,
            units: FnvHashMap::default(),
            units_by_source: FnvHashMap::default(),
            added: FnvHashSet::default(),
            removed: FnvHashSet::default(),
            duplicates: Vec::new(),
        }
    }

    pub fn name(&self) -> &Symbol {
        &self.name
    }

    fn add_design_unit(&mut self, unit: LockedUnit) {
        let unit_id = unit.unit_id().clone();
        match self.units.entry(unit.key().clone()) {
            Entry::Occupied(entry) => {
                self.duplicates
                    .push((entry.get().ident().pos.clone(), unit));
            }
            Entry::Vacant(entry) => {
                self.added.insert(unit_id);
                match self.units_by_source.entry(unit.source().clone()) {
                    Entry::Occupied(mut entry) => {
                        entry.get_mut().insert(unit.unit_id().clone());
                    }
                    Entry::Vacant(entry) => {
                        let mut set = FnvHashSet::default();
                        set.insert(unit.unit_id().clone());
                        entry.insert(set);
                    }
                }
                entry.insert(unit);
            }
        }
    }

    fn add_design_file(&mut self, design_file: DesignFile) {
        for design_unit in design_file.design_units {
            self.add_design_unit(LockedUnit::new(self.name(), design_unit));
        }
    }

    /// Refresh library after removing or adding new design units
    fn refresh(&mut self, diagnostics: &mut dyn DiagnosticHandler) {
        self.append_duplicate_diagnostics(diagnostics);
    }

    fn append_duplicate_diagnostics(&self, diagnostics: &mut dyn DiagnosticHandler) {
        for (prev_pos, unit) in self.duplicates.iter() {
            let diagnostic = match unit.key() {
                UnitKey::Primary(ref primary_name) => Diagnostic::error(
                    unit.pos(),
                    format!(
                        "A primary unit has already been declared with name '{}' in library '{}'",
                        primary_name, &self.name
                    ),
                ),
                UnitKey::Secondary(ref primary_name, ref name) => match unit.kind() {
                    AnyKind::Secondary(SecondaryKind::Architecture) => Diagnostic::error(
                        &unit.ident(),
                        format!(
                            "Duplicate architecture '{}' of entity '{}'",
                            name, primary_name,
                        ),
                    ),
                    AnyKind::Secondary(SecondaryKind::PackageBody) => Diagnostic::error(
                        unit.pos(),
                        format!("Duplicate package body of package '{}'", primary_name),
                    ),
                    AnyKind::Primary(_) => {
                        unreachable!();
                    }
                },
            };

            let diagnostic = diagnostic.related(prev_pos, "Previously defined here");
            diagnostics.push(diagnostic);
        }
    }

    /// Remove all design units defined in source
    /// This is used for incremental analysis where only a single source file is updated
    fn remove_source(&mut self, source: &Source) {
        let ref mut removed = self.removed;
        self.units.retain(|_, value| {
            if value.source() != source {
                true
            } else {
                removed.insert(value.unit_id().clone());
                false
            }
        });
        self.units_by_source.remove(source);
        self.duplicates
            .retain(|(_, value)| value.source() != source);

        // Try to add duplicates that were duplicated by a design unit in the removed file
        let num_duplicates = self.duplicates.len();
        let duplicates =
            std::mem::replace(&mut self.duplicates, Vec::with_capacity(num_duplicates));
        for (prev_pos, design_unit) in duplicates.into_iter() {
            if prev_pos.source() == source {
                self.add_design_unit(design_unit);
            } else {
                self.duplicates.push((prev_pos, design_unit));
            }
        }
    }

    fn package_body(&'a self, name: &Symbol) -> Option<&'a LockedUnit> {
        if let Some(ref unit) = self
            .units
            .get(&UnitKey::Secondary(name.clone(), name.clone()))
        {
            if unit.kind() == AnyKind::Secondary(SecondaryKind::PackageBody) {
                Some(unit)
            } else {
                None
            }
        } else {
            None
        }
    }

    /// Iterate over units in the order they appear in the file
    /// Ensures diagnostics does not have to be sorted later
    fn sorted_unit_ids(&self) -> Vec<UnitId> {
        // @TODO insert sort when adding instead
        let mut result = Vec::new();

        for unit_ids in self.units_by_source.values() {
            let mut unit_ids: Vec<UnitId> = unit_ids.clone().into_iter().collect();
            unit_ids.sort_by_key(|unit_id| {
                self.units
                    .get(unit_id.key())
                    .unwrap()
                    .ident()
                    .pos
                    .range()
                    .start
            });
            result.append(&mut unit_ids);
        }
        result
    }
}

pub struct DesignRoot {
    symbols: Arc<Symbols>,
    libraries: FnvHashMap<Symbol, Library>,

    // Dependency tracking for incremental analysis
    // user => set(users)
    users_of: RwLock<FnvHashMap<UnitId, FnvHashSet<UnitId>>>,

    // missing primary name  => set(affected)
    missing_primary: RwLock<FnvHashMap<(Symbol, Symbol), FnvHashSet<UnitId>>>,

    // library name  => set(affected)
    users_of_library_all: RwLock<FnvHashMap<Symbol, FnvHashSet<UnitId>>>,
}

impl DesignRoot {
    pub fn new(symbols: Arc<Symbols>) -> DesignRoot {
        DesignRoot {
            symbols,
            libraries: FnvHashMap::default(),
            users_of: RwLock::new(FnvHashMap::default()),
            missing_primary: RwLock::new(FnvHashMap::default()),
            users_of_library_all: RwLock::new(FnvHashMap::default()),
        }
    }

    /// Create library if it does not exist or return existing
    fn get_or_create_library(&mut self, name: Symbol) -> &mut Library {
        match self.libraries.entry(name) {
            Entry::Occupied(entry) => entry.into_mut(),
            Entry::Vacant(entry) => {
                let library = Library::new(entry.key().clone());
                entry.insert(library)
            }
        }
    }

    pub fn ensure_library(&mut self, name: Symbol) {
        self.get_or_create_library(name);
    }

    fn get_library(&self, library_name: &Symbol) -> Option<&Library> {
        self.libraries.get(library_name)
    }

    fn expect_library(&self, library_name: &Symbol) -> &Library {
        self.get_library(library_name)
            .expect("Library must be defined")
    }

    pub fn add_design_file(&mut self, library_name: Symbol, design_file: DesignFile) {
        self.get_or_create_library(library_name.clone())
            .add_design_file(design_file);
    }

    pub fn remove_source(&mut self, library_name: Symbol, source: &Source) {
        self.get_or_create_library(library_name.clone())
            .remove_source(source);
    }

    /// Search for reference at position
    /// Character offset on a line in a document (zero-based). Assuming that the line is
    /// represented as a string, the `character` value represents the gap between the
    /// `character` and `character + 1`.
    ///
    /// If the character value is greater than the line length it defaults back to the
    /// line length.
    pub fn search_reference(&self, source: &Source, cursor: Position) -> Option<SrcPos> {
        ItemAtCursor::search(self, source, cursor)
    }

    pub fn find_all_references(&self, decl_pos: &SrcPos) -> Vec<SrcPos> {
        FindAllReferences::search(self, decl_pos)
    }

    fn get_analysis<'a>(&self, locked_unit: &'a LockedUnit) -> UnitReadGuard<'a> {
        match locked_unit.unit.entry() {
            AnalysisEntry::Vacant(mut unit) => {
                let context = AnalyzeContext::new(self, locked_unit.unit_id());

                let mut diagnostics = Vec::new();
                let mut root_region = Region::default();
                let mut region = Region::default();

                let has_circular_dependency = if let Err(err) = unit.analyze_design_unit(
                    &context,
                    &mut root_region,
                    &mut region,
                    &mut diagnostics,
                ) {
                    err.push_into(&mut diagnostics);
                    true
                } else {
                    false
                };

                let result = AnalysisData {
                    diagnostics,
                    root_region: Arc::new(root_region),
                    region: Arc::new(region),
                    has_circular_dependency,
                };

                unit.finish(result)
            }
            AnalysisEntry::Occupied(unit) => unit,
        }
    }

    fn get_unit<'a>(&'a self, unit_id: &UnitId) -> Option<&'a LockedUnit> {
        self.libraries
            .get(unit_id.library_name())
            .and_then(|library| library.units.get(&unit_id.key()))
    }

    fn reset_affected(&self, mut affected: FnvHashSet<UnitId>) {
        // Reset analysis state of all design units
        for unit_id in affected.drain() {
            if let Some(unit) = self.get_unit(&unit_id) {
                unit.unit.reset();
            }
        }
    }

    /// Register a direct dependency between two library units
    pub(super) fn make_use_of(
        &self,
        use_pos: Option<&SrcPos>,
        user: &UnitId,
        unit_id: &UnitId,
    ) -> FatalNullResult {
        let mut users_of = self.users_of.write();
        match users_of.entry(unit_id.clone()) {
            Entry::Occupied(mut entry) => {
                entry.get_mut().insert(user.clone());
            }
            Entry::Vacant(entry) => {
                let mut set = FnvHashSet::default();
                set.insert(user.clone());
                entry.insert(set);
            }
        }

        let mut affected = FnvHashSet::default();
        affected.insert(user.clone());
        let all_affected = get_all_affected(&users_of, affected);

        if all_affected.contains(unit_id) {
            Err(CircularDependencyError::new(use_pos))
        } else {
            Ok(())
        }
    }

    /// Register a dependency of library unit for everything within library since .all was used
    pub(super) fn make_use_of_library_all(&self, user: &UnitId, library_name: &Symbol) {
        match self
            .users_of_library_all
            .write()
            .entry(library_name.clone())
        {
            Entry::Occupied(mut entry) => {
                entry.get_mut().insert(user.clone());
            }
            Entry::Vacant(entry) => {
                let mut set = FnvHashSet::default();
                set.insert(user.clone());
                entry.insert(set);
            }
        }
    }

    /// Make use of a missing primary name. The library unit will be sensitive to adding such a primary unit in the future.
    pub(super) fn make_use_of_missing_primary(
        &self,
        user: &UnitId,
        library_name: &Symbol,
        primary_name: &Symbol,
    ) {
        let mut missing_primary = self.missing_primary.write();
        let key = (library_name.clone(), primary_name.clone());
        match missing_primary.entry(key) {
            Entry::Occupied(mut entry) => {
                entry.get_mut().insert(user.clone());
            }
            Entry::Vacant(entry) => {
                let mut set = FnvHashSet::default();
                set.insert(user.clone());
                entry.insert(set);
            }
        }
    }

    /// Reset all unit that need to be re-analyzed
    fn reset(&mut self) {
        let mut removed = FnvHashSet::default();
        let mut added = FnvHashSet::default();

        for library in self.libraries.values_mut() {
            for unit_id in library.added.drain() {
                added.insert(unit_id);
            }
            for unit_id in library.removed.drain() {
                removed.insert(unit_id);
            }
        }

        let mut affected: FnvHashSet<_> = added.union(&removed).cloned().collect();
        let changed: FnvHashSet<_> = removed.intersection(&added).cloned().collect();
        removed = removed.difference(&changed).cloned().collect();
        added = added.difference(&changed).cloned().collect();

        let users_of = self.users_of.read();
        let users_of_library_all = self.users_of_library_all.read();

        // Add affected users which do 'use library.all'
        for unit_id in removed.iter().chain(added.iter()) {
            if let Some(library_all_affected) = users_of_library_all.get(unit_id.library_name()) {
                for user in library_all_affected.into_iter() {
                    affected.insert(user.clone());
                }
            }
        }
        let missing_primary = self.missing_primary.read();
        for ((library_name, primary_name), unit_ids) in missing_primary.iter() {
            let was_added = added.iter().any(|added_id| {
                added_id.library_name() == library_name && added_id.primary_name() == primary_name
            });

            if was_added {
                for unit_id in unit_ids.iter() {
                    affected.insert(unit_id.clone());
                }
            }
        }

        // Affect packages which have got body removed or added
        // Since a package without body may not have deferred constants
        for unit_id in added.iter().chain(removed.iter()) {
            if let AnyKind::Secondary(SecondaryKind::PackageBody) = unit_id.kind() {
                affected.insert(UnitId::package(
                    unit_id.library_name(),
                    unit_id.primary_name(),
                ));
            }
        }

        self.reset_affected(get_all_affected(&users_of, affected));
        drop(users_of);
        drop(users_of_library_all);
        drop(missing_primary);

        let mut users_of = self.users_of.write();
        let mut users_of_library_all = self.users_of_library_all.write();
        let mut missing_primary = self.missing_primary.write();

        // Clean-up after removed units
        for removed_unit in removed.iter() {
            users_of.remove(removed_unit);
            if let Some(library_all_affected) =
                users_of_library_all.get_mut(&removed_unit.library_name())
            {
                library_all_affected.remove(removed_unit);
            }

            missing_primary.retain(|_, unit_ids| {
                unit_ids.remove(removed_unit);
                !unit_ids.is_empty()
            });
        }
    }

    pub fn analyze(&mut self, diagnostics: &mut dyn DiagnosticHandler) {
        self.reset();

        for library in self.libraries.values_mut() {
            library.refresh(diagnostics);
        }

        use rayon::prelude::*;
        // @TODO run in parallel
        let mut units: Vec<_> = Vec::new();
        for library in self.libraries.values() {
            for unit in library.units.values() {
                units.push(unit);
            }
        }

        // @TODO compute the best order to process the units in parallel
        units.par_iter().for_each(|unit| {
            self.get_analysis(*unit);
        });

        // Emit diagnostics sorted within a file
        for library in self.libraries.values() {
            for unit_id in library.sorted_unit_ids() {
                let unit = library.units.get(&unit_id.key()).unwrap();
                diagnostics.append(unit.unit.expect_analyzed().result().diagnostics.clone());
            }
        }
    }
}

fn get_all_affected(
    users_of: &FnvHashMap<UnitId, FnvHashSet<UnitId>>,
    mut affected: FnvHashSet<UnitId>,
) -> FnvHashSet<UnitId> {
    let mut all_affected = FnvHashSet::default();
    let mut next_affected = FnvHashSet::default();

    while !affected.is_empty() {
        for user in affected.drain() {
            all_affected.insert(user.clone());

            if let Some(users) = users_of.get(&user) {
                for new_user in users.iter() {
                    if all_affected.insert(new_user.clone()) {
                        next_affected.insert(new_user.clone());
                    }
                }
            }
        }

        affected = std::mem::replace(&mut next_affected, affected);
    }
    all_affected
}

impl Search for DesignRoot {
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        for library in self.libraries.values() {
            return_if_found!(library.search(searcher));
        }
        NotFound
    }
}

impl Search for Library {
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        for unit_id in self.sorted_unit_ids() {
            let unit = self.units.get(&unit_id.key()).unwrap();
            return_if_found!(unit.unit.read().search(searcher));
        }
        NotFound
    }
}

pub(super) struct AnalyzeContext<'a> {
    root: &'a DesignRoot,

    pub work_sym: Symbol,
    std_sym: Symbol,
    standard_sym: Symbol,

    // Record dependencies and sensitivies when
    // analyzing design units
    //
    // Dependencies define the order in which design units must be analyzed
    //  - for example when doing 'use library.pkg' the pkg is a dependency
    //
    // Sensitivity defines conditions that require re-analysis of a design unit
    //  - for example when doing 'use library.missing' the file is sensitive to adding
    //    primary unit missing to library
    //  - for example when doing 'use missing' the file is sensitive to adding
    //    missing library
    //  - for example when doing 'use library.all' the file is sensitive to adding/removing
    //    anything from library
    current_unit: UnitId,
    uses: RefCell<FnvHashSet<UnitId>>,
    missing_primary: RefCell<FnvHashSet<(Symbol, Symbol)>>,
    uses_library_all: RefCell<FnvHashSet<Symbol>>,
}

impl<'a> AnalyzeContext<'a> {
    fn new(root: &'a DesignRoot, current_unit: &UnitId) -> AnalyzeContext<'a> {
        AnalyzeContext {
            work_sym: root.symbols.symtab().insert_utf8("work"),
            std_sym: root.symbols.symtab().insert_utf8("std"),
            standard_sym: root.symbols.symtab().insert_utf8("standard"),
            root,
            current_unit: current_unit.clone(),
            uses: RefCell::new(FnvHashSet::default()),
            missing_primary: RefCell::new(FnvHashSet::default()),
            uses_library_all: RefCell::new(FnvHashSet::default()),
        }
    }

    pub fn work_library_name(&self) -> &Symbol {
        self.current_unit.library_name()
    }

    pub fn current_unit_id(&self) -> &UnitId {
        &self.current_unit
    }

    pub fn symbol_utf8(&self, name: &str) -> Symbol {
        self.root.symbols.symtab().insert_utf8(name)
    }

    fn make_use_of(&self, use_pos: Option<&SrcPos>, unit_id: &UnitId) -> FatalNullResult {
        // Check local cache before taking lock
        if self.uses.borrow_mut().insert(unit_id.clone()) {
            self.root.make_use_of(use_pos, &self.current_unit, unit_id)
        } else {
            Ok(())
        }
    }

    fn make_use_of_library_all(&self, library_name: &Symbol) {
        // Check local cache before taking lock
        if self
            .uses_library_all
            .borrow_mut()
            .insert(library_name.clone())
        {
            self.root
                .make_use_of_library_all(&self.current_unit, library_name);
        }
    }

    fn make_use_of_missing_primary(&self, library_name: &Symbol, primary_name: &Symbol) {
        let key = (library_name.clone(), primary_name.clone());

        // Check local cache before taking lock
        if self.missing_primary.borrow_mut().insert(key) {
            self.root
                .make_use_of_missing_primary(&self.current_unit, library_name, primary_name);
        }
    }

    pub fn use_all_in_library(
        &self,
        use_pos: &SrcPos,
        library_name: &Symbol,
        region: &mut Region<'_>,
    ) -> FatalNullResult {
        let library = self.root.expect_library(library_name);

        for unit in library.units.values() {
            match unit.kind() {
                AnyKind::Primary(..) => {
                    let data = self.get_analysis(Some(use_pos), unit)?;
                    region.make_potentially_visible(Self::create_primary_unit_decl(
                        unit.unit_id(),
                        &data,
                    ));
                }
                AnyKind::Secondary(..) => {}
            }
        }

        self.make_use_of_library_all(library_name);
        Ok(())
    }

    fn is_standard_package(&self) -> bool {
        *self.work_library_name() == self.std_sym
            && *self.current_unit.primary_name() == self.standard_sym
    }

    /// Add implicit context clause for all packages except STD.STANDARD
    /// library STD, WORK;
    /// use STD.STANDARD.all;
    pub fn add_implicit_context_clause(&self, region: &mut Region<'_>) -> FatalNullResult {
        region.make_library_visible(&self.work_sym, self.work_library_name(), None);

        if self.is_standard_package() || !self.has_library(&self.std_sym) {
            // @TODO add warning for missing standard package
            return Ok(());
        };

        region.make_library_visible(&self.std_sym, &self.std_sym, None);

        let standard_pkg_data = self.expect_standard_package_analysis()?;
        region.make_all_potentially_visible(&standard_pkg_data.result().region);

        Ok(())
    }

    pub fn has_library(&self, library_name: &Symbol) -> bool {
        self.root.get_library(library_name).is_some()
    }

    pub fn has_package_body(&self) -> bool {
        self.root
            .get_library(self.work_library_name())
            .and_then(|library| library.package_body(self.current_unit.primary_name()))
            .is_some()
    }

    fn get_analysis(
        &self,
        use_pos: Option<&SrcPos>,
        unit: &'a LockedUnit,
    ) -> FatalResult<UnitReadGuard<'a>> {
        self.make_use_of(use_pos, &unit.unit_id())?;
        let data = self.root.get_analysis(unit);

        // Change circular dependency reference when used by another unit during analysis
        // The error is changed from within the used unit into the position of the use of the unit
        if data.result().has_circular_dependency {
            Err(CircularDependencyError::new(use_pos))
        } else {
            Ok(data)
        }
    }

    fn create_primary_unit_decl(unit_id: &UnitId, unit: &UnitReadGuard) -> VisibleDeclaration {
        // @TODO add PrimaryUnit Declaration struct

        let unit_id = unit_id.clone();
        let region = unit.result().region.clone();

        let primary_unit = if let Some(primary_unit) = unit.as_primary() {
            primary_unit
        } else {
            unreachable!("Expect primary unit");
        };

        let decl_data = match primary_unit {
            AnyPrimaryUnit::Entity(..) => AnyDeclaration::Entity(unit_id, region),
            AnyPrimaryUnit::Configuration(..) => AnyDeclaration::Configuration(unit_id, region),
            AnyPrimaryUnit::Package(ref package) => {
                if package.generic_clause.is_some() {
                    AnyDeclaration::UninstPackage(unit_id, region)
                } else {
                    AnyDeclaration::Package(unit_id, region)
                }
            }
            AnyPrimaryUnit::PackageInstance(..) => AnyDeclaration::PackageInstance(unit_id, region),
            AnyPrimaryUnit::Context(..) => AnyDeclaration::Context(unit_id, region),
        };

        let designator = WithPos::new(Designator::Identifier(unit.name().clone()), unit.pos());
        VisibleDeclaration::new(designator, decl_data)
    }

    fn get_primary_unit(&self, library_name: &Symbol, name: &Symbol) -> Option<&'a LockedUnit> {
        let library = self.root.get_library(library_name)?;
        // @TODO missing library

        if let Some(ref unit) = library.units.get(&UnitKey::Primary(name.clone())) {
            return Some(unit);
        }

        self.make_use_of_missing_primary(library_name, name);
        None
    }

    fn get_primary_unit_kind(
        &self,
        library_name: &Symbol,
        name: &Symbol,
        kind: PrimaryKind,
    ) -> Option<&'a LockedUnit> {
        let unit = self.get_primary_unit(library_name, name)?;
        if unit.kind() == AnyKind::Primary(kind) {
            Some(unit)
        } else {
            // @TODO no test case for incremental analysis going from wrong primary kind to right
            self.make_use_of_missing_primary(library_name, name);
            None
        }
    }

    pub fn lookup_in_library(
        &self,
        library_name: &Symbol,
        pos: &SrcPos,
        primary_name: &Designator,
    ) -> AnalysisResult<VisibleDeclaration> {
        if let Designator::Identifier(ref primary_name) = primary_name {
            if let Some(unit) = self.get_primary_unit(library_name, primary_name) {
                let data = self.get_analysis(Some(pos), unit)?;
                return Ok(Self::create_primary_unit_decl(unit.unit_id(), &data));
            }
        }

        Err(AnalysisError::NotFatal(Diagnostic::error(
            pos,
            format!(
                "No primary unit '{}' within library '{}'",
                primary_name, library_name
            ),
        )))
    }

    pub fn expect_standard_package_analysis(&self) -> FatalResult<UnitReadGuard<'a>> {
        if let Some(unit) =
            self.get_primary_unit_kind(&self.std_sym, &self.standard_sym, PrimaryKind::Package)
        {
            self.get_analysis(None, unit)
        } else {
            unreachable!(
                "Could not find package {}.{}",
                self.std_sym, self.standard_sym
            );
        }
    }

    pub fn get_primary_analysis(
        &self,
        use_pos: &SrcPos,
        library_name: &Symbol,
        name: &Symbol,
        kind: PrimaryKind,
    ) -> Option<FatalResult<UnitReadGuard<'a>>> {
        let unit = self.get_primary_unit_kind(library_name, name, kind)?;
        Some(self.get_analysis(Some(use_pos), unit))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::test::{check_diagnostics, Code};

    fn new_library_with_diagnostics<'a>(code: &Code, name: &str) -> (Library, Vec<Diagnostic>) {
        let mut diagnostics = Vec::new();
        let mut library = Library::new(code.symbol(name));
        library.add_design_file(code.design_file());
        library.refresh(&mut diagnostics);
        (library, diagnostics)
    }

    #[test]
    fn error_on_duplicate_package_body() {
        let code = Code::new(
            "
package pkg is
end package;

package body pkg is
end package body;

package body pkg is
end package body;
",
        );
        let (library, diagnostics) = new_library_with_diagnostics(&code, "libname");

        assert!(library.package_body(&code.symbol("pkg")).is_some());
        check_diagnostics(
            diagnostics,
            vec![
                Diagnostic::error(code.s("pkg", 3), "Duplicate package body of package 'pkg'")
                    .related(code.s("pkg", 2), "Previously defined here"),
            ],
        );
    }

    #[test]
    fn error_on_duplicate_primary_unit() {
        let code = Code::new(
            "
package pkg is
end package;

entity pkg is
end entity;

entity entname is
end entity;

package entname is
end package;

configuration pkg of entname is
  for rtl
  end for;
end configuration;

package pkg is new gpkg generic map (const => foo);
",
        );
        let (library, diagnostics) = new_library_with_diagnostics(&code, "libname");

        assert_eq!(library.units.len(), 2);
        assert_eq!(library.duplicates.len(), 4);
        check_diagnostics(
            diagnostics,
            vec![
                Diagnostic::error(
                    code.s("pkg", 2),
                    "A primary unit has already been declared with name 'pkg' in library 'libname'"
                ).related(code.s("pkg", 1), "Previously defined here"),
                Diagnostic::error(
                    code.s("entname", 2),
                    "A primary unit has already been declared with name 'entname' in library 'libname'"
                ).related(code.s("entname", 1), "Previously defined here"),
                Diagnostic::error(
                    code.s("pkg", 3),
                    "A primary unit has already been declared with name 'pkg' in library 'libname'"
                ).related(code.s("pkg", 1), "Previously defined here"),
                Diagnostic::error(
                    code.s("pkg", 4),
                    "A primary unit has already been declared with name 'pkg' in library 'libname'"
                ).related(code.s("pkg", 1), "Previously defined here"),
            ]
        );
    }

    #[test]
    fn error_on_duplicate_architecture() {
        let code = Code::new(
            "
entity ent is
end ent;

architecture rtl of ent is
begin
end architecture;

architecture rtl of ent is
begin
end architecture;
",
        );
        let (library, diagnostics) = new_library_with_diagnostics(&code, "libname");

        assert_eq!(library.units.len(), 2);
        assert_eq!(library.duplicates.len(), 1);
        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code.s("rtl", 2),
                "Duplicate architecture 'rtl' of entity 'ent'",
            )
            .related(code.s("rtl", 1), "Previously defined here")],
        );
    }

    #[test]
    fn error_on_duplicate_configuration() {
        let code = Code::new(
            "
entity ent is
end entity;

configuration cfg of ent is
  for rtl
  end for;
end configuration;

configuration cfg of work.ent is
  for rtl
  end for;
end configuration;
",
        );
        let (library, diagnostics) = new_library_with_diagnostics(&code, "libname");

        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code.s("cfg", 2),
                "A primary unit has already been declared with name 'cfg' in library 'libname'",
            )
            .related(code.s1("cfg"), "Previously defined here")],
        );
        assert_eq!(library.units.len(), 2);
        assert_eq!(library.duplicates.len(), 1);
    }
}
