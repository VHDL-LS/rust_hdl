// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use fnv::{FnvHashMap, FnvHashSet};
use std::collections::hash_map::Entry;

use super::analyze::*;
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
#[cfg_attr(test, derive(Clone))]
pub struct AnalysisData<T> {
    pub diagnostics: Vec<Diagnostic>,
    pub root_region: Arc<Region<'static>>,
    pub region: Arc<Region<'static>>,
    pub has_circular_dependency: bool,
    pub ast: T,
}

pub type EntityData = AnalysisData<EntityDeclaration>;
pub type PackageData = AnalysisData<PackageDeclaration>;

type Entity = PrimaryUnit<EntityDeclaration>;
type Architecture = SecondaryUnit<ArchitectureBody>;
type Configuration = PrimaryUnit<ConfigurationDeclaration>;
type Context = PrimaryUnit<ContextDeclaration>;
type Package = PrimaryUnit<PackageDeclaration>;
type PackageBody = SecondaryUnit<crate::ast::PackageBody>;
type PackageInstance = PrimaryUnit<PackageInstantiation>;

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
enum UnitId {
    Primary(PrimaryKind, Symbol),
    Secondary(SecondaryKind, Symbol, Symbol),
}

trait HasUnitId {
    fn unit_id(&self) -> UnitId;
}

impl<T: HasUnitId> HasUnitId for AnalysisData<T> {
    fn unit_id(&self) -> UnitId {
        self.ast.unit_id()
    }
}

/// Without Kind to get name conflict between different primary units
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
enum UnitKey {
    Primary(Symbol),
    Secondary(Symbol, Symbol),
}

#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
enum PrimaryKind {
    Entity,
    Configuration,
    Package,
    PackageInstance,
    Context,
}

#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
enum SecondaryKind {
    Architecture,
    PackageBody,
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct LibraryUnitId {
    library: Symbol,
    unit_id: UnitId,
}

impl UnitId {
    fn primary_name(&self) -> &Symbol {
        match self {
            UnitId::Primary(_, ref name) => name,
            UnitId::Secondary(_, ref name, _) => name,
        }
    }

    fn key(&self) -> UnitKey {
        match self {
            UnitId::Primary(_, ref name) => UnitKey::Primary(name.clone()),
            UnitId::Secondary(_, ref primary_name, ref name) => {
                UnitKey::Secondary(primary_name.clone(), name.clone())
            }
        }
    }

    fn in_library(&self, library_name: &Symbol) -> LibraryUnitId {
        LibraryUnitId {
            library: library_name.clone(),
            unit_id: self.clone(),
        }
    }
}

impl LibraryUnitId {
    pub fn package(library_name: &Symbol, primary_name: &Symbol) -> LibraryUnitId {
        LibraryUnitId {
            library: library_name.clone(),
            unit_id: UnitId::Primary(PrimaryKind::Package, primary_name.clone()),
        }
    }

    pub fn entity(library_name: &Symbol, primary_name: &Symbol) -> LibraryUnitId {
        LibraryUnitId {
            library: library_name.clone(),
            unit_id: UnitId::Primary(PrimaryKind::Entity, primary_name.clone()),
        }
    }

    pub fn library_name(&self) -> &Symbol {
        &self.library
    }

    pub fn primary_name(&self) -> &Symbol {
        self.unit_id.primary_name()
    }
}

enum AnyLockedPrimary {
    Entity(Entity),
    Configuration(Configuration),
    Package(Package),
    PackageInstance(PackageInstance),
    Context(Context),
}

enum AnyLockedSecondary {
    Architecture(Architecture),
    PackageBody(PackageBody),
}

enum AnyLocked {
    Primary(AnyLockedPrimary),
    Secondary(AnyLockedSecondary),
}

impl Into<AnyLockedPrimary> for AnyPrimaryUnit {
    fn into(self) -> AnyLockedPrimary {
        match self {
            AnyPrimaryUnit::EntityDeclaration(entity) => {
                AnyLockedPrimary::Entity(PrimaryUnit::new(PrimaryKind::Entity, entity))
            }
            AnyPrimaryUnit::PackageDeclaration(package) => {
                AnyLockedPrimary::Package(PrimaryUnit::new(PrimaryKind::Package, package))
            }
            AnyPrimaryUnit::PackageInstance(inst) => AnyLockedPrimary::PackageInstance(
                PrimaryUnit::new(PrimaryKind::PackageInstance, inst),
            ),
            AnyPrimaryUnit::ContextDeclaration(ctx) => {
                AnyLockedPrimary::Context(PrimaryUnit::new(PrimaryKind::Context, ctx))
            }
            AnyPrimaryUnit::Configuration(config) => AnyLockedPrimary::Configuration(
                PrimaryUnit::new(PrimaryKind::Configuration, config),
            ),
        }
    }
}

impl Into<AnyLockedSecondary> for AnySecondaryUnit {
    fn into(self) -> AnyLockedSecondary {
        match self {
            AnySecondaryUnit::Architecture(arch) => AnyLockedSecondary::Architecture(
                SecondaryUnit::new(SecondaryKind::Architecture, arch),
            ),
            AnySecondaryUnit::PackageBody(body) => AnyLockedSecondary::PackageBody(
                SecondaryUnit::new(SecondaryKind::PackageBody, body),
            ),
        }
    }
}

impl Into<AnyLocked> for AnyDesignUnit {
    fn into(self) -> AnyLocked {
        match self {
            AnyDesignUnit::Primary(unit) => AnyLocked::Primary(unit.into()),
            AnyDesignUnit::Secondary(unit) => AnyLocked::Secondary(unit.into()),
        }
    }
}

macro_rules! delegate_primary {
    ($primary:expr, $unit:ident, $block:expr) => {
        match $primary {
            AnyLockedPrimary::Entity($unit) => $block,
            AnyLockedPrimary::Package($unit) => $block,
            AnyLockedPrimary::PackageInstance($unit) => $block,
            AnyLockedPrimary::Context($unit) => $block,
            AnyLockedPrimary::Configuration($unit) => $block,
        }
    };
}

macro_rules! delegate_secondary {
    ($primary:expr, $unit:ident, $block:expr) => {
        match $primary {
            AnyLockedSecondary::Architecture($unit) => $block,
            AnyLockedSecondary::PackageBody($unit) => $block,
        }
    };
}

macro_rules! delegate_any_shallow {
    ($primary:expr, $unit:ident, $block:expr) => {
        match $primary {
            AnyLocked::Primary($unit) => $block,
            AnyLocked::Secondary($unit) => $block,
        }
    };
}

macro_rules! delegate_any {
    ($primary:expr, $unit:ident, $block:expr) => {
        match $primary {
            AnyLocked::Primary($unit) => delegate_primary!($unit, $unit, $block),
            AnyLocked::Secondary($unit) => delegate_secondary!($unit, $unit, $block),
        }
    };
}

impl HasIdent for AnyLockedPrimary {
    fn ident(&self) -> &Ident {
        delegate_primary!(self, unit, unit.ident())
    }
}

impl HasIdent for AnyLockedSecondary {
    fn ident(&self) -> &Ident {
        delegate_secondary!(self, unit, unit.ident())
    }
}

impl HasIdent for AnyLocked {
    fn ident(&self) -> &Ident {
        delegate_any_shallow!(self, unit, unit.ident())
    }
}

impl HasPrimaryIdent for AnyLockedSecondary {
    fn primary_ident(&self) -> &Ident {
        delegate_secondary!(self, unit, unit.primary_ident())
    }
}

impl<T> AnalysisData<T> {
    fn new(ast: T) -> AnalysisData<T> {
        AnalysisData {
            diagnostics: Vec::new(),
            root_region: Arc::new(Region::default()),
            region: Arc::new(Region::default()),
            has_circular_dependency: false,
            ast,
        }
    }

    /// Clear data for new analysis, keeping ast
    fn reset(&mut self) {
        // Clear region and diagnostics
        self.region = Arc::new(Region::default());
        self.diagnostics = Vec::new();
        self.has_circular_dependency = false;
        // Keep ast
    }
}

type LockedData<T> = AnalysisLock<AnalysisData<T>>;

/// Reset analysis state of unit
fn reset_data<T>(lock: &AnalysisLock<AnalysisData<T>>) {
    lock.reset(&|data| data.reset());
}

impl<T: HasIdent> HasIdent for AnalysisData<T> {
    fn ident(&self) -> &Ident {
        self.ast.ident()
    }
}

struct PrimaryUnit<T> {
    ident: Ident,
    kind: PrimaryKind,
    pub data: LockedData<T>,
}

impl<T> HasUnitId for PrimaryUnit<T> {
    fn unit_id(&self) -> UnitId {
        UnitId::Primary(self.kind, self.name().clone())
    }
}

impl<T: HasIdent> PrimaryUnit<T> {
    fn new(kind: PrimaryKind, unit: T) -> PrimaryUnit<T> {
        PrimaryUnit {
            kind,
            ident: unit.ident().clone(),
            data: AnalysisLock::new(AnalysisData::new(unit)),
        }
    }
}

impl<T> HasIdent for PrimaryUnit<T> {
    fn ident(&self) -> &Ident {
        &self.ident
    }
}

struct SecondaryUnit<T> {
    ident: Ident,
    primary_ident: Ident,
    kind: SecondaryKind,
    pub data: LockedData<T>,
}

impl<T> HasUnitId for SecondaryUnit<T> {
    fn unit_id(&self) -> UnitId {
        UnitId::Secondary(self.kind, self.primary_name().clone(), self.name().clone())
    }
}

impl<T: HasIdent + HasPrimaryIdent> SecondaryUnit<T> {
    fn new(kind: SecondaryKind, unit: T) -> SecondaryUnit<T> {
        SecondaryUnit {
            ident: unit.ident().clone(),
            primary_ident: unit.primary_ident().clone(),
            kind,
            data: AnalysisLock::new(AnalysisData::new(unit)),
        }
    }
}

impl<T> HasIdent for SecondaryUnit<T> {
    fn ident(&self) -> &Ident {
        &self.ident
    }
}

impl<T> HasPrimaryIdent for SecondaryUnit<T> {
    fn primary_ident(&self) -> &Ident {
        &self.primary_ident
    }
}

impl HasUnitId for AnyLocked {
    fn unit_id(&self) -> UnitId {
        delegate_any_shallow!(self, unit, unit.unit_id())
    }
}

impl HasUnitId for AnyLockedPrimary {
    fn unit_id(&self) -> UnitId {
        delegate_primary!(self, unit, unit.unit_id())
    }
}

impl HasUnitId for AnyLockedSecondary {
    fn unit_id(&self) -> UnitId {
        delegate_secondary!(self, unit, unit.unit_id())
    }
}

struct Library {
    name: Symbol,
    units: FnvHashMap<UnitKey, AnyLocked>,
    units_by_source: FnvHashMap<Source, FnvHashSet<UnitId>>,

    /// Units removed since last analysis
    removed: FnvHashSet<UnitId>,
    /// Units added since last analysis
    added: FnvHashSet<UnitId>,

    /// Design units which were not added since they were duplicates
    /// They need to be kept for later refresh which might make them not duplicates
    duplicates: Vec<(SrcPos, AnyLocked)>,
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

    fn add_design_unit(&mut self, unit: AnyLocked) {
        let unit_id = unit.unit_id();
        match self.units.entry(unit_id.key()) {
            Entry::Occupied(entry) => {
                self.duplicates
                    .push((entry.get().ident().pos.clone(), unit));
            }
            Entry::Vacant(entry) => {
                self.added.insert(unit_id);
                match self.units_by_source.entry(unit.source().clone()) {
                    Entry::Occupied(mut entry) => {
                        entry.get_mut().insert(unit.unit_id());
                    }
                    Entry::Vacant(entry) => {
                        let mut set = FnvHashSet::default();
                        set.insert(unit.unit_id());
                        entry.insert(set);
                    }
                }
                entry.insert(unit);
            }
        }
    }

    fn add_design_file(&mut self, design_file: DesignFile) {
        for design_unit in design_file.design_units {
            self.add_design_unit(design_unit.into());
        }
    }

    /// Refresh library after removing or adding new design units
    fn refresh(&mut self, diagnostics: &mut dyn DiagnosticHandler) {
        self.append_duplicate_diagnostics(diagnostics);
    }

    fn append_duplicate_diagnostics(&self, diagnostics: &mut dyn DiagnosticHandler) {
        for (prev_pos, design_unit) in self.duplicates.iter() {
            let diagnostic = match design_unit {
                AnyLocked::Primary(primary_unit) => Diagnostic::error(
                    primary_unit.pos(),
                    format!(
                        "A primary unit has already been declared with name '{}' in library '{}'",
                        primary_unit.name(),
                        &self.name
                    ),
                ),
                AnyLocked::Secondary(secondary_unit) => match secondary_unit {
                    AnyLockedSecondary::Architecture(arch) => Diagnostic::error(
                        &arch.ident(),
                        format!(
                            "Duplicate architecture '{}' of entity '{}'",
                            arch.name(),
                            arch.primary_name(),
                        ),
                    ),
                    AnyLockedSecondary::PackageBody(body) => Diagnostic::error(
                        body.pos(),
                        format!("Duplicate package body of package '{}'", body.name()),
                    ),
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
                removed.insert(value.unit_id());
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

    fn entity(&'a self, name: &Symbol) -> Option<&'a Entity> {
        if let Some(AnyLocked::Primary(AnyLockedPrimary::Entity(ref unit))) =
            self.units.get(&UnitKey::Primary(name.clone()))
        {
            Some(unit)
        } else {
            None
        }
    }

    fn package(&'a self, name: &Symbol) -> Option<&'a Package> {
        if let Some(AnyLocked::Primary(AnyLockedPrimary::Package(ref unit))) =
            self.units.get(&UnitKey::Primary(name.clone()))
        {
            Some(unit)
        } else {
            None
        }
    }

    fn expect_package(&'a self, name: &Symbol) -> &'a Package {
        self.package(name).expect("Package must exist")
    }

    fn package_body(&'a self, name: &Symbol) -> Option<&'a PackageBody> {
        if let Some(AnyLocked::Secondary(AnyLockedSecondary::PackageBody(ref unit))) = self
            .units
            .get(&UnitKey::Secondary(name.clone(), name.clone()))
        {
            Some(unit)
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
                    .get(&unit_id.key())
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
    users_of: RwLock<FnvHashMap<LibraryUnitId, FnvHashSet<LibraryUnitId>>>,

    // missing primary name  => set(affected)
    missing_primary: RwLock<FnvHashMap<(Symbol, Symbol), FnvHashSet<LibraryUnitId>>>,

    // library name  => set(affected)
    users_of_library_all: RwLock<FnvHashMap<Symbol, FnvHashSet<LibraryUnitId>>>,
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

    fn get_analysis<'a, T: Analyze>(
        &self,
        library: &Library,
        unit_id: &UnitId,
        lock: &'a AnalysisLock<AnalysisData<T>>,
    ) -> ReadGuard<'a, AnalysisData<T>> {
        match lock.entry() {
            AnalysisEntry::Vacant(mut data) => {
                let context = AnalyzeContext::new(
                    DependencyRecorder::new(self, unit_id.in_library(&library.name)),
                    library.name().clone(),
                    self.symbols.symtab(),
                );

                let AnalysisData {
                    ref mut ast,
                    ref mut has_circular_dependency,
                    ref mut diagnostics,
                    ..
                } = *data;

                let mut root_region = Region::default();
                let mut region = Region::default();
                if let Err(err) = ast.analyze(&context, &mut root_region, &mut region, diagnostics)
                {
                    *has_circular_dependency = true;
                    err.push_into(diagnostics);
                }
                data.root_region = Arc::new(root_region);
                data.region = Arc::new(region);
                data.downgrade()
            }
            AnalysisEntry::Occupied(data) => data,
        }
    }

    fn get_unit<'a>(&'a self, unit_id: &LibraryUnitId) -> Option<&'a AnyLocked> {
        self.libraries
            .get(unit_id.library_name())
            .and_then(|library| library.units.get(&unit_id.unit_id.key()))
    }

    fn reset_affected(&self, mut affected: FnvHashSet<LibraryUnitId>) {
        // Reset analysis state of all design units
        for unit_id in affected.drain() {
            if let Some(unit) = self.get_unit(&unit_id) {
                delegate_any!(unit, unit, reset_data(&unit.data));
            }
        }
    }

    /// Reset all unit that need to be re-analyzed
    fn reset(&mut self) {
        let mut removed = FnvHashSet::default();
        let mut added = FnvHashSet::default();

        for library in self.libraries.values_mut() {
            for unit_id in library.added.drain() {
                added.insert(unit_id.in_library(&library.name));
            }
            for unit_id in library.removed.drain() {
                removed.insert(unit_id.in_library(&library.name));
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
            if let Some(library_all_affected) = users_of_library_all.get(&unit_id.library) {
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
            if let UnitId::Secondary(kind, ..) = unit_id.unit_id {
                if kind == SecondaryKind::PackageBody {
                    affected.insert(LibraryUnitId::package(
                        unit_id.library_name(),
                        unit_id.primary_name(),
                    ));
                }
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
            if let Some(library_all_affected) = users_of_library_all.get_mut(&removed_unit.library)
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
                units.push((library, unit));
            }
        }

        // @TODO compute the best order to process the units in parallel
        units.par_iter().for_each(|(library, unit)| {
            delegate_any!(unit, unit, {
                self.get_analysis(library, &unit.unit_id(), &unit.data);
            });
        });

        // Emit diagnostics sorted within a file
        for library in self.libraries.values() {
            for unit_id in library.sorted_unit_ids() {
                delegate_any!(
                    library.units.get(&unit_id.key()).unwrap(),
                    unit,
                    diagnostics.append(unit.data.expect_analyzed().diagnostics.clone())
                );
            }
        }
    }
}

fn get_all_affected(
    users_of: &FnvHashMap<LibraryUnitId, FnvHashSet<LibraryUnitId>>,
    mut affected: FnvHashSet<LibraryUnitId>,
) -> FnvHashSet<LibraryUnitId> {
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
            return_if_found!(delegate_any!(
                unit,
                unit,
                unit.data.read().ast.search(searcher)
            ));
        }

        NotFound
    }
}

/// Record dependencies and sensitivies when
/// analyzing design units
///
/// Dependencies define the order in which design units must be analyzed
///  - for example when doing 'use library.pkg' the pkg is a dependency
///
/// Sensitivity defines conditions that require re-analysis of a design unit
///  - for example when doing 'use library.missing' the file is sensitive to adding
///    primary unit missing to library
///  - for example when doing 'use missing' the file is sensitive to adding
///    missing library
///  - for example when doing 'use library.all' the file is sensitive to adding/removing
///    anything from library

pub struct DependencyRecorder<'a> {
    root: &'a DesignRoot,
    user: LibraryUnitId,
    uses: RefCell<FnvHashSet<LibraryUnitId>>,
    missing_primary: RefCell<FnvHashSet<(Symbol, Symbol)>>,
    uses_library_all: RefCell<FnvHashSet<Symbol>>,
}

impl<'a> DependencyRecorder<'a> {
    fn new(root: &'a DesignRoot, user: LibraryUnitId) -> DependencyRecorder {
        DependencyRecorder {
            root,
            user,
            uses: RefCell::new(FnvHashSet::default()),
            missing_primary: RefCell::new(FnvHashSet::default()),
            uses_library_all: RefCell::new(FnvHashSet::default()),
        }
    }

    fn make_use_of(&self, use_pos: Option<&SrcPos>, unit_id: &LibraryUnitId) -> FatalNullResult {
        // Check local cache before taking lock
        if self.uses.borrow_mut().insert(unit_id.clone()) {
            let mut users_of = self.root.users_of.write();
            match users_of.entry(unit_id.clone()) {
                Entry::Occupied(mut entry) => {
                    entry.get_mut().insert(self.user.clone());
                }
                Entry::Vacant(entry) => {
                    let mut set = FnvHashSet::default();
                    set.insert(self.user.clone());
                    entry.insert(set);
                }
            }

            let mut affected = FnvHashSet::default();
            affected.insert(self.user.clone());
            let all_affected = get_all_affected(&users_of, affected);
            if all_affected.contains(unit_id) {
                return Err(CircularDependencyError::new(use_pos));
            }
        }

        Ok(())
    }

    fn make_use_of_library_all(&self, library_name: &Symbol) {
        let mut users_of_library_all = self.root.users_of_library_all.write();

        // Check local cache before taking lock
        if self
            .uses_library_all
            .borrow_mut()
            .insert(library_name.clone())
        {
            match users_of_library_all.entry(library_name.clone()) {
                Entry::Occupied(mut entry) => {
                    entry.get_mut().insert(self.user.clone());
                }
                Entry::Vacant(entry) => {
                    let mut set = FnvHashSet::default();
                    set.insert(self.user.clone());
                    entry.insert(set);
                }
            }
        }
    }

    fn make_use_of_missing_primary(&self, library_name: &Symbol, primary_name: &Symbol) {
        let key = (library_name.clone(), primary_name.clone());

        // Check local cache before taking lock
        if self.missing_primary.borrow_mut().insert(key.clone()) {
            let mut missing_primary = self.root.missing_primary.write();
            match missing_primary.entry(key) {
                Entry::Occupied(mut entry) => {
                    entry.get_mut().insert(self.user.clone());
                }
                Entry::Vacant(entry) => {
                    let mut set = FnvHashSet::default();
                    set.insert(self.user.clone());
                    entry.insert(set);
                }
            }
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
            match unit {
                AnyLocked::Primary(unit) => {
                    region.make_potentially_visible(self.create_primary_unit_decl(
                        Some(use_pos),
                        library,
                        unit,
                    )?);
                }
                AnyLocked::Secondary(..) => {}
            }
        }

        self.make_use_of_library_all(library_name);
        Ok(())
    }

    pub fn has_library(&self, library_name: &Symbol) -> bool {
        self.root.get_library(library_name).is_some()
    }

    pub fn has_package_body(&self, library_name: &Symbol, package_name: &Symbol) -> bool {
        self.root
            .get_library(library_name)
            .and_then(|library| library.package_body(package_name))
            .is_some()
    }

    fn create_primary_unit_decl(
        &self,
        use_pos: Option<&SrcPos>,
        library: &Library,
        unit: &AnyLockedPrimary,
    ) -> FatalResult<VisibleDeclaration> {
        // @TODO add PrimaryUnit Declaration struct

        let unit_id = unit.unit_id().in_library(library.name());
        self.make_use_of(use_pos, &unit_id)?;

        let decl_data = match unit {
            AnyLockedPrimary::Entity(unit) => {
                let data = change_circular_reference(
                    use_pos,
                    self.root.get_analysis(library, &unit.unit_id(), &unit.data),
                )?;

                AnyDeclaration::Entity(unit_id, data.region.clone())
            }
            AnyLockedPrimary::Configuration(unit) => {
                let data = change_circular_reference(
                    use_pos,
                    self.root.get_analysis(library, &unit.unit_id(), &unit.data),
                )?;

                AnyDeclaration::Configuration(unit_id, data.region.clone())
            }
            AnyLockedPrimary::Package(unit) => {
                let data = change_circular_reference(
                    use_pos,
                    self.root.get_analysis(library, &unit.unit_id(), &unit.data),
                )?;

                if data.ast.generic_clause.is_some() {
                    AnyDeclaration::UninstPackage(unit_id, data.region.clone())
                } else {
                    AnyDeclaration::Package(unit_id, data.region.clone())
                }
            }
            AnyLockedPrimary::PackageInstance(unit) => {
                let data = change_circular_reference(
                    use_pos,
                    self.root.get_analysis(library, &unit.unit_id(), &unit.data),
                )?;

                AnyDeclaration::PackageInstance(unit_id, data.region.clone())
            }
            AnyLockedPrimary::Context(unit) => {
                let data = change_circular_reference(
                    use_pos,
                    self.root.get_analysis(library, &unit.unit_id(), &unit.data),
                )?;

                AnyDeclaration::Context(unit_id, data.region.clone())
            }
        };

        let designator = WithPos::new(Designator::Identifier(unit.name().clone()), unit.pos());
        Ok(VisibleDeclaration::new(designator, decl_data))
    }

    pub fn lookup_in_library(
        &self,
        use_pos: Option<&SrcPos>,
        library_name: &Symbol,
        primary_name: &Symbol,
    ) -> FatalResult<Option<VisibleDeclaration>> {
        let library = self.root.expect_library(library_name);

        if let Some(unit) = library.units.get(&UnitKey::Primary(primary_name.clone())) {
            match unit {
                AnyLocked::Primary(unit) => {
                    Ok(Some(self.create_primary_unit_decl(use_pos, library, unit)?))
                }
                AnyLocked::Secondary(..) => {
                    unreachable!();
                }
            }
        } else {
            self.make_use_of_missing_primary(library_name, primary_name);
            Ok(None)
        }
    }

    pub fn expect_package_declaration_analysis(
        &self,
        use_pos: Option<&SrcPos>,
        unit_id: &LibraryUnitId,
    ) -> FatalResult<ReadGuard<'a, PackageData>> {
        let library = self.root.expect_library(&unit_id.library);
        let package = library.expect_package(unit_id.primary_name());
        self.make_use_of(use_pos, unit_id)?;
        change_circular_reference(
            use_pos,
            self.root
                .get_analysis(library, &package.unit_id(), &package.data),
        )
    }

    pub fn get_package_declaration_analysis(
        &self,
        use_pos: Option<&SrcPos>,
        unit_id: &LibraryUnitId,
    ) -> Option<FatalResult<ReadGuard<'a, PackageData>>> {
        let result = self
            .root
            .libraries
            .get(unit_id.library_name())
            .and_then(|library| {
                library.package(unit_id.primary_name()).map(|package| {
                    self.make_use_of(use_pos, unit_id)?;
                    change_circular_reference(
                        use_pos,
                        self.root
                            .get_analysis(library, &package.unit_id(), &package.data),
                    )
                })
            });

        if result.is_none() {
            self.make_use_of_missing_primary(unit_id.library_name(), unit_id.primary_name());
        }

        result
    }

    pub fn get_entity_declaration_analysis(
        &self,
        use_pos: Option<&SrcPos>,
        unit_id: &LibraryUnitId,
    ) -> Option<FatalResult<ReadGuard<'a, EntityData>>> {
        let result = self
            .root
            .libraries
            .get(unit_id.library_name())
            .and_then(|library| {
                library.entity(unit_id.primary_name()).map(|entity| {
                    self.make_use_of(use_pos, unit_id)?;
                    change_circular_reference(
                        use_pos,
                        self.root
                            .get_analysis(library, &entity.unit_id(), &entity.data),
                    )
                })
            });

        if result.is_none() {
            self.make_use_of_missing_primary(unit_id.library_name(), unit_id.primary_name());
        }
        result
    }
}

/// Change circular dependency reference when used by another unit during analysis
/// The error is changed from within the used unit into the position of the use of the unit
fn change_circular_reference<'a, T>(
    use_pos: Option<&SrcPos>,
    data: ReadGuard<'a, AnalysisData<T>>,
) -> FatalResult<ReadGuard<'a, AnalysisData<T>>> {
    if data.has_circular_dependency {
        Err(CircularDependencyError::new(use_pos))
    } else {
        Ok(data)
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
