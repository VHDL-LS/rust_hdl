// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::analyze::*;
use super::lock::*;
use super::named_entity::*;
use super::region::NamedEntities;
use super::region::Region;
use super::region::Scope;
use super::standard::StandardTypes;
use super::standard::UniversalTypes;
use super::visibility::Visibility;

use crate::ast::search::*;
use crate::ast::*;
use crate::data::*;
use crate::syntax::Symbols;
use fnv::{FnvHashMap, FnvHashSet};
use parking_lot::RwLock;
use std::collections::hash_map::Entry;
use std::ops::Deref;
use std::sync::Arc;

/// A design unit with design unit data
pub(super) struct AnalysisData {
    pub diagnostics: Vec<Diagnostic>,
    pub has_circular_dependency: bool,
    pub arena: FinalArena,
}

pub(super) type UnitReadGuard<'a> = ReadGuard<'a, AnyDesignUnit, AnalysisData>;
pub(super) type UnitWriteGuard<'a> = WriteGuard<'a, AnyDesignUnit, AnalysisData>;

/// Wraps the AST of a [design unit](../../ast/enum.AnyDesignUnit.html) in a thread-safe
/// r/w-lock for analysis.
pub(super) struct LockedUnit {
    ident: Ident,
    arena_id: ArenaId,
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
            arena_id: ArenaId::default(),
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

/// Represents a VHDL library containing zero or more design units.
///
/// This struct also keeps track of which source file contained which design units.
struct Library {
    name: Symbol,

    /// Arena is only used to store the AnyEnt for the library itself
    /// as there is no other good place to store it
    arena: FinalArena,
    id: EntityId,

    units: FnvHashMap<UnitKey, LockedUnit>,
    units_by_source: FnvHashMap<Source, FnvHashSet<UnitId>>,

    /// Units removed since last analysis.
    removed: FnvHashSet<UnitId>,
    /// Units added since last analysis.
    added: FnvHashSet<UnitId>,

    /// Design units which were not added since they were duplicates.
    /// They need to be kept for later refresh which might make them not duplicates.
    duplicates: Vec<(SrcPos, LockedUnit)>,
}

impl Library {
    fn new(name: Symbol) -> Library {
        let arena = Arena::new(ArenaId::default());

        let ent = arena.alloc(
            Designator::Identifier(name.clone()),
            None,
            Related::None,
            AnyEntKind::Library,
            None,
        );

        Library {
            name,
            id: ent.id(),
            arena: arena.finalize(),
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

    /// Refresh library after removing or adding new design units.
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
                        unit.ident(),
                        format!("Duplicate architecture '{name}' of entity '{primary_name}'",),
                    ),
                    AnyKind::Secondary(SecondaryKind::PackageBody) => Diagnostic::error(
                        unit.pos(),
                        format!("Duplicate package body of package '{primary_name}'"),
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

    /// Remove all design units defined in source.
    /// This is used for incremental analysis where only a single source file is updated.
    fn remove_source(&mut self, source: &Source) {
        let removed = &mut self.removed;
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

    /// Iterate over units in the order they appear in the file.
    /// Ensures diagnostics do not have to be sorted later.
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

    fn get_unit(&self, key: &UnitKey) -> Option<&LockedUnit> {
        self.units.get(key)
    }
}

/// Contains the entire design state.
///
/// Besides all loaded libraries and design units, `DesignRoot` also keeps track of
/// dependencies between design units.
pub struct DesignRoot {
    pub(super) symbols: Arc<Symbols>,
    pub(super) standard_pkg_id: Option<EntityId>,
    pub(super) standard_arena: Option<FinalArena>,
    pub(super) universal: Option<UniversalTypes>,
    pub(super) standard_types: Option<StandardTypes>,
    pub(super) std_ulogic: Option<EntityId>,
    libraries: FnvHashMap<Symbol, Library>,

    // Arena storage of all declaration in the design
    pub(super) arenas: FinalArena,

    // Dependency tracking for incremental analysis.
    // user  =>  set(users)
    users_of: RwLock<FnvHashMap<UnitId, FnvHashSet<UnitId>>>,

    // missing unit name  =>  set(affected)
    #[allow(clippy::type_complexity)]
    missing_unit: RwLock<FnvHashMap<(Symbol, Symbol, Option<Symbol>), FnvHashSet<UnitId>>>,

    // Tracks which units have a "use library.all;" clause.
    // library name  =>  set(affected)
    users_of_library_all: RwLock<FnvHashMap<Symbol, FnvHashSet<UnitId>>>,
}

impl DesignRoot {
    pub fn new(symbols: Arc<Symbols>) -> DesignRoot {
        DesignRoot {
            universal: None,
            standard_pkg_id: None,
            standard_arena: None,
            standard_types: None,
            std_ulogic: None,
            symbols,
            arenas: FinalArena::default(),
            libraries: FnvHashMap::default(),
            users_of: RwLock::new(FnvHashMap::default()),
            missing_unit: RwLock::new(FnvHashMap::default()),
            users_of_library_all: RwLock::new(FnvHashMap::default()),
        }
    }

    /// Create library if it does not exist or return existing
    fn get_or_create_library(&mut self, name: Symbol) -> &mut Library {
        match self.libraries.entry(name) {
            Entry::Occupied(entry) => entry.into_mut(),
            Entry::Vacant(entry) => {
                let name = entry.key().clone();
                let library = Library::new(name);
                entry.insert(library)
            }
        }
    }

    pub fn ensure_library(&mut self, name: Symbol) {
        self.get_or_create_library(name);
    }

    pub(super) fn get_library_units(
        &self,
        library_name: &Symbol,
    ) -> Option<&FnvHashMap<UnitKey, LockedUnit>> {
        self.libraries
            .get(library_name)
            .map(|library| &library.units)
    }

    pub(crate) fn get_design_entity<'a>(
        &'a self,
        library_name: &Symbol,
        ident: &Symbol,
    ) -> Option<DesignEnt<'a>> {
        let units = self.get_library_units(library_name)?;
        let unit = units.get(&UnitKey::Primary(ident.clone()))?;
        let data = self.get_analysis(unit);

        if let AnyDesignUnit::Primary(primary) = data.deref() {
            if let Some(id) = primary.ent_id() {
                let design = DesignEnt::from_any(self.arenas.get(id))?;
                if matches!(design.kind(), Design::Entity(..)) {
                    return Some(design);
                }
            }
        }
        None
    }

    /// Get a named entity corresponding to the library
    pub(super) fn get_library_arena(
        &self,
        library_name: &Symbol,
    ) -> Option<(&FinalArena, EntityId)> {
        self.libraries
            .get(library_name)
            .map(|library| (&library.arena, library.id))
    }

    pub fn add_design_file(&mut self, library_name: Symbol, design_file: DesignFile) {
        self.get_or_create_library(library_name)
            .add_design_file(design_file);
    }

    pub fn remove_source(&mut self, library_name: Symbol, source: &Source) {
        self.get_or_create_library(library_name)
            .remove_source(source);
    }

    /// Search for reference at position
    /// Character offset on a line in a document (zero-based). Assuming that the line is
    /// represented as a string, the `character` value represents the gap between the
    /// `character` and `character + 1`.
    ///
    /// If the character value is greater than the line length it defaults back to the
    /// line length.
    pub fn item_at_cursor<'a>(
        &'a self,
        source: &Source,
        cursor: Position,
    ) -> Option<(SrcPos, EntRef<'a>)> {
        let mut searcher = ItemAtCursor::new(source, cursor);
        let _ = self.search(&mut searcher);
        let (pos, id) = searcher.result?;
        let ent = self.get_ent(id);
        Some((pos, ent))
    }

    pub fn search_reference<'a>(&'a self, source: &Source, cursor: Position) -> Option<EntRef<'a>> {
        let (_, ent) = self.item_at_cursor(source, cursor)?;
        Some(ent)
    }

    pub fn find_definition_of<'a>(&'a self, decl: EntRef<'a>) -> Option<EntRef<'a>> {
        if decl.is_protected_type()
            || decl.is_subprogram_decl()
            || decl.kind().is_deferred_constant()
        {
            let mut searcher = FindEnt::new(self, |ent| ent.is_declared_by(decl));
            let _ = self.search(&mut searcher);

            Some(searcher.result.unwrap_or(decl))
        } else {
            // The definition is the same as the declaration
            Some(decl)
        }
    }

    pub fn find_implementation<'a>(&'a self, ent: EntRef<'a>) -> Vec<EntRef<'a>> {
        if let Designator::Identifier(ident) = ent.designator() {
            if let Some(library_name) = ent.library_name() {
                match ent.kind() {
                    // Find entity with same name as component in the library
                    AnyEntKind::Component(_) => {
                        if let Some(design) = self.get_design_entity(library_name, ident) {
                            return vec![design.into()];
                        }
                    }
                    // Find all components with same name as entity in the library
                    AnyEntKind::Design(Design::Entity(..)) => {
                        let mut searcher = FindAllEnt::new(self, |ent| {
                            matches!(ent.kind(), AnyEntKind::Component(_))
                                && matches!(
                                    ent.designator(),
                                    Designator::Identifier(comp_ident) if comp_ident == ident
                                )
                        });

                        let _ = self.search_library(library_name, &mut searcher);
                        return searcher.result;
                    }
                    _ => {}
                }
            }
        }
        Vec::default()
    }

    #[cfg(test)]
    pub fn search_reference_pos(&self, source: &Source, cursor: Position) -> Option<SrcPos> {
        self.search_reference(source, cursor)
            .and_then(|ent| ent.decl_pos().cloned())
    }
    /// Search for the declaration at decl_pos and format it
    pub fn format_declaration(&self, ent: &AnyEnt) -> Option<String> {
        if let AnyEntKind::Library = ent.kind() {
            Some(format!("library {};", ent.designator()))
        } else {
            let ent = if let Related::InstanceOf(ent) = ent.related {
                ent
            } else {
                ent
            };

            let mut searcher = FormatDeclaration::new(ent);
            let _ = self.search(&mut searcher);
            searcher.result
        }
    }

    /// Search for all references to the declaration at decl_pos
    pub fn find_all_references(&self, ent: EntRef) -> Vec<SrcPos> {
        let mut searcher = FindAllReferences::new(self, ent);
        let _ = self.search(&mut searcher);
        searcher.references
    }

    pub fn public_symbols<'a>(&'a self) -> Box<dyn Iterator<Item = EntRef<'a>> + 'a> {
        Box::new(self.libraries.values().flat_map(|library| {
            std::iter::once(self.arenas.get(library.id)).chain(library.units.values().flat_map(
                |unit| -> Box<dyn Iterator<Item = EntRef<'a>>> {
                    if matches!(unit.kind(), AnyKind::Primary(_)) {
                        let data = self.get_analysis(unit);
                        if let AnyDesignUnit::Primary(primary) = data.deref() {
                            if let Some(id) = primary.ent_id() {
                                let ent = self.arenas.get(id);
                                return Box::new(std::iter::once(ent).chain(public_symbols(ent)));
                            }
                        }
                    } else if matches!(unit.kind(), AnyKind::Secondary(SecondaryKind::Architecture))
                    {
                        let data = self.get_analysis(unit);
                        if let AnyDesignUnit::Secondary(AnySecondaryUnit::Architecture(arch)) =
                            data.deref()
                        {
                            if let Some(id) = arch.ident.decl {
                                let ent = self.arenas.get(id);
                                return Box::new(std::iter::once(ent));
                            }
                        }
                    } else if matches!(unit.kind(), AnyKind::Secondary(SecondaryKind::PackageBody))
                    {
                        let data = self.get_analysis(unit);
                        if let AnyDesignUnit::Secondary(AnySecondaryUnit::PackageBody(body)) =
                            data.deref()
                        {
                            if let Some(id) = body.ident.decl {
                                let ent = self.arenas.get(id);
                                return Box::new(std::iter::once(ent));
                            }
                        }
                    }
                    Box::new(std::iter::empty())
                },
            ))
        }))
    }

    pub fn document_symbols<'a>(
        &'a self,
        library_name: &Symbol,
        source: &Source,
    ) -> Vec<EntHierarchy<'a>> {
        let mut searcher = FindAllEnt::new(self, |ent| ent.is_explicit());

        if let Some(library) = self.libraries.get(library_name) {
            if let Some(unit_ids) = library.units_by_source.get(source) {
                for unit_id in unit_ids {
                    let unit = library.units.get(unit_id.key()).unwrap();
                    let _ = unit.unit.write().search(&mut searcher);
                }
            }
        }

        searcher.result.sort_by_key(|ent| ent.decl_pos());
        EntHierarchy::from_vec(searcher.result)
    }

    pub fn find_all_unresolved(&self) -> (usize, Vec<SrcPos>) {
        let mut searcher = FindAllUnresolved::default();
        let _ = self.search(&mut searcher);
        (searcher.count, searcher.unresolved)
    }

    #[cfg(test)]
    pub fn find_all_references_pos(&self, decl_pos: &SrcPos) -> Vec<SrcPos> {
        if let Some(ent) = self.search_reference(decl_pos.source(), decl_pos.start()) {
            self.find_all_references(ent)
        } else {
            Vec::new()
        }
    }

    #[cfg(test)]
    pub fn find_standard_pkg(&self) -> &AnyEnt {
        let std_lib = self.libraries.get(&self.symbol_utf8("std")).unwrap();
        let unit = std_lib
            .get_unit(&UnitKey::Primary(self.symbol_utf8("standard")))
            .unwrap();

        if let AnyPrimaryUnit::Package(pkg) = unit.unit.write().as_primary_mut().unwrap() {
            self.get_ent(pkg.ident.decl.unwrap())
        } else {
            panic!("Not a package");
        }
    }

    #[cfg(test)]
    pub fn find_standard_symbol(&self, name: &str) -> &AnyEnt {
        if let AnyEntKind::Design(Design::Package(_, region)) = self.find_standard_pkg().kind() {
            region
                .lookup_immediate(&Designator::Identifier(self.symbol_utf8(name)))
                .unwrap()
                .as_non_overloaded()
                .unwrap()
        } else {
            panic!("Not a package");
        }
    }

    pub fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        for library in self.libraries.values() {
            for unit_id in library.sorted_unit_ids() {
                let unit = library.units.get(unit_id.key()).unwrap();
                return_if_found!(unit.unit.write().search(searcher));
            }
        }
        NotFound
    }

    pub fn search_library(
        &self,
        library_name: &Symbol,
        searcher: &mut impl Searcher,
    ) -> SearchResult {
        if let Some(library) = self.libraries.get(library_name) {
            for unit_id in library.sorted_unit_ids() {
                let unit = library.units.get(unit_id.key()).unwrap();
                return_if_found!(unit.unit.write().search(searcher));
            }
        }
        NotFound
    }

    pub fn symbol_utf8(&self, name: &str) -> Symbol {
        self.symbols.symtab().insert_utf8(name)
    }

    fn analyze_unit(&self, arena_id: ArenaId, unit_id: &UnitId, unit: &mut UnitWriteGuard) {
        // All units reference the standard arena
        // @TODO keep the same ArenaId when re-using unit
        let arena = Arena::new(arena_id);
        let context = AnalyzeContext::new(self, unit_id, &arena);
        use std::ops::DerefMut;

        let mut diagnostics = Vec::new();
        let mut has_circular_dependency = false;

        // Ensure no remaining references from previous analysis
        clear_references(unit.deref_mut());

        let result = match unit.deref_mut() {
            AnyDesignUnit::Primary(unit) => {
                if let Err(err) = context.analyze_primary_unit(unit, &mut diagnostics) {
                    has_circular_dependency = true;
                    err.push_into(&mut diagnostics);
                };

                AnalysisData {
                    arena: arena.finalize(),
                    diagnostics,
                    has_circular_dependency,
                }
            }

            AnyDesignUnit::Secondary(unit) => {
                let mut diagnostics = Vec::new();

                if let Err(err) = context.analyze_secondary_unit(unit, &mut diagnostics) {
                    has_circular_dependency = true;
                    err.push_into(&mut diagnostics);
                };

                AnalysisData {
                    arena: arena.finalize(),
                    diagnostics,
                    has_circular_dependency,
                }
            }
        };

        unit.finish(result);
    }

    pub(super) fn get_analysis<'a>(&self, locked_unit: &'a LockedUnit) -> UnitReadGuard<'a> {
        match locked_unit.unit.entry() {
            AnalysisEntry::Vacant(mut unit) => {
                self.analyze_unit(locked_unit.arena_id, locked_unit.unit_id(), &mut unit);
                unit.downgrade()
            }
            AnalysisEntry::Occupied(unit) => unit,
        }
    }

    pub(super) fn get_unit<'a>(&'a self, unit_id: &UnitId) -> Option<&'a LockedUnit> {
        self.libraries
            .get(unit_id.library_name())
            .and_then(|library| library.units.get(unit_id.key()))
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
    ) -> FatalResult {
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

    /// Make use of a missing unit name. The library unit will be sensitive to adding such a unit in the future.
    pub(super) fn make_use_of_missing_unit(
        &self,
        user: &UnitId,
        library_name: &Symbol,
        primary_name: &Symbol,
        secondary_name: Option<&Symbol>,
    ) {
        let mut missing_unit = self.missing_unit.write();
        let key = (
            library_name.clone(),
            primary_name.clone(),
            secondary_name.cloned(),
        );
        match missing_unit.entry(key) {
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

    /// Resets the analysis state of all design units which need to be re-analyzed
    /// because another design unit has been added or removed.
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
                for user in library_all_affected.iter() {
                    affected.insert(user.clone());
                }
            }
        }
        let missing_unit = self.missing_unit.read();
        for ((library_name, primary_name, secondary_name), unit_ids) in missing_unit.iter() {
            let was_added = added.iter().any(|added_id| {
                added_id.library_name() == library_name
                    && added_id.primary_name() == primary_name
                    && added_id.secondary_name() == secondary_name.as_ref()
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
        drop(missing_unit);

        let mut users_of = self.users_of.write();
        let mut users_of_library_all = self.users_of_library_all.write();
        let mut missing_unit = self.missing_unit.write();

        // Clean-up after removed units
        for removed_unit in removed.iter() {
            users_of.remove(removed_unit);
            if let Some(library_all_affected) =
                users_of_library_all.get_mut(removed_unit.library_name())
            {
                library_all_affected.remove(removed_unit);
            }

            missing_unit.retain(|_, unit_ids| {
                unit_ids.remove(removed_unit);
                !unit_ids.is_empty()
            });
        }
    }

    fn analyze_standard_package(&mut self) {
        // Analyze standard package first if it exits
        let std_lib_name = self.symbol_utf8("std");
        if let Some(standard_units) = self
            .libraries
            .get(&std_lib_name)
            .map(|library| &library.units)
        {
            if let Some(locked_unit) =
                standard_units.get(&UnitKey::Primary(self.symbol_utf8("standard")))
            {
                if let AnalysisEntry::Vacant(mut unit) = locked_unit.unit.entry() {
                    // Clear to ensure the analysis of standard package does not believe it has the standard package
                    let arena = Arena::new_std();
                    self.standard_pkg_id = None;
                    self.standard_arena = None;

                    let std_package =
                        if let Some(AnyPrimaryUnit::Package(pkg)) = unit.as_primary_mut() {
                            assert!(pkg.context_clause.is_empty());
                            assert!(pkg.generic_clause.is_none());
                            pkg
                        } else {
                            panic!("Expected standard package is primary unit");
                        };
                    // Ensure no remaining references from previous analysis
                    clear_references(std_package);

                    let standard_pkg = {
                        let (lib_arena, id) = self.get_library_arena(&std_lib_name).unwrap();
                        arena.link(lib_arena);
                        let std_lib = arena.get(id);

                        arena.explicit(
                            self.symbol_utf8("standard"),
                            std_lib,
                            // Will be overwritten below
                            AnyEntKind::Design(Design::Package(
                                Visibility::default(),
                                Region::default(),
                            )),
                            Some(std_package.ident.pos()),
                        )
                    };

                    std_package.ident.decl = Some(standard_pkg.id());

                    let universal =
                        UniversalTypes::new(&arena, standard_pkg, self.symbols.as_ref());
                    self.universal = Some(universal);

                    // Reserve space in the arena for the standard types
                    self.standard_types = Some(StandardTypes::new(
                        &arena,
                        standard_pkg,
                        &mut std_package.decl,
                    ));

                    let context = AnalyzeContext::new(self, locked_unit.unit_id(), &arena);

                    let mut diagnostics = Vec::new();
                    let root_scope = Scope::default();
                    let scope = root_scope.nested().in_package_declaration();

                    {
                        for ent in context.universal_implicits(
                            UniversalType::Integer,
                            context.universal_integer().into(),
                        ) {
                            unsafe {
                                arena.add_implicit(universal.integer, ent);
                            };
                            scope.add(ent, &mut diagnostics);
                        }

                        for ent in context.universal_implicits(
                            UniversalType::Real,
                            context.universal_real().into(),
                        ) {
                            unsafe {
                                arena.add_implicit(universal.real, ent);
                            };
                            scope.add(ent, &mut diagnostics);
                        }
                    }

                    for decl in std_package.decl.iter_mut() {
                        if let Declaration::Type(ref mut type_decl) = decl {
                            context
                                .analyze_type_declaration(
                                    &scope,
                                    standard_pkg,
                                    type_decl,
                                    type_decl.ident.decl, // Set by standard types
                                    &mut diagnostics,
                                )
                                .unwrap();
                        } else {
                            context
                                .analyze_declaration(&scope, standard_pkg, decl, &mut diagnostics)
                                .unwrap();
                        }
                    }
                    scope.close(&mut diagnostics);

                    let mut region = scope.into_region();

                    context.end_of_package_implicits(&mut region, &mut diagnostics);
                    let visibility = root_scope.into_visibility();

                    let kind = AnyEntKind::Design(Design::Package(visibility, region));
                    unsafe {
                        standard_pkg.set_kind(kind);
                    }

                    self.standard_pkg_id = Some(standard_pkg.id());
                    let arena = arena.finalize();
                    self.standard_arena = Some(arena.clone());

                    let result = AnalysisData {
                        arena,
                        diagnostics,
                        has_circular_dependency: false,
                    };

                    unit.finish(result);
                }
            }
        }
    }

    /// Analyze ieee std_logic_1164 package library sequentially
    /// The matching operators such as ?= are implicitly defined for arrays with std_ulogic element
    /// So these types are blessed by the lanaguage and we need global access to them
    fn analyze_std_logic_1164(&mut self) {
        if let Some(lib) = self.libraries.get(&self.symbol_utf8("ieee")) {
            if let Some(unit) = lib.get_unit(&UnitKey::Primary(self.symbol_utf8("std_logic_1164")))
            {
                let data = self.get_analysis(unit);
                let std_logic_arena = &data.result().arena;
                if let AnyDesignUnit::Primary(primary) = data.deref() {
                    if let Some(ent) = primary.ent_id() {
                        let AnyEntKind::Design(Design::Package(_, ref region)) = std_logic_arena.get(ent).kind() else {
                            unreachable!()
                        };

                        if let Some(NamedEntities::Single(ent)) = region.lookup_immediate(
                            &Designator::Identifier(self.symbol_utf8("std_ulogic")),
                        ) {
                            self.std_ulogic = Some(ent.id());
                        }

                        self.arenas.link(&data.result().arena);
                    }
                }
            }
        }
    }

    pub fn analyze(&mut self, diagnostics: &mut dyn DiagnosticHandler) {
        self.reset();

        for library in self.libraries.values_mut() {
            library.refresh(diagnostics);
        }

        // Rebuild declaration arenas of named entities
        self.arenas.clear();

        // Analyze standard package first sequentially since everything else in the
        // language depends on it and we want to save a reference to all types there
        self.analyze_standard_package();
        if let Some(std_arena) = self.standard_arena.as_ref() {
            // @TODO some project.rs unit tests do not have the standard package
            self.arenas.link(std_arena);
        }

        self.analyze_std_logic_1164();

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
            self.get_analysis(unit);
        });

        for library in self.libraries.values() {
            self.arenas.link(&library.arena);
            for unit in library.units.values() {
                if let Some(result) = unit.unit.get() {
                    self.arenas.link(&result.result().arena);
                }
            }
        }

        // Emit diagnostics sorted within a file
        for library in self.libraries.values() {
            for unit_id in library.sorted_unit_ids() {
                let unit = library.units.get(unit_id.key()).unwrap();
                diagnostics.append(unit.unit.expect_analyzed().result().diagnostics.clone());
            }
        }
    }

    /// Get the named entity
    pub fn get_ent(&self, id: EntityId) -> &AnyEnt {
        self.arenas.get(id)
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

pub struct EntHierarchy<'a> {
    pub ent: EntRef<'a>,
    pub children: Vec<EntHierarchy<'a>>,
}

impl<'a> EntHierarchy<'a> {
    fn from_vec(mut symbols: Vec<EntRef<'a>>) -> Vec<EntHierarchy<'a>> {
        let mut by_parent: FnvHashMap<EntityId, Vec<EntRef>> = Default::default();

        symbols.retain(|ent| {
            if let Some(parent) = ent.parent_in_same_source() {
                by_parent.entry(parent.id()).or_default().push(ent);
                false
            } else {
                true
            }
        });
        symbols
            .into_iter()
            .map(|ent| Self::from_ent(ent, &by_parent))
            .collect()
    }

    fn from_ent(
        ent: EntRef<'a>,
        by_parent: &FnvHashMap<EntityId, Vec<EntRef<'a>>>,
    ) -> EntHierarchy<'a> {
        EntHierarchy {
            ent,
            children: if let Some(children) = by_parent.get(&ent.id()) {
                children
                    .iter()
                    .map(|ent| Self::from_ent(ent, by_parent))
                    .collect()
            } else {
                Vec::new()
            },
        }
    }

    pub fn into_flat(self) -> Vec<EntRef<'a>> {
        std::iter::once(self.ent)
            .chain(
                self.children
                    .into_iter()
                    .flat_map(|ent| ent.into_flat().into_iter()),
            )
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::test::{check_diagnostics, Code};

    fn new_library_with_diagnostics(code: &Code, name: &str) -> (Library, Vec<Diagnostic>) {
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

        assert_eq!(library.units.len(), 2);
        assert_eq!(library.duplicates.len(), 1);

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

fn public_symbols<'a>(ent: EntRef<'a>) -> Box<dyn Iterator<Item = EntRef<'a>> + 'a> {
    match ent.kind() {
        AnyEntKind::Design(d) => match d {
            Design::Entity(_, region)
            | Design::Package(_, region)
            | Design::UninstPackage(_, region) => Box::new(
                region
                    .immediates()
                    .flat_map(|ent| std::iter::once(ent).chain(public_symbols(ent))),
            ),
            _ => Box::new(std::iter::empty()),
        },
        AnyEntKind::Type(t) => match t {
            Type::Protected(region, is_body) if !is_body => Box::new(region.immediates()),
            _ => Box::new(std::iter::empty()),
        },
        _ => Box::new(std::iter::empty()),
    }
}
