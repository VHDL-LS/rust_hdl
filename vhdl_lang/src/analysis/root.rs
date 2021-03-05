// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use fnv::{FnvHashMap, FnvHashSet};
use std::collections::hash_map::Entry;

use super::analyze::*;
use super::lock::*;
use super::region::*;
use crate::ast::search::*;
use crate::ast::*;
use crate::data::*;
use crate::syntax::Symbols;
use parking_lot::RwLock;
use std::sync::Arc;

/// A design unit with design unit data
pub(super) struct AnalysisData {
    pub diagnostics: Vec<Diagnostic>,
    pub has_circular_dependency: bool,

    // Only for primary units
    pub root_region: Arc<Region<'static>>,
    pub region: Arc<Region<'static>>,
    pub ent: Option<Arc<NamedEntity>>,
}

pub(super) type UnitReadGuard<'a> = ReadGuard<'a, AnyDesignUnit, AnalysisData>;

/// Wraps the AST of a [design unit](../../ast/enum.AnyDesignUnit.html) in a thread-safe
/// r/w-lock for analysis.
pub(super) struct LockedUnit {
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

/// Represents a VHDL library containing zero or more design units.
///
/// This struct also keeps track of which source file contained which design units.
struct Library {
    name: Symbol,

    /// Named entity corresponding to the library.
    ent: Arc<NamedEntity>,

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

impl<'a> Library {
    fn new(name: Symbol) -> Library {
        let ent = Arc::new(NamedEntity::new(
            Designator::Identifier(name.clone()),
            NamedEntityKind::Library,
            None,
        ));
        Library {
            name,
            ent,
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
}

/// Contains the entire design state.
///
/// Besides all loaded libraries and design units, `DesignRoot` also keeps track of
/// dependencies between design units.
pub struct DesignRoot {
    symbols: Arc<Symbols>,
    libraries: FnvHashMap<Symbol, Library>,

    // Dependency tracking for incremental analysis.
    // user  =>  set(users)
    users_of: RwLock<FnvHashMap<UnitId, FnvHashSet<UnitId>>>,

    // missing primary name  =>  set(affected)
    missing_primary: RwLock<FnvHashMap<(Symbol, Symbol), FnvHashSet<UnitId>>>,

    // Tracks which units have a "use library.all;" clause.
    // library name  =>  set(affected)
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

    pub(super) fn get_library_units(
        &self,
        library_name: &Symbol,
    ) -> Option<&FnvHashMap<UnitKey, LockedUnit>> {
        self.libraries
            .get(library_name)
            .map(|library| &library.units)
    }

    /// Get a named entity corresponding to the library
    pub(super) fn get_library_ent(&self, library_name: &Symbol) -> Option<&Arc<NamedEntity>> {
        self.libraries.get(library_name).map(|library| &library.ent)
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
    pub fn search_reference(&self, source: &Source, cursor: Position) -> Option<SrcPos> {
        ItemAtCursor::search(self, source, cursor)
    }

    /// Search for the declaration at decl_pos and format it
    pub fn format_declaration(&self, decl_pos: &SrcPos) -> Option<String> {
        FormatDeclaration::search(self, decl_pos)
    }

    /// Search for all references to the declaration at decl_pos
    pub fn find_all_references(&self, decl_pos: &SrcPos) -> Vec<SrcPos> {
        FindAllReferences::search(self, decl_pos)
    }

    pub(super) fn symbol_utf8(&self, name: &str) -> Symbol {
        self.symbols.symtab().insert_utf8(name)
    }

    pub(super) fn get_analysis<'a>(&self, locked_unit: &'a LockedUnit) -> UnitReadGuard<'a> {
        match locked_unit.unit.entry() {
            AnalysisEntry::Vacant(mut unit) => {
                let context = AnalyzeContext::new(self, locked_unit.unit_id());

                let entity_id = super::named_entity::new_id();
                let mut diagnostics = Vec::new();
                let mut root_region = Region::default();
                let mut region = Region::default();

                let has_circular_dependency = if let Err(err) = context.analyze_design_unit(
                    entity_id,
                    &mut *unit,
                    &mut root_region,
                    &mut region,
                    &mut diagnostics,
                ) {
                    err.push_into(&mut diagnostics);
                    true
                } else {
                    false
                };

                let root_region = Arc::new(root_region);
                let region = Arc::new(region);

                let ent = if let Some(primary_unit) = unit.as_primary() {
                    let region = region.clone();
                    let kind = match primary_unit {
                        AnyPrimaryUnit::Entity(..) => NamedEntityKind::Entity(region),
                        AnyPrimaryUnit::Configuration(..) => NamedEntityKind::Configuration(region),
                        AnyPrimaryUnit::Package(ref package) => {
                            if package.generic_clause.is_some() {
                                NamedEntityKind::UninstPackage(region)
                            } else {
                                NamedEntityKind::Package(region)
                            }
                        }
                        AnyPrimaryUnit::PackageInstance(..) => {
                            NamedEntityKind::PackageInstance(region)
                        }
                        AnyPrimaryUnit::Context(..) => NamedEntityKind::Context(region),
                    };

                    Some(Arc::new(NamedEntity::new_with_id(
                        entity_id,
                        unit.name(),
                        kind,
                        Some(unit.pos()),
                    )))
                } else {
                    None
                };

                let result = AnalysisData {
                    diagnostics,
                    root_region,
                    region,
                    ent,
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
