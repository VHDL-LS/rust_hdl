// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use fnv::{FnvHashMap, FnvHashSet};
use std::collections::hash_map::Entry;

use super::declarative_region::{AnyDeclaration, DeclarativeRegion, VisibleDeclaration};
use super::lock::AnalysisLock;
use super::lock::{AnalysisEntry, ReadGuard};
use super::semantic::{Analyzer, FatalResult};
use crate::ast::search::*;
use crate::ast::*;
use crate::diagnostic::{Diagnostic, DiagnosticHandler};
use crate::source::HasSource;
use crate::symbol_table::SymbolTable;
use std::cell::RefCell;
use std::sync::{Arc, RwLock};

/// A design unit with design unit data
#[cfg_attr(test, derive(Clone))]
pub struct AnalysisData<T> {
    pub diagnostics: Vec<Diagnostic>,
    pub region: DeclarativeRegion<'static>,
    pub ast: T,
}

pub type EntityData = AnalysisData<EntityDeclaration>;
pub type ArchitectureData = AnalysisData<ArchitectureBody>;
pub type ConfigurationData = AnalysisData<ConfigurationDeclaration>;
pub type ContextData = AnalysisData<ContextDeclaration>;
pub type PackageData = AnalysisData<PackageDeclaration>;
pub type PackageBodyData = AnalysisData<crate::ast::PackageBody>;
pub type PackageInstanceData = AnalysisData<PackageInstantiation>;

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
            region: DeclarativeRegion::default(),
            ast,
        }
    }

    /// Clear data for new analysis, keeping ast
    fn reset(&mut self) {
        // Clear region and diagnostics
        self.region = DeclarativeRegion::default();
        self.diagnostics = Vec::new();
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

impl<T> PrimaryUnit<T> {
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

impl<T> SecondaryUnit<T> {
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

impl AnyLocked {
    fn unit_id(&self) -> UnitId {
        delegate_any_shallow!(self, unit, unit.unit_id())
    }
}

impl AnyLockedPrimary {
    fn unit_id(&self) -> UnitId {
        delegate_primary!(self, unit, unit.unit_id())
    }
}

impl AnyLockedSecondary {
    fn unit_id(&self) -> UnitId {
        delegate_secondary!(self, unit, unit.unit_id())
    }
}

struct Library {
    name: Symbol,
    region: DeclarativeRegion<'static>,
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
            region: DeclarativeRegion::default(),
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
        self.rebuild_region();
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

    fn rebuild_region(&mut self) {
        let mut diagnostics = Vec::new();
        self.region = DeclarativeRegion::default();

        for unit in self.units.values() {
            let unit_id = unit.unit_id().in_library(&self.name);

            match unit {
                AnyLocked::Primary(unit) => match unit {
                    AnyLockedPrimary::Entity(ent) => {
                        self.region.add(
                            ent.ident(),
                            AnyDeclaration::Entity(unit_id),
                            &mut diagnostics,
                        );
                    }
                    AnyLockedPrimary::Configuration(config) => {
                        self.region.add(
                            config.ident(),
                            AnyDeclaration::Configuration(unit_id),
                            &mut diagnostics,
                        );
                    }
                    AnyLockedPrimary::Package(pkg) => {
                        if pkg.data.expect_read().ast.generic_clause.is_some() {
                            self.region.add(
                                pkg.ident(),
                                AnyDeclaration::UninstPackage(unit_id),
                                &mut diagnostics,
                            );
                        } else {
                            self.region.add(
                                pkg.ident(),
                                AnyDeclaration::Package(unit_id),
                                &mut diagnostics,
                            );
                        }
                    }
                    AnyLockedPrimary::Context(ctx) => {
                        self.region.add(
                            ctx.ident(),
                            AnyDeclaration::Context(unit_id),
                            &mut diagnostics,
                        );
                    }
                    AnyLockedPrimary::PackageInstance(pkg) => {
                        self.region.add(
                            pkg.ident(),
                            AnyDeclaration::PackageInstance(unit_id),
                            &mut diagnostics,
                        );
                    }
                },
                AnyLocked::Secondary(_) => {}
            }
        }

        assert_eq!(
            diagnostics,
            vec![],
            "Expect no diagnostics when building library region"
        );
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

    fn package_instance(&'a self, name: &Symbol) -> Option<&'a PackageInstance> {
        if let Some(AnyLocked::Primary(AnyLockedPrimary::PackageInstance(ref unit))) =
            self.units.get(&UnitKey::Primary(name.clone()))
        {
            Some(unit)
        } else {
            None
        }
    }

    fn expect_package_instance(&'a self, name: &Symbol) -> &'a PackageInstance {
        self.package_instance(name)
            .expect("Package instance must exist")
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

    fn context(&'a self, name: &Symbol) -> Option<&'a Context> {
        if let Some(AnyLocked::Primary(AnyLockedPrimary::Context(ref unit))) =
            self.units.get(&UnitKey::Primary(name.clone()))
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

fn add_diagnostics<T>(
    diagnostics: &mut dyn DiagnosticHandler,
    data: FatalResult<ReadGuard<AnalysisData<T>>>,
) {
    match data {
        Ok(data) => {
            diagnostics.append(data.diagnostics.clone());
        }
        Err(fatal_err) => {
            fatal_err.push_into(diagnostics);
        }
    }
}

pub struct DesignRoot {
    symtab: Arc<SymbolTable>,
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
    pub fn new(symtab: Arc<SymbolTable>) -> DesignRoot {
        DesignRoot {
            symtab,
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
        self.search(&mut ItemAtCursor::new(source, cursor)).into()
    }

    pub fn find_all_references(&self, decl_pos: &SrcPos) -> Vec<SrcPos> {
        FindAllReferences::new(decl_pos).search(self)
    }

    fn get_package_instance_analysis<'a>(
        &self,
        library: &Library,
        instance: &'a PackageInstance,
    ) -> FatalResult<ReadGuard<'a, PackageInstanceData>> {
        match instance.data.entry()? {
            AnalysisEntry::Vacant(mut instance_data) => {
                let analyzer = Analyzer::new(
                    DependencyRecorder::new(self, instance.unit_id().in_library(&library.name)),
                    library.name().clone(),
                    self.symtab.clone(),
                );
                analyzer.analyze_package_instance(&mut instance_data)?;
                drop(instance_data);
                Ok(instance.data.expect_analyzed())
            }
            AnalysisEntry::Occupied(instance_data) => Ok(instance_data),
        }
    }

    fn get_configuration_analysis<'a>(
        &self,
        library: &Library,
        config: &'a Configuration,
    ) -> FatalResult<ReadGuard<'a, ConfigurationData>> {
        match config.data.entry()? {
            AnalysisEntry::Vacant(mut config_data) => {
                let analyzer = Analyzer::new(
                    DependencyRecorder::new(self, config.unit_id().in_library(&library.name)),
                    library.name().clone(),
                    self.symtab.clone(),
                );
                analyzer.analyze_configuration(&mut config_data)?;
                drop(config_data);
                Ok(config.data.expect_analyzed())
            }
            AnalysisEntry::Occupied(config_data) => Ok(config_data),
        }
    }

    fn get_package_declaration_analysis<'a>(
        &self,
        library: &Library,
        package: &'a Package,
    ) -> FatalResult<ReadGuard<'a, PackageData>> {
        match package.data.entry()? {
            AnalysisEntry::Vacant(mut package_data) => {
                let analyzer = Analyzer::new(
                    DependencyRecorder::new(self, package.unit_id().in_library(&library.name)),
                    library.name().clone(),
                    self.symtab.clone(),
                );
                analyzer.analyze_package_declaration(
                    library.package_body(package_data.ast.name()).is_some(),
                    &mut package_data,
                )?;
                drop(package_data);
                Ok(package.data.expect_analyzed())
            }
            AnalysisEntry::Occupied(package) => Ok(package),
        }
    }

    fn get_package_body_analysis<'a>(
        &self,
        library: &Library,
        body: &'a PackageBody,
    ) -> FatalResult<ReadGuard<'a, PackageBodyData>> {
        match body.data.entry()? {
            AnalysisEntry::Vacant(mut body_data) => {
                let analyzer = Analyzer::new(
                    DependencyRecorder::new(self, body.unit_id().in_library(&library.name)),
                    library.name().clone(),
                    self.symtab.clone(),
                );
                analyzer.analyze_package_body_unit(&mut body_data)?;
                drop(body_data);
                Ok(body.data.expect_analyzed())
            }
            AnalysisEntry::Occupied(body_data) => Ok(body_data),
        }
    }

    fn get_architecture_analysis<'a>(
        &self,
        library: &Library,
        arch: &'a Architecture,
    ) -> FatalResult<ReadGuard<'a, ArchitectureData>> {
        match arch.data.entry()? {
            AnalysisEntry::Vacant(mut arch_data) => {
                let analyzer = Analyzer::new(
                    DependencyRecorder::new(self, arch.unit_id().in_library(&library.name)),
                    library.name().clone(),
                    self.symtab.clone(),
                );
                analyzer.analyze_architecture(&mut arch_data)?;
                drop(arch_data);
                Ok(arch.data.expect_analyzed())
            }
            AnalysisEntry::Occupied(arch_data) => Ok(arch_data),
        }
    }

    fn get_entity_declaration_analysis<'a>(
        &self,
        library: &Library,
        entity: &'a Entity,
    ) -> FatalResult<ReadGuard<'a, EntityData>> {
        match entity.data.entry()? {
            AnalysisEntry::Vacant(mut entity_data) => {
                let analyzer = Analyzer::new(
                    DependencyRecorder::new(self, entity.unit_id().in_library(&library.name)),
                    library.name().clone(),
                    self.symtab.clone(),
                );
                analyzer.analyze_entity_declaration(&mut entity_data)?;
                drop(entity_data);
                Ok(entity.data.expect_analyzed())
            }
            AnalysisEntry::Occupied(entity_data) => Ok(entity_data),
        }
    }

    fn get_context_analysis<'a>(
        &self,
        library: &Library,
        context: &'a Context,
    ) -> FatalResult<ReadGuard<'a, ContextData>> {
        match context.data.entry()? {
            AnalysisEntry::Vacant(mut context_data) => {
                let analyzer = Analyzer::new(
                    DependencyRecorder::new(self, context.unit_id().in_library(&library.name)),
                    library.name().clone(),
                    self.symtab.clone(),
                );
                analyzer.analyze_context(&mut context_data)?;
                drop(context_data);
                Ok(context.data.expect_analyzed())
            }
            AnalysisEntry::Occupied(context_data) => Ok(context_data),
        }
    }

    fn analyze_unit(
        &self,
        library: &Library,
        unit: &AnyLocked,
        diagnostics: &mut dyn DiagnosticHandler,
    ) {
        match unit {
            AnyLocked::Primary(unit) => match unit {
                AnyLockedPrimary::Entity(entity) => {
                    add_diagnostics(
                        diagnostics,
                        self.get_entity_declaration_analysis(library, entity),
                    );
                }
                AnyLockedPrimary::Configuration(config) => {
                    add_diagnostics(
                        diagnostics,
                        self.get_configuration_analysis(library, config),
                    );
                }
                AnyLockedPrimary::Package(package) => {
                    add_diagnostics(
                        diagnostics,
                        self.get_package_declaration_analysis(library, package),
                    );
                }
                AnyLockedPrimary::PackageInstance(instance) => {
                    add_diagnostics(
                        diagnostics,
                        self.get_package_instance_analysis(library, instance),
                    );
                }
                AnyLockedPrimary::Context(context) => {
                    add_diagnostics(diagnostics, self.get_context_analysis(library, context));
                }
            },
            AnyLocked::Secondary(unit) => match unit {
                AnyLockedSecondary::Architecture(arch) => {
                    add_diagnostics(diagnostics, self.get_architecture_analysis(library, arch));
                }
                AnyLockedSecondary::PackageBody(body) => {
                    add_diagnostics(diagnostics, self.get_package_body_analysis(library, body));
                }
            },
        }
    }

    fn get_all_affected(
        &self,
        mut affected: FnvHashSet<LibraryUnitId>,
    ) -> FnvHashSet<LibraryUnitId> {
        let mut all_affected = FnvHashSet::default();
        let users_of = self.users_of.read().unwrap();

        while !affected.is_empty() {
            let mut next_affected = FnvHashSet::default();

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

            affected = next_affected;
        }
        all_affected
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

        let users_of = self.users_of.read().unwrap();
        let users_of_library_all = self.users_of_library_all.read().unwrap();

        // Add affected users which do 'use library.all'
        for unit_id in removed.iter().chain(added.iter()) {
            if let Some(library_all_affected) = users_of_library_all.get(&unit_id.library) {
                for user in library_all_affected.into_iter() {
                    affected.insert(user.clone());
                }
            }
        }
        let missing_primary = self.missing_primary.read().unwrap();
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

        drop(users_of);
        drop(users_of_library_all);
        drop(missing_primary);
        self.reset_affected(self.get_all_affected(affected));

        let mut users_of = self.users_of.write().unwrap();
        let mut users_of_library_all = self.users_of_library_all.write().unwrap();
        let mut missing_primary = self.missing_primary.write().unwrap();

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

        for library in self.libraries.values() {
            for unit_id in library.sorted_unit_ids() {
                self.analyze_unit(
                    library,
                    library.units.get(&unit_id.key()).unwrap(),
                    diagnostics,
                );
            }
        }
    }
}

impl<T> Search<T> for DesignRoot {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        for library in self.libraries.values() {
            return_if!(library.search(searcher));
        }
        NotFound
    }
}

impl<T> Search<T> for Library {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        for unit_id in self.sorted_unit_ids() {
            let unit = self.units.get(&unit_id.key()).unwrap();
            return_if!(delegate_any!(
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

    pub fn use_all_in_library(&self, library_name: &Symbol, region: &mut DeclarativeRegion<'_>) {
        let library = self.root.expect_library(library_name);
        region.make_all_potentially_visible(&library.region);

        self.uses_library_all
            .borrow_mut()
            .insert(library_name.clone());
    }

    pub fn has_library(&self, library_name: &Symbol) -> bool {
        self.root.get_library(library_name).is_some()
    }

    fn make_use_of(&self, unit_id: LibraryUnitId) {
        self.uses.borrow_mut().insert(unit_id);
    }

    pub fn lookup_in_library(
        &self,
        library_name: &Symbol,
        designator: &Designator,
    ) -> Option<&VisibleDeclaration> {
        if let Some(decl) = self
            .root
            .expect_library(library_name)
            .region
            .lookup(designator, false)
        {
            // @TODO libary does not need region, we can build it on the fly
            let unit_id = match decl.first() {
                AnyDeclaration::Entity(unit_id) => unit_id,
                AnyDeclaration::Configuration(unit_id) => unit_id,
                AnyDeclaration::Package(unit_id) => unit_id,
                AnyDeclaration::UninstPackage(unit_id) => unit_id,
                AnyDeclaration::PackageInstance(unit_id) => unit_id,
                AnyDeclaration::Context(unit_id) => unit_id,
                _ => panic!("Library may only design units"),
            };

            self.make_use_of(unit_id.clone());
            Some(decl)
        } else {
            if let Designator::Identifier(sym) = designator {
                self.missing_primary
                    .borrow_mut()
                    .insert((library_name.clone(), sym.clone()));
            }
            None
        }
    }

    pub fn expect_package_instance_analysis(
        &self,
        unit_id: &LibraryUnitId,
    ) -> FatalResult<ReadGuard<'a, PackageInstanceData>> {
        let library = self.root.expect_library(&unit_id.library);
        let instance = library.expect_package_instance(unit_id.primary_name());
        self.make_use_of(unit_id.clone());
        self.root.get_package_instance_analysis(library, instance)
    }

    pub fn expect_package_declaration_analysis(
        &self,
        unit_id: &LibraryUnitId,
    ) -> FatalResult<ReadGuard<'a, PackageData>> {
        let library = self.root.expect_library(&unit_id.library);
        let package = library.expect_package(unit_id.primary_name());
        self.make_use_of(unit_id.clone());
        self.root.get_package_declaration_analysis(library, package)
    }

    pub fn get_package_declaration_analysis(
        &self,
        unit_id: &LibraryUnitId,
    ) -> Option<FatalResult<ReadGuard<'a, PackageData>>> {
        self.make_use_of(unit_id.clone());
        self.root
            .libraries
            .get(unit_id.library_name())
            .and_then(|library| {
                library
                    .package(unit_id.primary_name())
                    .map(|package| self.root.get_package_declaration_analysis(library, package))
            })
    }

    pub fn expect_entity_declaration_analysis(
        &self,
        unit_id: &LibraryUnitId,
    ) -> FatalResult<ReadGuard<'a, EntityData>> {
        let library = self.root.expect_library(&unit_id.library);
        let entity = library.entity(unit_id.primary_name()).unwrap();
        self.make_use_of(unit_id.clone());
        self.root.get_entity_declaration_analysis(library, entity)
    }

    pub fn get_entity_declaration_analysis(
        &self,
        unit_id: &LibraryUnitId,
    ) -> Option<FatalResult<ReadGuard<'a, EntityData>>> {
        self.make_use_of(unit_id.clone());
        self.root
            .libraries
            .get(unit_id.library_name())
            .and_then(|library| {
                library
                    .entity(unit_id.primary_name())
                    .map(|entity| self.root.get_entity_declaration_analysis(library, entity))
            })
    }

    pub fn expect_context_analysis(
        &self,
        unit_id: &LibraryUnitId,
    ) -> FatalResult<ReadGuard<'a, ContextData>> {
        let library = self.root.expect_library(&unit_id.library);
        let context = library.context(unit_id.primary_name()).unwrap();
        self.make_use_of(unit_id.clone());
        self.root.get_context_analysis(library, context)
    }
}

impl<'a> Drop for DependencyRecorder<'a> {
    fn drop(&mut self) {
        let mut users_of = self.root.users_of.write().unwrap();
        let mut users_of_library_all = self.root.users_of_library_all.write().unwrap();

        for source in self.uses.borrow_mut().drain() {
            match users_of.entry(source.clone()) {
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

        for library_name in self.uses_library_all.borrow_mut().drain() {
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

        let mut missing_primary = self.root.missing_primary.write().unwrap();
        for key in self.missing_primary.borrow_mut().drain() {
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::{check_diagnostics, Code};

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
