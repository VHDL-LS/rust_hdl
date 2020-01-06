// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

use super::region::*;
use super::root::*;
use crate::ast::*;
use crate::data::*;
use fnv::FnvHashSet;
use std::cell::RefCell;

pub enum AnalysisError {
    Fatal(CircularDependencyError),
    NotFatal(Diagnostic),
}

impl AnalysisError {
    pub fn not_fatal_error(pos: impl AsRef<SrcPos>, msg: impl Into<String>) -> AnalysisError {
        AnalysisError::NotFatal(Diagnostic::error(pos, msg))
    }
}

#[derive(Clone, Debug)]
#[must_use]
pub struct CircularDependencyError {
    reference: Option<SrcPos>,
}

impl CircularDependencyError {
    pub fn new(reference: Option<&SrcPos>) -> CircularDependencyError {
        CircularDependencyError {
            reference: reference.cloned(),
        }
    }

    pub fn push_into(self, diagnostics: &mut dyn DiagnosticHandler) {
        if let Some(pos) = self.reference {
            diagnostics.push(Diagnostic::error(pos, "Found circular dependency"));
        }
    }
}

pub type AnalysisResult<T> = Result<T, AnalysisError>;
pub type FatalResult<T> = Result<T, CircularDependencyError>;
pub type FatalNullResult = FatalResult<()>;

impl From<CircularDependencyError> for AnalysisError {
    fn from(err: CircularDependencyError) -> AnalysisError {
        AnalysisError::Fatal(err)
    }
}

impl From<Diagnostic> for AnalysisError {
    fn from(diagnostic: Diagnostic) -> AnalysisError {
        AnalysisError::NotFatal(diagnostic)
    }
}

impl AnalysisError {
    // Add Non-fatal error to diagnostics or return fatal error
    pub fn add_to(self, diagnostics: &mut dyn DiagnosticHandler) -> FatalNullResult {
        match self {
            AnalysisError::Fatal(err) => Err(err),
            AnalysisError::NotFatal(diag) => {
                diagnostics.push(diag);
                Ok(())
            }
        }
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
    pub fn new(root: &'a DesignRoot, current_unit: &UnitId) -> AnalyzeContext<'a> {
        AnalyzeContext {
            work_sym: root.symbol_utf8("work"),
            std_sym: root.symbol_utf8("std"),
            standard_sym: root.symbol_utf8("standard"),
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
        self.root.symbol_utf8(name)
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
        let units = self.root.get_library_units(library_name).unwrap();

        for unit in units.values() {
            match unit.kind() {
                AnyKind::Primary(..) => {
                    let data = self.get_analysis(Some(use_pos), unit)?;
                    region.make_potentially_visible(
                        unit.name().into(),
                        Some(use_pos),
                        Self::create_primary_unit_decl(unit.unit_id(), &data),
                    );
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
        // work is not visible in context declarations
        if self.current_unit.kind() != AnyKind::Primary(PrimaryKind::Context) {
            region.make_library_visible(&self.work_sym, None, self.work_library_name());
        }

        if self.is_standard_package() || !self.has_library(&self.std_sym) {
            // @TODO add warning for missing standard package
            return Ok(());
        };

        region.make_library_visible(&self.std_sym, None, &self.std_sym);

        let standard_pkg_data = self.expect_standard_package_analysis()?;
        region.make_all_potentially_visible(None, &standard_pkg_data.result().region);

        Ok(())
    }

    pub fn has_library(&self, library_name: &Symbol) -> bool {
        self.root.get_library_units(library_name).is_some()
    }

    fn get_package_body(&self) -> Option<&'a LockedUnit> {
        let units = self.root.get_library_units(self.work_library_name())?;

        let name = self.current_unit.primary_name();

        if let Some(ref unit) = units.get(&UnitKey::Secondary(name.clone(), name.clone())) {
            if unit.kind() == AnyKind::Secondary(SecondaryKind::PackageBody) {
                Some(unit)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn has_package_body(&self) -> bool {
        self.get_package_body().is_some()
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

    fn create_primary_unit_decl(unit_id: &UnitId, unit: &UnitReadGuard) -> NamedEntity {
        // @TODO add PrimaryUnit Declaration struct

        let unit_id = unit_id.clone();
        let region = unit.result().region.clone();

        let primary_unit = if let Some(primary_unit) = unit.as_primary() {
            primary_unit
        } else {
            unreachable!("Expect primary unit");
        };

        let kind = match primary_unit {
            AnyPrimaryUnit::Entity(..) => NamedEntityKind::Entity(unit_id, region),
            AnyPrimaryUnit::Configuration(..) => NamedEntityKind::Configuration(unit_id, region),
            AnyPrimaryUnit::Package(ref package) => {
                if package.generic_clause.is_some() {
                    NamedEntityKind::UninstPackage(unit_id, region)
                } else {
                    NamedEntityKind::Package(unit_id, region)
                }
            }
            AnyPrimaryUnit::PackageInstance(..) => {
                NamedEntityKind::PackageInstance(unit_id, region)
            }
            AnyPrimaryUnit::Context(..) => NamedEntityKind::Context(unit_id, region),
        };

        NamedEntity::new(kind, Some(unit.pos()))
    }

    fn get_primary_unit(&self, library_name: &Symbol, name: &Symbol) -> Option<&'a LockedUnit> {
        let units = self.root.get_library_units(library_name)?;
        // @TODO missing library

        if let Some(ref unit) = units.get(&UnitKey::Primary(name.clone())) {
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
    ) -> AnalysisResult<NamedEntity> {
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
