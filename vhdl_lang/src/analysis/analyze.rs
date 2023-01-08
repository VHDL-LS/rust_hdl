// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

use super::named_entity::HasNamedEntity;
use super::region::*;
use super::root::*;
use super::standard::StandardRegion;
use crate::ast::*;
use crate::data::*;
use fnv::FnvHashSet;
use std::cell::RefCell;
use std::ops::Deref;
use std::sync::Arc;

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
pub type FatalResult<T = ()> = Result<T, CircularDependencyError>;
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
        let diag = self.into_non_fatal()?;
        diagnostics.push(diag);
        Ok(())
    }

    pub fn into_non_fatal(self) -> FatalResult<Diagnostic> {
        match self {
            AnalysisError::Fatal(err) => Err(err),
            AnalysisError::NotFatal(diag) => Ok(diag),
        }
    }
}

pub(super) struct AnalyzeContext<'a> {
    pub(super) root: &'a DesignRoot,

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

    pub fn work_library(&self) -> Arc<NamedEntity> {
        self.get_library(self.current_unit.library_name()).unwrap()
    }

    pub fn current_unit_id(&self) -> &UnitId {
        &self.current_unit
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
        scope: &mut Scope<'_>,
    ) -> FatalNullResult {
        let units = self.root.get_library_units(library_name).unwrap();

        for unit in units.values() {
            match unit.kind() {
                AnyKind::Primary(..) => {
                    let data = self.get_analysis(Some(use_pos), unit)?;
                    if let AnyDesignUnit::Primary(primary) = data.deref() {
                        if let Some(ent) = primary.named_entity() {
                            scope.make_potentially_visible(Some(use_pos), ent.clone());
                        }
                    }
                }
                AnyKind::Secondary(..) => {}
            }
        }

        self.make_use_of_library_all(library_name);
        Ok(())
    }

    pub fn is_standard_package(&self) -> bool {
        *self.work_library_name() == self.std_sym
            && *self.current_unit.primary_name() == self.standard_sym
    }

    /// Add implicit context clause for all packages except STD.STANDARD
    /// library STD, WORK;
    /// use STD.STANDARD.all;
    pub fn add_implicit_context_clause(&self, scope: &mut Scope<'_>) -> FatalNullResult {
        // work is not visible in context declarations
        if self.current_unit.kind() != AnyKind::Primary(PrimaryKind::Context) {
            scope.make_potentially_visible_with_name(
                None,
                self.work_sym.clone().into(),
                self.work_library(),
            );
        }

        if self.is_standard_package() {
            return Ok(());
        };

        if let Some(std_library) = self.get_library(&self.std_sym) {
            scope.make_potentially_visible(None, std_library);

            let standard_region = self
                .standard_package_region()
                .expect("Expected standard package");
            scope.make_all_potentially_visible(None, standard_region);
        }

        Ok(())
    }

    pub fn get_library(&self, library_name: &Symbol) -> Option<Arc<NamedEntity>> {
        self.root.get_library_ent(library_name).cloned()
    }

    fn get_package_body(&self) -> Option<&'a LockedUnit> {
        let units = self.root.get_library_units(self.work_library_name())?;

        let name = self.current_unit.primary_name();
        units
            .get(&UnitKey::Secondary(name.clone(), name.clone()))
            .filter(|&unit| unit.kind() == AnyKind::Secondary(SecondaryKind::PackageBody))
    }

    pub fn has_package_body(&self) -> bool {
        self.get_package_body().is_some()
    }

    fn get_analysis(
        &self,
        use_pos: Option<&SrcPos>,
        unit: &'a LockedUnit,
    ) -> FatalResult<UnitReadGuard<'a>> {
        self.make_use_of(use_pos, unit.unit_id())?;
        let data = self.root.get_analysis(unit);

        // Change circular dependency reference when used by another unit during analysis
        // The error is changed from within the used unit into the position of the use of the unit
        if data.result().has_circular_dependency {
            Err(CircularDependencyError::new(use_pos))
        } else {
            Ok(data)
        }
    }

    fn get_primary_unit(&self, library_name: &Symbol, name: &Symbol) -> Option<&'a LockedUnit> {
        let units = self.root.get_library_units(library_name)?;
        // @TODO missing library

        if let Some(unit) = units.get(&UnitKey::Primary(name.clone())) {
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
    ) -> AnalysisResult<Arc<NamedEntity>> {
        if let Designator::Identifier(ref primary_name) = primary_name {
            if let Some(unit) = self.get_primary_unit(library_name, primary_name) {
                let data = self.get_analysis(Some(pos), unit)?;
                if let AnyDesignUnit::Primary(primary) = data.deref() {
                    if let Some(ent) = primary.named_entity() {
                        return Ok(ent.clone());
                    }
                }
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

    // Returns None when analyzing the standard package itsel
    fn standard_package_region(&self) -> Option<&Arc<Region>> {
        if let Some(pkg) = self.root.standard_pkg.as_ref() {
            // Ensure things that depend on the standard package are re-analyzed
            self.make_use_of(None, &UnitId::package(&self.std_sym, &self.standard_sym))
                .unwrap();

            if let NamedEntityKind::Package(region) = pkg.kind() {
                Some(region)
            } else {
                unreachable!("Standard package is not a package");
            }
        } else {
            None
        }
    }

    pub fn standard_package(&'a self) -> Option<StandardRegion<'a>> {
        let region = self.standard_package_region()?;
        Some(StandardRegion::new(self.root, region.as_ref()))
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
