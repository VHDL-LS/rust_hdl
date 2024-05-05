// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

use super::root::*;
pub(crate) use super::scope::Scope;
use crate::ast::*;
use crate::data::error_codes::ErrorCode;
use crate::data::*;
use crate::named_entity::*;
use crate::syntax::TokenAccess;
use fnv::FnvHashSet;
use std::cell::RefCell;
use std::ops::Deref;

/// Indicates that a circular dependency is found at the position denoted by `reference`.
///
/// A circular dependency occurs when module A uses module B, which in turn
/// (either directly or indirectly via other modules) uses module A again.
///
/// ## Example
///
/// ```vhdl
/// use work.bar;
///
/// package foo is
/// end package;
///
/// use work.foo;
///
/// package bar is
/// end package;
/// ```
/// In this example, the package `bar` uses the package `foo` which in turn uses package `bar` –
/// making the dependency chain cyclic.
///
/// Commonly, two or more `CircularDependencyError`s are pushed to indicate what modules
/// the error affects.
#[derive(Clone, Debug, PartialEq, Eq)]
#[must_use]
pub struct CircularDependencyError {
    /// The position where the circular dependency was found.
    /// Is `None` when the circular dependency is found in the standard library.
    /// This should, in practice, never happen.
    reference: Option<SrcPos>,
}

impl CircularDependencyError {
    pub fn new(reference: Option<&SrcPos>) -> CircularDependencyError {
        CircularDependencyError {
            reference: reference.cloned(),
        }
    }

    /// Pushes this error into a diagnostic handler.
    pub fn push_into(self, diagnostics: &mut dyn DiagnosticHandler) {
        if let Some(pos) = self.reference {
            diagnostics.push(Diagnostic::circular_dependency(pos));
        }
    }
}

/// A `FatalResult` is a result that is either OK or contains a `CircularDependencyError`.
/// If this type contains the error case, most other errors encountered during analysis are ignored
/// as analysis cannot continue (resp. no further analysis is pursued)
pub type FatalResult<T = ()> = Result<T, CircularDependencyError>;

#[derive(Debug, PartialEq, Eq)]
pub enum EvalError {
    /// A circular dependency was found, see [CircularDependencyError](CircularDependencyError)
    Circular(CircularDependencyError),
    /// Indicates that evaluation is no longer possible, for example if encountering illegal code.
    /// Typically, functions returning Unknown will have published diagnostics on the side-channel
    /// and the unknown is returned to stop further upstream analysis
    Unknown,
}

impl From<CircularDependencyError> for EvalError {
    fn from(err: CircularDependencyError) -> EvalError {
        EvalError::Circular(err)
    }
}

/// The result of the evaluation of an AST element.
/// The result has either a value of `Ok(T)`, indicating a successful evaluation of
/// the AST and returning the result of that evaluation, or `Err(EvalError)`, indicating
/// an error during evaluation.
///
/// Most of the time, the error will be `EvalError::Unknown`. This means that the evaluation
/// step has pushed found problems in the code to some side-channel and simply returns an error,
/// signifying that some problem was found without further specifying that problem.
pub type EvalResult<T = ()> = Result<T, EvalError>;

/// Pushes the diagnostic to the provided handler and returns
/// with an `EvalError::Unknown` result.
///
/// This macro can be used for the common case of encountering an analysis error and
/// immediately returning as the error is not recoverable.
macro_rules! bail {
    ($diagnostics:expr, $error:expr) => {
        $diagnostics.push($error);
        return Err(EvalError::Unknown);
    };
}

pub trait IntoEvalResult<T> {
    /// Transforms `Self` into an `EvalResult`.
    ///
    /// If `Self` has any severe errors, these should be pushed to the provided handler
    /// and an `Err(EvalError::Unknown)` should be returned.
    fn into_eval_result(self, diagnostics: &mut dyn DiagnosticHandler) -> EvalResult<T>;
}

impl<T> IntoEvalResult<T> for Result<T, Diagnostic> {
    fn into_eval_result(self, diagnostics: &mut dyn DiagnosticHandler) -> EvalResult<T> {
        match self {
            Ok(value) => Ok(value),
            Err(diagnostic) => {
                bail!(diagnostics, diagnostic);
            }
        }
    }
}

pub fn as_fatal<T>(res: EvalResult<T>) -> FatalResult<Option<T>> {
    match res {
        Ok(val) => Ok(Some(val)),
        Err(EvalError::Unknown) => Ok(None),
        Err(EvalError::Circular(circ)) => Err(circ),
    }
}

pub(super) struct AnalyzeContext<'a, 't> {
    pub(super) root: &'a DesignRoot,

    pub work_sym: Symbol,
    std_sym: Symbol,
    standard_sym: Symbol,
    pub(super) is_std_logic_1164: bool,

    // Record dependencies and sensitives when
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
    pub(super) arena: &'a Arena,
    uses: RefCell<FnvHashSet<UnitId>>,
    missing_unit: RefCell<FnvHashSet<(Symbol, Symbol, Option<Symbol>)>>,
    uses_library_all: RefCell<FnvHashSet<Symbol>>,
    pub ctx: &'t dyn TokenAccess,
}

impl<'a, 't> AnalyzeContext<'a, 't> {
    pub fn new(
        root: &'a DesignRoot,
        current_unit: &UnitId,
        arena: &'a Arena,
        ctx: &'t dyn TokenAccess,
    ) -> AnalyzeContext<'a, 't> {
        AnalyzeContext {
            work_sym: root.symbol_utf8("work"),
            std_sym: root.symbol_utf8("std"),
            standard_sym: root.symbol_utf8("standard"),
            is_std_logic_1164: current_unit
                == &UnitId::package(
                    &root.symbol_utf8("ieee"),
                    &root.symbol_utf8("std_logic_1164"),
                ),
            root,
            current_unit: current_unit.clone(),
            arena,
            uses: RefCell::new(FnvHashSet::default()),
            missing_unit: RefCell::new(FnvHashSet::default()),
            uses_library_all: RefCell::new(FnvHashSet::default()),
            ctx,
        }
    }

    pub fn work_library_name(&self) -> &Symbol {
        self.current_unit.library_name()
    }

    pub fn work_library(&self) -> EntRef<'a> {
        self.get_library(self.current_unit.library_name()).unwrap()
    }

    pub fn current_unit_id(&self) -> &UnitId {
        &self.current_unit
    }

    fn make_use_of(&self, use_pos: Option<&SrcPos>, unit_id: &UnitId) -> FatalResult {
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

    fn make_use_of_missing_unit(
        &self,
        library_name: &Symbol,
        primary_name: &Symbol,
        secondary_name: Option<&Symbol>,
    ) {
        let key = (
            library_name.clone(),
            primary_name.clone(),
            secondary_name.cloned(),
        );

        // Check local cache before taking lock
        if self.missing_unit.borrow_mut().insert(key) {
            self.root.make_use_of_missing_unit(
                &self.current_unit,
                library_name,
                primary_name,
                secondary_name,
            );
        }
    }

    pub fn use_all_in_library(
        &self,
        use_pos: &SrcPos,
        library_name: &Symbol,
        scope: &Scope<'a>,
    ) -> FatalResult {
        let units = self.root.get_library_units(library_name).unwrap();

        for unit in units.values() {
            match unit.kind() {
                AnyKind::Primary(..) => {
                    let data = self.get_analysis(Some(use_pos), unit)?;
                    if let AnyDesignUnit::Primary(primary) = data.deref() {
                        if let Some(id) = primary.ent_id() {
                            scope.make_potentially_visible(Some(use_pos), self.arena.get(id));
                        }
                    }
                }
                AnyKind::Secondary(..) => {}
            }
        }

        self.make_use_of_library_all(library_name);
        Ok(())
    }

    /// Add implicit context clause for all packages except STD.STANDARD
    /// library STD, WORK;
    /// use STD.STANDARD.all;
    pub fn add_implicit_context_clause(&self, scope: &Scope<'a>) -> FatalResult {
        // work is not visible in context declarations
        if self.current_unit.kind() != AnyKind::Primary(PrimaryKind::Context) {
            scope.make_potentially_visible_with_name(
                None,
                self.work_sym.clone().into(),
                self.work_library(),
            );
        }

        if let Some(std_library) = self.get_library(&self.std_sym) {
            scope.make_potentially_visible(None, std_library);

            let standard_region = self
                .standard_package_region()
                .expect("Expected standard package");
            scope.make_all_potentially_visible(None, standard_region);
        }

        Ok(())
    }

    pub fn get_library(&self, library_name: &Symbol) -> Option<EntRef<'a>> {
        let (arena, id) = self.root.get_library_arena(library_name)?;
        self.arena.link(arena);
        Some(self.arena.get(id))
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

        // Add all referenced declaration arenas from other unit
        self.arena.link(&data.result().arena);

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
        if let Some(unit) = units.get(&UnitKey::Primary(name.clone())) {
            return Some(unit);
        }
        self.make_use_of_missing_unit(library_name, name, None);
        None
    }

    fn get_secondary_unit(
        &self,
        library_name: &Symbol,
        primary: &Symbol,
        name: &Symbol,
    ) -> Option<&'a LockedUnit> {
        let units = self.root.get_library_units(library_name)?;
        if let Some(unit) = units.get(&UnitKey::Secondary(primary.clone(), name.clone())) {
            return Some(unit);
        }
        self.make_use_of_missing_unit(library_name, primary, Some(name));
        None
    }

    /// Given an Entity, returns a reference to the architecture denoted by `architecture_name`.
    /// If this architecture cannot be found, pushes an appropriate error to the diagnostics-handler
    /// and returns `EvalError::Unknown`.
    ///
    /// # Arguments
    ///
    /// * `diagnostics` Reference to the diagnostic handler
    /// * `library_name` The name of the library where the entity resides
    /// * `pos` The position where the architecture name was declared
    /// * `entity_name` Name of the entity
    /// * `architecture_name` Name of the architecture to be resolved
    pub(super) fn get_architecture(
        &self,
        diagnostics: &mut dyn DiagnosticHandler,
        library_name: &Symbol,
        pos: &SrcPos,
        entity_name: &Symbol,
        architecture_name: &Symbol,
    ) -> EvalResult<DesignEnt<'a>> {
        if let Some(unit) = self.get_secondary_unit(library_name, entity_name, architecture_name) {
            let data = self.get_analysis(Some(pos), unit)?;
            if let AnyDesignUnit::Secondary(AnySecondaryUnit::Architecture(arch)) = data.deref() {
                if let Some(id) = arch.ident.decl.get() {
                    let ent = self.arena.get(id);
                    if let Some(design) = DesignEnt::from_any(ent) {
                        return Ok(design);
                    } else {
                        bail!(
                            diagnostics,
                            Diagnostic::internal(
                                pos,
                                format!(
                                    "Found non-design {} unit within library {}",
                                    ent.describe(),
                                    library_name
                                )
                            )
                        );
                    }
                }
            }
        }

        bail!(
            diagnostics,
            Diagnostic::new(
                pos,
                format!("No architecture '{architecture_name}' for entity '{library_name}.{entity_name}'"),
                ErrorCode::Unresolved,
            )
        );
    }

    pub fn lookup_in_library(
        &self,
        diagnostics: &mut dyn DiagnosticHandler,
        library_name: &Symbol,
        pos: &SrcPos,
        primary_name: &Designator,
    ) -> EvalResult<DesignEnt<'a>> {
        if let Designator::Identifier(ref primary_name) = primary_name {
            if let Some(unit) = self.get_primary_unit(library_name, primary_name) {
                let data = self.get_analysis(Some(pos), unit)?;
                if let AnyDesignUnit::Primary(primary) = data.deref() {
                    if let Some(id) = primary.ent_id() {
                        let ent = self.arena.get(id);
                        if let Some(design) = DesignEnt::from_any(ent) {
                            return Ok(design);
                        } else {
                            bail!(
                                diagnostics,
                                Diagnostic::internal(
                                    pos,
                                    format!(
                                        "Found non-design {} unit within library {}",
                                        ent.describe(),
                                        library_name
                                    ),
                                )
                            );
                        }
                    }
                }
            }
        }

        bail!(
            diagnostics,
            Diagnostic::new(
                pos,
                format!("No primary unit '{primary_name}' within library '{library_name}'"),
                ErrorCode::Unresolved,
            )
        );
    }

    // Returns None when analyzing the standard package itself
    fn standard_package_region(&self) -> Option<&'a Region<'a>> {
        if let Some(pkg) = self.root.standard_pkg_id.as_ref() {
            self.arena.link(self.root.standard_arena.as_ref().unwrap());

            // Ensure things that depend on the standard package are re-analyzed
            self.make_use_of(None, &UnitId::package(&self.std_sym, &self.standard_sym))
                .unwrap();

            if let AnyEntKind::Design(Design::Package(_, region)) = self.arena.get(*pkg).kind() {
                Some(region)
            } else {
                unreachable!("Standard package is not a package");
            }
        } else {
            None
        }
    }
}
