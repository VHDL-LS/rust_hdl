// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::declarative_region::{AnyDeclaration, DeclarativeRegion, VisibleDeclaration};
use super::library::*;
use super::lock::{CircularDependencyError, ReadGuard};
use crate::ast::*;
use crate::diagnostic::{Diagnostic, DiagnosticHandler};
use crate::latin_1::Latin1String;
use crate::source::{SrcPos, WithPos};
use crate::symbol_table::{Symbol, SymbolTable};
use std::sync::{Arc, RwLock};

enum AnalysisError {
    Fatal(CircularDependencyError),
    NotFatal(Diagnostic),
}

type AnalysisResult<T> = Result<T, AnalysisError>;
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
    fn add_to(self, diagnostics: &mut dyn DiagnosticHandler) -> FatalNullResult {
        match self {
            AnalysisError::Fatal(err) => Err(err),
            AnalysisError::NotFatal(diag) => {
                diagnostics.push(diag);
                Ok(())
            }
        }
    }

    /// Return self to enforce #[must_use]
    fn add_circular_reference(self, location: &SrcPos) -> AnalysisError {
        if let AnalysisError::Fatal(err) = self {
            AnalysisError::Fatal(err.add_reference(location))
        } else {
            self
        }
    }
}

trait HandleFatal<T> {
    fn push_fatal_error(&mut self, error: FatalResult<T>);
}

impl<'a, T> HandleFatal<T> for dyn DiagnosticHandler + 'a {
    fn push_fatal_error(&mut self, error: FatalResult<T>) {
        if let Err(err) = error {
            err.push_into(self);
        }
    }
}

enum LookupResult {
    /// A single name was selected
    Single(VisibleDeclaration),
    /// All names within was selected
    /// @TODO add pos for where declaration was made visible into VisibleDeclaration
    AllWithin(SrcPos, VisibleDeclaration),
    /// The name to lookup (or some part thereof was not a selected name)
    NotSelected,
}

impl LookupResult {
    fn to_single_prefix(self, prefix_pos: &SrcPos) -> Result<VisibleDeclaration, Diagnostic> {
        match self {
            LookupResult::Single(decl) => Ok(decl),
            LookupResult::AllWithin(..) => Err(Diagnostic::error(
                prefix_pos,
                "'.all' may not be the prefix of a selected name",
            ))?,
            LookupResult::NotSelected => Err(Diagnostic::error(
                prefix_pos,
                "may not be the prefix of a selected name",
            ))?,
        }
    }
}
pub struct Analyzer<'a> {
    symtab: Arc<SymbolTable>,
    work_sym: Symbol,
    work_library_name: Symbol,
    std_sym: Symbol,
    standard_sym: Symbol,
    root: DependencyRecorder<'a>,
}

impl<'a> Analyzer<'a> {
    pub fn new(
        root: DependencyRecorder<'a>,
        work_library_name: Symbol,
        symtab: Arc<SymbolTable>,
    ) -> Analyzer<'a> {
        Analyzer {
            work_sym: symtab.insert(&Latin1String::new(b"work")),
            work_library_name,
            std_sym: symtab.insert(&Latin1String::new(b"std")),
            standard_sym: symtab.insert(&Latin1String::new(b"standard")),
            symtab,
            root,
        }
    }

    fn lookup_in_library(
        &self,
        library_name: &Symbol,
        pos: &SrcPos,
        designator: &Designator,
    ) -> AnalysisResult<VisibleDeclaration> {
        if let Some(decl) = self.root.lookup_in_library(library_name, designator) {
            Ok(decl.clone())
        } else {
            Err(Diagnostic::error(
                pos,
                format!(
                    "No primary unit '{}' within library '{}'",
                    designator, library_name
                ),
            ))?
        }
    }

    fn lookup_within(
        &self,
        prefix_pos: &SrcPos,
        prefix: &VisibleDeclaration,
        suffix: &WithPos<WithRef<Designator>>,
    ) -> AnalysisResult<Option<VisibleDeclaration>> {
        match prefix.first() {
            AnyDeclaration::Library(ref library_name) => Ok(Some(self.lookup_in_library(
                library_name,
                &suffix.pos,
                suffix.designator(),
            )?)),

            AnyDeclaration::UninstPackage(ref unit_id) => {
                Err(uninstantiated_package_prefix_error(prefix_pos, unit_id))?
            }

            AnyDeclaration::Package(ref unit_id) => {
                let package_data = self.root.expect_package_declaration_analysis(unit_id)?;

                if let Some(decl) = package_data.region.lookup(suffix.designator(), false) {
                    Ok(Some(decl.clone()))
                } else {
                    Err(Diagnostic::error(
                        suffix.as_ref(),
                        format!(
                            "No declaration of '{}' within package '{}.{}'",
                            suffix.item,
                            unit_id.library_name(),
                            unit_id.primary_name()
                        ),
                    ))?
                }
            }

            AnyDeclaration::PackageInstance(ref unit_id) => {
                let instance_data = self.root.expect_package_instance_analysis(unit_id)?;
                if let Some(decl) = instance_data.region.lookup(suffix.designator(), false) {
                    Ok(Some(decl.clone()))
                } else {
                    Err(Diagnostic::error(
                        suffix.as_ref(),
                        format!(
                            "No declaration of '{}' within package instance '{}.{}'",
                            suffix.item,
                            unit_id.library_name(),
                            unit_id.primary_name()
                        ),
                    ))?
                }
            }

            AnyDeclaration::LocalPackageInstance(ref instance_name, ref instance_region) => {
                if let Some(decl) = instance_region.lookup(suffix.designator(), false) {
                    Ok(Some(decl.clone()))
                } else {
                    Err(Diagnostic::error(
                        suffix.as_ref(),
                        format!(
                            "No declaration of '{}' within package instance '{}'",
                            suffix.item, &instance_name
                        ),
                    ))?
                }
            }
            _ => Ok(None),
        }
    }

    fn resolve_selected_name(
        &self,
        region: &DeclarativeRegion<'_>,
        name: &mut WithPos<SelectedName>,
    ) -> AnalysisResult<VisibleDeclaration> {
        match name.item {
            SelectedName::Selected(ref mut prefix, ref mut suffix) => {
                suffix.clear_reference();
                let prefix_decl = self.resolve_selected_name(region, prefix)?;
                match self.lookup_within(&prefix.pos, &prefix_decl, suffix) {
                    Ok(Some(decl)) => {
                        suffix.set_reference(&decl);
                        Ok(decl)
                    }
                    Ok(None) => Err(Diagnostic::error(
                        &prefix.pos,
                        "Invalid prefix for selected name",
                    ))?,
                    Err(err) => Err(err.add_circular_reference(&name.pos)),
                }
            }
            SelectedName::Designator(ref mut designator) => {
                if let Some(decl) = region.lookup(designator.designator(), true) {
                    designator.set_reference(decl);
                    Ok(decl.clone())
                } else {
                    designator.clear_reference();
                    Err(Diagnostic::error(
                        &name.pos,
                        format!("No declaration of '{}'", designator),
                    ))?
                }
            }
        }
    }

    fn resolve_prefix(
        &self,
        region: &DeclarativeRegion<'_>,
        prefix_pos: &SrcPos,
        prefix: &mut Name,
        // Allow the resolution to stop when encountering a non selected prefix
        // which cannot be analyzed further without type information
        allow_incomplete: bool,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> AnalysisResult<Option<VisibleDeclaration>> {
        let resolved =
            self.resolve_name_pos(region, prefix_pos, prefix, allow_incomplete, diagnostics)?;

        match resolved.to_single_prefix(prefix_pos) {
            Ok(decl) => Ok(Some(decl)),
            Err(err) => {
                if allow_incomplete {
                    Ok(None)
                } else {
                    Err(AnalysisError::NotFatal(err))
                }
            }
        }
    }

    fn resolve_name_pos(
        &self,
        region: &DeclarativeRegion<'_>,
        name_pos: &SrcPos,
        name: &mut Name,
        // Allow the resolution to stop when encountering a non selected prefix
        // which cannot be analyzed further without type information
        allow_incomplete: bool,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> AnalysisResult<LookupResult> {
        match name {
            Name::Selected(prefix, suffix) => {
                suffix.clear_reference();

                match self.resolve_prefix(
                    region,
                    &prefix.pos,
                    &mut prefix.item,
                    allow_incomplete,
                    diagnostics,
                )? {
                    Some(decl) => match self.lookup_within(&prefix.pos, &decl, suffix) {
                        Ok(Some(decl)) => {
                            suffix.set_reference(&decl);
                            Ok(LookupResult::Single(decl))
                        }
                        Ok(None) => {
                            if allow_incomplete {
                                Ok(LookupResult::NotSelected)
                            } else {
                                Err(Diagnostic::error(
                                    &prefix.pos,
                                    "Invalid prefix for selected name",
                                ))?
                            }
                        }
                        Err(err) => Err(err.add_circular_reference(name_pos)),
                    },
                    None => Ok(LookupResult::NotSelected),
                }
            }

            Name::SelectedAll(prefix) => {
                match self.resolve_prefix(
                    region,
                    &prefix.pos,
                    &mut prefix.item,
                    allow_incomplete,
                    diagnostics,
                )? {
                    Some(decl) => Ok(LookupResult::AllWithin(prefix.pos.clone(), decl)),
                    None => Ok(LookupResult::NotSelected),
                }
            }
            Name::Designator(designator) => {
                if let Some(decl) = region.lookup(designator.designator(), true) {
                    designator.set_reference(decl);
                    Ok(LookupResult::Single(decl.clone()))
                } else {
                    designator.clear_reference();
                    Err(Diagnostic::error(
                        name_pos,
                        format!("No declaration of '{}'", designator),
                    ))?
                }
            }
            // @TODO more
            Name::Indexed(ref mut prefix, ..) => {
                self.resolve_name_pos(
                    region,
                    &prefix.pos,
                    &mut prefix.item,
                    allow_incomplete,
                    diagnostics,
                )?;
                Ok(LookupResult::NotSelected)
            }

            Name::Slice(ref mut prefix, ref mut drange) => {
                self.resolve_name_pos(
                    region,
                    &prefix.pos,
                    &mut prefix.item,
                    allow_incomplete,
                    diagnostics,
                )?;
                self.analyze_discrete_range(region, drange.as_mut(), diagnostics)?;
                Ok(LookupResult::NotSelected)
            }
            Name::Attribute(ref mut attr) => {
                // @TODO more
                let AttributeName { name, expr, .. } = attr.as_mut();
                self.resolve_name_pos(
                    region,
                    &name.pos,
                    &mut name.item,
                    allow_incomplete,
                    diagnostics,
                )?;
                if let Some(ref mut expr) = expr {
                    self.analyze_expression(region, expr, diagnostics)?;
                }
                Ok(LookupResult::NotSelected)
            }
            Name::FunctionCall(ref mut fcall) => {
                self.analyze_function_call(region, fcall, diagnostics)?;
                Ok(LookupResult::NotSelected)
            }
            Name::External(ref mut ename) => {
                let ExternalName { subtype, .. } = ename.as_mut();
                self.analyze_subtype_indication(region, subtype, diagnostics)?;
                Ok(LookupResult::NotSelected)
            }
        }
    }

    fn resolve_context_item_name(
        &self,
        region: &DeclarativeRegion<'_>,
        name: &mut WithPos<Name>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> AnalysisResult<LookupResult> {
        self.resolve_name_pos(region, &name.pos, &mut name.item, false, diagnostics)
    }

    fn analyze_interface_declaration(
        &self,
        region: &mut DeclarativeRegion<'_>,
        decl: &mut InterfaceDeclaration,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        match decl {
            InterfaceDeclaration::File(ref mut file_decl) => {
                self.analyze_subtype_indication(
                    region,
                    &mut file_decl.subtype_indication,
                    diagnostics,
                )?;
                region.add(&file_decl.ident, AnyDeclaration::Other, diagnostics);
            }
            InterfaceDeclaration::Object(ref mut object_decl) => {
                self.analyze_subtype_indication(
                    region,
                    &mut object_decl.subtype_indication,
                    diagnostics,
                )?;
                if let Some(ref mut expression) = object_decl.expression {
                    self.analyze_expression(region, expression, diagnostics)?
                }
                region.add(&object_decl.ident, AnyDeclaration::Other, diagnostics);
            }
            InterfaceDeclaration::Type(ref ident) => {
                region.add(ident, AnyDeclaration::Other, diagnostics);
            }
            InterfaceDeclaration::Subprogram(ref mut subpgm, ..) => {
                self.analyze_subprogram_declaration(region, subpgm, diagnostics)?;
                region.add(subpgm.designator(), AnyDeclaration::Overloaded, diagnostics);
            }
            InterfaceDeclaration::Package(ref mut instance) => {
                match self.analyze_package_instance_name(region, &mut instance.package_name) {
                    Ok(package) => region.add(
                        &instance.ident,
                        AnyDeclaration::LocalPackageInstance(
                            instance.ident.item.clone(),
                            Arc::new(package.region.clone()),
                        ),
                        diagnostics,
                    ),
                    Err(err) => {
                        let err = err.add_circular_reference(&instance.package_name.pos);
                        err.add_to(diagnostics)?;
                    }
                }
            }
        };
        Ok(())
    }

    fn analyze_interface_list(
        &self,
        region: &mut DeclarativeRegion<'_>,
        declarations: &mut [InterfaceDeclaration],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        for decl in declarations.iter_mut() {
            self.analyze_interface_declaration(region, decl, diagnostics)?;
        }
        Ok(())
    }

    fn resolve_type_mark(
        &self,
        region: &DeclarativeRegion<'_>,
        type_mark: &mut WithPos<SelectedName>,
    ) -> AnalysisResult<VisibleDeclaration> {
        self.resolve_selected_name(region, type_mark)
    }

    fn resolve_name(
        &self,
        region: &DeclarativeRegion<'_>,
        pos: &SrcPos,
        name: &mut Name,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult<Option<VisibleDeclaration>> {
        match self.resolve_name_pos(region, pos, name, true, diagnostics) {
            Ok(LookupResult::Single(result)) => Ok(Some(result)),
            Ok(..) => Ok(None),
            Err(err) => {
                err.add_to(diagnostics)?;
                Ok(None)
            }
        }
    }

    fn analyze_attribute_name(
        &self,
        region: &DeclarativeRegion<'_>,
        attr: &mut AttributeName,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        // @TODO more
        let AttributeName { name, .. } = attr;
        self.resolve_name(region, &name.pos, &mut name.item, diagnostics)?;
        Ok(())
    }

    fn analyze_range(
        &self,
        region: &DeclarativeRegion<'_>,
        range: &mut Range,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        match range {
            Range::Range(ref mut constraint) => {
                self.analyze_expression(region, &mut constraint.left_expr, diagnostics)?;
                self.analyze_expression(region, &mut constraint.right_expr, diagnostics)?;
            }
            Range::Attribute(ref mut attr) => {
                self.analyze_attribute_name(region, attr, diagnostics)?
            }
        }
        Ok(())
    }

    fn analyze_discrete_range(
        &self,
        region: &DeclarativeRegion<'_>,
        drange: &mut DiscreteRange,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        match drange {
            DiscreteRange::Discrete(ref mut type_mark, ref mut range) => {
                if let Err(err) = self.resolve_type_mark(region, type_mark) {
                    err.add_to(diagnostics)?;
                }
                if let Some(ref mut range) = range {
                    self.analyze_range(region, range, diagnostics)?;
                }
            }
            DiscreteRange::Range(ref mut range) => {
                self.analyze_range(region, range, diagnostics)?;
            }
        }
        Ok(())
    }

    fn analyze_array_index(
        &self,
        region: &mut DeclarativeRegion<'_>,
        array_index: &mut ArrayIndex,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        match array_index {
            ArrayIndex::IndexSubtypeDefintion(ref mut type_mark) => {
                if let Err(err) = self.resolve_type_mark(region, type_mark) {
                    err.add_to(diagnostics)?;
                }
            }
            ArrayIndex::Discrete(ref mut drange) => {
                self.analyze_discrete_range(region, drange, diagnostics)?;
            }
        }
        Ok(())
    }
    fn analyze_element_constraint(
        &self,
        region: &DeclarativeRegion<'_>,
        constraint: &mut ElementConstraint,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        // @TODO more
        let ElementConstraint { constraint, .. } = constraint;
        self.analyze_subtype_constraint(region, &mut constraint.item, diagnostics)
    }

    fn analyze_subtype_constraint(
        &self,
        region: &DeclarativeRegion<'_>,
        constraint: &mut SubtypeConstraint,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        match constraint {
            SubtypeConstraint::Array(ref mut dranges, ref mut constraint) => {
                for drange in dranges.iter_mut() {
                    self.analyze_discrete_range(region, drange, diagnostics)?;
                }
                if let Some(constraint) = constraint {
                    self.analyze_subtype_constraint(region, &mut constraint.item, diagnostics)?;
                }
            }
            SubtypeConstraint::Range(ref mut range) => {
                self.analyze_range(region, range, diagnostics)?;
            }
            SubtypeConstraint::Record(ref mut constraints) => {
                for constraint in constraints.iter_mut() {
                    self.analyze_element_constraint(region, constraint, diagnostics)?;
                }
            }
        }
        Ok(())
    }

    fn analyze_subtype_indication(
        &self,
        region: &DeclarativeRegion<'_>,
        subtype_indication: &mut SubtypeIndication,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        // @TODO more
        let SubtypeIndication {
            type_mark,
            constraint,
            ..
        } = subtype_indication;

        if let Err(err) = self.resolve_type_mark(region, type_mark) {
            err.add_to(diagnostics)?
        }

        if let Some(constraint) = constraint {
            self.analyze_subtype_constraint(region, &mut constraint.item, diagnostics)?;
        }

        Ok(())
    }

    fn analyze_subprogram_declaration<'r>(
        &self,
        parent: &'r DeclarativeRegion<'_>,
        subprogram: &mut SubprogramDeclaration,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult<DeclarativeRegion<'r>> {
        let mut region = DeclarativeRegion::new_borrowed_parent(parent);

        match subprogram {
            SubprogramDeclaration::Function(fun) => {
                self.analyze_interface_list(&mut region, &mut fun.parameter_list, diagnostics)?;
                if let Err(err) = self.resolve_type_mark(&parent, &mut fun.return_type) {
                    err.add_to(diagnostics)?
                }
            }
            SubprogramDeclaration::Procedure(procedure) => {
                self.analyze_interface_list(
                    &mut region,
                    &mut procedure.parameter_list,
                    diagnostics,
                )?;
            }
        }
        region.close_both(diagnostics);
        Ok(region)
    }

    fn analyze_choices(
        &self,
        region: &DeclarativeRegion<'_>,
        choices: &mut Vec<Choice>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        for choice in choices.iter_mut() {
            match choice {
                Choice::Expression(ref mut expr) => {
                    self.analyze_expression(region, expr, diagnostics)?;
                }
                Choice::DiscreteRange(ref mut drange) => {
                    self.analyze_discrete_range(region, drange, diagnostics)?;
                }
                Choice::Others => {}
            }
        }
        Ok(())
    }

    fn analyze_expression(
        &self,
        region: &DeclarativeRegion<'_>,
        expr: &mut WithPos<Expression>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        self.analyze_expression_pos(region, &expr.pos, &mut expr.item, diagnostics)
    }

    fn analyze_waveform(
        &self,
        region: &DeclarativeRegion<'_>,
        wavf: &mut Waveform,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        match wavf {
            Waveform::Elements(ref mut elems) => {
                for elem in elems.iter_mut() {
                    let WaveformElement { value, after } = elem;
                    self.analyze_expression(region, value, diagnostics)?;
                    if let Some(expr) = after {
                        self.analyze_expression(region, expr, diagnostics)?;
                    }
                }
            }
            Waveform::Unaffected => {}
        }
        Ok(())
    }

    fn analyze_assoc_elems(
        &self,
        region: &DeclarativeRegion<'_>,
        elems: &mut Vec<AssociationElement>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        for AssociationElement { actual, .. } in elems.iter_mut() {
            match actual.item {
                ActualPart::Expression(ref mut expr) => {
                    self.analyze_expression_pos(region, &actual.pos, expr, diagnostics)?;
                }
                ActualPart::Open => {}
            }
        }
        Ok(())
    }

    fn analyze_function_call(
        &self,
        region: &DeclarativeRegion<'_>,
        fcall: &mut FunctionCall,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        let FunctionCall { name, parameters } = fcall;
        if let Err(err) =
            self.resolve_name_pos(region, &name.pos, &mut name.item, true, diagnostics)
        {
            err.add_to(diagnostics)?;
        }
        self.analyze_assoc_elems(region, parameters, diagnostics)
    }

    fn analyze_aggregate(
        &self,
        region: &DeclarativeRegion<'_>,
        assocs: &mut Vec<ElementAssociation>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        for assoc in assocs.iter_mut() {
            match assoc {
                ElementAssociation::Named(ref mut choices, ref mut expr) => {
                    for choice in choices.iter_mut() {
                        match choice {
                            Choice::Expression(..) => {
                                // @TODO could be record field so we cannot do more now
                            }
                            Choice::DiscreteRange(ref mut drange) => {
                                self.analyze_discrete_range(region, drange, diagnostics)?;
                            }
                            Choice::Others => {}
                        }
                    }
                    self.analyze_expression(region, expr, diagnostics)?;
                }
                ElementAssociation::Positional(ref mut expr) => {
                    self.analyze_expression(region, expr, diagnostics)?;
                }
            }
        }
        Ok(())
    }

    fn analyze_qualified_expression(
        &self,
        region: &DeclarativeRegion<'_>,
        qexpr: &mut QualifiedExpression,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        let QualifiedExpression { name, expr } = qexpr;
        self.resolve_name(region, &name.pos, &mut name.item, diagnostics)?;
        self.analyze_expression(region, expr, diagnostics)?;
        Ok(())
    }

    fn analyze_expression_pos(
        &self,
        region: &DeclarativeRegion<'_>,
        pos: &SrcPos,
        expr: &mut Expression,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        match expr {
            Expression::Binary(_, ref mut left, ref mut right) => {
                self.analyze_expression(region, left, diagnostics)?;
                self.analyze_expression(region, right, diagnostics)
            }
            Expression::Unary(_, ref mut inner) => {
                self.analyze_expression(region, inner, diagnostics)
            }
            Expression::Name(ref mut name) => {
                self.resolve_name(region, pos, name, diagnostics)?;
                Ok(())
            }
            Expression::Aggregate(ref mut assocs) => {
                self.analyze_aggregate(region, assocs, diagnostics)
            }
            Expression::Qualified(ref mut qexpr) => {
                self.analyze_qualified_expression(region, qexpr, diagnostics)
            }

            Expression::New(ref mut alloc) => match alloc.item {
                Allocator::Qualified(ref mut qexpr) => {
                    self.analyze_qualified_expression(region, qexpr, diagnostics)
                }
                Allocator::Subtype(ref mut subtype) => {
                    self.analyze_subtype_indication(region, subtype, diagnostics)
                }
            },
            Expression::Literal(_) => Ok(()),
        }
    }

    fn analyze_signature(
        &self,
        region: &mut DeclarativeRegion<'_>,
        signature: &mut Signature,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        match signature {
            Signature::Function(ref mut args, ref mut ret) => {
                for arg in args.iter_mut() {
                    if let Err(err) = self.resolve_selected_name(region, arg) {
                        err.add_to(diagnostics)?;
                    }
                }
                if let Err(err) = self.resolve_selected_name(region, ret) {
                    err.add_to(diagnostics)?;
                }
            }
            Signature::Procedure(args) => {
                for arg in args.iter_mut() {
                    if let Err(err) = self.resolve_selected_name(region, arg) {
                        err.add_to(diagnostics)?;
                    }
                }
            }
        }
        Ok(())
    }

    fn analyze_declaration(
        &self,
        region: &mut DeclarativeRegion<'_>,
        decl: &mut Declaration,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        match decl {
            Declaration::Alias(alias) => {
                let AliasDeclaration {
                    designator,
                    name,
                    subtype_indication,
                    signature,
                } = alias;

                let resolved_name =
                    self.resolve_name(region, &name.pos, &mut name.item, diagnostics)?;

                if let Some(ref mut subtype_indication) = subtype_indication {
                    // Object alias
                    self.analyze_subtype_indication(region, subtype_indication, diagnostics)?;
                }

                if let Some(ref mut signature) = signature {
                    // Subprogram or enum literal alias
                    self.analyze_signature(region, signature, diagnostics)?;
                }

                let decl = {
                    if signature.is_some() {
                        AnyDeclaration::Overloaded
                    } else if subtype_indication.is_some() {
                        AnyDeclaration::Other
                    } else if let Some(decl) = resolved_name {
                        AnyDeclaration::AliasOf(Box::new(decl.first().clone()))
                    } else {
                        AnyDeclaration::Other
                    }
                };

                region.add(designator.clone(), decl, diagnostics);
            }
            Declaration::Object(ref mut object_decl) => {
                self.analyze_subtype_indication(
                    region,
                    &mut object_decl.subtype_indication,
                    diagnostics,
                )?;
                if let Some(ref mut expr) = object_decl.expression {
                    self.analyze_expression(region, expr, diagnostics)?;
                }
                region.add(
                    &object_decl.ident,
                    AnyDeclaration::from_object_declaration(object_decl),
                    diagnostics,
                );
            }
            Declaration::File(ref mut file) => {
                let FileDeclaration {
                    ident,
                    subtype_indication,
                    open_info,
                    file_name,
                } = file;
                self.analyze_subtype_indication(region, subtype_indication, diagnostics)?;
                if let Some(ref mut expr) = open_info {
                    self.analyze_expression(region, expr, diagnostics)?;
                }
                if let Some(ref mut expr) = file_name {
                    self.analyze_expression(region, expr, diagnostics)?;
                }
                region.add(ident.clone(), AnyDeclaration::Other, diagnostics);
            }
            Declaration::Component(ref mut component) => {
                region.add(&component.ident, AnyDeclaration::Other, diagnostics);
                let mut region = DeclarativeRegion::new_borrowed_parent(region);

                self.analyze_interface_list(&mut region, &mut component.generic_list, diagnostics)?;
                self.analyze_interface_list(&mut region, &mut component.port_list, diagnostics)?;
                region.close_both(diagnostics);
            }
            Declaration::Attribute(ref mut attr) => match attr {
                Attribute::Declaration(ref mut attr_decl) => {
                    if let Err(err) = self.resolve_type_mark(region, &mut attr_decl.type_mark) {
                        err.add_to(diagnostics)?;
                    }
                    region.add(&attr_decl.ident, AnyDeclaration::Other, diagnostics);
                }
                // @TODO Ignored for now
                Attribute::Specification(..) => {}
            },
            Declaration::SubprogramBody(ref mut body) => {
                region.add(
                    body.specification.designator(),
                    AnyDeclaration::Overloaded,
                    diagnostics,
                );
                let mut spec_region = self.analyze_subprogram_declaration(
                    region,
                    &mut body.specification,
                    diagnostics,
                )?;
                self.analyze_declarative_part(
                    &mut spec_region,
                    &mut body.declarations,
                    diagnostics,
                )?;
                self.analyze_sequential_part(&mut spec_region, &mut body.statements, diagnostics)?;
            }
            Declaration::SubprogramDeclaration(ref mut subdecl) => {
                region.add(
                    subdecl.designator(),
                    AnyDeclaration::Overloaded,
                    diagnostics,
                );
                self.analyze_subprogram_declaration(region, subdecl, diagnostics)?;
            }

            Declaration::Use(ref mut use_clause) => {
                self.analyze_use_clause(
                    region,
                    &mut use_clause.item,
                    &use_clause.pos,
                    diagnostics,
                )?;
            }

            Declaration::Package(ref mut instance) => {
                match self.analyze_package_instance_name(region, &mut instance.package_name) {
                    Ok(data) => region.add(
                        &instance.ident,
                        AnyDeclaration::LocalPackageInstance(
                            instance.ident.item.clone(),
                            Arc::new(data.region.clone()),
                        ),
                        diagnostics,
                    ),
                    Err(err) => {
                        let err = err.add_circular_reference(&instance.package_name.pos);
                        err.add_to(diagnostics)?
                    }
                }
            }
            Declaration::Configuration(..) => {}
            Declaration::Type(ref mut type_decl) => {
                self.analyze_type_declaration(region, type_decl, diagnostics)?;
            }
        };

        Ok(())
    }

    fn analyze_type_declaration(
        &self,
        parent: &mut DeclarativeRegion<'_>,
        type_decl: &mut TypeDeclaration,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        match type_decl.def {
            TypeDefinition::Enumeration(ref enumeration) => {
                for literal in enumeration.iter() {
                    parent.add(
                        literal.clone().map_into(|lit| lit.into_designator()),
                        AnyDeclaration::Overloaded,
                        diagnostics,
                    )
                }

                let mut enum_region = DeclarativeRegion::default();
                let mut ignored = Vec::new();
                for literal in enumeration.iter() {
                    enum_region.add(
                        literal.clone().map_into(|lit| lit.into_designator()),
                        AnyDeclaration::Overloaded,
                        // Ignore diagnostics as they will be given above
                        &mut ignored,
                    )
                }

                parent.add(
                    &type_decl.ident,
                    AnyDeclaration::TypeDeclaration(Some(Arc::new(enum_region))),
                    diagnostics,
                );
            }
            TypeDefinition::ProtectedBody(ref mut body) => {
                match parent.lookup(&type_decl.ident.item.clone().into(), true) {
                    Some(decl) => {
                        match decl.first() {
                            AnyDeclaration::ProtectedType(extended_region) => {
                                let extended_lock = extended_region.read().unwrap();
                                let mut region = extended_lock.extend(Some(parent));
                                self.analyze_declarative_part(
                                    &mut region,
                                    &mut body.decl,
                                    diagnostics,
                                )?;
                            }
                            _ => {
                                // @TODO detect incorrect type here instead
                            }
                        }
                    }
                    None => {
                        // @TODO detect missing protected type here insted
                    }
                }
                parent.add(
                    &type_decl.ident,
                    AnyDeclaration::ProtectedTypeBody,
                    diagnostics,
                );
            }
            TypeDefinition::Protected(ref mut prot_decl) => {
                let empty_region = Arc::new(RwLock::new(DeclarativeRegion::default()));

                // Protected type name is visible inside its declarative region
                parent.add(
                    &type_decl.ident,
                    AnyDeclaration::ProtectedType(empty_region.clone()),
                    diagnostics,
                );

                let mut region = DeclarativeRegion::new_borrowed_parent(parent);
                for item in prot_decl.items.iter_mut() {
                    match item {
                        ProtectedTypeDeclarativeItem::Subprogram(ref mut subprogram) => {
                            region.add(
                                subprogram.designator(),
                                AnyDeclaration::Overloaded,
                                diagnostics,
                            );
                            self.analyze_subprogram_declaration(
                                &mut region,
                                subprogram,
                                diagnostics,
                            )?;
                        }
                    }
                }

                // Save region for later since body extends region of declaration
                *empty_region.write().unwrap() = region.without_parent();
            }
            TypeDefinition::Record(ref mut element_decls) => {
                let mut region = DeclarativeRegion::default();
                for elem_decl in element_decls.iter_mut() {
                    self.analyze_subtype_indication(parent, &mut elem_decl.subtype, diagnostics)?;
                    region.add(&elem_decl.ident, AnyDeclaration::Other, diagnostics);
                }
                region.close_both(diagnostics);

                parent.add(
                    &type_decl.ident,
                    AnyDeclaration::TypeDeclaration(None),
                    diagnostics,
                );
            }
            TypeDefinition::Access(ref mut subtype_indication) => {
                self.analyze_subtype_indication(parent, subtype_indication, diagnostics)?;

                parent.add(
                    &type_decl.ident,
                    AnyDeclaration::TypeDeclaration(None),
                    diagnostics,
                );
            }
            TypeDefinition::Array(ref mut array_indexes, ref mut subtype_indication) => {
                for index in array_indexes.iter_mut() {
                    self.analyze_array_index(parent, index, diagnostics)?;
                }
                self.analyze_subtype_indication(parent, subtype_indication, diagnostics)?;

                parent.add(
                    &type_decl.ident,
                    AnyDeclaration::TypeDeclaration(None),
                    diagnostics,
                );
            }
            TypeDefinition::Subtype(ref mut subtype_indication) => {
                self.analyze_subtype_indication(parent, subtype_indication, diagnostics)?;

                parent.add(
                    &type_decl.ident,
                    AnyDeclaration::TypeDeclaration(None),
                    diagnostics,
                );
            }
            TypeDefinition::Physical(ref mut physical) => {
                parent.add(
                    physical.primary_unit.clone(),
                    AnyDeclaration::Constant,
                    diagnostics,
                );
                for (secondary_unit_name, _) in physical.secondary_units.iter_mut() {
                    parent.add(
                        secondary_unit_name.clone(),
                        AnyDeclaration::Constant,
                        diagnostics,
                    )
                }

                parent.add(
                    &type_decl.ident,
                    AnyDeclaration::TypeDeclaration(None),
                    diagnostics,
                );
            }
            TypeDefinition::Incomplete => {
                parent.add(
                    &type_decl.ident,
                    AnyDeclaration::IncompleteType,
                    diagnostics,
                );
            }

            TypeDefinition::Integer(ref mut range) => {
                self.analyze_range(parent, range, diagnostics)?;
                parent.add(
                    &type_decl.ident,
                    AnyDeclaration::TypeDeclaration(None),
                    diagnostics,
                );
            }

            TypeDefinition::File(ref mut type_mark) => {
                if let Err(err) = self.resolve_type_mark(parent, type_mark) {
                    err.add_to(diagnostics)?;
                }

                let mut implicit = DeclarativeRegion::default();
                for name in ["file_open", "file_close", "endfile"].iter() {
                    implicit.add_implicit(
                        Designator::Identifier(
                            self.symtab.insert(&Latin1String::from_utf8(name).unwrap()),
                        ),
                        None,
                        AnyDeclaration::Overloaded,
                        diagnostics,
                    );
                }
                parent.add(
                    &type_decl.ident,
                    AnyDeclaration::TypeDeclaration(Some(Arc::new(implicit))),
                    diagnostics,
                );
            }
        }

        Ok(())
    }

    fn analyze_declarative_part(
        &self,
        region: &mut DeclarativeRegion<'_>,
        declarations: &mut [Declaration],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        for decl in declarations.iter_mut() {
            self.analyze_declaration(region, decl, diagnostics)?;
        }
        Ok(())
    }

    fn analyze_use_clause(
        &self,
        region: &mut DeclarativeRegion<'_>,
        use_clause: &mut UseClause,
        use_pos: &SrcPos,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        for name in use_clause.name_list.iter_mut() {
            match name.item {
                Name::Selected(..) => {}
                Name::SelectedAll(..) => {}
                _ => {
                    diagnostics.push(Diagnostic::error(
                        &use_pos,
                        "Use clause must be a selected name",
                    ));
                    continue;
                }
            }

            match self.resolve_context_item_name(&region, name, diagnostics) {
                Ok(LookupResult::Single(visible_decl)) => {
                    region.make_potentially_visible(visible_decl);
                }
                Ok(LookupResult::AllWithin(visibility_pos, visible_decl)) => {
                    match visible_decl.first() {
                        AnyDeclaration::Library(ref library_name) => {
                            self.root.use_all_in_library(library_name, region);
                        }
                        AnyDeclaration::UninstPackage(ref unit_id) => {
                            diagnostics.push(uninstantiated_package_prefix_error(
                                &visibility_pos,
                                unit_id,
                            ));
                        }
                        AnyDeclaration::Package(ref unit_id) => {
                            match self.root.expect_package_declaration_analysis(unit_id) {
                                Ok(data) => {
                                    region.make_all_potentially_visible(&data.region);
                                }
                                Err(err) => {
                                    return Err(err.add_reference(&name.pos));
                                }
                            }
                        }
                        AnyDeclaration::PackageInstance(ref unit_id) => {
                            match self.root.expect_package_instance_analysis(unit_id) {
                                Ok(data) => {
                                    region.make_all_potentially_visible(&data.region);
                                }
                                Err(err) => {
                                    return Err(err.add_reference(&name.pos));
                                }
                            }
                        }
                        AnyDeclaration::LocalPackageInstance(_, ref instance_region) => {
                            region.make_all_potentially_visible(&instance_region);
                        }
                        // @TODO handle others
                        _ => {}
                    }
                }
                Ok(LookupResult::NotSelected) => {
                    diagnostics.push(Diagnostic::error(
                        &use_pos,
                        "Use clause must be a selected name",
                    ));
                }
                Err(err) => {
                    err.add_to(diagnostics)?;
                }
            }
        }

        Ok(())
    }

    fn analyze_context_clause(
        &self,
        region: &mut DeclarativeRegion<'_>,
        context_clause: &mut [WithPos<ContextItem>],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        for context_item in context_clause.iter_mut() {
            match context_item.item {
                ContextItem::Library(LibraryClause { ref name_list }) => {
                    for library_name in name_list.iter() {
                        if self.work_sym == library_name.item {
                            diagnostics.push(Diagnostic::hint(
                                &library_name,
                                "Library clause not necessary for current working library",
                            ))
                        } else if self.root.has_library(&library_name.item) {
                            region.make_library_visible(
                                &library_name.item,
                                &library_name.item,
                                Some(library_name.pos().clone()),
                            );
                        } else {
                            diagnostics.push(Diagnostic::error(
                                &library_name,
                                format!("No such library '{}'", library_name.item),
                            ));
                        }
                    }
                }
                ContextItem::Use(ref mut use_clause) => {
                    self.analyze_use_clause(region, use_clause, &context_item.pos, diagnostics)?;
                }
                ContextItem::Context(ContextReference { ref mut name_list }) => {
                    for name in name_list.iter_mut() {
                        match name.item {
                            Name::Selected(..) => {}
                            _ => {
                                diagnostics.push(Diagnostic::error(
                                    &context_item.pos,
                                    "Context reference must be a selected name",
                                ));
                                continue;
                            }
                        }

                        match self.resolve_context_item_name(&region, name, diagnostics) {
                            Ok(LookupResult::Single(visible_decl)) => {
                                match visible_decl.first() {
                                    // OK
                                    AnyDeclaration::Context(ref unit_id) => {
                                        match self.root.expect_context_analysis(unit_id) {
                                            Ok(context) => {
                                                region.copy_visibility_from(&context.region);
                                            }
                                            Err(fatal_err) => {
                                                return Err(fatal_err.add_reference(&name.pos));
                                            }
                                        }
                                    }
                                    _ => {
                                        // @TODO maybe lookup should return the source position of the suffix
                                        if let Name::Selected(_, ref suffix) = name.item {
                                            diagnostics.push(Diagnostic::error(
                                                &suffix,
                                                format!(
                                                    "'{}' does not denote a context declaration",
                                                    suffix.designator()
                                                ),
                                            ));
                                        }
                                    }
                                }
                            }
                            Ok(LookupResult::AllWithin(..)) => {
                                // @TODO
                            }
                            Ok(LookupResult::NotSelected) => {
                                diagnostics.push(Diagnostic::error(
                                    &context_item.pos,
                                    "Context reference must be a selected name",
                                ));
                            }
                            Err(err) => {
                                err.add_to(diagnostics)?;
                            }
                        }
                    }
                }
            }
        }

        Ok(())
    }

    fn analyze_generate_body(
        &self,
        region: &mut DeclarativeRegion<'_>,
        body: &mut GenerateBody,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        let GenerateBody {
            alternative_label,
            decl,
            statements,
        } = body;
        if let Some(label) = alternative_label {
            region.add(label.clone(), AnyDeclaration::Constant, diagnostics);
        }
        if let Some(ref mut decl) = decl {
            self.analyze_declarative_part(region, decl, diagnostics)?;
        }
        self.analyze_concurrent_part(region, statements, diagnostics)?;

        Ok(())
    }

    fn analyze_instance(
        &self,
        parent: &DeclarativeRegion<'_>,
        instance: &mut InstantiationStatement,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        match instance.unit {
            // @TODO architecture
            InstantiatedUnit::Entity(ref mut entity_name, ..) => {
                if let Err(err) = self.resolve_selected_name(parent, entity_name) {
                    err.add_to(diagnostics)?;
                }
            }
            InstantiatedUnit::Component(ref mut component_name) => {
                if let Err(err) = self.resolve_selected_name(parent, component_name) {
                    err.add_to(diagnostics)?;
                }
            }
            InstantiatedUnit::Configuration(ref mut config_name) => {
                if let Err(err) = self.resolve_selected_name(parent, config_name) {
                    err.add_to(diagnostics)?;
                }
            }
        };

        self.analyze_assoc_elems(parent, &mut instance.generic_map, diagnostics)?;
        self.analyze_assoc_elems(parent, &mut instance.port_map, diagnostics)?;

        Ok(())
    }

    // @TODO maybe make generic function for expression/waveform.
    // wait until type checking to see if it makes sense
    fn analyze_expr_assignment(
        &self,
        region: &DeclarativeRegion<'_>,
        target: &mut WithPos<Target>,
        rhs: &mut AssignmentRightHand<WithPos<Expression>>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        match rhs {
            AssignmentRightHand::Simple(expr) => {
                self.analyze_target(region, target, diagnostics)?;
                self.analyze_expression(region, expr, diagnostics)?;
            }
            AssignmentRightHand::Conditional(conditionals) => {
                let Conditionals {
                    conditionals,
                    else_item,
                } = conditionals;
                self.analyze_target(region, target, diagnostics)?;
                for conditional in conditionals {
                    let Conditional { condition, item } = conditional;
                    self.analyze_expression(region, item, diagnostics)?;
                    self.analyze_expression(region, condition, diagnostics)?;
                }
                if let Some(expr) = else_item {
                    self.analyze_expression(region, expr, diagnostics)?;
                }
            }
            AssignmentRightHand::Selected(selection) => {
                let Selection {
                    expression,
                    alternatives,
                } = selection;
                self.analyze_expression(region, expression, diagnostics)?;
                // target is located after expression
                self.analyze_target(region, target, diagnostics)?;
                for Alternative { choices, item } in alternatives.iter_mut() {
                    self.analyze_expression(region, item, diagnostics)?;
                    self.analyze_choices(region, choices, diagnostics)?;
                }
            }
        }
        Ok(())
    }

    fn analyze_waveform_assignment(
        &self,
        region: &DeclarativeRegion<'_>,
        target: &mut WithPos<Target>,
        rhs: &mut AssignmentRightHand<Waveform>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        match rhs {
            AssignmentRightHand::Simple(wavf) => {
                self.analyze_target(region, target, diagnostics)?;
                self.analyze_waveform(region, wavf, diagnostics)?;
            }
            AssignmentRightHand::Conditional(conditionals) => {
                let Conditionals {
                    conditionals,
                    else_item,
                } = conditionals;
                self.analyze_target(region, target, diagnostics)?;
                for conditional in conditionals {
                    let Conditional { condition, item } = conditional;
                    self.analyze_waveform(region, item, diagnostics)?;
                    self.analyze_expression(region, condition, diagnostics)?;
                }
                if let Some(wavf) = else_item {
                    self.analyze_waveform(region, wavf, diagnostics)?;
                }
            }
            AssignmentRightHand::Selected(selection) => {
                let Selection {
                    expression,
                    alternatives,
                } = selection;
                self.analyze_expression(region, expression, diagnostics)?;
                // target is located after expression
                self.analyze_target(region, target, diagnostics)?;
                for Alternative { choices, item } in alternatives.iter_mut() {
                    self.analyze_waveform(region, item, diagnostics)?;
                    self.analyze_choices(region, choices, diagnostics)?;
                }
            }
        }
        Ok(())
    }

    fn analyze_concurrent_statement(
        &self,
        parent: &mut DeclarativeRegion<'_>,
        statement: &mut LabeledConcurrentStatement,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        if let Some(ref label) = statement.label {
            parent.add(label.clone(), AnyDeclaration::Constant, diagnostics);
        }

        match statement.statement {
            ConcurrentStatement::Block(ref mut block) => {
                let mut region = DeclarativeRegion::new_borrowed_parent(parent);
                self.analyze_declarative_part(&mut region, &mut block.decl, diagnostics)?;
                self.analyze_concurrent_part(&mut region, &mut block.statements, diagnostics)?;
            }
            ConcurrentStatement::Process(ref mut process) => {
                let ProcessStatement {
                    postponed: _,
                    sensitivity_list,
                    decl,
                    statements,
                } = process;
                if let Some(sensitivity_list) = sensitivity_list {
                    match sensitivity_list {
                        SensitivityList::Names(names) => {
                            for name in names.iter_mut() {
                                self.resolve_name(parent, &name.pos, &mut name.item, diagnostics)?;
                            }
                        }
                        SensitivityList::All => {}
                    }
                }
                let mut region = DeclarativeRegion::new_borrowed_parent(parent);
                self.analyze_declarative_part(&mut region, decl, diagnostics)?;
                self.analyze_sequential_part(&mut region, statements, diagnostics)?;
            }
            ConcurrentStatement::ForGenerate(ref mut gen) => {
                let ForGenerateStatement {
                    index_name,
                    discrete_range,
                    body,
                } = gen;
                self.analyze_discrete_range(parent, discrete_range, diagnostics)?;
                let mut region = DeclarativeRegion::new_borrowed_parent(parent);
                region.add(index_name.clone(), AnyDeclaration::Constant, diagnostics);
                self.analyze_generate_body(&mut region, body, diagnostics)?;
            }
            ConcurrentStatement::IfGenerate(ref mut gen) => {
                let Conditionals {
                    conditionals,
                    else_item,
                } = gen;
                for conditional in conditionals.iter_mut() {
                    let Conditional { condition, item } = conditional;
                    self.analyze_expression(parent, condition, diagnostics)?;
                    let mut region = DeclarativeRegion::new_borrowed_parent(parent);
                    self.analyze_generate_body(&mut region, item, diagnostics)?;
                }
                if let Some(ref mut else_item) = else_item {
                    let mut region = DeclarativeRegion::new_borrowed_parent(parent);
                    self.analyze_generate_body(&mut region, else_item, diagnostics)?;
                }
            }
            ConcurrentStatement::CaseGenerate(ref mut gen) => {
                for alternative in gen.alternatives.iter_mut() {
                    let mut region = DeclarativeRegion::new_borrowed_parent(parent);
                    self.analyze_generate_body(&mut region, &mut alternative.item, diagnostics)?;
                }
            }
            ConcurrentStatement::Instance(ref mut instance) => {
                self.analyze_instance(parent, instance, diagnostics)?;
            }
            ConcurrentStatement::Assignment(ref mut assign) => {
                // @TODO more delaymechanism
                let ConcurrentSignalAssignment { target, rhs, .. } = assign;
                self.analyze_waveform_assignment(parent, target, rhs, diagnostics)?;
            }
            ConcurrentStatement::ProcedureCall(ref mut pcall) => {
                let ConcurrentProcedureCall {
                    call,
                    postponed: _postponed,
                } = pcall;
                self.analyze_function_call(parent, call, diagnostics)?;
            }
            ConcurrentStatement::Assert(ref mut assert) => {
                let ConcurrentAssertStatement {
                    postponed: _postponed,
                    statement:
                        AssertStatement {
                            condition,
                            report,
                            severity,
                        },
                } = assert;
                self.analyze_expression(parent, condition, diagnostics)?;
                if let Some(expr) = report {
                    self.analyze_expression(parent, expr, diagnostics)?;
                }
                if let Some(expr) = severity {
                    self.analyze_expression(parent, expr, diagnostics)?;
                }
            }
        };
        Ok(())
    }

    fn analyze_concurrent_part(
        &self,
        parent: &mut DeclarativeRegion<'_>,
        statements: &mut [LabeledConcurrentStatement],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        for statement in statements.iter_mut() {
            self.analyze_concurrent_statement(parent, statement, diagnostics)?;
        }

        Ok(())
    }

    fn analyze_target(
        &self,
        parent: &DeclarativeRegion<'_>,
        target: &mut WithPos<Target>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        match target.item {
            Target::Name(ref mut name) => {
                self.resolve_name(parent, &target.pos, name, diagnostics)?;
            }
            Target::Aggregate(ref mut assocs) => {
                self.analyze_aggregate(parent, assocs, diagnostics)?;
            }
        }
        Ok(())
    }

    fn analyze_sequential_statement(
        &self,
        parent: &mut DeclarativeRegion<'_>,
        statement: &mut LabeledSequentialStatement,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        if let Some(ref label) = statement.label {
            parent.add(label.clone(), AnyDeclaration::Constant, diagnostics);
        }

        match statement.statement {
            SequentialStatement::Return(ref mut ret) => {
                let ReturnStatement { expression } = ret;
                if let Some(ref mut expression) = expression {
                    self.analyze_expression(parent, expression, diagnostics)?;
                }
            }
            SequentialStatement::Wait(ref mut wait_stmt) => {
                let WaitStatement {
                    sensitivity_clause,
                    condition_clause,
                    timeout_clause,
                } = wait_stmt;
                for name in sensitivity_clause.iter_mut() {
                    self.resolve_name(parent, &name.pos, &mut name.item, diagnostics)?;
                }
                if let Some(expr) = condition_clause {
                    self.analyze_expression(parent, expr, diagnostics)?;
                }
                if let Some(expr) = timeout_clause {
                    self.analyze_expression(parent, expr, diagnostics)?;
                }
            }
            SequentialStatement::Assert(ref mut assert_stmt) => {
                let AssertStatement {
                    condition,
                    report,
                    severity,
                } = assert_stmt;
                self.analyze_expression(parent, condition, diagnostics)?;
                if let Some(expr) = report {
                    self.analyze_expression(parent, expr, diagnostics)?;
                }
                if let Some(expr) = severity {
                    self.analyze_expression(parent, expr, diagnostics)?;
                }
            }
            SequentialStatement::Report(ref mut report_stmt) => {
                let ReportStatement { report, severity } = report_stmt;
                self.analyze_expression(parent, report, diagnostics)?;
                if let Some(expr) = severity {
                    self.analyze_expression(parent, expr, diagnostics)?;
                }
            }
            SequentialStatement::Exit(ref mut exit_stmt) => {
                let ExitStatement {
                    condition,
                    // @TODO loop label
                    ..
                } = exit_stmt;

                if let Some(expr) = condition {
                    self.analyze_expression(parent, expr, diagnostics)?;
                }
            }
            SequentialStatement::Next(ref mut next_stmt) => {
                let NextStatement {
                    condition,
                    // @TODO loop label
                    ..
                } = next_stmt;

                if let Some(expr) = condition {
                    self.analyze_expression(parent, expr, diagnostics)?;
                }
            }
            SequentialStatement::If(ref mut ifstmt) => {
                let IfStatement {
                    conditionals,
                    else_item,
                } = ifstmt;

                // @TODO write generic function for this
                for conditional in conditionals {
                    let Conditional { condition, item } = conditional;
                    self.analyze_sequential_part(parent, item, diagnostics)?;
                    self.analyze_expression(parent, condition, diagnostics)?;
                }
                if let Some(else_item) = else_item {
                    self.analyze_sequential_part(parent, else_item, diagnostics)?;
                }
            }
            SequentialStatement::Case(ref mut case_stmt) => {
                let Selection {
                    expression,
                    alternatives,
                } = case_stmt;
                self.analyze_expression(parent, expression, diagnostics)?;
                for alternative in alternatives.iter_mut() {
                    let Alternative { choices, item } = alternative;
                    self.analyze_choices(parent, choices, diagnostics)?;
                    self.analyze_sequential_part(parent, item, diagnostics)?;
                }
            }
            SequentialStatement::Loop(ref mut loop_stmt) => {
                let LoopStatement {
                    iteration_scheme,
                    statements,
                } = loop_stmt;
                match iteration_scheme {
                    Some(IterationScheme::For(ref mut index, ref mut drange)) => {
                        self.analyze_discrete_range(parent, drange, diagnostics)?;
                        let mut region = DeclarativeRegion::new_borrowed_parent(parent);
                        let designator: WithPos<Designator> = index.clone().into();
                        region.add(designator, AnyDeclaration::Constant, diagnostics);
                        self.analyze_sequential_part(&mut region, statements, diagnostics)?;
                    }
                    Some(IterationScheme::While(ref mut expr)) => {
                        self.analyze_expression(parent, expr, diagnostics)?;
                        self.analyze_sequential_part(parent, statements, diagnostics)?;
                    }
                    None => {
                        self.analyze_sequential_part(parent, statements, diagnostics)?;
                    }
                }
            }
            SequentialStatement::ProcedureCall(ref mut pcall) => {
                self.analyze_function_call(parent, pcall, diagnostics)?;
            }
            SequentialStatement::SignalAssignment(ref mut assign) => {
                // @TODO more
                let SignalAssignment { target, rhs, .. } = assign;
                self.analyze_waveform_assignment(parent, target, rhs, diagnostics)?;
            }
            SequentialStatement::VariableAssignment(ref mut assign) => {
                let VariableAssignment { target, rhs } = assign;
                self.analyze_expr_assignment(parent, target, rhs, diagnostics)?;
            }
            SequentialStatement::SignalForceAssignment(ref mut assign) => {
                let SignalForceAssignment {
                    target,
                    force_mode: _,
                    rhs,
                } = assign;
                self.analyze_expr_assignment(parent, target, rhs, diagnostics)?;
            }
            SequentialStatement::SignalReleaseAssignment(ref mut assign) => {
                let SignalReleaseAssignment {
                    target,
                    force_mode: _,
                } = assign;
                self.analyze_target(parent, target, diagnostics)?;
            }
            SequentialStatement::Null => {}
        }
        Ok(())
    }

    fn analyze_sequential_part(
        &self,
        parent: &mut DeclarativeRegion<'_>,
        statements: &mut [LabeledSequentialStatement],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        for statement in statements.iter_mut() {
            self.analyze_sequential_statement(parent, statement, diagnostics)?;
        }

        Ok(())
    }

    /// Add implicit context clause for all packages except STD.STANDARD
    /// library STD, WORK;
    /// use STD.STANDARD.all;
    fn add_implicit_context_clause(&self, region: &mut DeclarativeRegion<'_>) -> FatalNullResult {
        region.make_library_visible(&self.work_sym, &self.work_library_name, None);

        // @TODO maybe add warning if standard library is missing
        if self.root.has_library(&self.std_sym) {
            region.make_library_visible(&self.std_sym, &self.std_sym, None);

            let standard_pkg_region = &self
                .root
                .expect_package_declaration_analysis(&LibraryUnitId::package(
                    &self.std_sym,
                    &self.standard_sym,
                ))?
                .region;
            region.make_all_potentially_visible(standard_pkg_region);
        }

        Ok(())
    }

    pub fn analyze_package_declaration(
        &self,
        has_body: bool,
        package_data: &mut PackageData,
    ) -> FatalNullResult {
        let mut diagnostics = Vec::new();
        let mut root_region = Box::new(DeclarativeRegion::default());
        if !(self.work_library_name == self.std_sym && *package_data.name() == self.standard_sym) {
            self.add_implicit_context_clause(&mut root_region)?;
        }

        self.analyze_context_clause(
            &mut root_region,
            &mut package_data.ast.context_clause,
            &mut diagnostics,
        )?;

        let mut region = DeclarativeRegion::new_owned_parent(root_region).in_package_declaration();

        if let Some(ref mut list) = package_data.ast.generic_clause {
            self.analyze_interface_list(&mut region, list, &mut diagnostics)?;
        }
        self.analyze_declarative_part(&mut region, &mut package_data.ast.decl, &mut diagnostics)?;

        if has_body {
            region.close_immediate(&mut diagnostics);
        } else {
            region.close_both(&mut diagnostics);
        }

        package_data.diagnostics = diagnostics;
        package_data.region = region;
        Ok(())
    }

    pub fn analyze_package_body_unit(&self, body: &mut PackageBodyData) -> FatalNullResult {
        let mut diagnostics = Vec::new();

        body.ast.ident.clear_reference();

        let package_data = {
            if let Some(package_data) =
                self.root
                    .get_package_declaration_analysis(&LibraryUnitId::package(
                        &self.work_library_name,
                        body.ast.primary_name(),
                    ))
            {
                package_data?
            } else {
                diagnostics.push(Diagnostic::error(
                    body.ast.pos(),
                    format!(
                        "No package '{}' within library '{}'",
                        body.ast.primary_name(),
                        &self.work_library_name
                    ),
                ));
                // Body does not need region
                body.diagnostics = diagnostics;
                return Ok(());
            }
        };

        let primary_pos = &package_data.pos();
        let secondary_pos = &body.pos();
        if primary_pos.source == secondary_pos.source && primary_pos.start() > secondary_pos.start()
        {
            diagnostics.push(Diagnostic::error(
                secondary_pos,
                format!(
                    "Package body declared before package '{}'",
                    package_data.name()
                ),
            ));
        }

        body.ast.ident.set_reference_pos(Some(package_data.pos()));
        // @TODO make pattern of primary/secondary extension
        let mut root_region = package_data.region.get_parent().unwrap().extend(None);
        self.analyze_context_clause(
            &mut root_region,
            &mut body.ast.context_clause,
            &mut diagnostics,
        )?;
        let mut region = package_data.region.extend(Some(&root_region));

        // Package name is visible in body
        region.make_potentially_visible(VisibleDeclaration::new(
            package_data.ast.ident(),
            AnyDeclaration::Constant,
        ));

        self.analyze_declarative_part(&mut region, &mut body.ast.decl, &mut diagnostics)?;
        region.close_both(&mut diagnostics);
        // Body does not need region
        body.diagnostics = diagnostics;
        Ok(())
    }

    pub fn analyze_architecture(&self, arch_data: &mut ArchitectureData) -> FatalNullResult {
        let mut diagnostics = Vec::new();
        arch_data.ast.entity_name.clear_reference();

        let entity = {
            if let Some(entity_data) =
                self.root
                    .get_entity_declaration_analysis(&LibraryUnitId::entity(
                        &self.work_library_name,
                        arch_data.ast.primary_name(),
                    ))
            {
                entity_data?
            } else {
                diagnostics.push(Diagnostic::error(
                    arch_data.ast.primary_pos(),
                    format!(
                        "No entity '{}' within library '{}'",
                        arch_data.ast.primary_name(),
                        &self.work_library_name
                    ),
                ));
                // @TODO architecture does not need a region
                arch_data.diagnostics = diagnostics;
                return Ok(());
            }
        };

        let primary_pos = entity.pos();
        let secondary_pos = &arch_data.pos();
        if primary_pos.source == secondary_pos.source && primary_pos.start() > secondary_pos.start()
        {
            diagnostics.push(Diagnostic::error(
                secondary_pos,
                format!(
                    "Architecture '{}' declared before entity '{}'",
                    &arch_data.name(),
                    entity.name()
                ),
            ));
        }

        arch_data
            .ast
            .entity_name
            .set_reference_pos(Some(entity.ast.pos()));

        let mut root_region = entity.region.get_parent().unwrap().extend(None);
        self.analyze_context_clause(
            &mut root_region,
            &mut arch_data.ast.context_clause,
            &mut diagnostics,
        )?;
        let mut region = entity.region.extend(Some(&root_region));

        // entity name is visible
        region.make_potentially_visible(VisibleDeclaration::new(
            entity.ast.ident(),
            AnyDeclaration::Constant,
        ));

        self.analyze_declarative_part(&mut region, &mut arch_data.ast.decl, &mut diagnostics)?;
        self.analyze_concurrent_part(&mut region, &mut arch_data.ast.statements, &mut diagnostics)?;
        region.close_both(&mut diagnostics);
        // @TODO architecture does not need a region
        arch_data.diagnostics = diagnostics;
        Ok(())
    }

    pub fn analyze_entity_declaration(&self, entity_data: &mut EntityData) -> FatalNullResult {
        let mut diagnostics = Vec::new();
        let mut root_region = DeclarativeRegion::default();
        self.add_implicit_context_clause(&mut root_region)?;
        self.analyze_context_clause(
            &mut root_region,
            &mut entity_data.ast.context_clause,
            &mut diagnostics,
        )?;

        let mut region = DeclarativeRegion::new_owned_parent(Box::new(root_region));

        if let Some(ref mut list) = entity_data.ast.generic_clause {
            self.analyze_interface_list(&mut region, list, &mut diagnostics)?;
        }
        if let Some(ref mut list) = entity_data.ast.port_clause {
            self.analyze_interface_list(&mut region, list, &mut diagnostics)?;
        }
        self.analyze_declarative_part(&mut region, &mut entity_data.ast.decl, &mut diagnostics)?;
        self.analyze_concurrent_part(
            &mut region,
            &mut entity_data.ast.statements,
            &mut diagnostics,
        )?;

        region.close_immediate(&mut diagnostics);

        entity_data.region = region;
        entity_data.diagnostics = diagnostics;
        Ok(())
    }

    /// Returns a reference to the the uninstantiated package
    fn analyze_package_instance_name(
        &self,
        region: &DeclarativeRegion<'_>,
        package_name: &mut WithPos<SelectedName>,
    ) -> AnalysisResult<ReadGuard<PackageData>> {
        let decl = self.resolve_selected_name(region, package_name)?;

        if let AnyDeclaration::UninstPackage(ref unit_id) = decl.first() {
            match self.root.expect_package_declaration_analysis(unit_id) {
                Ok(package) => Ok(package),
                Err(err) => Err(AnalysisError::Fatal(err)),
            }
        } else {
            Err(Diagnostic::error(
                &package_name.pos,
                format!(
                    "'{}' is not an uninstantiated generic package",
                    &decl.designator
                ),
            ))?
        }
    }

    pub fn analyze_package_instance(
        &self,
        instance_data: &mut PackageInstanceData,
    ) -> FatalNullResult {
        let mut diagnostics = Vec::new();
        let mut region = DeclarativeRegion::default();
        self.add_implicit_context_clause(&mut region)?;
        self.analyze_context_clause(
            &mut region,
            &mut instance_data.ast.context_clause,
            &mut diagnostics,
        )?;

        match self.analyze_package_instance_name(&region, &mut instance_data.ast.package_name) {
            Ok(package) => {
                instance_data.diagnostics = diagnostics;
                instance_data.region = package.region.clone();
                Ok(())
            }
            Err(AnalysisError::NotFatal(diagnostic)) => {
                diagnostics.push(diagnostic);
                // Failed to analyze, add empty region with diagnostics
                instance_data.diagnostics = diagnostics;
                Ok(())
            }
            Err(AnalysisError::Fatal(err)) => {
                Err(err.add_reference(&instance_data.ast.package_name.pos))
            }
        }
    }

    pub fn analyze_context(&self, context_data: &mut ContextData) -> FatalNullResult {
        let mut root_region = DeclarativeRegion::default();
        self.add_implicit_context_clause(&mut root_region)?;
        let mut diagnostics = Vec::new();
        let mut region = DeclarativeRegion::new_owned_parent(Box::new(root_region));
        self.analyze_context_clause(&mut region, &mut context_data.ast.items, &mut diagnostics)?;
        context_data.region = region;
        context_data.diagnostics = diagnostics;
        Ok(())
    }

    fn lookup_entity_for_configuration(
        &self,
        region: &DeclarativeRegion<'_>,
        config: &mut ConfigurationDeclaration,
    ) -> AnalysisResult<ReadGuard<EntityData>> {
        let ref mut ent_name = config.entity_name;

        let decl = {
            match ent_name.item {
                // Entitities are implicitly defined for configurations
                // configuration cfg of ent
                SelectedName::Designator(ref mut designator) => {
                    match self.lookup_in_library(
                        &self.work_library_name,
                        &ent_name.pos,
                        &designator.item,
                    ) {
                        Ok(decl) => {
                            designator.set_reference(&decl);
                            Ok(decl)
                        }
                        Err(err) => {
                            designator.clear_reference();
                            Err(err)
                        }
                    }
                }

                // configuration cfg of lib.ent
                _ => self.resolve_selected_name(&region, ent_name),
            }
        }?;

        match decl.first() {
            AnyDeclaration::Entity(ref unit_id) => {
                if unit_id.library_name() != &self.work_library_name {
                    Err(Diagnostic::error(
                                    &ent_name,
                                    format!("Configuration must be within the same library '{}' as the corresponding entity", &self.work_library_name),
                                ))?
                } else {
                    match self.root.expect_entity_declaration_analysis(unit_id) {
                        Ok(data) => Ok(data),
                        Err(err) => Err(AnalysisError::Fatal(err)),
                    }
                }
            }
            _ => Err(Diagnostic::error(&ent_name, "does not denote an entity"))?,
        }
    }

    pub fn analyze_configuration(&self, config: &mut ConfigurationData) -> FatalNullResult {
        let mut diagnostics = Vec::new();
        let mut region = DeclarativeRegion::default();
        self.add_implicit_context_clause(&mut region)?;
        self.analyze_context_clause(
            &mut region,
            &mut config.ast.context_clause,
            &mut diagnostics,
        )?;

        match self.lookup_entity_for_configuration(&region, &mut config.ast) {
            Ok(entity_unit) => {
                let primary_pos = entity_unit.pos();
                let secondary_pos = config.pos();
                if primary_pos.source == secondary_pos.source
                    && primary_pos.start() > secondary_pos.start()
                {
                    diagnostics.push(Diagnostic::error(
                        secondary_pos,
                        format!(
                            "Configuration '{}' declared before entity '{}'",
                            &config.name(),
                            entity_unit.name()
                        ),
                    ));
                }
            }
            Err(err) => {
                err.add_to(&mut diagnostics)?;
            }
        };

        config.region = region;
        config.diagnostics = diagnostics;
        Ok(())
    }
}

trait SetReference {
    fn set_reference(&mut self, decl: &VisibleDeclaration) {
        // @TODO handle built-ins without position
        // @TODO handle mutliple overloaded declarations
        if !decl.is_overloaded() {
            // We do not set references to overloaded names to avoid
            // To much incorrect behavior which will appear as low quality
            self.set_reference_pos(decl.first_pos());
        } else {
            self.clear_reference();
        }
    }

    fn clear_reference(&mut self) {
        self.set_reference_pos(None);
    }

    fn set_reference_pos(&mut self, pos: Option<&SrcPos>);
}

impl<T> SetReference for WithRef<T> {
    fn set_reference_pos(&mut self, pos: Option<&SrcPos>) {
        self.reference = pos.cloned();
    }
}

impl<T: SetReference> SetReference for WithPos<T> {
    fn set_reference_pos(&mut self, pos: Option<&SrcPos>) {
        self.item.set_reference_pos(pos);
    }
}

fn uninstantiated_package_prefix_error(prefix: &SrcPos, unit_id: &LibraryUnitId) -> Diagnostic {
    Diagnostic::error(
        prefix,
        format!(
            "Uninstantiated generic package '{}.{}' may not be the prefix of a selected name",
            unit_id.library_name(),
            unit_id.primary_name()
        ),
    )
}
