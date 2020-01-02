// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::analyze::*;
use super::region::*;
use super::root::*;
use crate::ast::Range;
use crate::ast::*;
use crate::data::*;

pub enum LookupResult {
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

impl<'a> AnalyzeContext<'a> {
    fn lookup_selected(
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

            AnyDeclaration::UninstPackage(ref unit_id, ..) => {
                Err(uninstantiated_package_prefix_error(prefix_pos, unit_id))?
            }

            AnyDeclaration::Package(ref unit_id, ref package_region) => {
                if let Some(decl) = package_region.lookup_selected(suffix.designator()) {
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

            AnyDeclaration::PackageInstance(ref unit_id, ref instance_region) => {
                if let Some(decl) = instance_region.lookup_selected(suffix.designator()) {
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
                if let Some(decl) = instance_region.lookup_selected(suffix.designator()) {
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

    pub fn resolve_selected_name(
        &self,
        region: &Region<'_>,
        name: &mut WithPos<SelectedName>,
    ) -> AnalysisResult<VisibleDeclaration> {
        match name.item {
            SelectedName::Selected(ref mut prefix, ref mut suffix) => {
                suffix.clear_reference();
                let prefix_decl = self.resolve_selected_name(region, prefix)?;
                match self.lookup_selected(&prefix.pos, &prefix_decl, suffix)? {
                    Some(decl) => {
                        suffix.set_reference(&decl);
                        Ok(decl)
                    }
                    None => Err(AnalysisError::NotFatal(Diagnostic::error(
                        &prefix.pos,
                        "Invalid prefix for selected name",
                    ))),
                }
            }
            SelectedName::Designator(ref mut designator) => {
                if let Some(decl) = region.lookup_within(designator.designator()) {
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
        region: &Region<'_>,
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

    pub fn resolve_name_pos(
        &self,
        region: &Region<'_>,
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
                    Some(decl) => match self.lookup_selected(&prefix.pos, &decl, suffix)? {
                        Some(decl) => {
                            suffix.set_reference(&decl);
                            Ok(LookupResult::Single(decl))
                        }
                        None => {
                            if allow_incomplete {
                                Ok(LookupResult::NotSelected)
                            } else {
                                Err(Diagnostic::error(
                                    &prefix.pos,
                                    "Invalid prefix for selected name",
                                ))?
                            }
                        }
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
                if let Some(decl) = region.lookup_within(designator.designator()) {
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

    pub fn resolve_type_mark(
        &self,
        region: &Region<'_>,
        type_mark: &mut WithPos<SelectedName>,
    ) -> AnalysisResult<VisibleDeclaration> {
        self.resolve_selected_name(region, type_mark)
    }

    pub fn resolve_name(
        &self,
        region: &Region<'_>,
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
        region: &Region<'_>,
        attr: &mut AttributeName,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        // @TODO more
        let AttributeName { name, .. } = attr;
        self.resolve_name(region, &name.pos, &mut name.item, diagnostics)?;
        Ok(())
    }

    pub fn analyze_range(
        &self,
        region: &Region<'_>,
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

    pub fn analyze_discrete_range(
        &self,
        region: &Region<'_>,
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

    pub fn analyze_choices(
        &self,
        region: &Region<'_>,
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

    pub fn analyze_expression(
        &self,
        region: &Region<'_>,
        expr: &mut WithPos<Expression>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        self.analyze_expression_pos(region, &expr.pos, &mut expr.item, diagnostics)
    }

    pub fn analyze_waveform(
        &self,
        region: &Region<'_>,
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

    pub fn analyze_assoc_elems(
        &self,
        region: &Region<'_>,
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

    pub fn analyze_function_call(
        &self,
        region: &Region<'_>,
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
        region: &Region<'_>,
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
        region: &Region<'_>,
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
        region: &Region<'_>,
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

    // @TODO maybe make generic function for expression/waveform.
    // wait until type checking to see if it makes sense
    pub fn analyze_expr_assignment(
        &self,
        region: &Region<'_>,
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

    pub fn analyze_waveform_assignment(
        &self,
        region: &Region<'_>,
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

    pub fn analyze_target(
        &self,
        parent: &Region<'_>,
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
}

pub fn uninstantiated_package_prefix_error(prefix: &SrcPos, unit_id: &UnitId) -> Diagnostic {
    Diagnostic::error(
        prefix,
        format!(
            "Uninstantiated generic package '{}.{}' may not be the prefix of a selected name",
            unit_id.library_name(),
            unit_id.primary_name()
        ),
    )
}
