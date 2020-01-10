// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::analyze::*;
use super::region::*;
use crate::ast::Range;
use crate::ast::*;
use crate::data::*;
use std::sync::Arc;

pub enum ResolvedName {
    /// A single name was selected
    Known(VisibleDeclaration),

    /// Until type checking is performed we keep unknown state
    Unknown,
}

impl ResolvedName {
    fn into_known(self) -> Option<VisibleDeclaration> {
        if let Self::Known(visible) = self {
            Some(visible)
        } else {
            None
        }
    }
    pub fn into_non_overloaded(self) -> Option<Arc<NamedEntity>> {
        self.into_known()
            .and_then(|visible| visible.into_non_overloaded().ok())
    }
}

impl<'a> AnalyzeContext<'a> {
    pub fn lookup_selected(
        &self,
        prefix_pos: &SrcPos,
        prefix: &NamedEntity,
        suffix: &WithPos<WithRef<Designator>>,
    ) -> AnalysisResult<ResolvedName> {
        match prefix.kind() {
            NamedEntityKind::Library => {
                let library_name = prefix.designator().expect_identifier();
                let named_entity =
                    self.lookup_in_library(library_name, &suffix.pos, suffix.designator())?;

                Ok(ResolvedName::Known(VisibleDeclaration::new(
                    suffix.designator().clone(),
                    named_entity,
                )))
            }
            NamedEntityKind::UninstPackage(..) => Err(AnalysisError::NotFatal(
                uninstantiated_package_prefix_error(prefix, prefix_pos),
            )),

            NamedEntityKind::Package(ref region)
            | NamedEntityKind::PackageInstance(ref region)
            | NamedEntityKind::LocalPackageInstance(ref region) => {
                if let Some(decl) = region.lookup_selected(suffix.designator()) {
                    Ok(ResolvedName::Known(decl.clone()))
                } else {
                    Err(AnalysisError::not_fatal_error(
                        suffix.as_ref(),
                        format!(
                            "No declaration of '{}' within {}",
                            suffix.item,
                            prefix.describe(),
                        ),
                    ))
                }
            }
            _ => Ok(ResolvedName::Unknown),
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

                let prefix_ent = self
                    .resolve_selected_name(region, prefix)?
                    .into_non_overloaded();
                if let Ok(prefix_ent) = prefix_ent {
                    match self.lookup_selected(&prefix.pos, &prefix_ent, suffix)? {
                        ResolvedName::Known(visible) => {
                            suffix.set_reference(&visible);
                            return Ok(visible);
                        }
                        ResolvedName::Unknown => {}
                    };
                };

                Err(AnalysisError::NotFatal(Diagnostic::error(
                    &prefix.pos,
                    "Invalid prefix for selected name",
                )))
            }
            SelectedName::Designator(ref mut designator) => {
                designator.clear_reference();
                let visible = region.lookup_within(&name.pos, designator.designator())?;
                designator.set_reference(&visible);
                Ok(visible)
            }
        }
    }

    fn resolve_prefix(
        &self,
        region: &Region<'_>,
        prefix_pos: &SrcPos,
        prefix: &mut Name,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult<Option<Arc<NamedEntity>>> {
        Ok(self
            .resolve_name(region, prefix_pos, prefix, diagnostics)?
            .and_then(|resolved| resolved.into_non_overloaded()))
    }

    pub fn resolve_name(
        &self,
        region: &Region<'_>,
        name_pos: &SrcPos,
        name: &mut Name,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult<Option<ResolvedName>> {
        match name {
            Name::Selected(prefix, suffix) => {
                suffix.clear_reference();

                match self.resolve_prefix(region, &prefix.pos, &mut prefix.item, diagnostics)? {
                    Some(ref named_entity) => {
                        match self.lookup_selected(&prefix.pos, named_entity, suffix) {
                            Ok(ResolvedName::Known(visible)) => {
                                suffix.set_reference(&visible);
                                Ok(Some(ResolvedName::Known(visible)))
                            }
                            Ok(ResolvedName::Unknown) => Ok(Some(ResolvedName::Unknown)),
                            Err(err) => {
                                err.add_to(diagnostics)?;
                                Ok(None)
                            }
                        }
                    }
                    None => Ok(Some(ResolvedName::Unknown)),
                }
            }

            Name::SelectedAll(prefix) => {
                self.resolve_prefix(region, &prefix.pos, &mut prefix.item, diagnostics)?;

                Ok(Some(ResolvedName::Unknown))
            }
            Name::Designator(designator) => {
                designator.clear_reference();
                match region.lookup_within(name_pos, designator.designator()) {
                    Ok(visible) => {
                        designator.set_reference(&visible);
                        Ok(Some(ResolvedName::Known(visible)))
                    }
                    Err(diagnostic) => {
                        diagnostics.push(diagnostic);
                        Ok(None)
                    }
                }
            }
            Name::Indexed(ref mut prefix, ref mut exprs) => {
                self.resolve_name(region, &prefix.pos, &mut prefix.item, diagnostics)?;
                for expr in exprs.iter_mut() {
                    self.analyze_expression(region, expr, diagnostics)?;
                }
                Ok(Some(ResolvedName::Unknown))
            }

            Name::Slice(ref mut prefix, ref mut drange) => {
                self.resolve_name(region, &prefix.pos, &mut prefix.item, diagnostics)?;
                self.analyze_discrete_range(region, drange.as_mut(), diagnostics)?;
                Ok(Some(ResolvedName::Unknown))
            }
            Name::Attribute(ref mut attr) => {
                self.analyze_attribute_name(region, attr, diagnostics)?;
                Ok(Some(ResolvedName::Unknown))
            }
            Name::FunctionCall(ref mut fcall) => {
                self.analyze_function_call(region, fcall, diagnostics)?;
                Ok(Some(ResolvedName::Unknown))
            }
            Name::External(ref mut ename) => {
                let ExternalName { subtype, .. } = ename.as_mut();
                self.analyze_subtype_indication(region, subtype, diagnostics)?;
                Ok(Some(ResolvedName::Unknown))
            }
        }
    }

    pub fn resolve_non_overloaded(
        &self,
        region: &Region<'_>,
        name: &mut WithPos<SelectedName>,
        kind_ok: &impl Fn(&NamedEntityKind) -> bool,
        expected: &str,
    ) -> AnalysisResult<Arc<NamedEntity>> {
        match self
            .resolve_selected_name(region, name)?
            .into_non_overloaded()
        {
            Ok(ent) => {
                if kind_ok(ent.as_actual().kind()) {
                    Ok(ent)
                } else {
                    let mut error = Diagnostic::error(
                        name.suffix_pos(),
                        format!("Expected {}, got {}", expected, ent.describe()),
                    );
                    if let Some(pos) = ent.decl_pos() {
                        error.add_related(pos, "Defined here");
                    }
                    Err(AnalysisError::NotFatal(error))
                }
            }
            Err(visible) => {
                let mut error = Diagnostic::error(
                    name.suffix_pos(),
                    format!("Expected {}, got overloaded name", expected),
                );
                for ent in visible.named_entities() {
                    if let Some(pos) = ent.decl_pos() {
                        error.add_related(pos, "Defined here");
                    }
                }
                Err(AnalysisError::NotFatal(error))
            }
        }
    }

    pub fn resolve_type_mark(
        &self,
        region: &Region<'_>,
        type_mark: &mut WithPos<SelectedName>,
    ) -> AnalysisResult<Arc<NamedEntity>> {
        fn is_type(kind: &NamedEntityKind) -> bool {
            match kind {
                NamedEntityKind::IncompleteType
                | NamedEntityKind::ProtectedType(..)
                | NamedEntityKind::InterfaceType
                | NamedEntityKind::TypeDeclaration(..) => true,
                _ => false,
            }
        }

        self.resolve_non_overloaded(region, type_mark, &is_type, "type")
    }

    fn analyze_attribute_name(
        &self,
        region: &Region<'_>,
        attr: &mut AttributeName,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        // @TODO more, attr must be checked inside the region of attributes of prefix
        let AttributeName {
            name,
            signature,
            expr,
            ..
        } = attr;

        self.resolve_name(region, &name.pos, &mut name.item, diagnostics)?;

        if let Some(ref mut signature) = signature {
            self.analyze_signature(region, signature, diagnostics)?;
        }
        if let Some(ref mut expr) = expr {
            self.analyze_expression(region, expr, diagnostics)?;
        }
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
        self.resolve_name(region, &name.pos, &mut name.item, diagnostics)?;
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

// @TODO make method
pub fn uninstantiated_package_prefix_error(
    named_entity: &NamedEntity,
    prefix: &SrcPos,
) -> Diagnostic {
    Diagnostic::error(
        prefix,
        capitalize(&format!(
            "{} may not be the prefix of a selected name",
            named_entity.describe(),
        )),
    )
}
