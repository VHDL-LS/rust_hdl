//! This Source Code Form is subject to the terms of the Mozilla Public
//! License, v. 2.0. If a copy of the MPL was not distributed with this file,
//! You can obtain one at http://mozilla.org/MPL/2.0/.
//!
//! Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::analyze::*;
use super::scope::*;
use super::target::AssignmentType;
use crate::ast::token_range::WithTokenSpan;
use crate::ast::*;
use crate::data::*;
use crate::named_entity::*;

impl<'a> AnalyzeContext<'a, '_> {
    // @TODO maybe make generic function for expression/waveform.
    // wait until type checking to see if it makes sense
    pub fn analyze_expr_assignment(
        &self,
        scope: &Scope<'a>,
        target: &mut WithTokenSpan<Target>,
        assignment_type: AssignmentType,
        rhs: &mut AssignmentRightHand<WithTokenSpan<Expression>>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        let ttyp = as_fatal(self.resolve_target(scope, target, assignment_type, diagnostics))?;
        match rhs {
            AssignmentRightHand::Simple(expr) => {
                self.analyze_expression_for_target(scope, ttyp, expr, diagnostics)?;
            }
            AssignmentRightHand::Conditional(conditionals) => {
                let Conditionals {
                    conditionals,
                    else_item,
                } = conditionals;
                for conditional in conditionals {
                    let Conditional { condition, item } = conditional;
                    self.analyze_expression_for_target(scope, ttyp, item, diagnostics)?;
                    self.boolean_expr(scope, condition, diagnostics)?;
                }
                if let Some((expr, _)) = else_item {
                    self.analyze_expression_for_target(scope, ttyp, expr, diagnostics)?;
                }
            }
            AssignmentRightHand::Selected(selection) => {
                let Selection {
                    expression,
                    alternatives,
                } = selection;
                let ctyp = as_fatal(self.expr_unambiguous_type(scope, expression, diagnostics))?;
                for Alternative {
                    choices,
                    item,
                    span: _,
                } in alternatives.iter_mut()
                {
                    self.analyze_expression_for_target(scope, ttyp, item, diagnostics)?;
                    self.choices_with_ttyp(scope, ctyp, choices, diagnostics)?;
                }
            }
        }
        Ok(())
    }

    pub fn analyze_waveform_assignment(
        &self,
        scope: &Scope<'a>,
        assignment: &mut SignalAssignment,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        let ttyp = as_fatal(self.resolve_target(
            scope,
            &mut assignment.target,
            AssignmentType::Signal,
            diagnostics,
        ))?;
        match &mut assignment.rhs {
            AssignmentRightHand::Simple(wavf) => {
                self.analyze_waveform(scope, ttyp, wavf, diagnostics)?;
            }
            AssignmentRightHand::Conditional(conditionals) => {
                let Conditionals {
                    conditionals,
                    else_item,
                } = conditionals;
                for conditional in conditionals {
                    let Conditional { condition, item } = conditional;
                    self.analyze_waveform(scope, ttyp, item, diagnostics)?;
                    self.boolean_expr(scope, condition, diagnostics)?;
                }
                if let Some((wavf, _)) = else_item {
                    self.analyze_waveform(scope, ttyp, wavf, diagnostics)?;
                }
            }
            AssignmentRightHand::Selected(selection) => {
                let Selection {
                    expression,
                    alternatives,
                } = selection;
                let ctyp = as_fatal(self.expr_unambiguous_type(scope, expression, diagnostics))?;
                for Alternative {
                    choices,
                    item,
                    span: _,
                } in alternatives.iter_mut()
                {
                    self.analyze_waveform(scope, ttyp, item, diagnostics)?;
                    self.choices_with_ttyp(scope, ctyp, choices, diagnostics)?;
                }
            }
        }
        Ok(())
    }

    fn analyze_waveform(
        &self,
        scope: &Scope<'a>,
        ttyp: Option<TypeEnt<'a>>,
        wavf: &mut Waveform,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        match wavf {
            Waveform::Elements(ref mut elems) => {
                for elem in elems.iter_mut() {
                    let WaveformElement { value, after } = elem;
                    self.analyze_expression_for_target(scope, ttyp, value, diagnostics)?;
                    if let Some(expr) = after {
                        self.expr_with_ttyp(scope, self.time(), expr, diagnostics)?;
                    }
                }
            }
            Waveform::Unaffected(_) => {}
        }
        Ok(())
    }

    pub fn analyze_expression_for_target(
        &self,
        scope: &Scope<'a>,
        ttyp: Option<TypeEnt<'a>>,
        expr: &mut WithTokenSpan<Expression>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        if let Some(ttyp) = ttyp {
            self.expr_with_ttyp(scope, ttyp, expr, diagnostics)?;
        } else {
            self.expr_unknown_ttyp(scope, expr, diagnostics)?;
        }
        Ok(())
    }
}
