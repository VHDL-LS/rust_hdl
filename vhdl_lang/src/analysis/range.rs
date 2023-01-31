//! This Source Code Form is subject to the terms of the Mozilla Public
//! License, v. 2.0. If a copy of the MPL was not distributed with this file,
//! You can obtain one at http://mozilla.org/MPL/2.0/.
//!
//! Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use super::analyze::*;
use super::named_entity::*;
use super::region::*;
use crate::ast::Range;
use crate::ast::*;
use crate::data::*;

impl<'a> AnalyzeContext<'a> {
    pub fn analyze_range(
        &self,
        scope: &Scope<'a>,
        range: &mut Range,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        match range {
            Range::Range(ref mut constraint) => {
                self.analyze_expression(scope, &mut constraint.left_expr, diagnostics)?;
                self.analyze_expression(scope, &mut constraint.right_expr, diagnostics)?;
            }
            Range::Attribute(ref mut attr) => {
                self.analyze_attribute_name(scope, attr, diagnostics)?
            }
        }
        Ok(())
    }

    pub fn analyze_range_with_target_type(
        &self,
        scope: &Scope<'a>,
        target_type: TypeEnt<'a>,
        range: &mut Range,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        match range {
            Range::Range(ref mut constraint) => {
                self.expr_with_ttyp(
                    scope,
                    target_type,
                    &constraint.left_expr.pos,
                    &mut constraint.left_expr.item,
                    diagnostics,
                )?;
                self.expr_with_ttyp(
                    scope,
                    target_type,
                    &constraint.right_expr.pos,
                    &mut constraint.right_expr.item,
                    diagnostics,
                )?;
            }
            Range::Attribute(ref mut attr) => {
                self.analyze_attribute_name(scope, attr, diagnostics)?;
            }
        }
        Ok(())
    }

    pub fn analyze_discrete_range(
        &self,
        scope: &Scope<'a>,
        drange: &mut DiscreteRange,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        match drange {
            DiscreteRange::Discrete(ref mut type_mark, ref mut range) => {
                if let Err(err) = self.resolve_type_mark(scope, type_mark) {
                    err.add_to(diagnostics)?;
                }
                if let Some(ref mut range) = range {
                    self.analyze_range(scope, range, diagnostics)?;
                }
            }
            DiscreteRange::Range(ref mut range) => {
                self.analyze_range(scope, range, diagnostics)?;
            }
        }
        Ok(())
    }

    pub fn analyze_discrete_range_with_target_type(
        &self,
        scope: &Scope<'a>,
        target_type: TypeEnt<'a>,
        drange: &mut DiscreteRange,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        match drange {
            DiscreteRange::Discrete(ref mut type_mark, ref mut range) => {
                if let Err(err) = self.resolve_type_mark(scope, type_mark) {
                    err.add_to(diagnostics)?;
                }
                if let Some(ref mut range) = range {
                    self.analyze_range_with_target_type(scope, target_type, range, diagnostics)?;
                }
            }
            DiscreteRange::Range(ref mut range) => {
                self.analyze_range_with_target_type(scope, target_type, range, diagnostics)?;
            }
        }
        Ok(())
    }
}
