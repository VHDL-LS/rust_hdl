// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use super::analyze::*;
use super::formal_region::FormalRegion;
use super::named_entity::*;
use super::region::*;
use super::semantic::TypeCheck;
use crate::ast::search::clear_references;
use crate::ast::*;
use crate::data::*;

impl<'a> AnalyzeContext<'a> {
    #[allow(clippy::too_many_arguments)]
    pub fn resolve_overloaded_with_target_type(
        &self,
        scope: &Scope<'a>,
        overloaded: OverloadedName<'a>,
        target_type: Option<TypeEnt<'a>>,
        pos: &SrcPos,
        designator: &Designator,
        reference: &mut Reference,
        parameters: &mut ParametersMut<'_>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult<TypeCheck> {
        let mut good = Vec::with_capacity(overloaded.len());
        let mut bad = Vec::with_capacity(overloaded.len());
        let mut uncertain = false;
        for name in overloaded.entities() {
            // Do not consider procedure vs. function
            if name.is_function() != target_type.is_some() {
                continue;
            }

            // Do not consider operators unary vs binary operators
            if let Some(operator_len) = parameters.operator_len() {
                if name.formals().len() != operator_len {
                    continue;
                }
            }

            if name.is_generic() {
                // Bail since we cannot handle this yet
                self.analyze_parameters(scope, parameters, diagnostics)?;
                return Ok(TypeCheck::Unknown);
            }

            let is_correct = if name.signature().match_return_type(target_type) {
                self.analyze_parameters_with_formal_region(
                    pos,
                    name.formals(),
                    scope,
                    parameters,
                    &mut NullDiagnostics,
                )?
            } else {
                TypeCheck::NotOk
            };

            // Clear references that could have been incorrectly set
            parameters.clear_references();

            match is_correct {
                TypeCheck::Ok => good.push(name),
                TypeCheck::NotOk => bad.push(name),
                TypeCheck::Unknown => uncertain = true,
            }
        }

        #[allow(clippy::if_same_then_else)]
        if good.len() > 1 {
            // Not unique
            let mut diagnostic =
                Diagnostic::error(pos, format!("Ambiguous use of '{}'", designator));
            diagnostic.add_subprogram_candidates("Migth be", &mut good);
            diagnostics.push(diagnostic);
            self.analyze_parameters(scope, parameters, diagnostics)?;
            Ok(TypeCheck::Unknown)
        } else if uncertain {
            self.analyze_parameters(scope, parameters, diagnostics)?;
            Ok(TypeCheck::Unknown)
        } else if let &[ent] = good.as_slice() {
            // Unique correct match
            reference.set_unique_reference(&ent);
            self.analyze_parameters_with_formal_region(
                pos,
                ent.formals(),
                scope,
                parameters,
                diagnostics,
            )?;
            Ok(TypeCheck::Ok)
        } else if let &[ent] = bad.as_slice() {
            // Unique incorrect match
            reference.set_unique_reference(&ent);
            if parameters.is_empty() && ent.formals().is_empty() {
                // Typically enumeration literals such as character, boolean
                // We provide a better diagnostic for those
                if let Some(target_type) = target_type {
                    diagnostics.error(
                        pos,
                        format!("'{}' does not match {}", designator, target_type.describe()),
                    )
                } else {
                    let mut diagnostic = Diagnostic::error(
                        pos,
                        format!("Could not resolve {}", designator.describe()),
                    );
                    diagnostic.add_subprogram_candidates("Does not match", &mut [ent]);
                    diagnostics.push(diagnostic)
                };
            } else {
                // The analysis below will produce the diagnostics for the bad option
                self.analyze_parameters_with_formal_region(
                    pos,
                    ent.formals(),
                    scope,
                    parameters,
                    diagnostics,
                )?;
            }
            Ok(TypeCheck::NotOk)
        } else {
            let mut diagnostic =
                Diagnostic::error(pos, format!("Could not resolve {}", designator.describe()));
            diagnostic.add_subprogram_candidates("Does not match", &mut bad);
            diagnostics.push(diagnostic);

            self.analyze_parameters(scope, parameters, diagnostics)?;
            Ok(TypeCheck::NotOk)
        }
    }

    fn analyze_parameters_with_formal_region(
        &self,
        error_pos: &SrcPos, // The position of the instance/call-site
        formal_region: &'a FormalRegion<'a>,
        scope: &Scope<'a>,
        parameters: &mut ParametersMut<'_>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult<TypeCheck> {
        match parameters {
            ParametersMut::AssociationList(elems) => self.analyze_assoc_elems_with_formal_region(
                error_pos,
                formal_region,
                scope,
                elems,
                diagnostics,
            ),
            ParametersMut::Binary(lexpr, rexpr) => {
                if let Some((left_formal, right_formal)) = formal_region.binary() {
                    let check = self.analyze_expression_with_target_type(
                        scope,
                        left_formal.type_mark(),
                        &lexpr.pos,
                        &mut lexpr.item,
                        diagnostics,
                    )?;

                    // Early exit to prevent combinatorial explosion in nested expressions
                    // @TODO check deepest tree first
                    if check != TypeCheck::Ok {
                        self.analyze_expression_pos(
                            scope,
                            &rexpr.pos,
                            &mut rexpr.item,
                            diagnostics,
                        )?;
                        Ok(check)
                    } else {
                        Ok(self.analyze_expression_with_target_type(
                            scope,
                            right_formal.type_mark(),
                            &rexpr.pos,
                            &mut rexpr.item,
                            diagnostics,
                        )?)
                    }
                } else {
                    self.analyze_expression_pos(scope, &lexpr.pos, &mut lexpr.item, diagnostics)?;
                    self.analyze_expression_pos(scope, &rexpr.pos, &mut rexpr.item, diagnostics)?;
                    Ok(TypeCheck::NotOk)
                }
            }
            ParametersMut::Unary(expr) => {
                if let Some(formal) = formal_region.nth(0) {
                    self.analyze_expression_with_target_type(
                        scope,
                        formal.type_mark(),
                        &expr.pos,
                        &mut expr.item,
                        diagnostics,
                    )
                } else {
                    self.analyze_expression_pos(scope, &expr.pos, &mut expr.item, diagnostics)?;
                    Ok(TypeCheck::NotOk)
                }
            }
        }
    }

    fn analyze_parameters(
        &self,
        scope: &Scope<'a>,
        parameters: &mut ParametersMut<'_>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        match parameters {
            ParametersMut::AssociationList(elems) => {
                self.analyze_assoc_elems(scope, elems, diagnostics)
            }
            ParametersMut::Binary(lexpr, rexpr) => {
                self.analyze_expression_pos(scope, &lexpr.pos, &mut lexpr.item, diagnostics)?;
                self.analyze_expression_pos(scope, &rexpr.pos, &mut rexpr.item, diagnostics)
            }
            ParametersMut::Unary(expr) => {
                self.analyze_expression_pos(scope, &expr.pos, &mut expr.item, diagnostics)
            }
        }
    }
}

// Allow us to handle functions and operators equally
pub enum ParametersMut<'a> {
    AssociationList(&'a mut [AssociationElement]),
    Binary(&'a mut WithPos<Expression>, &'a mut WithPos<Expression>),
    Unary(&'a mut WithPos<Expression>),
}

impl ParametersMut<'_> {
    fn clear_references(&mut self) {
        match self {
            ParametersMut::AssociationList(list) => {
                for elem in list.iter_mut() {
                    clear_references(elem);
                }
            }
            ParametersMut::Binary(lexpr, rexpr) => {
                clear_references(*lexpr);
                clear_references(*rexpr);
            }
            ParametersMut::Unary(expr) => {
                clear_references(*expr);
            }
        }
    }

    fn is_empty(&self) -> bool {
        match self {
            ParametersMut::AssociationList(list) => list.is_empty(),
            _ => false,
        }
    }

    fn operator_len(&self) -> Option<usize> {
        match self {
            ParametersMut::AssociationList(..) => None,
            ParametersMut::Unary(..) => Some(1),
            ParametersMut::Binary(..) => Some(2),
        }
    }
}
