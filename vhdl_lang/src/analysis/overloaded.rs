// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use super::analyze::*;
use super::formal_region::FormalRegion;
use super::region::*;
use super::semantic::TypeCheck;
use crate::ast::search::clear_references;
use crate::ast::*;
use crate::data::*;

impl<'a> AnalyzeContext<'a> {
    #[allow(clippy::too_many_arguments)]
    pub fn resolve_overloaded_with_target_type(
        &self,
        region: &Region<'_>,
        overloaded: OverloadedName,
        target_type: Option<&TypeEnt>,
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
            if let Some(sig) = name.signature() {
                let is_correct = if sig.match_return_type(target_type) {
                    self.analyze_parameters_with_formal_region(
                        pos,
                        &sig.params,
                        region,
                        parameters,
                        &mut NullDiagnostics,
                    )?
                } else {
                    TypeCheck::NotOk
                };

                // Clear references that could have been incorrectly set
                parameters.clear_references();

                match is_correct {
                    TypeCheck::Ok => good.push((name, sig)),
                    TypeCheck::NotOk => bad.push((name, sig)),
                    TypeCheck::Unknown => uncertain = true,
                }
            }
        }

        #[allow(clippy::if_same_then_else)]
        if good.len() > 1 {
            // Not unique
            let mut diagnostic =
                Diagnostic::error(pos, format!("Ambiguous use of '{}'", designator));
            diagnostic.add_subprogram_candidates(
                "Migth be",
                good.into_iter().map(|(ent, _)| ent).collect(),
            );
            diagnostics.push(diagnostic);
            self.analyze_parameters(region, parameters, diagnostics)?;
            Ok(TypeCheck::Unknown)
        } else if uncertain {
            self.analyze_parameters(region, parameters, diagnostics)?;
            Ok(TypeCheck::Unknown)
        } else if let &[(ent, sig)] = good.as_slice() {
            // Unique correct match
            reference.set_unique_reference(ent);
            self.analyze_parameters_with_formal_region(
                pos,
                &sig.params,
                region,
                parameters,
                diagnostics,
            )?;
            Ok(TypeCheck::Ok)
        } else if let &[(ent, sig)] = bad.as_slice() {
            // Unique incorrect match
            reference.set_unique_reference(ent);
            if parameters.is_empty() && sig.params.is_empty() {
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
                    diagnostic
                        .add_subprogram_candidates("Does not match", overloaded.sorted_entities());
                    diagnostics.push(diagnostic)
                };
            } else {
                // The analysis below will produce the diagnostics for the bad option
                self.analyze_parameters_with_formal_region(
                    pos,
                    &sig.params,
                    region,
                    parameters,
                    diagnostics,
                )?;
            }
            Ok(TypeCheck::NotOk)
        } else {
            // Found no function matching the target type
            if let (Some(ent), Some(target_type)) = (overloaded.as_unique(), target_type) {
                reference.set_unique_reference(ent);
                diagnostics.error(
                    pos,
                    format!("'{}' does not match {}", designator, target_type.describe()),
                )
            } else {
                let mut diagnostic =
                    Diagnostic::error(pos, format!("Could not resolve {}", designator.describe()));
                diagnostic
                    .add_subprogram_candidates("Does not match", overloaded.sorted_entities());
                diagnostics.push(diagnostic)
            }

            self.analyze_parameters(region, parameters, diagnostics)?;
            Ok(TypeCheck::NotOk)
        }
    }

    fn analyze_parameters_with_formal_region(
        &self,
        error_pos: &SrcPos, // The position of the instance/call-site
        formal_region: &FormalRegion,
        region: &Region<'_>,
        parameters: &mut ParametersMut<'_>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult<TypeCheck> {
        match parameters {
            ParametersMut::AssociationList(elems) => self.analyze_assoc_elems_with_formal_region(
                error_pos,
                formal_region,
                region,
                elems,
                diagnostics,
            ),
            ParametersMut::Unary(expr) => {
                if let Some(formal) = formal_region.nth(0) {
                    self.analyze_expression_with_target_type(
                        region,
                        formal.type_mark(),
                        &expr.pos,
                        &mut expr.item,
                        diagnostics,
                    )
                } else {
                    self.analyze_expression_pos(region, &expr.pos, &mut expr.item, diagnostics)?;
                    Ok(TypeCheck::NotOk)
                }
            }
        }
    }

    fn analyze_parameters(
        &self,
        region: &Region<'_>,
        parameters: &mut ParametersMut<'_>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        match parameters {
            ParametersMut::AssociationList(elems) => {
                self.analyze_assoc_elems(region, elems, diagnostics)
            }
            ParametersMut::Unary(expr) => {
                self.analyze_expression_pos(region, &expr.pos, &mut expr.item, diagnostics)
            }
        }
    }
}

// Allow us to handle functions and operators equally
pub enum ParametersMut<'a> {
    AssociationList(&'a mut [AssociationElement]),
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
            ParametersMut::Unary(expr) => {
                clear_references(*expr);
            }
        }
    }

    fn is_empty(&self) -> bool {
        match self {
            ParametersMut::AssociationList(list) => list.is_empty(),
            ParametersMut::Unary(_) => false,
        }
    }
}
