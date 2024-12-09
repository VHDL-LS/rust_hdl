//! This Source Code Form is subject to the terms of the Mozilla Public
//! License, v. 2.0. If a copy of the MPL was not distributed with this file,
//! You can obtain one at http://mozilla.org/MPL/2.0/.
//!
//! Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com
use super::analyze::*;
use super::names::ResolvedName;
use super::overloaded::Disambiguated;
use super::overloaded::SubprogramKind;
use super::scope::*;
use crate::ast::token_range::WithTokenSpan;
use crate::ast::*;
use crate::data::error_codes::ErrorCode;
use crate::data::*;
use crate::named_entity::*;

impl<'a> AnalyzeContext<'a, '_> {
    pub fn choices_with_ttyp(
        &self,
        scope: &Scope<'a>,
        ttyp: Option<TypeEnt<'a>>,
        choices: &mut [WithTokenSpan<Choice>],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        choices
            .iter_mut()
            .try_for_each(|choice| self.choice_with_ttyp(scope, ttyp, choice, diagnostics))
    }

    pub fn choice_with_ttyp(
        &self,
        scope: &Scope<'a>,
        ttyp: Option<TypeEnt<'a>>,
        choice: &mut WithTokenSpan<Choice>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        match choice.item {
            Choice::Expression(ref mut expr) => {
                // Check any names like `typ'range` before checking for any expressions
                if let Expression::Name(name) = expr {
                    if let Some(resolved_name) =
                        as_fatal(self.name_resolve(scope, choice.span, name, diagnostics))?
                    {
                        match resolved_name {
                            ResolvedName::Type(typ) => {
                                if let Some(ttyp) = ttyp {
                                    self.check_type_mismatch(typ, ttyp, choice.span, diagnostics);
                                }
                            }
                            _ => {
                                if let Some(ttyp) = ttyp {
                                    self.check_resolved_name_type(
                                        choice.span,
                                        &resolved_name,
                                        ttyp,
                                        name,
                                        diagnostics,
                                    );
                                }
                            }
                        }
                    }
                } else if let Some(ttyp) = ttyp {
                    self.expr_pos_with_ttyp(scope, ttyp, choice.span, expr, diagnostics)?;
                } else {
                    self.expr_pos_unknown_ttyp(scope, choice.span, expr, diagnostics)?;
                }
            }
            Choice::DiscreteRange(ref mut drange) => {
                if let Some(ttyp) = ttyp {
                    self.drange_with_ttyp(scope, ttyp, drange, diagnostics)?;
                } else {
                    self.drange_unknown_type(scope, drange, diagnostics)?;
                }
            }
            Choice::Others => {}
        }
        Ok(())
    }

    pub fn analyze_assoc_elems(
        &self,
        scope: &Scope<'a>,
        elems: &mut [AssociationElement],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        for AssociationElement { actual, .. } in elems.iter_mut() {
            match actual.item {
                ActualPart::Expression(ref mut expr) => {
                    self.expr_pos_unknown_ttyp(scope, actual.span, expr, diagnostics)?;
                }
                ActualPart::Open => {}
            }
        }
        Ok(())
    }

    pub fn analyze_procedure_call(
        &self,
        scope: &Scope<'a>,
        fcall: &mut WithTokenSpan<CallOrIndexed>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        let fcall_span = fcall.span;
        let CallOrIndexed { name, parameters } = &mut fcall.item;

        let resolved =
            match as_fatal(self.name_resolve(scope, name.span, &mut name.item, diagnostics))? {
                Some(resolved) => resolved,
                None => {
                    // Continue checking missing names even if procedure is not found
                    self.analyze_assoc_elems(scope, &mut parameters.items, diagnostics)?;
                    return Ok(());
                }
            };

        match resolved {
            ResolvedName::Overloaded(ref des, names) => {
                match as_fatal(self.disambiguate(
                    scope,
                    &fcall_span.pos(self.ctx),
                    des,
                    &mut parameters.items,
                    SubprogramKind::Procedure,
                    names.entities().collect(),
                    diagnostics,
                ))? {
                    Some(Disambiguated::Ambiguous(candidates)) => {
                        diagnostics.push(Diagnostic::ambiguous_call(self.ctx, des, candidates))
                    }
                    Some(Disambiguated::Unambiguous(ent)) => {
                        name.set_unique_reference(&ent);

                        if !ent.is_procedure() {
                            let mut diagnostic = Diagnostic::new(
                                name.pos(self.ctx),
                                "Invalid procedure call",
                                ErrorCode::InvalidCall,
                            );
                            for ent in names.sorted_entities() {
                                if let Some(decl_pos) = ent.decl_pos() {
                                    diagnostic.add_related(
                                        decl_pos,
                                        format!("{} is not a procedure", ent.describe()),
                                    );
                                }
                            }
                            diagnostics.push(diagnostic);
                        } else if ent.is_uninst_subprogram_body() {
                            diagnostics.add(
                                name.pos(self.ctx),
                                format!("uninstantiated {} cannot be called", ent.describe()),
                                ErrorCode::InvalidCall,
                            )
                        }
                    }
                    None => {}
                }
            }
            ResolvedName::Final(ent) => {
                if let AnyEntKind::Component(region) = ent.kind() {
                    name.set_unique_reference(ent);
                    let (generic_region, port_region) = region.to_entity_formal();
                    self.check_association(
                        &fcall.item.name.pos(self.ctx),
                        &generic_region,
                        scope,
                        &mut [],
                        diagnostics,
                    )?;
                    self.check_association(
                        &fcall.item.name.pos(self.ctx),
                        &port_region,
                        scope,
                        &mut [],
                        diagnostics,
                    )?;
                } else {
                    diagnostics.add(
                        name.pos(self.ctx),
                        format!("{} is not a procedure", resolved.describe_type()),
                        ErrorCode::MismatchedKinds,
                    );
                    self.analyze_assoc_elems(scope, &mut parameters.items, diagnostics)?;
                }
            }
            resolved => {
                diagnostics.add(
                    name.pos(self.ctx),
                    format!("{} is not a procedure", resolved.describe_type()),
                    ErrorCode::MismatchedKinds,
                );
                self.analyze_assoc_elems(scope, &mut parameters.items, diagnostics)?;
            }
        };

        Ok(())
    }
}

impl Diagnostic {
    pub fn add_subprogram_candidates<'a>(
        &mut self,
        prefix: &str,
        candidates: impl IntoIterator<Item = OverloadedEnt<'a>>,
    ) {
        let mut candidates: Vec<_> = candidates.into_iter().collect();
        candidates.sort_by(|x, y| x.decl_pos().cmp(&y.decl_pos()));

        for ent in candidates {
            if let Some(decl_pos) = ent.decl_pos() {
                self.add_related(decl_pos, format!("{} {}", prefix, ent.describe()))
            }
        }
    }

    pub fn add_type_candidates<'a>(
        &mut self,
        prefix: &str,
        candidates: impl IntoIterator<Item = BaseType<'a>>,
    ) {
        let mut candidates: Vec<_> = candidates.into_iter().collect();
        candidates.sort_by(|x, y| x.decl_pos().cmp(&y.decl_pos()));

        for ent in candidates {
            if let Some(decl_pos) = ent.decl_pos() {
                self.add_related(decl_pos, format!("{} {}", prefix, ent.describe()))
            }
        }
    }
}

impl ResolvedName<'_> {
    pub(super) fn kind_error(&self, pos: impl AsRef<SrcPos>, expected: &str) -> Diagnostic {
        let mut error = Diagnostic::mismatched_kinds(
            pos,
            format!("Expected {expected}, got {}", self.describe()),
        );
        if let Some(decl_pos) = self.decl_pos() {
            error.add_related(decl_pos, "Defined here");
        }
        error
    }
}

impl Diagnostic {
    pub(crate) fn type_mismatch(
        pos: &SrcPos,
        desc: &str,
        expected_type: TypeEnt<'_>,
    ) -> Diagnostic {
        Diagnostic::new(
            pos,
            format!("{} does not match {}", desc, expected_type.describe(),),
            ErrorCode::TypeMismatch,
        )
    }

    pub(crate) fn invalid_selected_name_prefix(
        named_entity: EntRef<'_>,
        prefix: &SrcPos,
    ) -> Diagnostic {
        Diagnostic::mismatched_kinds(
            prefix,
            capitalize(&format!(
                "{} may not be the prefix of a selected name",
                named_entity.describe(),
            )),
        )
    }

    pub(crate) fn no_declaration_within(
        named_entity: EntRef<'_>,
        pos: &SrcPos,
        suffix: &Designator,
    ) -> Diagnostic {
        Diagnostic::new(
            pos,
            format!(
                "No declaration of '{}' within {}",
                suffix,
                named_entity.describe(),
            ),
            ErrorCode::Unresolved,
        )
    }
}
