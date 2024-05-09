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

impl<'a, 't> AnalyzeContext<'a, 't> {
    pub fn resolve_type_mark(
        &self,
        scope: &Scope<'a>,
        type_mark: &mut WithTokenSpan<TypeMark>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<TypeEnt<'a>> {
        let name = self.name_resolve(
            scope,
            type_mark.item.name.span,
            &mut type_mark.item.name.item,
            diagnostics,
        )?;

        if let Some(attr) = &type_mark.item.attr {
            let span = type_mark.item.name.suffix_pos();

            let typ = match name {
                ResolvedName::Type(typ) if *attr == TypeAttribute::Element => typ,
                ResolvedName::ObjectName(obj) => obj.type_mark(),
                other => {
                    let mut diag = Diagnostic::new(
                        type_mark.pos(self.ctx),
                        format!("Expected type, got {}", other.describe()),
                        ErrorCode::MismatchedKinds,
                    );
                    if let Some(pos) = other.decl_pos() {
                        diag.add_related(pos, "Defined here");
                    }
                    diagnostics.push(diag);
                    return Err(EvalError::Unknown);
                }
            };

            match attr {
                TypeAttribute::Subtype => Ok(typ),
                TypeAttribute::Element => {
                    if let Some((elem_type, _)) = typ.array_type() {
                        Ok(elem_type)
                    } else {
                        diagnostics.add(
                            span.pos(self.ctx),
                            format!("array type expected for '{attr} attribute",),
                            ErrorCode::TypeMismatch,
                        );
                        Err(EvalError::Unknown)
                    }
                }
            }
        } else {
            match name {
                ResolvedName::Type(typ) => Ok(typ),
                other => {
                    let mut diag = Diagnostic::new(
                        type_mark.pos(self.ctx),
                        format!("Expected type, got {}", other.describe()),
                        ErrorCode::MismatchedKinds,
                    );
                    if let Some(pos) = other.decl_pos() {
                        diag.add_related(pos, "Defined here");
                    }
                    diagnostics.push(diag);
                    Err(EvalError::Unknown)
                }
            }
        }
    }

    pub fn choice_with_ttyp(
        &self,
        scope: &Scope<'a>,
        ttyp: Option<TypeEnt<'a>>,
        choices: &mut [WithTokenSpan<Choice>],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        for choice in choices.iter_mut() {
            match choice.item {
                Choice::Expression(ref mut expr) => {
                    if let Some(ttyp) = ttyp {
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
                    self.analyze_assoc_elems(scope, parameters, diagnostics)?;
                    return Ok(());
                }
            };

        match resolved {
            ResolvedName::Overloaded(ref des, names) => {
                match as_fatal(self.disambiguate(
                    scope,
                    &fcall_span.pos(self.ctx),
                    des,
                    parameters,
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
                                &name.pos(self.ctx),
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
                                &name.pos(self.ctx),
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
                        &name.pos(self.ctx),
                        format!("{} is not a procedure", resolved.describe_type()),
                        ErrorCode::MismatchedKinds,
                    );
                    self.analyze_assoc_elems(scope, parameters, diagnostics)?;
                }
            }
            resolved => {
                diagnostics.add(
                    &name.pos(self.ctx),
                    format!("{} is not a procedure", resolved.describe_type()),
                    ErrorCode::MismatchedKinds,
                );
                self.analyze_assoc_elems(scope, parameters, diagnostics)?;
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

    pub fn add_type_candididates<'a>(
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

impl<'a> ResolvedName<'a> {
    pub(super) fn kind_error(&self, pos: impl AsRef<SrcPos>, expected: &str) -> Diagnostic {
        let mut error = Diagnostic::new(
            pos,
            format!("Expected {}, got {}", expected, self.describe()),
            ErrorCode::MismatchedKinds,
        );
        if let Some(decl_pos) = self.decl_pos() {
            error.add_related(decl_pos, "Defined here");
        }
        error
    }
}

impl Diagnostic {
    pub(crate) fn type_mismatch(pos: &SrcPos, desc: &str, expected_type: TypeEnt) -> Diagnostic {
        Diagnostic::new(
            pos,
            format!("{} does not match {}", desc, expected_type.describe(),),
            ErrorCode::TypeMismatch,
        )
    }

    pub(crate) fn invalid_selected_name_prefix(
        named_entity: &AnyEnt,
        prefix: &SrcPos,
    ) -> Diagnostic {
        Diagnostic::new(
            prefix,
            capitalize(&format!(
                "{} may not be the prefix of a selected name",
                named_entity.describe(),
            )),
            ErrorCode::MismatchedKinds,
        )
    }

    pub(crate) fn no_declaration_within(
        named_entity: &AnyEnt,
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
