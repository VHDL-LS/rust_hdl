#![allow(clippy::only_used_in_recursion)]
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com
use super::analyze::*;
use super::named_entity::*;
use super::names::ResolvedName;
use super::overloaded::Disambiguated;
use super::region::*;
use crate::ast::Range;
use crate::ast::*;
use crate::data::*;

impl<'a> AnalyzeContext<'a> {
    pub fn resolve_non_overloaded_with_kind(
        &self,
        named_entities: NamedEntities<'a>,
        pos: &SrcPos,
        kind_ok: &impl Fn(&AnyEntKind) -> bool,
        expected: &str,
    ) -> AnalysisResult<EntRef<'a>> {
        let ent = self.resolve_non_overloaded(named_entities, pos, expected)?;
        if kind_ok(ent.actual_kind()) {
            Ok(ent)
        } else {
            Err(AnalysisError::NotFatal(ent.kind_error(pos, expected)))
        }
    }

    pub fn resolve_non_overloaded(
        &self,
        named_entities: NamedEntities<'a>,
        pos: &SrcPos,
        expected: &str,
    ) -> AnalysisResult<EntRef<'a>> {
        Ok(named_entities
            .expect_non_overloaded(pos, || format!("Expected {expected}, got overloaded name"))?)
    }

    pub fn resolve_type_mark_name(
        &self,
        scope: &Scope<'a>,
        type_mark: &mut WithPos<SelectedName>,
    ) -> AnalysisResult<TypeEnt<'a>> {
        let entities = self.resolve_selected_name(scope, type_mark)?;

        let pos = type_mark.suffix_pos();
        let expected = "type";
        let ent = self.resolve_non_overloaded(entities, pos, expected)?;
        TypeEnt::from_any(ent).ok_or_else(|| AnalysisError::NotFatal(ent.kind_error(pos, expected)))
    }

    pub fn resolve_type_mark(
        &self,
        scope: &Scope<'a>,
        type_mark: &mut WithPos<TypeMark>,
    ) -> AnalysisResult<TypeEnt<'a>> {
        if let Some(attr) = &type_mark.item.attr {
            let entities = self.resolve_selected_name(scope, &mut type_mark.item.name)?;

            let pos = type_mark.item.name.suffix_pos();

            let expected = if *attr == TypeAttribute::Element {
                "type, object or alias"
            } else {
                "object or alias"
            };

            let named_entity = self.resolve_non_overloaded(entities, pos, expected)?;

            let typ = match named_entity.kind() {
                AnyEntKind::Object(obj) => obj.subtype.type_mark(),
                AnyEntKind::ObjectAlias { type_mark, .. } => *type_mark,
                AnyEntKind::ElementDeclaration(subtype) => subtype.type_mark(),
                AnyEntKind::Type(_) if *attr == TypeAttribute::Element => {
                    TypeEnt::from_any(named_entity).unwrap()
                }
                _ => {
                    return Err(AnalysisError::NotFatal(
                        named_entity.kind_error(pos, expected),
                    ))
                }
            };

            match attr {
                TypeAttribute::Subtype => Ok(typ),
                TypeAttribute::Element => {
                    if let Some((elem_type, _)) = typ.array_type() {
                        Ok(elem_type)
                    } else {
                        Err(Diagnostic::error(
                            pos,
                            format!("array type expected for '{attr} attribute",),
                        )
                        .into())
                    }
                }
            }
        } else {
            self.resolve_type_mark_name(scope, &mut type_mark.item.name)
        }
    }

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
                if let Err(err) = self.resolve_type_mark_name(scope, type_mark) {
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
                if let Err(err) = self.resolve_type_mark_name(scope, type_mark) {
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

    pub fn analyze_choices(
        &self,
        scope: &Scope<'a>,
        choices: &mut [Choice],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        for choice in choices.iter_mut() {
            match choice {
                Choice::Expression(ref mut expr) => {
                    self.analyze_expression(scope, expr, diagnostics)?;
                }
                Choice::DiscreteRange(ref mut drange) => {
                    self.analyze_discrete_range(scope, drange, diagnostics)?;
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
                    self.analyze_expression_pos(scope, &actual.pos, expr, diagnostics)?;
                }
                ActualPart::Open => {}
            }
        }
        Ok(())
    }

    pub fn analyze_procedure_call(
        &self,
        scope: &Scope<'a>,
        fcall: &mut WithPos<CallOrIndexed>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        let CallOrIndexed { name, parameters } = &mut fcall.item;

        let resolved = match self.name_resolve(scope, &name.pos, &mut name.item, diagnostics) {
            Ok(resolved) => resolved,
            Err(err) => {
                diagnostics.push(err.into_non_fatal()?);
                // Continue checking missing names even if procedure is not found
                self.analyze_assoc_elems(scope, parameters, diagnostics)?;
                return Ok(());
            }
        };

        if let Some(entities) = resolved {
            match entities {
                ResolvedName::Overloaded(ref des, names) => {
                    let procedures: Vec<_> = names
                        .entities()
                        .filter(|name| name.is_procedure())
                        .collect();

                    if !procedures.is_empty() {
                        match self.disambiguate(
                            scope,
                            &fcall.pos,
                            des,
                            &mut fcall.item.parameters,
                            None,
                            procedures,
                            diagnostics,
                        )? {
                            Some(Disambiguated::Ambiguous(_)) => {
                                // @TODO ambiguous
                            }
                            Some(Disambiguated::Unambiguous(ent)) => {
                                fcall.item.name.set_unique_reference(&ent);
                            }
                            None => {}
                        }
                    } else {
                        let mut diagnostic = Diagnostic::error(&name.pos, "Invalid procedure call");
                        for ent in names.sorted_entities() {
                            if let Some(decl_pos) = ent.decl_pos() {
                                diagnostic.add_related(
                                    decl_pos,
                                    format!("{} is not a procedure", ent.describe()),
                                );
                            }
                        }
                        diagnostics.push(diagnostic);
                        self.analyze_assoc_elems(scope, parameters, diagnostics)?;
                    }
                }
                resolved => {
                    diagnostics.push(Diagnostic::error(
                        &name.pos,
                        format!("{} is not a procedure", resolved.describe_type()),
                    ));
                    self.analyze_assoc_elems(scope, parameters, diagnostics)?;
                }
            };
        } else {
            self.analyze_assoc_elems(scope, parameters, diagnostics)?;
        }
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
}

impl<'a> AnyEnt<'a> {
    pub(super) fn kind_error(&self, pos: &SrcPos, expected: &str) -> Diagnostic {
        let mut error = Diagnostic::error(
            pos,
            format!("Expected {}, got {}", expected, self.describe()),
        );
        if let Some(decl_pos) = self.decl_pos() {
            error.add_related(decl_pos, "Defined here");
        }
        error
    }
}

impl Diagnostic {
    pub(super) fn type_mismatch(pos: &SrcPos, desc: &str, expected_type: TypeEnt) -> Diagnostic {
        Diagnostic::error(
            pos,
            format!("{} does not match {}", desc, expected_type.describe()),
        )
    }

    pub(super) fn invalid_selected_name_prefix(
        named_entity: &AnyEnt,
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

    pub(super) fn no_declaration_within(
        named_entity: &AnyEnt,
        pos: &SrcPos,
        suffix: &Designator,
    ) -> Diagnostic {
        Diagnostic::error(
            pos,
            format!(
                "No declaration of '{}' within {}",
                suffix,
                named_entity.describe(),
            ),
        )
    }
}
