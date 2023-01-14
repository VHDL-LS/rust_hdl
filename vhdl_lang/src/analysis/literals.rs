use fnv::FnvHashSet;

// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com
use super::analyze::*;
use super::named_entity::*;
use super::region::*;
use crate::ast::*;
use crate::data::*;

impl<'a> AnalyzeContext<'a> {
    /// Returns true if the name actually matches the target type
    /// None if it was uncertain
    pub fn analyze_literal_with_target_type(
        &self,
        scope: &Scope<'a>,
        target_type: TypeEnt<'a>,
        pos: &SrcPos,
        literal: &mut Literal,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult<bool> {
        let target_base = target_type.base_type();

        let is_correct = match literal {
            Literal::AbstractLiteral(abst) => match abst {
                AbstractLiteral::Integer(_) => {
                    let is_correct = matches!(target_base.kind(), Type::Integer(..));

                    if !is_correct {
                        diagnostics.push(Diagnostic::error(
                            pos,
                            format!("integer literal does not match {}", target_type.describe()),
                        ));
                    }
                    is_correct
                }
                AbstractLiteral::Real(_) => {
                    let is_correct = matches!(target_base.kind(), Type::Real(..));

                    if !is_correct {
                        diagnostics.push(Diagnostic::error(
                            pos,
                            format!("real literal does not match {}", target_type.describe()),
                        ));
                    }
                    is_correct
                }
            },
            Literal::Character(char) => match target_base.kind() {
                Type::Enum(_, literals) => {
                    if literals.contains(&Designator::Character(*char)) {
                        true
                    } else {
                        diagnostics.push(Diagnostic::error(
                            pos,
                            format!(
                                "character literal does not match {}",
                                target_type.describe()
                            ),
                        ));
                        false
                    }
                }
                _ => {
                    diagnostics.push(Diagnostic::error(
                        pos,
                        format!(
                            "character literal does not match {}",
                            target_type.describe()
                        ),
                    ));
                    false
                }
            },
            Literal::String(string_lit) => {
                if let Some((elem_type, literals)) = as_single_index_enum_array(target_base) {
                    let mut is_correct = true;
                    for chr in string_lit.chars() {
                        let chr = Designator::Character(*chr);
                        if !literals.contains(&chr) {
                            is_correct = false;
                            diagnostics.push(Diagnostic::error(
                                pos,
                                format!(
                                    "{} does not define character {}",
                                    elem_type.describe(),
                                    chr
                                ),
                            ))
                        }
                    }
                    is_correct
                } else {
                    diagnostics.push(Diagnostic::error(
                        pos,
                        format!("string literal does not match {}", target_type.describe()),
                    ));
                    false
                }
            }
            Literal::Physical(PhysicalLiteral { ref mut unit, .. }) => {
                match self.resolve_physical_unit(scope, unit) {
                    Ok(physical_type) => physical_type.base_type() == target_base,
                    Err(diagnostic) => {
                        diagnostics.push(diagnostic);
                        false
                    }
                }
            }
            Literal::BitString(bitstring) => {
                if let Some((elem_type, literals)) = as_single_index_enum_array(target_base) {
                    let needs_1 = bitstring.value.chars().any(|c| *c != b'0');
                    let c0 = Designator::Character(b'0');
                    let c1 = Designator::Character(b'1');

                    let mut is_correct = true;
                    if !literals.contains(&c0) {
                        is_correct = false;
                        diagnostics.push(Diagnostic::error(
                            pos,
                            format!(
                                "element {} of {} does not define character {}",
                                elem_type.describe(),
                                target_type.describe(),
                                c0
                            ),
                        ))
                    }

                    if needs_1 && !literals.contains(&c1) {
                        is_correct = false;
                        diagnostics.push(Diagnostic::error(
                            pos,
                            format!(
                                "element {} of {} does not define character {}",
                                elem_type.describe(),
                                target_type.describe(),
                                c1
                            ),
                        ))
                    }
                    is_correct
                } else {
                    diagnostics.push(Diagnostic::error(
                        pos,
                        format!(
                            "bit string literal does not match {}",
                            target_type.describe()
                        ),
                    ));
                    false
                }
            }
            Literal::Null => {
                if let Type::Access(_, _) = target_base.kind() {
                    true
                } else {
                    diagnostics.push(Diagnostic::error(
                        pos,
                        format!("null literal does not match {}", target_base.describe()),
                    ));
                    false
                }
            }
        };

        Ok(is_correct)
    }

    pub fn resolve_physical_unit(
        &self,
        scope: &Scope<'a>,
        unit: &mut WithRef<Ident>,
    ) -> Result<TypeEnt, Diagnostic> {
        match scope.lookup(
            &unit.item.pos,
            &Designator::Identifier(unit.item.item.clone()),
        )? {
            NamedEntities::Single(unit_ent) => {
                unit.set_unique_reference(unit_ent);
                if let AnyEntKind::PhysicalLiteral(physical_ent) = unit_ent.actual_kind() {
                    Ok(*physical_ent)
                } else {
                    Err(Diagnostic::error(
                        &unit.item.pos,
                        format!("{} is not a physical unit", unit_ent.describe()),
                    ))
                }
            }
            NamedEntities::Overloaded(_) => Err(Diagnostic::error(
                &unit.item.pos,
                "Overloaded name may not be physical unit",
            )),
        }
    }
}

/// Must be an array type with a single index of enum type
fn as_single_index_enum_array(typ: TypeEnt) -> Option<(TypeEnt, &FnvHashSet<Designator>)> {
    if let Type::Array {
        indexes, elem_type, ..
    } = typ.kind()
    {
        if indexes.len() == 1 {
            if let Type::Enum(_, literals) = elem_type.base_type().kind() {
                return Some((*elem_type, literals));
            }
        }
    }
    None
}
