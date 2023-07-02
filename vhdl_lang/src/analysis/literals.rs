use fnv::FnvHashSet;

// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com
use super::analyze::*;
use super::named_entity::*;
use super::region::*;
use crate::analysis::static_expression::{bit_string_to_string, BitStringConversionError};
use crate::ast::*;
use crate::data::*;

impl<'a> AnalyzeContext<'a> {
    /// Analyze a string literal or expanded bit-string literal for type-matching
    fn analyze_string_literal(
        &self,
        pos: &SrcPos,
        string_lit: Latin1String,
        target_base: TypeEnt,
        target_type: TypeEnt,
        diagnostics: &mut dyn DiagnosticHandler,
    ) {
        if let Some((elem_type, literals)) = as_single_index_enum_array(target_base) {
            for chr in string_lit.chars() {
                let chr = Designator::Character(*chr);
                if !literals.contains(&chr) {
                    diagnostics.push(Diagnostic::error(
                        pos,
                        format!("{} does not define character {}", elem_type.describe(), chr),
                    ));
                    break;
                }
            }
        } else {
            diagnostics.push(Diagnostic::error(
                pos,
                format!("string literal does not match {}", target_type.describe()),
            ));
        }
    }

    /// Returns true if the name actually matches the target type
    /// None if it was uncertain
    pub fn analyze_literal_with_target_type(
        &self,
        scope: &Scope<'a>,
        target_type: TypeEnt<'a>,
        pos: &SrcPos,
        literal: &mut Literal,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        let target_base = target_type.base_type();

        match literal {
            Literal::AbstractLiteral(abst) => match abst {
                AbstractLiteral::Integer(_) => {
                    if !self.can_be_target_type(self.universal_integer().into(), target_type.base())
                    {
                        diagnostics.push(Diagnostic::error(
                            pos,
                            format!("integer literal does not match {}", target_type.describe()),
                        ));
                    }
                }
                AbstractLiteral::Real(_) => {
                    if !self.can_be_target_type(self.universal_real().into(), target_type.base()) {
                        diagnostics.push(Diagnostic::error(
                            pos,
                            format!("real literal does not match {}", target_type.describe()),
                        ));
                    }
                }
            },
            Literal::Character(char) => match target_base.kind() {
                Type::Enum(literals) => {
                    if !literals.contains(&Designator::Character(*char)) {
                        diagnostics.push(Diagnostic::error(
                            pos,
                            format!(
                                "character literal does not match {}",
                                target_type.describe()
                            ),
                        ));
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
                }
            },
            Literal::String(string_lit) => {
                self.analyze_string_literal(
                    pos,
                    string_lit.to_owned(),
                    target_base,
                    target_type,
                    diagnostics,
                );
            }
            Literal::BitString(bit_string) => {
                match bit_string_to_string(bit_string) {
                    Ok(string_lit) => self.analyze_string_literal(
                        pos,
                        string_lit,
                        target_base,
                        target_type,
                        diagnostics,
                    ),
                    Err(err) => {
                        match err {
                            BitStringConversionError::IllegalDecimalCharacter(rel_pos) => {
                                diagnostics.error(
                                    pos,
                                    format!(
                                        "Illegal digit '{}' for base 10",
                                        bit_string.value.bytes[rel_pos] as char,
                                    ),
                                )
                            }
                            BitStringConversionError::IllegalTruncate(_, _) => {
                                diagnostics.error(
                                    pos,
                                    format!(
                                        "Truncating vector to length {} would lose information",
                                        bit_string.length.unwrap() // Safe as this error can only happen when there is a length
                                    ),
                                );
                            }
                            BitStringConversionError::EmptySignedExpansion => {
                                diagnostics.error(pos, "Cannot expand an empty signed bit string");
                            }
                        }
                    }
                }
            }
            Literal::Physical(PhysicalLiteral { ref mut unit, .. }) => {
                match self.resolve_physical_unit(scope, unit) {
                    Ok(physical_type) => {
                        if physical_type.base_type() != target_base {
                            diagnostics.push(Diagnostic::type_mismatch(
                                pos,
                                &physical_type.describe(),
                                target_type,
                            ))
                        }
                    }
                    Err(diagnostic) => {
                        diagnostics.push(diagnostic);
                    }
                }
            }
            Literal::Null => {
                if !matches!(target_base.kind(), Type::Access(_)) {
                    diagnostics.push(Diagnostic::error(
                        pos,
                        format!("null literal does not match {}", target_base.describe()),
                    ));
                }
            }
        };
        Ok(())
    }

    pub fn resolve_physical_unit(
        &self,
        scope: &Scope<'a>,
        unit: &mut WithRef<Ident>,
    ) -> Result<TypeEnt<'a>, Diagnostic> {
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
            if let Type::Enum(literals) = elem_type.base_type().kind() {
                return Some((*elem_type, literals));
            }
        }
    }
    None
}
