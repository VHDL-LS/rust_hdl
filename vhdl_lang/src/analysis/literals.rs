use fnv::FnvHashSet;

// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com
use super::analyze::*;
use super::scope::*;
use crate::analysis::static_expression::{bit_string_to_string, BitStringConversionError};
use crate::ast::*;
use crate::data::error_codes::ErrorCode;
use crate::data::*;
use crate::named_entity::*;
use crate::TokenSpan;

impl<'a> AnalyzeContext<'a, '_> {
    /// Analyze a string literal or expanded bit-string literal for type-matching
    fn analyze_string_literal(
        &self,
        span: TokenSpan,
        string_lit: Latin1String,
        target_base: TypeEnt<'_>,
        target_type: TypeEnt<'_>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) {
        if let Some((elem_type, literals)) = as_single_index_enum_array(target_base) {
            for chr in string_lit.chars() {
                let chr = Designator::Character(*chr);
                if !literals.contains(&chr) {
                    diagnostics.add(
                        span.pos(self.ctx),
                        format!("{} does not define character {}", elem_type.describe(), chr),
                        ErrorCode::InvalidLiteral,
                    );
                    break;
                }
            }
        } else {
            diagnostics.add(
                span.pos(self.ctx),
                format!("string literal does not match {}", target_type.describe()),
                ErrorCode::TypeMismatch,
            );
        }
    }

    /// Returns true if the name actually matches the target type
    /// None if it was uncertain
    pub fn analyze_literal_with_target_type(
        &self,
        scope: &Scope<'a>,
        target_type: TypeEnt<'a>,
        span: TokenSpan,
        literal: &mut Literal,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        let target_base = target_type.base_type();

        match literal {
            Literal::AbstractLiteral(abst) => match abst {
                AbstractLiteral::Integer(_) => {
                    if !self.can_be_target_type(self.universal_integer().into(), target_type.base())
                    {
                        diagnostics.add(
                            span.pos(self.ctx),
                            format!("integer literal does not match {}", target_type.describe()),
                            ErrorCode::TypeMismatch,
                        );
                    }
                }
                AbstractLiteral::Real(_) => {
                    if !self.can_be_target_type(self.universal_real().into(), target_type.base()) {
                        diagnostics.add(
                            span.pos(self.ctx),
                            format!("real literal does not match {}", target_type.describe()),
                            ErrorCode::TypeMismatch,
                        );
                    }
                }
            },
            Literal::Character(char) => match target_base.kind() {
                Type::Enum(literals) => {
                    if !literals.contains(&Designator::Character(*char)) {
                        diagnostics.add(
                            span.pos(self.ctx),
                            format!(
                                "character literal does not match {}",
                                target_type.describe()
                            ),
                            ErrorCode::TypeMismatch,
                        );
                    }
                }
                _ => {
                    diagnostics.add(
                        span.pos(self.ctx),
                        format!(
                            "character literal does not match {}",
                            target_type.describe()
                        ),
                        ErrorCode::TypeMismatch,
                    );
                }
            },
            Literal::String(string_lit) => {
                self.analyze_string_literal(
                    span,
                    string_lit.to_owned(),
                    target_base,
                    target_type,
                    diagnostics,
                );
            }
            Literal::BitString(bit_string) => {
                match bit_string_to_string(bit_string) {
                    Ok(string_lit) => self.analyze_string_literal(
                        span,
                        string_lit,
                        target_base,
                        target_type,
                        diagnostics,
                    ),
                    Err(err) => {
                        match err {
                            BitStringConversionError::IllegalDecimalCharacter(rel_pos) => {
                                diagnostics.add(
                                    span.pos(self.ctx),
                                    format!(
                                        "Illegal digit '{}' for base 10",
                                        bit_string.value.bytes[rel_pos] as char,
                                    ),
                                    ErrorCode::InvalidLiteral,
                                )
                            }
                            BitStringConversionError::IllegalTruncate(_, _) => {
                                diagnostics.add(
                                    span.pos(self.ctx),
                                    format!(
                                        "Truncating vector to length {} would lose information",
                                        bit_string.length.unwrap() // Safe as this error can only happen when there is a length
                                    ),
                                    ErrorCode::InvalidLiteral,
                                );
                            }
                            BitStringConversionError::EmptySignedExpansion => {
                                diagnostics.add(
                                    span.pos(self.ctx),
                                    "Cannot expand an empty signed bit string",
                                    ErrorCode::InvalidLiteral,
                                );
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
                                &span.pos(self.ctx),
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
                    diagnostics.add(
                        span.pos(self.ctx),
                        format!("null literal does not match {}", target_base.describe()),
                        ErrorCode::TypeMismatch,
                    );
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
        match scope
            .lookup(&Designator::Identifier(unit.item.item.clone()))
            .map_err(|err| err.into_diagnostic(self.ctx, unit.item.token))?
        {
            NamedEntities::Single(unit_ent) => {
                unit.set_unique_reference(unit_ent);
                if let AnyEntKind::PhysicalLiteral(physical_ent) = unit_ent.actual_kind() {
                    Ok(*physical_ent)
                } else {
                    Err(Diagnostic::new(
                        unit.item.pos(self.ctx),
                        format!("{} is not a physical unit", unit_ent.describe()),
                        ErrorCode::InvalidLiteral,
                    ))
                }
            }
            NamedEntities::Overloaded(_) => Err(Diagnostic::mismatched_kinds(
                unit.item.pos(self.ctx),
                "Overloaded name may not be physical unit",
            )),
        }
    }
}

/// Must be an array type with a single index of enum type
fn as_single_index_enum_array(typ: TypeEnt<'_>) -> Option<(TypeEnt<'_>, &FnvHashSet<Designator>)> {
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
