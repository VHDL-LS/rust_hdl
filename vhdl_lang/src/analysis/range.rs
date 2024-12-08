//! This Source Code Form is subject to the terms of the Mozilla Public
//! License, v. 2.0. If a copy of the MPL was not distributed with this file,
//! You can obtain one at http://mozilla.org/MPL/2.0/.
//!
//! Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use super::analyze::*;
use super::expression::ExpressionType;
use super::names::AttributeSuffix;
use super::names::ResolvedName;
use super::overloaded::Disambiguated;
use super::overloaded::DisambiguatedType;
use super::scope::*;
use crate::ast::token_range::WithTokenSpan;
use crate::ast::Range;
use crate::ast::*;
use crate::data::error_codes::ErrorCode;
use crate::data::*;
use crate::named_entity::*;

impl<'a> AnalyzeContext<'a, '_> {
    pub fn range_unknown_typ(
        &self,
        scope: &Scope<'a>,
        range: &mut Range,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        as_fatal(self.range_type(scope, range, diagnostics))?;
        Ok(())
    }

    pub fn drange_unknown_type(
        &self,
        scope: &Scope<'a>,
        drange: &mut DiscreteRange,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        as_fatal(self.drange_type(scope, drange, diagnostics))?;
        Ok(())
    }

    fn range_expr_type(
        &self,
        scope: &Scope<'a>,
        expr: &mut WithTokenSpan<Expression>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<DisambiguatedType<'a>> {
        match self.expr_type(scope, expr, diagnostics)? {
            ExpressionType::Unambiguous(typ) => {
                if typ.base().is_scalar() {
                    Ok(DisambiguatedType::Unambiguous(typ))
                } else {
                    diagnostics.add(
                        expr.pos(self.ctx),
                        format!("Non-scalar {} cannot be used in a range", typ.describe()),
                        ErrorCode::NonScalarInRange,
                    );
                    Err(EvalError::Unknown)
                }
            }
            ExpressionType::Ambiguous(types) => Ok(DisambiguatedType::Ambiguous(
                types.into_iter().filter(|typ| typ.is_scalar()).collect(),
            )),
            ExpressionType::String | ExpressionType::Null | ExpressionType::Aggregate => {
                diagnostics.add(
                    expr.pos(self.ctx),
                    "Non-scalar expression cannot be used in a range",
                    ErrorCode::NonScalarInRange,
                );
                Err(EvalError::Unknown)
            }
        }
    }

    pub fn range_attribute_type(
        &self,
        scope: &Scope<'a>,
        attr: &mut AttributeName,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<BaseType<'a>> {
        let resolved =
            self.name_resolve(scope, attr.name.span, &mut attr.name.item, diagnostics)?;
        let typ = match resolved {
            ResolvedName::Type(typ) => typ,
            ResolvedName::ObjectName(oname) => oname.type_mark(),
            ResolvedName::Overloaded(ref des, ref overloaded) => {
                let disamb = self
                    .disambiguate_no_actuals(des, None, overloaded)
                    .into_eval_result(diagnostics)?;

                if let Some(disamb) = disamb {
                    if let Disambiguated::Unambiguous(ref ent) = disamb {
                        attr.name.set_unique_reference(ent);
                        ent.return_type().unwrap()
                    } else {
                        diagnostics.add(
                            attr.name.pos(self.ctx),
                            format!(
                            "{} cannot be prefix of range attribute, array type or object is required",
                            resolved.describe()
                        ),
                            ErrorCode::MismatchedKinds,
                    );
                        return Err(EvalError::Unknown);
                    }
                } else {
                    return Err(EvalError::Unknown);
                }
            }
            ResolvedName::Expression(DisambiguatedType::Unambiguous(typ)) => typ,
            ResolvedName::Expression(_)
            | ResolvedName::Final(_)
            | ResolvedName::Library(_)
            | ResolvedName::Design(_) => {
                diagnostics.add(
                    attr.name.pos(self.ctx),
                    format!(
                        "{} cannot be prefix of range attribute, array type or object is required",
                        resolved.describe()
                    ),
                    ErrorCode::MismatchedKinds,
                );
                return Err(EvalError::Unknown);
            }
        };

        if let Some((_, indexes)) = typ.array_type() {
            if let Some(first) = indexes.first() {
                if let Some(base_type) = first {
                    Ok(*base_type)
                } else {
                    // There was probably an error in the type definition of this signal/variable
                    Err(EvalError::Unknown)
                }
            } else {
                // This should never happen
                if let Some(decl_pos) = typ.decl_pos() {
                    // To debug if it ever happens
                    eprintln!("{}", decl_pos.show("Array with no indexes"));
                    eprintln!("{}", attr.name.pos(self.ctx).show("Used here"));
                    panic!("Internal error")
                }
                Err(EvalError::Unknown)
            }
        } else {
            diagnostics.add(
                attr.name.pos(self.ctx),
                format!(
                    "{} cannot be prefix of range attribute, array type or object is required",
                    resolved.describe()
                ),
                ErrorCode::MismatchedKinds,
            );
            Err(EvalError::Unknown)
        }
    }

    pub fn range_type(
        &self,
        scope: &Scope<'a>,
        range: &mut Range,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<BaseType<'a>> {
        match range {
            Range::Range(ref mut constraint) => {
                let left_types =
                    self.range_expr_type(scope, &mut constraint.left_expr, diagnostics)?;
                let right_types =
                    self.range_expr_type(scope, &mut constraint.right_expr, diagnostics)?;

                let left_ambig = matches!(left_types, DisambiguatedType::Ambiguous(_));
                let right_ambig = matches!(right_types, DisambiguatedType::Ambiguous(_));

                let types = match (left_types, right_types) {
                    (DisambiguatedType::Unambiguous(l), DisambiguatedType::Unambiguous(r)) => {
                        return if let Some(typ) = self.common_type(l.base(), r.base()) {
                            Ok(typ)
                        } else {
                            diagnostics.add(
                                constraint.span().pos(self.ctx),
                                format!(
                                    "Range type mismatch, left is {}, right is {}",
                                    l.base().describe(),
                                    r.base().describe()
                                ),
                                ErrorCode::TypeMismatch,
                            );
                            Err(EvalError::Unknown)
                        }
                    }
                    (DisambiguatedType::Unambiguous(l), DisambiguatedType::Ambiguous(r)) => {
                        self.common_types(r, l.base())
                    }

                    (DisambiguatedType::Ambiguous(l), DisambiguatedType::Unambiguous(r)) => {
                        self.common_types(l, r.base())
                    }
                    (DisambiguatedType::Ambiguous(_), DisambiguatedType::Ambiguous(_)) => {
                        diagnostics.add(
                            constraint.span().pos(self.ctx),
                            "Range is ambiguous",
                            ErrorCode::TypeMismatch,
                        );
                        return Err(EvalError::Unknown);
                    }
                };

                if types.len() == 1 {
                    let typ = types.into_iter().next().unwrap();

                    if left_ambig {
                        self.expr_with_ttyp(
                            scope,
                            typ.into(),
                            &mut constraint.left_expr,
                            diagnostics,
                        )?;
                    }

                    if right_ambig {
                        self.expr_with_ttyp(
                            scope,
                            typ.into(),
                            &mut constraint.right_expr,
                            diagnostics,
                        )?;
                    }

                    Ok(typ)
                } else if types.is_empty() {
                    diagnostics.add(
                        constraint.span().pos(self.ctx),
                        "Range type of left and right side does not match",
                        ErrorCode::TypeMismatch,
                    );
                    Err(EvalError::Unknown)
                } else {
                    diagnostics.add(
                        constraint.span().pos(self.ctx),
                        "Range is ambiguous",
                        ErrorCode::TypeMismatch,
                    );
                    Err(EvalError::Unknown)
                }
            }
            Range::Attribute(ref mut attr) => self.range_attribute_type(scope, attr, diagnostics),
        }
    }

    pub fn drange_type(
        &self,
        scope: &Scope<'a>,
        drange: &mut DiscreteRange,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<BaseType<'a>> {
        let typ = match drange {
            DiscreteRange::Discrete(ref mut type_mark, ref mut range) => {
                let typ = self
                    .type_name(scope, type_mark.span, &mut type_mark.item, diagnostics)?
                    .base();
                if let Some(ref mut range) = range {
                    self.range_with_ttyp(scope, typ.into(), range, diagnostics)?;
                }
                typ
            }
            DiscreteRange::Range(ref mut range) => self.range_type(scope, range, diagnostics)?,
        };

        if typ.is_discrete() {
            Ok(typ)
        } else {
            diagnostics.add(
                drange.span().pos(self.ctx),
                format!(
                    "Non-discrete {} cannot be used in discrete range",
                    typ.describe()
                ),
                ErrorCode::MismatchedKinds,
            );
            Err(EvalError::Unknown)
        }
    }

    pub fn range_with_ttyp(
        &self,
        scope: &Scope<'a>,
        target_type: TypeEnt<'a>,
        range: &mut Range,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        match range {
            Range::Range(ref mut constraint) => {
                self.expr_pos_with_ttyp(
                    scope,
                    target_type,
                    constraint.left_expr.span,
                    &mut constraint.left_expr.item,
                    diagnostics,
                )?;
                self.expr_pos_with_ttyp(
                    scope,
                    target_type,
                    constraint.right_expr.span,
                    &mut constraint.right_expr.item,
                    diagnostics,
                )?;
            }
            Range::Attribute(ref mut name) => {
                let AttributeName {
                    name,
                    signature,
                    expr,
                    attr, // Parser ensures this must be 'range or we would not end up here
                } = name.as_mut();

                let prefix_typ = as_fatal(
                    self.name_resolve(scope, name.span, &mut name.item, diagnostics)
                        .and_then(|prefix| {
                            prefix.as_type_of_attr_prefix(
                                self.ctx,
                                name.span,
                                &AttributeSuffix { attr, expr },
                                diagnostics,
                            )
                        }),
                )?;

                if let Some(ref mut signature) = signature {
                    diagnostics.add(
                        signature.pos(self.ctx),
                        format!("Did not expect signature for '{attr} attribute"),
                        ErrorCode::UnexpectedSignature,
                    );
                }

                if let Some(prefix_typ) = prefix_typ {
                    if let Some((_, indexes)) = prefix_typ.array_type() {
                        if let Some(index_typ) =
                            as_fatal(self.array_index_expression_in_attribute(
                                indexes,
                                expr.as_mut().map(|expr| expr.as_mut()),
                                diagnostics,
                            ))?
                        {
                            if !self.can_be_target_type(index_typ.into(), target_type.base()) {
                                diagnostics.push(Diagnostic::type_mismatch(
                                    &range.span().pos(self.ctx),
                                    &index_typ.describe(),
                                    target_type,
                                ))
                            }
                        }
                    }
                }
            }
        }
        Ok(())
    }

    pub fn drange_with_ttyp(
        &self,
        scope: &Scope<'a>,
        target_type: TypeEnt<'a>,
        drange: &mut DiscreteRange,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        match drange {
            DiscreteRange::Discrete(ref mut type_mark, ref mut range) => {
                let _ = as_fatal(self.name_resolve(
                    scope,
                    type_mark.span,
                    &mut type_mark.item,
                    diagnostics,
                ))?;
                if let Some(ref mut range) = range {
                    self.range_with_ttyp(scope, target_type, range, diagnostics)?;
                }
            }
            DiscreteRange::Range(ref mut range) => {
                self.range_with_ttyp(scope, target_type, range, diagnostics)?;
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::analysis::analyze::EvalError;
    use crate::analysis::analyze::EvalResult;
    use crate::analysis::tests::TestSetup;
    use crate::ast::search::check_no_unresolved;
    use crate::ast::Range;
    use crate::data::error_codes::ErrorCode;
    use crate::data::DiagnosticHandler;
    use crate::data::NoDiagnostics;
    use crate::named_entity::BaseType;
    use crate::syntax::test::check_diagnostics;
    use crate::syntax::test::Code;
    use crate::Diagnostic;
    use vhdl_lang::Token;

    impl<'a> TestSetup<'a> {
        fn range_type(
            &'a self,
            code: &Code,
            diagnostics: &mut dyn DiagnosticHandler,
        ) -> EvalResult<BaseType<'a>> {
            self.ctx(&code.tokenize())
                .range_type(&self.scope, &mut code.range(), diagnostics)
        }

        fn range_type_ast(
            &'a self,
            rng: &mut Range,
            tokens: &Vec<Token>,
            diagnostics: &mut dyn DiagnosticHandler,
        ) -> EvalResult<BaseType<'a>> {
            self.ctx(tokens).range_type(&self.scope, rng, diagnostics)
        }
    }

    #[test]
    fn universal_integer_range() {
        let test = TestSetup::new();

        let code = test.snippet("0 to 1");
        assert_eq!(
            test.range_type(&code, &mut NoDiagnostics),
            Ok(test.ctx(&code.tokenize()).universal_integer())
        );
    }

    #[test]
    fn universal_integer_range_expression() {
        let test = TestSetup::new();

        let code = test.snippet("-1 to 1");
        assert_eq!(
            test.range_type(&code, &mut NoDiagnostics),
            Ok(test.ctx(&code.tokenize()).universal_integer())
        );
    }

    #[test]
    fn character_range() {
        let test = TestSetup::new();

        let code = test.snippet("'a' to 'b'");
        assert_eq!(
            test.range_type(&code, &mut NoDiagnostics),
            Ok(test.lookup_type("CHARACTER").base())
        );
    }

    #[test]
    fn discrete_range_not_discrete_type() {
        let test = TestSetup::new();
        let code = test.snippet("0.0 to 1.0");
        let mut diagnostics = Vec::new();
        assert_eq!(
            test.ctx(&code.tokenize()).drange_type(
                &test.scope,
                &mut code.discrete_range(),
                &mut diagnostics
            ),
            Err(EvalError::Unknown)
        );

        check_diagnostics(
            diagnostics,
            vec![Diagnostic::mismatched_kinds(
                code.s1("0.0 to 1.0"),
                "Non-discrete type universal_real cannot be used in discrete range",
            )],
        )
    }

    #[test]
    fn range_not_discrete_expr() {
        let test = TestSetup::new();
        let code = test.snippet("0 to (0, 0)");
        let mut diagnostics = Vec::new();
        assert_eq!(
            test.range_type(&code, &mut diagnostics),
            Err(EvalError::Unknown)
        );

        check_diagnostics(
            diagnostics,
            vec![Diagnostic::new(
                code.s1("(0, 0)"),
                "Non-scalar expression cannot be used in a range",
                ErrorCode::NonScalarInRange,
            )],
        )
    }

    #[test]
    fn range_one_side_ambiguous() {
        let test = TestSetup::new();

        test.declarative_part(
            "
function f1 return character;
function f1 return integer;
        ",
        );

        let code = test.snippet("f1 to 'a'");
        let mut rng = code.range();
        assert_eq!(
            test.range_type_ast(&mut rng, &code.tokenize(), &mut NoDiagnostics),
            Ok(test.lookup_type("CHARACTER").base())
        );
        check_no_unresolved(&mut rng, &code.tokenize());

        let code = test.snippet("'a' to f1");
        let mut rng = code.range();
        assert_eq!(
            test.range_type_ast(&mut rng, &code.tokenize(), &mut NoDiagnostics),
            Ok(test.lookup_type("CHARACTER").base())
        );
        check_no_unresolved(&mut rng, &code.tokenize());
    }

    #[test]
    fn range_type_mismatch_error() {
        let test = TestSetup::new();

        let code = test.snippet("0 to false");
        let mut diagnostics = Vec::new();
        assert_eq!(
            test.range_type(&code, &mut diagnostics),
            Err(EvalError::Unknown)
        );

        check_diagnostics(
            diagnostics,
            vec![Diagnostic::new(
                code.s1("0 to false"),
                "Range type mismatch, left is type universal_integer, right is type 'BOOLEAN'",
                ErrorCode::TypeMismatch,
            )],
        );
    }

    #[test]
    fn range_one_side_ambiguous_error() {
        let test = TestSetup::new();

        test.declarative_part(
            "
function f1 return character;
function f1 return integer;
        ",
        );

        let code = test.snippet("f1 to false");
        let mut diagnostics = Vec::new();
        assert_eq!(
            test.range_type(&code, &mut diagnostics),
            Err(EvalError::Unknown)
        );

        check_diagnostics(
            diagnostics,
            vec![Diagnostic::new(
                code.s1("f1 to false"),
                "Range type of left and right side does not match",
                ErrorCode::TypeMismatch,
            )],
        );
    }

    #[test]
    fn range_ambiguous() {
        let test = TestSetup::new();

        test.declarative_part(
            "
function f1 return character;
function f1 return integer;
        ",
        );

        let code = test.snippet("f1 to f1");
        let mut diagnostics = Vec::new();
        assert_eq!(
            test.range_type(&code, &mut diagnostics),
            Err(EvalError::Unknown)
        );

        check_diagnostics(
            diagnostics,
            vec![Diagnostic::new(
                code.s1("f1 to f1"),
                "Range is ambiguous",
                ErrorCode::TypeMismatch,
            )],
        );
    }

    #[test]
    fn range_attribute_name() {
        let test = TestSetup::new();

        test.declarative_part(
            "
type arr_t is array (integer range <>) of boolean;

function myfun return arr_t;

            ",
        );

        let code = test.snippet("arr_t'range");
        assert_eq!(
            test.range_type(&code, &mut NoDiagnostics),
            Ok(test.lookup_type("integer").base())
        );

        let code = test.snippet("myfun'range");
        assert_eq!(
            test.range_type(&code, &mut NoDiagnostics),
            Ok(test.lookup_type("integer").base())
        );

        let mut diagnostics = Vec::new();
        let code = test.snippet("character'range");
        assert_eq!(
            test.range_type(&code, &mut diagnostics),
            Err(EvalError::Unknown)
        );

        check_diagnostics(
            diagnostics,
            vec![Diagnostic::mismatched_kinds(
                code.s1("character"),
                "type 'CHARACTER' cannot be prefix of range attribute, array type or object is required",
            )],
        );
    }

    #[test]
    fn range_attribute_name_of_access_type() {
        let test = TestSetup::new();

        test.declarative_part(
            "
type arr_t is array (integer range <>) of boolean;
type ptr_t is access arr_t;
variable v : ptr_t;
            ",
        );

        let code = test.snippet("v'range");
        assert_eq!(
            test.range_type(&code, &mut NoDiagnostics),
            Ok(test.lookup_type("integer").base())
        );
    }
}
