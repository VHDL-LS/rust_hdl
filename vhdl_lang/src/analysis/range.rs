//! This Source Code Form is subject to the terms of the Mozilla Public
//! License, v. 2.0. If a copy of the MPL was not distributed with this file,
//! You can obtain one at http://mozilla.org/MPL/2.0/.
//!
//! Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use super::analyze::*;
use super::expression::ExpressionType;
use super::named_entity::*;
use super::names::ResolvedName;
use super::overloaded::DisambiguatedType;
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

    fn discrete_expr_type(
        &self,
        scope: &Scope<'a>,
        expr: &mut WithPos<Expression>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<DisambiguatedType<'a>> {
        match self.expr_type(scope, expr, diagnostics)? {
            ExpressionType::Unambiguous(typ) => {
                if typ.base().is_discrete() {
                    Ok(DisambiguatedType::Unambiguous(typ))
                } else {
                    diagnostics.error(
                        &expr.pos,
                        format!(
                            "Non-discrete {} cannot be used in discrete range",
                            typ.describe()
                        ),
                    );
                    Err(EvalError::Unknown)
                }
            }
            ExpressionType::Ambiguous(types) => Ok(DisambiguatedType::Ambiguous(
                types.into_iter().filter(|typ| typ.is_discrete()).collect(),
            )),
            ExpressionType::String | ExpressionType::Null | ExpressionType::Aggregate => {
                diagnostics.error(
                    &expr.pos,
                    "Non-discrete expression cannot be used in discrete range",
                );
                Err(EvalError::Unknown)
            }
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
                    self.discrete_expr_type(scope, &mut constraint.left_expr, diagnostics)?;
                let right_types =
                    self.discrete_expr_type(scope, &mut constraint.right_expr, diagnostics)?;

                let types = match (left_types, right_types) {
                    (DisambiguatedType::Unambiguous(l), DisambiguatedType::Unambiguous(r)) => {
                        if let Some(typ) = self.common_type(l.base(), r.base()) {
                            return Ok(typ);
                        } else {
                            diagnostics.error(
                                constraint.pos(),
                                format!(
                                    "Range type mismatch, left is {}, right is {}",
                                    l.base().describe(),
                                    r.base().describe()
                                ),
                            );
                            return Err(EvalError::Unknown);
                        }
                    }
                    (DisambiguatedType::Unambiguous(l), DisambiguatedType::Ambiguous(r)) => {
                        self.common_types(r, l.base())
                    }

                    (DisambiguatedType::Ambiguous(l), DisambiguatedType::Unambiguous(r)) => {
                        self.common_types(l, r.base())
                    }
                    (DisambiguatedType::Ambiguous(_), DisambiguatedType::Ambiguous(_)) => {
                        diagnostics.error(constraint.pos(), "Range is ambiguous");
                        return Err(EvalError::Unknown);
                    }
                };

                if types.len() == 1 {
                    Ok(types.into_iter().next().unwrap())
                } else if types.is_empty() {
                    diagnostics.error(
                        constraint.pos(),
                        "Range type of left and right side does not match",
                    );
                    Err(EvalError::Unknown)
                } else {
                    // @TODO unsure if this can ever happen
                    diagnostics.error(constraint.pos(), "Range is ambiguous");
                    Err(EvalError::Unknown)
                }
            }
            Range::Attribute(ref mut attr) => {
                let resolved =
                    self.name_resolve(scope, &attr.name.pos, &mut attr.name.item, diagnostics)?;

                let typ = match resolved {
                    ResolvedName::Type(typ) => typ,
                    ResolvedName::ObjectName(oname) => oname.type_mark(),
                    ResolvedName::Overloaded(..)
                    | ResolvedName::Expression(_)
                    | ResolvedName::Final(_)
                    | ResolvedName::Library(_)
                    | ResolvedName::Design(_) => {
                        diagnostics.error(
                            &attr.name.pos,
                            format!(
                                "{} cannot be prefix of range attribute, array type or object is required",
                                resolved.describe()
                            ),
                        );
                        return Err(EvalError::Unknown);
                    }
                };

                if let Some((_, indexes)) = typ.array_type() {
                    Ok(indexes.first().unwrap().unwrap())
                } else {
                    diagnostics.error(
                        &attr.name.pos,
                        format!(
                            "{} cannot be prefix of range attribute, array type or object is required",
                            resolved.describe()
                        ),
                    );
                    Err(EvalError::Unknown)
                }
            }
        }
    }

    pub fn discrete_range_type(
        &self,
        scope: &Scope<'a>,
        drange: &mut DiscreteRange,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<BaseType<'a>> {
        match drange {
            DiscreteRange::Discrete(ref mut type_mark, ref mut range) => {
                let typ = match self.resolve_type_mark(scope, type_mark) {
                    Ok(typ) => typ.base(),
                    Err(err) => {
                        err.add_to(diagnostics)?;
                        return Err(EvalError::Unknown);
                    }
                };

                if let Some(ref mut range) = range {
                    self.analyze_range_with_target_type(scope, typ.into(), range, diagnostics)?;
                }
                Ok(typ)
            }
            DiscreteRange::Range(ref mut range) => self.range_type(scope, range, diagnostics),
        }
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

#[cfg(test)]
mod tests {
    use crate::analysis::analyze::EvalError;
    use crate::analysis::analyze::EvalResult;
    use crate::analysis::named_entity::BaseType;
    use crate::analysis::tests::NoDiagnostics;
    use crate::analysis::tests::TestSetup;
    use crate::data::DiagnosticHandler;
    use crate::syntax::test::check_diagnostics;
    use crate::syntax::test::Code;
    use crate::Diagnostic;

    impl<'a> TestSetup<'a> {
        fn range_type(
            &'a self,
            code: &Code,
            diagnostics: &mut dyn DiagnosticHandler,
        ) -> EvalResult<BaseType<'a>> {
            self.ctx()
                .range_type(&self.scope, &mut code.range(), diagnostics)
        }
    }

    #[test]
    fn universal_integer_range() {
        let test = TestSetup::new();

        let code = test.snippet("0 to 1");
        assert_eq!(
            test.range_type(&code, &mut NoDiagnostics),
            Ok(test.ctx().universal_integer())
        );
    }

    #[test]
    fn universal_integer_range_expression() {
        let test = TestSetup::new();

        let code = test.snippet("-1 to 1");
        assert_eq!(
            test.range_type(&code, &mut NoDiagnostics),
            Ok(test.ctx().universal_integer())
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
    fn range_not_discrete_type() {
        let test = TestSetup::new();
        let code = test.snippet("0.0 to 1.0");
        let mut diagnostics = Vec::new();
        assert_eq!(
            test.range_type(&code, &mut diagnostics),
            Err(crate::analysis::analyze::EvalError::Unknown)
        );

        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code.s1("0.0"),
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
            Err(crate::analysis::analyze::EvalError::Unknown)
        );

        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code.s1("(0, 0)"),
                "Non-discrete expression cannot be used in discrete range",
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
        assert_eq!(
            test.range_type(&code, &mut NoDiagnostics),
            Ok(test.lookup_type("CHARACTER").base())
        );

        let code = test.snippet("'a' to f1");
        assert_eq!(
            test.range_type(&code, &mut NoDiagnostics),
            Ok(test.lookup_type("CHARACTER").base())
        );
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
            vec![Diagnostic::error(
                code.s1("0 to false"),
                "Range type mismatch, left is type universal_integer, right is type 'BOOLEAN'",
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
            vec![Diagnostic::error(
                code.s1("f1 to false"),
                "Range type of left and right side does not match",
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
            vec![Diagnostic::error(code.s1("f1 to f1"), "Range is ambiguous")],
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
        let mut diagnostics = Vec::new();
        assert_eq!(
            test.range_type(&code, &mut diagnostics),
            Err(EvalError::Unknown)
        );
        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code.s1("myfun"),
                "myfun[return arr_t] cannot be prefix of range attribute, array type or object is required",
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
