// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::common::parse_optional;
use super::common::ParseResult;
use super::expression::name_to_type_mark;
use super::expression::parse_expression;
use super::tokens::Kind::*;
use crate::ast;
use crate::ast::token_range::WithTokenSpan;
use crate::ast::*;
use crate::data::Diagnostic;
use vhdl_lang::syntax::parser::ParsingContext;

pub fn parse_direction(ctx: &mut ParsingContext) -> ParseResult<Direction> {
    Ok(expect_token!(
        ctx.stream, token,
        To => Direction::Ascending,
        Downto => Direction::Descending
    ))
}

enum NameOrRange {
    Name(WithTokenSpan<Name>),
    Range(WithTokenSpan<ast::Range>),
}

fn parse_name_or_range(ctx: &mut ParsingContext<'_>) -> ParseResult<NameOrRange> {
    let expr = parse_expression(ctx)?;

    match ctx.stream.peek_kind() {
        Some(To) | Some(Downto) => {
            let direction = parse_direction(ctx)?;
            let right_expr = parse_expression(ctx)?;

            let span = expr.span.combine(right_expr.span);
            let range = ast::Range::Range(RangeConstraint {
                direction,
                left_expr: Box::new(expr),
                right_expr: Box::new(right_expr),
            });

            return Ok(NameOrRange::Range(WithTokenSpan::from(range, span)));
        }
        _ => {}
    }

    if let WithTokenSpan {
        item: Expression::Name(name),
        span,
    } = expr
    {
        if let Name::Attribute(attribute_name) = *name {
            if attribute_name.as_range().is_some() {
                let range = ast::Range::Attribute(attribute_name);
                Ok(NameOrRange::Range(WithTokenSpan::from(range, span)))
            } else {
                Ok(NameOrRange::Name(WithTokenSpan::from(
                    Name::Attribute(attribute_name),
                    span,
                )))
            }
        } else {
            Ok(NameOrRange::Name(WithTokenSpan::from(*name, span)))
        }
    } else {
        Err(Diagnostic::syntax_error(
            expr.pos(ctx),
            "Expected name or range",
        ))
    }
}

/// {selected_name}'range
/// {selected_name}'reverse_range
/// 2. {expr} to|downto {expr}
pub fn parse_range(ctx: &mut ParsingContext<'_>) -> ParseResult<WithTokenSpan<ast::Range>> {
    match parse_name_or_range(ctx)? {
        NameOrRange::Range(range) => Ok(range),
        NameOrRange::Name(name) => Err(Diagnostic::syntax_error(name.pos(ctx), "Expected range")),
    }
}

pub fn parse_discrete_range(ctx: &mut ParsingContext<'_>) -> ParseResult<DiscreteRange> {
    match parse_name_or_range(ctx) {
        Ok(NameOrRange::Range(range)) => Ok(DiscreteRange::Range(range.item)),
        Ok(NameOrRange::Name(name)) => {
            let type_mark = name_to_type_mark(ctx, name)?;
            let range = parse_optional(ctx, Range, parse_range)?.map(|range| range.item);
            Ok(DiscreteRange::Discrete(type_mark, range))
        }
        Err(diagnostic) => Err(diagnostic.when("parsing discrete_range")),
    }
}

pub fn parse_array_index_constraint(ctx: &mut ParsingContext<'_>) -> ParseResult<ArrayIndex> {
    match parse_name_or_range(ctx) {
        Ok(NameOrRange::Range(range)) => Ok(ArrayIndex::Discrete(WithTokenSpan::new(
            DiscreteRange::Range(range.item),
            range.span,
        ))),
        Ok(NameOrRange::Name(name)) => {
            let type_mark = name_to_type_mark(ctx, name)?;

            if ctx.stream.skip_if_kind(Range) {
                if ctx.stream.skip_if_kind(BOX) {
                    Ok(ArrayIndex::IndexSubtypeDefintion(type_mark))
                } else {
                    let range = parse_range(ctx)?;
                    let span = type_mark.span.combine(range.span);
                    Ok(ArrayIndex::Discrete(WithTokenSpan::new(
                        DiscreteRange::Discrete(type_mark, Some(range.item)),
                        span,
                    )))
                }
            } else {
                let type_mark_span = type_mark.span;
                Ok(ArrayIndex::Discrete(WithTokenSpan::new(
                    DiscreteRange::Discrete(type_mark, None),
                    type_mark_span,
                )))
            }
        }
        Err(diagnostic) => Err(diagnostic.when("parsing array index constraint")),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::test::Code;

    #[test]
    fn parse_range_range() {
        let code = Code::new("foo.bar to 1");
        assert_eq!(
            code.with_stream(parse_range),
            WithTokenSpan::new(
                ast::Range::Range(RangeConstraint {
                    direction: Direction::Ascending,
                    left_expr: Box::new(code.s1("foo.bar").expr()),
                    right_expr: Box::new(code.s1("1").expr())
                },),
                code.token_span()
            )
        );
    }

    #[test]
    fn parse_range_range_attribute() {
        let code = Code::new("foo.bar'range");
        assert_eq!(
            code.with_stream(parse_range),
            WithTokenSpan::new(
                ast::Range::Attribute(Box::new(code.s1("foo.bar'range").attribute_name())),
                code.token_span()
            )
        );
    }

    #[test]
    fn parse_range_reverse_range_attribute() {
        let code = Code::new("foo.bar'reverse_range");
        assert_eq!(
            code.with_stream(parse_range),
            WithTokenSpan::new(
                ast::Range::Attribute(Box::new(code.s1("foo.bar'reverse_range").attribute_name())),
                code.token_span()
            )
        );
    }

    #[test]
    fn parse_range_other_attribute() {
        let code = Code::new("foo.bar'length downto 0");
        assert_eq!(
            code.with_stream(parse_range),
            WithTokenSpan::new(
                ast::Range::Range(RangeConstraint {
                    direction: Direction::Descending,
                    left_expr: Box::new(code.s1("foo.bar'length").expr()),
                    right_expr: Box::new(code.s1("0").expr())
                }),
                code.token_span()
            )
        );
    }

    #[test]
    fn parse_discrete_range_range() {
        let code = Code::new("foo.bar to 1");
        assert_eq!(
            code.with_stream(parse_discrete_range),
            DiscreteRange::Range(ast::Range::Range(RangeConstraint {
                direction: Direction::Ascending,
                left_expr: Box::new(code.s1("foo.bar").expr()),
                right_expr: Box::new(code.s1("1").expr())
            }))
        );
    }

    #[test]
    fn parse_discrete_range_range_attribute() {
        let code = Code::new("foo.bar'range");
        assert_eq!(
            code.with_stream(parse_discrete_range),
            DiscreteRange::Range(ast::Range::Attribute(Box::new(
                code.s1("foo.bar'range").attribute_name()
            )))
        );
    }

    #[test]
    fn parse_discrete_range_discrete() {
        let code = Code::new("foo.bar");
        assert_eq!(
            code.with_stream(parse_discrete_range),
            DiscreteRange::Discrete(code.s1("foo.bar").type_mark(), None)
        );
    }

    #[test]
    fn parse_discrete_range_discrete_range() {
        let code = Code::new("foo.bar range 1 to 4");
        assert_eq!(
            code.with_stream(parse_discrete_range),
            DiscreteRange::Discrete(
                code.s1("foo.bar").type_mark(),
                Some(code.s1("1 to 4").range())
            )
        );
    }

    #[test]
    fn parse_array_index_constraint_subtype_definition() {
        let code = Code::new("foo.bar range <>");
        assert_eq!(
            code.with_stream(parse_array_index_constraint),
            ArrayIndex::IndexSubtypeDefintion(code.s1("foo.bar").type_mark())
        );
    }

    #[test]
    fn parse_array_index_constraint_range() {
        let code = Code::new("0 to 1");
        assert_eq!(
            code.with_stream(parse_array_index_constraint),
            ArrayIndex::Discrete(WithTokenSpan::new(
                code.s1("0 to 1").discrete_range(),
                code.token_span()
            ))
        );
    }

    #[test]
    fn parse_array_index_constraint_discrete_range() {
        let code = Code::new("foo.bar range 0 to 1");
        assert_eq!(
            code.with_stream(parse_array_index_constraint),
            ArrayIndex::Discrete(WithTokenSpan::new(
                code.s1("foo.bar range 0 to 1").discrete_range(),
                code.token_span()
            ))
        );
    }

    #[test]
    fn parse_array_index_constraint_discrete_no_range() {
        let code = Code::new("foo.bar");
        assert_eq!(
            code.with_stream(parse_array_index_constraint),
            ArrayIndex::Discrete(WithTokenSpan::new(
                code.s1("foo.bar").discrete_range(),
                code.token_span()
            ))
        );
    }
}
