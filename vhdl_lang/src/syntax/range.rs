// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::common::parse_optional;
use super::common::ParseResult;
use super::expression::name_to_type_mark;
use super::expression::parse_expression;
use super::tokens::{Kind::*, TokenStream};
use crate::ast;
use crate::ast::*;
use crate::data::{Diagnostic, WithPos};

pub fn parse_direction(stream: &TokenStream) -> ParseResult<Direction> {
    Ok(expect_token!(
        stream, token,
        To => Direction::Ascending,
        Downto => Direction::Descending
    ))
}

enum NameOrRange {
    Name(WithPos<Name>),
    Range(WithPos<ast::Range>),
}

fn parse_name_or_range(stream: &TokenStream) -> ParseResult<NameOrRange> {
    let expr = parse_expression(stream)?;

    match stream.peek_kind() {
        Some(To) | Some(Downto) => {
            let direction = parse_direction(stream)?;
            let right_expr = parse_expression(stream)?;

            let pos = expr.pos.combine(&right_expr.pos);
            let range = ast::Range::Range(RangeConstraint {
                direction,
                left_expr: Box::new(expr),
                right_expr: Box::new(right_expr),
            });

            return Ok(NameOrRange::Range(WithPos::from(range, pos)));
        }
        _ => {}
    }

    if let WithPos {
        item: Expression::Name(name),
        pos,
    } = expr
    {
        if let Name::Attribute(attribute_name) = *name {
            if attribute_name.as_range().is_some() {
                let range = ast::Range::Attribute(attribute_name);
                Ok(NameOrRange::Range(WithPos::from(range, pos)))
            } else {
                Ok(NameOrRange::Name(WithPos::from(
                    Name::Attribute(attribute_name),
                    pos,
                )))
            }
        } else {
            Ok(NameOrRange::Name(WithPos::from(*name, pos)))
        }
    } else {
        Err(Diagnostic::error(&expr, "Expected name or range"))
    }
}

/// {selected_name}'range
/// {selected_name}'reverse_range
/// 2. {expr} to|downto {expr}
pub fn parse_range(stream: &TokenStream) -> ParseResult<WithPos<ast::Range>> {
    match parse_name_or_range(stream)? {
        NameOrRange::Range(range) => Ok(range),
        NameOrRange::Name(name) => Err(Diagnostic::error(&name, "Expected range")),
    }
}

pub fn parse_discrete_range(stream: &TokenStream) -> ParseResult<DiscreteRange> {
    match parse_name_or_range(stream) {
        Ok(NameOrRange::Range(range)) => Ok(DiscreteRange::Range(range.item)),
        Ok(NameOrRange::Name(name)) => {
            let type_mark = name_to_type_mark(name)?;
            let range = parse_optional(stream, Range, parse_range)?.map(|range| range.item);
            Ok(DiscreteRange::Discrete(type_mark, range))
        }
        Err(diagnostic) => Err(diagnostic.when("parsing discrete_range")),
    }
}

pub fn parse_array_index_constraint(stream: &TokenStream) -> ParseResult<ArrayIndex> {
    match parse_name_or_range(stream) {
        Ok(NameOrRange::Range(range)) => Ok(ArrayIndex::Discrete(DiscreteRange::Range(range.item))),
        Ok(NameOrRange::Name(name)) => {
            let type_mark = name_to_type_mark(name)?;

            if stream.skip_if_kind(Range) {
                if stream.skip_if_kind(BOX) {
                    Ok(ArrayIndex::IndexSubtypeDefintion(type_mark))
                } else {
                    Ok(ArrayIndex::Discrete(DiscreteRange::Discrete(
                        type_mark,
                        Some(parse_range(stream)?.item),
                    )))
                }
            } else {
                Ok(ArrayIndex::Discrete(DiscreteRange::Discrete(
                    type_mark, None,
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
            WithPos::new(
                Range::Range(RangeConstraint {
                    direction: Direction::Ascending,
                    left_expr: Box::new(code.s1("foo.bar").expr()),
                    right_expr: Box::new(code.s1("1").expr())
                },),
                code.pos()
            )
        );
    }

    #[test]
    fn parse_range_range_attribute() {
        let code = Code::new("foo.bar'range");
        assert_eq!(
            code.with_stream(parse_range),
            WithPos::new(
                Range::Attribute(Box::new(code.s1("foo.bar'range").attribute_name())),
                code.pos()
            )
        );
    }

    #[test]
    fn parse_range_reverse_range_attribute() {
        let code = Code::new("foo.bar'reverse_range");
        assert_eq!(
            code.with_stream(parse_range),
            WithPos::new(
                Range::Attribute(Box::new(code.s1("foo.bar'reverse_range").attribute_name())),
                code.pos()
            )
        );
    }

    #[test]
    fn parse_range_other_attribute() {
        let code = Code::new("foo.bar'length downto 0");
        assert_eq!(
            code.with_stream(parse_range),
            WithPos::new(
                Range::Range(RangeConstraint {
                    direction: Direction::Descending,
                    left_expr: Box::new(code.s1("foo.bar'length").expr()),
                    right_expr: Box::new(code.s1("0").expr())
                }),
                code.pos()
            )
        );
    }

    #[test]
    fn parse_discrete_range_range() {
        let code = Code::new("foo.bar to 1");
        assert_eq!(
            code.with_stream(parse_discrete_range),
            DiscreteRange::Range(Range::Range(RangeConstraint {
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
            DiscreteRange::Range(Range::Attribute(Box::new(
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
            ArrayIndex::Discrete(code.s1("0 to 1").discrete_range())
        );
    }

    #[test]
    fn parse_array_index_constraint_discrete_range() {
        let code = Code::new("foo.bar range 0 to 1");
        assert_eq!(
            code.with_stream(parse_array_index_constraint),
            ArrayIndex::Discrete(code.s1("foo.bar range 0 to 1").discrete_range())
        );
    }

    #[test]
    fn parse_array_index_constraint_discrete_no_range() {
        let code = Code::new("foo.bar");
        assert_eq!(
            code.with_stream(parse_array_index_constraint),
            ArrayIndex::Discrete(code.s1("foo.bar").discrete_range())
        );
    }
}
