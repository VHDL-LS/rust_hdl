// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use crate::ast::{ArrayIndex, Direction, DiscreteRange, Expression, Name, Range, RangeConstraint};
use crate::common::parse_optional;
use crate::diagnostic::{Diagnostic, ParseResult};
use crate::expression::parse_expression;
use crate::names::into_selected_name;
use crate::source::WithPos;
use crate::tokenizer::Kind::*;
use crate::tokenstream::TokenStream;

pub fn parse_direction(stream: &mut TokenStream) -> ParseResult<Direction> {
    Ok(try_token_kind!(
        stream.expect()?,
        To => Direction::Ascending,
        Downto => Direction::Descending
    ))
}

enum NameOrRange {
    Name(WithPos<Name>),
    Range(WithPos<Range>),
}

fn parse_name_or_range(stream: &mut TokenStream) -> ParseResult<NameOrRange> {
    let expr = parse_expression(stream)?;

    match stream.peek_kind()? {
        Some(To) | Some(Downto) => {
            let direction = parse_direction(stream)?;
            let right_expr = parse_expression(stream)?;

            let pos = expr.pos.combine(&right_expr.pos);
            let range = Range::Range(RangeConstraint {
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
        if let Name::Attribute(ref attribute_name) = *name.as_ref() {
            if &attribute_name.attr.item == stream.range_sym()
                || &attribute_name.attr.item == stream.reverse_range_sym()
            {
                // @TODO avoid clone
                let range = Range::Attribute(attribute_name.clone());
                return Ok(NameOrRange::Range(WithPos::from(range, pos)));
            }
        }
        return Ok(NameOrRange::Name(WithPos::from(*name, pos)));
    }

    Err(Diagnostic::error(&expr, "Expected name or range"))
}

/// {selected_name}'range
/// {selected_name}'reverse_range
/// 2. {expr} to|downto {expr}
pub fn parse_range(stream: &mut TokenStream) -> ParseResult<WithPos<Range>> {
    match parse_name_or_range(stream)? {
        NameOrRange::Range(range) => Ok(range),
        NameOrRange::Name(name) => Err(Diagnostic::error(&name, "Expected range")),
    }
}

pub fn parse_discrete_range(stream: &mut TokenStream) -> ParseResult<DiscreteRange> {
    match parse_name_or_range(stream) {
        Ok(NameOrRange::Range(range)) => Ok(DiscreteRange::Range(range.item)),
        Ok(NameOrRange::Name(name)) => {
            let selected_name = into_selected_name(name)?;
            let range = parse_optional(stream, Range, parse_range)?.map(|range| range.item);
            Ok(DiscreteRange::Discrete(selected_name, range))
        }
        Err(diagnostic) => Err(diagnostic.when("parsing discrete_range")),
    }
}

pub fn parse_array_index_constraint(stream: &mut TokenStream) -> ParseResult<ArrayIndex> {
    match parse_name_or_range(stream) {
        Ok(NameOrRange::Range(range)) => Ok(ArrayIndex::Discrete(DiscreteRange::Range(range.item))),
        Ok(NameOrRange::Name(name)) => {
            let selected_name = into_selected_name(name)?;

            if stream.skip_if_kind(Range)? {
                if stream.skip_if_kind(BOX)? {
                    Ok(ArrayIndex::IndexSubtypeDefintion(selected_name))
                } else {
                    Ok(ArrayIndex::Discrete(DiscreteRange::Discrete(
                        selected_name,
                        Some(parse_range(stream)?.item),
                    )))
                }
            } else {
                Ok(ArrayIndex::Discrete(DiscreteRange::Discrete(
                    selected_name,
                    None,
                )))
            }
        }
        Err(diagnostic) => Err(diagnostic.when("parsing array index constraint")),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::Code;

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
            DiscreteRange::Discrete(code.s1("foo.bar").selected_name(), None)
        );
    }

    #[test]
    fn parse_discrete_range_discrete_range() {
        let code = Code::new("foo.bar range 1 to 4");
        assert_eq!(
            code.with_stream(parse_discrete_range),
            DiscreteRange::Discrete(
                code.s1("foo.bar").selected_name(),
                Some(code.s1("1 to 4").range())
            )
        );
    }

    #[test]
    fn parse_array_index_constraint_subtype_definition() {
        let code = Code::new("foo.bar range <>");
        assert_eq!(
            code.with_stream(parse_array_index_constraint),
            ArrayIndex::IndexSubtypeDefintion(code.s1("foo.bar").selected_name())
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
