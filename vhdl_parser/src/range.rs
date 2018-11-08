// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use ast::{ArrayIndex, Direction, DiscreteRange, Expression, Name, Range, RangeConstraint};
use common::parse_optional;
use expression::parse_expression;
use message::{error, ParseResult};
use names::to_selected_name;
use source::WithPos;
use tokenizer::Kind::*;
use tokenstream::TokenStream;

pub fn parse_direction(stream: &mut TokenStream) -> ParseResult<Direction> {
    Ok(try_token_kind!(
        stream.expect()?,
        To => Direction::Ascending,
        Downto => Direction::Descending
    ))
}

enum NameOrRange {
    Name(WithPos<Name>),
    Range(Range),
}

fn parse_name_or_range(stream: &mut TokenStream) -> ParseResult<NameOrRange> {
    let expr = parse_expression(stream)?;

    match stream.peek_kind()? {
        Some(To) | Some(Downto) => {
            let direction = parse_direction(stream)?;
            let right_expr = parse_expression(stream)?;
            return Ok(NameOrRange::Range(Range::Range(RangeConstraint {
                direction,
                left_expr: Box::new(expr),
                right_expr: Box::new(right_expr),
            })));
        }
        _ => {}
    }

    if let WithPos {
        item: Expression::Name(name),
        pos,
    } = expr
    {
        if let &Name::Attribute(ref attribute_name) = name.as_ref() {
            if attribute_name.attr.item == stream.tokenizer.range_ident
                || attribute_name.attr.item == stream.tokenizer.reverse_range_ident
            {
                // @TODO avoid clone
                return Ok(NameOrRange::Range(Range::Attribute(attribute_name.clone())));
            }
        }
        return Ok(NameOrRange::Name(WithPos::new(*name, pos)));
    }

    return Err(error(&expr, "Expected name or range"));
}

/// {selected_name}'range
/// {selected_name}'reverse_range
/// 2. {expr} to|downto {expr}
pub fn parse_range(stream: &mut TokenStream) -> ParseResult<Range> {
    match parse_name_or_range(stream)? {
        NameOrRange::Range(range) => Ok(range),
        NameOrRange::Name(name) => Err(error(&name, "Expected range")),
    }
}

pub fn parse_discrete_range(stream: &mut TokenStream) -> ParseResult<DiscreteRange> {
    match parse_name_or_range(stream) {
        Ok(NameOrRange::Range(range)) => Ok(DiscreteRange::Range(range)),
        Ok(NameOrRange::Name(name)) => {
            let selected_name = to_selected_name(&name)?;
            let range = parse_optional(stream, Range, parse_range)?;
            Ok(DiscreteRange::Discrete(selected_name, range))
        }
        Err(msg) => Err(msg.when("parsing discrete_range")),
    }
}

pub fn parse_array_index_constraint(stream: &mut TokenStream) -> ParseResult<ArrayIndex> {
    match parse_name_or_range(stream) {
        Ok(NameOrRange::Range(range)) => Ok(ArrayIndex::Discrete(DiscreteRange::Range(range))),
        Ok(NameOrRange::Name(name)) => {
            let selected_name = to_selected_name(&name)?;

            if stream.skip_if_kind(Range)? {
                if stream.skip_if_kind(BOX)? {
                    Ok(ArrayIndex::IndexSubtypeDefintion(selected_name))
                } else {
                    Ok(ArrayIndex::Discrete(DiscreteRange::Discrete(
                        selected_name,
                        Some(parse_range(stream)?),
                    )))
                }
            } else {
                Ok(ArrayIndex::Discrete(DiscreteRange::Discrete(
                    selected_name,
                    None,
                )))
            }
        }
        Err(msg) => Err(msg.when("parsing array index constraint")),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_util::with_stream;

    #[test]
    fn parse_range_range() {
        let (util, range) = with_stream(parse_range, "foo.bar to 1");
        assert_eq!(
            range,
            Range::Range(RangeConstraint {
                direction: Direction::Ascending,
                left_expr: Box::new(util.expr("foo.bar")),
                right_expr: Box::new(util.expr("1"))
            })
        );
    }

    #[test]
    fn parse_range_range_attribute() {
        let (util, range) = with_stream(parse_range, "foo.bar'range");
        assert_eq!(
            range,
            Range::Attribute(Box::new(util.attribute_name("foo.bar'range")))
        );
    }

    #[test]
    fn parse_range_reverse_range_attribute() {
        let (util, range) = with_stream(parse_range, "foo.bar'reverse_range");
        assert_eq!(
            range,
            Range::Attribute(Box::new(util.attribute_name("foo.bar'reverse_range")))
        );
    }

    #[test]
    fn parse_range_other_attribute() {
        let (util, range) = with_stream(parse_range, "foo.bar'length downto 0");
        assert_eq!(
            range,
            Range::Range(RangeConstraint {
                direction: Direction::Descending,
                left_expr: Box::new(util.expr("foo.bar'length")),
                right_expr: Box::new(util.expr("0"))
            })
        );
    }

    #[test]
    fn parse_discrete_range_range() {
        let (util, range) = with_stream(parse_discrete_range, "foo.bar to 1");
        assert_eq!(
            range,
            DiscreteRange::Range(Range::Range(RangeConstraint {
                direction: Direction::Ascending,
                left_expr: Box::new(util.expr("foo.bar")),
                right_expr: Box::new(util.expr("1"))
            }))
        );
    }

    #[test]
    fn parse_discrete_range_range_attribute() {
        let (util, range) = with_stream(parse_discrete_range, "foo.bar'range");
        assert_eq!(
            range,
            DiscreteRange::Range(Range::Attribute(Box::new(
                util.attribute_name("foo.bar'range")
            )))
        );
    }

    #[test]
    fn parse_discrete_range_discrete() {
        let (util, range) = with_stream(parse_discrete_range, "foo.bar");
        assert_eq!(
            range,
            DiscreteRange::Discrete(util.selected_name("foo.bar"), None)
        );
    }

    #[test]
    fn parse_discrete_range_discrete_range() {
        let (util, range) = with_stream(parse_discrete_range, "foo.bar range 1 to 4");
        assert_eq!(
            range,
            DiscreteRange::Discrete(util.selected_name("foo.bar"), Some(util.range("1 to 4")))
        );
    }

    #[test]
    fn parse_array_index_constraint_subtype_definition() {
        let (util, range) = with_stream(parse_array_index_constraint, "foo.bar range <>");
        assert_eq!(
            range,
            ArrayIndex::IndexSubtypeDefintion(util.selected_name("foo.bar"))
        );
    }

    #[test]
    fn parse_array_index_constraint_range() {
        let (util, range) = with_stream(parse_array_index_constraint, "0 to 1");
        assert_eq!(range, ArrayIndex::Discrete(util.discrete_range("0 to 1")));
    }

    #[test]
    fn parse_array_index_constraint_discrete_range() {
        let (util, range) = with_stream(parse_array_index_constraint, "foo.bar range 0 to 1");
        assert_eq!(
            range,
            ArrayIndex::Discrete(util.discrete_range("foo.bar range 0 to 1"))
        );
    }

    #[test]
    fn parse_array_index_constraint_discrete_no_range() {
        let (util, range) = with_stream(parse_array_index_constraint, "foo.bar");
        assert_eq!(range, ArrayIndex::Discrete(util.discrete_range("foo.bar")));
    }
}
