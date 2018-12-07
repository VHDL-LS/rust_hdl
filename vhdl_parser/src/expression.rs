// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use crate::ast::{
    Allocator, Binary, Choice, Designator, Direction, DiscreteRange, ElementAssociation,
    Expression, Literal, Name, QualifiedExpression, Range, RangeConstraint, ResolutionIndication,
    SubtypeIndication, Unary,
};
use crate::message::{Message, ParseResult};
use crate::names::{parse_name_initial_token, parse_selected_name};
use crate::source::WithPos;
use crate::subtype_indication::parse_subtype_constraint;
use crate::tokenizer::Kind::*;
use crate::tokenizer::{Kind, Token};
use crate::tokenstream::TokenStream;

fn name_to_expression(name: WithPos<Name>) -> WithPos<Expression> {
    WithPos {
        item: Expression::Name(Box::new(name.item)),
        pos: name.pos,
    }
}

/// Convert kind to binary operation
/// Also returns the precedence
fn kind_to_binary_op(kind: Kind) -> Option<(Binary, usize)> {
    match kind {
        And => Some((Binary::And, 2)),
        Or => Some((Binary::Or, 2)),
        Nand => Some((Binary::Nand, 2)),
        Nor => Some((Binary::Nor, 2)),
        Xor => Some((Binary::Xor, 2)),
        Xnor => Some((Binary::Xnor, 2)),

        EQ => Some((Binary::EQ, 3)),
        NE => Some((Binary::NE, 3)),
        LT => Some((Binary::LT, 3)),
        LTE => Some((Binary::LTE, 3)),
        GT => Some((Binary::GT, 3)),
        GTE => Some((Binary::GTE, 3)),
        QueEQ => Some((Binary::QueEQ, 3)),
        QueNE => Some((Binary::QueNE, 3)),
        QueLT => Some((Binary::QueLT, 3)),
        QueLTE => Some((Binary::QueLTE, 3)),
        QueGT => Some((Binary::QueGT, 3)),
        QueGTE => Some((Binary::QueGTE, 3)),

        SLL => Some((Binary::SLL, 4)),
        SRL => Some((Binary::SRL, 4)),
        SLA => Some((Binary::SLA, 4)),
        SRA => Some((Binary::SRA, 4)),
        ROL => Some((Binary::ROL, 4)),
        ROR => Some((Binary::ROR, 4)),

        Plus => Some((Binary::Plus, 5)),
        Minus => Some((Binary::Minus, 5)),
        Concat => Some((Binary::Concat, 5)),
        Times => Some((Binary::Times, 5)),

        Div => Some((Binary::Div, 7)),
        Mod => Some((Binary::Mod, 7)),
        Rem => Some((Binary::Rem, 7)),

        Pow => Some((Binary::Pow, 8)),
        _ => None,
    }
}
/// Convert kind to a unary operation that is the prefix of the value
/// Also returns the precedence
fn kind_to_prefix_unary_op(kind: Kind) -> Option<(Unary, usize)> {
    match kind {
        Abs => Some((Unary::Abs, 8)),
        Not => Some((Unary::Not, 8)),
        Plus => Some((Unary::Plus, 6)),
        Minus => Some((Unary::Minus, 6)),
        QueQue => Some((Unary::QueQue, 1)),

        // LRM: All of the binary logical operators belong to the
        // class of operators with the lowest precedence. The unary
        // logical operators belong to the class of operators with
        // the highest precedence.
        And => Some((Unary::And, 8)),
        Or => Some((Unary::Or, 8)),
        Nand => Some((Unary::Nand, 8)),
        Nor => Some((Unary::Nor, 8)),
        Xor => Some((Unary::Xor, 8)),
        Xnor => Some((Unary::Xnor, 8)),

        _ => None,
    }
}

pub fn parse_aggregate_initial_choices(
    stream: &mut TokenStream,
    choices: Vec<Choice>,
) -> ParseResult<WithPos<Vec<ElementAssociation>>> {
    let mut choices = choices;
    let mut result = Vec::new();
    loop {
        let token = stream.expect()?;
        try_token_kind!(
            token,
            RightPar => {
                if let [Choice::Expression(ref choice)] = *choices.as_slice() {
                    result.push(ElementAssociation::Positional(choice.clone()));
                    return Ok(WithPos::from(result, token))
                } else {
                    return Err(Message::error(&token, "Expected => after others"));
                }
            },
            Comma => {
                if let [Choice::Expression(ref choice)] = *choices.as_slice() {
                    result.push(ElementAssociation::Positional(choice.clone()));
                } else {
                    return Err(Message::error(&token, "Expected => after others"));
                }
                choices = parse_choices(stream)?;
            },
            RightArrow => {
                let rhs = parse_expression(stream)?;
                result.push(ElementAssociation::Named(choices, rhs));

                let token = stream.expect()?;
                try_token_kind!(
                    token,
                    RightPar => {
                        return Ok(WithPos::from(result, token))
                    },
                    Comma => {
                        choices = parse_choices(stream)?;
                    }
                )
            }
        );
    }
}

#[cfg(test)]
pub fn parse_aggregate(stream: &mut TokenStream) -> ParseResult<WithPos<Vec<ElementAssociation>>> {
    stream.expect_kind(LeftPar)?;
    parse_aggregate_leftpar_known(stream)
}

pub fn parse_aggregate_leftpar_known(
    stream: &mut TokenStream,
) -> ParseResult<WithPos<Vec<ElementAssociation>>> {
    if let Some(token) = stream.pop_if_kind(RightPar)? {
        return Ok(WithPos::from(Vec::new(), token));
    };
    let choices = parse_choices(stream)?;
    parse_aggregate_initial_choices(stream, choices)
}

fn parse_half_range(
    stream: &mut TokenStream,
    left_expr: WithPos<Expression>,
    direction: Direction,
) -> ParseResult<DiscreteRange> {
    let right_expr = parse_expression(stream)?;
    let range = DiscreteRange::Range(Range::Range(RangeConstraint {
        direction,
        left_expr: Box::new(left_expr),
        right_expr: Box::new(right_expr),
    }));
    Ok(range)
}

fn parse_choice(stream: &mut TokenStream) -> ParseResult<Choice> {
    if stream.skip_if_kind(Others)? {
        return Ok(Choice::Others);
    }
    let left_expr = parse_expression(stream)?;

    if stream.skip_if_kind(To)? {
        let range = parse_half_range(stream, left_expr, Direction::Ascending)?;
        Ok(Choice::DiscreteRange(range))
    } else if stream.skip_if_kind(Downto)? {
        let range = parse_half_range(stream, left_expr, Direction::Descending)?;
        Ok(Choice::DiscreteRange(range))
    } else {
        Ok(Choice::Expression(left_expr))
    }
}

pub fn parse_choices(stream: &mut TokenStream) -> ParseResult<Vec<Choice>> {
    let mut choices = Vec::new();
    loop {
        choices.push(parse_choice(stream)?);

        if !stream.skip_if_kind(Bar)? {
            break;
        }
    }
    Ok(choices)
}

/// LRM 9.3.7 Allocators
fn parse_allocator(stream: &mut TokenStream) -> ParseResult<WithPos<Allocator>> {
    let selected_name = parse_selected_name(stream)?;

    if stream.skip_if_kind(Tick)? {
        let expr = parse_expression(stream)?;
        let name: WithPos<Name> = selected_name.into();
        let pos = name.pos.clone().combine_into(&expr);
        Ok(WithPos {
            item: Allocator::Qualified(QualifiedExpression {
                name: Box::new(name),
                expr: Box::new(expr),
            }),
            pos,
        })
    } else {
        let mut pos = selected_name[0].pos.clone();

        let constraint = {
            if let Some(constraint) = parse_subtype_constraint(stream)? {
                pos = pos.combine(&constraint.pos);
                Some(constraint)
            } else {
                pos = pos.combine(&selected_name[selected_name.len() - 1].pos);
                None
            }
        };

        let subtype = SubtypeIndication {
            resolution: ResolutionIndication::Unresolved,
            type_mark: selected_name,
            constraint,
        };

        Ok(WithPos {
            item: Allocator::Subtype(subtype),
            pos,
        })
    }
}

/// Parse a primary value which is:
/// 1. CHARACTER_LITERAL|INTEGER_LITERAL|IDENTIFIER|BOOLEAN_LITERAL
/// 2. (expression)
/// 3. PREFIX_UNARY_OP expression
fn parse_primary_initial_token(
    stream: &mut TokenStream,
    token: Token,
) -> ParseResult<WithPos<Expression>> {
    match token.kind {
        Identifier => {
            let name = parse_name_initial_token(stream, token)?;
            if stream.skip_if_kind(Tick)? {
                let expr = parse_expression(stream)?;
                let pos = name.pos.combine(&expr);
                Ok(WithPos {
                    item: Expression::Qualified(QualifiedExpression {
                        name: Box::new(name),
                        expr: Box::new(expr),
                    }),
                    pos,
                })
            } else {
                Ok(name_to_expression(name))
            }
        }
        BitString => Ok(token
            .expect_bit_string()?
            .map_into(|bs| Expression::Literal(Literal::BitString(bs)))),
        Character => Ok(token
            .expect_character()?
            .map_into(|chr| Expression::Literal(Literal::Character(chr)))),
        StringLiteral => {
            let name = parse_name_initial_token(stream, token)?;
            match name.item {
                Name::Designator(Designator::OperatorSymbol(string)) => Ok(WithPos {
                    item: Expression::Literal(Literal::String(string)),
                    pos: name.pos,
                }),
                _ => Ok(name.map_into(|name| Expression::Name(Box::new(name)))),
            }
        }
        Null => Ok(WithPos {
            item: Expression::Literal(Literal::Null),
            pos: token.pos.clone(),
        }),
        New => {
            let alloc = parse_allocator(stream)?;

            let new_pos = token.pos.combine_into(&alloc);
            Ok(WithPos {
                item: Expression::New(alloc),
                pos: new_pos,
            })
        }
        AbstractLiteral => {
            let value = token.expect_abstract_literal()?;
            // Physical unit
            if let Some(unit_token) = stream.pop_if_kind(Identifier)? {
                let unit = unit_token.expect_ident()?;
                let pos = value.pos.combine_into(&unit);
                let physical = Literal::Physical(value.item, unit.item);
                Ok(WithPos {
                    item: Expression::Literal(physical),
                    pos,
                })
            } else {
                Ok(value.map_into(|value| Expression::Literal(Literal::AbstractLiteral(value))))
            }
        }

        LeftPar => {
            let choices = parse_choices(stream)?;
            // Parenthesized expression or aggregate
            match *choices.as_slice() {
                // Can be aggregate or expression
                [Choice::Expression(ref expr)] => {
                    let sep_token = stream.peek_expect()?;
                    match_token_kind!(
                        sep_token,

                        // Was aggregate
                        Comma | RightArrow => {
                            Ok(parse_aggregate_initial_choices(
                                stream,
                                vec![Choice::Expression(expr.clone())],
                            )?.map_into(Expression::Aggregate)
                               .combine_pos_with(&token))
                        },

                        // Was expression with parenthesis
                        RightPar => {
                            let rpar_token = stream.expect()?;
                            // Lexical position between parenthesis
                            let expr = WithPos {
                                item: expr.item.clone(),
                                pos: rpar_token.pos.combine_into(&token),
                            };
                            Ok(expr)
                        }
                    )
                }
                // Must be aggregate
                _ => Ok(parse_aggregate_initial_choices(stream, choices.clone())?
                    .map_into(Expression::Aggregate)
                    .combine_pos_with(&token)),
            }
        }

        kind => {
            // Prefix unary operation
            if let Some((unary_op, op_precedence)) = kind_to_prefix_unary_op(kind) {
                let expr = parse_expr(stream, op_precedence)?;
                let pos = token.pos.combine_into(&expr);
                Ok(WithPos {
                    item: Expression::Unary(unary_op, Box::new(expr)),
                    pos,
                })
            } else {
                Err(Message::error(&token, "Expected {expression}"))
            }
        }
    }
}

fn parse_expr(stream: &mut TokenStream, min_precedence: usize) -> ParseResult<WithPos<Expression>> {
    let token = stream.expect()?;
    parse_expr_initial_token(stream, token, min_precedence)
}

fn parse_expr_initial_token(
    stream: &mut TokenStream,
    token: Token,
    min_precedence: usize,
) -> ParseResult<WithPos<Expression>> {
    let mut lhs = parse_primary_initial_token(stream, token)?;
    while let Some(token) = stream.peek()? {
        if token.kind == RightPar {
            return Ok(lhs);
        };

        if let Some((binary_op, op_precedence)) = kind_to_binary_op(token.kind) {
            // Binary operation
            if op_precedence > min_precedence {
                stream.move_after(&token);
                let rhs = parse_expr(stream, op_precedence)?;
                let pos = lhs.pos.combine(&rhs);
                lhs = WithPos {
                    item: Expression::Binary(binary_op, Box::new(lhs), Box::new(rhs)),
                    pos,
                };
            } else {
                return Ok(lhs);
            }
        } else {
            return Ok(lhs);
        };
    }

    Ok(lhs)
}

/// Parse expressions using a [Pratt parser](https://en.wikipedia.org/wiki/Pratt_parser)
///
/// Use precedence from LRM 9.2 Operators
/// All operators are left associative.
/// Operator classes are listed in order of increasing precedence
///   1. condition_operator: ??
///   2. logical_operator: and | or | nand | nor | xor | xnor
///   3. relational_operator: = | /= | < | <= | > | >= | ?= | ?/= | ?< | ?<= | ?> | ?>=
///   4. shift_operator: sll | srl | sla | sra | rol | ror
///   5. adding_operator: + | - | &
///   6. sign: + | -
///   7. multiplying_operator: * | / | mod | rem
///   8. misc_operator: ** | abs | not
pub fn parse_expression(stream: &mut TokenStream) -> ParseResult<WithPos<Expression>> {
    let token = stream.expect()?;
    parse_expression_initial_token(stream, token)
}

pub fn parse_expression_initial_token(
    stream: &mut TokenStream,
    token: Token,
) -> ParseResult<WithPos<Expression>> {
    parse_expr_initial_token(stream, token, 0)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{AbstractLiteral, Name};
    use crate::latin_1::Latin1String;
    use crate::test_util::Code;

    #[test]
    fn parses_character_literal() {
        let code = Code::new("'a'");
        assert_eq!(
            code.with_stream(parse_expression),
            WithPos {
                item: Expression::Literal(Literal::Character(b'a')),
                pos: code.pos()
            }
        );
    }

    #[test]
    fn parses_abstract_literal_integer() {
        let code = Code::new("71");
        assert_eq!(
            code.with_stream(parse_expression),
            WithPos {
                item: Expression::Literal(Literal::AbstractLiteral(AbstractLiteral::Integer(71))),
                pos: code.pos()
            }
        );
    }

    #[test]
    fn parses_abstract_literal_real() {
        let code = Code::new("7.1");
        assert_eq!(
            code.with_stream(parse_expression),
            WithPos {
                item: Expression::Literal(Literal::AbstractLiteral(AbstractLiteral::Real(7.1))),
                pos: code.pos()
            }
        );
    }

    #[test]
    fn parses_string_literal() {
        let code = Code::new("\"string\"");
        assert_eq!(
            code.with_stream(parse_expression),
            WithPos {
                item: Expression::Literal(Literal::String(Latin1String::from_utf8_unchecked(
                    "string"
                ))),
                pos: code.pos()
            }
        );
    }

    #[test]
    fn parses_operator_symol() {
        let code = Code::new("\"string\"(1, 2)");
        assert_eq!(
            code.with_stream(parse_expression),
            code.s1("\"string\"(1, 2)")
                .name()
                .map_into(|name| Expression::Name(Box::new(name)))
        );
    }

    #[test]
    fn parses_null_literal() {
        let code = Code::new("null");
        assert_eq!(
            code.with_stream(parse_expression),
            WithPos {
                item: Expression::Literal(Literal::Null),
                pos: code.pos()
            }
        );
    }

    fn int(value: i64) -> Literal {
        Literal::AbstractLiteral(AbstractLiteral::Integer(value))
    }

    #[test]
    fn parses_add_expression() {
        let code = Code::new("1 + 2");

        let lhs = WithPos {
            item: Expression::Literal(int(1)),
            pos: code.s1("1").pos(),
        };

        let rhs = WithPos {
            item: Expression::Literal(int(2)),
            pos: code.s1("2").pos(),
        };

        let expr_add = WithPos {
            item: Expression::Binary(Binary::Plus, Box::new(lhs), Box::new(rhs)),
            pos: code.pos(),
        };

        assert_eq!(code.with_stream(parse_expression), expr_add);
    }

    #[test]
    fn parses_sub_expression() {
        let code = Code::new("1 - 2");
        let lhs = WithPos {
            item: Expression::Literal(int(1)),
            pos: code.s1("1").pos(),
        };

        let rhs = WithPos {
            item: Expression::Literal(int(2)),
            pos: code.s1("2").pos(),
        };

        let expr_sub = WithPos {
            item: Expression::Binary(Binary::Minus, Box::new(lhs), Box::new(rhs)),
            pos: code.pos(),
        };

        assert_eq!(code.with_stream(parse_expression), expr_sub);
    }

    #[test]
    fn parses_abs_expression() {
        let code = Code::new("abs 9");
        let expr = WithPos {
            item: Expression::Literal(int(9)),
            pos: code.s1("9").pos(),
        };

        let expr_abs = WithPos {
            item: Expression::Unary(Unary::Abs, Box::new(expr)),
            pos: code.pos(),
        };

        assert_eq!(code.with_stream(parse_expression), expr_abs);
    }

    #[test]
    fn parses_condition_operator() {
        let code = Code::new("?? 9");
        let expr = WithPos {
            item: Expression::Literal(int(9)),
            pos: code.s1("9").pos(),
        };

        let expr_cond = WithPos {
            item: Expression::Unary(Unary::QueQue, Box::new(expr)),
            pos: code.pos(),
        };

        assert_eq!(code.with_stream(parse_expression), expr_cond);
    }

    #[test]
    fn parses_not_expression() {
        let code = Code::new("not false");
        let name_false = WithPos {
            item: Expression::Name(Box::new(Name::Designator(Designator::Identifier(
                code.symbol("false"),
            )))),
            pos: code.s1("false").pos(),
        };

        let expr_not = WithPos {
            item: Expression::Unary(Unary::Not, Box::new(name_false)),
            pos: code.pos(),
        };

        assert_eq!(code.with_stream(parse_expression), expr_not);
    }

    #[test]
    fn parses_new_allocator_qualified() {
        let code = Code::new("new integer_vector'(0, 1)");
        let vec_name = code
            .s1("integer_vector")
            .ident()
            .map_into(Designator::Identifier)
            .map_into(Name::Designator);
        let expr = code.s1("(0, 1)").expr();

        let alloc = WithPos {
            item: Allocator::Qualified(QualifiedExpression {
                name: Box::new(vec_name),
                expr: Box::new(expr),
            }),
            pos: code.s1("integer_vector'(0, 1)").pos(),
        };

        let new_expr = WithPos {
            item: Expression::New(alloc),
            pos: code.pos(),
        };

        assert_eq!(code.with_stream(parse_expression), new_expr);
    }

    #[test]
    fn parses_new_allocator_subtype() {
        let code = Code::new("new integer_vector");

        let alloc = WithPos {
            item: Allocator::Subtype(code.s1("integer_vector").subtype_indication()),
            pos: code.s1("integer_vector").pos(),
        };

        let new_expr = WithPos {
            item: Expression::New(alloc),
            pos: code.pos(),
        };

        assert_eq!(code.with_stream(parse_expression), new_expr);
    }

    #[test]
    fn parses_new_allocator_subtype_constraint() {
        let code = Code::new("new integer_vector(0 to 1)");

        let alloc = WithPos {
            item: Allocator::Subtype(code.s1("integer_vector(0 to 1)").subtype_indication()),
            pos: code.s1("integer_vector(0 to 1)").pos(),
        };

        let new_expr = WithPos {
            item: Expression::New(alloc),
            pos: code.pos(),
        };

        assert_eq!(code.with_stream(parse_expression), new_expr);
    }

    #[test]
    fn parses_new_allocator_subtype_constraint_range_attribute() {
        let code = Code::new("new integer_vector(foo'range)");

        let alloc = WithPos {
            item: Allocator::Subtype(code.s1("integer_vector(foo'range)").subtype_indication()),
            pos: code.s1("integer_vector(foo'range)").pos(),
        };

        let new_expr = WithPos {
            item: Expression::New(alloc),
            pos: code.pos(),
        };

        assert_eq!(code.with_stream(parse_expression), new_expr);
    }

    #[test]
    fn parses_physical_unit_expression() {
        let code = Code::new("1 ns");
        let expr = WithPos {
            item: Expression::Literal(Literal::Physical(
                AbstractLiteral::Integer(1),
                code.symbol("ns"),
            )),
            pos: code.pos(),
        };

        assert_eq!(code.with_stream(parse_expression), expr);
    }

    #[test]
    fn parses_physical_unit_expression_real() {
        let code = Code::new("1.0 ns");
        let expr = WithPos {
            item: Expression::Literal(Literal::Physical(
                AbstractLiteral::Real(1.0),
                code.symbol("ns"),
            )),
            pos: code.pos(),
        };

        assert_eq!(code.with_stream(parse_expression), expr);
    }

    #[test]
    fn parses_physical_unit_expression_binary() {
        let code = Code::new("2 * 1 ns");
        let time_expr = WithPos {
            item: Expression::Literal(Literal::Physical(
                AbstractLiteral::Integer(1),
                code.symbol("ns"),
            )),
            pos: code.s1("1 ns").pos(),
        };
        let two_expr = WithPos {
            item: Expression::Literal(int(2)),
            pos: code.s1("2").pos(),
        };
        let expr = WithPos {
            item: Expression::Binary(Binary::Times, Box::new(two_expr), Box::new(time_expr)),
            pos: code.pos(),
        };
        assert_eq!(code.with_stream(parse_expression), expr);
    }

    #[test]
    fn parses_physical_unit_expression_unary() {
        let code = Code::new("- 1 ns");
        let time_expr = WithPos {
            item: Expression::Literal(Literal::Physical(
                AbstractLiteral::Integer(1),
                code.symbol("ns"),
            )),
            pos: code.s1("1 ns").pos(),
        };
        let expr = WithPos {
            item: Expression::Unary(Unary::Minus, Box::new(time_expr)),
            pos: code.pos(),
        };

        assert_eq!(code.with_stream(parse_expression), expr);
    }

    #[test]
    fn parses_qualified_expression() {
        let code = Code::new("foo'(1+2)");
        let foo_name = code
            .s1("foo")
            .ident()
            .map_into(Designator::Identifier)
            .map_into(Name::Designator);
        let expr = code.s1("(1+2)").expr();

        let qexpr = WithPos {
            item: Expression::Qualified(QualifiedExpression {
                name: Box::new(foo_name),
                expr: Box::new(expr),
            }),
            pos: code.pos(),
        };

        assert_eq!(code.with_stream(parse_expression), qexpr);
    }

    #[test]
    fn parses_qualified_aggregate() {
        let code = Code::new("foo'(others => '1')");
        let foo_name = code
            .s1("foo")
            .ident()
            .map_into(Designator::Identifier)
            .map_into(Name::Designator);
        let expr = code.s1("(others => '1')").expr();

        let qexpr = WithPos {
            item: Expression::Qualified(QualifiedExpression {
                name: Box::new(foo_name),
                expr: Box::new(expr),
            }),
            pos: code.pos(),
        };

        assert_eq!(code.with_stream(parse_expression), qexpr);
    }

    #[test]
    fn parses_positional_aggregate() {
        let code = Code::new("(1, 2)");
        let one_expr = WithPos {
            item: Expression::Literal(int(1)),
            pos: code.s1("1").pos(),
        };
        let two_expr = WithPos {
            item: Expression::Literal(int(2)),
            pos: code.s1("2").pos(),
        };

        let assoc_list = vec![
            ElementAssociation::Positional(one_expr),
            ElementAssociation::Positional(two_expr),
        ];
        let expr = WithPos {
            item: Expression::Aggregate(assoc_list),
            pos: code.pos(),
        };

        assert_eq!(code.with_stream(parse_expression), expr);
    }

    #[test]
    fn parses_named_aggregate() {
        let code = Code::new("(1 => 2)");
        let one_expr = WithPos {
            item: Expression::Literal(int(1)),
            pos: code.s1("1").pos(),
        };
        let two_expr = WithPos {
            item: Expression::Literal(int(2)),
            pos: code.s1("2").pos(),
        };

        let assoc_list = vec![ElementAssociation::Named(
            vec![Choice::Expression(one_expr)],
            two_expr,
        )];
        let expr = WithPos {
            item: Expression::Aggregate(assoc_list),
            pos: code.pos(),
        };

        assert_eq!(code.with_stream(parse_expression), expr);
    }

    #[test]
    fn parses_named_aggregate_many_choices() {
        let code = Code::new("(1 | 2 => 3)");
        let one_expr = WithPos {
            item: Expression::Literal(int(1)),
            pos: code.s1("1").pos(),
        };

        let two_expr = WithPos {
            item: Expression::Literal(int(2)),
            pos: code.s1("2").pos(),
        };

        let three_expr = WithPos {
            item: Expression::Literal(int(3)),
            pos: code.s1("3").pos(),
        };

        let assoc_list = vec![ElementAssociation::Named(
            vec![Choice::Expression(one_expr), Choice::Expression(two_expr)],
            three_expr,
        )];
        let expr = WithPos {
            item: Expression::Aggregate(assoc_list),
            pos: code.pos(),
        };

        assert_eq!(code.with_stream(parse_expression), expr);
    }

    #[test]
    fn parses_others_aggregate() {
        let code = Code::new("(others => 1)");
        let one_expr = WithPos {
            item: Expression::Literal(int(1)),
            pos: code.s1("1").pos(),
        };

        let assoc_list = vec![ElementAssociation::Named(vec![Choice::Others], one_expr)];
        let expr = WithPos {
            item: Expression::Aggregate(assoc_list),
            pos: code.pos(),
        };

        assert_eq!(code.with_stream(parse_expression), expr);
    }

    #[test]
    fn parses_aggregate_range() {
        for direction in [Direction::Descending, Direction::Ascending].iter() {
            let code = {
                if *direction == Direction::Descending {
                    Code::new("(1 downto 0 => 2)")
                } else {
                    Code::new("(1 to 0 => 2)")
                }
            };

            let one_expr = WithPos {
                item: Expression::Literal(int(1)),
                pos: code.s1("1").pos(),
            };

            let zero_expr = WithPos {
                item: Expression::Literal(int(0)),
                pos: code.s1("0").pos(),
            };

            let two_expr = WithPos {
                item: Expression::Literal(int(2)),
                pos: code.s1("2").pos(),
            };

            let range = DiscreteRange::Range(Range::Range(RangeConstraint {
                direction: *direction,
                left_expr: Box::new(one_expr),
                right_expr: Box::new(zero_expr),
            }));

            let assoc_list = vec![ElementAssociation::Named(
                vec![Choice::DiscreteRange(range)],
                two_expr,
            )];
            let expr = WithPos {
                item: Expression::Aggregate(assoc_list),
                pos: code.pos(),
            };

            assert_eq!(code.with_stream(parse_expression), expr);
        }
    }

    #[test]
    fn parses_multiple_others_aggregate() {
        let code = Code::new("(others => 1, others => 2)");
        let one_expr = WithPos {
            item: Expression::Literal(int(1)),
            pos: code.s1("1").pos(),
        };

        let two_expr = WithPos {
            item: Expression::Literal(int(2)),
            pos: code.s1("2").pos(),
        };

        let assoc_list = vec![
            ElementAssociation::Named(vec![Choice::Others], one_expr),
            ElementAssociation::Named(vec![Choice::Others], two_expr),
        ];
        let expr = WithPos {
            item: Expression::Aggregate(assoc_list),
            pos: code.pos(),
        };

        assert_eq!(code.with_stream(parse_expression), expr);
    }

    #[test]
    fn parses_mixed_aggregate() {
        let code = Code::new("(1 => 2, 3)");
        let one_expr = WithPos {
            item: Expression::Literal(int(1)),
            pos: code.s1("1").pos(),
        };
        let two_expr = WithPos {
            item: Expression::Literal(int(2)),
            pos: code.s1("2").pos(),
        };
        let three_expr = WithPos {
            item: Expression::Literal(int(3)),
            pos: code.s1("3").pos(),
        };

        let assoc_list = vec![
            ElementAssociation::Named(vec![Choice::Expression(one_expr)], two_expr),
            ElementAssociation::Positional(three_expr),
        ];
        let expr = WithPos {
            item: Expression::Aggregate(assoc_list),
            pos: code.pos(),
        };

        assert_eq!(code.with_stream(parse_expression), expr);
    }

    #[test]
    fn parses_huge_aggregate() {
        // Check that there is no stack overflow
        let mut code = "(".to_string();
        for _ in 0..(1 << 13) {
            code.push_str("11123, ");
        }
        code.push_str("11123)");
        let code = Code::new(&code);
        assert_eq!(code.with_stream(parse_expression).pos, code.pos());
    }

    #[test]
    fn parses_nested_expression_par_second() {
        let code = Code::new("1 + (2 + 3)");

        let one = WithPos {
            item: Expression::Literal(int(1)),
            pos: code.s1("1").pos(),
        };

        let two = WithPos {
            item: Expression::Literal(int(2)),
            pos: code.s1("2").pos(),
        };

        let three = WithPos {
            item: Expression::Literal(int(3)),
            pos: code.s1("3").pos(),
        };

        let expr_add0 = WithPos {
            item: Expression::Binary(Binary::Plus, Box::new(two), Box::new(three)),
            pos: code.s1("(2 + 3)").pos(),
        };

        let expr_add1 = WithPos {
            item: Expression::Binary(Binary::Plus, Box::new(one), Box::new(expr_add0)),
            pos: code.pos(),
        };

        assert_eq!(code.with_stream(parse_expression), expr_add1);
    }

    #[test]
    fn parses_nested_expression_par_first() {
        let code = Code::new("(1 + 2) + 3");

        let one = WithPos {
            item: Expression::Literal(int(1)),
            pos: code.s1("1").pos(),
        };

        let two = WithPos {
            item: Expression::Literal(int(2)),
            pos: code.s1("2").pos(),
        };

        let three = WithPos {
            item: Expression::Literal(int(3)),
            pos: code.s1("3").pos(),
        };

        let expr_add0 = WithPos {
            item: Expression::Binary(Binary::Plus, Box::new(one), Box::new(two)),
            pos: code.s1("(1 + 2)").pos(),
        };

        let expr_add1 = WithPos {
            item: Expression::Binary(Binary::Plus, Box::new(expr_add0), Box::new(three)),
            pos: code.pos(),
        };

        assert_eq!(code.with_stream(parse_expression), expr_add1);
    }

    /// Format expression as a string to simplify testing of precedence.
    fn fmt(expr: &WithPos<Expression>) -> String {
        match expr.item {
            Expression::Binary(ref op, ref lhs, ref rhs) => {
                format!("({} {:?} {})", fmt(lhs), op, fmt(rhs))
            }
            Expression::Unary(ref op, ref rhs) => format!("({:?} {})", op, fmt(rhs)),
            Expression::Literal(ref lit) => match lit {
                Literal::Null => "null".to_string(),
                // @TODO quote and escape
                Literal::String(val) => val.to_string(),
                Literal::AbstractLiteral(val) => match val {
                    AbstractLiteral::Integer(val) => format!("Integer({})", val),
                    AbstractLiteral::Real(val) => format!("Real({})", val),
                },
                Literal::Character(val) => format!("'{}'", Latin1String::new(&[*val]).to_string()),
                Literal::Physical(ref val, ref sym) => match val {
                    AbstractLiteral::Integer(val) => {
                        format!("Physical(Integer({}), {})", val, sym.name())
                    }
                    AbstractLiteral::Real(val) => {
                        format!("Physical(Real({}), {})", val, sym.name())
                    }
                },
                _ => {
                    println!("{}", expr.pos.code_context());
                    panic!("Cannot format {:?}", lit);
                }
            },
            _ => {
                println!("{}", expr.pos.code_context());
                panic!("Cannot format {:?}", expr);
            }
        }
    }

    fn assert_expression_is(code: &str, expr_str: &str) {
        assert_eq!(
            fmt(&Code::new(code).with_stream(parse_expression)),
            expr_str
        );
    }

    #[test]
    fn parses_function_errors() {
        let code = Code::new("fun(,)");
        assert_eq!(
            code.with_partial_stream(parse_expression),
            Err(Message::error(&code.s1(",").pos(), "Expected {expression}"))
        );

        let code = Code::new("fun(arg0,)");
        assert_eq!(
            code.with_partial_stream(parse_expression),
            Err(Message::error(&code.s1(")").pos(), "Expected {expression}"))
        );
        let code = Code::new("fun(arg0,,)");
        assert_eq!(
            code.with_partial_stream(parse_expression),
            Err(Message::error(
                &code.s(",", 2).pos(),
                "Expected {expression}"
            ))
        );
    }

    #[test]
    fn parses_nested_expression_precedence() {
        assert_expression_is("1 + 1 ns", "(Integer(1) Plus Physical(Integer(1), ns))");

        assert_expression_is(
            "1 * 1 ns * 2",
            "((Integer(1) Times Physical(Integer(1), ns)) Times Integer(2))",
        );

        assert_expression_is("1+2+3", "((Integer(1) Plus Integer(2)) Plus Integer(3))");

        assert_expression_is("1-2-3", "((Integer(1) Minus Integer(2)) Minus Integer(3))");

        assert_expression_is("not 1 + 2", "((Not Integer(1)) Plus Integer(2))");

        assert_expression_is("abs not 1 + 2", "((Abs (Not Integer(1))) Plus Integer(2))");

        assert_expression_is("not - 1", "(Not (Minus Integer(1)))");

        assert_expression_is("not + 1", "(Not (Plus Integer(1)))");

        assert_expression_is(
            "not + ?? 1 ** ?? 2",
            "(Not (Plus (QueQue (Integer(1) Pow (QueQue Integer(2))))))",
        );

        assert_expression_is(
            "abs 1 sll 2 + 3 and -1",
            "(((Abs Integer(1)) SLL (Integer(2) Plus Integer(3))) And (Minus Integer(1)))",
        );

        assert_expression_is(
            "1 + 2 and 3 + 4",
            "((Integer(1) Plus Integer(2)) And (Integer(3) Plus Integer(4)))",
        );

        assert_expression_is("and 1 + 2", "((And Integer(1)) Plus Integer(2))");
    }

}
