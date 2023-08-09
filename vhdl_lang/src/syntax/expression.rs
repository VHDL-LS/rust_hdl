// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::common::ParseResult;
use super::names::{parse_name, parse_type_mark};
use super::subtype_indication::parse_subtype_constraint;
use super::tokens::{Kind, Kind::*, TokenStream};
use crate::ast;
use crate::ast::*;
use crate::data::{Diagnostic, WithPos};

fn name_to_expression(name: WithPos<Name>) -> WithPos<Expression> {
    WithPos {
        item: Expression::Name(Box::new(name.item)),
        pos: name.pos,
    }
}

impl Operator {
    pub fn binary_precedence(&self) -> Option<usize> {
        Some(match self {
            Operator::And => 2,
            Operator::Or => 2,
            Operator::Nand => 2,
            Operator::Nor => 2,
            Operator::Xor => 2,
            Operator::Xnor => 2,
            Operator::EQ => 3,
            Operator::NE => 3,
            Operator::LT => 3,
            Operator::LTE => 3,
            Operator::GT => 3,
            Operator::GTE => 3,
            Operator::QueEQ => 3,
            Operator::QueNE => 3,
            Operator::QueLT => 3,
            Operator::QueLTE => 3,
            Operator::QueGT => 3,
            Operator::QueGTE => 3,
            Operator::SLL => 4,
            Operator::SRL => 4,
            Operator::SLA => 4,
            Operator::SRA => 4,
            Operator::ROL => 4,
            Operator::ROR => 4,
            Operator::Plus => 5,
            Operator::Minus => 5,
            Operator::Concat => 5,
            Operator::Times => 7,
            Operator::Div => 7,
            Operator::Mod => 7,
            Operator::Rem => 7,
            Operator::Pow => 8,
            _ => {
                return None;
            }
        })
    }

    pub fn unary_precedence(&self) -> Option<usize> {
        Some(match self {
            Operator::Abs => 8,
            Operator::Not => 8,
            Operator::Plus => 6,
            Operator::Minus => 6,
            Operator::QueQue => 1,

            // LRM: All of the binary logical operators belong to the
            // class of operators with the lowest precedence. The unary
            // logical operators belong to the class of operators with
            // the highest precedence.
            Operator::And => 8,
            Operator::Or => 8,
            Operator::Nand => 8,
            Operator::Nor => 8,
            Operator::Xor => 8,
            Operator::Xnor => 8,

            _ => {
                return None;
            }
        })
    }
}

fn kind_to_operator(kind: Kind) -> Option<Operator> {
    Some(match kind {
        And => Operator::And,
        Or => Operator::Or,
        Nand => Operator::Nand,
        Nor => Operator::Nor,
        Xor => Operator::Xor,
        Xnor => Operator::Xnor,
        EQ => Operator::EQ,
        NE => Operator::NE,
        LT => Operator::LT,
        LTE => Operator::LTE,
        GT => Operator::GT,
        GTE => Operator::GTE,
        QueEQ => Operator::QueEQ,
        QueNE => Operator::QueNE,
        QueLT => Operator::QueLT,
        QueLTE => Operator::QueLTE,
        QueGT => Operator::QueGT,
        QueGTE => Operator::QueGTE,
        SLL => Operator::SLL,
        SRL => Operator::SRL,
        SLA => Operator::SLA,
        SRA => Operator::SRA,
        ROL => Operator::ROL,
        ROR => Operator::ROR,
        Plus => Operator::Plus,
        Minus => Operator::Minus,
        Concat => Operator::Concat,
        Times => Operator::Times,
        Div => Operator::Div,
        Mod => Operator::Mod,
        Rem => Operator::Rem,
        Pow => Operator::Pow,
        Abs => Operator::Abs,
        Not => Operator::Not,
        QueQue => Operator::QueQue,
        _ => {
            return None;
        }
    })
}

fn kind_to_prefix_unary_op(kind: Kind) -> Option<(Operator, usize)> {
    let op = kind_to_operator(kind)?;
    let prec = op.unary_precedence()?;
    Some((op, prec))
}

fn kind_to_binary_op(kind: Kind) -> Option<(Operator, usize)> {
    let op = kind_to_operator(kind)?;
    let prec = op.binary_precedence()?;
    Some((op, prec))
}

pub fn parse_aggregate_initial_choices(
    stream: &TokenStream,
    choices: Vec<WithPos<Choice>>,
) -> ParseResult<WithPos<Vec<ElementAssociation>>> {
    let mut choices = choices;
    let mut result = Vec::new();
    loop {
        expect_token!(
            stream,
            token,
            RightPar => {
                if choices.len() == 1 {
                    if let Some(WithPos{item: Choice::Expression(expr), pos}) = choices.pop() {
                        result.push(ElementAssociation::Positional(WithPos::new(expr, pos)));
                        return Ok(WithPos::from(result, token.pos.clone()))
                    }
                }

                return Err(token.kinds_error(&[RightArrow]));
            },
            Comma => {
                if choices.len() == 1 {
                    if let Some(WithPos{item: Choice::Expression(expr), pos}) = choices.pop() {
                        result.push(ElementAssociation::Positional(WithPos::new(expr, pos)));
                        choices = parse_choices(stream)?;
                        continue;
                    }
                }

                return Err(token.kinds_error(&[RightArrow]));
            },
            RightArrow => {
                let rhs = parse_expression(stream)?;
                result.push(ElementAssociation::Named(choices, rhs));

                expect_token!(
                    stream,
                    token,
                    RightPar => {
                        return Ok(WithPos::from(result, token.pos.clone()))
                    },
                    Comma => {
                        choices = parse_choices(stream)?;
                    }
                )
            }
        );
    }
}

pub fn parse_aggregate(stream: &TokenStream) -> ParseResult<WithPos<Vec<ElementAssociation>>> {
    stream.expect_kind(LeftPar)?;
    if let Some(token) = stream.pop_if_kind(RightPar) {
        return Ok(WithPos::from(Vec::new(), token.pos.clone()));
    };
    let choices = parse_choices(stream)?;
    parse_aggregate_initial_choices(stream, choices)
}

fn parse_half_range(
    stream: &TokenStream,
    left_expr: WithPos<Expression>,
    direction: Direction,
) -> ParseResult<WithPos<DiscreteRange>> {
    let right_expr = parse_expression(stream)?;
    let pos = left_expr.pos.combine(&right_expr.pos);

    let range = DiscreteRange::Range(ast::Range::Range(RangeConstraint {
        direction,
        left_expr: Box::new(left_expr),
        right_expr: Box::new(right_expr),
    }));

    Ok(WithPos::new(range, pos))
}

fn parse_choice(stream: &TokenStream) -> ParseResult<WithPos<Choice>> {
    if let Some(token) = stream.pop_if_kind(Others) {
        return Ok(WithPos::new(Choice::Others, token.pos.clone()));
    }
    let left_expr = parse_expression(stream)?;

    if stream.skip_if_kind(To) {
        let range = parse_half_range(stream, left_expr, Direction::Ascending)?;
        Ok(range.map_into(Choice::DiscreteRange))
    } else if stream.skip_if_kind(Downto) {
        let range = parse_half_range(stream, left_expr, Direction::Descending)?;
        Ok(range.map_into(Choice::DiscreteRange))
    } else {
        Ok(left_expr.map_into(Choice::Expression))
    }
}

pub fn parse_choices(stream: &TokenStream) -> ParseResult<Vec<WithPos<Choice>>> {
    let mut choices = Vec::new();
    loop {
        choices.push(parse_choice(stream)?);

        if !stream.skip_if_kind(Bar) {
            break;
        }
    }
    Ok(choices)
}

/// LRM 9.3.7 Allocators
fn parse_allocator(stream: &TokenStream) -> ParseResult<WithPos<Allocator>> {
    stream.expect_kind(New)?;
    let type_mark = parse_type_mark(stream)?;

    if stream.skip_if_kind(Tick) {
        let expr = parse_expression(stream)?;
        let pos = type_mark.pos.clone().combine_into(&expr);
        Ok(WithPos {
            item: Allocator::Qualified(QualifiedExpression { type_mark, expr }),
            pos,
        })
    } else {
        let mut pos = type_mark.pos.clone();

        let constraint = {
            if let Some(constraint) = parse_subtype_constraint(stream)? {
                pos = pos.combine(&constraint.pos);
                Some(constraint)
            } else {
                None
            }
        };

        let subtype = SubtypeIndication {
            resolution: ResolutionIndication::Unresolved,
            type_mark,
            constraint,
        };

        Ok(WithPos {
            item: Allocator::Subtype(subtype),
            pos,
        })
    }
}

pub fn name_to_type_mark(name: WithPos<Name>) -> ParseResult<WithPos<TypeMark>> {
    let pos = name.pos.clone();
    let type_mark = name
        .try_map_into(|name| match name {
            Name::Attribute(attr) => {
                if let Some(typattr) = attr.as_type() {
                    Some(TypeMark {
                        name: attr.name.try_map_into(name_to_selected_name)?,
                        attr: Some(typattr),
                    })
                } else {
                    None
                }
            }
            _ => Some(TypeMark {
                name: WithPos::from(name_to_selected_name(name)?, pos.clone()),
                attr: None,
            }),
        })
        .ok_or_else(|| Diagnostic::error(&pos, "Expected type mark"))?;

    Ok(type_mark)
}

fn name_to_selected_name(name: Name) -> Option<SelectedName> {
    match name {
        Name::Designator(d) => Some(SelectedName::Designator(d)),
        Name::Selected(p, d) => Some(SelectedName::Selected(
            Box::new(p.try_map_into(name_to_selected_name)?),
            d,
        )),
        _ => None,
    }
}

fn parse_expression_or_aggregate(stream: &TokenStream) -> ParseResult<WithPos<Expression>> {
    let mut choices = parse_choices(stream)?;

    if choices.len() == 1
        && matches!(
            choices.first().unwrap(),
            WithPos {
                item: Choice::Expression(_),
                ..
            }
        )
    {
        let WithPos {
            item: Choice::Expression(expr),
            pos
        } = choices.pop().unwrap() else {
            unreachable!();
        };

        peek_token!(
            stream, token,

            // Was aggregate
            Comma | RightArrow => {
                Ok(parse_aggregate_initial_choices(
                    stream,
                    vec![WithPos::new(Choice::Expression(expr), pos)],
                )?.map_into(Expression::Aggregate))
            },

            // Was expression with parenthesis
            RightPar => {
                stream.skip();
                // Lexical position between parenthesis
                let expr = WithPos {
                    item: expr,
                    pos: token.pos.clone(),
                };
                Ok(expr)
            }
        )
    } else {
        // Must be aggregate
        Ok(parse_aggregate_initial_choices(stream, choices)?.map_into(Expression::Aggregate))
    }
}

/// Parse a primary value which is:
/// 1. CHARACTER_LITERAL|INTEGER_LITERAL|IDENTIFIER|BOOLEAN_LITERAL
/// 2. (expression)
/// 3. PREFIX_UNARY_OP expression
fn parse_primary(stream: &TokenStream) -> ParseResult<WithPos<Expression>> {
    let token = stream.peek_expect()?;
    match token.kind {
        Identifier | LtLt => {
            let name = parse_name(stream)?;
            if stream.skip_if_kind(Tick) {
                let lpar = stream.expect_kind(LeftPar)?;
                let expr = parse_expression_or_aggregate(stream)?.combine_pos_with(&lpar);
                let pos = name.pos.combine(&expr);
                Ok(WithPos {
                    item: Expression::Qualified(Box::new(QualifiedExpression {
                        type_mark: name_to_type_mark(name)?,
                        expr,
                    })),
                    pos,
                })
            } else {
                Ok(name_to_expression(name))
            }
        }
        BitString => {
            stream.skip();
            Ok(token
                .to_bit_string()?
                .map_into(|bs| Expression::Literal(Literal::BitString(bs))))
        }
        Character => {
            stream.skip();
            Ok(token
                .to_character_value()?
                .map_into(|chr| Expression::Literal(Literal::Character(chr))))
        }
        StringLiteral => {
            if stream.next_kinds_are(&[StringLiteral, LeftPar]) {
                // Probably an function call via operator symbol "foo"()
                parse_name(stream)
                    .map(|name| name.map_into(|name| Expression::Name(Box::new(name))))
            } else {
                stream.skip();
                Ok(token
                    .to_string_value()?
                    .map_into(|string| Expression::Literal(Literal::String(string))))
            }
        }
        Null => {
            stream.skip();
            Ok(WithPos {
                item: Expression::Literal(Literal::Null),
                pos: token.pos.clone(),
            })
        }
        New => {
            let alloc = parse_allocator(stream)?;

            let new_pos = token.pos.combine(&alloc);
            Ok(WithPos {
                item: Expression::New(Box::new(alloc)),
                pos: new_pos,
            })
        }
        AbstractLiteral => {
            stream.skip();
            let value = token.to_abstract_literal()?;
            // Physical unit
            if let Some(unit_token) = stream.pop_if_kind(Identifier) {
                let unit = unit_token.to_identifier_value()?;
                let pos = value.pos.combine_into(&unit);
                let physical = PhysicalLiteral {
                    value: value.item,
                    unit: WithRef::new(unit),
                };
                Ok(WithPos {
                    item: Expression::Literal(Literal::Physical(physical)),
                    pos,
                })
            } else {
                Ok(value.map_into(|value| Expression::Literal(Literal::AbstractLiteral(value))))
            }
        }

        LeftPar => {
            stream.skip();
            parse_expression_or_aggregate(stream).map(|expr| expr.combine_pos_with(&token))
        }

        kind => {
            // Prefix unary operation
            if let Some((unary_op, op_precedence)) = kind_to_prefix_unary_op(kind) {
                stream.skip();

                let expr = parse_expr(stream, op_precedence)?;
                let pos = token.pos.combine(&expr);

                Ok(WithPos {
                    item: Expression::Unary(
                        WithPos::new(WithRef::new(unary_op), token.pos.clone()),
                        Box::new(expr),
                    ),
                    pos,
                })
            } else {
                Err(Diagnostic::error(
                    stream.pos_before(token),
                    "Expected {expression}",
                ))
            }
        }
    }
}

fn parse_expr(stream: &TokenStream, min_precedence: usize) -> ParseResult<WithPos<Expression>> {
    let mut lhs = parse_primary(stream)?;
    while let Some(token) = stream.peek() {
        if token.kind == RightPar {
            return Ok(lhs);
        };

        if let Some((binary_op, op_precedence)) = kind_to_binary_op(token.kind) {
            // Binary operation
            if op_precedence > min_precedence {
                stream.skip();
                let rhs = parse_expr(stream, op_precedence)?;
                let pos = lhs.pos.combine(&rhs);
                lhs = WithPos {
                    item: Expression::Binary(
                        WithPos::new(WithRef::new(binary_op), token.pos.clone()),
                        Box::new(lhs),
                        Box::new(rhs),
                    ),
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
pub fn parse_expression(stream: &TokenStream) -> ParseResult<WithPos<Expression>> {
    let state = stream.state();
    parse_expr(stream, 0).map_err(|err| {
        stream.set_state(state);
        err
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{AbstractLiteral, Name};
    use crate::data::Latin1String;
    use crate::syntax::test::Code;

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
    fn parses_operator_symbol() {
        let code = Code::new("\"+\"(1, 2)");
        assert_eq!(
            code.with_stream(parse_expression),
            code.s1("\"+\"(1, 2)")
                .name()
                .map_into(|name| Expression::Name(Box::new(name)))
        );
    }

    #[test]
    fn parses_exteral_name() {
        let code = Code::new("<< signal dut.foo : boolean >>");
        assert_eq!(
            code.with_stream(parse_expression),
            code.s1("<< signal dut.foo : boolean >>")
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

    fn int(value: u64) -> Literal {
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
            item: Expression::Binary(
                WithPos::new(WithRef::new(Operator::Plus), code.s1("+").pos()),
                Box::new(lhs),
                Box::new(rhs),
            ),
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
            item: Expression::Binary(
                WithPos::new(WithRef::new(Operator::Minus), code.s1("-").pos()),
                Box::new(lhs),
                Box::new(rhs),
            ),
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
            item: Expression::Unary(
                WithPos::new(WithRef::new(Operator::Abs), code.s1("abs").pos()),
                Box::new(expr),
            ),
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
            item: Expression::Unary(
                WithPos::new(WithRef::new(Operator::QueQue), code.s1("??").pos()),
                Box::new(expr),
            ),
            pos: code.pos(),
        };

        assert_eq!(code.with_stream(parse_expression), expr_cond);
    }

    #[test]
    fn parses_not_expression() {
        let code = Code::new("not false");
        let name_false = WithPos {
            item: Expression::Name(Box::new(Name::Designator(
                Designator::Identifier(code.symbol("false")).into_ref(),
            ))),
            pos: code.s1("false").pos(),
        };

        let expr_not = WithPos {
            item: Expression::Unary(
                WithPos::new(WithRef::new(Operator::Not), code.s1("not").pos()),
                Box::new(name_false),
            ),
            pos: code.pos(),
        };

        assert_eq!(code.with_stream(parse_expression), expr_not);
    }

    #[test]
    fn parses_new_allocator_qualified() {
        let code = Code::new("new integer_vector'(0, 1)");
        let type_mark = code.s1("integer_vector").type_mark();
        let expr = code.s1("(0, 1)").expr();

        let alloc = WithPos {
            item: Allocator::Qualified(QualifiedExpression { type_mark, expr }),
            pos: code.s1("integer_vector'(0, 1)").pos(),
        };

        let new_expr = WithPos {
            item: Expression::New(Box::new(alloc)),
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
            item: Expression::New(Box::new(alloc)),
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
            item: Expression::New(Box::new(alloc)),
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
            item: Expression::New(Box::new(alloc)),
            pos: code.pos(),
        };

        assert_eq!(code.with_stream(parse_expression), new_expr);
    }

    #[test]
    fn parses_physical_unit_expression() {
        let code = Code::new("1 ns");
        let expr = WithPos {
            item: Expression::Literal(Literal::Physical(PhysicalLiteral {
                value: AbstractLiteral::Integer(1),
                unit: code.s1("ns").ident().into_ref(),
            })),
            pos: code.pos(),
        };

        assert_eq!(code.with_stream(parse_expression), expr);
    }

    #[test]
    fn parses_physical_unit_expression_real() {
        let code = Code::new("1.0 ns");
        let expr = WithPos {
            item: Expression::Literal(Literal::Physical(PhysicalLiteral {
                value: AbstractLiteral::Real(1.0),
                unit: code.s1("ns").ident().into_ref(),
            })),
            pos: code.pos(),
        };

        assert_eq!(code.with_stream(parse_expression), expr);
    }

    #[test]
    fn parses_physical_unit_expression_binary() {
        let code = Code::new("2 * 1 ns");
        let time_expr = WithPos {
            item: Expression::Literal(Literal::Physical(PhysicalLiteral {
                value: AbstractLiteral::Integer(1),
                unit: code.s1("ns").ident().into_ref(),
            })),
            pos: code.s1("1 ns").pos(),
        };
        let two_expr = WithPos {
            item: Expression::Literal(int(2)),
            pos: code.s1("2").pos(),
        };
        let expr = WithPos {
            item: Expression::Binary(
                WithPos::new(WithRef::new(Operator::Times), code.s1("*").pos()),
                Box::new(two_expr),
                Box::new(time_expr),
            ),
            pos: code.pos(),
        };
        assert_eq!(code.with_stream(parse_expression), expr);
    }

    #[test]
    fn parses_physical_unit_expression_unary() {
        let code = Code::new("- 1 ns");
        let time_expr = WithPos {
            item: Expression::Literal(Literal::Physical(PhysicalLiteral {
                value: AbstractLiteral::Integer(1),
                unit: code.s1("ns").ident().into_ref(),
            })),
            pos: code.s1("1 ns").pos(),
        };
        let expr = WithPos {
            item: Expression::Unary(
                WithPos::new(WithRef::new(Operator::Minus), code.s1("-").pos()),
                Box::new(time_expr),
            ),
            pos: code.pos(),
        };

        assert_eq!(code.with_stream(parse_expression), expr);
    }

    #[test]
    fn parses_qualified_expression() {
        let code = Code::new("foo'(1+2)");
        let type_mark = code.s1("foo").type_mark();
        let expr = code.s1("(1+2)").expr();

        let qexpr = WithPos {
            item: Expression::Qualified(Box::new(QualifiedExpression { type_mark, expr })),
            pos: code.pos(),
        };

        assert_eq!(code.with_stream(parse_expression), qexpr);
    }

    #[test]
    fn qualified_expression_precedence() {
        let code = Code::new("mark0'(0) < mark1'(1)");
        let expr = WithPos {
            item: Expression::Binary(
                WithPos::new(WithRef::new(Operator::LT), code.s1("<").pos()),
                Box::new(code.s1("mark0'(0)").expr()),
                Box::new(code.s1("mark1'(1)").expr()),
            ),
            pos: code.pos(),
        };

        assert_eq!(code.expr(), expr);
    }

    #[test]
    fn parses_qualified_aggregate() {
        let code = Code::new("foo'(others => '1')");
        let type_mark = code.s1("foo").type_mark();
        let expr = code.s1("(others => '1')").expr();

        let qexpr = WithPos {
            item: Expression::Qualified(Box::new(QualifiedExpression { type_mark, expr })),
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
            vec![one_expr.map_into(Choice::Expression)],
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
            vec![
                one_expr.map_into(Choice::Expression),
                two_expr.map_into(Choice::Expression),
            ],
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

        let assoc_list = vec![ElementAssociation::Named(
            vec![WithPos::new(Choice::Others, code.s1("others"))],
            one_expr,
        )];
        let expr = WithPos {
            item: Expression::Aggregate(assoc_list),
            pos: code.pos(),
        };

        assert_eq!(code.with_stream(parse_expression), expr);
    }

    #[test]
    fn parses_aggregate_range() {
        for direction in [Direction::Descending, Direction::Ascending].iter() {
            let (pos, code) = {
                if *direction == Direction::Descending {
                    let code = Code::new("(1 downto 0 => 2)");
                    (code.s1("1 downto 0").pos(), code)
                } else {
                    let code = Code::new("(1 to 0 => 2)");
                    (code.s1("1 to 0").pos(), code)
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
                vec![WithPos::new(Choice::DiscreteRange(range), pos)],
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
            ElementAssociation::Named(
                vec![WithPos::new(Choice::Others, code.s("others", 1))],
                one_expr,
            ),
            ElementAssociation::Named(
                vec![WithPos::new(Choice::Others, code.s("others", 2))],
                two_expr,
            ),
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
            ElementAssociation::Named(vec![one_expr.map_into(Choice::Expression)], two_expr),
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
            item: Expression::Binary(
                WithPos::new(WithRef::new(Operator::Plus), code.s("+", 2).pos()),
                Box::new(two),
                Box::new(three),
            ),
            pos: code.s1("(2 + 3)").pos(),
        };

        let expr_add1 = WithPos {
            item: Expression::Binary(
                WithPos::new(WithRef::new(Operator::Plus), code.s("+", 1).pos()),
                Box::new(one),
                Box::new(expr_add0),
            ),
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
            item: Expression::Binary(
                WithPos::new(WithRef::new(Operator::Plus), code.s("+", 1).pos()),
                Box::new(one),
                Box::new(two),
            ),
            pos: code.s1("(1 + 2)").pos(),
        };

        let expr_add1 = WithPos {
            item: Expression::Binary(
                WithPos::new(WithRef::new(Operator::Plus), code.s("+", 2).pos()),
                Box::new(expr_add0),
                Box::new(three),
            ),
            pos: code.pos(),
        };

        assert_eq!(code.with_stream(parse_expression), expr_add1);
    }

    /// Format expression as a string to simplify testing of precedence.
    fn fmt(expr: &WithPos<Expression>) -> String {
        match expr.item {
            Expression::Binary(ref op, ref lhs, ref rhs) => {
                format!("({} {:?} {})", fmt(lhs), op.item.item, fmt(rhs))
            }
            Expression::Unary(ref op, ref rhs) => format!("({:?} {})", op.item.item, fmt(rhs)),
            Expression::Literal(ref lit) => match lit {
                Literal::Null => "null".to_string(),
                // @TODO quote and escape
                Literal::String(val) => val.to_string(),
                Literal::AbstractLiteral(val) => match val {
                    AbstractLiteral::Integer(val) => format!("Integer({val})"),
                    AbstractLiteral::Real(val) => format!("Real({val})"),
                },
                Literal::Character(val) => format!("'{}'", Latin1String::new(&[*val])),
                Literal::Physical(ref physical) => match physical.value {
                    AbstractLiteral::Integer(val) => {
                        format!("Physical(Integer({}), {})", val, physical.unit.item.name())
                    }
                    AbstractLiteral::Real(val) => {
                        format!("Physical(Real({}), {})", val, physical.unit.item.name())
                    }
                },
                _ => {
                    println!("{}", expr.pos.code_context());
                    panic!("Cannot format {lit:?}");
                }
            },
            _ => {
                println!("{}", expr.pos.code_context());
                panic!("Cannot format {expr:?}");
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
            Err(Diagnostic::error(
                &code.s1(",").pos(),
                "Expected {expression}"
            ))
        );

        let code = Code::new("fun(arg0,)");
        assert_eq!(
            code.with_partial_stream(parse_expression),
            Err(Diagnostic::error(
                &code.s1(")").pos(),
                "Expected {expression}"
            ))
        );
        let code = Code::new("fun(arg0,,)");
        assert_eq!(
            code.with_partial_stream(parse_expression),
            Err(Diagnostic::error(
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

        assert_expression_is("1+2*3", "(Integer(1) Plus (Integer(2) Times Integer(3)))");

        assert_expression_is("(1+2)*3", "((Integer(1) Plus Integer(2)) Times Integer(3))");

        // Multiplication has precedence over negation.
        assert_expression_is("-1 * 2", "(Minus (Integer(1) Times Integer(2)))");

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
