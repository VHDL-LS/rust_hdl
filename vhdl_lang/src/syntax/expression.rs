// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::common::ParseResult;
use super::names::{parse_name, parse_type_mark};
use super::subtype_indication::parse_subtype_constraint;
use super::tokens::{Kind, Kind::*};
use crate::ast::{Literal, *};
use crate::data::{Diagnostic, WithTokenSpan};
use crate::syntax::TokenAccess;
use crate::{ast, TokenSpan};
use vhdl_lang::syntax::parser::ParsingContext;

impl WithTokenSpan<Name> {
    pub fn into_expression(self) -> WithTokenSpan<Expression> {
        self.map_into(|name| Expression::Name(Box::new(name)))
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
    ctx: &mut ParsingContext<'_>,
    choices: Vec<WithTokenSpan<Choice>>,
) -> ParseResult<WithTokenSpan<Vec<ElementAssociation>>> {
    let mut choices = choices;
    let mut result = Vec::new();
    loop {
        expect_token!(
            ctx.stream,
            token,
            token_id,
            RightPar => {
                if choices.len() == 1 {
                    if let Some(WithTokenSpan{item: Choice::Expression(expr), span}) = choices.pop() {
                        result.push(ElementAssociation::Positional(WithTokenSpan::new(expr, span)));
                        return Ok(WithTokenSpan::from(result, token_id))
                    }
                }

                return Err(token.kinds_error(&[RightArrow]));
            },
            Comma => {
                if choices.len() == 1 {
                    if let Some(WithTokenSpan{item: Choice::Expression(expr), span}) = choices.pop() {
                        result.push(ElementAssociation::Positional(WithTokenSpan::new(expr, span)));
                        choices = parse_choices(ctx)?;
                        continue;
                    }
                }

                return Err(token.kinds_error(&[RightArrow]));
            },
            RightArrow => {
                let rhs = parse_expression(ctx)?;
                result.push(ElementAssociation::Named(choices, rhs));

                expect_token!(
                    ctx.stream,
                    token,
                    token_id,
                    RightPar => {
                        return Ok(WithTokenSpan::from(result, token_id))
                    },
                    Comma => {
                        choices = parse_choices(ctx)?;
                    }
                )
            }
        );
    }
}

pub fn parse_aggregate(
    ctx: &mut ParsingContext<'_>,
) -> ParseResult<WithTokenSpan<Vec<ElementAssociation>>> {
    let start_tok = ctx.stream.expect_kind(LeftPar)?;
    if let Some(token) = ctx.stream.pop_if_kind(RightPar) {
        return Ok(WithTokenSpan::from(
            Vec::new(),
            TokenSpan::new(start_tok, token),
        ));
    };
    let choices = parse_choices(ctx)?;
    parse_aggregate_initial_choices(ctx, choices)
}

fn parse_half_range(
    ctx: &mut ParsingContext<'_>,
    left_expr: WithTokenSpan<Expression>,
    direction: Direction,
) -> ParseResult<WithTokenSpan<DiscreteRange>> {
    let right_expr = parse_expression(ctx)?;
    let pos = left_expr.span.combine(right_expr.span);

    let range = DiscreteRange::Range(ast::Range::Range(RangeConstraint {
        direction,
        left_expr: Box::new(left_expr),
        right_expr: Box::new(right_expr),
    }));

    Ok(WithTokenSpan::new(range, pos))
}

fn parse_choice(ctx: &mut ParsingContext<'_>) -> ParseResult<WithTokenSpan<Choice>> {
    if let Some(token) = ctx.stream.pop_if_kind(Others) {
        return Ok(WithTokenSpan::from(Choice::Others, token));
    }
    let left_expr = parse_expression(ctx)?;

    if ctx.stream.skip_if_kind(To) {
        let range = parse_half_range(ctx, left_expr, Direction::Ascending)?;
        Ok(range.map_into(Choice::DiscreteRange))
    } else if ctx.stream.skip_if_kind(Downto) {
        let range = parse_half_range(ctx, left_expr, Direction::Descending)?;
        Ok(range.map_into(Choice::DiscreteRange))
    } else {
        Ok(left_expr.map_into(Choice::Expression))
    }
}

pub fn parse_choices(ctx: &mut ParsingContext<'_>) -> ParseResult<Vec<WithTokenSpan<Choice>>> {
    let mut choices = Vec::new();
    loop {
        choices.push(parse_choice(ctx)?);

        if !ctx.stream.skip_if_kind(Bar) {
            break;
        }
    }
    Ok(choices)
}

/// LRM 9.3.7 Allocators
fn parse_allocator(ctx: &mut ParsingContext<'_>) -> ParseResult<WithTokenSpan<Allocator>> {
    ctx.stream.expect_kind(New)?;
    let type_mark = parse_type_mark(ctx)?;

    if ctx.stream.skip_if_kind(Tick) {
        let expr = parse_expression(ctx)?;
        let span = type_mark.span.combine(expr.span);
        Ok(WithTokenSpan {
            item: Allocator::Qualified(QualifiedExpression { type_mark, expr }),
            span,
        })
    } else {
        let mut span = type_mark.span;

        let constraint = {
            if let Some(constraint) = parse_subtype_constraint(ctx)? {
                span = span.combine(constraint.span);
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

        Ok(WithTokenSpan {
            item: Allocator::Subtype(subtype),
            span,
        })
    }
}

pub fn name_to_type_mark(
    ctx: &mut ParsingContext<'_>,
    name: WithTokenSpan<Name>,
) -> ParseResult<WithTokenSpan<TypeMark>> {
    let pos = name.to_pos(ctx);
    let name_span = name.span;
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
                name: WithTokenSpan::from(name_to_selected_name(name)?, name_span),
                attr: None,
            }),
        })
        .ok_or_else(|| Diagnostic::syntax_error(&pos, "Expected type mark"))?;

    Ok(type_mark)
}

fn name_to_selected_name(name: Name) -> Option<Name> {
    match name {
        Name::Designator(d) => Some(Name::Designator(d)),
        Name::Selected(p, d) => Some(Name::Selected(
            Box::new(p.try_map_into(name_to_selected_name)?),
            d,
        )),
        _ => None,
    }
}

fn parse_expression_or_aggregate(
    ctx: &mut ParsingContext<'_>,
) -> ParseResult<WithTokenSpan<Expression>> {
    let mut choices = parse_choices(ctx)?;

    if choices.len() == 1
        && matches!(
            choices.first().unwrap(),
            WithTokenSpan {
                item: Choice::Expression(_),
                ..
            }
        )
    {
        let WithTokenSpan {
            item: Choice::Expression(expr),
            span,
        } = choices.pop().unwrap()
        else {
            unreachable!();
        };

        peek_token!(
            ctx.stream, token, token_id,

            // Was aggregate
            Comma | RightArrow => {
                Ok(parse_aggregate_initial_choices(
                    ctx,
                    vec![WithTokenSpan::new(Choice::Expression(expr), span)],
                )?.map_into(Expression::Aggregate))
            },

            // Was expression with parenthesis
            RightPar => {
                ctx.stream.skip();
                // Lexical position between parenthesis
                let expr = WithTokenSpan::from(
                    expr,
                    token_id
                );
                Ok(expr)
            }
        )
    } else {
        // Must be aggregate
        Ok(parse_aggregate_initial_choices(ctx, choices)?.map_into(Expression::Aggregate))
    }
}

/// Parse a primary value which is:
/// 1. CHARACTER_LITERAL|INTEGER_LITERAL|IDENTIFIER|BOOLEAN_LITERAL
/// 2. (expression)
/// 3. PREFIX_UNARY_OP expression
fn parse_primary(ctx: &mut ParsingContext<'_>) -> ParseResult<WithTokenSpan<Expression>> {
    let token = ctx.stream.peek_expect()?;
    let token_id = ctx.stream.get_current_token_id();
    match token.kind {
        Identifier | LtLt => {
            let name = parse_name(ctx)?;
            if ctx.stream.skip_if_kind(Tick) {
                let lpar = ctx.stream.expect_kind(LeftPar)?;
                let expr = parse_expression_or_aggregate(ctx)?.start_with(lpar);
                let span = name.span.combine(expr.span);
                Ok(WithTokenSpan::new(
                    Expression::Qualified(Box::new(QualifiedExpression {
                        type_mark: name_to_type_mark(ctx, name)?,
                        expr,
                    })),
                    span,
                ))
            } else {
                Ok(name.into_expression())
            }
        }
        BitString => {
            ctx.stream.skip();
            Ok(token
                .to_bit_string(token_id)?
                .map_into_span(|bs| Expression::Literal(Literal::BitString(bs))))
        }
        Character => {
            ctx.stream.skip();
            Ok(token
                .to_character_value(token_id)?
                .map_into_span(|chr| Expression::Literal(Literal::Character(chr))))
        }
        StringLiteral => {
            if ctx.stream.next_kinds_are(&[StringLiteral, LeftPar]) {
                // Probably a function call via operator symbol "foo"()
                parse_name(ctx).map(|name| name.map_into(|name| Expression::Name(Box::new(name))))
            } else {
                ctx.stream.skip();
                Ok(token
                    .to_string_value(token_id)?
                    .map_into_span(|string| Expression::Literal(Literal::String(string))))
            }
        }
        Null => {
            ctx.stream.skip();
            Ok(WithTokenSpan::from(
                Expression::Literal(Literal::Null),
                token_id,
            ))
        }
        New => {
            let alloc = parse_allocator(ctx)?;

            let new_pos = TokenSpan::new(token_id, alloc.span.end_token);
            Ok(WithTokenSpan::new(
                Expression::New(Box::new(alloc)),
                new_pos,
            ))
        }
        AbstractLiteral => {
            ctx.stream.skip();
            let value = token.to_abstract_literal(token_id)?;
            // Physical unit
            if let Some(unit_token) = ctx.stream.pop_if_kind(Identifier) {
                let unit = ctx
                    .stream
                    .get_token(unit_token)
                    .to_identifier_value(unit_token)?;
                let span = TokenSpan::new(value.token, unit.token);
                let physical = PhysicalLiteral {
                    value: value.item,
                    unit: WithRef::new(unit),
                };
                Ok(WithTokenSpan::new(
                    Expression::Literal(Literal::Physical(physical)),
                    span,
                ))
            } else {
                Ok(value
                    .map_into_span(|value| Expression::Literal(Literal::AbstractLiteral(value))))
            }
        }

        LeftPar => {
            ctx.stream.skip();
            parse_expression_or_aggregate(ctx).map(|expr| expr.start_with(token_id))
        }

        kind => {
            // Prefix unary operation
            if let Some((unary_op, op_precedence)) = kind_to_prefix_unary_op(kind) {
                ctx.stream.skip();

                let expr = parse_expr(ctx, op_precedence)?;
                let span = TokenSpan::new(token_id, expr.span.end_token);

                Ok(WithTokenSpan::new(
                    Expression::Unary(
                        WithToken::new(WithRef::new(unary_op), token_id),
                        Box::new(expr),
                    ),
                    span,
                ))
            } else {
                Err(Diagnostic::syntax_error(
                    ctx.stream.pos_before(token),
                    "Expected {expression}",
                ))
            }
        }
    }
}

fn parse_expr(
    ctx: &mut ParsingContext<'_>,
    min_precedence: usize,
) -> ParseResult<WithTokenSpan<Expression>> {
    let mut lhs = parse_primary(ctx)?;
    while let Some(token) = ctx.stream.peek() {
        let token_id = ctx.stream.get_current_token_id();
        if token.kind == RightPar {
            return Ok(lhs);
        };

        if let Some((binary_op, op_precedence)) = kind_to_binary_op(token.kind) {
            // Binary operation
            if op_precedence > min_precedence {
                ctx.stream.skip();
                let rhs = parse_expr(ctx, op_precedence)?;
                let pos = lhs.span.combine(rhs.span);
                lhs = WithTokenSpan::new(
                    Expression::Binary(
                        WithToken::new(WithRef::new(binary_op), token_id),
                        Box::new(lhs),
                        Box::new(rhs),
                    ),
                    pos,
                );
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
pub fn parse_expression(ctx: &mut ParsingContext<'_>) -> ParseResult<WithTokenSpan<Expression>> {
    let state = ctx.stream.state();
    parse_expr(ctx, 0).map_err(|err| {
        ctx.stream.set_state(state);
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
            WithTokenSpan {
                item: Expression::Literal(Literal::Character(b'a')),
                span: code.token_span()
            }
        );
    }

    #[test]
    fn parses_abstract_literal_integer() {
        let code = Code::new("71");
        assert_eq!(
            code.with_stream(parse_expression),
            WithTokenSpan {
                item: Expression::Literal(Literal::AbstractLiteral(AbstractLiteral::Integer(71))),
                span: code.token_span()
            }
        );
    }

    #[test]
    fn parses_abstract_literal_real() {
        let code = Code::new("7.1");
        assert_eq!(
            code.with_stream(parse_expression),
            WithTokenSpan {
                item: Expression::Literal(Literal::AbstractLiteral(AbstractLiteral::Real(7.1))),
                span: code.token_span()
            }
        );
    }

    #[test]
    fn parses_string_literal() {
        let code = Code::new("\"string\"");
        assert_eq!(
            code.with_stream(parse_expression),
            WithTokenSpan {
                item: Expression::Literal(Literal::String(Latin1String::from_utf8_unchecked(
                    "string"
                ))),
                span: code.token_span()
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
            WithTokenSpan {
                item: Expression::Literal(Literal::Null),
                span: code.token_span()
            }
        );
    }

    fn int(value: u64) -> Literal {
        Literal::AbstractLiteral(AbstractLiteral::Integer(value))
    }

    #[test]
    fn parses_add_expression() {
        let code = Code::new("1 + 2");

        let lhs = WithTokenSpan {
            item: Expression::Literal(int(1)),
            span: code.s1("1").token_span(),
        };

        let rhs = WithTokenSpan {
            item: Expression::Literal(int(2)),
            span: code.s1("2").token_span(),
        };

        let expr_add = WithTokenSpan {
            item: Expression::Binary(
                WithToken::new(WithRef::new(Operator::Plus), code.s1("+").token()),
                Box::new(lhs),
                Box::new(rhs),
            ),
            span: code.token_span(),
        };

        assert_eq!(code.with_stream(parse_expression), expr_add);
    }

    #[test]
    fn parses_sub_expression() {
        let code = Code::new("1 - 2");
        let lhs = WithTokenSpan {
            item: Expression::Literal(int(1)),
            span: code.s1("1").token_span(),
        };

        let rhs = WithTokenSpan {
            item: Expression::Literal(int(2)),
            span: code.s1("2").token_span(),
        };

        let expr_sub = WithTokenSpan {
            item: Expression::Binary(
                WithToken::new(WithRef::new(Operator::Minus), code.s1("-").token()),
                Box::new(lhs),
                Box::new(rhs),
            ),
            span: code.token_span(),
        };

        assert_eq!(code.with_stream(parse_expression), expr_sub);
    }

    #[test]
    fn parses_abs_expression() {
        let code = Code::new("abs 9");
        let expr = WithTokenSpan {
            item: Expression::Literal(int(9)),
            span: code.s1("9").token_span(),
        };

        let expr_abs = WithTokenSpan {
            item: Expression::Unary(
                WithToken::new(WithRef::new(Operator::Abs), code.s1("abs").token()),
                Box::new(expr),
            ),
            span: code.token_span(),
        };

        assert_eq!(code.with_stream(parse_expression), expr_abs);
    }

    #[test]
    fn parses_condition_operator() {
        let code = Code::new("?? 9");
        let expr = WithTokenSpan {
            item: Expression::Literal(int(9)),
            span: code.s1("9").token_span(),
        };

        let expr_cond = WithTokenSpan {
            item: Expression::Unary(
                WithToken::new(WithRef::new(Operator::QueQue), code.s1("??").token()),
                Box::new(expr),
            ),
            span: code.token_span(),
        };

        assert_eq!(code.with_stream(parse_expression), expr_cond);
    }

    #[test]
    fn parses_not_expression() {
        let code = Code::new("not false");
        let name_false = WithTokenSpan {
            item: Expression::Name(Box::new(Name::Designator(
                Designator::Identifier(code.symbol("false")).into_ref(),
            ))),
            span: code.s1("false").token_span(),
        };

        let expr_not = WithTokenSpan {
            item: Expression::Unary(
                WithToken::new(WithRef::new(Operator::Not), code.s1("not").token()),
                Box::new(name_false),
            ),
            span: code.token_span(),
        };

        assert_eq!(code.with_stream(parse_expression), expr_not);
    }

    #[test]
    fn parses_new_allocator_qualified() {
        let code = Code::new("new integer_vector'(0, 1)");
        let type_mark = code.s1("integer_vector").type_mark();
        let expr = code.s1("(0, 1)").expr();

        let alloc = WithTokenSpan {
            item: Allocator::Qualified(QualifiedExpression { type_mark, expr }),
            span: code.s1("integer_vector'(0, 1)").token_span(),
        };

        let new_expr = WithTokenSpan {
            item: Expression::New(Box::new(alloc)),
            span: code.token_span(),
        };

        assert_eq!(code.with_stream(parse_expression), new_expr);
    }

    #[test]
    fn parses_new_allocator_subtype() {
        let code = Code::new("new integer_vector");

        let alloc = WithTokenSpan {
            item: Allocator::Subtype(code.s1("integer_vector").subtype_indication()),
            span: code.s1("integer_vector").token_span(),
        };

        let new_expr = WithTokenSpan {
            item: Expression::New(Box::new(alloc)),
            span: code.token_span(),
        };

        assert_eq!(code.with_stream(parse_expression), new_expr);
    }

    #[test]
    fn parses_new_allocator_subtype_constraint() {
        let code = Code::new("new integer_vector(0 to 1)");

        let alloc = WithTokenSpan {
            item: Allocator::Subtype(code.s1("integer_vector(0 to 1)").subtype_indication()),
            span: code.s1("integer_vector(0 to 1)").token_span(),
        };

        let new_expr = WithTokenSpan {
            item: Expression::New(Box::new(alloc)),
            span: code.token_span(),
        };

        assert_eq!(code.with_stream(parse_expression), new_expr);
    }

    #[test]
    fn parses_new_allocator_subtype_constraint_range_attribute() {
        let code = Code::new("new integer_vector(foo'range)");

        let alloc = WithTokenSpan {
            item: Allocator::Subtype(code.s1("integer_vector(foo'range)").subtype_indication()),
            span: code.s1("integer_vector(foo'range)").token_span(),
        };

        let new_expr = WithTokenSpan {
            item: Expression::New(Box::new(alloc)),
            span: code.token_span(),
        };

        assert_eq!(code.with_stream(parse_expression), new_expr);
    }

    #[test]
    fn parses_physical_unit_expression() {
        let code = Code::new("1 ns");
        let expr = WithTokenSpan {
            item: Expression::Literal(Literal::Physical(PhysicalLiteral {
                value: AbstractLiteral::Integer(1),
                unit: code.s1("ns").ident().into_ref(),
            })),
            span: code.token_span(),
        };

        assert_eq!(code.with_stream(parse_expression), expr);
    }

    #[test]
    fn parses_physical_unit_expression_real() {
        let code = Code::new("1.0 ns");
        let expr = WithTokenSpan {
            item: Expression::Literal(Literal::Physical(PhysicalLiteral {
                value: AbstractLiteral::Real(1.0),
                unit: code.s1("ns").ident().into_ref(),
            })),
            span: code.token_span(),
        };

        assert_eq!(code.with_stream(parse_expression), expr);
    }

    #[test]
    fn parses_physical_unit_expression_binary() {
        let code = Code::new("2 * 1 ns");
        let time_expr = WithTokenSpan {
            item: Expression::Literal(Literal::Physical(PhysicalLiteral {
                value: AbstractLiteral::Integer(1),
                unit: code.s1("ns").ident().into_ref(),
            })),
            span: code.s1("1 ns").token_span(),
        };
        let two_expr = WithTokenSpan {
            item: Expression::Literal(int(2)),
            span: code.s1("2").token_span(),
        };
        let expr = WithTokenSpan {
            item: Expression::Binary(
                WithToken::new(WithRef::new(Operator::Times), code.s1("*").token()),
                Box::new(two_expr),
                Box::new(time_expr),
            ),
            span: code.token_span(),
        };
        assert_eq!(code.with_stream(parse_expression), expr);
    }

    #[test]
    fn parses_physical_unit_expression_unary() {
        let code = Code::new("- 1 ns");
        let time_expr = WithTokenSpan {
            item: Expression::Literal(Literal::Physical(PhysicalLiteral {
                value: AbstractLiteral::Integer(1),
                unit: code.s1("ns").ident().into_ref(),
            })),
            span: code.s1("1 ns").token_span(),
        };
        let expr = WithTokenSpan {
            item: Expression::Unary(
                WithToken::new(WithRef::new(Operator::Minus), code.s1("-").token()),
                Box::new(time_expr),
            ),
            span: code.token_span(),
        };

        assert_eq!(code.with_stream(parse_expression), expr);
    }

    #[test]
    fn parses_qualified_expression() {
        let code = Code::new("foo'(1+2)");
        let type_mark = code.s1("foo").type_mark();
        let expr = code.s1("(1+2)").expr();

        let qexpr = WithTokenSpan {
            item: Expression::Qualified(Box::new(QualifiedExpression { type_mark, expr })),
            span: code.token_span(),
        };

        assert_eq!(code.with_stream(parse_expression), qexpr);
    }

    #[test]
    fn qualified_expression_precedence() {
        let code = Code::new("mark0'(0) < mark1'(1)");
        let expr = WithTokenSpan {
            item: Expression::Binary(
                WithToken::new(WithRef::new(Operator::LT), code.s1("<").token()),
                Box::new(code.s1("mark0'(0)").expr()),
                Box::new(code.s1("mark1'(1)").expr()),
            ),
            span: code.token_span(),
        };

        assert_eq!(code.expr(), expr);
    }

    #[test]
    fn parses_qualified_aggregate() {
        let code = Code::new("foo'(others => '1')");
        let type_mark = code.s1("foo").type_mark();
        let expr = code.s1("(others => '1')").expr();

        let qexpr = WithTokenSpan {
            item: Expression::Qualified(Box::new(QualifiedExpression { type_mark, expr })),
            span: code.token_span(),
        };

        assert_eq!(code.with_stream(parse_expression), qexpr);
    }

    #[test]
    fn parses_positional_aggregate() {
        let code = Code::new("(1, 2)");
        let one_expr = WithTokenSpan {
            item: Expression::Literal(int(1)),
            span: code.s1("1").token_span(),
        };
        let two_expr = WithTokenSpan {
            item: Expression::Literal(int(2)),
            span: code.s1("2").token_span(),
        };

        let assoc_list = vec![
            ElementAssociation::Positional(one_expr),
            ElementAssociation::Positional(two_expr),
        ];
        let expr = WithTokenSpan {
            item: Expression::Aggregate(assoc_list),
            span: code.token_span(),
        };

        assert_eq!(code.with_stream(parse_expression), expr);
    }

    #[test]
    fn parses_named_aggregate() {
        let code = Code::new("(1 => 2)");
        let one_expr = WithTokenSpan {
            item: Expression::Literal(int(1)),
            span: code.s1("1").token_span(),
        };
        let two_expr = WithTokenSpan {
            item: Expression::Literal(int(2)),
            span: code.s1("2").token_span(),
        };

        let assoc_list = vec![ElementAssociation::Named(
            vec![one_expr.map_into(Choice::Expression)],
            two_expr,
        )];
        let expr = WithTokenSpan {
            item: Expression::Aggregate(assoc_list),
            span: code.token_span(),
        };

        assert_eq!(code.with_stream(parse_expression), expr);
    }

    #[test]
    fn parses_named_aggregate_many_choices() {
        let code = Code::new("(1 | 2 => 3)");
        let one_expr = WithTokenSpan {
            item: Expression::Literal(int(1)),
            span: code.s1("1").token_span(),
        };

        let two_expr = WithTokenSpan {
            item: Expression::Literal(int(2)),
            span: code.s1("2").token_span(),
        };

        let three_expr = WithTokenSpan {
            item: Expression::Literal(int(3)),
            span: code.s1("3").token_span(),
        };

        let assoc_list = vec![ElementAssociation::Named(
            vec![
                one_expr.map_into(Choice::Expression),
                two_expr.map_into(Choice::Expression),
            ],
            three_expr,
        )];
        let expr = WithTokenSpan {
            item: Expression::Aggregate(assoc_list),
            span: code.token_span(),
        };

        assert_eq!(code.with_stream(parse_expression), expr);
    }

    #[test]
    fn parses_others_aggregate() {
        let code = Code::new("(others => 1)");
        let one_expr = WithTokenSpan {
            item: Expression::Literal(int(1)),
            span: code.s1("1").token_span(),
        };

        let assoc_list = vec![ElementAssociation::Named(
            vec![WithTokenSpan::new(
                Choice::Others,
                code.s1("others").token_span(),
            )],
            one_expr,
        )];
        let expr = WithTokenSpan {
            item: Expression::Aggregate(assoc_list),
            span: code.token_span(),
        };

        assert_eq!(code.with_stream(parse_expression), expr);
    }

    #[test]
    fn parses_aggregate_range() {
        for direction in [Direction::Descending, Direction::Ascending].iter() {
            let (pos, code) = {
                if *direction == Direction::Descending {
                    let code = Code::new("(1 downto 0 => 2)");
                    (code.s1("1 downto 0").token_span(), code)
                } else {
                    let code = Code::new("(1 to 0 => 2)");
                    (code.s1("1 to 0").token_span(), code)
                }
            };

            let one_expr = WithTokenSpan {
                item: Expression::Literal(int(1)),
                span: code.s1("1").token_span(),
            };

            let zero_expr = WithTokenSpan {
                item: Expression::Literal(int(0)),
                span: code.s1("0").token_span(),
            };

            let two_expr = WithTokenSpan {
                item: Expression::Literal(int(2)),
                span: code.s1("2").token_span(),
            };

            let range = DiscreteRange::Range(ast::Range::Range(RangeConstraint {
                direction: *direction,
                left_expr: Box::new(one_expr),
                right_expr: Box::new(zero_expr),
            }));

            let assoc_list = vec![ElementAssociation::Named(
                vec![WithTokenSpan::new(Choice::DiscreteRange(range), pos)],
                two_expr,
            )];
            let expr = WithTokenSpan {
                item: Expression::Aggregate(assoc_list),
                span: code.token_span(),
            };

            assert_eq!(code.with_stream(parse_expression), expr);
        }
    }

    #[test]
    fn parses_multiple_others_aggregate() {
        let code = Code::new("(others => 1, others => 2)");
        let one_expr = WithTokenSpan {
            item: Expression::Literal(int(1)),
            span: code.s1("1").token_span(),
        };

        let two_expr = WithTokenSpan {
            item: Expression::Literal(int(2)),
            span: code.s1("2").token_span(),
        };

        let assoc_list = vec![
            ElementAssociation::Named(
                vec![WithTokenSpan::new(
                    Choice::Others,
                    code.s("others", 1).token_span(),
                )],
                one_expr,
            ),
            ElementAssociation::Named(
                vec![WithTokenSpan::new(
                    Choice::Others,
                    code.s("others", 2).token_span(),
                )],
                two_expr,
            ),
        ];
        let expr = WithTokenSpan {
            item: Expression::Aggregate(assoc_list),
            span: code.token_span(),
        };

        assert_eq!(code.with_stream(parse_expression), expr);
    }

    #[test]
    fn parses_mixed_aggregate() {
        let code = Code::new("(1 => 2, 3)");
        let one_expr = WithTokenSpan {
            item: Expression::Literal(int(1)),
            span: code.s1("1").token_span(),
        };
        let two_expr = WithTokenSpan {
            item: Expression::Literal(int(2)),
            span: code.s1("2").token_span(),
        };
        let three_expr = WithTokenSpan {
            item: Expression::Literal(int(3)),
            span: code.s1("3").token_span(),
        };

        let assoc_list = vec![
            ElementAssociation::Named(vec![one_expr.map_into(Choice::Expression)], two_expr),
            ElementAssociation::Positional(three_expr),
        ];
        let expr = WithTokenSpan {
            item: Expression::Aggregate(assoc_list),
            span: code.token_span(),
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
        assert_eq!(code.with_stream(parse_expression).span, code.token_span());
    }

    #[test]
    fn parses_nested_expression_par_second() {
        let code = Code::new("1 + (2 + 3)");

        let one = WithTokenSpan {
            item: Expression::Literal(int(1)),
            span: code.s1("1").token_span(),
        };

        let two = WithTokenSpan {
            item: Expression::Literal(int(2)),
            span: code.s1("2").token_span(),
        };

        let three = WithTokenSpan {
            item: Expression::Literal(int(3)),
            span: code.s1("3").token_span(),
        };

        let expr_add0 = WithTokenSpan {
            item: Expression::Binary(
                WithToken::new(WithRef::new(Operator::Plus), code.s("+", 2).token()),
                Box::new(two),
                Box::new(three),
            ),
            span: code.s1("(2 + 3)").token_span(),
        };

        let expr_add1 = WithTokenSpan {
            item: Expression::Binary(
                WithToken::new(WithRef::new(Operator::Plus), code.s("+", 1).token()),
                Box::new(one),
                Box::new(expr_add0),
            ),
            span: code.token_span(),
        };

        assert_eq!(code.with_stream(parse_expression), expr_add1);
    }

    #[test]
    fn parses_nested_expression_par_first() {
        let code = Code::new("(1 + 2) + 3");

        let one = WithTokenSpan {
            item: Expression::Literal(int(1)),
            span: code.s1("1").token_span(),
        };

        let two = WithTokenSpan {
            item: Expression::Literal(int(2)),
            span: code.s1("2").token_span(),
        };

        let three = WithTokenSpan {
            item: Expression::Literal(int(3)),
            span: code.s1("3").token_span(),
        };

        let expr_add0 = WithTokenSpan {
            item: Expression::Binary(
                WithToken::new(WithRef::new(Operator::Plus), code.s("+", 1).token()),
                Box::new(one),
                Box::new(two),
            ),
            span: code.s1("(1 + 2)").token_span(),
        };

        let expr_add1 = WithTokenSpan {
            item: Expression::Binary(
                WithToken::new(WithRef::new(Operator::Plus), code.s("+", 2).token()),
                Box::new(expr_add0),
                Box::new(three),
            ),
            span: code.token_span(),
        };

        assert_eq!(code.with_stream(parse_expression), expr_add1);
    }

    /// Format expression as a string to simplify testing of precedence.
    fn fmt(ctx: &dyn TokenAccess, expr: &WithTokenSpan<Expression>) -> String {
        match expr.item {
            Expression::Binary(ref op, ref lhs, ref rhs) => {
                format!("({} {:?} {})", fmt(ctx, lhs), op.item.item, fmt(ctx, rhs))
            }
            Expression::Unary(ref op, ref rhs) => format!("({:?} {})", op.item.item, fmt(ctx, rhs)),
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
                    println!("{}", expr.to_pos(ctx).code_context());
                    panic!("Cannot format {lit:?}");
                }
            },
            _ => {
                println!("{}", expr.to_pos(ctx).code_context());
                panic!("Cannot format {expr:?}");
            }
        }
    }

    fn assert_expression_is(code: &str, expr_str: &str) {
        let code = Code::new(code);
        let ctx = code.tokenize();
        assert_eq!(fmt(&ctx, &code.with_stream(parse_expression)), expr_str);
    }

    #[test]
    fn parses_function_errors() {
        let code = Code::new("fun(,)");
        assert_eq!(
            code.with_partial_stream(parse_expression),
            Err(Diagnostic::syntax_error(
                &code.s1(",").pos(),
                "Expected {expression}"
            ))
        );

        let code = Code::new("fun(arg0,)");
        assert_eq!(
            code.with_partial_stream(parse_expression),
            Err(Diagnostic::syntax_error(
                &code.s1(")").pos(),
                "Expected {expression}"
            ))
        );
        let code = Code::new("fun(arg0,,)");
        assert_eq!(
            code.with_partial_stream(parse_expression),
            Err(Diagnostic::syntax_error(
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
