// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::marker::{CompletedMarker, Precede};
use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind::*;
use crate::tokens::Keyword as Kw;
use crate::tokens::TokenKind;
use crate::tokens::TokenKind::*;
use nonzero_ext::nonzero;
use std::num::NonZeroU8;

fn binary_precedence(token: TokenKind) -> Option<NonZeroU8> {
    Some(match token {
        // `to`/`downto` were a separate `range` production in the LRM; folding
        // them into the operator table lets `range_constraint` accept any
        // expression and reuse precedence climbing.
        Keyword(Kw::To | Kw::Downto) => nonzero!(1u8),
        Keyword(Kw::And | Kw::Or | Kw::Nand | Kw::Nor | Kw::Xor | Kw::Xnor) => nonzero!(2u8),
        EQ | NE | LT | LTE | GT | GTE | QueEQ | QueNE | QueLT | QueLTE | QueGT | QueGTE => {
            nonzero!(3u8)
        }
        Keyword(Kw::Sll | Kw::Srl | Kw::Sla | Kw::Sra | Kw::Rol | Kw::Ror) => nonzero!(4u8),
        Plus | Minus | Concat => nonzero!(5u8),
        Times | Div | Keyword(Kw::Mod | Kw::Rem) => nonzero!(7u8),
        Pow => nonzero!(8u8),
        _ => return None,
    })
}

fn unary_precedence(token: TokenKind) -> Option<NonZeroU8> {
    Some(match token {
        QueQue => nonzero!(1u8),
        Plus | Minus => nonzero!(6u8),
        Keyword(Kw::Abs | Kw::Not | Kw::And | Kw::Or | Kw::Nand | Kw::Nor | Kw::Xor | Kw::Xnor) => {
            nonzero!(8u8)
        }
        _ => return None,
    })
}

impl Parser {
    pub(crate) fn primary(&mut self) -> Option<CompletedMarker> {
        match_next_token!(self,
            Identifier, LtLt => {
              let name = self.name();
              Some(self.continue_primary_after_name(name))
            },
            BitStringLiteral, CharacterLiteral, StringLiteral, Keyword(Kw::Null) => Some(self.skip_into_node(LiteralExpression)),
            AbstractLiteral => {
                let literal_marker = self.start_unknown();
                self.skip();
                if self.next_is(Identifier) {
                    let literal = literal_marker.resolve(self, PhysicalLiteral);
                    self.name();
                    let literal = literal.complete(self);
                    Some(literal.precede(self, PhysicalLiteralExpression).complete(self))
                } else {
                    Some(literal_marker.complete(self, LiteralExpression))
                }
            },
            LeftPar => {
                Some(self.parenthesized_expression_or_aggregate())
            },
            Keyword(Kw::New) => {
              Some(self.allocator())
            }
        )
    }

    pub(crate) fn parenthesized_expression_or_aggregate(&mut self) -> CompletedMarker {
        self.node(ParenthesizedExpressionOrAggregate, Parser::aggregate_inner)
    }

    /// Finalize a primary.
    /// If a `Tick` follows, the name is the type mark of a
    /// `QualifiedExpression` and the `'(…)` is consumed here; otherwise the
    /// name is wrapped in `NameExpression`. Callers that need to continue
    /// with binary operators should follow up with `expression_from_primary`
    pub(crate) fn continue_primary_after_name(&mut self, name: CompletedMarker) -> CompletedMarker {
        if self.next_is(Tick) {
            let marker = name.precede(self, QualifiedExpression);
            self.skip();
            self.parenthesized_expression_or_aggregate();
            marker.complete(self)
        } else {
            name.precede(self, NameExpression).complete(self)
        }
    }

    pub(crate) fn allocator(&mut self) -> CompletedMarker {
        self.node(Allocator, |p| {
            p.expect_kw(Kw::New);
            p.expression();
        })
    }

    fn unary_expression(&mut self) -> Option<CompletedMarker> {
        if let Some(precedence) = unary_precedence(self.peek_token()) {
            Some(self.node(UnaryExpression, |p| {
                p.skip();
                p.expression_inner(precedence.into());
            }))
        } else {
            self.primary()
        }
    }

    fn expression_inner(&mut self, min_precedence: u8) -> Option<CompletedMarker> {
        let mut expression = self.unary_expression();

        while let Some(precedence) = binary_precedence(self.peek_token()) {
            let precedence: u8 = precedence.into();
            if precedence > min_precedence {
                let marker = expression.precede(self, BinaryExpression);
                self.skip();
                self.expression_inner(precedence);
                expression = Some(marker.complete(self));
            } else {
                break;
            }
        }

        expression
    }

    pub(crate) fn expression(&mut self) -> Option<CompletedMarker> {
        self.expression_inner(0)
    }

    /// Continue an expression parse from an already-emitted primary
    pub(crate) fn expression_from_primary(&mut self, primary: CompletedMarker) -> CompletedMarker {
        let mut expression = primary;
        while let Some(precedence) = binary_precedence(self.peek_token()) {
            let precedence: u8 = precedence.into();
            let marker = expression.precede(self, BinaryExpression);
            self.skip();
            self.expression_inner(precedence);
            expression = marker.complete(self);
        }
        expression
    }

    pub(crate) fn condition(&mut self) -> Option<CompletedMarker> {
        self.expression()
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils::to_test_text;
    use crate::parser::Parser;

    fn expr_to_test_text(input: &str) -> String {
        to_test_text(Parser::expression, input)
    }

    #[test]
    fn character_literal() {
        insta::assert_snapshot!(expr_to_test_text("'a'"));
    }

    #[test]
    fn abstract_integer_literal() {
        insta::assert_snapshot!(expr_to_test_text("71",));
    }

    #[test]
    fn abstract_real_literal() {
        insta::assert_snapshot!(expr_to_test_text("7.1",));
    }

    #[test]
    fn string_literal() {
        insta::assert_snapshot!(expr_to_test_text("\"string\"",));
    }

    #[test]
    fn null_literal() {
        insta::assert_snapshot!(expr_to_test_text("null",));
    }

    #[test]
    #[ignore]
    fn operator_symbol() {
        insta::assert_snapshot!(expr_to_test_text("\"+\"(1, 2)"));
    }

    #[test]
    fn external_name() {
        insta::assert_snapshot!(expr_to_test_text("<< signal dut.foo : boolean >>"));
    }

    #[test]
    fn add_expression() {
        insta::assert_snapshot!(expr_to_test_text("1 + 2"))
    }

    #[test]
    fn sub_expression() {
        insta::assert_snapshot!(expr_to_test_text("1 - 2"))
    }

    #[test]
    fn abs_expression() {
        insta::assert_snapshot!(expr_to_test_text("abs 9"))
    }

    #[test]
    fn condition_operator() {
        insta::assert_snapshot!(expr_to_test_text("?? 9"))
    }

    #[test]
    fn not_expression() {
        insta::assert_snapshot!(expr_to_test_text("not false"))
    }

    #[test]
    fn allocator() {
        insta::assert_snapshot!(expr_to_test_text("new integer_vector'(0, 1)"))
    }

    #[test]
    fn allocator_subtype() {
        insta::assert_snapshot!(expr_to_test_text("new integer_vector"))
    }

    #[test]
    fn allocator_subtype_constrained() {
        insta::assert_snapshot!(expr_to_test_text("new integer_vector(0 to 1)"))
    }

    #[test]
    fn allocator_subtype_constrained_range_attribute() {
        insta::assert_snapshot!(expr_to_test_text("new integer_vector(foo'range)"))
    }

    #[test]
    fn physical_unit() {
        insta::assert_snapshot!(expr_to_test_text("1 ns"))
    }

    #[test]
    fn physical_unit_real() {
        insta::assert_snapshot!(expr_to_test_text("1.0 ns"))
    }

    #[test]
    fn physical_unit_binary_expression() {
        insta::assert_snapshot!(expr_to_test_text("2 * 1 ns"))
    }

    #[test]
    fn physical_unit_unary_expression() {
        insta::assert_snapshot!(expr_to_test_text("- 1 ns"))
    }

    #[test]
    fn qualified_expression() {
        insta::assert_snapshot!(expr_to_test_text("foo'(1+2)"))
    }

    #[test]
    fn qualified_expression_precedence() {
        insta::assert_snapshot!(expr_to_test_text("mark0'(0) < mark1'(1)"))
    }

    #[test]
    fn qualified_aggregate() {
        insta::assert_snapshot!(expr_to_test_text("foo'(others => '1')"))
    }

    #[test]
    fn positional_aggregate() {
        insta::assert_snapshot!(expr_to_test_text("(1, 2)"))
    }

    #[test]
    fn named_aggregate() {
        insta::assert_snapshot!(expr_to_test_text("(1 => 2)"))
    }

    #[test]
    fn named_aggregate_many_choices() {
        insta::assert_snapshot!(expr_to_test_text("(1 | 2 => 3)"))
    }

    #[test]
    fn aggregate_others() {
        insta::assert_snapshot!(expr_to_test_text("(others => 1)"))
    }

    #[test]
    fn aggregate_range() {
        insta::assert_snapshot!(expr_to_test_text("(1 downto 0 => 2)"));
        insta::assert_snapshot!(expr_to_test_text("(1 to 0 => 2)"));
    }

    #[test]
    fn multiple_others_aggregate() {
        insta::assert_snapshot!(expr_to_test_text("(others => 1, others => 2)"))
    }

    #[test]
    fn mixed_aggregate() {
        insta::assert_snapshot!(expr_to_test_text("(1 => 2, 3)"))
    }

    #[test]
    fn nested_expression_par_second() {
        insta::assert_snapshot!(expr_to_test_text("1 + (2 + 3)"))
    }

    #[test]
    fn nested_expression_par_first() {
        insta::assert_snapshot!(expr_to_test_text("(1 + 2) + 3"))
    }

    #[test]
    fn expression_precedence() {
        insta::assert_snapshot!(expr_to_test_text("1 + 1 ns"));
        insta::assert_snapshot!(expr_to_test_text("1 * 1 ns * 2"));
        insta::assert_snapshot!(expr_to_test_text("1+2+3"));
        insta::assert_snapshot!(expr_to_test_text("1-2-3"));
        insta::assert_snapshot!(expr_to_test_text("1+2*3"));
        insta::assert_snapshot!(expr_to_test_text("(1+2)*3"));
        insta::assert_snapshot!(expr_to_test_text("-1 * 2"));
        insta::assert_snapshot!(expr_to_test_text("not 1 + 2"));
        insta::assert_snapshot!(expr_to_test_text("abs not 1 + 2"));
        insta::assert_snapshot!(expr_to_test_text("not - 1"));
        insta::assert_snapshot!(expr_to_test_text("not + 1"));
        insta::assert_snapshot!(expr_to_test_text("not + ?? 1 ** ?? 2"));
        insta::assert_snapshot!(expr_to_test_text("abs 1 sll 2 + 3 and -1"));
        insta::assert_snapshot!(expr_to_test_text("1 + 2 and 3 + 4"));
        insta::assert_snapshot!(expr_to_test_text("and 1 + 2"));
    }

    #[test]
    fn aggregate_choice_with_signature() {
        insta::assert_snapshot!(expr_to_test_text("(foo[bit, bit]'image => 1)"));
        insta::assert_snapshot!(expr_to_test_text(
            "(name(name(name), name(name))[name, name]'i => actual)"
        ));
    }
}
