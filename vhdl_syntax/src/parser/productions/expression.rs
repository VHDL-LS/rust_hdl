// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind::*;
use crate::tokens::Keyword as Kw;
use crate::tokens::TokenKind;
use crate::tokens::TokenKind::*;
use nonzero_ext::nonzero;
use std::num::NonZeroU8;

fn binary_precedence(token: TokenKind) -> Option<NonZeroU8> {
    Some(match token {
        Keyword(Kw::And | Kw::Or | Kw::Nand | Kw::Nor | Kw::Xor | Kw::Xnor) => nonzero!(2u8),
        EQ | NE | LT | LTE | GT | GTE | QueEQ | QueNE | QueLT | QueGT | QueGTE => nonzero!(3u8),
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
    pub fn primary(&mut self) {
        match_next_token!(self,
            Identifier, LtLt => {
              self.start_node(NameExpression);
              self.name();
              self.end_node();
            },
            BitStringLiteral, CharacterLiteral, StringLiteral, Keyword(Kw::Null) => self.skip_into_node(LiteralExpression),
            AbstractLiteral => {
                let checkpoint = self.checkpoint();
                self.skip();
                if self.next_is(Identifier) {
                    self.start_node_at(checkpoint, PhysicalLiteral);
                    self.name();
                } else {
                    self.start_node_at(checkpoint, LiteralExpression);
                }
                self.end_node();
            },
            LeftPar => {
                self.start_node(ParenthesizedExpressionOrAggregate);
                self.aggregate_inner();
                self.end_node();
            },
            Keyword(Kw::New) => {
              self.allocator();
            }
        );
    }

    pub fn allocator(&mut self) {
        self.start_node(ExpressionAllocator);
        self.expect_kw(Kw::New);
        self.subtype_indication();
        self.end_node();
    }

    fn unary_expression(&mut self) {
        if let Some(precedence) = self.peek_token().and_then(unary_precedence) {
            self.start_node(UnaryExpression);
            self.skip();
            self.expression_inner(precedence.into());
            self.end_node();
        } else {
            self.primary()
        }
    }

    fn expression_inner(&mut self, min_precedence: u8) {
        let checkpoint = self.checkpoint();
        self.unary_expression();

        while let Some(precedence) = self.peek_token().and_then(binary_precedence) {
            let precedence: u8 = precedence.into();
            if precedence > min_precedence {
                self.start_node_at(checkpoint, BinaryExpression);
                self.skip();
                self.expression_inner(precedence);
                self.end_node();
            } else {
                break;
            }
        }
    }

    pub fn expression(&mut self) {
        self.expression_inner(0);
    }

    pub fn condition(&mut self) {
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
}
