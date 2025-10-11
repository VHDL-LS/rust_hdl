// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind::*;
use crate::tokens::Keyword as Kw;
use crate::tokens::TokenKind::*;
use crate::tokens::{TokenKind, TokenStream};
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

impl<T: TokenStream> Parser<T> {
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
            }
        );
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
    use crate::parser::test_utils::check;
    use crate::parser::Parser;

    fn check_expr(input: &str, output: &str) {
        check(Parser::expression, input, output)
    }

    #[test]
    fn character_literal() {
        check_expr(
            "'a'",
            "\
LiteralExpression
  CharacterLiteral ''a''
        ",
        );
    }

    #[test]
    fn abstract_integer_literal() {
        check_expr(
            "71",
            "\
LiteralExpression
  AbstractLiteral '71'
        ",
        );
    }

    #[test]
    fn abstract_real_literal() {
        check_expr(
            "7.1",
            "\
LiteralExpression
  AbstractLiteral '7.1'
        ",
        );
    }

    #[test]
    fn string_literal() {
        check_expr(
            "\"string\"",
            "\
LiteralExpression
  StringLiteral '\"string\"'
        ",
        );
    }

    #[test]
    fn null_literal() {
        check_expr(
            "null",
            "\
LiteralExpression
  Keyword(Null)
        ",
        );
    }

    #[test]
    #[ignore]
    fn operator_symbol() {
        check_expr("\"+\"(1, 2)", todo!());
    }

    #[test]
    fn external_name() {
        check_expr(
            "<< signal dut.foo : boolean >>",
            "\
NameExpression
  Name
    ExternalSignalName
      LtLt
      Keyword(Signal)
      RelativePathname
        PartialPathname
          Identifier 'dut'
          Dot
          Identifier 'foo'
      Colon
      Identifier 'boolean'
      GtGt
        ",
        );
    }

    #[test]
    fn add_expression() {
        check_expr(
            "1 + 2",
            "\
BinaryExpression
  LiteralExpression
    AbstractLiteral '1'
  Plus
  LiteralExpression
    AbstractLiteral '2'
            ",
        )
    }

    #[test]
    fn sub_expression() {
        check_expr(
            "1 - 2",
            "\
BinaryExpression
  LiteralExpression
    AbstractLiteral '1'
  Minus
  LiteralExpression
    AbstractLiteral '2'
            ",
        )
    }

    #[test]
    fn abs_expression() {
        check_expr(
            "abs 9",
            "\
UnaryExpression
  Keyword(Abs)
  LiteralExpression
    AbstractLiteral '9'
            ",
        )
    }

    #[test]
    fn condition_operator() {
        check_expr(
            "?? 9",
            "\
UnaryExpression
  QueQue
  LiteralExpression
    AbstractLiteral '9'
            ",
        )
    }

    #[test]
    fn not_expression() {
        check_expr(
            "not false",
            "\
UnaryExpression
  Keyword(Not)
  NameExpression
    Name
      Identifier 'false'
            ",
        )
    }

    #[ignore]
    #[test]
    fn allocator() {
        check_expr("new integer_vector'(0, 1)", todo!())
    }

    #[ignore]
    #[test]
    fn allocator_subtype() {
        check_expr("new integer_vector", todo!())
    }

    #[ignore]
    #[test]
    fn allocator_subtype_constrained() {
        check_expr("new integer_vector(0 to 1)", todo!())
    }

    #[ignore]
    #[test]
    fn allocator_subtype_constrained_range_attribute() {
        check_expr("new integer_vector(foo'range)", todo!())
    }

    #[test]
    fn physical_unit() {
        check_expr(
            "1 ns",
            "\
PhysicalLiteral
  AbstractLiteral '1'
  Name
    Identifier 'ns'
        ",
        )
    }

    #[test]
    fn physical_unit_real() {
        check_expr(
            "1.0 ns",
            "\
PhysicalLiteral
  AbstractLiteral '1.0'
  Name
    Identifier 'ns'
        ",
        )
    }

    #[test]
    fn physical_unit_binary_expression() {
        check_expr(
            "2 * 1 ns",
            "\
BinaryExpression
  LiteralExpression
    AbstractLiteral '2'
  Times
  PhysicalLiteral
    AbstractLiteral '1'
    Name
      Identifier 'ns'
        ",
        )
    }

    #[test]
    fn physical_unit_unary_expression() {
        check_expr(
            "- 1 ns",
            "\
UnaryExpression
  Minus
  PhysicalLiteral
    AbstractLiteral '1'
    Name
      Identifier 'ns'
        ",
        )
    }

    #[ignore]
    #[test]
    fn qualified_expression() {
        check_expr("foo'(1+2)", todo!())
    }

    #[ignore]
    #[test]
    fn qualified_expression_precedence() {
        check_expr("mark0'(0) < mark1'(1)", todo!())
    }

    #[ignore]
    #[test]
    fn qualified_aggregate() {
        check_expr("foo'(others => '1')", todo!())
    }

    #[test]
    fn positional_aggregate() {
        check_expr(
            "(1, 2)",
            "\
ParenthesizedExpressionOrAggregate
  LeftPar
  LiteralExpression
    AbstractLiteral '1'
  Comma
  LiteralExpression
    AbstractLiteral '2'
  RightPar",
        )
    }

    #[test]
    fn named_aggregate() {
        check_expr(
            "(1 => 2)",
            "\
ParenthesizedExpressionOrAggregate
  LeftPar
  ElementAssociation
    Choices
      LiteralExpression
        AbstractLiteral '1'
    RightArrow
    LiteralExpression
      AbstractLiteral '2'
  RightPar",
        )
    }

    #[test]
    fn named_aggregate_many_choices() {
        check_expr(
            "(1 | 2 => 3)",
            "\
ParenthesizedExpressionOrAggregate
  LeftPar
  ElementAssociation
    Choices
      LiteralExpression
        AbstractLiteral '1'
      Bar
      LiteralExpression
        AbstractLiteral '2'
    RightArrow
    LiteralExpression
      AbstractLiteral '3'
  RightPar",
        )
    }

    #[test]
    fn aggregate_others() {
        check_expr(
            "(others => 1)",
            "\
ParenthesizedExpressionOrAggregate
  LeftPar
  ElementAssociation
    Choices
      Keyword(Others)
    RightArrow
    LiteralExpression
      AbstractLiteral '1'
  RightPar",
        )
    }

    #[ignore]
    #[test]
    fn aggregate_range() {
        todo!()
    }

    #[test]
    fn multiple_others_aggregate() {
        check_expr(
            "(others => 1, others => 2)",
            "\
ParenthesizedExpressionOrAggregate
  LeftPar
  ElementAssociation
    Choices
      Keyword(Others)
    RightArrow
    LiteralExpression
      AbstractLiteral '1'
  Comma
  ElementAssociation
    Choices
      Keyword(Others)
    RightArrow
    LiteralExpression
      AbstractLiteral '2'
  RightPar",
        )
    }

    #[test]
    fn mixed_aggregate() {
        check_expr(
            "(1 => 2, 3)",
            "\
ParenthesizedExpressionOrAggregate
  LeftPar
  ElementAssociation
    Choices
      LiteralExpression
        AbstractLiteral '1'
    RightArrow
    LiteralExpression
      AbstractLiteral '2'
  Comma
  LiteralExpression
    AbstractLiteral '3'
  RightPar",
        )
    }

    #[test]
    fn nested_expression_par_second() {
        check_expr(
            "1 + (2 + 3)",
            "\
BinaryExpression
  LiteralExpression
    AbstractLiteral '1'
  Plus
  ParenthesizedExpressionOrAggregate
    LeftPar
    BinaryExpression
      LiteralExpression
        AbstractLiteral '2'
      Plus
      LiteralExpression
        AbstractLiteral '3'
    RightPar
        ",
        )
    }

    #[test]
    fn nested_expression_par_first() {
        check_expr(
            "(1 + 2) + 3",
            "\
BinaryExpression
  ParenthesizedExpressionOrAggregate
    LeftPar
    BinaryExpression
      LiteralExpression
        AbstractLiteral '1'
      Plus
      LiteralExpression
        AbstractLiteral '2'
    RightPar
  Plus
  LiteralExpression
    AbstractLiteral '3'
        ",
        )
    }

    #[test]
    fn expression_precedence() {
        check_expr(
            "1 + 1 ns",
            "\
BinaryExpression
  LiteralExpression
    AbstractLiteral '1'
  Plus
  PhysicalLiteral
    AbstractLiteral '1'
    Name
      Identifier 'ns'
                ",
        );
        check_expr(
            "1 * 1 ns * 2",
            "\
BinaryExpression
  BinaryExpression
    LiteralExpression
      AbstractLiteral '1'
    Times
    PhysicalLiteral
      AbstractLiteral '1'
      Name
        Identifier 'ns'
  Times
  LiteralExpression
    AbstractLiteral '2'
        ",
        );
        check_expr(
            "1+2+3",
            "\
BinaryExpression
  BinaryExpression
    LiteralExpression
      AbstractLiteral '1'
    Plus
    LiteralExpression
      AbstractLiteral '2'
  Plus
  LiteralExpression
    AbstractLiteral '3'
        ",
        );
        check_expr(
            "1-2-3",
            "\
BinaryExpression
  BinaryExpression
    LiteralExpression
      AbstractLiteral '1'
    Minus
    LiteralExpression
      AbstractLiteral '2'
  Minus
  LiteralExpression
    AbstractLiteral '3'
        ",
        );
        check_expr(
            "1+2*3",
            "\
BinaryExpression
  LiteralExpression
    AbstractLiteral '1'
  Plus
  BinaryExpression
    LiteralExpression
      AbstractLiteral '2'
    Times
    LiteralExpression
      AbstractLiteral '3'
        ",
        );
        check_expr(
            "(1+2)*3",
            "\
BinaryExpression
  ParenthesizedExpressionOrAggregate
    LeftPar
    BinaryExpression
      LiteralExpression
        AbstractLiteral '1'
      Plus
      LiteralExpression
        AbstractLiteral '2'
    RightPar
  Times
  LiteralExpression
    AbstractLiteral '3'
        ",
        );
        check_expr(
            "-1 * 2",
            "\
UnaryExpression
  Minus
  BinaryExpression
    LiteralExpression
      AbstractLiteral '1'
    Times
    LiteralExpression
      AbstractLiteral '2'
        ",
        );
        check_expr(
            "not 1 + 2",
            "\
BinaryExpression
  UnaryExpression
    Keyword(Not)
    LiteralExpression
      AbstractLiteral '1'
  Plus
  LiteralExpression
    AbstractLiteral '2'
        ",
        );
        check_expr(
            "abs not 1 + 2",
            "\
BinaryExpression
  UnaryExpression
    Keyword(Abs)
    UnaryExpression
      Keyword(Not)
      LiteralExpression
        AbstractLiteral '1'
  Plus
  LiteralExpression
    AbstractLiteral '2'
        ",
        );
        check_expr(
            "not - 1",
            "\
UnaryExpression
  Keyword(Not)
  UnaryExpression
    Minus
    LiteralExpression
      AbstractLiteral '1'
        ",
        );
        check_expr(
            "not + 1",
            "\
UnaryExpression
  Keyword(Not)
  UnaryExpression
    Plus
    LiteralExpression
      AbstractLiteral '1'
        ",
        );
        check_expr(
            "not + ?? 1 ** ?? 2",
            "\
UnaryExpression
  Keyword(Not)
  UnaryExpression
    Plus
    UnaryExpression
      QueQue
      BinaryExpression
        LiteralExpression
          AbstractLiteral '1'
        Pow
        UnaryExpression
          QueQue
          LiteralExpression
            AbstractLiteral '2'
        ",
        );
        check_expr(
            "abs 1 sll 2 + 3 and -1",
            "\
BinaryExpression
  BinaryExpression
    UnaryExpression
      Keyword(Abs)
      LiteralExpression
        AbstractLiteral '1'
    Keyword(Sll)
    BinaryExpression
      LiteralExpression
        AbstractLiteral '2'
      Plus
      LiteralExpression
        AbstractLiteral '3'
  Keyword(And)
  UnaryExpression
    Minus
    LiteralExpression
      AbstractLiteral '1'
        ",
        );
        check_expr(
            "1 + 2 and 3 + 4",
            "\
BinaryExpression
  BinaryExpression
    LiteralExpression
      AbstractLiteral '1'
    Plus
    LiteralExpression
      AbstractLiteral '2'
  Keyword(And)
  BinaryExpression
    LiteralExpression
      AbstractLiteral '3'
    Plus
    LiteralExpression
      AbstractLiteral '4'
        ",
        );
        check_expr(
            "and 1 + 2",
            "\
BinaryExpression
  UnaryExpression
    Keyword(And)
    LiteralExpression
      AbstractLiteral '1'
  Plus
  LiteralExpression
    AbstractLiteral '2'
        ",
        );
    }
}
