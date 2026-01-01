// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com
/// Parsing of composite types (LRM §5.3)
use crate::parser::{util::LookaheadError, Parser};
use crate::syntax::node_kind::NodeKind::*;
use crate::tokens::token_kind::Keyword as Kw;
use crate::tokens::TokenKind::*;
use crate::tokens::TokenStream;

impl<T: TokenStream> Parser<T> {
    pub fn array_type_definition(&mut self) {
        let checkpoint = self.checkpoint();
        self.expect_kw(Kw::Array);
        let box_found = self.lookahead_skip_n(1, [BOX]).is_ok();

        if box_found {
            self.start_node_at(checkpoint, UnboundedArrayDefinition);
            self.expect_token(LeftPar);
            self.separated_list(Parser::index_subtype_definition, Comma);
            self.expect_token(RightPar);
        } else {
            self.start_node_at(checkpoint, ConstrainedArrayDefinition);
            self.index_constraint();
        }
        self.expect_kw(Kw::Of);
        self.subtype_indication();
        self.end_node();
    }

    pub fn record_type_definition(&mut self) {
        self.start_node(RecordTypeDefinition);
        self.expect_kw(Kw::Record);

        while !self.next_is(Keyword(Kw::End)) {
            self.element_declaration();
        }

        self.expect_tokens([Keyword(Kw::End), Keyword(Kw::Record)]);
        self.opt_identifier();
        self.end_node();
    }

    pub fn element_declaration(&mut self) {
        self.start_node(ElementDeclaration);
        self.identifier_list();
        self.expect_token(Colon);
        self.subtype_indication();
        self.expect_token(SemiColon);
        self.end_node();
    }

    pub fn index_subtype_definition(&mut self) {
        self.start_node(IndexSubtypeDefinition);
        self.type_mark();
        self.expect_tokens([Keyword(Kw::Range), BOX]);
        self.end_node();
    }

    pub fn index_constraint(&mut self) {
        self.start_node(IndexConstraint);
        self.expect_token(LeftPar);
        self.separated_list(Parser::discrete_range, Comma);
        self.expect_token(RightPar);
        self.end_node();
    }

    pub fn discrete_range(&mut self) {
        // One of the following tokens must follow after a `discrete_range`.
        //    FOLLOW(discrete_range) := "," | ")" | "|" | "=>" | "generate" | "loop" | ";"
        let end_of_range = match self.lookahead([
            Comma,
            RightPar,
            RightArrow,
            Bar,
            Keyword(Kw::Generate),
            Keyword(Kw::Loop),
            SemiColon
        ]) {
            Ok((tok, end_index)) => Some((tok, end_index)),
            // If EOF is reached, the range cannot be parsed correctly
            Err((LookaheadError::Eof, _)) => {
                self.eof_err();
                None
            }
            // Since we use `usize::MAX` as a maximum index, this error is not possible!
            Err((LookaheadError::MaxIndexReached, _)) => unreachable!(),
            // This error is only possible, when a `RightPar` is found before any token in `kinds`.
            // Since `RightPar` is in `kinds` that's not possible!
            Err((LookaheadError::TokenKindNotFound, _)) => unreachable!(),
        };

        if let Some((_, end_index)) = end_of_range {
            self.start_node(RawTokens);
            self.skip_to(end_index);
            self.end_node();
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils::{to_test_text};
    use crate::parser::Parser;

    #[test]
    fn array_type_declaration() {
        insta::assert_snapshot!(to_test_text(
            Parser::type_declaration,
            "type int_arr_t is array (natural range <>) of integer;",
        ));

        insta::assert_snapshot!(to_test_text(
            Parser::type_declaration,
            "type int_arr_2d_t is array (natural range <>, integer range <>) of positive;",
        ));
    }

    #[test]
    fn array_type_declaration_with_discrete_range() {
        insta::assert_snapshot!(to_test_text(
            Parser::type_declaration,
            "type constrained_int_arr is array (0 to 1) of positive;",
        ));

        insta::assert_snapshot!(to_test_text(
            Parser::type_declaration,
            "type constrained_int_arr_2d is array (10 downto 5, 'A' to 'B', enum_t'range) of bit;"
        ));
    }

    #[test]
    fn record_type_declaration() {
        insta::assert_snapshot!(to_test_text(
            Parser::type_declaration,
            "type rec_t is record state: enum_t; end record;",
        ));

        insta::assert_snapshot!(to_test_text(
            Parser::type_declaration,
            "type rec_t is record s1: bit; s2, s3: std_ulogic; end record;",
        ));
    }
}
