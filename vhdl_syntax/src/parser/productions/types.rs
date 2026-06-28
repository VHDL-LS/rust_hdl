// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind::*;
use crate::tokens::Keyword as Kw;
use crate::tokens::TokenKind::*;

impl Parser {
    pub fn type_declaration(&mut self) {
        let checkpoint = self.checkpoint();
        self.expect_kw(Kw::Type);
        self.identifier();
        if self.opt_token(SemiColon) {
            self.start_node_at(checkpoint, IncompleteTypeDeclaration);
            self.end_node();
            return;
        }
        self.start_node_at(checkpoint, FullTypeDeclaration);
        self.expect_kw(Kw::Is);
        self.type_definition();
        self.expect_token(SemiColon);
        self.end_node();
    }

    pub fn type_definition(&mut self) {
        match_next_token!(self,
            Keyword(Kw::Range) => self.numeric_type_definition(),
            Keyword(Kw::Access) => {
                self.start_node(AccessTypeDefinition);
                self.skip();
                self.subtype_indication();
                self.end_node();
            },
            Keyword(Kw::Protected) => self.protected_type_definition(),
            Keyword(Kw::File) => self.file_type_definition(),
            Keyword(Kw::Array) => self.array_type_definition(),
            Keyword(Kw::Record) => self.record_type_definition(),
            LeftPar => self.enumeration_type_definition()
        )
    }

    pub fn protected_type_definition(&mut self) {
        let checkpoint = self.checkpoint();
        let is_body =
            self.next_is(Keyword(Kw::Protected)) && self.next_nth_is(Keyword(Kw::Body), 1);
        // Build the preamble first so it owns the `protected [body]` keywords,
        // then retroactively wrap it (and the rest) in the outer node.
        if is_body {
            self.start_node(ProtectedTypeBodyPreamble);
            self.expect_tokens([Keyword(Kw::Protected), Keyword(Kw::Body)]);
        } else {
            self.start_node(ProtectedTypeDeclarationPreamble);
            self.expect_kw(Kw::Protected);
        }
        self.end_node();
        if is_body {
            self.start_node_at(checkpoint, ProtectedTypeBody);
        } else {
            self.start_node_at(checkpoint, ProtectedTypeDeclaration);
        }
        self.declarations();
        if is_body {
            self.start_node(ProtectedTypeBodyEpilogue);
        } else {
            self.start_node(ProtectedTypeDeclarationEpilogue);
        }
        self.expect_tokens([Keyword(Kw::End), Keyword(Kw::Protected)]);
        if is_body {
            self.expect_token(Keyword(Kw::Body));
        }
        self.opt_identifier();
        self.end_node();
        self.end_node();
    }

    pub fn file_type_definition(&mut self) {
        self.start_node(FileTypeDefinition);
        self.expect_tokens([Keyword(Kw::File), Keyword(Kw::Of)]);
        self.type_mark();
        self.end_node();
    }

    pub fn access_type_definition(&mut self) {
        self.start_node(AccessTypeDefinition);
        self.expect_kw(Kw::Access);
        self.subtype_indication();
        self.end_node();
    }

    pub fn subtype_declaration(&mut self) {
        self.start_node(SubtypeDeclaration);
        self.expect_kw(Kw::Subtype);
        self.identifier();
        self.expect_kw(Kw::Is);
        self.subtype_indication();
        self.expect_token(SemiColon);
        self.end_node();
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils::to_test_text;
    use crate::parser::Parser;

    #[test]
    fn incomplete_type_declaration() {
        insta::assert_snapshot!(to_test_text(
            Parser::type_declaration,
            "type incomplete_type;"
        ));
    }

    #[test]
    fn file_type_declaration() {
        insta::assert_snapshot!(to_test_text(
            Parser::type_declaration,
            "type IntegerFile is file of integer;"
        ));

        insta::assert_snapshot!(to_test_text(
            Parser::type_declaration,
            "type sl_file is file of ieee.std_logic_1164.std_ulogic;"
        ));
    }

    #[test]
    fn access_type_definition() {
        insta::assert_snapshot!(to_test_text(
            Parser::type_declaration,
            "type str_ptr_t is access string;"
        ));
    }

    #[test]
    fn protected_type_declaration() {
        insta::assert_snapshot!(to_test_text(
            Parser::type_declaration,
            "type p_t is protected end protected;",
        ));

        insta::assert_snapshot!(to_test_text(
            Parser::type_declaration,
            "type p_t is protected end protected p_t;"
        ));
        insta::assert_snapshot!(to_test_text(
            Parser::type_declaration,
            "\
type foo is protected
  procedure proc;
  function fun return ret;
end protected;"
        ));
    }

    #[test]
    fn protected_type_body() {
        insta::assert_snapshot!(to_test_text(
            Parser::type_declaration,
            "type p_t is protected body end protected body;"
        ));
        insta::assert_snapshot!(to_test_text(
            Parser::type_declaration,
            "type p_t is protected body end protected body p_t;"
        ));
        insta::assert_snapshot!(to_test_text(
            Parser::type_declaration,
            "\
type foo is protected body
  variable foo : natural;
  procedure proc is
  begin
  end;
end protected body;"
        ));
    }

    #[test]
    fn test_parse_subtype_declaration() {
        insta::assert_snapshot!(to_test_text(
            Parser::subtype_declaration,
            "subtype vec_t is integer_vector(2-1 downto 0);"
        ));
    }

    // MARK: Error recovery

    #[test]
    fn type_missing_is() {
        assert_recovery_snapshot!(
            "type state_t (idle, running, done);",
            Parser::type_declaration
        );
    }

    #[test]
    fn type_missing_definition() {
        assert_recovery_snapshot!("type state_t is ;", Parser::type_declaration);
    }

    #[test]
    fn subtype_missing_indication() {
        assert_recovery_snapshot!("subtype small_int is ;", Parser::subtype_declaration);
    }
}
