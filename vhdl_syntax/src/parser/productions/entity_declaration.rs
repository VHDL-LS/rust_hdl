//! Parsing of entity declarations
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com
use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind::*;
use crate::tokens::token_kind::Keyword as Kw;
use crate::tokens::token_kind::TokenKind::*;

impl Parser {
    pub fn entity_declaration(&mut self) {
        self.start_node(EntityDeclaration);
        self.entity_declaration_preamble();
        self.entity_header();
        self.opt_declarative_part();
        if self.next_is(Keyword(Kw::Begin)) {
            self.skip_into_node(DeclarationStatementSeparator);
            self.concurrent_statements();
        }
        self.entity_declaration_epilogue();
        self.end_node();
    }

    pub fn entity_declaration_preamble(&mut self) {
        self.start_node(EntityDeclarationPreamble);
        self.expect_token(Keyword(Kw::Entity));
        self.identifier();
        self.expect_token(Keyword(Kw::Is));
        self.end_node();
    }

    pub fn entity_declaration_epilogue(&mut self) {
        self.start_node(EntityDeclarationEpilogue);
        self.expect_token(Keyword(Kw::End));
        self.opt_token(Keyword(Kw::Entity));
        self.opt_identifier();
        self.expect_token(SemiColon);
        self.end_node();
    }

    fn entity_header(&mut self) {
        self.start_node(EntityHeader);
        self.opt_generic_clause();
        self.opt_port_clause();
        self.end_node();
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils::to_test_text;
    use crate::parser::Parser;

    #[test]
    fn parse_entity_declaration() {
        insta::assert_snapshot!(to_test_text(
            Parser::entity_declaration,
            "\
entity myent is
end entity;",
        ));

        insta::assert_snapshot!(to_test_text(
            Parser::entity_declaration,
            "\
entity myent is
end entity myent;",
        ));
    }

    #[test]
    fn parse_simple_entity() {
        insta::assert_snapshot!(to_test_text(
            Parser::entity_declaration,
            "\
entity my_ent is
begin
end my_ent;
",
        ));
    }

    #[test]
    fn parse_entity_with_generics() {
        insta::assert_snapshot!(to_test_text(
            Parser::entity_declaration,
            "\
entity my_ent is
    generic();
begin
end my_ent;
",
        ));
    }

    #[test]
    fn parse_entity_with_ports() {
        insta::assert_snapshot!(to_test_text(
            Parser::entity_declaration,
            "\
entity my_ent is
    port();
begin
end my_ent;
",
        ));
    }

    #[test]
    fn parse_entity_with_generics_and_ports() {
        insta::assert_snapshot!(to_test_text(
            Parser::entity_declaration,
            "\
entity my_ent is
    generic();
    port();
begin
end my_ent;
",
        ));
    }

    #[test]
    fn parse_entity_with_filled_generics_and_ports() {
        insta::assert_snapshot!(to_test_text(
            Parser::entity_declaration,
            "\
entity my_ent is
    generic(constant a: in bit);
    port(
        b, c : out std_logic;
        signal d : linkage boolean
    );
begin
end my_ent;
",
        ));
    }

    #[test]
    fn parse_entity_with_declarations() {
        insta::assert_snapshot!(to_test_text(
            Parser::entity_declaration,
            "\
entity myent is
  constant foo : natural := 0;
end entity;",
        ));
    }

    #[test]
    fn parse_entity_with_statements() {
        insta::assert_snapshot!(to_test_text(
            Parser::entity_declaration,
            "\
entity myent is
begin
  check(clk, valid);
end entity;",
        ));
    }
}
