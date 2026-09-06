//! Parsing of entity declarations
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com
use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind::*;
use crate::syntax::AstNode;
use crate::syntax::{EntityDeclarativeItemSyntax, EntityStatementSyntax};
use crate::tokens::token_kind::Keyword as Kw;
use crate::tokens::token_kind::TokenKind::*;

impl Parser {
    pub(crate) fn entity_declaration(&mut self) {
        self.node(EntityDeclaration, |p| {
            p.entity_declaration_preamble();
            p.entity_header();
            p.entity_declarative_part();
            if p.next_is(Keyword(Kw::Begin)) {
                p.node(EntityStatements, |p| {
                    p.skip_into_node(DeclarationStatementSeparator);
                    p.entity_statement_part();
                });
            }
            p.entity_declaration_epilogue();
        });
    }

    pub(crate) fn entity_declaration_preamble(&mut self) {
        self.node(EntityDeclarationPreamble, |p| {
            p.expect_token(Keyword(Kw::Entity));
            p.identifier();
            p.expect_token(Keyword(Kw::Is));
        });
    }

    pub(crate) fn entity_declaration_epilogue(&mut self) {
        self.node(EntityDeclarationEpilogue, |p| {
            p.expect_token(Keyword(Kw::End));
            p.opt_token(Keyword(Kw::Entity));
            p.opt_identifier();
            p.expect_token(SemiColon);
        });
    }

    fn entity_header(&mut self) {
        self.node(EntityHeader, |p| {
            p.opt_generic_clause();
            p.opt_port_clause();
        });
    }

    pub(crate) fn entity_declarative_part(&mut self) {
        self.declarations(EntityDeclarativePart, EntityDeclarativeItemSyntax::META);
    }

    pub(crate) fn entity_statement_part(&mut self) {
        self.concurrent_statements(EntityStatementPart, EntityStatementSyntax::META);
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils::to_test_text;
    use crate::parser::Parser;

    #[test]
    fn entity_statement_part_rejects_non_passive_statement() {
        assert_recovery_snapshot!(
            "\
entity myent is
begin
  sig <= '0';
end entity;",
            Parser::entity_declaration
        );
    }

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
    fn parse_entity_with_generics_and_ports() {
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

    // MARK: Error recovery

    #[test]
    fn entity_missing_is() {
        assert_recovery_snapshot!(
            "\
entity uart
end entity;",
            Parser::entity_declaration
        );
    }

    #[test]
    fn entity_unclosed_port_list() {
        assert_recovery_snapshot!(
            "\
entity uart is
  port (
    clk : in std_logic;
    rst : in std_logic
end entity;",
            Parser::entity_declaration
        );
    }

    /// An empty `generic ()` / `port ()` is a syntax error now that an `InterfaceList` cannot
    /// be empty. Each empty clause must cost exactly one diagnostic and leave the rest of the
    /// entity — including the second clause — parsed as usual.
    #[test]
    fn parse_entity_with_empty_generics() {
        assert_recovery_snapshot!(
            "\
entity my_ent is
    generic();
begin
end my_ent;
",
            Parser::entity_declaration
        );
    }

    /// See [`parse_entity_with_empty_generics`].
    #[test]
    fn parse_entity_with_empty_ports() {
        assert_recovery_snapshot!(
            "\
entity my_ent is
    port();
begin
end my_ent;
",
            Parser::entity_declaration
        );
    }

    /// See [`parse_entity_with_empty_generics`].
    #[test]
    fn parse_entity_with_empty_generics_and_ports() {
        assert_recovery_snapshot!(
            "\
entity my_ent is
    generic();
    port();
begin
end my_ent;
",
            Parser::entity_declaration
        );
    }

    #[test]
    fn entity_missing_trailing_semicolon() {
        assert_recovery_snapshot!(
            "\
entity uart is
end entity",
            Parser::entity_declaration
        );
    }
}
