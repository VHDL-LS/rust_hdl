//! Parsing of design files, and abstract design units.
//! The concrete design units (entity, architecture, ...) live in their own file.
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind;
use crate::tokens::token_kind::Keyword as Kw;
use crate::tokens::token_kind::TokenKind::*;

impl Parser {
    pub fn design_file(&mut self) {
        self.start_node(NodeKind::DesignFile);
        while self.peek_token() != Eof {
            self.design_unit();
        }
        assert!(self.next_is(Eof), "No EoF token in design file");
        self.skip();
        self.end_node();
    }

    pub fn design_unit(&mut self) {
        self.start_node(NodeKind::DesignUnit);

        self.context_clause();
        match self.peek_token() {
            Keyword(Kw::Architecture) => self.architecture(),
            Keyword(Kw::Package) => {
                if self.next_nth_is(Keyword(Kw::Body), 1) {
                    self.start_node(NodeKind::SecondaryUnitPackageBody);
                    self.package_body();
                    self.end_node();
                } else if self.next_nth_is(Keyword(Kw::New), 3) {
                    self.package_instantiation_declaration();
                } else {
                    self.package_declaration();
                }
            }
            Keyword(Kw::Entity) => self.entity_declaration(),
            Keyword(Kw::Configuration) => self.configuration_declaration(),
            Keyword(Kw::Context) => self.context_declaration(),
            _ => self.expect_tokens_err([
                Keyword(Kw::Architecture),
                Keyword(Kw::Package),
                Keyword(Kw::Entity),
                Keyword(Kw::Configuration),
                Keyword(Kw::Context),
            ]),
        }
        self.end_node();
    }

    pub fn context_declaration(&mut self) {
        self.start_node(NodeKind::ContextDeclaration);
        self.context_declaration_preamble();
        self.context_clause();
        self.context_declaration_epilogue();
        self.end_node();
    }

    pub fn context_declaration_preamble(&mut self) {
        self.start_node(NodeKind::ContextDeclarationPreamble);
        self.expect_kw(Kw::Context);
        self.identifier();
        self.expect_kw(Kw::Is);
        self.end_node();
    }

    pub fn context_declaration_epilogue(&mut self) {
        self.start_node(NodeKind::ContextDeclarationEpilogue);
        self.expect_kw(Kw::End);
        self.opt_token(Keyword(Kw::Context));
        self.opt_identifier();
        self.expect_token(SemiColon);
        self.end_node();
    }

    pub fn binding_indication(&mut self) {
        self.start_node(NodeKind::BindingIndication);
        if self.next_is(Keyword(Kw::Use)) {
            self.skip();
            self.entity_aspect();
        }
        if self.next_is(Keyword(Kw::Generic)) {
            self.generic_map_aspect();
        }
        if self.next_is(Keyword(Kw::Port)) {
            self.port_map_aspect();
        }
        self.end_node();
    }

    pub fn entity_aspect(&mut self) {
        if self.next_is(Keyword(Kw::Open)) {
            self.skip_into_node(NodeKind::EntityOpenAspect);
        } else if self.next_is(Keyword(Kw::Entity)) {
            self.start_node(NodeKind::EntityEntityAspect);
            self.skip();
            self.name();
            self.end_node();
        } else if self.next_is(Keyword(Kw::Configuration)) {
            self.start_node(NodeKind::EntityConfigurationAspect);
            self.skip();
            self.name();
            self.end_node();
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{test_utils::to_test_text, Parser};

    #[test]
    fn parse_empty() {
        insta::assert_snapshot!(to_test_text(Parser::design_file, ""));
    }

    #[test]
    fn parse_multiple_entity_declarations() {
        insta::assert_snapshot!(to_test_text(
            Parser::design_file,
            "\
entity myent is
end entity;

entity myent2 is
end entity myent2;

entity myent3 is
end myent3;

entity myent4 is
end;",
        ));
    }

    #[test]
    fn parse_simple_entity() {
        insta::assert_snapshot!(to_test_text(
            Parser::design_file,
            "\
entity my_ent is
begin
end my_ent;

entity my_ent2 is
begin
end entity;
",
        ));
    }

    #[test]
    fn parse_entity_with_context_clause() {
        insta::assert_snapshot!(to_test_text(
            Parser::design_file,
            "\
library ieee;
use ieee.std_logic_1164.all;

entity my_ent is
begin
end my_ent;",
        ));
    }

    #[test]
    fn parse_use_clause() {
        insta::assert_snapshot!(to_test_text(Parser::use_clause, "use lib1.lib2.lib3.all;"));
    }

    #[test]
    fn test_context_clause() {
        insta::assert_snapshot!(to_test_text(
            Parser::context_declaration,
            "\
context ident is
end;"
        ));
        insta::assert_snapshot!(to_test_text(
            Parser::context_declaration,
            "\
context ident is
end context;"
        ));
        insta::assert_snapshot!(to_test_text(
            Parser::context_declaration,
            "\
context ident is
end ident;"
        ));
        insta::assert_snapshot!(to_test_text(
            Parser::context_declaration,
            "\
context ident is
end context ident;"
        ));
    }

    #[test]
    fn test_context_clause_items() {
        insta::assert_snapshot!(to_test_text(
            Parser::context_declaration,
            "\
context ident is
  library foo;
  use foo.bar;
  context foo.ctx;
end context;"
        ));
    }

    #[test]
    fn context_clause_associated_with_design_units() {
        insta::assert_snapshot!(to_test_text(
            Parser::design_file,
            "\
library lib;
use lib.foo;

entity myent is
end entity;"
        ));
    }
}
