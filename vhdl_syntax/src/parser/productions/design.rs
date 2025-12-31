//! Parsing of design files, and abstract design units.
//! The concrete design units (entity, architecture, ...) live in their own file.
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com
use crate::parser::builder::Checkpoint;
use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind;
use crate::tokens::token_kind::Keyword as Kw;
use crate::tokens::token_kind::TokenKind::*;
use crate::tokens::TokenKind;
use crate::tokens::TokenStream;

impl<T: TokenStream> Parser<T> {
    pub fn design_file(&mut self) {
        self.start_node(NodeKind::DesignFile);
        loop {
            self.design_unit();
            if !self.tokenizer.has_next() {
                break;
            }
        }
        self.end_node();
    }

    pub fn design_unit(&mut self) {
        self.start_node(NodeKind::DesignUnit);

        self.context_clause();
        match self.peek_token() {
            Some(Keyword(Kw::Architecture)) => self.architecture(),
            Some(Keyword(Kw::Package)) => {
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
            Some(Keyword(Kw::Entity)) => self.entity(),
            Some(Keyword(Kw::Configuration)) => self.configuration_declaration(),
            Some(Keyword(Kw::Context)) => self.context_declaration(),
            Some(_) => todo!("token: {:?}", self.tokenizer.peek(0).unwrap()),
            None => {}
        }
        self.end_node();
    }

    pub fn context_declaration(&mut self) {
        self.start_node(NodeKind::ContextDeclaration);
        self.expect_kw(Kw::Context);
        self.identifier();
        self.expect_kw(Kw::Is);
        self.context_clause();
        self.expect_kw(Kw::End);
        self.opt_token(Keyword(Kw::Context));
        self.opt_identifier();
        self.expect_token(SemiColon);
        self.end_node();
    }

    pub fn context_clause(&mut self) {
        self.start_node(NodeKind::ContextClause);
        loop {
            match self.peek_token() {
                Some(Keyword(Kw::Use)) => self.use_clause(),
                Some(Keyword(Kw::Library)) => self.library_clause(),
                Some(Keyword(Kw::Context)) => {
                    if !self.next_nth_is(Keyword(Kw::Is), 2) {
                        self.context_reference()
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }
        self.end_node();
    }

    pub fn library_clause(&mut self) {
        self.start_node(NodeKind::LibraryClause);
        self.expect_kw(Kw::Library);
        self.identifier_list();
        self.expect_token(SemiColon);
        self.end_node();
    }

    pub fn use_clause(&mut self) {
        self.start_node(NodeKind::UseClause);
        self.expect_kw(Kw::Use);
        self.name_list();
        self.expect_token(SemiColon);
        self.end_node();
    }

    pub fn context_reference(&mut self) {
        self.start_node(NodeKind::ContextReference);
        self.expect_kw(Kw::Context);
        self.name_list();
        self.expect_token(SemiColon);
        self.end_node();
    }

    pub fn configuration_declaration(&mut self) {
        self.start_node(NodeKind::ConfigurationDeclaration);
        self.expect_kw(Kw::Configuration);
        self.identifier();
        self.expect_kw(Kw::Of);
        self.name();
        self.expect_kw(Kw::Is);
        self.declarative_part();
        if self.next_is(Keyword(Kw::Vunit)) {
            todo!("VUnit")
        }
        self.block_configuration();
        self.expect_kw(Kw::End);
        self.opt_token(Keyword(Kw::Configuration));
        self.opt_identifier();
        self.expect_token(SemiColon);
        self.end_node();
    }

    pub fn block_configuration(&mut self) {
        self.start_node(NodeKind::BlockConfiguration);
        self.expect_kw(Kw::For);
        self.name();
        self.block_configuration_known_spec();
        self.end_node();
    }

    pub fn configuration_item(&mut self) {
        let checkpoint = self.checkpoint();
        self.expect_kw(Kw::For);
        self.configuration_item_known_keyword(checkpoint);
    }

    fn configuration_item_known_keyword(&mut self, item_checkpoint: Checkpoint) {
        match self.peek_token() {
            Some(tok @ Keyword(Kw::All | Kw::Others)) => {
                self.start_node_at(item_checkpoint, NodeKind::ComponentConfigurationItem);
                self.start_node(NodeKind::ComponentSpecification);
                if tok == Keyword(Kw::All) {
                    self.start_node(NodeKind::InstantiationListAll);
                } else {
                    self.start_node(NodeKind::InstantiationListOthers);
                }
                self.skip();
                self.end_node();
                self.expect_token(Colon);
                self.name();
                self.end_node();
                self.component_configuration_known_spec();
                self.end_node();
            }
            Some(Identifier) => {
                let checkpoint = self.checkpoint();
                self.skip();
                self.name();
                match self.peek_token() {
                    Some(Colon) => {
                        self.start_node_at(item_checkpoint, NodeKind::ComponentConfigurationItem);
                        self.start_node_at(checkpoint, NodeKind::ComponentSpecification);
                        self.start_node_at(checkpoint, NodeKind::InstantiationListList);
                        self.end_node();
                        self.skip();
                        self.name();
                        self.end_node();
                        self.component_configuration_known_spec();
                        self.end_node();
                    }
                    Some(Comma) => {
                        self.start_node_at(item_checkpoint, NodeKind::ComponentConfigurationItem);
                        self.start_node_at(checkpoint, NodeKind::ComponentSpecification);
                        self.start_node_at(checkpoint, NodeKind::InstantiationListList);
                        while self.next_is(Comma) {
                            self.skip();
                            // Change to identifier?
                            self.name();
                        }
                        self.end_node();
                        self.name();
                        self.end_node();
                        self.component_configuration_known_spec();
                        self.end_node();
                    }
                    _ => {
                        self.start_node_at(item_checkpoint, NodeKind::BlockConfigurationItem);
                        self.block_configuration_known_spec();
                        self.end_node();
                    }
                }
            }
            None => {
                self.eof_err();
                return;
            }
            _ => {
                todo!("Error handling")
            }
        }
    }

    fn component_configuration_known_spec(&mut self) {
        if self.next_is_one_of([Keyword(Kw::Use), Keyword(Kw::Generic), Keyword(Kw::Port)]) {
            self.start_node(NodeKind::SemiColonTerminatedBindingIndication);
            self.binding_indication();
            self.expect_token(TokenKind::SemiColon);
            self.end_node();
        }
        if self.next_is(Keyword(Kw::Vunit)) {
            todo!("VUnit")
        }
        if self.next_is(Keyword(Kw::For)) {
            self.block_configuration();
        }
        self.expect_tokens([Keyword(Kw::End), Keyword(Kw::For), SemiColon]);
    }

    fn block_configuration_known_spec(&mut self) {
        while self.next_is(Keyword(Kw::Use)) {
            self.use_clause();
        }
        while self.next_is(Keyword(Kw::For)) {
            self.configuration_item();
        }
        self.expect_tokens([Keyword(Kw::End), Keyword(Kw::For), SemiColon]);
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
        } else if self.next_is(Keyword(Kw::Component)) {
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
}
