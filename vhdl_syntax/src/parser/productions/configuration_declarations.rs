// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::builder::Checkpoint;
use crate::parser::Parser;
use crate::syntax::NodeKind;
use crate::tokens::TokenKind::*;
use crate::tokens::TokenStream;
use crate::tokens::{Keyword as Kw, TokenKind};

impl<T: TokenStream> Parser<T> {
    pub fn configuration_declaration(&mut self) {
        self.start_node(NodeKind::ConfigurationDeclaration);
        self.expect_kw(Kw::Configuration);
        self.identifier();
        self.expect_kw(Kw::Of);
        self.name();
        self.expect_kw(Kw::Is);
        // In general, we want to allow as many declarations as possible
        // to enable robust parsing. However, "for" clashed with the
        // declarative part "for".
        if !self.next_is(Keyword(Kw::For)) {
            self.opt_declarative_part();
        }
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

    pub fn configuration_item(&mut self) {
        let checkpoint = self.checkpoint();
        self.expect_kw(Kw::For);
        self.configuration_item_known_keyword(checkpoint);
    }

    pub fn block_configuration(&mut self) {
        self.start_node(NodeKind::BlockConfiguration);
        self.expect_kw(Kw::For);
        self.name();
        self.block_configuration_known_spec();
        self.end_node();
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
}
