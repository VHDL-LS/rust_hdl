//! Parsing of identifiers
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com
use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind::IdentifierList;
use crate::tokens::TokenKind;
use crate::tokens::TokenKind::Comma;

impl Parser {
    pub fn identifier(&mut self) {
        self.expect_token(TokenKind::Identifier)
    }

    pub fn opt_identifier(&mut self) -> bool {
        self.opt_token(TokenKind::Identifier)
    }

    pub fn identifier_list(&mut self) {
        self.start_node(IdentifierList);
        self.separated_list(Parser::identifier, Comma);
        self.end_node();
    }
}
