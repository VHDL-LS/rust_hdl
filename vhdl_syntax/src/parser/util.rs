// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com
/// (private) utility functions used when parsing
use crate::parser::diagnostics::ParserDiagnostic;
use crate::parser::diagnostics::ParserError::*;
use crate::parser::Parser;
use crate::syntax::green::GreenNode;
use crate::syntax::node_kind::NodeKind;
use crate::tokens::TokenKind;
use crate::tokens::TokenStream;

impl<T: TokenStream> Parser<T> {
    pub(crate) fn expect_token(&mut self, kind: TokenKind) {
        if let Some(token) = self.tokenizer.next_if(|token| token.kind() == kind) {
            self.builder.push(token);
            return;
        }
        // TODO: what are possible recovery strategies?
        // - Leave as is
        // - Insert pseudo-token
        self.expect_tokens_err([kind]);
    }

    pub(crate) fn opt_token(&mut self, kind: TokenKind) -> bool {
        if let Some(token) = self.tokenizer.next_if(|token| token.kind() == kind) {
            self.builder.push(token);
            true
        } else {
            false
        }
    }

    pub(crate) fn start_node(&mut self, kind: NodeKind) {
        self.builder.start_node(kind)
    }

    pub(crate) fn end_node(&mut self) {
        self.builder.end_node()
    }

    pub(crate) fn eof_err(&mut self) {
        if !self.unexpected_eof {
            self.unexpected_eof = true;
            self.diagnostics
                .push(ParserDiagnostic::new(self.builder.current_pos(), Eof))
        }
    }

    pub(crate) fn expect_tokens_err(&mut self, tokens: impl Into<Box<[TokenKind]>>) {
        self.diagnostics.push(ParserDiagnostic::new(
            self.builder.current_pos(),
            ExpectingTokens(tokens.into()),
        ));
    }

    pub(crate) fn end(self) -> (GreenNode, Vec<ParserDiagnostic>) {
        (self.builder.end(), self.diagnostics)
    }
}
