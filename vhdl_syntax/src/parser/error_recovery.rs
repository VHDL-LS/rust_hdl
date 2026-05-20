// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

use crate::parser::Parser;
use crate::parser::diagnostics::ParserDiagnostic;
use crate::syntax::NodeKind;
use crate::tokens::{Keyword as Kw, TokenKind};

/// Tokens that begin a new design unit. Carried by `DesignFile` so any
/// runaway skip eventually terminates at the start of the next unit, even
/// if every intermediate frame has an empty FOLLOW.
const DESIGN_UNIT_STARTERS: &[TokenKind] = &[
    TokenKind::SemiColon,
    TokenKind::Keyword(Kw::Entity),
    TokenKind::Keyword(Kw::Architecture),
    TokenKind::Keyword(Kw::Package),
    TokenKind::Keyword(Kw::Configuration),
    TokenKind::Keyword(Kw::Context),
    TokenKind::Keyword(Kw::Library),
    TokenKind::Keyword(Kw::Use),
];

pub(crate) fn sync_tokens_for_node_kind(nk: NodeKind) -> &'static [TokenKind] {
    match nk {
        // Bottom-of-stack frame: always present, guarantees recovery
        // terminates at the next top-level construct.
        NodeKind::DesignFile => DESIGN_UNIT_STARTERS,

        // Inside an argument/element list of a parenthesised construct,
        // the natural recovery points are the comma separator and the
        // closing paren.
        NodeKind::ActualPart
        | NodeKind::ActualPartExpression
        | NodeKind::ActualPartOpen => &[TokenKind::RightPar, TokenKind::Comma],

        // Everything else: no node-specific recovery yet. Falls back on
        // ancestors via the sync stack.
        _ => &[],
    }
}

impl Parser {
    /// Is `tok` somewhere in the FOLLOW set of any currently-open node?
    /// Used by error recovery to decide when to stop panic-mode skipping.
    pub(crate) fn is_recovery_point(&self, tok: TokenKind) -> bool {
        self.sync_stack.iter().any(|follow| follow.contains(&tok))
    }

    /// Publish diagnostics and recover when expecting one of several tokens.
    pub(crate) fn expect_tokens_recover<const N: usize>(&mut self, expected: [TokenKind; N]) {
        assert!(
            !expected.contains(&self.peek_token()),
            "should only be called on an error path"
        );

        let start = self.builder.current_pos();

        // We're at EoF -> emit an EoF diagnostic if we haven't already
        if self.peek_token().is_eof() {
            if self.unexpected_eof {
                return;
            }
            self.unexpected_eof = true;
            self.diagnostics
                .push(ParserDiagnostic::eof_err(expected, start));
            return;
        }

        let mut skipped_any = false;
        loop {
            let tok = self.peek_token();
            // Expected contains the token before we hit recovery or EoF -> Assume garbage input
            if expected.contains(&tok) {
                let end = self.builder.current_pos();
                self.diagnostics
                    .push(ParserDiagnostic::unexpected_input(start..end));
                return;
            }

            // We have hit a recovery or EoF token. This means that it was likely only this token that's erroneous
            //
            // recovery should in theory always include EoF; this is simply a cheap safeguard
            // to avoid infinte looping.
            if self.is_recovery_point(tok) || tok.is_eof() {
                // We found a recovery token at the next position.
                // This means the token is simply missing.
                // Don't consume; simply report diagnostic.
                if !skipped_any {
                    let pos = self
                        .token_stream
                        .peek_next()
                        .map(|tok| start + tok.leading_trivia().len()..start + tok.byte_len())
                        .unwrap_or(start..start);
                    self.diagnostics
                        .push(ParserDiagnostic::missing_token(expected, start, tok, pos));
                // skipped tokens: Garbage input before recovery token.
                } else {
                    self.diagnostics.push(ParserDiagnostic::unexpected_input(
                        start..self.builder.current_pos(),
                    ));
                }
                return;
            }

            // inconclusive. Skip and look at the next token.
            self.skip();
            skipped_any = true;
        }
    }

}
