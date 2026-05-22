// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

use crate::parser::diagnostics::ParserDiagnostic;
use crate::parser::Parser;
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
        NodeKind::ActualPart | NodeKind::ActualPartExpression | NodeKind::ActualPartOpen => {
            &[TokenKind::RightPar, TokenKind::Comma]
        }

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
                        .map(|tok| start + tok.leading_trivia().byte_len()..start + tok.byte_len())
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::diagnostics::ParserDiagnostic;
    use crate::parser::parse_syntax;
    use crate::parser::{parse, Parser};

    fn assert_unexpected_input(diag: &ParserDiagnostic, expected_span: std::ops::Range<usize>) {
        match diag {
            ParserDiagnostic::UnexpectedInput { span } => {
                assert_eq!(*span, expected_span, "wrong span on UnexpectedInput");
            }
            other => panic!("expected UnexpectedInput, got {:?}", other),
        }
    }

    fn assert_expected_token(
        diag: &ParserDiagnostic,
        expected_kinds: &[TokenKind],
        found_kind: TokenKind,
    ) {
        match diag {
            ParserDiagnostic::ExpectedToken { expected, found } => {
                assert_eq!(&*expected.1, expected_kinds, "expected kinds mismatch");
                assert_eq!(found.1, found_kind, "found kind mismatch");
            }
            other => panic!("expected ExpectedToken, got {:?}", other),
        }
    }

    #[test]
    fn is_recovery_point_walks_full_sync_stack() {
        let (_, _diags) = parse_syntax("entity", |p: &mut Parser| {
            assert!(p.sync_stack.is_empty());
            assert!(!p.is_recovery_point(TokenKind::RightPar));

            // start_node(DesignFile) pushes DESIGN_UNIT_STARTERS.
            p.start_node(NodeKind::DesignFile);
            assert!(p.is_recovery_point(TokenKind::SemiColon));
            assert!(p.is_recovery_point(TokenKind::Keyword(Kw::Entity)));
            assert!(!p.is_recovery_point(TokenKind::RightPar));

            // start_node(ActualPart) pushes [RightPar, Comma].
            p.start_node(NodeKind::ActualPart);
            // A token only present in a deeper frame still counts.
            assert!(p.is_recovery_point(TokenKind::RightPar));
            assert!(p.is_recovery_point(TokenKind::Comma));
            // Outer frame still active.
            assert!(p.is_recovery_point(TokenKind::SemiColon));

            p.skip();
            p.end_node();
            p.end_node();
            assert!(!p.is_recovery_point(TokenKind::SemiColon));
        });
    }

    #[test]
    fn diagnostic_constructors_shape() {
        let eof = ParserDiagnostic::eof_err([TokenKind::SemiColon], 42);
        assert_expected_token(&eof, &[TokenKind::SemiColon], TokenKind::Eof);
        match eof {
            ParserDiagnostic::ExpectedToken { expected, found } => {
                assert_eq!(expected.0, 42);
                assert_eq!(found.0, 42..42);
            }
            _ => unreachable!(),
        }

        assert_unexpected_input(&ParserDiagnostic::unexpected_input(3..7), 3..7);

        let missing = ParserDiagnostic::missing_token(
            [TokenKind::Keyword(Kw::Is)],
            10,
            TokenKind::Keyword(Kw::End),
            10..13,
        );
        match missing {
            ParserDiagnostic::ExpectedToken { expected, found } => {
                assert_eq!(expected.0, 10);
                assert_eq!(&*expected.1, &[TokenKind::Keyword(Kw::Is)]);
                assert_eq!(found.0, 10..13);
                assert_eq!(found.1, TokenKind::Keyword(Kw::End));
            }
            _ => unreachable!(),
        }
    }

    /// Missing token: recovery token is sitting at the cursor, so nothing is
    /// skipped and a single ExpectedToken diagnostic is produced.
    #[test]
    fn expect_recover_emits_missing_token_when_recovery_point_is_next() {
        let (_, diags) = parse_syntax(";", |p: &mut Parser| {
            // start_node(DesignFile) opens a frame with DESIGN_UNIT_STARTERS.
            p.start_node(NodeKind::DesignFile);
            p.expect_tokens_recover([TokenKind::Keyword(Kw::Is)]);
            // The recovery token must NOT have been consumed.
            assert_eq!(p.peek_token(), TokenKind::SemiColon);
            p.skip(); // consume so the tree is non-empty
            p.end_node();
        });
        assert_eq!(diags.len(), 1);
        assert_expected_token(
            &diags[0],
            &[TokenKind::Keyword(Kw::Is)],
            TokenKind::SemiColon,
        );
    }

    /// Garbage before a recovery token: tokens are consumed up to (not
    /// including) the recovery token, and exactly one UnexpectedInput is
    /// reported covering the skipped range.
    #[test]
    fn expect_recover_emits_unexpected_input_after_skipping() {
        let (root, diags) = parse_syntax("foo bar ;", |p: &mut Parser| {
            p.start_node(NodeKind::DesignFile);
            p.expect_tokens_recover([TokenKind::Keyword(Kw::Is)]);
            assert_eq!(
                p.peek_token(),
                TokenKind::SemiColon,
                "recovery token must remain unconsumed"
            );
            p.skip();
            p.end_node();
        });
        assert_eq!(diags.len(), 1, "got: {:?}", diags);
        match &diags[0] {
            ParserDiagnostic::UnexpectedInput { span } => {
                assert!(span.start < span.end, "span should be non-empty");
            }
            other => panic!("expected UnexpectedInput, got {:?}", other),
        }
        // The skipped tokens must be attached to the green tree (not dropped).
        let mut buf = Vec::new();
        root.write_to(&mut buf).unwrap();
        let text = String::from_utf8(buf).unwrap();
        assert!(text.contains("foo"));
        assert!(text.contains("bar"));
    }

    /// Expected token shows up after garbage (not a recovery token):
    /// skipping stops once the expected token is reached, an
    /// UnexpectedInput diagnostic is reported, and the expected token
    /// remains unconsumed for the caller.
    #[test]
    fn expect_recover_stops_when_expected_token_appears() {
        let (_, diags) = parse_syntax("garbage is", |p: &mut Parser| {
            p.start_node(NodeKind::DesignFile);
            p.expect_tokens_recover([TokenKind::Keyword(Kw::Is)]);
            // expected token kept for the caller to consume.
            assert_eq!(p.peek_token(), TokenKind::Keyword(Kw::Is));
            p.skip();
            p.end_node();
        });
        assert_eq!(diags.len(), 1);
        match &diags[0] {
            ParserDiagnostic::UnexpectedInput { span } => {
                assert!(span.start < span.end);
            }
            other => panic!("expected UnexpectedInput, got {:?}", other),
        }
    }

    /// EOF path: a single EOF diagnostic is emitted, and a subsequent call
    /// is idempotent (the `unexpected_eof` flag suppresses duplicates).
    #[test]
    fn expect_recover_at_eof_emits_once() {
        // Non-empty input so the wrapper node has at least one child;
        // we consume the lone token before calling expect_tokens_recover.
        let (_, diags) = parse_syntax("x", |p: &mut Parser| {
            p.start_node(NodeKind::DesignFile);
            p.skip();
            assert_eq!(p.peek_token(), TokenKind::Eof);
            p.expect_tokens_recover([TokenKind::Keyword(Kw::Is)]);
            // Second call must NOT push another diagnostic.
            p.expect_tokens_recover([TokenKind::Keyword(Kw::End)]);
            p.end_node();
        });
        assert_eq!(diags.len(), 1, "expected exactly one EoF diagnostic");
        assert_expected_token(&diags[0], &[TokenKind::Keyword(Kw::Is)], TokenKind::Eof);
    }

    /// Recovery falls back to the bottom-of-stack DesignFile frame: with no
    /// matching token in any inner frame, skipping continues until a
    /// design-unit starter is seen.
    #[test]
    fn expect_recover_falls_through_to_design_unit_starter() {
        let (_, diags) = parse_syntax("garbage more entity", |p: &mut Parser| {
            // Bottom-of-stack DesignFile frame, plus an inner frame whose
            // FOLLOW does not contain any of the tokens in the input.
            p.start_node(NodeKind::DesignFile);
            p.sync_stack.push(&[TokenKind::RightPar]);
            p.expect_tokens_recover([TokenKind::Keyword(Kw::Is)]);
            assert_eq!(p.peek_token(), TokenKind::Keyword(Kw::Entity));
            p.sync_stack.pop();
            p.skip();
            p.end_node();
        });
        assert_eq!(diags.len(), 1);
        assert!(matches!(diags[0], ParserDiagnostic::UnexpectedInput { .. }));
    }

    /// The `missing_token` diagnostic must point at the *text* of the next
    /// token, not at its trivia. With multi-piece leading trivia (e.g. a
    /// blank line: newline + newline) the start of the reported span must
    /// be advanced by the trivia's byte length, not its piece count.
    #[test]
    fn missing_token_span_skips_full_trivia_bytes() {
        // Two newlines = two TriviaPiece entries, 2 bytes of trivia.
        let src = "\n\n;";
        let (_, diags) = parse_syntax(src, |p: &mut Parser| {
            p.start_node(NodeKind::DesignFile);
            p.expect_tokens_recover([TokenKind::Keyword(Kw::Is)]);
            p.skip();
            p.end_node();
        });
        assert_eq!(diags.len(), 1);
        match &diags[0] {
            ParserDiagnostic::ExpectedToken { found, .. } => {
                // The `;` token starts after both newlines (2 bytes of
                // trivia) and is itself 1 byte long.
                assert_eq!(found.0, 2..3, "diagnostic should point at ';' text");
                assert_eq!(found.1, TokenKind::SemiColon);
            }
            other => panic!("expected ExpectedToken, got {:?}", other),
        }
    }

    // ---- End-to-end smoke tests via the public `parse` entrypoint ----

    /// Top-level garbage triggers `expect_tokens_recover` from `design_unit`;
    /// the DesignFile frame guarantees the parser terminates rather than
    /// looping on the bad input.
    #[test]
    fn parse_garbage_at_top_level_terminates_with_diagnostic() {
        let (_, diags) = parse("xyz");
        assert!(!diags.is_empty(), "expected at least one diagnostic");
    }

    /// Garbage between two design units must not consume the second unit's
    /// starter keyword — recovery should leave `entity` in place so the
    /// next iteration of `design_file` picks it up.
    #[test]
    fn parse_recovers_to_next_design_unit() {
        let src = "garbage entity foo is end entity foo;";
        let (file, diags) = parse(src);
        assert!(!diags.is_empty(), "expected diagnostics for garbage prefix");
        // The recovered tree must still contain the second unit's text.
        let mut buf = Vec::new();
        file.0.write_to(&mut buf).unwrap();
        let text = String::from_utf8(buf).unwrap();
        assert!(text.contains("entity"));
        assert!(text.contains("foo"));
    }
}
