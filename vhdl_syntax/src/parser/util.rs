// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com
/// (private) utility functions used when parsing
use crate::parser::error::{SyntaxErr, SyntaxErrKind};
use crate::parser::marker::{CompletedMarker, Marker, UnknownMarker};
use crate::parser::Parser;
use crate::syntax::child::ChildKind;
use crate::syntax::green::GreenNode;
use crate::syntax::meta::Layout;
use crate::syntax::node_kind::NodeKind;
use crate::tokens::{Keyword, TokenKind};

/// Allows match-style syntax for tokens.
/// This function does not consume the next token.
/// If the token was not seen, or the parser is at EOF, this function pushes an error.
///
/// Note: It is possible to simulate 'Or' branches, i.e.,
/// ```no-test
/// match x {
///     A | B => { /*...*/ }
/// }
/// ```
/// However, due to the syntax limitations from macro_rules, these
/// are spelled out in the following way:
/// ```no-test
/// match_next_token!(x,
///     A, B => { /*...*/ }
/// )
/// ```
macro_rules! match_next_token {
    ($parser:expr, $($body:tt)*) => {
        match_next_token!(@inner $parser, [[ $($body)* ]], [[ $($body)* ]])
    };
    (@inner $parser:expr, [[ $($($pattern:pat_param),+ => $action:expr),+ $(,)? ]], [[ $($($pattern_expr:expr),+ => $_action_expr:expr),+ $(,)? ]]) => {
        match $parser.peek_token() {
            $($($pattern)|+ => $action),+,
            _ => {
                $parser.expect_tokens_recover([$($($pattern_expr),+),+]);
                Default::default()
            }
        }
    };
}

pub enum LookaheadError {
    /// EOF was encountered before any of the desired `TokenKind`s was found.
    Eof,
    /// The maximum index was reached before any of the desired `TokenKind`s was found.
    MaxIndexReached,
    /// None of the desired `TokenKind`s was found withing the current parenthesis
    TokenKindNotFound,
}

/// Guards a parsing loop against hangs by detecting lack of forward progress.
///
/// Call [`StallGuard::should_continue`] at the top of the loop. It returns
/// `false` once an entire iteration consumed no input (no token was pushed),
/// which means the loop is stalled and must stop. The first call always
/// returns `true` to prime the guard before the loop body has run.
pub(crate) struct StallGuard {
    last_token_index: Option<usize>,
}

impl StallGuard {
    pub(crate) fn new() -> StallGuard {
        StallGuard {
            last_token_index: None,
        }
    }

    pub(crate) fn should_continue(&mut self, parser: &mut Parser) -> bool {
        let current = parser.token_index();
        let Some(last) = self.last_token_index else {
            self.last_token_index = Some(current);
            return true;
        };
        self.last_token_index = Some(current);
        current > last
    }
}

pub(crate) const fn choice_options(layout: &Layout) -> &[NodeKind] {
    match layout {
        Layout::Choice(choice) => choice.options,
        _ => panic!("Not a layout choice"),
    }
}

impl Parser {
    /// Record an error. The parser says only what went wrong; where it lands
    /// follows from the kind and from where this sits in the event stream.
    pub(crate) fn push_err(&mut self, kind: SyntaxErrKind) {
        self.builder.push_err(kind);
    }

    pub(crate) fn check_node_is_allowed(&mut self, marker: &CompletedMarker, allowed: &[NodeKind]) {
        if !allowed.contains(&marker.kind()) {
            self.push_err(SyntaxErrKind::Unexpected(ChildKind::Node(marker.kind())));
        }
    }

    pub(crate) fn skip(&mut self) {
        if let Some((token, err)) = self.token_stream.next() {
            self.builder.push(token, err);
        }
    }

    pub(crate) fn skip_n(&mut self, n: usize) {
        for _ in 0..n {
            self.skip();
            if self.peek_token().is_eof() {
                break;
            }
        }
    }

    pub(crate) fn token_index(&self) -> usize {
        self.builder.current_token_index()
    }

    pub(crate) fn expect_token(&mut self, kind: TokenKind) {
        if let Some((token, err)) = self.token_stream.next_if(|token| token.kind() == kind) {
            self.builder.push(token, err);
            return;
        }

        self.expect_tokens_recover([kind]);
    }

    pub(crate) fn expect_tokens<const N: usize>(&mut self, kinds: [TokenKind; N]) {
        for kind in kinds {
            self.expect_token(kind)
        }
    }

    pub(crate) fn expect_one_of_tokens<const N: usize>(
        &mut self,
        kinds: [TokenKind; N],
    ) -> Option<TokenKind> {
        for kind in kinds {
            if self.opt_token(kind) {
                return Some(kind);
            }
        }
        self.expect_tokens_recover(kinds);
        None
    }

    pub(crate) fn peek_token(&self) -> TokenKind {
        self.token_stream
            .peek(0)
            .map(|tok| tok.kind())
            .unwrap_or(TokenKind::Eof)
    }

    pub(crate) fn peek_nth_token(&self, n: usize) -> TokenKind {
        self.token_stream
            .peek(n)
            .map(|tok| tok.kind())
            .unwrap_or(TokenKind::Eof)
    }

    pub(crate) fn next_is(&self, kind: TokenKind) -> bool {
        self.peek_token() == kind
    }

    pub(crate) fn next_is_one_of<const N: usize>(&self, kinds: [TokenKind; N]) -> bool {
        kinds.contains(&self.peek_token())
    }

    pub(crate) fn next_nth_is(&self, kind: TokenKind, n: usize) -> bool {
        self.peek_nth_token(n) == kind
    }

    pub(crate) fn expect_kw(&mut self, kind: Keyword) {
        self.expect_token(TokenKind::Keyword(kind))
    }

    pub(crate) fn opt_token(&mut self, kind: TokenKind) -> bool {
        if let Some((token, err)) = self.token_stream.next_if(|token| token.kind() == kind) {
            self.builder.push(token, err);
            true
        } else {
            false
        }
    }

    pub(crate) fn opt_tokens<const N: usize>(
        &mut self,
        kinds: [TokenKind; N],
    ) -> Option<TokenKind> {
        if let Some((token, err)) = self
            .token_stream
            .next_if(|token| kinds.contains(&token.kind()))
        {
            let kind = token.kind();
            self.builder.push(token, err);
            Some(kind)
        } else {
            None
        }
    }

    pub(crate) fn start_node(&mut self, kind: NodeKind) -> Marker {
        let marker = self.builder.start_node(kind);
        self.recovery.push(kind);
        marker
    }

    pub(crate) fn node(
        &mut self,
        kind: NodeKind,
        builder: impl FnOnce(&mut Parser),
    ) -> CompletedMarker {
        let marker = self.start_node(kind);
        builder(self);
        marker.complete(self)
    }

    pub(crate) fn start_unknown(&mut self) -> UnknownMarker {
        self.builder.start_unknown()
    }

    pub(crate) fn end(self) -> (GreenNode, Vec<SyntaxErr>) {
        self.builder.end()
    }

    pub(crate) fn lookahead_max_token_index<const N: usize>(
        &mut self,
        maximum_index: usize,
        kinds: [TokenKind; N],
    ) -> Result<(TokenKind, usize), (LookaheadError, usize)> {
        self.lookahead_max_token_index_skip_n(maximum_index, 0, kinds)
    }

    pub(crate) fn skip_into_node(&mut self, node: NodeKind) -> CompletedMarker {
        self.node(node, Parser::skip)
    }

    pub(crate) fn lookahead_skip_n<const N: usize>(
        &mut self,
        skip_n: usize,
        kinds: [TokenKind; N],
    ) -> Result<(TokenKind, usize), (LookaheadError, usize)> {
        self.lookahead_max_token_index_skip_n(usize::MAX, skip_n, kinds)
    }

    /// Lookahead in the current token stream until one of the given `TokenKind`s are found.
    /// In case of success, the matching `TokenKind` is returned, as well as the token index it was found at.
    /// In case of an error (EOF or if none of the `TokenKind`s was found) the index at which the lookahead ended is
    /// returned as well as the kind of error encountered.
    pub(crate) fn lookahead_max_token_index_skip_n<const N: usize>(
        &mut self,
        maximum_index: usize,
        skip_n: usize,
        kinds: [TokenKind; N],
    ) -> Result<(TokenKind, usize), (LookaheadError, usize)> {
        let mut depth = 0;
        let mut curr_token_index = self.token_index() + skip_n;

        while curr_token_index <= maximum_index && depth >= 0 {
            match self.peek_nth_token(curr_token_index - self.token_index()) {
                TokenKind::LeftPar | TokenKind::LeftSquare => depth += 1,
                tok @ (TokenKind::RightPar | TokenKind::RightSquare) => {
                    // Allow the closing parenthesis (or bracket) to match as well
                    if depth == 0 && kinds.contains(&tok) {
                        return Ok((tok, curr_token_index));
                    }

                    depth -= 1;
                }
                TokenKind::Eof => return Err((LookaheadError::Eof, curr_token_index)),
                tok => {
                    // To avoid matching tokens in some (potentially recursive) sub expression of some sort,
                    // only check the current token if we at the outer most grouping layer (`depth == 0`).
                    if depth == 0 && kinds.contains(&tok) {
                        return Ok((tok, curr_token_index));
                    }
                }
            }
            curr_token_index += 1;
        }

        if curr_token_index > maximum_index {
            Err((LookaheadError::MaxIndexReached, curr_token_index))
        } else {
            Err((LookaheadError::TokenKindNotFound, curr_token_index))
        }
    }
}
