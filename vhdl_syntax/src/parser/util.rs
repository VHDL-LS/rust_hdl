// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com
/// (private) utility functions used when parsing
use crate::parser::builder::Checkpoint;
use crate::parser::error::{SyntaxErr, SyntaxErrKind};
use crate::parser::Parser;
use crate::syntax::green::GreenNode;
use crate::syntax::node_kind::NodeKind;
use crate::tokens::tokenizer::LexErr;
use crate::tokens::{Keyword, Token, TokenKind};

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
#[macro_export]
macro_rules! match_next_token {
    ($parser:expr, $($body:tt)*) => {
        match_next_token!(@inner $parser, [[ $($body)* ]], [[ $($body)* ]])
    };
    (@inner $parser:expr, [[ $($($pattern:pat_param),+ => $action:expr),+ $(,)? ]], [[ $($($pattern_expr:expr),+ => $_action_expr:expr),+ $(,)? ]]) => {
        match $parser.peek_token() {
            $($($pattern)|+ => $action),+,
            _ => {
                let _ = $parser.expect_tokens_recover([$($($pattern_expr),+),+]);
            }
        }
    };
}

/// Allows match-style syntax for tokens.
/// This function consumes the next token, if found.
/// If the token was not seen, or the parser is at EOF, this function pushes an error.
#[macro_export]
macro_rules! match_next_token_consume {
    ($parser:expr, $($body:tt)*) => {
        match_next_token_consume!(@inner $parser, [[ $($body)* ]], [[ $($body)* ]])
    };
    (@inner $parser:expr, [[ $($($pattern:pat_param),+ => $action:expr),+ ]], [[ $($($pattern_expr:expr),+ => $_action_expr:expr),+ ]]) => {
        match $parser.peek_token() {
            $($($pattern)|+ => {
                $parser.skip();
                $action
            }),+
            _ => {
                let _ = $parser.expect_tokens_recover([$($($pattern_expr),+),+]);
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
/// `false` once an entire iteration consumed no input (the parser position did
/// not advance), which means the loop is stalled and must stop. The first call
/// always returns `true` to prime the guard before the loop body has run.
pub(crate) struct StallGuard {
    last_pos: Option<usize>,
}

impl StallGuard {
    pub(crate) fn new() -> StallGuard {
        StallGuard { last_pos: None }
    }

    pub(crate) fn should_continue(&mut self, parser: &mut Parser) -> bool {
        let current_pos = parser.builder.current_pos();
        let Some(last_pos) = self.last_pos else {
            self.last_pos = Some(current_pos);
            return true;
        };
        self.last_pos = Some(current_pos);
        current_pos > last_pos
    }
}

impl Parser {
    fn push_opt_lex_err(&mut self, err: Option<LexErr>, token: &Token, token_start: usize) {
        if let Some(err) = err {
            self.errors
                .push(SyntaxErr::from_lex_err(err, token, token_start));
        }
    }

    pub(crate) fn skip(&mut self) {
        let start = self.builder.current_pos();
        if let Some((token, err)) = self.token_stream.next() {
            self.push_opt_lex_err(err, &token, start);
            self.builder.push(token);
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
        let start = self.builder.current_pos();
        if let Some((token, err)) = self.token_stream.next_if(|token| token.kind() == kind) {
            self.push_opt_lex_err(err, &token, start);
            self.builder.push(token);
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
        self.expect_tokens_err(kinds);
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
        let start = self.builder.current_pos();
        if let Some((token, err)) = self.token_stream.next_if(|token| token.kind() == kind) {
            self.push_opt_lex_err(err, &token, start);
            self.builder.push(token);
            true
        } else {
            false
        }
    }

    pub(crate) fn opt_tokens<const N: usize>(
        &mut self,
        kinds: [TokenKind; N],
    ) -> Option<TokenKind> {
        let start = self.builder.current_pos();
        if let Some((token, err)) = self
            .token_stream
            .next_if(|token| kinds.contains(&token.kind()))
        {
            self.push_opt_lex_err(err, &token, start);
            let kind = token.kind();
            self.builder.push(token);
            Some(kind)
        } else {
            None
        }
    }

    pub(crate) fn start_node(&mut self, kind: NodeKind) {
        self.builder.start_node(kind);
        self.recovery.push(kind);
    }

    pub(crate) fn end_node(&mut self) {
        self.recovery.pop();
        self.builder.end_node()
    }

    pub(crate) fn checkpoint(&mut self) -> Checkpoint {
        self.builder.checkpoint()
    }

    /// Retroactively wrap children from `checkpoint` onward in a node of the
    /// given kind. Pushes the node's FOLLOW set onto the sync stack — the
    /// pre-checkpoint span was already parsed under the parent's recovery
    /// context, which is the right behaviour: until this commit point, the
    /// node's identity was undetermined and it had no FOLLOW to contribute.
    pub(crate) fn start_node_at(&mut self, checkpoint: Checkpoint, kind: NodeKind) {
        self.builder.start_node_at(checkpoint, kind);
        self.recovery.push(kind);
    }

    pub(crate) fn expect_tokens_err(&mut self, tokens: impl Into<Box<[TokenKind]>>) {
        self.errors.push(SyntaxErr::new(
            self.builder.current_pos()..self.builder.current_pos(),
            SyntaxErrKind::Expected(tokens.into()),
        ));
    }

    pub(crate) fn end(self) -> (GreenNode, Vec<SyntaxErr>) {
        (self.builder.end(), self.errors)
    }

    pub(crate) fn lookahead_max_token_index<const N: usize>(
        &mut self,
        maximum_index: usize,
        kinds: [TokenKind; N],
    ) -> Result<(TokenKind, usize), (LookaheadError, usize)> {
        self.lookahead_max_token_index_skip_n(maximum_index, 0, kinds)
    }

    pub(crate) fn skip_into_node(&mut self, node: NodeKind) {
        self.start_node(node);
        self.skip();
        self.end_node();
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
        let mut paren_count = 0;
        let mut curr_token_index = self.token_index() + skip_n;

        while curr_token_index <= maximum_index && paren_count >= 0 {
            match self.peek_nth_token(curr_token_index - self.token_index()) {
                TokenKind::LeftPar => paren_count += 1,
                TokenKind::RightPar => {
                    // Allow the closing parenthesis to match as well
                    if paren_count == 0 && kinds.contains(&TokenKind::RightPar) {
                        return Ok((TokenKind::RightPar, curr_token_index));
                    }

                    paren_count -= 1;
                }
                TokenKind::Eof => return Err((LookaheadError::Eof, curr_token_index)),
                tok => {
                    // To avoid matching tokens in some (potentially recursive) sub expression of some sort,
                    // only check the current token if we at the outer most grouping layer (`paren_count == 0`).
                    if paren_count == 0 && kinds.contains(&tok) {
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
