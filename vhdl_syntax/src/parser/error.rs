// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

use std::ops::Range;

use crate::tokens::tokenizer::{LexErr, LexErrKind, LexErrPos, UnterminatedKind};
use crate::tokens::{Token, TokenKind};

pub type Span = Range<usize>;

/// Syntax error kinds that may occur when parsing a VHDL source file
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SyntaxErrKind {
    /// One of the tokens was expected, but it is missing from the tree.
    Expected(Box<[TokenKind]>),
    /// A token was seen that was not expected in some context
    Unexpected(TokenKind),
    /// A token or error that was unterminated
    Unterminated(UnterminatedKind),
}

/// Syntax error that may occur when parsing a VHDL source file
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyntaxErr {
    /// The main span of the error.
    span: Span,
    /// the error that occured
    error: SyntaxErrKind,
}

impl SyntaxErr {
    pub fn new(span: Span, err: SyntaxErrKind) -> SyntaxErr {
        SyntaxErr { span, error: err }
    }

    /// The span where the error occured
    /// The meaning of this is dependent on the error kind.
    /// For example, when expecing some tokens, this defines the
    /// zero-width insertion point where the token was expected.
    /// For [SyntaxErrKind::Unexpected], this refers to location
    /// of the unexpected token.
    pub fn span(&self) -> &Span {
        &self.span
    }

    /// The error kind that occured
    pub fn err(&self) -> &SyntaxErrKind {
        &self.error
    }

    /// Convert a lexer error into a [`SyntaxErr`]
    pub fn from_lex_err(err: LexErr, token: &Token, token_start: usize) -> SyntaxErr {
        let trivia = token.leading_trivia();
        let span = match err.pos {
            LexErrPos::Token => {
                // `token.byte_len()` includes the leading trivia, so use `text_len()`
                // for the width of the token itself.
                let start = token_start + trivia.byte_len();
                start..start + token.text_len()
            }
            LexErrPos::Trivia(index) => {
                // Byte offset of the erroring piece is the sum of all pieces before it.
                let offset: usize = trivia[..index].iter().map(|piece| piece.byte_len()).sum();
                let start = token_start + offset;
                start..start + trivia[index].byte_len()
            }
        };

        let kind = match err.err {
            LexErrKind::Unterminated(kind) => SyntaxErrKind::Unterminated(kind),
            LexErrKind::IllegalInput => SyntaxErrKind::Unexpected(TokenKind::Unknown),
        };

        SyntaxErr::new(span, kind)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokens::tokenizer::Tokenize;

    /// Returns the first token that carries a lexer error, together with that error.
    fn first_lex_err(input: &str) -> (Token, LexErr) {
        input
            .tokenize()
            .find_map(|(tok, err)| err.map(|err| (tok, err)))
            .expect("expected a lexer error")
    }

    #[test]
    fn illegal_input_maps_to_unexpected_unknown_at_token() {
        // Two leading spaces (trivia), then the illegal `$`.
        let (tok, err) = first_lex_err("  $");
        let syntax_err = SyntaxErr::from_lex_err(err, &tok, 0);
        assert_eq!(*syntax_err.span(), 2..3);
        assert_eq!(
            *syntax_err.err(),
            SyntaxErrKind::Unexpected(TokenKind::Unknown)
        );
    }

    #[test]
    fn unterminated_string_maps_at_token() {
        // One leading space, then the unterminated string literal `"abc`.
        let (tok, err) = first_lex_err(" \"abc");
        let syntax_err = SyntaxErr::from_lex_err(err, &tok, 0);
        assert_eq!(*syntax_err.span(), 1..5);
        assert_eq!(
            *syntax_err.err(),
            SyntaxErrKind::Unterminated(UnterminatedKind::StringLiteral)
        );
    }

    #[test]
    fn unterminated_block_comment_span_skips_leading_trivia() {
        // Regression: two spaces precede the unterminated block comment (which is
        // leading trivia of the EOF token), so the span must start at byte 2, not 0.
        let (tok, err) = first_lex_err("  /* unterminated");
        let syntax_err = SyntaxErr::from_lex_err(err, &tok, 0);
        assert_eq!(syntax_err.span().start, 2);
        assert_eq!(
            *syntax_err.err(),
            SyntaxErrKind::Unterminated(UnterminatedKind::BlockComment)
        );
    }

    #[test]
    fn token_start_offset_is_applied() {
        // The same error, but the token does not start at the beginning of the
        // source: the resolved span must be shifted by `token_start`.
        let (tok, err) = first_lex_err(" \"abc");
        let syntax_err = SyntaxErr::from_lex_err(err, &tok, 10);
        assert_eq!(*syntax_err.span(), 11..15);
    }
}
