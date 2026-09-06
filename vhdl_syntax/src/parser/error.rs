// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

//! Syntax error definitions and printing
//!
//! As opposed to other implementations, syntax errors are typed, meaning a user can
//! define the rendering and custom message printing based on their preferences.
//!
//! *Note*: The printed error format is `<span> <error message>` where `span` is a byte-span
//! (start to end, both inclusive), not a `line:col` span that one might expect.
//! How to render useful, human-oriented error messages is opinionated and also depends
//! on the encoding of a text editor a user might see.
//! This crate aims to stay unopinonated and therefore only provides utilities like
//! [span to source code mapping](crate::text).
// TODO: Once a linter crate exists, link that here.

use std::error::Error;
use std::fmt::Display;
use std::ops::Range;

use crate::syntax::child::{Child, ChildKind};
use crate::syntax::NodeKind;
use crate::tokens::tokenizer::{LexErr, LexErrKind, LexErrPos, UnterminatedKind};
use crate::tokens::{Token, TokenKind};

/// A span in the source file as range of byte indices
pub type Span = Range<usize>;

/// Syntax error kinds that may occur when parsing a VHDL source file
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SyntaxErrKind {
    /// One of the items was expected, but it is missing from the tree.
    Expected(Child<Box<[NodeKind]>, Box<[TokenKind]>>),
    /// A token was seen that was not expected in some context
    Unexpected(ChildKind),
    /// A token or error that was unterminated
    Unterminated(UnterminatedKind),
}

struct DisplayTokenKind(TokenKind);

impl Display for DisplayTokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0.canonical_text() {
            // `Eof` has a canonical text, but it is empty
            Some(text) if !text.is_empty() => write!(f, "'{text}'"),
            _ => write!(f, "{:?}", self.0),
        }
    }
}

/// Writes the alternatives of an `expected` message as `a, b or c`.
fn write_alternatives<T>(
    f: &mut std::fmt::Formatter<'_>,
    items: &[T],
    mut write_item: impl FnMut(&mut std::fmt::Formatter<'_>, &T) -> std::fmt::Result,
) -> std::fmt::Result {
    for (index, item) in items.iter().enumerate() {
        if index > 0 {
            f.write_str(if index == items.len() - 1 {
                " or "
            } else {
                ", "
            })?;
        }
        write_item(f, item)?;
    }
    Ok(())
}

impl Display for SyntaxErrKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SyntaxErrKind::Expected(child) => {
                write!(f, "expected ")?;
                match child {
                    Child::Node(nodes) => {
                        write_alternatives(f, nodes, |f, node| write!(f, "{node:?}"))
                    }
                    Child::Token(tokens) => write_alternatives(f, tokens, |f, token| {
                        write!(f, "{}", DisplayTokenKind(*token))
                    }),
                }
            }
            SyntaxErrKind::Unexpected(child) => {
                write!(f, "unexpected ")?;
                match child {
                    Child::Node(node) => write!(f, "{node:?}"),
                    Child::Token(token) => write!(f, "{}", DisplayTokenKind(*token)),
                }
            }
            SyntaxErrKind::Unterminated(unterminated_kind) => {
                write!(f, "unterminated {unterminated_kind}")
            }
        }
    }
}

/// Syntax error that may occur when parsing a VHDL source file
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyntaxErr {
    /// The main span of the error.
    span: Span,
    /// the error that occurred
    error: SyntaxErrKind,
}

impl SyntaxErr {
    pub fn new(span: Span, err: SyntaxErrKind) -> SyntaxErr {
        SyntaxErr { span, error: err }
    }

    /// The span where the error occurred
    /// The meaning of this is dependent on the error kind.
    /// For example, when expecting some tokens, this defines the
    /// zero-width insertion point where the token was expected.
    /// For [SyntaxErrKind::Unexpected], this refers to location
    /// of the unexpected token.
    pub fn span(&self) -> &Span {
        &self.span
    }

    /// The error kind that occurred
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
            LexErrKind::IllegalInput => {
                SyntaxErrKind::Unexpected(ChildKind::Token(TokenKind::Unknown))
            }
        };

        SyntaxErr::new(span, kind)
    }
}

impl Display for SyntaxErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}..{} {}",
            self.span().start,
            self.span().end,
            self.err()
        )
    }
}

impl Error for SyntaxErr {}

/// Renders a sequence of syntax errors, one per line
pub fn display_errors<'a>(errors: impl IntoIterator<Item = &'a SyntaxErr>) -> String {
    errors
        .into_iter()
        .map(|err| err.to_string())
        .collect::<Vec<_>>()
        .join("\n")
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
            SyntaxErrKind::Unexpected(ChildKind::Token(TokenKind::Unknown))
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

    #[test]
    fn display_errors_renders_one_error_per_line() {
        assert_eq!(display_errors(&[]), "");
        let errors = [
            SyntaxErr::new(
                4..12,
                SyntaxErrKind::Unterminated(UnterminatedKind::StringLiteral),
            ),
            SyntaxErr::new(
                1..1,
                SyntaxErrKind::Expected(Child::Token(Box::new([TokenKind::Colon]))),
            ),
        ];
        assert_eq!(
            display_errors(&errors),
            "4..12 unterminated string literal\n1..1 expected ':'"
        );
    }

    #[test]
    fn format_error_messages() {
        assert_eq!(
            SyntaxErr::new(
                4..12,
                SyntaxErrKind::Unterminated(UnterminatedKind::StringLiteral)
            )
            .to_string(),
            "4..12 unterminated string literal"
        );
        assert_eq!(
            SyntaxErr::new(
                4..12,
                SyntaxErrKind::Unterminated(UnterminatedKind::BlockComment)
            )
            .to_string(),
            "4..12 unterminated block comment"
        );
        assert_eq!(
            SyntaxErr::new(
                4..12,
                SyntaxErrKind::Unterminated(UnterminatedKind::BasedLiteral)
            )
            .to_string(),
            "4..12 unterminated based literal"
        );
        assert_eq!(
            SyntaxErr::new(
                4..12,
                SyntaxErrKind::Unterminated(UnterminatedKind::ExtendedIdentifier)
            )
            .to_string(),
            "4..12 unterminated extended identifier"
        );

        assert_eq!(
            SyntaxErr::new(
                1..3,
                SyntaxErrKind::Unexpected(Child::Token(TokenKind::Colon))
            )
            .to_string(),
            "1..3 unexpected ':'"
        );
        assert_eq!(
            SyntaxErr::new(
                1..3,
                SyntaxErrKind::Unexpected(Child::Node(NodeKind::Assertion))
            )
            .to_string(),
            "1..3 unexpected Assertion"
        );

        assert_eq!(
            SyntaxErr::new(
                1..1,
                SyntaxErrKind::Expected(Child::Node(Box::new([NodeKind::DesignFile])))
            )
            .to_string(),
            "1..1 expected DesignFile"
        );
        assert_eq!(
            SyntaxErr::new(
                1..1,
                SyntaxErrKind::Expected(Child::Node(Box::new([
                    NodeKind::IfStatement,
                    NodeKind::LoopStatement
                ])))
            )
            .to_string(),
            "1..1 expected IfStatement or LoopStatement"
        );
        assert_eq!(
            SyntaxErr::new(
                1..1,
                SyntaxErrKind::Expected(Child::Node(Box::new([
                    NodeKind::IfStatement,
                    NodeKind::LoopStatement,
                    NodeKind::ReturnStatement
                ])))
            )
            .to_string(),
            "1..1 expected IfStatement, LoopStatement or ReturnStatement"
        );

        assert_eq!(
            SyntaxErr::new(
                1..1,
                SyntaxErrKind::Expected(Child::Token(Box::new([TokenKind::Colon])))
            )
            .to_string(),
            "1..1 expected ':'"
        );
        assert_eq!(
            SyntaxErr::new(
                1..1,
                SyntaxErrKind::Expected(Child::Token(Box::new([
                    TokenKind::Colon,
                    TokenKind::Identifier
                ])))
            )
            .to_string(),
            "1..1 expected ':' or Identifier"
        );
        assert_eq!(
            SyntaxErr::new(
                1..1,
                SyntaxErrKind::Expected(Child::Token(Box::new([
                    TokenKind::Colon,
                    TokenKind::Identifier,
                    TokenKind::Plus
                ])))
            )
            .to_string(),
            "1..1 expected ':', Identifier or '+'"
        );
    }
}
