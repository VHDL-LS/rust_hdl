// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

use crate::latin_1::{Latin1String, Latin1Str};
use crate::tokens::tokenizer::LexDiagnostic;
use crate::tokens::{Token, TokenKind, Tokenizer};
use std::collections::VecDeque;

/// Returns `true` if `text` is a valid VHDL bit-string base specifier.
fn is_base_specifier(text: &Latin1Str) -> bool {
    matches!(
        text.to_lowercase().as_bytes(),
        b"b" | b"o" | b"x" | b"d" | b"sb" | b"ub" | b"so" | b"uo" | b"sx" | b"ux"
    )
}

/// Merge adjacent tokens that form a bit-string literal.
///
/// VHDL bit-string literals have the form `[integer] base_specifier " bit_value "`,
/// e.g., `10ub"0101"` or `x"FF"`. The tokenizer produces these as separate tokens
/// (`AbstractLiteral`, `Identifier`, `StringLiteral`) because resolving the ambiguity
/// at the lexer level causes problems (e.g., `10s` being swallowed instead of being
/// treated as `AbstractLiteral` + `Identifier`).
///
/// This pass recognizes two patterns and merges them into a single `BitStringLiteral` token:
/// - `Identifier StringLiteral` where the identifier is a base specifier (e.g., `b"0101"`)
/// - `AbstractLiteral Identifier StringLiteral` where the identifier is a base specifier
///   (e.g., `10ub"0101"`)
///
/// Merging only occurs when there is no trivia (whitespace/comments) between the tokens.
fn merge_bit_string_literals(
    mut tokens: VecDeque<(Token, Option<LexDiagnostic>)>,
) -> VecDeque<(Token, Option<LexDiagnostic>)> {
    let mut result = VecDeque::with_capacity(tokens.len());

    while let Some((tok, diag)) = tokens.pop_front() {
        match tok.kind() {
            // Pattern: Identifier StringLiteral (e.g., b"0101", sx"FF")
            TokenKind::Identifier if is_base_specifier(tok.text()) => {
                if let Some((next, _)) = tokens.front() {
                    if next.kind() == TokenKind::StringLiteral && next.leading_trivia().is_empty() {
                        let (string_tok, string_diag) = tokens.pop_front().unwrap();
                        let mut merged_text = Latin1String::from(tok.text());
                        merged_text.extend(string_tok.text());
                        let merged = Token::new(
                            TokenKind::BitStringLiteral,
                            merged_text,
                            tok.leading_trivia.clone(),
                        );
                        result.push_back((merged, diag.or(string_diag)));
                        continue;
                    }
                }
                result.push_back((tok, diag));
            }
            // Pattern: AbstractLiteral Identifier StringLiteral (e.g., 10ub"0101")
            TokenKind::AbstractLiteral => {
                if let (Some((ident, _)), Some((string, _))) = (tokens.front(), tokens.get(1)) {
                    if ident.kind() == TokenKind::Identifier
                        && ident.leading_trivia().is_empty()
                        && is_base_specifier(ident.text())
                        && string.kind() == TokenKind::StringLiteral
                        && string.leading_trivia().is_empty()
                    {
                        let (ident_tok, ident_diag) = tokens.pop_front().unwrap();
                        let (string_tok, string_diag) = tokens.pop_front().unwrap();
                        let mut merged_text = Latin1String::from(tok.text());
                        merged_text.extend(ident_tok.text());
                        merged_text.extend(string_tok.text());
                        let merged = Token::new(
                            TokenKind::BitStringLiteral,
                            merged_text,
                            tok.leading_trivia.clone(),
                        );
                        result.push_back((merged, diag.or(ident_diag).or(string_diag)));
                        continue;
                    }
                }
                result.push_back((tok, diag));
            }
            _ => result.push_back((tok, diag)),
        }
    }

    result
}

/// A Token stream is similar to an `Iterator`, however the stream has arbitrary amount of
/// lookahead by using [TokenStream::peek]
pub struct TokenStream {
    // OPTIMIZATION: This is likely inefficient and should be replaced by a more ergonomic implementation.
    inner: VecDeque<(Token, Option<LexDiagnostic>)>,
}

impl TokenStream {
    fn new(tokens: VecDeque<(Token, Option<LexDiagnostic>)>) -> TokenStream {
        TokenStream {
            inner: merge_bit_string_literals(tokens),
        }
    }

    /// Peek `n` tokens in advance where `n == 0` means the next token
    /// (i.e., the token that would be returned by `next`)
    pub fn peek(&self, n: usize) -> Option<&Token> {
        self.inner.get(n).map(|(tok, _)| tok)
    }

    /// Peek the next token.
    pub fn peek_next(&self) -> Option<&Token> {
        self.peek(0)
    }

    /// Returns `true`, when the tokenizer has a next token (i.e., is not at EOF)
    pub fn has_next(&self) -> bool {
        self.peek_next().is_some()
    }

    /// Returns the next token if a given precondition is `true` for the next token.
    /// Returns `None`, if the precondition is `false` or the tokenizer is at EOF
    pub fn next_if(
        &mut self,
        cond: impl FnOnce(&Token) -> bool,
    ) -> Option<(Token, Option<LexDiagnostic>)> {
        if cond(self.peek_next()?) {
            self.next()
        } else {
            None
        }
    }
}

impl Iterator for TokenStream {
    type Item = (Token, Option<LexDiagnostic>);

    /// Consumes and returns the next token.
    /// Returns `None`, if the tokenizer is empty, i.e., there are
    /// no tokens left.
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.pop_front()
    }
}

impl FromIterator<(Token, Option<LexDiagnostic>)> for TokenStream {
    fn from_iter<T: IntoIterator<Item = (Token, Option<LexDiagnostic>)>>(iter: T) -> Self {
        TokenStream::new(iter.into_iter().collect())
    }
}

impl From<Vec<u8>> for TokenStream {
    fn from(value: Vec<u8>) -> Self {
        Tokenizer::new(value.into_iter()).collect()
    }
}

impl From<&[u8]> for TokenStream {
    fn from(value: &[u8]) -> Self {
        Tokenizer::new(value.iter().copied()).collect()
    }
}

impl From<&str> for TokenStream {
    fn from(value: &str) -> Self {
        Tokenizer::new(value.bytes()).collect()
    }
}

impl From<String> for TokenStream {
    fn from(value: String) -> Self {
        TokenStream::from(value.as_str())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn stream_kinds(input: &str) -> Vec<TokenKind> {
        let stream = TokenStream::from(input);
        stream.inner.iter().map(|(tok, _)| tok.kind()).collect()
    }

    fn stream_first_text(input: &str) -> String {
        let stream = TokenStream::from(input);
        stream.inner[0].0.text().to_string()
    }

    #[test]
    fn merge_bare_base_specifier_string() {
        // b"0101" → single BitStringLiteral
        assert_eq!(
            stream_kinds("b\"0101\""),
            vec![TokenKind::BitStringLiteral, TokenKind::Eof]
        );
        assert_eq!(stream_first_text("b\"0101\""), "b\"0101\"");
    }

    #[test]
    fn merge_signed_base_specifier_string() {
        assert_eq!(
            stream_kinds("sx\"FF\""),
            vec![TokenKind::BitStringLiteral, TokenKind::Eof]
        );
        assert_eq!(stream_first_text("sx\"FF\""), "sx\"FF\"");
    }

    #[test]
    fn merge_length_prefixed_bit_string() {
        // 10ub"0101" → single BitStringLiteral
        assert_eq!(
            stream_kinds("10ub\"0101\""),
            vec![TokenKind::BitStringLiteral, TokenKind::Eof]
        );
        assert_eq!(stream_first_text("10ub\"0101\""), "10ub\"0101\"");
    }

    #[test]
    fn no_merge_with_trivia_between() {
        // b "0101" has a space → no merge
        assert_eq!(
            stream_kinds("b \"0101\""),
            vec![
                TokenKind::Identifier,
                TokenKind::StringLiteral,
                TokenKind::Eof
            ]
        );
        // 10 b"0101" has a space before identifier → no merge
        assert_eq!(
            stream_kinds("10 b\"0101\""),
            vec![
                TokenKind::AbstractLiteral,
                TokenKind::BitStringLiteral,
                TokenKind::Eof
            ]
        );
    }

    #[test]
    fn no_merge_non_base_specifier() {
        // foo"bar" — foo is not a base specifier
        assert_eq!(
            stream_kinds("foo\"bar\""),
            vec![
                TokenKind::Identifier,
                TokenKind::StringLiteral,
                TokenKind::Eof
            ]
        );
    }

    #[test]
    fn no_merge_number_followed_by_identifier() {
        // 10s — not followed by a string, no merge
        assert_eq!(
            stream_kinds("10s"),
            vec![
                TokenKind::AbstractLiteral,
                TokenKind::Identifier,
                TokenKind::Eof
            ]
        );
    }

    #[test]
    fn merge_case_insensitive_base_specifier() {
        assert_eq!(
            stream_kinds("X\"FF\""),
            vec![TokenKind::BitStringLiteral, TokenKind::Eof]
        );
        assert_eq!(
            stream_kinds("UB\"01\""),
            vec![TokenKind::BitStringLiteral, TokenKind::Eof]
        );
    }

    #[test]
    fn merge_all_base_specifiers() {
        for base in ["b", "o", "x", "d", "sb", "so", "sx", "ub", "uo", "ux"] {
            let input = format!("{base}\"01\"");
            assert_eq!(
                stream_kinds(&input),
                vec![TokenKind::BitStringLiteral, TokenKind::Eof],
                "failed for base specifier: {base}"
            );

            let upper = input.to_uppercase();
            assert_eq!(
                stream_kinds(&upper),
                vec![TokenKind::BitStringLiteral, TokenKind::Eof],
                "failed for uppercase base specifier: {base}"
            );
        }
    }

    #[test]
    fn merge_all_base_specifiers_with_length() {
        for base in ["b", "o", "x", "d", "sb", "so", "sx", "ub", "uo", "ux"] {
            let input = format!("8{base}\"01\"");
            assert_eq!(
                stream_kinds(&input),
                vec![TokenKind::BitStringLiteral, TokenKind::Eof],
                "failed for length-prefixed base specifier: {base}"
            );
        }
    }
}
