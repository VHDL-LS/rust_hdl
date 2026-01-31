// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

use crate::tokens::{Token, Tokenizer};
use std::collections::VecDeque;

/// A Token stream is similar to an `Iterator`, however the stream has arbitrary amount of
/// lookahead by using [TokenStream::peek]
pub struct TokenStream {
    // OPTIMIZATION: This is likely inefficient and should be replaced by a more ergonomic implementation.
    inner: VecDeque<Token>,
}

impl TokenStream {
    fn new(tokens: VecDeque<Token>) -> TokenStream {
        TokenStream { inner: tokens }
    }

    /// Peek `n` tokens in advance where `n == 0` means the next token
    /// (i.e., the token that would be returned by `next`)
    pub fn peek(&self, n: usize) -> Option<&Token> {
        self.inner.get(n)
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
    pub fn next_if(&mut self, cond: impl FnOnce(&Token) -> bool) -> Option<Token> {
        if cond(self.peek_next()?) {
            self.next()
        } else {
            None
        }
    }
}

impl Iterator for TokenStream {
    type Item = Token;

    /// Consumes and returns the next token.
    /// Returns `None`, if the tokenizer is empty, i.e., there are
    /// no tokens left.
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.pop_front()
    }
}

impl FromIterator<Token> for TokenStream {
    fn from_iter<T: IntoIterator<Item = Token>>(iter: T) -> Self {
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
