// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

use crate::tokens::Token;
use std::collections::VecDeque;

/// A Token stream is similar to an `Iterator`, however the stream has arbitrary amount of
/// lookahead by using [TokenStream::peek]
pub trait TokenStream {
    /// Consumes and returns the next token.
    /// Returns `None`, if the tokenizer is empty, i.e., there are
    /// no tokens left.
    fn next(&mut self) -> Option<Token>;

    /// Peek `n` tokens in advance where `n == 0` means the next token
    /// (i.e., the token that would be returned by `next`)
    fn peek(&self, n: usize) -> Option<&Token>;

    /// Peek the next token.
    fn peek_next(&self) -> Option<&Token> {
        self.peek(0)
    }

    /// Returns `true`, when the tokenizer has a next token (i.e., is not at EOF)
    fn has_next(&self) -> bool {
        self.peek_next().is_some()
    }

    /// Returns the next token if a given precondition is `true` for the next token.
    /// Returns `None`, if the precondition is `false` or the tokenizer is at EOF
    fn next_if(&mut self, cond: impl FnOnce(&Token) -> bool) -> Option<Token> {
        if cond(self.peek_next()?) {
            self.next()
        } else {
            None
        }
    }
}

// Various implementations of Token Streams

// A `VecDequeue` is a Token Stream because one can pop elements
// and peek from the front.
// For a vector, this is not as simple and therefore the wrapper
// `VecTokenStream` exists.
impl TokenStream for VecDeque<Token> {
    fn next(&mut self) -> Option<Token> {
        self.pop_front()
    }

    fn peek(&self, n: usize) -> Option<&Token> {
        self.get(n)
    }
}

/// A [TokenStream] that wraps a vector of tokens.
pub struct VecTokenStream(Vec<Token>);

impl VecTokenStream {
    /// Creates a token stream by reversing the provided iterable of tokens.
    pub fn new<T: IntoIterator<Item = Token>>(tokens: T) -> VecTokenStream
    where
        T::IntoIter: DoubleEndedIterator,
    {
        VecTokenStream(tokens.into_iter().rev().collect())
    }
}

impl TokenStream for VecTokenStream {
    fn next(&mut self) -> Option<Token> {
        self.0.pop()
    }

    fn peek(&self, n: usize) -> Option<&Token> {
        if n == 0 {
            self.0.last()
        } else {
            self.0.get(self.0.len().checked_sub(n - 1)?)
        }
    }
}

/// Syntactic sugar to transform tokenizers (i.e., producers of tokens such as the
/// struct [Tokenizer], but also plain iterables of tokens) into token streams.
/// # Examples
/// ```
/// use vhdl_syntax::tokens::{IntoTokenStream, Keyword, TokenKind, TokenStream};
/// use vhdl_syntax::tokens::tokenizer::Tokenize;
///
/// let mut token_stream = "entity foo".tokenize().into_token_stream();
///
/// assert_eq!(token_stream.peek(1).unwrap().kind(), TokenKind::Identifier);
///
/// assert_eq!(token_stream.next().unwrap().kind(), TokenKind::Keyword(Keyword::Entity));
/// assert_eq!(token_stream.next().unwrap().kind(), TokenKind::Identifier);
/// assert_eq!(token_stream.next(), None);
/// ```
pub trait IntoTokenStream {
    fn into_token_stream(self) -> impl TokenStream;
}

impl<I> IntoTokenStream for I
where
    I: Iterator<Item = Token>,
{
    fn into_token_stream(self) -> impl TokenStream {
        VecDeque::from_iter(self)
    }
}
