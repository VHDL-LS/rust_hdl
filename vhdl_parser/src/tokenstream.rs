// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use ast::Ident;
use message::ParseResult;
use tokenizer::{kinds_str, Kind, Kind::*, Token, TokenState, Tokenizer};

pub struct TokenStream {
    pub tokenizer: Tokenizer,
}

impl TokenStream {
    pub fn new(tokenizer: Tokenizer) -> TokenStream {
        TokenStream { tokenizer }
    }

    pub fn state(&self) -> TokenState {
        self.tokenizer.state()
    }

    pub fn set_state(&mut self, state: TokenState) {
        self.tokenizer.set_state(state);
    }

    pub fn move_after(&mut self, token: &Token) {
        self.tokenizer.move_after(token);
    }

    pub fn pop(self: &mut Self) -> ParseResult<Option<Token>> {
        self.tokenizer.pop()
    }

    pub fn peek(self: &mut Self) -> ParseResult<Option<Token>> {
        let state = self.tokenizer.state();
        let result = self.tokenizer.pop();
        self.tokenizer.set_state(state);
        result
    }

    pub fn expect(self: &mut Self) -> ParseResult<Token> {
        if let Some(token) = self.pop()? {
            Ok(token)
        } else {
            Err(self.tokenizer.eof_error())
        }
    }

    pub fn expect_kind(self: &mut Self, kind: Kind) -> ParseResult<Token> {
        if let Some(token) = self.pop()? {
            if token.kind == kind {
                Ok(token)
            } else {
                Err(token.kinds_error(&[kind]))
            }
        } else {
            Err(self
                .tokenizer
                .eof_error()
                .when(&format!("expecting {}", kinds_str(&[kind]))))
        }
    }

    pub fn peek_expect(self: &mut Self) -> ParseResult<Token> {
        if let Some(token) = self.peek()? {
            Ok(token)
        } else {
            Err(self.tokenizer.eof_error())
        }
    }

    pub fn pop_kind(self: &mut Self) -> ParseResult<Option<Kind>> {
        let token = self.pop()?;
        Ok(token.map(|ref token| token.kind))
    }

    pub fn peek_kind(self: &mut Self) -> ParseResult<Option<Kind>> {
        Ok(self.peek()?.map(|ref token| token.kind))
    }

    pub fn is_peek_kinds(self: &mut Self, kinds: &[Kind]) -> ParseResult<bool> {
        let state = self.state();
        for kind in kinds {
            if self.pop_kind()? != Some(*kind) {
                self.set_state(state);
                return Ok(false);
            }
        }
        self.set_state(state);
        return Ok(true);
    }

    pub fn pop_if_kind(self: &mut Self, kind: Kind) -> ParseResult<Option<Token>> {
        if let Some(token) = self.peek()? {
            if token.kind == kind {
                self.move_after(&token);
                return Ok(Some(token));
            }
        }
        return Ok(None);
    }

    pub fn skip_if_kind(self: &mut Self, kind: Kind) -> ParseResult<bool> {
        Ok(self.pop_if_kind(kind)?.is_some())
    }

    pub fn pop_optional_ident(&mut self) -> ParseResult<Option<Ident>> {
        if let Some(token) = self.pop_if_kind(Identifier)? {
            Ok(Some(token.expect_ident()?))
        } else {
            Ok(None)
        }
    }

    pub fn expect_ident(self: &mut Self) -> ParseResult<Ident> {
        let token = self.expect()?;
        token.expect_ident()
    }

    /// Expect identifier or range keyword
    pub fn expect_ident_or_range(self: &mut Self) -> ParseResult<Ident> {
        let token = self.expect()?;
        match_token_kind!(
            token,
            Identifier => token.expect_ident(),
            Range => Ok(Ident {item: self.tokenizer.range_ident.clone(),
                               pos: token.pos})
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use message::error;
    use source::Source;
    use tokenizer::tokenize;

    fn new(code: &str) -> (Source, Vec<Token>, TokenStream) {
        let (source, symtab, tokens) = tokenize(code);
        let tokenizer = Tokenizer::new(symtab, source.clone(), source.contents().unwrap());
        let stream = TokenStream::new(tokenizer);
        (source, tokens, stream)
    }

    #[test]
    fn pop_and_peek() {
        let (_, tokens, mut stream) = new("hello world again");

        assert_eq!(stream.pop(), Ok(Some(tokens[0].clone())));
        assert_eq!(stream.peek(), Ok(Some(tokens[1].clone())));
        assert_eq!(stream.pop(), Ok(Some(tokens[1].clone())));
        assert_eq!(stream.peek(), Ok(Some(tokens[2].clone())));
        assert_eq!(stream.pop(), Ok(Some(tokens[2].clone())));
        assert_eq!(stream.peek(), Ok(None));
        assert_eq!(stream.pop(), Ok(None));
        assert_eq!(stream.peek(), Ok(None));
        assert_eq!(stream.pop(), Ok(None));
        assert_eq!(stream.pop(), Ok(None));
    }

    #[test]
    fn is_peek_kinds() {
        let (_, _, mut stream) = new("hello 1 +");

        assert_eq!(
            stream.is_peek_kinds(&[Identifier, AbstractLiteral, Plus]),
            Ok(true)
        );
        assert_eq!(
            stream.is_peek_kinds(&[Identifier, AbstractLiteral]),
            Ok(true)
        );
        assert_eq!(stream.is_peek_kinds(&[Identifier]), Ok(true));
        assert_eq!(
            stream.is_peek_kinds(&[Identifier, AbstractLiteral, AbstractLiteral]),
            Ok(false)
        );
        assert_eq!(stream.is_peek_kinds(&[AbstractLiteral]), Ok(false));
    }

    #[test]
    fn expect() {
        let (source, tokens, mut stream) = new("hello");

        assert_eq!(stream.peek_expect(), Ok(tokens[0].clone()));
        assert_eq!(stream.expect(), Ok(tokens[0].clone()));
        assert_eq!(
            stream.peek_expect(),
            Err(error(&source.pos(5, 1), "Unexpected EOF"))
        );
        assert_eq!(
            stream.expect(),
            Err(error(&source.pos(5, 1), "Unexpected EOF"))
        );
    }

    #[test]
    fn set_state_taken_before_peek() {
        let (_, tokens, mut stream) = new("hello world");

        let state = stream.state();
        assert_eq!(stream.peek(), Ok(Some(tokens[0].clone())));
        assert_eq!(stream.pop(), Ok(Some(tokens[0].clone())));
        assert_eq!(stream.peek(), Ok(Some(tokens[1].clone())));
        stream.set_state(state);
        assert_eq!(stream.peek(), Ok(Some(tokens[0].clone())));
        assert_eq!(stream.pop(), Ok(Some(tokens[0].clone())));
    }

    #[test]
    fn set_state_taken_after_peek() {
        let (_, tokens, mut stream) = new("hello world");

        assert_eq!(stream.peek(), Ok(Some(tokens[0].clone())));
        let state = stream.state();
        stream.set_state(state);
        assert_eq!(stream.pop(), Ok(Some(tokens[0].clone())));
    }

    #[test]
    fn expect_when_eof_empty() {
        let (source, _, mut stream) = new("");

        assert_eq!(
            stream.expect(),
            Err(error(&source.pos(0, 1), "Unexpected EOF"))
        );
    }

    #[test]
    fn expect_eof_after_whitespace() {
        let (source, _, mut stream) = new("a  ");

        stream.expect().unwrap();
        assert_eq!(
            stream.expect(),
            Err(error(&source.pos(3, 1), "Unexpected EOF"))
        );
    }

    #[test]
    fn expect_eof_after_comment() {
        let (source, _, mut stream) = new("a  -- foo");

        stream.expect().unwrap();
        assert_eq!(
            stream.expect(),
            Err(error(&source.pos(9, 1), "Unexpected EOF"))
        );
    }

    #[test]
    fn pop_kind() {
        let (_, _, mut stream) = new("hello world again");

        assert_eq!(stream.pop_kind(), Ok(Some(Identifier)));
    }
}
