// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::tokenizer::Kind::*;
use super::tokenizer::*;
use crate::ast::Ident;
use crate::data::{DiagnosticHandler, DiagnosticResult, Symbol};

pub struct TokenStream<'a> {
    tokenizer: Tokenizer<'a>,
}

impl<'a> TokenStream<'a> {
    pub fn new(tokenizer: Tokenizer<'a>) -> TokenStream<'a> {
        TokenStream { tokenizer }
    }

    pub fn range_sym(&self) -> &Symbol {
        &self.tokenizer.range_sym()
    }

    pub fn reverse_range_sym(&self) -> &Symbol {
        &self.tokenizer.reverse_range_sym()
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

    pub fn pop(&mut self) -> DiagnosticResult<Option<Token>> {
        self.tokenizer.pop()
    }

    pub fn peek(&mut self) -> DiagnosticResult<Option<Token>> {
        let state = self.tokenizer.state();
        let result = self.tokenizer.pop();
        self.tokenizer.set_state(state);
        result
    }

    pub fn expect(&mut self) -> DiagnosticResult<Token> {
        if let Some(token) = self.pop()? {
            Ok(token)
        } else {
            Err(self.tokenizer.eof_error())
        }
    }

    pub fn expect_kind(&mut self, kind: Kind) -> DiagnosticResult<Token> {
        if let Some(token) = self.pop()? {
            token.expect_kind(kind)
        } else {
            Err(self
                .tokenizer
                .eof_error()
                .when(&format!("expecting {}", kinds_str(&[kind]))))
        }
    }

    pub fn peek_expect(&mut self) -> DiagnosticResult<Token> {
        if let Some(token) = self.peek()? {
            Ok(token)
        } else {
            Err(self.tokenizer.eof_error())
        }
    }

    pub fn pop_kind(&mut self) -> DiagnosticResult<Option<Kind>> {
        let token = self.pop()?;
        Ok(token.map(|ref token| token.kind))
    }

    pub fn peek_kind(&mut self) -> DiagnosticResult<Option<Kind>> {
        Ok(self.peek()?.map(|ref token| token.kind))
    }

    pub fn next_kinds_are(&mut self, kinds: &[Kind]) -> DiagnosticResult<bool> {
        let state = self.state();
        for kind in kinds {
            if self.pop_kind()? != Some(*kind) {
                self.set_state(state);
                return Ok(false);
            }
        }
        self.set_state(state);
        Ok(true)
    }

    pub fn pop_if_kind(&mut self, kind: Kind) -> DiagnosticResult<Option<Token>> {
        if let Some(token) = self.peek()? {
            if token.kind == kind {
                self.move_after(&token);
                return Ok(Some(token));
            }
        }
        Ok(None)
    }

    pub fn skip_if_kind(&mut self, kind: Kind) -> DiagnosticResult<bool> {
        Ok(self.pop_if_kind(kind)?.is_some())
    }

    pub fn skip_until(&mut self, cond: fn(Kind) -> bool) -> DiagnosticResult<()> {
        loop {
            let token = self.peek_expect()?;
            if cond(token.kind) {
                return Ok(());
            }
            self.pop()?;
        }
    }

    pub fn pop_optional_ident(&mut self) -> DiagnosticResult<Option<Ident>> {
        if let Some(token) = self.pop_if_kind(Identifier)? {
            Ok(Some(token.expect_ident()?))
        } else {
            Ok(None)
        }
    }

    pub fn expect_ident(&mut self) -> DiagnosticResult<Ident> {
        let token = self.expect()?;
        token.expect_ident()
    }

    /// Expect identifier or range keyword
    pub fn expect_ident_or_range(&mut self) -> DiagnosticResult<Ident> {
        let token = self.expect()?;
        match_token_kind!(
            token,
            Identifier => token.expect_ident(),
            Range => Ok(Ident {item: self.range_sym().clone(),
                               pos: token.pos})
        )
    }
}

pub trait Recover<T> {
    fn or_recover_until(
        self,
        stream: &mut TokenStream,
        msgs: &mut dyn DiagnosticHandler,
        cond: fn(Kind) -> bool,
    ) -> DiagnosticResult<T>;

    fn log(self, msgs: &mut dyn DiagnosticHandler);
}

impl<T: std::fmt::Debug> Recover<T> for DiagnosticResult<T> {
    fn or_recover_until(
        self,
        stream: &mut TokenStream,
        msgs: &mut dyn DiagnosticHandler,
        cond: fn(Kind) -> bool,
    ) -> DiagnosticResult<T> {
        if self.is_ok() {
            return self;
        }

        let res = stream.skip_until(cond);
        match res {
            Ok(_) => self,
            Err(err) => {
                msgs.push(self.unwrap_err());
                Err(err)
            }
        }
    }

    fn log(self, msgs: &mut dyn DiagnosticHandler) {
        if let Err(err) = self {
            msgs.push(err)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::data::{ContentReader, Diagnostic};
    use crate::syntax::test::Code;

    macro_rules! new_stream {
        ($code:ident, $stream:ident) => {
            let source = $code.source();
            let contents = source.contents();
            let tokenizer = Tokenizer::new(&$code.symbols, source, ContentReader::new(&contents));
            let mut $stream = TokenStream::new(tokenizer);
        };
    }

    #[test]
    fn pop_and_peek() {
        let code = Code::new("hello world again");
        let tokens = code.tokenize();
        new_stream!(code, stream);

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
        let code = Code::new("hello 1 +");
        new_stream!(code, stream);

        assert_eq!(
            stream.next_kinds_are(&[Identifier, AbstractLiteral, Plus]),
            Ok(true)
        );
        assert_eq!(
            stream.next_kinds_are(&[Identifier, AbstractLiteral]),
            Ok(true)
        );
        assert_eq!(stream.next_kinds_are(&[Identifier]), Ok(true));
        assert_eq!(
            stream.next_kinds_are(&[Identifier, AbstractLiteral, AbstractLiteral]),
            Ok(false)
        );
        assert_eq!(stream.next_kinds_are(&[AbstractLiteral]), Ok(false));
    }

    #[test]
    fn expect() {
        let code = Code::new("hello");
        let tokens = code.tokenize();
        new_stream!(code, stream);

        assert_eq!(stream.peek_expect(), Ok(tokens[0].clone()));
        assert_eq!(stream.expect(), Ok(tokens[0].clone()));
        assert_eq!(
            stream.peek_expect(),
            Err(Diagnostic::error(code.eof_pos(), "Unexpected EOF"))
        );
        assert_eq!(
            stream.expect(),
            Err(Diagnostic::error(code.eof_pos(), "Unexpected EOF"))
        );
    }

    #[test]
    fn set_state_taken_before_peek() {
        let code = Code::new("hello world");
        let tokens = code.tokenize();
        new_stream!(code, stream);

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
        let code = Code::new("hello world");
        let tokens = code.tokenize();
        new_stream!(code, stream);

        assert_eq!(stream.peek(), Ok(Some(tokens[0].clone())));
        let state = stream.state();
        stream.set_state(state);
        assert_eq!(stream.pop(), Ok(Some(tokens[0].clone())));
    }

    #[test]
    fn expect_when_eof_empty() {
        let code = Code::new("");
        new_stream!(code, stream);

        assert_eq!(
            stream.expect(),
            Err(Diagnostic::error(code.eof_pos(), "Unexpected EOF"))
        );
    }

    #[test]
    fn expect_eof_after_whitespace() {
        let code = Code::new("a  ");
        new_stream!(code, stream);

        stream.expect().unwrap();
        assert_eq!(
            stream.expect(),
            Err(Diagnostic::error(code.eof_pos(), "Unexpected EOF"))
        );
    }

    #[test]
    fn expect_eof_after_comment() {
        let code = Code::new("a  -- foo");
        new_stream!(code, stream);

        stream.expect().unwrap();
        assert_eq!(
            stream.expect(),
            Err(Diagnostic::error(code.eof_pos(), "Unexpected EOF"))
        );
    }

    #[test]
    fn pop_kind() {
        let code = Code::new("hello world again");
        new_stream!(code, stream);

        assert_eq!(stream.pop_kind(), Ok(Some(Identifier)));
    }

    #[test]
    fn skip_until() {
        let code = Code::new("a begin for + ;");
        new_stream!(code, stream);

        assert!(stream
            .skip_until(|ref k| match k {
                Plus => true,
                _ => false,
            })
            .is_ok());
        assert_eq!(stream.peek().map(|t| t.map(|t| t.kind)), Ok(Some(Plus)));
    }
}
