// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::tokenizer::Kind::*;
use super::tokenizer::*;
use crate::ast::{AttributeDesignator, Ident, RangeAttribute, TypeAttribute};
use crate::data::{DiagnosticHandler, DiagnosticResult, WithPos};
use crate::Diagnostic;

pub struct TokenStream<'a> {
    tokenizer: Tokenizer<'a>,
    idx: usize,
    tokens: Vec<Token>,
}

impl<'a> TokenStream<'a> {
    pub fn new(
        mut tokenizer: Tokenizer<'a>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> TokenStream<'a> {
        let mut tokens = Vec::new();
        loop {
            match tokenizer.pop() {
                Ok(Some(token)) => tokens.push(token),
                Ok(None) => break,
                Err(err) => diagnostics.push(err),
            }
        }
        TokenStream {
            tokenizer,
            idx: 0,
            tokens,
        }
    }

    pub fn state(&self) -> usize {
        self.idx
    }

    pub fn set_state(&mut self, state: usize) {
        self.idx = state;
    }

    pub fn move_after(&mut self, token: &Token) {
        self.idx = token.idx + 1;
    }

    pub fn pop(&mut self) -> Option<Token> {
        if let Some(token) = self.tokens.get(self.idx) {
            self.idx += 1;
            Some(token.clone())
        } else {
            None
        }
    }

    pub fn peek(&mut self) -> Option<Token> {
        self.tokens.get(self.idx).cloned()
    }

    fn eof_error(&self) -> Diagnostic {
        let end = self.tokenizer.source.contents().end();
        Diagnostic::error(
            self.tokenizer.source.pos(end, end.next_char()),
            "Unexpected EOF",
        )
    }

    pub fn expect(&mut self) -> DiagnosticResult<Token> {
        if let Some(token) = self.pop() {
            Ok(token)
        } else {
            Err(self.eof_error())
        }
    }

    pub fn expect_kind(&mut self, kind: Kind) -> DiagnosticResult<Token> {
        if let Some(token) = self.peek() {
            if token.kind == kind {
                self.move_after(&token);
                Ok(token)
            } else {
                Err(token.kinds_error_before(&[kind]))
            }
        } else {
            Err(self
                .eof_error()
                .when(format!("expecting {}", kinds_str(&[kind]))))
        }
    }

    pub fn peek_expect(&mut self) -> DiagnosticResult<Token> {
        if let Some(token) = self.peek() {
            Ok(token)
        } else {
            Err(self.eof_error())
        }
    }

    pub fn pop_kind(&mut self) -> Option<Kind> {
        let token = self.pop();
        token.map(|ref token| token.kind)
    }

    pub fn peek_kind(&mut self) -> Option<Kind> {
        self.peek().map(|ref token| token.kind)
    }

    pub fn next_kinds_are(&mut self, kinds: &[Kind]) -> bool {
        let state = self.state();
        for kind in kinds {
            if self.pop_kind() != Some(*kind) {
                self.set_state(state);
                return false;
            }
        }
        self.set_state(state);
        true
    }

    pub fn pop_if_kind(&mut self, kind: Kind) -> Option<Token> {
        if let Some(token) = self.peek() {
            if token.kind == kind {
                self.move_after(&token);
                return Some(token);
            }
        }
        None
    }

    pub fn skip_if_kind(&mut self, kind: Kind) -> bool {
        self.pop_if_kind(kind).is_some()
    }

    pub fn skip_until(&mut self, cond: fn(Kind) -> bool) -> DiagnosticResult<()> {
        loop {
            let token = self.peek_expect()?;
            if cond(token.kind) {
                return Ok(());
            }
            self.pop();
        }
    }

    pub fn pop_optional_ident(&mut self) -> Option<Ident> {
        self.pop_if_kind(Identifier)
            .map(|token| token.expect_ident().unwrap())
    }

    pub fn expect_ident(&mut self) -> DiagnosticResult<Ident> {
        let token = self.expect()?;
        token.expect_ident()
    }

    /// Expect identifier or subtype/range keywords
    /// foo'subtype or foo'range
    pub fn expect_attribute_designator(
        &mut self,
    ) -> DiagnosticResult<WithPos<AttributeDesignator>> {
        let token = self.expect()?;
        let des = try_token_kind!(
            token,
            Identifier => {
                let ident = token.expect_ident()?;
                ident.map_into(|sym| self.tokenizer.attribute(sym))
            },
            Subtype => WithPos::new(AttributeDesignator::Type(TypeAttribute::Subtype), token.pos),
            Range => WithPos::new(AttributeDesignator::Range(RangeAttribute::Range), token.pos)
        );
        Ok(des)
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
    use crate::data::{ContentReader, Diagnostic, NoDiagnostics};
    use crate::syntax::test::Code;

    macro_rules! new_stream {
        ($code:ident, $stream:ident) => {
            let source = $code.source();
            let contents = source.contents();
            let tokenizer = Tokenizer::new(&$code.symbols, source, ContentReader::new(&contents));
            let mut $stream = TokenStream::new(tokenizer, &mut NoDiagnostics);
        };
    }

    #[test]
    fn pop_and_peek() {
        let code = Code::new("hello world again");
        let tokens = code.tokenize();
        new_stream!(code, stream);

        assert_eq!(stream.pop(), Some(tokens[0].clone()));
        assert_eq!(stream.peek(), Some(tokens[1].clone()));
        assert_eq!(stream.pop(), Some(tokens[1].clone()));
        assert_eq!(stream.peek(), Some(tokens[2].clone()));
        assert_eq!(stream.pop(), Some(tokens[2].clone()));
        assert_eq!(stream.peek(), None);
        assert_eq!(stream.pop(), None);
        assert_eq!(stream.peek(), None);
        assert_eq!(stream.pop(), None);
        assert_eq!(stream.pop(), None);
    }

    #[test]
    fn is_peek_kinds() {
        let code = Code::new("hello 1 +");
        new_stream!(code, stream);

        assert!(stream.next_kinds_are(&[Identifier, AbstractLiteral, Plus]),);
        assert!(stream.next_kinds_are(&[Identifier, AbstractLiteral]));
        assert!(stream.next_kinds_are(&[Identifier]));
        assert!(!stream.next_kinds_are(&[Identifier, AbstractLiteral, AbstractLiteral]),);
        assert!(!stream.next_kinds_are(&[AbstractLiteral]));
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
        assert_eq!(stream.peek(), Some(tokens[0].clone()));
        assert_eq!(stream.pop(), Some(tokens[0].clone()));
        assert_eq!(stream.peek(), Some(tokens[1].clone()));
        stream.set_state(state);
        assert_eq!(stream.peek(), Some(tokens[0].clone()));
        assert_eq!(stream.pop(), Some(tokens[0].clone()));
    }

    #[test]
    fn set_state_taken_after_peek() {
        let code = Code::new("hello world");
        let tokens = code.tokenize();
        new_stream!(code, stream);

        assert_eq!(stream.peek(), Some(tokens[0].clone()));
        let state = stream.state();
        stream.set_state(state);
        assert_eq!(stream.pop(), Some(tokens[0].clone()));
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

        assert_eq!(stream.pop_kind(), Some(Identifier));
    }

    #[test]
    fn skip_until() {
        let code = Code::new("a begin for + ;");
        new_stream!(code, stream);

        assert!(stream.skip_until(|ref k| matches!(k, Plus)).is_ok());
        assert_eq!(stream.peek().map(|t| t.kind), Some(Plus));
    }
}
