// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use std::cell::Cell;
use vhdl_lang::syntax::parser::ParsingContext;

use super::tokenizer::Kind::*;
use super::tokenizer::*;
use crate::ast::token_range::WithToken;
use crate::ast::{AttributeDesignator, Ident, RangeAttribute, TypeAttribute};
use crate::data::{DiagnosticHandler, DiagnosticResult};
use crate::{Diagnostic, SrcPos};

pub struct TokenStream<'a> {
    tokenizer: Tokenizer<'a>,
    idx: Cell<usize>,
    tokens: Vec<Token>,
    // This is the offset that a token's ID should be adapted
    // when getting it via `TokenStream::get_current_token_id()`
    // It is updated in the `slice_tokens` method
    token_offset: Cell<usize>,
}

impl<'a> TokenStream<'a> {
    /// Special handling for a tool directive of the form
    /// ```vhdl
    /// `identifier { any chars until newline }
    /// ```
    /// This needs special handling as the text that follows the identifier is arbitrary.
    fn handle_tool_directive(
        grave_accent: Token,
        tokenizer: &mut Tokenizer<'_>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) {
        let start_pos = grave_accent.pos.clone();
        match tokenizer.pop() {
            Ok(Some(tok)) => {
                if tok.kind != Identifier {
                    diagnostics.push(Diagnostic::syntax_error(tok, "Expecting identifier"));
                    let _ = tokenizer.text_until_newline(); // skip potentially invalid tokens
                    return;
                }
            }
            Err(err) => diagnostics.push(err),
            Ok(None) => {
                diagnostics.push(Diagnostic::syntax_error(start_pos, "Expecting identifier"));
                return;
            }
        }
        match tokenizer.text_until_newline() {
            Ok(_) => {}
            Err(err) => diagnostics.push(err),
        }
    }

    pub fn new(
        mut tokenizer: Tokenizer<'a>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> TokenStream<'a> {
        let mut tokens = Vec::new();
        loop {
            match tokenizer.pop() {
                Ok(Some(token)) if token.kind == GraveAccent => {
                    TokenStream::handle_tool_directive(token, &mut tokenizer, diagnostics)
                }
                Ok(Some(token)) => tokens.push(token),
                Ok(None) => break,
                Err(err) => diagnostics.push(err),
            }
        }
        TokenStream {
            tokenizer,
            idx: Cell::new(0),
            tokens,
            token_offset: Cell::new(0),
        }
    }

    pub fn state(&self) -> usize {
        self.get_idx()
    }

    pub fn set_state(&self, state: usize) {
        self.set_idx(state);
    }

    pub fn skip(&self) {
        self.set_idx(self.get_idx() + 1)
    }

    pub fn back(&self) {
        self.set_idx(self.get_idx() - 1)
    }

    fn get_idx(&self) -> usize {
        self.idx.get()
    }

    fn set_idx(&self, idx: usize) {
        self.idx.replace(idx);
    }

    pub fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.get_idx())
    }

    pub fn get_current_token_id(&self) -> TokenId {
        TokenId::new(self.get_idx() - self.token_offset.get())
    }

    pub fn get_last_token_id(&self) -> TokenId {
        TokenId::new(self.get_idx() - 1 - self.token_offset.get())
    }

    fn eof_error(&self) -> Diagnostic {
        let end = self.tokenizer.source.contents().end();
        Diagnostic::syntax_error(
            self.tokenizer.source.pos(end, end.next_char()),
            "Unexpected EOF",
        )
    }

    fn idx_of(&self, token: &Token) -> Option<usize> {
        let base = self.tokens.as_ptr() as usize;
        let ptr = (token as *const Token) as usize;
        let idx = ptr.checked_sub(base)? / std::mem::size_of::<Token>();

        if idx < self.tokens.len() {
            Some(idx)
        } else {
            None
        }
    }

    fn token_before(&self, token: &Token) -> Option<&Token> {
        let idx = self.idx_of(token)?;
        self.tokens.get(idx.wrapping_sub(1))
    }

    /// A position that aligns with the previous token
    ///
    /// Example:
    /// ```vhdl
    ///  signal sig : natural
    ///                      ~ <- want semi colon error here
    ///  signal
    ///  ~~~~~~ <- not here
    /// ```
    pub fn pos_before(&self, token: &Token) -> SrcPos {
        if let Some(prev_token) = self.token_before(token) {
            let prev_pos = prev_token.pos.end();

            if prev_pos.line != token.pos.range.start.line {
                return prev_token.pos.pos_at_end();
            }
        }

        token.pos.clone()
    }

    pub fn expect_kind(&self, kind: Kind) -> DiagnosticResult<TokenId> {
        if let Some(token) = self.peek() {
            if token.kind == kind {
                let id = self.get_current_token_id();
                self.skip();
                Ok(id)
            } else {
                Err(kinds_error(self.pos_before(token), &[kind]))
            }
        } else {
            Err(self
                .eof_error()
                .when(format!("expecting {}", kinds_str(&[kind]))))
        }
    }

    pub fn peek_expect(&self) -> DiagnosticResult<&Token> {
        if let Some(token) = self.peek() {
            Ok(token)
        } else {
            Err(self.eof_error())
        }
    }

    pub fn peek_kind(&self) -> Option<Kind> {
        self.peek().map(|token| token.kind)
    }

    pub fn next_kind_is(&self, kind: Kind) -> bool {
        self.nth_kind_is(0, kind)
    }

    pub fn nth_kind_is(&self, idx: usize, kind: Kind) -> bool {
        if let Some(token) = self.tokens.get(self.get_idx() + idx) {
            token.kind == kind
        } else {
            false
        }
    }

    pub fn next_kinds_are(&self, kinds: &[Kind]) -> bool {
        kinds
            .iter()
            .enumerate()
            .all(|(idx, kind)| self.nth_kind_is(idx, *kind))
    }

    pub fn pop_if_kind(&self, kind: Kind) -> Option<TokenId> {
        if let Some(token) = self.peek() {
            if token.kind == kind {
                let id = self.get_current_token_id();
                self.skip();
                return Some(id);
            }
        }
        None
    }

    pub fn skip_if_kind(&self, kind: Kind) -> bool {
        self.pop_if_kind(kind).is_some()
    }

    pub fn skip_until<F>(&self, cond: F) -> DiagnosticResult<()>
    where
        F: Fn(Kind) -> bool,
    {
        loop {
            let token = self.peek_expect()?;
            if cond(token.kind) {
                return Ok(());
            }
            self.skip();
        }
    }

    pub fn pop_optional_ident(&self) -> Option<Ident> {
        self.pop_if_kind(Identifier)
            .map(|id| self.index(id).to_identifier_value(id).unwrap())
    }

    pub fn expect_ident(&self) -> DiagnosticResult<Ident> {
        expect_token!(self, token, token_id, Identifier => token.to_identifier_value(token_id))
    }

    /// Expect identifier or subtype/range keywords
    /// foo'subtype or foo'range
    pub fn expect_attribute_designator(&self) -> DiagnosticResult<WithToken<AttributeDesignator>> {
        let des = expect_token!(
            self,
            token,
            token_id,
            Identifier => {
                let ident = token.to_identifier_value(token_id)?;
                ident.map_into(|sym| self.tokenizer.attribute(sym))
            },
            Subtype => WithToken::new(AttributeDesignator::Type(TypeAttribute::Subtype), token_id),
            Range => WithToken::new(AttributeDesignator::Range(RangeAttribute::Range), token_id)
        );
        Ok(des)
    }

    /// Slices the tokens until the current position.
    /// The token at the current position is not included.
    /// Subsequent calls to `slice_tokens` will start from the current position.
    ///
    /// Note that The function `TokenStream::get_token(TokenId)` only returns a token
    /// for `TokenId`s that are obtained after `slice_tokens` was called.
    ///
    /// # Example
    ///
    /// ```vhdl
    /// 1 2 abc; tok x
    ///          ^
    ///          current position
    /// ```
    /// After calling `slice_tokens`, the returned vec is `[1, 2, abc, ;]`.
    ///
    /// ```vhdl
    /// 1 2 abc; tok x
    ///                ^
    ///                current position (EOF)
    /// ```
    /// After calling `slice_tokens` again, the returned vec is `[tok x]`
    pub fn slice_tokens(&self) -> Vec<Token> {
        let vec = Vec::from(&self.tokens[self.token_offset.get()..self.state()]);
        self.token_offset.replace(self.state());
        vec
    }
}

impl TokenAccess for TokenStream<'_> {
    fn get_token(&self, id: TokenId) -> Option<&Token> {
        self.tokens[self.token_offset.get()..].get_token(id)
    }

    fn index(&self, id: TokenId) -> &Token {
        self.tokens[self.token_offset.get()..].index(id)
    }

    fn get_token_slice(&self, start_id: TokenId, end_id: TokenId) -> &[Token] {
        self.tokens[self.token_offset.get()..].get_token_slice(start_id, end_id)
    }
}

pub trait Recover<T> {
    fn or_recover_until<F>(self, ctx: &mut ParsingContext<'_>, cond: F) -> DiagnosticResult<T>
    where
        F: Fn(Kind) -> bool;
}

impl<T> Recover<T> for DiagnosticResult<T> {
    fn or_recover_until<F>(self, ctx: &mut ParsingContext<'_>, cond: F) -> DiagnosticResult<T>
    where
        F: Fn(Kind) -> bool,
    {
        match self {
            Ok(res) => Ok(res),
            Err(ref original_err) => {
                let res = ctx.stream.skip_until(cond);
                match res {
                    Ok(_) => self,
                    Err(err) => {
                        ctx.diagnostics.push(original_err.clone());
                        Err(err)
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::data::{ContentReader, Diagnostic, NoDiagnostics};
    use crate::syntax::test::Code;
    use itertools::Itertools;

    macro_rules! new_stream {
        ($code:ident, $stream:ident) => {
            let source = $code.source();
            let contents = source.contents();
            let tokenizer = Tokenizer::new(&$code.symbols, source, ContentReader::new(&contents));
            let $stream = TokenStream::new(tokenizer, &mut NoDiagnostics);
        };
        ($code:ident, $stream:ident, $diagnostics:ident) => {
            let source = $code.source();
            let contents = source.contents();
            let tokenizer = Tokenizer::new(&$code.symbols, source, ContentReader::new(&contents));
            let $stream = TokenStream::new(tokenizer, &mut $diagnostics);
        };
    }

    #[test]
    fn pop_and_peek() {
        let code = Code::new("hello world again");
        let tokens = code.tokenize();
        new_stream!(code, stream);

        stream.skip();
        assert_eq!(stream.peek(), Some(&tokens[1]));
        stream.skip();
        assert_eq!(stream.peek(), Some(&tokens[2]));
        stream.skip();
        assert_eq!(stream.peek(), None);
        stream.skip();
        assert_eq!(stream.peek(), None);
    }

    #[test]
    fn idx_of() {
        let code = Code::new("hello world again");
        new_stream!(code, stream);

        let mut idx = 0;
        while let Some(token) = stream.peek() {
            assert_eq!(idx, stream.idx_of(token).unwrap());
            idx += 1;
            stream.skip();
        }
    }

    #[test]
    fn prev_token() {
        let code = Code::new("hello world again");
        new_stream!(code, stream);

        let mut prev = None;
        while let Some(token) = stream.peek() {
            if let Some(prev) = prev {
                assert_eq!(prev, stream.token_before(token).unwrap());
            }
            prev = Some(token);
            stream.skip();
        }
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

        assert_eq!(stream.peek_expect(), Ok(&tokens[0]));
        stream.skip();
        assert_eq!(
            stream.peek_expect(),
            Err(Diagnostic::syntax_error(code.eof_pos(), "Unexpected EOF"))
        );
    }

    #[test]
    fn set_state_taken_before_peek() {
        let code = Code::new("hello world");
        let tokens = code.tokenize();
        new_stream!(code, stream);

        let state = stream.state();
        assert_eq!(stream.peek(), Some(&tokens[0]));
        stream.skip();
        assert_eq!(stream.peek(), Some(&tokens[1]));
        stream.set_state(state);
        assert_eq!(stream.peek(), Some(&tokens[0]));
    }

    #[test]
    fn expect_when_eof_empty() {
        let code = Code::new("");
        new_stream!(code, stream);

        assert_eq!(
            stream.peek_expect(),
            Err(Diagnostic::syntax_error(code.eof_pos(), "Unexpected EOF"))
        );
    }

    #[test]
    fn expect_eof_after_whitespace() {
        let code = Code::new("a  ");
        new_stream!(code, stream);

        stream.skip();
        assert_eq!(
            stream.peek_expect(),
            Err(Diagnostic::syntax_error(code.eof_pos(), "Unexpected EOF"))
        );
    }

    #[test]
    fn expect_eof_after_comment() {
        let code = Code::new("a  -- foo");
        new_stream!(code, stream);

        stream.skip();
        assert_eq!(
            stream.peek_expect(),
            Err(Diagnostic::syntax_error(code.eof_pos(), "Unexpected EOF"))
        );
    }

    #[test]
    fn skip_until() {
        let code = Code::new("a begin for + ;");
        new_stream!(code, stream);

        assert!(stream.skip_until(|ref k| matches!(k, Plus)).is_ok());
        assert_eq!(stream.peek().map(|t| t.kind), Some(Plus));
    }

    #[test]
    fn tokenize_simple_identifier_directive() {
        let code = Code::new("`protect begin");
        new_stream!(code, _stream);
    }

    #[test]
    fn tokenize_extended_identifier_directive() {
        let code = Code::new("`\\extended ident\\ begin other words");
        new_stream!(code, _stream);
    }

    #[test]
    fn tokenize_directive_illegal_identifier() {
        let code = Code::new("`123 begin other words");
        let mut diagnostics: Vec<Diagnostic> = vec![];
        new_stream!(code, _stream, diagnostics);
        assert_eq!(
            diagnostics,
            vec![Diagnostic::syntax_error(
                code.s1("123"),
                "Expecting identifier"
            )]
        )
    }

    #[test]
    fn tokenize_directive_then_end_of_stream() {
        let code = Code::new("`");
        let mut diagnostics: Vec<Diagnostic> = vec![];
        new_stream!(code, _stream, diagnostics);
        assert_eq!(
            diagnostics,
            vec![Diagnostic::syntax_error(
                code.s1("`"),
                "Expecting identifier"
            )]
        )
    }

    #[test]
    fn pop_tokens() {
        let code = Code::new(
            "\
entity my_ent is
end entity my_ent;

architecture arch of my_ent is
end arch;
        ",
        );
        new_stream!(code, stream);
        stream.skip_until(|it| it == SemiColon).expect("");
        stream.skip();
        assert_eq!(
            stream.slice_tokens().iter().map(|it| it.kind).collect_vec(),
            vec![Entity, Identifier, Is, End, Entity, Identifier, SemiColon]
        );
        stream.skip_until(|it| it == SemiColon).expect("");
        stream.skip();
        assert_eq!(
            stream.slice_tokens().iter().map(|it| it.kind).collect_vec(),
            vec![
                Architecture,
                Identifier,
                Of,
                Identifier,
                Is,
                End,
                Identifier,
                SemiColon
            ]
        );
    }

    #[test]
    fn indexing_tokens_after_slicing() {
        let code = Code::new("1 2 abc; () +");
        new_stream!(code, stream);
        let tokens = code.tokenize();
        assert_eq!(
            tokens[0],
            stream.index(stream.get_current_token_id()).clone()
        );
        stream.skip();
        assert_eq!(
            tokens[1],
            stream.index(stream.get_current_token_id()).clone()
        );
        stream
            .skip_until(|kind| kind == SemiColon)
            .expect("Unexpected EOF");
        stream.slice_tokens();
        assert_eq!(
            tokens[3],
            stream.index(stream.get_current_token_id()).clone()
        );
        stream.skip();
        assert_eq!(
            tokens[4],
            stream.index(stream.get_current_token_id()).clone()
        );
    }
}
