// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use crate::ast::Ident;
use crate::latin_1::Latin1String;
use crate::message::{MessageHandler, ParseResult};
use crate::source::SrcPos;
use crate::tokenizer::{kinds_str, Kind, Kind::*, Token, TokenState, Tokenizer, Value};

#[derive(PartialEq, Clone, Debug)]
pub struct WithComment<T> {
    pub item: T,
    pub comment: Option<Latin1String>,
}

pub fn combine_comments(
    leading_comments: Vec<Latin1String>,
    trailing_comment: Option<Latin1String>,
    unhandled_comments: Option<Vec<(SrcPos, Latin1String)>>,
    messages: &mut dyn MessageHandler,
) -> Option<Latin1String> {
    let mut comment = Latin1String::empty();
    for leading_comment in leading_comments {
        if comment.len() > 0 {
            comment.push_newline();
        }
        comment.push(&leading_comment);
    }
    match trailing_comment {
        None => {}
        Some(trailing) => {
            if comment.len() > 0 {
                comment.push_newline();
            }
            comment.push(&trailing);
        }
    }
    match unhandled_comments {
        None => {}
        Some(comments) => {
            // FIXME: Some handling should be done here to create warning messages
            // about the unhandled comments.
            // Requires modifying tests that expect no messages.
        }
    }
    if comment.len() == 0 {
        None
    } else {
        Some(comment)
    }
}

pub struct TokenStream {
    pub tokenizer: Tokenizer,
    unhandled_comments: Option<Vec<(SrcPos, Latin1String)>>,
}

impl TokenStream {
    pub fn new(tokenizer: Tokenizer) -> TokenStream {
        TokenStream {
            tokenizer,
            unhandled_comments: None,
        }
    }

    pub fn state(&self) -> TokenState {
        self.tokenizer.state()
    }

    pub fn set_state(&mut self, state: TokenState) {
        self.tokenizer.set_state(state);
        match &mut self.unhandled_comments {
            None => {}
            Some(comments) => loop {
                match comments.last() {
                    None => break,
                    Some((comment_pos, _)) => {
                        if comment_pos.start >= state.start() {
                            comments.pop();
                        } else {
                            break;
                        }
                    }
                }
            },
        }
    }

    pub fn move_after(&mut self, token: &Token) {
        let current_pos = self.state().start();
        let target_pos = token.pos.start + token.pos.length;
        if current_pos > target_pos {
            panic!("Cannot move use move_after to move backwards.");
        } else if current_pos < target_pos {
            loop {
                // We assume that we won't get any tokenizing errors since we've
                // already tokenized this.
                let maybe_token = self.pop().unwrap();
                match maybe_token {
                    None => panic!("Unexpected end of file."),
                    Some(popped_token) => {
                        if popped_token.pos.start >= token.pos.start {
                            break;
                        }
                    }
                }
            }
        }
    }

    pub fn pop(&mut self) -> ParseResult<Option<Token>> {
        loop {
            let maybe_token = self.tokenizer.pop()?;
            match maybe_token {
                None => {
                    return Ok(None);
                }
                Some(token) => {
                    match (&token.kind, &token.value) {
                        (LeadingComment, Value::String(comment))
                        | (TrailingComment, Value::String(comment)) => {
                            match &mut self.unhandled_comments {
                                None => {
                                    self.unhandled_comments =
                                        Some(vec![(token.pos, comment.clone())]);
                                }
                                Some(comments) => {
                                    comments.push((token.pos, comment.clone()));
                                }
                            };
                        }
                        _ => {
                            return Ok(Some(token));
                        }
                    };
                }
            }
        }
    }

    pub fn peek(self: &mut Self) -> ParseResult<Option<Token>> {
        let state = self.state();
        let token = self.pop();
        self.set_state(state);
        token
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
            token.expect_kind(kind)
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

    pub fn next_kinds_are(self: &mut Self, kinds: &[Kind]) -> ParseResult<bool> {
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

    pub fn pop_if_kind(self: &mut Self, kind: Kind) -> ParseResult<Option<Token>> {
        if let Some(token) = self.peek()? {
            if token.kind == kind {
                self.move_after(&token);
                return Ok(Some(token));
            }
        }
        Ok(None)
    }

    pub fn skip_if_kind(self: &mut Self, kind: Kind) -> ParseResult<bool> {
        Ok(self.pop_if_kind(kind)?.is_some())
    }

    pub fn skip_until(&mut self, cond: fn(&Kind) -> bool) -> ParseResult<()> {
        loop {
            let token = self.peek_expect()?;
            if cond(&token.kind) {
                return Ok(());
            }
            self.pop()?;
        }
    }

    pub fn pop_optional_ident(&mut self) -> ParseResult<Option<Ident>> {
        if let Some(token) = self.pop_if_kind(Identifier)? {
            Ok(Some(token.expect_ident()?))
        } else {
            Ok(None)
        }
    }

    pub fn expect_ident(&mut self) -> ParseResult<Ident> {
        let token = self.expect()?;
        token.expect_ident()
    }

    /// Expect identifier or range keyword
    pub fn expect_ident_or_range(&mut self) -> ParseResult<Ident> {
        let token = self.expect()?;
        match_token_kind!(
            token,
            Identifier => token.expect_ident(),
            Range => Ok(Ident {item: self.tokenizer.range_ident.clone(),
                               pos: token.pos})
        )
    }

    pub fn leading_comments(&mut self) -> ParseResult<Vec<Latin1String>> {
        let mut comments = vec![];
        loop {
            let state = self.state();
            let maybe_token = self.tokenizer.pop()?;
            match maybe_token {
                None => {
                    return Ok(comments);
                }
                Some(token) => match (token.kind, token.value) {
                    (LeadingComment, Value::String(comment)) => {
                        comments.push(comment);
                    }
                    _ => {
                        self.set_state(state);
                        return Ok(comments);
                    }
                },
            }
        }
    }

    pub fn trailing_comment(&mut self) -> ParseResult<Option<Latin1String>> {
        let state = self.state();
        let maybe_token = self.tokenizer.pop()?;
        match maybe_token {
            None => Ok(None),
            Some(token) => match (token.kind, token.value) {
                (TrailingComment, Value::String(comment)) => Ok(Some(comment)),
                _ => {
                    self.set_state(state);
                    Ok(None)
                }
            },
        }
    }

    pub fn unhandled_comments(&mut self) -> Option<Vec<(SrcPos, Latin1String)>> {
        self.unhandled_comments.take()
    }

    pub fn assert_no_unhandled_comments(self: &mut Self) {
        if self.unhandled_comments.is_some() {
            panic!("Some comments were not handled.");
        }
    }
}

pub trait Recover<T> {
    fn or_recover_until(
        self,
        stream: &mut TokenStream,
        msgs: &mut MessageHandler,
        cond: fn(&Kind) -> bool,
    ) -> ParseResult<T>;

    fn log(self, msgs: &mut MessageHandler);
}

impl<T: std::fmt::Debug> Recover<T> for ParseResult<T> {
    fn or_recover_until(
        self,
        stream: &mut TokenStream,
        msgs: &mut MessageHandler,
        cond: fn(&Kind) -> bool,
    ) -> ParseResult<T> {
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

    fn log(self, msgs: &mut MessageHandler) {
        match self {
            Err(err) => msgs.push(err),
            Ok(_) => (),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::message::Message;
    use crate::source::Source;
    use crate::tokenizer::tokenize;

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
        let (source, tokens, mut stream) = new("hello");
        assert_eq!(stream.peek_expect(), Ok(tokens[0].clone()));
        let token = stream.pop();
        assert_eq!(
            stream.peek_expect(),
            Err(Message::error(&source.pos(5, 1), "Unexpected EOF"))
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
            Err(Message::error(&source.pos(0, 1), "Unexpected EOF"))
        );
    }

    #[test]
    fn expect_eof_after_whitespace() {
        let (source, _, mut stream) = new("a  ");

        stream.expect().unwrap();
        assert_eq!(
            stream.expect(),
            Err(Message::error(&source.pos(3, 1), "Unexpected EOF"))
        );
    }

    #[test]
    fn expect_eof_after_comment() {
        let (source, _, mut stream) = new("a  -- foo");

        stream.expect().unwrap();
        assert_eq!(
            stream.expect(),
            Err(Message::error(&source.pos(9, 1), "Unexpected EOF"))
        );
    }

    #[test]
    fn pop_kind() {
        let (_, _, mut stream) = new("hello world again");

        assert_eq!(stream.pop_kind(), Ok(Some(Identifier)));
    }

    #[test]
    fn skip_until() {
        let (_, _, mut stream) = new("a begin for + ;");
        assert!(stream
            .skip_until(|ref k| match k {
                Plus => true,
                _ => false,
            })
            .is_ok());
        assert_eq!(stream.peek().map(|t| t.map(|t| t.kind)), Ok(Some(Plus)));
    }

    #[test]
    fn leading_and_trailing_comments() {
        let (src, tokens, mut stream) = new("--a leading comment
-- another leading comment
hello world -- a comment that we won't handle
-- another comment we won't handle
again --a trailing comment");
        // Peek at the first non-comment token.
        assert_eq!(stream.peek(), Ok(Some(tokens[2].clone())));
        assert_eq!(
            stream.leading_comments(),
            Ok(vec![
                Latin1String::from_utf8_unchecked("a leading comment"),
                Latin1String::from_utf8_unchecked(" another leading comment"),
            ])
        );
        // Pop the first non-comment token off.
        assert_eq!(stream.pop(), Ok(Some(tokens[2].clone())));
        // There are no comments between "hello" and "world"
        assert_eq!(stream.leading_comments(), Ok(vec![]));
        assert_eq!(stream.trailing_comment(), Ok(None));
        assert_eq!(stream.pop(), Ok(Some(tokens[3].clone())));
        assert_eq!(stream.pop(), Ok(Some(tokens[6].clone())));
        assert_eq!(stream.leading_comments(), Ok(vec![]));
        assert_eq!(
            stream.trailing_comment(),
            Ok(Some(Latin1String::from_utf8_unchecked(
                "a trailing comment"
            )))
        );
        assert_eq!(stream.peek(), Ok(None));
        assert_eq!(stream.pop(), Ok(None));
        assert_eq!(stream.peek(), Ok(None));
        assert_eq!(stream.pop(), Ok(None));
        assert_eq!(stream.pop(), Ok(None));
        assert_eq!(
            stream.unhandled_comments(),
            Some(vec![
                (
                    src.first_substr_pos("-- a comment that we won't handle"),
                    Latin1String::from_utf8_unchecked(" a comment that we won't handle")
                ),
                (
                    src.first_substr_pos("-- another comment we won't handle"),
                    Latin1String::from_utf8_unchecked(" another comment we won't handle")
                ),
            ])
        );
    }
}
