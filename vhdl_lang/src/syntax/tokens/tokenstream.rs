// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::tokenizer::Kind::*;
use super::tokenizer::*;
use crate::ast::{AttributeDesignator, Ident};
use crate::data::{DiagnosticHandler, DiagnosticResult, WithPos};
use crate::{Diagnostic, SrcPos};

/// A TokenStream is an immutable collection of tokens with mutable state.
/// The state points to a certain token. Methods exist to mutate that state and seek
/// to another token, e.g. to skip a token or go to a previous one.
pub trait TokenStream {
    /// Returns the current state.
    /// `token_at(self.state())` must always return the current token, or `None` if the
    /// state is invalid
    fn state(&self) -> usize;

    /// Sets the current state.
    /// An invalid state is OK but `token_at(invalid_state)` will return `None`
    fn set_state(&self, state: usize);

    /// Returns the token at an given state
    fn token_at(&self, state: usize) -> Option<&Token>;

    /// Returns the state of a token
    fn state_of(&self, token: &Token) -> Option<usize>;

    // Default implementations for common operations

    /// Skips the current token, i.e. increments the current state
    fn skip(&self) {
        self.set_state(self.state() + 1)
    }

    /// Returns the token at the current state
    fn peek(&self) -> Option<&Token> {
        self.token_at(self.state())
    }

    /// Returns the token kind at the current state
    fn peek_kind(&self) -> Option<Kind> {
        self.peek().map(|token| token.kind)
    }

    /// returns the token at the previous state
    fn last(&self) -> Option<&Token> {
        let state = self.state().checked_sub(1)?;
        self.token_at(state)
    }

    /// Returns `true`, when the nth next token is of kind `kind`
    fn nth_kind_is(&self, offset: usize, kind: Kind) -> bool {
        if let Some(token) = self.token_at(self.state() + offset) {
            token.kind == kind
        } else {
            false
        }
    }

    /// returns true, when the next token is of a certain kind
    fn next_kind_is(&self, kind: Kind) -> bool {
        self.nth_kind_is(0, kind)
    }

    /// returns true when all next tokens are of a certain kind
    fn next_kinds_are(&self, kinds: &[Kind]) -> bool {
        kinds
            .iter()
            .enumerate()
            .all(|(idx, kind)| self.nth_kind_is(idx, *kind))
    }

    /// advances (i.e. skips) if the next token is of a particular kind and returns the token
    fn pop_if_kind(&self, kind: Kind) -> Option<&Token> {
        if let Some(token) = self.peek() {
            if token.kind == kind {
                self.skip();
                return Some(token);
            }
        }
        None
    }

    /// advances (i.e. skips) if the next token is of a particular kind and returns true if successfully
    fn skip_if_kind(&self, kind: Kind) -> bool {
        self.pop_if_kind(kind).is_some()
    }

    /// skips if the current token is an identifier and returns that identifier, if found
    fn pop_optional_ident(&self) -> Option<Ident> {
        self.pop_if_kind(Identifier)
            .map(|token| token.to_identifier_value().unwrap())
    }

    /// Returns the token right before the given token
    ///
    /// Example:
    ///  entity my_entity is
    ///         ^         ^
    ///         |         | token 'is'
    ///         token_before('is')
    fn token_before(&self, token: &Token) -> Option<&Token> {
        let state = self.state_of(token)?;
        self.token_at(state.wrapping_sub(1))
    }

    /// A position that aligns with the previous token
    ///
    /// Example:
    ///  signal sig : natural
    ///                      ~ <- want semi colon error here
    ///  signal
    ///  ~~~~~~ <- not here
    fn pos_before(&self, token: &Token) -> SrcPos {
        if let Some(prev_token) = self.token_before(token) {
            let prev_pos = prev_token.pos.end();

            if prev_pos.line != token.pos.range.start.line {
                return prev_token.pos.pos_at_end();
            }
        }

        token.pos.clone()
    }

    // Diagnostic stuff

    fn handle_diagnostic(&self, exception: TokenizationException) -> Diagnostic;

    // TODO: this should not be an anonymous function (it's too specific)
    fn expect_attribute_designator(&self) -> DiagnosticResult<WithPos<AttributeDesignator>>;

    fn expect_kind(&self, kind: Kind) -> DiagnosticResult<&Token> {
        if let Some(token) = self.peek() {
            if token.kind == kind {
                self.skip();
                Ok(token)
            } else {
                Err(self.handle_diagnostic(TokenizationException::KindsError {
                    pos: self.pos_before(token),
                    kinds: &[kind],
                }))
            }
        } else {
            Err(self.handle_diagnostic(TokenizationException::EofError {
                expectations: Some(&[kind]),
            }))
        }
    }

    fn peek_expect(&self) -> DiagnosticResult<&Token> {
        if let Some(token) = self.peek() {
            Ok(token)
        } else {
            Err(self.handle_diagnostic(TokenizationException::EofError { expectations: None }))
        }
    }

    fn expect_ident(&self) -> DiagnosticResult<Ident> {
        expect_token!(self, token, Identifier => token.to_identifier_value())
    }

    fn skip_until(&self, cond: fn(Kind) -> bool) -> DiagnosticResult<()> {
        loop {
            let token = self.peek_expect()?;
            if cond(token.kind) {
                return Ok(());
            }
            self.skip();
        }
    }
}

pub enum TokenizationException<'a> {
    KindsError { pos: SrcPos, kinds: &'a [Kind] },
    EofError { expectations: Option<&'a [Kind]> },
}

pub trait Recover<T> {
    fn or_recover_until(
        self,
        stream: &dyn TokenStream,
        msgs: &mut dyn DiagnosticHandler,
        cond: fn(Kind) -> bool,
    ) -> DiagnosticResult<T>;

    fn log(self, msgs: &mut dyn DiagnosticHandler);
}

impl<T: std::fmt::Debug> Recover<T> for DiagnosticResult<T> {
    fn or_recover_until(
        self,
        stream: &dyn TokenStream,
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
