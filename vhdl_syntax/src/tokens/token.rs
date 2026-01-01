// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

use crate::token_interning::Symbol;
use crate::tokens::{TokenKind, Trivia};
use std::fmt::{Display, Formatter};

/// A source-code token.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Token {
    // TODO: Decide how to deal with trivia. Having two vectors is a bit wasted.
    pub(crate) leading_trivia: Trivia,
    pub(crate) trailing_trivia: Trivia,
    symbol: Symbol,
}

impl Token {
    pub fn new(
        kind: TokenKind,
        text: impl Into<String>,
        leading_trivia: Trivia,
        trailing_trivia: Trivia,
    ) -> Token {
        Token {
            leading_trivia,
            trailing_trivia,
            symbol: Symbol::allocate(kind, text.into()),
        }
    }

    pub fn simple(kind: TokenKind, text: impl Into<String>) -> Token {
        Token::new(kind, text, Trivia::default(), Trivia::default())
    }

    pub fn kind(&self) -> TokenKind {
        self.symbol.kind()
    }

    pub fn leading_trivia(&self) -> &Trivia {
        &self.leading_trivia
    }

    pub fn trailing_trivia(&self) -> &Trivia {
        &self.trailing_trivia
    }

    pub fn text(&self) -> &str {
        self.symbol.text()
    }

    /// The length of the main content of this token in bytes without any trivia
    pub fn text_len(&self) -> usize {
        self.text().len()
    }

    /// The length of this token including trivia
    pub fn byte_len(&self) -> usize {
        self.leading_trivia.byte_len() + self.text_len() + self.trailing_trivia.byte_len()
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for trivia in self.leading_trivia.iter() {
            write!(f, "{trivia}")?;
        }
        write!(f, "{}", self.text())?;
        for trivia in self.trailing_trivia.iter() {
            write!(f, "{trivia}")?;
        }
        Ok(())
    }
}
