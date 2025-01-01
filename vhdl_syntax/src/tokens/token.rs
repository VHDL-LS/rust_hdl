// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

use crate::tokens::{TokenKind, Trivia};
use std::fmt::{Display, Formatter};

/// A source-code token.
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Token {
    pub(crate) kind: TokenKind,
    pub(crate) leading_trivia: Trivia,
    pub(crate) trailing_trivia: Trivia,
    // TODO: this must be interned (e.g., by arena allocation) to avoid allocating every token
    // on the heap
    pub(crate) text: String,
}

impl Token {
    pub fn simple(kind: TokenKind, text: impl Into<String>) -> Token {
        Token {
            kind,
            text: text.into(),
            leading_trivia: Trivia::default(),
            trailing_trivia: Trivia::default(),
        }
    }

    pub fn kind(&self) -> TokenKind {
        self.kind
    }

    pub fn leading_trivia(&self) -> &Trivia {
        &self.leading_trivia
    }

    pub fn trailing_trivia(&self) -> &Trivia {
        &self.trailing_trivia
    }

    pub fn text(&self) -> &str {
        &self.text
    }

    /// The length of the main content of this token in bytes without any trivia
    pub fn text_len(&self) -> usize {
        self.text.len()
    }

    /// The length of this token including trivia
    pub fn byte_len(&self) -> usize {
        self.leading_trivia.byte_len() + self.text_len() + self.trailing_trivia.byte_len()
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for trivia in &self.leading_trivia {
            write!(f, "{trivia}")?;
        }
        write!(f, "{}", self.text)?;
        for trivia in &self.trailing_trivia {
            write!(f, "{trivia}")?;
        }
        Ok(())
    }
}

/// This macro can be used to easily generate a token from it's literal form:
/// ```
/// use vhdl_syntax::tok;
/// use vhdl_syntax::tokens::{Token, TokenKind};
///
/// assert_eq!(tok![;], Token::simple(TokenKind::SemiColon, ";"));
/// ```
///
/// See also [vhdl!](crate::vhdl) to generate a vector of tokens from their literal form.
#[macro_export]
macro_rules! tok {
    (begin) => {
        $crate::tokens::Token::simple(
            $crate::tokens::TokenKind::Keyword($crate::tokens::Keyword::Begin),
            "begin",
        )
    };
    (end) => {
        $crate::tokens::Token::simple(
            $crate::tokens::TokenKind::Keyword($crate::tokens::Keyword::End),
            "end",
        )
    };
    (entity) => {
        $crate::tokens::Token::simple(
            $crate::tokens::TokenKind::Keyword($crate::tokens::Keyword::Entity),
            "entity",
        )
    };
    (generic) => {
        $crate::tokens::Token::simple(
            $crate::tokens::TokenKind::Keyword($crate::tokens::Keyword::Generic),
            "generic",
        )
    };
    (is) => {
        $crate::tokens::Token::simple(
            $crate::tokens::TokenKind::Keyword($crate::tokens::Keyword::Is),
            "is",
        )
    };
    (;) => {
        $crate::tokens::Token::simple($crate::tokens::TokenKind::SemiColon, ";")
    };
    ($ident:ident) => {
        $crate::tokens::Token::simple($crate::tokens::TokenKind::Identifier, stringify!($ident))
    };
}

#[macro_export]
macro_rules! vhdl {
    ($($tokens:tt)+) => {
        std::collections::VecDeque::from([$($crate::tok![$tokens],)+])
    };
}

#[cfg(test)]
mod tests {
    #[test]
    fn macros() {
        let _ = vhdl! {
            entity my_ent is begin
            end my_ent;
        };
    }
}
