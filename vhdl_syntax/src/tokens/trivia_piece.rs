// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

use std::fmt::{Display, Formatter};

/// Single trivia pieces that can be combined to form [Trivia](crate::tokens::Trivia) tokens.
#[derive(Clone, Eq, PartialEq, Debug)]
pub enum TriviaPiece {
    /// Horizontal tabs '\t' (a.k.a regular tabs) characters
    HorizontalTabs(usize),
    /// Vertical tabs '\v' characters
    VerticalTabs(usize),
    /// newline '\r' characters
    CarriageReturns(usize),
    /// Carriage return ('\r') + newline ('\n') feeds
    CarriageReturnLineFeeds(usize),
    /// newline '\n' characters
    LineFeeds(usize),
    /// form feed '\f' characters
    FormFeeds(usize),
    /// A line comment starting with a '--'
    LineComment(String),
    /// A block comment starting with a '/*' and ending with a '*/'
    BlockComment(String),
    /// Space ' ' characters
    Spaces(usize),
    /// Non breaking space characters
    NonBreakingSpaces(usize),
    /// Any trivia not covered by the other branches
    Unexpected(String),
}

impl Display for TriviaPiece {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        fn fmt_repeated(f: &mut Formatter<'_>, el: impl Display, count: usize) -> std::fmt::Result {
            for _ in 0..count {
                write!(f, "{el}")?
            }
            Ok(())
        }
        use TriviaPiece::*;
        match self {
            HorizontalTabs(n) => fmt_repeated(f, '\t', *n),
            VerticalTabs(n) => fmt_repeated(f, '\u{b}', *n),
            CarriageReturns(n) => fmt_repeated(f, '\r', *n),
            CarriageReturnLineFeeds(n) => fmt_repeated(f, "\r\n", *n),
            LineFeeds(n) => fmt_repeated(f, '\n', *n),
            FormFeeds(n) => fmt_repeated(f, '\u{c}', *n),
            LineComment(str) => write!(f, "--{str}"),
            BlockComment(str) => write!(f, "/*{str}*/"),
            Spaces(n) => fmt_repeated(f, ' ', *n),
            NonBreakingSpaces(n) => fmt_repeated(f, '\u{a0}', *n),
            Unexpected(str) => write!(f, "{str}"),
        }
    }
}

impl TriviaPiece {
    /// Returns the length of this trivia piece.
    pub fn byte_len(&self) -> usize {
        use TriviaPiece::*;
        match self {
            HorizontalTabs(n) | VerticalTabs(n) | CarriageReturns(n) | LineFeeds(n)
            | FormFeeds(n) | Spaces(n) | NonBreakingSpaces(n) => *n,
            CarriageReturnLineFeeds(n) => *n * 2,
            LineComment(str) => 2 + str.len(),
            BlockComment(str) => 4 + str.len(),
            Unexpected(str) => str.len(),
        }
    }

    /// Returns `true` when this trivia piece represents a whitespace, newline or tab
    pub fn is_whitespace(&self) -> bool {
        self.is_newline() || self.is_space_or_tab()
    }

    /// Returns if this trivia represents a newline.
    pub fn is_newline(&self) -> bool {
        use TriviaPiece::*;
        matches!(
            self,
            CarriageReturns(_)
                | LineFeeds(_)
                | FormFeeds(_)
                | CarriageReturnLineFeeds(_)
                | VerticalTabs(_)
        )
    }

    /// Returns if this piece is a space or tab symbol, excluding vertical tabs
    pub fn is_space_or_tab(&self) -> bool {
        use TriviaPiece::*;
        matches!(self, Spaces(_) | HorizontalTabs(_) | NonBreakingSpaces(_))
    }

    /// Returns if this trivia piece is a block or line comment.
    pub fn is_comment(&self) -> bool {
        use TriviaPiece::*;
        matches!(self, BlockComment(_) | LineComment(_))
    }
}
