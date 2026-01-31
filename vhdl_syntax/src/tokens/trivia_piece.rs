// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

use std::{borrow::Cow, io::{self, Write}};

use crate::latin_1::Latin1Str;

/// A comment
///
/// Because VHDL allows comments to have any encoding,
/// this implementation makes no assumption as to that and is simply
/// backed by bytes. Utility methods exist to get the value with different
/// encodings.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Comment {
    inner: Vec<u8>,
}

impl Comment {
    pub fn new(bytes: impl Into<Vec<u8>>) -> Comment {
        Comment {
            inner: bytes.into(),
        }
    }

    pub fn as_latin1(&self) -> &Latin1Str {
        Latin1Str::new(&self.inner)
    }

    pub fn as_utf8(&self) -> Result<&str, std::str::Utf8Error> {
        str::from_utf8(&self.inner)
    }

    pub fn to_utf8_lossy(&self) -> Cow<'_, str> {
        String::from_utf8_lossy(&self.inner)
    }

    pub fn as_bytes(&self) -> &[u8] {
        &self.inner
    }

    pub fn byte_len(&self) -> usize {
        self.inner.len()
    }
}

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
    LineComment(Comment),
    /// A block comment starting with a '/*' and ending with a '*/'
    BlockComment(Comment),
    /// Space ' ' characters
    Spaces(usize),
    /// Non breaking space characters
    NonBreakingSpaces(usize),
    /// Any trivia not covered by the other branches
    Unexpected(Vec<u8>),
}

impl TriviaPiece {
    /// Returns the length of this trivia piece.
    pub fn byte_len(&self) -> usize {
        use TriviaPiece::*;
        match self {
            HorizontalTabs(n) | VerticalTabs(n) | CarriageReturns(n) | LineFeeds(n)
            | FormFeeds(n) | Spaces(n) | NonBreakingSpaces(n) => *n,
            CarriageReturnLineFeeds(n) => *n * 2,
            LineComment(str) => 2 + str.byte_len(),
            BlockComment(str) => 4 + str.byte_len(),
            Unexpected(unexpected) => unexpected.len(),
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

    pub fn write_to(&self, writer: &mut impl Write) -> io::Result<()> {
        fn write_repeated(writer: &mut impl Write, el: &[u8], count: usize) -> io::Result<()> {
            for _ in 0..count {
                writer.write_all(el)?;
            }
            Ok(())
        }
        use TriviaPiece::*;
        match self {
            HorizontalTabs(n) => write_repeated(writer, b"\t", *n),
            VerticalTabs(n) => write_repeated(writer, &[0x0Bu8], *n),
            CarriageReturns(n) => write_repeated(writer, b"\r", *n),
            CarriageReturnLineFeeds(n) => write_repeated(writer, b"\r\n", *n),
            LineFeeds(n) => write_repeated(writer, b"\n", *n),
            FormFeeds(n) => write_repeated(writer, &[0x0Cu8], *n),
            LineComment(comment) => {
                writer.write_all(b"--")?;
                writer.write_all(comment.as_bytes())
            }
            BlockComment(comment) => {
                writer.write_all(b"/*")?;
                writer.write_all(comment.as_bytes())?;
                writer.write_all(b"*/")
            }
            Spaces(n) => write_repeated(writer, b" ", *n),
            NonBreakingSpaces(n) => write_repeated(writer, &[0xA0u8], *n),
            Unexpected(unexpected) => writer.write_all(unexpected),
        }
    }
}
