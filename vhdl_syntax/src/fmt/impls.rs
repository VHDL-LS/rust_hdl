//! Implementations for nodes that can write formatted code

use std::{borrow::Cow, fmt, str::Utf8Error};

use crate::{
    fmt::{
        encoding::{Encoder, Latin1Encoder, LossyUtf8Encoder, Utf8Encoder},
        write::{WriteEncoded, WriteError, WriteResult},
    },
    latin_1::Latin1Str,
    syntax::{
        green::{GreenChild, GreenNode, GreenToken},
        node::{SyntaxNode, SyntaxToken},
    },
    tokens::{trivia_piece::Comment, Token, Trivia, TriviaPiece},
};

// MARK: Token

impl WriteEncoded for Token {
    fn fmt_to<E>(&self, writer: &mut impl fmt::Write) -> WriteResult<E::Err>
    where
        E: super::encoding::Encoder,
        for<'a> E::Str<'a>: fmt::Display,
    {
        self.leading_trivia().fmt_to::<E>(writer)?;
        write!(writer, "{}", self.text())?;
        Ok(())
    }
}

// MARK: Trivia

impl WriteEncoded for Trivia {
    fn fmt_to<E>(&self, writer: &mut impl fmt::Write) -> WriteResult<E::Err>
    where
        E: Encoder,
        for<'a> E::Str<'a>: fmt::Display,
    {
        for piece in self.iter() {
            piece.fmt_to::<E>(writer)?;
        }
        Ok(())
    }
}

impl WriteEncoded for TriviaPiece {
    fn fmt_to<E>(&self, writer: &mut impl fmt::Write) -> WriteResult<E::Err>
    where
        E: Encoder,
        for<'a> E::Str<'a>: fmt::Display,
    {
        use TriviaPiece::*;
        match self {
            HorizontalTabs(n) => write!(writer, "{}", "\t".repeat(*n))?,
            VerticalTabs(n) => write!(writer, "{}", "\x0B".repeat(*n))?,
            CarriageReturns(n) => write!(writer, "{}", "\r".repeat(*n))?,
            CarriageReturnLineFeeds(n) => write!(writer, "{}", "\r\n".repeat(*n))?,
            LineFeeds(n) => write!(writer, "{}", "\n".repeat(*n))?,
            FormFeeds(n) => write!(writer, "{}", "\u{0C}".repeat(*n))?,
            LineComment(comment) => {
                write!(writer, "--")?;
                comment.fmt_to::<E>(writer)?;
            }
            BlockComment(comment) => {
                write!(writer, "/*")?;
                comment.fmt_to::<E>(writer)?;
                write!(writer, "*/")?;
            }
            Spaces(n) => write!(writer, "{}", " ".repeat(*n))?,
            NonBreakingSpaces(n) => write!(writer, "{}", "\u{A0}".repeat(*n))?,
        }
        Ok(())
    }
}

impl Comment {
    /// Encode this comment using the specified encoder
    pub fn encode<E: Encoder>(&self) -> Result<E::Str<'_>, E::Err> {
        E::encode(self.as_bytes())
    }

    pub fn as_latin1(&self) -> &Latin1Str {
        self.encode::<Latin1Encoder>().unwrap()
    }

    pub fn as_utf8(&self) -> Result<&str, Utf8Error> {
        self.encode::<Utf8Encoder>()
    }

    pub fn to_utf8_lossy(&self) -> Cow<'_, str> {
        self.encode::<LossyUtf8Encoder>().unwrap()
    }
}

impl WriteEncoded for Comment {
    fn fmt_to<E>(&self, writer: &mut impl fmt::Write) -> WriteResult<E::Err>
    where
        E: Encoder,
        for<'a> E::Str<'a>: fmt::Display,
    {
        match self.encode::<E>() {
            Ok(str) => write!(writer, "{}", str)?,
            Err(e) => return Err(WriteError::Encoding(e)),
        };
        Ok(())
    }
}

// MARK: Green

impl WriteEncoded for GreenToken {
    fn fmt_to<E>(&self, writer: &mut impl fmt::Write) -> WriteResult<E::Err>
    where
        E: super::encoding::Encoder,
        for<'a> E::Str<'a>: fmt::Display,
    {
        self.token().fmt_to::<E>(writer)
    }
}

impl WriteEncoded for GreenNode {
    fn fmt_to<E>(&self, writer: &mut impl fmt::Write) -> WriteResult<E::Err>
    where
        E: Encoder,
        for<'a> E::Str<'a>: fmt::Display,
    {
        for child in self.children() {
            child.fmt_to::<E>(writer)?
        }
        Ok(())
    }
}

impl WriteEncoded for GreenChild {
    fn fmt_to<E>(&self, writer: &mut impl fmt::Write) -> WriteResult<E::Err>
    where
        E: Encoder,
        for<'a> E::Str<'a>: fmt::Display,
    {
        match self {
            GreenChild::Node(node) => node.fmt_to::<E>(writer),
            GreenChild::Token(token) => token.fmt_to::<E>(writer),
        }
    }
}

// MARK: Syntax

impl WriteEncoded for SyntaxToken {
    fn fmt_to<E>(&self, writer: &mut impl fmt::Write) -> WriteResult<E::Err>
    where
        E: super::encoding::Encoder,
        for<'a> E::Str<'a>: fmt::Display,
    {
        self.green().fmt_to::<E>(writer)
    }
}

impl WriteEncoded for SyntaxNode {
    fn fmt_to<E>(&self, writer: &mut impl fmt::Write) -> WriteResult<E::Err>
    where
        E: Encoder,
        for<'a> E::Str<'a>: fmt::Display,
    {
        self.green().fmt_to::<E>(writer)
    }
}
