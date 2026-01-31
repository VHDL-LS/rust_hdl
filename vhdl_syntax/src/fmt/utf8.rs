//! Provides facilities to format nodes and tokens to UTF-8 encoded strings.
//! 
//! # Important considerations
//!
//! ## Latin1 encoding
//!
//! VHDL is Latin-1 encoded while Rusts strings are UTF-8 encoded. While every Latin-1 character
//! can be represented by an equivalent UTF-8 character, the byte-representation is different.
//! For instance, the letter `é` in UTF-8 has the encoding `[0xC3 0xA9]` whereas in Latin-1
//! the encoding is simply `[0xE9]`.
//! As a consequence, the round-trip VHDL-file -> parse -> to string is not guaranteed to yield
//! the same result.
//! 
//! If you want to format to Latin-1, consider using the [fmt::latin1](crate::fmt::latin1) module. 
//!  
//! ## Comment handling
//! 
//! VHDL allows comments to have a different encoding compared to the rest of the file.
//! This implementation assumes `UTF-8` and will replace every invalid UTF-8 sequences
//! with `U+FFFD REPLACEMENT CHARACTER`, which looks like this: �
//! 
//! To circumvent the issues from above, the byte-oriented facilities
//! (e.g., [SyntaxNode::write_to], [Token::write_to]) can be used. These always guarantee that the round-trip
//! VHDL-file -> parse -> write will yield the exact same result.
use std::fmt::{self};

use crate::{
    syntax::{
        child::Child,
        green::{GreenChild, GreenNode, GreenToken},
        node::{SyntaxNode, SyntaxToken},
    },
    tokens::{Token, Trivia, TriviaPiece},
};

// MARK: Trivia

impl fmt::Display for Trivia {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for piece in self.iter() {
            write!(f, "{}", piece)?;
        }
        Ok(())
    }
}

impl fmt::Display for TriviaPiece {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TriviaPiece::HorizontalTabs(n) => write!(f, "{}", "\n".repeat(*n)),
            TriviaPiece::VerticalTabs(n) => write!(f, "{}", "\t".repeat(*n)),
            TriviaPiece::CarriageReturns(n) => write!(f, "{}", "\r".repeat(*n)),
            TriviaPiece::CarriageReturnLineFeeds(n) => write!(f, "{}", "\r\n".repeat(*n)),
            TriviaPiece::LineFeeds(n) => write!(f, "{}", "\n".repeat(*n)),
            TriviaPiece::FormFeeds(n) => write!(f, "{}", "\u{0C}".repeat(*n)),
            TriviaPiece::LineComment(comment) => write!(f, "--{}", comment.to_utf8_lossy()),
            TriviaPiece::BlockComment(comment) => write!(f, "/*{}*/", comment.to_utf8_lossy()),
            TriviaPiece::Spaces(n) => write!(f, "{}", " ".repeat(*n)),
            TriviaPiece::NonBreakingSpaces(n) => write!(f, "{}", "\u{A0}".repeat(*n)),
            TriviaPiece::Unexpected(items) => write!(f, "/*{}*/", String::from_utf8_lossy(items)),
        }
    }
}

// MARK: Token

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.leading_trivia())?;
        write!(f, "{}", self.text())?;
        write!(f, "{}", self.trailing_trivia())?;
        Ok(())
    }
}

impl fmt::Display for GreenToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.token(), f)
    }
}

impl fmt::Display for SyntaxToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.green(), f)
    }
}

// MARK: Child

impl fmt::Display for GreenChild {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Child::Node((_, node)) => write!(f, "{}", node),
            Child::Token((_, token)) => write!(f, "{}", token),
        }
    }
}

// MARK: Node

impl fmt::Display for GreenNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for green_child in self.children() {
            write!(f, "{}", green_child)?;
        }
        Ok(())
    }
}

impl fmt::Display for SyntaxNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.green(), f)
    }
}
