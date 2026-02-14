//! Provides facilities to format nodes and tokens to Latin-1 encoded strings.
//!
//! # Important considerations
//!
//! ## Comment handling
//!
//! VHDL allows comments to have a different encoding compared to the rest of the file.
//! This implementation assumes `Latin-1` where each character is valid, however the
//! symbol displayed to the user might differ if a different encoding (i.e., UTF-8) is used.
//!
//! To circumvent the issue from above, the byte-oriented facilities
//! (e.g., [SyntaxNode::write_to], [Token::write_to]) can be used. These always guarantee that the round-trip
//! VHDL-file -> parse -> write will yield the exact same result.

// MARK: Trivia

use crate::{
    latin_1::{Latin1String, ToLatin1},
    syntax::{
        child::Child,
        green::{GreenChild, GreenNode, GreenToken},
        node::{SyntaxNode, SyntaxToken},
    },
    tokens::{Token, Trivia, TriviaPiece},
};

impl ToLatin1 for Trivia {
    fn to_latin1(&self) -> crate::latin_1::Latin1String {
        // We need at a minimum
        let mut buf = Latin1String::with_capacity(self.len());
        for piece in self.iter() {
            buf.append(&mut piece.to_latin1());
        }
        buf
    }
}

impl ToLatin1 for TriviaPiece {
    fn to_latin1(&self) -> Latin1String {
        match self {
            TriviaPiece::HorizontalTabs(n) => Latin1String::from(b"\n".repeat(*n)),
            TriviaPiece::VerticalTabs(n) => Latin1String::from(b"\t".repeat(*n)),
            TriviaPiece::CarriageReturns(n) => Latin1String::from(b"\r".repeat(*n)),
            TriviaPiece::CarriageReturnLineFeeds(n) => Latin1String::from(b"\r\n".repeat(*n)),
            TriviaPiece::LineFeeds(n) => Latin1String::from(b"\n".repeat(*n)),
            TriviaPiece::FormFeeds(n) => Latin1String::from([0x0C].repeat(*n)),
            TriviaPiece::LineComment(comment) => {
                let mut buf = Latin1String::from(b"--");
                buf.extend(comment.as_bytes());
                buf
            }
            TriviaPiece::BlockComment(comment) => {
                let mut buf = Latin1String::from(b"/*");
                buf.extend(comment.as_bytes());
                buf.extend(b"*/");
                buf
            }
            TriviaPiece::Spaces(n) => Latin1String::from(b" ".repeat(*n)),
            TriviaPiece::NonBreakingSpaces(n) => Latin1String::from([0x0A].repeat(*n)),
            TriviaPiece::Unexpected(items) => Latin1String::from(items),
        }
    }
}

// MARK: Token

impl ToLatin1 for Token {
    fn to_latin1(&self) -> Latin1String {
        let mut buf = Latin1String::new();
        buf.append(&mut self.leading_trivia().to_latin1());
        buf.extend(self.text());
        buf
    }
}

impl ToLatin1 for GreenToken {
    fn to_latin1(&self) -> Latin1String {
        self.token().to_latin1()
    }
}

impl ToLatin1 for SyntaxToken {
    fn to_latin1(&self) -> Latin1String {
        self.green().to_latin1()
    }
}

// MARK: Child

impl ToLatin1 for GreenChild {
    fn to_latin1(&self) -> Latin1String {
        match self {
            Child::Node((_, node)) => node.to_latin1(),
            Child::Token((_, token)) => token.to_latin1(),
        }
    }
}

// MARK: Node

impl ToLatin1 for GreenNode {
    fn to_latin1(&self) -> Latin1String {
        let mut buf = Latin1String::with_capacity(self.byte_len());
        for green_child in self.children() {
            buf.append(&mut green_child.to_latin1());
        }
        buf
    }
}

impl ToLatin1 for SyntaxNode {
    fn to_latin1(&self) -> Latin1String {
        self.green().to_latin1()
    }
}
