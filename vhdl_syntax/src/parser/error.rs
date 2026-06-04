// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

use std::ops::Range;

use crate::tokens::TokenKind;

pub type Span = Range<usize>;

/// Syntax error kinds that may occur when parsing a VHDL source file
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SyntaxErrKind {
    /// One of the tokens was expected, but it is missing from the tree.
    Expected(Box<[TokenKind]>),
    /// A token was seen that was not expected in some context
    Unexpected(TokenKind),
}

/// Syntax error that may occur when parsing a VHDL source file
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyntaxErr {
    /// The main span of the error.
    span: Span,
    /// the error that occured
    error: SyntaxErrKind,
}

impl SyntaxErr {
    pub fn new(span: Span, err: SyntaxErrKind) -> SyntaxErr {
        SyntaxErr { span, error: err }
    }

    /// The span where the error occured
    /// The meaning of this is dependent on the error kind.
    /// For example, when expecing some tokens, this defines the
    /// zero-width insertion point where the token was expected.
    /// For [SyntaxErrKind::Unexpected], this refers to location
    /// of the unexpected token.
    pub fn span(&self) -> &Span {
        &self.span
    }

    /// The error kind that occured
    pub fn err(&self) -> &SyntaxErrKind {
        &self.error
    }
}
