// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

use crate::tokens::TokenKind;

/// Errors that occur during parsing.
/// These are not stringified so that downstream listeners
/// can react dynamically without having to parse the message
#[derive(Debug)]
pub enum ParserError {
    /// One of the provided token kinds was expected
    ExpectingTokens(Box<[TokenKind]>),
    /// An unexpected End-of file was detected
    Eof,
    /// A lookahead could not find the provided token
    LookaheadFailed(TokenKind),
}

#[derive(Debug)]
pub struct ParserDiagnostic {
    pub text_pos: usize,
    pub error: ParserError,
}

impl ParserDiagnostic {
    pub fn new(text_pos: usize, error: ParserError) -> ParserDiagnostic {
        ParserDiagnostic { text_pos, error }
    }
}
