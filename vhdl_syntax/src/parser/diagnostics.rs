// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

use std::ops::Range;

use crate::tokens::TokenKind;

type Span = Range<usize>;

/// Errors that occur during parsing.
/// These are not stringified so that downstream listeners
/// can react dynamically without having to parse the message
#[derive(Debug)]
pub enum ParserDiagnostic {
    /// A token was expected, but instead something else was found.
    /// Note: `found` can be TokenKind::EoF
    ExpectedToken {
        expected: (usize, Box<[TokenKind]>),
        found: (Span, TokenKind),
    },
    /// Input that came unexpectedly
    UnexpectedInput { span: Span },
}

impl ParserDiagnostic {
    pub fn eof_err(expected: impl Into<Box<[TokenKind]>>, pos: usize) -> ParserDiagnostic {
        ParserDiagnostic::ExpectedToken {
            expected: (pos, expected.into()),
            found: (pos..pos, TokenKind::Eof),
        }
    }

    pub fn unexpected_input(span: Span) -> ParserDiagnostic {
        ParserDiagnostic::UnexpectedInput { span }
    }

    pub fn missing_token(
        expected: impl Into<Box<[TokenKind]>>,
        insert_pos: usize,
        found: TokenKind,
        found_where: Span,
    ) -> ParserDiagnostic {
        ParserDiagnostic::ExpectedToken {
            expected: (insert_pos, expected.into()),
            found: (found_where, found),
        }
    }
}
