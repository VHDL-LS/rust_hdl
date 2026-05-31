// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

use std::ops::Range;

use crate::{
    text::source_loc::{EncodedSpan, SourceLocConverter},
    tokens::{tokenizer::UnterminatedKind, TokenKind},
};

type Span = Range<usize>;

#[derive(Debug, Clone)]
pub enum SyntaxErr {
    /// One of the tokens given by `kinds` was expected. `found` is the token
    /// that was seen instead; it is [`TokenKind::Eof`] when end of file was
    /// reached.
    ///
    /// The expected `kinds` carry no source location — they are absent from the
    /// input. The diagnostic's span is the *insertion locus*: the zero-width
    /// position where one of the expected tokens should be inserted. The
    /// location of the `found` token is not stored here; it is recovered from
    /// the syntax tree at render time (the insertion locus equals the found
    /// token's offset, i.e. the start of its leading trivia).
    Expected { kinds: Box<[TokenKind]> },
    /// A token was seen that was not expected in some context
    Unexpected { kind: TokenKind },
    /// Bytes that are not valid were observed
    Illegal { bytes: Box<[u8]> },
    /// A token that was unterminated
    Unterminated { kind: UnterminatedKind },
}

#[derive(Debug)]
pub struct Diagnostic {
    /// The main span of the error.
    span: Span,
    /// the error that occured
    error: SyntaxErr,
}

impl Diagnostic {
    pub fn new(span: Span, err: SyntaxErr) -> Diagnostic {
        Diagnostic { span, error: err }
    }

    pub fn eof_err(span: Span, expected: impl Into<Box<[TokenKind]>>) -> Diagnostic {
        Diagnostic::new(
            span,
            SyntaxErr::Expected {
                kinds: expected.into(),
            },
        )
    }

    pub fn span(&self, converter: &SourceLocConverter) -> EncodedSpan {
        converter.convert_byte_span(&self.span)
    }

    pub fn span_raw(&self) -> &Span {
        &self.span
    }

    pub fn err(&self) -> &SyntaxErr {
        &self.error
    }
}
