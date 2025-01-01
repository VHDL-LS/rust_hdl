//! Facilities for tokenizing raw text and token streams for the parser.
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

#[macro_use]
pub mod token;
pub mod token_kind;
pub mod token_stream;
pub mod tokenizer;
pub mod trivia;
pub mod trivia_piece;

pub use token::Token;
pub use token_kind::{Keyword, TokenKind};
pub use token_stream::{IntoTokenStream, TokenStream, VecTokenStream};
pub use tokenizer::{Tokenize, Tokenizer};
pub use trivia::Trivia;
pub use trivia_piece::TriviaPiece;
