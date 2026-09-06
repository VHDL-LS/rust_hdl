// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

//! Basic building blocks of syntax trees
//!
//! The first step of processing a VHDL file is tokenizing: turning bytes into small chunks.
//! A [`Token`] knows its kind, the text, and [`Trivia`] that *precedes* it: the whitespace,
//! newlines and comments between the previous token and this one.
//!
//! ```
//! use vhdl_syntax::tokens::tokenizer::Tokenize;
//! use vhdl_syntax::tokens::{Keyword, TokenKind};
//!
//! let tokens: Vec<_> = "\
//! entity -- a comment
//! foo".tokenize().map(|(tok, _)| tok).collect();
//!
//! assert_eq!(tokens[0].kind(), TokenKind::Keyword(Keyword::Entity));
//! assert!(tokens[0].leading_trivia().is_empty());
//!
//! // The comment, the newline and the indentation all belong to `foo`.
//! assert_eq!(tokens[1].kind(), TokenKind::Identifier);
//! assert!(tokens[1].leading_trivia().contains_comments());
//! assert_eq!(tokens[1].leading_trivia().count_newlines(), 1);
//!
//! // Every stream is terminated by an `Eof` token, which covers remaining trailing trivia.
//! assert_eq!(tokens[2].kind(), TokenKind::Eof);
//! ```
//!
//! # Lexing
//!
//! [`Tokenizer`] yields `(Token, Option<LexErr>)`: an unterminated string still produces a token,
//! with the error alongside it. Like the parser, the lexer reports and carries on rather than
//! stopping at the first problem.

#[macro_use]
pub mod token;
pub mod comment;
pub mod token_kind;
pub mod token_stream;
pub mod tokenizer;
pub mod trivia;
pub mod trivia_piece;

pub use token::Token;
pub use token_kind::{Keyword, TokenKind};
pub use token_stream::TokenStream;
pub use tokenizer::{Tokenize, Tokenizer};
pub use trivia::Trivia;
pub use trivia_piece::TriviaPiece;
