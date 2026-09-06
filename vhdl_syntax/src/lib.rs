// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

//! A lossless library for tokenizing, parsing, inspecting and modifying VHDL code.
//!
//! **Lossless** means that the a parsed tree contains the entire source file including whitespaces,
//! comments, or fragments that are invalid VHDL. As a consequence, parsing a tree from input and writing
//! the parsed tree back out reproduces identical input. This is opposed to, for example, a compiler that
//! discards that information for efficiency since it does not influence the compilation.
//!
//! ```
//! use vhdl_syntax::parser;
//! use vhdl_syntax::syntax::AstNode;
//!
//! let src = "\
//! entity foo is -- this comment is part of the tree
//! end foo;
//! ";
//! let (design, errors) = parser::parse(src);
//! assert!(errors.is_empty());
//!
//! let mut out = Vec::new();
//! design.raw().write_to(&mut out).unwrap();
//! assert_eq!(out, src.as_bytes());
//! ```
//!
//! # Untyped tree
//!
//! A consequence of being lossless is that the tree must be able to represent all possible, potentially
//! incorrect, inputs. To do this, each node can contain arbitrary children.
//! This is opposed to a typed node that contains a defined subset of children.
//! Read more at the documentation for [SyntaxNodes](crate::syntax::SyntaxNode).
//!
//! # Character set
//!
//! With the exception of [comments](crate::tokens::comment), VHDL source is Latin-1.
//! Entry-points using rust [str], [String], or other UTF-8 might get unexpected results for non-ASCII UTF-8.
//! Therefore, the byte-oriented input facilities are preferred (i.e., read a file in binary mode, not to string, and parse that).
//! Additionally, the [latin_1] module contains facilities to convert from UTF-8 to Latin-1.

pub mod builder;
pub mod fmt;
pub(crate) mod interning;
pub mod latin_1;
pub mod parser;
#[cfg(feature = "serde")]
pub mod serde;
pub mod standard;
pub mod syntax;
pub mod text;
pub mod tokens;
