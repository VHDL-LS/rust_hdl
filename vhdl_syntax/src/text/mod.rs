//! Utilities for mapping between byte offsets and source locations (line/column).
//!
//! The key abstraction is [`source_loc::SourceLocConverter`], which builds a
//! line index from a syntax tree and translates byte offsets to
//! `(line, column)` pairs in a target encoding.
//!
//! Source and target encoding are independent: a file may be stored as UTF-8
//! on disk while the LSP client expects UTF-16 column offsets.

pub mod char_encoding;
pub mod char_iter;
pub mod source_loc;
