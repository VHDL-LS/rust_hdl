// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2026, Lukas Scheller lukasscheller@icloud.com

use std::fmt::Debug;

/// A comment
///
/// Because VHDL allows comments to have any encoding,
/// this implementation makes no assumption as to that and is simply
/// backed by bytes. Utility methods exist to get the value with different
/// encodings.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Comment {
    // TODO: We should check if interning (globally or locally per file / library)
    // can provide an advantage. Comments are fairly unique, but stuff like
    // license headers or linter directives may profit from interning.
    inner: Box<[u8]>,
}

impl Comment {
    /// Creates a block comment with leading and trailing delimiters (`/*` and `*/`) already included
    ///
    /// # Example
    ///
    /// ```
    /// # use vhdl_syntax::tokens::comment::Comment;
    ///
    /// let comment = Comment::block(b"Hello");
    /// assert_eq!(comment.as_bytes(), b"/*Hello*/");
    /// ```
    pub fn block(bytes: impl AsRef<[u8]>) -> Comment {
        let mut vec = Vec::new();
        vec.extend_from_slice(b"/*");
        vec.extend_from_slice(bytes.as_ref());
        vec.extend_from_slice(b"*/");
        Comment::from_raw(vec)
    }

    /// Creates a line comment, including the leading `--` separator
    ///
    /// # Example
    ///
    /// ```
    /// # use vhdl_syntax::tokens::comment::Comment;
    ///
    /// let comment = Comment::line(b"Hello");
    /// assert_eq!(comment.as_bytes(), b"--Hello");
    /// ```
    pub fn line(bytes: impl AsRef<[u8]>) -> Comment {
        let mut vec = Vec::new();
        vec.extend_from_slice(b"--");
        vec.extend_from_slice(bytes.as_ref());
        Comment::from_raw(vec)
    }

    /// Creates a comment without leading or trailing delimiters, i.e.,
    /// to create a syntactically correct comment you must provide those yourself.
    /// Prefer [Comment::block] or [Comment::line] for safe alternatives.
    ///
    /// # Example
    ///
    /// ```
    /// # use vhdl_syntax::tokens::comment::Comment;
    ///
    /// // from_raw requires supplying the the delimiters:
    /// assert_eq!(Comment::from_raw(b"--Hello"), Comment::line(b"Hello"));
    ///  assert_eq!(Comment::from_raw(b"/*World*/"), Comment::block(b"World"));
    ///
    /// // from_raw allows creation of illegal or unterminated comments
    /// let illegal = Comment::from_raw(b"Hello");
    /// assert_eq!(illegal.as_bytes(), b"Hello");
    ///
    /// let unterminated = Comment::from_raw(b"/* Hello");
    /// assert_eq!(unterminated.as_bytes(), b"/* Hello");
    /// ```
    pub fn from_raw(bytes: impl AsRef<[u8]>) -> Comment {
        Comment {
            inner: bytes.as_ref().into(),
        }
    }

    /// Return the comment as byte-slice
    pub fn as_bytes(&self) -> &[u8] {
        &self.inner
    }

    /// Return the length of the comment
    pub fn byte_len(&self) -> usize {
        self.as_bytes().len()
    }
}
