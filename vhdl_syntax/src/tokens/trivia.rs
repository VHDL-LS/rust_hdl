// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

use crate::tokens::TriviaPiece;
use core::slice;
use std::{
    io::{self, Write},
    ops::{Deref, DerefMut},
    vec,
};

/// Trivia elements that are attached to tokens but do not influence the analysis of the text.
/// Such trivia elements may contain comments, whitespaces or other format effectors.
#[derive(Eq, PartialEq, Debug, Default, Clone)]
pub struct Trivia {
    // The single pieces of this trivia.
    // The fact that this is a vector is only an implementation detail.
    // This struct should behave vector-like, but also be cloneable without too much
    // overhead.
    pieces: Vec<TriviaPiece>,
}

impl From<Vec<TriviaPiece>> for Trivia {
    fn from(value: Vec<TriviaPiece>) -> Self {
        Trivia { pieces: value }
    }
}

impl<const N: usize> From<[TriviaPiece; N]> for Trivia {
    fn from(value: [TriviaPiece; N]) -> Self {
        Self::from(value.to_vec())
    }
}

impl<T: ?Sized + AsRef<[TriviaPiece]>> From<&T> for Trivia {
    fn from(s: &T) -> Trivia {
        Trivia::from(s.as_ref().to_vec())
    }
}

impl Trivia {
    /// Constructs a new and empty Trivia.
    pub fn new() -> Trivia {
        Trivia { pieces: Vec::new() }
    }

    pub fn byte_len(&self) -> usize {
        self.pieces
            .iter()
            .fold(0, |prev, curr| prev + curr.byte_len())
    }

    pub fn push(&mut self, piece: TriviaPiece) {
        self.pieces.push(piece)
    }

    pub fn pop(&mut self) -> Option<TriviaPiece> {
        self.pieces.pop()
    }

    pub fn is_empty(&self) -> bool {
        self.pieces.is_empty()
    }

    pub fn append(&mut self, other: &mut Trivia) {
        self.pieces.append(&mut other.pieces);
    }

    pub fn write_to(&self, writer: &mut impl Write) -> io::Result<()> {
        for trivia in self.iter() {
            trivia.write_to(writer)?;
        }
        Ok(())
    }

    pub fn clear(&mut self) {
        self.pieces.clear();
    }

    pub fn count_newlines(&self) -> usize {
        let mut count = 0;
        for piece in &self.pieces {
            match piece {
                TriviaPiece::CarriageReturns(n)
                | TriviaPiece::LineFeeds(n)
                | TriviaPiece::FormFeeds(n)
                | TriviaPiece::CarriageReturnLineFeeds(n)
                | TriviaPiece::VerticalTabs(n) => count += n,
                _ => {}
            }
        }
        count
    }

    pub fn count_spaces_or_tabs(&self) -> usize {
        let mut count = 0;
        for piece in &self.pieces {
            match piece {
                TriviaPiece::Spaces(n)
                | TriviaPiece::HorizontalTabs(n)
                | TriviaPiece::NonBreakingSpaces(n) => count += n,
                _ => {}
            }
        }
        count
    }

    pub fn contains_comments(&self) -> bool {
        self.pieces.iter().any(|piece| piece.is_comment())
    }
}

impl Trivia {
    pub fn clone_without_leading_spaces_or_tabs(&self) -> Trivia {
        let mut trivia = self.pieces.as_slice();
        while !trivia.is_empty() {
            if trivia[0].is_space_or_tab() {
                trivia = &trivia[1..];
            } else {
                break;
            }
        }
        Trivia::from(trivia)
    }

    pub fn clone_without_leading_whitespaces(&self) -> Trivia {
        let mut trivia = self.pieces.as_slice();
        while !trivia.is_empty() {
            if trivia[0].is_whitespace() {
                trivia = &trivia[1..];
            } else {
                break;
            }
        }
        Trivia::from(trivia)
    }

    pub fn remove_trailing_whitespaces(&mut self) {
        while self.last().is_some_and(TriviaPiece::is_whitespace) {
            self.pop();
        }
    }

    pub fn remove_trailing_spaces_or_tabs(&mut self) {
        while self.last().is_some_and(TriviaPiece::is_space_or_tab) {
            self.pop();
        }
    }

    pub fn has_newline(&self) -> bool {
        self.pieces.iter().any(TriviaPiece::is_newline)
    }

    pub fn has_spaces_or_tabs(&self) -> bool {
        self.pieces.iter().any(TriviaPiece::is_space_or_tab)
    }
}

impl Deref for Trivia {
    type Target = [TriviaPiece];

    fn deref(&self) -> &Self::Target {
        &self.pieces
    }
}

impl DerefMut for Trivia {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.pieces
    }
}

impl Extend<TriviaPiece> for Trivia {
    fn extend<T: IntoIterator<Item = TriviaPiece>>(&mut self, iter: T) {
        self.pieces.extend(iter);
    }
}

impl FromIterator<TriviaPiece> for Trivia {
    fn from_iter<T: IntoIterator<Item = TriviaPiece>>(iter: T) -> Self {
        Trivia {
            pieces: iter.into_iter().collect(),
        }
    }
}

impl<'a> FromIterator<&'a TriviaPiece> for Trivia {
    fn from_iter<T: IntoIterator<Item = &'a TriviaPiece>>(iter: T) -> Self {
        Trivia {
            pieces: iter.into_iter().cloned().collect(),
        }
    }
}

pub type Iter<'a> = slice::Iter<'a, TriviaPiece>;

impl<'a> IntoIterator for &'a Trivia {
    type Item = &'a TriviaPiece;

    type IntoIter = Iter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.pieces.iter()
    }
}

// Wrapped as newtype in order not to expose the underlying `vec`.
pub struct IntoIter(vec::IntoIter<TriviaPiece>);

impl Iterator for IntoIter {
    type Item = TriviaPiece;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl IntoIterator for Trivia {
    type Item = TriviaPiece;

    type IntoIter = IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter(self.pieces.into_iter())
    }
}
