// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

use crate::tokens::TriviaPiece;
use core::slice;
use std::{
    io::{self, Write},
    ops::Deref,
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

impl Trivia {
    /// Constructs a new and empty Trivia.
    pub fn new() -> Trivia {
        Trivia { pieces: Vec::new() }
    }
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
    pub fn byte_len(&self) -> usize {
        self.pieces
            .iter()
            .fold(0, |prev, curr| prev + curr.byte_len())
    }

    pub fn push(&mut self, piece: TriviaPiece) {
        self.pieces.push(piece)
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
}

impl Deref for Trivia {
    type Target = [TriviaPiece];

    fn deref(&self) -> &Self::Target {
        &self.pieces
    }
}

impl FromIterator<TriviaPiece> for Trivia {
    fn from_iter<T: IntoIterator<Item = TriviaPiece>>(iter: T) -> Self {
        Trivia {
            pieces: iter.into_iter().collect(),
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
