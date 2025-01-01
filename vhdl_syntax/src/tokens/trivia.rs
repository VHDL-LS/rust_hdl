// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

use crate::tokens::TriviaPiece;
use std::slice;

/// Trivia elements that are attached to tokens but do not influence the analysis of the text.
/// Such trivia elements may contain comments, whitespaces or other format effectors.
#[derive(Eq, PartialEq, Debug, Default, Clone)]
pub struct Trivia {
    pub pieces: Vec<TriviaPiece>,
}

impl Trivia {
    /// Constructs a new trivia by it's pieces
    pub fn new(pieces: impl Into<Vec<TriviaPiece>>) -> Trivia {
        Trivia {
            pieces: pieces.into(),
        }
    }
}

impl<'a> IntoIterator for &'a Trivia {
    type Item = &'a TriviaPiece;
    type IntoIter = slice::Iter<'a, TriviaPiece>;

    fn into_iter(self) -> Self::IntoIter {
        self.pieces.iter()
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
}
