// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

use crate::tokens::TriviaPiece;
use std::ops::Deref;

/// Trivia elements that are attached to tokens but do not influence the analysis of the text.
/// Such trivia elements may contain comments, whitespaces or other format effectors.
#[derive(Eq, PartialEq, Debug, Default, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Trivia {
    // The single pieces of this trivia.
    // The fact that this is a vector is only an implementation detail.
    // This struct should behave vector-like, but also be cloneable without too much
    // overhead.
    pieces: Vec<TriviaPiece>,
}

impl Trivia {
    /// Constructs a new trivia by its pieces
    pub fn new(pieces: impl Into<Vec<TriviaPiece>>) -> Trivia {
        Trivia::from_vec(pieces.into())
    }

    fn from_vec(vec: Vec<TriviaPiece>) -> Trivia {
        Trivia { pieces: vec }
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

impl Deref for Trivia {
    type Target = [TriviaPiece];

    fn deref(&self) -> &Self::Target {
        &self.pieces
    }
}

impl FromIterator<TriviaPiece> for Trivia {
    fn from_iter<T: IntoIterator<Item = TriviaPiece>>(iter: T) -> Self {
        let vec = iter.into_iter().collect::<Vec<_>>();
        Trivia::from_vec(vec)
    }
}
