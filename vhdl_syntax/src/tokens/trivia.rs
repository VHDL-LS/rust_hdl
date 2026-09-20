// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

use crate::tokens::TriviaPiece;
use core::slice;
use std::{
    borrow::Borrow,
    io::{self, Write},
    ops::{Deref, Index, Range, RangeFrom, RangeFull, RangeInclusive, RangeTo, RangeToInclusive},
    vec,
};

// MARK: Trivia

/// Trivia elements that are attached to tokens but do not influence the analysis of the text.
/// Such trivia elements may contain comments, whitespaces or other format effectors.
#[derive(Eq, PartialEq, Debug)]
#[repr(transparent)]
pub struct Trivia([TriviaPiece]);

impl Trivia {
    /// Trivia that contains no pieces at all.
    pub const EMPTY: &'static Trivia = Trivia::new(&[]);

    pub const fn new(pieces: &[TriviaPiece]) -> &Trivia {
        // SAFETY: Trivia is a transparent newtype wrapper around [TriviaPiece],
        // so this transmute is sound.
        unsafe { &*(pieces as *const [TriviaPiece] as *const Trivia) }
    }

    /// Returns the piece or sub-trivia at `index`, or `None` if it is out of bounds.
    pub fn get<I: TriviaIndex>(&self, index: I) -> Option<&I::Output> {
        index.get(self)
    }

    pub fn iter(&self) -> slice::Iter<'_, TriviaPiece> {
        self.0.iter()
    }

    /// The number of pieces of this trivia.
    /// Note that this is not the byte length, use [Trivia::byte_len] for that
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn byte_len(&self) -> usize {
        self.iter().fold(0, |prev, curr| prev + curr.byte_len())
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn write_to(&self, writer: &mut impl Write) -> io::Result<()> {
        for trivia in self.iter() {
            trivia.write_to(writer)?;
        }
        Ok(())
    }

    pub fn count_newlines(&self) -> usize {
        let mut count = 0;
        for piece in &self.0 {
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
        for piece in &self.0 {
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
        self.iter().any(|piece| piece.is_comment())
    }

    pub fn has_newline(&self) -> bool {
        self.iter().any(TriviaPiece::is_newline)
    }

    pub fn has_spaces_or_tabs(&self) -> bool {
        self.iter().any(TriviaPiece::is_space_or_tab)
    }

    pub fn last(&self) -> Option<&TriviaPiece> {
        self.0.last()
    }

    pub fn first(&self) -> Option<&TriviaPiece> {
        self.0.first()
    }

    pub fn without_leading_spaces_or_tabs(&self) -> &Trivia {
        let mut trivia = &self.0;
        while !trivia.is_empty() {
            if trivia[0].is_space_or_tab() {
                trivia = &trivia[1..];
            } else {
                break;
            }
        }
        Trivia::new(trivia)
    }

    pub fn without_leading_whitespaces(&self) -> &Trivia {
        let mut trivia = &self.0;
        while !trivia.is_empty() {
            if trivia[0].is_whitespace() {
                trivia = &trivia[1..];
            } else {
                break;
            }
        }
        Trivia::new(trivia)
    }
}

mod private {
    pub trait Sealed {}
}

/// A helper trait used for indexing operations on [`Trivia`].
///
/// This mirrors [`std::slice::SliceIndex`]: indexing with a `usize` yields a single
/// [`TriviaPiece`], while indexing with a range yields a [`Trivia`] sub-slice.
/// The trait is sealed and cannot be implemented outside of this crate.
pub trait TriviaIndex: private::Sealed {
    /// The value produced by this indexing operation.
    type Output: ?Sized;

    /// Returns a reference to the output at this location, if in bounds.
    fn get(self, trivia: &Trivia) -> Option<&Self::Output>;

    /// Returns a reference to the output at this location, panicking if out of bounds.
    fn index(self, trivia: &Trivia) -> &Self::Output;
}

impl private::Sealed for usize {}

impl TriviaIndex for usize {
    type Output = TriviaPiece;

    fn get(self, trivia: &Trivia) -> Option<&Self::Output> {
        trivia.0.get(self)
    }

    fn index(self, trivia: &Trivia) -> &Self::Output {
        trivia.0.index(self)
    }
}

macro_rules! impl_range_index {
    ($($range:ty),* $(,)?) => {
        $(
            impl private::Sealed for $range {}

            impl TriviaIndex for $range {
                type Output = Trivia;

                fn get(self, trivia: &Trivia) -> Option<&Self::Output> {
                    trivia.0.get(self).map(Trivia::new)
                }

                fn index(self, trivia: &Trivia) -> &Self::Output {
                    Trivia::new(trivia.0.index(self))
                }
            }
        )*
    };
}

impl_range_index!(
    Range<usize>,
    RangeFrom<usize>,
    RangeFull,
    RangeInclusive<usize>,
    RangeTo<usize>,
    RangeToInclusive<usize>,
);

impl<I: TriviaIndex> Index<I> for Trivia {
    type Output = I::Output;

    fn index(&self, index: I) -> &Self::Output {
        index.index(self)
    }
}

impl<'a> IntoIterator for &'a Trivia {
    type Item = &'a TriviaPiece;

    type IntoIter = slice::Iter<'a, TriviaPiece>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

// MARK: TriviaBuf

/// Owned trivia buffer with vec-like operations
#[derive(Eq, PartialEq, Debug, Default, Clone)]
pub struct TriviaBuf {
    pieces: Vec<TriviaPiece>,
}

impl TriviaBuf {
    /// Constructs a new and empty Trivia.
    pub const fn new() -> TriviaBuf {
        TriviaBuf { pieces: Vec::new() }
    }

    /// Constructs a new, empty `Trivia` with at least the specified capacity.
    pub fn with_capacity(capacity: usize) -> TriviaBuf {
        TriviaBuf {
            pieces: Vec::with_capacity(capacity),
        }
    }

    /// Borrows the contents as [`Trivia`].
    pub fn as_trivia(&self) -> &Trivia {
        Trivia::new(&self.pieces)
    }

    pub fn capacity(&self) -> usize {
        self.pieces.capacity()
    }

    pub fn reserve(&mut self, additional: usize) {
        self.pieces.reserve(additional);
    }

    pub fn truncate(&mut self, len: usize) {
        self.pieces.truncate(len);
    }

    pub fn insert(&mut self, index: usize, piece: TriviaPiece) {
        self.pieces.insert(index, piece);
    }

    pub fn remove(&mut self, index: usize) -> TriviaPiece {
        self.pieces.remove(index)
    }

    pub fn retain(&mut self, f: impl FnMut(&TriviaPiece) -> bool) {
        self.pieces.retain(f);
    }

    pub fn push(&mut self, piece: TriviaPiece) {
        self.pieces.push(piece)
    }

    pub fn pop(&mut self) -> Option<TriviaPiece> {
        self.pieces.pop()
    }

    pub fn append(&mut self, other: &mut TriviaBuf) {
        self.pieces.append(&mut other.pieces);
    }

    pub fn clear(&mut self) {
        self.pieces.clear();
    }
}

impl From<Vec<TriviaPiece>> for TriviaBuf {
    fn from(value: Vec<TriviaPiece>) -> Self {
        TriviaBuf { pieces: value }
    }
}

impl<'a> From<&'a [TriviaPiece]> for TriviaBuf {
    fn from(value: &'a [TriviaPiece]) -> Self {
        Self::from(value.to_vec())
    }
}

impl<const N: usize> From<[TriviaPiece; N]> for TriviaBuf {
    fn from(value: [TriviaPiece; N]) -> Self {
        Self::from(value.to_vec())
    }
}

impl Extend<TriviaPiece> for TriviaBuf {
    fn extend<T: IntoIterator<Item = TriviaPiece>>(&mut self, iter: T) {
        self.pieces.extend(iter);
    }
}

impl<'a> Extend<&'a TriviaPiece> for TriviaBuf {
    fn extend<T: IntoIterator<Item = &'a TriviaPiece>>(&mut self, iter: T) {
        self.pieces.extend(iter.into_iter().cloned());
    }
}

impl FromIterator<TriviaPiece> for TriviaBuf {
    fn from_iter<T: IntoIterator<Item = TriviaPiece>>(iter: T) -> Self {
        TriviaBuf {
            pieces: iter.into_iter().collect(),
        }
    }
}

impl<'a> FromIterator<&'a TriviaPiece> for TriviaBuf {
    fn from_iter<T: IntoIterator<Item = &'a TriviaPiece>>(iter: T) -> Self {
        TriviaBuf {
            pieces: iter.into_iter().cloned().collect(),
        }
    }
}

impl IntoIterator for TriviaBuf {
    type Item = TriviaPiece;

    type IntoIter = vec::IntoIter<TriviaPiece>;

    fn into_iter(self) -> Self::IntoIter {
        self.pieces.into_iter()
    }
}

impl<'a> IntoIterator for &'a TriviaBuf {
    type Item = &'a TriviaPiece;

    type IntoIter = slice::Iter<'a, TriviaPiece>;

    fn into_iter(self) -> Self::IntoIter {
        self.pieces.iter()
    }
}

// MARK: Conversions

impl Borrow<Trivia> for TriviaBuf {
    fn borrow(&self) -> &Trivia {
        Trivia::new(&self.pieces)
    }
}

impl AsRef<Trivia> for TriviaBuf {
    fn as_ref(&self) -> &Trivia {
        Trivia::new(&self.pieces)
    }
}

impl ToOwned for Trivia {
    type Owned = TriviaBuf;

    fn to_owned(&self) -> Self::Owned {
        TriviaBuf::from(&self.0)
    }
}

impl Deref for TriviaBuf {
    type Target = Trivia;

    fn deref(&self) -> &Self::Target {
        Trivia::new(&self.pieces)
    }
}

impl From<&Trivia> for TriviaBuf {
    fn from(val: &Trivia) -> Self {
        val.to_owned()
    }
}

impl From<TriviaBuf> for Vec<TriviaPiece> {
    fn from(val: TriviaBuf) -> Self {
        val.pieces
    }
}

impl AsRef<Trivia> for Trivia {
    fn as_ref(&self) -> &Trivia {
        self
    }
}

impl Default for &Trivia {
    fn default() -> Self {
        Trivia::EMPTY
    }
}

// MARK: Equality

macro_rules! impl_partial_eq {
    ($(($lhs:ty, $rhs:ty)),* $(,)?) => {
        $(
            impl PartialEq<$rhs> for $lhs {
                fn eq(&self, other: &$rhs) -> bool {
                    let lhs: &Trivia = self.as_ref();
                    let rhs: &Trivia = other.as_ref();
                    lhs == rhs
                }
            }
        )*
    };
}

impl_partial_eq!((Trivia, TriviaBuf), (TriviaBuf, Trivia),);

impl PartialEq<&Trivia> for TriviaBuf {
    fn eq(&self, other: &&Trivia) -> bool {
        self.pieces[..] == other.0
    }
}

impl<const N: usize> PartialEq<[TriviaPiece; N]> for Trivia {
    fn eq(&self, other: &[TriviaPiece; N]) -> bool {
        self.0 == other[..]
    }
}

impl<const N: usize> PartialEq<Trivia> for [TriviaPiece; N] {
    fn eq(&self, other: &Trivia) -> bool {
        self[..] == other.0
    }
}

impl<const N: usize> PartialEq<[TriviaPiece; N]> for TriviaBuf {
    fn eq(&self, other: &[TriviaPiece; N]) -> bool {
        self.pieces[..] == other[..]
    }
}

impl<const N: usize> PartialEq<TriviaBuf> for [TriviaPiece; N] {
    fn eq(&self, other: &TriviaBuf) -> bool {
        self[..] == other.pieces[..]
    }
}

#[cfg(test)]
mod tests {
    use crate::tokens::{Trivia, TriviaBuf, TriviaPiece};

    fn pieces() -> [TriviaPiece; 3] {
        [
            TriviaPiece::Spaces(1),
            TriviaPiece::LineFeeds(2),
            TriviaPiece::Spaces(3),
        ]
    }

    #[test]
    fn empty_trivia() {
        assert_eq!(Trivia::EMPTY.len(), 0);
        assert!(Trivia::EMPTY.is_empty());
        assert_eq!(<&Trivia>::default(), Trivia::EMPTY);
    }

    #[test]
    fn cross_type_equality() {
        let buf = TriviaBuf::from(pieces());
        let trivia: &Trivia = &buf;

        assert_eq!(trivia, &buf);
        assert_eq!(&buf, trivia);
        assert_eq!(*trivia, buf);
        assert_eq!(buf, *trivia);
        assert_eq!(trivia, &pieces());
        assert_eq!(&buf, &pieces());

        assert_ne!(trivia, &TriviaBuf::new());
        assert_ne!(&buf, &TriviaBuf::new());
    }

    #[test]
    fn indexing_with_every_range() {
        let buf = TriviaBuf::from(pieces());
        let trivia: &Trivia = &buf;

        assert_eq!(trivia[0], TriviaPiece::Spaces(1));
        assert_eq!(trivia[..1], [TriviaPiece::Spaces(1)]);
        assert_eq!(
            trivia[1..],
            [TriviaPiece::LineFeeds(2), TriviaPiece::Spaces(3)]
        );
        assert_eq!(trivia[1..2], [TriviaPiece::LineFeeds(2)]);
        assert_eq!(
            trivia[1..=2],
            [TriviaPiece::LineFeeds(2), TriviaPiece::Spaces(3)]
        );
        assert_eq!(trivia[..=0], [TriviaPiece::Spaces(1)]);
        assert_eq!(trivia[..], pieces());
    }

    #[test]
    fn get_is_bounds_checked() {
        let buf = TriviaBuf::from(pieces());
        let trivia: &Trivia = &buf;

        assert_eq!(trivia.get(2), Some(&TriviaPiece::Spaces(3)));
        assert_eq!(trivia.get(3), None);
        assert_eq!(trivia.get(1..3).map(Trivia::len), Some(2));
        assert!(trivia.get(1..4).is_none());
    }

    #[test]
    fn extend_from_references() {
        let mut buf = TriviaBuf::new();
        buf.extend(pieces().iter());
        assert_eq!(buf, pieces());
    }

    #[test]
    fn vec_like_mutation() {
        let mut buf = TriviaBuf::with_capacity(4);
        assert!(buf.capacity() >= 4);

        buf.extend(pieces());
        buf.insert(0, TriviaPiece::Spaces(9));
        assert_eq!(buf[0], TriviaPiece::Spaces(9));

        assert_eq!(buf.remove(0), TriviaPiece::Spaces(9));
        assert_eq!(buf, pieces());

        buf.retain(TriviaPiece::is_space_or_tab);
        assert_eq!(buf, [TriviaPiece::Spaces(1), TriviaPiece::Spaces(3)]);

        buf.truncate(1);
        assert_eq!(buf, [TriviaPiece::Spaces(1)]);

        buf.reserve(16);
        assert!(buf.capacity() >= 17);
    }

    #[test]
    fn as_ref_accepts_both_sides() {
        fn len_of(trivia: impl AsRef<Trivia>) -> usize {
            trivia.as_ref().len()
        }

        let buf = TriviaBuf::from(pieces());
        assert_eq!(len_of(&buf), 3);
        assert_eq!(len_of(buf.as_trivia()), 3);
    }
}
