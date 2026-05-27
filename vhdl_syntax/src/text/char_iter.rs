//! Byte-position–aware character iteration over encoded strings.

use crate::latin_1::Latin1Str;

/// Iterates characters together with their byte offset within the string.
///
/// This is the bridge between [`crate::fmt::encoding::Encoder::Str`] and
/// [`super::source_loc::SourceLocConverter`]: the converter needs to walk
/// characters while tracking byte positions so it can compute the byte width
/// of each character as the gap between consecutive offsets.
pub trait CharIter {
    /// Return an iterator yielding the byte position and the character
    fn iter_chars_indices(&self) -> impl Iterator<Item = (usize, char)>;

    /// Returns the total number of bytes this iterator will iterate over
    fn byte_count(&self) -> usize;
}

impl<T> CharIter for T
where
    T: AsRef<str>,
{
    fn iter_chars_indices(&self) -> impl Iterator<Item = (usize, char)> {
        self.as_ref().char_indices()
    }

    fn byte_count(&self) -> usize {
        self.as_ref().len()
    }
}

impl CharIter for &Latin1Str {
    fn iter_chars_indices(&self) -> impl Iterator<Item = (usize, char)> {
        // Every byte is one character in Latin-1, so byte index == char index.
        self.chars().enumerate()
    }

    fn byte_count(&self) -> usize {
        self.len()
    }
}
