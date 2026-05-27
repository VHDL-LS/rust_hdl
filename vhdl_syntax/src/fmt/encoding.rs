//! Utilities for arbitrary encoding and implementations for common cases
//!
//! # Rationale
//!
//! As per the LRM, VHDL is Latin-1 encoded. However, comments can carry a different encoding.
//! While almost no editor supports different encodings between code and comments,
//! this allows a VHDL file to be opened as UTF-8 with comments that can contain
//! UTF-8 characters while only using the ASCII subset in code
//! (ASCII is shared between UTF-8 and Latin-1, the default VHDL encoding).

use std::{borrow::Cow, convert::Infallible, str::Utf8Error};

use crate::latin_1::Latin1Str;

/// Interprets a byte slice as a string in some encoding.
///
/// Callers can further restrict the associated types for concrete use-cases
/// (e.g., `for<'a> E::Str<'a>: Display` for printing).
///
/// Encoding may fail (e.g., invalid UTF-8). Lossy encoders that always succeed
/// should also implement [`LossyEncoder`].
pub trait Encoder {
    /// The encoded string type. Carries a lifetime so encoding can be zero-copy.
    type Str<'a>;
    type Err;

    fn encode(bytes: &[u8]) -> Result<Self::Str<'_>, Self::Err>;
}

/// Marker: encoding reinterprets the input bytes without modifying them.
/// The encoded `Str` has the same byte representation as the input.
pub trait BytePreservingEncoder: Encoder {}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Replacement {
    /// Byte offset within the input slice where the replacement occurred.
    pub source_offset: usize,
    /// Number of source bytes consumed by this replacement.
    pub source_bytes: usize,
    /// Number of new bytes emitted
    pub target_bytes: usize,
}

impl Replacement {
    pub fn new(source_offset: usize, source_bytes: usize, target_bytes: usize) -> Replacement {
        Replacement {
            source_offset,
            source_bytes,
            target_bytes,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Replacements {
    entries: Vec<Replacement>,
}

impl Replacements {
    pub fn new() -> Replacements {
        Replacements::default()
    }

    pub fn push(&mut self, offset: usize, source_bytes: usize, target_bytes: usize) {
        self.entries.push(Replacement {
            source_offset: offset,
            source_bytes,
            target_bytes,
        });
    }

    pub fn entries(&self) -> &[Replacement] {
        &self.entries
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}

/// An encoder that always succeeds, replacing invalid byte sequences
/// rather than returning an error. Returns the encoded string together
/// with a record of which source bytes were replaced.
pub trait LossyEncoder {
    type Str<'a>;

    fn encode_lossy(bytes: &[u8]) -> (Self::Str<'_>, Replacements);
}

/// Encodes UTF-8 sequences. May fail with a `Utf8Error`
pub struct Utf8Encoder;

impl Encoder for Utf8Encoder {
    type Str<'a> = &'a str;
    type Err = Utf8Error;

    fn encode(bytes: &[u8]) -> Result<Self::Str<'_>, Self::Err> {
        str::from_utf8(bytes)
    }
}

impl BytePreservingEncoder for Utf8Encoder {}

/// A UTF-8 encoder that never fails and replaces each invalid byte with a replacement sequence.
pub struct LossyUtf8Encoder;

impl LossyEncoder for LossyUtf8Encoder {
    type Str<'a> = Cow<'a, str>;

    fn encode_lossy(bytes: &[u8]) -> (Self::Str<'_>, Replacements) {
        let mut iter = bytes.utf8_chunks();
        let Some(chunk) = iter.next() else {
            return (Cow::Borrowed(""), Replacements::default());
        };
        let first_valid = chunk.valid();
        if chunk.invalid().is_empty() {
            debug_assert_eq!(first_valid.len(), bytes.len());
            return (Cow::Borrowed(first_valid), Replacements::default());
        }

        const REPLACEMENT: &str = "\u{FFFD}";

        let mut res = String::with_capacity(bytes.len());
        let mut replacements = Replacements::new();
        let mut cursor = 0usize;

        res.push_str(first_valid);
        cursor += first_valid.len();
        res.push_str(REPLACEMENT);
        replacements.push(cursor, chunk.invalid().len(), REPLACEMENT.len());
        cursor += chunk.invalid().len();

        for chunk in iter {
            res.push_str(chunk.valid());
            cursor += chunk.valid().len();
            if !chunk.invalid().is_empty() {
                replacements.push(cursor, chunk.invalid().len(), REPLACEMENT.len());
                cursor += chunk.invalid().len();
                res.push_str(REPLACEMENT);
            }
        }

        (Cow::Owned(res), replacements)
    }
}

impl Encoder for LossyUtf8Encoder {
    type Str<'a> = Cow<'a, str>;

    type Err = Infallible;

    fn encode(bytes: &[u8]) -> Result<Self::Str<'_>, Self::Err> {
        Ok(String::from_utf8_lossy(bytes))
    }
}

/// Encodes Latin-1 sequences. All bytes represent valid Latin-1 sequences, therefore this encoder never fails.
pub struct Latin1Encoder;

impl Encoder for Latin1Encoder {
    type Str<'a> = &'a Latin1Str;
    type Err = Infallible;

    fn encode(bytes: &[u8]) -> Result<Self::Str<'_>, Self::Err> {
        Ok(Latin1Str::new(bytes))
    }
}

impl LossyEncoder for Latin1Encoder {
    type Str<'a> = &'a Latin1Str;

    fn encode_lossy(bytes: &[u8]) -> (Self::Str<'_>, Replacements) {
        (Latin1Str::new(bytes), Replacements::default())
    }
}

impl BytePreservingEncoder for Latin1Encoder {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty() {
        let (s, r) = LossyUtf8Encoder::encode_lossy(b"");
        assert_eq!(s, "");
        assert!(r.is_empty());
    }

    #[test]
    fn all_valid_ascii() {
        let (s, r) = LossyUtf8Encoder::encode_lossy(b"hello world");
        assert_eq!(s, "hello world");
        assert!(r.is_empty());
    }

    #[test]
    fn all_valid_multibyte() {
        let (s, r) = LossyUtf8Encoder::encode_lossy("äöü💣".as_bytes());
        assert_eq!(s, "äöü💣");
        assert!(r.is_empty());
    }

    #[test]
    fn single_invalid_byte() {
        let (s, r) = LossyUtf8Encoder::encode_lossy(b"\xE4");
        assert_eq!(s, "\u{FFFD}");
        assert_eq!(r.entries(), &[Replacement::new(0, 1, 3)]);
    }

    #[test]
    fn two_consecutive_invalid_bytes() {
        let (s, r) = LossyUtf8Encoder::encode_lossy(b"\xE4\xE5");
        assert_eq!(s, "\u{FFFD}\u{FFFD}");
        assert_eq!(
            r.entries(),
            &[Replacement::new(0, 1, 3), Replacement::new(1, 1, 3)]
        );
    }

    #[test]
    fn invalid_then_valid() {
        let (s, r) = LossyUtf8Encoder::encode_lossy(b"\xE4ABCD");
        assert_eq!(s, "\u{FFFD}ABCD");
        assert_eq!(r.entries(), &[Replacement::new(0, 1, 3)]);
    }

    #[test]
    fn valid_then_invalid() {
        let (s, r) = LossyUtf8Encoder::encode_lossy(b"ABCD\xE4");
        assert_eq!(s, "ABCD\u{FFFD}");
        assert_eq!(r.entries(), &[Replacement::new(4, 1, 3)]);
    }

    #[test]
    fn invalid_between_valid() {
        let (s, r) = LossyUtf8Encoder::encode_lossy(b"AB\xE4\xE5CD");
        assert_eq!(s, "AB\u{FFFD}\u{FFFD}CD");
        assert_eq!(
            r.entries(),
            &[Replacement::new(2, 1, 3), Replacement::new(3, 1, 3)]
        );
    }

    #[test]
    fn multiple_invalid_groups_separated_by_valid() {
        let (s, r) = LossyUtf8Encoder::encode_lossy(b"\xE4AB\xE5CD\xE6");
        assert_eq!(s, "\u{FFFD}AB\u{FFFD}CD\u{FFFD}");
        assert_eq!(
            r.entries(),
            &[
                Replacement::new(0, 1, 3),
                Replacement::new(3, 1, 3),
                Replacement::new(6, 1, 3)
            ]
        );
    }

    #[test]
    fn incomplete_multibyte_sequence() {
        // 0xF0 0x9F: valid start of a 4-byte sequence (e.g., emoji), truncated
        let (s, r) = LossyUtf8Encoder::encode_lossy(b"\xF0\x9FAB");
        assert_eq!(s, "\u{FFFD}AB");
        assert_eq!(r.entries(), &[Replacement::new(0, 2, 3)]);
    }

    #[test]
    fn overlong_encoding_splits_into_separate_replacements() {
        // 0xF0 0x80: 0x80 is not a valid second byte after 0xF0 (needs 0x90..=0xBF)
        let (s, r) = LossyUtf8Encoder::encode_lossy(b"\xF0\x80AB");
        assert_eq!(s, "\u{FFFD}\u{FFFD}AB");
        assert_eq!(
            r.entries(),
            &[Replacement::new(0, 1, 3), Replacement::new(1, 1, 3)]
        );
    }

    #[test]
    fn valid_multibyte_between_invalid() {
        // ä = C3 A4 in UTF-8
        let (s, r) = LossyUtf8Encoder::encode_lossy(b"\xE4\xC3\xA4\xE5");
        assert_eq!(s, "\u{FFFD}ä\u{FFFD}");
        assert_eq!(
            r.entries(),
            &[Replacement::new(0, 1, 3), Replacement::new(3, 1, 3)]
        );
    }
}
