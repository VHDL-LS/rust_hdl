//! Target encoding definitions for column-width measurement.
//!
//! A [`CharEncoding`] answers "how many code units does this character occupy
//! in a given encoding?" This is used by [`super::source_loc::SourceLocConverter`]
//! to translate byte offsets into columns that match a client's expected encoding
//! (e.g., UTF-16 for LSP).

/// Measures the width of a `char` in a target encoding's code units.
///
/// This is the number of code units the target system uses to represent
/// this character, *not* the byte width in the source file.
/// This is *not* the byte width in the source file.
pub trait CharEncoding {
    fn char_len(ch: char) -> usize;
}

/// UTF-8: 1–4 code units (bytes) per character.
pub struct Utf8;
impl CharEncoding for Utf8 {
    fn char_len(ch: char) -> usize {
        ch.len_utf8()
    }
}

/// UTF-16: 1–2 code units (16-bit) per character. This is the LSP default.
pub struct Utf16;
impl CharEncoding for Utf16 {
    fn char_len(ch: char) -> usize {
        ch.len_utf16()
    }
}

/// UTF-32 / code point index: always 1 unit per character.
pub struct Utf32;
impl CharEncoding for Utf32 {
    fn char_len(_ch: char) -> usize {
        1
    }
}

/// Latin-1: always 1 unit per character.
/// Identical to [`Utf32`] for column measurement since every Latin-1
/// character is a single code point.
pub struct Latin1;
impl CharEncoding for Latin1 {
    fn char_len(_ch: char) -> usize {
        1
    }
}
