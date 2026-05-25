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

/// A generic encoder.
/// This trait is very general to support various use-cases.
/// Callers should further restrict the associated types to support concrete use-cases.
/// For example, a caller might require `for <'a> Str<'a> : Display` to print the encoded string to some console.
///
/// Each encoder has an associated `Str` type.
/// This is the string when encoded in the specific encoding. For example,
/// for `latin-1`, this could be the [Latin1Str] type. For UTF-8, it's the builtin [str]
/// type.
///
/// This trait supports lossless and lossy encodings.
/// Lossless encoding may return an `Err` (e.g., [Utf8Error] when encoding UTF-8),
/// indicating that the given byte-sequence does not represent a valid string under the given encoding.
///
/// Lossy encoders should set `Err` to [Infallible] when they always produce valid output.
pub trait Encoder {
    /// The type that represents the encoded string.
    /// This carries a lifetime such that encoding can happen cheaply.
    /// Commonly, the `Str` type is a light wrapper around the given byte-sequence
    /// with certain guarantees and behaviour. This is the case for [str] and [Latin1Str],
    /// the two most used encoders.
    type Str<'a>;
    /// The error that may occur when encoding a string.
    type Err;

    /// Encode a byte-slice and return the string if successful or an error if not.
    fn encode(bytes: &[u8]) -> Result<Self::Str<'_>, Self::Err>;
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

/// A UTF-8 encoder that never fails and replaces each invalid byte with a replacement sequence.
pub struct LossyUtf8Encoder;

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
