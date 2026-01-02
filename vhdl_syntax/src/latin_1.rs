//! # Latin-1 String Types
//! 
//! As opposed to the Rust default, in VHDL, text is [ISO-8859-1](https://de.wikipedia.org/wiki/ISO_8859-1)
//! ("Latin-1") encoded.
//! This module provides string types for working with Latin-1 encoded text
//! that behave closely to [`String`] and [`prim@str`].
//! 
//! ## Differences to `String` and `str`
//!
//! In Rust's `String` and `str`, a character is a Unicode scalar value (represented as `char`),
//! which can be multiple bytes when encoded in UTF-8. In contrast, Latin-1 has a 1:1 mapping
//! between bytes and characters—each byte (0-255) represents exactly one character.
//! This means [`Latin1String`] and [`Latin1Str`] work directly with bytes, where a single `u8`
//! is equivalent to a single character, simplifying indexing and iteration.
//!
//! ## Conversions
//!
//! ### UTF-8 to Latin-1
//!
//! Conversion from a standard Rust `&str` (UTF-8) to Latin-1 may fail and produce a [`Utf8ToLatin1Error`] when a UTF-8
//! character may not be represented in Latin-1.
//!
//! ```
//! # use vhdl_syntax::latin_1::Latin1String;
//! // Converting ASCII and extended Latin-1 characters
//! let latin1 = Latin1String::from_utf8("café").unwrap();
//! assert_eq!(latin1.as_bytes(), &[99, 97, 102, 233]); // 233 is é in Latin-1
//!
//! // Characters outside Latin-1 will produce an error
//! assert!(Latin1String::from_utf8("Hello €").is_err()); // Euro sign not in Latin-1
//! ```
//!
//! ### Latin-1 to UTF-8
//!
//! Converting a [`Latin1String`] or [`Latin1Str`] to UTF-8 always succeeds since every Latin-1 byte
//! can be represented as UTF-8 value.
//!
//! ```
//! # use vhdl_syntax::latin_1::Latin1String;
//! let latin1 = Latin1String::from_utf8("café").unwrap();
//! let utf8_string: String = latin1.to_string();
//! assert_eq!(utf8_string, "café");
//! ```
//!
//! ### From Raw Bytes
//!
//! Create a [`Latin1String`] directly from a byte vector or byte slice without validation:
//!
//! ```
//! # use vhdl_syntax::latin_1::Latin1String;
//! let bytes = vec![99, 97, 102, 233]; // "café" in Latin-1
//! let latin1 = Latin1String::from(bytes);
//! assert_eq!(latin1.to_string(), "café");
//! ```

// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2025, Olof Kraigher olof.kraigher@gmail.com

use std::borrow::Borrow;
use std::hash::{Hash, Hasher};
use std::ops::{self, Range};
use std::str::{self, FromStr};
use std::{cmp, fmt, slice};

/// An owned Latin-1 string type.
pub struct Latin1String {
    bytes: Vec<u8>,
}

impl Latin1String {
    /// Creates a new empty `Latin1String`.
    #[must_use]
    pub fn new() -> Latin1String {
        Latin1String { bytes: Vec::new() }
    }

    /// Create a Latin1-string with allocated capacity.
    /// 
    /// The `capacity` argument is passed to the backing `Vec`.
    #[must_use]
    pub fn with_capacity(capacity: usize) -> Latin1String {
        Latin1String {
            bytes: Vec::with_capacity(capacity),
        }
    }

    /// Extracts a string slice containing the entire String.
    ///
    /// # Examples
    ///
    /// ```
    /// # use vhdl_syntax::latin_1::{Latin1String, Latin1Str};
    /// let s = Latin1String::from(b"foo");
    ///
    /// assert_eq!(Latin1Str::new(b"foo"), s.as_latin1_str());
    /// ```
    #[must_use]
    pub fn as_latin1_str(&self) -> &Latin1Str {
        self
    }

    /// Extracts a mutable string slice containing the entire String.
    ///
    /// # Examples
    ///
    /// ```
    /// # use vhdl_syntax::latin_1::{Latin1String, Latin1Str};
    /// let mut s = Latin1String::from(b"foo");
    /// 
    /// let slice = s.as_latin1_str_mut();
    /// slice[0] = b'o';
    ///
    /// assert_eq!(Latin1Str::new(b"ooo"), slice);
    /// ```
    #[must_use]
    pub fn as_latin1_str_mut(&mut self) -> &mut Latin1Str {
        self
    }

    #[must_use = "`self` will be dropped if the result is not used"]
    pub fn into_bytes(self) -> Vec<u8> {
        self.bytes
    }

    pub fn clear(&mut self) {
        self.bytes.clear()
    }

    pub fn from_utf8(string: &str) -> Result<Latin1String, Utf8ToLatin1Error> {
        let bytes = string.as_bytes();
        let mut latin1_bytes = Vec::with_capacity(string.len());
        let mut i = 0;

        let mut pos = 0;

        while i < bytes.len() {
            let byte = bytes[i];

            let mut error = false;
            if byte < 128 {
                latin1_bytes.push(byte);
                i += 1;
            } else if byte == 0xc2 {
                let next_byte = bytes[i + 1];
                if (128..192).contains(&next_byte) {
                    latin1_bytes.push(next_byte);
                    i += 2;
                } else {
                    error = true;
                }
            } else if byte == 0xc3 {
                let next_byte = bytes[i + 1];
                if (128..192).contains(&next_byte) {
                    latin1_bytes.push(next_byte + 64);
                    i += 2;
                } else {
                    error = true;
                }
            } else {
                error = true;
            }

            if error {
                let value = string[i..].chars().next().unwrap();
                return Err(Utf8ToLatin1Error { pos, value });
            }

            pos += 1;
        }
        Ok(Latin1String::from(latin1_bytes))
    }

    #[cfg(test)]
    pub fn from_utf8_unchecked(string: &str) -> Latin1String {
        Self::from_utf8(string).unwrap()
    }

    pub fn push(&mut self, byte: u8) {
        self.bytes.push(byte)
    }

    pub fn append(&mut self, other: &mut Latin1String) {
        self.bytes.append(&mut other.bytes);
    }
}

impl Clone for Latin1String {
    fn clone(&self) -> Self {
        Self {
            bytes: self.bytes.clone(),
        }
    }

    fn clone_from(&mut self, source: &Self) {
        self.bytes.clone_from(&source.bytes)
    }
}

impl<T: ?Sized + AsRef<[u8]>> From<&T> for Latin1String {
    fn from(s: &T) -> Latin1String {
        Latin1String::from(s.as_ref().to_vec())
    }
}

impl From<Vec<u8>> for Latin1String {
    fn from(value: Vec<u8>) -> Self {
        Latin1String { bytes: value }
    }
}

impl From<Latin1String> for Vec<u8> {
    fn from(value: Latin1String) -> Self {
        value.bytes
    }
}

impl From<&Latin1Str> for Latin1String {
    fn from(value: &Latin1Str) -> Self {
        value.to_owned()
    }
}

impl FromStr for Latin1String {
    type Err = Utf8ToLatin1Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Latin1String::from_utf8(s)
    }
}

impl FromIterator<u8> for Latin1String {
    fn from_iter<T: IntoIterator<Item = u8>>(iter: T) -> Self {
        Latin1String::from(iter.into_iter().collect::<Vec<_>>())
    }
}

impl Extend<u8> for Latin1String {
    fn extend<T: IntoIterator<Item = u8>>(&mut self, iter: T) {
        self.bytes.extend(iter);
    }
}

impl fmt::Debug for Latin1String {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&**self, f)
    }
}

impl ops::Deref for Latin1String {
    type Target = Latin1Str;

    fn deref(&self) -> &Self::Target {
        Latin1Str::new(self.bytes.as_ref())
    }
}

impl ops::DerefMut for Latin1String {
    fn deref_mut(&mut self) -> &mut Self::Target {
        Latin1Str::new_mut(&mut self.bytes)
    }
}

impl Borrow<Latin1Str> for Latin1String {
    fn borrow(&self) -> &Latin1Str {
        &self
    }
}

impl Default for Latin1String {
    fn default() -> Self {
        Latin1String::new()
    }
}

impl ToOwned for Latin1Str {
    type Owned = Latin1String;

    fn to_owned(&self) -> Self::Owned {
        self.to_latin1_string()
    }

    fn clone_into(&self, target: &mut Latin1String) {
        self.inner.clone_into(&mut target.bytes);
    }
}

impl PartialEq for Latin1String {
    fn eq(&self, other: &Self) -> bool {
        self.as_latin1_str() == other.as_latin1_str()
    }
}

impl Hash for Latin1String {
    fn hash<H: Hasher>(&self, h: &mut H) {
        self.as_latin1_str().hash(h)
    }
}

impl Eq for Latin1String {}

impl cmp::PartialEq<str> for Latin1String {
    fn eq(&self, other: &str) -> bool {
        cmp_latin1_to_str(self.as_latin1_str(), other)
    }
}

impl cmp::PartialEq<String> for Latin1String {
    fn eq(&self, other: &String) -> bool {
        cmp_latin1_to_str(self.as_latin1_str(), other.as_str())
    }
}

impl cmp::PartialEq<Latin1String> for str {
    fn eq(&self, other: &Latin1String) -> bool {
        cmp_latin1_to_str(other.as_latin1_str(), self)
    }
}

impl cmp::PartialEq<Latin1String> for String {
    fn eq(&self, other: &Latin1String) -> bool {
        cmp_latin1_to_str(other.as_latin1_str(), self.as_str())
    }
}

impl PartialOrd for Latin1String {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        self.inner.partial_cmp(&other.inner)
    }
}

impl Ord for Latin1String {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.as_latin1_str().cmp(other.as_latin1_str())
    }
}

impl AsRef<[u8]> for Latin1String {
    fn as_ref(&self) -> &[u8] {
        &self.bytes[..]
    }
}

impl fmt::Display for Latin1String {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&**self, f)
    }
}

/// A borrowed, Latin-1 string slice type.
#[derive(PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub struct Latin1Str {
    inner: [u8],
}

fn iso_8859_1_lowercase(chr: u8) -> u8 {
    match chr {
        b'A'..=b'Z' | 192..=214 | 216..=222 => chr + 32,
        _ => chr,
    }
}

impl Latin1Str {
    pub fn new(bytes: &[u8]) -> &Latin1Str {
        // SAFETY: Latin1Str is a transparent newtype wrapper around [u8],
        // so this transmute is sound.
        unsafe { &*(bytes as *const [u8] as *const Latin1Str) }
    }

    pub fn new_mut(bytes: &mut [u8]) -> &mut Latin1Str {
        // SAFETY: Latin1Str is a transparent newtype wrapper around [u8],
        // so this transmute is sound.
        unsafe { &mut *(bytes as *mut [u8] as *mut Latin1Str) }
    }

    pub fn as_bytes(&self) -> &[u8] {
        &self.inner
    }

    pub fn as_mut_bytes(&mut self) -> &mut [u8] {
        &mut self.inner
    }

    pub fn to_latin1_string(&self) -> Latin1String {
        Latin1String::from(&self.inner)
    }

    pub fn chars(&self) -> slice::Iter<'_, u8> {
        self.inner.iter()
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    pub fn make_lowercase(&mut self) {
        for i in 0..self.inner.len() {
            self.inner[i] = iso_8859_1_lowercase(self.inner[i]);
        }
    }

    pub fn to_lowercase(&self) -> Latin1String {
        let mut latin1 = self.to_owned();
        latin1.make_lowercase();
        latin1
    }

    pub fn starts_with(&self, other: &Latin1String) -> bool {
        if other.len() <= self.len() {
            self.inner[0..other.len()] == other.bytes
        } else {
            false
        }
    }
}

fn iso_8859_1_to_utf8(bytes: &[u8]) -> String {
    let mut utf8_bytes = Vec::with_capacity(bytes.len());
    for byte in bytes.iter().copied() {
        if byte < 128 {
            utf8_bytes.push(byte);
        } else if byte < 192 {
            utf8_bytes.push(0xc2);
            utf8_bytes.push(byte);
        } else {
            utf8_bytes.push(0xc3);
            utf8_bytes.push(byte - 64);
        }
    }

    // SAFETY: We only push valid UTF-8 sequences into utf8_bytes:
    // - ASCII bytes (< 128) are valid UTF-8 single bytes
    // - 0xc2 followed by 128..192 creates valid 2-byte UTF-8 sequences
    // - 0xc3 followed by 128..192 creates valid 2-byte UTF-8 sequences
    unsafe { String::from_utf8_unchecked(utf8_bytes) }
}

impl fmt::Debug for Latin1Str {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&iso_8859_1_to_utf8(&self.inner), f)
    }
}

impl fmt::Display for Latin1Str {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&iso_8859_1_to_utf8(&self.inner), f)
    }
}

impl AsRef<Latin1Str> for Latin1Str {
    fn as_ref(&self) -> &Latin1Str {
        self
    }
}

impl<const N: usize> From<[u8; N]> for Latin1String {
    fn from(value: [u8; N]) -> Self {
        Self {
            bytes: value.into(),
        }
    }
}

impl std::ops::Index<Range<usize>> for Latin1String {
    type Output = Latin1Str;

    fn index(&self, index: Range<usize>) -> &Self::Output {
        Latin1Str::new(&self.bytes.index(index))
    }
}

impl std::ops::Index<usize> for Latin1Str {
    type Output = u8;

    fn index(&self, index: usize) -> &Self::Output {
        &self.inner[index]
    }
}

impl std::ops::IndexMut<usize> for Latin1Str {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.inner[index]
    }
}

fn cmp_latin1_to_str(latin1: &Latin1Str, str: &str) -> bool {
    // OPTIMIZATION: This is for simplicity only.
    // There is no need to allocate a string just for comparison.
    latin1.to_string() == str
}

impl cmp::PartialEq<str> for Latin1Str {
    fn eq(&self, other: &str) -> bool {
        cmp_latin1_to_str(self, other)
    }
}

impl cmp::PartialEq<String> for Latin1Str {
    fn eq(&self, other: &String) -> bool {
        cmp_latin1_to_str(self, other.as_str())
    }
}

impl cmp::PartialEq<Latin1Str> for str {
    fn eq(&self, other: &Latin1Str) -> bool {
        cmp_latin1_to_str(other, self)
    }
}

impl cmp::PartialEq<Latin1Str> for String {
    fn eq(&self, other: &Latin1Str) -> bool {
        cmp_latin1_to_str(other, self.as_str())
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct Utf8ToLatin1Error {
    pub pos: usize,
    pub value: char,
}

impl fmt::Display for Utf8ToLatin1Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Invalid latin-1 character '{}' at position {}",
            self.value, self.pos
        )
    }
}

impl std::error::Error for Utf8ToLatin1Error {}

#[cfg(test)]
mod tests {
    use super::*;
    use assert_matches::assert_matches;

    #[test]
    fn latin1_to_utf8() {
        for byte in 0..=255 {
            let latin1 = Latin1String::from(&[byte]);
            assert_eq!(
                Latin1String::from_utf8(&latin1.to_string()).unwrap(),
                latin1
            )
        }
    }

    #[test]
    fn latin1_lowercase() {
        for byte in 0..=255u8 {
            let latin1 = Latin1String::from(&[byte]);
            let utf8 = latin1.to_string();
            assert_eq!(latin1.to_lowercase().to_string(), utf8.to_lowercase());
        }
    }

    #[test]
    fn utf8_to_latin1() {
        let utf8 = "åäö";
        assert_matches!(Latin1String::from_utf8(utf8), Ok(latin1) => {
            assert_eq!(latin1.bytes, [229, 228, 246]);
            assert_eq!(latin1.to_string(), utf8);
        })
    }

    #[test]
    fn utf8_to_latin1_error() {
        let utf8 = "abö€";
        assert_matches!(Latin1String::from_utf8(utf8), Err(err) => {
            assert_eq!(err.pos, 3);
            assert_eq!(err.value, '€');
        });

        let utf8 = "a\nbö\n€";
        assert_matches!(Latin1String::from_utf8(utf8), Err(err) => {
            assert_eq!(err.pos, 5);
            assert_eq!(err.value, '€');
            assert_eq!(format!("{}", err), "Invalid latin-1 character '€' at position 5");
        });
    }
}
