// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::Position;
use std::fmt;
use std::str;

pub fn iso_8859_1_to_utf8(bytes: &[u8]) -> String {
    let mut utf8_bytes = Vec::new();
    for byte in bytes.iter() {
        let byte = *byte;
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
    unsafe { str::from_utf8_unchecked(utf8_bytes.as_slice()).to_string() }
}

pub fn char_to_latin1(chr: char) -> Option<u8> {
    let mut bytes = [0; 4];
    chr.encode_utf8(&mut bytes);
    let byte = bytes[0];
    if byte < 128 {
        Some(byte)
    } else if byte == 0xc2 {
        let next_byte = bytes[1];
        if (128..192).contains(&next_byte) {
            Some(next_byte)
        } else {
            None
        }
    } else if byte == 0xc3 {
        let next_byte = bytes[1];
        if (128..192).contains(&next_byte) {
            Some(next_byte + 64)
        } else {
            None
        }
    } else {
        None
    }
}

#[derive(PartialEq, Eq, Hash, Clone)]
pub struct Latin1String {
    pub bytes: Vec<u8>,
}

impl Latin1String {
    pub fn empty() -> Latin1String {
        Latin1String { bytes: Vec::new() }
    }

    pub fn new(bytes: &[u8]) -> Latin1String {
        Latin1String {
            bytes: Vec::from(bytes),
        }
    }

    pub fn chars(&self) -> impl Iterator<Item = &u8> {
        self.bytes.iter()
    }

    pub fn from_vec(bytes: Vec<u8>) -> Latin1String {
        Latin1String { bytes }
    }

    pub fn len(&self) -> usize {
        self.bytes.len()
    }

    pub fn is_empty(&self) -> bool {
        self.bytes.is_empty()
    }

    pub fn lowercase(chr: u8) -> u8 {
        match chr {
            215 => chr,
            b'A'..=b'Z' | 192..=214 | 216..=222 => chr + 32,
            _ => chr,
        }
    }

    pub fn make_lowercase(&mut self) {
        for i in 0..self.bytes.len() {
            self.bytes[i] = Self::lowercase(self.bytes[i]);
        }
    }

    pub fn to_lowercase(&self) -> Latin1String {
        let mut latin1 = Latin1String {
            bytes: self.bytes.clone(),
        };
        latin1.make_lowercase();
        latin1
    }

    pub fn starts_with(&self, other: &Latin1String) -> bool {
        if other.len() <= self.len() {
            self.bytes[0..other.len()] == other.bytes
        } else {
            false
        }
    }

    #[cfg(test)]
    pub fn from_utf8_unchecked(string: &str) -> Latin1String {
        Self::from_utf8(string).unwrap()
    }

    pub fn from_utf8(string: &str) -> Result<Latin1String, Utf8ToLatin1Error> {
        let bytes = string.as_bytes();
        let mut latin1_bytes = Vec::with_capacity(string.len());
        let mut i = 0;

        let mut line = 0;
        let mut column = 0;

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
                return Err(Utf8ToLatin1Error {
                    pos: Position::new(line, column),
                    value,
                });
            }

            if byte == b'\n' {
                line += 1;
                column = 0;
            } else {
                column += 1;
            }
        }
        Ok(Latin1String::from_vec(latin1_bytes))
    }
}

impl Default for Latin1String {
    fn default() -> Self {
        Self::empty()
    }
}

impl fmt::Debug for Latin1String {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", iso_8859_1_to_utf8(&self.bytes))
    }
}

impl fmt::Display for Latin1String {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", iso_8859_1_to_utf8(&self.bytes))
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct Utf8ToLatin1Error {
    pub pos: Position,
    pub value: char,
}

impl Utf8ToLatin1Error {
    pub fn message(&self) -> String {
        format!("Found invalid latin-1 character '{}'", self.value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use assert_matches::assert_matches;

    #[test]
    fn latin1_to_utf8() {
        for byte in 0..=255 {
            let latin1 = Latin1String::new(&[byte]);
            assert_eq!(
                Latin1String::from_utf8(&latin1.to_string()).unwrap(),
                latin1
            )
        }
    }

    #[test]
    fn latin1_lowercase() {
        for byte in 0..=255 {
            let latin1 = Latin1String::new(&[byte]);
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
            assert_eq!(err.pos.line, 0);
            assert_eq!(err.pos.character, 3);
            assert_eq!(err.value, '€');
        });

        let utf8 = "a\nbö\n€";
        assert_matches!(Latin1String::from_utf8(utf8), Err(err) => {
            assert_eq!(err.pos.line, 2);
            assert_eq!(err.pos.character, 0);
            assert_eq!(err.value, '€');
            assert_eq!(err.message(), "Found invalid latin-1 character '€'");
        });
    }
}
