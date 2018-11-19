// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use std::fmt;
use std::str;

fn iso_8859_1_to_utf8(bytes: &[u8]) -> String {
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

    pub fn from_vec(bytes: Vec<u8>) -> Latin1String {
        Latin1String { bytes }
    }

    pub fn len(&self) -> usize {
        return self.bytes.len();
    }

    pub fn lowercase(chr: u8) -> u8 {
        match chr {
            215 => chr,
            b'A'..=b'Z' | 192..=222 => chr + 32,
            _ => chr,
        }
    }

    pub fn into_lowercase(&mut self) {
        for i in 0..self.bytes.len() {
            self.bytes[i] = Self::lowercase(self.bytes[i]);
        }
    }

    pub fn to_lowercase(&self) -> Latin1String {
        let mut latin1 = Latin1String {
            bytes: self.bytes.clone(),
        };
        latin1.into_lowercase();
        latin1
    }

    #[cfg(test)]
    pub fn from_utf8_unchecked(string: &str) -> Latin1String {
        Self::from_utf8(string).unwrap()
    }

    pub fn from_utf8(string: &str) -> Result<Latin1String, String> {
        let bytes = string.as_bytes();
        let mut latin1_bytes = Vec::with_capacity(string.len());
        let mut i = 0;
        while i < bytes.len() {
            let byte = bytes[i];

            if byte < 128 {
                latin1_bytes.push(byte);
                i += 1;
                continue;
            } else if byte == 0xc2 {
                let next_byte = bytes[i + 1];
                if 128 <= next_byte && next_byte < 192 {
                    latin1_bytes.push(next_byte);
                    i += 2;
                    continue;
                }
            } else if byte == 0xc3 {
                let next_byte = bytes[i + 1];
                if 128 <= next_byte && next_byte < 192 {
                    latin1_bytes.push(next_byte + 64);
                    i += 2;
                    continue;
                }
            }

            return Err(format!(
                "Invalid latin-1 character {} in {}",
                string[i..].chars().next().unwrap(),
                string
            ));
        }
        Ok(Latin1String::from_vec(latin1_bytes))
    }
}

impl fmt::Debug for Latin1String {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", iso_8859_1_to_utf8(&self.bytes))
    }
}

impl fmt::Display for Latin1String {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", iso_8859_1_to_utf8(&self.bytes))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_latin1_to_utf8() {
        for byte in 0..=255 {
            let latin1 = Latin1String::new(&[byte]);
            assert_eq!(
                Latin1String::from_utf8(&latin1.to_string()).unwrap(),
                latin1
            )
        }
    }

    #[test]
    fn test_latin1_lowercase() {
        for byte in 0..=255 {
            let latin1 = Latin1String::new(&[byte]);
            let utf8 = latin1.to_string();
            assert_eq!(latin1.to_lowercase().to_string(), utf8.to_lowercase());
        }
    }
}
