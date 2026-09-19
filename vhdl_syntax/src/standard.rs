// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

//! VHDL standard version identification and comparison.
//!
//! This module provides the [`VHDLStandard`] enum to represent all supported VHDL versions
//! and methods to work with standards.

use std::{fmt, str::FromStr};

/// VHDL standard version.
///
/// Represents different VHDL versions.
/// As default, VHDL2008 is chosen.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Default)]
#[non_exhaustive]
pub enum VHDLStandard {
    VHDL1987,
    VHDL1993,
    VHDL2000,
    VHDL2002,
    #[default]
    VHDL2008,
    VHDL2019,
}

impl VHDLStandard {
    /// All supported VHDL standards in order.
    pub const ALL: [Self; 6] = [
        Self::VHDL1987,
        Self::VHDL1993,
        Self::VHDL2000,
        Self::VHDL2002,
        Self::VHDL2008,
        Self::VHDL2019,
    ];

    /// Returns the standard as a string.
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::VHDL1987 => "1987",
            Self::VHDL1993 => "1993",
            Self::VHDL2000 => "2000",
            Self::VHDL2002 => "2002",
            Self::VHDL2008 => "2008",
            Self::VHDL2019 => "2019",
        }
    }

    /// Returns the year of the standard.
    pub const fn year(self) -> u16 {
        match self {
            Self::VHDL1987 => 1987,
            Self::VHDL1993 => 1993,
            Self::VHDL2000 => 2000,
            Self::VHDL2002 => 2002,
            Self::VHDL2008 => 2008,
            Self::VHDL2019 => 2019,
        }
    }

    /// Returns true if this standard is at or newer than `other`.
    pub fn is_at_least(self, other: Self) -> bool {
        self >= other
    }

    /// Returns true if this standard is older than `other`.
    pub fn is_before(self, other: Self) -> bool {
        self < other
    }

    /// Returns the latest supported VHDL standard.
    pub fn latest() -> VHDLStandard {
        Self::VHDL2019
    }
}

#[test]
fn order_of_standards() {
    assert!(VHDLStandard::VHDL2008 > VHDLStandard::VHDL1993);
}

/// Error returned when parsing an invalid VHDL standard string.
#[derive(Debug, Clone)]
pub struct ParseVHDLStandardError;

impl std::fmt::Display for ParseVHDLStandardError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "invalid VHDL standard")
    }
}

impl std::error::Error for ParseVHDLStandardError {}

impl FromStr for VHDLStandard {
    type Err = ParseVHDLStandardError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use VHDLStandard::*;
        Ok(match s {
            "1987" | "87" => VHDL1987,
            "1993" | "93" => VHDL1993,
            "2000" | "00" => VHDL2000,
            "2002" | "02" => VHDL2002,
            "2008" | "08" => VHDL2008,
            "2019" | "19" => VHDL2019,
            _ => return Err(ParseVHDLStandardError),
        })
    }
}

impl AsRef<str> for VHDLStandard {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl fmt::Display for VHDLStandard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

#[cfg(feature = "serde")]
impl serde::Serialize for VHDLStandard {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(self.as_str())
    }
}

#[cfg(feature = "serde")]
impl<'de> serde::Deserialize<'de> for VHDLStandard {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        struct Visitor;

        impl serde::de::Visitor<'_> for Visitor {
            type Value = VHDLStandard;

            fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str("a VHDL standard such as \"2008\" or \"08\"")
            }

            fn visit_str<E: serde::de::Error>(self, v: &str) -> Result<Self::Value, E> {
                v.parse()
                    .map_err(|_| E::invalid_value(serde::de::Unexpected::Str(v), &self))
            }

            fn visit_u64<E: serde::de::Error>(self, v: u64) -> Result<Self::Value, E> {
                // Deny the two-digit form for integers: this would allow
                // `standard = 8` which reads weird in a config file
                let text = if v > 1000 {
                    format!("{v}")
                } else {
                    return Err(E::invalid_value(serde::de::Unexpected::Unsigned(v), &self));
                };
                text.parse()
                    .map_err(|_| E::invalid_value(serde::de::Unexpected::Unsigned(v), &self))
            }

            fn visit_i64<E: serde::de::Error>(self, v: i64) -> Result<Self::Value, E> {
                u64::try_from(v)
                    .map_err(|_| E::invalid_value(serde::de::Unexpected::Signed(v), &self))
                    .and_then(|v| self.visit_u64(v))
            }
        }

        deserializer.deserialize_any(Visitor)
    }
}
