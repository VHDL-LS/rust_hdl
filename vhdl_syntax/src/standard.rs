// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

//! VHDL standard version identification and comparison.
//!
//! This module provides the [`VHDLStandard`] enum to represent all supported VHDL versions
//! and methods to work with standards.

use std::fmt;

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

impl TryFrom<&str> for VHDLStandard {
    type Error = ParseVHDLStandardError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        use VHDLStandard::*;
        Ok(match value {
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
