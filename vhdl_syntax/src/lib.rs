// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

pub mod fmt;
pub mod latin_1;
pub mod parser;
#[cfg(feature = "serde")]
pub mod serde;
pub mod syntax;
mod token_interning;
pub mod tokens;
pub mod standard;
