// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

mod declarative_region;
mod library;
mod pending;
mod semantic;
pub use self::library::{DesignRoot, Library};
pub use self::semantic::Analyzer;
