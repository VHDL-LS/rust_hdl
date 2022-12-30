// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

#[macro_use]
mod analyze;
mod concurrent;
mod declarative;
mod design_unit;
mod implicits;
mod lock;
mod named_entity;
mod names;
mod region;
mod root;
mod semantic;
mod sequential;
mod target;
mod visibility;

#[cfg(test)]
mod tests;

pub use self::root::DesignRoot;
pub use named_entity::NamedEntity;
