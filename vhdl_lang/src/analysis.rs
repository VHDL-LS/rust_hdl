// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

#[macro_use]
mod analyze;
mod assignment;
mod association;
mod concurrent;
mod declarative;
mod design_unit;
mod expression;
mod literals;
mod lock;
mod names;
mod overloaded;
mod package_instance;
mod range;
mod root;
mod scope;
mod semantic;
mod sequential;
mod standard;
mod static_expression;
mod subprogram;
mod target;
mod types;

// @TODO consider moving facade into analysis
pub(crate) use {lock::ReadGuard, root::AnalysisData};

#[cfg(test)]
pub(crate) mod tests;
pub(crate) use root::{Library, LockedUnit};

pub use self::root::{DesignRoot, EntHierarchy};
