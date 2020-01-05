// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

mod circular_dependencies;
mod context_clause;
mod deferred_constant;
mod homographs;
mod incomplete_type;
mod incremental_analysis;
mod package_instance;
mod protected_type;
mod resolves_design_units;
mod resolves_names;
mod resolves_type_mark;
mod util;
mod visibility;

pub use self::util::*;
pub use crate::data::Diagnostic;
pub use crate::syntax::test::*;
