//! This Source Code Form is subject to the terms of the Mozilla Public
//! License, v. 2.0. If a copy of the MPL was not distributed with this file,
//! You can obtain one at http://mozilla.org/MPL/2.0/.
//!
//! Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use std::sync::Arc;

use crate::analysis::region::Region;

pub enum Design {
    Entity(Arc<Region>),
    Configuration(Arc<Region>),
    Package(Arc<Region>),
    UninstPackage(Arc<Region>),
    PackageInstance(Arc<Region>),
    Context(Arc<Region>),
    LocalPackageInstance(Arc<Region>),
}

impl Design {
    pub fn describe(&self) -> &'static str {
        use Design::*;
        match self {
            Entity(..) => "entity",
            Configuration(..) => "configuration",
            Package(..) => "package",
            UninstPackage(..) => "uninstantiated package",
            PackageInstance(..) => "package instance",
            Context(..) => "context",
            LocalPackageInstance(..) => "package instance",
        }
    }
}
