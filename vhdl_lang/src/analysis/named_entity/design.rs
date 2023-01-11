//! This Source Code Form is subject to the terms of the Mozilla Public
//! License, v. 2.0. If a copy of the MPL was not distributed with this file,
//! You can obtain one at http://mozilla.org/MPL/2.0/.
//!
//! Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use std::ops::Deref;
use std::sync::Arc;

use crate::analysis::region::Region;
use std::borrow::Borrow;

use super::AnyEnt;
use super::AnyEntKind;

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

// A named entity that is known to be a type
#[derive(Clone, Debug)]
pub struct DesignEnt(Arc<AnyEnt>);

impl DesignEnt {
    pub fn from_any(ent: Arc<AnyEnt>) -> Result<DesignEnt, Arc<AnyEnt>> {
        if matches!(ent.kind(), AnyEntKind::Design(..)) {
            Ok(DesignEnt(ent))
        } else {
            Err(ent)
        }
    }
    pub fn kind(&self) -> &Design {
        if let AnyEntKind::Design(typ) = self.0.kind() {
            typ
        } else {
            unreachable!("Must be a design");
        }
    }
}

impl From<DesignEnt> for Arc<AnyEnt> {
    fn from(ent: DesignEnt) -> Self {
        ent.0
    }
}

impl std::cmp::PartialEq for DesignEnt {
    fn eq(&self, other: &Self) -> bool {
        self.deref() == other.deref()
    }
}

impl AsRef<Arc<AnyEnt>> for DesignEnt {
    fn as_ref(&self) -> &Arc<AnyEnt> {
        &self.0
    }
}

impl Deref for DesignEnt {
    type Target = Arc<AnyEnt>;
    fn deref(&self) -> &Arc<AnyEnt> {
        let val: &Arc<AnyEnt> = self.0.borrow();
        val
    }
}
