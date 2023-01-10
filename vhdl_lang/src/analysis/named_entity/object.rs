//! This Source Code Form is subject to the terms of the Mozilla Public
//! License, v. 2.0. If a copy of the MPL was not distributed with this file,
//! You can obtain one at http://mozilla.org/MPL/2.0/.
//!
//! Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use std::sync::Arc;

use crate::analysis::NamedEntityKind;
use crate::ast::Mode;
use crate::ast::ObjectClass;

use super::NamedEntity;
use super::Object;

// A named entity that is known to be an object
#[derive(Clone, Debug)]
pub struct ObjectEnt {
    pub ent: Arc<NamedEntity>,
}

impl ObjectEnt {
    pub fn new(ent: Arc<NamedEntity>) -> Self {
        debug_assert!(matches!(ent.actual_kind(), NamedEntityKind::Object(..)));
        Self { ent }
    }

    pub fn class(&self) -> ObjectClass {
        self.object().class
    }

    pub fn mode(&self) -> Option<Mode> {
        self.object().mode
    }

    pub fn describe_class(&self) -> String {
        if let Some(mode) = self.mode() {
            if self.class() == ObjectClass::Constant {
                format!("interface {}", self.class())
            } else {
                format!("interface {} of mode {}", self.class(), mode)
            }
        } else {
            format!("{}", self.class())
        }
    }

    pub fn object(&self) -> &Object {
        if let NamedEntityKind::Object(object) = self.ent.actual_kind() {
            object
        } else {
            unreachable!("Must be object");
        }
    }
}
