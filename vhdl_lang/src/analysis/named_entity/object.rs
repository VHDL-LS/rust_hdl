//! This Source Code Form is subject to the terms of the Mozilla Public
//! License, v. 2.0. If a copy of the MPL was not distributed with this file,
//! You can obtain one at http://mozilla.org/MPL/2.0/.
//!
//! Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use crate::analysis::AnyEntKind;
use crate::ast::Mode;
use crate::ast::ObjectClass;

use super::AnyEnt;
use super::EntRef;
use super::Subtype;

// A named entity that is known to be an object
#[derive(Clone, Debug)]
pub struct ObjectEnt<'a> {
    pub ent: EntRef<'a>,
}

impl<'a> ObjectEnt<'a> {
    pub fn new(ent: &'a AnyEnt) -> Self {
        debug_assert!(matches!(ent.actual_kind(), AnyEntKind::Object(..)));
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

    pub fn object(&self) -> &'a Object<'a> {
        if let AnyEntKind::Object(object) = self.ent.actual_kind() {
            object
        } else {
            unreachable!("Must be object");
        }
    }
}

/// An object or an interface object,
/// example signal, variable, constant
/// Is either an object (mode = None) or an interface object (mode = Some)
#[derive(Clone)]
pub struct Object<'a> {
    pub class: ObjectClass,
    pub mode: Option<Mode>,
    pub subtype: Subtype<'a>,
    pub has_default: bool,
}

impl ObjectClass {
    pub fn describe(&self) -> &str {
        use ObjectClass::*;
        match self {
            Constant => "constant",
            Variable => "variable",
            Signal => "signal",
            SharedVariable => "shared variable",
        }
    }
}
