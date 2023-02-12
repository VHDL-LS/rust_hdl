//! This Source Code Form is subject to the terms of the Mozilla Public
//! License, v. 2.0. If a copy of the MPL was not distributed with this file,
//! You can obtain one at http://mozilla.org/MPL/2.0/.
//!
//! Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use std::ops::Deref;

use crate::analysis::AnyEntKind;
use crate::ast::Mode;
use crate::ast::ObjectClass;

use super::AnyEnt;
use super::EntRef;
use super::Subtype;
use super::TypeEnt;

// A named entity that is known to be an object
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ObjectEnt<'a> {
    pub ent: EntRef<'a>,
}

impl<'a> ObjectEnt<'a> {
    pub fn from_any(ent: &'a AnyEnt) -> Option<Self> {
        if matches!(ent.actual_kind(), AnyEntKind::Object(..)) {
            Some(Self { ent })
        } else {
            None
        }
    }

    pub fn kind(&self) -> &'a Object<'a> {
        if let AnyEntKind::Object(object) = self.ent.actual_kind() {
            object
        } else {
            unreachable!("ObjectEnt type invariant broken")
        }
    }

    pub fn type_mark(&self) -> TypeEnt<'a> {
        self.kind().subtype.type_mark()
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
                format!("interface {}", self.describe_name())
            } else {
                format!("interface {} of mode {}", self.describe_name(), mode)
            }
        } else {
            self.describe_name()
        }
    }

    pub fn describe_name(&self) -> String {
        format!("{} '{}'", self.class(), self.designator())
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
    pub is_port: bool,
    pub mode: Option<Mode>,
    pub subtype: Subtype<'a>,
    pub has_default: bool,
}

impl<'a> Object<'a> {
    pub(crate) fn if_constant(subtype: Subtype<'a>) -> Object<'a> {
        Object {
            class: ObjectClass::Constant,
            is_port: false,
            mode: Some(Mode::In),
            subtype,
            has_default: false,
        }
    }

    pub(crate) fn with_default(mut self) -> Self {
        self.has_default = true;
        self
    }
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

impl<'a> Deref for ObjectEnt<'a> {
    type Target = EntRef<'a>;
    fn deref(&self) -> &Self::Target {
        &self.ent
    }
}
