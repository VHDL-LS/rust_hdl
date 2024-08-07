// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use crate::ast::Designator;
use crate::data::Symbol;
use crate::AnyEntKind;
use crate::EntRef;

use super::TypeEnt;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct AttributeEnt<'a> {
    pub ent: EntRef<'a>,
}

impl<'a> AttributeEnt<'a> {
    pub fn from_any(ent: EntRef<'a>) -> Option<Self> {
        if let AnyEntKind::Attribute(..) = ent.actual_kind() {
            Some(AttributeEnt { ent })
        } else {
            None
        }
    }

    pub fn name(&self) -> &Symbol {
        if let Designator::Identifier(sym) = self.ent.designator() {
            sym
        } else {
            panic!("Internal error: Bad attribute designator: {:?}", self.ent);
        }
    }

    pub fn typ(&self) -> TypeEnt<'a> {
        if let AnyEntKind::Attribute(typ) = self.ent.actual_kind() {
            *typ
        } else {
            unreachable!();
        }
    }
}

impl<'a> From<AttributeEnt<'a>> for EntRef<'a> {
    fn from(value: AttributeEnt<'a>) -> Self {
        value.ent
    }
}
