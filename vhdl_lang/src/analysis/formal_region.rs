// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2022, Olof Kraigher olof.kraigher@gmail.com

use crate::{
    ast::{Designator, InterfaceListType, Mode, ObjectClass},
    Diagnostic, SrcPos,
};

use super::named_entity::*;

#[derive(Copy, Clone)]
pub struct InterfaceEnt<'a> {
    /// InterfaceObject or InterfaceFile
    ent: EntRef<'a>,
}

impl<'a> InterfaceEnt<'a> {
    pub fn inner(&self) -> EntRef<'a> {
        self.ent
    }

    pub fn from_any(ent: EntRef<'a>) -> Option<Self> {
        match ent.kind() {
            AnyEntKind::Object(Object { mode: Some(_), .. }) | AnyEntKind::InterfaceFile(..) => {
                Some(InterfaceEnt { ent })
            }
            _ => None,
        }
    }

    pub fn has_default(&self) -> bool {
        if let AnyEntKind::Object(Object { has_default, .. }) = self.ent.kind() {
            *has_default
        } else {
            false
        }
    }

    pub fn is_signal(&self) -> bool {
        match self.ent.kind() {
            AnyEntKind::Object(obj) => obj.class == ObjectClass::Signal,
            _ => false,
        }
    }

    pub fn is_output_signal(&self) -> bool {
        match self.ent.kind() {
            AnyEntKind::Object(obj) => {
                obj.class == ObjectClass::Signal && obj.mode == Some(Mode::Out)
            }
            _ => false,
        }
    }

    pub fn type_mark(&self) -> TypeEnt<'a> {
        match self.ent.kind() {
            AnyEntKind::Object(obj) => obj.subtype.type_mark(),
            AnyEntKind::InterfaceFile(file_type) => *file_type,
            _ => {
                unreachable!();
            }
        }
    }

    pub fn base_type(&self) -> TypeEnt<'a> {
        self.type_mark().base_type()
    }
}

impl<'a> std::ops::Deref for InterfaceEnt<'a> {
    type Target = AnyEnt<'a>;
    fn deref(&self) -> EntRef<'a> {
        self.ent
    }
}

/// The formal region is an ordered list of interface elements such as ports, generics and subprogram arguments
#[derive(Clone)]
pub struct FormalRegion<'a> {
    pub typ: InterfaceListType,
    entities: Vec<InterfaceEnt<'a>>,
}

impl<'a> FormalRegion<'a> {
    pub fn new(typ: InterfaceListType) -> Self {
        Self {
            typ,
            entities: Default::default(),
        }
    }

    pub fn new_params() -> Self {
        Self {
            typ: InterfaceListType::Parameter,
            entities: Default::default(),
        }
    }

    pub fn new_with(typ: InterfaceListType, entities: Vec<InterfaceEnt<'a>>) -> Self {
        Self { typ, entities }
    }
    pub fn lookup(
        &self,
        pos: &SrcPos,
        designator: &Designator,
    ) -> Result<(usize, InterfaceEnt<'a>), Diagnostic> {
        for (idx, ent) in self.entities.iter().enumerate() {
            if ent.designator() == designator {
                return Ok((idx, *ent));
            }
        }
        Err(Diagnostic::error(
            pos,
            format!("No declaration of '{}'", designator),
        ))
    }

    pub fn is_empty(&self) -> bool {
        self.entities.is_empty()
    }

    pub fn len(&self) -> usize {
        self.entities.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = InterfaceEnt<'a>> + '_ {
        self.entities.iter().cloned()
    }

    pub fn add(&mut self, param: EntRef<'a>) {
        if let Some(ent) = InterfaceEnt::from_any(param) {
            self.entities.push(ent);
        } else {
            debug_assert!(false);
        }
    }

    pub fn nth(&self, idx: usize) -> Option<InterfaceEnt<'a>> {
        self.entities.get(idx).cloned()
    }

    pub fn binary(&self) -> Option<(InterfaceEnt, InterfaceEnt<'a>)> {
        let left = self.nth(0)?;
        let right = self.nth(1)?;
        Some((left, right))
    }
}

/// The formal region is an ordered list of interface elements such as ports, generics and subprogram arguments
#[derive(Default, Clone)]
pub struct RecordRegion<'a> {
    elems: Vec<RecordElement<'a>>,
}

impl<'a> RecordRegion<'a> {
    pub fn lookup(&self, designator: &Designator) -> Option<RecordElement<'a>> {
        self.elems
            .iter()
            .find(|ent| ent.designator() == designator)
            .cloned()
    }

    pub fn is_empty(&self) -> bool {
        self.elems.is_empty()
    }

    pub fn len(&self) -> usize {
        self.elems.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = RecordElement<'a>> + '_ {
        self.elems.iter().cloned()
    }

    pub fn add(&mut self, ent: &'a AnyEnt) {
        if let Some(elem) = RecordElement::from_any(ent) {
            self.elems.push(elem);
        } else {
            debug_assert!(false);
        }
    }

    pub fn nth(&self, idx: usize) -> Option<&RecordElement<'a>> {
        self.elems.get(idx)
    }
}

#[derive(Clone, Copy)]
pub struct RecordElement<'a> {
    // A record element declaration
    ent: EntRef<'a>,
}

impl<'a> RecordElement<'a> {
    pub fn from_any(ent: &'a AnyEnt) -> Option<Self> {
        if let AnyEntKind::ElementDeclaration(_) = ent.kind() {
            Some(RecordElement { ent })
        } else {
            None
        }
    }

    pub fn type_mark(&self) -> TypeEnt<'a> {
        match self.ent.kind() {
            AnyEntKind::ElementDeclaration(subtype) => subtype.type_mark(),
            _ => {
                unreachable!();
            }
        }
    }
}

impl<'a> std::ops::Deref for RecordElement<'a> {
    type Target = AnyEnt<'a>;
    fn deref(&self) -> EntRef<'a> {
        self.ent
    }
}

impl<'a> From<RecordElement<'a>> for EntRef<'a> {
    fn from(value: RecordElement<'a>) -> Self {
        value.ent
    }
}
