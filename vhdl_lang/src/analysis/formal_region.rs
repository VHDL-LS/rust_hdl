// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2022, Olof Kraigher olof.kraigher@gmail.com

use crate::{
    ast::{Designator, InterfaceType, Mode, ObjectClass},
    Diagnostic, SrcPos,
};

use super::named_entity::*;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
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
            AnyEntKind::Object(Object { iface: Some(_), .. }) | AnyEntKind::InterfaceFile(..) => {
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

    pub fn is_out_or_inout_signal(&self) -> bool {
        match self.ent.kind() {
            AnyEntKind::Object(obj) => {
                obj.class == ObjectClass::Signal
                    && matches!(obj.mode(), Some(Mode::Out) | Some(Mode::InOut))
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

    pub fn base(&self) -> BaseType<'a> {
        self.type_mark().base()
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
    pub(crate) typ: InterfaceType,
    pub(crate) entities: Vec<InterfaceEnt<'a>>,
}

impl<'a> FormalRegion<'a> {
    pub fn new(typ: InterfaceType) -> Self {
        Self {
            typ,
            entities: Default::default(),
        }
    }

    pub fn new_params() -> Self {
        Self {
            typ: InterfaceType::Parameter,
            entities: Default::default(),
        }
    }

    pub fn new_with(typ: InterfaceType, entities: Vec<InterfaceEnt<'a>>) -> Self {
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
            format!("No declaration of '{designator}'"),
        ))
    }

    pub fn is_empty(&self) -> bool {
        self.entities.is_empty()
    }

    pub fn len(&self) -> usize {
        self.entities.len()
    }

    pub fn iter(&self) -> impl ExactSizeIterator<Item = InterfaceEnt<'a>> + '_ {
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

    pub fn unary(&self) -> Option<InterfaceEnt<'a>> {
        if self.len() == 1 {
            self.nth(0)
        } else {
            None
        }
    }
}

/// The formal region is an ordered list of interface elements such as ports, generics and subprogram arguments
#[derive(Default, Clone)]
pub struct RecordRegion<'a> {
    pub(crate) elems: Vec<RecordElement<'a>>,
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
    pub fn from_any(ent: EntRef<'a>) -> Option<Self> {
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

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum GpkgInterfaceEnt<'a> {
    Type(TypeEnt<'a>),
    Constant(ObjectEnt<'a>),
    Subprogram(OverloadedEnt<'a>),
    Package(EntRef<'a>),
}

impl<'a> GpkgInterfaceEnt<'a> {
    pub fn from_any(ent: EntRef<'a>) -> Option<Self> {
        match ent.actual_kind() {
            AnyEntKind::Type(Type::Interface) => {
                Some(GpkgInterfaceEnt::Type(TypeEnt::from_any(ent).unwrap()))
            }
            AnyEntKind::Object(obj) if obj.is_generic() => Some(GpkgInterfaceEnt::Constant(
                ObjectEnt::from_any(ent).unwrap(),
            )),
            AnyEntKind::Overloaded(Overloaded::InterfaceSubprogram(_)) => Some(
                GpkgInterfaceEnt::Subprogram(OverloadedEnt::from_any(ent).unwrap()),
            ),
            AnyEntKind::Design(Design::PackageInstance(_)) => Some(GpkgInterfaceEnt::Package(ent)),
            _ => None,
        }
    }
}

impl<'a> std::ops::Deref for GpkgInterfaceEnt<'a> {
    type Target = AnyEnt<'a>;
    fn deref(&self) -> &Self::Target {
        match self {
            GpkgInterfaceEnt::Type(typ) => typ.deref(),
            GpkgInterfaceEnt::Constant(obj) => obj.deref(),
            GpkgInterfaceEnt::Subprogram(subp) => subp.deref(),
            GpkgInterfaceEnt::Package(ent) => ent.deref(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GpkgRegion<'a> {
    entities: Vec<GpkgInterfaceEnt<'a>>,
}

impl<'a> GpkgRegion<'a> {
    pub fn new(entities: Vec<GpkgInterfaceEnt<'a>>) -> Self {
        Self { entities }
    }

    pub fn lookup(
        &self,
        pos: &SrcPos,
        designator: &Designator,
    ) -> Result<(usize, GpkgInterfaceEnt<'a>), Diagnostic> {
        for (idx, ent) in self.entities.iter().enumerate() {
            if ent.designator() == designator {
                return Ok((idx, *ent));
            }
        }
        Err(Diagnostic::error(
            pos,
            format!("No declaration of '{designator}'"),
        ))
    }

    pub fn nth(&self, idx: usize) -> Option<GpkgInterfaceEnt<'a>> {
        self.entities.get(idx).cloned()
    }
}
