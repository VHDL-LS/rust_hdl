// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2022, Olof Kraigher olof.kraigher@gmail.com

use super::*;
use crate::{
    ast::{Designator, InterfaceType, Mode},
    Diagnostic, SrcPos,
};
use itertools::Itertools;
use std::ops::Deref;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum InterfaceEnt<'a> {
    Type(TypeEnt<'a>),
    Object(ObjectEnt<'a>),
    File(FileEnt<'a>),
    Subprogram(OverloadedEnt<'a>),
    Package(DesignEnt<'a>),
}

impl<'a> InterfaceEnt<'a> {
    pub fn inner(&self) -> EntRef<'a> {
        match self {
            InterfaceEnt::Type(typ) => typ.inner(),
            InterfaceEnt::File(typ) => typ.inner(),
            InterfaceEnt::Object(obj) => obj.inner(),
            InterfaceEnt::Subprogram(overloaded) => overloaded.inner(),
            InterfaceEnt::Package(pkg) => pkg.inner(),
        }
    }

    pub fn from_any(ent: EntRef<'a>) -> Option<Self> {
        match ent.kind() {
            AnyEntKind::Object(Object { iface: Some(_), .. }) => {
                Some(InterfaceEnt::Object(ObjectEnt::from_any(ent).unwrap()))
            }
            AnyEntKind::InterfaceFile(_) => {
                Some(InterfaceEnt::File(FileEnt::from_any(ent).unwrap()))
            }
            AnyEntKind::Design(Design::InterfacePackageInstance(_)) => {
                Some(InterfaceEnt::Package(DesignEnt::from_any(ent).unwrap()))
            }
            AnyEntKind::Type(Type::Interface) => {
                Some(InterfaceEnt::Type(TypeEnt::from_any(ent).unwrap()))
            }
            AnyEntKind::Overloaded(Overloaded::InterfaceSubprogram(_)) => Some(
                InterfaceEnt::Subprogram(OverloadedEnt::from_any(ent).unwrap()),
            ),
            _ => None,
        }
    }

    pub fn has_default(&self) -> bool {
        match self {
            InterfaceEnt::Object(o) => o.kind().has_default,
            _ => false,
        }
    }

    pub fn is_signal(&self) -> bool {
        matches!(self, InterfaceEnt::Object(obj) if obj.is_signal())
    }

    pub fn is_out_or_inout_signal(&self) -> bool {
        match self {
            InterfaceEnt::Object(obj) => {
                obj.class() == ObjectClass::Signal
                    && matches!(
                        obj.mode(),
                        Some(InterfaceMode::Simple(Mode::Out | Mode::InOut))
                    )
            }
            _ => false,
        }
    }

    pub fn type_mark(&self) -> Option<TypeEnt<'a>> {
        match self {
            InterfaceEnt::Object(obj) => Some(obj.type_mark()),
            InterfaceEnt::File(file_type) => Some(file_type.type_mark()),
            _ => None,
        }
    }
}

impl<'a> std::ops::Deref for InterfaceEnt<'a> {
    type Target = AnyEnt<'a>;
    fn deref(&self) -> EntRef<'a> {
        self.inner()
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum GpkgInterfaceEnt<'a> {
    Type(TypeEnt<'a>),
    Constant(ObjectEnt<'a>),
    Subprogram(OverloadedEnt<'a>),
    Package(DesignEnt<'a>),
}

impl<'a> From<GpkgInterfaceEnt<'a>> for InterfaceEnt<'a> {
    fn from(value: GpkgInterfaceEnt<'a>) -> Self {
        match value {
            GpkgInterfaceEnt::Type(typ) => InterfaceEnt::Type(typ),
            GpkgInterfaceEnt::Constant(obj) => InterfaceEnt::Object(obj),
            GpkgInterfaceEnt::Subprogram(ovlded) => InterfaceEnt::Subprogram(ovlded),
            GpkgInterfaceEnt::Package(pkg) => InterfaceEnt::Package(pkg),
        }
    }
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
            AnyEntKind::Design(Design::InterfacePackageInstance(_)) => {
                Some(GpkgInterfaceEnt::Package(DesignEnt::from_any(ent).unwrap()))
            }
            _ => None,
        }
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
        Err(Diagnostic::new(
            pos,
            format!("No declaration of '{designator}'"),
            ErrorCode::Unresolved,
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
            println!("kind: {:?}", param);
            debug_assert!(false);
        }
    }

    pub fn nth(&self, idx: usize) -> Option<InterfaceEnt<'a>> {
        self.entities.get(idx).cloned()
    }

    pub fn ref_from_param_ref(parameter_region: &'a ParameterRegion<'a>) -> &'a FormalRegion<'a> {
        &parameter_region.0
    }
}

#[derive(Clone, Debug)]
pub struct ParameterEnt<'a>(EntRef<'a>);

impl<'a> ParameterEnt<'a> {
    pub fn into_inner(self) -> EntRef<'a> {
        self.0
    }

    pub fn is_parameter(ent: EntRef<'a>) -> bool {
        Self::from_any(ent).is_some()
    }

    pub fn from_any(ent: EntRef<'a>) -> Option<ParameterEnt<'a>> {
        match ent.kind() {
            AnyEntKind::Object(Object { iface: Some(_), .. }) | AnyEntKind::InterfaceFile(_) => {
                Some(ParameterEnt(ent))
            }
            _ => None,
        }
    }

    pub fn type_mark(&self) -> TypeEnt<'a> {
        match self.0.kind() {
            AnyEntKind::Object(obj) => obj.subtype.type_mark(),
            AnyEntKind::InterfaceFile(file_type) => *file_type,
            _ => {
                unreachable!("unexpected type {:?}", self.0.kind());
            }
        }
    }

    pub fn base_type(&self) -> TypeEnt<'a> {
        self.type_mark().base_type()
    }

    pub fn base(&self) -> BaseType<'a> {
        self.type_mark().base()
    }

    pub fn has_default(&self) -> bool {
        if let AnyEntKind::Object(Object { has_default, .. }) = self.0.kind() {
            *has_default
        } else {
            false
        }
    }
}

impl<'a> From<ParameterEnt<'a>> for InterfaceEnt<'a> {
    fn from(value: ParameterEnt<'a>) -> Self {
        InterfaceEnt::from_any(value.0).expect("Any ParameterEnt should be an InterfaceEnt")
    }
}

impl<'a> Deref for ParameterEnt<'a> {
    type Target = AnyEnt<'a>;
    fn deref(&self) -> EntRef<'a> {
        self.0
    }
}

#[derive(Clone)]
#[repr(transparent)]
pub struct ParameterRegion<'a>(FormalRegion<'a>);

impl Default for ParameterRegion<'_> {
    fn default() -> Self {
        ParameterRegion(FormalRegion::new(InterfaceType::Parameter))
    }
}

impl<'a> ParameterRegion<'a> {
    pub fn new(entities: Vec<ParameterEnt<'a>>) -> ParameterRegion<'a> {
        ParameterRegion(FormalRegion::new_with(
            InterfaceType::Parameter,
            entities.into_iter().map(|ent| ent.into()).collect(),
        ))
    }

    pub fn from_formal_region(
        formal_region: FormalRegion<'a>,
    ) -> Result<ParameterRegion<'a>, Vec<InterfaceEnt<'a>>> {
        let invalid_formals = formal_region
            .entities
            .iter()
            .filter(|ent| !ParameterEnt::is_parameter(ent))
            .copied()
            .collect_vec();
        if invalid_formals.is_empty() {
            Ok(ParameterRegion(formal_region))
        } else {
            Err(invalid_formals)
        }
    }

    pub fn iter(&self) -> impl ExactSizeIterator<Item = ParameterEnt<'a>> + '_ {
        self.0.iter().map(|ent| ParameterEnt(ent.inner()))
    }

    pub fn nth(&self, idx: usize) -> Option<ParameterEnt<'a>> {
        self.0.nth(idx).map(|ent| ParameterEnt(ent.inner()))
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub(crate) fn add(&mut self, ent: EntRef<'a>) {
        assert!(ParameterEnt::is_parameter(ent));
        self.0.add(ent)
    }
}

impl<'a> From<ParameterRegion<'a>> for FormalRegion<'a> {
    fn from(value: ParameterRegion<'a>) -> Self {
        value.0
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

    pub fn add(&mut self, ent: EntRef<'a>) {
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

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct RecordElement<'a> {
    // A record element declaration
    pub ent: EntRef<'a>,
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
