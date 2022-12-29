// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2022, Olof Kraigher olof.kraigher@gmail.com

use std::sync::Arc;

use crate::{
    ast::{Designator, InterfaceListType, Mode, ObjectClass},
    Diagnostic, SrcPos,
};

use super::{
    region::{NamedEntityKind, Object, TypeEnt},
    NamedEntity,
};

#[derive(Clone)]
pub struct InterfaceEnt {
    /// InterfaceObject or InterfaceFile
    ent: Arc<NamedEntity>,
}

impl InterfaceEnt {
    pub fn inner(&self) -> &Arc<NamedEntity> {
        &self.ent
    }

    pub fn from_any(ent: Arc<NamedEntity>) -> Option<Self> {
        match ent.kind() {
            NamedEntityKind::Object(Object { mode: Some(_), .. })
            | NamedEntityKind::InterfaceFile(..) => Some(InterfaceEnt { ent }),
            _ => None,
        }
    }

    pub fn has_default(&self) -> bool {
        if let NamedEntityKind::Object(Object { has_default, .. }) = self.ent.kind() {
            *has_default
        } else {
            false
        }
    }

    pub fn is_signal(&self) -> bool {
        match self.ent.kind() {
            NamedEntityKind::Object(obj) => obj.class == ObjectClass::Signal,
            _ => false,
        }
    }

    pub fn is_output_signal(&self) -> bool {
        match self.ent.kind() {
            NamedEntityKind::Object(obj) => {
                obj.class == ObjectClass::Signal && obj.mode == Some(Mode::Out)
            }
            _ => false,
        }
    }

    pub fn type_mark(&self) -> &TypeEnt {
        match self.ent.kind() {
            NamedEntityKind::Object(obj) => obj.subtype.type_mark(),
            NamedEntityKind::InterfaceFile(file_type) => file_type,
            _ => {
                unreachable!();
            }
        }
    }

    pub fn base_type(&self) -> &TypeEnt {
        self.type_mark().base_type()
    }
}

impl std::ops::Deref for InterfaceEnt {
    type Target = NamedEntity;
    fn deref(&self) -> &NamedEntity {
        &self.ent
    }
}

/// The formal region is an ordered list of interface elements such as ports, generics and subprogram arguments
#[derive(Clone)]
pub struct FormalRegion {
    pub typ: InterfaceListType,
    entities: Vec<InterfaceEnt>,
}

impl FormalRegion {
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

    pub fn new_with(typ: InterfaceListType, entities: Vec<InterfaceEnt>) -> Self {
        Self { typ, entities }
    }
    pub fn lookup(
        &self,
        pos: &SrcPos,
        designator: &Designator,
    ) -> Result<(usize, InterfaceEnt), Diagnostic> {
        for (idx, ent) in self.entities.iter().enumerate() {
            if ent.designator() == designator {
                return Ok((idx, ent.clone()));
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

    pub fn iter(&self) -> impl Iterator<Item = &InterfaceEnt> {
        self.entities.iter()
    }

    pub fn add(&mut self, param: Arc<NamedEntity>) {
        if let Some(ent) = InterfaceEnt::from_any(param) {
            self.entities.push(ent);
        } else {
            debug_assert!(false);
        }
    }

    pub fn nth(&self, idx: usize) -> Option<&InterfaceEnt> {
        self.entities.get(idx)
    }
}
