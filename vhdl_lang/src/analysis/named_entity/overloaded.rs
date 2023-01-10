//! This Source Code Form is subject to the terms of the Mozilla Public
//! License, v. 2.0. If a copy of the MPL was not distributed with this file,
//! You can obtain one at http://mozilla.org/MPL/2.0/.
//!
//! Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use std::sync::Arc;

use crate::analysis::formal_region::FormalRegion;

use super::EntityId;
use super::NamedEntity;
use super::NamedEntityKind;
use super::TypeEnt;

#[derive(Clone)]
pub struct Signature {
    /// Vector of InterfaceObject or InterfaceFile
    pub params: FormalRegion,
    return_type: Option<TypeEnt>,
}

impl Signature {
    pub fn new(params: FormalRegion, return_type: Option<TypeEnt>) -> Signature {
        Signature {
            params,
            return_type: return_type.as_ref().map(TypeEnt::to_owned),
        }
    }

    pub fn key(&self) -> SignatureKey {
        let params = self
            .params
            .iter()
            .map(|param| param.base_type().id())
            .collect();
        let return_type = self.return_type.as_ref().map(|ent| ent.base_type().id());

        SignatureKey {
            params,
            return_type,
        }
    }

    pub fn describe(&self) -> String {
        let mut result = String::new();
        result.push('[');
        for (i, param) in self.params.iter().enumerate() {
            result.push_str(&param.type_mark().designator().to_string());

            if i + 1 < self.params.len() {
                result.push_str(", ");
            }
        }

        if !self.params.is_empty() && self.return_type.is_some() {
            result.push(' ');
        }

        if let Some(ref return_type) = self.return_type {
            result.push_str("return ");
            result.push_str(&return_type.designator().to_string());
        }

        result.push(']');
        result
    }

    /// Returns true if the function has no arguments
    /// or all arguments have defaults
    pub fn can_be_called_without_parameters(&self) -> bool {
        self.params.iter().all(|param| param.has_default())
    }

    pub fn can_be_called_with_single_parameter(&self, typ: &TypeEnt) -> bool {
        let mut params = self.params.iter();
        if let Some(first) = params.next() {
            if params.all(|param| param.has_default()) {
                return first.base_type() == typ.base_type();
            }
        }
        false
    }

    pub fn return_type(&self) -> Option<&TypeEnt> {
        self.return_type.as_ref()
    }

    pub fn match_return_type(&self, typ: Option<&TypeEnt>) -> bool {
        self.return_type().map(|ent| ent.base_type()) == typ.map(|ent| ent.base_type())
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct SignatureKey {
    params: Vec<EntityId>,
    return_type: Option<EntityId>,
}

impl SignatureKey {
    pub fn new(params: Vec<EntityId>, return_type: Option<EntityId>) -> SignatureKey {
        SignatureKey {
            params,
            return_type,
        }
    }
}

#[derive(Clone, Debug)]
pub struct OverloadedEnt {
    ent: Arc<NamedEntity>,
}

impl OverloadedEnt {
    pub fn from_any(ent: Arc<NamedEntity>) -> Result<Self, Arc<NamedEntity>> {
        match ent.actual_kind() {
            NamedEntityKind::Subprogram(..)
            | NamedEntityKind::SubprogramDecl(..)
            | NamedEntityKind::EnumLiteral(..) => Ok(OverloadedEnt { ent }),
            _ => Err(ent),
        }
    }

    pub fn signature(&self) -> &Signature {
        match self.actual_kind() {
            NamedEntityKind::Subprogram(ref signature)
            | NamedEntityKind::SubprogramDecl(ref signature)
            | NamedEntityKind::EnumLiteral(ref signature) => signature,
            _ => unreachable!(),
        }
    }

    pub fn return_type(&self) -> Option<&TypeEnt> {
        self.signature().return_type()
    }

    pub fn is_procedure(&self) -> bool {
        self.return_type().is_none()
    }

    pub fn is_function(&self) -> bool {
        self.return_type().is_some()
    }

    pub fn formals(&self) -> &FormalRegion {
        &self.signature().params
    }

    pub fn inner(&self) -> &Arc<NamedEntity> {
        &self.ent
    }
}

impl std::ops::Deref for OverloadedEnt {
    type Target = NamedEntity;
    fn deref(&self) -> &NamedEntity {
        &self.ent
    }
}

impl From<OverloadedEnt> for Arc<NamedEntity> {
    fn from(value: OverloadedEnt) -> Self {
        value.ent
    }
}
