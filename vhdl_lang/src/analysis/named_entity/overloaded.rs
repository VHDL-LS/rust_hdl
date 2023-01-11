//! This Source Code Form is subject to the terms of the Mozilla Public
//! License, v. 2.0. If a copy of the MPL was not distributed with this file,
//! You can obtain one at http://mozilla.org/MPL/2.0/.
//!
//! Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use std::sync::Arc;

use crate::analysis::formal_region::FormalRegion;

use super::AnyEnt;
use super::AnyEntKind;
use super::EntityId;
use super::TypeEnt;

pub enum Overloaded {
    SubprogramDecl(Signature),
    Subprogram(Signature),
    EnumLiteral(Signature),
    Alias(OverloadedEnt),
}

impl Overloaded {
    pub fn describe(&self) -> &'static str {
        use Overloaded::*;
        match self {
            SubprogramDecl(signature) | Subprogram(signature) => {
                if signature.return_type().is_some() {
                    "function"
                } else {
                    "procedure"
                }
            }
            EnumLiteral(..) => "enum literal",
            Alias(..) => "alias",
        }
    }

    pub fn signature(&self) -> &Signature {
        match self {
            Overloaded::Subprogram(ref signature)
            | Overloaded::SubprogramDecl(ref signature)
            | Overloaded::EnumLiteral(ref signature) => signature,
            Overloaded::Alias(ref overloaded) => overloaded.signature(),
        }
    }
}

#[derive(Clone)]
pub struct Signature {
    /// Vector of InterfaceObject or InterfaceFile
    pub formals: FormalRegion,
    return_type: Option<TypeEnt>,
}

impl Signature {
    pub fn new(formals: FormalRegion, return_type: Option<TypeEnt>) -> Signature {
        Signature {
            formals,
            return_type: return_type.as_ref().map(TypeEnt::to_owned),
        }
    }

    pub fn key(&self) -> SignatureKey {
        let formals = self
            .formals
            .iter()
            .map(|formal| formal.base_type().id())
            .collect();
        let return_type = self.return_type.as_ref().map(|ent| ent.base_type().id());

        SignatureKey {
            formals,
            return_type,
        }
    }

    pub fn describe(&self) -> String {
        let mut result = String::new();
        result.push('[');
        for (i, formal) in self.formals.iter().enumerate() {
            result.push_str(&formal.type_mark().designator().to_string());

            if i + 1 < self.formals.len() {
                result.push_str(", ");
            }
        }

        if !self.formals.is_empty() && self.return_type.is_some() {
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
        self.formals.iter().all(|formal| formal.has_default())
    }

    pub fn can_be_called_with_single_parameter(&self, typ: &TypeEnt) -> bool {
        let mut formals = self.formals.iter();
        if let Some(first) = formals.next() {
            if formals.all(|formal| formal.has_default()) {
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
    formals: Vec<EntityId>,
    return_type: Option<EntityId>,
}

impl SignatureKey {
    pub fn new(formals: Vec<EntityId>, return_type: Option<EntityId>) -> SignatureKey {
        SignatureKey {
            formals,
            return_type,
        }
    }
}

#[derive(Clone, Debug)]
pub struct OverloadedEnt {
    ent: Arc<AnyEnt>,
}

impl OverloadedEnt {
    pub fn from_any(ent: Arc<AnyEnt>) -> Result<Self, Arc<AnyEnt>> {
        if let AnyEntKind::Overloaded(..) = ent.actual_kind() {
            Ok(OverloadedEnt { ent })
        } else {
            Err(ent)
        }
    }

    pub fn kind(&self) -> &Overloaded {
        if let AnyEntKind::Overloaded(kind) = self.ent.actual_kind() {
            kind
        } else {
            unreachable!();
        }
    }

    pub fn signature(&self) -> &Signature {
        self.kind().signature()
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
        &self.signature().formals
    }

    pub fn inner(&self) -> &Arc<AnyEnt> {
        &self.ent
    }
}

impl std::ops::Deref for OverloadedEnt {
    type Target = AnyEnt;
    fn deref(&self) -> &AnyEnt {
        &self.ent
    }
}

impl From<OverloadedEnt> for Arc<AnyEnt> {
    fn from(value: OverloadedEnt) -> Self {
        value.ent
    }
}
