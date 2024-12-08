//! This Source Code Form is subject to the terms of the Mozilla Public
//! License, v. 2.0. If a copy of the MPL was not distributed with this file,
//! You can obtain one at http://mozilla.org/MPL/2.0/.
//!
//! Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com
use super::*;
use crate::ast::Designator;
use std::fmt::{Debug, Formatter};

#[derive(Clone)]
pub enum Overloaded<'a> {
    /// A subprogram declaration (meaning: without implementation).
    SubprogramDecl(Signature<'a>),
    /// A subprogram with an associated implementation.
    Subprogram(Signature<'a>),
    /// An uninstantiated subprogram (i.e., a subprogram that contains
    /// generics) without implementation.
    UninstSubprogramDecl(Signature<'a>, Region<'a>),
    /// An uninstantiated subprogram with implementation.
    UninstSubprogram(Signature<'a>, Region<'a>),
    /// A subprogram that was declared as part of a generic.
    InterfaceSubprogram(Signature<'a>),
    /// An enum literal.
    EnumLiteral(Signature<'a>),
    /// An alias of an overloaded entity.
    Alias(OverloadedEnt<'a>),
}

impl Debug for Overloaded<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Overloaded::UninstSubprogramDecl(..) => {
                write!(f, "uninstantiated subprogram declaration")
            }
            Overloaded::UninstSubprogram(..) => write!(f, "uninstantiated subprogram"),
            Overloaded::SubprogramDecl(..) => write!(f, "subprogram declaration"),
            Overloaded::Subprogram(..) => write!(f, "subprogram"),
            Overloaded::InterfaceSubprogram(_) => write!(f, "interface subprogram"),
            Overloaded::EnumLiteral(_) => write!(f, "enum literal"),
            Overloaded::Alias(_) => write!(f, "alias"),
        }
    }
}

impl<'a> Overloaded<'a> {
    pub fn describe(&self) -> &'static str {
        use Overloaded::*;
        match self {
            SubprogramDecl(signature)
            | Subprogram(signature)
            | UninstSubprogramDecl(signature, _)
            | UninstSubprogram(signature, _)
            | InterfaceSubprogram(signature) => {
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

    pub fn signature(&'a self) -> &'a Signature<'a> {
        match self {
            Overloaded::InterfaceSubprogram(ref signature)
            | Overloaded::Subprogram(ref signature)
            | Overloaded::SubprogramDecl(ref signature)
            | Overloaded::UninstSubprogram(ref signature, _)
            | Overloaded::UninstSubprogramDecl(ref signature, _)
            | Overloaded::EnumLiteral(ref signature) => signature,
            Overloaded::Alias(ref overloaded) => overloaded.signature(),
        }
    }
}

#[derive(Clone)]
pub struct Signature<'a> {
    /// Vector of InterfaceObject or InterfaceFile
    pub(crate) formals: FormalRegion<'a>,
    pub(crate) return_type: Option<TypeEnt<'a>>,
}

impl<'a> Signature<'a> {
    pub fn new(formals: FormalRegion<'a>, return_type: Option<TypeEnt<'a>>) -> Signature<'a> {
        Signature {
            formals,
            return_type: return_type.as_ref().map(TypeEnt::to_owned),
        }
    }

    pub fn key(&self) -> SignatureKey<'a> {
        let formals = self.formals.iter().map(|formal| formal.base()).collect();
        let return_type = self.return_type.as_ref().map(|ent| ent.base());

        SignatureKey {
            formals,
            return_type,
        }
    }

    pub fn describe(&self) -> String {
        describe_signature(
            self.formals
                .iter()
                .map(|formal| EntRef::from(formal.type_mark()).designator()),
            self.return_type.map(|rt| EntRef::from(rt).designator()),
        )
    }

    /// Returns true if the function has no arguments
    /// or all arguments have defaults
    pub fn can_be_called_without_actuals(&self) -> bool {
        self.formals.iter().all(|formal| formal.has_default())
    }

    pub fn formals_without_defaults(&self) -> impl Iterator<Item = InterfaceEnt<'a>> + '_ {
        self.formals.iter().filter(|formal| !formal.has_default())
    }

    pub fn can_be_called_with_single_parameter(&self, typ: TypeEnt<'a>) -> bool {
        let mut formals = self.formals.iter();
        if let Some(first) = formals.next() {
            if formals.all(|formal| formal.has_default()) {
                return first.base_type() == typ.base_type();
            }
        }
        false
    }

    pub fn return_type(&self) -> Option<TypeEnt<'a>> {
        self.return_type
    }

    pub fn match_return_type(&self, typ: Option<TypeEnt<'a>>) -> bool {
        self.return_type().map(|ent| ent.base_type()) == typ.map(|ent| ent.base_type())
    }
}

pub fn describe_signature<'a>(
    formals: impl ExactSizeIterator<Item = &'a Designator>,
    return_type: Option<&'a Designator>,
) -> String {
    use std::fmt::Write;

    let mut result = String::new();
    result.push('[');
    let num_formals = formals.len();
    for (i, formal) in formals.enumerate() {
        write!(result, "{}", &formal).unwrap();

        if i + 1 < num_formals {
            result.push_str(", ");
        }
    }

    if num_formals > 0 && return_type.is_some() {
        result.push(' ');
    }

    if let Some(ref return_type) = return_type {
        write!(result, "return {}", &return_type).unwrap();
    }

    result.push(']');
    result
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct SignatureKey<'a> {
    pub formals: Vec<BaseType<'a>>,
    pub return_type: Option<BaseType<'a>>,
}

/// An Uninstantiated key is that of an uninstantiated subprogram
/// or an uninstantiated subprogram declaration.
/// Everything else is a Normal key.
/// This indirection is introduced because uninstantiated subprograms can have
/// the same signature as regular subprograms.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum SubprogramKey<'a> {
    Normal(SignatureKey<'a>),
    Uninstantiated(SignatureKey<'a>),
}

impl<'a> SubprogramKey<'a> {
    pub fn map(self, map: impl Fn(BaseType<'a>) -> BaseType<'a>) -> Self {
        match self {
            SubprogramKey::Normal(sig) => SubprogramKey::Normal(sig.map(map)),
            SubprogramKey::Uninstantiated(sig) => SubprogramKey::Uninstantiated(sig.map(map)),
        }
    }

    pub(crate) fn key(&self) -> &SignatureKey<'a> {
        match self {
            SubprogramKey::Normal(key) => key,
            SubprogramKey::Uninstantiated(key) => key,
        }
    }
}

impl<'a> SignatureKey<'a> {
    pub fn new(formals: Vec<BaseType<'a>>, return_type: Option<BaseType<'a>>) -> SignatureKey<'a> {
        SignatureKey {
            formals,
            return_type,
        }
    }

    pub fn map(mut self, map: impl Fn(BaseType<'a>) -> BaseType<'a>) -> Self {
        for formal in self.formals.iter_mut() {
            *formal = map(*formal);
        }
        if let Some(ref mut return_type) = self.return_type {
            *return_type = map(*return_type);
        }
        self
    }

    pub fn describe(&self) -> String {
        describe_signature(
            self.formals
                .iter()
                .map(|formal| EntRef::from(*formal).designator()),
            self.return_type.map(|rt| EntRef::from(rt).designator()),
        )
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct OverloadedEnt<'a> {
    pub ent: EntRef<'a>,
}

impl<'a> OverloadedEnt<'a> {
    pub fn from_any(ent: EntRef<'a>) -> Option<Self> {
        if let AnyEntKind::Overloaded(..) = ent.actual_kind() {
            Some(OverloadedEnt { ent })
        } else {
            None
        }
    }

    pub fn subprogram_key(&self) -> SubprogramKey<'a> {
        let key = self.signature().key();
        if self.is_uninst_subprogram() {
            SubprogramKey::Uninstantiated(key)
        } else {
            SubprogramKey::Normal(key)
        }
    }

    pub fn kind(&self) -> &'a Overloaded<'a> {
        if let AnyEntKind::Overloaded(kind) = self.ent.actual_kind() {
            kind
        } else {
            unreachable!();
        }
    }

    pub fn designator(&self) -> &'a Designator {
        self.ent.designator()
    }

    pub fn signature(&self) -> &'a Signature<'a> {
        self.kind().signature()
    }

    pub fn return_type(&self) -> Option<TypeEnt<'a>> {
        self.signature().return_type()
    }

    pub fn is_procedure(&self) -> bool {
        self.return_type().is_none()
    }

    pub fn is_function(&self) -> bool {
        self.return_type().is_some()
    }

    pub fn formals(&self) -> &'a FormalRegion<'a> {
        &self.signature().formals
    }

    /// Base type of nth formal
    pub fn nth_base(&self, idx: usize) -> Option<BaseType<'a>> {
        let ent = self.formals().nth(idx)?;
        Some(ent.type_mark().base())
    }

    pub fn describe(&self) -> String {
        let prefix = match self.kind() {
            Overloaded::SubprogramDecl(_)
            | Overloaded::Subprogram(_)
            | Overloaded::UninstSubprogramDecl(..)
            | Overloaded::UninstSubprogram(..)
            | Overloaded::InterfaceSubprogram(_) => {
                if matches!(self.designator(), Designator::OperatorSymbol(_)) {
                    "operator "
                } else if self.is_function() {
                    "function "
                } else {
                    "procedure "
                }
            }
            Overloaded::EnumLiteral(_) => "",
            Overloaded::Alias(o) => return o.describe(),
        };

        format!(
            "{}{}{}",
            prefix,
            self.designator,
            self.signature().describe()
        )
    }
}

impl<'a> std::ops::Deref for OverloadedEnt<'a> {
    type Target = AnyEnt<'a>;
    fn deref(&self) -> EntRef<'a> {
        self.ent
    }
}

impl<'a> From<OverloadedEnt<'a>> for EntRef<'a> {
    fn from(value: OverloadedEnt<'a>) -> Self {
        value.ent
    }
}
