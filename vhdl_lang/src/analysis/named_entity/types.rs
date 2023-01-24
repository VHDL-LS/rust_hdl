//! This Source Code Form is subject to the terms of the Mozilla Public
//! License, v. 2.0. If a copy of the MPL was not distributed with this file,
//! You can obtain one at http://mozilla.org/MPL/2.0/.
//!
//! Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use std::ops::Deref;

use crate::analysis::named_entity::{AnyEnt, AnyEntKind, EntityId};

use crate::analysis::formal_region::{RecordElement, RecordRegion};
use crate::analysis::implicits::ImplicitVec;
use crate::analysis::region::{NamedEntities, OverloadedName, Region};
use crate::ast::WithDecl;
use crate::ast::{Designator, WithRef};
use crate::ast::{HasDesignator, Ident};
use crate::data::WithPos;
use crate::{Diagnostic, SrcPos};

use arc_swap::ArcSwapOption;
use fnv::FnvHashSet;

use super::{Arena, EntRef};

pub enum Type<'a> {
    // Some types have an optional list of implicit declarations
    // Use Weak reference since implicit declaration typically reference the type itself
    Array {
        implicit: ImplicitVec<'a>,
        // Indexes are Option<> to handle unknown types
        indexes: Vec<Option<TypeEnt<'a>>>,
        elem_type: TypeEnt<'a>,
    },
    Enum(ImplicitVec<'a>, FnvHashSet<Designator>),
    Integer(ImplicitVec<'a>),
    Real(ImplicitVec<'a>),
    Physical(ImplicitVec<'a>),
    Access(Subtype<'a>, ImplicitVec<'a>),
    Record(RecordRegion<'a>, ImplicitVec<'a>),
    // Incomplete type will be overwritten when full type is found
    Incomplete,
    Subtype(Subtype<'a>),
    // The region of the protected type which needs to be extendend by the body
    Protected(Region<'a>, ArcSwapOption<SrcPos>),
    File(ImplicitVec<'a>),
    Interface,
    Alias(TypeEnt<'a>),
    Universal(UniversalType, ImplicitVec<'a>),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum UniversalType {
    Real,
    Integer,
}

impl<'a> Type<'a> {
    pub fn implicit_declarations(&self) -> impl Iterator<Item = EntRef<'a>> + '_ {
        self.implicits().into_iter().flat_map(|imp| imp.iter())
    }

    pub fn implicits(&self) -> Option<&ImplicitVec<'a>> {
        match self {
            Type::Array { ref implicit, .. } => Some(implicit),
            Type::Enum(ref implicit, _) => Some(implicit),
            Type::Real(ref implicit) => Some(implicit),
            Type::Integer(ref implicit) => Some(implicit),
            Type::Physical(ref implicit) => Some(implicit),
            Type::File(ref implicit) => Some(implicit),
            Type::Access(.., ref implicit) => Some(implicit),
            Type::Record(.., ref implicit) => Some(implicit),
            Type::Universal(.., ref implicit) => Some(implicit),
            Type::Incomplete
            | Type::Interface
            | Type::Protected(..)
            | Type::Subtype(..)
            | Type::Alias(..) => None,
        }
    }

    pub fn describe(&self) -> &'static str {
        match self {
            Type::Alias(..) => "alias",
            Type::Record(..) => "record type",
            Type::Array { .. } => "array type",
            Type::Enum(..) => "type",
            Type::Integer(..) => "integer type",
            Type::Real(..) => "real type",
            Type::Physical(..) => "physical type",
            Type::Access(..) => "access type",
            Type::Subtype(..) => "subtype",
            Type::Incomplete => "type",
            Type::Interface => "type",
            Type::File(..) => "file type",
            Type::Protected(..) => "protected type",
            Type::Universal(univ, _) => univ.describe(),
        }
    }
}

impl UniversalType {
    pub fn describe(&self) -> &'static str {
        match self {
            UniversalType::Integer => "universal_integer",
            UniversalType::Real => "universal_real",
        }
    }
}

// A named entity that is known to be a type
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct TypeEnt<'a>(EntRef<'a>);

impl<'a> TypeEnt<'a> {
    pub fn define_with_opt_id(
        arena: &'a Arena,
        id: Option<EntityId>,
        ident: &mut WithDecl<Ident>,
        kind: Type<'a>,
    ) -> TypeEnt<'a> {
        let ent = if let Some(id) = id {
            unsafe {
                arena.update(
                    id,
                    ident.tree.item.clone().into(),
                    None,
                    AnyEntKind::Type(kind),
                    Some(ident.tree.pos.clone()),
                )
            }
        } else {
            arena.explicit(
                &ident.tree.item,
                AnyEntKind::Type(kind),
                Some(&ident.tree.pos),
            )
        };

        ident.decl = Some(ent.id());
        TypeEnt(ent)
    }

    pub fn from_any(ent: &'a AnyEnt) -> Option<TypeEnt<'a>> {
        if matches!(ent.kind(), AnyEntKind::Type(..)) {
            Some(TypeEnt(ent))
        } else {
            None
        }
    }

    pub fn kind(&self) -> &'a Type<'a> {
        if let AnyEntKind::Type(typ) = self.0.kind() {
            typ
        } else {
            unreachable!("Must be a type");
        }
    }

    pub fn base_type(&self) -> TypeEnt<'a> {
        match self.kind() {
            Type::Alias(alias) => alias.base_type(),
            Type::Subtype(subtype) => subtype.base_type(),
            _ => *self,
        }
    }

    pub fn base(&self) -> BaseType<'a> {
        BaseType::from(*self)
    }

    pub fn accessed_type(&self) -> Option<TypeEnt<'a>> {
        if let Type::Access(subtype, _) = self.base_type().kind() {
            Some(subtype.type_mark())
        } else {
            None
        }
    }

    pub fn array_type(&self) -> Option<(TypeEnt<'a>, usize)> {
        if let Type::Array {
            elem_type, indexes, ..
        } = self.base_type().kind()
        {
            Some((*elem_type, indexes.len()))
        } else if let Some(accessed_typ) = self.accessed_type() {
            accessed_typ.array_type()
        } else {
            None
        }
    }

    pub fn sliced_as(&self) -> Option<TypeEnt<'a>> {
        self.base().sliced_as()
    }

    /// Lookup a selected name prefix.suffix
    /// where prefix has this type
    pub fn selected(
        self,
        prefix_pos: &SrcPos,
        suffix: &WithPos<WithRef<Designator>>,
    ) -> Result<TypedSelection<'a>, Diagnostic> {
        match self.kind() {
            Type::Record(ref region, _) => {
                if let Some(decl) = region.lookup(suffix.designator()) {
                    Ok(TypedSelection::RecordElement(decl))
                } else {
                    Err(Diagnostic::no_declaration_within(
                        &self,
                        &suffix.pos,
                        &suffix.item.item,
                    ))
                }
            }
            Type::Protected(region, _) => {
                if let Some(decl) = region.lookup_immediate(suffix.designator()) {
                    match decl {
                        NamedEntities::Single(ent) => Err(Diagnostic::error(
                            &suffix.pos,
                            format!(
                                "Protected type selection must be a method, got {}",
                                ent.describe()
                            ),
                        )),
                        NamedEntities::Overloaded(overloaded) => {
                            Ok(TypedSelection::ProtectedMethod(overloaded.clone()))
                        }
                    }
                } else {
                    Err(Diagnostic::no_declaration_within(
                        &self,
                        &suffix.pos,
                        &suffix.item.item,
                    ))
                }
            }
            Type::Incomplete => Err(Diagnostic::error(
                prefix_pos,
                "Cannot select incomplete type before full type definition",
            )),
            Type::Subtype(subtype) => subtype.type_mark().selected(prefix_pos, suffix),
            Type::Access(subtype, ..) => subtype.type_mark().selected(prefix_pos, suffix),
            Type::Alias(alias) => alias.selected(prefix_pos, suffix),
            Type::Array { .. }
            | Type::File { .. }
            | Type::Interface { .. }
            | Type::Enum { .. }
            | Type::Physical { .. }
            | Type::Universal { .. }
            | Type::Integer { .. }
            | Type::Real { .. } => Err(Diagnostic::invalid_selected_name_prefix(&self, prefix_pos)),
        }
    }

    // @TODO used to skip things from instantiated packages which we cannot handle yet
    pub fn is_generic(&self) -> bool {
        matches!(self.base_type().kind(), Type::Interface)
    }
}

impl<'a> From<TypeEnt<'a>> for EntRef<'a> {
    fn from(ent: TypeEnt<'a>) -> Self {
        ent.0
    }
}

impl<'a> Deref for TypeEnt<'a> {
    type Target = AnyEnt<'a>;
    fn deref(&self) -> &AnyEnt<'a> {
        self.0
    }
}

// A named entity that is known to be a base type
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct BaseType<'a>(EntRef<'a>);

impl<'a> BaseType<'a> {
    pub fn kind(&self) -> &'a Type<'a> {
        if let AnyEntKind::Type(typ) = self.0.kind() {
            typ
        } else {
            unreachable!("Must be a type");
        }
    }

    pub fn is_access(&self) -> bool {
        matches!(self.kind(), Type::Access { .. })
    }

    pub fn is_composite(&self) -> bool {
        matches!(self.kind(), Type::Array { .. } | Type::Record { .. })
    }

    pub fn is_enum(&self) -> bool {
        matches!(self.kind(), Type::Enum { .. })
    }

    pub fn is_compatible_with_string_literal(&self) -> bool {
        if let Type::Array {
            indexes, elem_type, ..
        } = self.kind()
        {
            indexes.len() == 1 && elem_type.base().is_enum()
        } else {
            false
        }
    }

    pub fn sliced_as(&self) -> Option<TypeEnt<'a>> {
        let typ = if let Type::Access(ref subtype, ..) = self.kind() {
            subtype.type_mark()
        } else {
            TypeEnt(self.0)
        };

        if matches!(typ.base().kind(), Type::Array { .. }) {
            Some(typ)
        } else {
            None
        }
    }
}

impl<'a> From<BaseType<'a>> for EntRef<'a> {
    fn from(ent: BaseType<'a>) -> Self {
        ent.0
    }
}

impl<'a> From<TypeEnt<'a>> for BaseType<'a> {
    fn from(ent: TypeEnt<'a>) -> Self {
        BaseType(ent.base_type().0)
    }
}

impl<'a> From<BaseType<'a>> for TypeEnt<'a> {
    fn from(ent: BaseType<'a>) -> Self {
        TypeEnt(ent.0)
    }
}

impl<'a> Deref for BaseType<'a> {
    type Target = AnyEnt<'a>;
    fn deref(&self) -> &AnyEnt<'a> {
        self.0
    }
}

#[derive(Clone)]
pub struct Subtype<'a> {
    type_mark: TypeEnt<'a>,
}

impl<'a> Subtype<'a> {
    pub fn new(type_mark: TypeEnt<'a>) -> Subtype<'a> {
        Subtype { type_mark }
    }

    pub fn type_mark(&self) -> TypeEnt<'a> {
        self.type_mark
    }

    pub fn base_type(&self) -> TypeEnt<'a> {
        self.type_mark.base_type()
    }

    pub fn base(&self) -> BaseType<'a> {
        self.type_mark.base()
    }
}

/// The result of selecting an object
pub enum TypedSelection<'a> {
    RecordElement(RecordElement<'a>),
    ProtectedMethod(OverloadedName<'a>),
}

impl<'a> TypedSelection<'a> {
    pub fn into_any(self) -> NamedEntities<'a> {
        match self {
            TypedSelection::RecordElement(elem) => NamedEntities::Single(elem.into()),
            TypedSelection::ProtectedMethod(overloaded) => NamedEntities::Overloaded(overloaded),
        }
    }
}
