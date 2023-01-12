//! This Source Code Form is subject to the terms of the Mozilla Public
//! License, v. 2.0. If a copy of the MPL was not distributed with this file,
//! You can obtain one at http://mozilla.org/MPL/2.0/.
//!
//! Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use std::ops::Deref;
use std::sync::Arc;

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
use arc_swap::ArcSwapWeak;
use fnv::FnvHashSet;
use std::borrow::Borrow;

pub enum Type {
    // Some types have an optional list of implicit declarations
    // Use Weak reference since implicit declaration typically reference the type itself
    Array {
        implicit: ImplicitVec,
        // Indexes are Option<> to handle unknown types
        indexes: Vec<Option<TypeEnt>>,
        elem_type: TypeEnt,
    },
    Enum(ImplicitVec, FnvHashSet<Designator>),
    Integer(ImplicitVec),
    Real(ImplicitVec),
    Physical(ImplicitVec),
    Access(Subtype, ImplicitVec),
    Record(RecordRegion, ImplicitVec),
    // Weak references since incomplete access types can create cycles
    // The reference is for the full type which is filled in after creation
    Incomplete(ArcSwapWeak<AnyEnt>),
    Subtype(Subtype),
    // The region of the protected type which needs to be extendend by the body
    Protected(Region, ArcSwapOption<SrcPos>),
    File(ImplicitVec),
    Interface,
    Alias(TypeEnt),
    Universal(UniversalType, ImplicitVec),
}

#[derive(Copy, Clone, Debug)]
pub enum UniversalType {
    Real,
    Integer,
}

impl Type {
    pub fn implicit_declarations(&self) -> impl Iterator<Item = Arc<AnyEnt>> + '_ {
        self.implicits().into_iter().flat_map(|imp| imp.iter())
    }

    pub fn implicits(&self) -> Option<&ImplicitVec> {
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
            Type::Incomplete(..)
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
            Type::Incomplete(..) => "type",
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
#[derive(Clone, Debug)]
pub struct TypeEnt(Arc<AnyEnt>);

impl TypeEnt {
    pub fn define_with_opt_id(
        id: Option<EntityId>,
        ident: &mut WithDecl<Ident>,
        kind: Type,
    ) -> TypeEnt {
        let ent = Arc::new(AnyEnt {
            id: id.unwrap_or_else(EntityId::new),
            implicit_of: None,
            decl_pos: Some(ident.tree.pos.clone()),
            designator: ident.tree.item.clone().into(),
            kind: AnyEntKind::Type(kind),
        });
        ident.decl = Some(ent.clone());
        TypeEnt(ent)
    }

    pub fn from_any(ent: Arc<AnyEnt>) -> Result<TypeEnt, Arc<AnyEnt>> {
        if matches!(ent.kind(), AnyEntKind::Type(..)) {
            Ok(TypeEnt(ent))
        } else {
            Err(ent)
        }
    }

    pub fn from_any_ref(ent: &Arc<AnyEnt>) -> Option<TypeEnt> {
        if matches!(ent.kind(), AnyEntKind::Type(..)) {
            Some(TypeEnt(ent.clone()))
        } else {
            None
        }
    }

    pub fn kind(&self) -> &Type {
        if let AnyEntKind::Type(typ) = self.0.kind() {
            typ
        } else {
            unreachable!("Must be a type");
        }
    }

    pub fn base_type(&self) -> &TypeEnt {
        match self.kind() {
            Type::Alias(alias) => alias.base_type(),
            Type::Subtype(subtype) => subtype.base_type(),
            _ => self,
        }
    }

    /// Lookup a selected name prefix.suffix
    /// where prefix has this type
    pub fn selected(
        &self,
        prefix_pos: &SrcPos,
        suffix: &WithPos<WithRef<Designator>>,
    ) -> Result<TypedSelection, Diagnostic> {
        match self.kind() {
            Type::Record(ref region, _) => {
                if let Some(decl) = region.lookup(suffix.designator()) {
                    Ok(TypedSelection::RecordElement(decl.clone()))
                } else {
                    Err(Diagnostic::no_declaration_within(
                        self,
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
                        self,
                        &suffix.pos,
                        &suffix.item.item,
                    ))
                }
            }
            Type::Incomplete(full_type_ref) => {
                if let Some(full_type) = full_type_ref
                    .load()
                    .upgrade()
                    .and_then(|e| TypeEnt::from_any(e).ok())
                {
                    full_type.selected(prefix_pos, suffix)
                } else {
                    Err(Diagnostic::error(
                        prefix_pos,
                        "Internal error when referencing full type of incomplete type",
                    ))
                }
            }
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
            | Type::Real { .. } => Err(Diagnostic::invalid_selected_name_prefix(self, prefix_pos)),
        }
    }
}

impl From<TypeEnt> for Arc<AnyEnt> {
    fn from(ent: TypeEnt) -> Self {
        ent.0
    }
}

impl std::cmp::PartialEq for TypeEnt {
    fn eq(&self, other: &Self) -> bool {
        self.deref() == other.deref()
    }
}

impl AsRef<Arc<AnyEnt>> for TypeEnt {
    fn as_ref(&self) -> &Arc<AnyEnt> {
        &self.0
    }
}

impl Deref for TypeEnt {
    type Target = AnyEnt;
    fn deref(&self) -> &AnyEnt {
        let val: &Arc<AnyEnt> = self.0.borrow();
        val.as_ref()
    }
}

#[derive(Clone)]
pub struct Subtype {
    type_mark: TypeEnt,
}

impl Subtype {
    pub fn new(type_mark: TypeEnt) -> Subtype {
        Subtype { type_mark }
    }

    pub fn type_mark(&self) -> &TypeEnt {
        &self.type_mark
    }

    pub fn base_type(&self) -> &TypeEnt {
        self.type_mark.base_type()
    }
}

/// The result of selecting an object
pub enum TypedSelection {
    RecordElement(RecordElement),
    ProtectedMethod(OverloadedName),
}

impl TypedSelection {
    pub fn into_any(self) -> NamedEntities {
        match self {
            TypedSelection::RecordElement(elem) => NamedEntities::Single(elem.into()),
            TypedSelection::ProtectedMethod(overloaded) => NamedEntities::Overloaded(overloaded),
        }
    }
}
