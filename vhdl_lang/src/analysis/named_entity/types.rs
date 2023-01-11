//! This Source Code Form is subject to the terms of the Mozilla Public
//! License, v. 2.0. If a copy of the MPL was not distributed with this file,
//! You can obtain one at http://mozilla.org/MPL/2.0/.
//!
//! Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use std::ops::Deref;
use std::sync::Arc;

use crate::analysis::analyze::AnalysisResult;
use crate::analysis::named_entity::{EntityId, NamedEntity, NamedEntityKind};

use crate::analysis::formal_region::RecordRegion;
use crate::analysis::implicits::ImplicitVec;
use crate::analysis::region::{NamedEntities, Region};
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
    Incomplete(ArcSwapWeak<NamedEntity>),
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
    pub fn implicit_declarations(&self) -> impl Iterator<Item = Arc<NamedEntity>> + '_ {
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
pub struct TypeEnt(Arc<NamedEntity>);

impl TypeEnt {
    pub fn define_with_opt_id(
        id: Option<EntityId>,
        ident: &mut WithDecl<Ident>,
        kind: Type,
    ) -> TypeEnt {
        let ent = Arc::new(NamedEntity {
            id: id.unwrap_or_else(EntityId::new),
            implicit_of: None,
            decl_pos: Some(ident.tree.pos.clone()),
            designator: ident.tree.item.clone().into(),
            kind: NamedEntityKind::Type(kind),
        });
        ident.decl = Some(ent.clone());
        TypeEnt(ent)
    }

    pub fn from_any(ent: Arc<NamedEntity>) -> Result<TypeEnt, Arc<NamedEntity>> {
        if matches!(ent.kind(), NamedEntityKind::Type(..)) {
            Ok(TypeEnt(ent))
        } else {
            Err(ent)
        }
    }

    pub fn from_any_ref(ent: &Arc<NamedEntity>) -> Option<TypeEnt> {
        if matches!(ent.kind(), NamedEntityKind::Type(..)) {
            Some(TypeEnt(ent.clone()))
        } else {
            None
        }
    }

    pub fn kind(&self) -> &Type {
        if let NamedEntityKind::Type(typ) = self.0.kind() {
            typ
        } else {
            unreachable!("Must be a type");
        }
    }

    // Flatten all aliases
    pub fn flatten_alias(&self) -> &TypeEnt {
        if let Type::Alias(alias) = self.kind() {
            alias.flatten_alias()
        } else {
            self
        }
    }

    pub fn base_type(&self) -> &TypeEnt {
        let actual = self.flatten_alias();
        match actual.kind() {
            Type::Subtype(ref subtype) => subtype.base_type(),
            _ => actual,
        }
    }

    /// Lookup a selected name prefix.suffix
    /// where prefix has this type
    pub fn selected(
        &self,
        prefix_pos: &SrcPos,
        suffix: &WithPos<WithRef<Designator>>,
    ) -> AnalysisResult<NamedEntities> {
        match self.kind() {
            Type::Record(ref region, _) => {
                if let Some(decl) = region.lookup(suffix.designator()) {
                    Ok(NamedEntities::Single(decl.clone().into()))
                } else {
                    Err(
                        Diagnostic::no_declaration_within(self, &suffix.pos, &suffix.item.item)
                            .into(),
                    )
                }
            }
            Type::Protected(region, _) => {
                if let Some(decl) = region.lookup_immediate(suffix.designator()) {
                    Ok(decl.clone())
                } else {
                    Err(
                        Diagnostic::no_declaration_within(self, &suffix.pos, &suffix.item.item)
                            .into(),
                    )
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
                    )
                    .into())
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
            | Type::Real { .. } => {
                Err(Diagnostic::invalid_selected_name_prefix(self, prefix_pos).into())
            }
        }
    }
}

impl From<TypeEnt> for Arc<NamedEntity> {
    fn from(ent: TypeEnt) -> Self {
        ent.0
    }
}

impl std::cmp::PartialEq for TypeEnt {
    fn eq(&self, other: &Self) -> bool {
        self.deref() == other.deref()
    }
}

impl AsRef<Arc<NamedEntity>> for TypeEnt {
    fn as_ref(&self) -> &Arc<NamedEntity> {
        &self.0
    }
}

impl Deref for TypeEnt {
    type Target = NamedEntity;
    fn deref(&self) -> &NamedEntity {
        let val: &Arc<NamedEntity> = self.0.borrow();
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
        let flat = self.type_mark.flatten_alias();
        match flat.kind() {
            Type::Subtype(ref subtype) => subtype.base_type(),
            _ => flat.base_type(),
        }
    }
}
