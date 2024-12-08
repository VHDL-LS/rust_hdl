//! This Source Code Form is subject to the terms of the Mozilla Public
//! License, v. 2.0. If a copy of the MPL was not distributed with this file,
//! You can obtain one at http://mozilla.org/MPL/2.0/.
//!
//! Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use std::ops::Deref;

use super::*;
use crate::ast::{Designator, HasDesignator, Ident, WithDecl, WithRef};
use crate::Diagnostic;

use fnv::FnvHashSet;

use super::{Arena, EntRef, Related};

#[derive(Clone)]
pub enum Type<'a> {
    // Some types have an optional list of implicit declarations
    // Use Weak reference since implicit declaration typically reference the type itself
    Array {
        // Indexes are Option<> to handle unknown types
        indexes: Vec<Option<BaseType<'a>>>,
        elem_type: TypeEnt<'a>,
    },
    Enum(FnvHashSet<Designator>),
    Integer,
    Real,
    Physical,
    Access(Subtype<'a>),
    Record(RecordRegion<'a>),
    // Incomplete type will be overwritten when full type is found
    Incomplete,
    Subtype(Subtype<'a>),
    // The region of the protected type which needs to be extended by the body
    Protected(Region<'a>, bool),
    File,
    Interface,
    Alias(TypeEnt<'a>),
    Universal(UniversalType),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum UniversalType {
    Real,
    Integer,
}

impl Type<'_> {
    pub fn describe(&self) -> &'static str {
        match self {
            Type::Alias(..) => "alias",
            Type::Record(..) => "record type",
            Type::Array { .. } => "array type",
            Type::Enum(..) => "type",
            Type::Integer => "integer type",
            Type::Real => "real type",
            Type::Physical => "physical type",
            Type::Access(..) => "access type",
            Type::Subtype(..) => "subtype",
            Type::Incomplete => "type",
            Type::Interface => "type",
            Type::File => "file type",
            Type::Protected(..) => "protected type",
            Type::Universal(univ) => univ.describe(),
        }
    }

    pub fn is_scalar(&self) -> bool {
        use Type::*;
        matches!(self, Enum(_) | Integer | Real | Physical | Universal(_))
    }

    pub fn is_discrete(&self) -> bool {
        use Type::*;
        matches!(
            self,
            Integer | Enum(_) | Universal(UniversalType::Integer) | Physical
        )
    }

    pub fn is_physical(&self) -> bool {
        use Type::*;
        matches!(self, Physical)
    }

    pub fn is_array(&self) -> bool {
        use Type::*;
        matches!(self, Array { .. })
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
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TypeEnt<'a>(EntRef<'a>);

impl<'a> TypeEnt<'a> {
    #[allow(clippy::too_many_arguments)]
    pub fn define_with_opt_id(
        ctx: &dyn TokenAccess,
        arena: &'a Arena,
        id: Option<EntityId>,
        ident: &mut WithDecl<Ident>,
        parent: EntRef<'a>,
        declared_by: Option<EntRef<'a>>,
        kind: Type<'a>,
        src_span: TokenSpan,
        source: Source,
    ) -> TypeEnt<'a> {
        let related = if let Some(declared_by) = declared_by {
            Related::DeclaredBy(declared_by)
        } else {
            Related::None
        };

        let ent = if let Some(id) = id {
            unsafe {
                arena.update(
                    id,
                    ident.tree.item.clone().into(),
                    Some(parent),
                    related,
                    AnyEntKind::Type(kind),
                    Some(ident.pos(ctx).clone()),
                    src_span,
                    Some(source),
                )
            }
        } else {
            arena.alloc(
                ident.tree.item.clone().into(),
                Some(parent),
                related,
                AnyEntKind::Type(kind),
                Some(ident.pos(ctx).clone()),
                src_span,
                Some(source),
            )
        };

        ident.decl.set(ent.id());
        TypeEnt(ent)
    }

    pub fn from_any(ent: EntRef<'a>) -> Option<TypeEnt<'a>> {
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
            Type::Protected(.., is_body) if *is_body => {
                // Never use the protected type body in signatures, use the non-body
                if let Related::DeclaredBy(other) = self.related {
                    if let Some(typ) = TypeEnt::from_any(other) {
                        typ
                    } else {
                        *self
                    }
                } else {
                    *self
                }
            }
            _ => *self,
        }
    }

    pub fn base(&self) -> BaseType<'a> {
        BaseType::from(*self)
    }

    pub fn accessed_type(&self) -> Option<TypeEnt<'a>> {
        self.base().accessed_type()
    }

    pub fn array_type(&self) -> Option<(TypeEnt<'a>, &'a Vec<Option<BaseType<'a>>>)> {
        self.base().array_type()
    }

    pub fn is_scalar(&self) -> bool {
        self.base().is_scalar()
    }

    pub fn sliced_as(&self) -> Option<TypeEnt<'a>> {
        self.base().sliced_as()
    }

    /// Returns whether this type denotes a record type.
    pub fn is_record(&self) -> bool {
        matches!(self.kind(), Type::Record(..))
    }

    /// Lookup a selected name prefix.suffix
    /// where prefix has this type
    pub fn selected(
        self,
        ctx: &dyn TokenAccess,
        prefix_pos: TokenSpan,
        suffix: &WithToken<WithRef<Designator>>,
    ) -> Result<TypedSelection<'a>, Diagnostic> {
        match self.kind() {
            Type::Record(ref region) => {
                if let Some(decl) = region.lookup(suffix.designator()) {
                    Ok(TypedSelection::RecordElement(decl))
                } else {
                    Err(Diagnostic::no_declaration_within(
                        &self,
                        suffix.pos(ctx),
                        &suffix.item.item,
                    ))
                }
            }
            Type::Protected(region, _) => {
                if let Some(decl) = region.lookup_immediate(suffix.designator()) {
                    match decl {
                        NamedEntities::Single(ent) => Err(Diagnostic::mismatched_kinds(
                            suffix.pos(ctx),
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
                        suffix.pos(ctx),
                        &suffix.item.item,
                    ))
                }
            }
            Type::Incomplete => Err(Diagnostic::new(
                prefix_pos.pos(ctx),
                "Cannot select incomplete type before full type definition",
                ErrorCode::MismatchedKinds,
            )),
            Type::Subtype(subtype) => subtype.type_mark().selected(ctx, prefix_pos, suffix),
            Type::Access(subtype, ..) => subtype.type_mark().selected(ctx, prefix_pos, suffix),
            Type::Alias(alias) => alias.selected(ctx, prefix_pos, suffix),
            Type::Array { .. }
            | Type::File { .. }
            | Type::Interface { .. }
            | Type::Enum { .. }
            | Type::Physical { .. }
            | Type::Universal { .. }
            | Type::Integer { .. }
            | Type::Real { .. } => Err(Diagnostic::invalid_selected_name_prefix(
                &self,
                &prefix_pos.pos(ctx),
            )),
        }
    }

    // @TODO used to skip things from instantiated packages which we cannot handle yet
    pub fn is_generic(&self) -> bool {
        matches!(self.base_type().kind(), Type::Interface)
    }

    pub fn describe(&self) -> String {
        if matches!(self.kind(), Type::Universal(_)) {
            format!("type {}", self.designator())
        } else {
            format!("{} '{}'", self.kind().describe(), self.designator())
        }
    }
}

impl<'a> From<TypeEnt<'a>> for EntRef<'a> {
    fn from(ent: TypeEnt<'a>) -> Self {
        ent.0
    }
}

impl<'a> From<BaseType<'a>> for EntRef<'a> {
    fn from(ent: BaseType<'a>) -> Self {
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

    pub fn is_any_integer(&self) -> bool {
        matches!(
            self.kind(),
            Type::Integer | Type::Universal(UniversalType::Integer)
        )
    }

    pub fn is_any_real(&self) -> bool {
        matches!(
            self.kind(),
            Type::Real | Type::Universal(UniversalType::Real)
        )
    }

    pub fn is_abstract(&self) -> bool {
        self.is_any_integer() || self.is_any_real()
    }

    pub fn is_scalar(&self) -> bool {
        self.kind().is_scalar()
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

    pub fn is_universal_of(&self, other: BaseType<'a>) -> bool {
        let i = matches!(self.kind(), Type::Universal(UniversalType::Integer))
            && matches!(other.kind(), Type::Integer);

        let r = matches!(self.kind(), Type::Universal(UniversalType::Real))
            && matches!(other.kind(), Type::Real);

        i || r
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

    pub fn array_type(&self) -> Option<(TypeEnt<'a>, &'a Vec<Option<BaseType<'a>>>)> {
        if let Type::Array {
            elem_type, indexes, ..
        } = self.kind()
        {
            Some((*elem_type, indexes))
        } else if let Some(accessed_typ) = self.accessed_type() {
            accessed_typ.array_type()
        } else {
            None
        }
    }

    pub fn accessed_type(&self) -> Option<TypeEnt<'a>> {
        if let Type::Access(subtype) = self.kind() {
            Some(subtype.type_mark())
        } else {
            None
        }
    }

    pub fn is_discrete(&self) -> bool {
        self.kind().is_discrete()
    }

    pub fn is_physical(&self) -> bool {
        self.kind().is_physical()
    }

    pub fn is_closely_related(&self, other: BaseType<'a>) -> bool {
        if self.id() == other.id() {
            return true;
        }

        if self.is_abstract() && other.is_abstract() {
            return true;
        }

        if let Type::Array {
            indexes: my_indexes,
            elem_type: my_elem_type,
        } = self.kind()
        {
            if let Type::Array {
                indexes: other_indexes,
                elem_type: other_elem_type,
            } = other.kind()
            {
                return my_indexes.len() == other_indexes.len()
                    && my_elem_type
                        .base()
                        .is_closely_related(other_elem_type.base());
            }
        }

        false
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

#[derive(Clone, Copy)]
pub struct Subtype<'a> {
    pub(crate) type_mark: TypeEnt<'a>,
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
