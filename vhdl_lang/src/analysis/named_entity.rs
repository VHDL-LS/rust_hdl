// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2022, Olof Kraigher olof.kraigher@gmail.com

use super::formal_region::FormalRegion;
use super::region::Region;
use crate::ast::ExternalObjectClass;
use crate::ast::{
    AnyPrimaryUnit, Designator, HasIdent, Ident, ObjectClass, SubprogramDeclaration,
    SubprogramDesignator, WithDecl,
};
use crate::data::*;

mod types;
pub use types::{BaseType, Subtype, Type, TypeEnt, TypedSelection, UniversalType};

mod overloaded;
pub use overloaded::{Overloaded, OverloadedEnt, Signature, SignatureKey};

mod object;
pub use object::{Object, ObjectEnt};

mod design;
pub use design::{Design, DesignEnt};

mod arena;
pub use arena::{Arena, ArenaId, EntityId, FinalArena};

pub enum AnyEntKind<'a> {
    ExternalAlias {
        class: ExternalObjectClass,
        type_mark: TypeEnt<'a>,
    },
    ObjectAlias {
        base_object: ObjectEnt<'a>,
        type_mark: TypeEnt<'a>,
    },
    File(Subtype<'a>),
    InterfaceFile(TypeEnt<'a>),
    Component(Region<'a>),
    Attribute(TypeEnt<'a>),
    Overloaded(Overloaded<'a>),
    Type(Type<'a>),
    ElementDeclaration(Subtype<'a>),
    Label,
    Object(Object<'a>),
    LoopParameter,
    PhysicalLiteral(TypeEnt<'a>),
    DeferredConstant(Subtype<'a>),
    Library,
    Design(Design<'a>),
}

impl<'a> AnyEntKind<'a> {
    pub fn new_function_decl(
        formals: FormalRegion<'a>,
        return_type: TypeEnt<'a>,
    ) -> AnyEntKind<'a> {
        AnyEntKind::Overloaded(Overloaded::SubprogramDecl(Signature::new(
            formals,
            Some(return_type),
        )))
    }

    pub fn new_procedure_decl(formals: FormalRegion<'a>) -> AnyEntKind<'a> {
        AnyEntKind::Overloaded(Overloaded::SubprogramDecl(Signature::new(formals, None)))
    }

    pub fn is_deferred_constant(&self) -> bool {
        matches!(self, AnyEntKind::DeferredConstant(..))
    }

    pub fn is_non_deferred_constant(&self) -> bool {
        matches!(
            self,
            AnyEntKind::Object(Object {
                class: ObjectClass::Constant,
                mode: None,
                ..
            })
        )
    }

    pub fn is_protected_type(&self) -> bool {
        matches!(self, AnyEntKind::Type(Type::Protected(..)))
    }

    pub fn is_type(&self) -> bool {
        matches!(self, AnyEntKind::Type(..))
    }

    pub fn implicit_declarations(&self) -> impl Iterator<Item = EntRef<'a>> + '_ {
        match self {
            AnyEntKind::Type(typ) => Some(typ.implicit_declarations()),
            _ => None,
        }
        .into_iter()
        .flatten()
    }

    pub fn describe(&self) -> &str {
        use AnyEntKind::*;
        match self {
            ObjectAlias { .. } => "object alias",
            ExternalAlias { .. } => "external alias",
            File(..) => "file",
            InterfaceFile(..) => "file",
            ElementDeclaration(..) => "element declaration",
            Component(..) => "component",
            Attribute(..) => "attribute",
            Overloaded(overloaded) => overloaded.describe(),
            Label => "label",
            LoopParameter => "loop parameter",
            Object(object) => object.class.describe(),
            PhysicalLiteral(..) => "physical literal",
            DeferredConstant(..) => "deferred constant",
            Library => "library",
            Design(design) => design.describe(),
            Type(typ) => typ.describe(),
        }
    }
}

impl<'a> std::fmt::Debug for AnyEntKind<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.describe())
    }
}

pub type EntRef<'a> = &'a AnyEnt<'a>;

/// A named entity as defined in LRM 6.1.
///
/// Every declaration creates one or more named entities.
#[derive(Debug)]
pub struct AnyEnt<'a> {
    /// A unique id of the entity.
    /// Entities with the same id will be the same.
    pub id: EntityId,
    pub implicit_of: Option<EntRef<'a>>,
    /// The location where the declaration was made.
    /// Builtin and implicit declaration will not have a source position.
    pub designator: Designator,
    pub kind: AnyEntKind<'a>,
    pub decl_pos: Option<SrcPos>,
}

impl Arena {
    pub fn implicit<'a>(
        &'a self,
        of_ent: EntRef<'a>,
        designator: impl Into<Designator>,
        kind: AnyEntKind<'a>,
        decl_pos: Option<&SrcPos>,
    ) -> EntRef<'a> {
        self.alloc(designator.into(), Some(of_ent), kind, decl_pos.cloned())
    }

    pub fn define<'a, T: HasIdent>(
        &'a self,
        decl: &mut WithDecl<T>,
        kind: AnyEntKind<'a>,
    ) -> EntRef<'a> {
        let ent = self.explicit(decl.tree.name().clone(), kind, Some(decl.tree.pos()));
        decl.decl = Some(ent.id());
        ent
    }

    pub fn explicit<'a>(
        &'a self,
        designator: impl Into<Designator>,
        kind: AnyEntKind<'a>,
        decl_pos: Option<&SrcPos>,
    ) -> EntRef<'a> {
        self.alloc(designator.into(), None, kind, decl_pos.cloned())
    }
}

impl<'a> AnyEnt<'a> {
    pub fn id(&self) -> EntityId {
        self.id
    }

    pub fn is_implicit(&self) -> bool {
        self.implicit_of.is_some()
    }

    pub fn is_subprogram(&self) -> bool {
        matches!(
            self.kind,
            AnyEntKind::Overloaded(Overloaded::Subprogram(..))
        )
    }

    pub fn is_subprogram_decl(&self) -> bool {
        matches!(
            self.kind,
            AnyEntKind::Overloaded(Overloaded::SubprogramDecl(..))
        )
    }

    pub fn is_explicit(&self) -> bool {
        self.implicit_of.is_none()
    }

    pub fn decl_pos(&self) -> Option<&SrcPos> {
        self.decl_pos.as_ref()
    }

    pub fn designator(&self) -> &Designator {
        &self.designator
    }

    pub fn kind(&self) -> &AnyEntKind {
        &self.kind
    }

    pub fn error(&self, diagnostics: &mut dyn DiagnosticHandler, message: impl Into<String>) {
        if let Some(ref pos) = self.decl_pos {
            diagnostics.push(Diagnostic::error(pos, message));
        }
    }

    pub fn is_overloaded(&self) -> bool {
        self.signature().is_some()
    }

    pub fn signature(&self) -> Option<&Signature> {
        match self.actual_kind() {
            AnyEntKind::Overloaded(ref overloaded) => Some(overloaded.signature()),
            _ => None,
        }
    }

    pub fn as_actual(&self) -> &AnyEnt {
        match self.kind() {
            AnyEntKind::Overloaded(Overloaded::Alias(ref ent)) => ent.as_actual(),
            AnyEntKind::Type(Type::Alias(ref ent)) => ent.as_actual(),
            _ => self,
        }
    }

    /// Strip aliases and return reference to actual entity kind
    pub fn actual_kind(&self) -> &AnyEntKind {
        self.as_actual().kind()
    }

    /// Returns true if self is alias of other
    pub fn is_alias_of(&self, other: &AnyEnt) -> bool {
        match self.kind() {
            AnyEntKind::Type(Type::Alias(ref ent)) => {
                if ent.id() == other.id() {
                    true
                } else {
                    ent.is_alias_of(other)
                }
            }
            _ => false,
        }
    }

    pub fn describe(&self) -> String {
        match self.kind {
            AnyEntKind::Object(Object {
                ref class,
                mode: Some(ref mode),
                ..
            }) => {
                if *class == ObjectClass::Constant {
                    format!("interface {} '{}'", class.describe(), self.designator,)
                } else {
                    format!(
                        "interface {} '{}' : {}",
                        class.describe(),
                        self.designator,
                        mode
                    )
                }
            }
            AnyEntKind::Overloaded(ref overloaded) => {
                format!("{}{}", self.designator, overloaded.signature().describe())
            }

            _ => format!("{} '{}'", self.kind.describe(), self.designator),
        }
    }
}

impl<'a> std::cmp::PartialEq for AnyEnt<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<'a> std::hash::Hash for AnyEnt<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id().hash(state)
    }
}

impl<'a> Eq for AnyEnt<'a> {}

/// This trait is implemented for Ast-nodes which declare named entities
pub trait HasEntityId {
    fn ent_id(&self) -> Option<EntityId>;
}

impl HasEntityId for AnyPrimaryUnit {
    fn ent_id(&self) -> Option<EntityId> {
        delegate_primary!(self, unit, unit.ident.decl)
    }
}

impl WithDecl<Ident> {
    pub fn define<'a>(&mut self, arena: &'a Arena, kind: AnyEntKind<'a>) -> EntRef<'a> {
        let ent = arena.explicit(self.tree.name().clone(), kind, Some(self.tree.pos()));
        self.decl = Some(ent.id());
        ent
    }
}

impl WithDecl<WithPos<SubprogramDesignator>> {
    pub fn define<'a>(&mut self, arena: &'a Arena, kind: AnyEntKind<'a>) -> EntRef<'a> {
        let ent = arena.explicit(
            self.tree.item.clone().into_designator(),
            kind,
            Some(&self.tree.pos),
        );
        self.decl = Some(ent.id());
        ent
    }
}

impl WithDecl<WithPos<Designator>> {
    pub fn define<'a>(&mut self, arena: &'a Arena, kind: AnyEntKind<'a>) -> EntRef<'a> {
        let ent = arena.explicit(self.tree.item.clone(), kind, Some(&self.tree.pos));
        self.decl = Some(ent.id());
        ent
    }
}

impl SubprogramDeclaration {
    pub fn define<'a>(&mut self, arena: &'a Arena, kind: AnyEntKind<'a>) -> EntRef<'a> {
        match self {
            SubprogramDeclaration::Function(f) => f.designator.define(arena, kind),
            SubprogramDeclaration::Procedure(p) => p.designator.define(arena, kind),
        }
    }
}
