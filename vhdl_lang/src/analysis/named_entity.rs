// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2022, Olof Kraigher olof.kraigher@gmail.com

use super::formal_region::FormalRegion;
use super::region::Region;
use crate::ast::ExternalObjectClass;
use crate::ast::{
    AnyPrimaryUnit, Designator, HasIdent, Ident, ObjectClass, SubprogramDeclaration, WithDecl,
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
    LoopParameter(Option<BaseType<'a>>),
    PhysicalLiteral(TypeEnt<'a>),
    DeferredConstant(Subtype<'a>),
    Library,
    Design(Design<'a>),
}

impl<'a> AnyEntKind<'a> {
    pub(crate) fn new_function_decl(
        formals: FormalRegion<'a>,
        return_type: TypeEnt<'a>,
    ) -> AnyEntKind<'a> {
        AnyEntKind::Overloaded(Overloaded::SubprogramDecl(Signature::new(
            formals,
            Some(return_type),
        )))
    }

    pub(crate) fn new_procedure_decl(formals: FormalRegion<'a>) -> AnyEntKind<'a> {
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
            LoopParameter(_) => "loop parameter",
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

impl<'a> std::fmt::Debug for AnyEnt<'a> {
    // We need a custom debug implementation for AnyEnt to avoid stack overflow on circular references
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let AnyEnt {
            id,
            parent,
            related,
            implicits,
            designator,
            kind,
            decl_pos,
        } = self;

        let mut s = f.debug_struct(stringify!(AnyEnt));
        s.field(stringify!(id), id);
        s.field(stringify!(parent), &parent.is_some());
        s.field(stringify!(related), related);
        s.field(stringify!(implicits), &implicits.len());
        s.field(stringify!(designator), designator);
        s.field(stringify!(kind), kind);
        s.field(stringify!(decl_pos), decl_pos);
        s.finish()
    }
}

pub type EntRef<'a> = &'a AnyEnt<'a>;

#[derive(Debug, Copy, Clone)]
pub enum Related<'a> {
    ImplicitOf(EntRef<'a>),
    InstanceOf(EntRef<'a>),
    DeclaredBy(EntRef<'a>),
    None,
}

/// A named entity as defined in LRM 6.1.
///
/// Every declaration creates one or more named entities.
pub struct AnyEnt<'a> {
    /// A unique id of the entity.
    /// Entities with the same id will be the same.
    pub id: EntityId,
    pub parent: Option<EntRef<'a>>,
    pub related: Related<'a>,
    pub implicits: Vec<EntRef<'a>>,
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
        self.alloc(
            designator.into(),
            of_ent.parent,
            Related::ImplicitOf(of_ent),
            kind,
            decl_pos.cloned(),
        )
    }

    pub fn define<'a, T: HasIdent>(
        &'a self,
        decl: &mut WithDecl<T>,
        parent: Option<EntRef<'a>>,
        kind: AnyEntKind<'a>,
    ) -> EntRef<'a> {
        let ent = self.explicit(
            decl.tree.name().clone(),
            parent,
            kind,
            Some(decl.tree.pos()),
        );
        decl.decl = Some(ent.id());
        ent
    }

    pub fn explicit<'a>(
        &'a self,
        designator: impl Into<Designator>,
        parent: Option<EntRef<'a>>,
        kind: AnyEntKind<'a>,
        decl_pos: Option<&SrcPos>,
    ) -> EntRef<'a> {
        self.alloc(
            designator.into(),
            parent,
            Related::None,
            kind,
            decl_pos.cloned(),
        )
    }
}

impl<'a> AnyEnt<'a> {
    pub fn id(&self) -> EntityId {
        self.id
    }

    pub fn declaration(&'a self) -> EntRef<'a> {
        if let Related::DeclaredBy(other) = self.related {
            other
        } else {
            self
        }
    }

    pub fn is_implicit(&self) -> bool {
        match self.related {
            Related::ImplicitOf(_) => true,
            Related::InstanceOf(ent) => ent.is_implicit(),
            Related::DeclaredBy(_) => false,
            Related::None => false,
        }
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

    pub fn is_protected_type(&self) -> bool {
        matches!(
            self.kind,
            AnyEntKind::Type(Type::Protected(_, is_body)) if !is_body
        )
    }

    pub fn is_protected_type_body(&self) -> bool {
        matches!(
            self.kind,
            AnyEntKind::Type(Type::Protected(_, is_body)) if is_body
        )
    }

    pub fn is_declared_by(&self, other: EntRef) -> bool {
        if let Related::DeclaredBy(ent) = self.related {
            if ent.id() == other.id() {
                return true;
            }
        }

        false
    }

    pub fn is_explicit(&self) -> bool {
        !self.is_implicit()
    }

    pub fn decl_pos(&self) -> Option<&SrcPos> {
        self.decl_pos.as_ref()
    }

    pub fn library_name(&self) -> Option<&Symbol> {
        if let AnyEntKind::Library = self.kind() {
            if let Designator::Identifier(symbol) = self.designator() {
                return Some(symbol);
            }
        }

        if let Some(parent) = self.parent {
            parent.library_name()
        } else {
            None
        }
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

    pub(crate) fn add_implicit(&mut self, ent: EntRef<'a>) {
        self.implicits.push(ent);
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

            AnyEntKind::Type(Type::Universal(typ)) => format!("type {}", typ.describe().to_owned()),
            _ => format!("{} '{}'", self.kind.describe(), self.designator),
        }
    }

    #[cfg(test)]
    pub fn lookup_implicit_of(&self, name: &str) -> OverloadedEnt<'a> {
        let ent = self
            .implicits
            .iter()
            .find(|ent| matches!(ent.designator(), Designator::Identifier(ident) if ident.name_utf8() == name))
            .unwrap();
        OverloadedEnt::from_any(ent).unwrap()
    }

    #[allow(clippy::mut_from_ref)]
    unsafe fn unsafe_ref_mut(&self) -> &mut Self {
        let mut_self: *mut AnyEnt = self as *const AnyEnt as *mut AnyEnt;
        &mut *mut_self
    }

    // Used to update the kind of pre-declared symbols that are visible before they have been fully analyzed
    pub(crate) unsafe fn set_kind(&self, kind: AnyEntKind) {
        unsafe {
            self.unsafe_ref_mut().kind = kind;
        }
    }

    pub(crate) unsafe fn set_declared_by(&self, ent: EntRef<'a>) {
        unsafe {
            self.unsafe_ref_mut().related = Related::DeclaredBy(ent);
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
    pub fn define<'a>(
        &mut self,
        arena: &'a Arena,
        parent: Option<EntRef<'a>>,
        kind: AnyEntKind<'a>,
    ) -> EntRef<'a> {
        let ent = arena.explicit(
            self.tree.name().clone(),
            parent,
            kind,
            Some(self.tree.pos()),
        );
        self.decl = Some(ent.id());
        ent
    }
}

impl WithDecl<WithPos<Designator>> {
    pub fn define<'a>(
        &mut self,
        arena: &'a Arena,
        parent: Option<EntRef<'a>>,
        kind: AnyEntKind<'a>,
    ) -> EntRef<'a> {
        let ent = arena.explicit(self.tree.item.clone(), parent, kind, Some(&self.tree.pos));
        self.decl = Some(ent.id());
        ent
    }
}

impl SubprogramDeclaration {
    pub fn set_decl_id(&mut self, id: EntityId) {
        match self {
            SubprogramDeclaration::Function(f) => f.designator.decl = Some(id),
            SubprogramDeclaration::Procedure(p) => p.designator.decl = Some(id),
        }
    }
}
