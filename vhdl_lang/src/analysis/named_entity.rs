// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2022, Olof Kraigher olof.kraigher@gmail.com

use super::formal_region::FormalRegion;
use super::region::Region;
use crate::ast::{
    AnyPrimaryUnit, Designator, HasIdent, Ident, ObjectClass, SubprogramDeclaration,
    SubprogramDesignator, WithDecl,
};
use crate::ast::{ExternalObjectClass, Mode};
use crate::data::*;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

mod types;
pub use types::{Subtype, Type, TypeEnt, UniversalType};

mod overloaded;
pub use overloaded::{Overloaded, OverloadedEnt, Signature, SignatureKey};

mod object;
pub use object::ObjectEnt;

mod design;
pub use design::Design;

pub enum AnyEntKind {
    ExternalAlias {
        class: ExternalObjectClass,
        type_mark: TypeEnt,
    },
    ObjectAlias {
        base_object: ObjectEnt,
        type_mark: TypeEnt,
    },
    File(Subtype),
    InterfaceFile(TypeEnt),
    Component(Region),
    Attribute,
    Overloaded(Overloaded),
    Type(Type),
    ElementDeclaration(Subtype),
    Label,
    Object(Object),
    LoopParameter,
    PhysicalLiteral(TypeEnt),
    DeferredConstant(Subtype),
    Library,
    Design(Design),
}

impl AnyEntKind {
    pub fn new_function_decl(formals: FormalRegion, return_type: TypeEnt) -> AnyEntKind {
        AnyEntKind::Overloaded(Overloaded::SubprogramDecl(Signature::new(
            formals,
            Some(return_type),
        )))
    }

    pub fn new_procedure_decl(formals: FormalRegion) -> AnyEntKind {
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

    pub fn implicit_declarations(&self) -> impl Iterator<Item = Arc<AnyEnt>> + '_ {
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
            Attribute => "attribute",
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

impl std::fmt::Debug for AnyEntKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.describe())
    }
}

/// An object or an interface object,
/// example signal, variable, constant
/// Is either an object (mode = None) or an interface object (mode = Some)
#[derive(Clone)]
pub struct Object {
    pub class: ObjectClass,
    pub mode: Option<Mode>,
    pub subtype: Subtype,
    pub has_default: bool,
}

impl ObjectClass {
    fn describe(&self) -> &str {
        use ObjectClass::*;
        match self {
            Constant => "constant",
            Variable => "variable",
            Signal => "signal",
            SharedVariable => "shared variable",
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct EntityId {
    id: usize,
}

/// A named entity as defined in LRM 6.1.
///
/// Every declaration creates one or more named entities.
#[derive(Debug)]
pub struct AnyEnt {
    /// A unique id of the entity.
    /// Entities with the same id will be the same.
    id: EntityId,
    pub implicit_of: Option<Arc<AnyEnt>>,
    /// The location where the declaration was made.
    /// Builtin and implicit declaration will not have a source position.
    designator: Designator,
    kind: AnyEntKind,
    decl_pos: Option<SrcPos>,
}

impl AnyEnt {
    pub fn new(
        designator: impl Into<Designator>,
        kind: AnyEntKind,
        decl_pos: Option<&SrcPos>,
    ) -> AnyEnt {
        AnyEnt::new_with_id(EntityId::new(), designator.into(), kind, decl_pos.cloned())
    }

    pub fn new_with_id(
        id: EntityId,
        designator: Designator,
        kind: AnyEntKind,
        decl_pos: Option<SrcPos>,
    ) -> AnyEnt {
        AnyEnt {
            id,
            implicit_of: None,
            decl_pos,
            designator,
            kind,
        }
    }

    pub fn implicit(
        of_ent: Arc<AnyEnt>,
        designator: impl Into<Designator>,
        kind: AnyEntKind,
        decl_pos: Option<&SrcPos>,
    ) -> AnyEnt {
        AnyEnt {
            id: EntityId::new(),
            implicit_of: Some(of_ent),
            decl_pos: decl_pos.cloned(),
            designator: designator.into(),
            kind,
        }
    }

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

static UNIVERSAL_REAL_ID: EntityId = EntityId { id: 1 };
static UNIVERSAL_INTEGER_ID: EntityId = EntityId { id: 2 };
static COUNTER: AtomicUsize = AtomicUsize::new(3);

impl EntityId {
    // Using 64-bits we can create 5 * 10**9 ids per second for 100 years before wrapping
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        EntityId {
            id: COUNTER.fetch_add(1, Ordering::Relaxed),
        }
    }

    pub fn universal_integer() -> Self {
        UNIVERSAL_INTEGER_ID
    }

    pub fn universal_real() -> Self {
        UNIVERSAL_REAL_ID
    }
}

impl std::cmp::PartialEq for AnyEnt {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

/// This trait is implemented for Ast-nodes which declare named entities
pub trait HasNamedEntity {
    fn named_entity(&self) -> Option<&Arc<AnyEnt>>;
}

impl HasNamedEntity for AnyPrimaryUnit {
    fn named_entity(&self) -> Option<&Arc<AnyEnt>> {
        delegate_primary!(self, unit, unit.ident.decl.as_ref())
    }
}

impl WithDecl<Ident> {
    pub fn define(&mut self, kind: AnyEntKind) -> Arc<AnyEnt> {
        let ent = Arc::new(AnyEnt::new(
            self.tree.name().clone(),
            kind,
            Some(self.tree.pos()),
        ));
        self.decl = Some(ent.clone());
        ent
    }
    pub fn define_with_id(&mut self, id: EntityId, kind: AnyEntKind) -> Arc<AnyEnt> {
        let ent = Arc::new(AnyEnt::new_with_id(
            id,
            self.tree.name().clone().into(),
            kind,
            Some(self.tree.pos().clone()),
        ));
        self.decl = Some(ent.clone());
        ent
    }
}

impl WithDecl<WithPos<SubprogramDesignator>> {
    pub fn define(&mut self, kind: AnyEntKind) -> Arc<AnyEnt> {
        let ent = Arc::new(AnyEnt::new(
            self.tree.item.clone().into_designator(),
            kind,
            Some(&self.tree.pos),
        ));
        self.decl = Some(ent.clone());
        ent
    }
}

impl WithDecl<WithPos<Designator>> {
    pub fn define(&mut self, kind: AnyEntKind) -> Arc<AnyEnt> {
        let ent = Arc::new(AnyEnt::new(
            self.tree.item.clone(),
            kind,
            Some(&self.tree.pos),
        ));
        self.decl = Some(ent.clone());
        ent
    }
}

impl SubprogramDeclaration {
    pub fn define(&mut self, kind: AnyEntKind) -> Arc<AnyEnt> {
        match self {
            SubprogramDeclaration::Function(f) => f.designator.define(kind),
            SubprogramDeclaration::Procedure(p) => p.designator.define(kind),
        }
    }
}
