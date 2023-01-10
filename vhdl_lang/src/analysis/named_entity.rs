// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2022, Olof Kraigher olof.kraigher@gmail.com

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
pub use overloaded::{OverloadedEnt, Signature, SignatureKey};

pub enum NamedEntityKind {
    NonObjectAlias(Arc<NamedEntity>),
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
    SubprogramDecl(Signature),
    Subprogram(Signature),
    EnumLiteral(Signature),
    // An optional list of implicit declarations
    // Use Weak reference since implicit declaration typically reference the type itself
    Type(Type),
    ElementDeclaration(Subtype),
    Label,
    Object(Object),
    LoopParameter,
    PhysicalLiteral(TypeEnt),
    DeferredConstant(Subtype),
    Library,
    Entity(Arc<Region>),
    Configuration(Arc<Region>),
    Package(Arc<Region>),
    UninstPackage(Arc<Region>),
    PackageInstance(Arc<Region>),
    Context(Arc<Region>),
    LocalPackageInstance(Arc<Region>),
}

impl NamedEntityKind {
    pub fn is_deferred_constant(&self) -> bool {
        matches!(self, NamedEntityKind::DeferredConstant(..))
    }

    pub fn is_non_deferred_constant(&self) -> bool {
        matches!(
            self,
            NamedEntityKind::Object(Object {
                class: ObjectClass::Constant,
                mode: None,
                ..
            })
        )
    }

    pub fn is_protected_type(&self) -> bool {
        matches!(self, NamedEntityKind::Type(Type::Protected(..)))
    }

    pub fn is_type(&self) -> bool {
        matches!(self, NamedEntityKind::Type(..))
    }

    pub fn implicit_declarations(&self) -> impl Iterator<Item = Arc<NamedEntity>> + '_ {
        match self {
            NamedEntityKind::Type(typ) => Some(typ.implicit_declarations()),
            _ => None,
        }
        .into_iter()
        .flatten()
    }

    pub fn describe(&self) -> &str {
        use NamedEntityKind::*;
        match self {
            NonObjectAlias(..) => "alias",
            ObjectAlias { .. } => "object alias",
            ExternalAlias { .. } => "external alias",
            File(..) => "file",
            InterfaceFile(..) => "file",
            ElementDeclaration(..) => "element declaration",
            Component(..) => "component",
            Attribute => "attribute",
            SubprogramDecl(signature) | Subprogram(signature) => {
                if signature.return_type().is_some() {
                    "function"
                } else {
                    "procedure"
                }
            }
            EnumLiteral(..) => "enum literal",
            Label => "label",
            LoopParameter => "loop parameter",
            Object(object) => object.class.describe(),
            PhysicalLiteral(..) => "physical literal",
            DeferredConstant(..) => "deferred constant",
            Library => "library",
            Entity(..) => "entity",
            Configuration(..) => "configuration",
            Package(..) => "package",
            UninstPackage(..) => "uninstantiated package",
            PackageInstance(..) => "package instance",
            Context(..) => "context",
            LocalPackageInstance(..) => "package instance",
            Type(typ) => typ.describe(),
        }
    }
}

impl std::fmt::Debug for NamedEntityKind {
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
pub struct NamedEntity {
    /// A unique id of the entity.
    /// Entities with the same id will be the same.
    id: EntityId,
    pub implicit_of: Option<Arc<NamedEntity>>,
    /// The location where the declaration was made.
    /// Builtin and implicit declaration will not have a source position.
    designator: Designator,
    kind: NamedEntityKind,
    decl_pos: Option<SrcPos>,
}

impl NamedEntity {
    pub fn new(
        designator: impl Into<Designator>,
        kind: NamedEntityKind,
        decl_pos: Option<&SrcPos>,
    ) -> NamedEntity {
        NamedEntity::new_with_id(EntityId::new(), designator.into(), kind, decl_pos.cloned())
    }

    pub fn new_with_id(
        id: EntityId,
        designator: Designator,
        kind: NamedEntityKind,
        decl_pos: Option<SrcPos>,
    ) -> NamedEntity {
        NamedEntity {
            id,
            implicit_of: None,
            decl_pos,
            designator,
            kind,
        }
    }

    pub fn implicit(
        of_ent: Arc<NamedEntity>,
        designator: impl Into<Designator>,
        kind: NamedEntityKind,
        decl_pos: Option<&SrcPos>,
    ) -> NamedEntity {
        NamedEntity {
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
        matches!(self.kind, NamedEntityKind::Subprogram(..))
    }

    pub fn is_subprogram_decl(&self) -> bool {
        matches!(self.kind, NamedEntityKind::SubprogramDecl(..))
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

    pub fn kind(&self) -> &NamedEntityKind {
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
            NamedEntityKind::Subprogram(ref signature)
            | NamedEntityKind::SubprogramDecl(ref signature)
            | NamedEntityKind::EnumLiteral(ref signature) => Some(signature),
            _ => None,
        }
    }

    /// Strip aliases and return reference to actual named entity
    pub fn flatten_alias(ent: &Arc<NamedEntity>) -> &Arc<NamedEntity> {
        match ent.kind() {
            NamedEntityKind::NonObjectAlias(ref ent) => NamedEntity::flatten_alias(ent),
            NamedEntityKind::Type(Type::Alias(ref ent)) => NamedEntity::flatten_alias(ent.as_ref()),
            _ => ent,
        }
    }

    pub fn as_actual(&self) -> &NamedEntity {
        match self.kind() {
            NamedEntityKind::NonObjectAlias(ref ent) => ent.as_actual(),
            NamedEntityKind::Type(Type::Alias(ref ent)) => ent.as_actual(),
            _ => self,
        }
    }

    /// Strip aliases and return reference to actual entity kind
    pub fn actual_kind(&self) -> &NamedEntityKind {
        self.as_actual().kind()
    }

    /// Returns true if self is alias of other
    pub fn is_alias_of(&self, other: &NamedEntity) -> bool {
        match self.kind() {
            NamedEntityKind::Type(Type::Alias(ref ent)) => {
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
            NamedEntityKind::NonObjectAlias(..) => format!(
                "alias '{}' of {}",
                self.designator,
                self.as_actual().describe()
            ),
            NamedEntityKind::Object(Object {
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
            NamedEntityKind::EnumLiteral(ref signature)
            | NamedEntityKind::SubprogramDecl(ref signature)
            | NamedEntityKind::Subprogram(ref signature) => {
                format!("{}{}", self.designator, signature.describe())
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

impl std::cmp::PartialEq for NamedEntity {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

// A named entity that is known to be an object
#[derive(Clone, Debug)]
pub struct ObjectEnt {
    pub ent: Arc<NamedEntity>,
}

impl ObjectEnt {
    pub fn new(ent: Arc<NamedEntity>) -> Self {
        debug_assert!(matches!(ent.actual_kind(), NamedEntityKind::Object(..)));
        Self { ent }
    }

    pub fn class(&self) -> ObjectClass {
        self.object().class
    }

    pub fn mode(&self) -> Option<Mode> {
        self.object().mode
    }

    pub fn describe_class(&self) -> String {
        if let Some(mode) = self.mode() {
            if self.class() == ObjectClass::Constant {
                format!("interface {}", self.class())
            } else {
                format!("interface {} of mode {}", self.class(), mode)
            }
        } else {
            format!("{}", self.class())
        }
    }

    pub fn object(&self) -> &Object {
        if let NamedEntityKind::Object(object) = self.ent.actual_kind() {
            object
        } else {
            unreachable!("Must be object");
        }
    }
}

/// This trait is implemented for Ast-nodes which declare named entities
pub trait HasNamedEntity {
    fn named_entity(&self) -> Option<&Arc<NamedEntity>>;
}

impl HasNamedEntity for AnyPrimaryUnit {
    fn named_entity(&self) -> Option<&Arc<NamedEntity>> {
        delegate_primary!(self, unit, unit.ident.decl.as_ref())
    }
}

impl WithDecl<Ident> {
    pub fn define(&mut self, kind: NamedEntityKind) -> Arc<NamedEntity> {
        let ent = Arc::new(NamedEntity::new(
            self.tree.name().clone(),
            kind,
            Some(self.tree.pos()),
        ));
        self.decl = Some(ent.clone());
        ent
    }
    pub fn define_with_id(&mut self, id: EntityId, kind: NamedEntityKind) -> Arc<NamedEntity> {
        let ent = Arc::new(NamedEntity::new_with_id(
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
    pub fn define(&mut self, kind: NamedEntityKind) -> Arc<NamedEntity> {
        let ent = Arc::new(NamedEntity::new(
            self.tree.item.clone().into_designator(),
            kind,
            Some(&self.tree.pos),
        ));
        self.decl = Some(ent.clone());
        ent
    }
}

impl WithDecl<WithPos<Designator>> {
    pub fn define(&mut self, kind: NamedEntityKind) -> Arc<NamedEntity> {
        let ent = Arc::new(NamedEntity::new(
            self.tree.item.clone(),
            kind,
            Some(&self.tree.pos),
        ));
        self.decl = Some(ent.clone());
        ent
    }
}

impl SubprogramDeclaration {
    pub fn define(&mut self, kind: NamedEntityKind) -> Arc<NamedEntity> {
        match self {
            SubprogramDeclaration::Function(f) => f.designator.define(kind),
            SubprogramDeclaration::Procedure(p) => p.designator.define(kind),
        }
    }
}
