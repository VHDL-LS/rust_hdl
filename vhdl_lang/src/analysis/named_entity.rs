// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 20, Olof Kraigher olof.kraigher@gmail.com
use super::region::Region;
use crate::ast::*;
use crate::data::*;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

#[derive(Clone)]
pub enum NamedEntityKind {
    AliasOf(Arc<NamedEntity>),
    OtherAlias,
    File,
    InterfaceFile,
    RecordField,
    Component,
    Attribute,
    Overloaded,
    // An optional region with implicit declarations
    TypeDeclaration(Option<Arc<Region<'static>>>),
    Subtype(Subtype),
    IncompleteType,
    InterfaceType,
    Label,
    Object(ObjectClass),
    InterfaceObject(ObjectClass),
    PhysicalLiteral,
    DeferredConstant,
    // The region of the protected type which needs to be extendend by the body
    ProtectedType(Arc<Region<'static>>),
    Library,
    Entity(Arc<Region<'static>>),
    Configuration(Arc<Region<'static>>),
    Package(Arc<Region<'static>>),
    UninstPackage(Arc<Region<'static>>),
    PackageInstance(Arc<Region<'static>>),
    Context(Arc<Region<'static>>),
    LocalPackageInstance(Arc<Region<'static>>),
}

impl NamedEntityKind {
    pub fn from_object_declaration(decl: &ObjectDeclaration) -> NamedEntityKind {
        if decl.class == ObjectClass::Constant && decl.expression.is_none() {
            NamedEntityKind::DeferredConstant
        } else {
            NamedEntityKind::Object(decl.class)
        }
    }

    pub fn is_deferred_constant(&self) -> bool {
        if let NamedEntityKind::DeferredConstant = self {
            true
        } else {
            false
        }
    }

    pub fn is_non_deferred_constant(&self) -> bool {
        if let NamedEntityKind::Object(ObjectClass::Constant) = self {
            true
        } else {
            false
        }
    }

    pub fn is_protected_type(&self) -> bool {
        if let NamedEntityKind::ProtectedType(..) = self {
            true
        } else {
            false
        }
    }

    pub fn describe(&self) -> &str {
        use NamedEntityKind::*;
        match self {
            AliasOf(..) => "alias",
            OtherAlias => "alias",
            File => "file",
            InterfaceFile => "file",
            RecordField => "file",
            Component => "file",
            Attribute => "file",
            Overloaded => "file",
            TypeDeclaration(..) => "type",
            Subtype(..) => "subtype",
            IncompleteType => "type",
            InterfaceType => "type",
            Label => "label",
            Object(class) => class.describe(),
            InterfaceObject(class) => class.describe(),
            PhysicalLiteral => "physical literal",
            DeferredConstant => "deferred constant",
            ProtectedType(..) => "protected type",
            Library => "library",
            Entity(..) => "entity",
            Configuration(..) => "configuration",
            Package(..) => "package",
            UninstPackage(..) => "uninstantiated package",
            PackageInstance(..) => "package instance",
            Context(..) => "context",
            LocalPackageInstance(..) => "package instance",
        }
    }
}

#[derive(Clone)]
pub struct Subtype {
    base: Arc<NamedEntity>,
}

impl Subtype {
    pub fn new(base: Arc<NamedEntity>) -> Subtype {
        Subtype { base }
    }
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

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct EntityId {
    id: usize,
}

pub struct NamedEntity {
    /// An unique id of the entity
    /// Entities with the same id will be the same
    id: EntityId,
    /// The location where the declaration was made
    /// Builtin and implicit declaration will not have a source position
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
        NamedEntity::new_with_id(new_id(), designator, kind, decl_pos)
    }

    pub fn new_with_id(
        id: EntityId,
        designator: impl Into<Designator>,
        kind: NamedEntityKind,
        decl_pos: Option<&SrcPos>,
    ) -> NamedEntity {
        NamedEntity {
            id,
            decl_pos: decl_pos.cloned(),
            designator: designator.into(),
            kind,
        }
    }

    pub fn id(&self) -> EntityId {
        self.id
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
        if let NamedEntityKind::Overloaded = self.kind {
            true
        } else {
            false
        }
    }

    /// Return a duplicate declaration of the previously declared named entity
    pub fn is_duplicate_of<'a>(&self, prev: &'a Self) -> bool {
        if self.is_overloaded() && prev.is_overloaded() {
            return false;
        }

        match prev.kind {
            // Everything expect deferred combinations are forbidden
            NamedEntityKind::DeferredConstant if self.kind.is_non_deferred_constant() => {}
            _ => {
                return true;
            }
        }

        false
    }

    /// Strip aliases and return reference to actual named entity
    pub fn as_actual(&self) -> &NamedEntity {
        match self.kind() {
            NamedEntityKind::AliasOf(ref ent) => ent.as_actual(),
            _ => self,
        }
    }

    /// Returns true if self is alias of other
    pub fn is_alias_of(&self, other: &NamedEntity) -> bool {
        match self.kind() {
            NamedEntityKind::AliasOf(ref ent) => {
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
        if let NamedEntityKind::AliasOf(..) = self.kind {
            format!(
                "alias '{}' of {}",
                self.designator,
                self.as_actual().describe()
            )
        } else {
            format!("{} '{}'", self.kind.describe(), self.designator)
        }
    }
}

static COUNTER: AtomicUsize = AtomicUsize::new(1);

// Using 64-bits we can create 5 * 10**9 ids per second for 100 years before wrapping
pub fn new_id() -> EntityId {
    EntityId {
        id: COUNTER.fetch_add(1, Ordering::Relaxed),
    }
}
