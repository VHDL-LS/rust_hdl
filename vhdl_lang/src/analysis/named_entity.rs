// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 20, Olof Kraigher olof.kraigher@gmail.com
use super::region::Region;
use crate::ast::*;
use crate::data::*;
use arc_swap::ArcSwapWeak;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Weak};

pub enum NamedEntityKind {
    AliasOf(Arc<NamedEntity>),
    UnknownAlias,
    File,
    InterfaceFile(Arc<NamedEntity>),
    Component,
    Attribute,
    SubprogramDecl(Signature),
    Subprogram(Signature),
    EnumLiteral(Signature),
    // An optional list of implicit declarations
    // Use Weak reference since implicit declaration typically reference the type itself
    TypeDeclaration(Vec<Weak<NamedEntity>>),
    IntegerType(Vec<Weak<NamedEntity>>),
    AccessType(Subtype),
    RecordType(Arc<Region<'static>>),
    ElementDeclaration(Subtype),
    Subtype(Subtype),
    // Weak references since incomplete access types can create cycles
    // The reference is for the full type which is filled in after creation
    IncompleteType(ArcSwapWeak<NamedEntity>),
    InterfaceType,
    Label,
    Object(Object),
    LoopParameter,
    PhysicalLiteral(Arc<NamedEntity>),
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
    pub fn is_deferred_constant(&self) -> bool {
        matches!(self, NamedEntityKind::DeferredConstant)
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
        matches!(self, NamedEntityKind::ProtectedType(..))
    }

    pub fn is_type(&self) -> bool {
        matches!(
            self,
            NamedEntityKind::IncompleteType(..)
                | NamedEntityKind::ProtectedType(..)
                | NamedEntityKind::InterfaceType
                | NamedEntityKind::Subtype(..)
                | NamedEntityKind::TypeDeclaration(..)
                | NamedEntityKind::IntegerType(..)
                | NamedEntityKind::AccessType(..)
                | NamedEntityKind::RecordType(..)
        )
    }

    // Is object or interface object
    pub fn is_object(&self) -> bool {
        matches!(self, NamedEntityKind::Object(..))
    }

    pub fn implicit_declarations(&self) -> Vec<Arc<NamedEntity>> {
        let weak = match self {
            NamedEntityKind::TypeDeclaration(ref implicit) => implicit,
            NamedEntityKind::IntegerType(ref implicit) => implicit,
            _ => {
                return Vec::new();
            }
        };

        weak.iter()
            // We expect the implicit declarations to live as long as the type so unwrap
            .map(|ent| ent.upgrade().unwrap())
            .collect()
    }

    pub fn describe(&self) -> &str {
        use NamedEntityKind::*;
        match self {
            AliasOf(..) => "alias",
            UnknownAlias => "alias",
            File => "file",
            InterfaceFile(..) => "file",
            ElementDeclaration(..) => "element declaration",
            RecordType(..) => "record type",
            Component => "component",
            Attribute => "attribute",
            SubprogramDecl(signature) | Subprogram(signature) => {
                if signature.return_type.is_some() {
                    "function"
                } else {
                    "procedure"
                }
            }
            EnumLiteral(..) => "enum literal",
            TypeDeclaration(..) => "type",
            IntegerType(..) => "integer type",
            AccessType(..) => "access type",
            Subtype(..) => "subtype",
            IncompleteType(..) => "type",
            InterfaceType => "type",
            Label => "label",
            LoopParameter => "loop parameter",
            Object(object) => object.class.describe(),
            PhysicalLiteral(..) => "physical literal",
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

#[derive(Clone)]
pub struct Subtype {
    type_mark: Arc<NamedEntity>,
}

impl Subtype {
    pub fn new(type_mark: Arc<NamedEntity>) -> Subtype {
        debug_assert!(type_mark.actual_kind().is_type());
        Subtype { type_mark }
    }

    pub fn type_mark(&self) -> &Arc<NamedEntity> {
        &self.type_mark
    }

    pub fn base_type(&self) -> &Arc<NamedEntity> {
        base_type(&self.type_mark)
    }
}

// Strip aliases and subtypes down to base type
pub fn base_type(ent: &Arc<NamedEntity>) -> &Arc<NamedEntity> {
    match ent.kind() {
        NamedEntityKind::AliasOf(ref ent) => base_type(ent),
        NamedEntityKind::Subtype(ref ent) => ent.base_type(),
        _ => ent,
    }
}

#[derive(Clone)]
pub struct Parameter {
    /// InterfaceObject or InterfaceFile
    param: Arc<NamedEntity>,
}

#[derive(Clone, Default)]
pub struct ParameterList {
    params: Vec<Parameter>,
}

impl Parameter {
    pub fn new(param: Arc<NamedEntity>) -> Self {
        debug_assert!(matches!(
            param.kind(),
            NamedEntityKind::Object(Object { mode: Some(_), .. })
                | NamedEntityKind::InterfaceFile(..)
        ));
        Self { param }
    }

    pub fn has_default(&self) -> bool {
        if let NamedEntityKind::Object(Object { has_default, .. }) = self.param.kind() {
            *has_default
        } else {
            false
        }
    }

    pub fn base_type(&self) -> &NamedEntity {
        match self.param.kind() {
            NamedEntityKind::Object(obj) => obj.subtype.base_type(),
            NamedEntityKind::InterfaceFile(file_type) => file_type.base_type(),
            _ => {
                unreachable!();
            }
        }
    }

    pub fn type_mark(&self) -> &NamedEntity {
        match self.param.kind() {
            NamedEntityKind::Object(obj) => obj.subtype.type_mark(),
            NamedEntityKind::InterfaceFile(file_type) => file_type,
            _ => {
                unreachable!();
            }
        }
    }
}

impl ParameterList {
    pub fn add_param(&mut self, param: Arc<NamedEntity>) {
        self.params.push(Parameter::new(param));
    }
}

#[derive(Clone, Default)]
pub struct Signature {
    /// Vector of InterfaceObject or InterfaceFile
    params: ParameterList,
    return_type: Option<Arc<NamedEntity>>,
}

impl Signature {
    pub fn new(params: ParameterList, return_type: Option<Arc<NamedEntity>>) -> Signature {
        if let Some(ref return_type) = return_type {
            debug_assert!(return_type.actual_kind().is_type());
        }
        Signature {
            params,
            return_type,
        }
    }

    pub fn key(&self) -> SignatureKey {
        let params = self
            .params
            .params
            .iter()
            .map(|param| param.base_type().id())
            .collect();
        let return_type = self.return_type.as_ref().map(|ent| ent.base_type().id());

        SignatureKey {
            params,
            return_type,
        }
    }

    pub fn describe(&self) -> String {
        let mut result = String::new();
        result.push('[');
        for (i, param) in self.params.params.iter().enumerate() {
            result.push_str(&param.type_mark().designator().to_string());

            if i + 1 < self.params.params.len() {
                result.push_str(", ");
            }
        }

        if !self.params.params.is_empty() && self.return_type.is_some() {
            result.push(' ');
        }

        if let Some(ref return_type) = self.return_type {
            result.push_str("return ");
            result.push_str(&return_type.designator().to_string());
        }

        result.push(']');
        result
    }

    /// Returns true if the function has no arguments
    /// or all arguments have defaults
    pub fn can_be_called_without_parameters(&self) -> bool {
        self.params.params.iter().all(|param| param.has_default())
    }

    pub fn return_type(&self) -> Option<&NamedEntity> {
        self.return_type.as_ref().map(|ent| ent.as_ref())
    }

    pub fn return_base_type(&self) -> Option<&NamedEntity> {
        self.return_type().map(|ent| ent.base_type())
    }

    pub fn match_return_type(&self, typ: &NamedEntity) -> bool {
        if let Some(return_type) = self.return_base_type() {
            return_type == typ
        } else {
            false
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct SignatureKey {
    params: Vec<EntityId>,
    return_type: Option<EntityId>,
}

impl SignatureKey {
    pub fn new(params: Vec<EntityId>, return_type: Option<EntityId>) -> SignatureKey {
        SignatureKey {
            params,
            return_type,
        }
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
    implicit: bool,
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
            implicit: false,
            decl_pos: decl_pos.cloned(),
            designator: designator.into(),
            kind,
        }
    }

    pub fn new_with_opt_id(
        id: Option<EntityId>,
        designator: impl Into<Designator>,
        kind: NamedEntityKind,
        decl_pos: Option<&SrcPos>,
    ) -> NamedEntity {
        NamedEntity {
            id: id.unwrap_or_else(new_id),
            implicit: false,
            decl_pos: decl_pos.cloned(),
            designator: designator.into(),
            kind,
        }
    }

    pub fn implicit(
        designator: impl Into<Designator>,
        kind: NamedEntityKind,
        decl_pos: Option<&SrcPos>,
    ) -> NamedEntity {
        NamedEntity {
            id: new_id(),
            implicit: true,
            decl_pos: decl_pos.cloned(),
            designator: designator.into(),
            kind,
        }
    }

    pub fn id(&self) -> EntityId {
        self.id
    }

    pub fn is_implicit(&self) -> bool {
        self.implicit
    }

    pub fn is_subprogram(&self) -> bool {
        matches!(self.kind, NamedEntityKind::Subprogram(..))
    }

    pub fn is_subprogram_decl(&self) -> bool {
        matches!(self.kind, NamedEntityKind::SubprogramDecl(..))
    }

    pub fn is_explicit(&self) -> bool {
        !self.implicit
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

    /// Create a copy of this named entity with the same ID but with an updated kind
    /// The use case is to overwrite an entity with a new kind when the full kind cannot
    /// Be created initially due to cyclic dependencies such as when defining an enum literal
    /// With a reference to the enum type where the enum type also needs to know about the literals
    /// @TODO investigate get_mut_unchecked instead
    pub fn clone_with_kind(&self, kind: NamedEntityKind) -> NamedEntity {
        NamedEntity::new_with_id(
            self.id(),
            self.designator.clone(),
            kind,
            self.decl_pos.as_ref(),
        )
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
    pub fn as_actual(&self) -> &NamedEntity {
        match self.kind() {
            NamedEntityKind::AliasOf(ref ent) => ent.as_actual(),
            _ => self,
        }
    }

    /// Strip aliases and subtypes down to base type
    pub fn base_type(&self) -> &NamedEntity {
        match self.kind() {
            NamedEntityKind::AliasOf(ref ent) => ent.base_type(),
            NamedEntityKind::Subtype(ref ent) => ent.base_type(),
            NamedEntityKind::Object(ref ent) => ent.subtype.base_type(),
            NamedEntityKind::ElementDeclaration(ref subtype) => subtype.base_type(),
            NamedEntityKind::PhysicalLiteral(ref base_type) => base_type,
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
        match self.kind {
            NamedEntityKind::AliasOf(..) => format!(
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
            | NamedEntityKind::Subprogram(ref signature) => format!(
                "{} '{}' with signature {}",
                self.kind.describe(),
                self.designator,
                signature.describe()
            ),
            _ => format!("{} '{}'", self.kind.describe(), self.designator),
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

impl std::cmp::PartialEq for NamedEntity {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
