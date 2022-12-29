// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2022, Olof Kraigher olof.kraigher@gmail.com

use super::formal_region::FormalRegion;
use super::implicits::ImplicitVec;
use super::region::Region;
use crate::ast::*;
use crate::data::*;
use arc_swap::ArcSwapWeak;
use fnv::FnvHashSet;
use std::borrow::Borrow;
use std::ops::Deref;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

pub enum Type {
    // Some types have an optional list of implicit declarations
    // Use Weak reference since implicit declaration typically reference the type itself
    Array {
        implicit: ImplicitVec,
        // Indexes are Option<> to handle unknown types
        indexes: Vec<Option<Arc<NamedEntity>>>,
        elem_type: TypeEnt,
    },
    Enum(ImplicitVec, FnvHashSet<Designator>),
    Integer(ImplicitVec),
    Physical(ImplicitVec),
    Access(Subtype, ImplicitVec),
    Record(Arc<Region<'static>>),
    // Weak references since incomplete access types can create cycles
    // The reference is for the full type which is filled in after creation
    Incomplete(ArcSwapWeak<NamedEntity>),
    Subtype(Subtype),
    // The region of the protected type which needs to be extendend by the body
    Protected(Arc<Region<'static>>),
    File(ImplicitVec),
    Interface,
    Alias(TypeEnt),
}

impl Type {
    pub fn implicit_declarations(&self) -> impl Iterator<Item = Arc<NamedEntity>> + '_ {
        self.implicits().into_iter().flat_map(|imp| imp.iter())
    }

    pub fn implicits(&self) -> Option<&ImplicitVec> {
        match self {
            Type::Array { ref implicit, .. } => Some(implicit),
            Type::Enum(ref implicit, _) => Some(implicit),
            Type::Integer(ref implicit) => Some(implicit),
            Type::Physical(ref implicit) => Some(implicit),
            Type::File(ref implicit) => Some(implicit),
            Type::Access(.., ref implicit) => Some(implicit),
            Type::Incomplete(..)
            | Type::Interface
            | Type::Protected(..)
            | Type::Record(..)
            | Type::Subtype(..)
            | Type::Alias(..) => None,
        }
    }

    pub fn describe(&self) -> &str {
        match self {
            Type::Alias(..) => "alias",
            Type::Record(..) => "record type",
            Type::Array { .. } => "array type",
            Type::Enum(..) => "type",
            Type::Integer(..) => "integer type",
            Type::Physical(..) => "physical type",
            Type::Access(..) => "access type",
            Type::Subtype(..) => "subtype",
            Type::Incomplete(..) => "type",
            Type::Interface => "type",
            Type::File(..) => "file type",
            Type::Protected(..) => "protected type",
        }
    }
}

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
    Component(Region<'static>),
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
                if signature.return_type.is_some() {
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

#[derive(Clone)]
pub struct Signature {
    /// Vector of InterfaceObject or InterfaceFile
    pub params: FormalRegion,
    return_type: Option<TypeEnt>,
}

impl Signature {
    pub fn new(params: FormalRegion, return_type: Option<TypeEnt>) -> Signature {
        Signature {
            params,
            return_type: return_type.as_ref().map(TypeEnt::to_owned),
        }
    }

    pub fn key(&self) -> SignatureKey {
        let params = self
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
        for (i, param) in self.params.iter().enumerate() {
            result.push_str(&param.type_mark().designator().to_string());

            if i + 1 < self.params.len() {
                result.push_str(", ");
            }
        }

        if !self.params.is_empty() && self.return_type.is_some() {
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
        self.params.iter().all(|param| param.has_default())
    }

    pub fn can_be_called_with_single_parameter(&self, typ: &TypeEnt) -> bool {
        let mut params = self.params.iter();
        if let Some(first) = params.next() {
            if params.all(|param| param.has_default()) {
                return first.base_type() == typ.base_type();
            }
        }
        false
    }

    pub fn return_type(&self) -> Option<&TypeEnt> {
        self.return_type.as_ref()
    }

    pub fn match_return_type(&self, typ: Option<&TypeEnt>) -> bool {
        self.return_type().map(|ent| ent.base_type()) == typ.map(|ent| ent.base_type())
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
        NamedEntity::new_with_id(new_id(), designator.into(), kind, decl_pos.cloned())
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
            id: new_id(),
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
            self.decl_pos.clone(),
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
    pub fn flatten_alias(ent: &Arc<NamedEntity>) -> &Arc<NamedEntity> {
        match ent.kind() {
            NamedEntityKind::NonObjectAlias(ref ent) => NamedEntity::flatten_alias(ent),
            NamedEntityKind::Type(Type::Alias(ref ent)) => NamedEntity::flatten_alias(&ent.0),
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
            id: id.unwrap_or_else(new_id),
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

impl std::ops::Deref for TypeEnt {
    type Target = NamedEntity;
    fn deref(&self) -> &NamedEntity {
        let val: &Arc<NamedEntity> = self.0.borrow();
        val.as_ref()
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
