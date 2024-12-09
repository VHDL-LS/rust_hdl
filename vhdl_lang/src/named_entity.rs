// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2022, Olof Kraigher olof.kraigher@gmail.com

use crate::ast::ExternalObjectClass;
use crate::ast::{
    AliasDeclaration, AnyDesignUnit, AnyPrimaryUnit, AnySecondaryUnit, Attribute,
    AttributeDeclaration, AttributeSpecification, ComponentDeclaration, Designator, HasIdent,
    Ident, InterfacePackageDeclaration, ModeViewDeclaration, ObjectClass, PackageInstantiation,
    SubprogramBody, SubprogramInstantiation, SubprogramSpecification, TypeDeclaration, WithDecl,
};
use crate::data::*;
mod types;
use fnv::FnvHashMap;
pub use types::{BaseType, Subtype, Type, TypeEnt, TypedSelection, UniversalType};
mod overloaded;
pub use overloaded::{Overloaded, OverloadedEnt, Signature, SignatureKey, SubprogramKey};
mod object;
pub use object::{InterfaceMode, Object, ObjectEnt, ObjectInterface, ViewEnt};
mod design;
pub use design::{Design, DesignEnt};
mod attribute;
pub use attribute::AttributeEnt;
mod arena;
pub use arena::{Arena, ArenaId, EntityId, FinalArena, Reference};
mod visibility;
pub(crate) use visibility::IntoUnambiguousError;
pub use visibility::{Visibility, Visible};
mod region;
pub(crate) use region::RegionKind;
pub use region::{AsUnique, NamedEntities, OverloadedName, Region, SetReference};
mod formal_region;
use crate::ast::token_range::{WithToken, WithTokenSpan};
use crate::data::error_codes::ErrorCode;
use crate::{TokenAccess, TokenSpan};
pub use formal_region::{
    FormalRegion, GpkgInterfaceEnt, GpkgRegion, InterfaceClass, InterfaceEnt, RecordElement,
    RecordRegion,
};

/// The kind of [AnyEnt].
/// This contains relevant information obtained during analysis.
pub enum AnyEntKind<'a> {
    /// An alias that references an external name.
    ExternalAlias {
        /// The class of the external object (can be constant, signal or variable)
        /// The type of the referenced external name.
        class: ExternalObjectClass,
        type_mark: TypeEnt<'a>,
    },
    /// An alias that references an object.
    ObjectAlias {
        /// The named entity that this alias references
        base_object: ObjectEnt<'a>,
        /// The type of the base object
        type_mark: TypeEnt<'a>,
    },
    /// A file declared in a declarative region (i.e., an architecture body).
    /// The associated [Subtype] refers to the type of the file.
    File(Subtype<'a>),
    /// A file declaration made in an interface (i.e., as part of subprogram arguments)
    /// The associated [TypeEnt] refers to the type of the file (as it was declared)
    InterfaceFile(TypeEnt<'a>),
    /// A component declaration.
    /// The associated [Region] contains generics and ports
    /// declared in the component.
    Component(Region<'a>),
    /// A custom attribute.
    /// The associated [TypeEnt] refers to the return type of the attribute.
    Attribute(TypeEnt<'a>),
    /// An overloaded entity, i.e., an entity with potentially
    /// further entities that share the same name but other features to distinguish.
    /// A good example is a function. Two VHDL functions may exist that share
    /// the same name, but have a different signature and are hence different.
    /// The associated [Overloaded] enum contains more information about the
    /// actual entity.
    /// Overloaded entities are
    /// * Subprograms (both functions and procedures),
    /// * Enum literals
    /// * Aliases
    Overloaded(Overloaded<'a>),
    /// A custom type. The associated [Type] references the
    /// actual type that was declared.
    Type(Type<'a>),
    /// A record element.
    /// The associated [Subtype] references the actual type of the record.
    ElementDeclaration(Subtype<'a>),
    /// A concurrent statement. The associated [Concurrent] data is a reference
    /// to the exact statement (i.e., block process or generate).
    /// Not all statements are handled at the moment, therefore the associated
    /// data is optional.
    Concurrent(Option<Concurrent>),
    /// A sequential statement. The associated [Sequential] data is a reference
    /// to the exact statement (i.e., loop, if or else statement).
    /// Not all statements are handled at the moment, therefore the associated
    /// data is optional.
    Sequential(Option<Sequential>),
    /// An object is a signal, constant, variable or shared variable.
    /// The `File` object is handled in the [AnyEntKind::File] kind.
    /// The associated object contains more information and data about the
    /// actual entity.
    Object(Object<'a>),
    /// A loop parameter from a 'for generate' statement.
    /// loop, or a simple for loop.
    /// The type refers to the (inferred) type of the range of the loop.
    /// If it is `None`, the range expression is erroneous.
    LoopParameter(Option<BaseType<'a>>),
    /// A literal of some physical type (e.g., time).
    /// The associated [TypeEnt] refers to the type of the literal.
    PhysicalLiteral(TypeEnt<'a>),
    /// A constant, declared in a package, where the actual value is not given immediately.
    /// The actual value is defined in the associated package region.
    /// The associated [Subtype] data refers to the type of the constant.
    DeferredConstant(Subtype<'a>),
    /// A VHDL library
    Library,
    /// A design entity, such as a VHDL entity, an architecture
    /// or a package.
    /// The associated [Design] data contains more information about
    /// the individual named entity.
    Design(Design<'a>),
    /// A VHDL 2019 View.
    /// The [Subtype] data is the type of the associated record.
    View(Subtype<'a>),
}

impl<'a> AnyEntKind<'a> {
    /// Creates an [AnyEntKind] that denotes a function declaration.
    /// # Arguments
    /// * `formals` - The formal arguments of the function
    /// * `return_type` - The return type of the function
    pub(crate) fn new_function_decl(
        formals: FormalRegion<'a>,
        return_type: TypeEnt<'a>,
    ) -> AnyEntKind<'a> {
        AnyEntKind::Overloaded(Overloaded::SubprogramDecl(Signature::new(
            formals,
            Some(return_type),
        )))
    }

    /// Creates an [AnyEntKind] that denotes a procedure declaration.
    /// In contrast to [Self::new_function_decl], procedures do not have a return type.
    ///
    /// # Arguments
    /// * `formals` - The formal arguments of the procedure
    pub(crate) fn new_procedure_decl(formals: FormalRegion<'a>) -> AnyEntKind<'a> {
        AnyEntKind::Overloaded(Overloaded::SubprogramDecl(Signature::new(formals, None)))
    }

    /// Returns whether this kind is a deferred constant.
    pub fn is_deferred_constant(&self) -> bool {
        matches!(self, AnyEntKind::DeferredConstant(..))
    }

    /// Returns whether this kind is a constant that is not deferred.
    pub fn is_non_deferred_constant(&self) -> bool {
        matches!(
            self,
            AnyEntKind::Object(Object {
                class: ObjectClass::Constant,
                iface: None,
                ..
            })
        )
    }

    /// Returns whether this kind refers to a protected type.
    pub fn is_protected_type(&self) -> bool {
        matches!(self, AnyEntKind::Type(Type::Protected(..)))
    }

    /// Returns whether this kind refers to any type declaration.
    pub fn is_type(&self) -> bool {
        matches!(self, AnyEntKind::Type(..))
    }

    pub fn describe(&self) -> &str {
        use AnyEntKind::*;
        match self {
            ObjectAlias { .. } => "object alias",
            ExternalAlias { .. } => "external alias",
            File(..) => "file",
            InterfaceFile(..) => "file parameter",
            ElementDeclaration(..) => "record element",
            Component(..) => "component",
            Attribute(..) => "attribute",
            Overloaded(overloaded) => overloaded.describe(),
            Concurrent(Some(c)) => c.describe(),
            Concurrent(None) => "label",
            Sequential(Some(s)) => s.describe(),
            Sequential(None) => "label",
            LoopParameter(_) => "loop parameter",
            Object(object) => object.class.describe(),
            PhysicalLiteral(..) => "physical literal",
            DeferredConstant(..) => "deferred constant",
            Library => "library",
            Design(design) => design.describe(),
            Type(typ) => typ.describe(),
            View(..) => "view",
        }
    }
}

impl std::fmt::Debug for AnyEntKind<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.describe())
    }
}

impl std::fmt::Debug for AnyEnt<'_> {
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
            src_span,
            source,
            attrs,
        } = self;

        let mut s = f.debug_struct(stringify!(AnyEnt));
        s.field(stringify!(id), id);
        s.field(stringify!(parent), &parent.is_some());
        s.field(stringify!(related), related);
        s.field(stringify!(implicits), &implicits.len());
        s.field(stringify!(designator), designator);
        s.field(stringify!(kind), kind);
        s.field(stringify!(decl_pos), decl_pos);
        s.field(stringify!(src_span), src_span);
        s.field(stringify!(source), source);
        s.field(stringify!(attrs), attrs);
        s.finish()
    }
}

pub type EntRef<'a> = &'a AnyEnt<'a>;

#[derive(Debug, Copy, Clone)]
pub enum Related<'a> {
    ImplicitOf(EntRef<'a>),
    InstanceOf(EntRef<'a>),
    DeclaredBy(EntRef<'a>),
    /// An entity with that is derived from another entity using an attribute.
    /// For example, a delayed signal may be derived using the
    /// `delayed_sig <= s'delayed(t0)` syntax
    DerivedFrom(EntRef<'a>),
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
    /// The name of the entity.
    /// This will be a [Designator::Identifier](Identifier) for 'common'
    /// named entities (such as functions, architectures, e.t.c.)
    pub designator: Designator,
    /// contains more information about this named entity
    pub kind: AnyEntKind<'a>,
    /// The location where the declaration was made.
    /// Builtin and implicit declaration will not have a source position.
    pub decl_pos: Option<SrcPos>,
    /// The whole textual span that this entity encompasses.
    pub src_span: TokenSpan,
    /// Where this declaration was made.
    /// Builtin and implicit declaration will not have a source position.
    // This is here so that replacing `decl_pos` with a simple Token(span) is feasible later.
    pub source: Option<Source>,

    /// Custom attributes on this entity
    pub attrs: FnvHashMap<Symbol, (SrcPos, AttributeEnt<'a>)>,
}

impl Arena {
    pub fn implicit<'a>(
        &'a self,
        of_ent: EntRef<'a>,
        designator: impl Into<Designator>,
        kind: AnyEntKind<'a>,
    ) -> EntRef<'a> {
        self.alloc(
            designator.into(),
            of_ent.parent,
            Related::ImplicitOf(of_ent),
            kind,
            of_ent.decl_pos().cloned(),
            of_ent.src_span,
            of_ent.source.clone(),
        )
    }

    pub fn define<'a, T: HasIdent>(
        &'a self,
        ctx: &dyn TokenAccess,
        decl: &mut WithDecl<T>,
        parent: EntRef<'a>,
        kind: AnyEntKind<'a>,
        src_span: TokenSpan,
        source: Option<Source>,
    ) -> EntRef<'a> {
        let ent = self.explicit(
            decl.tree.name().clone(),
            parent,
            kind,
            Some(decl.tree.ident_pos(ctx)),
            src_span,
            source,
        );
        decl.decl.set(ent.id());
        ent
    }

    pub fn explicit<'a>(
        &'a self,
        designator: impl Into<Designator>,
        parent: EntRef<'a>,
        kind: AnyEntKind<'a>,
        decl_pos: Option<&SrcPos>,
        src_span: TokenSpan,
        source: Option<Source>,
    ) -> EntRef<'a> {
        self.alloc(
            designator.into(),
            Some(parent),
            Related::None,
            kind,
            decl_pos.cloned(),
            src_span,
            source,
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
        use Related::*;
        match self.related {
            ImplicitOf(_) => true,
            InstanceOf(ent) | DerivedFrom(ent) => ent.is_implicit(),
            DeclaredBy(_) | None => false,
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

    pub fn is_uninst_subprogram_decl(&self) -> bool {
        matches!(
            self.kind,
            AnyEntKind::Overloaded(Overloaded::UninstSubprogramDecl(..))
        )
    }

    pub fn is_uninst_subprogram_body(&self) -> bool {
        matches!(
            self.kind(),
            AnyEntKind::Overloaded(Overloaded::UninstSubprogram(..))
        )
    }

    pub fn is_uninst_subprogram(&self) -> bool {
        matches!(
            self.kind(),
            AnyEntKind::Overloaded(Overloaded::UninstSubprogram(..))
                | AnyEntKind::Overloaded(Overloaded::UninstSubprogramDecl(..))
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

    pub fn is_declared_by(&self, other: EntRef<'_>) -> bool {
        if let Related::DeclaredBy(ent) = self.related {
            if ent.id() == other.id() {
                return true;
            }
        }

        false
    }

    pub fn is_implicit_of(&self, other: EntityId) -> bool {
        use Related::*;
        match &self.related {
            ImplicitOf(ent) => ent.id() == other,
            InstanceOf(ent) => ent.is_implicit_of(other),
            DeclaredBy(_) | DerivedFrom(_) | None => false,
        }
    }

    pub fn is_instance_of(&self, other: EntRef<'_>) -> bool {
        if let Related::InstanceOf(ent) = &self.related {
            ent.id() == other.id()
        } else {
            false
        }
    }

    pub fn is_explicit(&self) -> bool {
        !self.is_implicit()
    }

    /// A statement without a label
    pub fn is_anonymous(&self) -> bool {
        matches!(self.designator(), Designator::Anonymous(_))
    }

    pub fn decl_pos(&self) -> Option<&SrcPos> {
        self.decl_pos.as_ref()
    }

    pub fn parent_in_same_source(&self) -> Option<EntRef<'a>> {
        let source = self.source.as_ref()?;
        let mut ent = self;

        while let Some(parent) = ent.parent {
            if let Some(pos) = &parent.source {
                return if pos == source { Some(parent) } else { None };
            }
            ent = parent;
        }
        None
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

    pub fn path_name(&self) -> String {
        if let Some(parent) = self.parent {
            format!("{}.{}", parent.path_name(), self.designator())
        } else {
            self.designator().to_string()
        }
    }

    pub fn kind(&self) -> &AnyEntKind<'_> {
        &self.kind
    }

    pub fn error(
        &self,
        diagnostics: &mut dyn DiagnosticHandler,
        message: impl Into<String>,
        code: ErrorCode,
    ) {
        if let Some(ref pos) = self.decl_pos {
            diagnostics.add(pos, message, code);
        }
    }

    pub fn is_overloaded(&self) -> bool {
        self.signature().is_some()
    }

    pub fn signature(&self) -> Option<&Signature<'_>> {
        match self.actual_kind() {
            AnyEntKind::Overloaded(ref overloaded) => Some(overloaded.signature()),
            _ => None,
        }
    }

    pub fn as_actual(&self) -> EntRef<'_> {
        match self.kind() {
            AnyEntKind::Overloaded(Overloaded::Alias(ref ent)) => ent.as_actual(),
            AnyEntKind::Type(Type::Alias(ref ent)) => ent.as_actual(),
            AnyEntKind::ObjectAlias { base_object, .. } => base_object.as_actual(),
            _ => self,
        }
    }

    pub(crate) fn add_implicit(&mut self, ent: EntRef<'a>) {
        self.implicits.push(ent);
    }

    pub(crate) fn add_attribute(
        &mut self,
        ent: AttributeEnt<'a>,
        pos: &SrcPos,
    ) -> Result<(), Diagnostic> {
        use std::collections::hash_map::Entry;
        match self.attrs.entry(ent.name().clone()) {
            Entry::Occupied(entry) => {
                let last_pos = entry.get().0.clone();
                Err(Diagnostic::new(
                    pos,
                    format!(
                        "Duplicate specification of attribute '{}' for {}",
                        ent.name(),
                        self.describe()
                    ),
                    ErrorCode::Duplicate,
                )
                .related(last_pos, "Previously specified here"))
            }
            Entry::Vacant(entry) => {
                entry.insert((pos.clone(), ent));
                Ok(())
            }
        }
    }

    pub fn get_attribute(&self, name: &Symbol) -> Option<AttributeEnt<'a>> {
        self.attrs.get(name).map(|(_, ent)| *ent)
    }

    /// Strip aliases and return reference to actual entity kind
    pub fn actual_kind(&self) -> &AnyEntKind<'_> {
        self.as_actual().kind()
    }

    /// Returns true if self is alias of other
    pub fn is_alias_of(&self, other: EntRef<'_>) -> bool {
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
            AnyEntKind::Object(_) => ObjectEnt::from_any(self).unwrap().describe(),
            AnyEntKind::Overloaded(_) => OverloadedEnt::from_any(self).unwrap().describe(),

            AnyEntKind::Type(_) => TypeEnt::from_any(self).unwrap().describe(),
            _ => {
                if matches!(self.designator, Designator::Anonymous(_)) {
                    self.kind.describe().to_string()
                } else {
                    format!("{} '{}'", self.kind.describe(), self.designator)
                }
            }
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
    #[allow(invalid_reference_casting)]
    unsafe fn unsafe_ref_mut(&self) -> &mut Self {
        unsafe {
            // NOTE: Use read_volatile to prevent compiler to optimization away assignment to the returned reference
            let const_ptr = std::ptr::read_volatile(&self) as *const Self;
            let mut_ptr = const_ptr as *mut Self;
            &mut *mut_ptr
        }
    }

    // Used to update the kind of pre-declared symbols that are visible before they have been fully analyzed
    pub(crate) unsafe fn set_kind(&self, kind: AnyEntKind<'_>) {
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

impl std::cmp::PartialEq for AnyEnt<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl std::hash::Hash for AnyEnt<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id().hash(state)
    }
}

impl Eq for AnyEnt<'_> {}

/// This trait is implemented for Ast-nodes which declare named entities
pub trait HasEntityId {
    fn ent_id(&self) -> Option<EntityId>;
}

impl HasEntityId for AnyPrimaryUnit {
    fn ent_id(&self) -> Option<EntityId> {
        delegate_primary!(self, unit, unit.ident.decl.get())
    }
}

impl HasEntityId for AnyDesignUnit {
    fn ent_id(&self) -> Option<EntityId> {
        match self {
            AnyDesignUnit::Primary(primary) => match primary {
                AnyPrimaryUnit::Entity(ent) => ent.ident.decl.get(),
                AnyPrimaryUnit::Configuration(config) => config.ident.decl.get(),
                AnyPrimaryUnit::Package(pkg) => pkg.ident.decl.get(),
                AnyPrimaryUnit::PackageInstance(inst) => inst.ident.decl.get(),
                AnyPrimaryUnit::Context(ctx) => ctx.ident.decl.get(),
            },
            AnyDesignUnit::Secondary(secondary) => match secondary {
                AnySecondaryUnit::Architecture(arch) => arch.ident.decl.get(),
                AnySecondaryUnit::PackageBody(bod) => bod.ident.decl.get(),
            },
        }
    }
}

impl HasEntityId for SubprogramSpecification {
    fn ent_id(&self) -> Option<EntityId> {
        match self {
            SubprogramSpecification::Procedure(proc) => proc.designator.decl.get(),
            SubprogramSpecification::Function(func) => func.designator.decl.get(),
        }
    }
}

impl HasEntityId for InterfacePackageDeclaration {
    fn ent_id(&self) -> Option<EntityId> {
        self.ident.decl.get()
    }
}

impl HasEntityId for SubprogramInstantiation {
    fn ent_id(&self) -> Option<EntityId> {
        self.ident.decl.get()
    }
}

impl HasEntityId for PackageInstantiation {
    fn ent_id(&self) -> Option<EntityId> {
        self.ident.decl.get()
    }
}

impl HasEntityId for SubprogramBody {
    fn ent_id(&self) -> Option<EntityId> {
        self.specification.ent_id()
    }
}

impl HasEntityId for AliasDeclaration {
    fn ent_id(&self) -> Option<EntityId> {
        self.designator.decl.get()
    }
}

impl HasEntityId for TypeDeclaration {
    fn ent_id(&self) -> Option<EntityId> {
        self.ident.decl.get()
    }
}

impl HasEntityId for ComponentDeclaration {
    fn ent_id(&self) -> Option<EntityId> {
        self.ident.decl.get()
    }
}

impl HasEntityId for Attribute {
    fn ent_id(&self) -> Option<EntityId> {
        match self {
            Attribute::Specification(spec) => spec.ent_id(),
            Attribute::Declaration(decl) => decl.ent_id(),
        }
    }
}

impl HasEntityId for AttributeDeclaration {
    fn ent_id(&self) -> Option<EntityId> {
        self.ident.decl.get()
    }
}

impl HasEntityId for AttributeSpecification {
    fn ent_id(&self) -> Option<EntityId> {
        self.ident.reference.get()
    }
}

impl HasEntityId for ModeViewDeclaration {
    fn ent_id(&self) -> Option<EntityId> {
        self.ident.decl.get()
    }
}

impl<T> HasEntityId for WithTokenSpan<T>
where
    T: HasEntityId,
{
    fn ent_id(&self) -> Option<EntityId> {
        self.item.ent_id()
    }
}

impl WithDecl<Ident> {
    pub fn define<'a>(
        &mut self,
        ctx: &dyn TokenAccess,
        arena: &'a Arena,
        parent: EntRef<'a>,
        kind: AnyEntKind<'a>,
        src_span: TokenSpan,
        source: Option<Source>,
    ) -> EntRef<'a> {
        let ent = arena.explicit(
            self.tree.name().clone(),
            parent,
            kind,
            Some(self.tree.pos(ctx)),
            src_span,
            source,
        );
        self.decl.set(ent.id());
        ent
    }
}

impl WithDecl<WithToken<Designator>> {
    pub fn define<'a>(
        &mut self,
        ctx: &dyn TokenAccess,
        arena: &'a Arena,
        parent: EntRef<'a>,
        kind: AnyEntKind<'a>,
        src_span: TokenSpan,
        source: Option<Source>,
    ) -> EntRef<'a> {
        let ent = arena.explicit(
            self.tree.item.clone(),
            parent,
            kind,
            Some(self.tree.pos(ctx)),
            src_span,
            source,
        );
        self.decl.set(ent.id());
        ent
    }
}

impl SubprogramSpecification {
    pub fn set_decl_id(&mut self, id: EntityId) {
        match self {
            SubprogramSpecification::Function(f) => f.designator.decl.set(id),
            SubprogramSpecification::Procedure(p) => p.designator.decl.set(id),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Concurrent {
    Block,
    Process,
    Generate,
    Instance,
}

impl Concurrent {
    fn describe(&self) -> &'static str {
        match self {
            Concurrent::Block => "block",
            Concurrent::Process => "process",
            Concurrent::Generate => "generate",
            Concurrent::Instance => "instance",
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Sequential {
    Loop,
    If,
    Case,
}

impl Sequential {
    fn describe(&self) -> &'static str {
        match self {
            Sequential::Case => "case statement",
            Sequential::If => "if statement",
            Sequential::Loop => "loop statement",
        }
    }
}
