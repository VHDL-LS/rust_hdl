// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com
use crate::ast::*;
use super::library::{EntityDesignUnit, Library, PackageDesignUnit};
use crate::message::{Message, MessageHandler};
use crate::source::{SrcPos, WithPos};

use self::fnv::FnvHashMap;
use fnv;
use std::collections::hash_map::Entry;
use std::ops::Deref;
use std::sync::Arc;

/// The analysis result of the primary unit
#[derive(Clone)]
pub struct PrimaryUnitData<'a> {
    messages: Vec<Message>,
    pub region: DeclarativeRegion<'a, 'a>,
}

// @TODO store data in library, declarative region or in analysis context?
impl<'a> PrimaryUnitData<'a> {
    pub fn new(messages: Vec<Message>, region: DeclarativeRegion<'a, 'a>) -> PrimaryUnitData<'_> {
        PrimaryUnitData { messages, region }
    }

    pub fn push_to(&self, messages: &mut dyn MessageHandler) {
        for message in self.messages.iter().cloned() {
            messages.push(message);
        }
    }
}

#[derive(Clone)]
pub enum AnyDeclaration<'a> {
    Declaration(&'a Declaration),
    Element(&'a ElementDeclaration),
    Enum(&'a WithPos<EnumerationLiteral>),
    Interface(&'a InterfaceDeclaration),
    Library(&'a Library),
    Package(&'a Library, &'a PackageDesignUnit),
    Context(&'a ContextDeclaration),
    Entity(&'a EntityDesignUnit),
    Configuration(&'a DesignUnit<ConfigurationDeclaration>),
    PackageInstance(&'a Library, &'a DesignUnit<PackageInstantiation>),
    LocalPackageInstance(&'a Ident, Arc<PrimaryUnitData<'a>>),
}

impl<'a> AnyDeclaration<'a> {
    fn is_deferred_constant(&self) -> bool {
        match self {
            AnyDeclaration::Declaration(Declaration::Object(ObjectDeclaration {
                ref class,
                ref expression,
                ..
            })) => *class == ObjectClass::Constant && expression.is_none(),
            _ => false,
        }
    }

    fn is_non_deferred_constant(&self) -> bool {
        match self {
            AnyDeclaration::Declaration(Declaration::Object(ObjectDeclaration {
                ref class,
                ref expression,
                ..
            })) => *class == ObjectClass::Constant && expression.is_some(),
            _ => false,
        }
    }

    fn is_protected_type(&self) -> bool {
        match self {
            AnyDeclaration::Declaration(Declaration::Type(TypeDeclaration {
                def: TypeDefinition::Protected { .. },
                ..
            })) => true,
            _ => false,
        }
    }

    fn is_protected_type_body(&self) -> bool {
        match self {
            AnyDeclaration::Declaration(Declaration::Type(TypeDeclaration {
                def: TypeDefinition::ProtectedBody { .. },
                ..
            })) => true,
            _ => false,
        }
    }

    fn is_incomplete_type(&self) -> bool {
        match self {
            AnyDeclaration::Declaration(Declaration::Type(TypeDeclaration {
                def: TypeDefinition::Incomplete,
                ..
            })) => true,
            _ => false,
        }
    }

    fn is_type_declaration(&self) -> bool {
        match self {
            AnyDeclaration::Declaration(Declaration::Type(..)) => true,
            _ => false,
        }
    }
}

#[derive(Clone)]
pub struct VisibleDeclaration<'a> {
    pub designator: Designator,

    /// The location where the declaration was made
    /// Builtin and implicit declaration will not have a source position
    pub decl_pos: Option<SrcPos>,
    pub decl: AnyDeclaration<'a>,
    pub may_overload: bool,
}

impl<'a> VisibleDeclaration<'a> {
    pub fn new(
        designator: impl Into<WithPos<Designator>>,
        decl: AnyDeclaration<'a>,
    ) -> VisibleDeclaration<'a> {
        let designator = designator.into();
        VisibleDeclaration {
            designator: designator.item,
            decl_pos: Some(designator.pos),
            decl,
            may_overload: false,
        }
    }

    fn error(&self, messages: &mut dyn MessageHandler, message: impl Into<String>) {
        if let Some(ref pos) = self.decl_pos {
            messages.push(Message::error(pos, message));
        }
    }

    fn hint(&self, messages: &mut dyn MessageHandler, message: impl Into<String>) {
        if let Some(ref pos) = self.decl_pos {
            messages.push(Message::hint(pos, message));
        }
    }

    pub fn with_overload(mut self, value: bool) -> VisibleDeclaration<'a> {
        self.may_overload = value;
        self
    }

    fn is_deferred_of(&self, other: &Self) -> bool {
        (self.decl.is_deferred_constant() && other.decl.is_non_deferred_constant())
            || (self.decl.is_protected_type() && other.decl.is_protected_type_body())
            || (self.decl.is_incomplete_type()
                && other.decl.is_type_declaration()
                && !other.decl.is_incomplete_type())
    }
}

#[derive(Copy, Clone, PartialEq)]
enum RegionKind {
    PackageDeclaration,
    PackageBody,
    Other,
}

/// Most parent regions can just be temporarily borrowed
/// For public regions of design units the parent must be owned such that these regions can be stored in a map
#[derive(Clone)]
enum ParentRegion<'r, 'a: 'r> {
    Borrowed(&'r DeclarativeRegion<'r, 'a>),
    Owned(Box<DeclarativeRegion<'r, 'a>>),
}

impl<'r, 'a> Deref for ParentRegion<'r, 'a> {
    type Target = DeclarativeRegion<'r, 'a>;

    fn deref(&self) -> &DeclarativeRegion<'r, 'a> {
        match self {
            ParentRegion::Borrowed(region) => region,
            ParentRegion::Owned(ref region) => region.as_ref(),
        }
    }
}

impl<'r, 'a> ParentRegion<'r, 'a> {
    fn into_owned(self) -> ParentRegion<'a, 'a> {
        match self {
            ParentRegion::Borrowed(region) => {
                ParentRegion::Owned(Box::new(region.clone().into_owned_parent()))
            }
            ParentRegion::Owned(region) => {
                ParentRegion::Owned(Box::new(region.into_owned_parent()))
            }
        }
    }
}

#[derive(Clone)]
pub struct DeclarativeRegion<'r, 'a: 'r> {
    parent: Option<ParentRegion<'r, 'a>>,
    visible: FnvHashMap<Designator, VisibleDeclaration<'a>>,
    decls: FnvHashMap<Designator, VisibleDeclaration<'a>>,
    kind: RegionKind,
}

impl<'r, 'a: 'r> DeclarativeRegion<'r, 'a> {
    pub fn new(parent: Option<&'r DeclarativeRegion<'r, 'a>>) -> DeclarativeRegion<'r, 'a> {
        DeclarativeRegion {
            parent: parent.map(|parent| ParentRegion::Borrowed(parent)),
            visible: FnvHashMap::default(),
            decls: FnvHashMap::default(),
            kind: RegionKind::Other,
        }
    }

    pub fn in_package_declaration(mut self) -> DeclarativeRegion<'r, 'a> {
        self.kind = RegionKind::PackageDeclaration;
        self
    }

    /// Clone the region with owned version of all parents
    pub fn into_owned_parent<'s>(self) -> DeclarativeRegion<'s, 'a> {
        let parent = {
            if let Some(parent) = self.parent {
                Some(parent.into_owned())
            } else {
                None
            }
        };

        DeclarativeRegion {
            parent,
            visible: self.visible,
            decls: self.decls,
            kind: self.kind,
        }
    }

    pub fn clone_parent(&self) -> Option<DeclarativeRegion<'r, 'a>> {
        self.parent.as_ref().map(|parent| parent.deref().to_owned())
    }

    pub fn into_extended(self, parent: &'r DeclarativeRegion<'r, 'a>) -> DeclarativeRegion<'r, 'a> {
        let kind = match self.kind {
            RegionKind::PackageDeclaration => RegionKind::PackageBody,
            _ => RegionKind::Other,
        };

        DeclarativeRegion {
            parent: Some(ParentRegion::Borrowed(parent)),
            visible: self.visible,
            decls: self.decls,
            kind,
        }
    }

    pub fn close_immediate(&mut self, messages: &mut dyn MessageHandler) {
        let mut to_remove = Vec::new();

        for decl in self.decls.values() {
            if decl.decl.is_incomplete_type() {
                to_remove.push(decl.designator.clone());
                decl.error(
                    messages,
                    format!(
                        "Missing full type declaration of incomplete type '{}'",
                        &decl.designator
                    ),
                );
                decl.hint(messages, "The full type declaration shall occur immediately within the same declarative part");
            }
        }

        for designator in to_remove {
            self.decls.remove(&designator);
        }
    }

    pub fn close_extended(&mut self, messages: &mut dyn MessageHandler) {
        let mut to_remove = Vec::new();

        for decl in self.decls.values() {
            if decl.decl.is_deferred_constant() {
                to_remove.push(decl.designator.clone());
                decl.error(messages, format!("Deferred constant '{}' lacks corresponding full constant declaration in package body", &decl.designator));
            } else if decl.decl.is_protected_type() {
                to_remove.push(decl.designator.clone());
                decl.error(
                    messages,
                    format!("Missing body for protected type '{}'", &decl.designator),
                );
            }
        }

        for designator in to_remove {
            self.decls.remove(&designator);
        }
    }

    pub fn close_both(&mut self, messages: &mut dyn MessageHandler) {
        self.close_immediate(messages);
        self.close_extended(messages);
    }

    pub fn add(&mut self, decl: VisibleDeclaration<'a>, messages: &mut dyn MessageHandler) {
        if self.kind != RegionKind::PackageDeclaration && decl.decl.is_deferred_constant() {
            decl.error(
                messages,
                "Deferred constants are only allowed in package declarations (not body)",
            );
        }

        match self.decls.entry(decl.designator.clone()) {
            Entry::Occupied(mut entry) => {
                let old_decl = entry.get_mut();

                if !decl.may_overload || !old_decl.may_overload {
                    if old_decl.is_deferred_of(&decl) {
                        if self.kind != RegionKind::PackageBody
                            && decl.decl.is_non_deferred_constant()
                        {
                            decl.error(messages, "Full declaration of deferred constant is only allowed in a package body");
                        }

                        std::mem::replace(old_decl, decl);
                    } else if let Some(ref pos) = decl.decl_pos {
                        let mut msg = Message::error(
                            pos,
                            format!("Duplicate declaration of '{}'", decl.designator),
                        );

                        if let Some(ref old_pos) = old_decl.decl_pos {
                            msg.add_related(old_pos, "Previously defined here");
                        }

                        messages.push(msg)
                    }
                }
            }
            Entry::Vacant(entry) => {
                if decl.decl.is_protected_type_body() {
                    decl.error(
                        messages,
                        format!("No declaration of protected type '{}'", &decl.designator),
                    );
                } else {
                    entry.insert(decl);
                }
            }
        }
    }

    pub fn make_library_visible(
        &mut self,
        designator: impl Into<Designator>,
        library: &'a Library,
    ) {
        let decl = VisibleDeclaration {
            designator: designator.into(),
            decl_pos: None,
            decl: AnyDeclaration::Library(library),
            may_overload: false,
        };
        self.visible.insert(decl.designator.clone(), decl);
    }

    pub fn make_potentially_visible(&mut self, decl: impl Into<VisibleDeclaration<'a>>) {
        let decl = decl.into();
        self.visible.insert(decl.designator.clone(), decl);
    }

    pub fn make_all_potentially_visible(&mut self, region: &DeclarativeRegion<'_, 'a>) {
        for decl in region.decls.values() {
            self.make_potentially_visible(decl.clone());
        }
    }

    pub fn lookup(&self, designator: &Designator, inside: bool) -> Option<&VisibleDeclaration<'a>> {
        self.decls
            .get(designator)
            .or_else(|| {
                if inside {
                    self.visible.get(designator)
                } else {
                    None
                }
            })
            .or_else(|| {
                self.parent
                    .as_ref()
                    .and_then(|parent| parent.lookup(designator, inside))
            })
    }
}
