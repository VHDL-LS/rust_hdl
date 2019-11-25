// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com
use super::library::Library;
use crate::ast::*;
use crate::diagnostic::{Diagnostic, DiagnosticHandler};
use crate::source::{SrcPos, WithPos};
use crate::symbol_table::Symbol;

use self::fnv::FnvHashMap;
use fnv;
use std::collections::hash_map::Entry;
use std::ops::Deref;
use std::sync::Arc;

/// The analysis result of the primary unit
#[derive(Clone)]
pub struct PrimaryUnitData {
    diagnostics: Vec<Diagnostic>,
    pub region: DeclarativeRegion<'static>,
}

// @TODO store data in library, declarative region or in analysis context?
impl PrimaryUnitData {
    pub fn new(
        diagnostics: Vec<Diagnostic>,
        region: DeclarativeRegion<'static>,
    ) -> PrimaryUnitData {
        PrimaryUnitData {
            diagnostics,
            region,
        }
    }

    pub fn push_to(&self, diagnostics: &mut dyn DiagnosticHandler) {
        for diagnostic in self.diagnostics.iter().cloned() {
            diagnostics.push(diagnostic);
        }
    }
}

#[derive(Clone)]
pub enum AnyDeclaration {
    Other,
    TypeDeclaration,
    IncompleteType,
    Constant,
    DeferredConstant,
    ProtectedType,
    ProtectedTypeBody,
    Library(Symbol),
    Package(Symbol, Symbol),
    Context(Symbol, Symbol),
    PackageInstance(Symbol, Symbol),
    LocalPackageInstance(Symbol, Arc<PrimaryUnitData>),
}

impl AnyDeclaration {
    pub fn from_declaration(decl: &Declaration) -> AnyDeclaration {
        match decl {
            Declaration::Object(ObjectDeclaration {
                class: ObjectClass::Constant,
                ref expression,
                ..
            }) => {
                if expression.is_none() {
                    AnyDeclaration::DeferredConstant
                } else {
                    AnyDeclaration::Constant
                }
            }
            Declaration::Type(TypeDeclaration {
                def: TypeDefinition::Protected { .. },
                ..
            }) => AnyDeclaration::ProtectedType,
            Declaration::Type(TypeDeclaration {
                def: TypeDefinition::ProtectedBody { .. },
                ..
            }) => AnyDeclaration::ProtectedTypeBody,
            Declaration::Type(TypeDeclaration {
                def: TypeDefinition::Incomplete,
                ..
            }) => AnyDeclaration::IncompleteType,
            Declaration::Type(..) => return AnyDeclaration::TypeDeclaration,
            _ => AnyDeclaration::Other,
        }
    }

    fn is_deferred_constant(&self) -> bool {
        if let AnyDeclaration::DeferredConstant = self {
            true
        } else {
            false
        }
    }

    fn is_non_deferred_constant(&self) -> bool {
        if let AnyDeclaration::Constant = self {
            true
        } else {
            false
        }
    }

    fn is_protected_type(&self) -> bool {
        if let AnyDeclaration::ProtectedType = self {
            true
        } else {
            false
        }
    }

    fn is_protected_type_body(&self) -> bool {
        if let AnyDeclaration::ProtectedTypeBody = self {
            true
        } else {
            false
        }
    }

    fn is_incomplete_type(&self) -> bool {
        if let AnyDeclaration::IncompleteType = self {
            true
        } else {
            false
        }
    }

    fn is_type_declaration(&self) -> bool {
        if let AnyDeclaration::TypeDeclaration = self {
            true
        } else {
            false
        }
    }
}

#[derive(Clone)]
pub struct VisibleDeclaration {
    pub designator: Designator,

    /// The location where the declaration was made
    /// Builtin and implicit declaration will not have a source position
    pub decl_pos: Option<SrcPos>,
    pub decl: AnyDeclaration,
    pub may_overload: bool,
}

impl VisibleDeclaration {
    pub fn new(
        designator: impl Into<WithPos<Designator>>,
        decl: AnyDeclaration,
    ) -> VisibleDeclaration {
        let designator = designator.into();
        VisibleDeclaration {
            designator: designator.item,
            decl_pos: Some(designator.pos),
            decl,
            may_overload: false,
        }
    }

    fn error(&self, diagnostics: &mut dyn DiagnosticHandler, message: impl Into<String>) {
        if let Some(ref pos) = self.decl_pos {
            diagnostics.push(Diagnostic::error(pos, message));
        }
    }

    fn hint(&self, diagnostics: &mut dyn DiagnosticHandler, message: impl Into<String>) {
        if let Some(ref pos) = self.decl_pos {
            diagnostics.push(Diagnostic::hint(pos, message));
        }
    }

    pub fn with_overload(mut self, value: bool) -> VisibleDeclaration {
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
enum ParentRegion<'a> {
    Borrowed(&'a DeclarativeRegion<'a>),
    Owned(Box<DeclarativeRegion<'static>>),
}

impl<'a> Deref for ParentRegion<'a> {
    type Target = DeclarativeRegion<'a>;

    fn deref(&self) -> &DeclarativeRegion<'a> {
        match self {
            ParentRegion::Borrowed(region) => region,
            ParentRegion::Owned(ref region) => region.as_ref(),
        }
    }
}

#[derive(Clone)]
pub struct DeclarativeRegion<'a> {
    parent: Option<ParentRegion<'a>>,
    visible: FnvHashMap<Designator, VisibleDeclaration>,
    decls: FnvHashMap<Designator, VisibleDeclaration>,
    kind: RegionKind,
}

impl<'a> DeclarativeRegion<'a> {
    pub fn new(parent: Option<&'a DeclarativeRegion<'a>>) -> DeclarativeRegion<'a> {
        DeclarativeRegion {
            parent: parent.map(|parent| ParentRegion::Borrowed(parent)),
            visible: FnvHashMap::default(),
            decls: FnvHashMap::default(),
            kind: RegionKind::Other,
        }
    }

    pub fn new_owned_parent(parent: Box<DeclarativeRegion<'static>>) -> DeclarativeRegion<'static> {
        DeclarativeRegion {
            parent: Some(ParentRegion::Owned(parent)),
            visible: FnvHashMap::default(),
            decls: FnvHashMap::default(),
            kind: RegionKind::Other,
        }
    }

    pub fn in_package_declaration(mut self) -> DeclarativeRegion<'a> {
        self.kind = RegionKind::PackageDeclaration;
        self
    }

    pub fn clone_parent(&self) -> Option<DeclarativeRegion<'a>> {
        self.parent.as_ref().map(|parent| parent.deref().to_owned())
    }

    pub fn into_extended(self, parent: &'a DeclarativeRegion<'a>) -> DeclarativeRegion<'a> {
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

    pub fn close_immediate(&mut self, diagnostics: &mut dyn DiagnosticHandler) {
        let mut to_remove = Vec::new();

        for decl in self.decls.values() {
            if decl.decl.is_incomplete_type() {
                to_remove.push(decl.designator.clone());
                decl.error(
                    diagnostics,
                    format!(
                        "Missing full type declaration of incomplete type '{}'",
                        &decl.designator
                    ),
                );
                decl.hint(diagnostics, "The full type declaration shall occur immediately within the same declarative part");
            }
        }

        for designator in to_remove {
            self.decls.remove(&designator);
        }
    }

    pub fn close_extended(&mut self, diagnostics: &mut dyn DiagnosticHandler) {
        let mut to_remove = Vec::new();

        for decl in self.decls.values() {
            if decl.decl.is_deferred_constant() {
                to_remove.push(decl.designator.clone());
                decl.error(diagnostics, format!("Deferred constant '{}' lacks corresponding full constant declaration in package body", &decl.designator));
            } else if decl.decl.is_protected_type() {
                to_remove.push(decl.designator.clone());
                decl.error(
                    diagnostics,
                    format!("Missing body for protected type '{}'", &decl.designator),
                );
            }
        }

        for designator in to_remove {
            self.decls.remove(&designator);
        }
    }

    pub fn close_both(&mut self, diagnostics: &mut dyn DiagnosticHandler) {
        self.close_immediate(diagnostics);
        self.close_extended(diagnostics);
    }

    pub fn add(&mut self, decl: VisibleDeclaration, diagnostics: &mut dyn DiagnosticHandler) {
        if self.kind != RegionKind::PackageDeclaration && decl.decl.is_deferred_constant() {
            decl.error(
                diagnostics,
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
                            decl.error(diagnostics, "Full declaration of deferred constant is only allowed in a package body");
                        }

                        std::mem::replace(old_decl, decl);
                    } else if let Some(ref pos) = decl.decl_pos {
                        let mut diagnostic = Diagnostic::error(
                            pos,
                            format!("Duplicate declaration of '{}'", decl.designator),
                        );

                        if let Some(ref old_pos) = old_decl.decl_pos {
                            diagnostic.add_related(old_pos, "Previously defined here");
                        }

                        diagnostics.push(diagnostic)
                    }
                }
            }
            Entry::Vacant(entry) => {
                if decl.decl.is_protected_type_body() {
                    decl.error(
                        diagnostics,
                        format!("No declaration of protected type '{}'", &decl.designator),
                    );
                } else {
                    entry.insert(decl);
                }
            }
        }
    }

    pub fn make_library_visible(&mut self, designator: impl Into<Designator>, library: &Library) {
        let decl = VisibleDeclaration {
            designator: designator.into(),
            decl_pos: None,
            decl: AnyDeclaration::Library(library.name.clone()),
            may_overload: false,
        };
        self.visible.insert(decl.designator.clone(), decl);
    }

    pub fn make_potentially_visible(&mut self, decl: impl Into<VisibleDeclaration>) {
        let decl = decl.into();
        self.visible.insert(decl.designator.clone(), decl);
    }

    pub fn make_all_potentially_visible(&mut self, region: &DeclarativeRegion<'a>) {
        for decl in region.decls.values() {
            self.make_potentially_visible(decl.clone());
        }
    }

    pub fn lookup(&self, designator: &Designator, inside: bool) -> Option<&VisibleDeclaration> {
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
