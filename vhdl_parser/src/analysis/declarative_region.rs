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
    Overloaded,
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
    pub fn from_object_declaration(decl: &ObjectDeclaration) -> AnyDeclaration {
        match decl {
            ObjectDeclaration {
                class: ObjectClass::Constant,
                ref expression,
                ..
            } => {
                if expression.is_none() {
                    AnyDeclaration::DeferredConstant
                } else {
                    AnyDeclaration::Constant
                }
            }
            _ => AnyDeclaration::Other,
        }
    }

    pub fn from_type_declaration(decl: &TypeDeclaration) -> AnyDeclaration {
        match decl {
            TypeDeclaration {
                def: TypeDefinition::Protected { .. },
                ..
            } => AnyDeclaration::ProtectedType,
            TypeDeclaration {
                def: TypeDefinition::ProtectedBody { .. },
                ..
            } => AnyDeclaration::ProtectedTypeBody,
            TypeDeclaration {
                def: TypeDefinition::Incomplete,
                ..
            } => AnyDeclaration::IncompleteType,
            _ => AnyDeclaration::TypeDeclaration,
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
struct AnyDeclarationData {
    /// The location where the declaration was made
    /// Builtin and implicit declaration will not have a source position
    decl_pos: Option<SrcPos>,
    decl: AnyDeclaration,
}

impl AnyDeclarationData {
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
}

#[derive(Clone)]
pub struct VisibleDeclaration {
    pub designator: Designator,
    data: Vec<AnyDeclarationData>,
}

impl VisibleDeclaration {
    pub fn new(
        designator: impl Into<WithPos<Designator>>,
        decl: AnyDeclaration,
    ) -> VisibleDeclaration {
        let designator = designator.into();

        VisibleDeclaration {
            designator: designator.item,
            data: vec![AnyDeclarationData {
                decl_pos: Some(designator.pos),
                decl,
            }],
        }
    }

    fn first_data(&self) -> &AnyDeclarationData {
        self.data
            .first()
            .expect("Declaration always contains one entry")
    }

    pub fn first(&self) -> &AnyDeclaration {
        &self.first_data().decl
    }

    pub fn second(&self) -> Option<&AnyDeclaration> {
        self.data.get(1).map(|data| &data.decl)
    }

    fn is_overloaded(&self) -> bool {
        if let AnyDeclaration::Overloaded = self.first_data().decl {
            true
        } else {
            false
        }
    }

    /// Return a duplicate declaration of the previous declaration if it exists
    fn find_duplicate_of<'a>(&self, prev_decl: &'a Self) -> Option<&'a AnyDeclarationData> {
        if self.is_overloaded() && prev_decl.is_overloaded() {
            return None;
        }

        let ref later_decl = self.first();
        for prev_decl_data in prev_decl.data.iter() {
            let ref prev_decl = prev_decl_data.decl;

            match prev_decl {
                // Everything expect deferred combinations are forbidden
                AnyDeclaration::DeferredConstant if later_decl.is_non_deferred_constant() => {}
                AnyDeclaration::ProtectedType if later_decl.is_protected_type_body() => {}
                AnyDeclaration::IncompleteType if later_decl.is_type_declaration() => {}
                _ => {
                    return Some(prev_decl_data);
                }
            }
        }
        None
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
    extends: Option<ParentRegion<'a>>,
    visible: FnvHashMap<Designator, VisibleDeclaration>,
    decls: FnvHashMap<Designator, VisibleDeclaration>,
    kind: RegionKind,
}

impl<'a> DeclarativeRegion<'a> {
    pub fn new(parent: Option<&'a DeclarativeRegion<'a>>) -> DeclarativeRegion<'a> {
        DeclarativeRegion {
            parent: parent.map(|parent| ParentRegion::Borrowed(parent)),
            extends: None,
            visible: FnvHashMap::default(),
            decls: FnvHashMap::default(),
            kind: RegionKind::Other,
        }
    }

    pub fn new_owned_parent(parent: Box<DeclarativeRegion<'static>>) -> DeclarativeRegion<'static> {
        DeclarativeRegion {
            parent: Some(ParentRegion::Owned(parent)),
            extends: None,
            visible: FnvHashMap::default(),
            decls: FnvHashMap::default(),
            kind: RegionKind::Other,
        }
    }

    pub fn get_parent(&'a self) -> Option<&'a DeclarativeRegion<'a>> {
        self.parent.as_ref().map(|parent| parent.deref())
    }

    pub fn in_package_declaration(mut self) -> DeclarativeRegion<'a> {
        self.kind = RegionKind::PackageDeclaration;
        self
    }

    pub fn extend(&'a self, parent: Option<&'a DeclarativeRegion<'a>>) -> DeclarativeRegion<'a> {
        let kind = match self.kind {
            RegionKind::PackageDeclaration => RegionKind::PackageBody,
            _ => RegionKind::Other,
        };

        DeclarativeRegion {
            parent: parent.map(|parent| ParentRegion::Borrowed(parent)),
            extends: Some(ParentRegion::Borrowed(self)),
            visible: FnvHashMap::default(),
            decls: FnvHashMap::default(),
            kind,
        }
    }

    pub fn close_immediate(&mut self, diagnostics: &mut dyn DiagnosticHandler) {
        assert!(self.extends.is_none());
        self.check_incomplete_types_are_defined(diagnostics);
    }

    /// Incomplete types must be defined in the same immediate region as they are declared
    fn check_incomplete_types_are_defined(&self, diagnostics: &mut dyn DiagnosticHandler) {
        for decl in self.decls.values() {
            if decl.first().is_incomplete_type() {
                let mut check_ok = false;
                if let Some(second) = decl.second() {
                    if second.is_type_declaration() {
                        check_ok = true;
                    }
                }

                if !check_ok {
                    decl.first_data().error(
                        diagnostics,
                        format!(
                            "Missing full type declaration of incomplete type '{}'",
                            &decl.designator
                        ),
                    );
                    decl.first_data().hint(diagnostics, "The full type declaration shall occur immediately within the same declarative part");
                }
            }
        }
    }

    fn check_deferred_constant_pairs(&self, diagnostics: &mut dyn DiagnosticHandler) {
        match self.kind {
            // Package without body may not have deferred constants
            RegionKind::PackageDeclaration => {
                for decl in self.decls.values() {
                    match decl.first() {
                        AnyDeclaration::DeferredConstant => {
                            decl.first_data().error(diagnostics, format!("Deferred constant '{}' lacks corresponding full constant declaration in package body", &decl.designator));
                        }
                        _ => {}
                    }
                }
            }
            RegionKind::PackageBody => {
                let ref extends = self
                    .extends
                    .as_ref()
                    .expect("Package body must extend package");
                for ext_decl in extends.decls.values() {
                    match ext_decl.first() {
                        AnyDeclaration::DeferredConstant => {
                            // Deferred constants may only be located in a package
                            // And only matched with a constant in the body
                            let mut found = false;
                            let decl = self.decls.get(&ext_decl.designator);

                            if let Some(decl) = decl {
                                if let AnyDeclaration::Constant = decl.first() {
                                    found = true;
                                }
                            }

                            if !found {
                                ext_decl.first_data().error(diagnostics, format!("Deferred constant '{}' lacks corresponding full constant declaration in package body", &ext_decl.designator));
                            }
                        }
                        _ => {}
                    }
                }
            }
            RegionKind::Other => {}
        }
    }

    fn check_protected_types_have_body(&self, diagnostics: &mut dyn DiagnosticHandler) {
        for decl in self.decls.values() {
            if decl.first().is_protected_type() {
                if let Some(second_decl) = decl.second() {
                    if second_decl.is_protected_type_body() {
                        continue;
                    }
                }
                decl.first_data().error(
                    diagnostics,
                    format!("Missing body for protected type '{}'", &decl.designator),
                );
            }
        }

        if let Some(ref extends) = self.extends {
            for ext_decl in extends.decls.values() {
                if ext_decl.first().is_protected_type() {
                    if let Some(second_decl) = ext_decl.second() {
                        if second_decl.is_protected_type_body() {
                            continue;
                        }
                    }

                    if let Some(decl) = self.decls.get(&ext_decl.designator) {
                        if decl.first().is_protected_type_body() {
                            continue;
                        }
                    }
                    ext_decl.first_data().error(
                        diagnostics,
                        format!("Missing body for protected type '{}'", &ext_decl.designator),
                    );
                }
            }
        }
    }

    #[must_use]
    fn check_deferred_constant_only_in_package(
        &self,
        decl: &VisibleDeclaration,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> bool {
        if self.kind != RegionKind::PackageDeclaration && decl.first().is_deferred_constant() {
            decl.first_data().error(
                diagnostics,
                "Deferred constants are only allowed in package declarations (not body)",
            );
            false
        } else {
            true
        }
    }

    #[must_use]
    fn check_full_constand_of_deferred_only_in_body(
        &self,
        decl: &VisibleDeclaration,
        prev_decl: &VisibleDeclaration,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> bool {
        if self.kind != RegionKind::PackageBody && decl.first().is_non_deferred_constant() {
            if prev_decl.first().is_deferred_constant() {
                decl.first_data().error(
                    diagnostics,
                    "Full declaration of deferred constant is only allowed in a package body",
                );
                return false;
            }
        }
        true
    }

    pub fn close_both(&mut self, diagnostics: &mut dyn DiagnosticHandler) {
        self.check_incomplete_types_are_defined(diagnostics);
        self.check_deferred_constant_pairs(diagnostics);
        self.check_protected_types_have_body(diagnostics);
    }

    /// Check duplicate declarations
    /// Allow deferred constants, incomplete types and protected type bodies
    /// Returns true if the declaration does not duplicates an existing declaration
    #[must_use]
    fn check_duplicate(
        decl: &VisibleDeclaration,
        prev_decl: &VisibleDeclaration,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> bool {
        if let Some(duplicate_decl) = decl.find_duplicate_of(&prev_decl) {
            if let Some(ref pos) = decl.first_data().decl_pos {
                let mut diagnostic = Diagnostic::error(
                    pos,
                    format!("Duplicate declaration of '{}'", decl.designator),
                );

                if let Some(ref prev_pos) = duplicate_decl.decl_pos {
                    diagnostic.add_related(prev_pos, "Previously defined here");
                }

                diagnostics.push(diagnostic)
            }
            false
        } else {
            true
        }
    }

    /// true if the declaration can be added
    fn check_add(
        // The declaration to add
        decl: &VisibleDeclaration,
        // Previous declaration in the same region
        prev_decl: Option<&VisibleDeclaration>,
        // Previous declaration in the region extended by this region
        ext_decl: Option<&VisibleDeclaration>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> bool {
        let mut check_ok = true;

        if let Some(prev_decl) = prev_decl {
            if !Self::check_duplicate(&decl, &prev_decl, diagnostics) {
                check_ok = false;
            }
        }

        if let Some(ext_decl) = ext_decl {
            if !Self::check_duplicate(&decl, &ext_decl, diagnostics) {
                check_ok = false;
            }
        }

        if decl.first().is_protected_type_body() {
            let mut is_orphan = true;

            if let Some(prev_decl) = prev_decl {
                if prev_decl.first().is_protected_type() {
                    is_orphan = false;
                }
            }

            if let Some(ext_decl) = ext_decl {
                if ext_decl.first().is_protected_type() {
                    is_orphan = false;
                }
            }
            if is_orphan {
                decl.first_data().error(
                    diagnostics,
                    format!("No declaration of protected type '{}'", &decl.designator),
                );
                check_ok = false;
            }
        }

        check_ok
    }

    pub fn add(
        &mut self,
        designator: impl Into<WithPos<Designator>>,
        decl: AnyDeclaration,
        diagnostics: &mut dyn DiagnosticHandler,
    ) {
        let decl = VisibleDeclaration::new(designator, decl);

        let ext_decl = self
            .extends
            .as_ref()
            .and_then(|extends| extends.decls.get(&decl.designator));

        if !self.check_deferred_constant_only_in_package(&decl, diagnostics) {
            return;
        }

        if let Some(ext_decl) = ext_decl {
            if !self.check_full_constand_of_deferred_only_in_body(&decl, ext_decl, diagnostics) {
                return;
            }
        }

        // @TODO merge with .entry below
        if let Some(prev_decl) = self.decls.get(&decl.designator) {
            if !self.check_full_constand_of_deferred_only_in_body(&decl, prev_decl, diagnostics) {
                return;
            }
        }

        match self.decls.entry(decl.designator.clone()) {
            Entry::Occupied(ref mut entry) => {
                let prev_decl = entry.get_mut();

                if Self::check_add(&decl, Some(&prev_decl), ext_decl, diagnostics) {
                    let mut decl = decl;
                    prev_decl.data.append(&mut decl.data);
                }
            }
            Entry::Vacant(entry) => {
                if Self::check_add(&decl, None, ext_decl, diagnostics) {
                    entry.insert(decl);
                }
            }
        }
    }

    pub fn make_library_visible(&mut self, designator: impl Into<Designator>, library: &Library) {
        let decl = VisibleDeclaration {
            designator: designator.into(),
            data: vec![AnyDeclarationData {
                decl_pos: None,
                decl: AnyDeclaration::Library(library.name.clone()),
            }],
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

    /// Lookup a designator in the region
    /// inside: true if looking from inside the region
    ///         false if looking from the outside such as through a selected name
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
                self.extends
                    .as_ref()
                    .and_then(|parent| parent.lookup(designator, inside))
            })
            .or_else(|| {
                self.parent
                    .as_ref()
                    .and_then(|parent| parent.lookup(designator, inside))
            })
    }
}
