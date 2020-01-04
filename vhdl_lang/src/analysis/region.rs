// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com
use crate::ast::*;
use crate::data::*;

use fnv::{FnvHashMap, FnvHashSet};
use std::collections::hash_map::Entry;
use std::sync::Arc;

#[derive(Clone)]
pub enum NamedEntityKind {
    AliasOf(Box<NamedEntityKind>),
    Other,
    Overloaded,
    // An optional region with implicit declarations
    TypeDeclaration(Option<Arc<Region<'static>>>),
    IncompleteType,
    Constant,
    DeferredConstant,
    // The region of the protected type which needs to be extendend by the body
    ProtectedType(Arc<Region<'static>>),
    Library(Symbol),
    Entity(UnitId, Arc<Region<'static>>),
    Configuration(UnitId, Arc<Region<'static>>),
    Package(UnitId, Arc<Region<'static>>),
    UninstPackage(UnitId, Arc<Region<'static>>),
    PackageInstance(UnitId, Arc<Region<'static>>),
    Context(UnitId, Arc<Region<'static>>),
    LocalPackageInstance(Symbol, Arc<Region<'static>>),
}

impl NamedEntityKind {
    pub fn from_object_declaration(decl: &ObjectDeclaration) -> NamedEntityKind {
        match decl {
            ObjectDeclaration {
                class: ObjectClass::Constant,
                ref expression,
                ..
            } => {
                if expression.is_none() {
                    NamedEntityKind::DeferredConstant
                } else {
                    NamedEntityKind::Constant
                }
            }
            _ => NamedEntityKind::Other,
        }
    }

    fn is_deferred_constant(&self) -> bool {
        if let NamedEntityKind::DeferredConstant = self {
            true
        } else {
            false
        }
    }

    fn is_non_deferred_constant(&self) -> bool {
        if let NamedEntityKind::Constant = self {
            true
        } else {
            false
        }
    }

    fn is_protected_type(&self) -> bool {
        if let NamedEntityKind::ProtectedType(..) = self {
            true
        } else {
            false
        }
    }
}

#[derive(Clone)]
pub struct NamedEntity {
    /// The location where the declaration was made
    /// Builtin and implicit declaration will not have a source position
    decl_pos: Option<SrcPos>,
    kind: NamedEntityKind,
}

impl NamedEntity {
    pub fn new(kind: NamedEntityKind, decl_pos: Option<&SrcPos>) -> NamedEntity {
        NamedEntity {
            kind,
            decl_pos: decl_pos.cloned(),
        }
    }

    pub fn decl_pos(&self) -> Option<&SrcPos> {
        self.decl_pos.as_ref()
    }

    pub fn kind(&self) -> &NamedEntityKind {
        &self.kind
    }

    fn error(&self, diagnostics: &mut dyn DiagnosticHandler, message: impl Into<String>) {
        if let Some(ref pos) = self.decl_pos {
            diagnostics.push(Diagnostic::error(pos, message));
        }
    }

    fn is_overloaded(&self) -> bool {
        if let NamedEntityKind::Overloaded = self.kind {
            true
        } else {
            false
        }
    }

    /// Return a duplicate declaration of the previously declared named entity
    fn is_duplicate_of<'a>(&self, prev: &'a Self) -> bool {
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
}

#[derive(Clone)]
pub struct VisibleDeclaration {
    designator: Designator,
    named_entities: Vec<NamedEntity>,
}

impl VisibleDeclaration {
    pub fn new(designator: Designator, named_entity: NamedEntity) -> VisibleDeclaration {
        VisibleDeclaration {
            designator,
            named_entities: vec![named_entity],
        }
    }

    /// Return single named entity if unique name is visible
    pub fn as_unique(&self) -> Option<&NamedEntity> {
        if self.named_entities.len() == 1 {
            self.named_entities.first()
        } else {
            None
        }
    }

    pub fn to_unique(mut self) -> Option<NamedEntity> {
        if self.named_entities.len() == 1 {
            self.named_entities.pop()
        } else {
            None
        }
    }

    fn first(&self) -> &NamedEntity {
        self.named_entities
            .first()
            .expect("Declaration always contains one entry")
    }

    pub fn first_kind(&self) -> &NamedEntityKind {
        &self.first().kind
    }

    pub fn first_pos(&self) -> Option<&SrcPos> {
        self.first().decl_pos.as_ref()
    }

    fn named_entities(&self) -> impl Iterator<Item = &NamedEntity> {
        self.named_entities.iter()
    }

    fn is_overloaded(&self) -> bool {
        self.first().is_overloaded()
    }

    fn push(&mut self, ent: NamedEntity) {
        self.named_entities.push(ent);
    }

    pub fn make_potentially_visible_in(&self, region: &mut Region<'_>) {
        for ent in self.named_entities.iter() {
            region.make_potentially_visible(self.designator.clone(), ent.clone());
        }
    }
}
#[derive(Copy, Clone, PartialEq)]
#[cfg_attr(test, derive(Debug))]
enum RegionKind {
    PackageDeclaration,
    PackageBody,
    Other,
}

impl Default for RegionKind {
    fn default() -> RegionKind {
        RegionKind::Other
    }
}

#[derive(Clone, Default)]
pub struct Region<'a> {
    parent: Option<&'a Region<'a>>,
    extends: Option<&'a Region<'a>>,
    hidden: FnvHashSet<Designator>,
    visible: FnvHashMap<Designator, VisibleDeclaration>,
    decls: FnvHashMap<Designator, VisibleDeclaration>,
    protected_bodies: FnvHashMap<Symbol, SrcPos>,
    kind: RegionKind,
}

impl<'a> Region<'a> {
    pub fn default() -> Region<'static> {
        Region {
            parent: None,
            extends: None,
            hidden: FnvHashSet::default(),
            visible: FnvHashMap::default(),
            decls: FnvHashMap::default(),
            protected_bodies: FnvHashMap::default(),
            kind: RegionKind::Other,
        }
    }

    pub fn nested(&'a self) -> Region<'a> {
        Region {
            parent: Some(self),
            extends: None,
            ..Region::default()
        }
    }

    pub fn without_parent(self) -> Region<'static> {
        Region {
            parent: None,
            extends: None,
            hidden: self.hidden,
            visible: self.visible,
            decls: self.decls,
            protected_bodies: self.protected_bodies,
            kind: self.kind,
        }
    }

    pub fn in_package_declaration(mut self) -> Region<'a> {
        self.kind = RegionKind::PackageDeclaration;
        self
    }

    pub fn extend(region: &'a Region<'a>, parent: Option<&'a Region<'a>>) -> Region<'a> {
        let kind = match region.kind {
            RegionKind::PackageDeclaration => RegionKind::PackageBody,
            _ => RegionKind::Other,
        };

        Region {
            parent: parent,
            extends: Some(region),
            kind,
            ..Region::default()
        }
    }

    fn check_deferred_constant_pairs(&self, diagnostics: &mut dyn DiagnosticHandler) {
        match self.kind {
            // Package without body may not have deferred constants
            RegionKind::PackageDeclaration => {
                for decl in self.decls.values() {
                    match decl.first_kind() {
                        NamedEntityKind::DeferredConstant => {
                            decl.first().error(diagnostics, format!("Deferred constant '{}' lacks corresponding full constant declaration in package body", &decl.designator));
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
                    match ext_decl.first_kind() {
                        NamedEntityKind::DeferredConstant => {
                            // Deferred constants may only be located in a package
                            // And only matched with a constant in the body
                            let mut found = false;
                            let decl = self.decls.get(&ext_decl.designator);

                            if let Some(decl) = decl {
                                if let NamedEntityKind::Constant = decl.first_kind() {
                                    found = true;
                                }
                            }

                            if !found {
                                ext_decl.first().error(diagnostics, format!("Deferred constant '{}' lacks corresponding full constant declaration in package body", &ext_decl.designator));
                            }
                        }
                        _ => {}
                    }
                }
            }
            RegionKind::Other => {}
        }
    }

    fn get_protected_body(&self, name: &Symbol) -> Option<&SrcPos> {
        self.protected_bodies.get(name).or_else(|| {
            self.extends
                .and_then(|extends| extends.get_protected_body(name))
        })
    }

    fn has_protected_body(&self, name: &Symbol) -> bool {
        self.get_protected_body(name).is_some()
    }

    fn check_protected_types_have_body(&self, diagnostics: &mut dyn DiagnosticHandler) {
        for decl in self.decls.values() {
            if decl.first_kind().is_protected_type() {
                if !self.has_protected_body(&decl.designator.expect_identifier()) {
                    decl.first().error(
                        diagnostics,
                        format!("Missing body for protected type '{}'", &decl.designator),
                    );
                }
            }
        }

        if let Some(ref extends) = self.extends {
            for ext_decl in extends.decls.values() {
                if ext_decl.first_kind().is_protected_type() {
                    if !self.has_protected_body(&ext_decl.designator.expect_identifier()) {
                        ext_decl.first().error(
                            diagnostics,
                            format!("Missing body for protected type '{}'", &ext_decl.designator),
                        );
                    }
                }
            }
        }
    }

    #[must_use]
    fn check_deferred_constant_only_in_package(
        &self,
        ent: &NamedEntity,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> bool {
        if self.kind != RegionKind::PackageDeclaration && ent.kind.is_deferred_constant() {
            ent.error(
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
        ent: &NamedEntity,
        prev_decl: &VisibleDeclaration,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> bool {
        if self.kind != RegionKind::PackageBody && ent.kind.is_non_deferred_constant() {
            if prev_decl.first_kind().is_deferred_constant() {
                ent.error(
                    diagnostics,
                    "Full declaration of deferred constant is only allowed in a package body",
                );
                return false;
            }
        }
        true
    }

    pub fn close(&mut self, diagnostics: &mut dyn DiagnosticHandler) {
        self.check_deferred_constant_pairs(diagnostics);
        self.check_protected_types_have_body(diagnostics);
    }

    /// Check duplicate declarations
    /// Allow deferred constants, incomplete types and protected type bodies
    /// Returns true if the declaration does not duplicates an existing declaration
    #[must_use]
    fn check_duplicate(
        ent: &NamedEntity,
        prev_decl: &VisibleDeclaration,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> bool {
        for prev_ent in prev_decl.named_entities() {
            if ent.is_duplicate_of(&prev_ent) {
                if let Some(ref pos) = ent.decl_pos {
                    diagnostics.push(duplicate_error(
                        &prev_decl.designator,
                        pos,
                        prev_ent.decl_pos.as_ref(),
                    ));
                }
                return false;
            }
        }

        true
    }

    /// true if the declaration can be added
    fn check_add(
        // The named entity to add
        ent: &NamedEntity,
        // Previous declaration in the same region
        prev_decl: Option<&VisibleDeclaration>,
        // Previous declaration in the region extended by this region
        ext_decl: Option<&VisibleDeclaration>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> bool {
        let mut check_ok = true;

        if let Some(prev_decl) = prev_decl {
            if !Self::check_duplicate(&ent, &prev_decl, diagnostics) {
                check_ok = false;
            }
        }

        if let Some(ext_decl) = ext_decl {
            if !Self::check_duplicate(&ent, &ext_decl, diagnostics) {
                check_ok = false;
            }
        }

        check_ok
    }

    pub fn add_protected_body(&mut self, ident: Ident, diagnostics: &mut dyn DiagnosticHandler) {
        if let Some(prev_pos) = self.get_protected_body(&ident.item) {
            diagnostics.push(duplicate_error(&ident.item, &ident.pos, Some(prev_pos)));
        } else {
            self.protected_bodies.insert(ident.item, ident.pos);
        }
    }

    fn add_named_entity(
        &mut self,
        designator: Designator,
        ent: NamedEntity,
        diagnostics: &mut dyn DiagnosticHandler,
    ) {
        let ext_decl = self
            .extends
            .as_ref()
            .and_then(|extends| extends.decls.get(&designator));

        if !self.check_deferred_constant_only_in_package(&ent, diagnostics) {
            return;
        }

        if let Some(ext_decl) = ext_decl {
            if !self.check_full_constand_of_deferred_only_in_body(&ent, ext_decl, diagnostics) {
                return;
            }
        }

        // @TODO merge with .entry below
        if let Some(prev_decl) = self.decls.get(&designator) {
            if !self.check_full_constand_of_deferred_only_in_body(&ent, prev_decl, diagnostics) {
                return;
            }
        }

        match self.decls.entry(designator.clone()) {
            Entry::Occupied(ref mut entry) => {
                let prev_decl = entry.get_mut();

                if Self::check_add(&ent, Some(&prev_decl), ext_decl, diagnostics) {
                    prev_decl.push(ent);
                }
            }
            Entry::Vacant(entry) => {
                if Self::check_add(&ent, None, ext_decl, diagnostics) {
                    entry.insert(VisibleDeclaration::new(designator, ent));
                }
            }
        }
    }

    pub fn add(
        &mut self,
        designator: impl Into<WithPos<Designator>>,
        kind: NamedEntityKind,
        diagnostics: &mut dyn DiagnosticHandler,
    ) {
        let designator = designator.into();
        self.add_named_entity(
            designator.item,
            NamedEntity::new(kind, Some(&designator.pos)),
            diagnostics,
        );
    }

    pub fn overwrite(&mut self, designator: impl Into<WithPos<Designator>>, kind: NamedEntityKind) {
        let designator = designator.into();
        let decl = VisibleDeclaration::new(
            designator.item,
            NamedEntity::new(kind, Some(&designator.pos)),
        );
        self.decls.insert(decl.designator.clone(), decl);
    }

    pub fn add_implicit(
        &mut self,
        designator: impl Into<Designator>,
        decl_pos: Option<&SrcPos>,
        kind: NamedEntityKind,
        diagnostics: &mut dyn DiagnosticHandler,
    ) {
        self.add_named_entity(
            designator.into(),
            NamedEntity {
                decl_pos: decl_pos.cloned(),
                kind,
            },
            diagnostics,
        );
    }

    pub fn make_library_visible(
        &mut self,
        designator: impl Into<Designator>,
        library_name: &Symbol,
        decl_pos: Option<SrcPos>,
    ) {
        let ent = NamedEntity {
            decl_pos: decl_pos.clone(),
            kind: NamedEntityKind::Library(library_name.clone()),
        };
        self.make_potentially_visible(designator.into(), ent);
    }

    /// Add implicit declarations when using declaration
    /// For example all enum literals are made implicititly visible when using an enum type
    fn add_implicit_declarations(&mut self, kind: &NamedEntityKind) {
        match kind {
            NamedEntityKind::TypeDeclaration(ref implicit) => {
                // Add implicitic declarations when using type
                if let Some(implicit) = implicit {
                    self.make_all_potentially_visible(&implicit);
                }
            }
            NamedEntityKind::AliasOf(ref kind) => {
                self.add_implicit_declarations(kind);
            }
            _ => {}
        }
    }

    fn has_duplicate(&self, designator: &Designator, ent: &NamedEntity) -> bool {
        if let Some(prev) = self.visible.get(&designator) {
            if !ent.is_overloaded() && !prev.is_overloaded() {
                // @TODO check that they actually correspond to the same object
                // The decl_pos serves as a good proxy for this except for libraries
                if ent.decl_pos() != prev.first_pos() {
                    if let NamedEntityKind::Library(..) = ent.kind() {
                    } else if let NamedEntityKind::Library(..) = prev.first_kind() {
                        // Until we have unique id:s we disable hidden check for alias
                    } else if let NamedEntityKind::AliasOf(..) = ent.kind() {
                    } else if let NamedEntityKind::AliasOf(..) = prev.first_kind() {
                    } else {
                        return true;
                    }
                }
            }
        }

        if let Some(parent) = self.parent {
            parent.has_duplicate(designator, ent)
        } else {
            false
        }
    }

    pub fn make_potentially_visible(&mut self, designator: Designator, ent: NamedEntity) {
        if self.has_duplicate(&designator, &ent) {
            // @TODO add error message when hiding
            self.hidden.insert(designator);
        } else {
            self.add_implicit_declarations(&ent.kind);
            match self.visible.entry(designator.clone()) {
                Entry::Vacant(entry) => {
                    entry.insert(VisibleDeclaration::new(designator, ent));
                }
                Entry::Occupied(mut entry) => {
                    entry.get_mut().push(ent);
                }
            }
        }
    }

    pub fn make_all_potentially_visible(&mut self, region: &Region<'a>) {
        for visible in region.decls.values() {
            for ent in visible.named_entities() {
                self.make_potentially_visible(visible.designator.clone(), ent.clone());
            }
        }
    }

    /// Used when using context clauses
    pub fn copy_visibility_from(&mut self, region: &Region<'a>) {
        for visible in region.visible.values() {
            for ent in visible.named_entities() {
                self.make_potentially_visible(visible.designator.clone(), ent.clone());
            }
        }
    }

    /// Helper function lookup a visible declaration within the region
    fn lookup(&self, designator: &Designator, is_selected: bool) -> Option<&VisibleDeclaration> {
        self.decls
            .get(designator)
            .or_else(|| {
                self.extends
                    .as_ref()
                    .and_then(|region| region.lookup(designator, is_selected))
            })
            .or_else(|| {
                if is_selected || self.hidden.contains(designator) {
                    None
                } else {
                    self.visible.get(designator)
                }
            })
            .or_else(|| {
                if is_selected || self.hidden.contains(designator) {
                    None
                } else {
                    self.parent
                        .as_ref()
                        .and_then(|region| region.lookup_within(designator))
                }
            })
    }

    /// Lookup where this region is the prefix of a selected name
    /// Thus any visibility inside the region is irrelevant
    pub fn lookup_selected(&self, designator: &Designator) -> Option<&VisibleDeclaration> {
        self.lookup(designator, true)
    }

    /// Lookup a designator from within the region itself
    /// Thus all parent regions and visibility is relevant
    pub fn lookup_within(&self, designator: &Designator) -> Option<&VisibleDeclaration> {
        self.lookup(designator, false)
    }
}

pub trait SetReference {
    fn set_unique_reference(&mut self, ent: &NamedEntity) {
        // @TODO handle built-ins without position
        // @TODO handle mutliple overloaded declarations
        if !ent.is_overloaded() {
            // We do not set references to overloaded names to avoid
            // incorrect behavior which will appear as low quality
            self.set_reference_pos(ent.decl_pos.as_ref());
        } else {
            self.clear_reference();
        }
    }

    fn set_reference(&mut self, visible: &VisibleDeclaration) {
        if let Some(ent) = visible.as_unique() {
            // We do not set references to non-unqiue names
            //  incorrect behavior which will appear as low quality
            self.set_unique_reference(ent);
        } else {
            self.clear_reference();
        }
    }

    fn clear_reference(&mut self) {
        self.set_reference_pos(None);
    }

    fn set_reference_pos(&mut self, pos: Option<&SrcPos>);
}

impl<T> SetReference for WithRef<T> {
    fn set_reference_pos(&mut self, pos: Option<&SrcPos>) {
        self.reference.set_reference_pos(pos);
    }
}

impl<T: SetReference> SetReference for WithPos<T> {
    fn set_reference_pos(&mut self, pos: Option<&SrcPos>) {
        self.item.set_reference_pos(pos);
    }
}

impl SetReference for Reference {
    fn set_reference_pos(&mut self, pos: Option<&SrcPos>) {
        *self = pos.cloned();
    }
}

pub(super) fn duplicate_error(
    name: &impl std::fmt::Display,
    pos: &SrcPos,
    prev_pos: Option<&SrcPos>,
) -> Diagnostic {
    let mut diagnostic = Diagnostic::error(pos, format!("Duplicate declaration of '{}'", name));

    if let Some(prev_pos) = prev_pos {
        diagnostic.add_related(prev_pos, "Previously defined here");
    }

    diagnostic
}
