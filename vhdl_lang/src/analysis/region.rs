// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com
use super::visibility::*;
use crate::ast::*;
use crate::data::*;

use fnv::FnvHashMap;
use std::collections::hash_map::Entry;
use std::sync::Arc;

#[derive(Clone)]
pub enum NamedEntityKind {
    AliasOf(Box<NamedEntity>),
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

    pub fn is_alias(&self) -> bool {
        if let NamedEntityKind::AliasOf(..) = self {
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

    pub fn is_overloaded(&self) -> bool {
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

    /// Strip aliases and return reference to actual named entity
    pub fn as_actual(&self) -> &NamedEntity {
        match self.kind() {
            NamedEntityKind::AliasOf(ref ent) => ent.as_actual(),
            _ => self,
        }
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

    pub fn new_vec(designator: Designator, named_entities: Vec<NamedEntity>) -> VisibleDeclaration {
        VisibleDeclaration {
            designator,
            named_entities,
        }
    }

    pub fn into_non_overloaded(mut self) -> Result<NamedEntity, VisibleDeclaration> {
        if !self.first().is_overloaded() {
            let ent = self.named_entities.pop().unwrap();
            Ok(ent)
        } else {
            Err(self)
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

    pub fn named_entities(&self) -> impl Iterator<Item = &NamedEntity> {
        self.named_entities.iter()
    }

    fn push(&mut self, ent: NamedEntity) {
        self.named_entities.push(ent);
    }

    pub fn make_potentially_visible_in(
        &self,
        visible_pos: Option<&SrcPos>,
        region: &mut Region<'_>,
    ) {
        for ent in self.named_entities.iter() {
            region.make_potentially_visible(self.designator.clone(), visible_pos, ent.clone());
        }
    }
}

#[derive(Copy, Clone, PartialEq)]
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
    visibility: Visibility,
    decls: FnvHashMap<Designator, VisibleDeclaration>,
    protected_bodies: FnvHashMap<Symbol, SrcPos>,
    kind: RegionKind,
}

impl<'a> Region<'a> {
    pub fn default() -> Region<'static> {
        Region {
            parent: None,
            extends: None,
            visibility: Visibility::default(),
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
            visibility: self.visibility,
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
        debug_assert!(
            region.extends.is_none(),
            "Regions can only be extended in pairs"
        );
        Region {
            parent,
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
                    if let NamedEntityKind::DeferredConstant = decl.first_kind() {
                        decl.first().error(diagnostics, format!("Deferred constant '{}' lacks corresponding full constant declaration in package body", &decl.designator));
                    }
                }
            }
            RegionKind::PackageBody => {
                let extends = self
                    .extends
                    .as_ref()
                    .expect("Package body must extend package");
                for ext_decl in extends.decls.values() {
                    if let NamedEntityKind::DeferredConstant = ext_decl.first_kind() {
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
            if decl.first_kind().is_protected_type()
                && !self.has_protected_body(&decl.designator.expect_identifier())
            {
                decl.first().error(
                    diagnostics,
                    format!("Missing body for protected type '{}'", &decl.designator),
                );
            }
        }

        if let Some(ref extends) = self.extends {
            for ext_decl in extends.decls.values() {
                if ext_decl.first_kind().is_protected_type()
                    && !self.has_protected_body(&ext_decl.designator.expect_identifier())
                {
                    ext_decl.first().error(
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
        if self.kind != RegionKind::PackageBody
            && ent.kind.is_non_deferred_constant()
            && prev_decl.first_kind().is_deferred_constant()
        {
            ent.error(
                diagnostics,
                "Full declaration of deferred constant is only allowed in a package body",
            );
            false
        } else {
            true
        }
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
        visible_pos: Option<&SrcPos>,
        library_name: &Symbol,
    ) {
        let ent = NamedEntity {
            decl_pos: None,
            kind: NamedEntityKind::Library(library_name.clone()),
        };
        self.make_potentially_visible(designator.into(), visible_pos, ent);
    }

    pub fn add_implicit_declarations(
        &mut self,
        decl_pos: Option<&SrcPos>,
        ent: &NamedEntity,
        diagnostics: &mut dyn DiagnosticHandler,
    ) {
        if let NamedEntityKind::TypeDeclaration(ref implicit) = ent.as_actual().kind() {
            if let Some(implicit) = implicit {
                for visible in implicit.decls.values() {
                    for named_ent in visible.named_entities() {
                        self.add_implicit(
                            visible.designator.clone(),
                            decl_pos,
                            named_ent.kind().clone(),
                            diagnostics,
                        );
                    }
                }
            }
        }
    }

    pub fn make_potentially_visible(
        &mut self,
        designator: Designator,
        visible_pos: Option<&SrcPos>,
        ent: NamedEntity,
    ) {
        self.visibility
            .make_potentially_visible(designator, visible_pos, ent);
    }

    pub fn make_all_potentially_visible(
        &mut self,
        visible_pos: Option<&SrcPos>,
        region: &Arc<Region<'static>>,
    ) {
        self.visibility
            .make_all_potentially_visible(visible_pos, region);
    }

    /// Used when using context clauses
    pub fn add_context_visibility(&mut self, visible_pos: Option<&SrcPos>, region: &Region<'a>) {
        // @TODO ignores parent, used only for contexts currently where this is true
        self.visibility
            .add_context_visibility(visible_pos, &region.visibility);
    }

    /// Lookup a named entity declared in this region
    pub fn lookup_immediate(&self, designator: &Designator) -> Option<&VisibleDeclaration> {
        self.decls.get(designator)
    }

    /// Lookup a named entity declared in this region or extended region
    pub fn lookup_extended(&self, designator: &Designator) -> Option<&VisibleDeclaration> {
        self.lookup_immediate(designator).or_else(|| {
            // Regions can only be extended once
            self.extends
                .as_ref()
                .and_then(|region| region.decls.get(designator))
        })
    }

    /// Lookup a named entity declared in this region or an enclosing region
    fn lookup_enclosing(&self, designator: &Designator) -> Option<&VisibleDeclaration> {
        // We do not need to look in the enclosing region of the extended region
        // since extended region always has the same parent except for protected types
        // split into package / package body.
        // In that case the package / package body parent of the protected type / body
        // is the same extended region anyway
        self.lookup_extended(designator).or_else(|| {
            self.parent
                .as_ref()
                .and_then(|region| region.lookup_enclosing(designator))
        })
    }

    fn lookup_visiblity_into(&'a self, designator: &Designator, visible: &mut Visible<'a>) {
        self.visibility.lookup_into(designator, visible);
        if let Some(extends) = self.extends {
            extends.lookup_visiblity_into(designator, visible);
        }
        if let Some(parent) = self.parent {
            parent.lookup_visiblity_into(designator, visible);
        }
    }

    /// Lookup a named entity that was made potentially visible via a use clause
    fn lookup_visible(
        &self,
        pos: &SrcPos,
        designator: &Designator,
    ) -> Result<Option<VisibleDeclaration>, Diagnostic> {
        let mut visible = Visible::default();
        self.lookup_visiblity_into(designator, &mut visible);
        visible.into_unambiguous(pos, designator)
    }

    /// Lookup where this region is the prefix of a selected name
    /// Thus any visibility inside the region is irrelevant
    pub fn lookup_selected(&self, designator: &Designator) -> Option<&VisibleDeclaration> {
        self.lookup_immediate(designator)
    }

    /// Lookup a designator from within the region itself
    /// Thus all parent regions and visibility is relevant
    pub fn lookup_within(
        &self,
        pos: &SrcPos,
        designator: &Designator,
    ) -> Result<VisibleDeclaration, Diagnostic> {
        let result = if let Some(visible) = self.lookup_enclosing(designator) {
            Some(visible.clone())
        } else {
            self.lookup_visible(pos, designator)?
        };

        match result {
            Some(visible) => Ok(visible),
            None => Err(Diagnostic::error(
                pos,
                format!("No declaration of '{}'", designator),
            )),
        }
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
        self.set_unique_reference(visible.first());
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
