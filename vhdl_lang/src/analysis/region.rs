use super::formal_region::FormalRegion;
use super::formal_region::GpkgInterfaceEnt;
use super::formal_region::GpkgRegion;
use super::formal_region::InterfaceEnt;
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com
use super::named_entity::*;
use super::{named_entity, visibility::*};
use crate::ast::*;
use crate::data::*;

use fnv::FnvHashMap;
use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq, Eq)]
/// A non-emtpy collection of overloaded entites
pub struct OverloadedName<'a> {
    entities: FnvHashMap<SignatureKey<'a>, OverloadedEnt<'a>>,
}

impl<'a> OverloadedName<'a> {
    pub fn new(entities: Vec<OverloadedEnt>) -> OverloadedName {
        debug_assert!(!entities.is_empty());
        let mut map = FnvHashMap::default();
        for ent in entities.into_iter() {
            map.insert(ent.signature().key(), ent);
        }
        OverloadedName { entities: map }
    }

    pub fn single(ent: OverloadedEnt) -> OverloadedName {
        let mut map = FnvHashMap::default();
        map.insert(ent.signature().key(), ent);
        OverloadedName { entities: map }
    }

    pub fn first(&self) -> OverloadedEnt<'a> {
        let first_key = self.entities.keys().next().unwrap();
        *self.entities.get(first_key).unwrap()
    }

    pub fn designator(&self) -> &Designator {
        self.first().designator()
    }

    pub fn len(&self) -> usize {
        self.entities.len()
    }

    pub fn entities(&self) -> impl Iterator<Item = OverloadedEnt<'a>> + '_ {
        self.entities.values().cloned()
    }

    pub fn sorted_entities(&self) -> Vec<OverloadedEnt<'a>> {
        let mut res: Vec<_> = self.entities.values().cloned().collect();
        res.sort_by(|x, y| x.decl_pos().cmp(&y.decl_pos()));
        res
    }

    pub fn signatures(&self) -> impl Iterator<Item = &named_entity::Signature<'a>> + '_ {
        self.entities().map(|ent| ent.signature())
    }

    pub fn get(&self, key: &SignatureKey) -> Option<OverloadedEnt<'a>> {
        self.entities.get(key).cloned()
    }

    #[allow(clippy::if_same_then_else)]
    fn insert(&mut self, ent: OverloadedEnt<'a>) -> Result<(), Diagnostic> {
        match self.entities.entry(ent.signature().key()) {
            Entry::Occupied(mut entry) => {
                let old_ent = entry.get();

                if (old_ent.is_implicit() && ent.is_explicit()) || (ent.is_declared_by(old_ent)) {
                    entry.insert(ent);
                    return Ok(());
                } else if old_ent.is_implicit()
                    && ent.is_implicit()
                    && (old_ent.as_actual().id() == ent.as_actual().id())
                {
                    return Ok(());
                } else if old_ent.is_explicit() && ent.is_implicit() {
                    return Ok(());
                }

                // @TODO only libraries have decl_pos=None
                let pos = ent.decl_pos().unwrap();

                let mut diagnostic = Diagnostic::error(
                    pos,
                    format!(
                        "Duplicate declaration of '{}' with signature {}",
                        ent.designator(),
                        ent.signature().describe()
                    ),
                );
                if let Some(old_pos) = old_ent.decl_pos() {
                    diagnostic.add_related(old_pos, "Previously defined here");
                }

                Err(diagnostic)
            }
            Entry::Vacant(entry) => {
                entry.insert(ent);
                Ok(())
            }
        }
    }

    // Merge overloaded names where self is overloaded names from an
    // immediate/enclosing region and visible are overloaded names that have been made visible
    fn with_visible(mut self, visible: Self) -> Self {
        for (signature, visible_entity) in visible.entities.into_iter() {
            // Ignore visible entites that conflict with those in the enclosing region
            if let Entry::Vacant(entry) = self.entities.entry(signature) {
                entry.insert(visible_entity);
            }
        }
        self
    }
}

#[derive(Clone, Debug)]
/// Identically named entities
pub enum NamedEntities<'a> {
    Single(EntRef<'a>),
    Overloaded(OverloadedName<'a>),
}

impl<'a> NamedEntities<'a> {
    pub fn new(ent: EntRef<'a>) -> NamedEntities<'a> {
        match OverloadedEnt::from_any(ent) {
            Some(ent) => Self::Overloaded(OverloadedName::new(vec![ent])),
            None => Self::Single(ent),
        }
    }

    pub fn new_overloaded(named_entities: Vec<OverloadedEnt<'a>>) -> NamedEntities<'a> {
        Self::Overloaded(OverloadedName::new(named_entities))
    }

    pub fn into_non_overloaded(self) -> Result<EntRef<'a>, OverloadedName<'a>> {
        match self {
            Self::Single(ent) => Ok(ent),
            Self::Overloaded(ent_vec) => Err(ent_vec),
        }
    }

    pub fn expect_non_overloaded(
        self,
        pos: &SrcPos,
        message: impl FnOnce() -> String,
    ) -> Result<EntRef<'a>, Diagnostic> {
        match self {
            Self::Single(ent) => Ok(ent),
            Self::Overloaded(overloaded) => {
                let mut error = Diagnostic::error(pos, message());
                for ent in overloaded.entities() {
                    if let Some(decl_pos) = ent.decl_pos() {
                        error.add_related(decl_pos, "Defined here");
                    }
                }
                Err(error)
            }
        }
    }

    pub fn as_non_overloaded(&self) -> Option<EntRef<'a>> {
        match self {
            Self::Single(ent) => Some(ent),
            Self::Overloaded(..) => None,
        }
    }

    pub fn designator(&self) -> &Designator {
        self.first().designator()
    }

    pub fn first(&self) -> EntRef<'a> {
        match self {
            Self::Single(ent) => ent,
            Self::Overloaded(overloaded) => overloaded.first().into(),
        }
    }

    pub fn first_kind(&self) -> &'a AnyEntKind<'a> {
        self.first().kind()
    }

    pub fn make_potentially_visible_in(&self, visible_pos: Option<&SrcPos>, scope: &Scope<'a>) {
        match self {
            Self::Single(ent) => {
                scope.make_potentially_visible(visible_pos, ent);
            }
            Self::Overloaded(overloaded) => {
                for ent in overloaded.entities() {
                    scope.make_potentially_visible(visible_pos, ent.into());
                }
            }
        }
    }
}

#[derive(Copy, Clone, PartialEq, Default)]
pub(crate) enum RegionKind {
    PackageDeclaration,
    PackageBody,
    #[default]
    Other,
}

#[derive(Default, Clone)]
pub struct Scope<'a>(Rc<RefCell<ScopeInner<'a>>>);

#[derive(Default)]
struct ScopeInner<'a> {
    parent: Option<Scope<'a>>,
    region: Region<'a>,
    cache: FnvHashMap<Designator, NamedEntities<'a>>,
    anon_idx: usize,
}

impl<'a> ScopeInner<'a> {
    pub fn into_region(self) -> Region<'a> {
        self.region
    }

    pub fn into_visibility(self) -> Visibility<'a> {
        self.region.visibility
    }

    pub fn close(&self, diagnostics: &mut dyn DiagnosticHandler) {
        self.region.close(diagnostics)
    }

    pub fn add(&mut self, ent: EntRef<'a>, diagnostics: &mut dyn DiagnosticHandler) {
        self.cache.remove(&ent.designator);
        self.region.add(ent, diagnostics)
    }

    fn make_potentially_visible(
        &mut self,
        visible_pos: Option<&SrcPos>,
        designator: Designator,
        ent: EntRef<'a>,
    ) {
        self.cache.remove(&ent.designator);
        self.region
            .visibility
            .make_potentially_visible_with_name(visible_pos, designator, ent);
    }

    pub fn make_all_potentially_visible(
        &mut self,
        visible_pos: Option<&SrcPos>,
        region: &'a Region<'a>,
    ) {
        self.cache.clear();
        self.region
            .visibility
            .make_all_potentially_visible(visible_pos, region);
    }

    /// Used when using context clauses
    pub fn add_context_visibility(&mut self, visible_pos: Option<&SrcPos>, region: &Region<'a>) {
        self.cache.clear();
        // ignores parent but used only for contexts where this is true
        self.region
            .visibility
            .add_context_visibility(visible_pos, &region.visibility);
    }

    pub fn lookup_immediate(&self, designator: &Designator) -> Option<&NamedEntities<'a>> {
        self.region.lookup_immediate(designator)
    }

    /// Lookup a named entity declared in this region or an enclosing region
    fn lookup_enclosing(&self, designator: &Designator) -> Option<NamedEntities<'a>> {
        // We do not need to look in the enclosing region of the extended region
        // since extended region always has the same parent except for protected types
        // split into package / package body.
        // In that case the package / package body parent of the protected type / body
        // is the same extended region anyway

        match self.lookup_immediate(designator).cloned() {
            // A non-overloaded name is found in the immediate region
            // no need to look further up
            Some(NamedEntities::Single(single)) => Some(NamedEntities::Single(single)),

            // The name is overloaded we must also check enclosing regions
            Some(NamedEntities::Overloaded(immediate)) => {
                if let Some(NamedEntities::Overloaded(enclosing)) = self
                    .parent
                    .as_ref()
                    .and_then(|region| region.0.borrow().lookup_enclosing(designator))
                {
                    Some(NamedEntities::Overloaded(immediate.with_visible(enclosing)))
                } else {
                    Some(NamedEntities::Overloaded(immediate))
                }
            }
            None => self
                .parent
                .as_ref()
                .and_then(|region| region.0.borrow().lookup_enclosing(designator)),
        }
    }

    fn lookup_visiblity_into(&self, designator: &Designator, visible: &mut Visible<'a>) {
        self.region.visibility.lookup_into(designator, visible);
        if let Some(ref parent) = self.parent {
            parent.0.borrow().lookup_visiblity_into(designator, visible);
        }
    }

    /// Lookup a named entity that was made potentially visible via a use clause
    fn lookup_visible(
        &self,
        pos: &SrcPos,
        designator: &Designator,
    ) -> Result<Option<NamedEntities<'a>>, Diagnostic> {
        let mut visible = Visible::default();
        self.lookup_visiblity_into(designator, &mut visible);
        visible.into_unambiguous(pos, designator)
    }

    /// Lookup a designator from within the region itself
    /// Thus all parent regions and visibility is relevant
    fn lookup_uncached(
        &self,
        pos: &SrcPos,
        designator: &Designator,
    ) -> Result<NamedEntities<'a>, Diagnostic> {
        let result = if let Some(enclosing) = self.lookup_enclosing(designator) {
            match enclosing {
                // non overloaded in enclosing region ignores any visible overloaded names
                NamedEntities::Single(..) => Some(enclosing),
                // In case of overloaded local, non-conflicting visible names are still relevant
                NamedEntities::Overloaded(enclosing_overloaded) => {
                    if let Ok(Some(NamedEntities::Overloaded(overloaded))) =
                        self.lookup_visible(pos, designator)
                    {
                        Some(NamedEntities::Overloaded(
                            enclosing_overloaded.with_visible(overloaded),
                        ))
                    } else {
                        Some(NamedEntities::Overloaded(enclosing_overloaded))
                    }
                }
            }
        } else {
            self.lookup_visible(pos, designator)?
        };

        match result {
            Some(visible) => Ok(visible),
            None => Err(Diagnostic::error(
                pos,
                match designator {
                    Designator::Identifier(ident) => {
                        format!("No declaration of '{ident}'")
                    }
                    Designator::OperatorSymbol(operator) => {
                        format!("No declaration of operator '{operator}'")
                    }
                    Designator::Character(chr) => {
                        format!("No declaration of '{chr}'")
                    }
                    Designator::Anonymous(_) => "No declaration of <anonymous>".to_owned(),
                },
            )),
        }
    }

    fn lookup(
        &mut self,
        pos: &SrcPos,
        designator: &Designator,
    ) -> Result<NamedEntities<'a>, Diagnostic> {
        if let Some(res) = self.cache.get(designator) {
            return Ok(res.clone());
        }

        let ents = self.lookup_uncached(pos, designator)?;
        if let Entry::Vacant(vacant) = self.cache.entry(designator.clone()) {
            Ok(vacant.insert(ents).clone())
        } else {
            unreachable!("Cache miss cannot be followed by occupied entry")
        }
    }
}

impl<'a> Scope<'a> {
    pub fn new(region: Region<'a>) -> Scope<'a> {
        Self(Rc::new(RefCell::new(ScopeInner {
            parent: None,
            region,
            cache: Default::default(),
            anon_idx: 0,
        })))
    }

    pub fn nested(&self) -> Scope<'a> {
        Self(Rc::new(RefCell::new(ScopeInner {
            region: Region::default(),
            parent: Some(self.clone()),
            cache: self.0.borrow().cache.clone(),
            anon_idx: 0,
        })))
    }

    pub fn with_parent(self, scope: &Scope<'a>) -> Scope<'a> {
        Self(Rc::new(RefCell::new(ScopeInner {
            parent: Some(scope.clone()),
            region: self.into_inner().region,
            cache: Default::default(),
            anon_idx: 0,
        })))
    }

    pub fn extend(region: &Region<'a>, parent: Option<&Scope<'a>>) -> Scope<'a> {
        let kind = match region.kind {
            RegionKind::PackageDeclaration => RegionKind::PackageBody,
            _ => RegionKind::Other,
        };

        let extended_region = Region {
            visibility: region.visibility.clone(),
            entities: region.entities.clone(),
            kind,
        };

        if let Some(parent) = parent {
            Scope::new(extended_region).with_parent(parent)
        } else {
            Scope::new(extended_region)
        }
    }

    pub fn in_package_declaration(self) -> Scope<'a> {
        let inner = self.into_inner();

        Self(Rc::new(RefCell::new(ScopeInner {
            parent: inner.parent,
            region: inner.region.in_package_declaration(),
            cache: inner.cache,
            anon_idx: inner.anon_idx,
        })))
    }

    pub fn add(&self, ent: EntRef<'a>, diagnostics: &mut dyn DiagnosticHandler) {
        self.0.as_ref().borrow_mut().add(ent, diagnostics);
    }

    pub fn make_potentially_visible(&self, visible_pos: Option<&SrcPos>, ent: &'a AnyEnt) {
        self.0.as_ref().borrow_mut().make_potentially_visible(
            visible_pos,
            ent.designator().clone(),
            ent,
        );
    }

    pub fn make_potentially_visible_with_name(
        &self,
        visible_pos: Option<&SrcPos>,
        designator: Designator,
        ent: EntRef<'a>,
    ) {
        self.0
            .as_ref()
            .borrow_mut()
            .make_potentially_visible(visible_pos, designator, ent);
    }

    pub fn make_all_potentially_visible(
        &self,
        visible_pos: Option<&SrcPos>,
        region: &'a Region<'a>,
    ) {
        self.0
            .as_ref()
            .borrow_mut()
            .make_all_potentially_visible(visible_pos, region);
    }

    pub fn close(&self, diagnostics: &mut dyn DiagnosticHandler) {
        self.0.as_ref().borrow().close(diagnostics)
    }

    fn into_inner(self) -> ScopeInner<'a> {
        if let Ok(cell) = Rc::try_unwrap(self.0) {
            cell.into_inner()
        } else {
            panic!("Expect no child regions");
        }
    }

    pub fn into_region(self) -> Region<'a> {
        self.into_inner().into_region()
    }

    pub fn into_visibility(self) -> Visibility<'a> {
        self.into_inner().into_visibility()
    }

    pub fn lookup_immediate(&self, designator: &Designator) -> Option<NamedEntities<'a>> {
        let inner = self.0.as_ref().borrow();
        let names = inner.lookup_immediate(designator)?;

        Some(names.clone())
    }

    pub fn lookup(
        &self,
        pos: &SrcPos,
        designator: &Designator,
    ) -> Result<NamedEntities<'a>, Diagnostic> {
        self.0.as_ref().borrow_mut().lookup(pos, designator)
    }

    /// Used when using context clauses
    pub fn add_context_visibility(&self, visible_pos: Option<&SrcPos>, region: &Region<'a>) {
        self.0
            .as_ref()
            .borrow_mut()
            .add_context_visibility(visible_pos, region)
    }

    pub fn next_anonymous(&self) -> usize {
        let mut inner = self.0.borrow_mut();
        let idx = inner.anon_idx;
        inner.anon_idx += 1;
        idx
    }
}

#[derive(Clone)]
pub struct Region<'a> {
    visibility: Visibility<'a>,
    pub(crate) entities: FnvHashMap<Designator, NamedEntities<'a>>,
    pub(crate) kind: RegionKind,
}

impl<'a> Default for Region<'a> {
    fn default() -> Region<'a> {
        Region {
            visibility: Visibility::default(),
            entities: FnvHashMap::default(),
            kind: RegionKind::Other,
        }
    }
}

impl<'a> Region<'a> {
    pub fn with_visibility(visibility: Visibility<'a>) -> Self {
        Self {
            visibility,
            ..Default::default()
        }
    }

    fn in_package_declaration(mut self) -> Region<'a> {
        self.kind = RegionKind::PackageDeclaration;
        self
    }

    pub fn to_entity_formal(&self) -> (FormalRegion, FormalRegion) {
        // @TODO separate generics and ports
        let mut generics = Vec::with_capacity(self.entities.len());
        let mut ports = Vec::with_capacity(self.entities.len());

        for ent in self.entities.values() {
            if let NamedEntities::Single(ent) = ent {
                if let Some(ent) = InterfaceEnt::from_any(ent) {
                    if ent.is_signal() {
                        ports.push(ent);
                    } else {
                        generics.push(ent);
                    }
                }
            }
        }
        // Sorting by source file position gives declaration order
        generics.sort_by_key(|ent| ent.decl_pos().map(|pos| pos.range().start));
        ports.sort_by_key(|ent| ent.decl_pos().map(|pos| pos.range().start));
        (
            FormalRegion::new_with(InterfaceType::Generic, generics),
            FormalRegion::new_with(InterfaceType::Port, ports),
        )
    }

    pub fn to_package_generic(&self) -> (GpkgRegion<'a>, Vec<EntRef<'a>>) {
        // @TODO separate generics and ports
        let mut generics = Vec::with_capacity(self.entities.len());
        let mut other = Vec::with_capacity(self.entities.len());

        for ent in self.entities.values() {
            match ent {
                NamedEntities::Single(ent) => {
                    if let Some(ent) = GpkgInterfaceEnt::from_any(ent) {
                        generics.push(ent);
                        continue;
                    }
                    other.push(*ent);
                }
                NamedEntities::Overloaded(overloaded) => {
                    if overloaded.len() == 1 {
                        if let Some(ent) = GpkgInterfaceEnt::from_any(overloaded.first().into()) {
                            generics.push(ent);
                            continue;
                        }
                    }
                    other.extend(overloaded.entities().map(EntRef::from));
                    // @TODO What about multiple overloaded interface subprograms?
                }
            }
        }
        // Sorting by source file position gives declaration order
        generics.sort_by_key(|ent| ent.decl_pos().map(|pos| pos.range().start));
        other.sort_by_key(|ent| ent.decl_pos().map(|pos| pos.range().start));
        (GpkgRegion::new(generics), other)
    }

    fn check_deferred_constant_pairs(&self, diagnostics: &mut dyn DiagnosticHandler) {
        match self.kind {
            // Package without body may not have deferred constants
            RegionKind::PackageDeclaration | RegionKind::PackageBody => {
                for ent in self.entities.values() {
                    if let AnyEntKind::DeferredConstant(..) = ent.first_kind() {
                        ent.first().error(diagnostics, format!("Deferred constant '{}' lacks corresponding full constant declaration in package body", ent.designator()));
                    }
                }
            }
            RegionKind::Other => {}
        }
    }

    fn check_protected_types_have_body(&self, diagnostics: &mut dyn DiagnosticHandler) {
        for ent in self.entities.values() {
            if let AnyEntKind::Type(Type::Protected(_, has_body)) = ent.first_kind() {
                if !has_body {
                    ent.first().error(
                        diagnostics,
                        format!("Missing body for protected type '{}'", ent.designator()),
                    );
                }
            }
        }
    }

    pub fn close(&self, diagnostics: &mut dyn DiagnosticHandler) {
        self.check_deferred_constant_pairs(diagnostics);
        self.check_protected_types_have_body(diagnostics);
    }

    pub fn add(&mut self, ent: EntRef<'a>, diagnostics: &mut dyn DiagnosticHandler) {
        if ent.kind().is_deferred_constant() && self.kind != RegionKind::PackageDeclaration {
            ent.error(
                diagnostics,
                "Deferred constants are only allowed in package declarations (not body)",
            );
            return;
        };

        match self.entities.entry(ent.designator().clone()) {
            Entry::Occupied(ref mut entry) => {
                let prev_ents = entry.get_mut();

                match prev_ents {
                    NamedEntities::Single(ref mut prev_ent) => {
                        if prev_ent.id() == ent.id() {
                            // Updated definition of previous entity
                            *prev_ent = ent;
                        } else if ent.is_declared_by(prev_ent) {
                            if self.kind != RegionKind::PackageBody
                                && ent.kind().is_non_deferred_constant()
                            {
                                ent.error(
                                    diagnostics,
                                    "Full declaration of deferred constant is only allowed in a package body",
                                );
                            } else {
                                *prev_ent = ent;
                            }
                        } else if let Some(pos) = ent.decl_pos() {
                            diagnostics.push(duplicate_error(
                                prev_ent.designator(),
                                pos,
                                prev_ent.decl_pos(),
                            ));
                        }
                    }
                    NamedEntities::Overloaded(ref mut overloaded) => {
                        match OverloadedEnt::from_any(ent) {
                            Some(ent) => {
                                if let Err(err) = overloaded.insert(ent) {
                                    diagnostics.push(err);
                                }
                            }
                            None => {
                                if let Some(pos) = ent.decl_pos() {
                                    diagnostics.push(duplicate_error(
                                        overloaded.first().designator(),
                                        pos,
                                        overloaded.first().decl_pos(),
                                    ));
                                }
                            }
                        }
                    }
                }
            }

            Entry::Vacant(entry) => {
                entry.insert(NamedEntities::new(ent));
            }
        }
    }

    /// Lookup a named entity declared in this region
    pub fn lookup_immediate(&self, designator: &Designator) -> Option<&NamedEntities<'a>> {
        self.entities.get(designator)
    }

    pub fn immediates(&self) -> impl Iterator<Item = EntRef<'a>> + '_ {
        self.entities
            .values()
            .flat_map(|ent| match ent {
                NamedEntities::Single(single) => itertools::Either::Left(std::iter::once(*single)),
                NamedEntities::Overloaded(overloaded) => {
                    itertools::Either::Right(overloaded.entities().map(EntRef::from))
                }
            })
            .filter(|ent| ent.is_explicit())
    }
}

pub trait SetReference {
    fn set_unique_reference(&mut self, ent: &AnyEnt);

    fn set_reference<'a>(&mut self, value: &'a impl AsUnique<'a>) {
        if let Some(ent) = value.as_unique() {
            self.set_unique_reference(ent);
        }
    }
}

pub trait AsUnique<'a> {
    fn as_unique(&self) -> Option<EntRef<'a>>;
}

impl<'a> AsUnique<'a> for OverloadedName<'a> {
    fn as_unique(&self) -> Option<EntRef<'a>> {
        if self.entities.len() == 1 {
            self.entities.values().next().map(|ent| (*ent).into())
        } else {
            None
        }
    }
}

impl<'a> AsUnique<'a> for NamedEntities<'a> {
    fn as_unique(&self) -> Option<EntRef<'a>> {
        match self {
            NamedEntities::Single(ent) => Some(ent),
            NamedEntities::Overloaded(overloaded) => overloaded.as_unique(),
        }
    }
}

impl<T> SetReference for WithRef<T> {
    fn set_unique_reference(&mut self, ent: &AnyEnt) {
        self.reference.set_unique_reference(ent);
    }
}

impl<T: SetReference> SetReference for WithPos<T> {
    fn set_unique_reference(&mut self, ent: &AnyEnt) {
        self.item.set_unique_reference(ent);
    }
}

impl SetReference for Reference {
    fn set_unique_reference(&mut self, ent: &AnyEnt) {
        *self = Some(ent.id());
    }
}

impl SetReference for Name {
    fn set_unique_reference(&mut self, ent: &AnyEnt) {
        if let Some(r) = self.suffix_reference_mut() {
            r.set_unique_reference(ent);
        }
    }
}

pub(super) fn duplicate_error(
    name: &impl std::fmt::Display,
    pos: &SrcPos,
    prev_pos: Option<&SrcPos>,
) -> Diagnostic {
    let mut diagnostic = Diagnostic::error(pos, format!("Duplicate declaration of '{name}'"));

    if let Some(prev_pos) = prev_pos {
        diagnostic.add_related(prev_pos, "Previously defined here");
    }

    diagnostic
}
