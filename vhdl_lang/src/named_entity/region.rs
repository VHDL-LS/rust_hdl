// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use super::*;
use crate::ast::*;
use crate::named_entity::overloaded::SubprogramKey;
use fnv::FnvHashMap;
use std::collections::hash_map::Entry;

#[derive(Clone)]
pub struct Region<'a> {
    pub(crate) visibility: Visibility<'a>,
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
    pub(crate) fn with_visibility(visibility: Visibility<'a>) -> Self {
        Self {
            visibility,
            ..Default::default()
        }
    }

    pub(crate) fn in_package_declaration(mut self) -> Region<'a> {
        self.kind = RegionKind::PackageDeclaration;
        self
    }

    pub(crate) fn to_entity_formal(&self) -> (FormalRegion<'a>, FormalRegion<'a>) {
        let (ports, generics) = self.ports_and_generics();
        (
            FormalRegion::new_with(InterfaceType::Generic, generics),
            FormalRegion::new_with(InterfaceType::Port, ports),
        )
    }

    pub fn ports_and_generics(&self) -> (Vec<InterfaceEnt<'a>>, Vec<InterfaceEnt<'a>>) {
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
        (ports, generics)
    }

    pub(crate) fn to_package_generic(&self) -> (GpkgRegion<'a>, Vec<EntRef<'a>>) {
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
                        ent.first().error(diagnostics, format!("Deferred constant '{}' lacks corresponding full constant declaration in package body", ent.designator()), ErrorCode::NotDeclared);
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
                        ErrorCode::MissingProtectedBodyType,
                    );
                }
            }
        }
    }

    pub(crate) fn close(&self, diagnostics: &mut dyn DiagnosticHandler) {
        self.check_deferred_constant_pairs(diagnostics);
        self.check_protected_types_have_body(diagnostics);
    }

    pub fn add(&mut self, ent: EntRef<'a>, diagnostics: &mut dyn DiagnosticHandler) {
        if ent.kind().is_deferred_constant() && self.kind != RegionKind::PackageDeclaration {
            ent.error(
                diagnostics,
                "Deferred constants are only allowed in package declarations (not body)",
                ErrorCode::DeferredConstantNotAllowed,
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
                                    ErrorCode::DeferredConstantNotAllowed
                                );
                            } else {
                                *prev_ent = ent;
                            }
                        } else if let Some(pos) = ent.decl_pos() {
                            diagnostics.push(Diagnostic::duplicate_error(
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
                                    diagnostics.push(Diagnostic::duplicate_error(
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

#[derive(Clone, Debug, PartialEq, Eq)]
/// A non-empty collection of overloaded entities
pub struct OverloadedName<'a> {
    entities: FnvHashMap<SubprogramKey<'a>, OverloadedEnt<'a>>,
}

impl<'a> OverloadedName<'a> {
    pub fn new(entities: Vec<OverloadedEnt>) -> OverloadedName {
        debug_assert!(!entities.is_empty());
        let mut map = FnvHashMap::default();
        for ent in entities.into_iter() {
            map.insert(ent.subprogram_key(), ent);
        }
        OverloadedName { entities: map }
    }

    pub fn single(ent: OverloadedEnt) -> OverloadedName {
        let mut map = FnvHashMap::default();
        map.insert(ent.subprogram_key(), ent);
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

    pub fn signatures(&self) -> impl Iterator<Item = &crate::named_entity::Signature<'a>> + '_ {
        self.entities().map(|ent| ent.signature())
    }

    pub fn get(&self, key: &SubprogramKey) -> Option<OverloadedEnt<'a>> {
        self.entities.get(key).cloned()
    }

    #[allow(clippy::if_same_then_else)]
    fn insert(&mut self, ent: OverloadedEnt<'a>) -> Result<(), Diagnostic> {
        match self.entities.entry(ent.subprogram_key()) {
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
                    ErrorCode::DuplicateDeclaration,
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
    pub(crate) fn with_visible(mut self, visible: Self) -> Self {
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
}

#[derive(Copy, Clone, PartialEq, Default)]
pub(crate) enum RegionKind {
    PackageDeclaration,
    PackageBody,
    #[default]
    Other,
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

pub trait SetReference {
    fn set_unique_reference(&mut self, ent: &AnyEnt);

    fn set_reference<'a>(&mut self, value: &'a impl AsUnique<'a>) {
        if let Some(ent) = value.as_unique() {
            self.set_unique_reference(ent);
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
        self.set(ent.id());
    }
}

impl SetReference for Name {
    fn set_unique_reference(&mut self, ent: &AnyEnt) {
        if let Some(r) = self.suffix_reference_mut() {
            r.set_unique_reference(ent);
        }
    }
}
