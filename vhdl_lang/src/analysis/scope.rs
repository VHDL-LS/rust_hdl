// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use crate::ast::*;
use crate::data::*;
use crate::named_entity::*;

use crate::data::error_codes::ErrorCode;
use fnv::FnvHashMap;
use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::rc::Rc;

#[derive(Default, Clone)]
pub(crate) struct Scope<'a>(Rc<RefCell<ScopeInner<'a>>>);

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
                ErrorCode::Unresolved,
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

impl<'a> NamedEntities<'a> {
    pub(crate) fn make_potentially_visible_in(
        &self,
        visible_pos: Option<&SrcPos>,
        scope: &Scope<'a>,
    ) {
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

impl Diagnostic {
    pub(crate) fn duplicate_error(
        name: &impl std::fmt::Display,
        pos: &SrcPos,
        prev_pos: Option<&SrcPos>,
    ) -> Diagnostic {
        let mut diagnostic = Diagnostic::error(
            pos,
            format!("Duplicate declaration of '{name}'"),
            ErrorCode::Duplicate,
        );

        if let Some(prev_pos) = prev_pos {
            diagnostic.add_related(prev_pos, "Previously defined here");
        }

        diagnostic
    }
}
