// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2020, Olof Kraigher olof.kraigher@gmail.com
use super::named_entity::*;
use super::region::*;
use crate::ast::*;
use crate::data::*;

use fnv::FnvHashMap;
use std::collections::hash_map::Entry;

#[derive(Clone, Debug)]
struct VisibleEntity<'a> {
    // The position where the entity was made visible
    visible_pos: Vec<Option<SrcPos>>,
    entity: EntRef<'a>,
}

impl<'a> VisibleEntity<'a> {
    fn clone_with_more_visiblity(&self, visible_pos: Option<&SrcPos>) -> VisibleEntity<'a> {
        let mut more = self.clone();
        more.visible_pos.push(visible_pos.cloned());
        more
    }
}

#[derive(Clone)]
struct VisibleRegion<'a> {
    // The position where the entity was made visible
    visible_pos: Vec<Option<SrcPos>>,
    region: &'a Region<'a>,
}

impl<'a> VisibleRegion<'a> {
    /// Add more visiblity so that when using a context we can follow the visibility chain
    fn clone_with_more_visiblity(&self, visible_pos: Option<&SrcPos>) -> VisibleRegion<'a> {
        let mut more = self.clone();
        more.visible_pos.push(visible_pos.cloned());
        more
    }
}

#[derive(Clone, Default)]
pub struct Visibility<'a> {
    // TODO store unique regions
    all_in_regions: Vec<VisibleRegion<'a>>,
    visible: FnvHashMap<Designator, FnvHashMap<EntityId, VisibleEntity<'a>>>,
}

impl<'a> Visibility<'a> {
    pub fn make_all_potentially_visible(
        &mut self,
        visible_pos: Option<&SrcPos>,
        region: &'a Region<'a>,
    ) {
        self.all_in_regions.push(VisibleRegion {
            visible_pos: vec![visible_pos.cloned()],
            region,
        });
    }

    pub fn add_context_visibility(
        &mut self,
        visible_pos: Option<&SrcPos>,
        visibility: &Visibility<'a>,
    ) {
        for visible_region in visibility.all_in_regions.iter() {
            self.all_in_regions
                .push(visible_region.clone_with_more_visiblity(visible_pos));
        }

        for (designator, visibile) in visibility.visible.iter() {
            for visible_ent in visibile.values() {
                // Implicit declarations will already have been added when used in the context
                self.insert(
                    designator.clone(),
                    visible_ent.clone_with_more_visiblity(visible_pos),
                );
            }
        }
    }

    pub fn make_potentially_visible_with_name(
        &mut self,
        visible_pos: Option<&SrcPos>,
        designator: Designator,
        ent: &'a AnyEnt,
    ) {
        // Add implicit declarations when using declaration
        // For example all enum literals are made implicititly visible when using an enum type
        for entity in ent.as_actual().implicits.iter() {
            self.make_potentially_visible_with_name(
                visible_pos,
                entity.designator().clone(),
                entity,
            );
        }

        let visible_ent = VisibleEntity {
            visible_pos: vec![visible_pos.cloned()],
            entity: ent,
        };

        self.insert(designator, visible_ent);
    }

    fn insert(&mut self, designator: Designator, visible_ent: VisibleEntity<'a>) {
        match self.visible.entry(designator) {
            Entry::Vacant(entry) => {
                let mut map = FnvHashMap::default();
                map.insert(visible_ent.entity.id(), visible_ent);
                entry.insert(map);
            }
            Entry::Occupied(mut entry) => {
                entry.get_mut().insert(visible_ent.entity.id(), visible_ent);
            }
        }
    }

    /// Helper function lookup a visible declaration within the region
    pub fn lookup_into(&self, designator: &Designator, visible: &mut Visible<'a>) {
        for visible_region in self.all_in_regions.iter() {
            if let Some(named_entities) = visible_region.region.lookup_immediate(designator) {
                match named_entities {
                    NamedEntities::Single(entity) => {
                        visible.insert(visible_region.visible_pos.clone(), entity);
                    }
                    NamedEntities::Overloaded(overloaded) => {
                        for entity in overloaded.entities() {
                            visible.insert(visible_region.visible_pos.clone(), entity.into());
                        }
                    }
                }
            }
        }

        if let Some(visible_entities) = self.visible.get(designator) {
            for visible_entity in visible_entities.values() {
                visible.insert(visible_entity.visible_pos.clone(), visible_entity.entity);
            }
        }
    }
}

#[derive(Default, Debug)]
pub struct Visible<'a> {
    visible_entities: FnvHashMap<EntityId, VisibleEntity<'a>>,
}

impl<'a> Visible<'a> {
    fn insert(&mut self, visible_pos: Vec<Option<SrcPos>>, entity: EntRef<'a>) {
        let actual_entity = entity.as_actual();

        match self.visible_entities.entry(actual_entity.id()) {
            Entry::Vacant(entry) => {
                entry.insert(VisibleEntity {
                    visible_pos,
                    entity,
                });
            }
            Entry::Occupied(mut entry) => {
                let old_entity = &entry.get().entity;
                if entity.is_alias_of(old_entity) {
                    // Ensure deepest alias is made visible
                    entry.insert(VisibleEntity {
                        visible_pos,
                        entity,
                    });
                }
            }
        };
    }

    pub fn into_unambiguous(
        self,
        pos: &SrcPos,
        designator: &Designator,
    ) -> Result<Option<NamedEntities<'a>>, Diagnostic> {
        let mut named_entities: Vec<_> = self
            .visible_entities
            .values()
            .map(|ent| ent.entity)
            .collect();

        if named_entities.is_empty() {
            Ok(None)
        } else if named_entities.iter().all(|ent| ent.is_overloaded()) {
            Ok(Some(NamedEntities::new_overloaded(
                named_entities
                    .into_iter()
                    .map(|ent| OverloadedEnt::from_any(ent).unwrap())
                    .collect(),
            )))
        } else if named_entities.len() == 1 {
            Ok(Some(NamedEntities::new(named_entities.pop().unwrap())))
        } else {
            // Duplicate visible items hide each other
            let mut error = Diagnostic::error(
                pos,
                format!("Name '{designator}' is hidden by conflicting use clause"),
            );

            fn last_visible_pos(visible_entity: &VisibleEntity) -> u32 {
                if let Some(pos) = visible_entity.visible_pos.iter().rev().flatten().next() {
                    return pos.range().start.line;
                }
                0
            }

            // Sort by last visible pos to make error messages and testing deterministic
            let mut visible_entities: Vec<_> = self.visible_entities.values().collect();
            visible_entities.sort_by_key(|ent| last_visible_pos(ent));

            for visible_entity in visible_entities {
                for visible_pos in visible_entity.visible_pos.iter().rev().flatten() {
                    error.add_related(
                        visible_pos,
                        format!("Conflicting name '{designator}' made visible here"),
                    );
                }
                if let Some(pos) = visible_entity.entity.decl_pos() {
                    error.add_related(
                        pos,
                        format!("Conflicting name '{designator}' declared here"),
                    );
                }
            }

            Err(error)
        }
    }
}
