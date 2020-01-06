// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2020, Olof Kraigher olof.kraigher@gmail.com
use super::region::*;
use crate::ast::*;
use crate::data::*;

use fnv::FnvHashMap;
use std::collections::hash_map::Entry;
use std::sync::Arc;

#[derive(Clone)]
struct VisibleEntity {
    // The position where the entity was made visible
    visible_pos: Vec<Option<SrcPos>>,
    entity: NamedEntity,
}

impl VisibleEntity {
    fn clone_with_more_visiblity(&self, visible_pos: Option<&SrcPos>) -> VisibleEntity {
        let mut more = self.clone();
        more.visible_pos.push(visible_pos.cloned());
        more
    }
}

#[derive(Clone)]
struct VisibleRegion {
    // The position where the entity was made visible
    visible_pos: Vec<Option<SrcPos>>,
    region: Arc<Region<'static>>,
}

impl VisibleRegion {
    /// Add more visiblity so that when using a context we can follow the visibility chain
    fn clone_with_more_visiblity(&self, visible_pos: Option<&SrcPos>) -> VisibleRegion {
        let mut more = self.clone();
        more.visible_pos.push(visible_pos.cloned());
        more
    }
}

#[derive(Clone, Default)]
pub struct Visibility {
    // TODO store unique regions
    all_in_regions: Vec<VisibleRegion>,
    // Use decl_pos as key until named entities have unique id:s
    visible: FnvHashMap<Designator, FnvHashMap<Option<SrcPos>, VisibleEntity>>,
}

impl Visibility {
    pub fn make_all_potentially_visible(
        &mut self,
        visible_pos: Option<&SrcPos>,
        region: &Arc<Region<'static>>,
    ) {
        self.all_in_regions.push(VisibleRegion {
            visible_pos: vec![visible_pos.cloned()],
            region: region.clone(),
        });
    }

    pub fn add_context_visibility(
        &mut self,
        visible_pos: Option<&SrcPos>,
        visibility: &Visibility,
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

    pub fn make_potentially_visible(
        &mut self,
        designator: Designator,
        visible_pos: Option<&SrcPos>,
        ent: NamedEntity,
    ) {
        // Add implicit declarations when using declaration
        // For example all enum literals are made implicititly visible when using an enum type
        if let NamedEntityKind::TypeDeclaration(ref implicit) = ent.as_actual().kind() {
            // Add implicitic declarations when using type
            if let Some(implicit) = implicit {
                self.make_all_potentially_visible(visible_pos, implicit);
            }
        }

        let visible_ent = VisibleEntity {
            visible_pos: vec![visible_pos.cloned()],
            entity: ent,
        };

        self.insert(designator, visible_ent);
    }

    fn insert(&mut self, designator: Designator, visible_ent: VisibleEntity) {
        match self.visible.entry(designator) {
            Entry::Vacant(entry) => {
                let mut map = FnvHashMap::default();
                map.insert(visible_ent.entity.decl_pos().cloned(), visible_ent);
                entry.insert(map);
            }
            Entry::Occupied(mut entry) => {
                entry
                    .get_mut()
                    .insert(visible_ent.entity.decl_pos().cloned(), visible_ent);
            }
        }
    }

    /// Helper function lookup a visible declaration within the region
    pub fn lookup_into<'a>(&'a self, designator: &Designator, visible: &mut Visible<'a>) {
        for visible_region in self.all_in_regions.iter() {
            if let Some(named_entities) = visible_region.region.lookup_selected(designator) {
                for named_entity in named_entities.named_entities() {
                    visible.insert(&visible_region.visible_pos, named_entity);
                }
            }
        }

        if let Some(visible_entities) = self.visible.get(designator) {
            for visible_entity in visible_entities.values() {
                visible.insert(&visible_entity.visible_pos, &visible_entity.entity);
            }
        }
    }
}

struct VisibleEntityRef<'a> {
    visible_pos: &'a [Option<SrcPos>],
    entity: &'a NamedEntity,
}

#[derive(Default)]
pub struct Visible<'a> {
    visible_entities: FnvHashMap<Option<&'a SrcPos>, VisibleEntityRef<'a>>,
}

impl<'a> Visible<'a> {
    fn insert(&mut self, visible_pos: &'a [Option<SrcPos>], entity: &'a NamedEntity) {
        // @TODO check that they actually correspond to the same object
        // The decl_pos serves as a good proxy for this except for libraries
        self.visible_entities.insert(
            entity.decl_pos(),
            VisibleEntityRef {
                visible_pos,
                entity,
            },
        );
    }

    pub fn into_unambiguous(
        self,
        pos: &SrcPos,
        designator: &Designator,
    ) -> Result<Option<VisibleDeclaration>, Diagnostic> {
        let named_entities: Vec<_> = self
            .visible_entities
            .iter()
            .map(|(_, ent)| ent.entity.clone())
            .collect();

        if named_entities.is_empty() {
            Ok(None)
        } else if named_entities.len() == 1
            || named_entities.iter().all(|ent| ent.is_overloaded())
            || named_entities.iter().any(|ent| ent.kind().is_alias())
        {
            let visible = VisibleDeclaration::new_vec(designator.clone(), named_entities);
            // Until we have unique id:s we disable hidden check for any alias
            Ok(Some(visible))
        } else {
            // Duplicate visible items hide each other
            // @TODO improve error message with visibility pos as well as decl pos
            let mut error = Diagnostic::error(
                pos,
                format!("Name '{}' is hidden by conflicting use clause", designator),
            );

            fn last_visible_pos(visible_entity: &VisibleEntityRef) -> u32 {
                for pos in visible_entity.visible_pos.iter().rev() {
                    if let Some(pos) = pos {
                        return pos.range().start.line;
                    }
                }
                0
            }

            // Sort by last visible pos to make error messages and testing deterministic
            let mut visible_entities: Vec<_> = self.visible_entities.values().collect();
            visible_entities.sort_by_key(|ent| last_visible_pos(*ent));

            for visible_entity in visible_entities {
                for visible_pos in visible_entity.visible_pos.iter().rev() {
                    if let Some(pos) = visible_pos {
                        error.add_related(
                            pos,
                            format!("Conflicting name '{}' made visible here", designator),
                        );
                    }
                }
                if let Some(pos) = visible_entity.entity.decl_pos() {
                    error.add_related(
                        pos,
                        format!("Conflicting name '{}' declared here", designator),
                    );
                }
            }

            Err(error)
        }
    }
}
