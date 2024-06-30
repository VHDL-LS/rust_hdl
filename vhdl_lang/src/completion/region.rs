use vhdl_lang::EntRef;
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2024, Olof Kraigher olof.kraigher@gmail.com
use crate::analysis::DesignRoot;
use crate::completion::entity_instantiation::get_architectures_for_entity;
use crate::named_entity::{AsUnique, NamedEntities, Region};
use crate::{AnyEntKind, CompletionItem, Design};

pub(crate) fn completion_items_from_region<'a>(
    root: &'a DesignRoot,
    region: &'a Region<'a>,
) -> impl Iterator<Item = CompletionItem<'a>> {
    region
        .entities
        .values()
        .map(|entities| named_entities_to_completion_item(root, entities))
}

fn named_entities_to_completion_item<'a>(
    root: &'a DesignRoot,
    named_entities: &'a NamedEntities<'a>,
) -> CompletionItem<'a> {
    match named_entities {
        NamedEntities::Single(ent) => any_ent_to_completion_item(ent, root),
        NamedEntities::Overloaded(overloaded) => match overloaded.as_unique() {
            None => CompletionItem::Overloaded(overloaded.designator().clone(), overloaded.len()),
            Some(ent) => CompletionItem::Simple(ent),
        },
    }
}

pub(crate) fn any_ent_to_completion_item<'a>(
    ent: EntRef<'a>,
    root: &'a DesignRoot,
) -> CompletionItem<'a> {
    match ent.kind() {
        AnyEntKind::Design(Design::Entity(..)) | AnyEntKind::Component(_) => {
            let architectures = get_architectures_for_entity(ent, root);
            CompletionItem::Instantiation(ent, architectures)
        }
        _ => CompletionItem::Simple(ent),
    }
}
