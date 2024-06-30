// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2024, Olof Kraigher olof.kraigher@gmail.com
use crate::named_entity::{AsUnique, NamedEntities, Region};
use crate::CompletionItem;

pub(crate) fn completion_items_from_region<'a>(
    region: &'a Region<'a>,
) -> impl Iterator<Item = CompletionItem<'a>> {
    region
        .entities
        .values()
        .map(named_entities_to_completion_item)
}

fn named_entities_to_completion_item<'a>(
    named_entities: &'a NamedEntities<'a>,
) -> CompletionItem<'a> {
    match named_entities {
        NamedEntities::Single(ent) => CompletionItem::Simple(ent),
        NamedEntities::Overloaded(overloaded) => match overloaded.as_unique() {
            None => CompletionItem::Overloaded(overloaded.designator().clone(), overloaded.len()),
            Some(ent) => CompletionItem::Simple(ent),
        },
    }
}
