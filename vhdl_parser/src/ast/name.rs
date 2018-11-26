// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

//! Name conversions
use super::*;

impl From<SelectedName> for WithPos<Name> {
    fn from(selected_name: SelectedName) -> WithPos<Name> {
        let first = selected_name.get(0).unwrap().clone();
        let name = Name::Designator(Designator::Identifier(first.item));
        let mut name = WithPos::from(name, first.pos);

        for simple_name in selected_name.into_iter().skip(1) {
            let pos = name.pos.combine(&simple_name.pos);
            let item: Name =
                Name::Selected(Box::new(name), simple_name.map_into(Designator::Identifier));
            name = WithPos::from(item, pos);
        }

        name
    }
}
