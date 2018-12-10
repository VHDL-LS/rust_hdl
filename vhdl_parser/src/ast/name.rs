// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

//! Name conversions
use super::*;

impl From<WithPos<SelectedName>> for WithPos<Name> {
    fn from(selected_name: WithPos<SelectedName>) -> WithPos<Name> {
        match selected_name.item {
            SelectedName::Designator(designator) => {
                WithPos::from(Name::Designator(designator), selected_name.pos)
            }

            SelectedName::Selected(prefix, suffix) => {
                let prefix: WithPos<SelectedName> = *prefix;
                WithPos::from(
                    Name::Selected(Box::new(prefix.into()), suffix),
                    selected_name.pos,
                )
            }
        }
    }
}
