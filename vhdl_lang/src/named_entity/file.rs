// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::named_entity::TypeEnt;
use crate::{AnyEnt, AnyEntKind, EntRef};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct FileEnt<'a>(EntRef<'a>);

impl<'a> FileEnt<'a> {
    pub fn from_any(ent: EntRef<'a>) -> Option<FileEnt<'a>> {
        match ent.kind() {
            AnyEntKind::File(_) | AnyEntKind::InterfaceFile(_) => Some(FileEnt(ent)),
            _ => None,
        }
    }

    pub fn inner(&self) -> EntRef<'a> {
        self.0
    }

    pub fn type_mark(&self) -> TypeEnt<'a> {
        match self.0.kind() {
            AnyEntKind::File(subtype) => subtype.type_mark(),
            AnyEntKind::InterfaceFile(typ) => *typ,
            _ => unreachable!(),
        }
    }
}

impl<'a> std::ops::Deref for FileEnt<'a> {
    type Target = AnyEnt<'a>;
    fn deref(&self) -> EntRef<'a> {
        self.inner()
    }
}
