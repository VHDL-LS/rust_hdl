// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2022, Olof Kraigher olof.kraigher@gmail.com

use super::named_entity::EntRef;
use super::AnyEnt;
use std::sync::Arc;

/// Implicits typically contain self-references thus we use weak pointers
/// Intialization is unsafe to be able to create the implicits after the type definition
/// which they reference is created.
/// This happens immediately after creation in the same thread and so it is safe.
#[derive(Clone, Default)]
pub struct Implicits<T>(Arc<T>);
pub type ImplicitVec<'a> = Implicits<Vec<EntRef<'a>>>;
pub type ImplicitVecBuilder<'a> = ImplicitsBuilder<Vec<EntRef<'a>>>;

impl<T: Default> Implicits<T> {
    pub fn build() -> ImplicitsBuilder<T> {
        ImplicitsBuilder(Default::default())
    }
}

impl<T> std::ops::Deref for Implicits<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a> ImplicitVec<'a> {
    pub fn iter(&self) -> impl Iterator<Item = EntRef<'a>> + '_ {
        self.0.iter().cloned()
    }

    pub unsafe fn push(&self, ent: &'a AnyEnt) {
        raw_mut(&self.0).push(ent);
    }
}

#[derive(Default)]
pub struct ImplicitsBuilder<T>(Arc<T>);

impl<T> ImplicitsBuilder<T> {
    pub fn inner(&self) -> Implicits<T> {
        Implicits(self.0.clone())
    }
}
impl<'a> ImplicitVecBuilder<'a> {
    pub fn push(&self, ent: &'a AnyEnt) {
        unsafe {
            raw_mut(&self.0).push(ent);
        }
    }
}

#[allow(clippy::mut_from_ref)]
unsafe fn raw_mut<T>(arc: &Arc<T>) -> &mut T {
    let ptr = arc.as_ref() as *const T as *mut T;
    &mut *ptr
}
