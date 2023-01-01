// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2022, Olof Kraigher olof.kraigher@gmail.com

use super::NamedEntity;
use std::sync::{Arc, Weak};

/// Implicits typically contain self-references thus we use weak pointers
/// Intialization is unsafe to be able to create the implicits after the type definition
/// which they reference is created.
/// This happens immediately after creation in the same thread and so it is safe.
#[derive(Clone, Default)]
pub struct Implicits<T>(Arc<T>);
pub type ImplicitVec = Implicits<Vec<Weak<NamedEntity>>>;
pub type ImplicitVecBuilder = ImplicitsBuilder<Vec<Weak<NamedEntity>>>;

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

impl ImplicitVec {
    pub fn iter(&self) -> impl Iterator<Item = Arc<NamedEntity>> + '_ {
        self.0.iter().filter_map(|weak_ent| weak_ent.upgrade())
    }
}

#[derive(Default)]
pub struct ImplicitsBuilder<T>(Arc<T>);

impl<T> ImplicitsBuilder<T> {
    #[allow(clippy::mut_from_ref)]
    unsafe fn raw_mut(&self) -> &mut T {
        let ptr = self.0.as_ref() as *const T as *mut T;
        &mut *ptr
    }

    pub fn inner(&self) -> Implicits<T> {
        Implicits(self.0.clone())
    }
}
impl ImplicitVecBuilder {
    pub fn push(&self, ent: &Arc<NamedEntity>) {
        unsafe {
            self.raw_mut().push(Arc::downgrade(ent));
        }
    }
}
