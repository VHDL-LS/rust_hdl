// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

//! Interning facilities for storing reoccuring values efficiently

use std::{collections::HashMap, hash::Hash, marker::PhantomData, sync::RwLock};

use rustc_hash::FxBuildHasher;

#[derive(Debug)]
pub(crate) struct Interned<T: ?Sized>(u32, PhantomData<fn() -> T>);

impl<T: ?Sized> Copy for Interned<T> {}

impl<T: ?Sized> Clone for Interned<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: ?Sized> std::hash::Hash for Interned<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl<T: ?Sized> PartialEq for Interned<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T: ?Sized> Eq for Interned<T> {}

impl<T: ?Sized> PartialOrd for Interned<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: ?Sized> Ord for Interned<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl<T: ?Sized> Interned<T> {
    fn new(value: u32) -> Interned<T> {
        Interned(value, PhantomData)
    }
}

impl<T> Interned<T>
where
    T: ?Sized + Hash + Eq,
    for<'a> Box<T>: From<&'a T>,
{
    pub fn get(interner: &RwLock<Interner<T>>, value: &T) -> Interned<T> {
        if let Some(sym) = interner
            .read()
            .unwrap_or_else(|e| e.into_inner())
            .get_sym(value)
        {
            return sym;
        }
        interner
            .write()
            .unwrap_or_else(|e| e.into_inner())
            .get_or_alloc(value)
    }

    pub fn value(&self, interner: &RwLock<Interner<T>>) -> &'static T {
        // Note: this could avoid the lock since strings are pushed to an append-only collection
        // might re-evaluate when thinking about optimization
        interner
            .read()
            .unwrap_or_else(|e| e.into_inner())
            .get_value(self)
    }
}

/// global interner implementation
///
/// Currently implemented in a simplistic way:
/// symbols are allocated globally and leaked; no re-allocation / drop is forseen
pub(crate) struct Interner<T: ?Sized + 'static> {
    // lookup T -> Interned<T>
    lookup: HashMap<&'static T, Interned<T>, FxBuildHasher>,
    // Interned<T> -> T
    entries: Vec<&'static T>,
}

impl<T: ?Sized + Hash + Eq + 'static> Interner<T>
where
    for<'a> Box<T>: From<&'a T>,
{
    pub(crate) const fn new() -> Interner<T> {
        Interner {
            lookup: HashMap::with_hasher(FxBuildHasher),
            entries: Vec::new(),
        }
    }

    fn get_sym(&self, value: &T) -> Option<Interned<T>> {
        self.lookup.get(value).copied()
    }

    fn get_or_alloc(&mut self, symbol: &T) -> Interned<T> {
        if let Some(sym) = self.get_sym(symbol) {
            return sym;
        }
        // each value is heap-allocated and lives forever.
        let val: &'static T = Box::leak(Box::<T>::from(symbol));
        let id = Interned::new(self.entries.len() as u32);
        self.entries.push(val);
        self.lookup.insert(val, id);
        id
    }

    fn get_value(&self, value: &Interned<T>) -> &'static T {
        self.entries[value.0 as usize]
    }
}
