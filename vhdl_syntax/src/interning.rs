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
        // Re-evaluate this when considering optimization
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::latin_1::Latin1Str;
    use std::collections::HashSet;
    use std::thread;

    fn interner<T>() -> RwLock<Interner<T>>
    where
        T: ?Sized + Hash + Eq + 'static,
        for<'a> Box<T>: From<&'a T>,
    {
        RwLock::new(Interner::new())
    }

    fn entry_count<T: ?Sized>(interner: &RwLock<Interner<T>>) -> usize {
        interner.read().unwrap().entries.len()
    }

    #[test]
    fn equal_values_share_a_symbol() {
        let interner = interner::<[u8]>();
        let first = Interned::get(&interner, b"entity".as_slice());
        let second = Interned::get(&interner, b"entity".as_slice());

        assert_eq!(first, second);
        // the second `get` must not allocate a new entry
        assert_eq!(entry_count(&interner), 1);
    }

    #[test]
    fn distinct_values_get_distinct_symbols() {
        let interner = interner::<[u8]>();
        let values: [&[u8]; 4] = [b"entity", b"architecture", b"", b"Entity"];
        let symbols: Vec<_> = values
            .iter()
            .map(|value| Interned::get(&interner, *value))
            .collect();

        assert_eq!(symbols.iter().collect::<HashSet<_>>().len(), values.len());
        assert_eq!(entry_count(&interner), values.len());
    }

    #[test]
    fn symbols_round_trip_to_their_value() {
        let interner = interner::<[u8]>();
        for value in [b"entity".as_slice(), b"architecture".as_slice(), b""] {
            let symbol = Interned::get(&interner, value);
            assert_eq!(symbol.value(&interner), value);
        }
    }

    #[test]
    fn interning_is_case_sensitive() {
        let interner = interner::<Latin1Str>();
        let lower = Interned::get(&interner, Latin1Str::new(b"entity"));
        let upper = Interned::get(&interner, Latin1Str::new(b"ENTITY"));

        assert_ne!(lower, upper);
        assert_eq!(lower.value(&interner), Latin1Str::new(b"entity"));
        assert_eq!(upper.value(&interner), Latin1Str::new(b"ENTITY"));
    }

    #[test]
    fn two_interners_of_the_same_type_are_independent() {
        let first = interner::<[u8]>();
        let second = interner::<[u8]>();

        Interned::get(&first, b"entity".as_slice());
        let a = Interned::get(&first, b"architecture".as_slice());
        let b = Interned::get(&second, b"architecture".as_slice());

        assert_eq!(a.value(&first), b"architecture".as_slice());
        assert_eq!(b.value(&second), b"architecture".as_slice());
        assert_eq!(entry_count(&first), 2);
        assert_eq!(entry_count(&second), 1);
    }

    #[test]
    fn concurrent_interning_of_one_value_yields_one_symbol() {
        let interner = interner::<[u8]>();

        let symbols: Vec<_> = thread::scope(|scope| {
            let handles: Vec<_> = (0..8)
                .map(|_| scope.spawn(|| Interned::get(&interner, b"entity".as_slice())))
                .collect();
            handles.into_iter().map(|h| h.join().unwrap()).collect()
        });

        assert!(symbols.iter().all(|symbol| *symbol == symbols[0]));
        assert_eq!(entry_count(&interner), 1);
    }
}
