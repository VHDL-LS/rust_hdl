//! Since source code has a lot of repeated strings (i.e., keywords, common identifiers, ...),
//! string interning is a common strategy to avoid allocation overhead.
//! This enables a token to only store a pointer to the interned string, instead of the whole string
//! itself.
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use std::{collections::HashMap, sync::RwLock};

use rustc_hash::FxBuildHasher;

use crate::latin_1::Latin1Str;

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub struct InternedLatin1(u32);

impl InternedLatin1 {
    fn new(value: u32) -> InternedLatin1 {
        InternedLatin1(value)
    }

    pub fn get(value: &Latin1Str) -> InternedLatin1 {
        if let Some(sym) = STR_INTERNER
            .read()
            .unwrap_or_else(|e| e.into_inner())
            .get_sym(value)
        {
            return sym;
        }
        STR_INTERNER
            .write()
            .unwrap_or_else(|e| e.into_inner())
            .alloc(value)
    }

    pub fn text(&self) -> &'static Latin1Str {
        // Note: this could avoid the lock since strings are pushed to an append-only collection
        // might re-evaluate when thinking about optimization
        STR_INTERNER
            .read()
            .unwrap_or_else(|e| e.into_inner())
            .get_text(self)
    }
}

/// Interner used for Latin1Str interning
///
/// Currently implemented in a simplistic way.
/// Further updates may improve this; the naive implementation
/// is mostly for fixing the shape and having something conceptually
/// close to a final solution.
struct Interner {
    // lookup str -> Symbol
    lookup: HashMap<&'static Latin1Str, InternedLatin1, FxBuildHasher>,
    // id -> text
    entries: Vec<&'static Latin1Str>,
}

impl Interner {
    const fn new() -> Interner {
        Interner {
            lookup: HashMap::with_hasher(FxBuildHasher),
            entries: Vec::new(),
        }
    }

    fn get_sym(&self, value: &Latin1Str) -> Option<InternedLatin1> {
        self.lookup.get(value).copied()
    }

    fn alloc(&mut self, symbol: &Latin1Str) -> InternedLatin1 {
        // We naively store each symbol on the heap
        let val = Box::leak(symbol.to_boxed());
        let new_id = InternedLatin1::new(self.entries.len() as u32);
        self.entries.push(val);
        self.lookup.insert(val, new_id);
        new_id
    }

    fn get_text(&self, value: &InternedLatin1) -> &'static Latin1Str {
        self.entries[value.0 as usize]
    }
}

static STR_INTERNER: RwLock<Interner> = RwLock::new(Interner::new());
