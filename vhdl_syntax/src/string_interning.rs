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

use crate::latin_1::{latin1_char_lowercased, Latin1Str, Latin1String};

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub struct Name(u32);

impl Name {
    fn new(value: u32) -> Name {
        Name(value)
    }

    pub fn text(&self) -> &'static Latin1Str {
        INTERNER
            .read()
            .unwrap_or_else(|e| e.into_inner())
            .get_str_raw(self.0 as usize)
    }
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub struct Symbol(u32);

impl Symbol {
    fn new(value: u32) -> Symbol {
        Symbol(value)
    }

    pub fn get(value: &Latin1Str) -> Symbol {
        if let Some(sym) = INTERNER
            .read()
            .unwrap_or_else(|e| e.into_inner())
            .get_sym(value)
        {
            return sym;
        }
        INTERNER
            .write()
            .unwrap_or_else(|e| e.into_inner())
            .get_or_alloc(value)
    }

    pub fn text(&self) -> &'static Latin1Str {
        INTERNER
            .read()
            .unwrap_or_else(|e| e.into_inner())
            .get_str_raw(self.0 as usize)
    }

    pub fn name(&self) -> Name {
        INTERNER
            .read()
            .unwrap_or_else(|e| e.into_inner())
            .get_name(self)
    }
}

struct Entry<'a> {
    text: &'a Latin1Str,
    canonical: u32,
}

impl<'a> Entry<'a> {
    pub fn new(text: &'a Latin1Str, canonical: u32) -> Entry<'a> {
        Entry { text, canonical }
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
    lookup: HashMap<&'static Latin1Str, Symbol, FxBuildHasher>,
    // id -> text
    entries: Vec<Entry<'static>>,
}

fn is_start_of_extended_identifier(value: &Latin1Str) -> bool {
    value.as_bytes().first() == Some(&b'\\')
}

fn is_canonical(value: &Latin1Str) -> bool {
    if is_start_of_extended_identifier(value) {
        return true;
    }
    value
        .as_bytes()
        .iter()
        .all(|&b| latin1_char_lowercased(b) == b)
}

fn canonicalize(value: &Latin1Str) -> Latin1String {
    debug_assert!(
        !is_start_of_extended_identifier(value),
        "should be guarded by is_canonical"
    );
    value.to_lowercase()
}

impl Interner {
    const fn new() -> Interner {
        Interner {
            lookup: HashMap::with_hasher(FxBuildHasher),
            entries: Vec::new(),
        }
    }

    /// Get a symbol, or allocate if it's not already interned
    fn get_or_alloc(&mut self, symbol: &Latin1Str) -> Symbol {
        match self.get_sym(symbol) {
            Some(sym) => sym,
            // Potential deadlock:
            None => self.alloc(symbol),
        }
    }

    fn alloc(&mut self, symbol: &Latin1Str) -> Symbol {
        let canonical = if !is_canonical(symbol) {
            Some(self.get_or_alloc(&canonicalize(symbol)).0)
        } else {
            None
        };
        // We naively store each symbol on the heap
        let val = Box::leak(symbol.to_boxed());
        let new_id = self.entries.len() as u32;
        let entry = Entry::new(val, canonical.unwrap_or(new_id));
        let sym = Symbol::new(new_id);
        self.lookup.insert(val, sym);
        self.entries.push(entry);
        sym
    }

    fn get_str_raw(&self, value: usize) -> &'static Latin1Str {
        self.entries[value].text
    }

    fn get_name(&self, symbol: &Symbol) -> Name {
        let canonical = self.entries[symbol.0 as usize].canonical;
        Name::new(canonical)
    }

    fn get_sym(&self, value: &Latin1Str) -> Option<Symbol> {
        self.lookup.get(value).copied()
    }
}

static INTERNER: RwLock<Interner> = RwLock::new(Interner::new());
