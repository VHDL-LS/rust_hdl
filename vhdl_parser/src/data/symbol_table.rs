// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::latin_1::Latin1String;
use parking_lot::RwLock;
use std::sync::Arc;

use self::fnv::FnvHashMap;
use fnv;

/// Represents an unique string symbol
/// The id can be used as a fast comparison key for symbols
#[derive(Clone, Debug, Eq)]
pub struct Symbol {
    /// The unique id of the symbol
    pub(crate) id: usize,

    /// The name of the symbol
    name: Arc<Latin1String>,
}

impl Symbol {
    /// Create a new symbol
    fn new(id: usize, name: &Arc<Latin1String>) -> Symbol {
        Symbol {
            id,
            name: Arc::clone(name),
        }
    }

    /// Return the name of the symbol
    pub fn name(self: &Self) -> &Latin1String {
        self.name.as_ref()
    }

    /// Return the name of the symbol
    pub fn name_utf8(self: &Self) -> String {
        self.name.to_string()
    }
}

impl PartialEq for Symbol {
    /// Symbols are compared just based on the id
    fn eq(self: &Self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name_utf8())
    }
}

impl std::hash::Hash for Symbol {
    fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
        self.id.hash(hasher);
    }
}

/// A case insensitive symbol table to allocate unique id:s to symbols
/// which are equal during case insensitive comparison
pub struct SymbolTable {
    name_to_symbol: RwLock<FnvHashMap<Arc<Latin1String>, Symbol>>,
}

impl SymbolTable {
    /// Create a new symbol table
    pub fn new() -> SymbolTable {
        SymbolTable {
            name_to_symbol: RwLock::new(FnvHashMap::default()),
        }
    }

    pub fn insert_utf8(&self, name: &str) -> Symbol {
        let name = Latin1String::from_utf8(name).unwrap();
        self.insert(&name)
    }

    #[cfg(test)]
    pub fn insert_extended_utf8(&self, name: &str) -> Symbol {
        let name = Latin1String::from_utf8_unchecked(name);
        self.insert_extended(&name)
    }

    pub fn lookup(&self, name: &Latin1String) -> Option<Symbol> {
        let name_to_symbol = self.name_to_symbol.read();
        if let Some(sym) = name_to_symbol.get(name) {
            // Symbol already exists with identical case
            return Some(sym.clone());
        } else {
            return None;
        }
    }

    /// Insert a new symbol and return it. If a symbol already exists
    /// that matches the case insensitive name it is returned
    pub fn insert(&self, name: &Latin1String) -> Symbol {
        if let Some(symbol) = self.lookup(name) {
            symbol
        } else {
            self.insert_new(name, false)
        }
    }

    pub fn insert_extended(&self, name: &Latin1String) -> Symbol {
        if let Some(symbol) = self.lookup(name) {
            symbol
        } else {
            self.insert_new(name, true)
        }
    }

    fn insert_new(&self, name: &Latin1String, is_extended: bool) -> Symbol {
        let mut name_to_symbol = self.name_to_symbol.write();

        // Lookup again after taking lock to avoid race-condition where new symbols are created in parallel
        if let Some(sym) = name_to_symbol.get(name) {
            // Symbol already exists with identical case
            return sym.clone();
        }

        debug_assert_eq!(name.bytes.get(0) == Some(&b'\\'), is_extended);
        let name = Arc::from(name.clone());
        if is_extended {
            let id = name_to_symbol.len();
            let sym = Symbol::new(id, &name);
            name_to_symbol.insert(name, sym.clone());
            return sym;
        }

        // Symbol does not exists with the given case, try normalizing case
        let normal_name = Arc::from(name.to_lowercase());

        match name_to_symbol.get(&normal_name).cloned() {
            // Symbol exists in normalized case
            Some(normal_sym) => {
                // Copy id from previous symbol
                // Insert new symbol with given case and return it
                let id = normal_sym.id;
                let sym = Symbol::new(id, &name);
                name_to_symbol.insert(name, sym.clone());
                sym
            }

            // Symbol does not exist
            None => {
                // Create new id
                let id = name_to_symbol.len();

                if normal_name != name {
                    // If symbol is not already normalized case insert it
                    let sym = Symbol::new(id, &normal_name);
                    name_to_symbol.insert(normal_name, sym);
                }

                let sym = Symbol::new(id, &name);
                name_to_symbol.insert(name, sym.clone());
                sym
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn symbol_table_insert() {
        let symtab = SymbolTable::new();
        let sym = symtab.insert_utf8("hello");
        assert_eq!(sym.name_utf8(), "hello");
    }

    #[test]
    fn symbols_are_equal() {
        let symtab = SymbolTable::new();
        let sym0 = symtab.insert_utf8("hello");
        let sym1 = symtab.insert_utf8("hello");
        assert_eq!(sym0, sym1);
        assert_eq!(sym0.name_utf8(), "hello");
        assert_eq!(sym1.name_utf8(), "hello");

        let sym0 = symtab.insert_utf8("Hello");
        let sym1 = symtab.insert_utf8("hello");
        assert_eq!(sym0, sym1);
        assert_eq!(sym0.name_utf8(), "Hello");
        assert_eq!(sym1.name_utf8(), "hello");
    }

    #[test]
    fn symbols_are_case_insensitive() {
        let symtab = SymbolTable::new();
        let sym0 = symtab.insert_utf8("Hello");
        let sym1 = symtab.insert_utf8("hello");
        let sym2 = symtab.insert_utf8("heLLo");
        assert_eq!(sym0, sym1);
        assert_eq!(sym0, sym2);
        assert_eq!(sym1, sym2);
        assert_eq!(sym0.name_utf8(), "Hello");
        assert_eq!(sym1.name_utf8(), "hello");
        assert_eq!(sym2.name_utf8(), "heLLo");
    }

    #[test]
    fn extended_identifiers_symbols_are_case_sensitive() {
        let symtab = SymbolTable::new();
        let sym0 = symtab.insert_extended_utf8("\\hello\\");
        let sym1 = symtab.insert_extended_utf8("\\HELLO\\");
        let sym2 = symtab.insert_extended_utf8("\\hello\\");
        assert_ne!(sym0, sym1);
        assert_eq!(sym0, sym2);
        assert_ne!(sym1, sym2);
        assert_eq!(sym0.name_utf8(), "\\hello\\");
        assert_eq!(sym1.name_utf8(), "\\HELLO\\");
        assert_eq!(sym2.name_utf8(), "\\hello\\");
    }

    #[test]
    fn symbols_are_not_equal() {
        let symtab = SymbolTable::new();
        let sym0 = symtab.insert_utf8("hello");
        let sym1 = symtab.insert_utf8("abc");
        assert_ne!(sym0, sym1);
    }
}
