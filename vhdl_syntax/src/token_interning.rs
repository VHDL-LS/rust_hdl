//! Since source code has a lot of repeated strings (i.e., keywords, common identifiers, ...),
//! string interning is a common strategy to avoid allocation overhead.
//! This enables a token to only store a pointer to the interned string, instead of the whole string
//! itself.
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::{latin_1::Latin1Str, tokens::TokenKind};
use std::sync::{Arc, LazyLock, RwLock};

/// An Arena that can allocate in some global context.
pub(crate) trait Arena {
    type Id;

    fn alloc(kind: TokenKind, value: Box<Latin1Str>) -> Symbol;
}

/// The global Arena allocates symbols using a vector and the static [GLOBAL_ARENA] value.
#[derive(Debug, Default)]
struct GlobalArena {
    // Using a vec is likely not the best choice since looking up an element takes O(N) time.
    // It would be better to have a collection where insertion and lookup is cheap,
    // but indexing doesn't have to be
    elements: RwLock<Vec<Arc<SymbolInner>>>,
}

static GLOBAL_ARENA: LazyLock<GlobalArena> = LazyLock::new(|| GlobalArena {
    elements: RwLock::new(Vec::default()),
});

impl Arena for GlobalArena {
    type Id = usize;

    fn alloc(kind: TokenKind, value: Box<Latin1Str>) -> Symbol {
        if let Ok(mut lock) = GLOBAL_ARENA.elements.write() {
            if let Some(existing_symbol) =
                lock.iter().find(|el| el.kind == kind && el.value == value)
            {
                Symbol::new(existing_symbol.clone())
            } else {
                let sym = Arc::new(SymbolInner {
                    value,
                    kind,
                    id: lock.len(),
                });
                lock.push(sym.clone());
                Symbol::new(sym)
            }
        } else {
            // TODO: This should never happen, but one should re-think panicking here.
            panic!("Multi access")
        }
    }
}

/// The internal representation of a string, used for a token,
/// and the token's kind.
#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) struct Symbol(Arc<SymbolInner>);

impl Symbol {
    pub fn kind(&self) -> TokenKind {
        self.0.kind
    }

    pub fn text(&self) -> &Latin1Str {
        &self.0.value
    }
}

#[derive(Debug)]
struct SymbolInner {
    // One should benchmark whether `TokenKind` should be included here or directly
    // in the token.
    // Practically, this is a performance <-> space tradeoff: In this implementation, the kind
    // is always used behind a pointer -> indirection.
    // On the other hand, this reduces the size of a token by roughly size_of(TokenKind)
    kind: TokenKind,
    // There is some optimization opportunity here.
    // The `SymbolInner` could store a `str` and the `TokenKind`
    // instead of boxing, since it's only usable behind a pointer.
    // Having the `value` boxed means double de-reference
    value: Box<Latin1Str>,
    /// A unique ID per symbol to enable efficient comparison.
    // TODO: This must be extended for case-insensitive comparison.
    id: usize,
}

impl PartialEq for SymbolInner {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for SymbolInner {}

impl Symbol {
    fn new(inner: Arc<SymbolInner>) -> Symbol {
        Symbol(inner)
    }

    pub fn allocate(kind: TokenKind, text: impl Into<Box<Latin1Str>>) -> Symbol {
        Self::allocate_in_arena::<GlobalArena>(kind, text)
    }

    pub fn allocate_in_arena<A: Arena>(kind: TokenKind, text: impl Into<Box<Latin1Str>>) -> Symbol {
        A::alloc(kind, text.into())
    }
}
