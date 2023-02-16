//! This Source Code Form is subject to the terms of the Mozilla Public
//! License, v. 2.0. If a copy of the MPL was not distributed with this file,
//! You can obtain one at http://mozilla.org/MPL/2.0/.
//!
//! Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use pinned_vec::PinnedVec;
use std::cell::RefCell;
use std::sync::atomic::AtomicU32;
use std::sync::atomic::Ordering;
use std::sync::Arc;

use fnv::FnvHashMap;

use crate::ast::Designator;
use crate::SrcPos;

use super::AnyEnt;
use super::AnyEntKind;
use super::EntRef;
use super::Related;
use super::TypeEnt;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct ArenaId(u32);

// Reserve 0 for standard package
static ACOUNTER: AtomicU32 = AtomicU32::new(1);

impl Default for ArenaId {
    fn default() -> Self {
        ArenaId(ACOUNTER.fetch_add(1, Ordering::Relaxed))
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct LocalId(u32);

/// Arena allocators used to store named entities

/// Local arena used for single design unit in a separate thread
struct LocalArena {
    pub id: ArenaId,
    items: PinnedVec<AnyEnt<'static>>,
}

impl LocalArena {
    pub fn new(id: ArenaId) -> Self {
        Self {
            id,
            items: PinnedVec::new(),
        }
    }

    unsafe fn alloc(&mut self, mut ent: AnyEnt) -> *const AnyEnt<'static> {
        let idx = self.items.len();

        if idx > u32::MAX as usize {
            panic!("Entity index overflow");
        }

        let ent_id = EntityId::new_arena(self.id, LocalId(idx as u32));
        ent.id = ent_id;
        self.items.push(std::mem::transmute(ent));
        self.get(ent_id.local_id())
    }

    unsafe fn get(&self, id: LocalId) -> *const AnyEnt<'static> {
        self.panic_on_missing(id);

        let item = self.items.get(id.0 as usize).unwrap();
        std::pin::Pin::into_inner(item) as *const AnyEnt
    }

    unsafe fn get_mut(&mut self, id: LocalId) -> *mut AnyEnt {
        self.panic_on_missing(id);
        let item = self.items.get_mut(id.0 as usize).unwrap();
        std::mem::transmute(std::pin::Pin::into_inner(item) as *mut AnyEnt)
    }

    fn panic_on_missing(&self, id: LocalId) {
        if (id.0 as usize) < self.items.len() {
            return;
        }
        eprintln!("Could not find {:?} within arena {:?}", id, self.id);
        eprintln!("Found these entities:");
        for i in 0..self.items.len() {
            let ent = self.items.get(i).unwrap();
            eprintln!(
                "{:?} {:?} {}",
                ent.id().arena_id(),
                ent.id().local_id(),
                ent.describe()
            );
        }
        panic!("Panic on missing id in arena");
    }
}

/// A read-only self-contained arena that only contain self references
#[derive(Clone, Default)]
pub struct FinalArena {
    refs: FnvHashMap<u32, Arc<LocalArena>>,
}

impl<'a> FinalArena {
    pub fn get(&'a self, id: EntityId) -> EntRef<'a> {
        unsafe {
            let ent = self.refs[&id.arena_id().0].get(id.local_id());
            &*ent as &'a AnyEnt
        }
    }

    pub fn link(&mut self, referenced: &FinalArena) {
        for (id, arena) in referenced.refs.iter() {
            self.refs.entry(*id).or_insert_with(|| arena.clone());
        }
    }

    pub fn clear(&mut self) {
        self.refs.clear();
    }
}

/// A combination of a local mutable arena together with non-local immutable references
/// This arena is used when analyzing a design unit in a single thread
pub struct Arena {
    local: RefCell<LocalArena>,
    refs: RefCell<FinalArena>,
}

impl Arena {
    pub fn new(id: ArenaId) -> Self {
        Self {
            local: RefCell::new(LocalArena::new(id)),
            refs: Default::default(),
        }
    }

    // Pre-defined id to store standard package
    pub fn new_std() -> Self {
        Self {
            local: RefCell::new(LocalArena::new(ArenaId(0))),
            refs: Default::default(),
        }
    }

    pub fn link(&self, referenced: &FinalArena) {
        self.refs.borrow_mut().link(referenced)
    }

    pub fn alloc<'a>(
        &'a self,
        designator: Designator,
        parent: Option<EntRef<'a>>,
        related: Related<'a>,
        kind: AnyEntKind<'a>,
        decl_pos: Option<SrcPos>,
    ) -> EntRef<'a> {
        let ent = AnyEnt {
            id: EntityId::undefined(),
            parent,
            related,
            implicits: Vec::new(),
            designator,
            kind,
            decl_pos,
        };

        unsafe {
            let ent = self.local.borrow_mut().alloc(ent);
            &*ent as EntRef<'a>
        }
    }

    pub(crate) unsafe fn update<'a>(
        &'a self,
        id: EntityId,
        designator: Designator,
        parent: Option<EntRef<'a>>,
        related: Related<'a>,
        kind: AnyEntKind<'a>,
        decl_pos: Option<SrcPos>,
    ) -> EntRef<'a> {
        unsafe {
            let local = self.local.borrow_mut();
            assert_eq!(id.arena_id(), local.id);
            let p = &mut *self.local.as_ptr() as &mut LocalArena;
            let eref = p.get_mut(id.local_id());
            *eref = AnyEnt {
                id,
                parent,
                related,
                implicits: Vec::new(),
                designator,
                kind,
                decl_pos,
            };
            &*eref as EntRef<'a>
        }
    }

    pub(crate) unsafe fn add_implicit<'a>(&'a self, id: EntityId, ent: EntRef<'a>) {
        let local = self.local.borrow_mut();
        assert_eq!(id.arena_id(), local.id);
        let p = &mut *self.local.as_ptr() as &mut LocalArena;
        let eref = p.get_mut(id.local_id());
        unsafe {
            let eref: &mut AnyEnt = &mut *eref as &mut AnyEnt;
            eref.add_implicit(ent);
        }
    }

    pub fn get<'a>(&'a self, id: EntityId) -> EntRef<'a> {
        // Since local uses PinnedVec we do not have to worry about
        // returning a pure reference here since allocating new
        // references do not move address
        //
        // @TODO care must be taken to use get_mut though
        unsafe {
            let p = &*self.local.as_ptr() as &LocalArena;
            if p.id == id.arena_id() {
                // Try local first
                let ent = p.get(id.local_id());
                return &*ent as &'a AnyEnt;
            }
        }

        // Address of referenced entities will not move
        // When adding more referenced arenas since
        // they are behind another pointer to a read only structure
        // in DesignRoot
        unsafe {
            let p = &*self.refs.as_ptr() as &FinalArena;
            let ent = p.get(id);
            ent as &'a AnyEnt
        }
    }

    pub fn get_type(&self, id: EntityId) -> TypeEnt {
        TypeEnt::from_any(self.get(id)).unwrap()
    }

    pub fn finalize(self) -> FinalArena {
        let Arena { local, refs } = self;
        let local = local.into_inner();
        let mut refs = refs.into_inner();
        refs.refs.insert(local.id.0, Arc::new(local));
        refs
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct EntityId {
    id: usize,
}

impl EntityId {
    // Using 64-bits we can create 5 * 10**9 ids per second for 100 years before wrapping
    #[allow(clippy::new_without_default)]
    pub fn undefined() -> Self {
        EntityId { id: usize::MAX }
    }

    pub(crate) fn new_arena(arena_id: ArenaId, id: LocalId) -> Self {
        EntityId {
            id: ((arena_id.0 as usize) << u32::BITS) | (id.0 as usize),
        }
    }

    pub fn arena_id(&self) -> ArenaId {
        ArenaId((self.id >> u32::BITS) as u32)
    }

    fn local_id(&self) -> LocalId {
        LocalId((self.id & (u32::MAX as usize)) as u32)
    }
}
