// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

mod assignment_typecheck;
mod association_formal;
mod circular_dependencies;
mod context_clause;
mod deferred_constant;
mod hierarchy;
mod homographs;
mod implicit;
mod incomplete_type;
mod incremental_analysis;
mod package_instance;
mod protected_type;
mod resolves_design_units;
mod resolves_names;
mod resolves_type_mark;
mod sensitivity_list;
mod subprogram_arguments;
mod typecheck_expression;
mod util;
mod visibility;

use std::cell::RefCell;

pub use self::util::*;
use crate::ast::Designator;
use crate::ast::UnitId;
pub use crate::data::Diagnostic;
use crate::data::NoDiagnostics;
pub use crate::syntax::test::*;

use super::analyze::AnalyzeContext;
use super::named_entity::*;
use super::region::*;
use super::DesignRoot;
use super::EntRef;

pub(super) struct TestSetup<'a> {
    builder: RefCell<LibraryBuilder>,
    root: DesignRoot,
    arena: Arena,
    pub scope: Scope<'a>,
}

impl<'a> TestSetup<'a> {
    pub fn new() -> Self {
        let builder = LibraryBuilder::new();
        let (mut root, _) = builder.get_analyzed_root();
        root.ensure_library(root.symbol_utf8("libname"));
        let arena = Arena::new(ArenaId::default());

        Self {
            arena,
            root,
            builder: RefCell::new(builder),
            scope: Scope::new(Region::default()),
        }
    }

    pub fn ctx(&'a self) -> AnalyzeContext<'a> {
        let ctx = AnalyzeContext::new(
            &self.root,
            &UnitId::package(
                &self.root.symbol_utf8("libname"),
                &self.root.symbol_utf8("dummy"),
            ),
            &self.arena,
        );
        ctx.add_implicit_context_clause(&self.scope).unwrap();
        ctx
    }

    pub fn snippet(&self, code: &str) -> Code {
        self.builder.borrow_mut().snippet(code)
    }

    pub fn declarative_part(&'a self, code: &str) -> Code {
        let code = self.snippet(code);
        let dummy_parent = self.arena.alloc(
            Designator::Anonymous(0),
            None,
            Related::None,
            AnyEntKind::Library,
            None,
        );
        self.ctx()
            .analyze_declarative_part(
                &self.scope,
                dummy_parent,
                code.declarative_part().as_mut(),
                &mut NoDiagnostics,
            )
            .unwrap();
        code
    }

    pub fn lookup(&'a self, sym: &str) -> EntRef<'a> {
        // We cheat and create a source pos as the lookup method requires it
        let designator = self.snippet(sym).designator();

        self.scope
            .lookup(&designator.pos, &designator.item)
            .unwrap()
            .into_non_overloaded()
            .unwrap()
    }

    pub fn lookup_overloaded(&'a self, code: Code) -> OverloadedEnt<'a> {
        let des = code.designator();

        if let NamedEntities::Overloaded(overloaded) =
            self.scope.lookup(&des.pos, &des.item).unwrap()
        {
            overloaded
                .entities()
                .find(|ent| ent.decl_pos() == Some(&code.pos()))
                .unwrap()
        } else {
            panic!("Expected overloaded name");
        }
    }

    pub fn lookup_type(&'a self, sym: &str) -> TypeEnt<'a> {
        TypeEnt::from_any(self.lookup(sym)).unwrap()
    }
}
