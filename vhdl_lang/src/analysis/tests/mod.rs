// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

mod assignment_typecheck;
mod association_formal;
mod circular_dependencies;
mod context_clause;
mod custom_attributes;
mod declarations;
mod deferred_constant;
mod discrete_ranges;
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
mod subprogram_instance;
mod tool_directive;
mod typecheck_expression;
mod util;
mod view_declarations;
mod visibility;

use std::cell::RefCell;
use std::path::PathBuf;
use vhdl_lang::TokenSpan;

pub use self::util::*;
use crate::ast::Designator;
use crate::ast::UnitId;
pub use crate::data::Diagnostic;
use crate::data::NoDiagnostics;
pub use crate::syntax::test::*;
use crate::syntax::Token;

use super::analyze::AnalyzeContext;
use super::scope::*;
use super::DesignRoot;
use crate::named_entity::*;
use crate::Source;

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

    #[allow(clippy::ptr_arg)]
    pub fn ctx<'t>(&'a self, tokens: &'t Vec<Token>) -> AnalyzeContext<'a, 't> {
        let ctx = AnalyzeContext::new(
            &self.root,
            &UnitId::package(
                &self.root.symbol_utf8("libname"),
                &self.root.symbol_utf8("dummy"),
            ),
            Source::inline(&PathBuf::new(), ""),
            &self.arena,
            tokens,
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
            TokenSpan::for_library(),
            Some(code.source().clone()),
        );
        self.ctx(&code.tokenize())
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
            .lookup(&designator.item)
            .unwrap()
            .into_non_overloaded()
            .unwrap()
    }

    pub fn lookup_overloaded(&'a self, code: Code) -> OverloadedEnt<'a> {
        let des = code.designator();

        if let NamedEntities::Overloaded(overloaded) = self.scope.lookup(&des.item).unwrap() {
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
