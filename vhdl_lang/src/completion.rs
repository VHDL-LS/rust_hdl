// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use crate::analysis::DesignRoot;
use crate::ast::search::{Found, FoundDeclaration, NotFinished, NotFound, SearchState, Searcher};
use crate::ast::{ConcurrentStatement, Designator, MapAspect, ObjectClass};
use crate::data::{ContentReader, Symbol};
use crate::named_entity::{self, AsUnique, DesignEnt, HasEntityId, NamedEntities, Region};
use crate::syntax::Kind::*;
use crate::syntax::{Kind, Symbols, Token, TokenAccess, Tokenizer};
use crate::{AnyEntKind, Design, EntRef, EntityId, HasTokenSpan, Overloaded, Position, Source};
use std::collections::HashSet;
use std::iter::once;
use vhdl_lang::ast::search::Finished;

#[derive(Debug, PartialEq, Clone)]
pub enum CompletionItem<'a> {
    /// Simply complete the entities
    /// e.g., `use std.` should simply list all elements in the std library
    Simple(EntRef<'a>),
    /// Formal parameter, e.g., in a port map
    /// `port map (` might choose to complete `<item> => $1`
    Formal(EntRef<'a>),
    /// Multiple overloaded items are applicable.
    /// The argument is the count of overloaded items in total.
    Overloaded(Designator, usize),
    /// Complete a keyword
    Keyword(Kind),
    /// Complete the 'work' library.
    /// This is handled in a special manner because the
    /// actual work library (using [CompletionItem::Simple] would complete the actual name
    /// of the library, not the string 'work'.
    Work,
    /// Entity instantiation, i.e.,
    /// ```vhdl
    /// my_ent: entity work.foo
    ///     generic map (
    ///         -- ...
    ///     )
    ///     port map (
    ///         -- ...
    ///     );
    /// ```
    ///
    /// The second argument is a vector of architectures that are associated
    /// to this entity
    EntityInstantiation(EntRef<'a>, Vec<EntRef<'a>>),
}

macro_rules! kind {
    ($kind: pat) => {
        Token { kind: $kind, .. }
    };
}

#[derive(Eq, PartialEq, Debug)]
enum MapAspectKind {
    Port,
    Generic,
}

impl<'a> Region<'a> {
    /// From this region, extracts those `AnyEntKind::Object`s where the class of the
    /// object matches the specified class.
    fn extract_objects_with_class(&self, object_class: ObjectClass) -> Vec<EntityId> {
        self.entities
            .values()
            .filter_map(|ent| ent.as_unique())
            .filter_map(|ent| match &ent.kind {
                AnyEntKind::Object(obj) if obj.class == object_class => Some(ent.id),
                AnyEntKind::Overloaded(Overloaded::InterfaceSubprogram(_))
                    if object_class == ObjectClass::Constant =>
                {
                    Some(ent.id)
                }
                AnyEntKind::Type(named_entity::Type::Interface)
                    if object_class == ObjectClass::Constant =>
                {
                    Some(ent.id)
                }
                _ => None,
            })
            .collect()
    }
}

impl DesignRoot {
    /// Extracts the name of ports or generics from an AST for an entity with a certain ID.
    /// The entity can be an `Entity`, `Component` or `Package`.
    ///
    /// # Arguments
    ///
    /// * `object_class` - What to extract. `ObjectClass::Signal` extracts ports
    /// while `ObjectClass::Constant` extracts constants.
    fn extract_port_or_generic_names(
        &self,
        id: EntityId,
        object_class: ObjectClass,
    ) -> Vec<EntityId> {
        let cmp_ent = self.get_ent(id);
        match cmp_ent.kind() {
            AnyEntKind::Component(region) => region.extract_objects_with_class(object_class),
            AnyEntKind::Design(Design::Entity(_, region)) => {
                region.extract_objects_with_class(object_class)
            }
            AnyEntKind::Design(Design::UninstPackage(_, region)) => {
                region.extract_objects_with_class(object_class)
            }
            _ => vec![],
        }
    }

    pub fn extract_port_names(&self, id: EntityId) -> Vec<EntityId> {
        self.extract_port_or_generic_names(id, ObjectClass::Signal)
    }

    pub fn extract_generic_names(&self, id: EntityId) -> Vec<EntityId> {
        self.extract_port_or_generic_names(id, ObjectClass::Constant)
    }
}

/// Searches completions for map aspects (VHDL port maps and generic maps).
/// Currently, this only means the formal part (i.e., the left hand side of a port or generic assignment)
/// but not the actual part.
struct MapAspectSearcher<'a> {
    root: &'a DesignRoot,
    cursor: Position,
    completions: Vec<CompletionItem<'a>>,
}

impl<'a> MapAspectSearcher<'a> {
    pub fn new(root: &'a DesignRoot, cursor: Position) -> MapAspectSearcher<'a> {
        MapAspectSearcher {
            root,
            cursor,
            completions: Vec::new(),
        }
    }

    /// Loads completion options for the given map aspect.
    /// Returns `true`, when the cursor is inside the map aspect and the search should not continue.
    /// Returns `false` otherwise
    fn load_completions_for_map_aspect(
        &mut self,
        ent_ref: Option<EntityId>,
        map: &MapAspect,
        ctx: &dyn TokenAccess,
        kind: MapAspectKind,
    ) -> bool {
        if !map.span(ctx).contains(self.cursor) {
            return false;
        }
        let formals_in_map: HashSet<EntityId> = HashSet::from_iter(map.formals().flatten());
        if let Some(ent) = ent_ref {
            let ids = match kind {
                MapAspectKind::Port => self.root.extract_port_names(ent),
                MapAspectKind::Generic => self.root.extract_generic_names(ent),
            };
            self.completions.extend(
                ids.iter()
                    .filter(|id| !formals_in_map.contains(id))
                    .map(|id| CompletionItem::Formal(self.root.get_ent(*id))),
            );
        }
        true
    }
}

impl<'a> Searcher for MapAspectSearcher<'a> {
    /// Visit an instantiation statement extracting completions for ports or generics.
    fn search_decl(&mut self, ctx: &dyn TokenAccess, decl: FoundDeclaration) -> SearchState {
        match decl {
            FoundDeclaration::ConcurrentStatement(stmt) => {
                if let ConcurrentStatement::Instance(inst) = &stmt.statement.item {
                    if let Some(map) = &inst.generic_map {
                        if self.load_completions_for_map_aspect(
                            inst.entity_reference(),
                            map,
                            ctx,
                            MapAspectKind::Generic,
                        ) {
                            return Finished(Found);
                        }
                    }
                    if let Some(map) = &inst.port_map {
                        if self.load_completions_for_map_aspect(
                            inst.entity_reference(),
                            map,
                            ctx,
                            MapAspectKind::Port,
                        ) {
                            return Finished(Found);
                        }
                    }
                }
            }
            FoundDeclaration::PackageInstance(inst) => {
                if let Some(map) = &inst.generic_map {
                    if self.load_completions_for_map_aspect(
                        inst.package_name.item.get_suffix_reference(),
                        map,
                        ctx,
                        MapAspectKind::Generic,
                    ) {
                        return Finished(Found);
                    }
                }
            }
            _ => {}
        }
        NotFinished
    }
}

/// Tokenizes `source` up to `cursor` but no further. The last token returned is the token
/// where the cursor currently resides or the token right before the cursor.
///
/// Examples:
///
/// input = "use ieee.std_logic_1|164.a"
///                              ^ cursor position
/// `tokenize_input(input)` -> {USE, ieee, DOT, std_logic_1164}
///
/// input = "use ieee.std_logic_1164|.a"
///                                 ^ cursor position
/// `tokenize_input(input)` -> {USE, ieee, DOT, std_logic_1164}
///
/// input = "use ieee.std_logic_1164.|a"
///                                  ^ cursor position
/// `tokenize_input(input)` -> {USE, ieee, DOT, std_logic_1164, DOT}
/// input = "use ieee.std_logic_1164.a|"
///                                   ^ cursor position
/// `tokenize_input(input)` -> {USE, ieee, DOT, std_logic_1164, DOT, a}
///
/// On error, or if the source is empty, returns an empty vector.
fn tokenize_input(symbols: &Symbols, source: &Source, cursor: Position) -> Vec<Token> {
    let contents = source.contents();
    let mut tokenizer = Tokenizer::new(symbols, source, ContentReader::new(&contents));
    let mut tokens = Vec::new();
    loop {
        match tokenizer.pop() {
            Ok(Some(token)) => {
                if token.pos.start() >= cursor {
                    break;
                }
                tokens.push(token);
            }
            Ok(None) => break,
            Err(_) => return vec![],
        }
    }
    tokens
}

/// helper function to list the name of all available libraries
fn list_all_libraries(root: &DesignRoot) -> Vec<CompletionItem> {
    root.libraries()
        .map(|lib| CompletionItem::Simple(root.get_ent(lib.id())))
        .chain(once(CompletionItem::Work))
        .collect()
}

/// List the name of all primary units for a given library.
/// If the library is non-resolvable, list an empty vector
fn list_primaries_for_lib<'a>(root: &'a DesignRoot, lib: &Symbol) -> Vec<CompletionItem<'a>> {
    let Some(lib) = root.get_lib(lib) else {
        return vec![];
    };
    lib.primary_units()
        .filter_map(|it| it.unit.get().and_then(|unit| unit.ent_id()))
        .map(|id| CompletionItem::Simple(root.get_ent(id)))
        .collect()
}

/// General-purpose Completion Searcher
/// when no more accurate searcher is available.
struct CompletionSearcher<'a> {
    root: &'a DesignRoot,
    cursor: Position,
    completions: Vec<CompletionItem<'a>>,
}

impl<'a> CompletionSearcher<'a> {
    pub fn new(cursor: Position, design_root: &'a DesignRoot) -> CompletionSearcher<'a> {
        CompletionSearcher {
            root: design_root,
            cursor,
            completions: Vec::new(),
        }
    }
}

impl<'a> Searcher for CompletionSearcher<'a> {
    fn search_decl(&mut self, ctx: &dyn TokenAccess, decl: FoundDeclaration) -> SearchState {
        match decl {
            FoundDeclaration::Architecture(body) => {
                // ensure we are in the concurrent region
                if !ctx
                    .get_span(body.begin_token, body.get_end_token())
                    .contains(self.cursor)
                {
                    return Finished(NotFound);
                }
                // Add architecture declarations to the list of completed names
                self.completions.extend(
                    body.decl
                        .iter()
                        .filter_map(|decl| decl.ent_id())
                        .map(|eid| self.root.get_ent(eid))
                        .filter(|ent| {
                            matches!(
                                ent.kind(),
                                AnyEntKind::Object(_) | AnyEntKind::ObjectAlias { .. }
                            )
                        })
                        .map(CompletionItem::Simple),
                );

                let Some(eid) = body.entity_name.reference.get() else {
                    return Finished(NotFound);
                };
                let Some(ent) = DesignEnt::from_any(self.root.get_ent(eid)) else {
                    return Finished(NotFound);
                };
                // Add ports and generics to the list of completed items
                if let Design::Entity(_, region) = ent.kind() {
                    self.completions
                        .extend(region.entities.values().filter_map(|ent| {
                            if let NamedEntities::Single(ent) = ent {
                                Some(CompletionItem::Simple(ent))
                            } else {
                                None
                            }
                        }))
                }
                // Early-exit for when we are inside a statement.
                for statement in &body.statements {
                    let pos = &statement.statement.pos(ctx);

                    // Early exit. The cursor is below the current statement.
                    if pos.start() > self.cursor {
                        break;
                    }

                    if pos.contains(self.cursor) {
                        return Finished(NotFound);
                    }
                }
                self.completions.extend(
                    self.root
                        .get_visible_entities_from_entity(ent)
                        .map(|eid| entity_to_completion_item(self.root, eid)),
                );
                Finished(Found)
            }
            _ => NotFinished,
        }
    }
}

fn entity_to_completion_item(root: &DesignRoot, eid: EntityId) -> CompletionItem {
    let ent = root.get_ent(eid);
    match ent.kind() {
        AnyEntKind::Design(Design::Entity(..)) => {
            let architectures = get_architectures_for_entity(ent, root);
            CompletionItem::EntityInstantiation(ent, architectures)
        }
        _ => CompletionItem::Simple(ent),
    }
}

/// Returns a vec populated with all architectures that belong to the given entity
fn get_architectures_for_entity<'a>(ent: EntRef<'a>, root: &'a DesignRoot) -> Vec<EntRef<'a>> {
    let Some(lib_symbol) = ent.library_name() else {
        return vec![];
    };
    let Some(lib) = root.get_lib(lib_symbol) else {
        return vec![];
    };
    let Some(sym) = ent.designator().as_identifier() else {
        return vec![];
    };
    lib.secondary_units(sym)
        .filter_map(|locked_unit| locked_unit.unit.get())
        .filter_map(|read_guard| read_guard.ent_id())
        .map(|eid| root.get_ent(eid))
        .collect()
}

impl DesignRoot {
    /// List all entities (entities in this context is a VHDL entity, not a `DesignEnt` or similar)
    /// that are visible from another VHDL entity.
    /// This could, at some point, be further generalized into a generic
    /// 'list visible entities of some kind of some generic design entity'.
    pub fn get_visible_entities_from_entity(
        &self,
        ent: DesignEnt,
    ) -> impl Iterator<Item = EntityId> {
        let mut entities: HashSet<EntityId> = HashSet::new();
        if let Design::Entity(vis, _) = ent.kind() {
            for ent_ref in vis.visible() {
                match ent_ref.kind() {
                    AnyEntKind::Design(Design::Entity(..)) => {
                        entities.insert(ent_ref.id());
                    }
                    AnyEntKind::Library => {
                        let Some(name) = ent_ref.library_name() else {
                            continue;
                        };
                        let Some(lib) = self.get_lib(name) else {
                            continue;
                        };
                        let itr = lib
                            .units()
                            .flat_map(|locked_unit| locked_unit.unit.get())
                            .map(|read_guard| read_guard.to_owned())
                            .filter(|design_unit| design_unit.is_entity())
                            .flat_map(|design_unit| design_unit.ent_id());
                        entities.extend(itr);
                    }
                    _ => {}
                }
            }
        }
        // Remove entity that this was called from.
        // Recursive instantiation is only possible with component instantiation.
        entities.remove(&ent.id());
        entities.into_iter()
    }
}

pub fn completions_for_type<'a>(typ: &named_entity::Type<'a>) -> Vec<CompletionItem<'a>> {
    use named_entity::Type::*;
    match typ {
        Record(record_region) => record_region
            .iter()
            .map(|item| CompletionItem::Simple(item.ent))
            .collect(),
        _ => vec![],
    }
}

pub fn completions_for_design<'a>(design: &Design<'a>) -> Vec<CompletionItem<'a>> {
    use Design::*;
    match design {
        Package(_, region) => region
            .entities
            .values()
            .map(|item| match item {
                NamedEntities::Single(single) => CompletionItem::Simple(single),
                NamedEntities::Overloaded(overloaded) => match overloaded.as_unique() {
                    None => CompletionItem::Overloaded(
                        overloaded.designator().clone(),
                        overloaded.len(),
                    ),
                    Some(ent) => CompletionItem::Simple(ent),
                },
            })
            .chain(once(CompletionItem::Keyword(All)))
            .collect(),
        _ => vec![],
    }
}

pub fn completions_after_dot<'b>(root: &'b DesignRoot, ent: EntRef<'b>) -> Vec<CompletionItem<'b>> {
    use AnyEntKind::*;
    match ent.kind() {
        Object(object) => completions_for_type(object.subtype.type_mark().kind()),
        Type(typ) => completions_for_type(typ),
        Design(design) => completions_for_design(design),
        Library => ent
            .library_name()
            .map(|sym| list_primaries_for_lib(root, sym))
            .unwrap_or_default(),
        _ => vec![],
    }
}

/// Main entry point for completion. Given a source-file and a cursor position,
/// lists available completion options at the cursor position.
pub fn list_completion_options<'a>(
    root: &'a DesignRoot,
    source: &Source,
    cursor: Position,
) -> Vec<CompletionItem<'a>> {
    let tokens = tokenize_input(root.symbols(), source, cursor);
    match &tokens[..] {
        [.., kind!(Library)] | [.., kind!(Use)] | [.., kind!(Use), kind!(Identifier)] => {
            list_all_libraries(root)
        }
        [.., token, kind!(Dot)] | [.., token, kind!(Dot), kind!(Identifier)] => {
            // get the entity before the token.
            // We rely on the syntax parsing to be resilient enough for this to yield a reasonable value.
            // Otherwise, we just return an empty value.
            if let Some((_, ent)) = root.item_at_cursor(source, token.pos.start()) {
                completions_after_dot(root, ent)
            } else {
                vec![]
            }
        }
        [.., kind!(LeftPar | Comma)] | [.., kind!(LeftPar | Comma), kind!(Identifier)] => {
            let mut searcher = MapAspectSearcher::new(root, cursor);
            let _ = root.search_source(source, &mut searcher);
            searcher.completions
        }
        _ => {
            let mut searcher = CompletionSearcher::new(cursor, root);
            let _ = root.search_source(source, &mut searcher);
            searcher.completions
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::analysis::tests::{check_no_diagnostics, LibraryBuilder};
    use crate::completion::tokenize_input;
    use crate::syntax::test::{assert_eq_unordered, Code};
    use assert_matches::assert_matches;

    #[test]
    fn tokenizing_an_empty_input() {
        let input = Code::new("");
        let tokens = tokenize_input(&input.symbols, input.source(), Position::new(0, 0));
        assert_eq!(tokens.len(), 0);
    }

    #[test]
    fn tokenizing_stops_at_the_cursors_position() {
        let input = Code::new("use ieee.std_logic_1164.all");
        let mut cursor = input.s1("std_logic_11").pos().end();
        let tokens = tokenize_input(&input.symbols, input.source(), cursor);
        assert_matches!(
            tokens[..],
            [kind!(Use), kind!(Identifier), kind!(Dot), kind!(Identifier)]
        );
        cursor = input.s1("std_logic_1164").pos().end();
        let tokens = tokenize_input(&input.symbols, input.source(), cursor);
        assert_matches!(
            tokens[..],
            [kind!(Use), kind!(Identifier), kind!(Dot), kind!(Identifier)]
        );
        cursor = input.s1("std_logic_1164.").pos().end();
        let tokens = tokenize_input(&input.symbols, input.source(), cursor);
        assert_matches!(
            tokens[..],
            [
                kind!(Use),
                kind!(Identifier),
                kind!(Dot),
                kind!(Identifier),
                kind!(Dot)
            ]
        );
        cursor = input.s1("std_logic_1164.all").pos().end();
        let tokens = tokenize_input(&input.symbols, input.source(), cursor);
        assert_matches!(
            tokens[..],
            [
                kind!(Use),
                kind!(Identifier),
                kind!(Dot),
                kind!(Identifier),
                kind!(Dot),
                kind!(All)
            ]
        );
    }

    #[test]
    pub fn completing_libraries() {
        let input = LibraryBuilder::new();
        let code = Code::new("library ");
        let (root, _) = input.get_analyzed_root();
        let cursor = code.s1("library ").pos().end();
        let options = list_completion_options(&root, code.source(), cursor);
        assert_eq!(options, list_all_libraries(&root))
    }

    #[test]
    pub fn completing_primaries() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "\
use std.

-- Need this package so that the 'use std.' is associated and can be analyzed correctly
package x is
end package x;
",
        );
        let (root, _) = builder.get_analyzed_root();
        let cursor = code.s1("use std.").end();
        let options = list_completion_options(&root, code.source(), cursor);
        assert_eq_unordered(
            &options,
            &[
                CompletionItem::Simple(root.find_textio_pkg()),
                CompletionItem::Simple(root.find_standard_pkg()),
                CompletionItem::Simple(root.find_env_pkg()),
            ],
        );

        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "\
use std.t

-- Need this package so that the 'use std.' is associated and can be analyzed correctly
package x is
end package x;
",
        );
        let (root, _) = builder.get_analyzed_root();
        let cursor = code.s1("use std.t").end();
        let options = list_completion_options(&root, code.source(), cursor);
        // Note that the filtering only happens at client side
        assert_eq_unordered(
            &options,
            &[
                CompletionItem::Simple(root.find_textio_pkg()),
                CompletionItem::Simple(root.find_standard_pkg()),
                CompletionItem::Simple(root.find_env_pkg()),
            ],
        );
    }

    #[test]
    pub fn completing_declarations() {
        let mut input = LibraryBuilder::new();
        let code = input.code(
            "libname",
            "\
use std.env.

-- Need this package sp that the 'use std.' is associated and can be analyzed correctly
package x is
end package x;
",
        );
        let (root, _) = input.get_analyzed_root();
        let cursor = code.s1("use std.env.").end();
        let options = list_completion_options(&root, code.source(), cursor);

        assert_eq_unordered(
            &options,
            &[
                CompletionItem::Overloaded(Designator::Identifier(root.symbol_utf8("stop")), 2),
                CompletionItem::Overloaded(Designator::Identifier(root.symbol_utf8("finish")), 2),
                CompletionItem::Simple(root.find_env_symbol("resolution_limit")),
                CompletionItem::Keyword(All),
            ],
        );
    }

    #[test]
    pub fn completing_instantiation_statement() {
        let mut input = LibraryBuilder::new();
        let code = input.code(
            "libname",
            "\
    entity my_ent is
    end entity my_ent;

    architecture arch of my_ent is
        component comp is
        generic (
          A: natural := 5;
          B: integer
        );
        port (
          clk : in bit;
          rst : in bit;
          dout : out bit
        );
        end component comp;
        signal clk, rst: bit;
    begin
        comp_inst: comp
        generic map (
            A => 2
        )
        port map (
            clk => clk
        );
    end arch;
            ",
        );
        let (root, _) = input.get_analyzed_root();
        let cursor = code.s1("generic map (").pos().end();
        let options = list_completion_options(&root, code.source(), cursor);
        let ent = root
            .search_reference(code.source(), code.s1("B").start())
            .unwrap();
        assert_eq!(options, vec![CompletionItem::Formal(ent)]);

        let rst = root
            .search_reference(code.source(), code.s1("rst").start())
            .unwrap();

        let dout = root
            .search_reference(code.source(), code.s1("dout").start())
            .unwrap();

        let clk_signal = root
            .search_reference(
                code.source(),
                code.s1("signal clk, rst: bit;").s1("clk").start(),
            )
            .unwrap();

        let rst_signal = root
            .search_reference(
                code.source(),
                code.s1("signal clk, rst: bit;").s1("rst").start(),
            )
            .unwrap();

        let cursor = code.s1("port map (").pos().end();
        let options = list_completion_options(&root, code.source(), cursor);
        assert!(options.contains(&CompletionItem::Formal(rst)));
        assert!(options.contains(&CompletionItem::Formal(dout)));
        assert_eq!(options.len(), 2);
        let cursor = code
            .s1("port map (
            clk =>")
            .pos()
            .end();
        let options = list_completion_options(&root, code.source(), cursor);
        assert_eq_unordered(
            &options,
            &[
                CompletionItem::Simple(clk_signal),
                CompletionItem::Simple(rst_signal),
            ],
        );
        let cursor = code
            .s1("port map (
            clk => c")
            .pos()
            .end();
        let options = list_completion_options(&root, code.source(), cursor);
        assert_eq_unordered(
            &options,
            &[
                CompletionItem::Simple(clk_signal),
                CompletionItem::Simple(rst_signal),
            ],
        );
    }

    #[test]
    pub fn complete_in_generic_map() {
        let mut input = LibraryBuilder::new();
        let code = input.code(
            "libname",
            "\
    package my_pkg is
    generic (
        function foo(x: Integer) return bit;
        function bar(y: Integer) return boolean;
        type T;
        x: natural
    );
    end my_pkg;

    use work.my_pkg.all ; 
    package my_pkg_inst is new work.my_pkg
    generic map (
         foo => foo
    );",
        );
        let (root, _) = input.get_analyzed_root();
        let bar_func = root
            .search_reference(code.source(), code.s1("bar").start())
            .unwrap();
        let x = root
            .search_reference(code.source(), code.s1("x: natural").s1("x").start())
            .unwrap();
        let t = root
            .search_reference(code.source(), code.s1("type T").s1("T").start())
            .unwrap();
        let cursor = code.s1("generic map (").pos().end();
        let options = list_completion_options(&root, code.source(), cursor);
        assert!(options.contains(&CompletionItem::Formal(bar_func)));
        assert!(options.contains(&CompletionItem::Formal(x)));
        assert!(options.contains(&CompletionItem::Formal(t)));
        assert_eq!(options.len(), 3);
    }

    #[test]
    fn complete_entities() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "\
entity my_ent is
end my_ent;

entity my_other_ent is
end my_other_ent;

entity my_third_ent is
end my_third_ent;

architecture arch of my_third_ent is
begin
end arch;
        ",
        );

        let (root, _) = builder.get_analyzed_root();
        let cursor = code.s1("begin").end();
        let options = list_completion_options(&root, code.source(), cursor);

        let my_ent = root
            .search_reference(code.source(), code.s1("my_ent").start())
            .unwrap();

        let my_other_ent = root
            .search_reference(code.source(), code.s1("my_other_ent").start())
            .unwrap();

        assert_eq_unordered(
            &options[..],
            &[
                CompletionItem::EntityInstantiation(my_ent, vec![]),
                CompletionItem::EntityInstantiation(my_other_ent, vec![]),
            ],
        );
    }

    #[test]
    fn complete_entities_from_different_libraries() {
        let mut builder = LibraryBuilder::new();
        let code1 = builder.code(
            "libA",
            "\
entity my_ent is
end my_ent;
        ",
        );

        let code2 = builder.code(
            "libB",
            "\
entity my_ent2 is
end my_ent2;

entity my_ent3 is
end my_ent3;

architecture arch of my_ent3 is
begin

end arch;

        ",
        );

        let code3 = builder.code(
            "libC",
            "\
entity my_ent2 is
end my_ent2;

library libA;

entity my_ent3 is
end my_ent3;

architecture arch of my_ent3 is
begin

end arch;
        ",
        );

        let (root, diag) = builder.get_analyzed_root();
        check_no_diagnostics(&diag[..]);
        let cursor = code2.s1("begin").end();
        let options = list_completion_options(&root, code2.source(), cursor);

        let my_ent2 = root
            .search_reference(code2.source(), code2.s1("my_ent2").start())
            .unwrap();

        assert_eq_unordered(
            &options[..],
            &[CompletionItem::EntityInstantiation(my_ent2, vec![])],
        );

        let ent1 = root
            .search_reference(code1.source(), code1.s1("my_ent").start())
            .unwrap();

        let cursor = code3.s1("begin").end();
        let options = list_completion_options(&root, code3.source(), cursor);

        let my_ent2 = root
            .search_reference(code3.source(), code3.s1("my_ent2").start())
            .unwrap();

        assert_eq_unordered(
            &options[..],
            &[
                CompletionItem::EntityInstantiation(my_ent2, vec![]),
                CompletionItem::EntityInstantiation(ent1, vec![]),
            ],
        );
    }

    #[test]
    pub fn entity_with_two_architecture() {
        let mut builder = LibraryBuilder::new();
        let code1 = builder.code(
            "libA",
            "\
entity my_ent is
end my_ent;

architecture arch1 of my_ent is
begin
end arch1;

architecture arch2 of my_ent is
begin
end arch2;
        ",
        );
        let code2 = builder.code(
            "libA",
            "\
entity my_ent2 is
end my_ent2;

architecture arch of my_ent2 is
begin

end arch;
        ",
        );

        let (root, diag) = builder.get_analyzed_root();
        check_no_diagnostics(&diag[..]);
        let cursor = code2.s("begin", 1).end();
        let options = list_completion_options(&root, code2.source(), cursor);

        let ent = root
            .search_reference(code1.source(), code1.s1("my_ent").start())
            .unwrap();

        let arch1 = root
            .search_reference(code1.source(), code1.s1("arch1").start())
            .unwrap();

        let arch2 = root
            .search_reference(code1.source(), code1.s1("arch2").start())
            .unwrap();

        match &options[..] {
            [CompletionItem::EntityInstantiation(got_ent, architectures)] => {
                assert_eq!(*got_ent, ent);
                assert_eq_unordered(architectures, &[arch1, arch2]);
            }
            _ => panic!("Expected entity instantiation"),
        }
    }

    #[test]
    pub fn completes_signals_and_ports() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libA",
            "\
entity my_ent is
    port (
        foo : in bit
    );
end my_ent;

architecture arch of my_ent is
    signal bar : natural;
    type foobaz is array(natural range <>) of bit;
begin
end arch;
        ",
        );

        let (root, diag) = builder.get_analyzed_root();
        check_no_diagnostics(&diag);
        let cursor = code.s1("begin").end();
        let options = list_completion_options(&root, code.source(), cursor);

        let ent1 = root
            .search_reference(code.source(), code.s1("foo").start())
            .unwrap();

        let ent2 = root
            .search_reference(code.source(), code.s1("bar").start())
            .unwrap();

        assert_eq_unordered(
            &options,
            &[CompletionItem::Simple(ent1), CompletionItem::Simple(ent2)],
        )
    }
}
