// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2024, Olof Kraigher olof.kraigher@gmail.com
use crate::analysis::DesignRoot;
use crate::ast::search::{
    DeclarationItem, Finished, Found, FoundDeclaration, NotFinished, SearchState, Searcher,
};
use crate::ast::{ConcurrentStatement, MapAspect, ObjectClass};
use crate::named_entity::{AsUnique, Region};
use crate::{
    named_entity, AnyEntKind, CompletionItem, Design, EntityId, HasTokenSpan, Overloaded, Position,
    Source, TokenAccess,
};
use std::collections::HashSet;

/// Produces completions for the left hand side of a map aspect, i.e.,
/// `port map (`
pub(crate) fn completions_for_map_aspect<'a>(
    root: &'a DesignRoot,
    cursor: Position,
    source: &Source,
) -> Vec<CompletionItem<'a>> {
    let mut searcher = MapAspectSearcher::new(root, cursor);
    let _ = root.search_source(source, &mut searcher);
    searcher.completions
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
        if !map.get_span(ctx).contains(self.cursor) {
            return false;
        }
        let formals_in_map: HashSet<EntityId> = HashSet::from_iter(map.formals().flatten());
        if let Some(ent) = ent_ref {
            let ids = match kind {
                MapAspectKind::Port => extract_port_names(self.root, ent),
                MapAspectKind::Generic => extract_generic_names(self.root, ent),
            };
            self.completions.extend(
                ids.into_iter()
                    .filter(|id| !formals_in_map.contains(id))
                    .map(|id| CompletionItem::Formal(self.root.get_ent(id))),
            );
        }
        true
    }
}

impl Searcher for MapAspectSearcher<'_> {
    /// Visit an instantiation statement extracting completions for ports or generics.
    fn search_decl(&mut self, ctx: &dyn TokenAccess, decl: FoundDeclaration<'_>) -> SearchState {
        match &decl.ast {
            DeclarationItem::ConcurrentStatement(stmt) => {
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
            DeclarationItem::PackageInstance(inst) => {
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

#[derive(Eq, PartialEq, Debug)]
enum MapAspectKind {
    Port,
    Generic,
}

/// From this region, extracts those `AnyEntKind::Object`s where the class of the
/// object matches the specified class.
fn extract_objects_with_class(region: &Region<'_>, object_class: ObjectClass) -> Vec<EntityId> {
    region
        .entities
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

/// Extracts the name of ports or generics from an AST for an entity with a certain ID.
/// The entity can be an `Entity`, `Component` or `Package`.
///
/// # Arguments
///
/// * `object_class` - What to extract. `ObjectClass::Signal` extracts ports
///   while `ObjectClass::Constant` extracts constants.
fn extract_port_or_generic_names(
    root: &DesignRoot,
    id: EntityId,
    object_class: ObjectClass,
) -> Vec<EntityId> {
    let cmp_ent = root.get_ent(id);
    match cmp_ent.kind() {
        AnyEntKind::Component(region) => extract_objects_with_class(region, object_class),
        AnyEntKind::Design(Design::Entity(_, region)) => {
            extract_objects_with_class(region, object_class)
        }
        AnyEntKind::Design(Design::UninstPackage(_, region)) => {
            extract_objects_with_class(region, object_class)
        }
        _ => vec![],
    }
}

pub fn extract_port_names(root: &DesignRoot, id: EntityId) -> Vec<EntityId> {
    extract_port_or_generic_names(root, id, ObjectClass::Signal)
}

pub fn extract_generic_names(root: &DesignRoot, id: EntityId) -> Vec<EntityId> {
    extract_port_or_generic_names(root, id, ObjectClass::Constant)
}

#[cfg(test)]
mod tests {
    use crate::analysis::tests::{check_no_diagnostics, LibraryBuilder};
    use crate::{list_completion_options, CompletionItem};

    #[test]
    pub fn complete_component_instantiation_map() {
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
        assert!(options.contains(&CompletionItem::Simple(clk_signal)));
        assert!(options.contains(&CompletionItem::Simple(rst_signal)));

        let cursor = code
            .s1("port map (
            clk => c")
            .pos()
            .end();
        let options = list_completion_options(&root, code.source(), cursor);
        assert!(options.contains(&CompletionItem::Simple(clk_signal)));
        assert!(options.contains(&CompletionItem::Simple(rst_signal)));
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
        pretty_assertions::assert_eq!(options.len(), 3);
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

        assert!(options.contains(&CompletionItem::Simple(ent1)));
        assert!(options.contains(&CompletionItem::Simple(ent2)));
    }
}
