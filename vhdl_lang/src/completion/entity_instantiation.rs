// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2024, Olof Kraigher olof.kraigher@gmail.com
use crate::analysis::DesignRoot;
use crate::named_entity::DesignEnt;
use crate::{AnyEntKind, CompletionItem, Design, EntRef, EntityId, HasEntityId};
use itertools::Itertools;
use std::collections::HashSet;

/// List all entities (entities in this context is a VHDL entity, not a `DesignEnt` or similar)
/// that are visible from another VHDL entity.
pub(crate) fn get_visible_entities_from_entity<'a>(
    root: &'a DesignRoot,
    ent: &DesignEnt<'a>,
) -> Vec<CompletionItem<'a>> {
    let mut entities: HashSet<EntityId> = HashSet::new();
    if let Design::Architecture(vis, _, ent_of_arch) = ent.kind() {
        for ent_ref in vis.visible() {
            match ent_ref.kind() {
                AnyEntKind::Design(Design::Entity(..)) => {
                    entities.insert(ent_ref.id());
                }
                AnyEntKind::Library => {
                    let Some(name) = ent_ref.library_name() else {
                        continue;
                    };
                    let Some(lib) = root.get_lib(name) else {
                        continue;
                    };
                    let itr = lib
                        .units()
                        .flat_map(|locked_unit| locked_unit.unit.get())
                        .filter(|design_unit| design_unit.is_entity())
                        .flat_map(|design_unit| design_unit.ent_id())
                        .filter(|id| id != &ent_of_arch.id()); // Remove the entity that belongs to this architecture
                    entities.extend(itr);
                }
                _ => {}
            }
        }
    }
    entities
        .into_iter()
        .map(|eid| root.get_ent(eid))
        .map(|ent| match ent.kind() {
            AnyEntKind::Design(Design::Entity(..)) => {
                let architectures = get_architectures_for_entity(ent, root);
                CompletionItem::EntityInstantiation(ent, architectures)
            }
            _ => CompletionItem::Simple(ent),
        })
        .collect_vec()
}

/// Returns a vec populated with all architectures that belong to a given entity
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

#[cfg(test)]
mod tests {
    use crate::analysis::tests::{assert_eq_unordered, check_no_diagnostics, LibraryBuilder};
    use crate::{list_completion_options, CompletionItem};
    use itertools::Itertools;

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

        assert!(options.contains(&CompletionItem::EntityInstantiation(my_ent, vec![])));
        assert!(options.contains(&CompletionItem::EntityInstantiation(my_other_ent, vec![])));
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

        assert!(options.contains(&CompletionItem::EntityInstantiation(my_ent2, vec![])));

        let ent1 = root
            .search_reference(code1.source(), code1.s1("my_ent").start())
            .unwrap();

        let cursor = code3.s1("begin").end();
        let options = list_completion_options(&root, code3.source(), cursor);

        let my_ent2 = root
            .search_reference(code3.source(), code3.s1("my_ent2").start())
            .unwrap();

        assert!(options.contains(&CompletionItem::EntityInstantiation(my_ent2, vec![])));
        assert!(options.contains(&CompletionItem::EntityInstantiation(ent1, vec![])));
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

        let applicable_options = options
            .into_iter()
            .filter_map(|option| match option {
                CompletionItem::EntityInstantiation(ent, architectures) => {
                    Some((ent, architectures))
                }
                _ => None,
            })
            .collect_vec();
        println!("{:?}", applicable_options);
        match &applicable_options[..] {
            [(got_ent, architectures)] => {
                pretty_assertions::assert_eq!(*got_ent, ent);
                assert_eq_unordered(architectures, &[arch1, arch2]);
            }
            _ => panic!("Expected entity instantiation"),
        }
    }
}
