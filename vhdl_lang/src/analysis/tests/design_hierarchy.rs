// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.

use super::*;
use crate::hierarchy::{
    compute_design_hierarchy, list_top_candidates, DesignHierarchyNode, HierarchyError,
    HierarchyKind,
};
use pretty_assertions::assert_eq;

fn build(libname: &str, code: &str) -> DesignRoot {
    let mut builder = LibraryBuilder::new();
    builder.code(libname, code);
    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);
    root
}

fn child_summary(node: &DesignHierarchyNode) -> Vec<(String, String)> {
    node.children
        .iter()
        .map(|c| (c.label.clone().unwrap_or_default(), c.entity_path.clone()))
        .collect()
}

#[test]
fn three_level_entity_instantiation() {
    let root = build(
        "lib",
        "
entity leaf is end entity;
architecture rtl of leaf is begin end architecture;

entity middle is end entity;
architecture rtl of middle is
begin
    leaf_inst : entity work.leaf;
end architecture;

entity top is end entity;
architecture rtl of top is
begin
    middle_inst : entity work.middle;
end architecture;
",
    );

    let tree = compute_design_hierarchy(&root, "lib", "top").expect("hierarchy");
    assert_eq!(tree.entity_path, "lib.top");
    assert_eq!(tree.architecture.as_deref(), Some("rtl"));
    assert_eq!(tree.kind, HierarchyKind::Top);

    assert_eq!(child_summary(&tree), vec![("middle_inst".into(), "lib.middle".into())]);
    let middle = &tree.children[0];
    assert_eq!(middle.kind, HierarchyKind::DirectEntity);
    assert_eq!(child_summary(middle), vec![("leaf_inst".into(), "lib.leaf".into())]);

    let leaf = &middle.children[0];
    assert!(leaf.children.is_empty());
}

#[test]
fn component_default_binding() {
    // The parser only recognises `inst : name` as a component instantiation
    // when followed by `port map` or `generic map`, so the component must
    // declare at least one port to be instantiable in this form.
    let root = build(
        "lib",
        "
entity leaf is
    port (i : bit);
end entity;
architecture rtl of leaf is begin end architecture;

entity top is end entity;
architecture rtl of top is
    component leaf is
        port (i : bit);
    end component;
    signal s : bit;
begin
    leaf_inst : leaf port map (i => s);
end architecture;
",
    );

    let tree = compute_design_hierarchy(&root, "lib", "top").expect("hierarchy");
    let child = &tree.children[0];
    assert_eq!(child.kind, HierarchyKind::BoundComponent);
    assert_eq!(child.entity_path, "lib.leaf");
    assert!(child.notes.is_empty(), "bound component should have no warnings");
}

#[test]
fn component_unbound_when_no_matching_entity() {
    let root = build(
        "lib",
        "
entity top is end entity;
architecture rtl of top is
    component nowhere is
        port (i : bit);
    end component;
    signal s : bit;
begin
    inst : nowhere port map (i => s);
end architecture;
",
    );

    let tree = compute_design_hierarchy(&root, "lib", "top").expect("hierarchy");
    let child = &tree.children[0];
    assert_eq!(child.kind, HierarchyKind::UnboundComponent);
    assert_eq!(child.entity_path, "lib.nowhere");
    assert!(
        child.notes.iter().any(|n| n.contains("no matching entity")),
        "expected unbound-component note, got {:?}",
        child.notes
    );
}

#[test]
fn multiple_architectures_prefers_rtl() {
    let root = build(
        "lib",
        "
entity leaf is end entity;
architecture sim of leaf is begin end architecture;
architecture rtl of leaf is begin end architecture;

entity top is end entity;
architecture rtl of top is
begin
    leaf_inst : entity work.leaf;
end architecture;
",
    );

    let tree = compute_design_hierarchy(&root, "lib", "top").expect("hierarchy");
    let leaf = &tree.children[0];
    assert_eq!(leaf.architecture.as_deref(), Some("rtl"));
    assert!(
        leaf.notes
            .iter()
            .any(|n| n.contains("multiple architectures") && n.contains("rtl")),
        "expected multi-arch note, got {:?}",
        leaf.notes
    );
}

#[test]
fn multiple_architectures_falls_back_to_alphabetical() {
    let root = build(
        "lib",
        "
entity leaf is end entity;
architecture zeta of leaf is begin end architecture;
architecture alpha of leaf is begin end architecture;

entity top is end entity;
architecture rtl of top is
begin
    leaf_inst : entity work.leaf;
end architecture;
",
    );

    let tree = compute_design_hierarchy(&root, "lib", "top").expect("hierarchy");
    let leaf = &tree.children[0];
    assert_eq!(leaf.architecture.as_deref(), Some("alpha"));
}

#[test]
fn cycle_detection_does_not_loop() {
    // 'a' instantiates 'b' as a direct entity; 'b' (illegal for synthesis
    // but accepted by parsing) instantiates 'a'. Confirm the walker does
    // not stack-overflow and tags the recursion.
    let root = build(
        "lib",
        "
entity a is end entity;
entity b is end entity;

architecture rtl of a is
begin
    b_inst : entity work.b;
end architecture;

architecture rtl of b is
begin
    a_inst : entity work.a;
end architecture;
",
    );

    let tree = compute_design_hierarchy(&root, "lib", "a").expect("hierarchy");
    let b = &tree.children[0];
    assert_eq!(b.entity_path, "lib.b");
    let a_again = &b.children[0];
    assert_eq!(a_again.entity_path, "lib.a");
    assert!(a_again.children.is_empty(), "cycle stops further recursion");
    assert!(
        a_again.notes.iter().any(|n| n.contains("cyclic")),
        "expected cycle note, got {:?}",
        a_again.notes
    );
}

#[test]
fn unknown_library_lists_known_libraries() {
    let root = build("lib", "entity foo is end entity;");
    let err = compute_design_hierarchy(&root, "missing", "foo").unwrap_err();
    match err {
        HierarchyError::UnknownLibrary { library, known } => {
            assert_eq!(library, "missing");
            assert!(known.contains(&"lib".to_string()));
        }
        other => panic!("expected UnknownLibrary, got {other:?}"),
    }
}

#[test]
fn unknown_entity_lists_entities_in_library() {
    let root = build(
        "lib",
        "
entity foo is end entity;
entity bar is end entity;
",
    );
    let err = compute_design_hierarchy(&root, "lib", "missing").unwrap_err();
    match err {
        HierarchyError::UnknownEntity {
            library,
            entity,
            known,
        } => {
            assert_eq!(library, "lib");
            assert_eq!(entity, "missing");
            assert!(known.contains(&"foo".to_string()));
            assert!(known.contains(&"bar".to_string()));
        }
        other => panic!("expected UnknownEntity, got {other:?}"),
    }
}

#[test]
fn list_candidates_ranks_real_top_first() {
    let root = build(
        "lib",
        "
entity leaf is end entity;
architecture rtl of leaf is begin end architecture;

entity middle is end entity;
architecture rtl of middle is
begin
    l : entity work.leaf;
end architecture;

entity top is end entity;
architecture rtl of top is
begin
    m : entity work.middle;
end architecture;

entity orphan is end entity;
architecture rtl of orphan is begin end architecture;
",
    );

    let candidates = list_top_candidates(&root);
    let by_name: std::collections::HashMap<String, _> = candidates
        .iter()
        .map(|c| (format!("{}.{}", c.library, c.entity), c))
        .collect();

    // top has depth 3, orphan has depth 1; both are roots.
    let top = by_name["lib.top"];
    let orphan = by_name["lib.orphan"];
    let middle = by_name["lib.middle"];
    let leaf = by_name["lib.leaf"];

    assert!(top.is_root, "top has no incoming instantiations");
    assert!(orphan.is_root, "orphan has no incoming instantiations");
    assert!(!middle.is_root);
    assert!(!leaf.is_root);

    assert_eq!(top.depth, 3);
    assert_eq!(middle.depth, 2);
    assert_eq!(leaf.depth, 1);
    assert_eq!(orphan.depth, 1);

    assert_eq!(top.instance_count, 0);
    assert_eq!(middle.instance_count, 1);
    assert_eq!(leaf.instance_count, 1);

    // Order: roots first (deepest first), then non-roots (deepest first).
    let order: Vec<&str> = candidates
        .iter()
        .map(|c| c.entity.as_str())
        .collect();
    assert_eq!(order, vec!["top", "orphan", "middle", "leaf"]);
}
