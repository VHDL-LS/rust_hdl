// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2025, Lukas Scheller lukasscheller@icloud.com

use crate::generate::naming::{node_kind_ident, syntax_type_ident, token_kind_path, variant_ident};
use crate::generate::Generator;
use crate::model::{
    Cardinality, ChoiceNode, Field, ListNode, Model, Node, NodeKind, NodeOrTokenKind,
    NodesOrTokens, SequenceNode, TokenKind,
};
use convert_case::{Case, Casing};
use proc_macro2::{Literal, TokenStream};
use quote::{format_ident, quote};
use std::collections::HashSet;

pub struct SyntaxNodeGenerator;

impl Generator for SyntaxNodeGenerator {
    fn name(&self) -> &str {
        "syntax_nodes"
    }

    fn generate_files(&self, model: &Model) -> Vec<(String, TokenStream)> {
        let mut stream = quote! {
            use super::*;
            use crate::syntax::node::{SyntaxNode, SyntaxToken};
            use crate::syntax::node_kind::NodeKind;
            use crate::syntax::AstNode;
            use crate::syntax::meta::{Layout, Sequence, Choice, List, LayoutItem, LayoutItemKind};
            use crate::tokens::Keyword as Kw;
            use crate::tokens::TokenKind;
        };

        // Sorted by node name for deterministic output
        let mut nodes: Vec<&Node> = model.all_nodes().collect();
        nodes.sort_by_key(|node| node.name());

        for node in nodes {
            stream.extend(generate_rust_struct(node, model));
            stream.extend(generate_ast_node_rust_impl(node, model));
            stream.extend(generate_rust_impl_getters(node, model));
        }

        vec![
            ("syntax_nodes".to_string(), stream),
            ("node_kind".to_string(), generate_node_kind_enum(model)),
            ("mod".to_string(), generate_mod()),
        ]
    }
}

// MARK: Struct/enum definitions

fn generate_rust_struct(node: &Node, model: &Model) -> TokenStream {
    match node {
        Node::Items(seq) => generate_syntax_node_struct(&seq.name),
        Node::List(list) => generate_syntax_node_struct(&list.kind),
        Node::Choices(choice) => generate_choice_enum(choice, model),
        // An alias is a second name for another node, not a node of its own: the tree holds
        // the aliased node and `FooSyntax` already exists for it.
        Node::Alias(_) => quote! {},
    }
}

/// Generate the struct `struct FooSyntax(SyntaxNode)`
fn generate_syntax_node_struct(name: &NodeKind) -> TokenStream {
    let struct_name = syntax_type_ident(name);
    quote! {
        #[derive(Debug, Clone)]
        pub struct #struct_name(pub(crate) SyntaxNode);
    }
}

/// Generate the choice enum `enum FooSyntax { /* elements of Foo */ }`
fn generate_choice_enum(node: &ChoiceNode, model: &Model) -> TokenStream {
    let name = syntax_type_ident(&node.name);
    let choices = enum_choices(node, model);
    quote! {
        #[derive(Debug, Clone)]
        pub enum #name {
            #(#choices),*
        }
    }
}

/// The node kind a choice alternative resolves to.
///
/// `Model::check_choice_alternatives_are_nodes` has already ruled out an alternative that
/// renames a token, so this only has to peel aliases.
fn resolved_alternative(kind: &NodeKind, model: &Model) -> NodeKind {
    match model.resolve_alias(kind) {
        NodeOrTokenKind::Node(kind) => kind,
        NodeOrTokenKind::Token(_) => {
            unreachable!("choice alternative {kind} renames a token")
        }
    }
}

/// Generate all choices (elements) of a choice enum
fn enum_choices(node: &ChoiceNode, model: &Model) -> Vec<TokenStream> {
    match &node.items {
        NodesOrTokens::Nodes(nodes) => nodes
            .iter()
            .map(|kind| {
                // An alternative names the variant; the type it wraps is the aliased node's.
                let variant = variant_ident(kind);
                let syntax = syntax_type_ident(resolved_alternative(kind, model));
                quote! { #variant(#syntax) }
            })
            .collect(),
        NodesOrTokens::Tokens(alternatives) => alternatives
            .iter()
            .map(|alternative| {
                // Likewise: the alternative names the variant, whether it is a token or renames
                // one. Either way the variant holds the bare token.
                let variant = variant_ident(&alternative.name);
                quote! { #variant(SyntaxToken) }
            })
            .collect(),
    }
}

// MARK: AstNode impls

fn generate_ast_node_rust_impl(node: &Node, model: &Model) -> TokenStream {
    match node {
        Node::Items(seq) => {
            let meta_items: Vec<TokenStream> = seq
                .items
                .iter()
                .map(|item| layout_item_ts(item, model))
                .collect();
            generate_sequence_ast_impl(&seq.name, &meta_items)
        }
        Node::List(list) => generate_list_ast_impl(
            &list.kind,
            &layout_item_ts(&list.element, model),
            &layout_item_ts(&list.separator, model),
        ),
        Node::Choices(choice) => generate_choice_ast_impl(choice, model),
        // No struct, so nothing to implement `AstNode` for.
        Node::Alias(_) => quote! {},
    }
}

fn generate_sequence_ast_impl(name: &NodeKind, meta_items: &[TokenStream]) -> TokenStream {
    let struct_name = syntax_type_ident(name);
    let node_kind = node_kind_ident(name);
    quote! {
        impl AstNode for #struct_name {
            const META: &'static Layout = &Layout::Sequence(Sequence {
                kind: NodeKind::#node_kind,
                items: &[#(#meta_items),*],
            });
            fn cast_unchecked(node: SyntaxNode) -> Self {
                #struct_name(node)
            }
            fn raw(&self) -> SyntaxNode {
                self.0.clone()
            }
        }
    }
}

fn generate_choice_ast_impl(node: &ChoiceNode, model: &Model) -> TokenStream {
    let enum_name = syntax_type_ident(&node.name);
    match &node.items {
        NodesOrTokens::Nodes(nodes) => {
            let node_kinds: Vec<TokenStream> = nodes
                .iter()
                .flat_map(|kind| collect_concrete_node_kinds(kind, model, &mut HashSet::new()))
                .collect();
            let cast_unchecked_branches: Vec<TokenStream> = nodes
                .iter()
                .map(|kind| {
                    let variant = variant_ident(kind);
                    let syntax = syntax_type_ident(resolved_alternative(kind, model));
                    quote! {
                        if #syntax::can_cast(&node) {
                            return #enum_name::#variant(#syntax::cast_unchecked(node));
                        }
                    }
                })
                .collect();
            let raw_branches: Vec<TokenStream> = nodes
                .iter()
                .map(|kind| {
                    let variant = variant_ident(kind);
                    quote! { #enum_name::#variant(inner) => inner.raw() }
                })
                .collect();
            quote! {
                impl AstNode for #enum_name {
                    const META: &'static Layout = &Layout::Choice(Choice {
                        options: &[#(#node_kinds),*],
                    });
                    fn cast_unchecked(node: SyntaxNode) -> Self {
                        #(#cast_unchecked_branches)*
                        unreachable!("cast_unchecked called with unexpected node kind {:?}", node.kind())
                    }
                    fn raw(&self) -> SyntaxNode {
                        match self {
                            #(#raw_branches, )*
                        }
                    }
                }
            }
        }
        NodesOrTokens::Tokens(alternatives) => {
            let cast_branches: Vec<_> = alternatives
                .iter()
                .map(|alternative| {
                    let kind_expr = token_kind_path(&model.alternative_token(alternative));
                    let variant = variant_ident(&alternative.name);
                    quote! { #kind_expr => Some(#enum_name::#variant(token)) }
                })
                .collect();
            let raw_branches: Vec<_> = alternatives
                .iter()
                .map(|alternative| {
                    let variant = variant_ident(&alternative.name);
                    quote! { #enum_name::#variant(token) => token.clone() }
                })
                .collect();
            quote! {
                impl #enum_name {
                    pub fn cast(token: SyntaxToken) -> Option<Self> {
                        match token.kind() {
                            #(#cast_branches ,)*
                            _ => None,
                        }
                    }
                    pub fn raw(&self) -> SyntaxToken {
                        match self {
                            #(#raw_branches ,)*
                        }
                    }
                }
            }
        }
    }
}

fn generate_list_ast_impl(
    name: &NodeKind,
    element: &TokenStream,
    separator: &TokenStream,
) -> TokenStream {
    let struct_name = syntax_type_ident(name);
    let node_kind = node_kind_ident(name);
    quote! {
        impl AstNode for #struct_name {
            const META: &'static Layout = &Layout::List(List {
                kind: NodeKind::#node_kind,
                element: &#element,
                separator: &#separator,
            });
            fn cast_unchecked(node: SyntaxNode) -> Self {
                #struct_name(node)
            }
            fn raw(&self) -> SyntaxNode {
                self.0.clone()
            }
        }
    }
}

// MARK: META helpers

/// Recursively collect all concrete (sequence / raw-token) `NodeKind::X` token-streams
/// for a named node, expanding nested choice nodes as needed.
/// `visited` guards against hypothetical cycles in the choice graph.
fn collect_concrete_node_kinds(
    name: &NodeKind,
    model: &Model,
    visited: &mut HashSet<NodeKind>,
) -> Vec<TokenStream> {
    // An alias materializes nothing of its own, so it stands for whatever it renames.
    let NodeOrTokenKind::Node(name) = model.resolve_alias(name) else {
        return vec![]; // an alias of a token contributes no NodeKind entry
    };
    if !visited.insert(name.clone()) {
        return vec![];
    }
    let node = model
        .node(&name)
        .unwrap_or_else(|| panic!("node '{name}' not found in model"));
    match node {
        // Both are materialized by the parser as a node of exactly this kind.
        Node::Items(_) | Node::List(_) => {
            let nk = node_kind_ident(&name);
            vec![quote! { NodeKind::#nk }]
        }
        Node::Choices(choice) => match &choice.items {
            NodesOrTokens::Nodes(alts) => alts
                .iter()
                .flat_map(|alt| collect_concrete_node_kinds(alt, model, visited))
                .collect(),
            NodesOrTokens::Tokens(_) => vec![], // token-choices don't produce NodeKind entries
        },
        Node::Alias(_) => unreachable!("resolve_alias never yields an alias"),
    }
}

// MARK: META item helpers

/// Build a `LayoutItem { ... }` token-stream for one item in a sequence.
fn layout_item_ts(item: &Field, model: &Model) -> TokenStream {
    let optional = item.is_optional();
    let repeated = item.is_repeated();
    let name_str = item.name.to_case(Case::Snake);
    // A reference through an alias describes whatever the alias resolves to.
    let kind_expr = match model.resolved_kind(item) {
        NodeOrTokenKind::Token(token_kind) => {
            let kind_expr = token_kind_path(&token_kind);
            quote! { LayoutItemKind::Token(#kind_expr) }
        }
        NodeOrTokenKind::Node(node_kind) => layout_item_kind_for_node_ref(&node_kind, model),
    };
    quote! {
        LayoutItem {
            optional: #optional,
            repeated: #repeated,
            name: #name_str,
            kind: #kind_expr,
        }
    }
}

/// Produce the `LayoutItemKind::…` expression for a reference to `node_kind`, which has already
/// been resolved through any alias.
fn layout_item_kind_for_node_ref(node_kind: &NodeKind, model: &Model) -> TokenStream {
    let target = model
        .node(node_kind)
        .unwrap_or_else(|| panic!("node '{node_kind}' not found in model"));

    match target {
        Node::Items(_) | Node::List(_) => {
            let nk = node_kind_ident(node_kind);
            quote! { LayoutItemKind::Node(NodeKind::#nk) }
        }
        Node::Choices(choice) => match &choice.items {
            NodesOrTokens::Nodes(_) => {
                let nks = collect_concrete_node_kinds(node_kind, model, &mut HashSet::new());
                quote! { LayoutItemKind::NodeChoice(&[#(#nks),*]) }
            }
            NodesOrTokens::Tokens(alternatives) => {
                let tks: Vec<TokenStream> = alternatives
                    .iter()
                    .map(|alternative| token_kind_path(&model.alternative_token(alternative)))
                    .collect();
                quote! { LayoutItemKind::TokenChoice(&[#(#tks),*]) }
            }
        },
        Node::Alias(_) => unreachable!("resolve_alias never yields an alias"),
    }
}

// MARK: Getter impls

fn generate_rust_impl_getters(node: &Node, model: &Model) -> TokenStream {
    match node {
        Node::Items(seq) => generate_sequence_getters(seq, model),
        Node::List(list) => generate_list_getters(list, model),
        Node::Choices(_) => quote! {},
        // The aliased node owns the getters; an alias only renames them at the use site.
        Node::Alias(_) => quote! {},
    }
}

fn generate_sequence_getters(node: &SequenceNode, model: &Model) -> TokenStream {
    let getters: TokenStream = node
        .items
        .iter()
        .map(|item| build_getter(item, model))
        .collect();
    let name = syntax_type_ident(&node.name);
    quote! {
        impl #name {
            #getters
        }
    }
}

fn generate_list_getters(list: &ListNode, model: &Model) -> TokenStream {
    let element_getter = build_getter(&list.element, model);
    let separator_getter = build_getter(&list.separator, model);
    let name = syntax_type_ident(&list.kind);
    quote! {
        impl #name {
            #element_getter
            #separator_getter
        }
    }
}

/// The field's *name* — the alias, where the reference is to one — names the getter; what it
/// resolves to decides the getter's shape, down to whether it is a node or a token getter.
fn build_getter(item: &Field, model: &Model) -> TokenStream {
    match model.resolved_kind(item) {
        NodeOrTokenKind::Node(node_kind) => build_node_getter(item, &node_kind, model),
        NodeOrTokenKind::Token(token_kind) => build_token_getter(item, &token_kind),
    }
}

fn build_node_getter(item: &Field, node_kind: &NodeKind, model: &Model) -> TokenStream {
    let fn_name = format_ident!("{}", item.getter_name());
    let syntax = syntax_type_ident(node_kind);
    let getter_fn_name = if model.is_token_choice(node_kind) {
        quote! { tokens }
    } else {
        quote! { children }
    };
    match item.cardinality {
        Cardinality::Repeated(_) => quote! {
            pub fn #fn_name(&self) -> impl Iterator<Item = #syntax>  + use<'_> {
                self.0.#getter_fn_name().filter_map(#syntax::cast)
            }
        },
        Cardinality::Required { nth } | Cardinality::Optional { nth } => {
            let nth = Literal::usize_unsuffixed(nth);
            quote! {
                pub fn #fn_name(&self) -> Option<#syntax> {
                    self.0.#getter_fn_name().filter_map(#syntax::cast).nth(#nth)
                }
            }
        }
    }
}

fn build_token_getter(item: &Field, token_kind: &TokenKind) -> TokenStream {
    let function_name = format_ident!("{}", item.getter_name());
    let kind_expr = token_kind_path(token_kind);
    match item.cardinality {
        Cardinality::Repeated(_) => quote! {
            pub fn #function_name(&self) -> impl Iterator<Item = SyntaxToken>  + use<'_> {
                self.0
                    .tokens()
                    .filter(|token| token.kind() == #kind_expr)
            }
        },
        Cardinality::Required { nth } | Cardinality::Optional { nth } => {
            let nth = Literal::usize_unsuffixed(nth);
            quote! {
                pub fn #function_name(&self) -> Option<SyntaxToken> {
                    self.0
                        .tokens()
                        .filter(|token| token.kind() == #kind_expr)
                        .nth(#nth)
                }
            }
        }
    }
}

// MARK: node_kind.rs and mod.rs

fn generate_node_kind_enum(model: &Model) -> TokenStream {
    let mut choices = model
        .collect_all_materialized_node_kinds()
        .into_iter()
        .map(|kind| format_ident!("{}", kind.as_str()))
        .collect::<Vec<_>>();
    choices.sort();
    quote! {
        #[derive(PartialEq, Eq, Copy, Clone, Debug)]
        #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
        pub enum NodeKind {
            #(#choices),*
        }
    }
}

fn generate_mod() -> TokenStream {
    quote! {
        pub mod node_kind;
        pub use node_kind::*;

        pub mod syntax_nodes;
        pub use syntax_nodes::*;

        pub mod builders;
        pub use builders::*;

        pub mod meta;
        pub use meta::*;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::token::TokenKind;
    use crate::model::{
        AliasNode, ChoiceNode, Field, Model, Node, NodeKind, NodesOrTokens, SequenceNode,
    };

    fn make_test_model() -> Model {
        let mut model = Model::default();

        // A token-choice node: RelOp -> { EQ | NE }
        let choice = ChoiceNode {
            name: NodeKind::from("RelOp"),
            items: NodesOrTokens::Tokens(vec![
                Field::token(TokenKind::EQ),
                Field::token(TokenKind::NE),
            ]),
        };
        model.push_node(Node::Choices(choice));

        // A sequence node: DesignFile -> [RelOp]
        let seq = SequenceNode::new("DesignFile", vec![Field::node("RelOp")]);
        model.push_node(Node::Items(seq));
        model.do_postprocessing();
        model
    }

    #[test]
    fn syntax_node_generator_produces_files() {
        let model = make_test_model();
        let gen = SyntaxNodeGenerator;
        let files = gen.generate_files(&model);
        // Should produce exactly: "syntax_nodes", "node_kind", "mod"
        let stems: Vec<&str> = files.iter().map(|(s, _)| s.as_str()).collect();
        assert!(
            stems.contains(&"syntax_nodes"),
            "missing 'syntax_nodes' file"
        );
        assert!(stems.contains(&"node_kind"), "missing 'node_kind' file");
        assert!(stems.contains(&"mod"), "missing 'mod' file");
    }

    #[test]
    fn sequence_node_getter_uses_tokens_for_token_choice() {
        let model = make_test_model();
        let gen = SyntaxNodeGenerator;
        let files = gen.generate_files(&model);
        let test_file = files.iter().find(|(s, _)| s == "syntax_nodes").unwrap();
        let code = test_file.1.to_string();
        // The getter for RelOp (a token choice) should use .tokens()
        assert!(
            code.contains("tokens"),
            "getter for token-choice node should use .tokens(), got:\n{code}"
        );
    }

    #[test]
    fn node_kind_enum_contains_sequence_nodes_only() {
        let model = make_test_model();
        let gen = SyntaxNodeGenerator;
        let files = gen.generate_files(&model);
        let nk = files.iter().find(|(s, _)| s == "node_kind").unwrap();
        let code = nk.1.to_string();
        // DesignFile is a sequence node → present
        assert!(
            code.contains("DesignFile"),
            "DesignFile missing from NodeKind"
        );
        // RelOp is a choice node → absent from NodeKind
        assert!(!code.contains("RelOp"), "RelOp should not be in NodeKind");
    }

    #[test]
    fn snapshot_sequence_node_output() {
        let model = make_test_model();
        let gen = SyntaxNodeGenerator;
        let files = gen.generate_files(&model);
        let test_file = files.iter().find(|(s, _)| s == "syntax_nodes").unwrap();
        insta::assert_snapshot!(test_file.1.to_string());
    }

    /// `Condition` aliases `Expression`, and `DesignFile` references the alias.
    fn make_alias_model() -> Model {
        let mut model = Model::default();
        model.push_node(SequenceNode::new(
            "Expression",
            vec![Field::token(TokenKind::Identifier)],
        ));
        model.push_node(AliasNode::node("Condition", "Expression"));
        model.push_node(SequenceNode::new(
            "DesignFile",
            vec![Field::node("Condition")],
        ));
        model.do_postprocessing();
        model
    }

    fn generated_syntax_nodes(model: &Model) -> String {
        let files = SyntaxNodeGenerator.generate_files(model);
        files
            .iter()
            .find(|(stem, _)| stem == "syntax_nodes")
            .unwrap()
            .1
            .to_string()
    }

    /// The alias renames the getter; everything else about it is the aliased node's.
    #[test]
    fn alias_renames_the_getter_and_keeps_the_aliased_type() {
        let code = generated_syntax_nodes(&make_alias_model());
        assert!(
            code.contains("pub fn condition (& self) -> Option < ExpressionSyntax >"),
            "expected a `condition` getter returning `ExpressionSyntax`, got:\n{code}"
        );
        assert!(
            !code.contains("ConditionSyntax"),
            "an alias must not get a syntax type of its own, got:\n{code}"
        );
    }

    /// An alias is not a node in the tree, so it contributes no `NodeKind` variant and no layout.
    #[test]
    fn alias_contributes_no_node_kind() {
        let model = make_alias_model();
        let files = SyntaxNodeGenerator.generate_files(&model);
        let code = files
            .iter()
            .find(|(stem, _)| stem == "node_kind")
            .unwrap()
            .1
            .to_string();
        assert!(code.contains("Expression"), "Expression missing:\n{code}");
        assert!(
            !code.contains("Condition"),
            "an alias must not become a NodeKind, got:\n{code}"
        );
    }

    /// A reference through several aliases lands on the node at the bottom.
    #[test]
    fn nested_alias_resolves_to_the_non_aliased_node() {
        let mut model = make_alias_model();
        model.push_node(AliasNode::node("Guard", "Condition"));
        model.push_node(SequenceNode::new(
            "GuardedThing",
            vec![Field::node("Guard")],
        ));

        let code = generated_syntax_nodes(&model);
        assert!(
            code.contains("pub fn guard (& self) -> Option < ExpressionSyntax >"),
            "expected a `guard` getter returning `ExpressionSyntax`, got:\n{code}"
        );
    }
}
