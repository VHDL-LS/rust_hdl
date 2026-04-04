// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2025, Lukas Scheller lukasscheller@icloud.com

use crate::generate::naming::{node_kind_ident, syntax_type_ident, token_kind_path, variant_ident};
use crate::generate::Generator;
use crate::model::{
    ChoiceNode, Model, Node, NodeRef, NodesOrTokens, SequenceNode, Token, TokenOrNode,
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
        let mut files = Vec::new();

        // Per-section files (sorted for determinism)
        let mut sections: Vec<(&String, &Vec<Node>)> = model.sections().iter().collect();
        sections.sort_by_key(|(name, _)| *name);

        for (category, nodes) in sections {
            let mut stream = quote! {
                use super::*;
                use crate::syntax::node::{SyntaxNode, SyntaxToken};
                use crate::syntax::node_kind::NodeKind;
                use crate::syntax::AstNode;
                use crate::syntax::meta::{Layout, Sequence, Choice, LayoutItem, LayoutItemKind};
                use crate::tokens::Keyword as Kw;
                use crate::tokens::TokenKind;
            };
            for node in nodes {
                stream.extend(generate_rust_struct(node));
                stream.extend(generate_ast_node_rust_impl(node, model));
                stream.extend(generate_rust_impl_getters(node, model));
            }
            files.push((category.clone(), stream));
        }

        // node_kind.rs
        files.push(("node_kind".to_string(), generate_node_kind_enum(model)));

        // mod.rs
        files.push(("mod".to_string(), generate_mod(model)));

        files
    }
}

// MARK: Struct/enum definitions

fn generate_rust_struct(node: &Node) -> TokenStream {
    match node {
        Node::Items(seq) => generate_syntax_node_struct(&seq.name),
        Node::Choices(choice) => generate_choice_enum(choice),
        Node::RawTokens(name) => generate_syntax_node_struct(name),
    }
}

/// Generate the struct `struct FooSyntax(SyntaxNode)`
fn generate_syntax_node_struct(name: &str) -> TokenStream {
    let struct_name = syntax_type_ident(name);
    quote! {
        #[derive(Debug, Clone)]
        pub struct #struct_name(pub(crate) SyntaxNode);
    }
}

/// Generate the choice enum `enum FooSyntax { /* elements of Foo */ }`
fn generate_choice_enum(node: &ChoiceNode) -> TokenStream {
    let name = syntax_type_ident(&node.name);
    let choices = enum_choices(node);
    quote! {
        #[derive(Debug, Clone)]
        pub enum #name {
            #(#choices),*
        }
    }
}

/// Generate all choices (elements) of a choice enum
fn enum_choices(node: &ChoiceNode) -> Vec<TokenStream> {
    match &node.items {
        NodesOrTokens::Nodes(nodes) => nodes
            .iter()
            .map(|item| {
                let variant = variant_ident(&item.kind);
                let syntax = syntax_type_ident(&item.kind);
                quote! { #variant(#syntax) }
            })
            .collect(),
        NodesOrTokens::Tokens(tokens) => tokens
            .iter()
            .map(|item| {
                let variant = variant_ident(&item.name);
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
        Node::Choices(choice) => generate_choice_ast_impl(choice, model),
        Node::RawTokens(name) => generate_sequence_ast_impl(name, &[]),
    }
}

fn generate_sequence_ast_impl(name: &str, meta_items: &[TokenStream]) -> TokenStream {
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
                .flat_map(|item| {
                    collect_concrete_node_kinds(&item.kind, model, &mut HashSet::new())
                })
                .collect();
            let cast_unchecked_branches: Vec<TokenStream> = nodes
                .iter()
                .map(|item| {
                    let variant = variant_ident(&item.kind);
                    let syntax = syntax_type_ident(&item.kind);
                    quote! {
                        if #syntax::can_cast(&node) {
                            return #enum_name::#variant(#syntax::cast_unchecked(node));
                        }
                    }
                })
                .collect();
            let raw_branches: Vec<TokenStream> = nodes
                .iter()
                .map(|item| {
                    let variant = variant_ident(&item.kind);
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
        NodesOrTokens::Tokens(tokens) => {
            let cast_branches: Vec<_> = tokens
                .iter()
                .map(|item| {
                    let kind_expr = token_kind_path(&item.kind);
                    let variant = variant_ident(&item.name);
                    quote! { #kind_expr => Some(#enum_name::#variant(token)) }
                })
                .collect();
            let raw_branches: Vec<_> = tokens
                .iter()
                .map(|item| {
                    let variant = variant_ident(&item.name);
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

// MARK: META helpers

/// Recursively collect all concrete (sequence / raw-token) `NodeKind::X` token-streams
/// for a named node, expanding nested choice nodes as needed.
/// `visited` guards against hypothetical cycles in the choice graph.
fn collect_concrete_node_kinds(
    name: &str,
    model: &Model,
    visited: &mut HashSet<String>,
) -> Vec<TokenStream> {
    if !visited.insert(name.to_owned()) {
        return vec![];
    }
    let node = model
        .all_nodes()
        .find(|n| n.name() == name)
        .unwrap_or_else(|| panic!("node '{}' not found in model", name));
    match node {
        Node::Items(_) | Node::RawTokens(_) => {
            let nk = node_kind_ident(name);
            vec![quote! { NodeKind::#nk }]
        }
        Node::Choices(choice) => match &choice.items {
            NodesOrTokens::Nodes(alts) => alts
                .iter()
                .flat_map(|a| collect_concrete_node_kinds(&a.kind, model, visited))
                .collect(),
            NodesOrTokens::Tokens(_) => vec![], // token-choices don't produce NodeKind entries
        },
    }
}

// MARK: META item helpers

/// Build a `LayoutItem { ... }` token-stream for one item in a sequence.
fn layout_item_ts(item: &TokenOrNode, model: &Model) -> TokenStream {
    match item {
        TokenOrNode::Token(t) => {
            let kind_expr = token_kind_path(&t.kind);
            let optional = t.optional;
            let repeated = t.repeated;
            let name_str = t.name.to_case(Case::Snake);
            quote! {
                LayoutItem {
                    optional: #optional,
                    repeated: #repeated,
                    name: #name_str,
                    kind: LayoutItemKind::Token(#kind_expr),
                }
            }
        }
        TokenOrNode::Node(node_ref) => {
            let optional = node_ref.optional;
            let repeated = node_ref.repeated;
            let name_str = node_ref.name.to_case(Case::Snake);
            let kind_expr = layout_item_kind_for_node_ref(node_ref, model);
            quote! {
                LayoutItem {
                    optional: #optional,
                    repeated: #repeated,
                    name: #name_str,
                    kind: #kind_expr,
                }
            }
        }
    }
}

/// Produce the `LayoutItemKind::…` expression for a node reference.
fn layout_item_kind_for_node_ref(node_ref: &NodeRef, model: &Model) -> TokenStream {
    let target = model
        .all_nodes()
        .find(|n| n.name() == node_ref.kind)
        .unwrap_or_else(|| panic!("node '{}' not found in model", node_ref.kind));

    match target {
        Node::Items(_) | Node::RawTokens(_) => {
            let nk = node_kind_ident(&node_ref.kind);
            quote! { LayoutItemKind::Node(NodeKind::#nk) }
        }
        Node::Choices(choice) => match &choice.items {
            NodesOrTokens::Nodes(_) => {
                let nks = collect_concrete_node_kinds(&node_ref.kind, model, &mut HashSet::new());
                quote! { LayoutItemKind::NodeChoice(&[#(#nks),*]) }
            }
            NodesOrTokens::Tokens(toks) => {
                let tks: Vec<TokenStream> = toks.iter().map(|t| token_kind_path(&t.kind)).collect();
                quote! { LayoutItemKind::TokenChoice(&[#(#tks),*]) }
            }
        },
    }
}

// MARK: Getter impls

fn generate_rust_impl_getters(node: &Node, model: &Model) -> TokenStream {
    match node {
        Node::Items(seq) => generate_sequence_getters(seq, model),
        Node::Choices(_) | Node::RawTokens(_) => quote! {},
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

fn build_getter(item: &TokenOrNode, model: &Model) -> TokenStream {
    match item {
        TokenOrNode::Node(node_ref) => build_node_getter(node_ref, model),
        TokenOrNode::Token(token) => build_token_getter(token),
    }
}

fn build_node_getter(node_ref: &NodeRef, model: &Model) -> TokenStream {
    let fn_name = format_ident!("{}", node_ref.getter_name());
    let syntax = syntax_type_ident(&node_ref.kind);
    let nth = Literal::usize_unsuffixed(node_ref.nth);
    let getter_fn_name = if model.is_token_choice(&node_ref.kind) {
        quote! { tokens }
    } else {
        quote! { children }
    };
    if node_ref.repeated {
        assert_eq!(
            node_ref.nth, 0,
            "node {node_ref:?} is not at position 0 but is repeated"
        );
        quote! {
            pub fn #fn_name(&self) -> impl Iterator<Item = #syntax>  + use<'_> {
                self.0.#getter_fn_name().filter_map(#syntax::cast)
            }
        }
    } else {
        quote! {
            pub fn #fn_name(&self) -> Option<#syntax> {
                self.0.#getter_fn_name().filter_map(#syntax::cast).nth(#nth)
            }
        }
    }
}

fn build_token_getter(token: &Token) -> TokenStream {
    let function_name = format_ident!("{}", token.getter_name());
    let kind_expr = token_kind_path(&token.kind);
    let nth = Literal::usize_unsuffixed(token.nth);
    if token.repeated {
        assert_eq!(token.nth, 0, "{} multiple", token.name);
        quote! {
            pub fn #function_name(&self) -> impl Iterator<Item = SyntaxToken>  + use<'_> {
                self.0
                    .tokens()
                    .filter(|token| token.kind() == #kind_expr)
            }
        }
    } else {
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

// MARK: node_kind.rs and mod.rs

fn generate_node_kind_enum(model: &Model) -> TokenStream {
    let mut choices = model
        .collect_all_sequence_node_kinds()
        .into_iter()
        .map(|kind| format_ident!("{}", kind))
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

fn generate_mod(model: &Model) -> TokenStream {
    let mut sections = model.sections().keys().collect::<Vec<_>>();
    sections.sort();
    let sections = sections
        .into_iter()
        .map(|section| {
            let mod_ident = format_ident!("{}", section);
            quote! {
                pub mod #mod_ident;
                pub use #mod_ident::*;
            }
        })
        .collect::<TokenStream>();
    quote! {
        pub mod node_kind;
        pub use node_kind::*;

        #sections

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
        ChoiceNode, Model, Node, NodeRef, NodesOrTokens, SequenceNode, Token, TokenOrNode,
    };

    fn make_test_model() -> Model {
        let mut model = Model::default();

        // A token-choice node: RelOp -> { EQ | NE }
        let choice = ChoiceNode {
            name: "RelOp".to_string(),
            items: NodesOrTokens::Tokens(vec![
                Token::from(TokenKind::EQ),
                Token::from(TokenKind::NE),
            ]),
        };
        model.push_node("test".to_string(), Node::Choices(choice));

        // A sequence node: DesignFile -> [RelOp]
        let seq = SequenceNode::new(
            "DesignFile",
            vec![TokenOrNode::Node(NodeRef {
                kind: "RelOp".to_string(),
                nth: 0,
                builtin: false,
                repeated: false,
                name: "rel_op".to_string(),
                optional: false,
            })],
        );
        model.push_node("test".to_string(), Node::Items(seq));
        model.do_postprocessing();
        model
    }

    #[test]
    fn syntax_node_generator_produces_files() {
        let model = make_test_model();
        let gen = SyntaxNodeGenerator;
        let files = gen.generate_files(&model);
        // Should produce at least: "test", "node_kind", "mod"
        let stems: Vec<&str> = files.iter().map(|(s, _)| s.as_str()).collect();
        assert!(stems.contains(&"test"), "missing 'test' file");
        assert!(stems.contains(&"node_kind"), "missing 'node_kind' file");
        assert!(stems.contains(&"mod"), "missing 'mod' file");
    }

    #[test]
    fn sequence_node_getter_uses_tokens_for_token_choice() {
        let model = make_test_model();
        let gen = SyntaxNodeGenerator;
        let files = gen.generate_files(&model);
        let test_file = files.iter().find(|(s, _)| s == "test").unwrap();
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
        let test_file = files.iter().find(|(s, _)| s == "test").unwrap();
        insta::assert_snapshot!(test_file.1.to_string());
    }
}
