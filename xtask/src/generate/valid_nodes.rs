// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2026, Lukas Scheller lukasscheller@icloud.com

//! Getters on `Valid<FooSyntax>` that lift the guarantees of validation into the types.
//!
//! A validated node has every required item of its layout, and every descendant is valid as
//! well. The getters therefore return a required child without the `Option`, and wrap every
//! child node in `Valid` again. Tokens carry no structure of their own and are returned as is.

use crate::generate::naming::{syntax_type_ident, variant_ident};
use crate::generate::syntax_nodes::resolved_alternative;
use crate::generate::Generator;
use crate::model::{
    ChoiceNode, Field, ListNode, Model, Node, NodeOrTokenKind, NodesOrTokens, SequenceNode,
};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

pub struct ValidNodeGenerator;

impl Generator for ValidNodeGenerator {
    fn name(&self) -> &str {
        "valid_nodes"
    }

    fn generate_files(&self, model: &Model) -> Vec<(String, TokenStream)> {
        let mut stream = quote! {
            use super::*;
            use crate::syntax::node::SyntaxToken;
            use crate::syntax::validate::valid_node::Valid;
        };

        // Sorted by node name for deterministic output
        let mut nodes: Vec<&Node> = model.all_nodes().collect();
        nodes.sort_by_key(|node| node.name());

        for node in nodes {
            stream.extend(match node {
                Node::Items(seq) => generate_sequence_getters(seq, model),
                Node::List(list) => generate_list_getters(list, model),
                Node::Choices(choice) => generate_choice(choice, model),
                // An alias has no type of its own.
                Node::Alias(_) => quote! {},
            });
        }

        vec![("valid_nodes".to_string(), stream)]
    }
}

fn valid_choice_ident(choice: &ChoiceNode) -> proc_macro2::Ident {
    format_ident!("Valid{}", variant_ident(&choice.name))
}

/// Generate `enum ValidFoo` for a node choice, and `Valid<FooSyntax>::alternative` to obtain it.
fn generate_choice(choice: &ChoiceNode, model: &Model) -> TokenStream {
    let NodesOrTokens::Nodes(alternatives) = &choice.items else {
        return quote! {};
    };
    let syntax_name = syntax_type_ident(&choice.name);
    let valid_name = valid_choice_ident(choice);

    let variants = alternatives.iter().map(|kind| {
        let variant = variant_ident(kind);
        let syntax = syntax_type_ident(resolved_alternative(kind, model));
        quote! { #variant(Valid<#syntax>) }
    });
    let kind_branches = alternatives.iter().map(|kind| {
        let variant = variant_ident(kind);
        quote! {
            #syntax_name::#variant(inner) => #valid_name::#variant(Valid::new_unchecked(inner.clone()))
        }
    });

    quote! {
        #[derive(Debug, Clone)]
        pub enum #valid_name {
            #(#variants),*
        }

        impl Valid<#syntax_name> {
            pub fn alternative(&self) -> #valid_name {
                match self.inner() {
                    #(#kind_branches,)*
                }
            }
        }
    }
}

fn generate_sequence_getters(node: &SequenceNode, model: &Model) -> TokenStream {
    let name = syntax_type_ident(&node.name);
    let getters = node.items.iter().map(|item| build_getter(item, model));
    quote! {
        impl Valid<#name> {
            #(#getters)*
        }
    }
}

fn generate_list_getters(list: &ListNode, model: &Model) -> TokenStream {
    let name = syntax_type_ident(&list.kind);
    let element_getter = build_getter(&list.element, model);
    let separator_getter = build_getter(&list.separator, model);
    quote! {
        impl Valid<#name> {
            #element_getter
            #separator_getter
        }
    }
}

fn build_getter(item: &Field, model: &Model) -> TokenStream {
    let fn_name = format_ident!("{}", item.getter_name());
    let (ty, wrap) = match model.resolved_kind(item) {
        NodeOrTokenKind::Node(kind) if !model.is_token_choice(&kind) => {
            let syntax = syntax_type_ident(&kind);
            (quote! { Valid<#syntax> }, true)
        }
        // A token choice is an enum over bare tokens, not an `AstNode`.
        NodeOrTokenKind::Node(kind) => {
            let syntax = syntax_type_ident(&kind);
            (quote! { #syntax }, false)
        }
        NodeOrTokenKind::Token(_) => (quote! { SyntaxToken }, false),
    };

    if item.is_repeated() {
        let body = if wrap {
            quote! { self.inner().#fn_name().map(Valid::new_unchecked) }
        } else {
            quote! { self.inner().#fn_name() }
        };
        quote! {
            pub fn #fn_name(&self) -> impl Iterator<Item = #ty> + use<'_> {
                #body
            }
        }
    } else if item.may_be_absent() {
        let body = if wrap {
            quote! { self.inner().#fn_name().map(Valid::new_unchecked) }
        } else {
            quote! { self.inner().#fn_name() }
        };
        quote! {
            pub fn #fn_name(&self) -> Option<#ty> {
                #body
            }
        }
    } else {
        let child = quote! { self.inner().#fn_name().expect("node must be valid") };
        let body = if wrap {
            quote! { Valid::new_unchecked(#child) }
        } else {
            child
        };
        quote! {
            pub fn #fn_name(&self) -> #ty {
                #body
            }
        }
    }
}
