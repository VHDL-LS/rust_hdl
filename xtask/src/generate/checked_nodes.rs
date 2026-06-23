// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2025, Lukas Scheller lukasscheller@icloud.com

use crate::generate::naming::{checked_type_ident, syntax_type_ident, variant_ident};
use crate::generate::Generator;
use crate::model::{ChoiceNode, Model, Node, NodeRef, NodesOrTokens, SequenceNode, Token, TokenOrNode};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

fn section_needs_syntax_token(nodes: &[Node]) -> bool {
    nodes.iter().any(|n| match n {
        Node::Items(seq) => seq.items.iter().any(|item| matches!(item, TokenOrNode::Token(_))),
        _ => false,
    })
}

fn section_needs_super(nodes: &[Node], model: &Model) -> bool {
    nodes.iter().any(|n| match n {
        Node::Items(seq) => seq.items.iter().any(|item| match item {
            TokenOrNode::Node(node_ref) => !model.is_token_choice(&node_ref.kind),
            _ => false,
        }),
        // Node-choice enums hold Checked* variants whose raw() impl references those
        // types — so the section needs `use super::*` to bring them into scope.
        Node::Choices(choice) => matches!(&choice.items, NodesOrTokens::Nodes(_)),
        _ => false,
    })
}

pub struct CheckedNodeGenerator;

impl Generator for CheckedNodeGenerator {
    fn name(&self) -> &str {
        "checked_nodes"
    }

    fn generate_files(&self, model: &Model) -> Vec<(String, TokenStream)> {
        let mut files = Vec::new();

        let mut sections: Vec<(&String, &Vec<Node>)> = model.sections().iter().collect();
        sections.sort_by_key(|(name, _)| *name);

        for (category, nodes) in &sections {
            let syntax_token_import = section_needs_syntax_token(nodes).then(|| quote! {
                use crate::syntax::node::SyntaxToken;
            });
            let super_import = section_needs_super(nodes, model).then(|| quote! {
                use super::*;
            });
            let mut stream = quote! {
                #super_import
                use crate::syntax::checked::CheckedNode;
                use crate::syntax::*;
                #syntax_token_import
            };
            for node in *nodes {
                stream.extend(generate_checked_node(node, model));
            }
            files.push(((*category).clone(), stream));
        }

        files.push(("mod".to_string(), generate_mod(model)));

        files
    }
}

fn generate_checked_node(node: &Node, model: &Model) -> TokenStream {
    match node {
        Node::Items(seq) => generate_checked_sequence(seq, model),
        Node::RawTokens(name) => generate_checked_raw_tokens(name),
        Node::Choices(choice) => match &choice.items {
            NodesOrTokens::Nodes(_) => generate_checked_node_choice(choice),
            // Token-choices don't need a checked wrapper: callers receive the
            // existing *Syntax enum directly (required fields unwrap the Option).
            NodesOrTokens::Tokens(_) => quote! {},
        },
    }
}

// MARK: Sequence nodes

fn generate_checked_sequence(seq: &SequenceNode, model: &Model) -> TokenStream {
    let checked_name = checked_type_ident(&seq.name);
    let syntax_name = syntax_type_ident(&seq.name);

    let getters: TokenStream = seq
        .items
        .iter()
        .map(|item| build_checked_getter(item, model))
        .collect();

    quote! {
        #[derive(Debug, Clone)]
        pub struct #checked_name(#syntax_name);

        impl CheckedNode for #checked_name {
            type Syntax = #syntax_name;
            fn cast_unchecked(syntax: #syntax_name) -> Self {
                #checked_name(syntax)
            }
            fn raw(&self) -> #syntax_name {
                self.0.clone()
            }
        }

        impl #checked_name {
            #getters
        }
    }
}

// MARK: Raw-token nodes

fn generate_checked_raw_tokens(name: &str) -> TokenStream {
    let checked_name = checked_type_ident(name);
    let syntax_name = syntax_type_ident(name);

    quote! {
        #[derive(Debug, Clone)]
        pub struct #checked_name(#syntax_name);

        impl CheckedNode for #checked_name {
            type Syntax = #syntax_name;
            fn cast_unchecked(syntax: #syntax_name) -> Self {
                #checked_name(syntax)
            }
            fn raw(&self) -> #syntax_name {
                self.0.clone()
            }
        }
    }
}

// MARK: Node-choice enums

fn generate_checked_node_choice(choice: &ChoiceNode) -> TokenStream {
    let NodesOrTokens::Nodes(variants) = &choice.items else {
        return quote! {};
    };

    let checked_name = checked_type_ident(&choice.name);
    let syntax_name = syntax_type_ident(&choice.name);

    let enum_variants: Vec<TokenStream> = variants
        .iter()
        .map(|item| {
            let variant = variant_ident(&item.kind);
            let checked = checked_type_ident(&item.kind);
            quote! { #variant(#checked) }
        })
        .collect();

    let cast_branches: Vec<TokenStream> = variants
        .iter()
        .map(|item| {
            let variant = variant_ident(&item.kind);
            let checked = checked_type_ident(&item.kind);
            quote! {
                #syntax_name::#variant(inner) =>
                    #checked_name::#variant(#checked::cast_unchecked(inner))
            }
        })
        .collect();

    let raw_cast_branches: Vec<TokenStream> = variants
        .iter()
        .map(|item| {
            let variant = variant_ident(&item.kind);
            quote! {
                #checked_name::#variant(inner) => #syntax_name::#variant(inner.raw())
            }
        })
        .collect();

    quote! {
        #[derive(Debug, Clone)]
        pub enum #checked_name {
            #(#enum_variants,)*
        }

        impl CheckedNode for #checked_name {
            type Syntax = #syntax_name;
            fn cast_unchecked(syntax: #syntax_name) -> Self {
                match syntax {
                    #(#cast_branches,)*
                }
            }
            fn raw(&self) -> Self::Syntax {
                match self {
                    #(#raw_cast_branches,)*
                }
            }
        }
    }
}

// MARK: Getter helpers

fn build_checked_getter(item: &TokenOrNode, model: &Model) -> TokenStream {
    match item {
        TokenOrNode::Token(token) => build_checked_token_getter(token),
        TokenOrNode::Node(node_ref) => build_checked_node_ref_getter(node_ref, model),
    }
}

fn build_checked_token_getter(token: &Token) -> TokenStream {
    let fn_name = format_ident!("{}", token.getter_name());
    if token.repeated {
        quote! {
            pub fn #fn_name(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
                self.0.#fn_name()
            }
        }
    } else if token.optional {
        quote! {
            pub fn #fn_name(&self) -> Option<SyntaxToken> {
                self.0.#fn_name()
            }
        }
    } else {
        quote! {
            pub fn #fn_name(&self) -> SyntaxToken {
                self.0.#fn_name().unwrap()
            }
        }
    }
}

fn build_checked_node_ref_getter(node_ref: &NodeRef, model: &Model) -> TokenStream {
    let fn_name = format_ident!("{}", node_ref.getter_name());
    let is_token_choice = model.is_token_choice(&node_ref.kind);

    // Token-choices return the existing *Syntax enum directly (no Checked* wrapper).
    // Everything else (sequence nodes, raw-token nodes, node-choice enums) returns
    // the corresponding Checked* type.
    if is_token_choice {
        let syntax_type = syntax_type_ident(&node_ref.kind);
        if node_ref.repeated {
            quote! {
                pub fn #fn_name(&self) -> impl Iterator<Item = #syntax_type> + use<'_> {
                    self.0.#fn_name()
                }
            }
        } else if node_ref.optional {
            quote! {
                pub fn #fn_name(&self) -> Option<#syntax_type> {
                    self.0.#fn_name()
                }
            }
        } else {
            quote! {
                pub fn #fn_name(&self) -> #syntax_type {
                    self.0.#fn_name().unwrap()
                }
            }
        }
    } else {
        let checked_type = checked_type_ident(&node_ref.kind);
        if node_ref.repeated {
            quote! {
                pub fn #fn_name(&self) -> impl Iterator<Item = #checked_type> + use<'_> {
                    self.0.#fn_name().map(#checked_type::cast_unchecked)
                }
            }
        } else if node_ref.optional {
            quote! {
                pub fn #fn_name(&self) -> Option<#checked_type> {
                    self.0.#fn_name().map(#checked_type::cast_unchecked)
                }
            }
        } else {
            quote! {
                pub fn #fn_name(&self) -> #checked_type {
                    #checked_type::cast_unchecked(self.0.#fn_name().unwrap())
                }
            }
        }
    }
}

// MARK: mod.rs

fn generate_mod(model: &Model) -> TokenStream {
    let mut sections = model.sections().keys().collect::<Vec<_>>();
    sections.sort();
    sections
        .into_iter()
        .map(|section| {
            let mod_ident = format_ident!("{}", section);
            quote! {
                pub mod #mod_ident;
                pub use #mod_ident::*;
            }
        })
        .collect()
}
