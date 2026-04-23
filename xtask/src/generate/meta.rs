// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2026, Lukas Scheller lukasscheller@icloud.com

use crate::generate::naming::syntax_type_ident;
use crate::generate::Generator;
use crate::model::{Model, Node};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

pub struct MetaGenerator;

impl Generator for MetaGenerator {
    fn name(&self) -> &str {
        "meta"
    }

    fn generate_files(&self, model: &Model) -> Vec<(String, TokenStream)> {
        vec![("meta".to_string(), generate_meta(model))]
    }
}

fn generate_meta(model: &Model) -> TokenStream {
    // Collect sequence and raw-token nodes (the only ones with NodeKind variants).
    // Sort for determinism — model iterates a HashMap.
    let mut entries: Vec<_> = model
        .all_nodes()
        .filter_map(|node| match node {
            Node::Items(seq) => Some(seq.name.clone()),
            Node::RawTokens(name) => Some(name.clone()),
            Node::Choices(_) => None,
        })
        .collect();
    entries.sort();

    let arms: Vec<TokenStream> = entries
        .iter()
        .map(|name| {
            let nk = format_ident!("{}", name);
            let syntax = syntax_type_ident(name);
            quote! { NodeKind::#nk => #syntax::META }
        })
        .collect();

    quote! {
        use super::*;
        use crate::syntax::meta::Layout;
        use crate::syntax::node_kind::NodeKind;
        use crate::syntax::AstNode;

        /// Return the static [`Layout`] for any concrete node kind.
        pub fn layout_of(kind: NodeKind) -> &'static Layout {
            match kind {
                #(#arms,)*
            }
        }
    }
}
