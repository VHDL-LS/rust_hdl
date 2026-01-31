// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2025, Lukas Scheller lukasscheller@icloud.com

use crate::token::{Token, TokenKind};
use convert_case::{Case, Casing};
use proc_macro2::{Ident, Literal, TokenStream};
use quote::{format_ident, quote};
use std::collections::{HashMap, HashSet};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TokenOrNode {
    Node(NodeRef),
    Token(Token),
}

impl From<TokenKind> for TokenOrNode {
    fn from(value: TokenKind) -> Self {
        TokenOrNode::Token(Token::from(value))
    }
}

impl From<NodeRef> for TokenOrNode {
    fn from(value: NodeRef) -> Self {
        TokenOrNode::Node(value)
    }
}

impl TokenOrNode {
    pub fn getter_name(&self) -> Ident {
        match self {
            TokenOrNode::Node(node) => node.getter_name(),
            TokenOrNode::Token(token) => token.getter_name(),
        }
    }
}

impl TokenOrNode {
    pub fn generate_rust_getter(&self) -> TokenStream {
        match self {
            TokenOrNode::Node(node) => node.build_getter(),
            TokenOrNode::Token(token) => token.build_getter(),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NodeRef {
    pub kind: String,
    pub nth: usize,
    pub builtin: bool,
    pub repeated: bool,
    pub name: String,
    pub is_token_node: bool,
}

impl From<String> for NodeRef {
    fn from(value: String) -> Self {
        NodeRef {
            kind: value.clone(),
            nth: 0,
            builtin: false,
            repeated: false,
            name: value,
            is_token_node: false,
        }
    }
}

impl NodeRef {
    fn getter_name(&self) -> Ident {
        format_ident!("{}", self.name.to_case(Case::Snake))
    }

    fn syntax_name(&self) -> Ident {
        format_ident!("{}Syntax", self.kind.to_case(Case::UpperCamel))
    }

    fn enum_variant_ident(&self) -> Ident {
        format_ident!("{}", self.kind.to_case(Case::UpperCamel))
    }

    pub fn build_getter(&self) -> TokenStream {
        let fn_name = self.getter_name();
        let syntax_name = self.syntax_name();
        let nth = Literal::usize_unsuffixed(self.nth);
        let getter_fn_name = if self.is_token_node {
            quote! {tokens}
        } else {
            quote! {children}
        };
        if self.repeated {
            assert_eq!(
                self.nth, 0,
                "node {self:?} is not at position 0 but is repeated"
            );
            quote! {
                pub fn #fn_name(&self) -> impl Iterator<Item = #syntax_name>  + use<'_> {
                    self.0.#getter_fn_name().filter_map(#syntax_name::cast)
                }
            }
        } else {
            quote! {
                pub fn #fn_name(&self) -> Option<#syntax_name> {
                    self.0.#getter_fn_name().filter_map(#syntax_name::cast).nth(#nth)
                }
            }
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Node {
    Items(SequenceNode),
    Choices(ChoiceNode),
}

impl Node {
    pub fn name(&self) -> String {
        match self {
            Node::Items(items) => items.name.clone(),
            Node::Choices(choices) => choices.name.clone(),
        }
    }

    pub fn is_all_token_choices(&self) -> bool {
        matches!(
            self,
            Node::Choices(ChoiceNode {
                name: _,
                items: NodesOrTokens::Tokens(_)
            })
        )
    }
}

impl From<SequenceNode> for Node {
    fn from(value: SequenceNode) -> Self {
        Node::Items(value)
    }
}

impl From<ChoiceNode> for Node {
    fn from(value: ChoiceNode) -> Self {
        Node::Choices(value)
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct SequenceNode {
    name: String,
    items: Vec<TokenOrNode>,
}

impl SequenceNode {
    pub fn new(name: impl Into<String>, items: Vec<TokenOrNode>) -> SequenceNode {
        SequenceNode {
            name: name.into().to_case(Case::UpperCamel),
            items,
        }
    }

    pub fn struct_name(&self) -> Ident {
        format_ident!("{}Syntax", self.name.to_case(Case::UpperCamel))
    }

    pub fn generate_rust_struct(&self) -> TokenStream {
        let name = self.struct_name();
        quote! {
            #[derive(Debug, Clone)]
            pub struct #name(pub(crate) SyntaxNode);
        }
    }

    pub fn generate_ast_node_rust_impl(&self) -> TokenStream {
        let struct_name = self.struct_name();
        let node_kind = format_ident!("{}", self.name.to_case(Case::UpperCamel));
        quote! {
            impl AstNode for #struct_name {
                fn cast(node: SyntaxNode) -> Option<Self> {
                    match node.kind() {
                        NodeKind::#node_kind => Some(#struct_name(node)),
                        _ => None,
                    }
                }
                fn can_cast(node: &SyntaxNode) -> bool {
                    matches!(node.kind(), NodeKind::#node_kind)
                }
                fn raw(&self) -> SyntaxNode {
                    self.0.clone()
                }
            }
        }
    }

    pub fn generate_rust_impl_getters(&self) -> TokenStream {
        let items: TokenStream = self
            .items
            .iter()
            .map(|item| item.generate_rust_getter())
            .collect();
        let name = self.struct_name();
        quote! {
            impl #name {
                #items
            }
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum NodesOrTokens {
    Nodes(Vec<NodeRef>),
    Tokens(Vec<Token>),
}

impl FromIterator<NodeRef> for NodesOrTokens {
    fn from_iter<T: IntoIterator<Item = NodeRef>>(iter: T) -> Self {
        NodesOrTokens::Nodes(iter.into_iter().collect())
    }
}

impl FromIterator<Token> for NodesOrTokens {
    fn from_iter<T: IntoIterator<Item = Token>>(iter: T) -> Self {
        NodesOrTokens::Tokens(iter.into_iter().collect())
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct ChoiceNode {
    pub name: String,
    pub items: NodesOrTokens,
}

impl ChoiceNode {
    pub fn enum_name(&self) -> Ident {
        format_ident!("{}Syntax", self.name.to_case(Case::UpperCamel))
    }

    fn enum_choices(&self) -> Vec<TokenStream> {
        match &self.items {
            NodesOrTokens::Nodes(nodes) => self.all_nodes_choices(nodes),
            NodesOrTokens::Tokens(tokens) => self.all_token_choices(tokens),
        }
    }

    fn all_nodes_choices(&self, items: &[NodeRef]) -> Vec<TokenStream> {
        items
            .iter()
            .map(|item| {
                let name = item.enum_variant_ident();
                let syntax_name = item.syntax_name();
                quote! {
                    #name(#syntax_name)
                }
            })
            .collect()
    }

    fn all_token_choices(&self, items: &[Token]) -> Vec<TokenStream> {
        items
            .iter()
            .map(|item| {
                let name = item.enum_variant_ident();
                quote! {
                    #name(SyntaxToken)
                }
            })
            .collect()
    }

    pub fn generate_rust_enum(&self) -> TokenStream {
        let name = self.enum_name();
        let choices = self.enum_choices();
        quote! {
            #[derive(Debug, Clone)]
            pub enum #name {
                #(#choices),*
            }
        }
    }

    pub fn generate_ast_node_rust_impl(&self) -> TokenStream {
        let enum_name = self.enum_name();
        match &self.items {
            NodesOrTokens::Nodes(nodes) => {
                let cast_branches: Vec<TokenStream> = nodes.iter().map(|item| {
                    let enum_variant = item.enum_variant_ident();
                    let syntax_name = item.syntax_name();
                    quote! {
                        if #syntax_name::can_cast(&node) {
                            // BlockStatementSyntax::cast(node).unwrap()
                            return Some(#enum_name::#enum_variant(#syntax_name::cast(node).unwrap()))
                        }
                    }
                }).collect();
                let can_cast_branches: Vec<TokenStream> = nodes.iter().map(|item| {
                    let syntax_name = item.syntax_name();
                    quote! {
                        #syntax_name::can_cast(node)
                    }
                }).collect();
                let raw_branches: Vec<TokenStream> = nodes
                    .iter()
                    .map(|item| {
                        let enum_variant = item.enum_variant_ident();
                        quote! {
                                #enum_name::#enum_variant(inner) => inner.raw()
                        }
                    })
                    .collect();
                quote! {
                    impl AstNode for #enum_name {
                        fn cast(node: SyntaxNode) -> Option<Self> {
                            #(#cast_branches ;)*
                            None
                        }
                        fn can_cast(node: &SyntaxNode) -> bool {
                            #(#can_cast_branches)||*
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
                        let token_kind = item.kind.build_expression();
                        let enum_variant = item.enum_variant_ident();
                        quote! {
                            #token_kind => Some(#enum_name::#enum_variant(token))
                        }
                    })
                    .collect();
                let raw_branches: Vec<_> = tokens
                    .iter()
                    .map(|item| {
                        let enum_variant = item.enum_variant_ident();
                        quote! {
                                #enum_name::#enum_variant(token) => token.clone()
                        }
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
}

impl Node {
    pub fn generate_rust_struct(&self) -> TokenStream {
        match self {
            Node::Items(items) => items.generate_rust_struct(),
            Node::Choices(choices) => choices.generate_rust_enum(),
        }
    }

    pub fn generate_ast_node_rust_impl(&self) -> TokenStream {
        match self {
            Node::Items(items) => items.generate_ast_node_rust_impl(),
            Node::Choices(choices) => choices.generate_ast_node_rust_impl(),
        }
    }

    pub fn generate_rust_impl_getters(&self) -> TokenStream {
        match self {
            Node::Items(items) => items.generate_rust_impl_getters(),
            Node::Choices(_) => quote! {},
        }
    }
}

#[derive(Debug, Default)]
pub struct Model {
    sections: HashMap<String, Vec<Node>>,
    builtins: HashSet<String>,
}

impl Model {
    pub fn push_node(&mut self, section: String, node: impl Into<Node>) {
        let new_node = node.into();
        if let Some(old_node) = self.all_nodes().find(|node| node.name() == new_node.name()) {
            assert_eq!(
                &new_node,
                old_node,
                "Node {} defined multiple, non-identical times",
                new_node.name()
            );
            return;
        }
        self.sections.entry(section).or_default().push(new_node);
    }

    pub fn push_builtin(&mut self, node: String) {
        self.builtins.insert(node);
    }

    pub fn sections(&self) -> &HashMap<String, Vec<Node>> {
        &self.sections
    }

    pub fn do_checks(&self) {
        self.check_no_duplicates();
        self.check_all_nodes_exist();
        self.check_choices_are_unique();
    }

    pub fn do_postprocessing(&mut self) {
        self.patch_all_token_choices();
    }

    pub fn patch_all_token_choices(&mut self) {
        let to_patch = self
            .all_nodes()
            .filter(|node| node.is_all_token_choices())
            .map(|node| node.name())
            .collect::<HashSet<_>>();
        for node in self.all_nodes_mut() {
            match node {
                Node::Items(sequence) => {
                    for item in &mut sequence.items {
                        match item {
                            TokenOrNode::Node(node_ref) if to_patch.contains(&node_ref.kind) => {
                                node_ref.is_token_node = true
                            }
                            _ => {}
                        }
                    }
                }
                Node::Choices(choice) => match &mut choice.items {
                    NodesOrTokens::Nodes(nodes) => {
                        for node_ref in nodes {
                            if to_patch.contains(&node_ref.name) {
                                node_ref.is_token_node = true
                            }
                        }
                    }
                    NodesOrTokens::Tokens(_) => {}
                },
            }
        }
    }

    pub fn check_no_duplicates(&self) {
        for (section, nodes) in &self.sections {
            for node in nodes {
                let mut seen = HashSet::new();
                match node {
                    Node::Items(seq_node) => {
                        for item in &seq_node.items {
                            if seen.contains(&item.getter_name()) {
                                panic!(
                                    "Duplicate node {} in node {} (section {})",
                                    item.getter_name(),
                                    node.name(),
                                    section
                                )
                            }
                            seen.insert(item.getter_name());
                        }
                    }
                    Node::Choices(choices_node) => match &choices_node.items {
                        NodesOrTokens::Nodes(nodes) => {
                            for item in nodes {
                                if seen.contains(&item.getter_name()) {
                                    panic!(
                                        "Duplicate node {} in node {}",
                                        item.getter_name(),
                                        node.name()
                                    )
                                }
                                seen.insert(item.getter_name());
                            }
                        }
                        NodesOrTokens::Tokens(tokens) => {
                            for item in tokens {
                                if seen.contains(&item.getter_name()) {
                                    panic!(
                                        "Duplicate node {} in node {}",
                                        item.getter_name(),
                                        node.name()
                                    )
                                }
                                seen.insert(item.getter_name());
                            }
                        }
                    },
                }
            }
        }
    }

    fn collect_referenced_nodes(&self) -> HashSet<String> {
        let mut referenced = HashSet::new();
        for node in self.sections.values().flatten() {
            match node {
                Node::Items(seq_node) => {
                    for item in &seq_node.items {
                        if let TokenOrNode::Node(node_ref) = item {
                            referenced.insert(node_ref.kind.clone());
                        }
                    }
                }
                Node::Choices(choices_node) => {
                    if let NodesOrTokens::Nodes(nodes) = &choices_node.items {
                        for node_ref in nodes {
                            referenced.insert(node_ref.kind.clone());
                        }
                    }
                }
            }
        }
        referenced
    }

    pub fn check_all_nodes_exist(&self) {
        let defined = self.collect_all_node_kinds();
        let referenced = self.collect_referenced_nodes();

        let referenced_not_defined: Vec<_> = referenced.difference(&defined).to_owned().collect();
        if !referenced_not_defined.is_empty() {
            println!("The following nodes are referenced, but not defined:");
            for node in referenced_not_defined {
                println!("{node}");
            }
            panic!()
        }

        let mut defined_not_referenced: HashSet<_> =
            defined.difference(&referenced).to_owned().collect();
        let top_node = "DesignFile".to_owned();
        // The top node should not be referenced
        assert!(
            defined_not_referenced.contains(&top_node),
            "'DesignFile' is not the top node (was referenced by some other production)"
        );
        defined_not_referenced.remove(&"DesignFile".to_owned());
        if !defined_not_referenced.is_empty() {
            println!("The following nodes are defined, but never referenced:");
            for node in defined_not_referenced {
                println!("{node}");
            }
            panic!()
        }
    }

    /// Check that all `Choice` nodes contain elements that are only reachable by this choice
    pub fn check_choices_are_unique(&self) {
        let mut found_nodes = HashSet::new();
        for node in self.all_nodes() {
            match node {
                Node::Items(_) => {}
                Node::Choices(choice) => match &choice.items {
                    NodesOrTokens::Nodes(nodes) => {
                        for node in nodes {
                            let used_sites = self.check_where_node_is_used(node);
                            if used_sites.len() > 1 && !found_nodes.contains(&node.name) {
                                found_nodes.insert(node.name.clone());
                                println!("Node {} is used multiple times, but must only be used in a single choice node", node.name);
                            }
                        }
                    }
                    NodesOrTokens::Tokens(_) => {}
                },
            }
        }
    }

    pub fn check_where_node_is_used(&self, orig_node: &NodeRef) -> Vec<NodeRef> {
        let mut referenced_nodes = vec![];
        for node in self.all_nodes() {
            match node {
                Node::Items(items) => {
                    for item in &items.items {
                        match item {
                            TokenOrNode::Node(node) => {
                                if node == orig_node {
                                    referenced_nodes.push(node.clone());
                                }
                            }
                            TokenOrNode::Token(_) => {}
                        }
                    }
                }
                Node::Choices(choices) => match &choices.items {
                    NodesOrTokens::Nodes(nodes) => {
                        for node in nodes {
                            if node == orig_node {
                                referenced_nodes.push(node.clone());
                            }
                        }
                    }
                    NodesOrTokens::Tokens(_) => {}
                },
            }
        }
        referenced_nodes
    }

    pub fn generate_mod(&self) -> TokenStream {
        let mut sections = self.sections.keys().collect::<Vec<_>>();
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
        }
    }

    pub fn collect_all_node_kinds(&self) -> HashSet<String> {
        self.all_nodes().map(|node| node.name()).collect()
    }

    pub fn collect_all_sequence_node_kinds(&self) -> HashSet<String> {
        self.all_nodes().filter(|node| matches!(node, Node::Items(_))).map(|node| node.name()).collect()
    }

    pub fn all_nodes(&self) -> impl Iterator<Item = &Node> {
        self.sections.values().flatten()
    }

    pub fn all_nodes_mut(&mut self) -> impl Iterator<Item = &mut Node> {
        self.sections.values_mut().flatten()
    }

    pub fn generate_node_kind_enum(&self) -> TokenStream {
        let mut choices = self
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
}
