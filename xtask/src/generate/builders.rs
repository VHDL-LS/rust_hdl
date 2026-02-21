// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2025, Lukas Scheller lukasscheller@icloud.com

use crate::generate::Generator;
use crate::model::{Model, Node, NodeRef, SequenceNode, Token, TokenKind, TokenOrNode};
use convert_case::{Case, Casing};
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use std::collections::HashSet;

pub struct BuilderGenerator;

impl Generator for BuilderGenerator {
    fn name(&self) -> &str {
        "builders"
    }

    fn generate_files(&self, model: &Model) -> Vec<(String, TokenStream)> {
        let imports = quote! {
            use super::*;
            use crate::builder::{AbstractLiteral, BitStringLiteral, CharLiteral, Identifier, StringLiteral};
            use crate::parser::builder::NodeBuilder;
            use crate::syntax::node::SyntaxNode;
            use crate::syntax::node_kind::NodeKind;
            use crate::syntax::AstNode;
            use crate::tokens::{Keyword as Kw, Token, TokenKind, Trivia, TriviaPiece};
        };

        // Compute which sequence nodes have builders whose new() takes zero args,
        // meaning they implement Default and can be auto-initialized.
        let defaultable = compute_defaultable_nodes(model);

        // Collect all sequence nodes, sorted for deterministic output.
        let mut sequence_nodes: Vec<&SequenceNode> =
            model.all_nodes().filter_map(Node::as_sequence).collect();
        sequence_nodes.sort_by_key(|n| &n.name);

        let builders: TokenStream = sequence_nodes
            .iter()
            .map(|node| generate_builder(node, model, &defaultable))
            .collect();

        let stream = quote! {
            #imports
            #builders
        };

        vec![("builders".to_string(), stream)]
    }
}

// MARK: Token classification

/// Returns true if this token kind has a fixed canonical text representation.
/// Returns false for tokens whose text depends on user input (identifiers, literals, etc.).
fn has_canonical_text(kind: &TokenKind) -> bool {
    !matches!(
        kind,
        TokenKind::Identifier
            | TokenKind::AbstractLiteral
            | TokenKind::StringLiteral
            | TokenKind::BitStringLiteral
            | TokenKind::CharacterLiteral
            | TokenKind::ToolDirective
            | TokenKind::Unterminated
            | TokenKind::Unknown
            | TokenKind::Eof
    )
}

/// Maps a `TokenKind` to its domain type path for builder method signatures.
///
/// Returns `None` for canonical tokens (keywords, symbols), which keep `impl Into<Token>`.
fn domain_type(kind: &TokenKind) -> Option<TokenStream> {
    match kind {
        TokenKind::Identifier => Some(quote! { crate::builder::Identifier }),
        TokenKind::AbstractLiteral => Some(quote! { crate::builder::AbstractLiteral }),
        TokenKind::StringLiteral => Some(quote! { crate::builder::StringLiteral }),
        TokenKind::BitStringLiteral => Some(quote! { crate::builder::BitStringLiteral }),
        TokenKind::CharacterLiteral => Some(quote! { crate::builder::CharLiteral }),
        _ => None,
    }
}

/// Returns true when a token must appear as a constructor argument.
fn is_constructor_arg_token(token: &Token) -> bool {
    !token.optional && !token.repeated && !has_canonical_text(&token.kind)
}

/// Returns true when a node ref must appear as a constructor argument.
fn is_constructor_arg_node(node_ref: &NodeRef, defaultable: &HashSet<String>) -> bool {
    !node_ref.optional && !node_ref.repeated && !defaultable.contains(&node_ref.kind)
}

// MARK: Defaultable node computation

/// Computes which `SequenceNode`s have builders whose `new()` takes zero arguments
/// (and therefore implement `Default`).
///
/// A node is defaultable when every item in its sequence is either:
/// - an optional or repeated token/node, or
/// - a required token with canonical text (keyword or symbol), or
/// - a required node whose target is itself defaultable.
///
/// Because defaultability is self-referential we compute it via fixed-point iteration.
fn compute_defaultable_nodes(model: &Model) -> HashSet<String> {
    let mut defaultable: HashSet<String> = HashSet::new();

    loop {
        let prev_size = defaultable.len();

        for node in model.all_nodes() {
            if let Node::Items(seq) = node {
                if defaultable.contains(&seq.name) {
                    continue;
                }

                let is_defaultable = seq.items.iter().all(|item| match item {
                    TokenOrNode::Token(token) => {
                        token.optional || token.repeated || has_canonical_text(&token.kind)
                    }
                    TokenOrNode::Node(node_ref) => {
                        node_ref.optional
                            || node_ref.repeated
                            || defaultable.contains(&node_ref.kind)
                    }
                });

                if is_defaultable {
                    defaultable.insert(seq.name.clone());
                }
            }
        }

        if defaultable.len() == prev_size {
            break; // fixed point reached
        }
    }

    defaultable
}

// MARK: Name helpers

fn builder_ident(name: &str) -> Ident {
    format_ident!("{}Builder", name.to_case(Case::UpperCamel))
}

fn syntax_type_ident(name: &str) -> Ident {
    format_ident!("{}Syntax", name.to_case(Case::UpperCamel))
}

fn node_kind_ident(name: &str) -> Ident {
    format_ident!("{}", name.to_case(Case::UpperCamel))
}

// MARK: Token default expression (code emitted into builders.rs)

/// Generates the `Token::new(...)` expression for a token that has canonical text.
fn token_default_expr(token: &Token) -> TokenStream {
    match &token.kind {
        TokenKind::Keyword(kw) => {
            let kw_ident = format_ident!("{}", kw.to_string());
            quote! {
                Token::new(
                    TokenKind::Keyword(Kw::#kw_ident),
                    Kw::#kw_ident.canonical_text(),
                    Trivia::from([TriviaPiece::Spaces(1)]),
                )
            }
        }
        _ => {
            let kind_ident = format_ident!("{}", token.kind.to_string());
            quote! {
                Token::new(
                    TokenKind::#kind_ident,
                    TokenKind::#kind_ident.canonical_text().unwrap(),
                    Trivia::from([TriviaPiece::Spaces(1)]),
                )
            }
        }
    }
}

// MARK: Trivia setter generation

/// Generates a `with_*_trivia(Trivia) -> Self` setter for a token field.
///
/// - **Required tokens**: mutates the stored `Token` directly.
/// - **Optional canonical tokens**: materialises the token from its canonical default when
///   `None`, then sets trivia — so the caller does not need a separate `with_*_token()` call
///   just to control spacing.
/// - **Optional non-canonical tokens**: only mutates when already `Some`; the user chooses
///   the value via the domain type's own `.with_trivia()` setter.
/// - **Repeated tokens**: returns an empty stream — no unambiguous single target.
fn generate_token_trivia_setter(token: &Token) -> TokenStream {
    if token.repeated {
        return quote! {};
    }
    let field = format_ident!("{}", token.getter_name());
    let with_trivia = format_ident!("with_{}_trivia", token.getter_name());

    if token.optional {
        if has_canonical_text(&token.kind) {
            let default_expr = token_default_expr(token);
            quote! {
                pub fn #with_trivia(mut self, trivia: Trivia) -> Self {
                    let tok = self.#field.get_or_insert_with(|| #default_expr);
                    tok.set_leading_trivia(trivia);
                    self
                }
            }
        } else {
            quote! {
                pub fn #with_trivia(mut self, trivia: Trivia) -> Self {
                    if let Some(ref mut t) = self.#field {
                        t.set_leading_trivia(trivia);
                    }
                    self
                }
            }
        }
    } else {
        quote! {
            pub fn #with_trivia(mut self, trivia: Trivia) -> Self {
                self.#field.set_leading_trivia(trivia);
                self
            }
        }
    }
}

// MARK: Builder generation

fn generate_builder(
    node: &SequenceNode,
    model: &Model,
    defaultable: &HashSet<String>,
) -> TokenStream {
    let builder = builder_ident(&node.name);
    let syntax = syntax_type_ident(&node.name);
    let kind = node_kind_ident(&node.name);

    // --- Field declarations ---
    let fields: Vec<TokenStream> = node
        .items
        .iter()
        .map(|item| match item {
            TokenOrNode::Token(token) => {
                let field = format_ident!("{}", token.getter_name());
                if token.repeated {
                    quote! { #field: Vec<Token> }
                } else if token.optional {
                    quote! { #field: Option<Token> }
                } else {
                    quote! { #field: Token }
                }
            }
            TokenOrNode::Node(node_ref) => {
                let field = format_ident!("{}", node_ref.getter_name());
                let ty = syntax_type_ident(&node_ref.kind);
                if node_ref.repeated {
                    quote! { #field: Vec<#ty> }
                } else if node_ref.optional {
                    quote! { #field: Option<#ty> }
                } else {
                    quote! { #field: #ty }
                }
            }
        })
        .collect();

    let constructor_args: Vec<TokenStream> = node
        .items
        .iter()
        .filter_map(|item| match item {
            TokenOrNode::Token(token) if is_constructor_arg_token(token) => {
                let field = format_ident!("{}", token.getter_name());
                if let Some(domain) = domain_type(&token.kind) {
                    Some(quote! { #field: impl Into<#domain> })
                } else {
                    Some(quote! { #field: impl Into<Token> })
                }
            }
            TokenOrNode::Node(node_ref) if is_constructor_arg_node(node_ref, defaultable) => {
                let field = format_ident!("{}", node_ref.getter_name());
                let ty = syntax_type_ident(&node_ref.kind);
                Some(quote! { #field: impl Into<#ty> })
            }
            _ => None
        })
        .collect();

    // --- Field initializers in new() ---
    let field_inits: Vec<TokenStream> = node
        .items
        .iter()
        .map(|item| match item {
            TokenOrNode::Token(token) => {
                let field = format_ident!("{}", token.getter_name());
                if token.repeated {
                    quote! { #field: Vec::new() }
                } else if token.optional {
                    quote! { #field: None }
                } else if is_constructor_arg_token(token) {
                    if domain_type(&token.kind).is_some() {
                        quote! { #field: #field.into().into() }
                    } else {
                        quote! { #field: #field.into() }
                    }
                } else {
                    let default = token_default_expr(token);
                    quote! { #field: #default }
                }
            }
            TokenOrNode::Node(node_ref) => {
                let field = format_ident!("{}", node_ref.getter_name());
                if node_ref.repeated {
                    quote! { #field: Vec::new() }
                } else if node_ref.optional {
                    quote! { #field: None }
                } else if is_constructor_arg_node(node_ref, defaultable) {
                    // Not defaultable — caller must supply this node.
                    quote! { #field: #field.into() }
                } else {
                    // Defaultable — auto-initialize via its builder's Default impl.
                    let node_builder = builder_ident(&node_ref.kind);
                    quote! { #field: #node_builder::default().build() }
                }
            }
        })
        .collect();

    // --- Setter methods ---
    let setters: Vec<TokenStream> = node
        .items
        .iter()
        .map(|item| match item {
            TokenOrNode::Token(token) => {
                let field = format_ident!("{}", token.getter_name());
                let value_setter = if token.repeated {
                    let add = format_ident!("add_{}", token.getter_name());
                    if let Some(domain) = domain_type(&token.kind) {
                        quote! {
                            pub fn #add(mut self, t: impl Into<#domain>) -> Self {
                                self.#field.push(t.into().into());
                                self
                            }
                        }
                    } else {
                        quote! {
                            pub fn #add(mut self, t: impl Into<Token>) -> Self {
                                self.#field.push(t.into());
                                self
                            }
                        }
                    }
                } else {
                    let with = format_ident!("with_{}", token.getter_name());
                    if token.optional {
                        if let Some(domain) = domain_type(&token.kind) {
                            quote! {
                                pub fn #with(mut self, t: impl Into<#domain>) -> Self {
                                    self.#field = Some(t.into().into());
                                    self
                                }
                            }
                        } else {
                            quote! {
                                pub fn #with(mut self, t: impl Into<Token>) -> Self {
                                    self.#field = Some(t.into());
                                    self
                                }
                            }
                        }
                    } else if let Some(domain) = domain_type(&token.kind) {
                        quote! {
                            pub fn #with(mut self, t: impl Into<#domain>) -> Self {
                                self.#field = t.into().into();
                                self
                            }
                        }
                    } else {
                        quote! {
                            pub fn #with(mut self, t: impl Into<Token>) -> Self {
                                self.#field = t.into();
                                self
                            }
                        }
                    }
                };
                let trivia_setter = generate_token_trivia_setter(token);
                quote! { #value_setter #trivia_setter }
            }
            TokenOrNode::Node(node_ref) => {
                let field = format_ident!("{}", node_ref.getter_name());
                let ty = syntax_type_ident(&node_ref.kind);
                if node_ref.repeated {
                    let add = format_ident!("add_{}", node_ref.getter_name());
                    quote! {
                        pub fn #add(mut self, n: #ty) -> Self {
                            self.#field.push(n);
                            self
                        }
                    }
                } else {
                    let with = format_ident!("with_{}", node_ref.getter_name());
                    if node_ref.optional {
                        quote! {
                            pub fn #with(mut self, n: impl Into<#ty>) -> Self {
                                self.#field = Some(n.into());
                                self
                            }
                        }
                    } else {
                        quote! {
                            pub fn #with(mut self, n: impl Into<#ty>) -> Self {
                                self.#field = n.into();
                                self
                            }
                        }
                    }
                }
            }
        })
        .collect();

    // --- build() body: push items in YAML order ---
    let build_stmts: Vec<TokenStream> = node
        .items
        .iter()
        .map(|item| match item {
            TokenOrNode::Token(token) => {
                let field = format_ident!("{}", token.getter_name());
                if token.repeated {
                    quote! {
                        for t in self.#field {
                            builder.push(t);
                        }
                    }
                } else if token.optional {
                    quote! {
                        if let Some(t) = self.#field {
                            builder.push(t);
                        }
                    }
                } else {
                    quote! { builder.push(self.#field); }
                }
            }
            TokenOrNode::Node(node_ref) => {
                let field = format_ident!("{}", node_ref.getter_name());
                // Token-choice nodes wrap a SyntaxToken, not a SyntaxNode.
                // Their .raw() returns SyntaxToken; push the underlying Token.
                if model.is_token_choice(&node_ref.kind) {
                    if node_ref.repeated {
                        quote! {
                            for n in self.#field {
                                builder.push(n.raw().token().clone());
                            }
                        }
                    } else if node_ref.optional {
                        quote! {
                            if let Some(n) = self.#field {
                                builder.push(n.raw().token().clone());
                            }
                        }
                    } else {
                        quote! { builder.push(self.#field.raw().token().clone()); }
                    }
                } else if node_ref.repeated {
                    quote! {
                        for n in self.#field {
                            builder.push_node(n.raw().green().clone());
                        }
                    }
                } else if node_ref.optional {
                    quote! {
                        if let Some(n) = self.#field {
                            builder.push_node(n.raw().green().clone());
                        }
                    }
                } else {
                    quote! { builder.push_node(self.#field.raw().green().clone()); }
                }
            }
        })
        .collect();

    // --- Default impl (only when new() takes no args) ---
    let default_impl = if constructor_args.is_empty() {
        quote! {
            impl Default for #builder {
                fn default() -> Self {
                    Self::new()
                }
            }
        }
    } else {
        quote! {}
    };

    quote! {
        pub struct #builder {
            #(#fields,)*
        }

        #default_impl

        impl #builder {
            pub fn new(#(#constructor_args,)*) -> Self {
                Self {
                    #(#field_inits,)*
                }
            }

            #(#setters)*

            pub fn build(self) -> #syntax {
                let mut builder = NodeBuilder::new();
                builder.start_node(NodeKind::#kind);
                #(#build_stmts)*
                builder.end_node();
                let green = builder.end();
                let node = SyntaxNode::new_root(green);
                #syntax::cast(node).unwrap()
            }
        }

        impl From<#builder> for #syntax {
            fn from(value: #builder) -> Self {
                value.build()
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::token::TokenKind;
    use crate::model::{
        ChoiceNode, Node, NodeRef, NodesOrTokens, SequenceNode, Token, TokenOrNode,
    };

    fn make_test_model() -> Model {
        let mut model = Model::default();

        // A token-choice node (no builder generated for this, but used as a child)
        let choice = ChoiceNode {
            name: "RelOp".to_string(),
            items: NodesOrTokens::Tokens(vec![
                Token::from(TokenKind::EQ),
                Token::from(TokenKind::NE),
            ]),
        };
        model.push_node("test".to_string(), Node::Choices(choice));

        // A simple sequence node: DesignFile -> [RelOp]
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

    /// Build a model with a "defaultable" leaf (only canonical-text tokens) and a parent
    /// that embeds it as a required node alongside a non-defaultable Identifier token.
    fn make_defaultable_model() -> Model {
        let mut model = Model::default();

        // Leaf: only tokens with canonical text (SemiColon, EQ) → defaultable
        let leaf = SequenceNode::new(
            "DesignFile",
            vec![
                TokenOrNode::Token(Token::from(TokenKind::SemiColon)),
                TokenOrNode::Token(Token::from(TokenKind::EQ)),
            ],
        );
        model.push_node("test".to_string(), Node::Items(leaf));

        // Parent: requires DesignFile (defaultable) plus an Identifier (not defaultable)
        let parent = SequenceNode::new(
            "ParentNode",
            vec![
                TokenOrNode::Node(NodeRef {
                    kind: "DesignFile".to_string(),
                    nth: 0,
                    builtin: false,
                    repeated: false,
                    name: "design_file".to_string(),
                    optional: false,
                }),
                TokenOrNode::Token(Token {
                    kind: TokenKind::Identifier,
                    name: "name".to_string(),
                    nth: 0,
                    repeated: false,
                    optional: false,
                }),
            ],
        );
        model.push_node("test".to_string(), Node::Items(parent));
        model.do_postprocessing();
        model
    }

    #[test]
    fn builder_generator_produces_builders_file() {
        let model = make_test_model();
        let gen = BuilderGenerator;
        let files = gen.generate_files(&model);
        assert_eq!(files.len(), 1);
        assert_eq!(files[0].0, "builders");
    }

    #[test]
    fn builder_generator_output_contains_builder_struct() {
        let model = make_test_model();
        let gen = BuilderGenerator;
        let files = gen.generate_files(&model);
        let code = files[0].1.to_string();
        assert!(
            code.contains("DesignFileBuilder"),
            "missing DesignFileBuilder in:\n{code}"
        );
        // RelOp is a choice node → no builder
        assert!(
            !code.contains("RelOpBuilder"),
            "RelOpBuilder should not be generated"
        );
    }

    #[test]
    fn defaultable_node_omitted_from_constructor() {
        let model = make_defaultable_model();
        let gen = BuilderGenerator;
        let files = gen.generate_files(&model);
        let code = files[0].1.to_string();

        // ParentNodeBuilder::new() should only take name_token (Identifier),
        // NOT design_file (DesignFile is defaultable).
        let parent_builder = code
            .split("pub struct ParentNodeBuilder")
            .nth(1)
            .expect("ParentNodeBuilder not found");
        // The auto-initialized DesignFile should appear as a default init, not an arg.
        assert!(
            parent_builder.contains("DesignFileBuilder :: default ()"),
            "defaultable node should be auto-initialized:\n{parent_builder}"
        );
    }

    #[test]
    fn defaultable_leaf_has_default_impl() {
        let model = make_defaultable_model();
        let defaultable = compute_defaultable_nodes(&model);
        assert!(
            defaultable.contains("DesignFile"),
            "DesignFile (all keywords) should be defaultable"
        );
        assert!(
            !defaultable.contains("ParentNode"),
            "ParentNode (has Identifier arg) should not be defaultable"
        );
    }

    #[test]
    fn trivia_setter_emitted_for_required_canonical_token() {
        let mut model = Model::default();
        let seq = SequenceNode::new(
            "DesignFile",
            vec![TokenOrNode::Token(Token::from(TokenKind::SemiColon))],
        );
        model.push_node("test".to_string(), Node::Items(seq));
        model.do_postprocessing();

        let gen = BuilderGenerator;
        let code = gen.generate_files(&model)[0].1.to_string();

        assert!(
            code.contains("with_semi_colon_token_trivia"),
            "expected trivia setter for required canonical token:\n{code}"
        );
    }

    #[test]
    fn trivia_setter_emitted_for_optional_canonical_token() {
        let mut model = Model::default();
        let seq = SequenceNode::new(
            "DesignFile",
            vec![TokenOrNode::Token(Token {
                kind: TokenKind::SemiColon,
                name: "semicolon".to_string(),
                nth: 0,
                repeated: false,
                optional: true,
            })],
        );
        model.push_node("test".to_string(), Node::Items(seq));
        model.do_postprocessing();

        let gen = BuilderGenerator;
        let code = gen.generate_files(&model)[0].1.to_string();

        assert!(
            code.contains("with_semicolon_token_trivia"),
            "expected trivia setter for optional canonical token:\n{code}"
        );
        assert!(
            code.contains("get_or_insert_with"),
            "optional canonical trivia setter should auto-initialise:\n{code}"
        );
    }

    #[test]
    fn trivia_setter_not_emitted_for_repeated_token() {
        let mut model = Model::default();
        let seq = SequenceNode::new(
            "DesignFile",
            vec![TokenOrNode::Token(Token {
                kind: TokenKind::SemiColon,
                name: "semicolon".to_string(),
                nth: 0,
                repeated: true,
                optional: false,
            })],
        );
        model.push_node("test".to_string(), Node::Items(seq));
        model.do_postprocessing();

        let gen = BuilderGenerator;
        let code = gen.generate_files(&model)[0].1.to_string();

        assert!(
            !code.contains("with_semicolon_token_trivia"),
            "trivia setter should NOT be generated for repeated tokens:\n{code}"
        );
    }

    #[test]
    fn has_canonical_text_returns_false_for_identifier() {
        assert!(!has_canonical_text(&TokenKind::Identifier));
        assert!(!has_canonical_text(&TokenKind::AbstractLiteral));
        assert!(!has_canonical_text(&TokenKind::StringLiteral));
        assert!(!has_canonical_text(&TokenKind::Eof));
    }

    #[test]
    fn has_canonical_text_returns_true_for_keyword_and_symbols() {
        use crate::model::token::Keyword;
        assert!(has_canonical_text(&TokenKind::Keyword(Keyword::Entity)));
        assert!(has_canonical_text(&TokenKind::SemiColon));
        assert!(has_canonical_text(&TokenKind::Plus));
    }
}
