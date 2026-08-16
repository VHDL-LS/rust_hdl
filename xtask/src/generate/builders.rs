// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2025, Lukas Scheller lukasscheller@icloud.com

use crate::generate::naming::{
    builder_ident, method_ident, node_kind_ident, syntax_type_ident, token_kind_path,
    token_type_ident,
};
use crate::generate::Generator;
use crate::model::{
    Cardinality, ChoiceNode, Field, ListNode, Model, Node, NodeKind, NodeOrTokenKind,
    NodesOrTokens, SequenceNode, TokenKind,
};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use std::collections::HashSet;

pub struct BuilderGenerator;

impl Generator for BuilderGenerator {
    fn name(&self) -> &str {
        "builders"
    }

    fn generate_files(&self, model: &Model) -> Vec<(String, TokenStream)> {
        let mut token_stream = quote! {
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

        // Regular builders (e.g., `struct EntityDeclarationBuilder`)
        token_stream.extend(
            sequence_nodes
                .iter()
                .map(|node| generate_builder(node, model, &defaultable)),
        );

        // List builders (e.g., `struct InterfaceListBuilder`)
        let mut list_nodes: Vec<&ListNode> = model
            .all_nodes()
            .filter_map(|n| match n {
                Node::List(list) => Some(list),
                _ => None,
            })
            .collect();
        list_nodes.sort_by_key(|n| &n.kind);

        token_stream.extend(
            list_nodes
                .iter()
                .map(|list| generate_list_builder(list, model)),
        );

        // Token builders (e.g., `struct ForceToken`)
        let mut choice_nodes: Vec<&ChoiceNode> = model
            .all_nodes()
            .filter_map(|n| match n {
                Node::Choices(c) if matches!(c.items, NodesOrTokens::Tokens(_)) => Some(c),
                _ => None,
            })
            .collect();
        choice_nodes.sort_by_key(|n| &n.name);

        // Token choice nodes (e.g., `ForceModeToken`)
        token_stream.extend(choice_nodes.iter().map(|c| {
            let NodesOrTokens::Tokens(tokens) = &c.items else {
                unreachable!()
            };
            generate_token_choice_token(&c.name, tokens)
        }));

        vec![("builders".to_string(), token_stream)]
    }
}

// MARK: Classification

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

/// Returns true when a sequence item can be default-constructed: optional and repeated items
/// default to absent/empty, tokens with canonical text to that text and node references to the
/// referenced node's own default (when it has one).
fn is_defaultable_item(item: &Field, defaultable: &HashSet<NodeKind>) -> bool {
    if item.may_be_absent() {
        return true;
    }
    match &item.kind {
        NodeOrTokenKind::Token(kind) => has_canonical_text(kind),
        NodeOrTokenKind::Node(kind) => defaultable.contains(kind),
    }
}

// MARK: Defaultable

/// Computes which `SequenceNode`s have builders whose `new()` takes zero arguments
/// (and therefore implement `Default`).
///
/// Because defaultability is self-referential we compute it via fixed-point iteration.
fn compute_defaultable_nodes(model: &Model) -> HashSet<NodeKind> {
    let mut defaultable: HashSet<NodeKind> = HashSet::new();

    loop {
        let prev_size = defaultable.len();

        for node in model.all_nodes() {
            if let Node::Items(seq) = node {
                if defaultable.contains(&seq.name) {
                    continue;
                }

                let is_defaultable = seq
                    .items
                    .iter()
                    .all(|item| is_defaultable_item(item, &defaultable));

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

/// Generates the `Token::new(...)` expression for a token that has canonical text.
fn token_default_expr(kind: &TokenKind) -> TokenStream {
    let kind_path = token_kind_path(kind);
    match kind {
        TokenKind::Keyword(kw) => {
            let kw_ident = format_ident!("{}", kw.to_string());
            quote! {
                Kw::#kw_ident.canonical_token()
            }
        }
        _ => quote! {
            #kind_path.canonical_token().unwrap()
        },
    }
}

// MARK: Trivia setter

/// Generates a `with_*_trivia(Trivia) -> Self` setter for a token field.
///
/// - **Required tokens**: mutates the stored `Token` directly.
/// - **Optional canonical tokens**: materialises the token from its canonical default when
///   `None`, then sets trivia — so the caller does not need a separate `with_*_token()` call
///   just to control spacing.
/// - **Optional non-canonical tokens**: only mutates when already `Some`; the user chooses
///   the value via the domain type's own `.with_trivia()` setter.
/// - **Repeated tokens**: returns an empty stream — no unambiguous single target.
fn generate_token_trivia_setter(item: &Field, kind: &TokenKind) -> TokenStream {
    let field = format_ident!("{}", item.getter_name());
    let with_trivia = format_ident!("with_{}_trivia", item.getter_name());

    match item.cardinality {
        Cardinality::Repeated => quote! {},
        Cardinality::Optional { .. } if has_canonical_text(kind) => {
            let default_expr = token_default_expr(kind);
            quote! {
                pub fn #with_trivia(mut self, trivia: Trivia) -> Self {
                    let tok = self.#field.get_or_insert_with(|| #default_expr);
                    tok.set_leading_trivia(trivia);
                    self
                }
            }
        }
        Cardinality::Optional { .. } => quote! {
            pub fn #with_trivia(mut self, trivia: Trivia) -> Self {
                if let Some(ref mut t) = self.#field {
                    t.set_leading_trivia(trivia);
                }
                self
            }
        },
        Cardinality::Required { .. } => quote! {
            pub fn #with_trivia(mut self, trivia: Trivia) -> Self {
                self.#field.set_leading_trivia(trivia);
                self
            }
        },
    }
}

// MARK: Builder

struct ItemDescriptor {
    field_decl: TokenStream,
    constructor_arg: Option<TokenStream>,
    field_init: TokenStream,
    setter: TokenStream,
    build_stmt: TokenStream,
}

fn describe_item(item: &Field, model: &Model, defaultable: &HashSet<NodeKind>) -> ItemDescriptor {
    let is_ctor_arg = !is_defaultable_item(item, defaultable);
    match &item.kind {
        NodeOrTokenKind::Token(token_kind) => {
            let field = format_ident!("{}", item.getter_name());

            let field_decl = match item.cardinality {
                Cardinality::Repeated => quote! { #field: Vec<Token> },
                Cardinality::Optional { .. } => quote! { #field: Option<Token> },
                Cardinality::Required { .. } => quote! { #field: Token },
            };

            let constructor_arg = if is_ctor_arg {
                if let Some(domain) = domain_type(token_kind) {
                    Some(quote! { #field: impl Into<#domain> })
                } else {
                    Some(quote! { #field: impl Into<Token> })
                }
            } else {
                None
            };

            // The type of the `Into<...>`. Either `Into<#domain_type>` for identifier, string literals, e.t.c.
            // or `Into<Token>` for everything else.
            let parameter_type = if let Some(domain) = domain_type(token_kind) {
                // Into<#domain_type>: convert once to the actual type (e.g., into `Identifier`), then into `Token`
                quote! { #domain }
            } else {
                // Into<Token>: convert once into `Token`
                quote! { Token }
            };

            let convert_into_token = if domain_type(token_kind).is_some() {
                // Into<#domain_type>: convert once to the actual type (e.g., into `Identifier`), then into `Token`
                quote! { into().into() }
            } else {
                // Into<Token>: convert once into `Token`
                quote! { into() }
            };

            let field_init = match item.cardinality {
                Cardinality::Repeated => quote! { #field: Vec::new() },
                Cardinality::Optional { .. } => quote! { #field: None },
                Cardinality::Required { .. } if is_ctor_arg => {
                    quote! { #field: #field.#convert_into_token }
                }
                Cardinality::Required { .. } => {
                    let default = token_default_expr(token_kind);
                    quote! { #field: #default }
                }
            };

            let mut setter = match item.cardinality {
                Cardinality::Repeated => {
                    let add = format_ident!("add_{}", item.getter_name());
                    quote! {
                        pub fn #add(mut self, t: impl Into<#parameter_type>) -> Self {
                            self.#field.push(t.#convert_into_token);
                            self
                        }
                    }
                }
                Cardinality::Optional { .. } => {
                    let with = format_ident!("with_{}", item.getter_name());
                    quote! {
                        pub fn #with(mut self, t: impl Into<#parameter_type>) -> Self {
                            self.#field = Some(t.#convert_into_token);
                            self
                        }
                    }
                }
                Cardinality::Required { .. } => {
                    let with = format_ident!("with_{}", item.getter_name());
                    quote! {
                        pub fn #with(mut self, t: impl Into<#parameter_type>) -> Self {
                            self.#field = t.#convert_into_token;
                            self
                        }
                    }
                }
            };
            setter.extend(generate_token_trivia_setter(item, token_kind));

            let build_stmt = match item.cardinality {
                Cardinality::Repeated => quote! {
                    for t in self.#field {
                        builder.push(t);
                    }
                },
                Cardinality::Optional { .. } => quote! {
                    if let Some(t) = self.#field {
                        builder.push(t);
                    }
                },
                Cardinality::Required { .. } => quote! { builder.push(self.#field); },
            };

            ItemDescriptor {
                field_decl,
                constructor_arg,
                field_init,
                setter,
                build_stmt,
            }
        }
        NodeOrTokenKind::Node(node_kind) => {
            let field = format_ident!("{}", item.getter_name());
            let ty = if model.is_token_choice(node_kind) {
                token_type_ident(node_kind)
            } else {
                syntax_type_ident(node_kind)
            };

            let field_decl = match item.cardinality {
                Cardinality::Repeated => quote! { #field: Vec<#ty> },
                Cardinality::Optional { .. } => quote! { #field: Option<#ty> },
                Cardinality::Required { .. } => quote! { #field: #ty },
            };

            let constructor_arg = if is_ctor_arg {
                Some(quote! { #field: impl Into<#ty> })
            } else {
                None
            };

            let field_init = match item.cardinality {
                Cardinality::Repeated => quote! { #field: Vec::new() },
                Cardinality::Optional { .. } => quote! { #field: None },
                Cardinality::Required { .. } if is_ctor_arg => quote! { #field: #field.into() },
                Cardinality::Required { .. } => {
                    let node_builder = builder_ident(node_kind);
                    quote! { #field: #node_builder::default().build() }
                }
            };

            let setter = match item.cardinality {
                Cardinality::Repeated => {
                    let add = format_ident!("add_{}", item.getter_name());
                    quote! {
                        pub fn #add(mut self, n: impl Into<#ty>) -> Self {
                            self.#field.push(n.into());
                            self
                        }
                    }
                }
                Cardinality::Optional { .. } => {
                    let with = format_ident!("with_{}", item.getter_name());
                    quote! {
                        pub fn #with(mut self, n: impl Into<#ty>) -> Self {
                            self.#field = Some(n.into());
                            self
                        }
                    }
                }
                Cardinality::Required { .. } => {
                    let with = format_ident!("with_{}", item.getter_name());
                    quote! {
                        pub fn #with(mut self, n: impl Into<#ty>) -> Self {
                            self.#field = n.into();
                            self
                        }
                    }
                }
            };

            // A token-choice child is a thin `XyzToken` wrapper around a raw token, so it is
            // pushed as a token; every other child contributes its own green node.
            let (push_bound, push_owned) = if model.is_token_choice(node_kind) {
                (
                    quote! { builder.push(n.0); },
                    quote! { builder.push(self.#field.0); },
                )
            } else {
                (
                    quote! { builder.push_node(n.raw().green().clone()); },
                    quote! { builder.push_node(self.#field.raw().green().clone()); },
                )
            };
            let build_stmt = match item.cardinality {
                Cardinality::Repeated => quote! {
                    for n in self.#field {
                        #push_bound
                    }
                },
                Cardinality::Optional { .. } => quote! {
                    if let Some(n) = self.#field {
                        #push_bound
                    }
                },
                Cardinality::Required { .. } => push_owned,
            };

            ItemDescriptor {
                field_decl,
                constructor_arg,
                field_init,
                setter,
                build_stmt,
            }
        }
    }
}

fn generate_builder(
    node: &SequenceNode,
    model: &Model,
    defaultable: &HashSet<NodeKind>,
) -> TokenStream {
    let builder = builder_ident(&node.name);
    let syntax = syntax_type_ident(&node.name);
    let kind = node_kind_ident(&node.name);

    let descriptors: Vec<ItemDescriptor> = node
        .items
        .iter()
        .map(|item| describe_item(item, model, defaultable))
        .collect();

    let fields: Vec<_> = descriptors.iter().map(|d| &d.field_decl).collect();
    let constructor_args: Vec<_> = descriptors
        .iter()
        .filter_map(|d| d.constructor_arg.as_ref())
        .collect();
    let field_inits: Vec<_> = descriptors.iter().map(|d| &d.field_init).collect();
    let setters: Vec<_> = descriptors.iter().map(|d| &d.setter).collect();
    let build_stmts: Vec<_> = descriptors.iter().map(|d| &d.build_stmt).collect();

    // Default impl (only when new() takes no args)
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

// MARK: List builder

/// How the builder for a list stores, accepts and emits one element.
struct ElementShape {
    /// The type a caller hands in, behind `impl Into<_>`.
    ty: TokenStream,
    /// How one stored element is pushed onto the `NodeBuilder`.
    push: TokenStream,
}

fn element_shape(element: &Field, model: &Model) -> ElementShape {
    match &element.kind {
        NodeOrTokenKind::Token(kind) => match domain_type(kind) {
            Some(domain) => ElementShape {
                ty: domain,
                push: quote! { builder.push(element.into()); },
            },
            None => ElementShape {
                ty: quote! { Token },
                push: quote! { builder.push(element); },
            },
        },
        // A token-choice child is a thin wrapper around a raw token.
        NodeOrTokenKind::Node(kind) if model.is_token_choice(kind) => {
            let ty = token_type_ident(kind);
            ElementShape {
                ty: quote! { #ty },
                push: quote! { builder.push(element.0); },
            }
        }
        NodeOrTokenKind::Node(kind) => {
            let ty = syntax_type_ident(kind);
            ElementShape {
                ty: quote! { #ty },
                push: quote! { builder.push_node(element.raw().green().clone()); },
            }
        }
    }
}

/// Generates the builder for a separated-list node.
///
/// The separator is synthesized from its canonical text and interleaved by `build()`, so a
/// caller only ever supplies elements and cannot get the ordering wrong. A list that may not
/// be empty takes its first element in `new()`, which is also what keeps it out of
/// [`compute_defaultable_nodes`].
fn generate_list_builder(list: &ListNode, model: &Model) -> TokenStream {
    let builder = builder_ident(&list.kind);
    let syntax = syntax_type_ident(&list.kind);
    let kind = node_kind_ident(&list.kind);

    let separator_kind = list
        .separator
        .as_token_kind()
        .unwrap_or_else(|| panic!("separator of list {} is not a token", list.kind));
    assert!(
        has_canonical_text(separator_kind),
        "separator {separator_kind:?} of list {} has no canonical text, so `build()` cannot \
         synthesize it",
        list.kind
    );
    let separator_expr = token_default_expr(separator_kind);

    let ElementShape { ty, push } = element_shape(&list.element, model);

    quote! {
        pub struct #builder {
            elements: Vec<#ty>,
        }

        impl #builder {
            pub fn new(first: impl Into<#ty>) -> Self {
                Self { elements: vec![first.into()] }
            }

            pub fn push(mut self, element: impl Into<#ty>) -> Self {
                self.elements.push(element.into());
                self
            }

            pub fn extend(mut self, elements: impl IntoIterator<Item = impl Into<#ty>>) -> Self {
                self.elements.extend(elements.into_iter().map(|e| e.into()));
                self
            }

            pub fn build(self) -> #syntax {
                let mut builder = NodeBuilder::new();
                builder.start_node(NodeKind::#kind);
                let mut first = true;
                for element in self.elements {
                    if !first {
                        // Trivia is leading, so whitespace *after* a separator belongs to the
                        // next element; the separator itself carries none.
                        let mut separator = #separator_expr;
                        separator.set_leading_trivia(Trivia::default());
                        builder.push(separator);
                    }
                    first = false;
                    #push
                }
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

// MARK: Token choice

/// Generates `pub struct XyzToken(pub(crate) Token)` with named constructors and
/// `From` impls for each token-choice choice node.
fn generate_token_choice_token(name: &NodeKind, tokens: &[TokenKind]) -> TokenStream {
    let token_name = token_type_ident(name);
    let syntax_name = syntax_type_ident(name);

    // For ForceModeToken: `fn in() -> ForceModeToken` and `fn out() -> ForceModeToken`
    let constructors: Vec<TokenStream> = tokens
        .iter()
        .map(|kind| {
            let method = method_ident(kind.default_name());
            if let Some(domain) = domain_type(kind) {
                quote! {
                    pub fn #method(v: impl Into<#domain>) -> Self {
                        Self(v.into().into())
                    }
                }
            } else {
                let expr = token_default_expr(kind);
                quote! {
                    pub fn #method() -> Self {
                        Self(#expr)
                    }
                }
            }
        })
        .collect();

    // For ForceModeToken: `impl From<ForceModeSyntax> for ForceModeToken`
    let from_syntax = quote! {
        impl From<#syntax_name> for #token_name {
            fn from(s: #syntax_name) -> Self {
                #token_name(s.raw().token().clone())
            }
        }
    };

    // For ForceModeToken: no impl.
    // For `LiteralToken`: From<BitStringLiteral>, From<CharLiteral>, From<StringLiteral>
    let from_domain_impls: Vec<TokenStream> = tokens
        .iter()
        .filter_map(|kind| {
            let domain = domain_type(kind)?;
            let method = method_ident(kind.default_name());
            Some(quote! {
                impl From<#domain> for #token_name {
                    fn from(v: #domain) -> Self {
                        #token_name::#method(v)
                    }
                }
            })
        })
        .collect();

    quote! {
        pub struct #token_name(pub(crate) Token);
        impl #token_name {
            #(#constructors)*
        }
        #from_syntax
        #(#from_domain_impls)*
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::token::TokenKind;
    use crate::model::{ChoiceNode, Field, Node, NodeKind, NodesOrTokens, SequenceNode};

    fn make_test_model() -> Model {
        let mut model = Model::default();

        // A token-choice node (no builder generated for this, but used as a child)
        let choice = ChoiceNode {
            name: NodeKind::from("RelOp"),
            items: NodesOrTokens::Tokens(vec![TokenKind::EQ, TokenKind::NE]),
        };
        model.push_node(Node::Choices(choice));

        // A simple sequence node: DesignFile -> [RelOp]
        let seq = SequenceNode::new("DesignFile", vec![Field::node("RelOp")]);
        model.push_node(Node::Items(seq));
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
                Field::token(TokenKind::SemiColon),
                Field::token(TokenKind::EQ),
            ],
        );
        model.push_node(Node::Items(leaf));

        // Parent: requires DesignFile (defaultable) plus an Identifier (not defaultable)
        let parent = SequenceNode::new(
            "ParentNode",
            vec![
                Field::node("DesignFile"),
                Field::token(TokenKind::Identifier).with_name("name"),
            ],
        );
        model.push_node(Node::Items(parent));
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
        let seq = SequenceNode::new("DesignFile", vec![Field::token(TokenKind::SemiColon)]);
        model.push_node(Node::Items(seq));
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
            vec![Field::token(TokenKind::SemiColon).make_optional()],
        );
        model.push_node(Node::Items(seq));
        model.do_postprocessing();

        let gen = BuilderGenerator;
        let code = gen.generate_files(&model)[0].1.to_string();

        assert!(
            code.contains("with_semi_colon_token_trivia"),
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
            vec![Field::token(TokenKind::SemiColon).make_repeated()],
        );
        model.push_node(Node::Items(seq));
        model.do_postprocessing();

        let gen = BuilderGenerator;
        let code = gen.generate_files(&model)[0].1.to_string();

        assert!(
            !code.contains("with_semi_colon_token_trivia"),
            "trivia setter should NOT be generated for repeated tokens:\n{code}"
        );
    }

    #[test]
    fn has_canonical_text_returns_false_for_identifier() {
        assert!(!has_canonical_text(&TokenKind::Identifier));
        assert!(!has_canonical_text(&TokenKind::AbstractLiteral));
        assert!(!has_canonical_text(&TokenKind::StringLiteral));
    }

    #[test]
    fn has_canonical_text_returns_true_for_keyword_and_symbols() {
        use crate::model::token::Keyword;
        assert!(has_canonical_text(&TokenKind::Keyword(Keyword::Entity)));
        assert!(has_canonical_text(&TokenKind::SemiColon));
        assert!(has_canonical_text(&TokenKind::Plus));
        assert!(has_canonical_text(&TokenKind::Eof));
    }
}
