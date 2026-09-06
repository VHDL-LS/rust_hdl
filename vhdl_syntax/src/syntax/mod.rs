// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

//! The syntax tree: Untyped nodes, and typed views.
//!
//! The result of parsing a file, or building a VHDL element is a tree of [`SyntaxNode`]s and
//! [`SyntaxElement`]s. Every node and every token has the same rust type, they are told apart at
//! runtime by their [`NodeKind`] or [`TokenKind`].
//! This enables generic tree traversal, rewriting, and pretty-printing.
//!
//! On top of the untyped tree sit generated `*Syntax` types, one per production of the VHDL grammar.
//! They are simply views of the untyped tree with typed accessors with cheap cloning and concurrent
//! read characteristics.  
//!
//! ```
//! use vhdl_syntax::parser;
//! use vhdl_syntax::syntax::visitor::WalkEvent;
//! use vhdl_syntax::syntax::{AstNode, EntityDeclarationSyntax, NodeKind};
//!
//! let (design, _) = parser::parse("\
//! entity foo is
//! end foo;
//! ");
//!
//! // Untyped: walk the tree and look at kinds.
//! let entity = design
//!     .walk()
//!     .find_map(|event| match event {
//!         WalkEvent::Enter(node) if node.kind() == NodeKind::EntityDeclaration => Some(node),
//!         _ => None,
//!     })
//!     .unwrap();
//!
//! // Typed: the same node, addressed by name.
//! let entity = EntityDeclarationSyntax::cast(entity).unwrap();
//! let name = entity
//!     .entity_declaration_preamble()
//!     .unwrap()
//!     .identifier_token()
//!     .unwrap();
//! assert_eq!(name.text(), "foo");
//! ```
//!
//! # Why is every accessor optional?
//!
//! Because the parser never fails, a tree may be missing anything: For example, `entity foo` parses, but the resulting
//! syntax node has no epilogue.

pub(crate) mod builder;
pub mod child;
#[allow(unused)]
mod generated;
pub(crate) mod green;
pub mod meta;
pub mod node;
pub mod rewrite;
pub mod validate;
pub mod visitor;

use crate::syntax::meta::Layout;
pub use crate::syntax::node::{SyntaxElement, SyntaxNode, SyntaxToken};
use crate::syntax::rewrite::RewriteAction;
use crate::syntax::visitor::Preorder;
pub use crate::tokens::TokenKind;
pub use generated::*;

pub trait AstNode
where
    Self: Sized,
{
    /// Static meta-information about this node's layout.
    const META: &'static Layout;

    /// Cast without a kind check — caller must ensure `can_cast` is true.
    fn cast_unchecked(node: SyntaxNode) -> Self;

    /// Return the underlying Syntax Node.
    fn raw(&self) -> SyntaxNode;

    /// Cast an abstract SyntaxNode into the AstNode described by `Self`.
    fn cast(node: SyntaxNode) -> Option<Self> {
        if Self::can_cast(&node) {
            Some(Self::cast_unchecked(node))
        } else {
            None
        }
    }

    /// Returns whether this AST node can successfully cast `node`.
    fn can_cast(node: &SyntaxNode) -> bool {
        match Self::META {
            Layout::Sequence(seq) => node.kind() == seq.kind,
            Layout::List(list) => node.kind() == list.kind,
            Layout::Choice(choice) => choice.options.contains(&node.kind()),
        }
    }

    /// Walk the tree according to the textual order.
    fn walk(&self) -> Preorder {
        Preorder::new(self.raw())
    }

    fn rewrite(&self, rewrite: impl FnMut(&SyntaxElement) -> RewriteAction) -> Self {
        let result = self.raw().rewrite(rewrite);
        Self::cast_unchecked(result)
    }
}
