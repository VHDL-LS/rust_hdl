//! AST elements, Syntax Tokens and methods to traverse and rewrite those.
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com
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
use crate::syntax::node::{SyntaxElement, SyntaxNode};
use crate::syntax::rewrite::RewriteAction;
use crate::syntax::validate::{check, ValidationError};
use crate::syntax::visitor::Preorder;
pub use generated::*;

pub trait AstNode
where
    Self: Sized,
{
    /// Static meta-information about this node's layout.
    const META: &'static Layout;

    /// Cast without a kind check — caller must ensure `can_cast` is true.
    /// Panics for Choice nodes (although this might change in the future)
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
            Layout::Choice(choice) => choice.options.contains(&node.kind()),
        }
    }

    /// Walk the tree according to the textual order.
    fn walk(&self) -> Preorder {
        Preorder::new(self.raw())
    }

    fn rewrite(&self, rewrite: impl Fn(&SyntaxElement) -> RewriteAction) -> Self {
        let result = self.raw().rewrite(rewrite);
        Self::cast_unchecked(result)
    }
}

/// A layer on top of the `AstNode` with guarantees that there are no missing or extraneous elements.
pub trait CheckedNode
where
    Self: Sized,
{
    /// Cast without a check. Caller has to ensure that there are no missing or extraneous elements in the provided node.
    fn cast_unchecked(node: SyntaxNode) -> Self;

    fn cast(node: SyntaxNode) -> Result<Self, ValidationError> {
        check(&node).map(|_| CheckedNode::cast_unchecked(node))
    }
}
