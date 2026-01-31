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
pub mod node;
pub mod rewrite;
pub mod visitor;

use crate::syntax::node::{SyntaxElement, SyntaxNode};
use crate::syntax::rewrite::RewriteAction;
use crate::syntax::visitor::Preorder;
pub use generated::*;

pub trait AstNode
where
    Self: Sized,
{
    /// Cast an abstract SyntaxNode into the AstNode described by `Self`
    fn cast(node: SyntaxNode) -> Option<Self>;

    /// Returns whether this AST node can successfully cast the `node`
    fn can_cast(node: &SyntaxNode) -> bool;

    /// Return the underlying Syntax Node
    fn raw(&self) -> SyntaxNode;

    /// Walk the tree according to the textual order.
    fn walk(&self) -> Preorder {
        Preorder::new(self.raw())
    }

    fn rewrite(&self, rewrite: impl Fn(&SyntaxElement) -> RewriteAction) -> Self {
        let result = self.raw().rewrite(rewrite);
        Self::cast(result).expect("Invariant: rewrite cannot change type of self")
    }
}
