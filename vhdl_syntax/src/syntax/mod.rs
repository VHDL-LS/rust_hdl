//! AST elements, Syntax Tokens and methods to traverse and rewrite those.
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com
pub mod child;
pub mod design;
pub mod entity;
pub(crate) mod green;
pub mod node;
pub mod node_kind;
pub mod rewrite;
pub mod visitor;

use crate::syntax::node::SyntaxNode;
use crate::syntax::visitor::Preorder;

pub trait AstNode
where
    Self: Sized,
{
    /// Cast an abstract SyntaxNode into the AstNode described by `Self`
    fn cast(node: SyntaxNode) -> Option<Self>;

    /// Return the underlying Syntax Node
    fn raw(&self) -> SyntaxNode;

    /// Walk the tree according to the textual order.
    fn walk(&self) -> Preorder {
        Preorder::new(self.raw())
    }
}
