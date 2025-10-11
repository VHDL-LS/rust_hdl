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
mod tests;
pub mod visitor;

use crate::parser::diagnostics::ParserDiagnostic;
use crate::parser::Parser;
use crate::syntax::node::SyntaxNode;
use crate::syntax::visitor::Preorder;
use crate::tokens::{IntoTokenStream, Tokenize};
pub use generated::*;
use std::str::FromStr;

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
}

impl FromStr for DesignFileSyntax {
    type Err = Vec<ParserDiagnostic>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parser = Parser::new(s.tokenize().into_token_stream());
        let (node, diagnostics) = parser.parse();
        if diagnostics.is_empty() {
            Ok(DesignFileSyntax(node))
        } else {
            Err(diagnostics)
        }
    }
}

impl FromStr for EntityDeclarationSyntax {
    type Err = Vec<ParserDiagnostic>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut parser = Parser::new(s.tokenize().into_token_stream());
        parser.entity();
        let (node, diagnostics) = parser.end();
        if diagnostics.is_empty() {
            Ok(EntityDeclarationSyntax(SyntaxNode::new_root(node)))
        } else {
            Err(diagnostics)
        }
    }
}
