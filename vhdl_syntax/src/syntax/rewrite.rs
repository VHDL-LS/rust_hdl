//! Facilities to rewrite a [SyntaxNode]
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::syntax::child::Child;
use crate::syntax::green::GreenNode;
use crate::syntax::node::SyntaxNode;

// TODO: should also support delete and add
pub enum RewriteAction {
    /// Leave the node as-is
    Leave,
    /// Change the node to a different one
    Change(SyntaxNode),
}

pub struct Rewriter<R: Fn(&SyntaxNode) -> RewriteAction> {
    rewrite_action: R,
}

impl<R: Fn(&SyntaxNode) -> RewriteAction> Rewriter<R> {
    pub fn new(rewrite_action: R) -> Self {
        Rewriter { rewrite_action }
    }

    pub fn rewrite(&self, syntax_node: SyntaxNode) -> SyntaxNode {
        SyntaxNode::new_root(self.rewrite_to_green(syntax_node))
    }

    fn rewrite_to_green(&self, syntax_node: SyntaxNode) -> GreenNode {
        match (self.rewrite_action)(&syntax_node) {
            RewriteAction::Leave => {
                let mut new_green_node = syntax_node.green().data().clone();
                for (i, child) in syntax_node.children().enumerate() {
                    match (self.rewrite_action)(&child) {
                        RewriteAction::Leave => {
                            // TODO: The offset is currently fixed at 0
                            new_green_node
                                .replace_child(i, Child::Node((0, self.rewrite_to_green(child))));
                        }
                        RewriteAction::Change(node) => {
                            new_green_node.replace_child(i, Child::Node((0, node.green().clone())));
                        }
                    }
                }
                GreenNode::new(new_green_node)
            }
            RewriteAction::Change(node) => node.green().clone(),
        }
    }
}
