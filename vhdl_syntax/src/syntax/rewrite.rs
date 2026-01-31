//! Facilities to rewrite a [SyntaxNode]
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::syntax::green::{GreenChild, GreenNode};
use crate::syntax::node::{SyntaxElement, SyntaxNode, SyntaxToken};

// TODO: should also support delete and add
pub enum RewriteAction {
    /// Leave the node as-is
    Leave,
    /// Change the node to a different one
    Change(SyntaxElement),
}

/// Facility to rewrite a `SyntaxNode` by re-building it while changing individual nodes.
///
/// Note that when calling `Rewrite` on a node, the node itself cannot change, only it's children
/// may.
pub struct Rewriter<R: Fn(&SyntaxElement) -> RewriteAction> {
    rewrite_action: R,
}

impl<R: Fn(&SyntaxElement) -> RewriteAction> Rewriter<R> {
    pub fn new(rewrite_action: R) -> Self {
        Rewriter { rewrite_action }
    }

    pub fn rewrite(&self, syntax_node: SyntaxNode) -> SyntaxNode {
        SyntaxNode::new_root(self.rewrite_node_to_green(syntax_node))
    }

    fn rewrite_element_to_green(&self, syntax_element: SyntaxElement) -> GreenChild {
        match (self.rewrite_action)(&syntax_element) {
            RewriteAction::Leave => match syntax_element {
                SyntaxElement::Node(node) => {
                    GreenChild::Node((node.offset(), self.rewrite_node_to_green(node)))
                }
                SyntaxElement::Token(token) => {
                    GreenChild::Token((token.offset(), token.green().clone()))
                }
            },
            RewriteAction::Change(element) => element.green(),
        }
    }

    fn rewrite_node_to_green(&self, syntax_node: SyntaxNode) -> GreenNode {
        let mut new_green_node = syntax_node.green().data().clone();
        for (i, child) in syntax_node.children_with_tokens().enumerate() {
            match (self.rewrite_action)(&child) {
                RewriteAction::Leave => {
                    new_green_node.replace_child(i, self.rewrite_element_to_green(child));
                }
                RewriteAction::Change(node) => {
                    new_green_node.replace_child(i, node.green());
                }
            }
        }
        GreenNode::new(new_green_node)
    }
}

pub struct TokenRewriter<R: TokenRewrite> {
    rewrite: R,
}

pub enum TokenRewriteAction {
    Keep,
    Replace(SyntaxToken),
}

pub trait TokenRewrite {
    /// Called when traversal enters a node.
    fn enter(&mut self, _node: &SyntaxNode) {}

    /// Called for each token in traversal order.
    fn token(&mut self, token: &SyntaxToken) -> TokenRewriteAction;

    /// Called when traversal exits a node.
    fn exit(&mut self, _node: &SyntaxNode) {}
}

impl<R: TokenRewrite> TokenRewriter<R> {
    pub fn new(rewrite: R) -> Self {
        TokenRewriter { rewrite }
    }

    pub fn rewrite(&mut self, syntax_node: SyntaxNode) -> SyntaxNode {
        SyntaxNode::new_root(self.rewrite_node_to_green(syntax_node))
    }

    fn rewrite_node_to_green(&mut self, syntax_node: SyntaxNode) -> GreenNode {
        self.rewrite.enter(&syntax_node);
        let mut new_green_node = syntax_node.green().data().clone();
        for (i, child) in syntax_node.children_with_tokens().enumerate() {
            match child {
                SyntaxElement::Node(node) => {
                    new_green_node.replace_child(
                        i,
                        GreenChild::Node((node.offset(), self.rewrite_node_to_green(node))),
                    );
                }
                SyntaxElement::Token(tok) => match self.rewrite.token(&tok) {
                    TokenRewriteAction::Keep => {}
                    TokenRewriteAction::Replace(syntax_token) => {
                        new_green_node.replace_child(i, SyntaxElement::Token(syntax_token).green());
                    }
                },
            }
        }
        self.rewrite.exit(&syntax_node);
        GreenNode::new(new_green_node)
    }
}
