//! Public API for abstract and untyped nodes.
//!
//! Every language element is either represented by a [SyntaxToken] or a [SyntaxNode].
//! The SyntaxToken wraps a [Token](crate::tokens::Token) and therefore allows access
//! to the original source text and the kind of token. A SyntaxNode simply groups these tokens
//! and other sub-nodes to generate a tree - the abstract syntax tree.
//! For example, for the following design-file, the syntax tree might look like the following:
//! ```vhdl
//! entity foo is
//! end foo;
//! ```
//!
//! ```no-check
//!            DesignFile
//!                 |
//!          EntityDeclaration
//!                 |
//!     ------------------------------
//!     |      |    |     |     |    |
//! `Entity` `foo` `is` `end` `foo` `;`
//! ```
//!
//! # Tree traversal
//! Traversal cen be accommodated by the many methods on `SyntaxNode`, such as
//! [SyntaxNode::children], [SyntaxNode::parent] or [SyntaxNode::ancestors]. To traverse the tree
//! in textual pre-order, for example, to search for a node of a certain type, use
//! [Preorder](crate::syntax::Preorder).
//!
//! # Mutability
//! Once created, a [SyntaxNode] is immutable. To change the tree, for example, for refactorings,
//! use the [Rewriter]. This creates a new tree from the old tree while replacing selected nodes
//! with new ones.
//!
//! # Efficiency
//! This implementation takes inspiration from and is a mixture between
//! [rowan](https://github.com/rust-analyzer/rowan) and swift's
//! [libsyntax](https://github.com/swiftlang/swift-syntax). The basic idea is that there is
//! a simple, underlying tree (the green tree) that contains all relevant data. `SyntaxNode`s,
//! that allow for traversal of siblings or parents, are generated on-the fly while traversing
//! the tree. This allows the actual tree to be quite small, yet traversal is still reasonably
//! efficient.
//!
//! Note that currently the main effort is on creating a public and well-tested API.
//! Many known optimization possibilities are currently ignored.
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::syntax::child::Child;
use crate::syntax::green::{GreenNode, GreenToken};
use crate::syntax::node_kind::NodeKind;
use crate::syntax::rewrite::{RewriteAction, Rewriter};
use crate::tokens::{TokenKind, Trivia};
use std::fmt::{Display, Formatter};
use std::iter;
use std::sync::Arc;

type SyntaxElement = Child<SyntaxNode, SyntaxToken>;

#[derive(Clone, Debug)]
pub struct SyntaxToken(Arc<SyntaxTokenData>);

impl SyntaxToken {
    pub(crate) fn new(
        offset: usize,
        index: usize,
        parent: SyntaxNode,
        green: GreenToken,
    ) -> SyntaxToken {
        SyntaxToken(Arc::new(SyntaxTokenData {
            offset,
            index,
            parent,
            green,
        }))
    }

    pub fn byte_len(&self) -> usize {
        self.green().byte_len()
    }

    pub fn kind(&self) -> TokenKind {
        self.green().kind()
    }

    pub fn leading_trivia(&self) -> &Trivia {
        self.green().leading_trivia()
    }

    pub fn trailing_trivia(&self) -> &Trivia {
        self.green().trailing_trivia()
    }

    pub fn text(&self) -> &str {
        self.green().text()
    }

    pub fn parent(&self) -> SyntaxNode {
        self.0.parent.clone()
    }

    pub fn text_pos(&self) -> usize {
        self.0.offset
    }

    pub fn ancestors(&self) -> impl Iterator<Item = SyntaxNode> {
        iter::successors(Some(self.parent()), SyntaxNode::parent)
    }

    fn siblings(&self) -> impl Iterator<Item = SyntaxElement> + use<'_> {
        self.0.parent.children_with_tokens()
    }

    pub fn prev_sibling_or_token(&self) -> Option<SyntaxElement> {
        self.siblings().nth(self.index().checked_sub(1)?)
    }

    pub fn prev_token(&self) -> Option<SyntaxToken> {
        match self.prev_sibling_or_token() {
            Some(element) => element.last_token(),
            None => self
                .ancestors()
                .find_map(|it| it.prev_sibling_or_token())
                .and_then(|element| element.last_token()),
        }
    }

    pub(crate) fn green(&self) -> &GreenToken {
        &self.0.green
    }

    fn index(&self) -> usize {
        self.0.index
    }
}

impl Display for SyntaxToken {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.green())
    }
}

#[derive(Debug)]
pub struct SyntaxTokenData {
    offset: usize,
    index: usize,
    parent: SyntaxNode,
    green: GreenToken,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SyntaxNode(Arc<SyntaxNodeData>);

#[derive(Debug, Eq, PartialEq)]
pub struct SyntaxNodeData {
    offset: usize,
    index: usize,
    parent: Option<SyntaxNode>,
    green: GreenNode,
}

impl SyntaxNode {
    pub fn parent(&self) -> Option<SyntaxNode> {
        self.0.parent.clone()
    }

    pub fn kind(&self) -> NodeKind {
        self.0.green.kind()
    }

    pub fn offset(&self) -> usize {
        self.0.offset
    }

    pub fn byte_len(&self) -> usize {
        // TODO: This should be cached on the node
        self.children_with_tokens()
            .fold(0, |acc, next| acc + next.byte_len())
    }

    pub fn children_with_tokens(
        &self,
    ) -> impl Iterator<Item = Child<SyntaxNode, SyntaxToken>> + use<'_> {
        self.green()
            .children()
            .enumerate()
            .map(|(i, child)| match child {
                Child::Token((rel_offset, token)) => Child::Token(SyntaxToken::new(
                    self.offset() + rel_offset,
                    i,
                    self.clone(),
                    token.clone(),
                )),
                Child::Node((rel_offset, node)) => Child::Node(SyntaxNode::new_child(
                    self.offset() + rel_offset,
                    i,
                    self.clone(),
                    node.clone(),
                )),
            })
    }

    pub fn children(&self) -> impl Iterator<Item = SyntaxNode> + use<'_> {
        self.children_with_tokens()
            .filter_map(|child| child.as_node())
    }

    pub fn first_token(&self) -> Option<SyntaxToken> {
        self.children_with_tokens()
            .filter_map(|child| child.as_token())
            .next()
    }

    pub fn tokens(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.children_with_tokens()
            .filter_map(|element| element.as_token())
    }

    pub fn first_child(&self) -> Option<SyntaxNode> {
        self.children().next()
    }

    pub fn nth_child(&self, n: usize) -> Option<SyntaxNode> {
        self.children().nth(n)
    }

    pub fn nth_child_or_token(&self, n: usize) -> Option<Child<SyntaxNode, SyntaxToken>> {
        self.children_with_tokens().nth(n)
    }

    pub fn prev_sibling(&self) -> Option<Child<SyntaxNode, SyntaxToken>> {
        self.parent()?
            .nth_child_or_token(self.index().checked_sub(1)?)
    }

    pub fn next_sibling(&self) -> Option<SyntaxNode> {
        self.parent()?.nth_child(self.0.index + 1)
    }

    pub fn last_token(&self) -> Option<SyntaxToken> {
        self.last_child_or_token()?.last_token()
    }

    pub fn last_child_or_token(&self) -> Option<SyntaxElement> {
        self.children_with_tokens().last()
    }

    pub fn ancestors(&self) -> impl Iterator<Item = SyntaxNode> {
        iter::successors(Some(self.clone()), SyntaxNode::parent)
    }

    pub fn rewrite(&self, rewrite: impl Fn(&SyntaxNode) -> RewriteAction) -> SyntaxNode {
        Rewriter::new(rewrite).rewrite(self.clone())
    }

    pub(crate) fn new_root(green: GreenNode) -> SyntaxNode {
        SyntaxNode(Arc::new(SyntaxNodeData {
            offset: 0,
            index: 0,
            parent: None,
            green,
        }))
    }

    pub(crate) fn green(&self) -> &GreenNode {
        &self.0.green
    }

    fn new_child(offset: usize, index: usize, parent: SyntaxNode, green: GreenNode) -> SyntaxNode {
        SyntaxNode(Arc::new(SyntaxNodeData {
            offset,
            index,
            parent: Some(parent),
            green,
        }))
    }

    fn index(&self) -> usize {
        self.0.index
    }

    fn prev_sibling_or_token(&self) -> Option<SyntaxElement> {
        self.parent()?
            .children_with_tokens()
            .nth(self.index().checked_sub(1)?)
    }

    #[cfg(test)]
    pub(crate) fn test_text(&self) -> String {
        self.green().test_text(0)
    }
}

impl Display for SyntaxNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.green())
    }
}

impl SyntaxElement {
    pub fn last_token(&self) -> Option<SyntaxToken> {
        match self {
            Child::Token(token) => Some(token.clone()),
            Child::Node(node) => node.last_token(),
        }
    }

    pub fn offset(&self) -> usize {
        match self {
            Child::Token(token) => token.text_pos(),
            Child::Node(node) => node.offset(),
        }
    }

    pub fn byte_len(&self) -> usize {
        match self {
            Child::Token(token) => token.byte_len(),
            Child::Node(node) => node.byte_len(),
        }
    }
}
