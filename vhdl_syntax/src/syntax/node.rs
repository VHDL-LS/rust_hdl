// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

//! Public API for abstract and untyped nodes.
//!
//! Every language element is either represented by a [SyntaxToken] or a [SyntaxNode].
//! The SyntaxToken wraps a [Token] and therefore allows access
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

use crate::latin_1::{Latin1Str, Latin1String, Utf8ToLatin1Error};
use crate::syntax::child::Child;
use crate::syntax::green::{GreenChild, GreenNode, GreenToken};
use crate::syntax::node_kind::NodeKind;
use crate::syntax::rewrite::{RewriteAction, Rewriter};
use crate::tokens::{Token, TokenKind, Trivia};
use std::fmt::Debug;
use std::io::{self, Write};
use std::iter;
use std::ops::Range;
use std::sync::Arc;

/// A union of either a child or a token
pub type SyntaxElement = Child<SyntaxNode, SyntaxToken>;

impl SyntaxElement {
    pub(crate) fn green(&self) -> GreenChild {
        match self {
            SyntaxElement::Node(node) => GreenChild::Node(node.green().clone()),
            SyntaxElement::Token(token) => GreenChild::Token(token.green().clone()),
        }
    }
}

impl SyntaxElement {
    pub fn next_sibling_or_token(&self) -> Option<SyntaxElement> {
        match self {
            Child::Node(node) => node.next_sibling_or_token(),
            Child::Token(token) => token.next_sibling_or_token(),
        }
    }

    pub fn parent(&self) -> Option<SyntaxNode> {
        match self {
            Child::Node(node) => node.parent(),
            Child::Token(token) => Some(token.parent()),
        }
    }
}

impl From<SyntaxNode> for SyntaxElement {
    fn from(value: SyntaxNode) -> Self {
        Self::Node(value)
    }
}

impl From<SyntaxToken> for SyntaxElement {
    fn from(value: SyntaxToken) -> Self {
        Self::Token(value)
    }
}

/// SyntaxTokens, in conjunction with [SyntaxNode]s
/// are the building blocks of the concrete syntax tree.
#[derive(Clone, Eq, PartialEq)]
pub struct SyntaxToken(Arc<SyntaxTokenData>);

impl Debug for SyntaxToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SyntaxToken")
            .field("kind", &self.green().kind())
            .field("leading_trivia", &self.green().leading_trivia())
            .field("text", &self.green().text())
            .finish()
    }
}

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

    /// Returns the offset in bytes of this token.
    /// This includes leading trivia.
    pub fn offset(&self) -> usize {
        self.0.offset
    }

    /// Returns the offset of the text-portion only.
    /// This is similar to [SyntaxToken::offset], but excludes leading trivia.
    pub fn text_offset(&self) -> usize {
        self.offset() + self.leading_trivia().byte_len()
    }

    /// Returns the full byte range covered by this token, including its leading trivia.
    ///
    /// Use this for tree traversal, where every byte must belong to exactly one token.
    /// For user-facing spans (diagnostics, highlights, hover), prefer [`text_range`](Self::text_range).
    pub fn range(&self) -> Range<usize> {
        self.offset()..self.offset() + self.byte_len()
    }

    /// Returns the byte range of this token's text, excluding leading trivia.
    ///
    /// This is the span a user perceives as "the token" — what to point at in diagnostics
    /// or highlight on hover. Gaps between successive `text_range`s contain trivia
    /// (whitespace and comments). For the full extent including trivia, see [`range`](Self::range).
    pub fn text_range(&self) -> Range<usize> {
        self.text_offset()..self.offset() + self.byte_len()
    }

    /// Returns the token associated to this `SyntaxToken`
    pub fn token(&self) -> &Token {
        self.green().token()
    }

    /// Returns the length, in bytes, of this token (including leading trivia)
    pub fn byte_len(&self) -> usize {
        self.green().byte_len()
    }

    /// Returns the token-kind
    pub fn kind(&self) -> TokenKind {
        self.green().kind()
    }

    /// Returns all trivia between this token and the previous one, resp. only the leading trivia
    /// of this token, if there is no previous token.
    pub fn leading_trivia(&self) -> &Trivia {
        self.green().leading_trivia()
    }

    /// Returns all trailing trivia between this token and the next one, resp. only the trailing
    /// trivia of this token, if there is no next token.
    /// TODO: After trivia-interning, we should be able to return `&Trivia` here, similar to `leading_trivia`
    pub fn trailing_trivia(&self) -> Trivia {
        self.next_token()
            .map(|tok| tok.leading_trivia().to_owned())
            .unwrap_or_default()
    }

    /// Returns the token-text, stripped of any trivia
    pub fn text(&self) -> &Latin1Str {
        self.green().text()
    }

    /// Returns the parent node that contains this token
    pub fn parent(&self) -> SyntaxNode {
        self.0.parent.clone()
    }

    pub fn ancestors(&self) -> impl Iterator<Item = SyntaxNode> {
        iter::successors(Some(self.parent()), SyntaxNode::parent)
    }

    fn siblings(&self) -> impl Iterator<Item = SyntaxElement> + use<'_> {
        self.0.parent.children_with_tokens()
    }

    /// Returns the previous child or token.
    ///
    /// # Example
    ///
    /// If the previous element is a node, `prev_sibling` selects the node
    ///
    /// ```text
    /// Parent
    /// ├── Previous node  <- prev_sibling()
    /// │   └── "previous"
    /// └── Self           <- self
    ///     └── "current"
    /// ```
    ///
    /// If the previous element is a token, `prev_sibling` selects the token
    ///
    /// ```text
    /// Parent
    /// ├── "previous"  <- prev_sibling()
    /// └── Self           <- self
    ///     └── "current"
    /// ```
    pub fn prev_sibling_or_token(&self) -> Option<SyntaxElement> {
        self.siblings().nth(self.index().checked_sub(1)?)
    }

    /// Returns the next child or token.
    ///
    /// # Example
    ///
    /// If `self` is the second child, `prev_sibling` selects the first element
    ///
    /// ```text
    /// Parent
    /// ├── Previous node  <- prev_sibling()
    /// │   └── "previous"
    /// └── Self           <- self
    ///     └── "current"
    /// ```
    pub fn next_sibling_or_token(&self) -> Option<SyntaxElement> {
        self.siblings().nth(self.index().checked_add(1)?)
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

    pub fn next_token(&self) -> Option<SyntaxToken> {
        match self.next_sibling_or_token() {
            Some(element) => element.first_token(),
            None => self
                .ancestors()
                .find_map(|node| node.next_sibling_or_token())
                .and_then(|element| element.first_token()),
        }
    }

    pub fn clone_with_text(&self, text: impl AsRef<Latin1Str>) -> SyntaxToken {
        let token = Token::new(self.kind(), text, self.green().leading_trivia().clone());
        self.clone_with_token(token)
    }

    pub fn clone_with_leading_trivia(&self, trivia: Trivia) -> SyntaxToken {
        let token = Token::new(self.kind(), self.green().text(), trivia);
        self.clone_with_token(token)
    }

    /// Clones all content (trivia, kind) of this token but changes the textual representation.
    ///
    /// If the text is not valid `Latin1`, a `Utf8ToLatin1Error` will be returned.
    pub fn clone_with_utf8_text(
        &self,
        text: impl AsRef<str>,
    ) -> Result<SyntaxToken, Utf8ToLatin1Error> {
        Ok(self.clone_with_text(Latin1String::from_utf8(text.as_ref())?))
    }

    pub fn clone_with_token(&self, token: Token) -> SyntaxToken {
        let green = GreenToken::new(token);
        SyntaxToken::new(self.offset(), self.index(), self.parent(), green)
    }

    pub(crate) fn green(&self) -> &GreenToken {
        &self.0.green
    }

    fn index(&self) -> usize {
        self.0.index
    }

    pub fn write_to(&self, writer: &mut impl Write) -> io::Result<()> {
        self.green().write_to(writer)
    }

    /// Returns `true` when this token is the last child of its parent
    pub fn is_first_sibling(&self) -> bool {
        self.prev_sibling_or_token().is_none()
    }

    /// Returns `true` when this token is the last child of its parent
    pub fn is_last_sibling(&self) -> bool {
        self.next_sibling_or_token().is_none()
    }

    /// Returns `true` if `offset` lies within [`range`](Self::range), i.e. within this
    /// token or its leading trivia. Use when descending the tree.
    pub fn contains_offset(&self, offset: usize) -> bool {
        self.range().contains(&offset)
    }

    /// Returns `true` if `offset` lies within [`text_range`](Self::text_range), i.e. on
    /// the token's text and not in surrounding trivia. Use for cursor-on-token queries.
    pub fn text_contains_offset(&self, offset: usize) -> bool {
        self.text_range().contains(&offset)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct SyntaxTokenData {
    offset: usize,
    index: usize,
    parent: SyntaxNode,
    green: GreenToken,
}

/// SyntaxNodes, in conjunction with [SyntaxToken]s
/// are the building blocks of the concrete syntax tree.
/// Every syntax node carries
/// - the [NodeKind], e.g., `EntityDeclaration`, `Name`, ...
/// - One or more children where each child is either another node or a token
/// - its position in the tree: the parent, and an index into the parent node
///
/// ## Tree traversal
/// Since a syntax node carries its parent and an index into the parent,
/// the tree can be traversed: children of this node, siblings, and parents can be visited from this node.
/// Use, for example, the [SyntaxNode::parent], [SyntaxNode::children], [SyntaxNode::prev_sibling], [SyntaxNode::next_sibling] methods
///
/// ## Construction
/// Syntax nodes are never constructed by the user directly.
/// Instead, they are either produced by the [parser](crate::parser),
/// or by one of the [builders](crate::syntax::builders)
#[derive(Clone, Eq, PartialEq)]
pub struct SyntaxNode(Arc<SyntaxNodeData>);

impl Debug for SyntaxNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SyntaxNode")
            .field("kind", &self.kind())
            .field("children", &self.children_with_tokens().collect::<Vec<_>>())
            .finish()
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct SyntaxNodeData {
    offset: usize,
    index: usize,
    parent: Option<SyntaxNode>,
    green: GreenNode,
}

impl SyntaxNode {
    /// Returns the parent, i.e., the syntax node that has this as a direct child.
    pub fn parent(&self) -> Option<SyntaxNode> {
        self.0.parent.clone()
    }

    /// Returns the kind of this node
    pub fn kind(&self) -> NodeKind {
        self.0.green.kind()
    }

    /// Returns the absolute byte-offset of this syntax node from the origin
    pub fn offset(&self) -> usize {
        self.0.offset
    }

    /// Returns the length, in bytes, of the text contained within this syntax nodes
    pub fn byte_len(&self) -> usize {
        self.green().byte_len()
    }

    /// Produces an iterator over all direct children of this node in lexicographical order
    /// (i.e., as written in the source text).
    /// As opposed of simply [SyntaxNode::children], this includes tokens too.
    pub fn children_with_tokens(
        &self,
    ) -> impl Iterator<Item = Child<SyntaxNode, SyntaxToken>> + use<'_> {
        let parent_offset = self.offset();
        self.green()
            .children()
            .enumerate()
            .scan(0usize, move |run, (i, child)| {
                let child_offset = parent_offset + *run;
                *run += child.byte_len();
                Some(match child {
                    Child::Token(t) => {
                        Child::Token(SyntaxToken::new(child_offset, i, self.clone(), t.clone()))
                    }
                    Child::Node(n) => Child::Node(SyntaxNode::new_child(
                        child_offset,
                        i,
                        self.clone(),
                        n.clone(),
                    )),
                })
            })
    }

    /// Produces an iterator over all direct (i.e., not nested) child syntax nodes.
    /// To iterate over all child nodes and all tokens use [SyntaxNode::children_with_tokens]
    pub fn children(&self) -> impl Iterator<Item = SyntaxNode> + use<'_> {
        self.children_with_tokens()
            .filter_map(|child| child.as_node())
    }

    /// Returns an iterator over all direct (i.e., not nested) child tokens.
    /// To iterate over all child nodes and all tokens use [SyntaxNode::children_with_tokens]
    pub fn tokens(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.children_with_tokens()
            .filter_map(|element| element.as_token())
    }

    /// Return the first token of this node.
    /// Note that this method searches deep, i.e., also considers tokens of sub-nodes
    // TODO: drop the `Option` as `SyntaxNode`s cannot be empty.
    pub fn first_token(&self) -> Option<SyntaxToken> {
        self.children_with_tokens()
            .filter_map(|node| match node {
                SyntaxElement::Node(n) => n.first_token(),
                SyntaxElement::Token(t) => Some(t),
            })
            .next()
    }

    /// Returns the first child-node of this node.
    // TODO: drop the `Option` as `SyntaxNode`s cannot be empty.
    pub fn first_child(&self) -> Option<SyntaxNode> {
        self.children().next()
    }

    /// Returns the first element in this node, i.e, a `SyntaxToken` if its a token
    /// or a `SyntaxNode` if its a node
    // TODO: drop the `Option` as `SyntaxNode`s cannot be empty.
    pub fn first_child_or_token(&self) -> Option<SyntaxElement> {
        self.children_with_tokens().next()
    }

    /// Returns the `nth` child (i.e., syntax node) of this node
    pub fn nth_child(&self, n: usize) -> Option<SyntaxNode> {
        self.children().nth(n)
    }

    /// Returns the `nth` child or token of this node
    pub fn nth_child_or_token(&self, n: usize) -> Option<SyntaxElement> {
        self.children_with_tokens().nth(n)
    }

    /// Returns the previous sibling node
    pub fn prev_sibling(&self) -> Option<SyntaxNode> {
        self.parent()?.nth_child(self.index().checked_sub(1)?)
    }

    /// Returns the next sibling node
    pub fn next_sibling(&self) -> Option<SyntaxNode> {
        self.parent()?.nth_child(self.0.index + 1)
    }

    /// Returns the previous child or token.
    ///
    /// # Example
    ///
    /// If the previous element is a node, `prev_sibling` selects the node
    ///
    /// ```text
    /// Parent
    /// ├── Previous node  <- prev_sibling()
    /// │   └── "previous"
    /// └── Self           <- self
    ///     └── "current"
    /// ```
    ///
    /// If the previous element is a token, `prev_sibling` selects the token
    ///
    /// ```text
    /// Parent
    /// ├── "previous"  <- prev_sibling()
    /// └── Self           <- self
    ///     └── "current"
    /// ```
    pub fn prev_sibling_or_token(&self) -> Option<SyntaxElement> {
        self.parent()?
            .children_with_tokens()
            .nth(self.index().checked_sub(1)?)
    }

    /// Returns the next child or token.
    ///
    /// # Example
    ///
    /// If `self` is the second child, `prev_sibling` selects the first element
    ///
    /// ```text
    /// Parent
    /// ├── Previous node  <- prev_sibling()
    /// │   └── "previous"
    /// └── Self           <- self
    ///     └── "current"
    /// ```
    pub fn next_sibling_or_token(&self) -> Option<SyntaxElement> {
        self.parent()?
            .children_with_tokens()
            .nth(self.index().checked_add(1)?)
    }

    /// Returns the last token of the direct children of this node
    // TODO: drop Option
    pub fn last_token(&self) -> Option<SyntaxToken> {
        self.last_child_or_token()?.last_token()
    }

    /// Returns the last child or token of the direct children of this node
    // TODO: drop Option
    pub fn last_child_or_token(&self) -> Option<SyntaxElement> {
        self.children_with_tokens().last()
    }

    /// Return an iterator over all ancestors of this node, i.e., the parent,
    /// the parent of the parent, e.t.c.
    pub fn ancestors(&self) -> impl Iterator<Item = SyntaxNode> {
        iter::successors(Some(self.clone()), SyntaxNode::parent)
    }

    /// Rewrite this node, i.e., change selected elements and produce a different `SyntaxNode`
    pub fn rewrite(&self, rewrite: impl FnMut(&SyntaxElement) -> RewriteAction) -> SyntaxNode {
        Rewriter::new(rewrite).rewrite(self.clone())
    }

    /// Rewrite nodes. Like [SyntaxNode::rewrite], but only called on nodes
    pub fn rewrite_nodes(&self, rewrite: impl Fn(&SyntaxNode) -> RewriteAction) -> SyntaxNode {
        Rewriter::new(|element| match element {
            SyntaxElement::Node(node) => rewrite(node),
            SyntaxElement::Token(_) => RewriteAction::Leave,
        })
        .rewrite(self.clone())
    }

    /// Rewrite tokens. Like [SyntaxNode::rewrite], but only called on tokens
    pub fn rewrite_tokens(&self, rewrite: impl Fn(&SyntaxToken) -> RewriteAction) -> SyntaxNode {
        Rewriter::new(|element| match element {
            SyntaxElement::Node(_) => RewriteAction::Leave,
            SyntaxElement::Token(token) => rewrite(token),
        })
        .rewrite(self.clone())
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

    #[cfg(test)]
    pub(crate) fn test_text(&self) -> String {
        self.green().test_text(0)
    }

    pub fn write_to(&self, writer: &mut impl Write) -> io::Result<()> {
        self.green().write_to(writer)
    }

    /// Returns the full byte range covered by this node.
    pub fn range(&self) -> Range<usize> {
        self.offset()..self.offset() + self.byte_len()
    }

    /// Returns `true` if `offset` lies within [`range`](Self::range).
    pub fn contains_offset(&self, offset: usize) -> bool {
        self.range().contains(&offset)
    }

    // TODO: Add binary search capabilitiy.
    // children are ordered by their text position.
    // We can make use of this to make finding a token more efficiently.

    /// Descends from this node, at each level taking the first child matching
    /// `predicate`, and returns the token reached at the leaf.
    ///
    /// Returns `None` if no child matches at some level along the descent.
    pub fn find_token(&self, predicate: impl Fn(&SyntaxElement) -> bool) -> Option<SyntaxToken> {
        let mut current: SyntaxNode = self.clone();
        loop {
            let child = current.children_with_tokens().find(&predicate)?;
            match child {
                Child::Token(tok) => return Some(tok),
                Child::Node(node) => current = node,
            }
        }
    }

    /// Returns the token whose [`text_range`](SyntaxToken::text_range) contains `offset`.
    ///
    /// Returns `None` if `offset` lies in trivia (whitespace or comments) or
    /// outside this subtree. For a total variant that always returns a token,
    /// see [`covering_token_at_offset`](Self::covering_token_at_offset).
    ///
    /// At a token/token boundary with no trivia between, `offset` is attributed
    /// to the following token (half-open ranges).
    pub fn token_at_offset(&self, offset: usize) -> Option<SyntaxToken> {
        self.find_token(|child| match child {
            Child::Node(node) => node.contains_offset(offset),
            Child::Token(tok) => tok.text_contains_offset(offset),
        })
    }

    /// Returns the token whose [`range`](SyntaxToken::range) (including leading
    /// trivia) contains `offset`, clamping to the first or last token when
    /// `offset` lies outside this subtree.
    ///
    /// Unlike [`token_at_offset`](Self::token_at_offset), an offset inside
    /// trivia is attributed to the following token rather than returning
    /// `None`. Useful when a caller needs some token to anchor an action on
    /// (e.g. a cursor position in an editor).
    ///
    /// # Panics
    ///
    /// Panics if this node contains no tokens. The builder API guarantees that
    /// every `SyntaxNode` contains at least one token transitively.
    pub fn covering_token_at_offset(&self, offset: usize) -> SyntaxToken {
        match self.find_token(|child| match child {
            Child::Node(node) => node.contains_offset(offset),
            Child::Token(tok) => tok.contains_offset(offset),
        }) {
            Some(token) => token,
            None => {
                let first_tok = self
                    .first_token()
                    .expect("every SyntaxNode must contain at least one token");
                if offset < first_tok.offset() {
                    first_tok
                } else {
                    self.last_token()
                        .expect("every SyntaxNode must contain at least one token")
                }
            }
        }
    }
}

impl SyntaxElement {
    pub fn last_token(&self) -> Option<SyntaxToken> {
        match self {
            Child::Token(token) => Some(token.clone()),
            Child::Node(node) => node.last_token(),
        }
    }

    pub fn first_token(&self) -> Option<SyntaxToken> {
        match self {
            SyntaxElement::Node(node) => node.first_token(),
            SyntaxElement::Token(token) => Some(token.clone()),
        }
    }

    pub fn offset(&self) -> usize {
        match self {
            Child::Token(token) => token.offset(),
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

#[cfg(test)]
mod tests {
    use crate::syntax::green::{GreenChild, GreenNode, GreenNodeData};
    use crate::syntax::node::{SyntaxElement, SyntaxNode};
    use crate::syntax::node_kind::NodeKind::*;
    use crate::syntax::rewrite::RewriteAction;
    use crate::tokens::Tokenize;
    use crate::tokens::{Keyword, Token, TokenKind, Trivia, TriviaPiece};
    use pretty_assertions::assert_eq;
    use std::collections::VecDeque;

    #[test]
    fn no_leading_trivia() {
        let token = Token::simple(TokenKind::Keyword(Keyword::Entity), b"entity");
        let mut green_node = GreenNodeData::new(EntityDeclaration);
        green_node.push_token(token);
        let node = SyntaxNode::new_root(GreenNode::new(green_node));
        assert_eq!(
            node.first_token().unwrap().leading_trivia(),
            &Trivia::default()
        );
    }

    #[test]
    fn leading_trivia_no_previous_token() {
        let token = Token::new(
            TokenKind::Keyword(Keyword::Entity),
            b"entity",
            Trivia::from([TriviaPiece::Spaces(2), TriviaPiece::LineFeeds(1)]),
        );
        let mut green_node = GreenNodeData::new(EntityDeclaration);
        green_node.push_token(token);
        let node = SyntaxNode::new_root(GreenNode::new(green_node));
        assert_eq!(
            node.first_token().unwrap().leading_trivia(),
            &Trivia::from([TriviaPiece::Spaces(2), TriviaPiece::LineFeeds(1)])
        );
    }

    #[test]
    fn leading_trivia_with_previous_token() {
        let tokens = [
            Token::new(
                TokenKind::Keyword(Keyword::Entity),
                b"entity",
                Trivia::from([TriviaPiece::Spaces(2), TriviaPiece::LineFeeds(1)]),
            ),
            Token::new(
                TokenKind::Identifier,
                b"foo",
                Trivia::from([TriviaPiece::Spaces(2), TriviaPiece::LineFeeds(1)]),
            ),
        ];
        let mut green_node = GreenNodeData::new(EntityDeclaration);
        green_node.push_tokens(tokens);
        let node = SyntaxNode::new_root(GreenNode::new(green_node));
        assert_eq!(
            node.tokens().nth(1).unwrap().leading_trivia(),
            &Trivia::from([TriviaPiece::Spaces(2), TriviaPiece::LineFeeds(1)])
        );
    }

    #[test]
    fn no_trailing_trivia() {
        let token = Token::simple(TokenKind::Keyword(Keyword::Entity), b"entity");
        let mut green_node = GreenNodeData::new(EntityDeclaration);
        green_node.push_token(token);
        let node = SyntaxNode::new_root(GreenNode::new(green_node));
        assert_eq!(
            node.first_token().unwrap().trailing_trivia(),
            Trivia::default()
        );
    }

    #[test]
    fn trailing_trivia_with_next_token() {
        let tokens = [
            Token::new(
                TokenKind::Keyword(Keyword::Entity),
                b"entity",
                Trivia::from([TriviaPiece::Spaces(2), TriviaPiece::LineFeeds(1)]),
            ),
            Token::new(
                TokenKind::Identifier,
                b"foo",
                Trivia::from([TriviaPiece::Spaces(2), TriviaPiece::LineFeeds(1)]),
            ),
        ];
        let mut green_node = GreenNodeData::new(EntityDeclaration);
        green_node.push_tokens(tokens);
        let node = SyntaxNode::new_root(GreenNode::new(green_node));
        assert_eq!(
            node.first_token().unwrap().trailing_trivia(),
            Trivia::from([TriviaPiece::Spaces(2), TriviaPiece::LineFeeds(1)])
        );
    }

    #[test]
    fn no_rewrite_is_noop() {
        let orig_tokens = "entity foo is end foo"
            .tokenize()
            .map(|(tok, _)| tok)
            .collect::<Vec<_>>();
        let mut data = GreenNodeData::new(EntityDeclaration);
        data.push_tokens(orig_tokens.clone());
        let node = SyntaxNode::new_root(GreenNode::new(data));
        let new_node = node.rewrite(|_| RewriteAction::Leave);
        let new_tokens = new_node
            .tokens()
            .map(|syntax_token| syntax_token.token().clone())
            .collect::<Vec<_>>();
        assert_eq!(new_tokens, orig_tokens);
    }

    #[test]
    fn rewrite_tokens() {
        let mut data = GreenNodeData::new(EntityDeclaration);
        data.push_tokens("entity foo is end foo;".tokenize().map(|(tok, _)| tok));
        let node = SyntaxNode::new_root(GreenNode::new(data));
        let new_node = node.rewrite_tokens(|tok| {
            if tok.text() == "foo" {
                RewriteAction::Change(SyntaxElement::Token(tok.clone_with_text(b"bar")))
            } else {
                RewriteAction::Leave
            }
        });
        let new_tokens = new_node
            .tokens()
            .map(|syntax_token| syntax_token.token().clone())
            .collect::<VecDeque<_>>();
        assert_eq!(
            new_tokens,
            "entity bar is end bar;"
                .tokenize()
                .map(|(tok, _)| tok)
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn rewrite_does_not_modify_self() {
        let orig_tokens = "entity foo is end foo"
            .tokenize()
            .map(|(tok, _)| tok)
            .collect::<Vec<_>>();
        let mut data = GreenNodeData::new(EntityDeclaration);
        data.push_tokens(orig_tokens.clone());
        let node = SyntaxNode::new_root(GreenNode::new(data));
        let new_node = node.rewrite_nodes(|node| match node.kind() {
            EntityDeclaration => panic!("Should not modify self"),
            _ => RewriteAction::Leave,
        });
        let new_tokens = new_node
            .tokens()
            .map(|syntax_token| syntax_token.token().clone())
            .collect::<VecDeque<_>>();
        assert_eq!(new_tokens, orig_tokens);
    }

    #[test]
    fn next_token() {
        let mut top = GreenNodeData::new(DesignFile);
        let mut n1 = GreenNodeData::new(EntityDeclaration);
        n1.push_tokens([
            Token::simple(TokenKind::Keyword(Keyword::Entity), b"entity"),
            Token::simple(TokenKind::Identifier, b"foo"),
        ]);
        let n1 = GreenNode::new(n1);
        let mut n2 = GreenNodeData::new(ArchitectureBody);
        n2.push_tokens([
            Token::simple(TokenKind::Keyword(Keyword::Architecture), b"architecture"),
            Token::simple(TokenKind::Identifier, b"bar"),
        ]);
        let n2 = GreenNode::new(n2);

        top.push_children([GreenChild::Node(n1), GreenChild::Node(n2)]);

        let s = SyntaxNode::new_root(GreenNode::new(top));
        let first_token = s.first_token().expect("Node must have first token");
        assert!(first_token.kind() == TokenKind::Keyword(Keyword::Entity));
        // Same node
        let second_token = first_token
            .next_token()
            .expect("Node must have second token");
        assert!(second_token.kind() == TokenKind::Identifier);
        // Neighbor node
        let third_token = second_token
            .next_token()
            .expect("Node must have second token");
        assert!(third_token.kind() == TokenKind::Keyword(Keyword::Architecture));
    }
}
