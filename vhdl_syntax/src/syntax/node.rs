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
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::latin_1::{Latin1Str, Latin1String, Utf8ToLatin1Error};
use crate::syntax::child::Child;
use crate::syntax::green::{GreenChild, GreenNode, GreenToken};
use crate::syntax::node_kind::NodeKind;
use crate::syntax::rewrite::{RewriteAction, Rewriter};
use crate::tokens::{Token, TokenKind, Trivia};
use std::io::{self, Write};
use std::iter;
use std::sync::Arc;

pub type SyntaxElement = Child<SyntaxNode, SyntaxToken>;

impl SyntaxElement {
    pub(crate) fn green(&self) -> GreenChild {
        match self {
            SyntaxElement::Node(node) => GreenChild::Node((node.offset(), node.green().clone())),
            SyntaxElement::Token(token) => {
                GreenChild::Token((token.offset(), token.green().clone()))
            }
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
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

    pub fn offset(&self) -> usize {
        self.0.offset
    }

    pub fn token(&self) -> &Token {
        self.green().token()
    }

    pub fn byte_len(&self) -> usize {
        self.green().byte_len()
    }

    pub fn kind(&self) -> TokenKind {
        self.green().kind()
    }

    /// Returns all trivia between this token and the previous one, resp. only the leading trivia
    /// of this token, if there is no previous token.
    pub fn leading_trivia(&self) -> Trivia {
        let mut trivia = self
            .prev_token()
            .map(|trivia| trivia.green().trailing_trivia().clone())
            .unwrap_or_default();
        trivia.append(&mut self.green().leading_trivia().clone());
        trivia
    }

    /// Returns all trailing trivia between this token and the next one, resp. only the trailing
    /// trivia of this token, if there is no next token.
    pub fn trailing_trivia(&self) -> Trivia {
        let mut trivia = self.green().trailing_trivia().clone();
        trivia.append(
            &mut self
                .next_token()
                .map(|trivia| trivia.green().leading_trivia().clone())
                .unwrap_or_default(),
        );
        trivia
    }

    pub fn text(&self) -> &Latin1Str {
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

    pub fn clone_with_text(&self, text: impl Into<Box<Latin1Str>>) -> SyntaxToken {
        let token = Token::new(
            self.kind(),
            text,
            self.green().leading_trivia().clone(),
            self.green().trailing_trivia().clone(),
        );
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
}

#[derive(Debug, Eq, PartialEq)]
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

    pub fn rewrite(&self, rewrite: impl Fn(&SyntaxElement) -> RewriteAction) -> SyntaxNode {
        Rewriter::new(rewrite).rewrite(self.clone())
    }

    pub fn rewrite_nodes(&self, rewrite: impl Fn(&SyntaxNode) -> RewriteAction) -> SyntaxNode {
        Rewriter::new(|element| match element {
            SyntaxElement::Node(node) => rewrite(node),
            SyntaxElement::Token(_) => RewriteAction::Leave,
        })
        .rewrite(self.clone())
    }

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

    fn prev_sibling_or_token(&self) -> Option<SyntaxElement> {
        self.parent()?
            .children_with_tokens()
            .nth(self.index().checked_sub(1)?)
    }

    fn next_sibling_or_token(&self) -> Option<SyntaxElement> {
        self.parent()?
            .children_with_tokens()
            .nth(self.index().checked_add(1)?)
    }

    #[cfg(test)]
    pub(crate) fn test_text(&self) -> String {
        self.green().test_text(0)
    }

    pub fn write_to(&self, writer: &mut impl Write) -> io::Result<()> {
        self.green().write_to(writer)
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

#[cfg(test)]
mod tests {
    use crate::syntax::green::{GreenNode, GreenNodeData};
    use crate::syntax::node::{SyntaxElement, SyntaxNode};
    use crate::syntax::node_kind::NodeKind::EntityDeclaration;
    use crate::syntax::rewrite::RewriteAction;
    use crate::tokens::Tokenize;
    use crate::tokens::{Keyword, Token, TokenKind, Trivia, TriviaPiece};
    use pretty_assertions::assert_eq;
    use std::collections::VecDeque;

    #[test]
    fn no_leading_trivia() {
        let token = Token::simple(TokenKind::Keyword(Keyword::Entity), b"entity");
        let mut green_node = GreenNodeData::new(EntityDeclaration);
        green_node.push_token(0, token);
        let node = SyntaxNode::new_root(GreenNode::new(green_node));
        assert_eq!(
            node.first_token().unwrap().leading_trivia(),
            Trivia::default()
        );
    }

    #[test]
    fn leading_trivia_no_previous_token() {
        let token = Token::new(
            TokenKind::Keyword(Keyword::Entity),
            b"entity",
            Trivia::from([TriviaPiece::Spaces(2), TriviaPiece::LineFeeds(1)]),
            Default::default(),
        );
        let mut green_node = GreenNodeData::new(EntityDeclaration);
        green_node.push_token(0, token);
        let node = SyntaxNode::new_root(GreenNode::new(green_node));
        assert_eq!(
            node.first_token().unwrap().leading_trivia(),
            Trivia::from([TriviaPiece::Spaces(2), TriviaPiece::LineFeeds(1)])
        );
    }

    #[test]
    fn leading_trivia_with_previous_token() {
        let tokens = [
            Token::new(
                TokenKind::Keyword(Keyword::Entity),
                b"entity",
                Trivia::from([TriviaPiece::Spaces(2), TriviaPiece::LineFeeds(1)]),
                Trivia::from([TriviaPiece::Spaces(2)]),
            ),
            Token::new(
                TokenKind::Identifier,
                b"foo",
                Trivia::from([TriviaPiece::LineFeeds(1)]),
                Trivia::from([TriviaPiece::FormFeeds(2)]),
            ),
        ];
        let mut green_node = GreenNodeData::new(EntityDeclaration);
        green_node.push_tokens(0, tokens);
        let node = SyntaxNode::new_root(GreenNode::new(green_node));
        assert_eq!(
            node.tokens().nth(1).unwrap().leading_trivia(),
            Trivia::from([TriviaPiece::Spaces(2), TriviaPiece::LineFeeds(1)])
        );
    }

    #[test]
    fn no_trailing_trivia() {
        let token = Token::simple(TokenKind::Keyword(Keyword::Entity), b"entity");
        let mut green_node = GreenNodeData::new(EntityDeclaration);
        green_node.push_token(0, token);
        let node = SyntaxNode::new_root(GreenNode::new(green_node));
        assert_eq!(
            node.first_token().unwrap().trailing_trivia(),
            Trivia::default()
        );
    }

    #[test]
    fn trailing_trivia_no_next_token() {
        let token = Token::new(
            TokenKind::Keyword(Keyword::Entity),
            b"entity",
            Trivia::new(),
            Trivia::from([TriviaPiece::Spaces(2), TriviaPiece::LineFeeds(1)]),
        );
        let mut green_node = GreenNodeData::new(EntityDeclaration);
        green_node.push_token(0, token);
        let node = SyntaxNode::new_root(GreenNode::new(green_node));
        assert_eq!(
            node.first_token().unwrap().trailing_trivia(),
            Trivia::from([TriviaPiece::Spaces(2), TriviaPiece::LineFeeds(1)])
        );
    }

    #[test]
    fn trailing_trivia_with_next_token() {
        let tokens = [
            Token::new(
                TokenKind::Keyword(Keyword::Entity),
                b"entity",
                Trivia::from([TriviaPiece::Spaces(2), TriviaPiece::LineFeeds(1)]),
                Trivia::from([TriviaPiece::Spaces(2)]),
            ),
            Token::new(
                TokenKind::Identifier,
                b"foo",
                Trivia::from([TriviaPiece::LineFeeds(1)]),
                Trivia::from([TriviaPiece::FormFeeds(2)]),
            ),
        ];
        let mut green_node = GreenNodeData::new(EntityDeclaration);
        green_node.push_tokens(0, tokens);
        let node = SyntaxNode::new_root(GreenNode::new(green_node));
        assert_eq!(
            node.first_token().unwrap().trailing_trivia(),
            Trivia::from([TriviaPiece::Spaces(2), TriviaPiece::LineFeeds(1)])
        );
    }

    #[test]
    fn no_rewrite_is_noop() {
        let orig_tokens = "entity foo is end foo".tokenize().collect::<Vec<_>>();
        let mut data = GreenNodeData::new(EntityDeclaration);
        data.push_tokens(0, orig_tokens.clone());
        let node = SyntaxNode::new_root(GreenNode::new(data));
        let new_node = node.rewrite(|_| RewriteAction::Leave);
        let new_tokens = new_node
            .tokens()
            .map(|syntax_token| syntax_token.token().clone())
            .collect::<VecDeque<_>>();
        assert_eq!(new_tokens, orig_tokens);
    }

    #[test]
    fn rewrite_tokens() {
        let mut data = GreenNodeData::new(EntityDeclaration);
        data.push_tokens(0, "entity foo is end foo;".tokenize());
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
            "entity bar is end bar;".tokenize().collect::<Vec<_>>()
        );
    }

    #[test]
    fn rewrite_does_not_modify_self() {
        let orig_tokens = "entity foo is end foo".tokenize().collect::<Vec<_>>();
        let mut data = GreenNodeData::new(EntityDeclaration);
        data.push_tokens(0, orig_tokens.clone());
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
}
