//! Private API for the underlying Green Tree
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com
use crate::latin_1::Latin1Str;
use crate::non_empty::NonEmpty;
use crate::syntax::child::{Child, ChildKind};
use crate::syntax::node_kind::NodeKind;
use crate::tokens::{Token, TokenKind, Trivia};
use std::io::{self, Write};
use std::slice;
use std::sync::Arc;

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct GreenToken(Arc<Token>);

impl GreenToken {
    pub(crate) fn new(token: Token) -> GreenToken {
        GreenToken(Arc::new(token))
    }

    pub fn kind(&self) -> TokenKind {
        self.0.kind()
    }

    pub fn leading_trivia(&self) -> &Trivia {
        &self.0.leading_trivia
    }

    pub fn text(&self) -> &Latin1Str {
        self.0.text()
    }

    pub fn byte_len(&self) -> usize {
        self.0.byte_len()
    }

    pub(crate) fn token(&self) -> &Token {
        self.0.as_ref()
    }

    pub fn write_to(&self, writer: &mut impl Write) -> io::Result<()> {
        self.0.write_to(writer)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct GreenNode(Arc<GreenNodeData>);

pub(crate) type GreenChild = Child<GreenNode, GreenToken>;

impl GreenChild {
    pub fn kind(&self) -> ChildKind {
        match self {
            Child::Node(node) => Child::Node(node.kind()),
            Child::Token(token) => Child::Token(token.kind()),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub(crate) struct GreenNodeData {
    /// The kind of this node
    kind: NodeKind,
    /// The sub-nodes or token of this node
    children: NonEmpty<GreenChild>,
    byte_len: usize,
}

impl GreenNodeData {
    pub(crate) fn new(
        kind: NodeKind,
        children: impl IntoIterator<Item = GreenChild>,
    ) -> Option<GreenNodeData> {
        let children = NonEmpty::from_iter(children)?;
        let byte_len = children.iter().map(GreenChild::byte_len).sum();
        Some(GreenNodeData {
            kind,
            children,
            byte_len,
        })
    }

    pub fn byte_len(&self) -> usize {
        self.byte_len
    }
}

impl GreenNode {
    pub(crate) fn new(data: GreenNodeData) -> GreenNode {
        GreenNode(Arc::new(data))
    }

    /// Builds a node directly from its children, panicking when no child is given.
    #[cfg(test)]
    pub(crate) fn from_children(
        kind: NodeKind,
        children: impl IntoIterator<Item = GreenChild>,
    ) -> GreenNode {
        GreenNode::new(GreenNodeData::new(kind, children).expect("Cannot build empty nodes"))
    }

    /// Builds a node consisting solely of tokens, panicking when no token is given.
    #[cfg(test)]
    pub(crate) fn from_tokens(
        kind: NodeKind,
        tokens: impl IntoIterator<Item = Token>,
    ) -> GreenNode {
        GreenNode::from_children(
            kind,
            tokens.into_iter().map(GreenToken::new).map(Child::Token),
        )
    }

    pub fn children(&self) -> slice::Iter<'_, GreenChild> {
        self.0.children.iter()
    }

    pub fn kind(&self) -> NodeKind {
        self.0.kind
    }

    pub fn byte_len(&self) -> usize {
        self.0.byte_len()
    }

    pub fn write_to(&self, writer: &mut impl Write) -> io::Result<()> {
        for child in self.children() {
            match child {
                Child::Node(node) => node.write_to(writer)?,
                Child::Token(token) => token.write_to(writer)?,
            }
        }
        Ok(())
    }

    #[cfg(test)]
    pub fn test_text(&self, indent: usize) -> String {
        use std::fmt::Write;
        let fail_fn: fn(&GreenNode, usize) -> Result<String, std::fmt::Error> =
            |n: &GreenNode, indent: usize| {
                let mut w = String::new();
                write!(&mut w, "{:indent$}", "", indent = indent)?;
                writeln!(&mut w, "{:?}", n.kind())?;
                for child in n.children() {
                    match child {
                        Child::Node(subnode) => {
                            write!(&mut w, "{}", subnode.test_text(indent + 2))?
                        }
                        Child::Token(token) => {
                            write!(&mut w, "{:indent$}", "", indent = indent + 2)?;
                            write!(&mut w, "{:?}", token.kind())?;
                            if matches!(
                                token.kind(),
                                TokenKind::Identifier
                                    | TokenKind::StringLiteral
                                    | TokenKind::BitStringLiteral
                                    | TokenKind::CharacterLiteral
                                    | TokenKind::AbstractLiteral
                            ) {
                                write!(&mut w, " '{}'", token.text())?;
                            }
                            writeln!(&mut w)?;
                        }
                    }
                }
                Ok(w)
            };
        fail_fn(self, indent).unwrap()
    }
}

impl Child<GreenNode, GreenToken> {
    pub fn byte_len(&self) -> usize {
        match self {
            Child::Token(token) => token.byte_len(),
            Child::Node(node) => node.byte_len(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::node_kind::NodeKind;
    use crate::tokens::{Keyword, Token, TokenKind};
    use pretty_assertions::assert_eq;

    /// Walks a green tree and asserts that every node's cached `byte_len`
    /// equals the sum of its children's lengths.
    fn assert_byte_len_consistent(node: &GreenNode) {
        let mut sum = 0usize;
        for child in node.children() {
            sum += child.byte_len();
            if let Child::Node(n) = child {
                assert_byte_len_consistent(n);
            }
        }
        assert_eq!(
            sum,
            node.byte_len(),
            "byte_len cache out of sync in {:?}",
            node.kind()
        );
    }

    #[test]
    fn byte_len_is_the_sum_of_all_children() {
        let node = GreenNode::from_tokens(
            NodeKind::EntityDeclaration,
            [Token::simple(
                TokenKind::Keyword(Keyword::Entity),
                b"entity",
            )],
        );
        assert_eq!(node.byte_len(), 6);

        let node = GreenNode::from_tokens(
            NodeKind::EntityDeclaration,
            [
                Token::simple(TokenKind::Keyword(Keyword::Entity), b"entity"),
                Token::simple(TokenKind::Identifier, b"foo"),
            ],
        );
        assert_eq!(node.byte_len(), 9);
    }

    #[test]
    fn empty_nodes_cannot_be_built() {
        assert_eq!(GreenNodeData::new(NodeKind::EntityDeclaration, []), None);
    }

    #[test]
    fn nested_byte_len_consistent() {
        let inner = GreenNode::from_tokens(
            NodeKind::EntityDeclarationPreamble,
            [
                Token::simple(TokenKind::Keyword(Keyword::Entity), b"entity"),
                Token::simple(TokenKind::Identifier, b"foo"),
            ],
        );

        let outer = GreenNode::from_children(NodeKind::EntityDeclaration, [Child::Node(inner)]);

        assert_byte_len_consistent(&outer);
        assert_eq!(outer.byte_len(), 9);
    }
}
