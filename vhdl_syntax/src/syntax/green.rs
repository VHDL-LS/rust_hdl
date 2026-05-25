//! Private API for the underlying Green Tree
use crate::fmt::{encoding::Encoder, FormatTo};
use crate::latin_1::Latin1Str;
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com
use crate::syntax::child::Child;
use crate::syntax::node_kind::NodeKind;
use crate::tokens::{Token, TokenKind, Trivia};
use std::fmt;
use std::io::{self, Write};
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

impl FormatTo for GreenToken {
    fn write_encoded<E>(&self, writer: &mut impl fmt::Write) -> crate::fmt::Result<E::Err>
    where
        E: Encoder,
        for <'a> E::Str<'a>: fmt::Display,
    {
        self.0.write_encoded::<E>(writer)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct GreenNode(Arc<GreenNodeData>);

pub(crate) type GreenChild = Child<GreenNode, GreenToken>;

#[derive(Debug, Eq, PartialEq, Clone)]
pub(crate) struct GreenNodeData {
    /// The kind of this node
    kind: NodeKind,
    /// The sub-nodes or token of this node
    children: Vec<GreenChild>,
    byte_len: usize,
}

impl GreenNodeData {
    pub(crate) fn push(&mut self, child: GreenChild) {
        self.byte_len += child.byte_len();
        self.children.push(child);
    }

    #[cfg(test)]
    pub(crate) fn push_token(&mut self, token: Token) {
        self.push(Child::Token(GreenToken::new(token)))
    }

    #[cfg(test)]
    pub fn push_tokens(&mut self, tokens: impl IntoIterator<Item = Token>) {
        for token in tokens {
            self.push_token(token);
        }
    }

    pub(crate) fn push_children(&mut self, children: impl IntoIterator<Item = GreenChild>) {
        for child in children {
            self.push(child);
        }
    }

    pub(crate) fn new(kind: NodeKind) -> GreenNodeData {
        GreenNodeData {
            kind,
            children: vec![],
            byte_len: 0,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.children.is_empty()
    }

    pub fn byte_len(&self) -> usize {
        self.byte_len
    }
}

impl GreenNode {
    pub(crate) fn new(data: GreenNodeData) -> GreenNode {
        GreenNode(Arc::new(data))
    }

    pub fn children(&self) -> impl Iterator<Item = &GreenChild> {
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

impl FormatTo for GreenNode {
    fn write_encoded<E>(&self, writer: &mut impl fmt::Write) -> crate::fmt::Result<E::Err>
    where
        E: Encoder,
        for <'a> E::Str<'a>: fmt::Display,
    {
        for child in self.children() {
            match child {
                Child::Node(node) => node.write_encoded::<E>(writer)?,
                Child::Token(token) => token.write_encoded::<E>(writer)?,
            }
        }
        Ok(())
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
    fn push_keeps_byte_len_in_sync() {
        let mut data = GreenNodeData::new(NodeKind::EntityDeclaration);
        assert_eq!(data.byte_len(), 0);
        data.push_token(Token::simple(
            TokenKind::Keyword(Keyword::Entity),
            b"entity",
        ));
        assert_eq!(data.byte_len(), 6);
        data.push_token(Token::simple(TokenKind::Identifier, b"foo"));
        assert_eq!(data.byte_len(), 9);
    }

    #[test]
    fn nested_byte_len_consistent() {
        let mut inner = GreenNodeData::new(NodeKind::EntityDeclarationPreamble);
        inner.push_tokens([
            Token::simple(TokenKind::Keyword(Keyword::Entity), b"entity"),
            Token::simple(TokenKind::Identifier, b"foo"),
        ]);
        let inner = GreenNode::new(inner);

        let mut outer = GreenNodeData::new(NodeKind::EntityDeclaration);
        outer.push(Child::Node(inner));
        let outer = GreenNode::new(outer);

        assert_byte_len_consistent(&outer);
        assert_eq!(outer.byte_len(), 9);
    }
}
