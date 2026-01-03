//! Private API for the underlying Green Tree
use crate::latin_1::Latin1Str;
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com
use crate::syntax::child::Child;
use crate::syntax::node_kind::NodeKind;
use crate::tokens::{Token, TokenKind, Trivia};
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

    pub fn trailing_trivia(&self) -> &Trivia {
        &self.0.trailing_trivia
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

impl GreenNode {
    pub(crate) fn data(&self) -> &GreenNodeData {
        &self.0
    }
}

pub(crate) type GreenChild = Child<(usize, GreenNode), (usize, GreenToken)>;

impl GreenChild {
    pub fn byte_len(&self) -> usize {
        match self {
            GreenChild::Token((_, token)) => token.byte_len(),
            GreenChild::Node((_, node)) => node.byte_len(),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub(crate) struct GreenNodeData {
    /// The kind of this node
    kind: NodeKind,
    /// The sub-nodes or token of this node
    children: Vec<GreenChild>,
}

impl GreenNodeData {
    #[cfg(test)]
    fn push_child(&mut self, child: GreenChild) {
        self.children.push(child)
    }

    #[cfg(test)]
    pub(crate) fn push_token(&mut self, offset: usize, token: Token) {
        self.push_child(Child::Token((offset, GreenToken::new(token))))
    }

    #[cfg(test)]
    pub fn push_tokens(&mut self, offset: usize, tokens: impl IntoIterator<Item = Token>) {
        let mut new_offset = offset;
        for token in tokens {
            let tok_len = token.byte_len();
            self.push_token(new_offset, token);
            new_offset += tok_len;
        }
    }

    pub(crate) fn push_children(&mut self, children: impl IntoIterator<Item = GreenChild>) {
        self.children.extend(children)
    }

    pub(crate) fn replace_child(&mut self, index: usize, child: GreenChild) {
        self.children[index] = child;
    }

    pub(crate) fn new(kind: NodeKind) -> GreenNodeData {
        GreenNodeData {
            kind,
            children: vec![],
        }
    }

    pub fn byte_len(&self) -> usize {
        self.children
            .iter()
            .fold(0, |acc, next| acc + next.byte_len())
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
                Child::Node((_, node)) => node.write_to(writer)?,
                Child::Token((_, token)) => token.write_to(writer)?,
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
                        Child::Node((_, subnode)) => {
                            write!(&mut w, "{}", subnode.test_text(indent + 2))?
                        }
                        Child::Token((_, token)) => {
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
