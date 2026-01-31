// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::node::{ChoiceNode, Model, Node, NodeRef, SequenceNode, TokenOrNode};
use crate::serialize::{NodeContents, NodeOrToken};
use crate::token::{Keyword, Token, TokenKind};

impl Model {
    pub fn insert_ser_nodes(&mut self, category: &str, nodes: crate::serialize::Nodes) {
        for node in nodes {
            self.insert_ser_node(category, node)
        }
    }

    pub fn insert_ser_node(&mut self, category: &str, node: crate::serialize::Node) {
        match node.contents {
            NodeContents::Sequence(seq) => {
                self.insert_ser_seq(category, node.name, seq);
            }
            NodeContents::Choice(choice) => self.insert_ser_choice(category, node.name, choice),
            NodeContents::Builtin => self.push_builtin(node.name),
        }
    }

    pub fn node_to_parenthesized_node(
        &mut self,
        category: &str,
        node: crate::serialize::NodeRef,
    ) -> crate::serialize::NodeRef {
        // e.g., `ParenthesizedSemicolonTerminatedVUnitBindingIndicationSyntax`
        let node_name = format!("Parenthesized{}", node.kind());
        self.push_node(
            category.to_owned(),
            SequenceNode::new(
                node_name.clone(),
                vec![
                    TokenKind::LeftPar.into(),
                    TokenOrNode::Node(node.kind().into()),
                    TokenKind::RightPar.into(),
                ],
            ),
        );
        crate::serialize::NodeRef {
            node: node_name,
            parenthesized: false,
            ..node
        }
    }

    pub fn node_to_terminated_node(
        &mut self,
        category: &str,
        node: crate::serialize::NodeRef,
    ) -> crate::serialize::NodeRef {
        let terminator = node.terminated.clone().unwrap();
        // e.g., `SemicolonTerminatedVUnitBindingIndicationSyntax`
        let node_name = format!("{}Terminated{}", terminator, node.kind());
        self.push_node(
            category.to_owned(),
            SequenceNode::new(
                node_name.clone(),
                vec![
                    TokenOrNode::Node(node.kind().into()),
                    TokenKind::from_str_expect(&terminator).into(),
                ],
            ),
        );
        crate::serialize::NodeRef {
            node: node_name,
            terminated: None,
            ..node
        }
    }

    fn ser_node_to_node(
        &mut self,
        category: &str,
        items: &[TokenOrNode],
        mut node: crate::serialize::NodeRef,
    ) -> NodeRef {
        // Terminated takes precedence over repeated
        if node.terminated.is_some() {
            node = self.node_to_terminated_node(category, node)
        }
        if node.parenthesized {
            node = self.node_to_parenthesized_node(category, node)
        }
        let nth = items
            .iter()
            .filter(|other_node| match other_node {
                TokenOrNode::Node(other_node) => other_node.kind == node.kind(),
                _ => false,
            })
            .count();
        NodeRef {
            kind: node.kind(),
            name: node.name(),
            repeated: node.repeated,
            nth,
            builtin: false,
            // will be patched later
            is_token_node: false,
        }
    }

    fn ser_token_to_token(
        &mut self,
        items: &[TokenOrNode],
        token: crate::serialize::TokenRef,
    ) -> Token {
        let kind = TokenKind::from_str_expect(&token.token);
        let nth = items
            .iter()
            .filter(|other_node| match other_node {
                TokenOrNode::Token(other_token) => other_token.kind == kind,
                _ => false,
            })
            .count();
        Token {
            name: token.name(),
            repeated: token.repeated,
            nth,
            kind,
        }
    }

    fn ser_keyword_to_token(
        &mut self,
        items: &[TokenOrNode],
        token: crate::serialize::KeywordRef,
    ) -> Token {
        let kind = TokenKind::Keyword(Keyword::from_str_expect(&token.keyword));
        let nth = items
            .iter()
            .filter(|other_node| match other_node {
                TokenOrNode::Token(other_token) => other_token.kind == kind,
                _ => false,
            })
            .count();
        Token {
            name: token.name(),
            repeated: false,
            nth,
            kind,
        }
    }

    fn ser_token_or_node_to_node_or_token(
        &mut self,
        category: &str,
        items: &[TokenOrNode],
        node_or_token: NodeOrToken,
    ) -> TokenOrNode {
        match node_or_token {
            NodeOrToken::Node(node) => {
                TokenOrNode::Node(self.ser_node_to_node(category, items, node))
            }
            NodeOrToken::Token(token) => TokenOrNode::Token(self.ser_token_to_token(items, token)),
            NodeOrToken::Keyword(keyword) => {
                TokenOrNode::Token(self.ser_keyword_to_token(items, keyword))
            }
        }
    }

    pub fn insert_ser_seq(&mut self, category: &str, name: String, seq: Vec<NodeOrToken>) {
        let mut items = Vec::new();
        for node_or_token in seq {
            items.push(self.ser_token_or_node_to_node_or_token(category, &items, node_or_token));
        }
        let node = Node::Items(SequenceNode::new(name, items));
        self.push_node(category.to_owned(), node);
    }

    pub fn insert_ser_choice(&mut self, category: &str, name: String, choices: Vec<NodeOrToken>) {
        let mut nodes = Vec::new();
        let mut tokens = Vec::new();
        for node_or_token in choices {
            match node_or_token {
                NodeOrToken::Node(node) => {
                    nodes.push(TokenOrNode::Node(
                        self.ser_node_to_node(category, &nodes, node),
                    ));
                }
                NodeOrToken::Token(token) => {
                    tokens.push(TokenOrNode::Token(self.ser_token_to_token(&tokens, token)));
                }
                NodeOrToken::Keyword(kw) => {
                    tokens.push(TokenOrNode::Token(self.ser_keyword_to_token(&tokens, kw)));
                }
            }
        }
        let node = if tokens.is_empty() {
            Node::Choices(ChoiceNode {
                name: name.to_owned(),
                items: nodes
                    .into_iter()
                    .map(|ton| match ton {
                        TokenOrNode::Node(node) => node,
                        _ => unreachable!(),
                    })
                    .collect(),
            })
        } else if nodes.is_empty() {
            Node::Choices(ChoiceNode {
                name: name.to_owned(),
                items: tokens
                    .into_iter()
                    .map(|ton| match ton {
                        TokenOrNode::Token(token) => token,
                        _ => unreachable!(),
                    })
                    .collect(),
            })
        } else {
            panic!("Heterogeneous Tokens / Nodes not allowed (node {name})");
        };
        self.push_node(category.to_owned(), node);
    }
}
