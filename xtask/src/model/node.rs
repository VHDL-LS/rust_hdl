// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2025, Lukas Scheller lukasscheller@icloud.com

use crate::model::token::Token;
use convert_case::{Case, Casing};
use std::collections::{HashMap, HashSet};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TokenOrNode {
    Node(NodeRef),
    Token(Token),
}

impl From<crate::model::token::TokenKind> for TokenOrNode {
    fn from(value: crate::model::token::TokenKind) -> Self {
        TokenOrNode::Token(Token::from(value))
    }
}

impl From<NodeRef> for TokenOrNode {
    fn from(value: NodeRef) -> Self {
        TokenOrNode::Node(value)
    }
}

impl TokenOrNode {
    pub fn getter_name(&self) -> String {
        match self {
            TokenOrNode::Node(node) => node.getter_name(),
            TokenOrNode::Token(token) => token.getter_name(),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NodeRef {
    pub kind: String,
    pub nth: usize,
    pub builtin: bool,
    pub repeated: bool,
    pub name: String,
    /// whether this node is optional in the grammar
    pub optional: bool,
}

impl From<String> for NodeRef {
    fn from(value: String) -> Self {
        NodeRef {
            kind: value.clone(),
            nth: 0,
            builtin: false,
            repeated: false,
            name: value,
            optional: false,
        }
    }
}

impl NodeRef {
    pub fn getter_name(&self) -> String {
        self.name.to_case(Case::Snake)
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Node {
    Items(SequenceNode),
    Choices(ChoiceNode),
}

impl Node {
    pub fn name(&self) -> String {
        match self {
            Node::Items(items) => items.name.clone(),
            Node::Choices(choices) => choices.name.clone(),
        }
    }

    pub fn is_all_token_choices(&self) -> bool {
        matches!(
            self,
            Node::Choices(ChoiceNode {
                name: _,
                items: NodesOrTokens::Tokens(_)
            })
        )
    }
}

impl From<SequenceNode> for Node {
    fn from(value: SequenceNode) -> Self {
        Node::Items(value)
    }
}

impl From<ChoiceNode> for Node {
    fn from(value: ChoiceNode) -> Self {
        Node::Choices(value)
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct SequenceNode {
    pub name: String,
    pub items: Vec<TokenOrNode>,
}

impl SequenceNode {
    pub fn new(name: impl Into<String>, items: Vec<TokenOrNode>) -> SequenceNode {
        SequenceNode {
            name: name.into().to_case(Case::UpperCamel),
            items,
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum NodesOrTokens {
    Nodes(Vec<NodeRef>),
    Tokens(Vec<Token>),
}

impl FromIterator<NodeRef> for NodesOrTokens {
    fn from_iter<T: IntoIterator<Item = NodeRef>>(iter: T) -> Self {
        NodesOrTokens::Nodes(iter.into_iter().collect())
    }
}

impl FromIterator<Token> for NodesOrTokens {
    fn from_iter<T: IntoIterator<Item = Token>>(iter: T) -> Self {
        NodesOrTokens::Tokens(iter.into_iter().collect())
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct ChoiceNode {
    pub name: String,
    pub items: NodesOrTokens,
}

#[derive(Debug, Default)]
pub struct Model {
    pub(crate) sections: HashMap<String, Vec<Node>>,
    pub(crate) builtins: HashSet<String>,
    /// Set of node kinds whose choices are all tokens.
    pub(crate) token_choice_kinds: HashSet<String>,
}

impl Model {
    pub fn push_node(&mut self, section: String, node: impl Into<Node>) {
        let new_node = node.into();
        if let Some(old_node) = self.all_nodes().find(|node| node.name() == new_node.name()) {
            assert_eq!(
                &new_node,
                old_node,
                "Node {} defined multiple, non-identical times",
                new_node.name()
            );
            return;
        }
        self.sections.entry(section).or_default().push(new_node);
    }

    pub fn push_builtin(&mut self, node: String) {
        self.builtins.insert(node);
    }

    pub fn sections(&self) -> &HashMap<String, Vec<Node>> {
        &self.sections
    }

    /// Returns true if the given node kind is a choice node whose choices are all tokens.
    pub fn is_token_choice(&self, kind: &str) -> bool {
        self.token_choice_kinds.contains(kind)
    }

    // MARK: Checks
    pub fn do_checks(&self) {
        self.check_no_duplicates();
        self.check_all_nodes_exist();
        self.check_choices_are_unique();
    }

    pub fn check_no_duplicates(&self) {
        for (section, nodes) in &self.sections {
            for node in nodes {
                let mut seen = HashSet::new();
                match node {
                    Node::Items(seq_node) => {
                        for item in &seq_node.items {
                            let name = item.getter_name();
                            if seen.contains(&name) {
                                panic!(
                                    "Duplicate node {} in node {} (section {})",
                                    name,
                                    node.name(),
                                    section
                                )
                            }
                            seen.insert(name);
                        }
                    }
                    Node::Choices(choices_node) => match &choices_node.items {
                        NodesOrTokens::Nodes(nodes) => {
                            for item in nodes {
                                let name = item.getter_name();
                                if seen.contains(&name) {
                                    panic!("Duplicate node {} in node {}", name, node.name())
                                }
                                seen.insert(name);
                            }
                        }
                        NodesOrTokens::Tokens(tokens) => {
                            for item in tokens {
                                let name = item.getter_name();
                                if seen.contains(&name) {
                                    panic!("Duplicate node {} in node {}", name, node.name())
                                }
                                seen.insert(name);
                            }
                        }
                    },
                }
            }
        }
    }

    pub fn check_all_nodes_exist(&self) {
        let defined = self.collect_all_node_kinds();
        let referenced = self.collect_referenced_nodes();

        let referenced_not_defined: Vec<_> = referenced.difference(&defined).collect();
        if !referenced_not_defined.is_empty() {
            println!("The following nodes are referenced, but not defined:");
            for node in referenced_not_defined {
                println!("{node}");
            }
            panic!()
        }

        let mut defined_not_referenced: HashSet<_> = defined.difference(&referenced).collect();
        let top_node = "DesignFile".to_owned();
        assert!(
            defined_not_referenced.contains(&top_node),
            "'DesignFile' is not the top node (was referenced by some other production)"
        );
        defined_not_referenced.remove(&top_node);
        if !defined_not_referenced.is_empty() {
            println!("The following nodes are defined, but never referenced:");
            for node in defined_not_referenced {
                println!("{node}");
            }
            panic!()
        }
    }

    /// Check that all `Choice` nodes contain elements that are only reachable by this choice
    pub fn check_choices_are_unique(&self) {
        let mut found_nodes = HashSet::new();
        for node in self.all_nodes() {
            match node {
                Node::Items(_) => {}
                Node::Choices(choice) => match &choice.items {
                    NodesOrTokens::Nodes(nodes) => {
                        for node in nodes {
                            let used_sites = self.check_where_node_is_used(node);
                            if used_sites.len() > 1 && !found_nodes.contains(&node.name) {
                                found_nodes.insert(node.name.clone());
                                println!("Node {} is used multiple times, but must only be used in a single choice node", node.name);
                            }
                        }
                    }
                    NodesOrTokens::Tokens(_) => {}
                },
            }
        }
    }

    pub fn check_where_node_is_used(&self, orig_node: &NodeRef) -> Vec<NodeRef> {
        let mut referenced_nodes = vec![];
        for node in self.all_nodes() {
            match node {
                Node::Items(items) => {
                    for item in &items.items {
                        match item {
                            TokenOrNode::Node(node) => {
                                if node == orig_node {
                                    referenced_nodes.push(node.clone());
                                }
                            }
                            TokenOrNode::Token(_) => {}
                        }
                    }
                }
                Node::Choices(choices) => match &choices.items {
                    NodesOrTokens::Nodes(nodes) => {
                        for node in nodes {
                            if node == orig_node {
                                referenced_nodes.push(node.clone());
                            }
                        }
                    }
                    NodesOrTokens::Tokens(_) => {}
                },
            }
        }
        referenced_nodes
    }

    // MARK: Postprocessing

    pub fn do_postprocessing(&mut self) {
        self.token_choice_kinds = self
            .all_nodes()
            .filter(|node| node.is_all_token_choices())
            .map(|node| node.name())
            .collect();
    }

    fn collect_referenced_nodes(&self) -> HashSet<String> {
        let mut referenced = HashSet::new();
        for node in self.sections.values().flatten() {
            match node {
                Node::Items(seq_node) => {
                    for item in &seq_node.items {
                        if let TokenOrNode::Node(node_ref) = item {
                            referenced.insert(node_ref.kind.clone());
                        }
                    }
                }
                Node::Choices(choices_node) => {
                    if let NodesOrTokens::Nodes(nodes) = &choices_node.items {
                        for node_ref in nodes {
                            referenced.insert(node_ref.kind.clone());
                        }
                    }
                }
            }
        }
        referenced
    }

    pub fn collect_all_node_kinds(&self) -> HashSet<String> {
        self.all_nodes().map(|node| node.name()).collect()
    }

    pub fn collect_all_sequence_node_kinds(&self) -> HashSet<String> {
        self.all_nodes()
            .filter(|node| matches!(node, Node::Items(_)))
            .map(|node| node.name())
            .collect()
    }

    pub fn all_nodes(&self) -> impl Iterator<Item = &Node> {
        self.sections.values().flatten()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_simple_model() -> Model {
        let mut model = Model::default();
        // Add a token-choice node: RelationalOperator -> { EQ | NE | LT }
        let choice = ChoiceNode {
            name: "RelationalOperator".to_string(),
            items: NodesOrTokens::Tokens(vec![
                Token::from(crate::model::token::TokenKind::EQ),
                Token::from(crate::model::token::TokenKind::NE),
            ]),
        };
        model.push_node("test".to_string(), Node::Choices(choice));
        // Add a sequence node that references the choice node
        let seq = SequenceNode::new(
            "DesignFile",
            vec![TokenOrNode::Node(NodeRef {
                kind: "RelationalOperator".to_string(),
                nth: 0,
                builtin: false,
                repeated: false,
                name: "relational_operator".to_string(),
                optional: false,
            })],
        );
        model.push_node("test".to_string(), Node::Items(seq));
        model.do_postprocessing();
        model
    }

    #[test]
    fn is_token_choice_true_for_all_token_choices() {
        let model = make_simple_model();
        assert!(model.is_token_choice("RelationalOperator"));
    }

    #[test]
    fn is_token_choice_false_for_non_token_choice() {
        let model = make_simple_model();
        assert!(!model.is_token_choice("DesignFile"));
    }

    #[test]
    fn is_token_choice_false_for_unknown() {
        let model = make_simple_model();
        assert!(!model.is_token_choice("NonExistent"));
    }

    #[test]
    #[should_panic]
    fn check_no_duplicates_panics_on_duplicate() {
        let mut model = Model::default();
        let seq = SequenceNode::new(
            "DesignFile",
            vec![
                TokenOrNode::Token(Token::from(crate::model::token::TokenKind::EQ)),
                // Same token kind = same getter name → duplicate
                TokenOrNode::Token(Token::from(crate::model::token::TokenKind::EQ)),
            ],
        );
        model.push_node("test".to_string(), Node::Items(seq));
        model.check_no_duplicates();
    }
}
