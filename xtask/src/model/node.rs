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
    /// A node whose children are populated by the parser as raw tokens rather than
    /// being composed from named sub-nodes. Such a node always has ≥1 token at
    /// runtime; it is therefore never empty-capable and never gets a builder.
    RawTokens(String),
}

impl Node {
    pub fn name(&self) -> String {
        match self {
            Node::Items(items) => items.name.clone(),
            Node::Choices(choices) => choices.name.clone(),
            Node::RawTokens(name) => name.clone(),
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

    pub fn as_sequence(&self) -> Option<&SequenceNode> {
        match self {
            Node::Items(seq) => Some(seq),
            Node::Choices(_) | Node::RawTokens(_) => None,
        }
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
        self.check_empty_capable_nodes_marked_optional();
    }

    /// Computes the set of sequence nodes that can produce a completely empty green tree node.
    ///
    /// A node is empty-capable when every item in its sequence is either:
    /// - an optional or repeated token/node, or
    /// - a required node reference whose target is itself empty-capable.
    ///
    /// Nodes with canonical-text tokens (keywords, symbols) are **not** empty-capable because
    /// those tokens are always emitted. The computation is a fixed-point iteration to handle
    /// transitive cases.
    pub fn compute_empty_capable_nodes(&self) -> HashSet<String> {
        let mut empty_capable: HashSet<String> = HashSet::new();
        loop {
            let prev_size = empty_capable.len();
            for node in self.all_nodes() {
                // RawTokens nodes are populated by the parser and always contain ≥1 token;
                // they are never empty-capable even though they have no declared children.
                if let Node::Items(seq) = node {
                    if empty_capable.contains(&seq.name) {
                        continue;
                    }
                    let is_empty_capable = seq.items.iter().all(|item| match item {
                        TokenOrNode::Token(token) => token.optional || token.repeated,
                        TokenOrNode::Node(node_ref) => {
                            node_ref.optional
                                || node_ref.repeated
                                || empty_capable.contains(&node_ref.kind)
                            // Note: a required reference to a RawTokens node correctly
                            // returns false here (raw token nodes never enter empty_capable),
                            // so the parent is rightly considered non-empty-capable.
                        }
                    });
                    if is_empty_capable {
                        empty_capable.insert(seq.name.clone());
                    }
                }
            }
            if empty_capable.len() == prev_size {
                break;
            }
        }
        empty_capable
    }

    /// Checks that every sequence node that can produce empty output is marked `optional: true`
    /// at every non-repeated use site.
    ///
    /// The syntax tree silently drops empty nodes, so a required reference to an empty-capable
    /// node is a modelling error: the child will sometimes be absent but the parent doesn't
    /// declare it as optional.
    pub fn check_empty_capable_nodes_marked_optional(&self) {
        // Known limitation: the model has no "one-or-more" (required-non-empty list) concept.
        // A node whose items are all `repeated: true` (e.g. NameList, PartialPathname) is
        // structurally empty-capable even when the VHDL grammar guarantees ≥1 element at that
        // use site. Such nodes must still be marked `optional: true` in the YAML so that their
        // accessor returns `Option<T>` rather than causing a model inconsistency. The semantic
        // "must be present" constraint is enforced by the parser and the analysis layer.
        let empty_capable = self.compute_empty_capable_nodes();
        let mut violations: Vec<(String, String)> = vec![];
        for node in self.all_nodes() {
            if let Node::Items(seq) = node {
                for item in &seq.items {
                    if let TokenOrNode::Node(node_ref) = item {
                        if !node_ref.optional
                            && !node_ref.repeated
                            && empty_capable.contains(&node_ref.kind)
                        {
                            violations.push((seq.name.clone(), node_ref.kind.clone()));
                        }
                    }
                }
            }
        }
        if !violations.is_empty() {
            println!(
                "The following nodes can produce empty output but are used without `optional: true`:"
            );
            for (parent, child) in &violations {
                println!("  {child} in {parent}");
            }
            panic!("fix the violations above by adding `optional: true` to each listed node reference in the YAML definitions");
        }
    }

    pub fn check_no_duplicates(&self) {
        for (section, nodes) in &self.sections {
            for node in nodes {
                let mut seen = HashSet::new();
                match node {
                    Node::RawTokens(_) => {}
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
                Node::Items(_) | Node::RawTokens(_) => {}
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
                Node::RawTokens(_) => {}
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

    /// Automatically marks required (non-optional, non-repeated) inner node references as
    /// `optional` when the referenced node is empty-capable.
    ///
    /// This is needed for auto-generated wrapper nodes (e.g. `SemiColonTerminatedBindingIndication`
    /// from `terminated: SemiColon`, or `ParenthesizedInterfaceList` from `parenthesized: true`)
    /// that are created programmatically without knowledge of whether their inner node is
    /// empty-capable. These wrappers always have a canonical delimiter token, so marking the inner
    /// node as optional does not make the wrapper itself empty-capable.
    pub fn fixup_empty_capable_optional_markers(&mut self) {
        let empty_capable = self.compute_empty_capable_nodes();
        for section in self.sections.values_mut() {
            for node in section.iter_mut() {
                if let Node::Items(seq) = node {
                    for item in &mut seq.items {
                        if let TokenOrNode::Node(node_ref) = item {
                            if !node_ref.optional
                                && !node_ref.repeated
                                && empty_capable.contains(&node_ref.kind)
                            {
                                node_ref.optional = true;
                            }
                        }
                    }
                }
            }
        }
    }

    fn collect_referenced_nodes(&self) -> HashSet<String> {
        let mut referenced = HashSet::new();
        for node in self.sections.values().flatten() {
            match node {
                Node::RawTokens(_) => {}
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
            .filter(|node| matches!(node, Node::Items(_) | Node::RawTokens(_)))
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

    /// A node whose items are all repeated is empty-capable.
    #[test]
    fn compute_empty_capable_nodes_all_repeated() {
        let mut model = Model::default();
        // InterfaceList: only repeated items → empty-capable
        let list = SequenceNode::new(
            "InterfaceList",
            vec![TokenOrNode::Node(NodeRef {
                kind: "DesignFile".to_string(),
                nth: 0,
                builtin: false,
                repeated: true,
                name: "items".to_string(),
                optional: false,
            })],
        );
        // DesignFile: required non-canonical token → NOT empty-capable
        let root = SequenceNode::new(
            "DesignFile",
            vec![TokenOrNode::Node(NodeRef {
                kind: "InterfaceList".to_string(),
                nth: 0,
                builtin: false,
                repeated: false,
                name: "interface_list".to_string(),
                optional: false,
            })],
        );
        model.push_node("test".to_string(), Node::Items(list));
        model.push_node("test".to_string(), Node::Items(root));
        model.do_postprocessing();

        let empty_capable = model.compute_empty_capable_nodes();
        assert!(
            empty_capable.contains("InterfaceList"),
            "InterfaceList (all repeated) must be empty-capable"
        );
        // DesignFile contains a required reference to InterfaceList, which is empty-capable,
        // so DesignFile is itself empty-capable too.
        assert!(
            empty_capable.contains("DesignFile"),
            "DesignFile (required ref to empty-capable child) must be empty-capable"
        );
    }

    /// A node with a required canonical-text token is NOT empty-capable.
    #[test]
    fn compute_empty_capable_nodes_canonical_token_not_empty() {
        use crate::model::token::TokenKind;
        let mut model = Model::default();
        let seq = SequenceNode::new(
            "DesignFile",
            vec![TokenOrNode::Token(Token::from(TokenKind::SemiColon))],
        );
        model.push_node("test".to_string(), Node::Items(seq));
        model.do_postprocessing();

        let empty_capable = model.compute_empty_capable_nodes();
        assert!(
            !empty_capable.contains("DesignFile"),
            "DesignFile with required canonical token must NOT be empty-capable"
        );
    }

    /// A required use of an empty-capable node must trigger the check.
    #[test]
    #[should_panic(expected = "optional: true")]
    fn check_empty_capable_required_use_panics() {
        let mut model = Model::default();
        // Leaf: all-optional → empty-capable
        let leaf = SequenceNode::new(
            "Leaf",
            vec![TokenOrNode::Node(NodeRef {
                kind: "DesignFile".to_string(),
                nth: 0,
                builtin: false,
                repeated: true,
                name: "items".to_string(),
                optional: false,
            })],
        );
        // Root: required (non-optional, non-repeated) reference to empty-capable Leaf → violation
        let root = SequenceNode::new(
            "DesignFile",
            vec![TokenOrNode::Node(NodeRef {
                kind: "Leaf".to_string(),
                nth: 0,
                builtin: false,
                repeated: false,
                name: "leaf".to_string(),
                optional: false, // ← violation
            })],
        );
        model.push_node("test".to_string(), Node::Items(leaf));
        model.push_node("test".to_string(), Node::Items(root));
        model.do_postprocessing();
        model.check_empty_capable_nodes_marked_optional();
    }

    /// A repeated use of an empty-capable node is fine (no panic).
    #[test]
    fn check_empty_capable_repeated_use_is_ok() {
        let mut model = Model::default();
        let leaf = SequenceNode::new(
            "Leaf",
            vec![TokenOrNode::Node(NodeRef {
                kind: "DesignFile".to_string(),
                nth: 0,
                builtin: false,
                repeated: true,
                name: "items".to_string(),
                optional: false,
            })],
        );
        let root = SequenceNode::new(
            "DesignFile",
            vec![TokenOrNode::Node(NodeRef {
                kind: "Leaf".to_string(),
                nth: 0,
                builtin: false,
                repeated: true, // repeated → fine
                name: "leaf".to_string(),
                optional: false,
            })],
        );
        model.push_node("test".to_string(), Node::Items(leaf));
        model.push_node("test".to_string(), Node::Items(root));
        model.do_postprocessing();
        model.check_empty_capable_nodes_marked_optional(); // must not panic
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
