// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2025, Lukas Scheller lukasscheller@icloud.com

use crate::model::TokenKind;
use convert_case::{Case, Casing};
use std::borrow::Borrow;
use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};

/// The name of a production in the grammar, i.e. the kind of a syntax node.
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
pub struct NodeKind(String);

impl NodeKind {
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl From<&NodeKind> for NodeKind {
    fn from(val: &NodeKind) -> Self {
        NodeKind(val.0.clone())
    }
}

impl Borrow<str> for NodeKind {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl AsRef<str> for NodeKind {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl Display for NodeKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl From<String> for NodeKind {
    fn from(value: String) -> Self {
        NodeKind(value)
    }
}

impl From<&str> for NodeKind {
    fn from(value: &str) -> Self {
        NodeKind(value.to_owned())
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum NodeOrTokenKind {
    Node(NodeKind),
    Token(TokenKind),
}

/// How often a [`Field`] occurs in its parent production.
///
/// `nth` distinguishes several fields of the same kind within one production (the 2nd
/// `Identifier`, say) and is therefore meaningless for a repeated field, which owns every
/// occurrence of its kind — hence it lives in the variants rather than beside them.
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Cardinality {
    Required { nth: usize },
    Optional { nth: usize },
    Repeated,
}

impl Cardinality {
    pub fn is_optional(self) -> bool {
        matches!(self, Cardinality::Optional { .. })
    }

    pub fn is_repeated(self) -> bool {
        matches!(self, Cardinality::Repeated)
    }

    /// Whether the field may be absent from the green tree — i.e. everything but `Required`.
    pub fn may_be_absent(self) -> bool {
        !matches!(self, Cardinality::Required { .. })
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Field {
    pub kind: NodeOrTokenKind,
    pub name: String,
    pub cardinality: Cardinality,
}

impl Field {
    pub fn token(kind: TokenKind) -> Field {
        Field {
            name: kind.default_name(),
            kind: NodeOrTokenKind::Token(kind),
            cardinality: Cardinality::Required { nth: 0 },
        }
    }

    pub fn node(kind: impl Into<NodeKind>) -> Field {
        let kind = kind.into();
        Field {
            name: kind.as_str().to_owned(),
            kind: NodeOrTokenKind::Node(kind),
            cardinality: Cardinality::Required { nth: 0 },
        }
    }

    pub fn with_name(self, name: impl Into<String>) -> Field {
        Field {
            name: name.into(),
            ..self
        }
    }

    /// Sets which occurrence of its kind this field denotes, keeping the cardinality.
    pub fn with_nth(self, nth: usize) -> Field {
        let cardinality = match self.cardinality {
            Cardinality::Required { .. } => Cardinality::Required { nth },
            Cardinality::Optional { .. } => Cardinality::Optional { nth },
            Cardinality::Repeated => Cardinality::Repeated,
        };
        Field {
            cardinality,
            ..self
        }
    }

    pub fn make_optional(mut self) -> Field {
        self.set_optional();
        self
    }

    /// Marks the field optional in place, keeping its `nth`. A repeated field already may be
    /// absent, so it is left alone.
    pub fn set_optional(&mut self) {
        if let Cardinality::Required { nth } = self.cardinality {
            self.cardinality = Cardinality::Optional { nth };
        }
    }

    pub fn make_repeated(mut self) -> Field {
        if self.as_node_kind().is_some() {
            // The accessor for a repeated node is plural. Naive, but the only names it
            // has to handle are the node names in the grammar file.
            if !self.name.ends_with('s') {
                self.name.push('s');
            }
        }

        Field {
            cardinality: Cardinality::Repeated,
            ..self
        }
    }

    pub fn is_optional(&self) -> bool {
        self.cardinality.is_optional()
    }

    pub fn is_repeated(&self) -> bool {
        self.cardinality.is_repeated()
    }

    /// Whether the field may be absent from the green tree — i.e. everything but `Required`.
    pub fn may_be_absent(&self) -> bool {
        self.cardinality.may_be_absent()
    }

    pub fn getter_name(&self) -> String {
        match self.kind {
            NodeOrTokenKind::Node(_) => self.name.to_case(Case::Snake),
            NodeOrTokenKind::Token(_) => format!("{}_token", self.name.to_case(Case::Snake)),
        }
    }

    /// The kind of the referenced token, or `None` when this item references a node.
    pub fn as_token_kind(&self) -> Option<&TokenKind> {
        match &self.kind {
            NodeOrTokenKind::Token(kind) => Some(kind),
            NodeOrTokenKind::Node(_) => None,
        }
    }

    /// The kind of the referenced node, or `None` when this item references a token.
    pub fn as_node_kind(&self) -> Option<&NodeKind> {
        match &self.kind {
            NodeOrTokenKind::Node(kind) => Some(kind),
            NodeOrTokenKind::Token(_) => None,
        }
    }
}

impl From<TokenKind> for Field {
    fn from(value: TokenKind) -> Self {
        Field::token(value)
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Node {
    Items(SequenceNode),
    Choices(ChoiceNode),
    List(ListNode),
}

impl Node {
    pub fn name(&self) -> &NodeKind {
        match self {
            Node::Items(items) => &items.name,
            Node::Choices(choices) => &choices.name,
            Node::List(list) => &list.kind,
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
            Node::Choices(_) | Node::List(_) => None,
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

impl From<ListNode> for Node {
    fn from(value: ListNode) -> Self {
        Node::List(value)
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct SequenceNode {
    pub name: NodeKind,
    pub items: Vec<Field>,
}

#[cfg(test)]
impl SequenceNode {
    /// Test-only convenience constructor. Production code builds `SequenceNode`s in
    /// [`crate::model::load_model`], which takes the name verbatim from the ungrammar.
    pub fn new(name: impl Into<NodeKind>, items: Vec<Field>) -> SequenceNode {
        SequenceNode {
            name: name.into(),
            items,
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum NodesOrTokens {
    /// The alternatives of a node choice, as node kinds.
    Nodes(Vec<NodeKind>),
    /// The alternatives of a token choice, as token kinds. Same rule as [`NodesOrTokens::Nodes`]:
    /// an alternative is always a bare token reference.
    Tokens(Vec<TokenKind>),
}

impl FromIterator<NodeKind> for NodesOrTokens {
    fn from_iter<T: IntoIterator<Item = NodeKind>>(iter: T) -> Self {
        NodesOrTokens::Nodes(iter.into_iter().collect())
    }
}

impl FromIterator<TokenKind> for NodesOrTokens {
    fn from_iter<T: IntoIterator<Item = TokenKind>>(iter: T) -> Self {
        NodesOrTokens::Tokens(iter.into_iter().collect())
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct ChoiceNode {
    pub name: NodeKind,
    pub items: NodesOrTokens,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct ListNode {
    pub kind: NodeKind,
    pub element: Field,
    pub separator: Field,
}

#[derive(Debug, Default)]
pub struct Model {
    pub(crate) nodes: HashMap<NodeKind, Node>,
    /// Set of node kinds whose choices are all tokens.
    pub(crate) token_choice_kinds: HashSet<NodeKind>,
}

impl Model {
    pub fn push_node(&mut self, node: impl Into<Node>) {
        let node = node.into();
        if let Some(previous) = self.nodes.insert(node.name().clone(), node.clone()) {
            if previous != node {
                panic!(
                    "Node {} is defined twice with different content. A production name and the \
                    label of an inlined group share one namespace, so one name always means one \
                    node: repeating a construct at several use sites is fine, but every \
                    definition of {} must be spelled identically.",
                    previous.name(),
                    previous.name()
                );
            }
        }
    }

    /// The node of the given kind, or `None` when the grammar defines no such production.
    pub fn node(&self, kind: &NodeKind) -> Option<&Node> {
        self.nodes.get(kind)
    }

    /// Returns true if the given node kind is a choice node whose choices are all tokens.
    pub fn is_token_choice(&self, kind: &NodeKind) -> bool {
        self.token_choice_kinds.contains(kind)
    }

    // MARK: Checks
    pub fn do_checks(&self) {
        self.check_no_duplicates();
        self.check_all_nodes_exist();
        self.check_choices_are_unique();
        self.check_empty_capable_nodes_marked_optional();
        self.check_nth_accessors_are_unambiguous();
    }

    /// Checks that every `nth`-based accessor actually addresses the field it was generated for.
    pub fn check_nth_accessors_are_unambiguous(&self) {
        let mut violations: Vec<String> = vec![];
        for node in self.all_nodes() {
            let Node::Items(seq) = node else {
                continue;
            };
            for (index, item) in seq.items.iter().enumerate() {
                if item.is_repeated() {
                    if let Some((_, other)) =
                        seq.items.iter().enumerate().find(|(other_index, other)| {
                            *other_index != index && other.kind == item.kind
                        })
                    {
                        violations.push(format!(
                            "  {} in {}: repeated field shares its kind with `{}`",
                            item.name, seq.name, other.name
                        ));
                    }
                    continue;
                }
                for earlier in seq.items[..index]
                    .iter()
                    .filter(|earlier| earlier.kind == item.kind)
                {
                    if earlier.may_be_absent() {
                        violations.push(format!(
                            "  {} in {}: preceded by `{}` of the same kind, which may be absent",
                            item.name, seq.name, earlier.name
                        ));
                    }
                }
            }
        }
        if !violations.is_empty() {
            println!("The following fields cannot be addressed by their position:");
            for violation in &violations {
                println!("{violation}");
            }
            panic!(
                "fix the violations above by giving the conflicting fields distinct node kinds, \
                 e.g. by wrapping them in a labelled group"
            );
        }
    }

    /// Computes the set of sequence nodes that can produce a completely empty green tree node.
    ///
    /// A sequence node is empty-capable when every item is either:
    /// - an optional or repeated token/node, or
    /// - a required node reference whose target is itself empty-capable.
    ///
    /// A choice node is empty-capable when any of its alternatives is.
    ///
    /// Nodes with canonical-text tokens (keywords, symbols) are **not** empty-capable because
    /// those tokens are always emitted. The computation is a fixed-point iteration to handle
    /// transitive cases.
    pub fn compute_empty_capable_nodes(&self) -> HashSet<NodeKind> {
        let mut empty_capable: HashSet<NodeKind> = HashSet::new();
        loop {
            let prev_size = empty_capable.len();
            for node in self.all_nodes() {
                match node {
                    Node::Items(seq) => {
                        if empty_capable.contains(&seq.name) {
                            continue;
                        }
                        let is_empty_capable = seq.items.iter().all(|item| match &item.kind {
                            NodeOrTokenKind::Token(_) => item.may_be_absent(),
                            NodeOrTokenKind::Node(kind) => {
                                item.may_be_absent() || empty_capable.contains(kind)
                            }
                        });
                        if is_empty_capable {
                            empty_capable.insert(seq.name.clone());
                        }
                    }
                    // A choice is empty-capable when any alternative is: the parser
                    // can select that alternative, emit nothing, and the empty node
                    // is dropped — leaving the choice reference absent in the parent.
                    Node::Choices(choice) => {
                        if empty_capable.contains(&choice.name) {
                            continue;
                        }
                        if let NodesOrTokens::Nodes(options) = &choice.items {
                            if options.iter().any(|option| empty_capable.contains(option)) {
                                empty_capable.insert(choice.name.clone());
                            }
                        }
                    }
                    // Lists are always non-optional
                    Node::List(_) => {}
                }
            }
            if empty_capable.len() == prev_size {
                break;
            }
        }
        empty_capable
    }

    /// Checks that every sequence node that can produce empty output is marked optional (`?`)
    /// at every non-repeated use site.
    ///
    /// The syntax tree silently drops empty nodes, so a required reference to an empty-capable
    /// node is a modelling error: the child will sometimes be absent but the parent doesn't
    /// declare it as optional.
    pub fn check_empty_capable_nodes_marked_optional(&self) {
        // Known limitation: the model has no "one-or-more" (required-non-empty list) concept.
        // A node whose items are all repeated (e.g. NameList, PartialPathname) is structurally
        // empty-capable even when the VHDL grammar guarantees ≥1 element at that use site. Such
        // nodes must still be marked optional (`?`) in the grammar so that their accessor returns
        // `Option<T>` rather than causing a model inconsistency. The semantic "must be present"
        // constraint is enforced by the parser and the analysis layer.
        let empty_capable = self.compute_empty_capable_nodes();
        let mut violations: Vec<(&NodeKind, &NodeKind)> = vec![];
        for node in self.all_nodes() {
            if let Node::Items(seq) = node {
                for item in &seq.items {
                    if let NodeOrTokenKind::Node(kind) = &item.kind {
                        if !item.may_be_absent() && empty_capable.contains(kind) {
                            violations.push((&seq.name, kind));
                        }
                    }
                }
            }
        }
        if !violations.is_empty() {
            println!("The following nodes can produce empty output but are used without `?`:");
            for (parent, child) in &violations {
                println!("  {child} in {parent}");
            }
            panic!("fix the violations above by appending `?` to each listed node reference in the grammar definition");
        }
    }

    pub fn check_no_duplicates(&self) {
        for node in self.all_nodes() {
            let mut seen = HashSet::new();
            let mut check_field = |field: &Field| {
                let name = field.getter_name();
                if seen.contains(&name) {
                    panic!("Duplicate node {} in node {}", name, node.name())
                }
                seen.insert(name);
            };

            match node {
                Node::Items(seq_node) => {
                    for item in &seq_node.items {
                        check_field(item);
                    }
                }
                Node::Choices(choices_node) => match &choices_node.items {
                    NodesOrTokens::Nodes(nodes) => {
                        for item in nodes {
                            let name = item.as_str().to_case(Case::Snake);
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
                Node::List(list) => {
                    check_field(&list.element);
                    check_field(&list.separator);
                }
            }
        }
    }

    pub fn check_all_nodes_exist(&self) {
        let defined = self.collect_all_node_kinds();
        let referenced = self.collect_referenced_nodes();

        let mut defined_not_referenced: HashSet<_> = defined.difference(&referenced).collect();
        let top_node = NodeKind::from("DesignFile");
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
                Node::Items(_) | Node::List(_) => {}
                Node::Choices(choice) => match &choice.items {
                    NodesOrTokens::Nodes(nodes) => {
                        for node in nodes {
                            if self.count_uses_of_node(node) > 1 && !found_nodes.contains(node) {
                                found_nodes.insert(node.clone());
                                println!("Node {node} is used multiple times, but must only be used in a single choice node");
                            }
                        }
                    }
                    NodesOrTokens::Tokens(_) => {}
                },
            }
        }
    }

    /// The number of places (sequence items and choice alternatives) that reference `kind`.
    pub fn count_uses_of_node(&self, kind: &NodeKind) -> usize {
        let mut uses = 0;
        for node in self.all_nodes() {
            match node {
                Node::Items(items) => {
                    uses += items
                        .items
                        .iter()
                        .filter(|item| item.as_node_kind() == Some(kind))
                        .count();
                }
                Node::Choices(choices) => match &choices.items {
                    NodesOrTokens::Nodes(nodes) => {
                        uses += nodes.iter().filter(|node| *node == kind).count();
                    }
                    NodesOrTokens::Tokens(_) => {}
                },
                Node::List(list) => {
                    if list.element.as_node_kind() == Some(kind) {
                        uses += 1;
                    }
                    if list.separator.as_node_kind() == Some(kind) {
                        uses += 1;
                    }
                }
            }
        }
        uses
    }

    // MARK: Postprocessing

    pub fn do_postprocessing(&mut self) {
        self.token_choice_kinds = self
            .all_nodes()
            .filter(|node| node.is_all_token_choices())
            .map(|node| node.name().clone())
            .collect();
    }

    /// Automatically marks required (non-optional, non-repeated) inner node references as
    /// `optional` when the referenced node is empty-capable.
    ///
    /// This spares the grammar from spelling out `?` on wrapper nodes whose sole purpose is to
    /// attach a delimiter to an inner node (e.g. `SemiColonTerminatedBindingIndication` or
    /// `ParenthesizedInterfaceList`), where whether the inner node is empty-capable is a
    /// non-local property. These wrappers always have a canonical delimiter token, so marking the
    /// inner node as optional does not make the wrapper itself empty-capable.
    pub fn fixup_empty_capable_optional_markers(&mut self) {
        let empty_capable = self.compute_empty_capable_nodes();
        for node in self.nodes.values_mut() {
            if let Node::Items(seq) = node {
                for item in &mut seq.items {
                    if let NodeOrTokenKind::Node(node_ref) = &item.kind {
                        if !item.may_be_absent() && empty_capable.contains(node_ref) {
                            item.set_optional();
                        }
                    }
                }
            }
        }
    }

    fn collect_referenced_nodes(&self) -> HashSet<NodeKind> {
        let mut referenced = HashSet::new();
        for node in self.all_nodes() {
            match node {
                Node::Items(seq_node) => {
                    for item in &seq_node.items {
                        if let Some(kind) = item.as_node_kind() {
                            referenced.insert(kind.clone());
                        }
                    }
                }
                Node::Choices(choices_node) => {
                    if let NodesOrTokens::Nodes(nodes) = &choices_node.items {
                        referenced.extend(nodes.iter().cloned());
                    }
                }
                Node::List(list) => {
                    if let Some(kind) = list.element.as_node_kind() {
                        referenced.insert(kind.clone());
                    }
                    if let Some(kind) = list.separator.as_node_kind() {
                        referenced.insert(kind.clone());
                    }
                }
            }
        }
        referenced
    }

    pub fn collect_all_node_kinds(&self) -> HashSet<NodeKind> {
        self.all_nodes().map(|node| node.name().clone()).collect()
    }

    /// The kinds that the parser actually materializes as green nodes: sequences and lists.
    /// A choice is abstract — the parser emits one of its options, never the choice itself —
    /// so choice kinds are deliberately absent from the generated `NodeKind` enum.
    pub fn collect_all_materialized_node_kinds(&self) -> HashSet<NodeKind> {
        self.all_nodes()
            .filter(|node| matches!(node, Node::Items(_) | Node::List(_)))
            .map(|node| node.name().clone())
            .collect()
    }

    /// Iterates the nodes in unspecified order
    pub fn all_nodes(&self) -> impl Iterator<Item = &Node> {
        self.nodes.values()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_simple_model() -> Model {
        let mut model = Model::default();
        // Add a token-choice node: RelationalOperator -> { EQ | NE | LT }
        let choice = ChoiceNode {
            name: NodeKind::from("RelationalOperator"),
            items: NodesOrTokens::Tokens(vec![TokenKind::EQ, TokenKind::NE]),
        };
        model.push_node(Node::Choices(choice));
        // Add a sequence node that references the choice node
        let seq = SequenceNode::new("DesignFile", vec![Field::node("RelationalOperator")]);
        model.push_node(Node::Items(seq));
        model.do_postprocessing();
        model
    }

    #[test]
    fn is_token_choice_true_for_all_token_choices() {
        let model = make_simple_model();
        assert!(model.is_token_choice(&NodeKind::from("RelationalOperator")));
    }

    #[test]
    fn is_token_choice_false_for_non_token_choice() {
        let model = make_simple_model();
        assert!(!model.is_token_choice(&NodeKind::from("DesignFile")));
    }

    #[test]
    fn is_token_choice_false_for_unknown() {
        let model = make_simple_model();
        assert!(!model.is_token_choice(&NodeKind::from("NonExistent")));
    }

    /// A node whose items are all repeated is empty-capable.
    #[test]
    fn compute_empty_capable_nodes_all_repeated() {
        let mut model = Model::default();
        // InterfaceList: only repeated items → empty-capable
        let list = SequenceNode::new(
            "InterfaceList",
            vec![Field::node("DesignFile").make_repeated().with_name("items")],
        );
        // DesignFile: required non-canonical token → NOT empty-capable
        let root = SequenceNode::new("DesignFile", vec![Field::node("InterfaceList")]);
        model.push_node(Node::Items(list));
        model.push_node(Node::Items(root));
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
        let mut model = Model::default();
        let seq = SequenceNode::new("DesignFile", vec![Field::token(TokenKind::SemiColon)]);
        model.push_node(Node::Items(seq));
        model.do_postprocessing();

        let empty_capable = model.compute_empty_capable_nodes();
        assert!(
            !empty_capable.contains("DesignFile"),
            "DesignFile with required canonical token must NOT be empty-capable"
        );
    }

    /// A required use of an empty-capable node must trigger the check.
    #[test]
    #[should_panic(expected = "appending `?`")]
    fn check_empty_capable_required_use_panics() {
        let mut model = Model::default();
        // Leaf: all-optional → empty-capable
        let leaf = SequenceNode::new(
            "Leaf",
            vec![Field::node("DesignFile").make_repeated().with_name("items")],
        );
        // Root: required (non-optional, non-repeated) reference to empty-capable Leaf → violation
        let root = SequenceNode::new("DesignFile", vec![Field::node("Leaf")]);
        model.push_node(Node::Items(leaf));
        model.push_node(Node::Items(root));
        model.do_postprocessing();
        model.check_empty_capable_nodes_marked_optional();
    }

    /// A repeated use of an empty-capable node is fine (no panic).
    #[test]
    fn check_empty_capable_repeated_use_is_ok() {
        let mut model = Model::default();
        let leaf = SequenceNode::new("Leaf", vec![Field::node("DesignFile").make_repeated()]);
        let root = SequenceNode::new("DesignFile", vec![Field::node("Leaf").make_repeated()]);
        model.push_node(Node::Items(leaf));
        model.push_node(Node::Items(root));
        model.do_postprocessing();
        model.check_empty_capable_nodes_marked_optional(); // must not panic
    }

    fn model_with(items: Vec<Field>) -> Model {
        let mut model = Model::default();
        model.push_node(Node::Items(SequenceNode::new("DesignFile", items)));
        model
    }

    /// `A A?` is fine: the optional one comes last, so nothing shifts.
    #[test]
    fn check_nth_accessors_allows_trailing_optional() {
        let model = model_with(vec![
            Field::node("Expression").with_name("condition"),
            Field::node("Expression").with_nth(1).make_optional(),
        ]);
        model.check_nth_accessors_are_unambiguous(); // must not panic
    }

    /// `A? A` shifts the second field's ordinal whenever the first is absent.
    #[test]
    #[should_panic(expected = "distinct node kinds")]
    fn check_nth_accessors_rejects_preceding_optional() {
        let model = model_with(vec![
            Field::node("Expression")
                .make_optional()
                .with_name("report"),
            Field::node("Expression")
                .with_nth(1)
                .make_optional()
                .with_name("severity"),
        ]);
        model.check_nth_accessors_are_unambiguous();
    }

    /// The same holds for tokens.
    #[test]
    #[should_panic(expected = "distinct node kinds")]
    fn check_nth_accessors_rejects_preceding_optional_token() {
        let model = model_with(vec![
            Field::token(TokenKind::SemiColon).make_optional(),
            Field::token(TokenKind::SemiColon).with_nth(1),
        ]);
        model.check_nth_accessors_are_unambiguous();
    }

    /// A repeated field owns every child of its kind, so no sibling may share it.
    #[test]
    #[should_panic(expected = "distinct node kinds")]
    fn check_nth_accessors_rejects_repeated_sharing_a_kind() {
        let model = model_with(vec![
            Field::node("Expression").with_name("first"),
            Field::node("Expression").make_repeated().with_name("rest"),
        ]);
        model.check_nth_accessors_are_unambiguous();
    }

    #[test]
    #[should_panic]
    fn check_no_duplicates_panics_on_duplicate() {
        let mut model = Model::default();
        let seq = SequenceNode::new(
            "DesignFile",
            vec![
                Field::token(TokenKind::EQ),
                // Same token kind = same getter name → duplicate
                Field::token(TokenKind::EQ),
            ],
        );
        model.push_node(Node::Items(seq));
        model.check_no_duplicates();
    }
}
