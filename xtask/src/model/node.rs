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

impl NodeOrTokenKind {
    /// The node kind, or `None` when this is a token.
    pub fn as_node_kind(&self) -> Option<&NodeKind> {
        match self {
            NodeOrTokenKind::Node(kind) => Some(kind),
            NodeOrTokenKind::Token(_) => None,
        }
    }

    /// The token kind, or `None` when this is a node.
    pub fn as_token_kind(&self) -> Option<&TokenKind> {
        match self {
            NodeOrTokenKind::Token(kind) => Some(kind),
            NodeOrTokenKind::Node(_) => None,
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum RepeatedCardinality {
    ZeroOrMore,
    OneOrMore,
}

/// How often a [`Field`] occurs in its parent production.
///
/// `nth` distinguishes several fields of the same kind within one production (the 2nd
/// `Identifier`, say) and is therefore meaningless for a repeated field, which owns every
/// occurrence of its kind — hence it lives in the variants rather than beside them.
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Cardinality {
    Required {
        nth: usize,
    },
    Optional {
        nth: usize,
    },
    Repeated(RepeatedCardinality),
}

impl Cardinality {
    pub fn is_optional(self) -> bool {
        matches!(self, Cardinality::Optional { .. })
    }

    pub fn is_repeated(self) -> bool {
        matches!(self, Cardinality::Repeated(_))
    }

    /// Whether the field may be absent from the green tree — i.e. everything but `Required`
    /// and `RepeatedAtLeastOnce`, both of which always contribute at least one child.
    pub fn may_be_absent(self) -> bool {
        matches!(
            self,
            Cardinality::Optional { .. } | Cardinality::Repeated(RepeatedCardinality::ZeroOrMore)
        )
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

    #[cfg(test)]
    pub fn with_name(self, name: impl Into<String>) -> Field {
        Field {
            name: name.into(),
            ..self
        }
    }

    /// Sets which occurrence of its kind this field denotes, keeping the cardinality.
    #[cfg(test)]
    pub fn with_nth(mut self, nth: usize) -> Field {
        self.set_nth(nth);
        self
    }

    /// [`Field::with_nth`], in place. A repeated field owns every occurrence of its kind, so it
    /// has no ordinal to set.
    pub fn set_nth(&mut self, nth: usize) {
        self.cardinality = match self.cardinality {
            Cardinality::Required { .. } => Cardinality::Required { nth },
            Cardinality::Optional { .. } => Cardinality::Optional { nth },
            repeated @ Cardinality::Repeated(_) => repeated,
        };
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
            cardinality: Cardinality::Repeated(RepeatedCardinality::ZeroOrMore),
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
        self.kind.as_token_kind()
    }

    /// The kind of the referenced node, or `None` when this item references a token.
    pub fn as_node_kind(&self) -> Option<&NodeKind> {
        self.kind.as_node_kind()
    }
}

impl From<TokenKind> for Field {
    fn from(value: TokenKind) -> Self {
        Field::token(value)
    }
}

impl From<NodeOrTokenKind> for Field {
    fn from(value: NodeOrTokenKind) -> Self {
        match value {
            NodeOrTokenKind::Node(kind) => Field::node(kind),
            NodeOrTokenKind::Token(kind) => Field::token(kind),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Node {
    Items(SequenceNode),
    Choices(ChoiceNode),
    List(ListNode),
    Alias(AliasNode),
}

impl Node {
    pub fn name(&self) -> &NodeKind {
        match self {
            Node::Items(items) => &items.name,
            Node::Choices(choices) => &choices.name,
            Node::List(list) => &list.kind,
            Node::Alias(alias) => &alias.name,
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
            _ => None,
        }
    }

    /// The node an alias stands for, or `None` for a node that is not an alias.
    pub fn as_alias(&self) -> Option<&AliasNode> {
        match self {
            Node::Alias(alias) => Some(alias),
            _ => None,
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

impl From<AliasNode> for Node {
    fn from(value: AliasNode) -> Self {
        Node::Alias(value)
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
    /// The alternatives of a node choice, as node kinds. An alternative is always a bare node
    /// reference, so the kind is also the name.
    Nodes(Vec<NodeKind>),
    /// The alternatives of a token choice, as spelled: a bare token, or a reference to a
    /// production that renames one (`OperatorSymbol = '#string_literal'`). The name of the
    /// alternative names the variant, [`Model::alternative_token`] gives the token it denotes.
    Tokens(Vec<Field>),
}

impl FromIterator<NodeKind> for NodesOrTokens {
    fn from_iter<T: IntoIterator<Item = NodeKind>>(iter: T) -> Self {
        NodesOrTokens::Nodes(iter.into_iter().collect())
    }
}

impl FromIterator<Field> for NodesOrTokens {
    fn from_iter<T: IntoIterator<Item = Field>>(iter: T) -> Self {
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

/// A second name for another node.
///
/// The alias is a name and nothing else: it materializes no node of its own, so a reference to
/// it is a reference to what it renames. A token can be renamed too — `OthersChoice = 'others'`
/// gives the keyword a node-shaped name, which is what lets it stand as the alternative of a
/// choice.
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct AliasNode {
    /// The kind this alias introduces, i.e. the name under which the aliased node is known here.
    pub name: NodeKind,
    /// What the alias renames. Held as a kind, not a node: the aliased production need not have
    /// been mapped yet when the alias is built, and a token has no node to hold.
    pub aliased: NodeOrTokenKind,
}

impl AliasNode {
    pub fn new(name: impl Into<NodeKind>, aliased: NodeOrTokenKind) -> AliasNode {
        AliasNode {
            name: name.into(),
            aliased,
        }
    }
}

/// Test-only convenience constructors. Production code goes through [`AliasNode::new`] in
/// [`crate::model::load_model`], where the aliased kind comes from the ungrammar.
#[cfg(test)]
impl AliasNode {
    pub fn node(name: impl Into<NodeKind>, aliased: impl Into<NodeKind>) -> AliasNode {
        AliasNode::new(name, NodeOrTokenKind::Node(aliased.into()))
    }

    pub fn token(name: impl Into<NodeKind>, aliased: TokenKind) -> AliasNode {
        AliasNode::new(name, NodeOrTokenKind::Token(aliased))
    }
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

    /// What a reference to `kind` actually denotes in the tree.
    ///
    /// An alias is a pure renaming — it materializes nothing of its own — so a reference to one
    /// resolves to what it renames, through as many alias layers as it takes, and ends at a node
    /// or at a token. Every other kind, and every kind the model doesn't know, resolves to
    /// itself.
    pub fn resolve_alias(&self, kind: &NodeKind) -> NodeOrTokenKind {
        let mut current = kind;
        // An alias chain cannot revisit a kind without being a cycle, so it is at most as long
        // as the model has nodes.
        for _ in 0..=self.nodes.len() {
            match self.node(current) {
                Some(Node::Alias(alias)) => match &alias.aliased {
                    NodeOrTokenKind::Node(next) => current = next,
                    NodeOrTokenKind::Token(token) => return NodeOrTokenKind::Token(*token),
                },
                _ => return NodeOrTokenKind::Node(current.clone()),
            }
        }
        panic!("alias {kind} is part of a cycle: following it never reaches a node or a token");
    }

    /// The token an alternative of a token choice denotes: the token it is, or the token it
    /// renames.
    pub fn alternative_token(&self, alternative: &Field) -> TokenKind {
        match self.resolved_kind(alternative) {
            NodeOrTokenKind::Token(kind) => kind,
            NodeOrTokenKind::Node(kind) => {
                unreachable!("alternative {kind} of a token choice does not denote a token")
            }
        }
    }

    /// The kind a field addresses in the tree, i.e. its own with any alias resolved.
    pub fn resolved_kind(&self, field: &Field) -> NodeOrTokenKind {
        match &field.kind {
            NodeOrTokenKind::Node(kind) => self.resolve_alias(kind),
            NodeOrTokenKind::Token(kind) => NodeOrTokenKind::Token(*kind),
        }
    }

    /// Returns true if the given node kind is a choice node whose choices are all tokens.
    pub fn is_token_choice(&self, kind: &NodeKind) -> bool {
        self.token_choice_kinds.contains(kind)
    }

    // MARK: Checks
    pub fn do_checks(&self) {
        self.check_no_duplicates();
        self.check_all_nodes_exist();
        self.check_aliased_nodes_are_defined();
        self.check_choice_alternatives_are_nodes();
        self.check_choices_are_unique();
        self.check_empty_capable_nodes_marked_optional();
        self.check_nth_accessors_are_unambiguous();
    }

    /// Checks that what every alias renames exists.
    ///
    /// An alias generates nothing: a reference to it resolves to the aliased node's kind, struct
    /// and getters, so those have to be generated from a definition of that node elsewhere in the
    /// model. A token needs no definition, so a token alias is always fine.
    pub fn check_aliased_nodes_are_defined(&self) {
        for node in self.all_nodes() {
            let Some(target) = node
                .as_alias()
                .and_then(|alias| alias.aliased.as_node_kind())
            else {
                continue;
            };
            assert!(
                self.node(target).is_some(),
                "alias {} renames {target}, which is not defined anywhere: an alias is a second \
                 name for a node, so the node it renames must be a production of its own.",
                node.name()
            );
        }
    }

    /// Checks that no alternative of a choice resolves to a token.
    ///
    /// An alternative is spelled as a node reference, but an alias can rename a token
    /// (`OthersChoice = 'others'`), so a choice can end up part nodes and part tokens. The
    /// generated enum would then have to be castable from a token as well as from a node, which
    /// `AstNode` — `cast_unchecked(SyntaxNode)`, `raw() -> SyntaxNode` — cannot express.
    pub fn check_choice_alternatives_are_nodes(&self) {
        let mut violations: Vec<String> = vec![];
        for node in self.all_nodes() {
            let Node::Choices(choice) = node else {
                continue;
            };
            let NodesOrTokens::Nodes(alternatives) = &choice.items else {
                continue;
            };
            for alternative in alternatives {
                if let NodeOrTokenKind::Token(token) = self.resolve_alias(alternative) {
                    violations.push(format!(
                        "  {alternative} in {}: renames the token {token:?}",
                        choice.name
                    ));
                }
            }
        }
        if !violations.is_empty() {
            println!("The following choice alternatives are tokens rather than nodes:");
            for violation in &violations {
                println!("{violation}");
            }
            panic!(
                "a choice is cast from a syntax node, so every alternative must be one; an \
                 alternative that renames a token has no node to cast from"
            );
        }
    }

    /// Checks that every `nth`-based accessor actually addresses the field it was generated for.
    ///
    /// Fields are compared by the kind they *resolve* to, the same kind
    /// [`Model::fixup_nth_by_resolved_kind`] numbered them by: two names for one node — an alias
    /// and the node itself, or two aliases — are one kind to an accessor that casts and counts.
    pub fn check_nth_accessors_are_unambiguous(&self) {
        let mut violations: Vec<String> = vec![];
        for node in self.all_nodes() {
            let Node::Items(seq) = node else {
                continue;
            };
            let kinds: Vec<NodeOrTokenKind> = seq
                .items
                .iter()
                .map(|item| self.resolved_kind(item))
                .collect();
            for (index, item) in seq.items.iter().enumerate() {
                if item.is_repeated() {
                    if let Some(other) = (0..seq.items.len())
                        .find(|&other| other != index && kinds[other] == kinds[index])
                    {
                        violations.push(format!(
                            "  {} in {}: repeated field shares its kind with `{}`",
                            item.name, seq.name, seq.items[other].name
                        ));
                    }
                    continue;
                }
                for (earlier_index, earlier) in seq.items[..index].iter().enumerate() {
                    if kinds[earlier_index] == kinds[index] && earlier.may_be_absent() {
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
    /// An alias node is empty-capable when the node it aliases is.
    ///
    /// Nodes with canonical-text tokens (keywords, symbols) are **not** empty-capable because
    /// those tokens are always emitted. The computation is a fixed-point iteration to handle
    /// transitive cases.
    pub fn compute_empty_capable_nodes(&self) -> HashSet<NodeKind> {
        let mut empty_capable: HashSet<NodeKind> = HashSet::new();
        loop {
            let prev_size = empty_capable.len();
            for node in self.all_nodes() {
                if empty_capable.contains(node.name()) {
                    continue;
                }
                if is_empty_capable(node, &empty_capable) {
                    empty_capable.insert(node.name().clone());
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
            check_no_duplicate_accessors(node);
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
            if let Node::Choices(choice) = node {
                if let NodesOrTokens::Nodes(nodes) = &choice.items {
                    for node in nodes {
                        if self.count_uses_of_node(node) > 1 && !found_nodes.contains(node) {
                            found_nodes.insert(node.clone());
                            println!("Node {node} is used multiple times, but must only be used in a single choice node");
                        }
                    }
                }
            }
        }
    }

    /// The number of places (sequence items and choice alternatives) that reference `kind`.
    pub fn count_uses_of_node(&self, kind: &NodeKind) -> usize {
        self.all_nodes().map(|node| uses_of_node(node, kind)).sum()
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

    /// Numbers each sequence item by the kind it *resolves* to.
    ///
    /// A getter reaches its child by casting and counting, so what decides an item's ordinal is
    /// the kind in the tree, not the name the grammar spells. An alias and the node it renames
    /// are one kind there — `('else' Expression 'when' Condition)` with `Condition = Expression`
    /// holds two `Expression`s — so they are numbered 0 and 1 and the two getters address
    /// different children.
    ///
    /// This cannot happen while mapping the grammar: resolving an alias needs the whole model,
    /// and the alias may not have been mapped yet.
    pub fn fixup_nth_by_resolved_kind(&mut self) {
        // Resolution borrows the model, so number every item first and write back afterwards.
        let ordinals: HashMap<NodeKind, Vec<usize>> = self
            .all_nodes()
            .filter_map(|node| {
                let Node::Items(seq) = node else {
                    return None;
                };
                let mut seen: Vec<NodeOrTokenKind> = Vec::with_capacity(seq.items.len());
                let nths = seq
                    .items
                    .iter()
                    .map(|item| {
                        let kind = self.resolved_kind(item);
                        let nth = seen.iter().filter(|earlier| **earlier == kind).count();
                        seen.push(kind);
                        nth
                    })
                    .collect();
                Some((seq.name.clone(), nths))
            })
            .collect();

        for (name, nths) in ordinals {
            let Some(Node::Items(seq)) = self.nodes.get_mut(&name) else {
                unreachable!("{name} was a sequence node a moment ago");
            };
            for (item, nth) in seq.items.iter_mut().zip(nths) {
                item.set_nth(nth);
            }
        }
    }

    fn collect_referenced_nodes(&self) -> HashSet<NodeKind> {
        let mut referenced = HashSet::new();
        for node in self.all_nodes() {
            collect_referenced_nodes_of(node, &mut referenced);
        }
        referenced
    }

    pub fn collect_all_node_kinds(&self) -> HashSet<NodeKind> {
        self.all_nodes().map(|node| node.name().clone()).collect()
    }

    /// The kinds that the parser actually materializes as green nodes: sequences and lists.
    /// A choice is abstract — the parser emits one of its options, never the choice itself —
    /// so choice kinds are deliberately absent from the generated `NodeKind` enum. So is an
    /// alias: the tree holds a node of the aliased kind, the alias only renames the accessor
    /// that reaches it.
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

// MARK: Single-node traversals
//
// One `Node` at a time, so that the alias case is spelled out beside the shapes that do carry
// fields of their own.

/// Whether `node` can produce a completely empty green node, given the kinds already known to be
/// empty-capable. See [`Model::compute_empty_capable_nodes`].
fn is_empty_capable(node: &Node, empty_capable: &HashSet<NodeKind>) -> bool {
    match node {
        Node::Items(seq) => seq.items.iter().all(|item| match &item.kind {
            NodeOrTokenKind::Token(_) => item.may_be_absent(),
            NodeOrTokenKind::Node(kind) => item.may_be_absent() || empty_capable.contains(kind),
        }),
        // A choice is empty-capable when any alternative is: the parser can select that
        // alternative, emit nothing, and the empty node is dropped — leaving the choice
        // reference absent in the parent.
        Node::Choices(choice) => match &choice.items {
            NodesOrTokens::Nodes(options) => {
                options.iter().any(|option| empty_capable.contains(option))
            }
            NodesOrTokens::Tokens(_) => false,
        },
        // Lists are never empty-capable
        Node::List(_) => false,
        // An alias is empty-capable exactly when what it renames is. A token never is.
        Node::Alias(alias) => alias
            .aliased
            .as_node_kind()
            .is_some_and(|kind| empty_capable.contains(kind)),
    }
}

/// Checks that no two fields of `node` generate the same accessor name.
fn check_no_duplicate_accessors(node: &Node) {
    let name = node.name();
    let mut seen = HashSet::new();
    let mut check_accessor = |accessor: String| {
        if seen.contains(&accessor) {
            panic!("Duplicate node {accessor} in node {name}")
        }
        seen.insert(accessor);
    };

    match node {
        Node::Items(seq_node) => {
            for item in &seq_node.items {
                check_accessor(item.getter_name());
            }
        }
        Node::Choices(choices_node) => match &choices_node.items {
            NodesOrTokens::Nodes(nodes) => {
                for item in nodes {
                    check_accessor(item.as_str().to_case(Case::Snake));
                }
            }
            NodesOrTokens::Tokens(tokens) => {
                for item in tokens {
                    check_accessor(item.getter_name());
                }
            }
        },
        Node::List(list) => {
            check_accessor(list.element.getter_name());
            check_accessor(list.separator.getter_name());
        }
        // An alias has no fields of its own; the accessors belong to what it renames.
        Node::Alias(_) => {}
    }
}

/// The number of places within `node` — sequence items, choice alternatives, a list's element and
/// separator — that reference `kind`.
fn uses_of_node(node: &Node, kind: &NodeKind) -> usize {
    match node {
        Node::Items(items) => items
            .items
            .iter()
            .filter(|item| item.as_node_kind() == Some(kind))
            .count(),
        Node::Choices(choices) => match &choices.items {
            NodesOrTokens::Nodes(nodes) => nodes.iter().filter(|node| *node == kind).count(),
            // An alternative of a token choice may still be a reference: one that renames a token.
            NodesOrTokens::Tokens(alternatives) => alternatives
                .iter()
                .filter(|alternative| alternative.as_node_kind() == Some(kind))
                .count(),
        },
        Node::List(list) => {
            usize::from(list.element.as_node_kind() == Some(kind))
                + usize::from(list.separator.as_node_kind() == Some(kind))
        }
        // An alias is one use site of the node it renames.
        Node::Alias(alias) => usize::from(alias.aliased.as_node_kind() == Some(kind)),
    }
}

/// Adds every node kind that `node` references to `referenced`.
fn collect_referenced_nodes_of(node: &Node, referenced: &mut HashSet<NodeKind>) {
    match node {
        Node::Items(seq_node) => {
            for item in &seq_node.items {
                if let Some(kind) = item.as_node_kind() {
                    referenced.insert(kind.clone());
                }
            }
        }
        Node::Choices(choices_node) => match &choices_node.items {
            NodesOrTokens::Nodes(nodes) => referenced.extend(nodes.iter().cloned()),
            // An alternative that renames a token references that renaming production.
            NodesOrTokens::Tokens(alternatives) => referenced.extend(
                alternatives
                    .iter()
                    .filter_map(|alternative| alternative.as_node_kind())
                    .cloned(),
            ),
        },
        Node::List(list) => {
            if let Some(kind) = list.element.as_node_kind() {
                referenced.insert(kind.clone());
            }
            if let Some(kind) = list.separator.as_node_kind() {
                referenced.insert(kind.clone());
            }
        }
        // An alias references what it renames, which is what keeps a production that is only
        // ever reached through an alias from looking unreferenced.
        Node::Alias(alias) => {
            if let Some(kind) = alias.aliased.as_node_kind() {
                referenced.insert(kind.clone());
            }
        }
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
            items: NodesOrTokens::Tokens(vec![
                Field::token(TokenKind::EQ),
                Field::token(TokenKind::NE),
            ]),
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

    /// `Expression` plus `Condition`, an alias for it, referenced from `DesignFile`.
    fn model_with_alias() -> Model {
        let mut model = Model::default();
        model.push_node(SequenceNode::new(
            "Expression",
            vec![Field::token(TokenKind::Identifier)],
        ));
        model.push_node(AliasNode::node("Condition", "Expression"));
        model.push_node(SequenceNode::new(
            "DesignFile",
            vec![Field::node("Condition")],
        ));
        model.do_postprocessing();
        model
    }

    #[test]
    fn resolve_alias_yields_the_aliased_kind() {
        let model = model_with_alias();
        assert_eq!(
            model.resolve_alias(&NodeKind::from("Condition")),
            NodeOrTokenKind::Node("Expression".into())
        );
    }

    /// A kind that is not an alias — and one the model has never heard of — is its own answer.
    #[test]
    fn resolve_alias_leaves_other_kinds_alone() {
        let model = model_with_alias();
        for kind in ["Expression", "DesignFile", "NotInTheModel"] {
            assert_eq!(
                model.resolve_alias(&NodeKind::from(kind)),
                NodeOrTokenKind::Node(kind.into())
            );
        }
    }

    #[test]
    fn resolve_alias_peels_nested_aliases() {
        let mut model = model_with_alias();
        model.push_node(AliasNode::node("Guard", "Condition"));
        assert_eq!(
            model.resolve_alias(&NodeKind::from("Guard")),
            NodeOrTokenKind::Node("Expression".into())
        );
    }

    /// An alias may rename a token, which is what lets a keyword stand as a choice alternative.
    #[test]
    fn resolve_alias_ends_at_a_token() {
        let mut model = Model::default();
        model.push_node(AliasNode::token("OthersChoice", TokenKind::Identifier));
        model.push_node(AliasNode::node("Renamed", "OthersChoice"));
        assert_eq!(
            model.resolve_alias(&NodeKind::from("Renamed")),
            NodeOrTokenKind::Token(TokenKind::Identifier)
        );
    }

    #[test]
    #[should_panic(expected = "part of a cycle")]
    fn resolve_alias_rejects_a_cycle() {
        let mut model = Model::default();
        model.push_node(AliasNode::node("A", "B"));
        model.push_node(AliasNode::node("B", "A"));
        model.resolve_alias(&NodeKind::from("A"));
    }

    /// The tree holds a node of the aliased kind, so the alias itself is not a `NodeKind`.
    #[test]
    fn an_alias_is_not_a_materialized_node_kind() {
        let kinds = model_with_alias().collect_all_materialized_node_kinds();
        assert!(kinds.contains("Expression"));
        assert!(!kinds.contains("Condition"));
    }

    #[test]
    fn an_alias_is_empty_capable_with_its_target() {
        let mut model = Model::default();
        // All-repeated → empty-capable.
        let leaf = SequenceNode::new(
            "Leaf",
            vec![Field::token(TokenKind::SemiColon).make_repeated()],
        );
        model.push_node(leaf);
        model.push_node(AliasNode::node("LeafAlias", "Leaf"));
        model.push_node(SequenceNode::new(
            "DesignFile",
            vec![Field::token(TokenKind::SemiColon)],
        ));

        let empty_capable = model.compute_empty_capable_nodes();
        assert!(empty_capable.contains("LeafAlias"));
        assert!(!empty_capable.contains("DesignFile"));
    }

    /// A reference to an alias is a reference, so the alias is not "defined but never used" —
    /// and neither is the node it renames.
    #[test]
    fn check_all_nodes_exist_accepts_an_alias() {
        model_with_alias().check_all_nodes_exist();
    }

    /// An alias and the node it renames are one kind in the tree, so they are numbered as one
    /// run: `expression()` reaches the first child, `condition()` the second.
    #[test]
    fn fixup_nth_numbers_an_alias_and_its_target_as_one_kind() {
        let mut model = model_with_alias();
        model.push_node(SequenceNode::new(
            "ElseWhenExpression",
            vec![
                Field::token(TokenKind::Comma),
                Field::node("Expression"),
                Field::token(TokenKind::SemiColon),
                Field::node("Condition"),
            ],
        ));
        model.fixup_nth_by_resolved_kind();
        model.check_nth_accessors_are_unambiguous(); // must not panic

        let Some(Node::Items(seq)) = model.node(&"ElseWhenExpression".into()) else {
            panic!("ElseWhenExpression should be a sequence node")
        };
        assert_eq!(
            seq.items[1].cardinality,
            Cardinality::Required { nth: 0 },
            "the bare Expression is the first of its kind"
        );
        assert_eq!(
            seq.items[3].cardinality,
            Cardinality::Required { nth: 1 },
            "Condition renames Expression, so it is the second of that kind"
        );
    }

    /// The two keywords are distinct kinds, so each stays at ordinal 0.
    #[test]
    fn fixup_nth_numbers_each_kind_separately() {
        let mut model = Model::default();
        model.push_node(SequenceNode::new(
            "DesignFile",
            vec![
                Field::token(TokenKind::SemiColon),
                Field::token(TokenKind::Comma),
                Field::token(TokenKind::SemiColon),
            ],
        ));
        model.fixup_nth_by_resolved_kind();

        let Some(Node::Items(seq)) = model.node(&"DesignFile".into()) else {
            panic!("DesignFile should be a sequence node")
        };
        let nths: Vec<Cardinality> = seq.items.iter().map(|item| item.cardinality).collect();
        assert_eq!(
            nths,
            [
                Cardinality::Required { nth: 0 },
                Cardinality::Required { nth: 0 },
                Cardinality::Required { nth: 1 },
            ]
        );
    }

    #[test]
    #[should_panic(expected = "which is not defined anywhere")]
    fn check_aliased_nodes_are_defined_panics_on_a_dangling_alias() {
        let mut model = Model::default();
        model.push_node(AliasNode::node("Condition", "Expression"));
        model.check_aliased_nodes_are_defined();
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
