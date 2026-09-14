//! Meta API that provides static information about the layout of a SyntaxNode at runtime.

use crate::{
    syntax::{layout_of, NodeKind},
    tokens::TokenKind,
};

/// Whether a node of `kind` may legally hold no children at all
pub fn is_empty_capable(kind: NodeKind) -> bool {
    is_empty_capable_seen(kind, &mut Vec::new())
}

fn is_empty_capable_seen(kind: NodeKind, seen: &mut Vec<NodeKind>) -> bool {
    if seen.contains(&kind) {
        return false;
    }
    seen.push(kind);
    match layout_of(kind) {
        Layout::Sequence(sequence) => sequence.items.iter().all(|item| item.optional),
        Layout::List(_) => false,
        Layout::Choice(choice) => choice
            .options
            .iter()
            .any(|option| is_empty_capable_seen(*option, seen)),
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Layout {
    /// A fixed sequence of child items (also used for raw-token nodes, which have empty items).
    Sequence(Sequence),
    /// A choice between several node kinds.
    Choice(Choice),
    /// A list comprising several separated items.
    List(List),
}

impl Layout {
    /// Returns the concrete node kinds that a node with this layout can have.
    ///
    /// For a sequence or list, this is the node's own kind.
    /// For a choice, these are all kinds that the choice covers.
    ///
    /// # Example
    ///
    /// ```
    /// # use vhdl_syntax::syntax::{AstNode, NodeKind, TargetSyntax, EntityDeclarationSyntax};
    /// // A Target can be a name target (`foo <= ...`) or an aggregate target (`(foo, bar) <= ...`)
    /// assert_eq!(TargetSyntax::META.concrete_kinds(), &[NodeKind::NameTarget, NodeKind::AggregateTarget]);
    ///
    /// // An EntityDeclarationSyntax is always an EntityDeclaration
    /// assert_eq!(EntityDeclarationSyntax::META.concrete_kinds(), &[NodeKind::EntityDeclaration]);
    /// ```
    pub fn concrete_kinds(&self) -> &[NodeKind] {
        match self {
            Layout::Sequence(sequence) => std::slice::from_ref(&sequence.kind),
            Layout::Choice(choices) => choices.options,
            Layout::List(list) => std::slice::from_ref(&list.kind),
        }
    }
}

#[derive(Debug, Copy, Clone)]
/// The Layout of a Syntax Node
pub struct Sequence {
    pub kind: NodeKind,
    pub items: &'static [LayoutItem],
}

#[derive(Debug, Copy, Clone)]
pub struct Choice {
    pub options: &'static [NodeKind],
}

/// A separated list: `element (separator element)*`
#[derive(Debug, Copy, Clone)]
pub struct List {
    pub kind: NodeKind,
    pub element: &'static LayoutItem,
    pub separator: &'static LayoutItem,
}

/// A Layout Item
#[derive(Debug, Copy, Clone)]
pub struct LayoutItem {
    /// Whether this item can be missing in correctly formed VHDL
    pub optional: bool,
    /// Whether this item can appear multiple times.
    /// A zero-or-more item is encoded via `optional=true`
    pub repeated: bool,
    /// The name of this item. Mostly for informative purposes.
    /// Usually derived from the `kind`, but may be overwritten. For example, the left hand side of a `BinaryExpression`
    /// is an `Expression`, but called `lhs`
    pub name: &'static str,
    /// The kind of the item (a token, sub-node, ...)
    pub kind: LayoutItemKind,
}

#[derive(Debug, Copy, Clone)]
pub enum LayoutItemKind {
    /// A direct token.
    Token(TokenKind),
    /// A child node.
    Node(NodeKind),
    /// A node-choice child; lists every concrete alternative.
    NodeChoice(&'static [NodeKind]),
    /// A token-choice child; lists every token alternative.
    TokenChoice(&'static [TokenKind]),
}
