//! Meta API that provides static information about the layout of a SyntaxNode at runtime.

use crate::{syntax::NodeKind, tokens::TokenKind};

#[derive(Debug, Copy, Clone)]
pub enum Layout {
    /// A fixed sequence of child items (also used for raw-token nodes, which have empty items).
    Sequence(Sequence),
    /// A choice between several node kinds.
    Choice(Choice),
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

/// A Layout Item
#[derive(Debug, Copy, Clone)]
pub struct LayoutItem {
    /// Whether this item can be missing in correctly formed VHDL
    pub optional: bool,
    /// Whether this item can appear multiple times (a `repeated` item is always `optionsl`, i.e., zero or more)
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
