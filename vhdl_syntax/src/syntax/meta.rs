//! Meta API that provides static information about the layout of a SyntaxNode at runtime.

use crate::{syntax::NodeKind, tokens::TokenKind};

#[derive(Debug, Copy, Clone)]
pub enum Layout {
    /// A fixed sequence of child items (also used for raw-token nodes, which have empty items).
    Sequence(Sequence),
    /// A choice between several concrete sequence/raw-token node kinds.
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

#[derive(Debug, Copy, Clone)]
pub struct TokenChoice {
    pub options: &'static [TokenKind],
}

/// A Layout Item
#[derive(Debug, Copy, Clone)]
pub struct LayoutItem {
    pub optional: bool,
    pub repeated: bool,
    pub name: &'static str,
    pub kind: LayoutItemKind,
}

#[derive(Debug, Copy, Clone)]
pub enum LayoutItemKind {
    /// A direct token.
    Token(TokenKind),
    /// A direct reference to a concrete (sequence or raw-token) child node.
    Node(NodeKind),
    /// A reference to a node-choice child; lists every concrete alternative.
    NodeChoice(&'static [NodeKind]),
    /// A reference to a token-choice child; lists every token alternative.
    TokenGroup(&'static [TokenKind]),
}
