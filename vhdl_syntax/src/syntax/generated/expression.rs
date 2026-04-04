// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2026, Lukas Scheller lukasscheller@icloud.com
use super::*;
use crate::syntax::meta::{Choice, Layout, LayoutItem, LayoutItemKind, Sequence};
use crate::syntax::node::{SyntaxNode, SyntaxToken};
use crate::syntax::node_kind::NodeKind;
use crate::syntax::AstNode;
use crate::tokens::Keyword as Kw;
use crate::tokens::TokenKind;
#[derive(Debug, Clone)]
pub enum LiteralSyntax {
    BitStringLiteral(SyntaxToken),
    CharacterLiteral(SyntaxToken),
    StringLiteral(SyntaxToken),
    Null(SyntaxToken),
}
impl LiteralSyntax {
    pub fn cast(token: SyntaxToken) -> Option<Self> {
        match token.kind() {
            TokenKind::BitStringLiteral => Some(LiteralSyntax::BitStringLiteral(token)),
            TokenKind::CharacterLiteral => Some(LiteralSyntax::CharacterLiteral(token)),
            TokenKind::StringLiteral => Some(LiteralSyntax::StringLiteral(token)),
            TokenKind::Keyword(Kw::Null) => Some(LiteralSyntax::Null(token)),
            _ => None,
        }
    }
    pub fn raw(&self) -> SyntaxToken {
        match self {
            LiteralSyntax::BitStringLiteral(token) => token.clone(),
            LiteralSyntax::CharacterLiteral(token) => token.clone(),
            LiteralSyntax::StringLiteral(token) => token.clone(),
            LiteralSyntax::Null(token) => token.clone(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct ParenthesizedExpressionOrAggregateSyntax(pub(crate) SyntaxNode);
impl AstNode for ParenthesizedExpressionOrAggregateSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ParenthesizedExpressionOrAggregate,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::LeftPar),
            },
            LayoutItem {
                optional: false,
                repeated: true,
                name: "element_associations",
                kind: LayoutItemKind::Node(NodeKind::ElementAssociation),
            },
            LayoutItem {
                optional: false,
                repeated: true,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Comma),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::RightPar),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ParenthesizedExpressionOrAggregateSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ParenthesizedExpressionOrAggregateSyntax {
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::LeftPar)
            .nth(0)
    }
    pub fn element_associations(&self) -> impl Iterator<Item = ElementAssociationSyntax> + use<'_> {
        self.0.children().filter_map(ElementAssociationSyntax::cast)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Comma)
    }
    pub fn right_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::RightPar)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum UnaryOperatorSyntax {
    QueQue(SyntaxToken),
    Plus(SyntaxToken),
    Minus(SyntaxToken),
    Abs(SyntaxToken),
    Not(SyntaxToken),
    And(SyntaxToken),
    Or(SyntaxToken),
    Nand(SyntaxToken),
    Nor(SyntaxToken),
    Xor(SyntaxToken),
    Xnor(SyntaxToken),
}
impl UnaryOperatorSyntax {
    pub fn cast(token: SyntaxToken) -> Option<Self> {
        match token.kind() {
            TokenKind::QueQue => Some(UnaryOperatorSyntax::QueQue(token)),
            TokenKind::Plus => Some(UnaryOperatorSyntax::Plus(token)),
            TokenKind::Minus => Some(UnaryOperatorSyntax::Minus(token)),
            TokenKind::Keyword(Kw::Abs) => Some(UnaryOperatorSyntax::Abs(token)),
            TokenKind::Keyword(Kw::Not) => Some(UnaryOperatorSyntax::Not(token)),
            TokenKind::Keyword(Kw::And) => Some(UnaryOperatorSyntax::And(token)),
            TokenKind::Keyword(Kw::Or) => Some(UnaryOperatorSyntax::Or(token)),
            TokenKind::Keyword(Kw::Nand) => Some(UnaryOperatorSyntax::Nand(token)),
            TokenKind::Keyword(Kw::Nor) => Some(UnaryOperatorSyntax::Nor(token)),
            TokenKind::Keyword(Kw::Xor) => Some(UnaryOperatorSyntax::Xor(token)),
            TokenKind::Keyword(Kw::Xnor) => Some(UnaryOperatorSyntax::Xnor(token)),
            _ => None,
        }
    }
    pub fn raw(&self) -> SyntaxToken {
        match self {
            UnaryOperatorSyntax::QueQue(token) => token.clone(),
            UnaryOperatorSyntax::Plus(token) => token.clone(),
            UnaryOperatorSyntax::Minus(token) => token.clone(),
            UnaryOperatorSyntax::Abs(token) => token.clone(),
            UnaryOperatorSyntax::Not(token) => token.clone(),
            UnaryOperatorSyntax::And(token) => token.clone(),
            UnaryOperatorSyntax::Or(token) => token.clone(),
            UnaryOperatorSyntax::Nand(token) => token.clone(),
            UnaryOperatorSyntax::Nor(token) => token.clone(),
            UnaryOperatorSyntax::Xor(token) => token.clone(),
            UnaryOperatorSyntax::Xnor(token) => token.clone(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct UnaryExpressionSyntax(pub(crate) SyntaxNode);
impl AstNode for UnaryExpressionSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::UnaryExpression,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "op",
                kind: LayoutItemKind::TokenGroup(&[
                    TokenKind::QueQue,
                    TokenKind::Plus,
                    TokenKind::Minus,
                    TokenKind::Keyword(Kw::Abs),
                    TokenKind::Keyword(Kw::Not),
                    TokenKind::Keyword(Kw::And),
                    TokenKind::Keyword(Kw::Or),
                    TokenKind::Keyword(Kw::Nand),
                    TokenKind::Keyword(Kw::Nor),
                    TokenKind::Keyword(Kw::Xor),
                    TokenKind::Keyword(Kw::Xnor),
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "expression",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::LiteralExpression,
                    NodeKind::PhysicalLiteralExpression,
                    NodeKind::UnaryExpression,
                    NodeKind::BinaryExpression,
                    NodeKind::ParenthesizedExpressionOrAggregate,
                    NodeKind::SubtypeIndicationAllocator,
                    NodeKind::ExpressionAllocator,
                    NodeKind::QualifiedExpression,
                    NodeKind::TypeConversion,
                    NodeKind::NameExpression,
                ]),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        UnaryExpressionSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl UnaryExpressionSyntax {
    pub fn op(&self) -> Option<UnaryOperatorSyntax> {
        self.0.tokens().filter_map(UnaryOperatorSyntax::cast).nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum BinaryOperatorSyntax {
    And(SyntaxToken),
    Or(SyntaxToken),
    Nand(SyntaxToken),
    Nor(SyntaxToken),
    Xor(SyntaxToken),
    Xnor(SyntaxToken),
    Eq(SyntaxToken),
    Ne(SyntaxToken),
    Lt(SyntaxToken),
    Lte(SyntaxToken),
    Gt(SyntaxToken),
    Gte(SyntaxToken),
    QueEq(SyntaxToken),
    QueNe(SyntaxToken),
    QueLt(SyntaxToken),
    QueGt(SyntaxToken),
    QueGte(SyntaxToken),
    Sll(SyntaxToken),
    Srl(SyntaxToken),
    Sla(SyntaxToken),
    Sra(SyntaxToken),
    Rol(SyntaxToken),
    Ror(SyntaxToken),
    Plus(SyntaxToken),
    Minus(SyntaxToken),
    Concat(SyntaxToken),
    Times(SyntaxToken),
    Div(SyntaxToken),
    Mod(SyntaxToken),
    Rem(SyntaxToken),
    Pow(SyntaxToken),
}
impl BinaryOperatorSyntax {
    pub fn cast(token: SyntaxToken) -> Option<Self> {
        match token.kind() {
            TokenKind::Keyword(Kw::And) => Some(BinaryOperatorSyntax::And(token)),
            TokenKind::Keyword(Kw::Or) => Some(BinaryOperatorSyntax::Or(token)),
            TokenKind::Keyword(Kw::Nand) => Some(BinaryOperatorSyntax::Nand(token)),
            TokenKind::Keyword(Kw::Nor) => Some(BinaryOperatorSyntax::Nor(token)),
            TokenKind::Keyword(Kw::Xor) => Some(BinaryOperatorSyntax::Xor(token)),
            TokenKind::Keyword(Kw::Xnor) => Some(BinaryOperatorSyntax::Xnor(token)),
            TokenKind::EQ => Some(BinaryOperatorSyntax::Eq(token)),
            TokenKind::NE => Some(BinaryOperatorSyntax::Ne(token)),
            TokenKind::LT => Some(BinaryOperatorSyntax::Lt(token)),
            TokenKind::LTE => Some(BinaryOperatorSyntax::Lte(token)),
            TokenKind::GT => Some(BinaryOperatorSyntax::Gt(token)),
            TokenKind::GTE => Some(BinaryOperatorSyntax::Gte(token)),
            TokenKind::QueEQ => Some(BinaryOperatorSyntax::QueEq(token)),
            TokenKind::QueNE => Some(BinaryOperatorSyntax::QueNe(token)),
            TokenKind::QueLT => Some(BinaryOperatorSyntax::QueLt(token)),
            TokenKind::QueGT => Some(BinaryOperatorSyntax::QueGt(token)),
            TokenKind::QueGTE => Some(BinaryOperatorSyntax::QueGte(token)),
            TokenKind::Keyword(Kw::Sll) => Some(BinaryOperatorSyntax::Sll(token)),
            TokenKind::Keyword(Kw::Srl) => Some(BinaryOperatorSyntax::Srl(token)),
            TokenKind::Keyword(Kw::Sla) => Some(BinaryOperatorSyntax::Sla(token)),
            TokenKind::Keyword(Kw::Sra) => Some(BinaryOperatorSyntax::Sra(token)),
            TokenKind::Keyword(Kw::Rol) => Some(BinaryOperatorSyntax::Rol(token)),
            TokenKind::Keyword(Kw::Ror) => Some(BinaryOperatorSyntax::Ror(token)),
            TokenKind::Plus => Some(BinaryOperatorSyntax::Plus(token)),
            TokenKind::Minus => Some(BinaryOperatorSyntax::Minus(token)),
            TokenKind::Concat => Some(BinaryOperatorSyntax::Concat(token)),
            TokenKind::Times => Some(BinaryOperatorSyntax::Times(token)),
            TokenKind::Div => Some(BinaryOperatorSyntax::Div(token)),
            TokenKind::Keyword(Kw::Mod) => Some(BinaryOperatorSyntax::Mod(token)),
            TokenKind::Keyword(Kw::Rem) => Some(BinaryOperatorSyntax::Rem(token)),
            TokenKind::Pow => Some(BinaryOperatorSyntax::Pow(token)),
            _ => None,
        }
    }
    pub fn raw(&self) -> SyntaxToken {
        match self {
            BinaryOperatorSyntax::And(token) => token.clone(),
            BinaryOperatorSyntax::Or(token) => token.clone(),
            BinaryOperatorSyntax::Nand(token) => token.clone(),
            BinaryOperatorSyntax::Nor(token) => token.clone(),
            BinaryOperatorSyntax::Xor(token) => token.clone(),
            BinaryOperatorSyntax::Xnor(token) => token.clone(),
            BinaryOperatorSyntax::Eq(token) => token.clone(),
            BinaryOperatorSyntax::Ne(token) => token.clone(),
            BinaryOperatorSyntax::Lt(token) => token.clone(),
            BinaryOperatorSyntax::Lte(token) => token.clone(),
            BinaryOperatorSyntax::Gt(token) => token.clone(),
            BinaryOperatorSyntax::Gte(token) => token.clone(),
            BinaryOperatorSyntax::QueEq(token) => token.clone(),
            BinaryOperatorSyntax::QueNe(token) => token.clone(),
            BinaryOperatorSyntax::QueLt(token) => token.clone(),
            BinaryOperatorSyntax::QueGt(token) => token.clone(),
            BinaryOperatorSyntax::QueGte(token) => token.clone(),
            BinaryOperatorSyntax::Sll(token) => token.clone(),
            BinaryOperatorSyntax::Srl(token) => token.clone(),
            BinaryOperatorSyntax::Sla(token) => token.clone(),
            BinaryOperatorSyntax::Sra(token) => token.clone(),
            BinaryOperatorSyntax::Rol(token) => token.clone(),
            BinaryOperatorSyntax::Ror(token) => token.clone(),
            BinaryOperatorSyntax::Plus(token) => token.clone(),
            BinaryOperatorSyntax::Minus(token) => token.clone(),
            BinaryOperatorSyntax::Concat(token) => token.clone(),
            BinaryOperatorSyntax::Times(token) => token.clone(),
            BinaryOperatorSyntax::Div(token) => token.clone(),
            BinaryOperatorSyntax::Mod(token) => token.clone(),
            BinaryOperatorSyntax::Rem(token) => token.clone(),
            BinaryOperatorSyntax::Pow(token) => token.clone(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct BinaryExpressionSyntax(pub(crate) SyntaxNode);
impl AstNode for BinaryExpressionSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::BinaryExpression,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "lhs",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::LiteralExpression,
                    NodeKind::PhysicalLiteralExpression,
                    NodeKind::UnaryExpression,
                    NodeKind::BinaryExpression,
                    NodeKind::ParenthesizedExpressionOrAggregate,
                    NodeKind::SubtypeIndicationAllocator,
                    NodeKind::ExpressionAllocator,
                    NodeKind::QualifiedExpression,
                    NodeKind::TypeConversion,
                    NodeKind::NameExpression,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "op",
                kind: LayoutItemKind::TokenGroup(&[
                    TokenKind::Keyword(Kw::And),
                    TokenKind::Keyword(Kw::Or),
                    TokenKind::Keyword(Kw::Nand),
                    TokenKind::Keyword(Kw::Nor),
                    TokenKind::Keyword(Kw::Xor),
                    TokenKind::Keyword(Kw::Xnor),
                    TokenKind::EQ,
                    TokenKind::NE,
                    TokenKind::LT,
                    TokenKind::LTE,
                    TokenKind::GT,
                    TokenKind::GTE,
                    TokenKind::QueEQ,
                    TokenKind::QueNE,
                    TokenKind::QueLT,
                    TokenKind::QueGT,
                    TokenKind::QueGTE,
                    TokenKind::Keyword(Kw::Sll),
                    TokenKind::Keyword(Kw::Srl),
                    TokenKind::Keyword(Kw::Sla),
                    TokenKind::Keyword(Kw::Sra),
                    TokenKind::Keyword(Kw::Rol),
                    TokenKind::Keyword(Kw::Ror),
                    TokenKind::Plus,
                    TokenKind::Minus,
                    TokenKind::Concat,
                    TokenKind::Times,
                    TokenKind::Div,
                    TokenKind::Keyword(Kw::Mod),
                    TokenKind::Keyword(Kw::Rem),
                    TokenKind::Pow,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "rhs",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::LiteralExpression,
                    NodeKind::PhysicalLiteralExpression,
                    NodeKind::UnaryExpression,
                    NodeKind::BinaryExpression,
                    NodeKind::ParenthesizedExpressionOrAggregate,
                    NodeKind::SubtypeIndicationAllocator,
                    NodeKind::ExpressionAllocator,
                    NodeKind::QualifiedExpression,
                    NodeKind::TypeConversion,
                    NodeKind::NameExpression,
                ]),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        BinaryExpressionSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl BinaryExpressionSyntax {
    pub fn lhs(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn op(&self) -> Option<BinaryOperatorSyntax> {
        self.0
            .tokens()
            .filter_map(BinaryOperatorSyntax::cast)
            .nth(0)
    }
    pub fn rhs(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(1)
    }
}
#[derive(Debug, Clone)]
pub struct LiteralExpressionSyntax(pub(crate) SyntaxNode);
impl AstNode for LiteralExpressionSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::LiteralExpression,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "literal",
            kind: LayoutItemKind::TokenGroup(&[
                TokenKind::BitStringLiteral,
                TokenKind::CharacterLiteral,
                TokenKind::StringLiteral,
                TokenKind::Keyword(Kw::Null),
            ]),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        LiteralExpressionSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl LiteralExpressionSyntax {
    pub fn literal(&self) -> Option<LiteralSyntax> {
        self.0.tokens().filter_map(LiteralSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct PhysicalLiteralExpressionSyntax(pub(crate) SyntaxNode);
impl AstNode for PhysicalLiteralExpressionSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PhysicalLiteralExpression,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "physical_literal",
            kind: LayoutItemKind::Node(NodeKind::PhysicalLiteral),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        PhysicalLiteralExpressionSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PhysicalLiteralExpressionSyntax {
    pub fn physical_literal(&self) -> Option<PhysicalLiteralSyntax> {
        self.0
            .children()
            .filter_map(PhysicalLiteralSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct NameExpressionSyntax(pub(crate) SyntaxNode);
impl AstNode for NameExpressionSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::NameExpression,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "name",
            kind: LayoutItemKind::Node(NodeKind::Name),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        NameExpressionSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl NameExpressionSyntax {
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum ExpressionSyntax {
    LiteralExpression(LiteralExpressionSyntax),
    PhysicalLiteralExpression(PhysicalLiteralExpressionSyntax),
    UnaryExpression(UnaryExpressionSyntax),
    BinaryExpression(BinaryExpressionSyntax),
    ParenthesizedExpressionOrAggregate(ParenthesizedExpressionOrAggregateSyntax),
    Allocator(AllocatorSyntax),
    QualifiedExpression(QualifiedExpressionSyntax),
    TypeConversion(TypeConversionSyntax),
    NameExpression(NameExpressionSyntax),
}
impl AstNode for ExpressionSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::LiteralExpression,
            NodeKind::PhysicalLiteralExpression,
            NodeKind::UnaryExpression,
            NodeKind::BinaryExpression,
            NodeKind::ParenthesizedExpressionOrAggregate,
            NodeKind::SubtypeIndicationAllocator,
            NodeKind::ExpressionAllocator,
            NodeKind::QualifiedExpression,
            NodeKind::TypeConversion,
            NodeKind::NameExpression,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if LiteralExpressionSyntax::can_cast(&node) {
            return ExpressionSyntax::LiteralExpression(LiteralExpressionSyntax::cast_unchecked(
                node,
            ));
        }
        if PhysicalLiteralExpressionSyntax::can_cast(&node) {
            return ExpressionSyntax::PhysicalLiteralExpression(
                PhysicalLiteralExpressionSyntax::cast_unchecked(node),
            );
        }
        if UnaryExpressionSyntax::can_cast(&node) {
            return ExpressionSyntax::UnaryExpression(UnaryExpressionSyntax::cast_unchecked(node));
        }
        if BinaryExpressionSyntax::can_cast(&node) {
            return ExpressionSyntax::BinaryExpression(BinaryExpressionSyntax::cast_unchecked(
                node,
            ));
        }
        if ParenthesizedExpressionOrAggregateSyntax::can_cast(&node) {
            return ExpressionSyntax::ParenthesizedExpressionOrAggregate(
                ParenthesizedExpressionOrAggregateSyntax::cast_unchecked(node),
            );
        }
        if AllocatorSyntax::can_cast(&node) {
            return ExpressionSyntax::Allocator(AllocatorSyntax::cast_unchecked(node));
        }
        if QualifiedExpressionSyntax::can_cast(&node) {
            return ExpressionSyntax::QualifiedExpression(
                QualifiedExpressionSyntax::cast_unchecked(node),
            );
        }
        if TypeConversionSyntax::can_cast(&node) {
            return ExpressionSyntax::TypeConversion(TypeConversionSyntax::cast_unchecked(node));
        }
        if NameExpressionSyntax::can_cast(&node) {
            return ExpressionSyntax::NameExpression(NameExpressionSyntax::cast_unchecked(node));
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            ExpressionSyntax::LiteralExpression(inner) => inner.raw(),
            ExpressionSyntax::PhysicalLiteralExpression(inner) => inner.raw(),
            ExpressionSyntax::UnaryExpression(inner) => inner.raw(),
            ExpressionSyntax::BinaryExpression(inner) => inner.raw(),
            ExpressionSyntax::ParenthesizedExpressionOrAggregate(inner) => inner.raw(),
            ExpressionSyntax::Allocator(inner) => inner.raw(),
            ExpressionSyntax::QualifiedExpression(inner) => inner.raw(),
            ExpressionSyntax::TypeConversion(inner) => inner.raw(),
            ExpressionSyntax::NameExpression(inner) => inner.raw(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct AggregateSyntax(pub(crate) SyntaxNode);
impl AstNode for AggregateSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::Aggregate,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::LeftPar),
            },
            LayoutItem {
                optional: false,
                repeated: true,
                name: "element_associations",
                kind: LayoutItemKind::Node(NodeKind::ElementAssociation),
            },
            LayoutItem {
                optional: false,
                repeated: true,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Comma),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::RightPar),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        AggregateSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl AggregateSyntax {
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::LeftPar)
            .nth(0)
    }
    pub fn element_associations(&self) -> impl Iterator<Item = ElementAssociationSyntax> + use<'_> {
        self.0.children().filter_map(ElementAssociationSyntax::cast)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Comma)
    }
    pub fn right_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::RightPar)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct SubtypeIndicationAllocatorSyntax(pub(crate) SyntaxNode);
impl AstNode for SubtypeIndicationAllocatorSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SubtypeIndicationAllocator,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::New)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "subtype_indication",
                kind: LayoutItemKind::Node(NodeKind::SubtypeIndication),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        SubtypeIndicationAllocatorSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SubtypeIndicationAllocatorSyntax {
    pub fn new_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::New))
            .nth(0)
    }
    pub fn subtype_indication(&self) -> Option<SubtypeIndicationSyntax> {
        self.0
            .children()
            .filter_map(SubtypeIndicationSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ExpressionAllocatorSyntax(pub(crate) SyntaxNode);
impl AstNode for ExpressionAllocatorSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ExpressionAllocator,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::New)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "expression",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::LiteralExpression,
                    NodeKind::PhysicalLiteralExpression,
                    NodeKind::UnaryExpression,
                    NodeKind::BinaryExpression,
                    NodeKind::ParenthesizedExpressionOrAggregate,
                    NodeKind::SubtypeIndicationAllocator,
                    NodeKind::ExpressionAllocator,
                    NodeKind::QualifiedExpression,
                    NodeKind::TypeConversion,
                    NodeKind::NameExpression,
                ]),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ExpressionAllocatorSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ExpressionAllocatorSyntax {
    pub fn new_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::New))
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum AllocatorSyntax {
    SubtypeIndicationAllocator(SubtypeIndicationAllocatorSyntax),
    ExpressionAllocator(ExpressionAllocatorSyntax),
}
impl AstNode for AllocatorSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::SubtypeIndicationAllocator,
            NodeKind::ExpressionAllocator,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if SubtypeIndicationAllocatorSyntax::can_cast(&node) {
            return AllocatorSyntax::SubtypeIndicationAllocator(
                SubtypeIndicationAllocatorSyntax::cast_unchecked(node),
            );
        }
        if ExpressionAllocatorSyntax::can_cast(&node) {
            return AllocatorSyntax::ExpressionAllocator(
                ExpressionAllocatorSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            AllocatorSyntax::SubtypeIndicationAllocator(inner) => inner.raw(),
            AllocatorSyntax::ExpressionAllocator(inner) => inner.raw(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct OthersChoiceSyntax(pub(crate) SyntaxNode);
impl AstNode for OthersChoiceSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::OthersChoice,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "",
            kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Others)),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        OthersChoiceSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl OthersChoiceSyntax {
    pub fn others_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Others))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ExpressionChoiceSyntax(pub(crate) SyntaxNode);
impl AstNode for ExpressionChoiceSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ExpressionChoice,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "expression",
            kind: LayoutItemKind::NodeChoice(&[
                NodeKind::LiteralExpression,
                NodeKind::PhysicalLiteralExpression,
                NodeKind::UnaryExpression,
                NodeKind::BinaryExpression,
                NodeKind::ParenthesizedExpressionOrAggregate,
                NodeKind::SubtypeIndicationAllocator,
                NodeKind::ExpressionAllocator,
                NodeKind::QualifiedExpression,
                NodeKind::TypeConversion,
                NodeKind::NameExpression,
            ]),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ExpressionChoiceSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ExpressionChoiceSyntax {
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct DiscreteRangeChoiceSyntax(pub(crate) SyntaxNode);
impl AstNode for DiscreteRangeChoiceSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::DiscreteRangeChoice,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "discrete_range",
            kind: LayoutItemKind::NodeChoice(&[
                NodeKind::SubtypeIndicationDiscreteDiscreteRange,
                NodeKind::SubtypeIndicationDiscreteRange,
                NodeKind::OpenDiscreteRange,
            ]),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        DiscreteRangeChoiceSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl DiscreteRangeChoiceSyntax {
    pub fn discrete_range(&self) -> Option<DiscreteRangeSyntax> {
        self.0
            .children()
            .filter_map(DiscreteRangeSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum ChoiceSyntax {
    ExpressionChoice(ExpressionChoiceSyntax),
    OthersChoice(OthersChoiceSyntax),
    DiscreteRangeChoice(DiscreteRangeChoiceSyntax),
}
impl AstNode for ChoiceSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::ExpressionChoice,
            NodeKind::OthersChoice,
            NodeKind::DiscreteRangeChoice,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if ExpressionChoiceSyntax::can_cast(&node) {
            return ChoiceSyntax::ExpressionChoice(ExpressionChoiceSyntax::cast_unchecked(node));
        }
        if OthersChoiceSyntax::can_cast(&node) {
            return ChoiceSyntax::OthersChoice(OthersChoiceSyntax::cast_unchecked(node));
        }
        if DiscreteRangeChoiceSyntax::can_cast(&node) {
            return ChoiceSyntax::DiscreteRangeChoice(DiscreteRangeChoiceSyntax::cast_unchecked(
                node,
            ));
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            ChoiceSyntax::ExpressionChoice(inner) => inner.raw(),
            ChoiceSyntax::OthersChoice(inner) => inner.raw(),
            ChoiceSyntax::DiscreteRangeChoice(inner) => inner.raw(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct ChoicesSyntax(pub(crate) SyntaxNode);
impl AstNode for ChoicesSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::Choices,
        items: &[
            LayoutItem {
                optional: false,
                repeated: true,
                name: "choices",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::ExpressionChoice,
                    NodeKind::OthersChoice,
                    NodeKind::DiscreteRangeChoice,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: true,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Bar),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ChoicesSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ChoicesSyntax {
    pub fn choices(&self) -> impl Iterator<Item = ChoiceSyntax> + use<'_> {
        self.0.children().filter_map(ChoiceSyntax::cast)
    }
    pub fn bar_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Bar)
    }
}
#[derive(Debug, Clone)]
pub struct ElementAssociationSyntax(pub(crate) SyntaxNode);
impl AstNode for ElementAssociationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ElementAssociation,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "choice",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::ExpressionChoice,
                    NodeKind::OthersChoice,
                    NodeKind::DiscreteRangeChoice,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::RightArrow),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "expression",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::LiteralExpression,
                    NodeKind::PhysicalLiteralExpression,
                    NodeKind::UnaryExpression,
                    NodeKind::BinaryExpression,
                    NodeKind::ParenthesizedExpressionOrAggregate,
                    NodeKind::SubtypeIndicationAllocator,
                    NodeKind::ExpressionAllocator,
                    NodeKind::QualifiedExpression,
                    NodeKind::TypeConversion,
                    NodeKind::NameExpression,
                ]),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ElementAssociationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ElementAssociationSyntax {
    pub fn choice(&self) -> Option<ChoiceSyntax> {
        self.0.children().filter_map(ChoiceSyntax::cast).nth(0)
    }
    pub fn right_arrow_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::RightArrow)
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct QualifiedExpressionSyntax(pub(crate) SyntaxNode);
impl AstNode for QualifiedExpressionSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::QualifiedExpression,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "name",
                kind: LayoutItemKind::Node(NodeKind::Name),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Tick),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "expression",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::LiteralExpression,
                    NodeKind::PhysicalLiteralExpression,
                    NodeKind::UnaryExpression,
                    NodeKind::BinaryExpression,
                    NodeKind::ParenthesizedExpressionOrAggregate,
                    NodeKind::SubtypeIndicationAllocator,
                    NodeKind::ExpressionAllocator,
                    NodeKind::QualifiedExpression,
                    NodeKind::TypeConversion,
                    NodeKind::NameExpression,
                ]),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        QualifiedExpressionSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl QualifiedExpressionSyntax {
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
    pub fn tick_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Tick)
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct TypeConversionSyntax(pub(crate) SyntaxNode);
impl AstNode for TypeConversionSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::TypeConversion,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "name",
                kind: LayoutItemKind::Node(NodeKind::Name),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::LeftPar),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "expression",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::LiteralExpression,
                    NodeKind::PhysicalLiteralExpression,
                    NodeKind::UnaryExpression,
                    NodeKind::BinaryExpression,
                    NodeKind::ParenthesizedExpressionOrAggregate,
                    NodeKind::SubtypeIndicationAllocator,
                    NodeKind::ExpressionAllocator,
                    NodeKind::QualifiedExpression,
                    NodeKind::TypeConversion,
                    NodeKind::NameExpression,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::RightPar),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        TypeConversionSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl TypeConversionSyntax {
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::LeftPar)
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn right_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::RightPar)
            .nth(0)
    }
}
