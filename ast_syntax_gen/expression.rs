// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2025, Lukas Scheller lukasscheller@icloud.com
use super::*;
use crate::syntax::node::{SyntaxNode, SyntaxToken};
use crate::syntax::node_kind::NodeKind;
use crate::syntax::AstNode;
use crate::tokens::Keyword as Kw;
use crate::tokens::TokenKind::*;

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
            BitStringLiteral => Some(LiteralSyntax::BitStringLiteral(token)),
            CharacterLiteral => Some(LiteralSyntax::CharacterLiteral(token)),
            StringLiteral => Some(LiteralSyntax::StringLiteral(token)),
            Keyword(Kw::Null) => Some(LiteralSyntax::Null(token)),
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
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ParenthesizedExpressionOrAggregate => {
                Some(ParenthesizedExpressionOrAggregateSyntax(node))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ParenthesizedExpressionOrAggregateSyntax {
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == LeftPar)
            .nth(0)
    }
    pub fn element_associations(&self) -> impl Iterator<Item = ElementAssociationSyntax> + use<'_> {
        self.0.children().filter_map(ElementAssociationSyntax::cast)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.tokens().filter(|token| token.kind() == Comma)
    }
    pub fn right_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == RightPar)
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
            QueQue => Some(UnaryOperatorSyntax::QueQue(token)),
            Plus => Some(UnaryOperatorSyntax::Plus(token)),
            Minus => Some(UnaryOperatorSyntax::Minus(token)),
            Keyword(Kw::Abs) => Some(UnaryOperatorSyntax::Abs(token)),
            Keyword(Kw::Not) => Some(UnaryOperatorSyntax::Not(token)),
            Keyword(Kw::And) => Some(UnaryOperatorSyntax::And(token)),
            Keyword(Kw::Or) => Some(UnaryOperatorSyntax::Or(token)),
            Keyword(Kw::Nand) => Some(UnaryOperatorSyntax::Nand(token)),
            Keyword(Kw::Nor) => Some(UnaryOperatorSyntax::Nor(token)),
            Keyword(Kw::Xor) => Some(UnaryOperatorSyntax::Xor(token)),
            Keyword(Kw::Xnor) => Some(UnaryOperatorSyntax::Xnor(token)),
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
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::UnaryExpression => Some(UnaryExpressionSyntax(node)),
            _ => None,
        }
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
            Keyword(Kw::And) => Some(BinaryOperatorSyntax::And(token)),
            Keyword(Kw::Or) => Some(BinaryOperatorSyntax::Or(token)),
            Keyword(Kw::Nand) => Some(BinaryOperatorSyntax::Nand(token)),
            Keyword(Kw::Nor) => Some(BinaryOperatorSyntax::Nor(token)),
            Keyword(Kw::Xor) => Some(BinaryOperatorSyntax::Xor(token)),
            Keyword(Kw::Xnor) => Some(BinaryOperatorSyntax::Xnor(token)),
            EQ => Some(BinaryOperatorSyntax::Eq(token)),
            NE => Some(BinaryOperatorSyntax::Ne(token)),
            LT => Some(BinaryOperatorSyntax::Lt(token)),
            LTE => Some(BinaryOperatorSyntax::Lte(token)),
            GT => Some(BinaryOperatorSyntax::Gt(token)),
            GTE => Some(BinaryOperatorSyntax::Gte(token)),
            QueEQ => Some(BinaryOperatorSyntax::QueEq(token)),
            QueNE => Some(BinaryOperatorSyntax::QueNe(token)),
            QueLT => Some(BinaryOperatorSyntax::QueLt(token)),
            QueGT => Some(BinaryOperatorSyntax::QueGt(token)),
            QueGTE => Some(BinaryOperatorSyntax::QueGte(token)),
            Keyword(Kw::Sll) => Some(BinaryOperatorSyntax::Sll(token)),
            Keyword(Kw::Srl) => Some(BinaryOperatorSyntax::Srl(token)),
            Keyword(Kw::Sla) => Some(BinaryOperatorSyntax::Sla(token)),
            Keyword(Kw::Sra) => Some(BinaryOperatorSyntax::Sra(token)),
            Keyword(Kw::Rol) => Some(BinaryOperatorSyntax::Rol(token)),
            Keyword(Kw::Ror) => Some(BinaryOperatorSyntax::Ror(token)),
            Plus => Some(BinaryOperatorSyntax::Plus(token)),
            Minus => Some(BinaryOperatorSyntax::Minus(token)),
            Concat => Some(BinaryOperatorSyntax::Concat(token)),
            Times => Some(BinaryOperatorSyntax::Times(token)),
            Div => Some(BinaryOperatorSyntax::Div(token)),
            Keyword(Kw::Mod) => Some(BinaryOperatorSyntax::Mod(token)),
            Keyword(Kw::Rem) => Some(BinaryOperatorSyntax::Rem(token)),
            Pow => Some(BinaryOperatorSyntax::Pow(token)),
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
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::BinaryExpression => Some(BinaryExpressionSyntax(node)),
            _ => None,
        }
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
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::LiteralExpression => Some(LiteralExpressionSyntax(node)),
            _ => None,
        }
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
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::PhysicalLiteralExpression => Some(PhysicalLiteralExpressionSyntax(node)),
            _ => None,
        }
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
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::NameExpression => Some(NameExpressionSyntax(node)),
            _ => None,
        }
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
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::LiteralExpression => Some(ExpressionSyntax::LiteralExpression(
                LiteralExpressionSyntax::cast(node).unwrap(),
            )),
            NodeKind::PhysicalLiteralExpression => {
                Some(ExpressionSyntax::PhysicalLiteralExpression(
                    PhysicalLiteralExpressionSyntax::cast(node).unwrap(),
                ))
            }
            NodeKind::UnaryExpression => Some(ExpressionSyntax::UnaryExpression(
                UnaryExpressionSyntax::cast(node).unwrap(),
            )),
            NodeKind::BinaryExpression => Some(ExpressionSyntax::BinaryExpression(
                BinaryExpressionSyntax::cast(node).unwrap(),
            )),
            NodeKind::ParenthesizedExpressionOrAggregate => {
                Some(ExpressionSyntax::ParenthesizedExpressionOrAggregate(
                    ParenthesizedExpressionOrAggregateSyntax::cast(node).unwrap(),
                ))
            }
            NodeKind::Allocator => Some(ExpressionSyntax::Allocator(
                AllocatorSyntax::cast(node).unwrap(),
            )),
            NodeKind::QualifiedExpression => Some(ExpressionSyntax::QualifiedExpression(
                QualifiedExpressionSyntax::cast(node).unwrap(),
            )),
            NodeKind::TypeConversion => Some(ExpressionSyntax::TypeConversion(
                TypeConversionSyntax::cast(node).unwrap(),
            )),
            NodeKind::NameExpression => Some(ExpressionSyntax::NameExpression(
                NameExpressionSyntax::cast(node).unwrap(),
            )),
            _ => None,
        }
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
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::Aggregate => Some(AggregateSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl AggregateSyntax {
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == LeftPar)
            .nth(0)
    }
    pub fn element_associations(&self) -> impl Iterator<Item = ElementAssociationSyntax> + use<'_> {
        self.0.children().filter_map(ElementAssociationSyntax::cast)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.tokens().filter(|token| token.kind() == Comma)
    }
    pub fn right_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == RightPar)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct SubtypeIndicationAllocatorSyntax(pub(crate) SyntaxNode);
impl AstNode for SubtypeIndicationAllocatorSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SubtypeIndicationAllocator => Some(SubtypeIndicationAllocatorSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SubtypeIndicationAllocatorSyntax {
    pub fn new_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::New))
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
pub struct QualifiedExpressionAllocatorSyntax(pub(crate) SyntaxNode);
impl AstNode for QualifiedExpressionAllocatorSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::QualifiedExpressionAllocator => {
                Some(QualifiedExpressionAllocatorSyntax(node))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl QualifiedExpressionAllocatorSyntax {
    pub fn new_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::New))
            .nth(0)
    }
    pub fn qualified_expression(&self) -> Option<QualifiedExpressionSyntax> {
        self.0
            .children()
            .filter_map(QualifiedExpressionSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum AllocatorSyntax {
    SubtypeIndicationAllocator(SubtypeIndicationAllocatorSyntax),
    QualifiedExpressionAllocator(QualifiedExpressionAllocatorSyntax),
}
impl AstNode for AllocatorSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SubtypeIndicationAllocator => {
                Some(AllocatorSyntax::SubtypeIndicationAllocator(
                    SubtypeIndicationAllocatorSyntax::cast(node).unwrap(),
                ))
            }
            NodeKind::QualifiedExpressionAllocator => {
                Some(AllocatorSyntax::QualifiedExpressionAllocator(
                    QualifiedExpressionAllocatorSyntax::cast(node).unwrap(),
                ))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            AllocatorSyntax::SubtypeIndicationAllocator(inner) => inner.raw(),
            AllocatorSyntax::QualifiedExpressionAllocator(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct OthersChoiceSyntax(pub(crate) SyntaxNode);
impl AstNode for OthersChoiceSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::OthersChoice => Some(OthersChoiceSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl OthersChoiceSyntax {
    pub fn others_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Others))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum ChoiceSyntax {
    Expression(ExpressionSyntax),
    OthersChoice(OthersChoiceSyntax),
    DiscreteRange(DiscreteRangeSyntax),
}
impl AstNode for ChoiceSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::Expression => Some(ChoiceSyntax::Expression(
                ExpressionSyntax::cast(node).unwrap(),
            )),
            NodeKind::OthersChoice => Some(ChoiceSyntax::OthersChoice(
                OthersChoiceSyntax::cast(node).unwrap(),
            )),
            NodeKind::DiscreteRange => Some(ChoiceSyntax::DiscreteRange(
                DiscreteRangeSyntax::cast(node).unwrap(),
            )),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            ChoiceSyntax::Expression(inner) => inner.raw(),
            ChoiceSyntax::OthersChoice(inner) => inner.raw(),
            ChoiceSyntax::DiscreteRange(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ChoicesSyntax(pub(crate) SyntaxNode);
impl AstNode for ChoicesSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::Choices => Some(ChoicesSyntax(node)),
            _ => None,
        }
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
        self.0.tokens().filter(|token| token.kind() == Bar)
    }
}
#[derive(Debug, Clone)]
pub struct ElementAssociationSyntax(pub(crate) SyntaxNode);
impl AstNode for ElementAssociationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ElementAssociation => Some(ElementAssociationSyntax(node)),
            _ => None,
        }
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
            .filter(|token| token.kind() == RightArrow)
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct QualifiedExpressionSyntax(pub(crate) SyntaxNode);
impl AstNode for QualifiedExpressionSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::QualifiedExpression => Some(QualifiedExpressionSyntax(node)),
            _ => None,
        }
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
        self.0.tokens().filter(|token| token.kind() == Tick).nth(0)
    }
    pub fn parenthesized_expression_or_aggregate(
        &self,
    ) -> Option<ParenthesizedExpressionOrAggregateSyntax> {
        self.0
            .children()
            .filter_map(ParenthesizedExpressionOrAggregateSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct TypeConversionSyntax(pub(crate) SyntaxNode);
impl AstNode for TypeConversionSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::TypeConversion => Some(TypeConversionSyntax(node)),
            _ => None,
        }
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
            .filter(|token| token.kind() == LeftPar)
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn right_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == RightPar)
            .nth(0)
    }
}
