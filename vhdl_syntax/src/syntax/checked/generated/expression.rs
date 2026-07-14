// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2026, Lukas Scheller lukasscheller@icloud.com
use super::*;
use crate::syntax::checked::CheckedNode;
use crate::syntax::node::SyntaxToken;
use crate::syntax::*;
#[derive(Debug, Clone)]
pub struct CheckedParenthesizedExpressionOrAggregate(ParenthesizedExpressionOrAggregateSyntax);
impl CheckedNode for CheckedParenthesizedExpressionOrAggregate {
    type Syntax = ParenthesizedExpressionOrAggregateSyntax;
    fn cast_unchecked(syntax: ParenthesizedExpressionOrAggregateSyntax) -> Self {
        CheckedParenthesizedExpressionOrAggregate(syntax)
    }
    fn raw(&self) -> ParenthesizedExpressionOrAggregateSyntax {
        self.0.clone()
    }
}
impl CheckedParenthesizedExpressionOrAggregate {
    pub fn left_par_token(&self) -> SyntaxToken {
        self.0.left_par_token().unwrap()
    }
    pub fn element_associations(
        &self,
    ) -> impl Iterator<Item = CheckedElementAssociation> + use<'_> {
        self.0
            .element_associations()
            .map(CheckedElementAssociation::cast_unchecked)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.comma_token()
    }
    pub fn right_par_token(&self) -> SyntaxToken {
        self.0.right_par_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedUnaryExpression(UnaryExpressionSyntax);
impl CheckedNode for CheckedUnaryExpression {
    type Syntax = UnaryExpressionSyntax;
    fn cast_unchecked(syntax: UnaryExpressionSyntax) -> Self {
        CheckedUnaryExpression(syntax)
    }
    fn raw(&self) -> UnaryExpressionSyntax {
        self.0.clone()
    }
}
impl CheckedUnaryExpression {
    pub fn op(&self) -> UnaryOperatorSyntax {
        self.0.op().unwrap()
    }
    pub fn expression(&self) -> CheckedExpression {
        CheckedExpression::cast_unchecked(self.0.expression().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedBinaryExpression(BinaryExpressionSyntax);
impl CheckedNode for CheckedBinaryExpression {
    type Syntax = BinaryExpressionSyntax;
    fn cast_unchecked(syntax: BinaryExpressionSyntax) -> Self {
        CheckedBinaryExpression(syntax)
    }
    fn raw(&self) -> BinaryExpressionSyntax {
        self.0.clone()
    }
}
impl CheckedBinaryExpression {
    pub fn lhs(&self) -> CheckedExpression {
        CheckedExpression::cast_unchecked(self.0.lhs().unwrap())
    }
    pub fn op(&self) -> BinaryOperatorSyntax {
        self.0.op().unwrap()
    }
    pub fn rhs(&self) -> CheckedExpression {
        CheckedExpression::cast_unchecked(self.0.rhs().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedLiteralExpression(LiteralExpressionSyntax);
impl CheckedNode for CheckedLiteralExpression {
    type Syntax = LiteralExpressionSyntax;
    fn cast_unchecked(syntax: LiteralExpressionSyntax) -> Self {
        CheckedLiteralExpression(syntax)
    }
    fn raw(&self) -> LiteralExpressionSyntax {
        self.0.clone()
    }
}
impl CheckedLiteralExpression {
    pub fn literal(&self) -> LiteralSyntax {
        self.0.literal().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedPhysicalLiteralExpression(PhysicalLiteralExpressionSyntax);
impl CheckedNode for CheckedPhysicalLiteralExpression {
    type Syntax = PhysicalLiteralExpressionSyntax;
    fn cast_unchecked(syntax: PhysicalLiteralExpressionSyntax) -> Self {
        CheckedPhysicalLiteralExpression(syntax)
    }
    fn raw(&self) -> PhysicalLiteralExpressionSyntax {
        self.0.clone()
    }
}
impl CheckedPhysicalLiteralExpression {
    pub fn physical_literal(&self) -> CheckedPhysicalLiteral {
        CheckedPhysicalLiteral::cast_unchecked(self.0.physical_literal().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedNameExpression(NameExpressionSyntax);
impl CheckedNode for CheckedNameExpression {
    type Syntax = NameExpressionSyntax;
    fn cast_unchecked(syntax: NameExpressionSyntax) -> Self {
        CheckedNameExpression(syntax)
    }
    fn raw(&self) -> NameExpressionSyntax {
        self.0.clone()
    }
}
impl CheckedNameExpression {
    pub fn name(&self) -> CheckedName {
        CheckedName::cast_unchecked(self.0.name().unwrap())
    }
}
#[derive(Debug, Clone)]
pub enum CheckedExpression {
    LiteralExpression(CheckedLiteralExpression),
    PhysicalLiteralExpression(CheckedPhysicalLiteralExpression),
    UnaryExpression(CheckedUnaryExpression),
    BinaryExpression(CheckedBinaryExpression),
    ParenthesizedExpressionOrAggregate(CheckedParenthesizedExpressionOrAggregate),
    Allocator(CheckedAllocator),
    QualifiedExpression(CheckedQualifiedExpression),
    TypeConversion(CheckedTypeConversion),
    NameExpression(CheckedNameExpression),
}
impl CheckedNode for CheckedExpression {
    type Syntax = ExpressionSyntax;
    fn cast_unchecked(syntax: ExpressionSyntax) -> Self {
        match syntax {
            ExpressionSyntax::LiteralExpression(inner) => CheckedExpression::LiteralExpression(
                CheckedLiteralExpression::cast_unchecked(inner),
            ),
            ExpressionSyntax::PhysicalLiteralExpression(inner) => {
                CheckedExpression::PhysicalLiteralExpression(
                    CheckedPhysicalLiteralExpression::cast_unchecked(inner),
                )
            }
            ExpressionSyntax::UnaryExpression(inner) => {
                CheckedExpression::UnaryExpression(CheckedUnaryExpression::cast_unchecked(inner))
            }
            ExpressionSyntax::BinaryExpression(inner) => {
                CheckedExpression::BinaryExpression(CheckedBinaryExpression::cast_unchecked(inner))
            }
            ExpressionSyntax::ParenthesizedExpressionOrAggregate(inner) => {
                CheckedExpression::ParenthesizedExpressionOrAggregate(
                    CheckedParenthesizedExpressionOrAggregate::cast_unchecked(inner),
                )
            }
            ExpressionSyntax::Allocator(inner) => {
                CheckedExpression::Allocator(CheckedAllocator::cast_unchecked(inner))
            }
            ExpressionSyntax::QualifiedExpression(inner) => CheckedExpression::QualifiedExpression(
                CheckedQualifiedExpression::cast_unchecked(inner),
            ),
            ExpressionSyntax::TypeConversion(inner) => {
                CheckedExpression::TypeConversion(CheckedTypeConversion::cast_unchecked(inner))
            }
            ExpressionSyntax::NameExpression(inner) => {
                CheckedExpression::NameExpression(CheckedNameExpression::cast_unchecked(inner))
            }
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedExpression::LiteralExpression(inner) => {
                ExpressionSyntax::LiteralExpression(inner.raw())
            }
            CheckedExpression::PhysicalLiteralExpression(inner) => {
                ExpressionSyntax::PhysicalLiteralExpression(inner.raw())
            }
            CheckedExpression::UnaryExpression(inner) => {
                ExpressionSyntax::UnaryExpression(inner.raw())
            }
            CheckedExpression::BinaryExpression(inner) => {
                ExpressionSyntax::BinaryExpression(inner.raw())
            }
            CheckedExpression::ParenthesizedExpressionOrAggregate(inner) => {
                ExpressionSyntax::ParenthesizedExpressionOrAggregate(inner.raw())
            }
            CheckedExpression::Allocator(inner) => ExpressionSyntax::Allocator(inner.raw()),
            CheckedExpression::QualifiedExpression(inner) => {
                ExpressionSyntax::QualifiedExpression(inner.raw())
            }
            CheckedExpression::TypeConversion(inner) => {
                ExpressionSyntax::TypeConversion(inner.raw())
            }
            CheckedExpression::NameExpression(inner) => {
                ExpressionSyntax::NameExpression(inner.raw())
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedAggregate(AggregateSyntax);
impl CheckedNode for CheckedAggregate {
    type Syntax = AggregateSyntax;
    fn cast_unchecked(syntax: AggregateSyntax) -> Self {
        CheckedAggregate(syntax)
    }
    fn raw(&self) -> AggregateSyntax {
        self.0.clone()
    }
}
impl CheckedAggregate {
    pub fn left_par_token(&self) -> SyntaxToken {
        self.0.left_par_token().unwrap()
    }
    pub fn element_associations(
        &self,
    ) -> impl Iterator<Item = CheckedElementAssociation> + use<'_> {
        self.0
            .element_associations()
            .map(CheckedElementAssociation::cast_unchecked)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.comma_token()
    }
    pub fn right_par_token(&self) -> SyntaxToken {
        self.0.right_par_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedSubtypeIndicationAllocator(SubtypeIndicationAllocatorSyntax);
impl CheckedNode for CheckedSubtypeIndicationAllocator {
    type Syntax = SubtypeIndicationAllocatorSyntax;
    fn cast_unchecked(syntax: SubtypeIndicationAllocatorSyntax) -> Self {
        CheckedSubtypeIndicationAllocator(syntax)
    }
    fn raw(&self) -> SubtypeIndicationAllocatorSyntax {
        self.0.clone()
    }
}
impl CheckedSubtypeIndicationAllocator {
    pub fn new_token(&self) -> SyntaxToken {
        self.0.new_token().unwrap()
    }
    pub fn subtype_indication(&self) -> CheckedSubtypeIndication {
        CheckedSubtypeIndication::cast_unchecked(self.0.subtype_indication().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedExpressionAllocator(ExpressionAllocatorSyntax);
impl CheckedNode for CheckedExpressionAllocator {
    type Syntax = ExpressionAllocatorSyntax;
    fn cast_unchecked(syntax: ExpressionAllocatorSyntax) -> Self {
        CheckedExpressionAllocator(syntax)
    }
    fn raw(&self) -> ExpressionAllocatorSyntax {
        self.0.clone()
    }
}
impl CheckedExpressionAllocator {
    pub fn new_token(&self) -> SyntaxToken {
        self.0.new_token().unwrap()
    }
    pub fn expression(&self) -> CheckedExpression {
        CheckedExpression::cast_unchecked(self.0.expression().unwrap())
    }
}
#[derive(Debug, Clone)]
pub enum CheckedAllocator {
    SubtypeIndicationAllocator(CheckedSubtypeIndicationAllocator),
    ExpressionAllocator(CheckedExpressionAllocator),
}
impl CheckedNode for CheckedAllocator {
    type Syntax = AllocatorSyntax;
    fn cast_unchecked(syntax: AllocatorSyntax) -> Self {
        match syntax {
            AllocatorSyntax::SubtypeIndicationAllocator(inner) => {
                CheckedAllocator::SubtypeIndicationAllocator(
                    CheckedSubtypeIndicationAllocator::cast_unchecked(inner),
                )
            }
            AllocatorSyntax::ExpressionAllocator(inner) => CheckedAllocator::ExpressionAllocator(
                CheckedExpressionAllocator::cast_unchecked(inner),
            ),
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedAllocator::SubtypeIndicationAllocator(inner) => {
                AllocatorSyntax::SubtypeIndicationAllocator(inner.raw())
            }
            CheckedAllocator::ExpressionAllocator(inner) => {
                AllocatorSyntax::ExpressionAllocator(inner.raw())
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedOthersChoice(OthersChoiceSyntax);
impl CheckedNode for CheckedOthersChoice {
    type Syntax = OthersChoiceSyntax;
    fn cast_unchecked(syntax: OthersChoiceSyntax) -> Self {
        CheckedOthersChoice(syntax)
    }
    fn raw(&self) -> OthersChoiceSyntax {
        self.0.clone()
    }
}
impl CheckedOthersChoice {
    pub fn others_token(&self) -> SyntaxToken {
        self.0.others_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedExpressionChoice(ExpressionChoiceSyntax);
impl CheckedNode for CheckedExpressionChoice {
    type Syntax = ExpressionChoiceSyntax;
    fn cast_unchecked(syntax: ExpressionChoiceSyntax) -> Self {
        CheckedExpressionChoice(syntax)
    }
    fn raw(&self) -> ExpressionChoiceSyntax {
        self.0.clone()
    }
}
impl CheckedExpressionChoice {
    pub fn expression(&self) -> CheckedExpression {
        CheckedExpression::cast_unchecked(self.0.expression().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedDiscreteRangeChoice(DiscreteRangeChoiceSyntax);
impl CheckedNode for CheckedDiscreteRangeChoice {
    type Syntax = DiscreteRangeChoiceSyntax;
    fn cast_unchecked(syntax: DiscreteRangeChoiceSyntax) -> Self {
        CheckedDiscreteRangeChoice(syntax)
    }
    fn raw(&self) -> DiscreteRangeChoiceSyntax {
        self.0.clone()
    }
}
impl CheckedDiscreteRangeChoice {
    pub fn discrete_range(&self) -> CheckedDiscreteRange {
        CheckedDiscreteRange::cast_unchecked(self.0.discrete_range().unwrap())
    }
}
#[derive(Debug, Clone)]
pub enum CheckedChoice {
    ExpressionChoice(CheckedExpressionChoice),
    OthersChoice(CheckedOthersChoice),
    DiscreteRangeChoice(CheckedDiscreteRangeChoice),
}
impl CheckedNode for CheckedChoice {
    type Syntax = ChoiceSyntax;
    fn cast_unchecked(syntax: ChoiceSyntax) -> Self {
        match syntax {
            ChoiceSyntax::ExpressionChoice(inner) => {
                CheckedChoice::ExpressionChoice(CheckedExpressionChoice::cast_unchecked(inner))
            }
            ChoiceSyntax::OthersChoice(inner) => {
                CheckedChoice::OthersChoice(CheckedOthersChoice::cast_unchecked(inner))
            }
            ChoiceSyntax::DiscreteRangeChoice(inner) => CheckedChoice::DiscreteRangeChoice(
                CheckedDiscreteRangeChoice::cast_unchecked(inner),
            ),
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedChoice::ExpressionChoice(inner) => ChoiceSyntax::ExpressionChoice(inner.raw()),
            CheckedChoice::OthersChoice(inner) => ChoiceSyntax::OthersChoice(inner.raw()),
            CheckedChoice::DiscreteRangeChoice(inner) => {
                ChoiceSyntax::DiscreteRangeChoice(inner.raw())
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedChoices(ChoicesSyntax);
impl CheckedNode for CheckedChoices {
    type Syntax = ChoicesSyntax;
    fn cast_unchecked(syntax: ChoicesSyntax) -> Self {
        CheckedChoices(syntax)
    }
    fn raw(&self) -> ChoicesSyntax {
        self.0.clone()
    }
}
impl CheckedChoices {
    pub fn choices(&self) -> impl Iterator<Item = CheckedChoice> + use<'_> {
        self.0.choices().map(CheckedChoice::cast_unchecked)
    }
    pub fn bar_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.bar_token()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedElementAssociation(ElementAssociationSyntax);
impl CheckedNode for CheckedElementAssociation {
    type Syntax = ElementAssociationSyntax;
    fn cast_unchecked(syntax: ElementAssociationSyntax) -> Self {
        CheckedElementAssociation(syntax)
    }
    fn raw(&self) -> ElementAssociationSyntax {
        self.0.clone()
    }
}
impl CheckedElementAssociation {
    pub fn choice(&self) -> CheckedChoice {
        CheckedChoice::cast_unchecked(self.0.choice().unwrap())
    }
    pub fn right_arrow_token(&self) -> SyntaxToken {
        self.0.right_arrow_token().unwrap()
    }
    pub fn expression(&self) -> CheckedExpression {
        CheckedExpression::cast_unchecked(self.0.expression().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedQualifiedExpression(QualifiedExpressionSyntax);
impl CheckedNode for CheckedQualifiedExpression {
    type Syntax = QualifiedExpressionSyntax;
    fn cast_unchecked(syntax: QualifiedExpressionSyntax) -> Self {
        CheckedQualifiedExpression(syntax)
    }
    fn raw(&self) -> QualifiedExpressionSyntax {
        self.0.clone()
    }
}
impl CheckedQualifiedExpression {
    pub fn name(&self) -> CheckedName {
        CheckedName::cast_unchecked(self.0.name().unwrap())
    }
    pub fn tick_token(&self) -> SyntaxToken {
        self.0.tick_token().unwrap()
    }
    pub fn expression(&self) -> CheckedExpression {
        CheckedExpression::cast_unchecked(self.0.expression().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedTypeConversion(TypeConversionSyntax);
impl CheckedNode for CheckedTypeConversion {
    type Syntax = TypeConversionSyntax;
    fn cast_unchecked(syntax: TypeConversionSyntax) -> Self {
        CheckedTypeConversion(syntax)
    }
    fn raw(&self) -> TypeConversionSyntax {
        self.0.clone()
    }
}
impl CheckedTypeConversion {
    pub fn name(&self) -> CheckedName {
        CheckedName::cast_unchecked(self.0.name().unwrap())
    }
    pub fn left_par_token(&self) -> SyntaxToken {
        self.0.left_par_token().unwrap()
    }
    pub fn expression(&self) -> CheckedExpression {
        CheckedExpression::cast_unchecked(self.0.expression().unwrap())
    }
    pub fn right_par_token(&self) -> SyntaxToken {
        self.0.right_par_token().unwrap()
    }
}
