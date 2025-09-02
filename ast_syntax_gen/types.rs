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
pub struct AccessTypeDefinitionSyntax(pub(crate) SyntaxNode);
impl AstNode for AccessTypeDefinitionSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::AccessTypeDefinition => Some(AccessTypeDefinitionSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl AccessTypeDefinitionSyntax {
    pub fn access_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Access))
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
pub struct ArrayConstraintSyntax(pub(crate) SyntaxNode);
impl AstNode for ArrayConstraintSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ArrayConstraint => Some(ArrayConstraintSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ArrayConstraintSyntax {
    pub fn index_constraint(&self) -> Option<IndexConstraintSyntax> {
        self.0
            .children()
            .filter_map(IndexConstraintSyntax::cast)
            .nth(0)
    }
    pub fn element_constraint(&self) -> Option<ElementConstraintSyntax> {
        self.0
            .children()
            .filter_map(ElementConstraintSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum ArrayTypeDefinitionSyntax {
    UnboundedArrayDefinition(UnboundedArrayDefinitionSyntax),
    ConstrainedArrayDefinition(ConstrainedArrayDefinitionSyntax),
}
impl AstNode for ArrayTypeDefinitionSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::UnboundedArrayDefinition => {
                Some(ArrayTypeDefinitionSyntax::UnboundedArrayDefinition(
                    UnboundedArrayDefinitionSyntax::cast(node).unwrap(),
                ))
            }
            NodeKind::ConstrainedArrayDefinition => {
                Some(ArrayTypeDefinitionSyntax::ConstrainedArrayDefinition(
                    ConstrainedArrayDefinitionSyntax::cast(node).unwrap(),
                ))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            ArrayTypeDefinitionSyntax::UnboundedArrayDefinition(inner) => inner.raw(),
            ArrayTypeDefinitionSyntax::ConstrainedArrayDefinition(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum CompositeTypeDefinitionSyntax {
    ArrayTypeDefinition(ArrayTypeDefinitionSyntax),
    RecordTypeDefinition(RecordTypeDefinitionSyntax),
}
impl AstNode for CompositeTypeDefinitionSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ArrayTypeDefinition => {
                Some(CompositeTypeDefinitionSyntax::ArrayTypeDefinition(
                    ArrayTypeDefinitionSyntax::cast(node).unwrap(),
                ))
            }
            NodeKind::RecordTypeDefinition => {
                Some(CompositeTypeDefinitionSyntax::RecordTypeDefinition(
                    RecordTypeDefinitionSyntax::cast(node).unwrap(),
                ))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            CompositeTypeDefinitionSyntax::ArrayTypeDefinition(inner) => inner.raw(),
            CompositeTypeDefinitionSyntax::RecordTypeDefinition(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ConstrainedArrayDefinitionSyntax(pub(crate) SyntaxNode);
impl AstNode for ConstrainedArrayDefinitionSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ConstrainedArrayDefinition => Some(ConstrainedArrayDefinitionSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConstrainedArrayDefinitionSyntax {
    pub fn array_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Array))
            .nth(0)
    }
    pub fn index_constraint(&self) -> Option<IndexConstraintSyntax> {
        self.0
            .children()
            .filter_map(IndexConstraintSyntax::cast)
            .nth(0)
    }
    pub fn of_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Of))
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
pub enum DirectionSyntax {
    To(SyntaxToken),
    Downto(SyntaxToken),
}
impl DirectionSyntax {
    pub fn cast(token: SyntaxToken) -> Option<Self> {
        match token.kind() {
            Keyword(Kw::To) => Some(DirectionSyntax::To(token)),
            Keyword(Kw::Downto) => Some(DirectionSyntax::Downto(token)),
            _ => None,
        }
    }
    pub fn raw(&self) -> SyntaxToken {
        match self {
            DirectionSyntax::To(token) => token.clone(),
            DirectionSyntax::Downto(token) => token.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum DiscreteRangeSyntax {
    SubtypeIndication(SubtypeIndicationSyntax),
    Range(RangeSyntax),
    OpenRange(OpenRangeSyntax),
}
impl AstNode for DiscreteRangeSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SubtypeIndication => Some(DiscreteRangeSyntax::SubtypeIndication(
                SubtypeIndicationSyntax::cast(node).unwrap(),
            )),
            NodeKind::Range => Some(DiscreteRangeSyntax::Range(RangeSyntax::cast(node).unwrap())),
            NodeKind::OpenRange => Some(DiscreteRangeSyntax::OpenRange(
                OpenRangeSyntax::cast(node).unwrap(),
            )),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            DiscreteRangeSyntax::SubtypeIndication(inner) => inner.raw(),
            DiscreteRangeSyntax::Range(inner) => inner.raw(),
            DiscreteRangeSyntax::OpenRange(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct OpenRangeSyntax(pub(crate) SyntaxNode);
impl AstNode for OpenRangeSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::OpenRange => Some(OpenRangeSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl OpenRangeSyntax {
    pub fn open_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Open))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ElementDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for ElementDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ElementDeclaration => Some(ElementDeclarationSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ElementDeclarationSyntax {
    pub fn identifier_list(&self) -> Option<IdentifierListSyntax> {
        self.0
            .children()
            .filter_map(IdentifierListSyntax::cast)
            .nth(0)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == Colon).nth(0)
    }
    pub fn subtype_indication(&self) -> Option<SubtypeIndicationSyntax> {
        self.0
            .children()
            .filter_map(SubtypeIndicationSyntax::cast)
            .nth(0)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct EnumerationTypeDefinitionSyntax(pub(crate) SyntaxNode);
impl AstNode for EnumerationTypeDefinitionSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::EnumerationTypeDefinition => Some(EnumerationTypeDefinitionSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl EnumerationTypeDefinitionSyntax {
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == LeftPar)
            .nth(0)
    }
    pub fn discrete_ranges(&self) -> impl Iterator<Item = DiscreteRangeSyntax> + use<'_> {
        self.0.children().filter_map(DiscreteRangeSyntax::cast)
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
pub struct FileTypeDefinitionSyntax(pub(crate) SyntaxNode);
impl AstNode for FileTypeDefinitionSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::FileTypeDefinition => Some(FileTypeDefinitionSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl FileTypeDefinitionSyntax {
    pub fn file_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::File))
            .nth(0)
    }
    pub fn of_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Of))
            .nth(0)
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct IdentifierListSyntax(pub(crate) SyntaxNode);
impl AstNode for IdentifierListSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::IdentifierList => Some(IdentifierListSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl IdentifierListSyntax {
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Identifier)
            .nth(0)
    }
    pub fn comma_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == Comma).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct IncompleteTypeDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for IncompleteTypeDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::IncompleteTypeDeclaration => Some(IncompleteTypeDeclarationSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl IncompleteTypeDeclarationSyntax {
    pub fn type_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Type))
            .nth(0)
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Identifier)
            .nth(0)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct IndexSubtypeDefinitionSyntax(pub(crate) SyntaxNode);
impl AstNode for IndexSubtypeDefinitionSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::IndexSubtypeDefinition => Some(IndexSubtypeDefinitionSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl IndexSubtypeDefinitionSyntax {
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
    pub fn range_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Range))
            .nth(0)
    }
    pub fn box_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == BOX).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct PhysicalLiteralSyntax(pub(crate) SyntaxNode);
impl AstNode for PhysicalLiteralSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::PhysicalLiteral => Some(PhysicalLiteralSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PhysicalLiteralSyntax {
    pub fn abstract_literal_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == AbstractLiteral)
            .nth(0)
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct PhysicalTypeDefinitionSyntax(pub(crate) SyntaxNode);
impl AstNode for PhysicalTypeDefinitionSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::PhysicalTypeDefinition => Some(PhysicalTypeDefinitionSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PhysicalTypeDefinitionSyntax {
    pub fn range_constraint(&self) -> Option<RangeConstraintSyntax> {
        self.0
            .children()
            .filter_map(RangeConstraintSyntax::cast)
            .nth(0)
    }
    pub fn units_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Units))
            .nth(0)
    }
    pub fn primary_unit_declaration(&self) -> Option<PrimaryUnitDeclarationSyntax> {
        self.0
            .children()
            .filter_map(PrimaryUnitDeclarationSyntax::cast)
            .nth(0)
    }
    pub fn secondary_unit_declarations(
        &self,
    ) -> impl Iterator<Item = SecondaryUnitDeclarationSyntax> + use<'_> {
        self.0
            .children()
            .filter_map(SecondaryUnitDeclarationSyntax::cast)
    }
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::End))
            .nth(0)
    }
    pub fn trailing_units_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Units))
            .nth(1)
    }
    pub fn name_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Identifier)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct PrimaryUnitDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for PrimaryUnitDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::PrimaryUnitDeclaration => Some(PrimaryUnitDeclarationSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PrimaryUnitDeclarationSyntax {
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Identifier)
            .nth(0)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ProtectedTypeBodySyntax(pub(crate) SyntaxNode);
impl AstNode for ProtectedTypeBodySyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ProtectedTypeBody => Some(ProtectedTypeBodySyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ProtectedTypeBodySyntax {
    pub fn protected_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Protected))
            .nth(0)
    }
    pub fn body_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Body))
            .nth(0)
    }
    pub fn declarations(&self) -> impl Iterator<Item = DeclarationSyntax> + use<'_> {
        self.0.children().filter_map(DeclarationSyntax::cast)
    }
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::End))
            .nth(0)
    }
    pub fn trailing_protected_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Protected))
            .nth(1)
    }
    pub fn trailing_body_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Body))
            .nth(1)
    }
    pub fn name_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Identifier)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ProtectedTypeDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for ProtectedTypeDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ProtectedTypeDeclaration => Some(ProtectedTypeDeclarationSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ProtectedTypeDeclarationSyntax {
    pub fn protected_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Protected))
            .nth(0)
    }
    pub fn declarations(&self) -> impl Iterator<Item = DeclarationSyntax> + use<'_> {
        self.0.children().filter_map(DeclarationSyntax::cast)
    }
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::End))
            .nth(0)
    }
    pub fn trailing_protected_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Protected))
            .nth(1)
    }
    pub fn name_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Identifier)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum ProtectedTypeDefinitionSyntax {
    ProtectedTypeDeclaration(ProtectedTypeDeclarationSyntax),
    ProtectedTypeBody(ProtectedTypeBodySyntax),
}
impl AstNode for ProtectedTypeDefinitionSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ProtectedTypeDeclaration => {
                Some(ProtectedTypeDefinitionSyntax::ProtectedTypeDeclaration(
                    ProtectedTypeDeclarationSyntax::cast(node).unwrap(),
                ))
            }
            NodeKind::ProtectedTypeBody => Some(ProtectedTypeDefinitionSyntax::ProtectedTypeBody(
                ProtectedTypeBodySyntax::cast(node).unwrap(),
            )),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            ProtectedTypeDefinitionSyntax::ProtectedTypeDeclaration(inner) => inner.raw(),
            ProtectedTypeDefinitionSyntax::ProtectedTypeBody(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct RangeExpressionSyntax(pub(crate) SyntaxNode);
impl AstNode for RangeExpressionSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::RangeExpression => Some(RangeExpressionSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl RangeExpressionSyntax {
    pub fn lhs(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn direction(&self) -> Option<DirectionSyntax> {
        self.0.tokens().filter_map(DirectionSyntax::cast).nth(0)
    }
    pub fn rhs(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(1)
    }
}
#[derive(Debug, Clone)]
pub enum RangeSyntax {
    Name(NameSyntax),
    RangeExpression(RangeExpressionSyntax),
}
impl AstNode for RangeSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::Name => Some(RangeSyntax::Name(NameSyntax::cast(node).unwrap())),
            NodeKind::RangeExpression => Some(RangeSyntax::RangeExpression(
                RangeExpressionSyntax::cast(node).unwrap(),
            )),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            RangeSyntax::Name(inner) => inner.raw(),
            RangeSyntax::RangeExpression(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct RangeConstraintSyntax(pub(crate) SyntaxNode);
impl AstNode for RangeConstraintSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::RangeConstraint => Some(RangeConstraintSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl RangeConstraintSyntax {
    pub fn range_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Range))
            .nth(0)
    }
    pub fn range(&self) -> Option<RangeSyntax> {
        self.0.children().filter_map(RangeSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct RecordConstraintSyntax(pub(crate) SyntaxNode);
impl AstNode for RecordConstraintSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::RecordConstraint => Some(RecordConstraintSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl RecordConstraintSyntax {
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == LeftPar)
            .nth(0)
    }
    pub fn record_element_constraints(
        &self,
    ) -> impl Iterator<Item = RecordElementConstraintSyntax> + use<'_> {
        self.0
            .children()
            .filter_map(RecordElementConstraintSyntax::cast)
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
pub struct RecordElementConstraintSyntax(pub(crate) SyntaxNode);
impl AstNode for RecordElementConstraintSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::RecordElementConstraint => Some(RecordElementConstraintSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl RecordElementConstraintSyntax {
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
    pub fn element_constraint(&self) -> Option<ElementConstraintSyntax> {
        self.0
            .children()
            .filter_map(ElementConstraintSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct RecordTypeDefinitionSyntax(pub(crate) SyntaxNode);
impl AstNode for RecordTypeDefinitionSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::RecordTypeDefinition => Some(RecordTypeDefinitionSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl RecordTypeDefinitionSyntax {
    pub fn record_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Record))
            .nth(0)
    }
    pub fn element_declarations(&self) -> impl Iterator<Item = ElementDeclarationSyntax> + use<'_> {
        self.0.children().filter_map(ElementDeclarationSyntax::cast)
    }
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::End))
            .nth(0)
    }
    pub fn trailing_record_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Record))
            .nth(1)
    }
    pub fn trailing_name_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Identifier)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum ScalarTypeDefinitionSyntax {
    EnumerationTypeDefinition(EnumerationTypeDefinitionSyntax),
    RangeConstraint(RangeConstraintSyntax),
    PhysicalTypeDefinition(PhysicalTypeDefinitionSyntax),
}
impl AstNode for ScalarTypeDefinitionSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::EnumerationTypeDefinition => {
                Some(ScalarTypeDefinitionSyntax::EnumerationTypeDefinition(
                    EnumerationTypeDefinitionSyntax::cast(node).unwrap(),
                ))
            }
            NodeKind::RangeConstraint => Some(ScalarTypeDefinitionSyntax::RangeConstraint(
                RangeConstraintSyntax::cast(node).unwrap(),
            )),
            NodeKind::PhysicalTypeDefinition => {
                Some(ScalarTypeDefinitionSyntax::PhysicalTypeDefinition(
                    PhysicalTypeDefinitionSyntax::cast(node).unwrap(),
                ))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            ScalarTypeDefinitionSyntax::EnumerationTypeDefinition(inner) => inner.raw(),
            ScalarTypeDefinitionSyntax::RangeConstraint(inner) => inner.raw(),
            ScalarTypeDefinitionSyntax::PhysicalTypeDefinition(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SecondaryUnitDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for SecondaryUnitDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SecondaryUnitDeclaration => Some(SecondaryUnitDeclarationSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SecondaryUnitDeclarationSyntax {
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Identifier)
            .nth(0)
    }
    pub fn eq_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == EQ).nth(0)
    }
    pub fn physical_literal(&self) -> Option<PhysicalLiteralSyntax> {
        self.0
            .children()
            .filter_map(PhysicalLiteralSyntax::cast)
            .nth(0)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct IndexConstraintSyntax(pub(crate) SyntaxNode);
impl AstNode for IndexConstraintSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::IndexConstraint => Some(IndexConstraintSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl IndexConstraintSyntax {
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == LeftPar)
            .nth(0)
    }
    pub fn discrete_ranges(&self) -> impl Iterator<Item = DiscreteRangeSyntax> + use<'_> {
        self.0.children().filter_map(DiscreteRangeSyntax::cast)
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
pub struct IndexSubtypeDefinitionListSyntax(pub(crate) SyntaxNode);
impl AstNode for IndexSubtypeDefinitionListSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::IndexSubtypeDefinitionList => Some(IndexSubtypeDefinitionListSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl IndexSubtypeDefinitionListSyntax {
    pub fn index_subtype_definitions(
        &self,
    ) -> impl Iterator<Item = IndexSubtypeDefinitionSyntax> + use<'_> {
        self.0
            .children()
            .filter_map(IndexSubtypeDefinitionSyntax::cast)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.tokens().filter(|token| token.kind() == Comma)
    }
}
#[derive(Debug, Clone)]
pub struct UnboundedArrayDefinitionSyntax(pub(crate) SyntaxNode);
impl AstNode for UnboundedArrayDefinitionSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::UnboundedArrayDefinition => Some(UnboundedArrayDefinitionSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl UnboundedArrayDefinitionSyntax {
    pub fn array_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Array))
            .nth(0)
    }
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == LeftPar)
            .nth(0)
    }
    pub fn index_subtype_definition_list(&self) -> Option<IndexSubtypeDefinitionListSyntax> {
        self.0
            .children()
            .filter_map(IndexSubtypeDefinitionListSyntax::cast)
            .nth(0)
    }
    pub fn right_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == RightPar)
            .nth(0)
    }
    pub fn of_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Of))
            .nth(0)
    }
    pub fn subtype_indication(&self) -> Option<SubtypeIndicationSyntax> {
        self.0
            .children()
            .filter_map(SubtypeIndicationSyntax::cast)
            .nth(0)
    }
}
