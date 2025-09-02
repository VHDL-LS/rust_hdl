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
pub enum DeclarationSyntax {
    SubprogramDeclaration(SubprogramDeclarationSyntax),
    SubprogramBody(SubprogramBodySyntax),
    SubprogramInstantiationDeclaration(SubprogramInstantiationDeclarationSyntax),
    PackageDeclaration(PackageDeclarationSyntax),
    PackageBody(PackageBodySyntax),
    PackageInstantiationDeclaration(PackageInstantiationDeclarationSyntax),
    TypeDeclaration(TypeDeclarationSyntax),
    SubtypeDeclaration(SubtypeDeclarationSyntax),
    FileDeclaration(FileDeclarationSyntax),
    AliasDeclaration(AliasDeclarationSyntax),
    ComponentDeclaration(ComponentDeclarationSyntax),
    AttributeDeclaration(AttributeDeclarationSyntax),
    AttributeSpecification(AttributeSpecificationSyntax),
    ConfigurationSpecification(ConfigurationSpecificationSyntax),
    DisconnectionSpecification(DisconnectionSpecificationSyntax),
    UseClause(UseClauseSyntax),
    GroupTemplateDeclaration(GroupTemplateDeclarationSyntax),
    GroupDeclaration(GroupDeclarationSyntax),
    ConstantDeclaration(ConstantDeclarationSyntax),
    SignalDeclaration(SignalDeclarationSyntax),
    VariableDeclaration(VariableDeclarationSyntax),
    SharedVariableDeclaration(SharedVariableDeclarationSyntax),
    PslPropertyDeclaration(PslPropertyDeclarationSyntax),
    PslSequenceDeclaration(PslSequenceDeclarationSyntax),
    PslClockDeclaration(PslClockDeclarationSyntax),
}
impl AstNode for DeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SubprogramDeclaration => Some(DeclarationSyntax::SubprogramDeclaration(
                SubprogramDeclarationSyntax::cast(node).unwrap(),
            )),
            NodeKind::SubprogramBody => Some(DeclarationSyntax::SubprogramBody(
                SubprogramBodySyntax::cast(node).unwrap(),
            )),
            NodeKind::SubprogramInstantiationDeclaration => {
                Some(DeclarationSyntax::SubprogramInstantiationDeclaration(
                    SubprogramInstantiationDeclarationSyntax::cast(node).unwrap(),
                ))
            }
            NodeKind::PackageDeclaration => Some(DeclarationSyntax::PackageDeclaration(
                PackageDeclarationSyntax::cast(node).unwrap(),
            )),
            NodeKind::PackageBody => Some(DeclarationSyntax::PackageBody(
                PackageBodySyntax::cast(node).unwrap(),
            )),
            NodeKind::PackageInstantiationDeclaration => {
                Some(DeclarationSyntax::PackageInstantiationDeclaration(
                    PackageInstantiationDeclarationSyntax::cast(node).unwrap(),
                ))
            }
            NodeKind::TypeDeclaration => Some(DeclarationSyntax::TypeDeclaration(
                TypeDeclarationSyntax::cast(node).unwrap(),
            )),
            NodeKind::SubtypeDeclaration => Some(DeclarationSyntax::SubtypeDeclaration(
                SubtypeDeclarationSyntax::cast(node).unwrap(),
            )),
            NodeKind::FileDeclaration => Some(DeclarationSyntax::FileDeclaration(
                FileDeclarationSyntax::cast(node).unwrap(),
            )),
            NodeKind::AliasDeclaration => Some(DeclarationSyntax::AliasDeclaration(
                AliasDeclarationSyntax::cast(node).unwrap(),
            )),
            NodeKind::ComponentDeclaration => Some(DeclarationSyntax::ComponentDeclaration(
                ComponentDeclarationSyntax::cast(node).unwrap(),
            )),
            NodeKind::AttributeDeclaration => Some(DeclarationSyntax::AttributeDeclaration(
                AttributeDeclarationSyntax::cast(node).unwrap(),
            )),
            NodeKind::AttributeSpecification => Some(DeclarationSyntax::AttributeSpecification(
                AttributeSpecificationSyntax::cast(node).unwrap(),
            )),
            NodeKind::ConfigurationSpecification => {
                Some(DeclarationSyntax::ConfigurationSpecification(
                    ConfigurationSpecificationSyntax::cast(node).unwrap(),
                ))
            }
            NodeKind::DisconnectionSpecification => {
                Some(DeclarationSyntax::DisconnectionSpecification(
                    DisconnectionSpecificationSyntax::cast(node).unwrap(),
                ))
            }
            NodeKind::UseClause => Some(DeclarationSyntax::UseClause(
                UseClauseSyntax::cast(node).unwrap(),
            )),
            NodeKind::GroupTemplateDeclaration => {
                Some(DeclarationSyntax::GroupTemplateDeclaration(
                    GroupTemplateDeclarationSyntax::cast(node).unwrap(),
                ))
            }
            NodeKind::GroupDeclaration => Some(DeclarationSyntax::GroupDeclaration(
                GroupDeclarationSyntax::cast(node).unwrap(),
            )),
            NodeKind::ConstantDeclaration => Some(DeclarationSyntax::ConstantDeclaration(
                ConstantDeclarationSyntax::cast(node).unwrap(),
            )),
            NodeKind::SignalDeclaration => Some(DeclarationSyntax::SignalDeclaration(
                SignalDeclarationSyntax::cast(node).unwrap(),
            )),
            NodeKind::VariableDeclaration => Some(DeclarationSyntax::VariableDeclaration(
                VariableDeclarationSyntax::cast(node).unwrap(),
            )),
            NodeKind::SharedVariableDeclaration => {
                Some(DeclarationSyntax::SharedVariableDeclaration(
                    SharedVariableDeclarationSyntax::cast(node).unwrap(),
                ))
            }
            NodeKind::PslPropertyDeclaration => Some(DeclarationSyntax::PslPropertyDeclaration(
                PslPropertyDeclarationSyntax::cast(node).unwrap(),
            )),
            NodeKind::PslSequenceDeclaration => Some(DeclarationSyntax::PslSequenceDeclaration(
                PslSequenceDeclarationSyntax::cast(node).unwrap(),
            )),
            NodeKind::PslClockDeclaration => Some(DeclarationSyntax::PslClockDeclaration(
                PslClockDeclarationSyntax::cast(node).unwrap(),
            )),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            DeclarationSyntax::SubprogramDeclaration(inner) => inner.raw(),
            DeclarationSyntax::SubprogramBody(inner) => inner.raw(),
            DeclarationSyntax::SubprogramInstantiationDeclaration(inner) => inner.raw(),
            DeclarationSyntax::PackageDeclaration(inner) => inner.raw(),
            DeclarationSyntax::PackageBody(inner) => inner.raw(),
            DeclarationSyntax::PackageInstantiationDeclaration(inner) => inner.raw(),
            DeclarationSyntax::TypeDeclaration(inner) => inner.raw(),
            DeclarationSyntax::SubtypeDeclaration(inner) => inner.raw(),
            DeclarationSyntax::FileDeclaration(inner) => inner.raw(),
            DeclarationSyntax::AliasDeclaration(inner) => inner.raw(),
            DeclarationSyntax::ComponentDeclaration(inner) => inner.raw(),
            DeclarationSyntax::AttributeDeclaration(inner) => inner.raw(),
            DeclarationSyntax::AttributeSpecification(inner) => inner.raw(),
            DeclarationSyntax::ConfigurationSpecification(inner) => inner.raw(),
            DeclarationSyntax::DisconnectionSpecification(inner) => inner.raw(),
            DeclarationSyntax::UseClause(inner) => inner.raw(),
            DeclarationSyntax::GroupTemplateDeclaration(inner) => inner.raw(),
            DeclarationSyntax::GroupDeclaration(inner) => inner.raw(),
            DeclarationSyntax::ConstantDeclaration(inner) => inner.raw(),
            DeclarationSyntax::SignalDeclaration(inner) => inner.raw(),
            DeclarationSyntax::VariableDeclaration(inner) => inner.raw(),
            DeclarationSyntax::SharedVariableDeclaration(inner) => inner.raw(),
            DeclarationSyntax::PslPropertyDeclaration(inner) => inner.raw(),
            DeclarationSyntax::PslSequenceDeclaration(inner) => inner.raw(),
            DeclarationSyntax::PslClockDeclaration(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct InertialExpressionSyntax(pub(crate) SyntaxNode);
impl AstNode for InertialExpressionSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::InertialExpression => Some(InertialExpressionSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InertialExpressionSyntax {
    pub fn inertial_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Inertial))
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum ActualDesignatorSyntax {
    InertialExpression(InertialExpressionSyntax),
    Name(NameSyntax),
    SubtypeIndication(SubtypeIndicationSyntax),
    OpenRange(OpenRangeSyntax),
}
impl AstNode for ActualDesignatorSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::InertialExpression => Some(ActualDesignatorSyntax::InertialExpression(
                InertialExpressionSyntax::cast(node).unwrap(),
            )),
            NodeKind::Name => Some(ActualDesignatorSyntax::Name(
                NameSyntax::cast(node).unwrap(),
            )),
            NodeKind::SubtypeIndication => Some(ActualDesignatorSyntax::SubtypeIndication(
                SubtypeIndicationSyntax::cast(node).unwrap(),
            )),
            NodeKind::OpenRange => Some(ActualDesignatorSyntax::OpenRange(
                OpenRangeSyntax::cast(node).unwrap(),
            )),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            ActualDesignatorSyntax::InertialExpression(inner) => inner.raw(),
            ActualDesignatorSyntax::Name(inner) => inner.raw(),
            ActualDesignatorSyntax::SubtypeIndication(inner) => inner.raw(),
            ActualDesignatorSyntax::OpenRange(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParenthesizedActualDesignatorSyntax(pub(crate) SyntaxNode);
impl AstNode for ParenthesizedActualDesignatorSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ParenthesizedActualDesignator => {
                Some(ParenthesizedActualDesignatorSyntax(node))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ParenthesizedActualDesignatorSyntax {
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == LeftPar)
            .nth(0)
    }
    pub fn actual_designator(&self) -> Option<ActualDesignatorSyntax> {
        self.0
            .children()
            .filter_map(ActualDesignatorSyntax::cast)
            .nth(0)
    }
    pub fn right_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == RightPar)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum ActualPartSyntax {
    Name(NameSyntax),
    ParenthesizedActualDesignator(ParenthesizedActualDesignatorSyntax),
}
impl AstNode for ActualPartSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::Name => Some(ActualPartSyntax::Name(NameSyntax::cast(node).unwrap())),
            NodeKind::ParenthesizedActualDesignator => {
                Some(ActualPartSyntax::ParenthesizedActualDesignator(
                    ParenthesizedActualDesignatorSyntax::cast(node).unwrap(),
                ))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            ActualPartSyntax::Name(inner) => inner.raw(),
            ActualPartSyntax::ParenthesizedActualDesignator(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct AliasDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for AliasDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::AliasDeclaration => Some(AliasDeclarationSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl AliasDeclarationSyntax {
    pub fn alias_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Alias))
            .nth(0)
    }
    pub fn alias_designator(&self) -> Option<AliasDesignatorSyntax> {
        self.0
            .tokens()
            .filter_map(AliasDesignatorSyntax::cast)
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
    pub fn is_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Is))
            .nth(0)
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
    pub fn signature(&self) -> Option<SignatureSyntax> {
        self.0.children().filter_map(SignatureSyntax::cast).nth(0)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum AliasDesignatorSyntax {
    Identifier(SyntaxToken),
    CharacterLiteral(SyntaxToken),
    StringLiteral(SyntaxToken),
}
impl AliasDesignatorSyntax {
    pub fn cast(token: SyntaxToken) -> Option<Self> {
        match token.kind() {
            Identifier => Some(AliasDesignatorSyntax::Identifier(token)),
            CharacterLiteral => Some(AliasDesignatorSyntax::CharacterLiteral(token)),
            StringLiteral => Some(AliasDesignatorSyntax::StringLiteral(token)),
            _ => None,
        }
    }
    pub fn raw(&self) -> SyntaxToken {
        match self {
            AliasDesignatorSyntax::Identifier(token) => token.clone(),
            AliasDesignatorSyntax::CharacterLiteral(token) => token.clone(),
            AliasDesignatorSyntax::StringLiteral(token) => token.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct AssociationElementSyntax(pub(crate) SyntaxNode);
impl AstNode for AssociationElementSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::AssociationElement => Some(AssociationElementSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl AssociationElementSyntax {
    pub fn formal_part(&self) -> Option<FormalPartSyntax> {
        self.0.children().filter_map(FormalPartSyntax::cast).nth(0)
    }
    pub fn right_arrow_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == RightArrow)
            .nth(0)
    }
    pub fn actual_part(&self) -> Option<ActualPartSyntax> {
        self.0.children().filter_map(ActualPartSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct AssociationListSyntax(pub(crate) SyntaxNode);
impl AstNode for AssociationListSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::AssociationList => Some(AssociationListSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl AssociationListSyntax {
    pub fn association_elements(&self) -> impl Iterator<Item = AssociationElementSyntax> + use<'_> {
        self.0.children().filter_map(AssociationElementSyntax::cast)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.tokens().filter(|token| token.kind() == Comma)
    }
}
#[derive(Debug, Clone)]
pub struct AttributeDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for AttributeDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::AttributeDeclaration => Some(AttributeDeclarationSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl AttributeDeclarationSyntax {
    pub fn attribute_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Attribute))
            .nth(0)
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Identifier)
            .nth(0)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == Colon).nth(0)
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ComponentDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for ComponentDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ComponentDeclaration => Some(ComponentDeclarationSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ComponentDeclarationSyntax {
    pub fn component_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Component))
            .nth(0)
    }
    pub fn name_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Identifier)
            .nth(0)
    }
    pub fn is_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Is))
            .nth(0)
    }
    pub fn generic_clause(&self) -> Option<GenericClauseSyntax> {
        self.0
            .children()
            .filter_map(GenericClauseSyntax::cast)
            .nth(0)
    }
    pub fn port_clause(&self) -> Option<PortClauseSyntax> {
        self.0.children().filter_map(PortClauseSyntax::cast).nth(0)
    }
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::End))
            .nth(0)
    }
    pub fn trailing_component_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Component))
            .nth(1)
    }
    pub fn trailing_name_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Identifier)
            .nth(1)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ConstantDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for ConstantDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ConstantDeclaration => Some(ConstantDeclarationSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConstantDeclarationSyntax {
    pub fn constant_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Constant))
            .nth(0)
    }
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
    pub fn colon_eq_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == ColonEq)
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum ConstraintSyntax {
    RangeConstraint(RangeConstraintSyntax),
    ArrayConstraint(ArrayConstraintSyntax),
    RecordConstraint(RecordConstraintSyntax),
}
impl AstNode for ConstraintSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::RangeConstraint => Some(ConstraintSyntax::RangeConstraint(
                RangeConstraintSyntax::cast(node).unwrap(),
            )),
            NodeKind::ArrayConstraint => Some(ConstraintSyntax::ArrayConstraint(
                ArrayConstraintSyntax::cast(node).unwrap(),
            )),
            NodeKind::RecordConstraint => Some(ConstraintSyntax::RecordConstraint(
                RecordConstraintSyntax::cast(node).unwrap(),
            )),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            ConstraintSyntax::RangeConstraint(inner) => inner.raw(),
            ConstraintSyntax::ArrayConstraint(inner) => inner.raw(),
            ConstraintSyntax::RecordConstraint(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ElementConstraintSyntax {
    ArrayConstraint(ArrayConstraintSyntax),
    RecordConstraint(RecordConstraintSyntax),
}
impl AstNode for ElementConstraintSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ArrayConstraint => Some(ElementConstraintSyntax::ArrayConstraint(
                ArrayConstraintSyntax::cast(node).unwrap(),
            )),
            NodeKind::RecordConstraint => Some(ElementConstraintSyntax::RecordConstraint(
                RecordConstraintSyntax::cast(node).unwrap(),
            )),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            ElementConstraintSyntax::ArrayConstraint(inner) => inner.raw(),
            ElementConstraintSyntax::RecordConstraint(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ElementResolutionSyntax {
    ResolutionIndication(ResolutionIndicationSyntax),
    RecordResolution(RecordResolutionSyntax),
}
impl AstNode for ElementResolutionSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ResolutionIndication => Some(ElementResolutionSyntax::ResolutionIndication(
                ResolutionIndicationSyntax::cast(node).unwrap(),
            )),
            NodeKind::RecordResolution => Some(ElementResolutionSyntax::RecordResolution(
                RecordResolutionSyntax::cast(node).unwrap(),
            )),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            ElementResolutionSyntax::ResolutionIndication(inner) => inner.raw(),
            ElementResolutionSyntax::RecordResolution(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct EntityClassEntrySyntax(pub(crate) SyntaxNode);
impl AstNode for EntityClassEntrySyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::EntityClassEntry => Some(EntityClassEntrySyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl EntityClassEntrySyntax {
    pub fn entity_class(&self) -> Option<EntityClassSyntax> {
        self.0.tokens().filter_map(EntityClassSyntax::cast).nth(0)
    }
    pub fn box_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == BOX).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct EntityClassEntryListSyntax(pub(crate) SyntaxNode);
impl AstNode for EntityClassEntryListSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::EntityClassEntryList => Some(EntityClassEntryListSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl EntityClassEntryListSyntax {
    pub fn entity_class_entrys(&self) -> impl Iterator<Item = EntityClassEntrySyntax> + use<'_> {
        self.0.children().filter_map(EntityClassEntrySyntax::cast)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.tokens().filter(|token| token.kind() == Comma)
    }
}
#[derive(Debug, Clone)]
pub struct FileDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for FileDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::FileDeclaration => Some(FileDeclarationSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl FileDeclarationSyntax {
    pub fn file_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::File))
            .nth(0)
    }
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
    pub fn file_open_information(&self) -> Option<FileOpenInformationSyntax> {
        self.0
            .children()
            .filter_map(FileOpenInformationSyntax::cast)
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
pub struct FileOpenInformationSyntax(pub(crate) SyntaxNode);
impl AstNode for FileOpenInformationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::FileOpenInformation => Some(FileOpenInformationSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl FileOpenInformationSyntax {
    pub fn open_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Open))
            .nth(0)
    }
    pub fn file_open_kind(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn is_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Is))
            .nth(0)
    }
    pub fn file_logical_name(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(1)
    }
}
#[derive(Debug, Clone)]
pub struct ParenthesizedNameSyntax(pub(crate) SyntaxNode);
impl AstNode for ParenthesizedNameSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ParenthesizedName => Some(ParenthesizedNameSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ParenthesizedNameSyntax {
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == LeftPar)
            .nth(0)
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
    pub fn right_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == RightPar)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct FormalPartSyntax(pub(crate) SyntaxNode);
impl AstNode for FormalPartSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::FormalPart => Some(FormalPartSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl FormalPartSyntax {
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
    pub fn parenthesized_name(&self) -> Option<ParenthesizedNameSyntax> {
        self.0
            .children()
            .filter_map(ParenthesizedNameSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct FullTypeDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for FullTypeDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::FullTypeDeclaration => Some(FullTypeDeclarationSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl FullTypeDeclarationSyntax {
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
    pub fn is_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Is))
            .nth(0)
    }
    pub fn type_definition(&self) -> Option<TypeDefinitionSyntax> {
        self.0
            .children()
            .filter_map(TypeDefinitionSyntax::cast)
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
pub struct GenericClauseSyntax(pub(crate) SyntaxNode);
impl AstNode for GenericClauseSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::GenericClause => Some(GenericClauseSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl GenericClauseSyntax {
    pub fn generic_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Generic))
            .nth(0)
    }
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == LeftPar)
            .nth(0)
    }
    pub fn interface_list(&self) -> Option<InterfaceListSyntax> {
        self.0
            .children()
            .filter_map(InterfaceListSyntax::cast)
            .nth(0)
    }
    pub fn right_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == RightPar)
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
pub struct GenericMapAspectSyntax(pub(crate) SyntaxNode);
impl AstNode for GenericMapAspectSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::GenericMapAspect => Some(GenericMapAspectSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl GenericMapAspectSyntax {
    pub fn generic_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Generic))
            .nth(0)
    }
    pub fn map_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Map))
            .nth(0)
    }
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == LeftPar)
            .nth(0)
    }
    pub fn association_list(&self) -> Option<AssociationListSyntax> {
        self.0
            .children()
            .filter_map(AssociationListSyntax::cast)
            .nth(0)
    }
    pub fn right_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == RightPar)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct GroupConstituentListSyntax(pub(crate) SyntaxNode);
impl AstNode for GroupConstituentListSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::GroupConstituentList => Some(GroupConstituentListSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl GroupConstituentListSyntax {
    pub fn names(&self) -> impl Iterator<Item = NameSyntax> + use<'_> {
        self.0.children().filter_map(NameSyntax::cast)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.tokens().filter(|token| token.kind() == Comma)
    }
}
#[derive(Debug, Clone)]
pub struct GroupDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for GroupDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::GroupDeclaration => Some(GroupDeclarationSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl GroupDeclarationSyntax {
    pub fn group_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Group))
            .nth(0)
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Identifier)
            .nth(0)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == Colon).nth(0)
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == LeftPar)
            .nth(0)
    }
    pub fn group_constituent_list(&self) -> Option<GroupConstituentListSyntax> {
        self.0
            .children()
            .filter_map(GroupConstituentListSyntax::cast)
            .nth(0)
    }
    pub fn right_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == RightPar)
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
pub struct GroupTemplateDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for GroupTemplateDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::GroupTemplateDeclaration => Some(GroupTemplateDeclarationSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl GroupTemplateDeclarationSyntax {
    pub fn group_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Group))
            .nth(0)
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Identifier)
            .nth(0)
    }
    pub fn is_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Is))
            .nth(0)
    }
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == LeftPar)
            .nth(0)
    }
    pub fn entity_class_entry_list(&self) -> Option<EntityClassEntryListSyntax> {
        self.0
            .children()
            .filter_map(EntityClassEntryListSyntax::cast)
            .nth(0)
    }
    pub fn right_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == RightPar)
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
pub struct InterfaceConstantDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfaceConstantDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::InterfaceConstantDeclaration => {
                Some(InterfaceConstantDeclarationSyntax(node))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfaceConstantDeclarationSyntax {
    pub fn constant_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Constant))
            .nth(0)
    }
    pub fn identifier_list(&self) -> Option<IdentifierListSyntax> {
        self.0
            .children()
            .filter_map(IdentifierListSyntax::cast)
            .nth(0)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == Colon).nth(0)
    }
    pub fn in_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::In))
            .nth(0)
    }
    pub fn subtype_indication(&self) -> Option<SubtypeIndicationSyntax> {
        self.0
            .children()
            .filter_map(SubtypeIndicationSyntax::cast)
            .nth(0)
    }
    pub fn colon_eq_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == ColonEq)
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum InterfaceDeclarationSyntax {
    InterfaceObjectDeclaration(InterfaceObjectDeclarationSyntax),
    InterfaceIncompleteTypeDeclaration(InterfaceIncompleteTypeDeclarationSyntax),
    InterfaceSubprogramDeclaration(InterfaceSubprogramDeclarationSyntax),
    InterfacePackageDeclaration(InterfacePackageDeclarationSyntax),
}
impl AstNode for InterfaceDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::InterfaceObjectDeclaration => {
                Some(InterfaceDeclarationSyntax::InterfaceObjectDeclaration(
                    InterfaceObjectDeclarationSyntax::cast(node).unwrap(),
                ))
            }
            NodeKind::InterfaceIncompleteTypeDeclaration => Some(
                InterfaceDeclarationSyntax::InterfaceIncompleteTypeDeclaration(
                    InterfaceIncompleteTypeDeclarationSyntax::cast(node).unwrap(),
                ),
            ),
            NodeKind::InterfaceSubprogramDeclaration => {
                Some(InterfaceDeclarationSyntax::InterfaceSubprogramDeclaration(
                    InterfaceSubprogramDeclarationSyntax::cast(node).unwrap(),
                ))
            }
            NodeKind::InterfacePackageDeclaration => {
                Some(InterfaceDeclarationSyntax::InterfacePackageDeclaration(
                    InterfacePackageDeclarationSyntax::cast(node).unwrap(),
                ))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            InterfaceDeclarationSyntax::InterfaceObjectDeclaration(inner) => inner.raw(),
            InterfaceDeclarationSyntax::InterfaceIncompleteTypeDeclaration(inner) => inner.raw(),
            InterfaceDeclarationSyntax::InterfaceSubprogramDeclaration(inner) => inner.raw(),
            InterfaceDeclarationSyntax::InterfacePackageDeclaration(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct InterfaceFileDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfaceFileDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::InterfaceFileDeclaration => Some(InterfaceFileDeclarationSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfaceFileDeclarationSyntax {
    pub fn file_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::File))
            .nth(0)
    }
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
}
#[derive(Debug, Clone)]
pub struct InterfaceFunctionSpecificationSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfaceFunctionSpecificationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::InterfaceFunctionSpecification => {
                Some(InterfaceFunctionSpecificationSyntax(node))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfaceFunctionSpecificationSyntax {
    pub fn function_purity(&self) -> Option<FunctionPuritySyntax> {
        self.0
            .tokens()
            .filter_map(FunctionPuritySyntax::cast)
            .nth(0)
    }
    pub fn function_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Function))
            .nth(0)
    }
    pub fn designator(&self) -> Option<DesignatorSyntax> {
        self.0.tokens().filter_map(DesignatorSyntax::cast).nth(0)
    }
    pub fn parameter_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Parameter))
            .nth(0)
    }
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == LeftPar)
            .nth(0)
    }
    pub fn interface_list(&self) -> Option<InterfaceListSyntax> {
        self.0
            .children()
            .filter_map(InterfaceListSyntax::cast)
            .nth(0)
    }
    pub fn return_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Return))
            .nth(0)
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct InterfaceIncompleteTypeDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfaceIncompleteTypeDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::InterfaceIncompleteTypeDeclaration => {
                Some(InterfaceIncompleteTypeDeclarationSyntax(node))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfaceIncompleteTypeDeclarationSyntax {
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
pub struct InterfaceListSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfaceListSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::InterfaceList => Some(InterfaceListSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfaceListSyntax {
    pub fn interface_declarations(
        &self,
    ) -> impl Iterator<Item = InterfaceDeclarationSyntax> + use<'_> {
        self.0
            .children()
            .filter_map(InterfaceDeclarationSyntax::cast)
    }
    pub fn semi_colon_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.tokens().filter(|token| token.kind() == SemiColon)
    }
}
#[derive(Debug, Clone)]
pub enum InterfaceObjectDeclarationSyntax {
    InterfaceConstantDeclaration(InterfaceConstantDeclarationSyntax),
    InterfaceSignalDeclaration(InterfaceSignalDeclarationSyntax),
    InterfaceVariableDeclaration(InterfaceVariableDeclarationSyntax),
    InterfaceFileDeclaration(InterfaceFileDeclarationSyntax),
}
impl AstNode for InterfaceObjectDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::InterfaceConstantDeclaration => Some(
                InterfaceObjectDeclarationSyntax::InterfaceConstantDeclaration(
                    InterfaceConstantDeclarationSyntax::cast(node).unwrap(),
                ),
            ),
            NodeKind::InterfaceSignalDeclaration => Some(
                InterfaceObjectDeclarationSyntax::InterfaceSignalDeclaration(
                    InterfaceSignalDeclarationSyntax::cast(node).unwrap(),
                ),
            ),
            NodeKind::InterfaceVariableDeclaration => Some(
                InterfaceObjectDeclarationSyntax::InterfaceVariableDeclaration(
                    InterfaceVariableDeclarationSyntax::cast(node).unwrap(),
                ),
            ),
            NodeKind::InterfaceFileDeclaration => {
                Some(InterfaceObjectDeclarationSyntax::InterfaceFileDeclaration(
                    InterfaceFileDeclarationSyntax::cast(node).unwrap(),
                ))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            InterfaceObjectDeclarationSyntax::InterfaceConstantDeclaration(inner) => inner.raw(),
            InterfaceObjectDeclarationSyntax::InterfaceSignalDeclaration(inner) => inner.raw(),
            InterfaceObjectDeclarationSyntax::InterfaceVariableDeclaration(inner) => inner.raw(),
            InterfaceObjectDeclarationSyntax::InterfaceFileDeclaration(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct InterfacePackageDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfacePackageDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::InterfacePackageDeclaration => Some(InterfacePackageDeclarationSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfacePackageDeclarationSyntax {
    pub fn package_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Package))
            .nth(0)
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Identifier)
            .nth(0)
    }
    pub fn is_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Is))
            .nth(0)
    }
    pub fn new_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::New))
            .nth(0)
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
    pub fn interface_package_generic_map_aspect(
        &self,
    ) -> Option<InterfacePackageGenericMapAspectSyntax> {
        self.0
            .children()
            .filter_map(InterfacePackageGenericMapAspectSyntax::cast)
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
pub struct InterfacePackageGenericMapAspectSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfacePackageGenericMapAspectSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::InterfacePackageGenericMapAspect => {
                Some(InterfacePackageGenericMapAspectSyntax(node))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfacePackageGenericMapAspectSyntax {
    pub fn generic_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Generic))
            .nth(0)
    }
    pub fn map_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Map))
            .nth(0)
    }
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == LeftPar)
            .nth(0)
    }
    pub fn interface_package_generic_map_aspect_inner(
        &self,
    ) -> Option<InterfacePackageGenericMapAspectInnerSyntax> {
        self.0
            .children()
            .filter_map(InterfacePackageGenericMapAspectInnerSyntax::cast)
            .nth(0)
    }
    pub fn right_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == RightPar)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct BoxTokenSyntax(pub(crate) SyntaxNode);
impl AstNode for BoxTokenSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::BoxToken => Some(BoxTokenSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl BoxTokenSyntax {
    pub fn box_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == BOX).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct DefaultTokenSyntax(pub(crate) SyntaxNode);
impl AstNode for DefaultTokenSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::DefaultToken => Some(DefaultTokenSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl DefaultTokenSyntax {
    pub fn default_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Default))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum InterfacePackageGenericMapAspectInnerSyntax {
    BoxToken(BoxTokenSyntax),
    DefaultToken(DefaultTokenSyntax),
    AssociationList(AssociationListSyntax),
}
impl AstNode for InterfacePackageGenericMapAspectInnerSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::BoxToken => Some(InterfacePackageGenericMapAspectInnerSyntax::BoxToken(
                BoxTokenSyntax::cast(node).unwrap(),
            )),
            NodeKind::DefaultToken => {
                Some(InterfacePackageGenericMapAspectInnerSyntax::DefaultToken(
                    DefaultTokenSyntax::cast(node).unwrap(),
                ))
            }
            NodeKind::AssociationList => Some(
                InterfacePackageGenericMapAspectInnerSyntax::AssociationList(
                    AssociationListSyntax::cast(node).unwrap(),
                ),
            ),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            InterfacePackageGenericMapAspectInnerSyntax::BoxToken(inner) => inner.raw(),
            InterfacePackageGenericMapAspectInnerSyntax::DefaultToken(inner) => inner.raw(),
            InterfacePackageGenericMapAspectInnerSyntax::AssociationList(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct InterfaceProcedureSpecificationSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfaceProcedureSpecificationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::InterfaceProcedureSpecification => {
                Some(InterfaceProcedureSpecificationSyntax(node))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfaceProcedureSpecificationSyntax {
    pub fn procedure_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Procedure))
            .nth(0)
    }
    pub fn designator(&self) -> Option<DesignatorSyntax> {
        self.0.tokens().filter_map(DesignatorSyntax::cast).nth(0)
    }
    pub fn parameter_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Parameter))
            .nth(0)
    }
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == LeftPar)
            .nth(0)
    }
    pub fn interface_list(&self) -> Option<InterfaceListSyntax> {
        self.0
            .children()
            .filter_map(InterfaceListSyntax::cast)
            .nth(0)
    }
    pub fn right_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == RightPar)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct InterfaceSignalDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfaceSignalDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::InterfaceSignalDeclaration => Some(InterfaceSignalDeclarationSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfaceSignalDeclarationSyntax {
    pub fn signal_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Signal))
            .nth(0)
    }
    pub fn identifier_list(&self) -> Option<IdentifierListSyntax> {
        self.0
            .children()
            .filter_map(IdentifierListSyntax::cast)
            .nth(0)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == Colon).nth(0)
    }
    pub fn mode(&self) -> Option<ModeSyntax> {
        self.0.tokens().filter_map(ModeSyntax::cast).nth(0)
    }
    pub fn subtype_indication(&self) -> Option<SubtypeIndicationSyntax> {
        self.0
            .children()
            .filter_map(SubtypeIndicationSyntax::cast)
            .nth(0)
    }
    pub fn bus_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Bus))
            .nth(0)
    }
    pub fn colon_eq_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == ColonEq)
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct InterfaceSubprogramDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfaceSubprogramDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::InterfaceSubprogramDeclaration => {
                Some(InterfaceSubprogramDeclarationSyntax(node))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfaceSubprogramDeclarationSyntax {
    pub fn interface_subprogram_specification(
        &self,
    ) -> Option<InterfaceSubprogramSpecificationSyntax> {
        self.0
            .children()
            .filter_map(InterfaceSubprogramSpecificationSyntax::cast)
            .nth(0)
    }
    pub fn is_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Is))
            .nth(0)
    }
    pub fn interface_subprogram_default(&self) -> Option<InterfaceSubprogramDefaultSyntax> {
        self.0
            .children()
            .filter_map(InterfaceSubprogramDefaultSyntax::cast)
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
pub enum InterfaceSubprogramDefaultSyntax {
    Name(NameSyntax),
    BoxToken(BoxTokenSyntax),
}
impl AstNode for InterfaceSubprogramDefaultSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::Name => Some(InterfaceSubprogramDefaultSyntax::Name(
                NameSyntax::cast(node).unwrap(),
            )),
            NodeKind::BoxToken => Some(InterfaceSubprogramDefaultSyntax::BoxToken(
                BoxTokenSyntax::cast(node).unwrap(),
            )),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            InterfaceSubprogramDefaultSyntax::Name(inner) => inner.raw(),
            InterfaceSubprogramDefaultSyntax::BoxToken(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum InterfaceSubprogramSpecificationSyntax {
    InterfaceProcedureSpecification(InterfaceProcedureSpecificationSyntax),
    InterfaceFunctionSpecification(InterfaceFunctionSpecificationSyntax),
}
impl AstNode for InterfaceSubprogramSpecificationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::InterfaceProcedureSpecification => Some(
                InterfaceSubprogramSpecificationSyntax::InterfaceProcedureSpecification(
                    InterfaceProcedureSpecificationSyntax::cast(node).unwrap(),
                ),
            ),
            NodeKind::InterfaceFunctionSpecification => Some(
                InterfaceSubprogramSpecificationSyntax::InterfaceFunctionSpecification(
                    InterfaceFunctionSpecificationSyntax::cast(node).unwrap(),
                ),
            ),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            InterfaceSubprogramSpecificationSyntax::InterfaceProcedureSpecification(inner) => {
                inner.raw()
            }
            InterfaceSubprogramSpecificationSyntax::InterfaceFunctionSpecification(inner) => {
                inner.raw()
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct InterfaceVariableDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfaceVariableDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::InterfaceVariableDeclaration => {
                Some(InterfaceVariableDeclarationSyntax(node))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfaceVariableDeclarationSyntax {
    pub fn variable_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Variable))
            .nth(0)
    }
    pub fn identifier_list(&self) -> Option<IdentifierListSyntax> {
        self.0
            .children()
            .filter_map(IdentifierListSyntax::cast)
            .nth(0)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == Colon).nth(0)
    }
    pub fn mode(&self) -> Option<ModeSyntax> {
        self.0.tokens().filter_map(ModeSyntax::cast).nth(0)
    }
    pub fn subtype_indication(&self) -> Option<SubtypeIndicationSyntax> {
        self.0
            .children()
            .filter_map(SubtypeIndicationSyntax::cast)
            .nth(0)
    }
    pub fn colon_eq_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == ColonEq)
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum ModeSyntax {
    In(SyntaxToken),
    Out(SyntaxToken),
    Inout(SyntaxToken),
    Buffer(SyntaxToken),
    Linkage(SyntaxToken),
}
impl ModeSyntax {
    pub fn cast(token: SyntaxToken) -> Option<Self> {
        match token.kind() {
            Keyword(Kw::In) => Some(ModeSyntax::In(token)),
            Keyword(Kw::Out) => Some(ModeSyntax::Out(token)),
            Keyword(Kw::Inout) => Some(ModeSyntax::Inout(token)),
            Keyword(Kw::Buffer) => Some(ModeSyntax::Buffer(token)),
            Keyword(Kw::Linkage) => Some(ModeSyntax::Linkage(token)),
            _ => None,
        }
    }
    pub fn raw(&self) -> SyntaxToken {
        match self {
            ModeSyntax::In(token) => token.clone(),
            ModeSyntax::Out(token) => token.clone(),
            ModeSyntax::Inout(token) => token.clone(),
            ModeSyntax::Buffer(token) => token.clone(),
            ModeSyntax::Linkage(token) => token.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct PortClauseSyntax(pub(crate) SyntaxNode);
impl AstNode for PortClauseSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::PortClause => Some(PortClauseSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PortClauseSyntax {
    pub fn port_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Port))
            .nth(0)
    }
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == LeftPar)
            .nth(0)
    }
    pub fn interface_list(&self) -> Option<InterfaceListSyntax> {
        self.0
            .children()
            .filter_map(InterfaceListSyntax::cast)
            .nth(0)
    }
    pub fn right_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == RightPar)
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
pub struct PortMapAspectSyntax(pub(crate) SyntaxNode);
impl AstNode for PortMapAspectSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::PortMapAspect => Some(PortMapAspectSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PortMapAspectSyntax {
    pub fn port_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Port))
            .nth(0)
    }
    pub fn map_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Map))
            .nth(0)
    }
    pub fn interface_list(&self) -> Option<InterfaceListSyntax> {
        self.0
            .children()
            .filter_map(InterfaceListSyntax::cast)
            .nth(0)
    }
    pub fn association_list(&self) -> Option<AssociationListSyntax> {
        self.0
            .children()
            .filter_map(AssociationListSyntax::cast)
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
pub struct RecordElementResolutionSyntax(pub(crate) SyntaxNode);
impl AstNode for RecordElementResolutionSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::RecordElementResolution => Some(RecordElementResolutionSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl RecordElementResolutionSyntax {
    pub fn name_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Identifier)
            .nth(0)
    }
    pub fn resolution_indication(&self) -> Option<ResolutionIndicationSyntax> {
        self.0
            .children()
            .filter_map(ResolutionIndicationSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct RecordResolutionSyntax(pub(crate) SyntaxNode);
impl AstNode for RecordResolutionSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::RecordResolution => Some(RecordResolutionSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl RecordResolutionSyntax {
    pub fn record_element_resolutions(
        &self,
    ) -> impl Iterator<Item = RecordElementResolutionSyntax> + use<'_> {
        self.0
            .children()
            .filter_map(RecordElementResolutionSyntax::cast)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.tokens().filter(|token| token.kind() == Comma)
    }
}
#[derive(Debug, Clone)]
pub struct ParenthesizedElementResolutionSyntax(pub(crate) SyntaxNode);
impl AstNode for ParenthesizedElementResolutionSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ParenthesizedElementResolution => {
                Some(ParenthesizedElementResolutionSyntax(node))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ParenthesizedElementResolutionSyntax {
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == LeftPar)
            .nth(0)
    }
    pub fn element_resolution(&self) -> Option<ElementResolutionSyntax> {
        self.0
            .children()
            .filter_map(ElementResolutionSyntax::cast)
            .nth(0)
    }
    pub fn right_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == RightPar)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum ResolutionIndicationSyntax {
    Name(NameSyntax),
    ParenthesizedElementResolution(ParenthesizedElementResolutionSyntax),
}
impl AstNode for ResolutionIndicationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::Name => Some(ResolutionIndicationSyntax::Name(
                NameSyntax::cast(node).unwrap(),
            )),
            NodeKind::ParenthesizedElementResolution => {
                Some(ResolutionIndicationSyntax::ParenthesizedElementResolution(
                    ParenthesizedElementResolutionSyntax::cast(node).unwrap(),
                ))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            ResolutionIndicationSyntax::Name(inner) => inner.raw(),
            ResolutionIndicationSyntax::ParenthesizedElementResolution(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SignalDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for SignalDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SignalDeclaration => Some(SignalDeclarationSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SignalDeclarationSyntax {
    pub fn signal_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Signal))
            .nth(0)
    }
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
    pub fn signal_kind(&self) -> Option<SignalKindSyntax> {
        self.0.tokens().filter_map(SignalKindSyntax::cast).nth(0)
    }
    pub fn colon_eq_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == ColonEq)
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum SignalKindSyntax {
    Register(SyntaxToken),
    Bus(SyntaxToken),
}
impl SignalKindSyntax {
    pub fn cast(token: SyntaxToken) -> Option<Self> {
        match token.kind() {
            Keyword(Kw::Register) => Some(SignalKindSyntax::Register(token)),
            Keyword(Kw::Bus) => Some(SignalKindSyntax::Bus(token)),
            _ => None,
        }
    }
    pub fn raw(&self) -> SyntaxToken {
        match self {
            SignalKindSyntax::Register(token) => token.clone(),
            SignalKindSyntax::Bus(token) => token.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SubtypeDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for SubtypeDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SubtypeDeclaration => Some(SubtypeDeclarationSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SubtypeDeclarationSyntax {
    pub fn subtype_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Subtype))
            .nth(0)
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Identifier)
            .nth(0)
    }
    pub fn is_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Is))
            .nth(0)
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
pub struct SubtypeIndicationSyntax(pub(crate) SyntaxNode);
impl AstNode for SubtypeIndicationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SubtypeIndication => Some(SubtypeIndicationSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SubtypeIndicationSyntax {
    pub fn resolution_indication(&self) -> Option<ResolutionIndicationSyntax> {
        self.0
            .children()
            .filter_map(ResolutionIndicationSyntax::cast)
            .nth(0)
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
    pub fn constraint(&self) -> Option<ConstraintSyntax> {
        self.0.children().filter_map(ConstraintSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum TypeDeclarationSyntax {
    FullTypeDeclaration(FullTypeDeclarationSyntax),
    IncompleteTypeDeclaration(IncompleteTypeDeclarationSyntax),
}
impl AstNode for TypeDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::FullTypeDeclaration => Some(TypeDeclarationSyntax::FullTypeDeclaration(
                FullTypeDeclarationSyntax::cast(node).unwrap(),
            )),
            NodeKind::IncompleteTypeDeclaration => {
                Some(TypeDeclarationSyntax::IncompleteTypeDeclaration(
                    IncompleteTypeDeclarationSyntax::cast(node).unwrap(),
                ))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            TypeDeclarationSyntax::FullTypeDeclaration(inner) => inner.raw(),
            TypeDeclarationSyntax::IncompleteTypeDeclaration(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeDefinitionSyntax {
    ScalarTypeDefinition(ScalarTypeDefinitionSyntax),
    CompositeTypeDefinition(CompositeTypeDefinitionSyntax),
    AccessTypeDefinition(AccessTypeDefinitionSyntax),
    FileTypeDefinition(FileTypeDefinitionSyntax),
    ProtectedTypeDefinition(ProtectedTypeDefinitionSyntax),
}
impl AstNode for TypeDefinitionSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ScalarTypeDefinition => Some(TypeDefinitionSyntax::ScalarTypeDefinition(
                ScalarTypeDefinitionSyntax::cast(node).unwrap(),
            )),
            NodeKind::CompositeTypeDefinition => {
                Some(TypeDefinitionSyntax::CompositeTypeDefinition(
                    CompositeTypeDefinitionSyntax::cast(node).unwrap(),
                ))
            }
            NodeKind::AccessTypeDefinition => Some(TypeDefinitionSyntax::AccessTypeDefinition(
                AccessTypeDefinitionSyntax::cast(node).unwrap(),
            )),
            NodeKind::FileTypeDefinition => Some(TypeDefinitionSyntax::FileTypeDefinition(
                FileTypeDefinitionSyntax::cast(node).unwrap(),
            )),
            NodeKind::ProtectedTypeDefinition => {
                Some(TypeDefinitionSyntax::ProtectedTypeDefinition(
                    ProtectedTypeDefinitionSyntax::cast(node).unwrap(),
                ))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            TypeDefinitionSyntax::ScalarTypeDefinition(inner) => inner.raw(),
            TypeDefinitionSyntax::CompositeTypeDefinition(inner) => inner.raw(),
            TypeDefinitionSyntax::AccessTypeDefinition(inner) => inner.raw(),
            TypeDefinitionSyntax::FileTypeDefinition(inner) => inner.raw(),
            TypeDefinitionSyntax::ProtectedTypeDefinition(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct VariableDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for VariableDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::VariableDeclaration => Some(VariableDeclarationSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl VariableDeclarationSyntax {
    pub fn variable_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Variable))
            .nth(0)
    }
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
    pub fn colon_eq_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == ColonEq)
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct SharedVariableDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for SharedVariableDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SharedVariableDeclaration => Some(SharedVariableDeclarationSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SharedVariableDeclarationSyntax {
    pub fn shared_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Shared))
            .nth(0)
    }
    pub fn variable_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Variable))
            .nth(0)
    }
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
    pub fn colon_eq_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == ColonEq)
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == SemiColon)
            .nth(0)
    }
}
