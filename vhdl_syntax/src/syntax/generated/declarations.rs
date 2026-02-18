// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2026, Lukas Scheller lukasscheller@icloud.com
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
    PackageBodyDeclaration(PackageBodyDeclarationSyntax),
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
    UseClauseDeclaration(UseClauseDeclarationSyntax),
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
        if SubprogramDeclarationSyntax::can_cast(&node) {
            return Some(DeclarationSyntax::SubprogramDeclaration(
                SubprogramDeclarationSyntax::cast(node).unwrap(),
            ));
        };
        if SubprogramBodySyntax::can_cast(&node) {
            return Some(DeclarationSyntax::SubprogramBody(
                SubprogramBodySyntax::cast(node).unwrap(),
            ));
        };
        if SubprogramInstantiationDeclarationSyntax::can_cast(&node) {
            return Some(DeclarationSyntax::SubprogramInstantiationDeclaration(
                SubprogramInstantiationDeclarationSyntax::cast(node).unwrap(),
            ));
        };
        if PackageDeclarationSyntax::can_cast(&node) {
            return Some(DeclarationSyntax::PackageDeclaration(
                PackageDeclarationSyntax::cast(node).unwrap(),
            ));
        };
        if PackageBodyDeclarationSyntax::can_cast(&node) {
            return Some(DeclarationSyntax::PackageBodyDeclaration(
                PackageBodyDeclarationSyntax::cast(node).unwrap(),
            ));
        };
        if PackageInstantiationDeclarationSyntax::can_cast(&node) {
            return Some(DeclarationSyntax::PackageInstantiationDeclaration(
                PackageInstantiationDeclarationSyntax::cast(node).unwrap(),
            ));
        };
        if TypeDeclarationSyntax::can_cast(&node) {
            return Some(DeclarationSyntax::TypeDeclaration(
                TypeDeclarationSyntax::cast(node).unwrap(),
            ));
        };
        if SubtypeDeclarationSyntax::can_cast(&node) {
            return Some(DeclarationSyntax::SubtypeDeclaration(
                SubtypeDeclarationSyntax::cast(node).unwrap(),
            ));
        };
        if FileDeclarationSyntax::can_cast(&node) {
            return Some(DeclarationSyntax::FileDeclaration(
                FileDeclarationSyntax::cast(node).unwrap(),
            ));
        };
        if AliasDeclarationSyntax::can_cast(&node) {
            return Some(DeclarationSyntax::AliasDeclaration(
                AliasDeclarationSyntax::cast(node).unwrap(),
            ));
        };
        if ComponentDeclarationSyntax::can_cast(&node) {
            return Some(DeclarationSyntax::ComponentDeclaration(
                ComponentDeclarationSyntax::cast(node).unwrap(),
            ));
        };
        if AttributeDeclarationSyntax::can_cast(&node) {
            return Some(DeclarationSyntax::AttributeDeclaration(
                AttributeDeclarationSyntax::cast(node).unwrap(),
            ));
        };
        if AttributeSpecificationSyntax::can_cast(&node) {
            return Some(DeclarationSyntax::AttributeSpecification(
                AttributeSpecificationSyntax::cast(node).unwrap(),
            ));
        };
        if ConfigurationSpecificationSyntax::can_cast(&node) {
            return Some(DeclarationSyntax::ConfigurationSpecification(
                ConfigurationSpecificationSyntax::cast(node).unwrap(),
            ));
        };
        if DisconnectionSpecificationSyntax::can_cast(&node) {
            return Some(DeclarationSyntax::DisconnectionSpecification(
                DisconnectionSpecificationSyntax::cast(node).unwrap(),
            ));
        };
        if UseClauseDeclarationSyntax::can_cast(&node) {
            return Some(DeclarationSyntax::UseClauseDeclaration(
                UseClauseDeclarationSyntax::cast(node).unwrap(),
            ));
        };
        if GroupTemplateDeclarationSyntax::can_cast(&node) {
            return Some(DeclarationSyntax::GroupTemplateDeclaration(
                GroupTemplateDeclarationSyntax::cast(node).unwrap(),
            ));
        };
        if GroupDeclarationSyntax::can_cast(&node) {
            return Some(DeclarationSyntax::GroupDeclaration(
                GroupDeclarationSyntax::cast(node).unwrap(),
            ));
        };
        if ConstantDeclarationSyntax::can_cast(&node) {
            return Some(DeclarationSyntax::ConstantDeclaration(
                ConstantDeclarationSyntax::cast(node).unwrap(),
            ));
        };
        if SignalDeclarationSyntax::can_cast(&node) {
            return Some(DeclarationSyntax::SignalDeclaration(
                SignalDeclarationSyntax::cast(node).unwrap(),
            ));
        };
        if VariableDeclarationSyntax::can_cast(&node) {
            return Some(DeclarationSyntax::VariableDeclaration(
                VariableDeclarationSyntax::cast(node).unwrap(),
            ));
        };
        if SharedVariableDeclarationSyntax::can_cast(&node) {
            return Some(DeclarationSyntax::SharedVariableDeclaration(
                SharedVariableDeclarationSyntax::cast(node).unwrap(),
            ));
        };
        if PslPropertyDeclarationSyntax::can_cast(&node) {
            return Some(DeclarationSyntax::PslPropertyDeclaration(
                PslPropertyDeclarationSyntax::cast(node).unwrap(),
            ));
        };
        if PslSequenceDeclarationSyntax::can_cast(&node) {
            return Some(DeclarationSyntax::PslSequenceDeclaration(
                PslSequenceDeclarationSyntax::cast(node).unwrap(),
            ));
        };
        if PslClockDeclarationSyntax::can_cast(&node) {
            return Some(DeclarationSyntax::PslClockDeclaration(
                PslClockDeclarationSyntax::cast(node).unwrap(),
            ));
        };
        None
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        SubprogramDeclarationSyntax::can_cast(node)
            || SubprogramBodySyntax::can_cast(node)
            || SubprogramInstantiationDeclarationSyntax::can_cast(node)
            || PackageDeclarationSyntax::can_cast(node)
            || PackageBodyDeclarationSyntax::can_cast(node)
            || PackageInstantiationDeclarationSyntax::can_cast(node)
            || TypeDeclarationSyntax::can_cast(node)
            || SubtypeDeclarationSyntax::can_cast(node)
            || FileDeclarationSyntax::can_cast(node)
            || AliasDeclarationSyntax::can_cast(node)
            || ComponentDeclarationSyntax::can_cast(node)
            || AttributeDeclarationSyntax::can_cast(node)
            || AttributeSpecificationSyntax::can_cast(node)
            || ConfigurationSpecificationSyntax::can_cast(node)
            || DisconnectionSpecificationSyntax::can_cast(node)
            || UseClauseDeclarationSyntax::can_cast(node)
            || GroupTemplateDeclarationSyntax::can_cast(node)
            || GroupDeclarationSyntax::can_cast(node)
            || ConstantDeclarationSyntax::can_cast(node)
            || SignalDeclarationSyntax::can_cast(node)
            || VariableDeclarationSyntax::can_cast(node)
            || SharedVariableDeclarationSyntax::can_cast(node)
            || PslPropertyDeclarationSyntax::can_cast(node)
            || PslSequenceDeclarationSyntax::can_cast(node)
            || PslClockDeclarationSyntax::can_cast(node)
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            DeclarationSyntax::SubprogramDeclaration(inner) => inner.raw(),
            DeclarationSyntax::SubprogramBody(inner) => inner.raw(),
            DeclarationSyntax::SubprogramInstantiationDeclaration(inner) => inner.raw(),
            DeclarationSyntax::PackageDeclaration(inner) => inner.raw(),
            DeclarationSyntax::PackageBodyDeclaration(inner) => inner.raw(),
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
            DeclarationSyntax::UseClauseDeclaration(inner) => inner.raw(),
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
pub struct PackageDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for PackageDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::PackageDeclaration => Some(PackageDeclarationSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::PackageDeclaration)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PackageDeclarationSyntax {
    pub fn package(&self) -> Option<PackageSyntax> {
        self.0.children().filter_map(PackageSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct PackageInstantiationDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for PackageInstantiationDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::PackageInstantiationDeclaration => {
                Some(PackageInstantiationDeclarationSyntax(node))
            }
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::PackageInstantiationDeclaration)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PackageInstantiationDeclarationSyntax {
    pub fn package_instantiation(&self) -> Option<PackageInstantiationSyntax> {
        self.0
            .children()
            .filter_map(PackageInstantiationSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct PackageBodyDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for PackageBodyDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::PackageBodyDeclaration => Some(PackageBodyDeclarationSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::PackageBodyDeclaration)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PackageBodyDeclarationSyntax {
    pub fn package_body(&self) -> Option<PackageBodySyntax> {
        self.0.children().filter_map(PackageBodySyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct UseClauseDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for UseClauseDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::UseClauseDeclaration => Some(UseClauseDeclarationSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::UseClauseDeclaration)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl UseClauseDeclarationSyntax {
    pub fn use_clause(&self) -> Option<UseClauseSyntax> {
        self.0.children().filter_map(UseClauseSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ActualPartSyntax(pub(crate) SyntaxNode);
impl AstNode for ActualPartSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ActualPart => Some(ActualPartSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ActualPart)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ActualPartSyntax {}
#[derive(Debug, Clone)]
pub struct AliasDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for AliasDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::AliasDeclaration => Some(AliasDeclarationSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::AliasDeclaration)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::AssociationElement)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::AssociationList)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::AttributeDeclaration)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ComponentDeclaration)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ComponentDeclarationSyntax {
    pub fn component_declaration_preamble(&self) -> Option<ComponentDeclarationPreambleSyntax> {
        self.0
            .children()
            .filter_map(ComponentDeclarationPreambleSyntax::cast)
            .nth(0)
    }
    pub fn component_declaration_items(&self) -> Option<ComponentDeclarationItemsSyntax> {
        self.0
            .children()
            .filter_map(ComponentDeclarationItemsSyntax::cast)
            .nth(0)
    }
    pub fn component_declaration_epilogue(&self) -> Option<ComponentDeclarationEpilogueSyntax> {
        self.0
            .children()
            .filter_map(ComponentDeclarationEpilogueSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ComponentDeclarationItemsSyntax(pub(crate) SyntaxNode);
impl AstNode for ComponentDeclarationItemsSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ComponentDeclarationItems => Some(ComponentDeclarationItemsSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ComponentDeclarationItems)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ComponentDeclarationItemsSyntax {
    pub fn generic_clause(&self) -> Option<GenericClauseSyntax> {
        self.0
            .children()
            .filter_map(GenericClauseSyntax::cast)
            .nth(0)
    }
    pub fn port_clause(&self) -> Option<PortClauseSyntax> {
        self.0.children().filter_map(PortClauseSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ComponentDeclarationPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for ComponentDeclarationPreambleSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ComponentDeclarationPreamble => {
                Some(ComponentDeclarationPreambleSyntax(node))
            }
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ComponentDeclarationPreamble)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ComponentDeclarationPreambleSyntax {
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
}
#[derive(Debug, Clone)]
pub struct ComponentDeclarationEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for ComponentDeclarationEpilogueSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ComponentDeclarationEpilogue => {
                Some(ComponentDeclarationEpilogueSyntax(node))
            }
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ComponentDeclarationEpilogue)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ComponentDeclarationEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::End))
            .nth(0)
    }
    pub fn component_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Component))
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
pub struct ConstantDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for ConstantDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ConstantDeclaration => Some(ConstantDeclarationSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ConstantDeclaration)
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
pub struct RangeConstraintConstraintSyntax(pub(crate) SyntaxNode);
impl AstNode for RangeConstraintConstraintSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::RangeConstraintConstraint => Some(RangeConstraintConstraintSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::RangeConstraintConstraint)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl RangeConstraintConstraintSyntax {
    pub fn range_constraint(&self) -> Option<RangeConstraintSyntax> {
        self.0
            .children()
            .filter_map(RangeConstraintSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum ConstraintSyntax {
    RangeConstraintConstraint(RangeConstraintConstraintSyntax),
    ArrayConstraint(ArrayConstraintSyntax),
    RecordConstraint(RecordConstraintSyntax),
}
impl AstNode for ConstraintSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        if RangeConstraintConstraintSyntax::can_cast(&node) {
            return Some(ConstraintSyntax::RangeConstraintConstraint(
                RangeConstraintConstraintSyntax::cast(node).unwrap(),
            ));
        };
        if ArrayConstraintSyntax::can_cast(&node) {
            return Some(ConstraintSyntax::ArrayConstraint(
                ArrayConstraintSyntax::cast(node).unwrap(),
            ));
        };
        if RecordConstraintSyntax::can_cast(&node) {
            return Some(ConstraintSyntax::RecordConstraint(
                RecordConstraintSyntax::cast(node).unwrap(),
            ));
        };
        None
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        RangeConstraintConstraintSyntax::can_cast(node)
            || ArrayConstraintSyntax::can_cast(node)
            || RecordConstraintSyntax::can_cast(node)
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            ConstraintSyntax::RangeConstraintConstraint(inner) => inner.raw(),
            ConstraintSyntax::ArrayConstraint(inner) => inner.raw(),
            ConstraintSyntax::RecordConstraint(inner) => inner.raw(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct ResolutionIndicationElementResolutionSyntax(pub(crate) SyntaxNode);
impl AstNode for ResolutionIndicationElementResolutionSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ResolutionIndicationElementResolution => {
                Some(ResolutionIndicationElementResolutionSyntax(node))
            }
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ResolutionIndicationElementResolution)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ResolutionIndicationElementResolutionSyntax {
    pub fn resolution_indication(&self) -> Option<ResolutionIndicationSyntax> {
        self.0
            .children()
            .filter_map(ResolutionIndicationSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct RecordResolutionElementResolutionSyntax(pub(crate) SyntaxNode);
impl AstNode for RecordResolutionElementResolutionSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::RecordResolutionElementResolution => {
                Some(RecordResolutionElementResolutionSyntax(node))
            }
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::RecordResolutionElementResolution)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl RecordResolutionElementResolutionSyntax {
    pub fn record_resolution(&self) -> Option<RecordResolutionSyntax> {
        self.0
            .children()
            .filter_map(RecordResolutionSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum ElementResolutionSyntax {
    ResolutionIndicationElementResolution(ResolutionIndicationElementResolutionSyntax),
    RecordResolutionElementResolution(RecordResolutionElementResolutionSyntax),
}
impl AstNode for ElementResolutionSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        if ResolutionIndicationElementResolutionSyntax::can_cast(&node) {
            return Some(
                ElementResolutionSyntax::ResolutionIndicationElementResolution(
                    ResolutionIndicationElementResolutionSyntax::cast(node).unwrap(),
                ),
            );
        };
        if RecordResolutionElementResolutionSyntax::can_cast(&node) {
            return Some(ElementResolutionSyntax::RecordResolutionElementResolution(
                RecordResolutionElementResolutionSyntax::cast(node).unwrap(),
            ));
        };
        None
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        ResolutionIndicationElementResolutionSyntax::can_cast(node)
            || RecordResolutionElementResolutionSyntax::can_cast(node)
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            ElementResolutionSyntax::ResolutionIndicationElementResolution(inner) => inner.raw(),
            ElementResolutionSyntax::RecordResolutionElementResolution(inner) => inner.raw(),
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::EntityClassEntry)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::EntityClassEntryList)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::FileDeclaration)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::FileOpenInformation)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ParenthesizedName)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::FormalPart)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::FullTypeDeclaration)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::GenericClause)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl GenericClauseSyntax {
    pub fn generic_clause_preamble(&self) -> Option<GenericClausePreambleSyntax> {
        self.0
            .children()
            .filter_map(GenericClausePreambleSyntax::cast)
            .nth(0)
    }
    pub fn interface_list(&self) -> Option<InterfaceListSyntax> {
        self.0
            .children()
            .filter_map(InterfaceListSyntax::cast)
            .nth(0)
    }
    pub fn generic_clause_epilogue(&self) -> Option<GenericClauseEpilogueSyntax> {
        self.0
            .children()
            .filter_map(GenericClauseEpilogueSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct GenericClausePreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for GenericClausePreambleSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::GenericClausePreamble => Some(GenericClausePreambleSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::GenericClausePreamble)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl GenericClausePreambleSyntax {
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
}
#[derive(Debug, Clone)]
pub struct GenericClauseEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for GenericClauseEpilogueSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::GenericClauseEpilogue => Some(GenericClauseEpilogueSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::GenericClauseEpilogue)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl GenericClauseEpilogueSyntax {
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::GenericMapAspect)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::GroupConstituentList)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::GroupDeclaration)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::GroupTemplateDeclaration)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::InterfaceConstantDeclaration)
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
        if InterfaceObjectDeclarationSyntax::can_cast(&node) {
            return Some(InterfaceDeclarationSyntax::InterfaceObjectDeclaration(
                InterfaceObjectDeclarationSyntax::cast(node).unwrap(),
            ));
        };
        if InterfaceIncompleteTypeDeclarationSyntax::can_cast(&node) {
            return Some(
                InterfaceDeclarationSyntax::InterfaceIncompleteTypeDeclaration(
                    InterfaceIncompleteTypeDeclarationSyntax::cast(node).unwrap(),
                ),
            );
        };
        if InterfaceSubprogramDeclarationSyntax::can_cast(&node) {
            return Some(InterfaceDeclarationSyntax::InterfaceSubprogramDeclaration(
                InterfaceSubprogramDeclarationSyntax::cast(node).unwrap(),
            ));
        };
        if InterfacePackageDeclarationSyntax::can_cast(&node) {
            return Some(InterfaceDeclarationSyntax::InterfacePackageDeclaration(
                InterfacePackageDeclarationSyntax::cast(node).unwrap(),
            ));
        };
        None
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        InterfaceObjectDeclarationSyntax::can_cast(node)
            || InterfaceIncompleteTypeDeclarationSyntax::can_cast(node)
            || InterfaceSubprogramDeclarationSyntax::can_cast(node)
            || InterfacePackageDeclarationSyntax::can_cast(node)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::InterfaceFileDeclaration)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::InterfaceFunctionSpecification)
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
    pub fn parameter_list(&self) -> Option<ParameterListSyntax> {
        self.0
            .children()
            .filter_map(ParameterListSyntax::cast)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::InterfaceIncompleteTypeDeclaration)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::InterfaceList)
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
        if InterfaceConstantDeclarationSyntax::can_cast(&node) {
            return Some(
                InterfaceObjectDeclarationSyntax::InterfaceConstantDeclaration(
                    InterfaceConstantDeclarationSyntax::cast(node).unwrap(),
                ),
            );
        };
        if InterfaceSignalDeclarationSyntax::can_cast(&node) {
            return Some(
                InterfaceObjectDeclarationSyntax::InterfaceSignalDeclaration(
                    InterfaceSignalDeclarationSyntax::cast(node).unwrap(),
                ),
            );
        };
        if InterfaceVariableDeclarationSyntax::can_cast(&node) {
            return Some(
                InterfaceObjectDeclarationSyntax::InterfaceVariableDeclaration(
                    InterfaceVariableDeclarationSyntax::cast(node).unwrap(),
                ),
            );
        };
        if InterfaceFileDeclarationSyntax::can_cast(&node) {
            return Some(InterfaceObjectDeclarationSyntax::InterfaceFileDeclaration(
                InterfaceFileDeclarationSyntax::cast(node).unwrap(),
            ));
        };
        None
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        InterfaceConstantDeclarationSyntax::can_cast(node)
            || InterfaceSignalDeclarationSyntax::can_cast(node)
            || InterfaceVariableDeclarationSyntax::can_cast(node)
            || InterfaceFileDeclarationSyntax::can_cast(node)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::InterfacePackageDeclaration)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfacePackageDeclarationSyntax {
    pub fn interface_package_declaration_preamble(
        &self,
    ) -> Option<InterfacePackageDeclarationPreambleSyntax> {
        self.0
            .children()
            .filter_map(InterfacePackageDeclarationPreambleSyntax::cast)
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
pub struct InterfacePackageDeclarationPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfacePackageDeclarationPreambleSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::InterfacePackageDeclarationPreamble => {
                Some(InterfacePackageDeclarationPreambleSyntax(node))
            }
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::InterfacePackageDeclarationPreamble)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfacePackageDeclarationPreambleSyntax {
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::InterfacePackageGenericMapAspect)
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
pub struct InterfacePackageGenericMapAspectBoxSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfacePackageGenericMapAspectBoxSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::InterfacePackageGenericMapAspectBox => {
                Some(InterfacePackageGenericMapAspectBoxSyntax(node))
            }
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::InterfacePackageGenericMapAspectBox)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfacePackageGenericMapAspectBoxSyntax {
    pub fn box_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == BOX).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct InterfacePackageGenericMapAspectDefaultSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfacePackageGenericMapAspectDefaultSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::InterfacePackageGenericMapAspectDefault => {
                Some(InterfacePackageGenericMapAspectDefaultSyntax(node))
            }
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(
            node.kind(),
            NodeKind::InterfacePackageGenericMapAspectDefault
        )
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfacePackageGenericMapAspectDefaultSyntax {
    pub fn default_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Default))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct InterfacePackageGenericMapAspectAssociationsSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfacePackageGenericMapAspectAssociationsSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::InterfacePackageGenericMapAspectAssociations => {
                Some(InterfacePackageGenericMapAspectAssociationsSyntax(node))
            }
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(
            node.kind(),
            NodeKind::InterfacePackageGenericMapAspectAssociations
        )
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfacePackageGenericMapAspectAssociationsSyntax {
    pub fn association_list(&self) -> Option<AssociationListSyntax> {
        self.0
            .children()
            .filter_map(AssociationListSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum InterfacePackageGenericMapAspectInnerSyntax {
    InterfacePackageGenericMapAspectBox(InterfacePackageGenericMapAspectBoxSyntax),
    InterfacePackageGenericMapAspectDefault(InterfacePackageGenericMapAspectDefaultSyntax),
    InterfacePackageGenericMapAspectAssociations(
        InterfacePackageGenericMapAspectAssociationsSyntax,
    ),
}
impl AstNode for InterfacePackageGenericMapAspectInnerSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        if InterfacePackageGenericMapAspectBoxSyntax::can_cast(&node) {
            return Some(
                InterfacePackageGenericMapAspectInnerSyntax::InterfacePackageGenericMapAspectBox(
                    InterfacePackageGenericMapAspectBoxSyntax::cast(node).unwrap(),
                ),
            );
        };
        if InterfacePackageGenericMapAspectDefaultSyntax::can_cast(&node) {
            return Some (InterfacePackageGenericMapAspectInnerSyntax :: InterfacePackageGenericMapAspectDefault (InterfacePackageGenericMapAspectDefaultSyntax :: cast (node) . unwrap ()));
        };
        if InterfacePackageGenericMapAspectAssociationsSyntax::can_cast(&node) {
            return Some (InterfacePackageGenericMapAspectInnerSyntax :: InterfacePackageGenericMapAspectAssociations (InterfacePackageGenericMapAspectAssociationsSyntax :: cast (node) . unwrap ()));
        };
        None
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        InterfacePackageGenericMapAspectBoxSyntax::can_cast(node)
            || InterfacePackageGenericMapAspectDefaultSyntax::can_cast(node)
            || InterfacePackageGenericMapAspectAssociationsSyntax::can_cast(node)
    }
    fn raw(&self) -> SyntaxNode {
        match self { InterfacePackageGenericMapAspectInnerSyntax :: InterfacePackageGenericMapAspectBox (inner) => inner . raw () , InterfacePackageGenericMapAspectInnerSyntax :: InterfacePackageGenericMapAspectDefault (inner) => inner . raw () , InterfacePackageGenericMapAspectInnerSyntax :: InterfacePackageGenericMapAspectAssociations (inner) => inner . raw () , }
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::InterfaceProcedureSpecification)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::InterfaceSignalDeclaration)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::InterfaceSubprogramDeclaration)
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
pub struct InterfaceSubprogramDefaultNameSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfaceSubprogramDefaultNameSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::InterfaceSubprogramDefaultName => {
                Some(InterfaceSubprogramDefaultNameSyntax(node))
            }
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::InterfaceSubprogramDefaultName)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfaceSubprogramDefaultNameSyntax {
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct InterfaceSubprogramDefaultBoxSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfaceSubprogramDefaultBoxSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::InterfaceSubprogramDefaultBox => {
                Some(InterfaceSubprogramDefaultBoxSyntax(node))
            }
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::InterfaceSubprogramDefaultBox)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfaceSubprogramDefaultBoxSyntax {
    pub fn box_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == BOX).nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum InterfaceSubprogramDefaultSyntax {
    InterfaceSubprogramDefaultName(InterfaceSubprogramDefaultNameSyntax),
    InterfaceSubprogramDefaultBox(InterfaceSubprogramDefaultBoxSyntax),
}
impl AstNode for InterfaceSubprogramDefaultSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        if InterfaceSubprogramDefaultNameSyntax::can_cast(&node) {
            return Some(
                InterfaceSubprogramDefaultSyntax::InterfaceSubprogramDefaultName(
                    InterfaceSubprogramDefaultNameSyntax::cast(node).unwrap(),
                ),
            );
        };
        if InterfaceSubprogramDefaultBoxSyntax::can_cast(&node) {
            return Some(
                InterfaceSubprogramDefaultSyntax::InterfaceSubprogramDefaultBox(
                    InterfaceSubprogramDefaultBoxSyntax::cast(node).unwrap(),
                ),
            );
        };
        None
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        InterfaceSubprogramDefaultNameSyntax::can_cast(node)
            || InterfaceSubprogramDefaultBoxSyntax::can_cast(node)
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            InterfaceSubprogramDefaultSyntax::InterfaceSubprogramDefaultName(inner) => inner.raw(),
            InterfaceSubprogramDefaultSyntax::InterfaceSubprogramDefaultBox(inner) => inner.raw(),
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
        if InterfaceProcedureSpecificationSyntax::can_cast(&node) {
            return Some(
                InterfaceSubprogramSpecificationSyntax::InterfaceProcedureSpecification(
                    InterfaceProcedureSpecificationSyntax::cast(node).unwrap(),
                ),
            );
        };
        if InterfaceFunctionSpecificationSyntax::can_cast(&node) {
            return Some(
                InterfaceSubprogramSpecificationSyntax::InterfaceFunctionSpecification(
                    InterfaceFunctionSpecificationSyntax::cast(node).unwrap(),
                ),
            );
        };
        None
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        InterfaceProcedureSpecificationSyntax::can_cast(node)
            || InterfaceFunctionSpecificationSyntax::can_cast(node)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::InterfaceVariableDeclaration)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::PortClause)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PortClauseSyntax {
    pub fn port_clause_preamble(&self) -> Option<PortClausePreambleSyntax> {
        self.0
            .children()
            .filter_map(PortClausePreambleSyntax::cast)
            .nth(0)
    }
    pub fn interface_list(&self) -> Option<InterfaceListSyntax> {
        self.0
            .children()
            .filter_map(InterfaceListSyntax::cast)
            .nth(0)
    }
    pub fn port_clause_epilogue(&self) -> Option<PortClauseEpilogueSyntax> {
        self.0
            .children()
            .filter_map(PortClauseEpilogueSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct PortClausePreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for PortClausePreambleSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::PortClausePreamble => Some(PortClausePreambleSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::PortClausePreamble)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PortClausePreambleSyntax {
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
}
#[derive(Debug, Clone)]
pub struct PortClauseEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for PortClauseEpilogueSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::PortClauseEpilogue => Some(PortClauseEpilogueSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::PortClauseEpilogue)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PortClauseEpilogueSyntax {
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::PortMapAspect)
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
pub struct RecordElementResolutionSyntax(pub(crate) SyntaxNode);
impl AstNode for RecordElementResolutionSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::RecordElementResolution => Some(RecordElementResolutionSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::RecordElementResolution)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::RecordResolution)
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
pub struct NameResolutionIndicationSyntax(pub(crate) SyntaxNode);
impl AstNode for NameResolutionIndicationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::NameResolutionIndication => Some(NameResolutionIndicationSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::NameResolutionIndication)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl NameResolutionIndicationSyntax {
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ElementResolutionResolutionIndicationSyntax(pub(crate) SyntaxNode);
impl AstNode for ElementResolutionResolutionIndicationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ElementResolutionResolutionIndication => {
                Some(ElementResolutionResolutionIndicationSyntax(node))
            }
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ElementResolutionResolutionIndication)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ElementResolutionResolutionIndicationSyntax {
    pub fn element_resolution(&self) -> Option<ElementResolutionSyntax> {
        self.0
            .children()
            .filter_map(ElementResolutionSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ParenthesizedElementResolutionResolutionIndicationSyntax(pub(crate) SyntaxNode);
impl AstNode for ParenthesizedElementResolutionResolutionIndicationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ParenthesizedElementResolutionResolutionIndication => Some(
                ParenthesizedElementResolutionResolutionIndicationSyntax(node),
            ),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(
            node.kind(),
            NodeKind::ParenthesizedElementResolutionResolutionIndication
        )
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ParenthesizedElementResolutionResolutionIndicationSyntax {
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == LeftPar)
            .nth(0)
    }
    pub fn element_resolution_resolution_indication(
        &self,
    ) -> Option<ElementResolutionResolutionIndicationSyntax> {
        self.0
            .children()
            .filter_map(ElementResolutionResolutionIndicationSyntax::cast)
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
    NameResolutionIndication(NameResolutionIndicationSyntax),
    ParenthesizedElementResolutionResolutionIndication(
        ParenthesizedElementResolutionResolutionIndicationSyntax,
    ),
}
impl AstNode for ResolutionIndicationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        if NameResolutionIndicationSyntax::can_cast(&node) {
            return Some(ResolutionIndicationSyntax::NameResolutionIndication(
                NameResolutionIndicationSyntax::cast(node).unwrap(),
            ));
        };
        if ParenthesizedElementResolutionResolutionIndicationSyntax::can_cast(&node) {
            return Some(
                ResolutionIndicationSyntax::ParenthesizedElementResolutionResolutionIndication(
                    ParenthesizedElementResolutionResolutionIndicationSyntax::cast(node).unwrap(),
                ),
            );
        };
        None
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        NameResolutionIndicationSyntax::can_cast(node)
            || ParenthesizedElementResolutionResolutionIndicationSyntax::can_cast(node)
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            ResolutionIndicationSyntax::NameResolutionIndication(inner) => inner.raw(),
            ResolutionIndicationSyntax::ParenthesizedElementResolutionResolutionIndication(
                inner,
            ) => inner.raw(),
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::SignalDeclaration)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::SubtypeDeclaration)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::SubtypeIndication)
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
}
#[derive(Debug, Clone)]
pub enum TypeDeclarationSyntax {
    FullTypeDeclaration(FullTypeDeclarationSyntax),
    IncompleteTypeDeclaration(IncompleteTypeDeclarationSyntax),
}
impl AstNode for TypeDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        if FullTypeDeclarationSyntax::can_cast(&node) {
            return Some(TypeDeclarationSyntax::FullTypeDeclaration(
                FullTypeDeclarationSyntax::cast(node).unwrap(),
            ));
        };
        if IncompleteTypeDeclarationSyntax::can_cast(&node) {
            return Some(TypeDeclarationSyntax::IncompleteTypeDeclaration(
                IncompleteTypeDeclarationSyntax::cast(node).unwrap(),
            ));
        };
        None
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        FullTypeDeclarationSyntax::can_cast(node) || IncompleteTypeDeclarationSyntax::can_cast(node)
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
        if ScalarTypeDefinitionSyntax::can_cast(&node) {
            return Some(TypeDefinitionSyntax::ScalarTypeDefinition(
                ScalarTypeDefinitionSyntax::cast(node).unwrap(),
            ));
        };
        if CompositeTypeDefinitionSyntax::can_cast(&node) {
            return Some(TypeDefinitionSyntax::CompositeTypeDefinition(
                CompositeTypeDefinitionSyntax::cast(node).unwrap(),
            ));
        };
        if AccessTypeDefinitionSyntax::can_cast(&node) {
            return Some(TypeDefinitionSyntax::AccessTypeDefinition(
                AccessTypeDefinitionSyntax::cast(node).unwrap(),
            ));
        };
        if FileTypeDefinitionSyntax::can_cast(&node) {
            return Some(TypeDefinitionSyntax::FileTypeDefinition(
                FileTypeDefinitionSyntax::cast(node).unwrap(),
            ));
        };
        if ProtectedTypeDefinitionSyntax::can_cast(&node) {
            return Some(TypeDefinitionSyntax::ProtectedTypeDefinition(
                ProtectedTypeDefinitionSyntax::cast(node).unwrap(),
            ));
        };
        None
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        ScalarTypeDefinitionSyntax::can_cast(node)
            || CompositeTypeDefinitionSyntax::can_cast(node)
            || AccessTypeDefinitionSyntax::can_cast(node)
            || FileTypeDefinitionSyntax::can_cast(node)
            || ProtectedTypeDefinitionSyntax::can_cast(node)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::VariableDeclaration)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::SharedVariableDeclaration)
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
#[derive(Debug, Clone)]
pub struct DeclarationsSyntax(pub(crate) SyntaxNode);
impl AstNode for DeclarationsSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::Declarations => Some(DeclarationsSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::Declarations)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl DeclarationsSyntax {
    pub fn declarations(&self) -> impl Iterator<Item = DeclarationSyntax> + use<'_> {
        self.0.children().filter_map(DeclarationSyntax::cast)
    }
}
