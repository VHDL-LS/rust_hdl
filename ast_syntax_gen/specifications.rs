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
pub struct AttributeSpecificationSyntax(pub(crate) SyntaxNode);
impl AstNode for AttributeSpecificationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::AttributeSpecification => Some(AttributeSpecificationSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl AttributeSpecificationSyntax {
    pub fn attribute_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Attribute))
            .nth(0)
    }
    pub fn attribute_designator_token_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Identifier)
            .nth(0)
    }
    pub fn of_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Of))
            .nth(0)
    }
    pub fn entity_specification(&self) -> Option<EntitySpecificationSyntax> {
        self.0
            .children()
            .filter_map(EntitySpecificationSyntax::cast)
            .nth(0)
    }
    pub fn is_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Is))
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
pub struct BindingIndicationSyntax(pub(crate) SyntaxNode);
impl AstNode for BindingIndicationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::BindingIndication => Some(BindingIndicationSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl BindingIndicationSyntax {
    pub fn use_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Use))
            .nth(0)
    }
    pub fn entity_aspect(&self) -> Option<EntityAspectSyntax> {
        self.0
            .children()
            .filter_map(EntityAspectSyntax::cast)
            .nth(0)
    }
    pub fn generic_map_aspect(&self) -> Option<GenericMapAspectSyntax> {
        self.0
            .children()
            .filter_map(GenericMapAspectSyntax::cast)
            .nth(0)
    }
    pub fn port_map_aspect(&self) -> Option<PortMapAspectSyntax> {
        self.0
            .children()
            .filter_map(PortMapAspectSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ComponentSpecificationSyntax(pub(crate) SyntaxNode);
impl AstNode for ComponentSpecificationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ComponentSpecification => Some(ComponentSpecificationSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ComponentSpecificationSyntax {
    pub fn instantiation_list(&self) -> Option<InstantiationListSyntax> {
        self.0
            .children()
            .filter_map(InstantiationListSyntax::cast)
            .nth(0)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == Colon).nth(0)
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct CompoundConfigurationSpecificationSyntax(pub(crate) SyntaxNode);
impl AstNode for CompoundConfigurationSpecificationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::CompoundConfigurationSpecification => {
                Some(CompoundConfigurationSpecificationSyntax(node))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl CompoundConfigurationSpecificationSyntax {
    pub fn for_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::For))
            .nth(0)
    }
    pub fn component_specification(&self) -> Option<ComponentSpecificationSyntax> {
        self.0
            .children()
            .filter_map(ComponentSpecificationSyntax::cast)
            .nth(0)
    }
    pub fn binding_indication(&self) -> Option<BindingIndicationSyntax> {
        self.0
            .children()
            .filter_map(BindingIndicationSyntax::cast)
            .nth(0)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == SemiColon)
            .nth(0)
    }
    pub fn verification_unit_binding_indications(
        &self,
    ) -> impl Iterator<Item = VerificationUnitBindingIndicationSyntax> + use<'_> {
        self.0
            .children()
            .filter_map(VerificationUnitBindingIndicationSyntax::cast)
    }
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::End))
            .nth(0)
    }
    pub fn trailing_for_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::For))
            .nth(1)
    }
    pub fn trailing_semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == SemiColon)
            .nth(1)
    }
}
#[derive(Debug, Clone)]
pub enum ConfigurationSpecificationSyntax {
    SimpleConfigurationSpecification(SimpleConfigurationSpecificationSyntax),
    CompoundConfigurationSpecification(CompoundConfigurationSpecificationSyntax),
}
impl AstNode for ConfigurationSpecificationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SimpleConfigurationSpecification => Some(
                ConfigurationSpecificationSyntax::SimpleConfigurationSpecification(
                    SimpleConfigurationSpecificationSyntax::cast(node).unwrap(),
                ),
            ),
            NodeKind::CompoundConfigurationSpecification => Some(
                ConfigurationSpecificationSyntax::CompoundConfigurationSpecification(
                    CompoundConfigurationSpecificationSyntax::cast(node).unwrap(),
                ),
            ),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            ConfigurationSpecificationSyntax::SimpleConfigurationSpecification(inner) => {
                inner.raw()
            }
            ConfigurationSpecificationSyntax::CompoundConfigurationSpecification(inner) => {
                inner.raw()
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct DisconnectionSpecificationSyntax(pub(crate) SyntaxNode);
impl AstNode for DisconnectionSpecificationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::DisconnectionSpecification => Some(DisconnectionSpecificationSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl DisconnectionSpecificationSyntax {
    pub fn disconnect_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Disconnect))
            .nth(0)
    }
    pub fn guarded_signal_specification(&self) -> Option<GuardedSignalSpecificationSyntax> {
        self.0
            .children()
            .filter_map(GuardedSignalSpecificationSyntax::cast)
            .nth(0)
    }
    pub fn after_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::After))
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
pub struct EntityEntityAspectSyntax(pub(crate) SyntaxNode);
impl AstNode for EntityEntityAspectSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::EntityEntityAspect => Some(EntityEntityAspectSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl EntityEntityAspectSyntax {
    pub fn entity_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Entity))
            .nth(0)
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
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Identifier)
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
pub struct EntityConfigurationAspectSyntax(pub(crate) SyntaxNode);
impl AstNode for EntityConfigurationAspectSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::EntityConfigurationAspect => Some(EntityConfigurationAspectSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl EntityConfigurationAspectSyntax {
    pub fn configuration_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Configuration))
            .nth(0)
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct EntityOpenAspectSyntax(pub(crate) SyntaxNode);
impl AstNode for EntityOpenAspectSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::EntityOpenAspect => Some(EntityOpenAspectSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl EntityOpenAspectSyntax {
    pub fn open_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Open))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum EntityAspectSyntax {
    EntityEntityAspect(EntityEntityAspectSyntax),
    EntityConfigurationAspect(EntityConfigurationAspectSyntax),
    EntityOpenAspect(EntityOpenAspectSyntax),
}
impl AstNode for EntityAspectSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::EntityEntityAspect => Some(EntityAspectSyntax::EntityEntityAspect(
                EntityEntityAspectSyntax::cast(node).unwrap(),
            )),
            NodeKind::EntityConfigurationAspect => {
                Some(EntityAspectSyntax::EntityConfigurationAspect(
                    EntityConfigurationAspectSyntax::cast(node).unwrap(),
                ))
            }
            NodeKind::EntityOpenAspect => Some(EntityAspectSyntax::EntityOpenAspect(
                EntityOpenAspectSyntax::cast(node).unwrap(),
            )),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            EntityAspectSyntax::EntityEntityAspect(inner) => inner.raw(),
            EntityAspectSyntax::EntityConfigurationAspect(inner) => inner.raw(),
            EntityAspectSyntax::EntityOpenAspect(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum EntityClassSyntax {
    Entity(SyntaxToken),
    Architecture(SyntaxToken),
    Configuration(SyntaxToken),
    Procedure(SyntaxToken),
    Function(SyntaxToken),
    Package(SyntaxToken),
    Type(SyntaxToken),
    Subtype(SyntaxToken),
    Constant(SyntaxToken),
    Signal(SyntaxToken),
    Variable(SyntaxToken),
    Component(SyntaxToken),
    Label(SyntaxToken),
    Literal(SyntaxToken),
    Units(SyntaxToken),
    Group(SyntaxToken),
    File(SyntaxToken),
    Property(SyntaxToken),
    Sequence(SyntaxToken),
}
impl EntityClassSyntax {
    pub fn cast(token: SyntaxToken) -> Option<Self> {
        match token.kind() {
            Keyword(Kw::Entity) => Some(EntityClassSyntax::Entity(token)),
            Keyword(Kw::Architecture) => Some(EntityClassSyntax::Architecture(token)),
            Keyword(Kw::Configuration) => Some(EntityClassSyntax::Configuration(token)),
            Keyword(Kw::Procedure) => Some(EntityClassSyntax::Procedure(token)),
            Keyword(Kw::Function) => Some(EntityClassSyntax::Function(token)),
            Keyword(Kw::Package) => Some(EntityClassSyntax::Package(token)),
            Keyword(Kw::Type) => Some(EntityClassSyntax::Type(token)),
            Keyword(Kw::Subtype) => Some(EntityClassSyntax::Subtype(token)),
            Keyword(Kw::Constant) => Some(EntityClassSyntax::Constant(token)),
            Keyword(Kw::Signal) => Some(EntityClassSyntax::Signal(token)),
            Keyword(Kw::Variable) => Some(EntityClassSyntax::Variable(token)),
            Keyword(Kw::Component) => Some(EntityClassSyntax::Component(token)),
            Keyword(Kw::Label) => Some(EntityClassSyntax::Label(token)),
            Keyword(Kw::Literal) => Some(EntityClassSyntax::Literal(token)),
            Keyword(Kw::Units) => Some(EntityClassSyntax::Units(token)),
            Keyword(Kw::Group) => Some(EntityClassSyntax::Group(token)),
            Keyword(Kw::File) => Some(EntityClassSyntax::File(token)),
            Keyword(Kw::Property) => Some(EntityClassSyntax::Property(token)),
            Keyword(Kw::Sequence) => Some(EntityClassSyntax::Sequence(token)),
            _ => None,
        }
    }
    pub fn raw(&self) -> SyntaxToken {
        match self {
            EntityClassSyntax::Entity(token) => token.clone(),
            EntityClassSyntax::Architecture(token) => token.clone(),
            EntityClassSyntax::Configuration(token) => token.clone(),
            EntityClassSyntax::Procedure(token) => token.clone(),
            EntityClassSyntax::Function(token) => token.clone(),
            EntityClassSyntax::Package(token) => token.clone(),
            EntityClassSyntax::Type(token) => token.clone(),
            EntityClassSyntax::Subtype(token) => token.clone(),
            EntityClassSyntax::Constant(token) => token.clone(),
            EntityClassSyntax::Signal(token) => token.clone(),
            EntityClassSyntax::Variable(token) => token.clone(),
            EntityClassSyntax::Component(token) => token.clone(),
            EntityClassSyntax::Label(token) => token.clone(),
            EntityClassSyntax::Literal(token) => token.clone(),
            EntityClassSyntax::Units(token) => token.clone(),
            EntityClassSyntax::Group(token) => token.clone(),
            EntityClassSyntax::File(token) => token.clone(),
            EntityClassSyntax::Property(token) => token.clone(),
            EntityClassSyntax::Sequence(token) => token.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct EntityDesignatorSyntax(pub(crate) SyntaxNode);
impl AstNode for EntityDesignatorSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::EntityDesignator => Some(EntityDesignatorSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl EntityDesignatorSyntax {
    pub fn entity_tag(&self) -> Option<EntityTagSyntax> {
        self.0.tokens().filter_map(EntityTagSyntax::cast).nth(0)
    }
    pub fn signature(&self) -> Option<SignatureSyntax> {
        self.0.children().filter_map(SignatureSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct OthersDesignatorSyntax(pub(crate) SyntaxNode);
impl AstNode for OthersDesignatorSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::OthersDesignator => Some(OthersDesignatorSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl OthersDesignatorSyntax {
    pub fn all_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::All))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct AllDesignatorSyntax(pub(crate) SyntaxNode);
impl AstNode for AllDesignatorSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::AllDesignator => Some(AllDesignatorSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl AllDesignatorSyntax {
    pub fn all_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::All))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct EntityDesignatorListSyntax(pub(crate) SyntaxNode);
impl AstNode for EntityDesignatorListSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::EntityDesignatorList => Some(EntityDesignatorListSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl EntityDesignatorListSyntax {
    pub fn entity_designators(&self) -> impl Iterator<Item = EntityDesignatorSyntax> + use<'_> {
        self.0.children().filter_map(EntityDesignatorSyntax::cast)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.tokens().filter(|token| token.kind() == Comma)
    }
}
#[derive(Debug, Clone)]
pub enum EntityNameListSyntax {
    EntityDesignatorList(EntityDesignatorListSyntax),
    AllDesignator(AllDesignatorSyntax),
    OthersDesignator(OthersDesignatorSyntax),
}
impl AstNode for EntityNameListSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::EntityDesignatorList => Some(EntityNameListSyntax::EntityDesignatorList(
                EntityDesignatorListSyntax::cast(node).unwrap(),
            )),
            NodeKind::AllDesignator => Some(EntityNameListSyntax::AllDesignator(
                AllDesignatorSyntax::cast(node).unwrap(),
            )),
            NodeKind::OthersDesignator => Some(EntityNameListSyntax::OthersDesignator(
                OthersDesignatorSyntax::cast(node).unwrap(),
            )),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            EntityNameListSyntax::EntityDesignatorList(inner) => inner.raw(),
            EntityNameListSyntax::AllDesignator(inner) => inner.raw(),
            EntityNameListSyntax::OthersDesignator(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct EntitySpecificationSyntax(pub(crate) SyntaxNode);
impl AstNode for EntitySpecificationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::EntitySpecification => Some(EntitySpecificationSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl EntitySpecificationSyntax {
    pub fn entity_name_list(&self) -> Option<EntityNameListSyntax> {
        self.0
            .children()
            .filter_map(EntityNameListSyntax::cast)
            .nth(0)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == Colon).nth(0)
    }
    pub fn entity_class(&self) -> Option<EntityClassSyntax> {
        self.0.tokens().filter_map(EntityClassSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum EntityTagSyntax {
    Identifier(SyntaxToken),
    CharacterLiteral(SyntaxToken),
    StringLiteral(SyntaxToken),
}
impl EntityTagSyntax {
    pub fn cast(token: SyntaxToken) -> Option<Self> {
        match token.kind() {
            Identifier => Some(EntityTagSyntax::Identifier(token)),
            CharacterLiteral => Some(EntityTagSyntax::CharacterLiteral(token)),
            StringLiteral => Some(EntityTagSyntax::StringLiteral(token)),
            _ => None,
        }
    }
    pub fn raw(&self) -> SyntaxToken {
        match self {
            EntityTagSyntax::Identifier(token) => token.clone(),
            EntityTagSyntax::CharacterLiteral(token) => token.clone(),
            EntityTagSyntax::StringLiteral(token) => token.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct GuardedSignalSpecificationSyntax(pub(crate) SyntaxNode);
impl AstNode for GuardedSignalSpecificationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::GuardedSignalSpecification => Some(GuardedSignalSpecificationSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl GuardedSignalSpecificationSyntax {
    pub fn signal_list(&self) -> Option<SignalListSyntax> {
        self.0.children().filter_map(SignalListSyntax::cast).nth(0)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == Colon).nth(0)
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct InstantiationListListSyntax(pub(crate) SyntaxNode);
impl AstNode for InstantiationListListSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::InstantiationListList => Some(InstantiationListListSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InstantiationListListSyntax {
    pub fn identifier_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.tokens().filter(|token| token.kind() == Identifier)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.tokens().filter(|token| token.kind() == Comma)
    }
}
#[derive(Debug, Clone)]
pub enum InstantiationListSyntax {
    InstantiationListList(InstantiationListListSyntax),
    OthersDesignator(OthersDesignatorSyntax),
    AllDesignator(AllDesignatorSyntax),
}
impl AstNode for InstantiationListSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::InstantiationListList => {
                Some(InstantiationListSyntax::InstantiationListList(
                    InstantiationListListSyntax::cast(node).unwrap(),
                ))
            }
            NodeKind::OthersDesignator => Some(InstantiationListSyntax::OthersDesignator(
                OthersDesignatorSyntax::cast(node).unwrap(),
            )),
            NodeKind::AllDesignator => Some(InstantiationListSyntax::AllDesignator(
                AllDesignatorSyntax::cast(node).unwrap(),
            )),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            InstantiationListSyntax::InstantiationListList(inner) => inner.raw(),
            InstantiationListSyntax::OthersDesignator(inner) => inner.raw(),
            InstantiationListSyntax::AllDesignator(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SignalListListSyntax(pub(crate) SyntaxNode);
impl AstNode for SignalListListSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SignalListList => Some(SignalListListSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SignalListListSyntax {
    pub fn names(&self) -> impl Iterator<Item = NameSyntax> + use<'_> {
        self.0.children().filter_map(NameSyntax::cast)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.tokens().filter(|token| token.kind() == Comma)
    }
}
#[derive(Debug, Clone)]
pub enum SignalListSyntax {
    SignalListList(SignalListListSyntax),
    OthersDesignator(OthersDesignatorSyntax),
    AllDesignator(AllDesignatorSyntax),
}
impl AstNode for SignalListSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SignalListList => Some(SignalListSyntax::SignalListList(
                SignalListListSyntax::cast(node).unwrap(),
            )),
            NodeKind::OthersDesignator => Some(SignalListSyntax::OthersDesignator(
                OthersDesignatorSyntax::cast(node).unwrap(),
            )),
            NodeKind::AllDesignator => Some(SignalListSyntax::AllDesignator(
                AllDesignatorSyntax::cast(node).unwrap(),
            )),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            SignalListSyntax::SignalListList(inner) => inner.raw(),
            SignalListSyntax::OthersDesignator(inner) => inner.raw(),
            SignalListSyntax::AllDesignator(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SimpleConfigurationSpecificationSyntax(pub(crate) SyntaxNode);
impl AstNode for SimpleConfigurationSpecificationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SimpleConfigurationSpecification => {
                Some(SimpleConfigurationSpecificationSyntax(node))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SimpleConfigurationSpecificationSyntax {
    pub fn for_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::For))
            .nth(0)
    }
    pub fn component_specification(&self) -> Option<ComponentSpecificationSyntax> {
        self.0
            .children()
            .filter_map(ComponentSpecificationSyntax::cast)
            .nth(0)
    }
    pub fn binding_indication(&self) -> Option<BindingIndicationSyntax> {
        self.0
            .children()
            .filter_map(BindingIndicationSyntax::cast)
            .nth(0)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == SemiColon)
            .nth(0)
    }
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::End))
            .nth(0)
    }
    pub fn trailing_for_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::For))
            .nth(1)
    }
    pub fn trailing_semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == SemiColon)
            .nth(1)
    }
}
#[derive(Debug, Clone)]
pub struct VerificationUnitBindingIndicationSyntax(pub(crate) SyntaxNode);
impl AstNode for VerificationUnitBindingIndicationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::VerificationUnitBindingIndication => {
                Some(VerificationUnitBindingIndicationSyntax(node))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl VerificationUnitBindingIndicationSyntax {
    pub fn use_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Use))
            .nth(0)
    }
    pub fn vunit_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Vunit))
            .nth(0)
    }
    pub fn verification_unit_list(&self) -> Option<VerificationUnitListSyntax> {
        self.0
            .children()
            .filter_map(VerificationUnitListSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct VerificationUnitListSyntax(pub(crate) SyntaxNode);
impl AstNode for VerificationUnitListSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::VerificationUnitList => Some(VerificationUnitListSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl VerificationUnitListSyntax {
    pub fn names(&self) -> impl Iterator<Item = NameSyntax> + use<'_> {
        self.0.children().filter_map(NameSyntax::cast)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.tokens().filter(|token| token.kind() == Comma)
    }
}
