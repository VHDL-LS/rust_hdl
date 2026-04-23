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
pub struct AttributeSpecificationSyntax(pub(crate) SyntaxNode);
impl AstNode for AttributeSpecificationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::AttributeSpecification,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "attribute",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Attribute)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "attribute_designator_token",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "of",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Of)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "entity_specification",
                kind: LayoutItemKind::Node(NodeKind::EntitySpecification),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "is",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Is)),
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
                name: "semi_colon",
                kind: LayoutItemKind::Token(TokenKind::SemiColon),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        AttributeSpecificationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl AttributeSpecificationSyntax {
    pub fn attribute_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Attribute))
            .nth(0)
    }
    pub fn attribute_designator_token_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Identifier)
            .nth(0)
    }
    pub fn of_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Of))
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
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Is))
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct BindingIndicationSyntax(pub(crate) SyntaxNode);
impl AstNode for BindingIndicationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::BindingIndication,
        items: &[
            LayoutItem {
                optional: true,
                repeated: false,
                name: "use",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Use)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "entity_aspect",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::EntityEntityAspect,
                    NodeKind::EntityConfigurationAspect,
                    NodeKind::EntityOpenAspect,
                ]),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "generic_map_aspect",
                kind: LayoutItemKind::Node(NodeKind::GenericMapAspect),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "port_map_aspect",
                kind: LayoutItemKind::Node(NodeKind::PortMapAspect),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        BindingIndicationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl BindingIndicationSyntax {
    pub fn use_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Use))
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ComponentSpecification,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "instantiation_list",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::InstantiationListList,
                    NodeKind::InstantiationListAll,
                    NodeKind::InstantiationListOthers,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "colon",
                kind: LayoutItemKind::Token(TokenKind::Colon),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "name",
                kind: LayoutItemKind::Node(NodeKind::Name),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ComponentSpecificationSyntax(node)
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
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Colon)
            .nth(0)
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct CompoundConfigurationSpecificationSyntax(pub(crate) SyntaxNode);
impl AstNode for CompoundConfigurationSpecificationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::CompoundConfigurationSpecification,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "component_configuration_preamble",
                kind: LayoutItemKind::Node(NodeKind::ComponentConfigurationPreamble),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "compound_configuration_specification_items",
                kind: LayoutItemKind::Node(NodeKind::CompoundConfigurationSpecificationItems),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "component_configuration_epilogue",
                kind: LayoutItemKind::Node(NodeKind::ComponentConfigurationEpilogue),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        CompoundConfigurationSpecificationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl CompoundConfigurationSpecificationSyntax {
    pub fn component_configuration_preamble(&self) -> Option<ComponentConfigurationPreambleSyntax> {
        self.0
            .children()
            .filter_map(ComponentConfigurationPreambleSyntax::cast)
            .nth(0)
    }
    pub fn compound_configuration_specification_items(
        &self,
    ) -> Option<CompoundConfigurationSpecificationItemsSyntax> {
        self.0
            .children()
            .filter_map(CompoundConfigurationSpecificationItemsSyntax::cast)
            .nth(0)
    }
    pub fn component_configuration_epilogue(&self) -> Option<ComponentConfigurationEpilogueSyntax> {
        self.0
            .children()
            .filter_map(ComponentConfigurationEpilogueSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct CompoundConfigurationSpecificationItemsSyntax(pub(crate) SyntaxNode);
impl AstNode for CompoundConfigurationSpecificationItemsSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::CompoundConfigurationSpecificationItems,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "semi_colon_terminated_binding_indication",
                kind: LayoutItemKind::Node(NodeKind::SemiColonTerminatedBindingIndication),
            },
            LayoutItem {
                optional: true,
                repeated: true,
                name: "verification_unit_binding_indications",
                kind: LayoutItemKind::Node(NodeKind::VerificationUnitBindingIndication),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        CompoundConfigurationSpecificationItemsSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl CompoundConfigurationSpecificationItemsSyntax {
    pub fn semi_colon_terminated_binding_indication(
        &self,
    ) -> Option<SemiColonTerminatedBindingIndicationSyntax> {
        self.0
            .children()
            .filter_map(SemiColonTerminatedBindingIndicationSyntax::cast)
            .nth(0)
    }
    pub fn verification_unit_binding_indications(
        &self,
    ) -> impl Iterator<Item = VerificationUnitBindingIndicationSyntax> + use<'_> {
        self.0
            .children()
            .filter_map(VerificationUnitBindingIndicationSyntax::cast)
    }
}
#[derive(Debug, Clone)]
pub enum ConfigurationSpecificationSyntax {
    SimpleConfigurationSpecification(SimpleConfigurationSpecificationSyntax),
    CompoundConfigurationSpecification(CompoundConfigurationSpecificationSyntax),
}
impl AstNode for ConfigurationSpecificationSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::SimpleConfigurationSpecification,
            NodeKind::CompoundConfigurationSpecification,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if SimpleConfigurationSpecificationSyntax::can_cast(&node) {
            return ConfigurationSpecificationSyntax::SimpleConfigurationSpecification(
                SimpleConfigurationSpecificationSyntax::cast_unchecked(node),
            );
        }
        if CompoundConfigurationSpecificationSyntax::can_cast(&node) {
            return ConfigurationSpecificationSyntax::CompoundConfigurationSpecification(
                CompoundConfigurationSpecificationSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::DisconnectionSpecification,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "disconnect",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Disconnect)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "guarded_signal_specification",
                kind: LayoutItemKind::Node(NodeKind::GuardedSignalSpecification),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "after",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::After)),
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
                name: "semi_colon",
                kind: LayoutItemKind::Token(TokenKind::SemiColon),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        DisconnectionSpecificationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl DisconnectionSpecificationSyntax {
    pub fn disconnect_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Disconnect))
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
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::After))
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct EntityEntityAspectSyntax(pub(crate) SyntaxNode);
impl AstNode for EntityEntityAspectSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::EntityEntityAspect,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "entity",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Entity)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "name",
                kind: LayoutItemKind::Node(NodeKind::Name),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        EntityEntityAspectSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl EntityEntityAspectSyntax {
    pub fn entity_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Entity))
            .nth(0)
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct EntityConfigurationAspectSyntax(pub(crate) SyntaxNode);
impl AstNode for EntityConfigurationAspectSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::EntityConfigurationAspect,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "configuration",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Configuration)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "name",
                kind: LayoutItemKind::Node(NodeKind::Name),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        EntityConfigurationAspectSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl EntityConfigurationAspectSyntax {
    pub fn configuration_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Configuration))
            .nth(0)
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct EntityOpenAspectSyntax(pub(crate) SyntaxNode);
impl AstNode for EntityOpenAspectSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::EntityOpenAspect,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "open",
            kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Open)),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        EntityOpenAspectSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl EntityOpenAspectSyntax {
    pub fn open_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Open))
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
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::EntityEntityAspect,
            NodeKind::EntityConfigurationAspect,
            NodeKind::EntityOpenAspect,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if EntityEntityAspectSyntax::can_cast(&node) {
            return EntityAspectSyntax::EntityEntityAspect(
                EntityEntityAspectSyntax::cast_unchecked(node),
            );
        }
        if EntityConfigurationAspectSyntax::can_cast(&node) {
            return EntityAspectSyntax::EntityConfigurationAspect(
                EntityConfigurationAspectSyntax::cast_unchecked(node),
            );
        }
        if EntityOpenAspectSyntax::can_cast(&node) {
            return EntityAspectSyntax::EntityOpenAspect(EntityOpenAspectSyntax::cast_unchecked(
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
            TokenKind::Keyword(Kw::Entity) => Some(EntityClassSyntax::Entity(token)),
            TokenKind::Keyword(Kw::Architecture) => Some(EntityClassSyntax::Architecture(token)),
            TokenKind::Keyword(Kw::Configuration) => Some(EntityClassSyntax::Configuration(token)),
            TokenKind::Keyword(Kw::Procedure) => Some(EntityClassSyntax::Procedure(token)),
            TokenKind::Keyword(Kw::Function) => Some(EntityClassSyntax::Function(token)),
            TokenKind::Keyword(Kw::Package) => Some(EntityClassSyntax::Package(token)),
            TokenKind::Keyword(Kw::Type) => Some(EntityClassSyntax::Type(token)),
            TokenKind::Keyword(Kw::Subtype) => Some(EntityClassSyntax::Subtype(token)),
            TokenKind::Keyword(Kw::Constant) => Some(EntityClassSyntax::Constant(token)),
            TokenKind::Keyword(Kw::Signal) => Some(EntityClassSyntax::Signal(token)),
            TokenKind::Keyword(Kw::Variable) => Some(EntityClassSyntax::Variable(token)),
            TokenKind::Keyword(Kw::Component) => Some(EntityClassSyntax::Component(token)),
            TokenKind::Keyword(Kw::Label) => Some(EntityClassSyntax::Label(token)),
            TokenKind::Keyword(Kw::Literal) => Some(EntityClassSyntax::Literal(token)),
            TokenKind::Keyword(Kw::Units) => Some(EntityClassSyntax::Units(token)),
            TokenKind::Keyword(Kw::Group) => Some(EntityClassSyntax::Group(token)),
            TokenKind::Keyword(Kw::File) => Some(EntityClassSyntax::File(token)),
            TokenKind::Keyword(Kw::Property) => Some(EntityClassSyntax::Property(token)),
            TokenKind::Keyword(Kw::Sequence) => Some(EntityClassSyntax::Sequence(token)),
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::EntityDesignator,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "entity_tag",
                kind: LayoutItemKind::TokenChoice(&[
                    TokenKind::Identifier,
                    TokenKind::CharacterLiteral,
                    TokenKind::StringLiteral,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "signature",
                kind: LayoutItemKind::Node(NodeKind::Signature),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        EntityDesignatorSyntax(node)
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
pub struct EntityDesignatorListSyntax(pub(crate) SyntaxNode);
impl AstNode for EntityDesignatorListSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::EntityDesignatorList,
        items: &[
            LayoutItem {
                optional: false,
                repeated: true,
                name: "entity_designators",
                kind: LayoutItemKind::Node(NodeKind::EntityDesignator),
            },
            LayoutItem {
                optional: false,
                repeated: true,
                name: "comma",
                kind: LayoutItemKind::Token(TokenKind::Comma),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        EntityDesignatorListSyntax(node)
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
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Comma)
    }
}
#[derive(Debug, Clone)]
pub struct EntityNameListAllSyntax(pub(crate) SyntaxNode);
impl AstNode for EntityNameListAllSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::EntityNameListAll,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "all",
            kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::All)),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        EntityNameListAllSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl EntityNameListAllSyntax {
    pub fn all_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::All))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct EntityNameListOthersSyntax(pub(crate) SyntaxNode);
impl AstNode for EntityNameListOthersSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::EntityNameListOthers,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "others",
            kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Others)),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        EntityNameListOthersSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl EntityNameListOthersSyntax {
    pub fn others_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Others))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum EntityNameListSyntax {
    EntityDesignatorList(EntityDesignatorListSyntax),
    EntityNameListAll(EntityNameListAllSyntax),
    EntityNameListOthers(EntityNameListOthersSyntax),
}
impl AstNode for EntityNameListSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::EntityDesignatorList,
            NodeKind::EntityNameListAll,
            NodeKind::EntityNameListOthers,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if EntityDesignatorListSyntax::can_cast(&node) {
            return EntityNameListSyntax::EntityDesignatorList(
                EntityDesignatorListSyntax::cast_unchecked(node),
            );
        }
        if EntityNameListAllSyntax::can_cast(&node) {
            return EntityNameListSyntax::EntityNameListAll(
                EntityNameListAllSyntax::cast_unchecked(node),
            );
        }
        if EntityNameListOthersSyntax::can_cast(&node) {
            return EntityNameListSyntax::EntityNameListOthers(
                EntityNameListOthersSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            EntityNameListSyntax::EntityDesignatorList(inner) => inner.raw(),
            EntityNameListSyntax::EntityNameListAll(inner) => inner.raw(),
            EntityNameListSyntax::EntityNameListOthers(inner) => inner.raw(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct EntitySpecificationSyntax(pub(crate) SyntaxNode);
impl AstNode for EntitySpecificationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::EntitySpecification,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "entity_name_list",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::EntityDesignatorList,
                    NodeKind::EntityNameListAll,
                    NodeKind::EntityNameListOthers,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "colon",
                kind: LayoutItemKind::Token(TokenKind::Colon),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "entity_class",
                kind: LayoutItemKind::TokenChoice(&[
                    TokenKind::Keyword(Kw::Entity),
                    TokenKind::Keyword(Kw::Architecture),
                    TokenKind::Keyword(Kw::Configuration),
                    TokenKind::Keyword(Kw::Procedure),
                    TokenKind::Keyword(Kw::Function),
                    TokenKind::Keyword(Kw::Package),
                    TokenKind::Keyword(Kw::Type),
                    TokenKind::Keyword(Kw::Subtype),
                    TokenKind::Keyword(Kw::Constant),
                    TokenKind::Keyword(Kw::Signal),
                    TokenKind::Keyword(Kw::Variable),
                    TokenKind::Keyword(Kw::Component),
                    TokenKind::Keyword(Kw::Label),
                    TokenKind::Keyword(Kw::Literal),
                    TokenKind::Keyword(Kw::Units),
                    TokenKind::Keyword(Kw::Group),
                    TokenKind::Keyword(Kw::File),
                    TokenKind::Keyword(Kw::Property),
                    TokenKind::Keyword(Kw::Sequence),
                ]),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        EntitySpecificationSyntax(node)
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
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Colon)
            .nth(0)
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
            TokenKind::Identifier => Some(EntityTagSyntax::Identifier(token)),
            TokenKind::CharacterLiteral => Some(EntityTagSyntax::CharacterLiteral(token)),
            TokenKind::StringLiteral => Some(EntityTagSyntax::StringLiteral(token)),
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::GuardedSignalSpecification,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "signal_list",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::SignalListList,
                    NodeKind::SignalListAll,
                    NodeKind::SignalListOthers,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "colon",
                kind: LayoutItemKind::Token(TokenKind::Colon),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "name",
                kind: LayoutItemKind::Node(NodeKind::Name),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        GuardedSignalSpecificationSyntax(node)
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
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Colon)
            .nth(0)
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct InstantiationListListSyntax(pub(crate) SyntaxNode);
impl AstNode for InstantiationListListSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::InstantiationListList,
        items: &[
            LayoutItem {
                optional: false,
                repeated: true,
                name: "identifier",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
            LayoutItem {
                optional: false,
                repeated: true,
                name: "comma",
                kind: LayoutItemKind::Token(TokenKind::Comma),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        InstantiationListListSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InstantiationListListSyntax {
    pub fn identifier_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Identifier)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Comma)
    }
}
#[derive(Debug, Clone)]
pub struct InstantiationListAllSyntax(pub(crate) SyntaxNode);
impl AstNode for InstantiationListAllSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::InstantiationListAll,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "all",
            kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::All)),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        InstantiationListAllSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InstantiationListAllSyntax {
    pub fn all_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::All))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct InstantiationListOthersSyntax(pub(crate) SyntaxNode);
impl AstNode for InstantiationListOthersSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::InstantiationListOthers,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "others",
            kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Others)),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        InstantiationListOthersSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InstantiationListOthersSyntax {
    pub fn others_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Others))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum InstantiationListSyntax {
    InstantiationListList(InstantiationListListSyntax),
    InstantiationListAll(InstantiationListAllSyntax),
    InstantiationListOthers(InstantiationListOthersSyntax),
}
impl AstNode for InstantiationListSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::InstantiationListList,
            NodeKind::InstantiationListAll,
            NodeKind::InstantiationListOthers,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if InstantiationListListSyntax::can_cast(&node) {
            return InstantiationListSyntax::InstantiationListList(
                InstantiationListListSyntax::cast_unchecked(node),
            );
        }
        if InstantiationListAllSyntax::can_cast(&node) {
            return InstantiationListSyntax::InstantiationListAll(
                InstantiationListAllSyntax::cast_unchecked(node),
            );
        }
        if InstantiationListOthersSyntax::can_cast(&node) {
            return InstantiationListSyntax::InstantiationListOthers(
                InstantiationListOthersSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            InstantiationListSyntax::InstantiationListList(inner) => inner.raw(),
            InstantiationListSyntax::InstantiationListAll(inner) => inner.raw(),
            InstantiationListSyntax::InstantiationListOthers(inner) => inner.raw(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct SignalListListSyntax(pub(crate) SyntaxNode);
impl AstNode for SignalListListSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SignalListList,
        items: &[
            LayoutItem {
                optional: false,
                repeated: true,
                name: "names",
                kind: LayoutItemKind::Node(NodeKind::Name),
            },
            LayoutItem {
                optional: false,
                repeated: true,
                name: "comma",
                kind: LayoutItemKind::Token(TokenKind::Comma),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        SignalListListSyntax(node)
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
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Comma)
    }
}
#[derive(Debug, Clone)]
pub struct SignalListAllSyntax(pub(crate) SyntaxNode);
impl AstNode for SignalListAllSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SignalListAll,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "all",
            kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::All)),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        SignalListAllSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SignalListAllSyntax {
    pub fn all_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::All))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct SignalListOthersSyntax(pub(crate) SyntaxNode);
impl AstNode for SignalListOthersSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SignalListOthers,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "others",
            kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Others)),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        SignalListOthersSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SignalListOthersSyntax {
    pub fn others_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Others))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum SignalListSyntax {
    SignalListList(SignalListListSyntax),
    SignalListAll(SignalListAllSyntax),
    SignalListOthers(SignalListOthersSyntax),
}
impl AstNode for SignalListSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::SignalListList,
            NodeKind::SignalListAll,
            NodeKind::SignalListOthers,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if SignalListListSyntax::can_cast(&node) {
            return SignalListSyntax::SignalListList(SignalListListSyntax::cast_unchecked(node));
        }
        if SignalListAllSyntax::can_cast(&node) {
            return SignalListSyntax::SignalListAll(SignalListAllSyntax::cast_unchecked(node));
        }
        if SignalListOthersSyntax::can_cast(&node) {
            return SignalListSyntax::SignalListOthers(SignalListOthersSyntax::cast_unchecked(
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
            SignalListSyntax::SignalListList(inner) => inner.raw(),
            SignalListSyntax::SignalListAll(inner) => inner.raw(),
            SignalListSyntax::SignalListOthers(inner) => inner.raw(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct SimpleConfigurationSpecificationSyntax(pub(crate) SyntaxNode);
impl AstNode for SimpleConfigurationSpecificationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SimpleConfigurationSpecification,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "component_configuration_preamble",
                kind: LayoutItemKind::Node(NodeKind::ComponentConfigurationPreamble),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "semi_colon_terminated_binding_indication",
                kind: LayoutItemKind::Node(NodeKind::SemiColonTerminatedBindingIndication),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "semi_colon",
                kind: LayoutItemKind::Token(TokenKind::SemiColon),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "component_configuration_epilogue",
                kind: LayoutItemKind::Node(NodeKind::ComponentConfigurationEpilogue),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        SimpleConfigurationSpecificationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SimpleConfigurationSpecificationSyntax {
    pub fn component_configuration_preamble(&self) -> Option<ComponentConfigurationPreambleSyntax> {
        self.0
            .children()
            .filter_map(ComponentConfigurationPreambleSyntax::cast)
            .nth(0)
    }
    pub fn semi_colon_terminated_binding_indication(
        &self,
    ) -> Option<SemiColonTerminatedBindingIndicationSyntax> {
        self.0
            .children()
            .filter_map(SemiColonTerminatedBindingIndicationSyntax::cast)
            .nth(0)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
    pub fn component_configuration_epilogue(&self) -> Option<ComponentConfigurationEpilogueSyntax> {
        self.0
            .children()
            .filter_map(ComponentConfigurationEpilogueSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct VerificationUnitBindingIndicationSyntax(pub(crate) SyntaxNode);
impl AstNode for VerificationUnitBindingIndicationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::VerificationUnitBindingIndication,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "use",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Use)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "vunit",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Vunit)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "verification_unit_list",
                kind: LayoutItemKind::Node(NodeKind::VerificationUnitList),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        VerificationUnitBindingIndicationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl VerificationUnitBindingIndicationSyntax {
    pub fn use_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Use))
            .nth(0)
    }
    pub fn vunit_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Vunit))
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::VerificationUnitList,
        items: &[
            LayoutItem {
                optional: false,
                repeated: true,
                name: "names",
                kind: LayoutItemKind::Node(NodeKind::Name),
            },
            LayoutItem {
                optional: false,
                repeated: true,
                name: "comma",
                kind: LayoutItemKind::Token(TokenKind::Comma),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        VerificationUnitListSyntax(node)
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
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Comma)
    }
}
