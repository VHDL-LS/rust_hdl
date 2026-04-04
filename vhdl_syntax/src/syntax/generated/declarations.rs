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
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::SubprogramDeclaration,
            NodeKind::SubprogramBody,
            NodeKind::SubprogramInstantiationDeclaration,
            NodeKind::PackageDeclaration,
            NodeKind::PackageBodyDeclaration,
            NodeKind::PackageInstantiationDeclaration,
            NodeKind::FullTypeDeclaration,
            NodeKind::IncompleteTypeDeclaration,
            NodeKind::SubtypeDeclaration,
            NodeKind::FileDeclaration,
            NodeKind::AliasDeclaration,
            NodeKind::ComponentDeclaration,
            NodeKind::AttributeDeclaration,
            NodeKind::AttributeSpecification,
            NodeKind::SimpleConfigurationSpecification,
            NodeKind::CompoundConfigurationSpecification,
            NodeKind::DisconnectionSpecification,
            NodeKind::UseClauseDeclaration,
            NodeKind::GroupTemplateDeclaration,
            NodeKind::GroupDeclaration,
            NodeKind::ConstantDeclaration,
            NodeKind::SignalDeclaration,
            NodeKind::VariableDeclaration,
            NodeKind::SharedVariableDeclaration,
            NodeKind::PslPropertyDeclaration,
            NodeKind::PslSequenceDeclaration,
            NodeKind::PslClockDeclaration,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if SubprogramDeclarationSyntax::can_cast(&node) {
            return DeclarationSyntax::SubprogramDeclaration(
                SubprogramDeclarationSyntax::cast_unchecked(node),
            );
        }
        if SubprogramBodySyntax::can_cast(&node) {
            return DeclarationSyntax::SubprogramBody(SubprogramBodySyntax::cast_unchecked(node));
        }
        if SubprogramInstantiationDeclarationSyntax::can_cast(&node) {
            return DeclarationSyntax::SubprogramInstantiationDeclaration(
                SubprogramInstantiationDeclarationSyntax::cast_unchecked(node),
            );
        }
        if PackageDeclarationSyntax::can_cast(&node) {
            return DeclarationSyntax::PackageDeclaration(
                PackageDeclarationSyntax::cast_unchecked(node),
            );
        }
        if PackageBodyDeclarationSyntax::can_cast(&node) {
            return DeclarationSyntax::PackageBodyDeclaration(
                PackageBodyDeclarationSyntax::cast_unchecked(node),
            );
        }
        if PackageInstantiationDeclarationSyntax::can_cast(&node) {
            return DeclarationSyntax::PackageInstantiationDeclaration(
                PackageInstantiationDeclarationSyntax::cast_unchecked(node),
            );
        }
        if TypeDeclarationSyntax::can_cast(&node) {
            return DeclarationSyntax::TypeDeclaration(TypeDeclarationSyntax::cast_unchecked(node));
        }
        if SubtypeDeclarationSyntax::can_cast(&node) {
            return DeclarationSyntax::SubtypeDeclaration(
                SubtypeDeclarationSyntax::cast_unchecked(node),
            );
        }
        if FileDeclarationSyntax::can_cast(&node) {
            return DeclarationSyntax::FileDeclaration(FileDeclarationSyntax::cast_unchecked(node));
        }
        if AliasDeclarationSyntax::can_cast(&node) {
            return DeclarationSyntax::AliasDeclaration(AliasDeclarationSyntax::cast_unchecked(
                node,
            ));
        }
        if ComponentDeclarationSyntax::can_cast(&node) {
            return DeclarationSyntax::ComponentDeclaration(
                ComponentDeclarationSyntax::cast_unchecked(node),
            );
        }
        if AttributeDeclarationSyntax::can_cast(&node) {
            return DeclarationSyntax::AttributeDeclaration(
                AttributeDeclarationSyntax::cast_unchecked(node),
            );
        }
        if AttributeSpecificationSyntax::can_cast(&node) {
            return DeclarationSyntax::AttributeSpecification(
                AttributeSpecificationSyntax::cast_unchecked(node),
            );
        }
        if ConfigurationSpecificationSyntax::can_cast(&node) {
            return DeclarationSyntax::ConfigurationSpecification(
                ConfigurationSpecificationSyntax::cast_unchecked(node),
            );
        }
        if DisconnectionSpecificationSyntax::can_cast(&node) {
            return DeclarationSyntax::DisconnectionSpecification(
                DisconnectionSpecificationSyntax::cast_unchecked(node),
            );
        }
        if UseClauseDeclarationSyntax::can_cast(&node) {
            return DeclarationSyntax::UseClauseDeclaration(
                UseClauseDeclarationSyntax::cast_unchecked(node),
            );
        }
        if GroupTemplateDeclarationSyntax::can_cast(&node) {
            return DeclarationSyntax::GroupTemplateDeclaration(
                GroupTemplateDeclarationSyntax::cast_unchecked(node),
            );
        }
        if GroupDeclarationSyntax::can_cast(&node) {
            return DeclarationSyntax::GroupDeclaration(GroupDeclarationSyntax::cast_unchecked(
                node,
            ));
        }
        if ConstantDeclarationSyntax::can_cast(&node) {
            return DeclarationSyntax::ConstantDeclaration(
                ConstantDeclarationSyntax::cast_unchecked(node),
            );
        }
        if SignalDeclarationSyntax::can_cast(&node) {
            return DeclarationSyntax::SignalDeclaration(SignalDeclarationSyntax::cast_unchecked(
                node,
            ));
        }
        if VariableDeclarationSyntax::can_cast(&node) {
            return DeclarationSyntax::VariableDeclaration(
                VariableDeclarationSyntax::cast_unchecked(node),
            );
        }
        if SharedVariableDeclarationSyntax::can_cast(&node) {
            return DeclarationSyntax::SharedVariableDeclaration(
                SharedVariableDeclarationSyntax::cast_unchecked(node),
            );
        }
        if PslPropertyDeclarationSyntax::can_cast(&node) {
            return DeclarationSyntax::PslPropertyDeclaration(
                PslPropertyDeclarationSyntax::cast_unchecked(node),
            );
        }
        if PslSequenceDeclarationSyntax::can_cast(&node) {
            return DeclarationSyntax::PslSequenceDeclaration(
                PslSequenceDeclarationSyntax::cast_unchecked(node),
            );
        }
        if PslClockDeclarationSyntax::can_cast(&node) {
            return DeclarationSyntax::PslClockDeclaration(
                PslClockDeclarationSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PackageDeclaration,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "package",
            kind: LayoutItemKind::Node(NodeKind::Package),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        PackageDeclarationSyntax(node)
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PackageInstantiationDeclaration,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "package_instantiation",
            kind: LayoutItemKind::Node(NodeKind::PackageInstantiation),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        PackageInstantiationDeclarationSyntax(node)
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PackageBodyDeclaration,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "package_body",
            kind: LayoutItemKind::Node(NodeKind::PackageBody),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        PackageBodyDeclarationSyntax(node)
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::UseClauseDeclaration,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "use_clause",
            kind: LayoutItemKind::Node(NodeKind::UseClause),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        UseClauseDeclarationSyntax(node)
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ActualPart,
        items: &[],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ActualPartSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
#[derive(Debug, Clone)]
pub struct AliasDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for AliasDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::AliasDeclaration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "alias",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Alias)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "alias_designator",
                kind: LayoutItemKind::TokenGroup(&[
                    TokenKind::Identifier,
                    TokenKind::CharacterLiteral,
                    TokenKind::StringLiteral,
                ]),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "colon",
                kind: LayoutItemKind::Token(TokenKind::Colon),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "subtype_indication",
                kind: LayoutItemKind::Node(NodeKind::SubtypeIndication),
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
                name: "name",
                kind: LayoutItemKind::Node(NodeKind::Name),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "signature",
                kind: LayoutItemKind::Node(NodeKind::Signature),
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
        AliasDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl AliasDeclarationSyntax {
    pub fn alias_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Alias))
            .nth(0)
    }
    pub fn alias_designator(&self) -> Option<AliasDesignatorSyntax> {
        self.0
            .tokens()
            .filter_map(AliasDesignatorSyntax::cast)
            .nth(0)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Colon)
            .nth(0)
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
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Is))
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
            .filter(|token| token.kind() == TokenKind::SemiColon)
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
            TokenKind::Identifier => Some(AliasDesignatorSyntax::Identifier(token)),
            TokenKind::CharacterLiteral => Some(AliasDesignatorSyntax::CharacterLiteral(token)),
            TokenKind::StringLiteral => Some(AliasDesignatorSyntax::StringLiteral(token)),
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::AssociationElement,
        items: &[
            LayoutItem {
                optional: true,
                repeated: false,
                name: "formal_part",
                kind: LayoutItemKind::Node(NodeKind::FormalPart),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "right_arrow",
                kind: LayoutItemKind::Token(TokenKind::RightArrow),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "actual_part",
                kind: LayoutItemKind::Node(NodeKind::ActualPart),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        AssociationElementSyntax(node)
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
            .filter(|token| token.kind() == TokenKind::RightArrow)
            .nth(0)
    }
    pub fn actual_part(&self) -> Option<ActualPartSyntax> {
        self.0.children().filter_map(ActualPartSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct AssociationListSyntax(pub(crate) SyntaxNode);
impl AstNode for AssociationListSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::AssociationList,
        items: &[
            LayoutItem {
                optional: false,
                repeated: true,
                name: "association_elements",
                kind: LayoutItemKind::Node(NodeKind::AssociationElement),
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
        AssociationListSyntax(node)
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
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Comma)
    }
}
#[derive(Debug, Clone)]
pub struct AttributeDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for AttributeDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::AttributeDeclaration,
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
                name: "identifier",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
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
            LayoutItem {
                optional: false,
                repeated: false,
                name: "semi_colon",
                kind: LayoutItemKind::Token(TokenKind::SemiColon),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        AttributeDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl AttributeDeclarationSyntax {
    pub fn attribute_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Attribute))
            .nth(0)
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Identifier)
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
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ComponentDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for ComponentDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ComponentDeclaration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "component_declaration_preamble",
                kind: LayoutItemKind::Node(NodeKind::ComponentDeclarationPreamble),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "component_declaration_items",
                kind: LayoutItemKind::Node(NodeKind::ComponentDeclarationItems),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "component_declaration_epilogue",
                kind: LayoutItemKind::Node(NodeKind::ComponentDeclarationEpilogue),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ComponentDeclarationSyntax(node)
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ComponentDeclarationItems,
        items: &[
            LayoutItem {
                optional: true,
                repeated: false,
                name: "generic_clause",
                kind: LayoutItemKind::Node(NodeKind::GenericClause),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "port_clause",
                kind: LayoutItemKind::Node(NodeKind::PortClause),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ComponentDeclarationItemsSyntax(node)
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ComponentDeclarationPreamble,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "component",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Component)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "name",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "is",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Is)),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ComponentDeclarationPreambleSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ComponentDeclarationPreambleSyntax {
    pub fn component_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Component))
            .nth(0)
    }
    pub fn name_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Identifier)
            .nth(0)
    }
    pub fn is_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Is))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ComponentDeclarationEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for ComponentDeclarationEpilogueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ComponentDeclarationEpilogue,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "end",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::End)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "component",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Component)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "identifier",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
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
        ComponentDeclarationEpilogueSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ComponentDeclarationEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::End))
            .nth(0)
    }
    pub fn component_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Component))
            .nth(0)
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Identifier)
            .nth(0)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ConstantDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for ConstantDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ConstantDeclaration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "constant",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Constant)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "identifier_list",
                kind: LayoutItemKind::Node(NodeKind::IdentifierList),
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
                name: "subtype_indication",
                kind: LayoutItemKind::Node(NodeKind::SubtypeIndication),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "colon_eq",
                kind: LayoutItemKind::Token(TokenKind::ColonEq),
            },
            LayoutItem {
                optional: true,
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
        ConstantDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConstantDeclarationSyntax {
    pub fn constant_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Constant))
            .nth(0)
    }
    pub fn identifier_list(&self) -> Option<IdentifierListSyntax> {
        self.0
            .children()
            .filter_map(IdentifierListSyntax::cast)
            .nth(0)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Colon)
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
            .filter(|token| token.kind() == TokenKind::ColonEq)
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
pub struct RangeConstraintConstraintSyntax(pub(crate) SyntaxNode);
impl AstNode for RangeConstraintConstraintSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::RangeConstraintConstraint,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "range_constraint",
            kind: LayoutItemKind::Node(NodeKind::RangeConstraint),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        RangeConstraintConstraintSyntax(node)
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
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::RangeConstraintConstraint,
            NodeKind::ArrayConstraint,
            NodeKind::RecordConstraint,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if RangeConstraintConstraintSyntax::can_cast(&node) {
            return ConstraintSyntax::RangeConstraintConstraint(
                RangeConstraintConstraintSyntax::cast_unchecked(node),
            );
        }
        if ArrayConstraintSyntax::can_cast(&node) {
            return ConstraintSyntax::ArrayConstraint(ArrayConstraintSyntax::cast_unchecked(node));
        }
        if RecordConstraintSyntax::can_cast(&node) {
            return ConstraintSyntax::RecordConstraint(RecordConstraintSyntax::cast_unchecked(
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
            ConstraintSyntax::RangeConstraintConstraint(inner) => inner.raw(),
            ConstraintSyntax::ArrayConstraint(inner) => inner.raw(),
            ConstraintSyntax::RecordConstraint(inner) => inner.raw(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct ResolutionIndicationElementResolutionSyntax(pub(crate) SyntaxNode);
impl AstNode for ResolutionIndicationElementResolutionSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ResolutionIndicationElementResolution,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "resolution_indication",
            kind: LayoutItemKind::NodeChoice(&[
                NodeKind::NameResolutionIndication,
                NodeKind::ParenthesizedElementResolutionResolutionIndication,
            ]),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ResolutionIndicationElementResolutionSyntax(node)
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::RecordResolutionElementResolution,
        items: &[LayoutItem {
            optional: true,
            repeated: false,
            name: "record_resolution",
            kind: LayoutItemKind::Node(NodeKind::RecordResolution),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        RecordResolutionElementResolutionSyntax(node)
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
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::ResolutionIndicationElementResolution,
            NodeKind::RecordResolutionElementResolution,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if ResolutionIndicationElementResolutionSyntax::can_cast(&node) {
            return ElementResolutionSyntax::ResolutionIndicationElementResolution(
                ResolutionIndicationElementResolutionSyntax::cast_unchecked(node),
            );
        }
        if RecordResolutionElementResolutionSyntax::can_cast(&node) {
            return ElementResolutionSyntax::RecordResolutionElementResolution(
                RecordResolutionElementResolutionSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::EntityClassEntry,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "entity_class",
                kind: LayoutItemKind::TokenGroup(&[
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
            LayoutItem {
                optional: true,
                repeated: false,
                name: "box",
                kind: LayoutItemKind::Token(TokenKind::BOX),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        EntityClassEntrySyntax(node)
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
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::BOX)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct EntityClassEntryListSyntax(pub(crate) SyntaxNode);
impl AstNode for EntityClassEntryListSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::EntityClassEntryList,
        items: &[
            LayoutItem {
                optional: false,
                repeated: true,
                name: "entity_class_entrys",
                kind: LayoutItemKind::Node(NodeKind::EntityClassEntry),
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
        EntityClassEntryListSyntax(node)
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
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Comma)
    }
}
#[derive(Debug, Clone)]
pub struct FileDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for FileDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::FileDeclaration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "file",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::File)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "identifier_list",
                kind: LayoutItemKind::Node(NodeKind::IdentifierList),
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
                name: "subtype_indication",
                kind: LayoutItemKind::Node(NodeKind::SubtypeIndication),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "file_open_information",
                kind: LayoutItemKind::Node(NodeKind::FileOpenInformation),
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
        FileDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl FileDeclarationSyntax {
    pub fn file_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::File))
            .nth(0)
    }
    pub fn identifier_list(&self) -> Option<IdentifierListSyntax> {
        self.0
            .children()
            .filter_map(IdentifierListSyntax::cast)
            .nth(0)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Colon)
            .nth(0)
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
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct FileOpenInformationSyntax(pub(crate) SyntaxNode);
impl AstNode for FileOpenInformationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::FileOpenInformation,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "open",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Open)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "file_open_kind",
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
                name: "is",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Is)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "file_logical_name",
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
        FileOpenInformationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl FileOpenInformationSyntax {
    pub fn open_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Open))
            .nth(0)
    }
    pub fn file_open_kind(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn is_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Is))
            .nth(0)
    }
    pub fn file_logical_name(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(1)
    }
}
#[derive(Debug, Clone)]
pub struct ParenthesizedNameSyntax(pub(crate) SyntaxNode);
impl AstNode for ParenthesizedNameSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ParenthesizedName,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "left_par",
                kind: LayoutItemKind::Token(TokenKind::LeftPar),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "name",
                kind: LayoutItemKind::Node(NodeKind::Name),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "right_par",
                kind: LayoutItemKind::Token(TokenKind::RightPar),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ParenthesizedNameSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ParenthesizedNameSyntax {
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::LeftPar)
            .nth(0)
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
    pub fn right_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::RightPar)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct FormalPartSyntax(pub(crate) SyntaxNode);
impl AstNode for FormalPartSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::FormalPart,
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
                name: "parenthesized_name",
                kind: LayoutItemKind::Node(NodeKind::ParenthesizedName),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        FormalPartSyntax(node)
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::FullTypeDeclaration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "type",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Type)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "identifier",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
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
                name: "type_definition",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::EnumerationTypeDefinition,
                    NodeKind::NumericTypeDefinition,
                    NodeKind::PhysicalTypeDefinition,
                    NodeKind::UnboundedArrayDefinition,
                    NodeKind::ConstrainedArrayDefinition,
                    NodeKind::RecordTypeDefinition,
                    NodeKind::AccessTypeDefinition,
                    NodeKind::FileTypeDefinition,
                    NodeKind::ProtectedTypeDeclaration,
                    NodeKind::ProtectedTypeBody,
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
        FullTypeDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl FullTypeDeclarationSyntax {
    pub fn type_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Type))
            .nth(0)
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Identifier)
            .nth(0)
    }
    pub fn is_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Is))
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
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct GenericClauseSyntax(pub(crate) SyntaxNode);
impl AstNode for GenericClauseSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::GenericClause,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "generic_clause_preamble",
                kind: LayoutItemKind::Node(NodeKind::GenericClausePreamble),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "interface_list",
                kind: LayoutItemKind::Node(NodeKind::InterfaceList),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "generic_clause_epilogue",
                kind: LayoutItemKind::Node(NodeKind::GenericClauseEpilogue),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        GenericClauseSyntax(node)
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::GenericClausePreamble,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "generic",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Generic)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "left_par",
                kind: LayoutItemKind::Token(TokenKind::LeftPar),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        GenericClausePreambleSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl GenericClausePreambleSyntax {
    pub fn generic_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Generic))
            .nth(0)
    }
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::LeftPar)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct GenericClauseEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for GenericClauseEpilogueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::GenericClauseEpilogue,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "right_par",
                kind: LayoutItemKind::Token(TokenKind::RightPar),
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
        GenericClauseEpilogueSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl GenericClauseEpilogueSyntax {
    pub fn right_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::RightPar)
            .nth(0)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct GenericMapAspectSyntax(pub(crate) SyntaxNode);
impl AstNode for GenericMapAspectSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::GenericMapAspect,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "generic",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Generic)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "map",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Map)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "left_par",
                kind: LayoutItemKind::Token(TokenKind::LeftPar),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "association_list",
                kind: LayoutItemKind::Node(NodeKind::AssociationList),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "right_par",
                kind: LayoutItemKind::Token(TokenKind::RightPar),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        GenericMapAspectSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl GenericMapAspectSyntax {
    pub fn generic_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Generic))
            .nth(0)
    }
    pub fn map_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Map))
            .nth(0)
    }
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::LeftPar)
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
            .filter(|token| token.kind() == TokenKind::RightPar)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct GroupConstituentListSyntax(pub(crate) SyntaxNode);
impl AstNode for GroupConstituentListSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::GroupConstituentList,
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
        GroupConstituentListSyntax(node)
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
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Comma)
    }
}
#[derive(Debug, Clone)]
pub struct GroupDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for GroupDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::GroupDeclaration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "group",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Group)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "identifier",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
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
            LayoutItem {
                optional: false,
                repeated: false,
                name: "left_par",
                kind: LayoutItemKind::Token(TokenKind::LeftPar),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "group_constituent_list",
                kind: LayoutItemKind::Node(NodeKind::GroupConstituentList),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "right_par",
                kind: LayoutItemKind::Token(TokenKind::RightPar),
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
        GroupDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl GroupDeclarationSyntax {
    pub fn group_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Group))
            .nth(0)
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Identifier)
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
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::LeftPar)
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
            .filter(|token| token.kind() == TokenKind::RightPar)
            .nth(0)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct GroupTemplateDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for GroupTemplateDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::GroupTemplateDeclaration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "group",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Group)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "identifier",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
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
                name: "left_par",
                kind: LayoutItemKind::Token(TokenKind::LeftPar),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "entity_class_entry_list",
                kind: LayoutItemKind::Node(NodeKind::EntityClassEntryList),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "right_par",
                kind: LayoutItemKind::Token(TokenKind::RightPar),
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
        GroupTemplateDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl GroupTemplateDeclarationSyntax {
    pub fn group_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Group))
            .nth(0)
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Identifier)
            .nth(0)
    }
    pub fn is_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Is))
            .nth(0)
    }
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::LeftPar)
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
            .filter(|token| token.kind() == TokenKind::RightPar)
            .nth(0)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct InterfaceConstantDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfaceConstantDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::InterfaceConstantDeclaration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "constant",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Constant)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "identifier_list",
                kind: LayoutItemKind::Node(NodeKind::IdentifierList),
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
                name: "in",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::In)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "subtype_indication",
                kind: LayoutItemKind::Node(NodeKind::SubtypeIndication),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "colon_eq",
                kind: LayoutItemKind::Token(TokenKind::ColonEq),
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
        InterfaceConstantDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfaceConstantDeclarationSyntax {
    pub fn constant_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Constant))
            .nth(0)
    }
    pub fn identifier_list(&self) -> Option<IdentifierListSyntax> {
        self.0
            .children()
            .filter_map(IdentifierListSyntax::cast)
            .nth(0)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Colon)
            .nth(0)
    }
    pub fn in_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::In))
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
            .filter(|token| token.kind() == TokenKind::ColonEq)
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
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::InterfaceConstantDeclaration,
            NodeKind::InterfaceSignalDeclaration,
            NodeKind::InterfaceVariableDeclaration,
            NodeKind::InterfaceFileDeclaration,
            NodeKind::InterfaceIncompleteTypeDeclaration,
            NodeKind::InterfaceSubprogramDeclaration,
            NodeKind::InterfacePackageDeclaration,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if InterfaceObjectDeclarationSyntax::can_cast(&node) {
            return InterfaceDeclarationSyntax::InterfaceObjectDeclaration(
                InterfaceObjectDeclarationSyntax::cast_unchecked(node),
            );
        }
        if InterfaceIncompleteTypeDeclarationSyntax::can_cast(&node) {
            return InterfaceDeclarationSyntax::InterfaceIncompleteTypeDeclaration(
                InterfaceIncompleteTypeDeclarationSyntax::cast_unchecked(node),
            );
        }
        if InterfaceSubprogramDeclarationSyntax::can_cast(&node) {
            return InterfaceDeclarationSyntax::InterfaceSubprogramDeclaration(
                InterfaceSubprogramDeclarationSyntax::cast_unchecked(node),
            );
        }
        if InterfacePackageDeclarationSyntax::can_cast(&node) {
            return InterfaceDeclarationSyntax::InterfacePackageDeclaration(
                InterfacePackageDeclarationSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::InterfaceFileDeclaration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "file",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::File)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "identifier_list",
                kind: LayoutItemKind::Node(NodeKind::IdentifierList),
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
                name: "subtype_indication",
                kind: LayoutItemKind::Node(NodeKind::SubtypeIndication),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        InterfaceFileDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfaceFileDeclarationSyntax {
    pub fn file_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::File))
            .nth(0)
    }
    pub fn identifier_list(&self) -> Option<IdentifierListSyntax> {
        self.0
            .children()
            .filter_map(IdentifierListSyntax::cast)
            .nth(0)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Colon)
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
pub struct InterfaceFunctionSpecificationSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfaceFunctionSpecificationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::InterfaceFunctionSpecification,
        items: &[
            LayoutItem {
                optional: true,
                repeated: false,
                name: "function_purity",
                kind: LayoutItemKind::TokenGroup(&[
                    TokenKind::Keyword(Kw::Pure),
                    TokenKind::Keyword(Kw::Impure),
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "function",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Function)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "designator",
                kind: LayoutItemKind::TokenGroup(&[
                    TokenKind::Identifier,
                    TokenKind::StringLiteral,
                ]),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "parameter_list",
                kind: LayoutItemKind::Node(NodeKind::ParameterList),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "return",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Return)),
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
        InterfaceFunctionSpecificationSyntax(node)
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
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Function))
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
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Return))
            .nth(0)
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct InterfaceIncompleteTypeDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfaceIncompleteTypeDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::InterfaceIncompleteTypeDeclaration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "type",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Type)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "identifier",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
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
        InterfaceIncompleteTypeDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfaceIncompleteTypeDeclarationSyntax {
    pub fn type_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Type))
            .nth(0)
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Identifier)
            .nth(0)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct InterfaceListSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfaceListSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::InterfaceList,
        items: &[
            LayoutItem {
                optional: false,
                repeated: true,
                name: "interface_declarations",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::InterfaceConstantDeclaration,
                    NodeKind::InterfaceSignalDeclaration,
                    NodeKind::InterfaceVariableDeclaration,
                    NodeKind::InterfaceFileDeclaration,
                    NodeKind::InterfaceIncompleteTypeDeclaration,
                    NodeKind::InterfaceSubprogramDeclaration,
                    NodeKind::InterfacePackageDeclaration,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: true,
                name: "semi_colon",
                kind: LayoutItemKind::Token(TokenKind::SemiColon),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        InterfaceListSyntax(node)
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
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::SemiColon)
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
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::InterfaceConstantDeclaration,
            NodeKind::InterfaceSignalDeclaration,
            NodeKind::InterfaceVariableDeclaration,
            NodeKind::InterfaceFileDeclaration,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if InterfaceConstantDeclarationSyntax::can_cast(&node) {
            return InterfaceObjectDeclarationSyntax::InterfaceConstantDeclaration(
                InterfaceConstantDeclarationSyntax::cast_unchecked(node),
            );
        }
        if InterfaceSignalDeclarationSyntax::can_cast(&node) {
            return InterfaceObjectDeclarationSyntax::InterfaceSignalDeclaration(
                InterfaceSignalDeclarationSyntax::cast_unchecked(node),
            );
        }
        if InterfaceVariableDeclarationSyntax::can_cast(&node) {
            return InterfaceObjectDeclarationSyntax::InterfaceVariableDeclaration(
                InterfaceVariableDeclarationSyntax::cast_unchecked(node),
            );
        }
        if InterfaceFileDeclarationSyntax::can_cast(&node) {
            return InterfaceObjectDeclarationSyntax::InterfaceFileDeclaration(
                InterfaceFileDeclarationSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::InterfacePackageDeclaration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "interface_package_declaration_preamble",
                kind: LayoutItemKind::Node(NodeKind::InterfacePackageDeclarationPreamble),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "new",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::New)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "name",
                kind: LayoutItemKind::Node(NodeKind::Name),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "interface_package_generic_map_aspect",
                kind: LayoutItemKind::Node(NodeKind::InterfacePackageGenericMapAspect),
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
        InterfacePackageDeclarationSyntax(node)
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
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::New))
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
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct InterfacePackageDeclarationPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfacePackageDeclarationPreambleSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::InterfacePackageDeclarationPreamble,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "package",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Package)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "identifier",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "is",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Is)),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        InterfacePackageDeclarationPreambleSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfacePackageDeclarationPreambleSyntax {
    pub fn package_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Package))
            .nth(0)
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Identifier)
            .nth(0)
    }
    pub fn is_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Is))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct InterfacePackageGenericMapAspectSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfacePackageGenericMapAspectSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::InterfacePackageGenericMapAspect,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "generic",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Generic)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "map",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Map)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "left_par",
                kind: LayoutItemKind::Token(TokenKind::LeftPar),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "interface_package_generic_map_aspect_inner",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::InterfacePackageGenericMapAspectBox,
                    NodeKind::InterfacePackageGenericMapAspectDefault,
                    NodeKind::InterfacePackageGenericMapAspectAssociations,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "right_par",
                kind: LayoutItemKind::Token(TokenKind::RightPar),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        InterfacePackageGenericMapAspectSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfacePackageGenericMapAspectSyntax {
    pub fn generic_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Generic))
            .nth(0)
    }
    pub fn map_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Map))
            .nth(0)
    }
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::LeftPar)
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
            .filter(|token| token.kind() == TokenKind::RightPar)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct InterfacePackageGenericMapAspectBoxSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfacePackageGenericMapAspectBoxSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::InterfacePackageGenericMapAspectBox,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "box",
            kind: LayoutItemKind::Token(TokenKind::BOX),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        InterfacePackageGenericMapAspectBoxSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfacePackageGenericMapAspectBoxSyntax {
    pub fn box_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::BOX)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct InterfacePackageGenericMapAspectDefaultSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfacePackageGenericMapAspectDefaultSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::InterfacePackageGenericMapAspectDefault,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "default",
            kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Default)),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        InterfacePackageGenericMapAspectDefaultSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfacePackageGenericMapAspectDefaultSyntax {
    pub fn default_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Default))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct InterfacePackageGenericMapAspectAssociationsSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfacePackageGenericMapAspectAssociationsSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::InterfacePackageGenericMapAspectAssociations,
        items: &[LayoutItem {
            optional: true,
            repeated: false,
            name: "association_list",
            kind: LayoutItemKind::Node(NodeKind::AssociationList),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        InterfacePackageGenericMapAspectAssociationsSyntax(node)
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
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::InterfacePackageGenericMapAspectBox,
            NodeKind::InterfacePackageGenericMapAspectDefault,
            NodeKind::InterfacePackageGenericMapAspectAssociations,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if InterfacePackageGenericMapAspectBoxSyntax::can_cast(&node) {
            return InterfacePackageGenericMapAspectInnerSyntax :: InterfacePackageGenericMapAspectBox (InterfacePackageGenericMapAspectBoxSyntax :: cast_unchecked (node)) ;
        }
        if InterfacePackageGenericMapAspectDefaultSyntax::can_cast(&node) {
            return InterfacePackageGenericMapAspectInnerSyntax :: InterfacePackageGenericMapAspectDefault (InterfacePackageGenericMapAspectDefaultSyntax :: cast_unchecked (node)) ;
        }
        if InterfacePackageGenericMapAspectAssociationsSyntax::can_cast(&node) {
            return InterfacePackageGenericMapAspectInnerSyntax :: InterfacePackageGenericMapAspectAssociations (InterfacePackageGenericMapAspectAssociationsSyntax :: cast_unchecked (node)) ;
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
    }
    fn raw(&self) -> SyntaxNode {
        match self { InterfacePackageGenericMapAspectInnerSyntax :: InterfacePackageGenericMapAspectBox (inner) => inner . raw () , InterfacePackageGenericMapAspectInnerSyntax :: InterfacePackageGenericMapAspectDefault (inner) => inner . raw () , InterfacePackageGenericMapAspectInnerSyntax :: InterfacePackageGenericMapAspectAssociations (inner) => inner . raw () , }
    }
}
#[derive(Debug, Clone)]
pub struct InterfaceProcedureSpecificationSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfaceProcedureSpecificationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::InterfaceProcedureSpecification,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "procedure",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Procedure)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "designator",
                kind: LayoutItemKind::TokenGroup(&[
                    TokenKind::Identifier,
                    TokenKind::StringLiteral,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "parameter",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Parameter)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "left_par",
                kind: LayoutItemKind::Token(TokenKind::LeftPar),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "interface_list",
                kind: LayoutItemKind::Node(NodeKind::InterfaceList),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "right_par",
                kind: LayoutItemKind::Token(TokenKind::RightPar),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        InterfaceProcedureSpecificationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfaceProcedureSpecificationSyntax {
    pub fn procedure_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Procedure))
            .nth(0)
    }
    pub fn designator(&self) -> Option<DesignatorSyntax> {
        self.0.tokens().filter_map(DesignatorSyntax::cast).nth(0)
    }
    pub fn parameter_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Parameter))
            .nth(0)
    }
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::LeftPar)
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
            .filter(|token| token.kind() == TokenKind::RightPar)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct InterfaceSignalDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfaceSignalDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::InterfaceSignalDeclaration,
        items: &[
            LayoutItem {
                optional: true,
                repeated: false,
                name: "signal",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Signal)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "identifier_list",
                kind: LayoutItemKind::Node(NodeKind::IdentifierList),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "colon",
                kind: LayoutItemKind::Token(TokenKind::Colon),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "mode",
                kind: LayoutItemKind::TokenGroup(&[
                    TokenKind::Keyword(Kw::In),
                    TokenKind::Keyword(Kw::Out),
                    TokenKind::Keyword(Kw::Inout),
                    TokenKind::Keyword(Kw::Buffer),
                    TokenKind::Keyword(Kw::Linkage),
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "subtype_indication",
                kind: LayoutItemKind::Node(NodeKind::SubtypeIndication),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "bus",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Bus)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "colon_eq",
                kind: LayoutItemKind::Token(TokenKind::ColonEq),
            },
            LayoutItem {
                optional: true,
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
        InterfaceSignalDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfaceSignalDeclarationSyntax {
    pub fn signal_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Signal))
            .nth(0)
    }
    pub fn identifier_list(&self) -> Option<IdentifierListSyntax> {
        self.0
            .children()
            .filter_map(IdentifierListSyntax::cast)
            .nth(0)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Colon)
            .nth(0)
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
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Bus))
            .nth(0)
    }
    pub fn colon_eq_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::ColonEq)
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
pub struct InterfaceSubprogramDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfaceSubprogramDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::InterfaceSubprogramDeclaration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "interface_subprogram_specification",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::InterfaceProcedureSpecification,
                    NodeKind::InterfaceFunctionSpecification,
                ]),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "is",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Is)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "interface_subprogram_default",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::InterfaceSubprogramDefaultName,
                    NodeKind::InterfaceSubprogramDefaultBox,
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
        InterfaceSubprogramDeclarationSyntax(node)
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
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Is))
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
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct InterfaceSubprogramDefaultNameSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfaceSubprogramDefaultNameSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::InterfaceSubprogramDefaultName,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "name",
            kind: LayoutItemKind::Node(NodeKind::Name),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        InterfaceSubprogramDefaultNameSyntax(node)
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::InterfaceSubprogramDefaultBox,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "box",
            kind: LayoutItemKind::Token(TokenKind::BOX),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        InterfaceSubprogramDefaultBoxSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfaceSubprogramDefaultBoxSyntax {
    pub fn box_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::BOX)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum InterfaceSubprogramDefaultSyntax {
    InterfaceSubprogramDefaultName(InterfaceSubprogramDefaultNameSyntax),
    InterfaceSubprogramDefaultBox(InterfaceSubprogramDefaultBoxSyntax),
}
impl AstNode for InterfaceSubprogramDefaultSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::InterfaceSubprogramDefaultName,
            NodeKind::InterfaceSubprogramDefaultBox,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if InterfaceSubprogramDefaultNameSyntax::can_cast(&node) {
            return InterfaceSubprogramDefaultSyntax::InterfaceSubprogramDefaultName(
                InterfaceSubprogramDefaultNameSyntax::cast_unchecked(node),
            );
        }
        if InterfaceSubprogramDefaultBoxSyntax::can_cast(&node) {
            return InterfaceSubprogramDefaultSyntax::InterfaceSubprogramDefaultBox(
                InterfaceSubprogramDefaultBoxSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
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
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::InterfaceProcedureSpecification,
            NodeKind::InterfaceFunctionSpecification,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if InterfaceProcedureSpecificationSyntax::can_cast(&node) {
            return InterfaceSubprogramSpecificationSyntax::InterfaceProcedureSpecification(
                InterfaceProcedureSpecificationSyntax::cast_unchecked(node),
            );
        }
        if InterfaceFunctionSpecificationSyntax::can_cast(&node) {
            return InterfaceSubprogramSpecificationSyntax::InterfaceFunctionSpecification(
                InterfaceFunctionSpecificationSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::InterfaceVariableDeclaration,
        items: &[
            LayoutItem {
                optional: true,
                repeated: false,
                name: "variable",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Variable)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "identifier_list",
                kind: LayoutItemKind::Node(NodeKind::IdentifierList),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "colon",
                kind: LayoutItemKind::Token(TokenKind::Colon),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "mode",
                kind: LayoutItemKind::TokenGroup(&[
                    TokenKind::Keyword(Kw::In),
                    TokenKind::Keyword(Kw::Out),
                    TokenKind::Keyword(Kw::Inout),
                    TokenKind::Keyword(Kw::Buffer),
                    TokenKind::Keyword(Kw::Linkage),
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "subtype_indication",
                kind: LayoutItemKind::Node(NodeKind::SubtypeIndication),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "colon_eq",
                kind: LayoutItemKind::Token(TokenKind::ColonEq),
            },
            LayoutItem {
                optional: true,
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
        InterfaceVariableDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfaceVariableDeclarationSyntax {
    pub fn variable_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Variable))
            .nth(0)
    }
    pub fn identifier_list(&self) -> Option<IdentifierListSyntax> {
        self.0
            .children()
            .filter_map(IdentifierListSyntax::cast)
            .nth(0)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Colon)
            .nth(0)
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
            .filter(|token| token.kind() == TokenKind::ColonEq)
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
            TokenKind::Keyword(Kw::In) => Some(ModeSyntax::In(token)),
            TokenKind::Keyword(Kw::Out) => Some(ModeSyntax::Out(token)),
            TokenKind::Keyword(Kw::Inout) => Some(ModeSyntax::Inout(token)),
            TokenKind::Keyword(Kw::Buffer) => Some(ModeSyntax::Buffer(token)),
            TokenKind::Keyword(Kw::Linkage) => Some(ModeSyntax::Linkage(token)),
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PortClause,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "port_clause_preamble",
                kind: LayoutItemKind::Node(NodeKind::PortClausePreamble),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "interface_list",
                kind: LayoutItemKind::Node(NodeKind::InterfaceList),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "port_clause_epilogue",
                kind: LayoutItemKind::Node(NodeKind::PortClauseEpilogue),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        PortClauseSyntax(node)
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PortClausePreamble,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "port",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Port)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "left_par",
                kind: LayoutItemKind::Token(TokenKind::LeftPar),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        PortClausePreambleSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PortClausePreambleSyntax {
    pub fn port_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Port))
            .nth(0)
    }
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::LeftPar)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct PortClauseEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for PortClauseEpilogueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PortClauseEpilogue,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "right_par",
                kind: LayoutItemKind::Token(TokenKind::RightPar),
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
        PortClauseEpilogueSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PortClauseEpilogueSyntax {
    pub fn right_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::RightPar)
            .nth(0)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct PortMapAspectSyntax(pub(crate) SyntaxNode);
impl AstNode for PortMapAspectSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PortMapAspect,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "port",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Port)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "map",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Map)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "left_par",
                kind: LayoutItemKind::Token(TokenKind::LeftPar),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "association_list",
                kind: LayoutItemKind::Node(NodeKind::AssociationList),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "right_par",
                kind: LayoutItemKind::Token(TokenKind::RightPar),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        PortMapAspectSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PortMapAspectSyntax {
    pub fn port_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Port))
            .nth(0)
    }
    pub fn map_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Map))
            .nth(0)
    }
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::LeftPar)
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
            .filter(|token| token.kind() == TokenKind::RightPar)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct RecordElementResolutionSyntax(pub(crate) SyntaxNode);
impl AstNode for RecordElementResolutionSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::RecordElementResolution,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "name",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "resolution_indication",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::NameResolutionIndication,
                    NodeKind::ParenthesizedElementResolutionResolutionIndication,
                ]),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        RecordElementResolutionSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl RecordElementResolutionSyntax {
    pub fn name_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Identifier)
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::RecordResolution,
        items: &[
            LayoutItem {
                optional: false,
                repeated: true,
                name: "record_element_resolutions",
                kind: LayoutItemKind::Node(NodeKind::RecordElementResolution),
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
        RecordResolutionSyntax(node)
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
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Comma)
    }
}
#[derive(Debug, Clone)]
pub struct NameResolutionIndicationSyntax(pub(crate) SyntaxNode);
impl AstNode for NameResolutionIndicationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::NameResolutionIndication,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "name",
            kind: LayoutItemKind::Node(NodeKind::Name),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        NameResolutionIndicationSyntax(node)
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ElementResolutionResolutionIndication,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "element_resolution",
            kind: LayoutItemKind::NodeChoice(&[
                NodeKind::ResolutionIndicationElementResolution,
                NodeKind::RecordResolutionElementResolution,
            ]),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ElementResolutionResolutionIndicationSyntax(node)
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ParenthesizedElementResolutionResolutionIndication,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "left_par",
                kind: LayoutItemKind::Token(TokenKind::LeftPar),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "element_resolution_resolution_indication",
                kind: LayoutItemKind::Node(NodeKind::ElementResolutionResolutionIndication),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "right_par",
                kind: LayoutItemKind::Token(TokenKind::RightPar),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ParenthesizedElementResolutionResolutionIndicationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ParenthesizedElementResolutionResolutionIndicationSyntax {
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::LeftPar)
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
            .filter(|token| token.kind() == TokenKind::RightPar)
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
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::NameResolutionIndication,
            NodeKind::ParenthesizedElementResolutionResolutionIndication,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if NameResolutionIndicationSyntax::can_cast(&node) {
            return ResolutionIndicationSyntax::NameResolutionIndication(
                NameResolutionIndicationSyntax::cast_unchecked(node),
            );
        }
        if ParenthesizedElementResolutionResolutionIndicationSyntax::can_cast(&node) {
            return ResolutionIndicationSyntax::ParenthesizedElementResolutionResolutionIndication(
                ParenthesizedElementResolutionResolutionIndicationSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SignalDeclaration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "signal",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Signal)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "identifier_list",
                kind: LayoutItemKind::Node(NodeKind::IdentifierList),
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
                name: "subtype_indication",
                kind: LayoutItemKind::Node(NodeKind::SubtypeIndication),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "signal_kind",
                kind: LayoutItemKind::TokenGroup(&[
                    TokenKind::Keyword(Kw::Register),
                    TokenKind::Keyword(Kw::Bus),
                ]),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "colon_eq",
                kind: LayoutItemKind::Token(TokenKind::ColonEq),
            },
            LayoutItem {
                optional: true,
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
        SignalDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SignalDeclarationSyntax {
    pub fn signal_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Signal))
            .nth(0)
    }
    pub fn identifier_list(&self) -> Option<IdentifierListSyntax> {
        self.0
            .children()
            .filter_map(IdentifierListSyntax::cast)
            .nth(0)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Colon)
            .nth(0)
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
            .filter(|token| token.kind() == TokenKind::ColonEq)
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
pub enum SignalKindSyntax {
    Register(SyntaxToken),
    Bus(SyntaxToken),
}
impl SignalKindSyntax {
    pub fn cast(token: SyntaxToken) -> Option<Self> {
        match token.kind() {
            TokenKind::Keyword(Kw::Register) => Some(SignalKindSyntax::Register(token)),
            TokenKind::Keyword(Kw::Bus) => Some(SignalKindSyntax::Bus(token)),
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SubtypeDeclaration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "subtype",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Subtype)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "identifier",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
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
                name: "subtype_indication",
                kind: LayoutItemKind::Node(NodeKind::SubtypeIndication),
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
        SubtypeDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SubtypeDeclarationSyntax {
    pub fn subtype_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Subtype))
            .nth(0)
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Identifier)
            .nth(0)
    }
    pub fn is_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Is))
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
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct SubtypeIndicationSyntax(pub(crate) SyntaxNode);
impl AstNode for SubtypeIndicationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SubtypeIndication,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "resolution_indication",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::NameResolutionIndication,
                    NodeKind::ParenthesizedElementResolutionResolutionIndication,
                ]),
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
        SubtypeIndicationSyntax(node)
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
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::FullTypeDeclaration,
            NodeKind::IncompleteTypeDeclaration,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if FullTypeDeclarationSyntax::can_cast(&node) {
            return TypeDeclarationSyntax::FullTypeDeclaration(
                FullTypeDeclarationSyntax::cast_unchecked(node),
            );
        }
        if IncompleteTypeDeclarationSyntax::can_cast(&node) {
            return TypeDeclarationSyntax::IncompleteTypeDeclaration(
                IncompleteTypeDeclarationSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
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
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::EnumerationTypeDefinition,
            NodeKind::NumericTypeDefinition,
            NodeKind::PhysicalTypeDefinition,
            NodeKind::UnboundedArrayDefinition,
            NodeKind::ConstrainedArrayDefinition,
            NodeKind::RecordTypeDefinition,
            NodeKind::AccessTypeDefinition,
            NodeKind::FileTypeDefinition,
            NodeKind::ProtectedTypeDeclaration,
            NodeKind::ProtectedTypeBody,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if ScalarTypeDefinitionSyntax::can_cast(&node) {
            return TypeDefinitionSyntax::ScalarTypeDefinition(
                ScalarTypeDefinitionSyntax::cast_unchecked(node),
            );
        }
        if CompositeTypeDefinitionSyntax::can_cast(&node) {
            return TypeDefinitionSyntax::CompositeTypeDefinition(
                CompositeTypeDefinitionSyntax::cast_unchecked(node),
            );
        }
        if AccessTypeDefinitionSyntax::can_cast(&node) {
            return TypeDefinitionSyntax::AccessTypeDefinition(
                AccessTypeDefinitionSyntax::cast_unchecked(node),
            );
        }
        if FileTypeDefinitionSyntax::can_cast(&node) {
            return TypeDefinitionSyntax::FileTypeDefinition(
                FileTypeDefinitionSyntax::cast_unchecked(node),
            );
        }
        if ProtectedTypeDefinitionSyntax::can_cast(&node) {
            return TypeDefinitionSyntax::ProtectedTypeDefinition(
                ProtectedTypeDefinitionSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::VariableDeclaration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "variable",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Variable)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "identifier_list",
                kind: LayoutItemKind::Node(NodeKind::IdentifierList),
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
                name: "subtype_indication",
                kind: LayoutItemKind::Node(NodeKind::SubtypeIndication),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "colon_eq",
                kind: LayoutItemKind::Token(TokenKind::ColonEq),
            },
            LayoutItem {
                optional: true,
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
        VariableDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl VariableDeclarationSyntax {
    pub fn variable_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Variable))
            .nth(0)
    }
    pub fn identifier_list(&self) -> Option<IdentifierListSyntax> {
        self.0
            .children()
            .filter_map(IdentifierListSyntax::cast)
            .nth(0)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Colon)
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
            .filter(|token| token.kind() == TokenKind::ColonEq)
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
pub struct SharedVariableDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for SharedVariableDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SharedVariableDeclaration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "shared",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Shared)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "variable",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Variable)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "identifier_list",
                kind: LayoutItemKind::Node(NodeKind::IdentifierList),
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
                name: "subtype_indication",
                kind: LayoutItemKind::Node(NodeKind::SubtypeIndication),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "colon_eq",
                kind: LayoutItemKind::Token(TokenKind::ColonEq),
            },
            LayoutItem {
                optional: true,
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
        SharedVariableDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SharedVariableDeclarationSyntax {
    pub fn shared_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Shared))
            .nth(0)
    }
    pub fn variable_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Variable))
            .nth(0)
    }
    pub fn identifier_list(&self) -> Option<IdentifierListSyntax> {
        self.0
            .children()
            .filter_map(IdentifierListSyntax::cast)
            .nth(0)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Colon)
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
            .filter(|token| token.kind() == TokenKind::ColonEq)
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
pub struct DeclarationsSyntax(pub(crate) SyntaxNode);
impl AstNode for DeclarationsSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::Declarations,
        items: &[LayoutItem {
            optional: false,
            repeated: true,
            name: "declarations",
            kind: LayoutItemKind::NodeChoice(&[
                NodeKind::SubprogramDeclaration,
                NodeKind::SubprogramBody,
                NodeKind::SubprogramInstantiationDeclaration,
                NodeKind::PackageDeclaration,
                NodeKind::PackageBodyDeclaration,
                NodeKind::PackageInstantiationDeclaration,
                NodeKind::FullTypeDeclaration,
                NodeKind::IncompleteTypeDeclaration,
                NodeKind::SubtypeDeclaration,
                NodeKind::FileDeclaration,
                NodeKind::AliasDeclaration,
                NodeKind::ComponentDeclaration,
                NodeKind::AttributeDeclaration,
                NodeKind::AttributeSpecification,
                NodeKind::SimpleConfigurationSpecification,
                NodeKind::CompoundConfigurationSpecification,
                NodeKind::DisconnectionSpecification,
                NodeKind::UseClauseDeclaration,
                NodeKind::GroupTemplateDeclaration,
                NodeKind::GroupDeclaration,
                NodeKind::ConstantDeclaration,
                NodeKind::SignalDeclaration,
                NodeKind::VariableDeclaration,
                NodeKind::SharedVariableDeclaration,
                NodeKind::PslPropertyDeclaration,
                NodeKind::PslSequenceDeclaration,
                NodeKind::PslClockDeclaration,
            ]),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        DeclarationsSyntax(node)
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
