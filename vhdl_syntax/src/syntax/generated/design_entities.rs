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
pub struct ArchitectureBodySyntax(pub(crate) SyntaxNode);
impl AstNode for ArchitectureBodySyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ArchitectureBody,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "architecture_preamble",
                kind: LayoutItemKind::Node(NodeKind::ArchitecturePreamble),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "declarations",
                kind: LayoutItemKind::Node(NodeKind::Declarations),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "declaration_statement_separator",
                kind: LayoutItemKind::Node(NodeKind::DeclarationStatementSeparator),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "concurrent_statements",
                kind: LayoutItemKind::Node(NodeKind::ConcurrentStatements),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "architecture_epilogue",
                kind: LayoutItemKind::Node(NodeKind::ArchitectureEpilogue),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ArchitectureBodySyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ArchitectureBodySyntax {
    pub fn architecture_preamble(&self) -> Option<ArchitecturePreambleSyntax> {
        self.0
            .children()
            .filter_map(ArchitecturePreambleSyntax::cast)
            .nth(0)
    }
    pub fn declarations(&self) -> Option<DeclarationsSyntax> {
        self.0
            .children()
            .filter_map(DeclarationsSyntax::cast)
            .nth(0)
    }
    pub fn declaration_statement_separator(&self) -> Option<DeclarationStatementSeparatorSyntax> {
        self.0
            .children()
            .filter_map(DeclarationStatementSeparatorSyntax::cast)
            .nth(0)
    }
    pub fn concurrent_statements(&self) -> Option<ConcurrentStatementsSyntax> {
        self.0
            .children()
            .filter_map(ConcurrentStatementsSyntax::cast)
            .nth(0)
    }
    pub fn architecture_epilogue(&self) -> Option<ArchitectureEpilogueSyntax> {
        self.0
            .children()
            .filter_map(ArchitectureEpilogueSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ArchitecturePreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for ArchitecturePreambleSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ArchitecturePreamble,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Architecture)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Of)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "entity_name",
                kind: LayoutItemKind::Node(NodeKind::Name),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Is)),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ArchitecturePreambleSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ArchitecturePreambleSyntax {
    pub fn architecture_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Architecture))
            .nth(0)
    }
    pub fn name_token(&self) -> Option<SyntaxToken> {
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
    pub fn entity_name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
    pub fn is_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Is))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ArchitectureEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for ArchitectureEpilogueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ArchitectureEpilogue,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::End)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Architecture)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::SemiColon),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ArchitectureEpilogueSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ArchitectureEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::End))
            .nth(0)
    }
    pub fn architecture_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Architecture))
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
pub struct BlockConfigurationItemSyntax(pub(crate) SyntaxNode);
impl AstNode for BlockConfigurationItemSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::BlockConfigurationItem,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "block_configuration",
            kind: LayoutItemKind::Node(NodeKind::BlockConfiguration),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        BlockConfigurationItemSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl BlockConfigurationItemSyntax {
    pub fn block_configuration(&self) -> Option<BlockConfigurationSyntax> {
        self.0
            .children()
            .filter_map(BlockConfigurationSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum ConfigurationItemSyntax {
    BlockConfigurationItem(BlockConfigurationItemSyntax),
    ComponentConfiguration(ComponentConfigurationSyntax),
}
impl AstNode for ConfigurationItemSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::BlockConfigurationItem,
            NodeKind::ComponentConfiguration,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if BlockConfigurationItemSyntax::can_cast(&node) {
            return ConfigurationItemSyntax::BlockConfigurationItem(
                BlockConfigurationItemSyntax::cast_unchecked(node),
            );
        }
        if ComponentConfigurationSyntax::can_cast(&node) {
            return ConfigurationItemSyntax::ComponentConfiguration(
                ComponentConfigurationSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            ConfigurationItemSyntax::BlockConfigurationItem(inner) => inner.raw(),
            ConfigurationItemSyntax::ComponentConfiguration(inner) => inner.raw(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct BlockConfigurationSyntax(pub(crate) SyntaxNode);
impl AstNode for BlockConfigurationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::BlockConfiguration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "block_configuration_preamble",
                kind: LayoutItemKind::Node(NodeKind::BlockConfigurationPreamble),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "block_configuration_items",
                kind: LayoutItemKind::Node(NodeKind::BlockConfigurationItems),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "block_configuration_epilogue",
                kind: LayoutItemKind::Node(NodeKind::BlockConfigurationEpilogue),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        BlockConfigurationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl BlockConfigurationSyntax {
    pub fn block_configuration_preamble(&self) -> Option<BlockConfigurationPreambleSyntax> {
        self.0
            .children()
            .filter_map(BlockConfigurationPreambleSyntax::cast)
            .nth(0)
    }
    pub fn block_configuration_items(&self) -> Option<BlockConfigurationItemsSyntax> {
        self.0
            .children()
            .filter_map(BlockConfigurationItemsSyntax::cast)
            .nth(0)
    }
    pub fn block_configuration_epilogue(&self) -> Option<BlockConfigurationEpilogueSyntax> {
        self.0
            .children()
            .filter_map(BlockConfigurationEpilogueSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct BlockConfigurationItemsSyntax(pub(crate) SyntaxNode);
impl AstNode for BlockConfigurationItemsSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::BlockConfigurationItems,
        items: &[
            LayoutItem {
                optional: false,
                repeated: true,
                name: "use_clauses",
                kind: LayoutItemKind::Node(NodeKind::UseClause),
            },
            LayoutItem {
                optional: false,
                repeated: true,
                name: "configuration_items",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::BlockConfigurationItem,
                    NodeKind::ComponentConfiguration,
                ]),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        BlockConfigurationItemsSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl BlockConfigurationItemsSyntax {
    pub fn use_clauses(&self) -> impl Iterator<Item = UseClauseSyntax> + use<'_> {
        self.0.children().filter_map(UseClauseSyntax::cast)
    }
    pub fn configuration_items(&self) -> impl Iterator<Item = ConfigurationItemSyntax> + use<'_> {
        self.0.children().filter_map(ConfigurationItemSyntax::cast)
    }
}
#[derive(Debug, Clone)]
pub struct BlockConfigurationPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for BlockConfigurationPreambleSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::BlockConfigurationPreamble,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::For)),
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
        BlockConfigurationPreambleSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl BlockConfigurationPreambleSyntax {
    pub fn for_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::For))
            .nth(0)
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct BlockConfigurationEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for BlockConfigurationEpilogueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::BlockConfigurationEpilogue,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::End)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::For)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::SemiColon),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        BlockConfigurationEpilogueSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl BlockConfigurationEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::End))
            .nth(0)
    }
    pub fn for_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::For))
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
pub struct ComponentConfigurationSyntax(pub(crate) SyntaxNode);
impl AstNode for ComponentConfigurationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ComponentConfiguration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "component_configuration_preamble",
                kind: LayoutItemKind::Node(NodeKind::ComponentConfigurationPreamble),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "component_configuration_items",
                kind: LayoutItemKind::Node(NodeKind::ComponentConfigurationItems),
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
        ComponentConfigurationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ComponentConfigurationSyntax {
    pub fn component_configuration_preamble(&self) -> Option<ComponentConfigurationPreambleSyntax> {
        self.0
            .children()
            .filter_map(ComponentConfigurationPreambleSyntax::cast)
            .nth(0)
    }
    pub fn component_configuration_items(&self) -> Option<ComponentConfigurationItemsSyntax> {
        self.0
            .children()
            .filter_map(ComponentConfigurationItemsSyntax::cast)
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
pub struct SemiColonTerminatedBindingIndicationSyntax(pub(crate) SyntaxNode);
impl AstNode for SemiColonTerminatedBindingIndicationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SemiColonTerminatedBindingIndication,
        items: &[
            LayoutItem {
                optional: true,
                repeated: false,
                name: "BindingIndication",
                kind: LayoutItemKind::Node(NodeKind::BindingIndication),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::SemiColon),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        SemiColonTerminatedBindingIndicationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SemiColonTerminatedBindingIndicationSyntax {
    pub fn binding_indication(&self) -> Option<BindingIndicationSyntax> {
        self.0
            .children()
            .filter_map(BindingIndicationSyntax::cast)
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
pub struct SemiColonTerminatedVerificationUnitBindingIndicationSyntax(pub(crate) SyntaxNode);
impl AstNode for SemiColonTerminatedVerificationUnitBindingIndicationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SemiColonTerminatedVerificationUnitBindingIndication,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "VerificationUnitBindingIndication",
                kind: LayoutItemKind::Node(NodeKind::VerificationUnitBindingIndication),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::SemiColon),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        SemiColonTerminatedVerificationUnitBindingIndicationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SemiColonTerminatedVerificationUnitBindingIndicationSyntax {
    pub fn verification_unit_binding_indication(
        &self,
    ) -> Option<VerificationUnitBindingIndicationSyntax> {
        self.0
            .children()
            .filter_map(VerificationUnitBindingIndicationSyntax::cast)
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
pub struct ComponentConfigurationItemsSyntax(pub(crate) SyntaxNode);
impl AstNode for ComponentConfigurationItemsSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ComponentConfigurationItems,
        items: &[
            LayoutItem {
                optional: true,
                repeated: false,
                name: "semi_colon_terminated_binding_indication",
                kind: LayoutItemKind::Node(NodeKind::SemiColonTerminatedBindingIndication),
            },
            LayoutItem {
                optional: false,
                repeated: true,
                name: "semi_colon_terminated_verification_unit_binding_indications",
                kind: LayoutItemKind::Node(
                    NodeKind::SemiColonTerminatedVerificationUnitBindingIndication,
                ),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "block_configuration",
                kind: LayoutItemKind::Node(NodeKind::BlockConfiguration),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ComponentConfigurationItemsSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ComponentConfigurationItemsSyntax {
    pub fn semi_colon_terminated_binding_indication(
        &self,
    ) -> Option<SemiColonTerminatedBindingIndicationSyntax> {
        self.0
            .children()
            .filter_map(SemiColonTerminatedBindingIndicationSyntax::cast)
            .nth(0)
    }
    pub fn semi_colon_terminated_verification_unit_binding_indications(
        &self,
    ) -> impl Iterator<Item = SemiColonTerminatedVerificationUnitBindingIndicationSyntax> + use<'_>
    {
        self.0
            .children()
            .filter_map(SemiColonTerminatedVerificationUnitBindingIndicationSyntax::cast)
    }
    pub fn block_configuration(&self) -> Option<BlockConfigurationSyntax> {
        self.0
            .children()
            .filter_map(BlockConfigurationSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ComponentConfigurationPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for ComponentConfigurationPreambleSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ComponentConfigurationPreamble,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::For)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "component_specification",
                kind: LayoutItemKind::Node(NodeKind::ComponentSpecification),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ComponentConfigurationPreambleSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ComponentConfigurationPreambleSyntax {
    pub fn for_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::For))
            .nth(0)
    }
    pub fn component_specification(&self) -> Option<ComponentSpecificationSyntax> {
        self.0
            .children()
            .filter_map(ComponentSpecificationSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ComponentConfigurationEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for ComponentConfigurationEpilogueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ComponentConfigurationEpilogue,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::End)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::For)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::SemiColon),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ComponentConfigurationEpilogueSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ComponentConfigurationEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::End))
            .nth(0)
    }
    pub fn for_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::For))
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
pub struct ConfigurationDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for ConfigurationDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ConfigurationDeclaration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "configuration_declaration_preamble",
                kind: LayoutItemKind::Node(NodeKind::ConfigurationDeclarationPreamble),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "configuration_declaration_items",
                kind: LayoutItemKind::Node(NodeKind::ConfigurationDeclarationItems),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "configuration_declaration_epilogue",
                kind: LayoutItemKind::Node(NodeKind::ConfigurationDeclarationEpilogue),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ConfigurationDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConfigurationDeclarationSyntax {
    pub fn configuration_declaration_preamble(
        &self,
    ) -> Option<ConfigurationDeclarationPreambleSyntax> {
        self.0
            .children()
            .filter_map(ConfigurationDeclarationPreambleSyntax::cast)
            .nth(0)
    }
    pub fn configuration_declaration_items(&self) -> Option<ConfigurationDeclarationItemsSyntax> {
        self.0
            .children()
            .filter_map(ConfigurationDeclarationItemsSyntax::cast)
            .nth(0)
    }
    pub fn configuration_declaration_epilogue(
        &self,
    ) -> Option<ConfigurationDeclarationEpilogueSyntax> {
        self.0
            .children()
            .filter_map(ConfigurationDeclarationEpilogueSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ConfigurationDeclarationItemsSyntax(pub(crate) SyntaxNode);
impl AstNode for ConfigurationDeclarationItemsSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ConfigurationDeclarationItems,
        items: &[
            LayoutItem {
                optional: true,
                repeated: false,
                name: "declarations",
                kind: LayoutItemKind::Node(NodeKind::Declarations),
            },
            LayoutItem {
                optional: false,
                repeated: true,
                name: "semi_colon_terminated_verification_unit_binding_indications",
                kind: LayoutItemKind::Node(
                    NodeKind::SemiColonTerminatedVerificationUnitBindingIndication,
                ),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "block_configuration",
                kind: LayoutItemKind::Node(NodeKind::BlockConfiguration),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ConfigurationDeclarationItemsSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConfigurationDeclarationItemsSyntax {
    pub fn declarations(&self) -> Option<DeclarationsSyntax> {
        self.0
            .children()
            .filter_map(DeclarationsSyntax::cast)
            .nth(0)
    }
    pub fn semi_colon_terminated_verification_unit_binding_indications(
        &self,
    ) -> impl Iterator<Item = SemiColonTerminatedVerificationUnitBindingIndicationSyntax> + use<'_>
    {
        self.0
            .children()
            .filter_map(SemiColonTerminatedVerificationUnitBindingIndicationSyntax::cast)
    }
    pub fn block_configuration(&self) -> Option<BlockConfigurationSyntax> {
        self.0
            .children()
            .filter_map(BlockConfigurationSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ConfigurationDeclarationPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for ConfigurationDeclarationPreambleSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ConfigurationDeclarationPreamble,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Configuration)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Of)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "entity_name",
                kind: LayoutItemKind::Node(NodeKind::Name),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Is)),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ConfigurationDeclarationPreambleSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConfigurationDeclarationPreambleSyntax {
    pub fn configuration_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Configuration))
            .nth(0)
    }
    pub fn name_token(&self) -> Option<SyntaxToken> {
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
    pub fn entity_name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
    pub fn is_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Is))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ConfigurationDeclarationEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for ConfigurationDeclarationEpilogueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ConfigurationDeclarationEpilogue,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::End)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Configuration)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::SemiColon),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ConfigurationDeclarationEpilogueSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConfigurationDeclarationEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::End))
            .nth(0)
    }
    pub fn configuration_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Configuration))
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
pub struct EntityDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for EntityDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::EntityDeclaration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "entity_declaration_preamble",
                kind: LayoutItemKind::Node(NodeKind::EntityDeclarationPreamble),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "entity_header",
                kind: LayoutItemKind::Node(NodeKind::EntityHeader),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "declarations",
                kind: LayoutItemKind::Node(NodeKind::Declarations),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "declaration_statement_separator",
                kind: LayoutItemKind::Node(NodeKind::DeclarationStatementSeparator),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "concurrent_statements",
                kind: LayoutItemKind::Node(NodeKind::ConcurrentStatements),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "entity_declaration_epilogue",
                kind: LayoutItemKind::Node(NodeKind::EntityDeclarationEpilogue),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        EntityDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl EntityDeclarationSyntax {
    pub fn entity_declaration_preamble(&self) -> Option<EntityDeclarationPreambleSyntax> {
        self.0
            .children()
            .filter_map(EntityDeclarationPreambleSyntax::cast)
            .nth(0)
    }
    pub fn entity_header(&self) -> Option<EntityHeaderSyntax> {
        self.0
            .children()
            .filter_map(EntityHeaderSyntax::cast)
            .nth(0)
    }
    pub fn declarations(&self) -> Option<DeclarationsSyntax> {
        self.0
            .children()
            .filter_map(DeclarationsSyntax::cast)
            .nth(0)
    }
    pub fn declaration_statement_separator(&self) -> Option<DeclarationStatementSeparatorSyntax> {
        self.0
            .children()
            .filter_map(DeclarationStatementSeparatorSyntax::cast)
            .nth(0)
    }
    pub fn concurrent_statements(&self) -> Option<ConcurrentStatementsSyntax> {
        self.0
            .children()
            .filter_map(ConcurrentStatementsSyntax::cast)
            .nth(0)
    }
    pub fn entity_declaration_epilogue(&self) -> Option<EntityDeclarationEpilogueSyntax> {
        self.0
            .children()
            .filter_map(EntityDeclarationEpilogueSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct EntityDeclarationPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for EntityDeclarationPreambleSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::EntityDeclarationPreamble,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Entity)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Is)),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        EntityDeclarationPreambleSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl EntityDeclarationPreambleSyntax {
    pub fn entity_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Entity))
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
pub struct EntityDeclarationEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for EntityDeclarationEpilogueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::EntityDeclarationEpilogue,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::End)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Entity)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::SemiColon),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        EntityDeclarationEpilogueSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl EntityDeclarationEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::End))
            .nth(0)
    }
    pub fn entity_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Entity))
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
pub struct EntityHeaderSyntax(pub(crate) SyntaxNode);
impl AstNode for EntityHeaderSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::EntityHeader,
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
        EntityHeaderSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl EntityHeaderSyntax {
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
