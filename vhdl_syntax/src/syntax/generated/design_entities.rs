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
pub struct ArchitectureBodySyntax(pub(crate) SyntaxNode);
impl AstNode for ArchitectureBodySyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ArchitectureBody => Some(ArchitectureBodySyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ArchitectureBody)
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
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ArchitecturePreamble => Some(ArchitecturePreambleSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ArchitecturePreamble)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ArchitecturePreambleSyntax {
    pub fn architecture_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Architecture))
            .nth(0)
    }
    pub fn name_token(&self) -> Option<SyntaxToken> {
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
    pub fn entity_name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
    pub fn is_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Is))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ArchitectureEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for ArchitectureEpilogueSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ArchitectureEpilogue => Some(ArchitectureEpilogueSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ArchitectureEpilogue)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ArchitectureEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::End))
            .nth(0)
    }
    pub fn architecture_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Architecture))
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
pub struct BlockConfigurationItemSyntax(pub(crate) SyntaxNode);
impl AstNode for BlockConfigurationItemSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::BlockConfigurationItem => Some(BlockConfigurationItemSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::BlockConfigurationItem)
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
    fn cast(node: SyntaxNode) -> Option<Self> {
        if BlockConfigurationItemSyntax::can_cast(&node) {
            return Some(ConfigurationItemSyntax::BlockConfigurationItem(
                BlockConfigurationItemSyntax::cast(node).unwrap(),
            ));
        };
        if ComponentConfigurationSyntax::can_cast(&node) {
            return Some(ConfigurationItemSyntax::ComponentConfiguration(
                ComponentConfigurationSyntax::cast(node).unwrap(),
            ));
        };
        None
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        BlockConfigurationItemSyntax::can_cast(node) || ComponentConfigurationSyntax::can_cast(node)
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
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::BlockConfiguration => Some(BlockConfigurationSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::BlockConfiguration)
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
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::BlockConfigurationItems => Some(BlockConfigurationItemsSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::BlockConfigurationItems)
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
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::BlockConfigurationPreamble => Some(BlockConfigurationPreambleSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::BlockConfigurationPreamble)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl BlockConfigurationPreambleSyntax {
    pub fn for_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::For))
            .nth(0)
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct BlockConfigurationEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for BlockConfigurationEpilogueSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::BlockConfigurationEpilogue => Some(BlockConfigurationEpilogueSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::BlockConfigurationEpilogue)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl BlockConfigurationEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::End))
            .nth(0)
    }
    pub fn for_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::For))
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
pub struct ComponentConfigurationSyntax(pub(crate) SyntaxNode);
impl AstNode for ComponentConfigurationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ComponentConfiguration => Some(ComponentConfigurationSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ComponentConfiguration)
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
pub struct SemiColonTerminatedVerificationUnitBindingIndicationSyntax(pub(crate) SyntaxNode);
impl AstNode for SemiColonTerminatedVerificationUnitBindingIndicationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SemiColonTerminatedVerificationUnitBindingIndication => Some(
                SemiColonTerminatedVerificationUnitBindingIndicationSyntax(node),
            ),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(
            node.kind(),
            NodeKind::SemiColonTerminatedVerificationUnitBindingIndication
        )
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
            .filter(|token| token.kind() == SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ComponentConfigurationItemsSyntax(pub(crate) SyntaxNode);
impl AstNode for ComponentConfigurationItemsSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ComponentConfigurationItems => Some(ComponentConfigurationItemsSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ComponentConfigurationItems)
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
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ComponentConfigurationPreamble => {
                Some(ComponentConfigurationPreambleSyntax(node))
            }
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ComponentConfigurationPreamble)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ComponentConfigurationPreambleSyntax {
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
}
#[derive(Debug, Clone)]
pub struct ComponentConfigurationEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for ComponentConfigurationEpilogueSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ComponentConfigurationEpilogue => {
                Some(ComponentConfigurationEpilogueSyntax(node))
            }
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ComponentConfigurationEpilogue)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ComponentConfigurationEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::End))
            .nth(0)
    }
    pub fn for_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::For))
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
pub struct ConfigurationDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for ConfigurationDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ConfigurationDeclaration => Some(ConfigurationDeclarationSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ConfigurationDeclaration)
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
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ConfigurationDeclarationItems => {
                Some(ConfigurationDeclarationItemsSyntax(node))
            }
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ConfigurationDeclarationItems)
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
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ConfigurationDeclarationPreamble => {
                Some(ConfigurationDeclarationPreambleSyntax(node))
            }
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ConfigurationDeclarationPreamble)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConfigurationDeclarationPreambleSyntax {
    pub fn configuration_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Configuration))
            .nth(0)
    }
    pub fn name_token(&self) -> Option<SyntaxToken> {
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
    pub fn entity_name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
    pub fn is_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Is))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ConfigurationDeclarationEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for ConfigurationDeclarationEpilogueSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ConfigurationDeclarationEpilogue => {
                Some(ConfigurationDeclarationEpilogueSyntax(node))
            }
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ConfigurationDeclarationEpilogue)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConfigurationDeclarationEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::End))
            .nth(0)
    }
    pub fn configuration_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Configuration))
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
pub struct EntityDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for EntityDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::EntityDeclaration => Some(EntityDeclarationSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::EntityDeclaration)
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
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::EntityDeclarationPreamble => Some(EntityDeclarationPreambleSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::EntityDeclarationPreamble)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl EntityDeclarationPreambleSyntax {
    pub fn entity_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Entity))
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
pub struct EntityDeclarationEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for EntityDeclarationEpilogueSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::EntityDeclarationEpilogue => Some(EntityDeclarationEpilogueSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::EntityDeclarationEpilogue)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl EntityDeclarationEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::End))
            .nth(0)
    }
    pub fn entity_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Entity))
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
pub struct EntityHeaderSyntax(pub(crate) SyntaxNode);
impl AstNode for EntityHeaderSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::EntityHeader => Some(EntityHeaderSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::EntityHeader)
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
