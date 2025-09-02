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
pub struct ArchitectureBodySyntax(pub(crate) SyntaxNode);
impl AstNode for ArchitectureBodySyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ArchitectureBody => Some(ArchitectureBodySyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ArchitectureBodySyntax {
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
    pub fn declarations(&self) -> impl Iterator<Item = DeclarationSyntax> + use<'_> {
        self.0.children().filter_map(DeclarationSyntax::cast)
    }
    pub fn begin_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Begin))
            .nth(0)
    }
    pub fn concurrent_statements(
        &self,
    ) -> impl Iterator<Item = ConcurrentStatementSyntax> + use<'_> {
        self.0
            .children()
            .filter_map(ConcurrentStatementSyntax::cast)
    }
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::End))
            .nth(0)
    }
    pub fn trailing_architecture_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Architecture))
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
pub struct BlockConfigurationSyntax(pub(crate) SyntaxNode);
impl AstNode for BlockConfigurationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::BlockConfiguration => Some(BlockConfigurationSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl BlockConfigurationSyntax {
    pub fn for_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::For))
            .nth(0)
    }
    pub fn block_specification(&self) -> Option<BlockSpecificationSyntax> {
        self.0
            .children()
            .filter_map(BlockSpecificationSyntax::cast)
            .nth(0)
    }
    pub fn use_clauses(&self) -> impl Iterator<Item = UseClauseSyntax> + use<'_> {
        self.0.children().filter_map(UseClauseSyntax::cast)
    }
    pub fn configuration_items(&self) -> impl Iterator<Item = ConfigurationItemSyntax> + use<'_> {
        self.0.children().filter_map(ConfigurationItemSyntax::cast)
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
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ParenthesizedGenerateSpecificationSyntax(pub(crate) SyntaxNode);
impl AstNode for ParenthesizedGenerateSpecificationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ParenthesizedGenerateSpecification => {
                Some(ParenthesizedGenerateSpecificationSyntax(node))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ParenthesizedGenerateSpecificationSyntax {
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == LeftPar)
            .nth(0)
    }
    pub fn generate_specification(&self) -> Option<GenerateSpecificationSyntax> {
        self.0
            .children()
            .filter_map(GenerateSpecificationSyntax::cast)
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
pub struct BlockSpecificationSyntax(pub(crate) SyntaxNode);
impl AstNode for BlockSpecificationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::BlockSpecification => Some(BlockSpecificationSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl BlockSpecificationSyntax {
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
    pub fn parenthesized_generate_specification(
        &self,
    ) -> Option<ParenthesizedGenerateSpecificationSyntax> {
        self.0
            .children()
            .filter_map(ParenthesizedGenerateSpecificationSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum GenerateSpecificationSyntax {
    DiscreteRange(DiscreteRangeSyntax),
    Expression(ExpressionSyntax),
}
impl AstNode for GenerateSpecificationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::DiscreteRange => Some(GenerateSpecificationSyntax::DiscreteRange(
                DiscreteRangeSyntax::cast(node).unwrap(),
            )),
            NodeKind::Expression => Some(GenerateSpecificationSyntax::Expression(
                ExpressionSyntax::cast(node).unwrap(),
            )),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            GenerateSpecificationSyntax::DiscreteRange(inner) => inner.raw(),
            GenerateSpecificationSyntax::Expression(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SemiColonTerminatedBindingIndicationSyntax(pub(crate) SyntaxNode);
impl AstNode for SemiColonTerminatedBindingIndicationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SemiColonTerminatedBindingIndication => {
                Some(SemiColonTerminatedBindingIndicationSyntax(node))
            }
            _ => None,
        }
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
            .filter(|token| token.kind() == SemiColon)
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
pub struct ComponentConfigurationSyntax(pub(crate) SyntaxNode);
impl AstNode for ComponentConfigurationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ComponentConfiguration => Some(ComponentConfigurationSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ComponentConfigurationSyntax {
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
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConfigurationDeclarationSyntax {
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
    pub fn declarations(&self) -> impl Iterator<Item = DeclarationSyntax> + use<'_> {
        self.0.children().filter_map(DeclarationSyntax::cast)
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
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::End))
            .nth(0)
    }
    pub fn trailing_configuration_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Configuration))
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
pub enum ConfigurationItemSyntax {
    BlockConfiguration(BlockConfigurationSyntax),
    ComponentConfiguration(ComponentConfigurationSyntax),
}
impl AstNode for ConfigurationItemSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::BlockConfiguration => Some(ConfigurationItemSyntax::BlockConfiguration(
                BlockConfigurationSyntax::cast(node).unwrap(),
            )),
            NodeKind::ComponentConfiguration => {
                Some(ConfigurationItemSyntax::ComponentConfiguration(
                    ComponentConfigurationSyntax::cast(node).unwrap(),
                ))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            ConfigurationItemSyntax::BlockConfiguration(inner) => inner.raw(),
            ConfigurationItemSyntax::ComponentConfiguration(inner) => inner.raw(),
        }
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
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl EntityDeclarationSyntax {
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
    pub fn entity_header(&self) -> Option<EntityHeaderSyntax> {
        self.0
            .children()
            .filter_map(EntityHeaderSyntax::cast)
            .nth(0)
    }
    pub fn declarations(&self) -> impl Iterator<Item = DeclarationSyntax> + use<'_> {
        self.0.children().filter_map(DeclarationSyntax::cast)
    }
    pub fn begin_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Begin))
            .nth(0)
    }
    pub fn concurrent_statements(
        &self,
    ) -> impl Iterator<Item = ConcurrentStatementSyntax> + use<'_> {
        self.0
            .children()
            .filter_map(ConcurrentStatementSyntax::cast)
    }
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::End))
            .nth(0)
    }
    pub fn trailing_entity_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Entity))
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
pub struct EntityHeaderSyntax(pub(crate) SyntaxNode);
impl AstNode for EntityHeaderSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::EntityHeader => Some(EntityHeaderSyntax(node)),
            _ => None,
        }
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
