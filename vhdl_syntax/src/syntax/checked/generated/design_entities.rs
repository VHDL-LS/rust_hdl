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
pub struct CheckedArchitectureBody(ArchitectureBodySyntax);
impl CheckedNode for CheckedArchitectureBody {
    type Syntax = ArchitectureBodySyntax;
    fn cast_unchecked(syntax: ArchitectureBodySyntax) -> Self {
        CheckedArchitectureBody(syntax)
    }
    fn raw(&self) -> ArchitectureBodySyntax {
        self.0.clone()
    }
}
impl CheckedArchitectureBody {
    pub fn architecture_preamble(&self) -> CheckedArchitecturePreamble {
        CheckedArchitecturePreamble::cast_unchecked(self.0.architecture_preamble().unwrap())
    }
    pub fn declarations(&self) -> Option<CheckedDeclarations> {
        self.0
            .declarations()
            .map(CheckedDeclarations::cast_unchecked)
    }
    pub fn declaration_statement_separator(&self) -> CheckedDeclarationStatementSeparator {
        CheckedDeclarationStatementSeparator::cast_unchecked(
            self.0.declaration_statement_separator().unwrap(),
        )
    }
    pub fn concurrent_statements(&self) -> Option<CheckedConcurrentStatements> {
        self.0
            .concurrent_statements()
            .map(CheckedConcurrentStatements::cast_unchecked)
    }
    pub fn architecture_epilogue(&self) -> CheckedArchitectureEpilogue {
        CheckedArchitectureEpilogue::cast_unchecked(self.0.architecture_epilogue().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedArchitecturePreamble(ArchitecturePreambleSyntax);
impl CheckedNode for CheckedArchitecturePreamble {
    type Syntax = ArchitecturePreambleSyntax;
    fn cast_unchecked(syntax: ArchitecturePreambleSyntax) -> Self {
        CheckedArchitecturePreamble(syntax)
    }
    fn raw(&self) -> ArchitecturePreambleSyntax {
        self.0.clone()
    }
}
impl CheckedArchitecturePreamble {
    pub fn architecture_token(&self) -> SyntaxToken {
        self.0.architecture_token().unwrap()
    }
    pub fn name_token(&self) -> SyntaxToken {
        self.0.name_token().unwrap()
    }
    pub fn of_token(&self) -> SyntaxToken {
        self.0.of_token().unwrap()
    }
    pub fn entity_name(&self) -> CheckedName {
        CheckedName::cast_unchecked(self.0.entity_name().unwrap())
    }
    pub fn is_token(&self) -> SyntaxToken {
        self.0.is_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedArchitectureEpilogue(ArchitectureEpilogueSyntax);
impl CheckedNode for CheckedArchitectureEpilogue {
    type Syntax = ArchitectureEpilogueSyntax;
    fn cast_unchecked(syntax: ArchitectureEpilogueSyntax) -> Self {
        CheckedArchitectureEpilogue(syntax)
    }
    fn raw(&self) -> ArchitectureEpilogueSyntax {
        self.0.clone()
    }
}
impl CheckedArchitectureEpilogue {
    pub fn end_token(&self) -> SyntaxToken {
        self.0.end_token().unwrap()
    }
    pub fn architecture_token(&self) -> Option<SyntaxToken> {
        self.0.architecture_token()
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0.identifier_token()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedBlockConfigurationItem(BlockConfigurationItemSyntax);
impl CheckedNode for CheckedBlockConfigurationItem {
    type Syntax = BlockConfigurationItemSyntax;
    fn cast_unchecked(syntax: BlockConfigurationItemSyntax) -> Self {
        CheckedBlockConfigurationItem(syntax)
    }
    fn raw(&self) -> BlockConfigurationItemSyntax {
        self.0.clone()
    }
}
impl CheckedBlockConfigurationItem {
    pub fn block_configuration(&self) -> CheckedBlockConfiguration {
        CheckedBlockConfiguration::cast_unchecked(self.0.block_configuration().unwrap())
    }
}
#[derive(Debug, Clone)]
pub enum CheckedConfigurationItem {
    BlockConfigurationItem(CheckedBlockConfigurationItem),
    ComponentConfiguration(CheckedComponentConfiguration),
}
impl CheckedNode for CheckedConfigurationItem {
    type Syntax = ConfigurationItemSyntax;
    fn cast_unchecked(syntax: ConfigurationItemSyntax) -> Self {
        match syntax {
            ConfigurationItemSyntax::BlockConfigurationItem(inner) => {
                CheckedConfigurationItem::BlockConfigurationItem(
                    CheckedBlockConfigurationItem::cast_unchecked(inner),
                )
            }
            ConfigurationItemSyntax::ComponentConfiguration(inner) => {
                CheckedConfigurationItem::ComponentConfiguration(
                    CheckedComponentConfiguration::cast_unchecked(inner),
                )
            }
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedConfigurationItem::BlockConfigurationItem(inner) => {
                ConfigurationItemSyntax::BlockConfigurationItem(inner.raw())
            }
            CheckedConfigurationItem::ComponentConfiguration(inner) => {
                ConfigurationItemSyntax::ComponentConfiguration(inner.raw())
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedBlockConfiguration(BlockConfigurationSyntax);
impl CheckedNode for CheckedBlockConfiguration {
    type Syntax = BlockConfigurationSyntax;
    fn cast_unchecked(syntax: BlockConfigurationSyntax) -> Self {
        CheckedBlockConfiguration(syntax)
    }
    fn raw(&self) -> BlockConfigurationSyntax {
        self.0.clone()
    }
}
impl CheckedBlockConfiguration {
    pub fn block_configuration_preamble(&self) -> CheckedBlockConfigurationPreamble {
        CheckedBlockConfigurationPreamble::cast_unchecked(
            self.0.block_configuration_preamble().unwrap(),
        )
    }
    pub fn block_configuration_items(&self) -> Option<CheckedBlockConfigurationItems> {
        self.0
            .block_configuration_items()
            .map(CheckedBlockConfigurationItems::cast_unchecked)
    }
    pub fn block_configuration_epilogue(&self) -> CheckedBlockConfigurationEpilogue {
        CheckedBlockConfigurationEpilogue::cast_unchecked(
            self.0.block_configuration_epilogue().unwrap(),
        )
    }
}
#[derive(Debug, Clone)]
pub struct CheckedBlockConfigurationItems(BlockConfigurationItemsSyntax);
impl CheckedNode for CheckedBlockConfigurationItems {
    type Syntax = BlockConfigurationItemsSyntax;
    fn cast_unchecked(syntax: BlockConfigurationItemsSyntax) -> Self {
        CheckedBlockConfigurationItems(syntax)
    }
    fn raw(&self) -> BlockConfigurationItemsSyntax {
        self.0.clone()
    }
}
impl CheckedBlockConfigurationItems {
    pub fn use_clauses(&self) -> impl Iterator<Item = CheckedUseClause> + use<'_> {
        self.0.use_clauses().map(CheckedUseClause::cast_unchecked)
    }
    pub fn configuration_items(&self) -> impl Iterator<Item = CheckedConfigurationItem> + use<'_> {
        self.0
            .configuration_items()
            .map(CheckedConfigurationItem::cast_unchecked)
    }
}
#[derive(Debug, Clone)]
pub struct CheckedBlockConfigurationPreamble(BlockConfigurationPreambleSyntax);
impl CheckedNode for CheckedBlockConfigurationPreamble {
    type Syntax = BlockConfigurationPreambleSyntax;
    fn cast_unchecked(syntax: BlockConfigurationPreambleSyntax) -> Self {
        CheckedBlockConfigurationPreamble(syntax)
    }
    fn raw(&self) -> BlockConfigurationPreambleSyntax {
        self.0.clone()
    }
}
impl CheckedBlockConfigurationPreamble {
    pub fn for_token(&self) -> SyntaxToken {
        self.0.for_token().unwrap()
    }
    pub fn name(&self) -> CheckedName {
        CheckedName::cast_unchecked(self.0.name().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedBlockConfigurationEpilogue(BlockConfigurationEpilogueSyntax);
impl CheckedNode for CheckedBlockConfigurationEpilogue {
    type Syntax = BlockConfigurationEpilogueSyntax;
    fn cast_unchecked(syntax: BlockConfigurationEpilogueSyntax) -> Self {
        CheckedBlockConfigurationEpilogue(syntax)
    }
    fn raw(&self) -> BlockConfigurationEpilogueSyntax {
        self.0.clone()
    }
}
impl CheckedBlockConfigurationEpilogue {
    pub fn end_token(&self) -> SyntaxToken {
        self.0.end_token().unwrap()
    }
    pub fn for_token(&self) -> SyntaxToken {
        self.0.for_token().unwrap()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedComponentConfiguration(ComponentConfigurationSyntax);
impl CheckedNode for CheckedComponentConfiguration {
    type Syntax = ComponentConfigurationSyntax;
    fn cast_unchecked(syntax: ComponentConfigurationSyntax) -> Self {
        CheckedComponentConfiguration(syntax)
    }
    fn raw(&self) -> ComponentConfigurationSyntax {
        self.0.clone()
    }
}
impl CheckedComponentConfiguration {
    pub fn component_configuration_preamble(&self) -> CheckedComponentConfigurationPreamble {
        CheckedComponentConfigurationPreamble::cast_unchecked(
            self.0.component_configuration_preamble().unwrap(),
        )
    }
    pub fn component_configuration_items(&self) -> Option<CheckedComponentConfigurationItems> {
        self.0
            .component_configuration_items()
            .map(CheckedComponentConfigurationItems::cast_unchecked)
    }
    pub fn component_configuration_epilogue(&self) -> CheckedComponentConfigurationEpilogue {
        CheckedComponentConfigurationEpilogue::cast_unchecked(
            self.0.component_configuration_epilogue().unwrap(),
        )
    }
}
#[derive(Debug, Clone)]
pub struct CheckedSemiColonTerminatedBindingIndication(SemiColonTerminatedBindingIndicationSyntax);
impl CheckedNode for CheckedSemiColonTerminatedBindingIndication {
    type Syntax = SemiColonTerminatedBindingIndicationSyntax;
    fn cast_unchecked(syntax: SemiColonTerminatedBindingIndicationSyntax) -> Self {
        CheckedSemiColonTerminatedBindingIndication(syntax)
    }
    fn raw(&self) -> SemiColonTerminatedBindingIndicationSyntax {
        self.0.clone()
    }
}
impl CheckedSemiColonTerminatedBindingIndication {
    pub fn binding_indication(&self) -> Option<CheckedBindingIndication> {
        self.0
            .binding_indication()
            .map(CheckedBindingIndication::cast_unchecked)
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedSemiColonTerminatedVerificationUnitBindingIndication(
    SemiColonTerminatedVerificationUnitBindingIndicationSyntax,
);
impl CheckedNode for CheckedSemiColonTerminatedVerificationUnitBindingIndication {
    type Syntax = SemiColonTerminatedVerificationUnitBindingIndicationSyntax;
    fn cast_unchecked(syntax: SemiColonTerminatedVerificationUnitBindingIndicationSyntax) -> Self {
        CheckedSemiColonTerminatedVerificationUnitBindingIndication(syntax)
    }
    fn raw(&self) -> SemiColonTerminatedVerificationUnitBindingIndicationSyntax {
        self.0.clone()
    }
}
impl CheckedSemiColonTerminatedVerificationUnitBindingIndication {
    pub fn verification_unit_binding_indication(&self) -> CheckedVerificationUnitBindingIndication {
        CheckedVerificationUnitBindingIndication::cast_unchecked(
            self.0.verification_unit_binding_indication().unwrap(),
        )
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedComponentConfigurationItems(ComponentConfigurationItemsSyntax);
impl CheckedNode for CheckedComponentConfigurationItems {
    type Syntax = ComponentConfigurationItemsSyntax;
    fn cast_unchecked(syntax: ComponentConfigurationItemsSyntax) -> Self {
        CheckedComponentConfigurationItems(syntax)
    }
    fn raw(&self) -> ComponentConfigurationItemsSyntax {
        self.0.clone()
    }
}
impl CheckedComponentConfigurationItems {
    pub fn semi_colon_terminated_binding_indication(
        &self,
    ) -> Option<CheckedSemiColonTerminatedBindingIndication> {
        self.0
            .semi_colon_terminated_binding_indication()
            .map(CheckedSemiColonTerminatedBindingIndication::cast_unchecked)
    }
    pub fn semi_colon_terminated_verification_unit_binding_indications(
        &self,
    ) -> impl Iterator<Item = CheckedSemiColonTerminatedVerificationUnitBindingIndication> + use<'_>
    {
        self.0
            .semi_colon_terminated_verification_unit_binding_indications()
            .map(CheckedSemiColonTerminatedVerificationUnitBindingIndication::cast_unchecked)
    }
    pub fn block_configuration(&self) -> Option<CheckedBlockConfiguration> {
        self.0
            .block_configuration()
            .map(CheckedBlockConfiguration::cast_unchecked)
    }
}
#[derive(Debug, Clone)]
pub struct CheckedComponentConfigurationPreamble(ComponentConfigurationPreambleSyntax);
impl CheckedNode for CheckedComponentConfigurationPreamble {
    type Syntax = ComponentConfigurationPreambleSyntax;
    fn cast_unchecked(syntax: ComponentConfigurationPreambleSyntax) -> Self {
        CheckedComponentConfigurationPreamble(syntax)
    }
    fn raw(&self) -> ComponentConfigurationPreambleSyntax {
        self.0.clone()
    }
}
impl CheckedComponentConfigurationPreamble {
    pub fn for_token(&self) -> SyntaxToken {
        self.0.for_token().unwrap()
    }
    pub fn component_specification(&self) -> CheckedComponentSpecification {
        CheckedComponentSpecification::cast_unchecked(self.0.component_specification().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedComponentConfigurationEpilogue(ComponentConfigurationEpilogueSyntax);
impl CheckedNode for CheckedComponentConfigurationEpilogue {
    type Syntax = ComponentConfigurationEpilogueSyntax;
    fn cast_unchecked(syntax: ComponentConfigurationEpilogueSyntax) -> Self {
        CheckedComponentConfigurationEpilogue(syntax)
    }
    fn raw(&self) -> ComponentConfigurationEpilogueSyntax {
        self.0.clone()
    }
}
impl CheckedComponentConfigurationEpilogue {
    pub fn end_token(&self) -> SyntaxToken {
        self.0.end_token().unwrap()
    }
    pub fn for_token(&self) -> SyntaxToken {
        self.0.for_token().unwrap()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedConfigurationDeclaration(ConfigurationDeclarationSyntax);
impl CheckedNode for CheckedConfigurationDeclaration {
    type Syntax = ConfigurationDeclarationSyntax;
    fn cast_unchecked(syntax: ConfigurationDeclarationSyntax) -> Self {
        CheckedConfigurationDeclaration(syntax)
    }
    fn raw(&self) -> ConfigurationDeclarationSyntax {
        self.0.clone()
    }
}
impl CheckedConfigurationDeclaration {
    pub fn configuration_declaration_preamble(&self) -> CheckedConfigurationDeclarationPreamble {
        CheckedConfigurationDeclarationPreamble::cast_unchecked(
            self.0.configuration_declaration_preamble().unwrap(),
        )
    }
    pub fn configuration_declaration_items(&self) -> CheckedConfigurationDeclarationItems {
        CheckedConfigurationDeclarationItems::cast_unchecked(
            self.0.configuration_declaration_items().unwrap(),
        )
    }
    pub fn configuration_declaration_epilogue(&self) -> CheckedConfigurationDeclarationEpilogue {
        CheckedConfigurationDeclarationEpilogue::cast_unchecked(
            self.0.configuration_declaration_epilogue().unwrap(),
        )
    }
}
#[derive(Debug, Clone)]
pub struct CheckedConfigurationDeclarationItems(ConfigurationDeclarationItemsSyntax);
impl CheckedNode for CheckedConfigurationDeclarationItems {
    type Syntax = ConfigurationDeclarationItemsSyntax;
    fn cast_unchecked(syntax: ConfigurationDeclarationItemsSyntax) -> Self {
        CheckedConfigurationDeclarationItems(syntax)
    }
    fn raw(&self) -> ConfigurationDeclarationItemsSyntax {
        self.0.clone()
    }
}
impl CheckedConfigurationDeclarationItems {
    pub fn declarations(&self) -> Option<CheckedDeclarations> {
        self.0
            .declarations()
            .map(CheckedDeclarations::cast_unchecked)
    }
    pub fn semi_colon_terminated_verification_unit_binding_indications(
        &self,
    ) -> impl Iterator<Item = CheckedSemiColonTerminatedVerificationUnitBindingIndication> + use<'_>
    {
        self.0
            .semi_colon_terminated_verification_unit_binding_indications()
            .map(CheckedSemiColonTerminatedVerificationUnitBindingIndication::cast_unchecked)
    }
    pub fn block_configuration(&self) -> CheckedBlockConfiguration {
        CheckedBlockConfiguration::cast_unchecked(self.0.block_configuration().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedConfigurationDeclarationPreamble(ConfigurationDeclarationPreambleSyntax);
impl CheckedNode for CheckedConfigurationDeclarationPreamble {
    type Syntax = ConfigurationDeclarationPreambleSyntax;
    fn cast_unchecked(syntax: ConfigurationDeclarationPreambleSyntax) -> Self {
        CheckedConfigurationDeclarationPreamble(syntax)
    }
    fn raw(&self) -> ConfigurationDeclarationPreambleSyntax {
        self.0.clone()
    }
}
impl CheckedConfigurationDeclarationPreamble {
    pub fn configuration_token(&self) -> SyntaxToken {
        self.0.configuration_token().unwrap()
    }
    pub fn name_token(&self) -> SyntaxToken {
        self.0.name_token().unwrap()
    }
    pub fn of_token(&self) -> SyntaxToken {
        self.0.of_token().unwrap()
    }
    pub fn entity_name(&self) -> CheckedName {
        CheckedName::cast_unchecked(self.0.entity_name().unwrap())
    }
    pub fn is_token(&self) -> SyntaxToken {
        self.0.is_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedConfigurationDeclarationEpilogue(ConfigurationDeclarationEpilogueSyntax);
impl CheckedNode for CheckedConfigurationDeclarationEpilogue {
    type Syntax = ConfigurationDeclarationEpilogueSyntax;
    fn cast_unchecked(syntax: ConfigurationDeclarationEpilogueSyntax) -> Self {
        CheckedConfigurationDeclarationEpilogue(syntax)
    }
    fn raw(&self) -> ConfigurationDeclarationEpilogueSyntax {
        self.0.clone()
    }
}
impl CheckedConfigurationDeclarationEpilogue {
    pub fn end_token(&self) -> SyntaxToken {
        self.0.end_token().unwrap()
    }
    pub fn configuration_token(&self) -> Option<SyntaxToken> {
        self.0.configuration_token()
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0.identifier_token()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedEntityDeclaration(EntityDeclarationSyntax);
impl CheckedNode for CheckedEntityDeclaration {
    type Syntax = EntityDeclarationSyntax;
    fn cast_unchecked(syntax: EntityDeclarationSyntax) -> Self {
        CheckedEntityDeclaration(syntax)
    }
    fn raw(&self) -> EntityDeclarationSyntax {
        self.0.clone()
    }
}
impl CheckedEntityDeclaration {
    pub fn entity_declaration_preamble(&self) -> CheckedEntityDeclarationPreamble {
        CheckedEntityDeclarationPreamble::cast_unchecked(
            self.0.entity_declaration_preamble().unwrap(),
        )
    }
    pub fn entity_header(&self) -> Option<CheckedEntityHeader> {
        self.0
            .entity_header()
            .map(CheckedEntityHeader::cast_unchecked)
    }
    pub fn declarations(&self) -> Option<CheckedDeclarations> {
        self.0
            .declarations()
            .map(CheckedDeclarations::cast_unchecked)
    }
    pub fn declaration_statement_separator(&self) -> Option<CheckedDeclarationStatementSeparator> {
        self.0
            .declaration_statement_separator()
            .map(CheckedDeclarationStatementSeparator::cast_unchecked)
    }
    pub fn concurrent_statements(&self) -> Option<CheckedConcurrentStatements> {
        self.0
            .concurrent_statements()
            .map(CheckedConcurrentStatements::cast_unchecked)
    }
    pub fn entity_declaration_epilogue(&self) -> CheckedEntityDeclarationEpilogue {
        CheckedEntityDeclarationEpilogue::cast_unchecked(
            self.0.entity_declaration_epilogue().unwrap(),
        )
    }
}
#[derive(Debug, Clone)]
pub struct CheckedEntityDeclarationPreamble(EntityDeclarationPreambleSyntax);
impl CheckedNode for CheckedEntityDeclarationPreamble {
    type Syntax = EntityDeclarationPreambleSyntax;
    fn cast_unchecked(syntax: EntityDeclarationPreambleSyntax) -> Self {
        CheckedEntityDeclarationPreamble(syntax)
    }
    fn raw(&self) -> EntityDeclarationPreambleSyntax {
        self.0.clone()
    }
}
impl CheckedEntityDeclarationPreamble {
    pub fn entity_token(&self) -> SyntaxToken {
        self.0.entity_token().unwrap()
    }
    pub fn name_token(&self) -> SyntaxToken {
        self.0.name_token().unwrap()
    }
    pub fn is_token(&self) -> SyntaxToken {
        self.0.is_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedEntityDeclarationEpilogue(EntityDeclarationEpilogueSyntax);
impl CheckedNode for CheckedEntityDeclarationEpilogue {
    type Syntax = EntityDeclarationEpilogueSyntax;
    fn cast_unchecked(syntax: EntityDeclarationEpilogueSyntax) -> Self {
        CheckedEntityDeclarationEpilogue(syntax)
    }
    fn raw(&self) -> EntityDeclarationEpilogueSyntax {
        self.0.clone()
    }
}
impl CheckedEntityDeclarationEpilogue {
    pub fn end_token(&self) -> SyntaxToken {
        self.0.end_token().unwrap()
    }
    pub fn entity_token(&self) -> Option<SyntaxToken> {
        self.0.entity_token()
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0.identifier_token()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedEntityHeader(EntityHeaderSyntax);
impl CheckedNode for CheckedEntityHeader {
    type Syntax = EntityHeaderSyntax;
    fn cast_unchecked(syntax: EntityHeaderSyntax) -> Self {
        CheckedEntityHeader(syntax)
    }
    fn raw(&self) -> EntityHeaderSyntax {
        self.0.clone()
    }
}
impl CheckedEntityHeader {
    pub fn generic_clause(&self) -> Option<CheckedGenericClause> {
        self.0
            .generic_clause()
            .map(CheckedGenericClause::cast_unchecked)
    }
    pub fn port_clause(&self) -> Option<CheckedPortClause> {
        self.0.port_clause().map(CheckedPortClause::cast_unchecked)
    }
}
