// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2026, Lukas Scheller lukasscheller@icloud.com
use super::*;
use crate::syntax::node::SyntaxToken;
use crate::syntax::validate::valid_node::Valid;
impl Valid<AbsolutePathnameSyntax> {
    pub fn dot_token(&self) -> SyntaxToken {
        self.inner().dot_token().expect("node must be valid")
    }
    pub fn partial_pathname(&self) -> Valid<PartialPathnameSyntax> {
        Valid::new_unchecked(self.inner().partial_pathname().expect("node must be valid"))
    }
}
impl Valid<AccessTypeDefinitionSyntax> {
    pub fn access_token(&self) -> SyntaxToken {
        self.inner().access_token().expect("node must be valid")
    }
    pub fn subtype_indication(&self) -> Valid<SubtypeIndicationSyntax> {
        Valid::new_unchecked(
            self.inner()
                .subtype_indication()
                .expect("node must be valid"),
        )
    }
}
impl Valid<ActualPartSyntax> {
    pub fn inertial_token(&self) -> Option<SyntaxToken> {
        self.inner().inertial_token()
    }
    pub fn actual_part_body(&self) -> Valid<ActualPartBodySyntax> {
        Valid::new_unchecked(self.inner().actual_part_body().expect("node must be valid"))
    }
}
#[derive(Debug, Clone)]
pub enum ValidActualPartBody {
    ActualPartExpression(Valid<ActualPartExpressionSyntax>),
    ActualPartSubtypeIndication(Valid<ActualPartSubtypeIndicationSyntax>),
    ActualPartOpen(Valid<ActualPartOpenSyntax>),
}
impl Valid<ActualPartBodySyntax> {
    pub fn alternative(&self) -> ValidActualPartBody {
        match self.inner() {
            ActualPartBodySyntax::ActualPartExpression(inner) => {
                ValidActualPartBody::ActualPartExpression(Valid::new_unchecked(inner.clone()))
            }
            ActualPartBodySyntax::ActualPartSubtypeIndication(inner) => {
                ValidActualPartBody::ActualPartSubtypeIndication(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            ActualPartBodySyntax::ActualPartOpen(inner) => {
                ValidActualPartBody::ActualPartOpen(Valid::new_unchecked(inner.clone()))
            }
        }
    }
}
impl Valid<ActualPartExpressionSyntax> {
    pub fn expression(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().expression().expect("node must be valid"))
    }
}
impl Valid<ActualPartOpenSyntax> {
    pub fn open_token(&self) -> SyntaxToken {
        self.inner().open_token().expect("node must be valid")
    }
}
impl Valid<ActualPartSubtypeIndicationSyntax> {
    pub fn subtype_indication(&self) -> Valid<SubtypeIndicationSyntax> {
        Valid::new_unchecked(
            self.inner()
                .subtype_indication()
                .expect("node must be valid"),
        )
    }
}
impl Valid<AfterClauseSyntax> {
    pub fn after_token(&self) -> SyntaxToken {
        self.inner().after_token().expect("node must be valid")
    }
    pub fn expression(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().expression().expect("node must be valid"))
    }
}
impl Valid<AggregateSyntax> {
    pub fn left_par_token(&self) -> SyntaxToken {
        self.inner().left_par_token().expect("node must be valid")
    }
    pub fn element_association_list(&self) -> Valid<ElementAssociationListSyntax> {
        Valid::new_unchecked(
            self.inner()
                .element_association_list()
                .expect("node must be valid"),
        )
    }
    pub fn right_par_token(&self) -> SyntaxToken {
        self.inner().right_par_token().expect("node must be valid")
    }
}
impl Valid<AggregateTargetSyntax> {
    pub fn aggregate(&self) -> Valid<AggregateSyntax> {
        Valid::new_unchecked(self.inner().aggregate().expect("node must be valid"))
    }
}
impl Valid<AliasDeclarationSyntax> {
    pub fn alias_token(&self) -> SyntaxToken {
        self.inner().alias_token().expect("node must be valid")
    }
    pub fn alias_designator(&self) -> AliasDesignatorSyntax {
        self.inner().alias_designator().expect("node must be valid")
    }
    pub fn alias_subtype(&self) -> Option<Valid<AliasSubtypeSyntax>> {
        self.inner().alias_subtype().map(Valid::new_unchecked)
    }
    pub fn is_token(&self) -> SyntaxToken {
        self.inner().is_token().expect("node must be valid")
    }
    pub fn name(&self) -> Valid<NameSyntax> {
        Valid::new_unchecked(self.inner().name().expect("node must be valid"))
    }
    pub fn signature(&self) -> Option<Valid<SignatureSyntax>> {
        self.inner().signature().map(Valid::new_unchecked)
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<AliasSubtypeSyntax> {
    pub fn colon_token(&self) -> SyntaxToken {
        self.inner().colon_token().expect("node must be valid")
    }
    pub fn subtype_indication(&self) -> Valid<SubtypeIndicationSyntax> {
        Valid::new_unchecked(
            self.inner()
                .subtype_indication()
                .expect("node must be valid"),
        )
    }
}
impl Valid<AllSensitivityListSyntax> {
    pub fn all_token(&self) -> SyntaxToken {
        self.inner().all_token().expect("node must be valid")
    }
}
impl Valid<AllocatorSyntax> {
    pub fn new_token(&self) -> SyntaxToken {
        self.inner().new_token().expect("node must be valid")
    }
    pub fn expression(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().expression().expect("node must be valid"))
    }
}
impl Valid<ArchitectureBodySyntax> {
    pub fn architecture_preamble(&self) -> Valid<ArchitecturePreambleSyntax> {
        Valid::new_unchecked(
            self.inner()
                .architecture_preamble()
                .expect("node must be valid"),
        )
    }
    pub fn architecture_declarative_part(
        &self,
    ) -> Option<Valid<ArchitectureDeclarativePartSyntax>> {
        self.inner()
            .architecture_declarative_part()
            .map(Valid::new_unchecked)
    }
    pub fn declaration_statement_separator(&self) -> Valid<DeclarationStatementSeparatorSyntax> {
        Valid::new_unchecked(
            self.inner()
                .declaration_statement_separator()
                .expect("node must be valid"),
        )
    }
    pub fn architecture_statement_part(&self) -> Option<Valid<ArchitectureStatementPartSyntax>> {
        self.inner()
            .architecture_statement_part()
            .map(Valid::new_unchecked)
    }
    pub fn architecture_epilogue(&self) -> Valid<ArchitectureEpilogueSyntax> {
        Valid::new_unchecked(
            self.inner()
                .architecture_epilogue()
                .expect("node must be valid"),
        )
    }
}
impl Valid<ArchitectureDeclarativePartSyntax> {
    pub fn block_declarative_items(
        &self,
    ) -> impl Iterator<Item = Valid<BlockDeclarativeItemSyntax>> + use<'_> {
        self.inner()
            .block_declarative_items()
            .map(Valid::new_unchecked)
    }
}
impl Valid<ArchitectureEpilogueSyntax> {
    pub fn end_token(&self) -> SyntaxToken {
        self.inner().end_token().expect("node must be valid")
    }
    pub fn architecture_token(&self) -> Option<SyntaxToken> {
        self.inner().architecture_token()
    }
    pub fn simple_name(&self) -> Option<SyntaxToken> {
        self.inner().simple_name()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<ArchitecturePreambleSyntax> {
    pub fn architecture_token(&self) -> SyntaxToken {
        self.inner()
            .architecture_token()
            .expect("node must be valid")
    }
    pub fn identifier_token(&self) -> SyntaxToken {
        self.inner().identifier_token().expect("node must be valid")
    }
    pub fn of_token(&self) -> SyntaxToken {
        self.inner().of_token().expect("node must be valid")
    }
    pub fn name(&self) -> Valid<NameSyntax> {
        Valid::new_unchecked(self.inner().name().expect("node must be valid"))
    }
    pub fn is_token(&self) -> SyntaxToken {
        self.inner().is_token().expect("node must be valid")
    }
}
impl Valid<ArchitectureStatementPartSyntax> {
    pub fn concurrent_statements(
        &self,
    ) -> impl Iterator<Item = Valid<ConcurrentStatementSyntax>> + use<'_> {
        self.inner()
            .concurrent_statements()
            .map(Valid::new_unchecked)
    }
}
#[derive(Debug, Clone)]
pub enum ValidArrayTypeDefinition {
    UnboundedArrayDefinition(Valid<UnboundedArrayDefinitionSyntax>),
    ConstrainedArrayDefinition(Valid<ConstrainedArrayDefinitionSyntax>),
}
impl Valid<ArrayTypeDefinitionSyntax> {
    pub fn alternative(&self) -> ValidArrayTypeDefinition {
        match self.inner() {
            ArrayTypeDefinitionSyntax::UnboundedArrayDefinition(inner) => {
                ValidArrayTypeDefinition::UnboundedArrayDefinition(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            ArrayTypeDefinitionSyntax::ConstrainedArrayDefinition(inner) => {
                ValidArrayTypeDefinition::ConstrainedArrayDefinition(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
        }
    }
}
impl Valid<AssertionSyntax> {
    pub fn assert_token(&self) -> SyntaxToken {
        self.inner().assert_token().expect("node must be valid")
    }
    pub fn condition(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().condition().expect("node must be valid"))
    }
    pub fn report_clause(&self) -> Option<Valid<ReportClauseSyntax>> {
        self.inner().report_clause().map(Valid::new_unchecked)
    }
    pub fn severity_clause(&self) -> Option<Valid<SeverityClauseSyntax>> {
        self.inner().severity_clause().map(Valid::new_unchecked)
    }
}
impl Valid<AssertionStatementSyntax> {
    pub fn stmt_label(&self) -> Option<Valid<StmtLabelSyntax>> {
        self.inner().stmt_label().map(Valid::new_unchecked)
    }
    pub fn assertion(&self) -> Valid<AssertionSyntax> {
        Valid::new_unchecked(self.inner().assertion().expect("node must be valid"))
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<AssociationElementSyntax> {
    pub fn formal(&self) -> Option<Valid<FormalSyntax>> {
        self.inner().formal().map(Valid::new_unchecked)
    }
    pub fn actual_part(&self) -> Valid<ActualPartSyntax> {
        Valid::new_unchecked(self.inner().actual_part().expect("node must be valid"))
    }
}
impl Valid<AssociationListSyntax> {
    pub fn association_elements(
        &self,
    ) -> impl Iterator<Item = Valid<AssociationElementSyntax>> + use<'_> {
        self.inner()
            .association_elements()
            .map(Valid::new_unchecked)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.inner().comma_token()
    }
}
impl Valid<AttributeDeclarationSyntax> {
    pub fn attribute_token(&self) -> SyntaxToken {
        self.inner().attribute_token().expect("node must be valid")
    }
    pub fn identifier_token(&self) -> SyntaxToken {
        self.inner().identifier_token().expect("node must be valid")
    }
    pub fn colon_token(&self) -> SyntaxToken {
        self.inner().colon_token().expect("node must be valid")
    }
    pub fn type_mark(&self) -> Valid<NameSyntax> {
        Valid::new_unchecked(self.inner().type_mark().expect("node must be valid"))
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<AttributeNameSyntax> {
    pub fn signature(&self) -> Option<Valid<SignatureSyntax>> {
        self.inner().signature().map(Valid::new_unchecked)
    }
    pub fn tick_token(&self) -> SyntaxToken {
        self.inner().tick_token().expect("node must be valid")
    }
    pub fn attribute_designator(&self) -> AttributeDesignatorSyntax {
        self.inner()
            .attribute_designator()
            .expect("node must be valid")
    }
}
impl Valid<AttributeSpecificationSyntax> {
    pub fn attribute_token(&self) -> SyntaxToken {
        self.inner().attribute_token().expect("node must be valid")
    }
    pub fn identifier_token(&self) -> SyntaxToken {
        self.inner().identifier_token().expect("node must be valid")
    }
    pub fn of_token(&self) -> SyntaxToken {
        self.inner().of_token().expect("node must be valid")
    }
    pub fn entity_specification(&self) -> Valid<EntitySpecificationSyntax> {
        Valid::new_unchecked(
            self.inner()
                .entity_specification()
                .expect("node must be valid"),
        )
    }
    pub fn is_token(&self) -> SyntaxToken {
        self.inner().is_token().expect("node must be valid")
    }
    pub fn expression(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().expression().expect("node must be valid"))
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<BinaryExpressionSyntax> {
    pub fn lhs(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().lhs().expect("node must be valid"))
    }
    pub fn binary_operator(&self) -> BinaryOperatorSyntax {
        self.inner().binary_operator().expect("node must be valid")
    }
    pub fn rhs(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().rhs().expect("node must be valid"))
    }
}
impl Valid<BindingSyntax> {
    pub fn binding_indication(&self) -> Option<Valid<BindingIndicationSyntax>> {
        self.inner().binding_indication().map(Valid::new_unchecked)
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<BindingIndicationSyntax> {
    pub fn binding_use_clause(&self) -> Option<Valid<BindingUseClauseSyntax>> {
        self.inner().binding_use_clause().map(Valid::new_unchecked)
    }
    pub fn generic_map_aspect(&self) -> Option<Valid<GenericMapAspectSyntax>> {
        self.inner().generic_map_aspect().map(Valid::new_unchecked)
    }
    pub fn port_map_aspect(&self) -> Option<Valid<PortMapAspectSyntax>> {
        self.inner().port_map_aspect().map(Valid::new_unchecked)
    }
}
impl Valid<BindingUseClauseSyntax> {
    pub fn use_token(&self) -> SyntaxToken {
        self.inner().use_token().expect("node must be valid")
    }
    pub fn entity_aspect(&self) -> Valid<EntityAspectSyntax> {
        Valid::new_unchecked(self.inner().entity_aspect().expect("node must be valid"))
    }
}
impl Valid<BlockConfigurationSyntax> {
    pub fn block_configuration_preamble(&self) -> Valid<BlockConfigurationPreambleSyntax> {
        Valid::new_unchecked(
            self.inner()
                .block_configuration_preamble()
                .expect("node must be valid"),
        )
    }
    pub fn use_clauses(&self) -> impl Iterator<Item = Valid<UseClauseSyntax>> + use<'_> {
        self.inner().use_clauses().map(Valid::new_unchecked)
    }
    pub fn configuration_items(
        &self,
    ) -> impl Iterator<Item = Valid<ConfigurationItemSyntax>> + use<'_> {
        self.inner().configuration_items().map(Valid::new_unchecked)
    }
    pub fn block_configuration_epilogue(&self) -> Valid<BlockConfigurationEpilogueSyntax> {
        Valid::new_unchecked(
            self.inner()
                .block_configuration_epilogue()
                .expect("node must be valid"),
        )
    }
}
impl Valid<BlockConfigurationEpilogueSyntax> {
    pub fn end_token(&self) -> SyntaxToken {
        self.inner().end_token().expect("node must be valid")
    }
    pub fn for_token(&self) -> SyntaxToken {
        self.inner().for_token().expect("node must be valid")
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<BlockConfigurationItemSyntax> {
    pub fn block_configuration(&self) -> Valid<BlockConfigurationSyntax> {
        Valid::new_unchecked(
            self.inner()
                .block_configuration()
                .expect("node must be valid"),
        )
    }
}
impl Valid<BlockConfigurationPreambleSyntax> {
    pub fn for_token(&self) -> SyntaxToken {
        self.inner().for_token().expect("node must be valid")
    }
    pub fn block_specification(&self) -> Valid<NameSyntax> {
        Valid::new_unchecked(
            self.inner()
                .block_specification()
                .expect("node must be valid"),
        )
    }
}
#[derive(Debug, Clone)]
pub enum ValidBlockDeclarativeItem {
    SubprogramDeclaration(Valid<SubprogramDeclarationSyntax>),
    SubprogramBody(Valid<SubprogramBodySyntax>),
    SubprogramInstantiationDeclaration(Valid<SubprogramInstantiationDeclarationSyntax>),
    PackageDeclarationItem(Valid<PackageDeclarationItemSyntax>),
    PackageBodyDeclaration(Valid<PackageBodyDeclarationSyntax>),
    PackageInstantiationDeclarationItem(Valid<PackageInstantiationDeclarationItemSyntax>),
    TypeDeclaration(Valid<TypeDeclarationSyntax>),
    SubtypeDeclaration(Valid<SubtypeDeclarationSyntax>),
    ConstantDeclaration(Valid<ConstantDeclarationSyntax>),
    SignalDeclaration(Valid<SignalDeclarationSyntax>),
    VariableDeclaration(Valid<VariableDeclarationSyntax>),
    FileDeclaration(Valid<FileDeclarationSyntax>),
    AliasDeclaration(Valid<AliasDeclarationSyntax>),
    ComponentDeclaration(Valid<ComponentDeclarationSyntax>),
    AttributeDeclaration(Valid<AttributeDeclarationSyntax>),
    AttributeSpecification(Valid<AttributeSpecificationSyntax>),
    ConfigurationSpecification(Valid<ConfigurationSpecificationSyntax>),
    DisconnectionSpecification(Valid<DisconnectionSpecificationSyntax>),
    UseClauseDeclaration(Valid<UseClauseDeclarationSyntax>),
    GroupTemplateDeclaration(Valid<GroupTemplateDeclarationSyntax>),
    GroupDeclaration(Valid<GroupDeclarationSyntax>),
}
impl Valid<BlockDeclarativeItemSyntax> {
    pub fn alternative(&self) -> ValidBlockDeclarativeItem {
        match self.inner() {
            BlockDeclarativeItemSyntax::SubprogramDeclaration(inner) => {
                ValidBlockDeclarativeItem::SubprogramDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            BlockDeclarativeItemSyntax::SubprogramBody(inner) => {
                ValidBlockDeclarativeItem::SubprogramBody(Valid::new_unchecked(inner.clone()))
            }
            BlockDeclarativeItemSyntax::SubprogramInstantiationDeclaration(inner) => {
                ValidBlockDeclarativeItem::SubprogramInstantiationDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            BlockDeclarativeItemSyntax::PackageDeclarationItem(inner) => {
                ValidBlockDeclarativeItem::PackageDeclarationItem(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            BlockDeclarativeItemSyntax::PackageBodyDeclaration(inner) => {
                ValidBlockDeclarativeItem::PackageBodyDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            BlockDeclarativeItemSyntax::PackageInstantiationDeclarationItem(inner) => {
                ValidBlockDeclarativeItem::PackageInstantiationDeclarationItem(
                    Valid::new_unchecked(inner.clone()),
                )
            }
            BlockDeclarativeItemSyntax::TypeDeclaration(inner) => {
                ValidBlockDeclarativeItem::TypeDeclaration(Valid::new_unchecked(inner.clone()))
            }
            BlockDeclarativeItemSyntax::SubtypeDeclaration(inner) => {
                ValidBlockDeclarativeItem::SubtypeDeclaration(Valid::new_unchecked(inner.clone()))
            }
            BlockDeclarativeItemSyntax::ConstantDeclaration(inner) => {
                ValidBlockDeclarativeItem::ConstantDeclaration(Valid::new_unchecked(inner.clone()))
            }
            BlockDeclarativeItemSyntax::SignalDeclaration(inner) => {
                ValidBlockDeclarativeItem::SignalDeclaration(Valid::new_unchecked(inner.clone()))
            }
            BlockDeclarativeItemSyntax::VariableDeclaration(inner) => {
                ValidBlockDeclarativeItem::VariableDeclaration(Valid::new_unchecked(inner.clone()))
            }
            BlockDeclarativeItemSyntax::FileDeclaration(inner) => {
                ValidBlockDeclarativeItem::FileDeclaration(Valid::new_unchecked(inner.clone()))
            }
            BlockDeclarativeItemSyntax::AliasDeclaration(inner) => {
                ValidBlockDeclarativeItem::AliasDeclaration(Valid::new_unchecked(inner.clone()))
            }
            BlockDeclarativeItemSyntax::ComponentDeclaration(inner) => {
                ValidBlockDeclarativeItem::ComponentDeclaration(Valid::new_unchecked(inner.clone()))
            }
            BlockDeclarativeItemSyntax::AttributeDeclaration(inner) => {
                ValidBlockDeclarativeItem::AttributeDeclaration(Valid::new_unchecked(inner.clone()))
            }
            BlockDeclarativeItemSyntax::AttributeSpecification(inner) => {
                ValidBlockDeclarativeItem::AttributeSpecification(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            BlockDeclarativeItemSyntax::ConfigurationSpecification(inner) => {
                ValidBlockDeclarativeItem::ConfigurationSpecification(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            BlockDeclarativeItemSyntax::DisconnectionSpecification(inner) => {
                ValidBlockDeclarativeItem::DisconnectionSpecification(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            BlockDeclarativeItemSyntax::UseClauseDeclaration(inner) => {
                ValidBlockDeclarativeItem::UseClauseDeclaration(Valid::new_unchecked(inner.clone()))
            }
            BlockDeclarativeItemSyntax::GroupTemplateDeclaration(inner) => {
                ValidBlockDeclarativeItem::GroupTemplateDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            BlockDeclarativeItemSyntax::GroupDeclaration(inner) => {
                ValidBlockDeclarativeItem::GroupDeclaration(Valid::new_unchecked(inner.clone()))
            }
        }
    }
}
impl Valid<BlockDeclarativePartSyntax> {
    pub fn block_declarative_items(
        &self,
    ) -> impl Iterator<Item = Valid<BlockDeclarativeItemSyntax>> + use<'_> {
        self.inner()
            .block_declarative_items()
            .map(Valid::new_unchecked)
    }
}
impl Valid<BlockEpilogueSyntax> {
    pub fn end_token(&self) -> SyntaxToken {
        self.inner().end_token().expect("node must be valid")
    }
    pub fn block_token(&self) -> SyntaxToken {
        self.inner().block_token().expect("node must be valid")
    }
    pub fn label(&self) -> Option<SyntaxToken> {
        self.inner().label()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<BlockHeaderSyntax> {
    pub fn generic_part(&self) -> Option<Valid<GenericPartSyntax>> {
        self.inner().generic_part().map(Valid::new_unchecked)
    }
    pub fn port_part(&self) -> Option<Valid<PortPartSyntax>> {
        self.inner().port_part().map(Valid::new_unchecked)
    }
}
impl Valid<BlockPreambleSyntax> {
    pub fn block_token(&self) -> SyntaxToken {
        self.inner().block_token().expect("node must be valid")
    }
    pub fn parenthesized_condition(&self) -> Option<Valid<ParenthesizedConditionSyntax>> {
        self.inner()
            .parenthesized_condition()
            .map(Valid::new_unchecked)
    }
    pub fn is_token(&self) -> Option<SyntaxToken> {
        self.inner().is_token()
    }
}
impl Valid<BlockStatementSyntax> {
    pub fn stmt_label(&self) -> Valid<StmtLabelSyntax> {
        Valid::new_unchecked(self.inner().stmt_label().expect("node must be valid"))
    }
    pub fn block_preamble(&self) -> Valid<BlockPreambleSyntax> {
        Valid::new_unchecked(self.inner().block_preamble().expect("node must be valid"))
    }
    pub fn block_header(&self) -> Option<Valid<BlockHeaderSyntax>> {
        self.inner().block_header().map(Valid::new_unchecked)
    }
    pub fn block_declarative_part(&self) -> Option<Valid<BlockDeclarativePartSyntax>> {
        self.inner()
            .block_declarative_part()
            .map(Valid::new_unchecked)
    }
    pub fn declaration_statement_separator(&self) -> Valid<DeclarationStatementSeparatorSyntax> {
        Valid::new_unchecked(
            self.inner()
                .declaration_statement_separator()
                .expect("node must be valid"),
        )
    }
    pub fn block_statement_part(&self) -> Option<Valid<BlockStatementPartSyntax>> {
        self.inner()
            .block_statement_part()
            .map(Valid::new_unchecked)
    }
    pub fn block_epilogue(&self) -> Valid<BlockEpilogueSyntax> {
        Valid::new_unchecked(self.inner().block_epilogue().expect("node must be valid"))
    }
}
impl Valid<BlockStatementPartSyntax> {
    pub fn concurrent_statements(
        &self,
    ) -> impl Iterator<Item = Valid<ConcurrentStatementSyntax>> + use<'_> {
        self.inner()
            .concurrent_statements()
            .map(Valid::new_unchecked)
    }
}
impl Valid<CaseGenerateAlternativeSyntax> {
    pub fn when_token(&self) -> SyntaxToken {
        self.inner().when_token().expect("node must be valid")
    }
    pub fn stmt_label(&self) -> Option<Valid<StmtLabelSyntax>> {
        self.inner().stmt_label().map(Valid::new_unchecked)
    }
    pub fn choices(&self) -> Valid<ChoicesSyntax> {
        Valid::new_unchecked(self.inner().choices().expect("node must be valid"))
    }
    pub fn right_arrow_token(&self) -> SyntaxToken {
        self.inner()
            .right_arrow_token()
            .expect("node must be valid")
    }
    pub fn generate_statement_body(&self) -> Option<Valid<GenerateStatementBodySyntax>> {
        self.inner()
            .generate_statement_body()
            .map(Valid::new_unchecked)
    }
}
impl Valid<CaseGeneratePreambleSyntax> {
    pub fn case_token(&self) -> SyntaxToken {
        self.inner().case_token().expect("node must be valid")
    }
    pub fn expression(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().expression().expect("node must be valid"))
    }
    pub fn generate_token(&self) -> SyntaxToken {
        self.inner().generate_token().expect("node must be valid")
    }
}
impl Valid<CaseGenerateStatementSyntax> {
    pub fn stmt_label(&self) -> Valid<StmtLabelSyntax> {
        Valid::new_unchecked(self.inner().stmt_label().expect("node must be valid"))
    }
    pub fn case_generate_preamble(&self) -> Valid<CaseGeneratePreambleSyntax> {
        Valid::new_unchecked(
            self.inner()
                .case_generate_preamble()
                .expect("node must be valid"),
        )
    }
    pub fn case_generate_alternatives(
        &self,
    ) -> impl Iterator<Item = Valid<CaseGenerateAlternativeSyntax>> + use<'_> {
        self.inner()
            .case_generate_alternatives()
            .map(Valid::new_unchecked)
    }
    pub fn generate_epilogue(&self) -> Valid<GenerateEpilogueSyntax> {
        Valid::new_unchecked(
            self.inner()
                .generate_epilogue()
                .expect("node must be valid"),
        )
    }
}
impl Valid<CaseStatementSyntax> {
    pub fn case_statement_preamble(&self) -> Valid<CaseStatementPreambleSyntax> {
        Valid::new_unchecked(
            self.inner()
                .case_statement_preamble()
                .expect("node must be valid"),
        )
    }
    pub fn case_statement_alternatives(
        &self,
    ) -> impl Iterator<Item = Valid<CaseStatementAlternativeSyntax>> + use<'_> {
        self.inner()
            .case_statement_alternatives()
            .map(Valid::new_unchecked)
    }
    pub fn case_statement_epilogue(&self) -> Valid<CaseStatementEpilogueSyntax> {
        Valid::new_unchecked(
            self.inner()
                .case_statement_epilogue()
                .expect("node must be valid"),
        )
    }
}
impl Valid<CaseStatementAlternativeSyntax> {
    pub fn case_statement_alternative_preamble(
        &self,
    ) -> Valid<CaseStatementAlternativePreambleSyntax> {
        Valid::new_unchecked(
            self.inner()
                .case_statement_alternative_preamble()
                .expect("node must be valid"),
        )
    }
    pub fn sequence_of_statements(&self) -> Option<Valid<SequenceOfStatementsSyntax>> {
        self.inner()
            .sequence_of_statements()
            .map(Valid::new_unchecked)
    }
}
impl Valid<CaseStatementAlternativePreambleSyntax> {
    pub fn when_token(&self) -> SyntaxToken {
        self.inner().when_token().expect("node must be valid")
    }
    pub fn choices(&self) -> Valid<ChoicesSyntax> {
        Valid::new_unchecked(self.inner().choices().expect("node must be valid"))
    }
    pub fn right_arrow_token(&self) -> SyntaxToken {
        self.inner()
            .right_arrow_token()
            .expect("node must be valid")
    }
}
impl Valid<CaseStatementEpilogueSyntax> {
    pub fn end_token(&self) -> SyntaxToken {
        self.inner().end_token().expect("node must be valid")
    }
    pub fn case_token(&self) -> SyntaxToken {
        self.inner().case_token().expect("node must be valid")
    }
    pub fn que_token(&self) -> Option<SyntaxToken> {
        self.inner().que_token()
    }
    pub fn label(&self) -> Option<SyntaxToken> {
        self.inner().label()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<CaseStatementPreambleSyntax> {
    pub fn stmt_label(&self) -> Option<Valid<StmtLabelSyntax>> {
        self.inner().stmt_label().map(Valid::new_unchecked)
    }
    pub fn case_token(&self) -> SyntaxToken {
        self.inner().case_token().expect("node must be valid")
    }
    pub fn que_token(&self) -> Option<SyntaxToken> {
        self.inner().que_token()
    }
    pub fn expression(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().expression().expect("node must be valid"))
    }
    pub fn is_token(&self) -> SyntaxToken {
        self.inner().is_token().expect("node must be valid")
    }
}
#[derive(Debug, Clone)]
pub enum ValidChoice {
    ExpressionChoice(Valid<ExpressionChoiceSyntax>),
    OthersChoice(Valid<OthersChoiceSyntax>),
}
impl Valid<ChoiceSyntax> {
    pub fn alternative(&self) -> ValidChoice {
        match self.inner() {
            ChoiceSyntax::ExpressionChoice(inner) => {
                ValidChoice::ExpressionChoice(Valid::new_unchecked(inner.clone()))
            }
            ChoiceSyntax::OthersChoice(inner) => {
                ValidChoice::OthersChoice(Valid::new_unchecked(inner.clone()))
            }
        }
    }
}
impl Valid<ChoicesSyntax> {
    pub fn choices(&self) -> impl Iterator<Item = Valid<ChoiceSyntax>> + use<'_> {
        self.inner().choices().map(Valid::new_unchecked)
    }
    pub fn bar_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.inner().bar_token()
    }
}
impl Valid<ComponentConfigurationSyntax> {
    pub fn component_configuration_preamble(&self) -> Valid<ComponentConfigurationPreambleSyntax> {
        Valid::new_unchecked(
            self.inner()
                .component_configuration_preamble()
                .expect("node must be valid"),
        )
    }
    pub fn binding(&self) -> Option<Valid<BindingSyntax>> {
        self.inner().binding().map(Valid::new_unchecked)
    }
    pub fn verification_unit_bindings(
        &self,
    ) -> impl Iterator<Item = Valid<VerificationUnitBindingSyntax>> + use<'_> {
        self.inner()
            .verification_unit_bindings()
            .map(Valid::new_unchecked)
    }
    pub fn block_configuration(&self) -> Option<Valid<BlockConfigurationSyntax>> {
        self.inner().block_configuration().map(Valid::new_unchecked)
    }
    pub fn component_configuration_epilogue(&self) -> Valid<ComponentConfigurationEpilogueSyntax> {
        Valid::new_unchecked(
            self.inner()
                .component_configuration_epilogue()
                .expect("node must be valid"),
        )
    }
}
impl Valid<ComponentConfigurationEpilogueSyntax> {
    pub fn end_token(&self) -> SyntaxToken {
        self.inner().end_token().expect("node must be valid")
    }
    pub fn for_token(&self) -> SyntaxToken {
        self.inner().for_token().expect("node must be valid")
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<ComponentConfigurationPreambleSyntax> {
    pub fn for_token(&self) -> SyntaxToken {
        self.inner().for_token().expect("node must be valid")
    }
    pub fn component_specification(&self) -> Valid<ComponentSpecificationSyntax> {
        Valid::new_unchecked(
            self.inner()
                .component_specification()
                .expect("node must be valid"),
        )
    }
}
impl Valid<ComponentDeclarationSyntax> {
    pub fn component_declaration_preamble(&self) -> Valid<ComponentDeclarationPreambleSyntax> {
        Valid::new_unchecked(
            self.inner()
                .component_declaration_preamble()
                .expect("node must be valid"),
        )
    }
    pub fn generic_clause(&self) -> Option<Valid<GenericClauseSyntax>> {
        self.inner().generic_clause().map(Valid::new_unchecked)
    }
    pub fn port_clause(&self) -> Option<Valid<PortClauseSyntax>> {
        self.inner().port_clause().map(Valid::new_unchecked)
    }
    pub fn component_declaration_epilogue(&self) -> Valid<ComponentDeclarationEpilogueSyntax> {
        Valid::new_unchecked(
            self.inner()
                .component_declaration_epilogue()
                .expect("node must be valid"),
        )
    }
}
impl Valid<ComponentDeclarationEpilogueSyntax> {
    pub fn end_token(&self) -> SyntaxToken {
        self.inner().end_token().expect("node must be valid")
    }
    pub fn component_token(&self) -> SyntaxToken {
        self.inner().component_token().expect("node must be valid")
    }
    pub fn simple_name(&self) -> Option<SyntaxToken> {
        self.inner().simple_name()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<ComponentDeclarationPreambleSyntax> {
    pub fn component_token(&self) -> SyntaxToken {
        self.inner().component_token().expect("node must be valid")
    }
    pub fn identifier_token(&self) -> SyntaxToken {
        self.inner().identifier_token().expect("node must be valid")
    }
    pub fn is_token(&self) -> Option<SyntaxToken> {
        self.inner().is_token()
    }
}
impl Valid<ComponentInstantiationStatementSyntax> {
    pub fn stmt_label(&self) -> Valid<StmtLabelSyntax> {
        Valid::new_unchecked(self.inner().stmt_label().expect("node must be valid"))
    }
    pub fn instantiated_unit(&self) -> Valid<InstantiatedUnitSyntax> {
        Valid::new_unchecked(
            self.inner()
                .instantiated_unit()
                .expect("node must be valid"),
        )
    }
    pub fn generic_map_aspect(&self) -> Option<Valid<GenericMapAspectSyntax>> {
        self.inner().generic_map_aspect().map(Valid::new_unchecked)
    }
    pub fn port_map_aspect(&self) -> Option<Valid<PortMapAspectSyntax>> {
        self.inner().port_map_aspect().map(Valid::new_unchecked)
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<ComponentSpecificationSyntax> {
    pub fn instantiation_list(&self) -> Valid<InstantiationListSyntax> {
        Valid::new_unchecked(
            self.inner()
                .instantiation_list()
                .expect("node must be valid"),
        )
    }
    pub fn colon_token(&self) -> SyntaxToken {
        self.inner().colon_token().expect("node must be valid")
    }
    pub fn name(&self) -> Valid<NameSyntax> {
        Valid::new_unchecked(self.inner().name().expect("node must be valid"))
    }
}
#[derive(Debug, Clone)]
pub enum ValidCompositeTypeDefinition {
    ArrayTypeDefinition(Valid<ArrayTypeDefinitionSyntax>),
    RecordTypeDefinition(Valid<RecordTypeDefinitionSyntax>),
}
impl Valid<CompositeTypeDefinitionSyntax> {
    pub fn alternative(&self) -> ValidCompositeTypeDefinition {
        match self.inner() {
            CompositeTypeDefinitionSyntax::ArrayTypeDefinition(inner) => {
                ValidCompositeTypeDefinition::ArrayTypeDefinition(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            CompositeTypeDefinitionSyntax::RecordTypeDefinition(inner) => {
                ValidCompositeTypeDefinition::RecordTypeDefinition(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
        }
    }
}
impl Valid<CompoundConfigurationSpecificationSyntax> {
    pub fn component_configuration_preamble(&self) -> Valid<ComponentConfigurationPreambleSyntax> {
        Valid::new_unchecked(
            self.inner()
                .component_configuration_preamble()
                .expect("node must be valid"),
        )
    }
    pub fn binding_indication(&self) -> Option<Valid<BindingIndicationSyntax>> {
        self.inner().binding_indication().map(Valid::new_unchecked)
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
    pub fn verification_unit_bindings(
        &self,
    ) -> impl Iterator<Item = Valid<VerificationUnitBindingSyntax>> + use<'_> {
        self.inner()
            .verification_unit_bindings()
            .map(Valid::new_unchecked)
    }
    pub fn component_configuration_epilogue(&self) -> Valid<ComponentConfigurationEpilogueSyntax> {
        Valid::new_unchecked(
            self.inner()
                .component_configuration_epilogue()
                .expect("node must be valid"),
        )
    }
}
impl Valid<ConcurrentAssertionStatementSyntax> {
    pub fn stmt_label(&self) -> Option<Valid<StmtLabelSyntax>> {
        self.inner().stmt_label().map(Valid::new_unchecked)
    }
    pub fn postponed_token(&self) -> Option<SyntaxToken> {
        self.inner().postponed_token()
    }
    pub fn assertion(&self) -> Valid<AssertionSyntax> {
        Valid::new_unchecked(self.inner().assertion().expect("node must be valid"))
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<ConcurrentConditionalSignalAssignmentSyntax> {
    pub fn stmt_label(&self) -> Option<Valid<StmtLabelSyntax>> {
        self.inner().stmt_label().map(Valid::new_unchecked)
    }
    pub fn postponed_token(&self) -> Option<SyntaxToken> {
        self.inner().postponed_token()
    }
    pub fn target(&self) -> Valid<TargetSyntax> {
        Valid::new_unchecked(self.inner().target().expect("node must be valid"))
    }
    pub fn lte_token(&self) -> SyntaxToken {
        self.inner().lte_token().expect("node must be valid")
    }
    pub fn guarded_token(&self) -> Option<SyntaxToken> {
        self.inner().guarded_token()
    }
    pub fn delay_mechanism(&self) -> Option<Valid<DelayMechanismSyntax>> {
        self.inner().delay_mechanism().map(Valid::new_unchecked)
    }
    pub fn conditional_waveforms(&self) -> Valid<ConditionalWaveformsSyntax> {
        Valid::new_unchecked(
            self.inner()
                .conditional_waveforms()
                .expect("node must be valid"),
        )
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<ConcurrentProcedureCallOrComponentInstantiationStatementSyntax> {
    pub fn stmt_label(&self) -> Option<Valid<StmtLabelSyntax>> {
        self.inner().stmt_label().map(Valid::new_unchecked)
    }
    pub fn postponed_token(&self) -> Option<SyntaxToken> {
        self.inner().postponed_token()
    }
    pub fn name(&self) -> Valid<NameSyntax> {
        Valid::new_unchecked(self.inner().name().expect("node must be valid"))
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<ConcurrentSelectedSignalAssignmentSyntax> {
    pub fn stmt_label(&self) -> Option<Valid<StmtLabelSyntax>> {
        self.inner().stmt_label().map(Valid::new_unchecked)
    }
    pub fn postponed_token(&self) -> Option<SyntaxToken> {
        self.inner().postponed_token()
    }
    pub fn selected_assignment_preamble(&self) -> Valid<SelectedAssignmentPreambleSyntax> {
        Valid::new_unchecked(
            self.inner()
                .selected_assignment_preamble()
                .expect("node must be valid"),
        )
    }
    pub fn target(&self) -> Valid<TargetSyntax> {
        Valid::new_unchecked(self.inner().target().expect("node must be valid"))
    }
    pub fn lte_token(&self) -> SyntaxToken {
        self.inner().lte_token().expect("node must be valid")
    }
    pub fn guarded_token(&self) -> Option<SyntaxToken> {
        self.inner().guarded_token()
    }
    pub fn delay_mechanism(&self) -> Option<Valid<DelayMechanismSyntax>> {
        self.inner().delay_mechanism().map(Valid::new_unchecked)
    }
    pub fn selected_waveforms(&self) -> Valid<SelectedWaveformsSyntax> {
        Valid::new_unchecked(
            self.inner()
                .selected_waveforms()
                .expect("node must be valid"),
        )
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
#[derive(Debug, Clone)]
pub enum ValidConcurrentSignalAssignmentStatement {
    ConcurrentSimpleSignalAssignment(Valid<ConcurrentSimpleSignalAssignmentSyntax>),
    ConcurrentConditionalSignalAssignment(Valid<ConcurrentConditionalSignalAssignmentSyntax>),
    ConcurrentSelectedSignalAssignment(Valid<ConcurrentSelectedSignalAssignmentSyntax>),
}
impl Valid<ConcurrentSignalAssignmentStatementSyntax> {
    pub fn alternative(&self) -> ValidConcurrentSignalAssignmentStatement {
        match self.inner() {
            ConcurrentSignalAssignmentStatementSyntax::ConcurrentSimpleSignalAssignment(inner) => {
                ValidConcurrentSignalAssignmentStatement::ConcurrentSimpleSignalAssignment(
                    Valid::new_unchecked(inner.clone()),
                )
            }
            ConcurrentSignalAssignmentStatementSyntax::ConcurrentConditionalSignalAssignment(
                inner,
            ) => ValidConcurrentSignalAssignmentStatement::ConcurrentConditionalSignalAssignment(
                Valid::new_unchecked(inner.clone()),
            ),
            ConcurrentSignalAssignmentStatementSyntax::ConcurrentSelectedSignalAssignment(
                inner,
            ) => ValidConcurrentSignalAssignmentStatement::ConcurrentSelectedSignalAssignment(
                Valid::new_unchecked(inner.clone()),
            ),
        }
    }
}
impl Valid<ConcurrentSimpleSignalAssignmentSyntax> {
    pub fn stmt_label(&self) -> Option<Valid<StmtLabelSyntax>> {
        self.inner().stmt_label().map(Valid::new_unchecked)
    }
    pub fn postponed_token(&self) -> Option<SyntaxToken> {
        self.inner().postponed_token()
    }
    pub fn target(&self) -> Valid<TargetSyntax> {
        Valid::new_unchecked(self.inner().target().expect("node must be valid"))
    }
    pub fn lte_token(&self) -> SyntaxToken {
        self.inner().lte_token().expect("node must be valid")
    }
    pub fn guarded_token(&self) -> Option<SyntaxToken> {
        self.inner().guarded_token()
    }
    pub fn delay_mechanism(&self) -> Option<Valid<DelayMechanismSyntax>> {
        self.inner().delay_mechanism().map(Valid::new_unchecked)
    }
    pub fn waveform(&self) -> Valid<WaveformSyntax> {
        Valid::new_unchecked(self.inner().waveform().expect("node must be valid"))
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
#[derive(Debug, Clone)]
pub enum ValidConcurrentStatement {
    BlockStatement(Valid<BlockStatementSyntax>),
    ProcessStatement(Valid<ProcessStatementSyntax>),
    ConcurrentProcedureCallOrComponentInstantiationStatement(
        Valid<ConcurrentProcedureCallOrComponentInstantiationStatementSyntax>,
    ),
    ConcurrentAssertionStatement(Valid<ConcurrentAssertionStatementSyntax>),
    ConcurrentSignalAssignmentStatement(Valid<ConcurrentSignalAssignmentStatementSyntax>),
    ComponentInstantiationStatement(Valid<ComponentInstantiationStatementSyntax>),
    GenerateStatement(Valid<GenerateStatementSyntax>),
}
impl Valid<ConcurrentStatementSyntax> {
    pub fn alternative(&self) -> ValidConcurrentStatement {
        match self.inner() {
            ConcurrentStatementSyntax::BlockStatement(inner) => {
                ValidConcurrentStatement::BlockStatement(Valid::new_unchecked(inner.clone()))
            }
            ConcurrentStatementSyntax::ProcessStatement(inner) => {
                ValidConcurrentStatement::ProcessStatement(Valid::new_unchecked(inner.clone()))
            }
            ConcurrentStatementSyntax::ConcurrentProcedureCallOrComponentInstantiationStatement(
                inner,
            ) => {
                ValidConcurrentStatement::ConcurrentProcedureCallOrComponentInstantiationStatement(
                    Valid::new_unchecked(inner.clone()),
                )
            }
            ConcurrentStatementSyntax::ConcurrentAssertionStatement(inner) => {
                ValidConcurrentStatement::ConcurrentAssertionStatement(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            ConcurrentStatementSyntax::ConcurrentSignalAssignmentStatement(inner) => {
                ValidConcurrentStatement::ConcurrentSignalAssignmentStatement(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            ConcurrentStatementSyntax::ComponentInstantiationStatement(inner) => {
                ValidConcurrentStatement::ComponentInstantiationStatement(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            ConcurrentStatementSyntax::GenerateStatement(inner) => {
                ValidConcurrentStatement::GenerateStatement(Valid::new_unchecked(inner.clone()))
            }
        }
    }
}
impl Valid<ConditionClauseSyntax> {
    pub fn until_token(&self) -> SyntaxToken {
        self.inner().until_token().expect("node must be valid")
    }
    pub fn condition(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().condition().expect("node must be valid"))
    }
}
impl Valid<ConditionalExpressionsSyntax> {
    pub fn when_expression(&self) -> Valid<WhenExpressionSyntax> {
        Valid::new_unchecked(self.inner().when_expression().expect("node must be valid"))
    }
    pub fn else_when_expressions(
        &self,
    ) -> impl Iterator<Item = Valid<ElseWhenExpressionSyntax>> + use<'_> {
        self.inner()
            .else_when_expressions()
            .map(Valid::new_unchecked)
    }
    pub fn else_expression(&self) -> Option<Valid<ElseExpressionSyntax>> {
        self.inner().else_expression().map(Valid::new_unchecked)
    }
}
impl Valid<ConditionalForceAssignmentSyntax> {
    pub fn stmt_label(&self) -> Option<Valid<StmtLabelSyntax>> {
        self.inner().stmt_label().map(Valid::new_unchecked)
    }
    pub fn target(&self) -> Valid<TargetSyntax> {
        Valid::new_unchecked(self.inner().target().expect("node must be valid"))
    }
    pub fn lte_token(&self) -> SyntaxToken {
        self.inner().lte_token().expect("node must be valid")
    }
    pub fn force_token(&self) -> SyntaxToken {
        self.inner().force_token().expect("node must be valid")
    }
    pub fn force_mode(&self) -> Option<ForceModeSyntax> {
        self.inner().force_mode()
    }
    pub fn conditional_expressions(&self) -> Valid<ConditionalExpressionsSyntax> {
        Valid::new_unchecked(
            self.inner()
                .conditional_expressions()
                .expect("node must be valid"),
        )
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
#[derive(Debug, Clone)]
pub enum ValidConditionalSignalAssignment {
    ConditionalWaveformAssignment(Valid<ConditionalWaveformAssignmentSyntax>),
    ConditionalForceAssignment(Valid<ConditionalForceAssignmentSyntax>),
}
impl Valid<ConditionalSignalAssignmentSyntax> {
    pub fn alternative(&self) -> ValidConditionalSignalAssignment {
        match self.inner() {
            ConditionalSignalAssignmentSyntax::ConditionalWaveformAssignment(inner) => {
                ValidConditionalSignalAssignment::ConditionalWaveformAssignment(
                    Valid::new_unchecked(inner.clone()),
                )
            }
            ConditionalSignalAssignmentSyntax::ConditionalForceAssignment(inner) => {
                ValidConditionalSignalAssignment::ConditionalForceAssignment(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
        }
    }
}
impl Valid<ConditionalVariableAssignmentSyntax> {
    pub fn stmt_label(&self) -> Option<Valid<StmtLabelSyntax>> {
        self.inner().stmt_label().map(Valid::new_unchecked)
    }
    pub fn target(&self) -> Valid<TargetSyntax> {
        Valid::new_unchecked(self.inner().target().expect("node must be valid"))
    }
    pub fn colon_eq_token(&self) -> SyntaxToken {
        self.inner().colon_eq_token().expect("node must be valid")
    }
    pub fn conditional_expressions(&self) -> Valid<ConditionalExpressionsSyntax> {
        Valid::new_unchecked(
            self.inner()
                .conditional_expressions()
                .expect("node must be valid"),
        )
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<ConditionalWaveformAssignmentSyntax> {
    pub fn stmt_label(&self) -> Option<Valid<StmtLabelSyntax>> {
        self.inner().stmt_label().map(Valid::new_unchecked)
    }
    pub fn target(&self) -> Valid<TargetSyntax> {
        Valid::new_unchecked(self.inner().target().expect("node must be valid"))
    }
    pub fn lte_token(&self) -> SyntaxToken {
        self.inner().lte_token().expect("node must be valid")
    }
    pub fn delay_mechanism(&self) -> Option<Valid<DelayMechanismSyntax>> {
        self.inner().delay_mechanism().map(Valid::new_unchecked)
    }
    pub fn conditional_waveforms(&self) -> Valid<ConditionalWaveformsSyntax> {
        Valid::new_unchecked(
            self.inner()
                .conditional_waveforms()
                .expect("node must be valid"),
        )
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<ConditionalWaveformsSyntax> {
    pub fn when_waveform(&self) -> Valid<WhenWaveformSyntax> {
        Valid::new_unchecked(self.inner().when_waveform().expect("node must be valid"))
    }
    pub fn else_when_waveforms(
        &self,
    ) -> impl Iterator<Item = Valid<ElseWhenWaveformSyntax>> + use<'_> {
        self.inner().else_when_waveforms().map(Valid::new_unchecked)
    }
    pub fn else_waveform(&self) -> Option<Valid<ElseWaveformSyntax>> {
        self.inner().else_waveform().map(Valid::new_unchecked)
    }
}
impl Valid<ConfigurationDeclarationSyntax> {
    pub fn configuration_declaration_preamble(
        &self,
    ) -> Valid<ConfigurationDeclarationPreambleSyntax> {
        Valid::new_unchecked(
            self.inner()
                .configuration_declaration_preamble()
                .expect("node must be valid"),
        )
    }
    pub fn configuration_declarative_part(
        &self,
    ) -> Option<Valid<ConfigurationDeclarativePartSyntax>> {
        self.inner()
            .configuration_declarative_part()
            .map(Valid::new_unchecked)
    }
    pub fn verification_unit_bindings(
        &self,
    ) -> impl Iterator<Item = Valid<VerificationUnitBindingSyntax>> + use<'_> {
        self.inner()
            .verification_unit_bindings()
            .map(Valid::new_unchecked)
    }
    pub fn block_configuration(&self) -> Valid<BlockConfigurationSyntax> {
        Valid::new_unchecked(
            self.inner()
                .block_configuration()
                .expect("node must be valid"),
        )
    }
    pub fn configuration_declaration_epilogue(
        &self,
    ) -> Valid<ConfigurationDeclarationEpilogueSyntax> {
        Valid::new_unchecked(
            self.inner()
                .configuration_declaration_epilogue()
                .expect("node must be valid"),
        )
    }
}
impl Valid<ConfigurationDeclarationEpilogueSyntax> {
    pub fn end_token(&self) -> SyntaxToken {
        self.inner().end_token().expect("node must be valid")
    }
    pub fn configuration_token(&self) -> Option<SyntaxToken> {
        self.inner().configuration_token()
    }
    pub fn simple_name(&self) -> Option<SyntaxToken> {
        self.inner().simple_name()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<ConfigurationDeclarationPreambleSyntax> {
    pub fn configuration_token(&self) -> SyntaxToken {
        self.inner()
            .configuration_token()
            .expect("node must be valid")
    }
    pub fn identifier_token(&self) -> SyntaxToken {
        self.inner().identifier_token().expect("node must be valid")
    }
    pub fn of_token(&self) -> SyntaxToken {
        self.inner().of_token().expect("node must be valid")
    }
    pub fn name(&self) -> Valid<NameSyntax> {
        Valid::new_unchecked(self.inner().name().expect("node must be valid"))
    }
    pub fn is_token(&self) -> SyntaxToken {
        self.inner().is_token().expect("node must be valid")
    }
}
#[derive(Debug, Clone)]
pub enum ValidConfigurationDeclarativeItem {
    UseClauseDeclaration(Valid<UseClauseDeclarationSyntax>),
    AttributeSpecification(Valid<AttributeSpecificationSyntax>),
    GroupDeclaration(Valid<GroupDeclarationSyntax>),
}
impl Valid<ConfigurationDeclarativeItemSyntax> {
    pub fn alternative(&self) -> ValidConfigurationDeclarativeItem {
        match self.inner() {
            ConfigurationDeclarativeItemSyntax::UseClauseDeclaration(inner) => {
                ValidConfigurationDeclarativeItem::UseClauseDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            ConfigurationDeclarativeItemSyntax::AttributeSpecification(inner) => {
                ValidConfigurationDeclarativeItem::AttributeSpecification(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            ConfigurationDeclarativeItemSyntax::GroupDeclaration(inner) => {
                ValidConfigurationDeclarativeItem::GroupDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
        }
    }
}
impl Valid<ConfigurationDeclarativePartSyntax> {
    pub fn configuration_declarative_items(
        &self,
    ) -> impl Iterator<Item = Valid<ConfigurationDeclarativeItemSyntax>> + use<'_> {
        self.inner()
            .configuration_declarative_items()
            .map(Valid::new_unchecked)
    }
}
#[derive(Debug, Clone)]
pub enum ValidConfigurationItem {
    BlockConfigurationItem(Valid<BlockConfigurationItemSyntax>),
    ComponentConfiguration(Valid<ComponentConfigurationSyntax>),
}
impl Valid<ConfigurationItemSyntax> {
    pub fn alternative(&self) -> ValidConfigurationItem {
        match self.inner() {
            ConfigurationItemSyntax::BlockConfigurationItem(inner) => {
                ValidConfigurationItem::BlockConfigurationItem(Valid::new_unchecked(inner.clone()))
            }
            ConfigurationItemSyntax::ComponentConfiguration(inner) => {
                ValidConfigurationItem::ComponentConfiguration(Valid::new_unchecked(inner.clone()))
            }
        }
    }
}
#[derive(Debug, Clone)]
pub enum ValidConfigurationSpecification {
    SimpleConfigurationSpecification(Valid<SimpleConfigurationSpecificationSyntax>),
    CompoundConfigurationSpecification(Valid<CompoundConfigurationSpecificationSyntax>),
}
impl Valid<ConfigurationSpecificationSyntax> {
    pub fn alternative(&self) -> ValidConfigurationSpecification {
        match self.inner() {
            ConfigurationSpecificationSyntax::SimpleConfigurationSpecification(inner) => {
                ValidConfigurationSpecification::SimpleConfigurationSpecification(
                    Valid::new_unchecked(inner.clone()),
                )
            }
            ConfigurationSpecificationSyntax::CompoundConfigurationSpecification(inner) => {
                ValidConfigurationSpecification::CompoundConfigurationSpecification(
                    Valid::new_unchecked(inner.clone()),
                )
            }
        }
    }
}
impl Valid<ConstantDeclarationSyntax> {
    pub fn constant_token(&self) -> SyntaxToken {
        self.inner().constant_token().expect("node must be valid")
    }
    pub fn identifier_list(&self) -> Valid<IdentifierListSyntax> {
        Valid::new_unchecked(self.inner().identifier_list().expect("node must be valid"))
    }
    pub fn colon_token(&self) -> SyntaxToken {
        self.inner().colon_token().expect("node must be valid")
    }
    pub fn subtype_indication(&self) -> Valid<SubtypeIndicationSyntax> {
        Valid::new_unchecked(
            self.inner()
                .subtype_indication()
                .expect("node must be valid"),
        )
    }
    pub fn initial_value(&self) -> Option<Valid<InitialValueSyntax>> {
        self.inner().initial_value().map(Valid::new_unchecked)
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<ConstrainedArrayDefinitionSyntax> {
    pub fn array_token(&self) -> SyntaxToken {
        self.inner().array_token().expect("node must be valid")
    }
    pub fn index_constraint(&self) -> Valid<IndexConstraintSyntax> {
        Valid::new_unchecked(self.inner().index_constraint().expect("node must be valid"))
    }
    pub fn of_token(&self) -> SyntaxToken {
        self.inner().of_token().expect("node must be valid")
    }
    pub fn subtype_indication(&self) -> Valid<SubtypeIndicationSyntax> {
        Valid::new_unchecked(
            self.inner()
                .subtype_indication()
                .expect("node must be valid"),
        )
    }
}
impl Valid<ContextClauseSyntax> {
    pub fn context_items(&self) -> impl Iterator<Item = Valid<ContextItemSyntax>> + use<'_> {
        self.inner().context_items().map(Valid::new_unchecked)
    }
}
impl Valid<ContextDeclarationSyntax> {
    pub fn context_declaration_preamble(&self) -> Valid<ContextDeclarationPreambleSyntax> {
        Valid::new_unchecked(
            self.inner()
                .context_declaration_preamble()
                .expect("node must be valid"),
        )
    }
    pub fn context_clause(&self) -> Option<Valid<ContextClauseSyntax>> {
        self.inner().context_clause().map(Valid::new_unchecked)
    }
    pub fn context_declaration_epilogue(&self) -> Valid<ContextDeclarationEpilogueSyntax> {
        Valid::new_unchecked(
            self.inner()
                .context_declaration_epilogue()
                .expect("node must be valid"),
        )
    }
}
impl Valid<ContextDeclarationEpilogueSyntax> {
    pub fn end_token(&self) -> SyntaxToken {
        self.inner().end_token().expect("node must be valid")
    }
    pub fn context_token(&self) -> Option<SyntaxToken> {
        self.inner().context_token()
    }
    pub fn simple_name(&self) -> Option<SyntaxToken> {
        self.inner().simple_name()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<ContextDeclarationPreambleSyntax> {
    pub fn context_token(&self) -> SyntaxToken {
        self.inner().context_token().expect("node must be valid")
    }
    pub fn identifier_token(&self) -> SyntaxToken {
        self.inner().identifier_token().expect("node must be valid")
    }
    pub fn is_token(&self) -> SyntaxToken {
        self.inner().is_token().expect("node must be valid")
    }
}
#[derive(Debug, Clone)]
pub enum ValidContextItem {
    LibraryClause(Valid<LibraryClauseSyntax>),
    UseClauseContextItem(Valid<UseClauseContextItemSyntax>),
    ContextReference(Valid<ContextReferenceSyntax>),
}
impl Valid<ContextItemSyntax> {
    pub fn alternative(&self) -> ValidContextItem {
        match self.inner() {
            ContextItemSyntax::LibraryClause(inner) => {
                ValidContextItem::LibraryClause(Valid::new_unchecked(inner.clone()))
            }
            ContextItemSyntax::UseClauseContextItem(inner) => {
                ValidContextItem::UseClauseContextItem(Valid::new_unchecked(inner.clone()))
            }
            ContextItemSyntax::ContextReference(inner) => {
                ValidContextItem::ContextReference(Valid::new_unchecked(inner.clone()))
            }
        }
    }
}
impl Valid<ContextReferenceSyntax> {
    pub fn context_token(&self) -> SyntaxToken {
        self.inner().context_token().expect("node must be valid")
    }
    pub fn name_list(&self) -> Valid<NameListSyntax> {
        Valid::new_unchecked(self.inner().name_list().expect("node must be valid"))
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<DeclarationStatementSeparatorSyntax> {
    pub fn begin_token(&self) -> SyntaxToken {
        self.inner().begin_token().expect("node must be valid")
    }
}
#[derive(Debug, Clone)]
pub enum ValidDelayMechanism {
    TransportDelayMechanism(Valid<TransportDelayMechanismSyntax>),
    InertialDelayMechanism(Valid<InertialDelayMechanismSyntax>),
}
impl Valid<DelayMechanismSyntax> {
    pub fn alternative(&self) -> ValidDelayMechanism {
        match self.inner() {
            DelayMechanismSyntax::TransportDelayMechanism(inner) => {
                ValidDelayMechanism::TransportDelayMechanism(Valid::new_unchecked(inner.clone()))
            }
            DelayMechanismSyntax::InertialDelayMechanism(inner) => {
                ValidDelayMechanism::InertialDelayMechanism(Valid::new_unchecked(inner.clone()))
            }
        }
    }
}
impl Valid<DesignFileSyntax> {
    pub fn design_units(&self) -> impl Iterator<Item = Valid<DesignUnitSyntax>> + use<'_> {
        self.inner().design_units().map(Valid::new_unchecked)
    }
    pub fn eof_token(&self) -> SyntaxToken {
        self.inner().eof_token().expect("node must be valid")
    }
}
impl Valid<DesignUnitSyntax> {
    pub fn context_clause(&self) -> Option<Valid<ContextClauseSyntax>> {
        self.inner().context_clause().map(Valid::new_unchecked)
    }
    pub fn library_unit(&self) -> Valid<LibraryUnitSyntax> {
        Valid::new_unchecked(self.inner().library_unit().expect("node must be valid"))
    }
}
impl Valid<DisconnectionSpecificationSyntax> {
    pub fn disconnect_token(&self) -> SyntaxToken {
        self.inner().disconnect_token().expect("node must be valid")
    }
    pub fn guarded_signal_specification(&self) -> Valid<GuardedSignalSpecificationSyntax> {
        Valid::new_unchecked(
            self.inner()
                .guarded_signal_specification()
                .expect("node must be valid"),
        )
    }
    pub fn after_token(&self) -> SyntaxToken {
        self.inner().after_token().expect("node must be valid")
    }
    pub fn expression(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().expression().expect("node must be valid"))
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<ElementAssociationSyntax> {
    pub fn element_choices(&self) -> Option<Valid<ElementChoicesSyntax>> {
        self.inner().element_choices().map(Valid::new_unchecked)
    }
    pub fn expression(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().expression().expect("node must be valid"))
    }
}
impl Valid<ElementAssociationListSyntax> {
    pub fn element_associations(
        &self,
    ) -> impl Iterator<Item = Valid<ElementAssociationSyntax>> + use<'_> {
        self.inner()
            .element_associations()
            .map(Valid::new_unchecked)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.inner().comma_token()
    }
}
impl Valid<ElementChoicesSyntax> {
    pub fn choices(&self) -> Valid<ChoicesSyntax> {
        Valid::new_unchecked(self.inner().choices().expect("node must be valid"))
    }
    pub fn right_arrow_token(&self) -> SyntaxToken {
        self.inner()
            .right_arrow_token()
            .expect("node must be valid")
    }
}
impl Valid<ElementDeclarationSyntax> {
    pub fn identifier_list(&self) -> Valid<IdentifierListSyntax> {
        Valid::new_unchecked(self.inner().identifier_list().expect("node must be valid"))
    }
    pub fn colon_token(&self) -> SyntaxToken {
        self.inner().colon_token().expect("node must be valid")
    }
    pub fn element_subtype_definition(&self) -> Valid<SubtypeIndicationSyntax> {
        Valid::new_unchecked(
            self.inner()
                .element_subtype_definition()
                .expect("node must be valid"),
        )
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
#[derive(Debug, Clone)]
pub enum ValidElementResolution {
    ArrayElementResolution(Valid<ResolutionIndicationSyntax>),
    RecordResolutionElementResolution(Valid<RecordResolutionElementResolutionSyntax>),
}
impl Valid<ElementResolutionSyntax> {
    pub fn alternative(&self) -> ValidElementResolution {
        match self.inner() {
            ElementResolutionSyntax::ArrayElementResolution(inner) => {
                ValidElementResolution::ArrayElementResolution(Valid::new_unchecked(inner.clone()))
            }
            ElementResolutionSyntax::RecordResolutionElementResolution(inner) => {
                ValidElementResolution::RecordResolutionElementResolution(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
        }
    }
}
impl Valid<ElementResolutionResolutionIndicationSyntax> {
    pub fn element_resolution(&self) -> Valid<ElementResolutionSyntax> {
        Valid::new_unchecked(
            self.inner()
                .element_resolution()
                .expect("node must be valid"),
        )
    }
}
impl Valid<ElseExpressionSyntax> {
    pub fn else_token(&self) -> SyntaxToken {
        self.inner().else_token().expect("node must be valid")
    }
    pub fn expression(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().expression().expect("node must be valid"))
    }
}
impl Valid<ElseWaveformSyntax> {
    pub fn else_token(&self) -> SyntaxToken {
        self.inner().else_token().expect("node must be valid")
    }
    pub fn waveform(&self) -> Valid<WaveformSyntax> {
        Valid::new_unchecked(self.inner().waveform().expect("node must be valid"))
    }
}
impl Valid<ElseWhenExpressionSyntax> {
    pub fn else_token(&self) -> SyntaxToken {
        self.inner().else_token().expect("node must be valid")
    }
    pub fn expression(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().expression().expect("node must be valid"))
    }
    pub fn when_token(&self) -> SyntaxToken {
        self.inner().when_token().expect("node must be valid")
    }
    pub fn condition(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().condition().expect("node must be valid"))
    }
}
impl Valid<ElseWhenWaveformSyntax> {
    pub fn else_token(&self) -> SyntaxToken {
        self.inner().else_token().expect("node must be valid")
    }
    pub fn waveform(&self) -> Valid<WaveformSyntax> {
        Valid::new_unchecked(self.inner().waveform().expect("node must be valid"))
    }
    pub fn when_token(&self) -> SyntaxToken {
        self.inner().when_token().expect("node must be valid")
    }
    pub fn condition(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().condition().expect("node must be valid"))
    }
}
impl Valid<EndPackageBodySyntax> {
    pub fn package_token(&self) -> SyntaxToken {
        self.inner().package_token().expect("node must be valid")
    }
    pub fn body_token(&self) -> SyntaxToken {
        self.inner().body_token().expect("node must be valid")
    }
}
#[derive(Debug, Clone)]
pub enum ValidEntityAspect {
    EntityEntityAspect(Valid<EntityEntityAspectSyntax>),
    EntityConfigurationAspect(Valid<EntityConfigurationAspectSyntax>),
    EntityOpenAspect(Valid<EntityOpenAspectSyntax>),
}
impl Valid<EntityAspectSyntax> {
    pub fn alternative(&self) -> ValidEntityAspect {
        match self.inner() {
            EntityAspectSyntax::EntityEntityAspect(inner) => {
                ValidEntityAspect::EntityEntityAspect(Valid::new_unchecked(inner.clone()))
            }
            EntityAspectSyntax::EntityConfigurationAspect(inner) => {
                ValidEntityAspect::EntityConfigurationAspect(Valid::new_unchecked(inner.clone()))
            }
            EntityAspectSyntax::EntityOpenAspect(inner) => {
                ValidEntityAspect::EntityOpenAspect(Valid::new_unchecked(inner.clone()))
            }
        }
    }
}
impl Valid<EntityClassEntrySyntax> {
    pub fn entity_class(&self) -> EntityClassSyntax {
        self.inner().entity_class().expect("node must be valid")
    }
    pub fn box_token(&self) -> Option<SyntaxToken> {
        self.inner().box_token()
    }
}
impl Valid<EntityClassEntryListSyntax> {
    pub fn entity_class_entrys(
        &self,
    ) -> impl Iterator<Item = Valid<EntityClassEntrySyntax>> + use<'_> {
        self.inner().entity_class_entrys().map(Valid::new_unchecked)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.inner().comma_token()
    }
}
impl Valid<EntityConfigurationAspectSyntax> {
    pub fn configuration_token(&self) -> SyntaxToken {
        self.inner()
            .configuration_token()
            .expect("node must be valid")
    }
    pub fn name(&self) -> Valid<NameSyntax> {
        Valid::new_unchecked(self.inner().name().expect("node must be valid"))
    }
}
impl Valid<EntityDeclarationSyntax> {
    pub fn entity_declaration_preamble(&self) -> Valid<EntityDeclarationPreambleSyntax> {
        Valid::new_unchecked(
            self.inner()
                .entity_declaration_preamble()
                .expect("node must be valid"),
        )
    }
    pub fn entity_header(&self) -> Option<Valid<EntityHeaderSyntax>> {
        self.inner().entity_header().map(Valid::new_unchecked)
    }
    pub fn entity_declarative_part(&self) -> Option<Valid<EntityDeclarativePartSyntax>> {
        self.inner()
            .entity_declarative_part()
            .map(Valid::new_unchecked)
    }
    pub fn entity_statements(&self) -> Option<Valid<EntityStatementsSyntax>> {
        self.inner().entity_statements().map(Valid::new_unchecked)
    }
    pub fn entity_declaration_epilogue(&self) -> Valid<EntityDeclarationEpilogueSyntax> {
        Valid::new_unchecked(
            self.inner()
                .entity_declaration_epilogue()
                .expect("node must be valid"),
        )
    }
}
impl Valid<EntityDeclarationEpilogueSyntax> {
    pub fn end_token(&self) -> SyntaxToken {
        self.inner().end_token().expect("node must be valid")
    }
    pub fn entity_token(&self) -> Option<SyntaxToken> {
        self.inner().entity_token()
    }
    pub fn simple_name(&self) -> Option<SyntaxToken> {
        self.inner().simple_name()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<EntityDeclarationPreambleSyntax> {
    pub fn entity_token(&self) -> SyntaxToken {
        self.inner().entity_token().expect("node must be valid")
    }
    pub fn identifier_token(&self) -> SyntaxToken {
        self.inner().identifier_token().expect("node must be valid")
    }
    pub fn is_token(&self) -> SyntaxToken {
        self.inner().is_token().expect("node must be valid")
    }
}
#[derive(Debug, Clone)]
pub enum ValidEntityDeclarativeItem {
    SubprogramDeclaration(Valid<SubprogramDeclarationSyntax>),
    SubprogramBody(Valid<SubprogramBodySyntax>),
    SubprogramInstantiationDeclaration(Valid<SubprogramInstantiationDeclarationSyntax>),
    PackageDeclarationItem(Valid<PackageDeclarationItemSyntax>),
    PackageBodyDeclaration(Valid<PackageBodyDeclarationSyntax>),
    PackageInstantiationDeclarationItem(Valid<PackageInstantiationDeclarationItemSyntax>),
    TypeDeclaration(Valid<TypeDeclarationSyntax>),
    SubtypeDeclaration(Valid<SubtypeDeclarationSyntax>),
    ConstantDeclaration(Valid<ConstantDeclarationSyntax>),
    SignalDeclaration(Valid<SignalDeclarationSyntax>),
    VariableDeclaration(Valid<VariableDeclarationSyntax>),
    FileDeclaration(Valid<FileDeclarationSyntax>),
    AliasDeclaration(Valid<AliasDeclarationSyntax>),
    AttributeDeclaration(Valid<AttributeDeclarationSyntax>),
    AttributeSpecification(Valid<AttributeSpecificationSyntax>),
    DisconnectionSpecification(Valid<DisconnectionSpecificationSyntax>),
    UseClauseDeclaration(Valid<UseClauseDeclarationSyntax>),
    GroupTemplateDeclaration(Valid<GroupTemplateDeclarationSyntax>),
    GroupDeclaration(Valid<GroupDeclarationSyntax>),
}
impl Valid<EntityDeclarativeItemSyntax> {
    pub fn alternative(&self) -> ValidEntityDeclarativeItem {
        match self.inner() {
            EntityDeclarativeItemSyntax::SubprogramDeclaration(inner) => {
                ValidEntityDeclarativeItem::SubprogramDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            EntityDeclarativeItemSyntax::SubprogramBody(inner) => {
                ValidEntityDeclarativeItem::SubprogramBody(Valid::new_unchecked(inner.clone()))
            }
            EntityDeclarativeItemSyntax::SubprogramInstantiationDeclaration(inner) => {
                ValidEntityDeclarativeItem::SubprogramInstantiationDeclaration(
                    Valid::new_unchecked(inner.clone()),
                )
            }
            EntityDeclarativeItemSyntax::PackageDeclarationItem(inner) => {
                ValidEntityDeclarativeItem::PackageDeclarationItem(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            EntityDeclarativeItemSyntax::PackageBodyDeclaration(inner) => {
                ValidEntityDeclarativeItem::PackageBodyDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            EntityDeclarativeItemSyntax::PackageInstantiationDeclarationItem(inner) => {
                ValidEntityDeclarativeItem::PackageInstantiationDeclarationItem(
                    Valid::new_unchecked(inner.clone()),
                )
            }
            EntityDeclarativeItemSyntax::TypeDeclaration(inner) => {
                ValidEntityDeclarativeItem::TypeDeclaration(Valid::new_unchecked(inner.clone()))
            }
            EntityDeclarativeItemSyntax::SubtypeDeclaration(inner) => {
                ValidEntityDeclarativeItem::SubtypeDeclaration(Valid::new_unchecked(inner.clone()))
            }
            EntityDeclarativeItemSyntax::ConstantDeclaration(inner) => {
                ValidEntityDeclarativeItem::ConstantDeclaration(Valid::new_unchecked(inner.clone()))
            }
            EntityDeclarativeItemSyntax::SignalDeclaration(inner) => {
                ValidEntityDeclarativeItem::SignalDeclaration(Valid::new_unchecked(inner.clone()))
            }
            EntityDeclarativeItemSyntax::VariableDeclaration(inner) => {
                ValidEntityDeclarativeItem::VariableDeclaration(Valid::new_unchecked(inner.clone()))
            }
            EntityDeclarativeItemSyntax::FileDeclaration(inner) => {
                ValidEntityDeclarativeItem::FileDeclaration(Valid::new_unchecked(inner.clone()))
            }
            EntityDeclarativeItemSyntax::AliasDeclaration(inner) => {
                ValidEntityDeclarativeItem::AliasDeclaration(Valid::new_unchecked(inner.clone()))
            }
            EntityDeclarativeItemSyntax::AttributeDeclaration(inner) => {
                ValidEntityDeclarativeItem::AttributeDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            EntityDeclarativeItemSyntax::AttributeSpecification(inner) => {
                ValidEntityDeclarativeItem::AttributeSpecification(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            EntityDeclarativeItemSyntax::DisconnectionSpecification(inner) => {
                ValidEntityDeclarativeItem::DisconnectionSpecification(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            EntityDeclarativeItemSyntax::UseClauseDeclaration(inner) => {
                ValidEntityDeclarativeItem::UseClauseDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            EntityDeclarativeItemSyntax::GroupTemplateDeclaration(inner) => {
                ValidEntityDeclarativeItem::GroupTemplateDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            EntityDeclarativeItemSyntax::GroupDeclaration(inner) => {
                ValidEntityDeclarativeItem::GroupDeclaration(Valid::new_unchecked(inner.clone()))
            }
        }
    }
}
impl Valid<EntityDeclarativePartSyntax> {
    pub fn entity_declarative_items(
        &self,
    ) -> impl Iterator<Item = Valid<EntityDeclarativeItemSyntax>> + use<'_> {
        self.inner()
            .entity_declarative_items()
            .map(Valid::new_unchecked)
    }
}
impl Valid<EntityDesignatorSyntax> {
    pub fn entity_tag(&self) -> EntityTagSyntax {
        self.inner().entity_tag().expect("node must be valid")
    }
    pub fn signature(&self) -> Option<Valid<SignatureSyntax>> {
        self.inner().signature().map(Valid::new_unchecked)
    }
}
impl Valid<EntityDesignatorListSyntax> {
    pub fn entity_designators(
        &self,
    ) -> impl Iterator<Item = Valid<EntityDesignatorSyntax>> + use<'_> {
        self.inner().entity_designators().map(Valid::new_unchecked)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.inner().comma_token()
    }
}
impl Valid<EntityEntityAspectSyntax> {
    pub fn entity_token(&self) -> SyntaxToken {
        self.inner().entity_token().expect("node must be valid")
    }
    pub fn name(&self) -> Valid<NameSyntax> {
        Valid::new_unchecked(self.inner().name().expect("node must be valid"))
    }
}
impl Valid<EntityHeaderSyntax> {
    pub fn generic_clause(&self) -> Option<Valid<GenericClauseSyntax>> {
        self.inner().generic_clause().map(Valid::new_unchecked)
    }
    pub fn port_clause(&self) -> Option<Valid<PortClauseSyntax>> {
        self.inner().port_clause().map(Valid::new_unchecked)
    }
}
#[derive(Debug, Clone)]
pub enum ValidEntityNameList {
    EntityDesignatorList(Valid<EntityDesignatorListSyntax>),
    EntityNameListOthers(Valid<EntityNameListOthersSyntax>),
    EntityNameListAll(Valid<EntityNameListAllSyntax>),
}
impl Valid<EntityNameListSyntax> {
    pub fn alternative(&self) -> ValidEntityNameList {
        match self.inner() {
            EntityNameListSyntax::EntityDesignatorList(inner) => {
                ValidEntityNameList::EntityDesignatorList(Valid::new_unchecked(inner.clone()))
            }
            EntityNameListSyntax::EntityNameListOthers(inner) => {
                ValidEntityNameList::EntityNameListOthers(Valid::new_unchecked(inner.clone()))
            }
            EntityNameListSyntax::EntityNameListAll(inner) => {
                ValidEntityNameList::EntityNameListAll(Valid::new_unchecked(inner.clone()))
            }
        }
    }
}
impl Valid<EntityNameListAllSyntax> {
    pub fn all_token(&self) -> SyntaxToken {
        self.inner().all_token().expect("node must be valid")
    }
}
impl Valid<EntityNameListOthersSyntax> {
    pub fn others_token(&self) -> SyntaxToken {
        self.inner().others_token().expect("node must be valid")
    }
}
impl Valid<EntityOpenAspectSyntax> {
    pub fn open_token(&self) -> SyntaxToken {
        self.inner().open_token().expect("node must be valid")
    }
}
impl Valid<EntitySpecificationSyntax> {
    pub fn entity_name_list(&self) -> Valid<EntityNameListSyntax> {
        Valid::new_unchecked(self.inner().entity_name_list().expect("node must be valid"))
    }
    pub fn colon_token(&self) -> SyntaxToken {
        self.inner().colon_token().expect("node must be valid")
    }
    pub fn entity_class(&self) -> EntityClassSyntax {
        self.inner().entity_class().expect("node must be valid")
    }
}
#[derive(Debug, Clone)]
pub enum ValidEntityStatement {
    ConcurrentAssertionStatement(Valid<ConcurrentAssertionStatementSyntax>),
    ConcurrentProcedureCallOrComponentInstantiationStatement(
        Valid<ConcurrentProcedureCallOrComponentInstantiationStatementSyntax>,
    ),
    ProcessStatement(Valid<ProcessStatementSyntax>),
}
impl Valid<EntityStatementSyntax> {
    pub fn alternative(&self) -> ValidEntityStatement {
        match self.inner() {
            EntityStatementSyntax::ConcurrentAssertionStatement(inner) => {
                ValidEntityStatement::ConcurrentAssertionStatement(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            EntityStatementSyntax::ConcurrentProcedureCallOrComponentInstantiationStatement(
                inner,
            ) => ValidEntityStatement::ConcurrentProcedureCallOrComponentInstantiationStatement(
                Valid::new_unchecked(inner.clone()),
            ),
            EntityStatementSyntax::ProcessStatement(inner) => {
                ValidEntityStatement::ProcessStatement(Valid::new_unchecked(inner.clone()))
            }
        }
    }
}
impl Valid<EntityStatementPartSyntax> {
    pub fn entity_statements(
        &self,
    ) -> impl Iterator<Item = Valid<EntityStatementSyntax>> + use<'_> {
        self.inner().entity_statements().map(Valid::new_unchecked)
    }
}
impl Valid<EntityStatementsSyntax> {
    pub fn declaration_statement_separator(&self) -> Valid<DeclarationStatementSeparatorSyntax> {
        Valid::new_unchecked(
            self.inner()
                .declaration_statement_separator()
                .expect("node must be valid"),
        )
    }
    pub fn entity_statement_part(&self) -> Option<Valid<EntityStatementPartSyntax>> {
        self.inner()
            .entity_statement_part()
            .map(Valid::new_unchecked)
    }
}
impl Valid<EnumerationListSyntax> {
    pub fn enumeration_literals(&self) -> impl Iterator<Item = EnumerationLiteralSyntax> + use<'_> {
        self.inner().enumeration_literals()
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.inner().comma_token()
    }
}
impl Valid<EnumerationTypeDefinitionSyntax> {
    pub fn left_par_token(&self) -> SyntaxToken {
        self.inner().left_par_token().expect("node must be valid")
    }
    pub fn enumeration_list(&self) -> Valid<EnumerationListSyntax> {
        Valid::new_unchecked(self.inner().enumeration_list().expect("node must be valid"))
    }
    pub fn right_par_token(&self) -> SyntaxToken {
        self.inner().right_par_token().expect("node must be valid")
    }
}
impl Valid<ExitStatementSyntax> {
    pub fn stmt_label(&self) -> Option<Valid<StmtLabelSyntax>> {
        self.inner().stmt_label().map(Valid::new_unchecked)
    }
    pub fn exit_token(&self) -> SyntaxToken {
        self.inner().exit_token().expect("node must be valid")
    }
    pub fn label(&self) -> Option<SyntaxToken> {
        self.inner().label()
    }
    pub fn when_clause(&self) -> Option<Valid<WhenClauseSyntax>> {
        self.inner().when_clause().map(Valid::new_unchecked)
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
#[derive(Debug, Clone)]
pub enum ValidExpression {
    LiteralExpression(Valid<LiteralExpressionSyntax>),
    PhysicalLiteralExpression(Valid<PhysicalLiteralExpressionSyntax>),
    UnaryExpression(Valid<UnaryExpressionSyntax>),
    BinaryExpression(Valid<BinaryExpressionSyntax>),
    ParenthesizedExpressionOrAggregate(Valid<ParenthesizedExpressionOrAggregateSyntax>),
    Allocator(Valid<AllocatorSyntax>),
    NameExpression(Valid<NameExpressionSyntax>),
    QualifiedExpression(Valid<QualifiedExpressionSyntax>),
}
impl Valid<ExpressionSyntax> {
    pub fn alternative(&self) -> ValidExpression {
        match self.inner() {
            ExpressionSyntax::LiteralExpression(inner) => {
                ValidExpression::LiteralExpression(Valid::new_unchecked(inner.clone()))
            }
            ExpressionSyntax::PhysicalLiteralExpression(inner) => {
                ValidExpression::PhysicalLiteralExpression(Valid::new_unchecked(inner.clone()))
            }
            ExpressionSyntax::UnaryExpression(inner) => {
                ValidExpression::UnaryExpression(Valid::new_unchecked(inner.clone()))
            }
            ExpressionSyntax::BinaryExpression(inner) => {
                ValidExpression::BinaryExpression(Valid::new_unchecked(inner.clone()))
            }
            ExpressionSyntax::ParenthesizedExpressionOrAggregate(inner) => {
                ValidExpression::ParenthesizedExpressionOrAggregate(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            ExpressionSyntax::Allocator(inner) => {
                ValidExpression::Allocator(Valid::new_unchecked(inner.clone()))
            }
            ExpressionSyntax::NameExpression(inner) => {
                ValidExpression::NameExpression(Valid::new_unchecked(inner.clone()))
            }
            ExpressionSyntax::QualifiedExpression(inner) => {
                ValidExpression::QualifiedExpression(Valid::new_unchecked(inner.clone()))
            }
        }
    }
}
impl Valid<ExpressionChoiceSyntax> {
    pub fn expression(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().expression().expect("node must be valid"))
    }
}
impl Valid<ExpressionListSyntax> {
    pub fn expressions(&self) -> impl Iterator<Item = Valid<ExpressionSyntax>> + use<'_> {
        self.inner().expressions().map(Valid::new_unchecked)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.inner().comma_token()
    }
}
impl Valid<ExternalConstantNameSyntax> {
    pub fn lt_lt_token(&self) -> SyntaxToken {
        self.inner().lt_lt_token().expect("node must be valid")
    }
    pub fn constant_token(&self) -> SyntaxToken {
        self.inner().constant_token().expect("node must be valid")
    }
    pub fn external_pathname(&self) -> Valid<ExternalPathnameSyntax> {
        Valid::new_unchecked(
            self.inner()
                .external_pathname()
                .expect("node must be valid"),
        )
    }
    pub fn colon_token(&self) -> SyntaxToken {
        self.inner().colon_token().expect("node must be valid")
    }
    pub fn subtype_indication(&self) -> Valid<SubtypeIndicationSyntax> {
        Valid::new_unchecked(
            self.inner()
                .subtype_indication()
                .expect("node must be valid"),
        )
    }
    pub fn gt_gt_token(&self) -> SyntaxToken {
        self.inner().gt_gt_token().expect("node must be valid")
    }
}
#[derive(Debug, Clone)]
pub enum ValidExternalName {
    ExternalConstantName(Valid<ExternalConstantNameSyntax>),
    ExternalSignalName(Valid<ExternalSignalNameSyntax>),
    ExternalVariableName(Valid<ExternalVariableNameSyntax>),
}
impl Valid<ExternalNameSyntax> {
    pub fn alternative(&self) -> ValidExternalName {
        match self.inner() {
            ExternalNameSyntax::ExternalConstantName(inner) => {
                ValidExternalName::ExternalConstantName(Valid::new_unchecked(inner.clone()))
            }
            ExternalNameSyntax::ExternalSignalName(inner) => {
                ValidExternalName::ExternalSignalName(Valid::new_unchecked(inner.clone()))
            }
            ExternalNameSyntax::ExternalVariableName(inner) => {
                ValidExternalName::ExternalVariableName(Valid::new_unchecked(inner.clone()))
            }
        }
    }
}
#[derive(Debug, Clone)]
pub enum ValidExternalPathname {
    PackagePathname(Valid<PackagePathnameSyntax>),
    AbsolutePathname(Valid<AbsolutePathnameSyntax>),
    RelativePathname(Valid<RelativePathnameSyntax>),
}
impl Valid<ExternalPathnameSyntax> {
    pub fn alternative(&self) -> ValidExternalPathname {
        match self.inner() {
            ExternalPathnameSyntax::PackagePathname(inner) => {
                ValidExternalPathname::PackagePathname(Valid::new_unchecked(inner.clone()))
            }
            ExternalPathnameSyntax::AbsolutePathname(inner) => {
                ValidExternalPathname::AbsolutePathname(Valid::new_unchecked(inner.clone()))
            }
            ExternalPathnameSyntax::RelativePathname(inner) => {
                ValidExternalPathname::RelativePathname(Valid::new_unchecked(inner.clone()))
            }
        }
    }
}
impl Valid<ExternalSignalNameSyntax> {
    pub fn lt_lt_token(&self) -> SyntaxToken {
        self.inner().lt_lt_token().expect("node must be valid")
    }
    pub fn signal_token(&self) -> SyntaxToken {
        self.inner().signal_token().expect("node must be valid")
    }
    pub fn external_pathname(&self) -> Valid<ExternalPathnameSyntax> {
        Valid::new_unchecked(
            self.inner()
                .external_pathname()
                .expect("node must be valid"),
        )
    }
    pub fn colon_token(&self) -> SyntaxToken {
        self.inner().colon_token().expect("node must be valid")
    }
    pub fn subtype_indication(&self) -> Valid<SubtypeIndicationSyntax> {
        Valid::new_unchecked(
            self.inner()
                .subtype_indication()
                .expect("node must be valid"),
        )
    }
    pub fn gt_gt_token(&self) -> SyntaxToken {
        self.inner().gt_gt_token().expect("node must be valid")
    }
}
impl Valid<ExternalVariableNameSyntax> {
    pub fn lt_lt_token(&self) -> SyntaxToken {
        self.inner().lt_lt_token().expect("node must be valid")
    }
    pub fn variable_token(&self) -> SyntaxToken {
        self.inner().variable_token().expect("node must be valid")
    }
    pub fn external_pathname(&self) -> Valid<ExternalPathnameSyntax> {
        Valid::new_unchecked(
            self.inner()
                .external_pathname()
                .expect("node must be valid"),
        )
    }
    pub fn colon_token(&self) -> SyntaxToken {
        self.inner().colon_token().expect("node must be valid")
    }
    pub fn subtype_indication(&self) -> Valid<SubtypeIndicationSyntax> {
        Valid::new_unchecked(
            self.inner()
                .subtype_indication()
                .expect("node must be valid"),
        )
    }
    pub fn gt_gt_token(&self) -> SyntaxToken {
        self.inner().gt_gt_token().expect("node must be valid")
    }
}
impl Valid<FileDeclarationSyntax> {
    pub fn file_token(&self) -> SyntaxToken {
        self.inner().file_token().expect("node must be valid")
    }
    pub fn identifier_list(&self) -> Valid<IdentifierListSyntax> {
        Valid::new_unchecked(self.inner().identifier_list().expect("node must be valid"))
    }
    pub fn colon_token(&self) -> SyntaxToken {
        self.inner().colon_token().expect("node must be valid")
    }
    pub fn subtype_indication(&self) -> Valid<SubtypeIndicationSyntax> {
        Valid::new_unchecked(
            self.inner()
                .subtype_indication()
                .expect("node must be valid"),
        )
    }
    pub fn file_open_information(&self) -> Option<Valid<FileOpenInformationSyntax>> {
        self.inner()
            .file_open_information()
            .map(Valid::new_unchecked)
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<FileOpenInformationSyntax> {
    pub fn file_open_kind(&self) -> Option<Valid<FileOpenKindSyntax>> {
        self.inner().file_open_kind().map(Valid::new_unchecked)
    }
    pub fn is_token(&self) -> SyntaxToken {
        self.inner().is_token().expect("node must be valid")
    }
    pub fn file_logical_name(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(
            self.inner()
                .file_logical_name()
                .expect("node must be valid"),
        )
    }
}
impl Valid<FileOpenKindSyntax> {
    pub fn open_token(&self) -> SyntaxToken {
        self.inner().open_token().expect("node must be valid")
    }
    pub fn expression(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().expression().expect("node must be valid"))
    }
}
impl Valid<FileTypeDefinitionSyntax> {
    pub fn file_token(&self) -> SyntaxToken {
        self.inner().file_token().expect("node must be valid")
    }
    pub fn of_token(&self) -> SyntaxToken {
        self.inner().of_token().expect("node must be valid")
    }
    pub fn type_mark(&self) -> Valid<NameSyntax> {
        Valid::new_unchecked(self.inner().type_mark().expect("node must be valid"))
    }
}
impl Valid<ForGeneratePreambleSyntax> {
    pub fn for_token(&self) -> SyntaxToken {
        self.inner().for_token().expect("node must be valid")
    }
    pub fn parameter_specification(&self) -> Valid<ParameterSpecificationSyntax> {
        Valid::new_unchecked(
            self.inner()
                .parameter_specification()
                .expect("node must be valid"),
        )
    }
    pub fn generate_token(&self) -> SyntaxToken {
        self.inner().generate_token().expect("node must be valid")
    }
}
impl Valid<ForGenerateStatementSyntax> {
    pub fn stmt_label(&self) -> Valid<StmtLabelSyntax> {
        Valid::new_unchecked(self.inner().stmt_label().expect("node must be valid"))
    }
    pub fn for_generate_preamble(&self) -> Valid<ForGeneratePreambleSyntax> {
        Valid::new_unchecked(
            self.inner()
                .for_generate_preamble()
                .expect("node must be valid"),
        )
    }
    pub fn generate_statement_body(&self) -> Option<Valid<GenerateStatementBodySyntax>> {
        self.inner()
            .generate_statement_body()
            .map(Valid::new_unchecked)
    }
    pub fn generate_epilogue(&self) -> Valid<GenerateEpilogueSyntax> {
        Valid::new_unchecked(
            self.inner()
                .generate_epilogue()
                .expect("node must be valid"),
        )
    }
}
impl Valid<ForSchemeSyntax> {
    pub fn for_token(&self) -> SyntaxToken {
        self.inner().for_token().expect("node must be valid")
    }
    pub fn parameter_specification(&self) -> Valid<ParameterSpecificationSyntax> {
        Valid::new_unchecked(
            self.inner()
                .parameter_specification()
                .expect("node must be valid"),
        )
    }
}
impl Valid<FormalSyntax> {
    pub fn formal_part(&self) -> Valid<NameSyntax> {
        Valid::new_unchecked(self.inner().formal_part().expect("node must be valid"))
    }
    pub fn right_arrow_token(&self) -> SyntaxToken {
        self.inner()
            .right_arrow_token()
            .expect("node must be valid")
    }
}
impl Valid<FullTypeDeclarationSyntax> {
    pub fn type_token(&self) -> SyntaxToken {
        self.inner().type_token().expect("node must be valid")
    }
    pub fn identifier_token(&self) -> SyntaxToken {
        self.inner().identifier_token().expect("node must be valid")
    }
    pub fn is_token(&self) -> SyntaxToken {
        self.inner().is_token().expect("node must be valid")
    }
    pub fn type_definition(&self) -> Valid<TypeDefinitionSyntax> {
        Valid::new_unchecked(self.inner().type_definition().expect("node must be valid"))
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<FunctionSpecificationSyntax> {
    pub fn purity(&self) -> Option<PuritySyntax> {
        self.inner().purity()
    }
    pub fn function_token(&self) -> SyntaxToken {
        self.inner().function_token().expect("node must be valid")
    }
    pub fn designator(&self) -> DesignatorSyntax {
        self.inner().designator().expect("node must be valid")
    }
    pub fn subprogram_header(&self) -> Option<Valid<SubprogramHeaderSyntax>> {
        self.inner().subprogram_header().map(Valid::new_unchecked)
    }
    pub fn parameter_list(&self) -> Option<Valid<ParameterListSyntax>> {
        self.inner().parameter_list().map(Valid::new_unchecked)
    }
    pub fn return_token(&self) -> SyntaxToken {
        self.inner().return_token().expect("node must be valid")
    }
    pub fn type_mark(&self) -> Valid<NameSyntax> {
        Valid::new_unchecked(self.inner().type_mark().expect("node must be valid"))
    }
}
impl Valid<GenerateBodyDeclarationsSyntax> {
    pub fn block_declarative_part(&self) -> Option<Valid<BlockDeclarativePartSyntax>> {
        self.inner()
            .block_declarative_part()
            .map(Valid::new_unchecked)
    }
    pub fn declaration_statement_separator(&self) -> Valid<DeclarationStatementSeparatorSyntax> {
        Valid::new_unchecked(
            self.inner()
                .declaration_statement_separator()
                .expect("node must be valid"),
        )
    }
}
impl Valid<GenerateBodyEpilogueSyntax> {
    pub fn end_token(&self) -> SyntaxToken {
        self.inner().end_token().expect("node must be valid")
    }
    pub fn label(&self) -> Option<SyntaxToken> {
        self.inner().label()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<GenerateEpilogueSyntax> {
    pub fn end_token(&self) -> SyntaxToken {
        self.inner().end_token().expect("node must be valid")
    }
    pub fn generate_token(&self) -> SyntaxToken {
        self.inner().generate_token().expect("node must be valid")
    }
    pub fn label(&self) -> Option<SyntaxToken> {
        self.inner().label()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
#[derive(Debug, Clone)]
pub enum ValidGenerateStatement {
    ForGenerateStatement(Valid<ForGenerateStatementSyntax>),
    IfGenerateStatement(Valid<IfGenerateStatementSyntax>),
    CaseGenerateStatement(Valid<CaseGenerateStatementSyntax>),
}
impl Valid<GenerateStatementSyntax> {
    pub fn alternative(&self) -> ValidGenerateStatement {
        match self.inner() {
            GenerateStatementSyntax::ForGenerateStatement(inner) => {
                ValidGenerateStatement::ForGenerateStatement(Valid::new_unchecked(inner.clone()))
            }
            GenerateStatementSyntax::IfGenerateStatement(inner) => {
                ValidGenerateStatement::IfGenerateStatement(Valid::new_unchecked(inner.clone()))
            }
            GenerateStatementSyntax::CaseGenerateStatement(inner) => {
                ValidGenerateStatement::CaseGenerateStatement(Valid::new_unchecked(inner.clone()))
            }
        }
    }
}
impl Valid<GenerateStatementBodySyntax> {
    pub fn generate_body_declarations(&self) -> Option<Valid<GenerateBodyDeclarationsSyntax>> {
        self.inner()
            .generate_body_declarations()
            .map(Valid::new_unchecked)
    }
    pub fn concurrent_statements(
        &self,
    ) -> impl Iterator<Item = Valid<ConcurrentStatementSyntax>> + use<'_> {
        self.inner()
            .concurrent_statements()
            .map(Valid::new_unchecked)
    }
    pub fn generate_body_epilogue(&self) -> Option<Valid<GenerateBodyEpilogueSyntax>> {
        self.inner()
            .generate_body_epilogue()
            .map(Valid::new_unchecked)
    }
}
impl Valid<GenericClauseSyntax> {
    pub fn generic_token(&self) -> SyntaxToken {
        self.inner().generic_token().expect("node must be valid")
    }
    pub fn left_par_token(&self) -> SyntaxToken {
        self.inner().left_par_token().expect("node must be valid")
    }
    pub fn generic_list(&self) -> Valid<InterfaceListSyntax> {
        Valid::new_unchecked(self.inner().generic_list().expect("node must be valid"))
    }
    pub fn right_par_token(&self) -> SyntaxToken {
        self.inner().right_par_token().expect("node must be valid")
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<GenericMapSyntax> {
    pub fn generic_map_aspect(&self) -> Valid<GenericMapAspectSyntax> {
        Valid::new_unchecked(
            self.inner()
                .generic_map_aspect()
                .expect("node must be valid"),
        )
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<GenericMapAspectSyntax> {
    pub fn generic_token(&self) -> SyntaxToken {
        self.inner().generic_token().expect("node must be valid")
    }
    pub fn map_token(&self) -> SyntaxToken {
        self.inner().map_token().expect("node must be valid")
    }
    pub fn left_par_token(&self) -> SyntaxToken {
        self.inner().left_par_token().expect("node must be valid")
    }
    pub fn association_list(&self) -> Valid<AssociationListSyntax> {
        Valid::new_unchecked(self.inner().association_list().expect("node must be valid"))
    }
    pub fn right_par_token(&self) -> SyntaxToken {
        self.inner().right_par_token().expect("node must be valid")
    }
}
impl Valid<GenericPartSyntax> {
    pub fn generic_clause(&self) -> Valid<GenericClauseSyntax> {
        Valid::new_unchecked(self.inner().generic_clause().expect("node must be valid"))
    }
    pub fn generic_map(&self) -> Option<Valid<GenericMapSyntax>> {
        self.inner().generic_map().map(Valid::new_unchecked)
    }
}
impl Valid<GroupDeclarationSyntax> {
    pub fn group_token(&self) -> SyntaxToken {
        self.inner().group_token().expect("node must be valid")
    }
    pub fn identifier_token(&self) -> SyntaxToken {
        self.inner().identifier_token().expect("node must be valid")
    }
    pub fn colon_token(&self) -> SyntaxToken {
        self.inner().colon_token().expect("node must be valid")
    }
    pub fn name(&self) -> Valid<NameSyntax> {
        Valid::new_unchecked(self.inner().name().expect("node must be valid"))
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<GroupTemplateDeclarationSyntax> {
    pub fn group_token(&self) -> SyntaxToken {
        self.inner().group_token().expect("node must be valid")
    }
    pub fn identifier_token(&self) -> SyntaxToken {
        self.inner().identifier_token().expect("node must be valid")
    }
    pub fn is_token(&self) -> SyntaxToken {
        self.inner().is_token().expect("node must be valid")
    }
    pub fn left_par_token(&self) -> SyntaxToken {
        self.inner().left_par_token().expect("node must be valid")
    }
    pub fn entity_class_entry_list(&self) -> Valid<EntityClassEntryListSyntax> {
        Valid::new_unchecked(
            self.inner()
                .entity_class_entry_list()
                .expect("node must be valid"),
        )
    }
    pub fn right_par_token(&self) -> SyntaxToken {
        self.inner().right_par_token().expect("node must be valid")
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<GuardedSignalSpecificationSyntax> {
    pub fn signal_list(&self) -> Valid<SignalListSyntax> {
        Valid::new_unchecked(self.inner().signal_list().expect("node must be valid"))
    }
    pub fn colon_token(&self) -> SyntaxToken {
        self.inner().colon_token().expect("node must be valid")
    }
    pub fn type_mark(&self) -> Valid<NameSyntax> {
        Valid::new_unchecked(self.inner().type_mark().expect("node must be valid"))
    }
}
impl Valid<IdentifierListSyntax> {
    pub fn identifier_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.inner().identifier_token()
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.inner().comma_token()
    }
}
impl Valid<IfGenerateElseSyntax> {
    pub fn else_token(&self) -> SyntaxToken {
        self.inner().else_token().expect("node must be valid")
    }
    pub fn stmt_label(&self) -> Option<Valid<StmtLabelSyntax>> {
        self.inner().stmt_label().map(Valid::new_unchecked)
    }
    pub fn generate_token(&self) -> SyntaxToken {
        self.inner().generate_token().expect("node must be valid")
    }
    pub fn generate_statement_body(&self) -> Option<Valid<GenerateStatementBodySyntax>> {
        self.inner()
            .generate_statement_body()
            .map(Valid::new_unchecked)
    }
}
impl Valid<IfGenerateElsifSyntax> {
    pub fn elsif_token(&self) -> SyntaxToken {
        self.inner().elsif_token().expect("node must be valid")
    }
    pub fn stmt_label(&self) -> Option<Valid<StmtLabelSyntax>> {
        self.inner().stmt_label().map(Valid::new_unchecked)
    }
    pub fn condition(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().condition().expect("node must be valid"))
    }
    pub fn generate_token(&self) -> SyntaxToken {
        self.inner().generate_token().expect("node must be valid")
    }
    pub fn generate_statement_body(&self) -> Option<Valid<GenerateStatementBodySyntax>> {
        self.inner()
            .generate_statement_body()
            .map(Valid::new_unchecked)
    }
}
impl Valid<IfGenerateIfSyntax> {
    pub fn if_token(&self) -> SyntaxToken {
        self.inner().if_token().expect("node must be valid")
    }
    pub fn stmt_label(&self) -> Option<Valid<StmtLabelSyntax>> {
        self.inner().stmt_label().map(Valid::new_unchecked)
    }
    pub fn condition(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().condition().expect("node must be valid"))
    }
    pub fn generate_token(&self) -> SyntaxToken {
        self.inner().generate_token().expect("node must be valid")
    }
    pub fn generate_statement_body(&self) -> Option<Valid<GenerateStatementBodySyntax>> {
        self.inner()
            .generate_statement_body()
            .map(Valid::new_unchecked)
    }
}
impl Valid<IfGenerateStatementSyntax> {
    pub fn stmt_label(&self) -> Valid<StmtLabelSyntax> {
        Valid::new_unchecked(self.inner().stmt_label().expect("node must be valid"))
    }
    pub fn if_generate_if(&self) -> Valid<IfGenerateIfSyntax> {
        Valid::new_unchecked(self.inner().if_generate_if().expect("node must be valid"))
    }
    pub fn if_generate_elsifs(
        &self,
    ) -> impl Iterator<Item = Valid<IfGenerateElsifSyntax>> + use<'_> {
        self.inner().if_generate_elsifs().map(Valid::new_unchecked)
    }
    pub fn if_generate_else(&self) -> Option<Valid<IfGenerateElseSyntax>> {
        self.inner().if_generate_else().map(Valid::new_unchecked)
    }
    pub fn generate_epilogue(&self) -> Valid<GenerateEpilogueSyntax> {
        Valid::new_unchecked(
            self.inner()
                .generate_epilogue()
                .expect("node must be valid"),
        )
    }
}
impl Valid<IfStatementSyntax> {
    pub fn if_statement_preamble(&self) -> Valid<IfStatementPreambleSyntax> {
        Valid::new_unchecked(
            self.inner()
                .if_statement_preamble()
                .expect("node must be valid"),
        )
    }
    pub fn sequence_of_statements(&self) -> Option<Valid<SequenceOfStatementsSyntax>> {
        self.inner()
            .sequence_of_statements()
            .map(Valid::new_unchecked)
    }
    pub fn if_statement_elsifs(
        &self,
    ) -> impl Iterator<Item = Valid<IfStatementElsifSyntax>> + use<'_> {
        self.inner().if_statement_elsifs().map(Valid::new_unchecked)
    }
    pub fn if_statement_else(&self) -> Option<Valid<IfStatementElseSyntax>> {
        self.inner().if_statement_else().map(Valid::new_unchecked)
    }
    pub fn if_statement_epilogue(&self) -> Valid<IfStatementEpilogueSyntax> {
        Valid::new_unchecked(
            self.inner()
                .if_statement_epilogue()
                .expect("node must be valid"),
        )
    }
}
impl Valid<IfStatementElseSyntax> {
    pub fn else_token(&self) -> SyntaxToken {
        self.inner().else_token().expect("node must be valid")
    }
    pub fn sequence_of_statements(&self) -> Option<Valid<SequenceOfStatementsSyntax>> {
        self.inner()
            .sequence_of_statements()
            .map(Valid::new_unchecked)
    }
}
impl Valid<IfStatementElsifSyntax> {
    pub fn elsif_token(&self) -> SyntaxToken {
        self.inner().elsif_token().expect("node must be valid")
    }
    pub fn condition(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().condition().expect("node must be valid"))
    }
    pub fn then_token(&self) -> SyntaxToken {
        self.inner().then_token().expect("node must be valid")
    }
    pub fn sequence_of_statements(&self) -> Option<Valid<SequenceOfStatementsSyntax>> {
        self.inner()
            .sequence_of_statements()
            .map(Valid::new_unchecked)
    }
}
impl Valid<IfStatementEpilogueSyntax> {
    pub fn end_token(&self) -> SyntaxToken {
        self.inner().end_token().expect("node must be valid")
    }
    pub fn if_token(&self) -> SyntaxToken {
        self.inner().if_token().expect("node must be valid")
    }
    pub fn label(&self) -> Option<SyntaxToken> {
        self.inner().label()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<IfStatementPreambleSyntax> {
    pub fn stmt_label(&self) -> Option<Valid<StmtLabelSyntax>> {
        self.inner().stmt_label().map(Valid::new_unchecked)
    }
    pub fn if_token(&self) -> SyntaxToken {
        self.inner().if_token().expect("node must be valid")
    }
    pub fn condition(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().condition().expect("node must be valid"))
    }
    pub fn then_token(&self) -> SyntaxToken {
        self.inner().then_token().expect("node must be valid")
    }
}
impl Valid<IncompleteTypeDeclarationSyntax> {
    pub fn type_token(&self) -> SyntaxToken {
        self.inner().type_token().expect("node must be valid")
    }
    pub fn identifier_token(&self) -> SyntaxToken {
        self.inner().identifier_token().expect("node must be valid")
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<IndexConstraintSyntax> {
    pub fn left_par_token(&self) -> SyntaxToken {
        self.inner().left_par_token().expect("node must be valid")
    }
    pub fn expression_list(&self) -> Valid<ExpressionListSyntax> {
        Valid::new_unchecked(self.inner().expression_list().expect("node must be valid"))
    }
    pub fn right_par_token(&self) -> SyntaxToken {
        self.inner().right_par_token().expect("node must be valid")
    }
}
impl Valid<IndexSubtypeDefinitionSyntax> {
    pub fn type_mark(&self) -> Valid<NameSyntax> {
        Valid::new_unchecked(self.inner().type_mark().expect("node must be valid"))
    }
    pub fn range_token(&self) -> SyntaxToken {
        self.inner().range_token().expect("node must be valid")
    }
    pub fn box_token(&self) -> SyntaxToken {
        self.inner().box_token().expect("node must be valid")
    }
}
impl Valid<IndexSubtypeDefinitionListSyntax> {
    pub fn index_subtype_definitions(
        &self,
    ) -> impl Iterator<Item = Valid<IndexSubtypeDefinitionSyntax>> + use<'_> {
        self.inner()
            .index_subtype_definitions()
            .map(Valid::new_unchecked)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.inner().comma_token()
    }
}
impl Valid<InertialDelayMechanismSyntax> {
    pub fn reject_clause(&self) -> Option<Valid<RejectClauseSyntax>> {
        self.inner().reject_clause().map(Valid::new_unchecked)
    }
    pub fn inertial_token(&self) -> SyntaxToken {
        self.inner().inertial_token().expect("node must be valid")
    }
}
impl Valid<InitialValueSyntax> {
    pub fn colon_eq_token(&self) -> SyntaxToken {
        self.inner().colon_eq_token().expect("node must be valid")
    }
    pub fn expression(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().expression().expect("node must be valid"))
    }
}
impl Valid<InstantiatedComponentSyntax> {
    pub fn component_token(&self) -> Option<SyntaxToken> {
        self.inner().component_token()
    }
    pub fn name(&self) -> Valid<NameSyntax> {
        Valid::new_unchecked(self.inner().name().expect("node must be valid"))
    }
}
impl Valid<InstantiatedConfigurationSyntax> {
    pub fn configuration_token(&self) -> SyntaxToken {
        self.inner()
            .configuration_token()
            .expect("node must be valid")
    }
    pub fn name(&self) -> Valid<NameSyntax> {
        Valid::new_unchecked(self.inner().name().expect("node must be valid"))
    }
}
impl Valid<InstantiatedEntitySyntax> {
    pub fn entity_token(&self) -> SyntaxToken {
        self.inner().entity_token().expect("node must be valid")
    }
    pub fn name(&self) -> Valid<NameSyntax> {
        Valid::new_unchecked(self.inner().name().expect("node must be valid"))
    }
}
#[derive(Debug, Clone)]
pub enum ValidInstantiatedUnit {
    InstantiatedComponent(Valid<InstantiatedComponentSyntax>),
    InstantiatedEntity(Valid<InstantiatedEntitySyntax>),
    InstantiatedConfiguration(Valid<InstantiatedConfigurationSyntax>),
}
impl Valid<InstantiatedUnitSyntax> {
    pub fn alternative(&self) -> ValidInstantiatedUnit {
        match self.inner() {
            InstantiatedUnitSyntax::InstantiatedComponent(inner) => {
                ValidInstantiatedUnit::InstantiatedComponent(Valid::new_unchecked(inner.clone()))
            }
            InstantiatedUnitSyntax::InstantiatedEntity(inner) => {
                ValidInstantiatedUnit::InstantiatedEntity(Valid::new_unchecked(inner.clone()))
            }
            InstantiatedUnitSyntax::InstantiatedConfiguration(inner) => {
                ValidInstantiatedUnit::InstantiatedConfiguration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
        }
    }
}
#[derive(Debug, Clone)]
pub enum ValidInstantiationList {
    InstantiationListList(Valid<InstantiationListListSyntax>),
    InstantiationListOthers(Valid<InstantiationListOthersSyntax>),
    InstantiationListAll(Valid<InstantiationListAllSyntax>),
}
impl Valid<InstantiationListSyntax> {
    pub fn alternative(&self) -> ValidInstantiationList {
        match self.inner() {
            InstantiationListSyntax::InstantiationListList(inner) => {
                ValidInstantiationList::InstantiationListList(Valid::new_unchecked(inner.clone()))
            }
            InstantiationListSyntax::InstantiationListOthers(inner) => {
                ValidInstantiationList::InstantiationListOthers(Valid::new_unchecked(inner.clone()))
            }
            InstantiationListSyntax::InstantiationListAll(inner) => {
                ValidInstantiationList::InstantiationListAll(Valid::new_unchecked(inner.clone()))
            }
        }
    }
}
impl Valid<InstantiationListAllSyntax> {
    pub fn all_token(&self) -> SyntaxToken {
        self.inner().all_token().expect("node must be valid")
    }
}
impl Valid<InstantiationListListSyntax> {
    pub fn labels(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.inner().labels()
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.inner().comma_token()
    }
}
impl Valid<InstantiationListOthersSyntax> {
    pub fn others_token(&self) -> SyntaxToken {
        self.inner().others_token().expect("node must be valid")
    }
}
#[derive(Debug, Clone)]
pub enum ValidInterfaceDeclaration {
    InterfaceObjectDeclaration(Valid<InterfaceObjectDeclarationSyntax>),
    InterfaceFileDeclaration(Valid<InterfaceFileDeclarationSyntax>),
    InterfaceTypeDeclaration(Valid<InterfaceIncompleteTypeDeclarationSyntax>),
    InterfaceSubprogramDeclaration(Valid<InterfaceSubprogramDeclarationSyntax>),
    InterfacePackageDeclaration(Valid<InterfacePackageDeclarationSyntax>),
}
impl Valid<InterfaceDeclarationSyntax> {
    pub fn alternative(&self) -> ValidInterfaceDeclaration {
        match self.inner() {
            InterfaceDeclarationSyntax::InterfaceObjectDeclaration(inner) => {
                ValidInterfaceDeclaration::InterfaceObjectDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            InterfaceDeclarationSyntax::InterfaceFileDeclaration(inner) => {
                ValidInterfaceDeclaration::InterfaceFileDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            InterfaceDeclarationSyntax::InterfaceTypeDeclaration(inner) => {
                ValidInterfaceDeclaration::InterfaceTypeDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            InterfaceDeclarationSyntax::InterfaceSubprogramDeclaration(inner) => {
                ValidInterfaceDeclaration::InterfaceSubprogramDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            InterfaceDeclarationSyntax::InterfacePackageDeclaration(inner) => {
                ValidInterfaceDeclaration::InterfacePackageDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
        }
    }
}
impl Valid<InterfaceFileDeclarationSyntax> {
    pub fn file_token(&self) -> SyntaxToken {
        self.inner().file_token().expect("node must be valid")
    }
    pub fn identifier_list(&self) -> Valid<IdentifierListSyntax> {
        Valid::new_unchecked(self.inner().identifier_list().expect("node must be valid"))
    }
    pub fn colon_token(&self) -> SyntaxToken {
        self.inner().colon_token().expect("node must be valid")
    }
    pub fn subtype_indication(&self) -> Valid<SubtypeIndicationSyntax> {
        Valid::new_unchecked(
            self.inner()
                .subtype_indication()
                .expect("node must be valid"),
        )
    }
}
impl Valid<InterfaceFunctionSpecificationSyntax> {
    pub fn purity(&self) -> Option<PuritySyntax> {
        self.inner().purity()
    }
    pub fn function_token(&self) -> SyntaxToken {
        self.inner().function_token().expect("node must be valid")
    }
    pub fn designator(&self) -> DesignatorSyntax {
        self.inner().designator().expect("node must be valid")
    }
    pub fn parameter_list(&self) -> Option<Valid<ParameterListSyntax>> {
        self.inner().parameter_list().map(Valid::new_unchecked)
    }
    pub fn return_token(&self) -> SyntaxToken {
        self.inner().return_token().expect("node must be valid")
    }
    pub fn type_mark(&self) -> Valid<NameSyntax> {
        Valid::new_unchecked(self.inner().type_mark().expect("node must be valid"))
    }
}
impl Valid<InterfaceIncompleteTypeDeclarationSyntax> {
    pub fn type_token(&self) -> SyntaxToken {
        self.inner().type_token().expect("node must be valid")
    }
    pub fn identifier_token(&self) -> SyntaxToken {
        self.inner().identifier_token().expect("node must be valid")
    }
}
impl Valid<InterfaceListSyntax> {
    pub fn interface_elements(
        &self,
    ) -> impl Iterator<Item = Valid<InterfaceDeclarationSyntax>> + use<'_> {
        self.inner().interface_elements().map(Valid::new_unchecked)
    }
    pub fn semi_colon_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.inner().semi_colon_token()
    }
}
impl Valid<InterfaceObjectDeclarationSyntax> {
    pub fn interface_object_class(&self) -> Option<InterfaceObjectClassSyntax> {
        self.inner().interface_object_class()
    }
    pub fn identifier_list(&self) -> Valid<IdentifierListSyntax> {
        Valid::new_unchecked(self.inner().identifier_list().expect("node must be valid"))
    }
    pub fn colon_token(&self) -> SyntaxToken {
        self.inner().colon_token().expect("node must be valid")
    }
    pub fn mode(&self) -> Option<ModeSyntax> {
        self.inner().mode()
    }
    pub fn subtype_indication(&self) -> Valid<SubtypeIndicationSyntax> {
        Valid::new_unchecked(
            self.inner()
                .subtype_indication()
                .expect("node must be valid"),
        )
    }
    pub fn bus_token(&self) -> Option<SyntaxToken> {
        self.inner().bus_token()
    }
    pub fn initial_value(&self) -> Option<Valid<InitialValueSyntax>> {
        self.inner().initial_value().map(Valid::new_unchecked)
    }
}
impl Valid<InterfacePackageDeclarationSyntax> {
    pub fn interface_package_declaration_preamble(
        &self,
    ) -> Valid<InterfacePackageDeclarationPreambleSyntax> {
        Valid::new_unchecked(
            self.inner()
                .interface_package_declaration_preamble()
                .expect("node must be valid"),
        )
    }
    pub fn new_token(&self) -> SyntaxToken {
        self.inner().new_token().expect("node must be valid")
    }
    pub fn name(&self) -> Valid<NameSyntax> {
        Valid::new_unchecked(self.inner().name().expect("node must be valid"))
    }
    pub fn interface_package_generic_map_aspect(
        &self,
    ) -> Valid<InterfacePackageGenericMapAspectSyntax> {
        Valid::new_unchecked(
            self.inner()
                .interface_package_generic_map_aspect()
                .expect("node must be valid"),
        )
    }
}
impl Valid<InterfacePackageDeclarationPreambleSyntax> {
    pub fn package_token(&self) -> SyntaxToken {
        self.inner().package_token().expect("node must be valid")
    }
    pub fn identifier_token(&self) -> SyntaxToken {
        self.inner().identifier_token().expect("node must be valid")
    }
    pub fn is_token(&self) -> SyntaxToken {
        self.inner().is_token().expect("node must be valid")
    }
}
impl Valid<InterfacePackageGenericMapAspectSyntax> {
    pub fn generic_token(&self) -> SyntaxToken {
        self.inner().generic_token().expect("node must be valid")
    }
    pub fn map_token(&self) -> SyntaxToken {
        self.inner().map_token().expect("node must be valid")
    }
    pub fn left_par_token(&self) -> SyntaxToken {
        self.inner().left_par_token().expect("node must be valid")
    }
    pub fn interface_package_generic_map_aspect_inner(
        &self,
    ) -> Valid<InterfacePackageGenericMapAspectInnerSyntax> {
        Valid::new_unchecked(
            self.inner()
                .interface_package_generic_map_aspect_inner()
                .expect("node must be valid"),
        )
    }
    pub fn right_par_token(&self) -> SyntaxToken {
        self.inner().right_par_token().expect("node must be valid")
    }
}
impl Valid<InterfacePackageGenericMapAspectAssociationsSyntax> {
    pub fn association_list(&self) -> Valid<AssociationListSyntax> {
        Valid::new_unchecked(self.inner().association_list().expect("node must be valid"))
    }
}
impl Valid<InterfacePackageGenericMapAspectBoxSyntax> {
    pub fn box_token(&self) -> SyntaxToken {
        self.inner().box_token().expect("node must be valid")
    }
}
impl Valid<InterfacePackageGenericMapAspectDefaultSyntax> {
    pub fn default_token(&self) -> SyntaxToken {
        self.inner().default_token().expect("node must be valid")
    }
}
#[derive(Debug, Clone)]
pub enum ValidInterfacePackageGenericMapAspectInner {
    InterfacePackageGenericMapAspectBox(Valid<InterfacePackageGenericMapAspectBoxSyntax>),
    InterfacePackageGenericMapAspectDefault(Valid<InterfacePackageGenericMapAspectDefaultSyntax>),
    InterfacePackageGenericMapAspectAssociations(
        Valid<InterfacePackageGenericMapAspectAssociationsSyntax>,
    ),
}
impl Valid<InterfacePackageGenericMapAspectInnerSyntax> {
    pub fn alternative(&self) -> ValidInterfacePackageGenericMapAspectInner {
        match self . inner () { InterfacePackageGenericMapAspectInnerSyntax :: InterfacePackageGenericMapAspectBox (inner) => ValidInterfacePackageGenericMapAspectInner :: InterfacePackageGenericMapAspectBox (Valid :: new_unchecked (inner . clone ())) , InterfacePackageGenericMapAspectInnerSyntax :: InterfacePackageGenericMapAspectDefault (inner) => ValidInterfacePackageGenericMapAspectInner :: InterfacePackageGenericMapAspectDefault (Valid :: new_unchecked (inner . clone ())) , InterfacePackageGenericMapAspectInnerSyntax :: InterfacePackageGenericMapAspectAssociations (inner) => ValidInterfacePackageGenericMapAspectInner :: InterfacePackageGenericMapAspectAssociations (Valid :: new_unchecked (inner . clone ())) , }
    }
}
impl Valid<InterfaceProcedureSpecificationSyntax> {
    pub fn procedure_token(&self) -> SyntaxToken {
        self.inner().procedure_token().expect("node must be valid")
    }
    pub fn designator(&self) -> DesignatorSyntax {
        self.inner().designator().expect("node must be valid")
    }
    pub fn parameter_list(&self) -> Option<Valid<ParameterListSyntax>> {
        self.inner().parameter_list().map(Valid::new_unchecked)
    }
}
impl Valid<InterfaceSubprogramDeclarationSyntax> {
    pub fn interface_subprogram_specification(
        &self,
    ) -> Valid<InterfaceSubprogramSpecificationSyntax> {
        Valid::new_unchecked(
            self.inner()
                .interface_subprogram_specification()
                .expect("node must be valid"),
        )
    }
    pub fn subprogram_default(&self) -> Option<Valid<SubprogramDefaultSyntax>> {
        self.inner().subprogram_default().map(Valid::new_unchecked)
    }
}
#[derive(Debug, Clone)]
pub enum ValidInterfaceSubprogramDefault {
    InterfaceSubprogramDefaultName(Valid<InterfaceSubprogramDefaultNameSyntax>),
    InterfaceSubprogramDefaultBox(Valid<InterfaceSubprogramDefaultBoxSyntax>),
}
impl Valid<InterfaceSubprogramDefaultSyntax> {
    pub fn alternative(&self) -> ValidInterfaceSubprogramDefault {
        match self.inner() {
            InterfaceSubprogramDefaultSyntax::InterfaceSubprogramDefaultName(inner) => {
                ValidInterfaceSubprogramDefault::InterfaceSubprogramDefaultName(
                    Valid::new_unchecked(inner.clone()),
                )
            }
            InterfaceSubprogramDefaultSyntax::InterfaceSubprogramDefaultBox(inner) => {
                ValidInterfaceSubprogramDefault::InterfaceSubprogramDefaultBox(
                    Valid::new_unchecked(inner.clone()),
                )
            }
        }
    }
}
impl Valid<InterfaceSubprogramDefaultBoxSyntax> {
    pub fn box_token(&self) -> SyntaxToken {
        self.inner().box_token().expect("node must be valid")
    }
}
impl Valid<InterfaceSubprogramDefaultNameSyntax> {
    pub fn name(&self) -> Valid<NameSyntax> {
        Valid::new_unchecked(self.inner().name().expect("node must be valid"))
    }
}
#[derive(Debug, Clone)]
pub enum ValidInterfaceSubprogramSpecification {
    InterfaceProcedureSpecification(Valid<InterfaceProcedureSpecificationSyntax>),
    InterfaceFunctionSpecification(Valid<InterfaceFunctionSpecificationSyntax>),
}
impl Valid<InterfaceSubprogramSpecificationSyntax> {
    pub fn alternative(&self) -> ValidInterfaceSubprogramSpecification {
        match self.inner() {
            InterfaceSubprogramSpecificationSyntax::InterfaceProcedureSpecification(inner) => {
                ValidInterfaceSubprogramSpecification::InterfaceProcedureSpecification(
                    Valid::new_unchecked(inner.clone()),
                )
            }
            InterfaceSubprogramSpecificationSyntax::InterfaceFunctionSpecification(inner) => {
                ValidInterfaceSubprogramSpecification::InterfaceFunctionSpecification(
                    Valid::new_unchecked(inner.clone()),
                )
            }
        }
    }
}
#[derive(Debug, Clone)]
pub enum ValidIterationScheme {
    WhileScheme(Valid<WhileSchemeSyntax>),
    ForScheme(Valid<ForSchemeSyntax>),
}
impl Valid<IterationSchemeSyntax> {
    pub fn alternative(&self) -> ValidIterationScheme {
        match self.inner() {
            IterationSchemeSyntax::WhileScheme(inner) => {
                ValidIterationScheme::WhileScheme(Valid::new_unchecked(inner.clone()))
            }
            IterationSchemeSyntax::ForScheme(inner) => {
                ValidIterationScheme::ForScheme(Valid::new_unchecked(inner.clone()))
            }
        }
    }
}
impl Valid<LibraryClauseSyntax> {
    pub fn library_token(&self) -> SyntaxToken {
        self.inner().library_token().expect("node must be valid")
    }
    pub fn logical_name_list(&self) -> Valid<LogicalNameListSyntax> {
        Valid::new_unchecked(
            self.inner()
                .logical_name_list()
                .expect("node must be valid"),
        )
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
#[derive(Debug, Clone)]
pub enum ValidLibraryUnit {
    PrimaryUnit(Valid<PrimaryUnitSyntax>),
    SecondaryUnit(Valid<SecondaryUnitSyntax>),
}
impl Valid<LibraryUnitSyntax> {
    pub fn alternative(&self) -> ValidLibraryUnit {
        match self.inner() {
            LibraryUnitSyntax::PrimaryUnit(inner) => {
                ValidLibraryUnit::PrimaryUnit(Valid::new_unchecked(inner.clone()))
            }
            LibraryUnitSyntax::SecondaryUnit(inner) => {
                ValidLibraryUnit::SecondaryUnit(Valid::new_unchecked(inner.clone()))
            }
        }
    }
}
impl Valid<LiteralExpressionSyntax> {
    pub fn literal(&self) -> LiteralSyntax {
        self.inner().literal().expect("node must be valid")
    }
}
impl Valid<LogicalNameListSyntax> {
    pub fn logical_names(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.inner().logical_names()
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.inner().comma_token()
    }
}
impl Valid<LoopStatementSyntax> {
    pub fn loop_statement_preamble(&self) -> Valid<LoopStatementPreambleSyntax> {
        Valid::new_unchecked(
            self.inner()
                .loop_statement_preamble()
                .expect("node must be valid"),
        )
    }
    pub fn sequence_of_statements(&self) -> Option<Valid<SequenceOfStatementsSyntax>> {
        self.inner()
            .sequence_of_statements()
            .map(Valid::new_unchecked)
    }
    pub fn loop_statement_epilogue(&self) -> Valid<LoopStatementEpilogueSyntax> {
        Valid::new_unchecked(
            self.inner()
                .loop_statement_epilogue()
                .expect("node must be valid"),
        )
    }
}
impl Valid<LoopStatementEpilogueSyntax> {
    pub fn end_token(&self) -> SyntaxToken {
        self.inner().end_token().expect("node must be valid")
    }
    pub fn loop_token(&self) -> SyntaxToken {
        self.inner().loop_token().expect("node must be valid")
    }
    pub fn label(&self) -> Option<SyntaxToken> {
        self.inner().label()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<LoopStatementPreambleSyntax> {
    pub fn stmt_label(&self) -> Option<Valid<StmtLabelSyntax>> {
        self.inner().stmt_label().map(Valid::new_unchecked)
    }
    pub fn iteration_scheme(&self) -> Option<Valid<IterationSchemeSyntax>> {
        self.inner().iteration_scheme().map(Valid::new_unchecked)
    }
    pub fn loop_token(&self) -> SyntaxToken {
        self.inner().loop_token().expect("node must be valid")
    }
}
impl Valid<NameSyntax> {
    pub fn prefix(&self) -> Valid<PrefixSyntax> {
        Valid::new_unchecked(self.inner().prefix().expect("node must be valid"))
    }
    pub fn name_tails(&self) -> impl Iterator<Item = Valid<NameTailSyntax>> + use<'_> {
        self.inner().name_tails().map(Valid::new_unchecked)
    }
    pub fn range_constraint(&self) -> Option<Valid<RangeConstraintSyntax>> {
        self.inner().range_constraint().map(Valid::new_unchecked)
    }
}
impl Valid<NameDesignatorPrefixSyntax> {
    pub fn name_designator(&self) -> NameDesignatorSyntax {
        self.inner().name_designator().expect("node must be valid")
    }
}
impl Valid<NameExpressionSyntax> {
    pub fn name(&self) -> Valid<NameSyntax> {
        Valid::new_unchecked(self.inner().name().expect("node must be valid"))
    }
}
impl Valid<NameListSyntax> {
    pub fn names(&self) -> impl Iterator<Item = Valid<NameSyntax>> + use<'_> {
        self.inner().names().map(Valid::new_unchecked)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.inner().comma_token()
    }
}
impl Valid<NameResolutionIndicationSyntax> {
    pub fn name(&self) -> Valid<NameSyntax> {
        Valid::new_unchecked(self.inner().name().expect("node must be valid"))
    }
}
#[derive(Debug, Clone)]
pub enum ValidNameTail {
    SelectedName(Valid<SelectedNameSyntax>),
    ParenthesizedName(Valid<ParenthesizedNameSyntax>),
    AttributeName(Valid<AttributeNameSyntax>),
}
impl Valid<NameTailSyntax> {
    pub fn alternative(&self) -> ValidNameTail {
        match self.inner() {
            NameTailSyntax::SelectedName(inner) => {
                ValidNameTail::SelectedName(Valid::new_unchecked(inner.clone()))
            }
            NameTailSyntax::ParenthesizedName(inner) => {
                ValidNameTail::ParenthesizedName(Valid::new_unchecked(inner.clone()))
            }
            NameTailSyntax::AttributeName(inner) => {
                ValidNameTail::AttributeName(Valid::new_unchecked(inner.clone()))
            }
        }
    }
}
impl Valid<NameTargetSyntax> {
    pub fn name(&self) -> Valid<NameSyntax> {
        Valid::new_unchecked(self.inner().name().expect("node must be valid"))
    }
}
impl Valid<NextStatementSyntax> {
    pub fn stmt_label(&self) -> Option<Valid<StmtLabelSyntax>> {
        self.inner().stmt_label().map(Valid::new_unchecked)
    }
    pub fn next_token(&self) -> SyntaxToken {
        self.inner().next_token().expect("node must be valid")
    }
    pub fn label(&self) -> Option<SyntaxToken> {
        self.inner().label()
    }
    pub fn when_clause(&self) -> Option<Valid<WhenClauseSyntax>> {
        self.inner().when_clause().map(Valid::new_unchecked)
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<NullStatementSyntax> {
    pub fn stmt_label(&self) -> Option<Valid<StmtLabelSyntax>> {
        self.inner().stmt_label().map(Valid::new_unchecked)
    }
    pub fn null_token(&self) -> SyntaxToken {
        self.inner().null_token().expect("node must be valid")
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<NumericTypeDefinitionSyntax> {
    pub fn range_constraint(&self) -> Valid<RangeConstraintSyntax> {
        Valid::new_unchecked(self.inner().range_constraint().expect("node must be valid"))
    }
}
impl Valid<OthersChoiceSyntax> {
    pub fn others_token(&self) -> SyntaxToken {
        self.inner().others_token().expect("node must be valid")
    }
}
impl Valid<PackageBodySyntax> {
    pub fn package_body_preamble(&self) -> Valid<PackageBodyPreambleSyntax> {
        Valid::new_unchecked(
            self.inner()
                .package_body_preamble()
                .expect("node must be valid"),
        )
    }
    pub fn package_body_declarative_part(&self) -> Option<Valid<PackageBodyDeclarativePartSyntax>> {
        self.inner()
            .package_body_declarative_part()
            .map(Valid::new_unchecked)
    }
    pub fn package_body_epilogue(&self) -> Valid<PackageBodyEpilogueSyntax> {
        Valid::new_unchecked(
            self.inner()
                .package_body_epilogue()
                .expect("node must be valid"),
        )
    }
}
impl Valid<PackageBodyDeclarationSyntax> {
    pub fn package_body(&self) -> Valid<PackageBodySyntax> {
        Valid::new_unchecked(self.inner().package_body().expect("node must be valid"))
    }
}
#[derive(Debug, Clone)]
pub enum ValidPackageBodyDeclarativeItem {
    SubprogramDeclaration(Valid<SubprogramDeclarationSyntax>),
    SubprogramBody(Valid<SubprogramBodySyntax>),
    SubprogramInstantiationDeclaration(Valid<SubprogramInstantiationDeclarationSyntax>),
    PackageDeclarationItem(Valid<PackageDeclarationItemSyntax>),
    PackageBodyDeclaration(Valid<PackageBodyDeclarationSyntax>),
    PackageInstantiationDeclarationItem(Valid<PackageInstantiationDeclarationItemSyntax>),
    TypeDeclaration(Valid<TypeDeclarationSyntax>),
    SubtypeDeclaration(Valid<SubtypeDeclarationSyntax>),
    ConstantDeclaration(Valid<ConstantDeclarationSyntax>),
    VariableDeclaration(Valid<VariableDeclarationSyntax>),
    FileDeclaration(Valid<FileDeclarationSyntax>),
    AliasDeclaration(Valid<AliasDeclarationSyntax>),
    AttributeDeclaration(Valid<AttributeDeclarationSyntax>),
    AttributeSpecification(Valid<AttributeSpecificationSyntax>),
    UseClauseDeclaration(Valid<UseClauseDeclarationSyntax>),
    GroupTemplateDeclaration(Valid<GroupTemplateDeclarationSyntax>),
    GroupDeclaration(Valid<GroupDeclarationSyntax>),
}
impl Valid<PackageBodyDeclarativeItemSyntax> {
    pub fn alternative(&self) -> ValidPackageBodyDeclarativeItem {
        match self.inner() {
            PackageBodyDeclarativeItemSyntax::SubprogramDeclaration(inner) => {
                ValidPackageBodyDeclarativeItem::SubprogramDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            PackageBodyDeclarativeItemSyntax::SubprogramBody(inner) => {
                ValidPackageBodyDeclarativeItem::SubprogramBody(Valid::new_unchecked(inner.clone()))
            }
            PackageBodyDeclarativeItemSyntax::SubprogramInstantiationDeclaration(inner) => {
                ValidPackageBodyDeclarativeItem::SubprogramInstantiationDeclaration(
                    Valid::new_unchecked(inner.clone()),
                )
            }
            PackageBodyDeclarativeItemSyntax::PackageDeclarationItem(inner) => {
                ValidPackageBodyDeclarativeItem::PackageDeclarationItem(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            PackageBodyDeclarativeItemSyntax::PackageBodyDeclaration(inner) => {
                ValidPackageBodyDeclarativeItem::PackageBodyDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            PackageBodyDeclarativeItemSyntax::PackageInstantiationDeclarationItem(inner) => {
                ValidPackageBodyDeclarativeItem::PackageInstantiationDeclarationItem(
                    Valid::new_unchecked(inner.clone()),
                )
            }
            PackageBodyDeclarativeItemSyntax::TypeDeclaration(inner) => {
                ValidPackageBodyDeclarativeItem::TypeDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            PackageBodyDeclarativeItemSyntax::SubtypeDeclaration(inner) => {
                ValidPackageBodyDeclarativeItem::SubtypeDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            PackageBodyDeclarativeItemSyntax::ConstantDeclaration(inner) => {
                ValidPackageBodyDeclarativeItem::ConstantDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            PackageBodyDeclarativeItemSyntax::VariableDeclaration(inner) => {
                ValidPackageBodyDeclarativeItem::VariableDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            PackageBodyDeclarativeItemSyntax::FileDeclaration(inner) => {
                ValidPackageBodyDeclarativeItem::FileDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            PackageBodyDeclarativeItemSyntax::AliasDeclaration(inner) => {
                ValidPackageBodyDeclarativeItem::AliasDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            PackageBodyDeclarativeItemSyntax::AttributeDeclaration(inner) => {
                ValidPackageBodyDeclarativeItem::AttributeDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            PackageBodyDeclarativeItemSyntax::AttributeSpecification(inner) => {
                ValidPackageBodyDeclarativeItem::AttributeSpecification(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            PackageBodyDeclarativeItemSyntax::UseClauseDeclaration(inner) => {
                ValidPackageBodyDeclarativeItem::UseClauseDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            PackageBodyDeclarativeItemSyntax::GroupTemplateDeclaration(inner) => {
                ValidPackageBodyDeclarativeItem::GroupTemplateDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            PackageBodyDeclarativeItemSyntax::GroupDeclaration(inner) => {
                ValidPackageBodyDeclarativeItem::GroupDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
        }
    }
}
impl Valid<PackageBodyDeclarativePartSyntax> {
    pub fn package_body_declarative_items(
        &self,
    ) -> impl Iterator<Item = Valid<PackageBodyDeclarativeItemSyntax>> + use<'_> {
        self.inner()
            .package_body_declarative_items()
            .map(Valid::new_unchecked)
    }
}
impl Valid<PackageBodyEpilogueSyntax> {
    pub fn end_token(&self) -> SyntaxToken {
        self.inner().end_token().expect("node must be valid")
    }
    pub fn end_package_body(&self) -> Option<Valid<EndPackageBodySyntax>> {
        self.inner().end_package_body().map(Valid::new_unchecked)
    }
    pub fn simple_name(&self) -> Option<SyntaxToken> {
        self.inner().simple_name()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<PackageBodyPreambleSyntax> {
    pub fn package_token(&self) -> SyntaxToken {
        self.inner().package_token().expect("node must be valid")
    }
    pub fn body_token(&self) -> SyntaxToken {
        self.inner().body_token().expect("node must be valid")
    }
    pub fn simple_name(&self) -> SyntaxToken {
        self.inner().simple_name().expect("node must be valid")
    }
    pub fn is_token(&self) -> SyntaxToken {
        self.inner().is_token().expect("node must be valid")
    }
}
impl Valid<PackageDeclarationSyntax> {
    pub fn package_preamble(&self) -> Valid<PackagePreambleSyntax> {
        Valid::new_unchecked(self.inner().package_preamble().expect("node must be valid"))
    }
    pub fn package_header(&self) -> Option<Valid<PackageHeaderSyntax>> {
        self.inner().package_header().map(Valid::new_unchecked)
    }
    pub fn package_declarative_part(&self) -> Option<Valid<PackageDeclarativePartSyntax>> {
        self.inner()
            .package_declarative_part()
            .map(Valid::new_unchecked)
    }
    pub fn package_epilogue(&self) -> Valid<PackageEpilogueSyntax> {
        Valid::new_unchecked(self.inner().package_epilogue().expect("node must be valid"))
    }
}
impl Valid<PackageDeclarationItemSyntax> {
    pub fn package_declaration(&self) -> Valid<PackageDeclarationSyntax> {
        Valid::new_unchecked(
            self.inner()
                .package_declaration()
                .expect("node must be valid"),
        )
    }
}
#[derive(Debug, Clone)]
pub enum ValidPackageDeclarativeItem {
    SubprogramDeclaration(Valid<SubprogramDeclarationSyntax>),
    SubprogramInstantiationDeclaration(Valid<SubprogramInstantiationDeclarationSyntax>),
    PackageDeclarationItem(Valid<PackageDeclarationItemSyntax>),
    PackageInstantiationDeclarationItem(Valid<PackageInstantiationDeclarationItemSyntax>),
    TypeDeclaration(Valid<TypeDeclarationSyntax>),
    SubtypeDeclaration(Valid<SubtypeDeclarationSyntax>),
    ConstantDeclaration(Valid<ConstantDeclarationSyntax>),
    SignalDeclaration(Valid<SignalDeclarationSyntax>),
    VariableDeclaration(Valid<VariableDeclarationSyntax>),
    FileDeclaration(Valid<FileDeclarationSyntax>),
    AliasDeclaration(Valid<AliasDeclarationSyntax>),
    ComponentDeclaration(Valid<ComponentDeclarationSyntax>),
    AttributeDeclaration(Valid<AttributeDeclarationSyntax>),
    AttributeSpecification(Valid<AttributeSpecificationSyntax>),
    DisconnectionSpecification(Valid<DisconnectionSpecificationSyntax>),
    UseClauseDeclaration(Valid<UseClauseDeclarationSyntax>),
    GroupTemplateDeclaration(Valid<GroupTemplateDeclarationSyntax>),
    GroupDeclaration(Valid<GroupDeclarationSyntax>),
}
impl Valid<PackageDeclarativeItemSyntax> {
    pub fn alternative(&self) -> ValidPackageDeclarativeItem {
        match self.inner() {
            PackageDeclarativeItemSyntax::SubprogramDeclaration(inner) => {
                ValidPackageDeclarativeItem::SubprogramDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            PackageDeclarativeItemSyntax::SubprogramInstantiationDeclaration(inner) => {
                ValidPackageDeclarativeItem::SubprogramInstantiationDeclaration(
                    Valid::new_unchecked(inner.clone()),
                )
            }
            PackageDeclarativeItemSyntax::PackageDeclarationItem(inner) => {
                ValidPackageDeclarativeItem::PackageDeclarationItem(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            PackageDeclarativeItemSyntax::PackageInstantiationDeclarationItem(inner) => {
                ValidPackageDeclarativeItem::PackageInstantiationDeclarationItem(
                    Valid::new_unchecked(inner.clone()),
                )
            }
            PackageDeclarativeItemSyntax::TypeDeclaration(inner) => {
                ValidPackageDeclarativeItem::TypeDeclaration(Valid::new_unchecked(inner.clone()))
            }
            PackageDeclarativeItemSyntax::SubtypeDeclaration(inner) => {
                ValidPackageDeclarativeItem::SubtypeDeclaration(Valid::new_unchecked(inner.clone()))
            }
            PackageDeclarativeItemSyntax::ConstantDeclaration(inner) => {
                ValidPackageDeclarativeItem::ConstantDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            PackageDeclarativeItemSyntax::SignalDeclaration(inner) => {
                ValidPackageDeclarativeItem::SignalDeclaration(Valid::new_unchecked(inner.clone()))
            }
            PackageDeclarativeItemSyntax::VariableDeclaration(inner) => {
                ValidPackageDeclarativeItem::VariableDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            PackageDeclarativeItemSyntax::FileDeclaration(inner) => {
                ValidPackageDeclarativeItem::FileDeclaration(Valid::new_unchecked(inner.clone()))
            }
            PackageDeclarativeItemSyntax::AliasDeclaration(inner) => {
                ValidPackageDeclarativeItem::AliasDeclaration(Valid::new_unchecked(inner.clone()))
            }
            PackageDeclarativeItemSyntax::ComponentDeclaration(inner) => {
                ValidPackageDeclarativeItem::ComponentDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            PackageDeclarativeItemSyntax::AttributeDeclaration(inner) => {
                ValidPackageDeclarativeItem::AttributeDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            PackageDeclarativeItemSyntax::AttributeSpecification(inner) => {
                ValidPackageDeclarativeItem::AttributeSpecification(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            PackageDeclarativeItemSyntax::DisconnectionSpecification(inner) => {
                ValidPackageDeclarativeItem::DisconnectionSpecification(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            PackageDeclarativeItemSyntax::UseClauseDeclaration(inner) => {
                ValidPackageDeclarativeItem::UseClauseDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            PackageDeclarativeItemSyntax::GroupTemplateDeclaration(inner) => {
                ValidPackageDeclarativeItem::GroupTemplateDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            PackageDeclarativeItemSyntax::GroupDeclaration(inner) => {
                ValidPackageDeclarativeItem::GroupDeclaration(Valid::new_unchecked(inner.clone()))
            }
        }
    }
}
impl Valid<PackageDeclarativePartSyntax> {
    pub fn package_declarative_items(
        &self,
    ) -> impl Iterator<Item = Valid<PackageDeclarativeItemSyntax>> + use<'_> {
        self.inner()
            .package_declarative_items()
            .map(Valid::new_unchecked)
    }
}
impl Valid<PackageEpilogueSyntax> {
    pub fn end_token(&self) -> SyntaxToken {
        self.inner().end_token().expect("node must be valid")
    }
    pub fn package_token(&self) -> Option<SyntaxToken> {
        self.inner().package_token()
    }
    pub fn simple_name(&self) -> Option<SyntaxToken> {
        self.inner().simple_name()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<PackageHeaderSyntax> {
    pub fn generic_clause(&self) -> Valid<GenericClauseSyntax> {
        Valid::new_unchecked(self.inner().generic_clause().expect("node must be valid"))
    }
    pub fn generic_map(&self) -> Option<Valid<GenericMapSyntax>> {
        self.inner().generic_map().map(Valid::new_unchecked)
    }
}
impl Valid<PackageInstantiationDeclarationSyntax> {
    pub fn package_instantiation_preamble(&self) -> Valid<PackageInstantiationPreambleSyntax> {
        Valid::new_unchecked(
            self.inner()
                .package_instantiation_preamble()
                .expect("node must be valid"),
        )
    }
    pub fn generic_map_aspect(&self) -> Option<Valid<GenericMapAspectSyntax>> {
        self.inner().generic_map_aspect().map(Valid::new_unchecked)
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<PackageInstantiationDeclarationItemSyntax> {
    pub fn package_instantiation_declaration(
        &self,
    ) -> Valid<PackageInstantiationDeclarationSyntax> {
        Valid::new_unchecked(
            self.inner()
                .package_instantiation_declaration()
                .expect("node must be valid"),
        )
    }
}
impl Valid<PackageInstantiationDeclarationPrimaryUnitSyntax> {
    pub fn package_instantiation_declaration(
        &self,
    ) -> Valid<PackageInstantiationDeclarationSyntax> {
        Valid::new_unchecked(
            self.inner()
                .package_instantiation_declaration()
                .expect("node must be valid"),
        )
    }
}
impl Valid<PackageInstantiationPreambleSyntax> {
    pub fn package_token(&self) -> SyntaxToken {
        self.inner().package_token().expect("node must be valid")
    }
    pub fn identifier_token(&self) -> SyntaxToken {
        self.inner().identifier_token().expect("node must be valid")
    }
    pub fn is_token(&self) -> SyntaxToken {
        self.inner().is_token().expect("node must be valid")
    }
    pub fn new_token(&self) -> SyntaxToken {
        self.inner().new_token().expect("node must be valid")
    }
    pub fn name(&self) -> Valid<NameSyntax> {
        Valid::new_unchecked(self.inner().name().expect("node must be valid"))
    }
}
impl Valid<PackagePathSyntax> {
    pub fn identifier_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.inner().identifier_token()
    }
    pub fn dot_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.inner().dot_token()
    }
}
impl Valid<PackagePathnameSyntax> {
    pub fn comm_at_token(&self) -> SyntaxToken {
        self.inner().comm_at_token().expect("node must be valid")
    }
    pub fn package_path(&self) -> Valid<PackagePathSyntax> {
        Valid::new_unchecked(self.inner().package_path().expect("node must be valid"))
    }
}
impl Valid<PackagePreambleSyntax> {
    pub fn package_token(&self) -> SyntaxToken {
        self.inner().package_token().expect("node must be valid")
    }
    pub fn identifier_token(&self) -> SyntaxToken {
        self.inner().identifier_token().expect("node must be valid")
    }
    pub fn is_token(&self) -> SyntaxToken {
        self.inner().is_token().expect("node must be valid")
    }
}
impl Valid<ParameterListSyntax> {
    pub fn parameter_token(&self) -> Option<SyntaxToken> {
        self.inner().parameter_token()
    }
    pub fn parenthesized_interface_list(&self) -> Valid<ParenthesizedInterfaceListSyntax> {
        Valid::new_unchecked(
            self.inner()
                .parenthesized_interface_list()
                .expect("node must be valid"),
        )
    }
}
impl Valid<ParameterSpecificationSyntax> {
    pub fn identifier_token(&self) -> SyntaxToken {
        self.inner().identifier_token().expect("node must be valid")
    }
    pub fn in_token(&self) -> SyntaxToken {
        self.inner().in_token().expect("node must be valid")
    }
    pub fn expression(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().expression().expect("node must be valid"))
    }
}
impl Valid<ParenthesizedConditionSyntax> {
    pub fn left_par_token(&self) -> SyntaxToken {
        self.inner().left_par_token().expect("node must be valid")
    }
    pub fn condition(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().condition().expect("node must be valid"))
    }
    pub fn right_par_token(&self) -> SyntaxToken {
        self.inner().right_par_token().expect("node must be valid")
    }
}
impl Valid<ParenthesizedElementResolutionSyntax> {
    pub fn left_par_token(&self) -> SyntaxToken {
        self.inner().left_par_token().expect("node must be valid")
    }
    pub fn element_resolution_resolution_indication(
        &self,
    ) -> Valid<ElementResolutionResolutionIndicationSyntax> {
        Valid::new_unchecked(
            self.inner()
                .element_resolution_resolution_indication()
                .expect("node must be valid"),
        )
    }
    pub fn right_par_token(&self) -> SyntaxToken {
        self.inner().right_par_token().expect("node must be valid")
    }
}
impl Valid<ParenthesizedExpressionSyntax> {
    pub fn left_par_token(&self) -> SyntaxToken {
        self.inner().left_par_token().expect("node must be valid")
    }
    pub fn expression(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().expression().expect("node must be valid"))
    }
    pub fn right_par_token(&self) -> SyntaxToken {
        self.inner().right_par_token().expect("node must be valid")
    }
}
impl Valid<ParenthesizedExpressionOrAggregateSyntax> {
    pub fn left_par_token(&self) -> SyntaxToken {
        self.inner().left_par_token().expect("node must be valid")
    }
    pub fn element_association_list(&self) -> Valid<ElementAssociationListSyntax> {
        Valid::new_unchecked(
            self.inner()
                .element_association_list()
                .expect("node must be valid"),
        )
    }
    pub fn right_par_token(&self) -> SyntaxToken {
        self.inner().right_par_token().expect("node must be valid")
    }
}
impl Valid<ParenthesizedInterfaceListSyntax> {
    pub fn left_par_token(&self) -> SyntaxToken {
        self.inner().left_par_token().expect("node must be valid")
    }
    pub fn formal_parameter_list(&self) -> Valid<InterfaceListSyntax> {
        Valid::new_unchecked(
            self.inner()
                .formal_parameter_list()
                .expect("node must be valid"),
        )
    }
    pub fn right_par_token(&self) -> SyntaxToken {
        self.inner().right_par_token().expect("node must be valid")
    }
}
impl Valid<ParenthesizedNameSyntax> {
    pub fn left_par_token(&self) -> SyntaxToken {
        self.inner().left_par_token().expect("node must be valid")
    }
    pub fn association_list(&self) -> Valid<AssociationListSyntax> {
        Valid::new_unchecked(self.inner().association_list().expect("node must be valid"))
    }
    pub fn right_par_token(&self) -> SyntaxToken {
        self.inner().right_par_token().expect("node must be valid")
    }
}
impl Valid<ParenthesizedProcessSensitivityListSyntax> {
    pub fn left_par_token(&self) -> SyntaxToken {
        self.inner().left_par_token().expect("node must be valid")
    }
    pub fn process_sensitivity_list(&self) -> Valid<ProcessSensitivityListSyntax> {
        Valid::new_unchecked(
            self.inner()
                .process_sensitivity_list()
                .expect("node must be valid"),
        )
    }
    pub fn right_par_token(&self) -> SyntaxToken {
        self.inner().right_par_token().expect("node must be valid")
    }
}
impl Valid<PartialPathnameSyntax> {
    pub fn pathname_elements(
        &self,
    ) -> impl Iterator<Item = Valid<PathnameElementSyntax>> + use<'_> {
        self.inner().pathname_elements().map(Valid::new_unchecked)
    }
    pub fn dot_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.inner().dot_token()
    }
}
impl Valid<PathnameElementSyntax> {
    pub fn identifier_token(&self) -> SyntaxToken {
        self.inner().identifier_token().expect("node must be valid")
    }
    pub fn parenthesized_expression(&self) -> Option<Valid<ParenthesizedExpressionSyntax>> {
        self.inner()
            .parenthesized_expression()
            .map(Valid::new_unchecked)
    }
}
impl Valid<PhysicalLiteralSyntax> {
    pub fn abstract_literal_token(&self) -> Option<SyntaxToken> {
        self.inner().abstract_literal_token()
    }
    pub fn name(&self) -> Valid<NameSyntax> {
        Valid::new_unchecked(self.inner().name().expect("node must be valid"))
    }
}
impl Valid<PhysicalLiteralExpressionSyntax> {
    pub fn physical_literal(&self) -> Valid<PhysicalLiteralSyntax> {
        Valid::new_unchecked(self.inner().physical_literal().expect("node must be valid"))
    }
}
impl Valid<PhysicalTypeDefinitionSyntax> {
    pub fn range_constraint(&self) -> Valid<RangeConstraintSyntax> {
        Valid::new_unchecked(self.inner().range_constraint().expect("node must be valid"))
    }
    pub fn unit_declarations(&self) -> Valid<UnitDeclarationsSyntax> {
        Valid::new_unchecked(
            self.inner()
                .unit_declarations()
                .expect("node must be valid"),
        )
    }
    pub fn physical_type_definition_epilogue(&self) -> Valid<PhysicalTypeDefinitionEpilogueSyntax> {
        Valid::new_unchecked(
            self.inner()
                .physical_type_definition_epilogue()
                .expect("node must be valid"),
        )
    }
}
impl Valid<PhysicalTypeDefinitionEpilogueSyntax> {
    pub fn end_token(&self) -> SyntaxToken {
        self.inner().end_token().expect("node must be valid")
    }
    pub fn units_token(&self) -> SyntaxToken {
        self.inner().units_token().expect("node must be valid")
    }
    pub fn simple_name(&self) -> Option<SyntaxToken> {
        self.inner().simple_name()
    }
}
impl Valid<PortClauseSyntax> {
    pub fn port_token(&self) -> SyntaxToken {
        self.inner().port_token().expect("node must be valid")
    }
    pub fn left_par_token(&self) -> SyntaxToken {
        self.inner().left_par_token().expect("node must be valid")
    }
    pub fn port_list(&self) -> Valid<InterfaceListSyntax> {
        Valid::new_unchecked(self.inner().port_list().expect("node must be valid"))
    }
    pub fn right_par_token(&self) -> SyntaxToken {
        self.inner().right_par_token().expect("node must be valid")
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<PortMapSyntax> {
    pub fn port_map_aspect(&self) -> Valid<PortMapAspectSyntax> {
        Valid::new_unchecked(self.inner().port_map_aspect().expect("node must be valid"))
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<PortMapAspectSyntax> {
    pub fn port_token(&self) -> SyntaxToken {
        self.inner().port_token().expect("node must be valid")
    }
    pub fn map_token(&self) -> SyntaxToken {
        self.inner().map_token().expect("node must be valid")
    }
    pub fn left_par_token(&self) -> SyntaxToken {
        self.inner().left_par_token().expect("node must be valid")
    }
    pub fn association_list(&self) -> Valid<AssociationListSyntax> {
        Valid::new_unchecked(self.inner().association_list().expect("node must be valid"))
    }
    pub fn right_par_token(&self) -> SyntaxToken {
        self.inner().right_par_token().expect("node must be valid")
    }
}
impl Valid<PortPartSyntax> {
    pub fn port_clause(&self) -> Valid<PortClauseSyntax> {
        Valid::new_unchecked(self.inner().port_clause().expect("node must be valid"))
    }
    pub fn port_map(&self) -> Option<Valid<PortMapSyntax>> {
        self.inner().port_map().map(Valid::new_unchecked)
    }
}
#[derive(Debug, Clone)]
pub enum ValidPrefix {
    ExternalName(Valid<ExternalNameSyntax>),
    NameDesignatorPrefix(Valid<NameDesignatorPrefixSyntax>),
}
impl Valid<PrefixSyntax> {
    pub fn alternative(&self) -> ValidPrefix {
        match self.inner() {
            PrefixSyntax::ExternalName(inner) => {
                ValidPrefix::ExternalName(Valid::new_unchecked(inner.clone()))
            }
            PrefixSyntax::NameDesignatorPrefix(inner) => {
                ValidPrefix::NameDesignatorPrefix(Valid::new_unchecked(inner.clone()))
            }
        }
    }
}
#[derive(Debug, Clone)]
pub enum ValidPrimaryUnit {
    EntityDeclaration(Valid<EntityDeclarationSyntax>),
    ConfigurationDeclaration(Valid<ConfigurationDeclarationSyntax>),
    PrimaryUnitPackageDeclaration(Valid<PrimaryUnitPackageDeclarationSyntax>),
    PackageInstantiationDeclarationPrimaryUnit(
        Valid<PackageInstantiationDeclarationPrimaryUnitSyntax>,
    ),
    ContextDeclaration(Valid<ContextDeclarationSyntax>),
}
impl Valid<PrimaryUnitSyntax> {
    pub fn alternative(&self) -> ValidPrimaryUnit {
        match self.inner() {
            PrimaryUnitSyntax::EntityDeclaration(inner) => {
                ValidPrimaryUnit::EntityDeclaration(Valid::new_unchecked(inner.clone()))
            }
            PrimaryUnitSyntax::ConfigurationDeclaration(inner) => {
                ValidPrimaryUnit::ConfigurationDeclaration(Valid::new_unchecked(inner.clone()))
            }
            PrimaryUnitSyntax::PrimaryUnitPackageDeclaration(inner) => {
                ValidPrimaryUnit::PrimaryUnitPackageDeclaration(Valid::new_unchecked(inner.clone()))
            }
            PrimaryUnitSyntax::PackageInstantiationDeclarationPrimaryUnit(inner) => {
                ValidPrimaryUnit::PackageInstantiationDeclarationPrimaryUnit(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            PrimaryUnitSyntax::ContextDeclaration(inner) => {
                ValidPrimaryUnit::ContextDeclaration(Valid::new_unchecked(inner.clone()))
            }
        }
    }
}
impl Valid<PrimaryUnitDeclarationSyntax> {
    pub fn identifier_token(&self) -> SyntaxToken {
        self.inner().identifier_token().expect("node must be valid")
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<PrimaryUnitPackageDeclarationSyntax> {
    pub fn package_declaration(&self) -> Valid<PackageDeclarationSyntax> {
        Valid::new_unchecked(
            self.inner()
                .package_declaration()
                .expect("node must be valid"),
        )
    }
}
impl Valid<ProcedureCallStatementSyntax> {
    pub fn stmt_label(&self) -> Option<Valid<StmtLabelSyntax>> {
        self.inner().stmt_label().map(Valid::new_unchecked)
    }
    pub fn procedure_call(&self) -> Valid<NameSyntax> {
        Valid::new_unchecked(self.inner().procedure_call().expect("node must be valid"))
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<ProcedureSpecificationSyntax> {
    pub fn procedure_token(&self) -> SyntaxToken {
        self.inner().procedure_token().expect("node must be valid")
    }
    pub fn designator(&self) -> DesignatorSyntax {
        self.inner().designator().expect("node must be valid")
    }
    pub fn subprogram_header(&self) -> Option<Valid<SubprogramHeaderSyntax>> {
        self.inner().subprogram_header().map(Valid::new_unchecked)
    }
    pub fn parameter_list(&self) -> Option<Valid<ParameterListSyntax>> {
        self.inner().parameter_list().map(Valid::new_unchecked)
    }
}
#[derive(Debug, Clone)]
pub enum ValidProcessDeclarativeItem {
    SubprogramDeclaration(Valid<SubprogramDeclarationSyntax>),
    SubprogramBody(Valid<SubprogramBodySyntax>),
    SubprogramInstantiationDeclaration(Valid<SubprogramInstantiationDeclarationSyntax>),
    PackageDeclarationItem(Valid<PackageDeclarationItemSyntax>),
    PackageBodyDeclaration(Valid<PackageBodyDeclarationSyntax>),
    PackageInstantiationDeclarationItem(Valid<PackageInstantiationDeclarationItemSyntax>),
    TypeDeclaration(Valid<TypeDeclarationSyntax>),
    SubtypeDeclaration(Valid<SubtypeDeclarationSyntax>),
    ConstantDeclaration(Valid<ConstantDeclarationSyntax>),
    VariableDeclaration(Valid<VariableDeclarationSyntax>),
    FileDeclaration(Valid<FileDeclarationSyntax>),
    AliasDeclaration(Valid<AliasDeclarationSyntax>),
    AttributeDeclaration(Valid<AttributeDeclarationSyntax>),
    AttributeSpecification(Valid<AttributeSpecificationSyntax>),
    UseClauseDeclaration(Valid<UseClauseDeclarationSyntax>),
    GroupTemplateDeclaration(Valid<GroupTemplateDeclarationSyntax>),
    GroupDeclaration(Valid<GroupDeclarationSyntax>),
}
impl Valid<ProcessDeclarativeItemSyntax> {
    pub fn alternative(&self) -> ValidProcessDeclarativeItem {
        match self.inner() {
            ProcessDeclarativeItemSyntax::SubprogramDeclaration(inner) => {
                ValidProcessDeclarativeItem::SubprogramDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            ProcessDeclarativeItemSyntax::SubprogramBody(inner) => {
                ValidProcessDeclarativeItem::SubprogramBody(Valid::new_unchecked(inner.clone()))
            }
            ProcessDeclarativeItemSyntax::SubprogramInstantiationDeclaration(inner) => {
                ValidProcessDeclarativeItem::SubprogramInstantiationDeclaration(
                    Valid::new_unchecked(inner.clone()),
                )
            }
            ProcessDeclarativeItemSyntax::PackageDeclarationItem(inner) => {
                ValidProcessDeclarativeItem::PackageDeclarationItem(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            ProcessDeclarativeItemSyntax::PackageBodyDeclaration(inner) => {
                ValidProcessDeclarativeItem::PackageBodyDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            ProcessDeclarativeItemSyntax::PackageInstantiationDeclarationItem(inner) => {
                ValidProcessDeclarativeItem::PackageInstantiationDeclarationItem(
                    Valid::new_unchecked(inner.clone()),
                )
            }
            ProcessDeclarativeItemSyntax::TypeDeclaration(inner) => {
                ValidProcessDeclarativeItem::TypeDeclaration(Valid::new_unchecked(inner.clone()))
            }
            ProcessDeclarativeItemSyntax::SubtypeDeclaration(inner) => {
                ValidProcessDeclarativeItem::SubtypeDeclaration(Valid::new_unchecked(inner.clone()))
            }
            ProcessDeclarativeItemSyntax::ConstantDeclaration(inner) => {
                ValidProcessDeclarativeItem::ConstantDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            ProcessDeclarativeItemSyntax::VariableDeclaration(inner) => {
                ValidProcessDeclarativeItem::VariableDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            ProcessDeclarativeItemSyntax::FileDeclaration(inner) => {
                ValidProcessDeclarativeItem::FileDeclaration(Valid::new_unchecked(inner.clone()))
            }
            ProcessDeclarativeItemSyntax::AliasDeclaration(inner) => {
                ValidProcessDeclarativeItem::AliasDeclaration(Valid::new_unchecked(inner.clone()))
            }
            ProcessDeclarativeItemSyntax::AttributeDeclaration(inner) => {
                ValidProcessDeclarativeItem::AttributeDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            ProcessDeclarativeItemSyntax::AttributeSpecification(inner) => {
                ValidProcessDeclarativeItem::AttributeSpecification(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            ProcessDeclarativeItemSyntax::UseClauseDeclaration(inner) => {
                ValidProcessDeclarativeItem::UseClauseDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            ProcessDeclarativeItemSyntax::GroupTemplateDeclaration(inner) => {
                ValidProcessDeclarativeItem::GroupTemplateDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            ProcessDeclarativeItemSyntax::GroupDeclaration(inner) => {
                ValidProcessDeclarativeItem::GroupDeclaration(Valid::new_unchecked(inner.clone()))
            }
        }
    }
}
impl Valid<ProcessDeclarativePartSyntax> {
    pub fn process_declarative_items(
        &self,
    ) -> impl Iterator<Item = Valid<ProcessDeclarativeItemSyntax>> + use<'_> {
        self.inner()
            .process_declarative_items()
            .map(Valid::new_unchecked)
    }
}
impl Valid<ProcessEpilogueSyntax> {
    pub fn end_token(&self) -> SyntaxToken {
        self.inner().end_token().expect("node must be valid")
    }
    pub fn postponed_token(&self) -> Option<SyntaxToken> {
        self.inner().postponed_token()
    }
    pub fn process_token(&self) -> SyntaxToken {
        self.inner().process_token().expect("node must be valid")
    }
    pub fn label(&self) -> Option<SyntaxToken> {
        self.inner().label()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<ProcessPreambleSyntax> {
    pub fn postponed_token(&self) -> Option<SyntaxToken> {
        self.inner().postponed_token()
    }
    pub fn process_token(&self) -> SyntaxToken {
        self.inner().process_token().expect("node must be valid")
    }
    pub fn parenthesized_process_sensitivity_list(
        &self,
    ) -> Option<Valid<ParenthesizedProcessSensitivityListSyntax>> {
        self.inner()
            .parenthesized_process_sensitivity_list()
            .map(Valid::new_unchecked)
    }
    pub fn is_token(&self) -> Option<SyntaxToken> {
        self.inner().is_token()
    }
}
#[derive(Debug, Clone)]
pub enum ValidProcessSensitivityList {
    AllSensitivityList(Valid<AllSensitivityListSyntax>),
    SensitivityList(Valid<SensitivityListSyntax>),
}
impl Valid<ProcessSensitivityListSyntax> {
    pub fn alternative(&self) -> ValidProcessSensitivityList {
        match self.inner() {
            ProcessSensitivityListSyntax::AllSensitivityList(inner) => {
                ValidProcessSensitivityList::AllSensitivityList(Valid::new_unchecked(inner.clone()))
            }
            ProcessSensitivityListSyntax::SensitivityList(inner) => {
                ValidProcessSensitivityList::SensitivityList(Valid::new_unchecked(inner.clone()))
            }
        }
    }
}
impl Valid<ProcessStatementSyntax> {
    pub fn stmt_label(&self) -> Option<Valid<StmtLabelSyntax>> {
        self.inner().stmt_label().map(Valid::new_unchecked)
    }
    pub fn process_preamble(&self) -> Valid<ProcessPreambleSyntax> {
        Valid::new_unchecked(self.inner().process_preamble().expect("node must be valid"))
    }
    pub fn process_declarative_part(&self) -> Option<Valid<ProcessDeclarativePartSyntax>> {
        self.inner()
            .process_declarative_part()
            .map(Valid::new_unchecked)
    }
    pub fn declaration_statement_separator(&self) -> Valid<DeclarationStatementSeparatorSyntax> {
        Valid::new_unchecked(
            self.inner()
                .declaration_statement_separator()
                .expect("node must be valid"),
        )
    }
    pub fn process_statement_part(&self) -> Option<Valid<ProcessStatementPartSyntax>> {
        self.inner()
            .process_statement_part()
            .map(Valid::new_unchecked)
    }
    pub fn process_epilogue(&self) -> Valid<ProcessEpilogueSyntax> {
        Valid::new_unchecked(self.inner().process_epilogue().expect("node must be valid"))
    }
}
impl Valid<ProcessStatementPartSyntax> {
    pub fn sequential_statements(
        &self,
    ) -> impl Iterator<Item = Valid<SequentialStatementSyntax>> + use<'_> {
        self.inner()
            .sequential_statements()
            .map(Valid::new_unchecked)
    }
}
impl Valid<ProtectedPreambleSyntax> {
    pub fn protected_token(&self) -> SyntaxToken {
        self.inner().protected_token().expect("node must be valid")
    }
}
impl Valid<ProtectedTypeBodySyntax> {
    pub fn protected_type_body_preamble(&self) -> Valid<ProtectedTypeBodyPreambleSyntax> {
        Valid::new_unchecked(
            self.inner()
                .protected_type_body_preamble()
                .expect("node must be valid"),
        )
    }
    pub fn protected_type_body_declarative_part(
        &self,
    ) -> Option<Valid<ProtectedTypeBodyDeclarativePartSyntax>> {
        self.inner()
            .protected_type_body_declarative_part()
            .map(Valid::new_unchecked)
    }
    pub fn protected_type_body_epilogue(&self) -> Valid<ProtectedTypeBodyEpilogueSyntax> {
        Valid::new_unchecked(
            self.inner()
                .protected_type_body_epilogue()
                .expect("node must be valid"),
        )
    }
}
#[derive(Debug, Clone)]
pub enum ValidProtectedTypeBodyDeclarativeItem {
    SubprogramDeclaration(Valid<SubprogramDeclarationSyntax>),
    SubprogramBody(Valid<SubprogramBodySyntax>),
    SubprogramInstantiationDeclaration(Valid<SubprogramInstantiationDeclarationSyntax>),
    PackageDeclarationItem(Valid<PackageDeclarationItemSyntax>),
    PackageBodyDeclaration(Valid<PackageBodyDeclarationSyntax>),
    PackageInstantiationDeclarationItem(Valid<PackageInstantiationDeclarationItemSyntax>),
    TypeDeclaration(Valid<TypeDeclarationSyntax>),
    SubtypeDeclaration(Valid<SubtypeDeclarationSyntax>),
    ConstantDeclaration(Valid<ConstantDeclarationSyntax>),
    VariableDeclaration(Valid<VariableDeclarationSyntax>),
    FileDeclaration(Valid<FileDeclarationSyntax>),
    AliasDeclaration(Valid<AliasDeclarationSyntax>),
    AttributeDeclaration(Valid<AttributeDeclarationSyntax>),
    AttributeSpecification(Valid<AttributeSpecificationSyntax>),
    UseClauseDeclaration(Valid<UseClauseDeclarationSyntax>),
    GroupTemplateDeclaration(Valid<GroupTemplateDeclarationSyntax>),
    GroupDeclaration(Valid<GroupDeclarationSyntax>),
}
impl Valid<ProtectedTypeBodyDeclarativeItemSyntax> {
    pub fn alternative(&self) -> ValidProtectedTypeBodyDeclarativeItem {
        match self.inner() {
            ProtectedTypeBodyDeclarativeItemSyntax::SubprogramDeclaration(inner) => {
                ValidProtectedTypeBodyDeclarativeItem::SubprogramDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            ProtectedTypeBodyDeclarativeItemSyntax::SubprogramBody(inner) => {
                ValidProtectedTypeBodyDeclarativeItem::SubprogramBody(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            ProtectedTypeBodyDeclarativeItemSyntax::SubprogramInstantiationDeclaration(inner) => {
                ValidProtectedTypeBodyDeclarativeItem::SubprogramInstantiationDeclaration(
                    Valid::new_unchecked(inner.clone()),
                )
            }
            ProtectedTypeBodyDeclarativeItemSyntax::PackageDeclarationItem(inner) => {
                ValidProtectedTypeBodyDeclarativeItem::PackageDeclarationItem(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            ProtectedTypeBodyDeclarativeItemSyntax::PackageBodyDeclaration(inner) => {
                ValidProtectedTypeBodyDeclarativeItem::PackageBodyDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            ProtectedTypeBodyDeclarativeItemSyntax::PackageInstantiationDeclarationItem(inner) => {
                ValidProtectedTypeBodyDeclarativeItem::PackageInstantiationDeclarationItem(
                    Valid::new_unchecked(inner.clone()),
                )
            }
            ProtectedTypeBodyDeclarativeItemSyntax::TypeDeclaration(inner) => {
                ValidProtectedTypeBodyDeclarativeItem::TypeDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            ProtectedTypeBodyDeclarativeItemSyntax::SubtypeDeclaration(inner) => {
                ValidProtectedTypeBodyDeclarativeItem::SubtypeDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            ProtectedTypeBodyDeclarativeItemSyntax::ConstantDeclaration(inner) => {
                ValidProtectedTypeBodyDeclarativeItem::ConstantDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            ProtectedTypeBodyDeclarativeItemSyntax::VariableDeclaration(inner) => {
                ValidProtectedTypeBodyDeclarativeItem::VariableDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            ProtectedTypeBodyDeclarativeItemSyntax::FileDeclaration(inner) => {
                ValidProtectedTypeBodyDeclarativeItem::FileDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            ProtectedTypeBodyDeclarativeItemSyntax::AliasDeclaration(inner) => {
                ValidProtectedTypeBodyDeclarativeItem::AliasDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            ProtectedTypeBodyDeclarativeItemSyntax::AttributeDeclaration(inner) => {
                ValidProtectedTypeBodyDeclarativeItem::AttributeDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            ProtectedTypeBodyDeclarativeItemSyntax::AttributeSpecification(inner) => {
                ValidProtectedTypeBodyDeclarativeItem::AttributeSpecification(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            ProtectedTypeBodyDeclarativeItemSyntax::UseClauseDeclaration(inner) => {
                ValidProtectedTypeBodyDeclarativeItem::UseClauseDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            ProtectedTypeBodyDeclarativeItemSyntax::GroupTemplateDeclaration(inner) => {
                ValidProtectedTypeBodyDeclarativeItem::GroupTemplateDeclaration(
                    Valid::new_unchecked(inner.clone()),
                )
            }
            ProtectedTypeBodyDeclarativeItemSyntax::GroupDeclaration(inner) => {
                ValidProtectedTypeBodyDeclarativeItem::GroupDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
        }
    }
}
impl Valid<ProtectedTypeBodyDeclarativePartSyntax> {
    pub fn protected_type_body_declarative_items(
        &self,
    ) -> impl Iterator<Item = Valid<ProtectedTypeBodyDeclarativeItemSyntax>> + use<'_> {
        self.inner()
            .protected_type_body_declarative_items()
            .map(Valid::new_unchecked)
    }
}
impl Valid<ProtectedTypeBodyEpilogueSyntax> {
    pub fn end_token(&self) -> SyntaxToken {
        self.inner().end_token().expect("node must be valid")
    }
    pub fn protected_token(&self) -> SyntaxToken {
        self.inner().protected_token().expect("node must be valid")
    }
    pub fn body_token(&self) -> SyntaxToken {
        self.inner().body_token().expect("node must be valid")
    }
    pub fn simple_name(&self) -> Option<SyntaxToken> {
        self.inner().simple_name()
    }
}
impl Valid<ProtectedTypeBodyPreambleSyntax> {
    pub fn protected_token(&self) -> SyntaxToken {
        self.inner().protected_token().expect("node must be valid")
    }
    pub fn body_token(&self) -> SyntaxToken {
        self.inner().body_token().expect("node must be valid")
    }
}
impl Valid<ProtectedTypeDeclarationSyntax> {
    pub fn protected_preamble(&self) -> Valid<ProtectedPreambleSyntax> {
        Valid::new_unchecked(
            self.inner()
                .protected_preamble()
                .expect("node must be valid"),
        )
    }
    pub fn protected_type_declarative_part(
        &self,
    ) -> Option<Valid<ProtectedTypeDeclarativePartSyntax>> {
        self.inner()
            .protected_type_declarative_part()
            .map(Valid::new_unchecked)
    }
    pub fn protected_type_declaration_epilogue(
        &self,
    ) -> Valid<ProtectedTypeDeclarationEpilogueSyntax> {
        Valid::new_unchecked(
            self.inner()
                .protected_type_declaration_epilogue()
                .expect("node must be valid"),
        )
    }
}
impl Valid<ProtectedTypeDeclarationEpilogueSyntax> {
    pub fn end_token(&self) -> SyntaxToken {
        self.inner().end_token().expect("node must be valid")
    }
    pub fn protected_token(&self) -> SyntaxToken {
        self.inner().protected_token().expect("node must be valid")
    }
    pub fn simple_name(&self) -> Option<SyntaxToken> {
        self.inner().simple_name()
    }
}
#[derive(Debug, Clone)]
pub enum ValidProtectedTypeDeclarativeItem {
    SubprogramDeclaration(Valid<SubprogramDeclarationSyntax>),
    SubprogramInstantiationDeclaration(Valid<SubprogramInstantiationDeclarationSyntax>),
    AttributeSpecification(Valid<AttributeSpecificationSyntax>),
    UseClauseDeclaration(Valid<UseClauseDeclarationSyntax>),
}
impl Valid<ProtectedTypeDeclarativeItemSyntax> {
    pub fn alternative(&self) -> ValidProtectedTypeDeclarativeItem {
        match self.inner() {
            ProtectedTypeDeclarativeItemSyntax::SubprogramDeclaration(inner) => {
                ValidProtectedTypeDeclarativeItem::SubprogramDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            ProtectedTypeDeclarativeItemSyntax::SubprogramInstantiationDeclaration(inner) => {
                ValidProtectedTypeDeclarativeItem::SubprogramInstantiationDeclaration(
                    Valid::new_unchecked(inner.clone()),
                )
            }
            ProtectedTypeDeclarativeItemSyntax::AttributeSpecification(inner) => {
                ValidProtectedTypeDeclarativeItem::AttributeSpecification(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            ProtectedTypeDeclarativeItemSyntax::UseClauseDeclaration(inner) => {
                ValidProtectedTypeDeclarativeItem::UseClauseDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
        }
    }
}
impl Valid<ProtectedTypeDeclarativePartSyntax> {
    pub fn protected_type_declarative_items(
        &self,
    ) -> impl Iterator<Item = Valid<ProtectedTypeDeclarativeItemSyntax>> + use<'_> {
        self.inner()
            .protected_type_declarative_items()
            .map(Valid::new_unchecked)
    }
}
#[derive(Debug, Clone)]
pub enum ValidProtectedTypeDefinition {
    ProtectedTypeDeclaration(Valid<ProtectedTypeDeclarationSyntax>),
    ProtectedTypeBody(Valid<ProtectedTypeBodySyntax>),
}
impl Valid<ProtectedTypeDefinitionSyntax> {
    pub fn alternative(&self) -> ValidProtectedTypeDefinition {
        match self.inner() {
            ProtectedTypeDefinitionSyntax::ProtectedTypeDeclaration(inner) => {
                ValidProtectedTypeDefinition::ProtectedTypeDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            ProtectedTypeDefinitionSyntax::ProtectedTypeBody(inner) => {
                ValidProtectedTypeDefinition::ProtectedTypeBody(Valid::new_unchecked(inner.clone()))
            }
        }
    }
}
impl Valid<QualifiedExpressionSyntax> {
    pub fn type_mark(&self) -> Valid<NameSyntax> {
        Valid::new_unchecked(self.inner().type_mark().expect("node must be valid"))
    }
    pub fn tick_token(&self) -> SyntaxToken {
        self.inner().tick_token().expect("node must be valid")
    }
    pub fn parenthesized_expression_or_aggregate(
        &self,
    ) -> Valid<ParenthesizedExpressionOrAggregateSyntax> {
        Valid::new_unchecked(
            self.inner()
                .parenthesized_expression_or_aggregate()
                .expect("node must be valid"),
        )
    }
}
impl Valid<RangeConstraintSyntax> {
    pub fn range_token(&self) -> SyntaxToken {
        self.inner().range_token().expect("node must be valid")
    }
    pub fn expression(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().expression().expect("node must be valid"))
    }
}
impl Valid<RecordElementDeclarationsSyntax> {
    pub fn element_declarations(
        &self,
    ) -> impl Iterator<Item = Valid<ElementDeclarationSyntax>> + use<'_> {
        self.inner()
            .element_declarations()
            .map(Valid::new_unchecked)
    }
}
impl Valid<RecordElementResolutionSyntax> {
    pub fn simple_name(&self) -> SyntaxToken {
        self.inner().simple_name().expect("node must be valid")
    }
    pub fn resolution_indication(&self) -> Valid<ResolutionIndicationSyntax> {
        Valid::new_unchecked(
            self.inner()
                .resolution_indication()
                .expect("node must be valid"),
        )
    }
}
impl Valid<RecordResolutionSyntax> {
    pub fn record_element_resolutions(
        &self,
    ) -> impl Iterator<Item = Valid<RecordElementResolutionSyntax>> + use<'_> {
        self.inner()
            .record_element_resolutions()
            .map(Valid::new_unchecked)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.inner().comma_token()
    }
}
impl Valid<RecordResolutionElementResolutionSyntax> {
    pub fn record_resolution(&self) -> Valid<RecordResolutionSyntax> {
        Valid::new_unchecked(
            self.inner()
                .record_resolution()
                .expect("node must be valid"),
        )
    }
}
impl Valid<RecordTypeDefinitionSyntax> {
    pub fn record_type_definition_preamble(&self) -> Valid<RecordTypeDefinitionPreambleSyntax> {
        Valid::new_unchecked(
            self.inner()
                .record_type_definition_preamble()
                .expect("node must be valid"),
        )
    }
    pub fn record_element_declarations(&self) -> Valid<RecordElementDeclarationsSyntax> {
        Valid::new_unchecked(
            self.inner()
                .record_element_declarations()
                .expect("node must be valid"),
        )
    }
    pub fn record_type_definition_epilogue(&self) -> Valid<RecordTypeDefinitionEpilogueSyntax> {
        Valid::new_unchecked(
            self.inner()
                .record_type_definition_epilogue()
                .expect("node must be valid"),
        )
    }
}
impl Valid<RecordTypeDefinitionEpilogueSyntax> {
    pub fn end_token(&self) -> SyntaxToken {
        self.inner().end_token().expect("node must be valid")
    }
    pub fn record_token(&self) -> SyntaxToken {
        self.inner().record_token().expect("node must be valid")
    }
    pub fn simple_name(&self) -> Option<SyntaxToken> {
        self.inner().simple_name()
    }
}
impl Valid<RecordTypeDefinitionPreambleSyntax> {
    pub fn record_token(&self) -> SyntaxToken {
        self.inner().record_token().expect("node must be valid")
    }
}
impl Valid<RejectClauseSyntax> {
    pub fn reject_token(&self) -> SyntaxToken {
        self.inner().reject_token().expect("node must be valid")
    }
    pub fn expression(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().expression().expect("node must be valid"))
    }
}
impl Valid<RelativePathnameSyntax> {
    pub fn up_levels(&self) -> impl Iterator<Item = Valid<UpLevelSyntax>> + use<'_> {
        self.inner().up_levels().map(Valid::new_unchecked)
    }
    pub fn partial_pathname(&self) -> Valid<PartialPathnameSyntax> {
        Valid::new_unchecked(self.inner().partial_pathname().expect("node must be valid"))
    }
}
impl Valid<ReportClauseSyntax> {
    pub fn report_token(&self) -> SyntaxToken {
        self.inner().report_token().expect("node must be valid")
    }
    pub fn expression(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().expression().expect("node must be valid"))
    }
}
impl Valid<ReportStatementSyntax> {
    pub fn stmt_label(&self) -> Option<Valid<StmtLabelSyntax>> {
        self.inner().stmt_label().map(Valid::new_unchecked)
    }
    pub fn report_token(&self) -> SyntaxToken {
        self.inner().report_token().expect("node must be valid")
    }
    pub fn expression(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().expression().expect("node must be valid"))
    }
    pub fn severity_clause(&self) -> Option<Valid<SeverityClauseSyntax>> {
        self.inner().severity_clause().map(Valid::new_unchecked)
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
#[derive(Debug, Clone)]
pub enum ValidResolutionIndication {
    NameResolutionIndication(Valid<NameResolutionIndicationSyntax>),
    ParenthesizedElementResolution(Valid<ParenthesizedElementResolutionSyntax>),
}
impl Valid<ResolutionIndicationSyntax> {
    pub fn alternative(&self) -> ValidResolutionIndication {
        match self.inner() {
            ResolutionIndicationSyntax::NameResolutionIndication(inner) => {
                ValidResolutionIndication::NameResolutionIndication(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            ResolutionIndicationSyntax::ParenthesizedElementResolution(inner) => {
                ValidResolutionIndication::ParenthesizedElementResolution(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
        }
    }
}
impl Valid<ReturnStatementSyntax> {
    pub fn stmt_label(&self) -> Option<Valid<StmtLabelSyntax>> {
        self.inner().stmt_label().map(Valid::new_unchecked)
    }
    pub fn return_token(&self) -> SyntaxToken {
        self.inner().return_token().expect("node must be valid")
    }
    pub fn expression(&self) -> Option<Valid<ExpressionSyntax>> {
        self.inner().expression().map(Valid::new_unchecked)
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<ReturnTypeSyntax> {
    pub fn return_token(&self) -> SyntaxToken {
        self.inner().return_token().expect("node must be valid")
    }
    pub fn type_mark(&self) -> Valid<NameSyntax> {
        Valid::new_unchecked(self.inner().type_mark().expect("node must be valid"))
    }
}
#[derive(Debug, Clone)]
pub enum ValidScalarTypeDefinition {
    EnumerationTypeDefinition(Valid<EnumerationTypeDefinitionSyntax>),
    NumericTypeDefinition(Valid<NumericTypeDefinitionSyntax>),
    PhysicalTypeDefinition(Valid<PhysicalTypeDefinitionSyntax>),
}
impl Valid<ScalarTypeDefinitionSyntax> {
    pub fn alternative(&self) -> ValidScalarTypeDefinition {
        match self.inner() {
            ScalarTypeDefinitionSyntax::EnumerationTypeDefinition(inner) => {
                ValidScalarTypeDefinition::EnumerationTypeDefinition(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            ScalarTypeDefinitionSyntax::NumericTypeDefinition(inner) => {
                ValidScalarTypeDefinition::NumericTypeDefinition(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            ScalarTypeDefinitionSyntax::PhysicalTypeDefinition(inner) => {
                ValidScalarTypeDefinition::PhysicalTypeDefinition(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
        }
    }
}
#[derive(Debug, Clone)]
pub enum ValidSecondaryUnit {
    ArchitectureBody(Valid<ArchitectureBodySyntax>),
    SecondaryUnitPackageBody(Valid<SecondaryUnitPackageBodySyntax>),
}
impl Valid<SecondaryUnitSyntax> {
    pub fn alternative(&self) -> ValidSecondaryUnit {
        match self.inner() {
            SecondaryUnitSyntax::ArchitectureBody(inner) => {
                ValidSecondaryUnit::ArchitectureBody(Valid::new_unchecked(inner.clone()))
            }
            SecondaryUnitSyntax::SecondaryUnitPackageBody(inner) => {
                ValidSecondaryUnit::SecondaryUnitPackageBody(Valid::new_unchecked(inner.clone()))
            }
        }
    }
}
impl Valid<SecondaryUnitDeclarationSyntax> {
    pub fn identifier_token(&self) -> SyntaxToken {
        self.inner().identifier_token().expect("node must be valid")
    }
    pub fn eq_token(&self) -> SyntaxToken {
        self.inner().eq_token().expect("node must be valid")
    }
    pub fn physical_literal(&self) -> Valid<PhysicalLiteralSyntax> {
        Valid::new_unchecked(self.inner().physical_literal().expect("node must be valid"))
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<SecondaryUnitPackageBodySyntax> {
    pub fn package_body(&self) -> Valid<PackageBodySyntax> {
        Valid::new_unchecked(self.inner().package_body().expect("node must be valid"))
    }
}
impl Valid<SelectedAssignmentPreambleSyntax> {
    pub fn with_token(&self) -> SyntaxToken {
        self.inner().with_token().expect("node must be valid")
    }
    pub fn expression(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().expression().expect("node must be valid"))
    }
    pub fn select_token(&self) -> SyntaxToken {
        self.inner().select_token().expect("node must be valid")
    }
    pub fn que_token(&self) -> Option<SyntaxToken> {
        self.inner().que_token()
    }
}
impl Valid<SelectedExpressionItemSyntax> {
    pub fn expression(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().expression().expect("node must be valid"))
    }
    pub fn when_token(&self) -> SyntaxToken {
        self.inner().when_token().expect("node must be valid")
    }
    pub fn choices(&self) -> Valid<ChoicesSyntax> {
        Valid::new_unchecked(self.inner().choices().expect("node must be valid"))
    }
}
impl Valid<SelectedExpressionsSyntax> {
    pub fn selected_expression_items(
        &self,
    ) -> impl Iterator<Item = Valid<SelectedExpressionItemSyntax>> + use<'_> {
        self.inner()
            .selected_expression_items()
            .map(Valid::new_unchecked)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.inner().comma_token()
    }
}
impl Valid<SelectedForceAssignmentSyntax> {
    pub fn stmt_label(&self) -> Option<Valid<StmtLabelSyntax>> {
        self.inner().stmt_label().map(Valid::new_unchecked)
    }
    pub fn selected_assignment_preamble(&self) -> Valid<SelectedAssignmentPreambleSyntax> {
        Valid::new_unchecked(
            self.inner()
                .selected_assignment_preamble()
                .expect("node must be valid"),
        )
    }
    pub fn target(&self) -> Valid<TargetSyntax> {
        Valid::new_unchecked(self.inner().target().expect("node must be valid"))
    }
    pub fn lte_token(&self) -> SyntaxToken {
        self.inner().lte_token().expect("node must be valid")
    }
    pub fn force_token(&self) -> SyntaxToken {
        self.inner().force_token().expect("node must be valid")
    }
    pub fn force_mode(&self) -> Option<ForceModeSyntax> {
        self.inner().force_mode()
    }
    pub fn selected_expressions(&self) -> Valid<SelectedExpressionsSyntax> {
        Valid::new_unchecked(
            self.inner()
                .selected_expressions()
                .expect("node must be valid"),
        )
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<SelectedNameSyntax> {
    pub fn dot_token(&self) -> SyntaxToken {
        self.inner().dot_token().expect("node must be valid")
    }
    pub fn suffix(&self) -> SuffixSyntax {
        self.inner().suffix().expect("node must be valid")
    }
}
#[derive(Debug, Clone)]
pub enum ValidSelectedSignalAssignment {
    SelectedWaveformAssignment(Valid<SelectedWaveformAssignmentSyntax>),
    SelectedForceAssignment(Valid<SelectedForceAssignmentSyntax>),
}
impl Valid<SelectedSignalAssignmentSyntax> {
    pub fn alternative(&self) -> ValidSelectedSignalAssignment {
        match self.inner() {
            SelectedSignalAssignmentSyntax::SelectedWaveformAssignment(inner) => {
                ValidSelectedSignalAssignment::SelectedWaveformAssignment(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            SelectedSignalAssignmentSyntax::SelectedForceAssignment(inner) => {
                ValidSelectedSignalAssignment::SelectedForceAssignment(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
        }
    }
}
impl Valid<SelectedVariableAssignmentSyntax> {
    pub fn stmt_label(&self) -> Option<Valid<StmtLabelSyntax>> {
        self.inner().stmt_label().map(Valid::new_unchecked)
    }
    pub fn selected_assignment_preamble(&self) -> Valid<SelectedAssignmentPreambleSyntax> {
        Valid::new_unchecked(
            self.inner()
                .selected_assignment_preamble()
                .expect("node must be valid"),
        )
    }
    pub fn target(&self) -> Valid<TargetSyntax> {
        Valid::new_unchecked(self.inner().target().expect("node must be valid"))
    }
    pub fn colon_eq_token(&self) -> SyntaxToken {
        self.inner().colon_eq_token().expect("node must be valid")
    }
    pub fn selected_expressions(&self) -> Valid<SelectedExpressionsSyntax> {
        Valid::new_unchecked(
            self.inner()
                .selected_expressions()
                .expect("node must be valid"),
        )
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<SelectedWaveformAssignmentSyntax> {
    pub fn stmt_label(&self) -> Option<Valid<StmtLabelSyntax>> {
        self.inner().stmt_label().map(Valid::new_unchecked)
    }
    pub fn selected_assignment_preamble(&self) -> Valid<SelectedAssignmentPreambleSyntax> {
        Valid::new_unchecked(
            self.inner()
                .selected_assignment_preamble()
                .expect("node must be valid"),
        )
    }
    pub fn target(&self) -> Valid<TargetSyntax> {
        Valid::new_unchecked(self.inner().target().expect("node must be valid"))
    }
    pub fn lte_token(&self) -> SyntaxToken {
        self.inner().lte_token().expect("node must be valid")
    }
    pub fn delay_mechanism(&self) -> Option<Valid<DelayMechanismSyntax>> {
        self.inner().delay_mechanism().map(Valid::new_unchecked)
    }
    pub fn selected_waveforms(&self) -> Valid<SelectedWaveformsSyntax> {
        Valid::new_unchecked(
            self.inner()
                .selected_waveforms()
                .expect("node must be valid"),
        )
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<SelectedWaveformItemSyntax> {
    pub fn waveform(&self) -> Valid<WaveformSyntax> {
        Valid::new_unchecked(self.inner().waveform().expect("node must be valid"))
    }
    pub fn when_token(&self) -> SyntaxToken {
        self.inner().when_token().expect("node must be valid")
    }
    pub fn choices(&self) -> Valid<ChoicesSyntax> {
        Valid::new_unchecked(self.inner().choices().expect("node must be valid"))
    }
}
impl Valid<SelectedWaveformsSyntax> {
    pub fn selected_waveform_items(
        &self,
    ) -> impl Iterator<Item = Valid<SelectedWaveformItemSyntax>> + use<'_> {
        self.inner()
            .selected_waveform_items()
            .map(Valid::new_unchecked)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.inner().comma_token()
    }
}
impl Valid<SensitivityClauseSyntax> {
    pub fn on_token(&self) -> SyntaxToken {
        self.inner().on_token().expect("node must be valid")
    }
    pub fn sensitivity_list(&self) -> Valid<SensitivityListSyntax> {
        Valid::new_unchecked(self.inner().sensitivity_list().expect("node must be valid"))
    }
}
impl Valid<SensitivityListSyntax> {
    pub fn names(&self) -> impl Iterator<Item = Valid<NameSyntax>> + use<'_> {
        self.inner().names().map(Valid::new_unchecked)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.inner().comma_token()
    }
}
impl Valid<SequenceOfStatementsSyntax> {
    pub fn sequential_statements(
        &self,
    ) -> impl Iterator<Item = Valid<SequentialStatementSyntax>> + use<'_> {
        self.inner()
            .sequential_statements()
            .map(Valid::new_unchecked)
    }
}
#[derive(Debug, Clone)]
pub enum ValidSequentialStatement {
    WaitStatement(Valid<WaitStatementSyntax>),
    AssertionStatement(Valid<AssertionStatementSyntax>),
    ReportStatement(Valid<ReportStatementSyntax>),
    SignalAssignmentStatement(Valid<SignalAssignmentStatementSyntax>),
    VariableAssignmentStatement(Valid<VariableAssignmentStatementSyntax>),
    ProcedureCallStatement(Valid<ProcedureCallStatementSyntax>),
    IfStatement(Valid<IfStatementSyntax>),
    CaseStatement(Valid<CaseStatementSyntax>),
    LoopStatement(Valid<LoopStatementSyntax>),
    NextStatement(Valid<NextStatementSyntax>),
    ExitStatement(Valid<ExitStatementSyntax>),
    ReturnStatement(Valid<ReturnStatementSyntax>),
    NullStatement(Valid<NullStatementSyntax>),
}
impl Valid<SequentialStatementSyntax> {
    pub fn alternative(&self) -> ValidSequentialStatement {
        match self.inner() {
            SequentialStatementSyntax::WaitStatement(inner) => {
                ValidSequentialStatement::WaitStatement(Valid::new_unchecked(inner.clone()))
            }
            SequentialStatementSyntax::AssertionStatement(inner) => {
                ValidSequentialStatement::AssertionStatement(Valid::new_unchecked(inner.clone()))
            }
            SequentialStatementSyntax::ReportStatement(inner) => {
                ValidSequentialStatement::ReportStatement(Valid::new_unchecked(inner.clone()))
            }
            SequentialStatementSyntax::SignalAssignmentStatement(inner) => {
                ValidSequentialStatement::SignalAssignmentStatement(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            SequentialStatementSyntax::VariableAssignmentStatement(inner) => {
                ValidSequentialStatement::VariableAssignmentStatement(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            SequentialStatementSyntax::ProcedureCallStatement(inner) => {
                ValidSequentialStatement::ProcedureCallStatement(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            SequentialStatementSyntax::IfStatement(inner) => {
                ValidSequentialStatement::IfStatement(Valid::new_unchecked(inner.clone()))
            }
            SequentialStatementSyntax::CaseStatement(inner) => {
                ValidSequentialStatement::CaseStatement(Valid::new_unchecked(inner.clone()))
            }
            SequentialStatementSyntax::LoopStatement(inner) => {
                ValidSequentialStatement::LoopStatement(Valid::new_unchecked(inner.clone()))
            }
            SequentialStatementSyntax::NextStatement(inner) => {
                ValidSequentialStatement::NextStatement(Valid::new_unchecked(inner.clone()))
            }
            SequentialStatementSyntax::ExitStatement(inner) => {
                ValidSequentialStatement::ExitStatement(Valid::new_unchecked(inner.clone()))
            }
            SequentialStatementSyntax::ReturnStatement(inner) => {
                ValidSequentialStatement::ReturnStatement(Valid::new_unchecked(inner.clone()))
            }
            SequentialStatementSyntax::NullStatement(inner) => {
                ValidSequentialStatement::NullStatement(Valid::new_unchecked(inner.clone()))
            }
        }
    }
}
impl Valid<SeverityClauseSyntax> {
    pub fn severity_token(&self) -> SyntaxToken {
        self.inner().severity_token().expect("node must be valid")
    }
    pub fn expression(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().expression().expect("node must be valid"))
    }
}
#[derive(Debug, Clone)]
pub enum ValidSignalAssignmentStatement {
    SimpleSignalAssignment(Valid<SimpleSignalAssignmentSyntax>),
    ConditionalSignalAssignment(Valid<ConditionalSignalAssignmentSyntax>),
    SelectedSignalAssignment(Valid<SelectedSignalAssignmentSyntax>),
}
impl Valid<SignalAssignmentStatementSyntax> {
    pub fn alternative(&self) -> ValidSignalAssignmentStatement {
        match self.inner() {
            SignalAssignmentStatementSyntax::SimpleSignalAssignment(inner) => {
                ValidSignalAssignmentStatement::SimpleSignalAssignment(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            SignalAssignmentStatementSyntax::ConditionalSignalAssignment(inner) => {
                ValidSignalAssignmentStatement::ConditionalSignalAssignment(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            SignalAssignmentStatementSyntax::SelectedSignalAssignment(inner) => {
                ValidSignalAssignmentStatement::SelectedSignalAssignment(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
        }
    }
}
impl Valid<SignalDeclarationSyntax> {
    pub fn signal_token(&self) -> SyntaxToken {
        self.inner().signal_token().expect("node must be valid")
    }
    pub fn identifier_list(&self) -> Valid<IdentifierListSyntax> {
        Valid::new_unchecked(self.inner().identifier_list().expect("node must be valid"))
    }
    pub fn colon_token(&self) -> SyntaxToken {
        self.inner().colon_token().expect("node must be valid")
    }
    pub fn subtype_indication(&self) -> Valid<SubtypeIndicationSyntax> {
        Valid::new_unchecked(
            self.inner()
                .subtype_indication()
                .expect("node must be valid"),
        )
    }
    pub fn signal_kind(&self) -> Option<SignalKindSyntax> {
        self.inner().signal_kind()
    }
    pub fn initial_value(&self) -> Option<Valid<InitialValueSyntax>> {
        self.inner().initial_value().map(Valid::new_unchecked)
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
#[derive(Debug, Clone)]
pub enum ValidSignalList {
    SignalListList(Valid<SignalListListSyntax>),
    SignalListOthers(Valid<SignalListOthersSyntax>),
    SignalListAll(Valid<SignalListAllSyntax>),
}
impl Valid<SignalListSyntax> {
    pub fn alternative(&self) -> ValidSignalList {
        match self.inner() {
            SignalListSyntax::SignalListList(inner) => {
                ValidSignalList::SignalListList(Valid::new_unchecked(inner.clone()))
            }
            SignalListSyntax::SignalListOthers(inner) => {
                ValidSignalList::SignalListOthers(Valid::new_unchecked(inner.clone()))
            }
            SignalListSyntax::SignalListAll(inner) => {
                ValidSignalList::SignalListAll(Valid::new_unchecked(inner.clone()))
            }
        }
    }
}
impl Valid<SignalListAllSyntax> {
    pub fn all_token(&self) -> SyntaxToken {
        self.inner().all_token().expect("node must be valid")
    }
}
impl Valid<SignalListListSyntax> {
    pub fn names(&self) -> impl Iterator<Item = Valid<NameSyntax>> + use<'_> {
        self.inner().names().map(Valid::new_unchecked)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.inner().comma_token()
    }
}
impl Valid<SignalListOthersSyntax> {
    pub fn others_token(&self) -> SyntaxToken {
        self.inner().others_token().expect("node must be valid")
    }
}
impl Valid<SignatureSyntax> {
    pub fn left_square_token(&self) -> SyntaxToken {
        self.inner()
            .left_square_token()
            .expect("node must be valid")
    }
    pub fn type_mark_list(&self) -> Option<Valid<TypeMarkListSyntax>> {
        self.inner().type_mark_list().map(Valid::new_unchecked)
    }
    pub fn return_type(&self) -> Option<Valid<ReturnTypeSyntax>> {
        self.inner().return_type().map(Valid::new_unchecked)
    }
    pub fn right_square_token(&self) -> SyntaxToken {
        self.inner()
            .right_square_token()
            .expect("node must be valid")
    }
}
impl Valid<SimpleConfigurationSpecificationSyntax> {
    pub fn component_configuration_preamble(&self) -> Valid<ComponentConfigurationPreambleSyntax> {
        Valid::new_unchecked(
            self.inner()
                .component_configuration_preamble()
                .expect("node must be valid"),
        )
    }
    pub fn binding_indication(&self) -> Option<Valid<BindingIndicationSyntax>> {
        self.inner().binding_indication().map(Valid::new_unchecked)
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
    pub fn component_configuration_epilogue(
        &self,
    ) -> Option<Valid<ComponentConfigurationEpilogueSyntax>> {
        self.inner()
            .component_configuration_epilogue()
            .map(Valid::new_unchecked)
    }
}
impl Valid<SimpleForceAssignmentSyntax> {
    pub fn stmt_label(&self) -> Option<Valid<StmtLabelSyntax>> {
        self.inner().stmt_label().map(Valid::new_unchecked)
    }
    pub fn target(&self) -> Valid<TargetSyntax> {
        Valid::new_unchecked(self.inner().target().expect("node must be valid"))
    }
    pub fn lte_token(&self) -> SyntaxToken {
        self.inner().lte_token().expect("node must be valid")
    }
    pub fn force_token(&self) -> SyntaxToken {
        self.inner().force_token().expect("node must be valid")
    }
    pub fn force_mode(&self) -> Option<ForceModeSyntax> {
        self.inner().force_mode()
    }
    pub fn expression(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().expression().expect("node must be valid"))
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<SimpleReleaseAssignmentSyntax> {
    pub fn stmt_label(&self) -> Option<Valid<StmtLabelSyntax>> {
        self.inner().stmt_label().map(Valid::new_unchecked)
    }
    pub fn target(&self) -> Valid<TargetSyntax> {
        Valid::new_unchecked(self.inner().target().expect("node must be valid"))
    }
    pub fn lte_token(&self) -> SyntaxToken {
        self.inner().lte_token().expect("node must be valid")
    }
    pub fn release_token(&self) -> SyntaxToken {
        self.inner().release_token().expect("node must be valid")
    }
    pub fn force_mode(&self) -> Option<ForceModeSyntax> {
        self.inner().force_mode()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
#[derive(Debug, Clone)]
pub enum ValidSimpleSignalAssignment {
    SimpleWaveformAssignment(Valid<SimpleWaveformAssignmentSyntax>),
    SimpleForceAssignment(Valid<SimpleForceAssignmentSyntax>),
    SimpleReleaseAssignment(Valid<SimpleReleaseAssignmentSyntax>),
}
impl Valid<SimpleSignalAssignmentSyntax> {
    pub fn alternative(&self) -> ValidSimpleSignalAssignment {
        match self.inner() {
            SimpleSignalAssignmentSyntax::SimpleWaveformAssignment(inner) => {
                ValidSimpleSignalAssignment::SimpleWaveformAssignment(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            SimpleSignalAssignmentSyntax::SimpleForceAssignment(inner) => {
                ValidSimpleSignalAssignment::SimpleForceAssignment(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            SimpleSignalAssignmentSyntax::SimpleReleaseAssignment(inner) => {
                ValidSimpleSignalAssignment::SimpleReleaseAssignment(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
        }
    }
}
impl Valid<SimpleVariableAssignmentSyntax> {
    pub fn stmt_label(&self) -> Option<Valid<StmtLabelSyntax>> {
        self.inner().stmt_label().map(Valid::new_unchecked)
    }
    pub fn target(&self) -> Valid<TargetSyntax> {
        Valid::new_unchecked(self.inner().target().expect("node must be valid"))
    }
    pub fn colon_eq_token(&self) -> SyntaxToken {
        self.inner().colon_eq_token().expect("node must be valid")
    }
    pub fn expression(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().expression().expect("node must be valid"))
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<SimpleWaveformAssignmentSyntax> {
    pub fn stmt_label(&self) -> Option<Valid<StmtLabelSyntax>> {
        self.inner().stmt_label().map(Valid::new_unchecked)
    }
    pub fn target(&self) -> Valid<TargetSyntax> {
        Valid::new_unchecked(self.inner().target().expect("node must be valid"))
    }
    pub fn lte_token(&self) -> SyntaxToken {
        self.inner().lte_token().expect("node must be valid")
    }
    pub fn delay_mechanism(&self) -> Option<Valid<DelayMechanismSyntax>> {
        self.inner().delay_mechanism().map(Valid::new_unchecked)
    }
    pub fn waveform(&self) -> Valid<WaveformSyntax> {
        Valid::new_unchecked(self.inner().waveform().expect("node must be valid"))
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<StmtLabelSyntax> {
    pub fn label(&self) -> SyntaxToken {
        self.inner().label().expect("node must be valid")
    }
    pub fn colon_token(&self) -> SyntaxToken {
        self.inner().colon_token().expect("node must be valid")
    }
}
impl Valid<SubprogramBodySyntax> {
    pub fn subprogram_body_preamble(&self) -> Valid<SubprogramBodyPreambleSyntax> {
        Valid::new_unchecked(
            self.inner()
                .subprogram_body_preamble()
                .expect("node must be valid"),
        )
    }
    pub fn subprogram_declarative_part(&self) -> Option<Valid<SubprogramDeclarativePartSyntax>> {
        self.inner()
            .subprogram_declarative_part()
            .map(Valid::new_unchecked)
    }
    pub fn declaration_statement_separator(&self) -> Valid<DeclarationStatementSeparatorSyntax> {
        Valid::new_unchecked(
            self.inner()
                .declaration_statement_separator()
                .expect("node must be valid"),
        )
    }
    pub fn subprogram_statement_part(&self) -> Option<Valid<SubprogramStatementPartSyntax>> {
        self.inner()
            .subprogram_statement_part()
            .map(Valid::new_unchecked)
    }
    pub fn subprogram_body_epilogue(&self) -> Valid<SubprogramBodyEpilogueSyntax> {
        Valid::new_unchecked(
            self.inner()
                .subprogram_body_epilogue()
                .expect("node must be valid"),
        )
    }
}
impl Valid<SubprogramBodyEpilogueSyntax> {
    pub fn end_token(&self) -> SyntaxToken {
        self.inner().end_token().expect("node must be valid")
    }
    pub fn subprogram_kind(&self) -> Option<SubprogramKindSyntax> {
        self.inner().subprogram_kind()
    }
    pub fn designator(&self) -> Option<DesignatorSyntax> {
        self.inner().designator()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<SubprogramBodyPreambleSyntax> {
    pub fn subprogram_specification(&self) -> Valid<SubprogramSpecificationSyntax> {
        Valid::new_unchecked(
            self.inner()
                .subprogram_specification()
                .expect("node must be valid"),
        )
    }
    pub fn is_token(&self) -> SyntaxToken {
        self.inner().is_token().expect("node must be valid")
    }
}
impl Valid<SubprogramDeclarationSyntax> {
    pub fn subprogram_specification(&self) -> Valid<SubprogramSpecificationSyntax> {
        Valid::new_unchecked(
            self.inner()
                .subprogram_specification()
                .expect("node must be valid"),
        )
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
#[derive(Debug, Clone)]
pub enum ValidSubprogramDeclarativeItem {
    SubprogramDeclaration(Valid<SubprogramDeclarationSyntax>),
    SubprogramBody(Valid<SubprogramBodySyntax>),
    SubprogramInstantiationDeclaration(Valid<SubprogramInstantiationDeclarationSyntax>),
    PackageDeclarationItem(Valid<PackageDeclarationItemSyntax>),
    PackageBodyDeclaration(Valid<PackageBodyDeclarationSyntax>),
    PackageInstantiationDeclarationItem(Valid<PackageInstantiationDeclarationItemSyntax>),
    TypeDeclaration(Valid<TypeDeclarationSyntax>),
    SubtypeDeclaration(Valid<SubtypeDeclarationSyntax>),
    ConstantDeclaration(Valid<ConstantDeclarationSyntax>),
    VariableDeclaration(Valid<VariableDeclarationSyntax>),
    FileDeclaration(Valid<FileDeclarationSyntax>),
    AliasDeclaration(Valid<AliasDeclarationSyntax>),
    AttributeDeclaration(Valid<AttributeDeclarationSyntax>),
    AttributeSpecification(Valid<AttributeSpecificationSyntax>),
    UseClauseDeclaration(Valid<UseClauseDeclarationSyntax>),
    GroupTemplateDeclaration(Valid<GroupTemplateDeclarationSyntax>),
    GroupDeclaration(Valid<GroupDeclarationSyntax>),
}
impl Valid<SubprogramDeclarativeItemSyntax> {
    pub fn alternative(&self) -> ValidSubprogramDeclarativeItem {
        match self.inner() {
            SubprogramDeclarativeItemSyntax::SubprogramDeclaration(inner) => {
                ValidSubprogramDeclarativeItem::SubprogramDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            SubprogramDeclarativeItemSyntax::SubprogramBody(inner) => {
                ValidSubprogramDeclarativeItem::SubprogramBody(Valid::new_unchecked(inner.clone()))
            }
            SubprogramDeclarativeItemSyntax::SubprogramInstantiationDeclaration(inner) => {
                ValidSubprogramDeclarativeItem::SubprogramInstantiationDeclaration(
                    Valid::new_unchecked(inner.clone()),
                )
            }
            SubprogramDeclarativeItemSyntax::PackageDeclarationItem(inner) => {
                ValidSubprogramDeclarativeItem::PackageDeclarationItem(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            SubprogramDeclarativeItemSyntax::PackageBodyDeclaration(inner) => {
                ValidSubprogramDeclarativeItem::PackageBodyDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            SubprogramDeclarativeItemSyntax::PackageInstantiationDeclarationItem(inner) => {
                ValidSubprogramDeclarativeItem::PackageInstantiationDeclarationItem(
                    Valid::new_unchecked(inner.clone()),
                )
            }
            SubprogramDeclarativeItemSyntax::TypeDeclaration(inner) => {
                ValidSubprogramDeclarativeItem::TypeDeclaration(Valid::new_unchecked(inner.clone()))
            }
            SubprogramDeclarativeItemSyntax::SubtypeDeclaration(inner) => {
                ValidSubprogramDeclarativeItem::SubtypeDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            SubprogramDeclarativeItemSyntax::ConstantDeclaration(inner) => {
                ValidSubprogramDeclarativeItem::ConstantDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            SubprogramDeclarativeItemSyntax::VariableDeclaration(inner) => {
                ValidSubprogramDeclarativeItem::VariableDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            SubprogramDeclarativeItemSyntax::FileDeclaration(inner) => {
                ValidSubprogramDeclarativeItem::FileDeclaration(Valid::new_unchecked(inner.clone()))
            }
            SubprogramDeclarativeItemSyntax::AliasDeclaration(inner) => {
                ValidSubprogramDeclarativeItem::AliasDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            SubprogramDeclarativeItemSyntax::AttributeDeclaration(inner) => {
                ValidSubprogramDeclarativeItem::AttributeDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            SubprogramDeclarativeItemSyntax::AttributeSpecification(inner) => {
                ValidSubprogramDeclarativeItem::AttributeSpecification(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            SubprogramDeclarativeItemSyntax::UseClauseDeclaration(inner) => {
                ValidSubprogramDeclarativeItem::UseClauseDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            SubprogramDeclarativeItemSyntax::GroupTemplateDeclaration(inner) => {
                ValidSubprogramDeclarativeItem::GroupTemplateDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            SubprogramDeclarativeItemSyntax::GroupDeclaration(inner) => {
                ValidSubprogramDeclarativeItem::GroupDeclaration(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
        }
    }
}
impl Valid<SubprogramDeclarativePartSyntax> {
    pub fn subprogram_declarative_items(
        &self,
    ) -> impl Iterator<Item = Valid<SubprogramDeclarativeItemSyntax>> + use<'_> {
        self.inner()
            .subprogram_declarative_items()
            .map(Valid::new_unchecked)
    }
}
impl Valid<SubprogramDefaultSyntax> {
    pub fn is_token(&self) -> SyntaxToken {
        self.inner().is_token().expect("node must be valid")
    }
    pub fn interface_subprogram_default(&self) -> Valid<InterfaceSubprogramDefaultSyntax> {
        Valid::new_unchecked(
            self.inner()
                .interface_subprogram_default()
                .expect("node must be valid"),
        )
    }
}
impl Valid<SubprogramHeaderSyntax> {
    pub fn subprogram_header_generic_clause(&self) -> Valid<SubprogramHeaderGenericClauseSyntax> {
        Valid::new_unchecked(
            self.inner()
                .subprogram_header_generic_clause()
                .expect("node must be valid"),
        )
    }
    pub fn generic_map_aspect(&self) -> Option<Valid<GenericMapAspectSyntax>> {
        self.inner().generic_map_aspect().map(Valid::new_unchecked)
    }
}
impl Valid<SubprogramHeaderGenericClauseSyntax> {
    pub fn generic_token(&self) -> SyntaxToken {
        self.inner().generic_token().expect("node must be valid")
    }
    pub fn left_par_token(&self) -> SyntaxToken {
        self.inner().left_par_token().expect("node must be valid")
    }
    pub fn generic_list(&self) -> Valid<InterfaceListSyntax> {
        Valid::new_unchecked(self.inner().generic_list().expect("node must be valid"))
    }
    pub fn right_par_token(&self) -> SyntaxToken {
        self.inner().right_par_token().expect("node must be valid")
    }
}
impl Valid<SubprogramInstantiationDeclarationSyntax> {
    pub fn subprogram_instantiation_declaration_preamble(
        &self,
    ) -> Valid<SubprogramInstantiationDeclarationPreambleSyntax> {
        Valid::new_unchecked(
            self.inner()
                .subprogram_instantiation_declaration_preamble()
                .expect("node must be valid"),
        )
    }
    pub fn generic_map_aspect(&self) -> Option<Valid<GenericMapAspectSyntax>> {
        self.inner().generic_map_aspect().map(Valid::new_unchecked)
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<SubprogramInstantiationDeclarationPreambleSyntax> {
    pub fn subprogram_kind(&self) -> SubprogramKindSyntax {
        self.inner().subprogram_kind().expect("node must be valid")
    }
    pub fn identifier_token(&self) -> SyntaxToken {
        self.inner().identifier_token().expect("node must be valid")
    }
    pub fn is_token(&self) -> SyntaxToken {
        self.inner().is_token().expect("node must be valid")
    }
    pub fn new_token(&self) -> SyntaxToken {
        self.inner().new_token().expect("node must be valid")
    }
    pub fn name(&self) -> Valid<NameSyntax> {
        Valid::new_unchecked(self.inner().name().expect("node must be valid"))
    }
    pub fn signature(&self) -> Option<Valid<SignatureSyntax>> {
        self.inner().signature().map(Valid::new_unchecked)
    }
}
#[derive(Debug, Clone)]
pub enum ValidSubprogramSpecification {
    ProcedureSpecification(Valid<ProcedureSpecificationSyntax>),
    FunctionSpecification(Valid<FunctionSpecificationSyntax>),
}
impl Valid<SubprogramSpecificationSyntax> {
    pub fn alternative(&self) -> ValidSubprogramSpecification {
        match self.inner() {
            SubprogramSpecificationSyntax::ProcedureSpecification(inner) => {
                ValidSubprogramSpecification::ProcedureSpecification(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            SubprogramSpecificationSyntax::FunctionSpecification(inner) => {
                ValidSubprogramSpecification::FunctionSpecification(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
        }
    }
}
impl Valid<SubprogramStatementPartSyntax> {
    pub fn sequential_statements(
        &self,
    ) -> impl Iterator<Item = Valid<SequentialStatementSyntax>> + use<'_> {
        self.inner()
            .sequential_statements()
            .map(Valid::new_unchecked)
    }
}
impl Valid<SubtypeDeclarationSyntax> {
    pub fn subtype_token(&self) -> SyntaxToken {
        self.inner().subtype_token().expect("node must be valid")
    }
    pub fn identifier_token(&self) -> SyntaxToken {
        self.inner().identifier_token().expect("node must be valid")
    }
    pub fn is_token(&self) -> SyntaxToken {
        self.inner().is_token().expect("node must be valid")
    }
    pub fn subtype_indication(&self) -> Valid<SubtypeIndicationSyntax> {
        Valid::new_unchecked(
            self.inner()
                .subtype_indication()
                .expect("node must be valid"),
        )
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<SubtypeIndicationSyntax> {
    pub fn resolution_indication(&self) -> Option<Valid<ResolutionIndicationSyntax>> {
        self.inner()
            .resolution_indication()
            .map(Valid::new_unchecked)
    }
    pub fn type_mark(&self) -> Valid<NameSyntax> {
        Valid::new_unchecked(self.inner().type_mark().expect("node must be valid"))
    }
}
#[derive(Debug, Clone)]
pub enum ValidTarget {
    NameTarget(Valid<NameTargetSyntax>),
    AggregateTarget(Valid<AggregateTargetSyntax>),
}
impl Valid<TargetSyntax> {
    pub fn alternative(&self) -> ValidTarget {
        match self.inner() {
            TargetSyntax::NameTarget(inner) => {
                ValidTarget::NameTarget(Valid::new_unchecked(inner.clone()))
            }
            TargetSyntax::AggregateTarget(inner) => {
                ValidTarget::AggregateTarget(Valid::new_unchecked(inner.clone()))
            }
        }
    }
}
impl Valid<TimeoutClauseSyntax> {
    pub fn for_token(&self) -> SyntaxToken {
        self.inner().for_token().expect("node must be valid")
    }
    pub fn expression(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().expression().expect("node must be valid"))
    }
}
impl Valid<TransportDelayMechanismSyntax> {
    pub fn transport_token(&self) -> SyntaxToken {
        self.inner().transport_token().expect("node must be valid")
    }
}
#[derive(Debug, Clone)]
pub enum ValidTypeDeclaration {
    FullTypeDeclaration(Valid<FullTypeDeclarationSyntax>),
    IncompleteTypeDeclaration(Valid<IncompleteTypeDeclarationSyntax>),
}
impl Valid<TypeDeclarationSyntax> {
    pub fn alternative(&self) -> ValidTypeDeclaration {
        match self.inner() {
            TypeDeclarationSyntax::FullTypeDeclaration(inner) => {
                ValidTypeDeclaration::FullTypeDeclaration(Valid::new_unchecked(inner.clone()))
            }
            TypeDeclarationSyntax::IncompleteTypeDeclaration(inner) => {
                ValidTypeDeclaration::IncompleteTypeDeclaration(Valid::new_unchecked(inner.clone()))
            }
        }
    }
}
#[derive(Debug, Clone)]
pub enum ValidTypeDefinition {
    ScalarTypeDefinition(Valid<ScalarTypeDefinitionSyntax>),
    CompositeTypeDefinition(Valid<CompositeTypeDefinitionSyntax>),
    AccessTypeDefinition(Valid<AccessTypeDefinitionSyntax>),
    FileTypeDefinition(Valid<FileTypeDefinitionSyntax>),
    ProtectedTypeDefinition(Valid<ProtectedTypeDefinitionSyntax>),
}
impl Valid<TypeDefinitionSyntax> {
    pub fn alternative(&self) -> ValidTypeDefinition {
        match self.inner() {
            TypeDefinitionSyntax::ScalarTypeDefinition(inner) => {
                ValidTypeDefinition::ScalarTypeDefinition(Valid::new_unchecked(inner.clone()))
            }
            TypeDefinitionSyntax::CompositeTypeDefinition(inner) => {
                ValidTypeDefinition::CompositeTypeDefinition(Valid::new_unchecked(inner.clone()))
            }
            TypeDefinitionSyntax::AccessTypeDefinition(inner) => {
                ValidTypeDefinition::AccessTypeDefinition(Valid::new_unchecked(inner.clone()))
            }
            TypeDefinitionSyntax::FileTypeDefinition(inner) => {
                ValidTypeDefinition::FileTypeDefinition(Valid::new_unchecked(inner.clone()))
            }
            TypeDefinitionSyntax::ProtectedTypeDefinition(inner) => {
                ValidTypeDefinition::ProtectedTypeDefinition(Valid::new_unchecked(inner.clone()))
            }
        }
    }
}
impl Valid<TypeMarkListSyntax> {
    pub fn type_marks(&self) -> impl Iterator<Item = Valid<NameSyntax>> + use<'_> {
        self.inner().type_marks().map(Valid::new_unchecked)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.inner().comma_token()
    }
}
impl Valid<UnaffectedWaveformSyntax> {
    pub fn unaffected_token(&self) -> SyntaxToken {
        self.inner().unaffected_token().expect("node must be valid")
    }
}
impl Valid<UnaryExpressionSyntax> {
    pub fn unary_operator(&self) -> UnaryOperatorSyntax {
        self.inner().unary_operator().expect("node must be valid")
    }
    pub fn expression(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().expression().expect("node must be valid"))
    }
}
impl Valid<UnboundedArrayDefinitionSyntax> {
    pub fn array_token(&self) -> SyntaxToken {
        self.inner().array_token().expect("node must be valid")
    }
    pub fn left_par_token(&self) -> SyntaxToken {
        self.inner().left_par_token().expect("node must be valid")
    }
    pub fn index_subtype_definition_list(&self) -> Valid<IndexSubtypeDefinitionListSyntax> {
        Valid::new_unchecked(
            self.inner()
                .index_subtype_definition_list()
                .expect("node must be valid"),
        )
    }
    pub fn right_par_token(&self) -> SyntaxToken {
        self.inner().right_par_token().expect("node must be valid")
    }
    pub fn of_token(&self) -> SyntaxToken {
        self.inner().of_token().expect("node must be valid")
    }
    pub fn subtype_indication(&self) -> Valid<SubtypeIndicationSyntax> {
        Valid::new_unchecked(
            self.inner()
                .subtype_indication()
                .expect("node must be valid"),
        )
    }
}
impl Valid<UnitDeclarationsSyntax> {
    pub fn units_token(&self) -> SyntaxToken {
        self.inner().units_token().expect("node must be valid")
    }
    pub fn primary_unit_declaration(&self) -> Valid<PrimaryUnitDeclarationSyntax> {
        Valid::new_unchecked(
            self.inner()
                .primary_unit_declaration()
                .expect("node must be valid"),
        )
    }
    pub fn secondary_unit_declarations(
        &self,
    ) -> impl Iterator<Item = Valid<SecondaryUnitDeclarationSyntax>> + use<'_> {
        self.inner()
            .secondary_unit_declarations()
            .map(Valid::new_unchecked)
    }
}
impl Valid<UpLevelSyntax> {
    pub fn circ_token(&self) -> SyntaxToken {
        self.inner().circ_token().expect("node must be valid")
    }
    pub fn dot_token(&self) -> SyntaxToken {
        self.inner().dot_token().expect("node must be valid")
    }
}
impl Valid<UseClauseSyntax> {
    pub fn use_token(&self) -> SyntaxToken {
        self.inner().use_token().expect("node must be valid")
    }
    pub fn name_list(&self) -> Valid<NameListSyntax> {
        Valid::new_unchecked(self.inner().name_list().expect("node must be valid"))
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<UseClauseContextItemSyntax> {
    pub fn use_clause(&self) -> Valid<UseClauseSyntax> {
        Valid::new_unchecked(self.inner().use_clause().expect("node must be valid"))
    }
}
impl Valid<UseClauseDeclarationSyntax> {
    pub fn use_clause(&self) -> Valid<UseClauseSyntax> {
        Valid::new_unchecked(self.inner().use_clause().expect("node must be valid"))
    }
}
#[derive(Debug, Clone)]
pub enum ValidVariableAssignmentStatement {
    SimpleVariableAssignment(Valid<SimpleVariableAssignmentSyntax>),
    ConditionalVariableAssignment(Valid<ConditionalVariableAssignmentSyntax>),
    SelectedVariableAssignment(Valid<SelectedVariableAssignmentSyntax>),
}
impl Valid<VariableAssignmentStatementSyntax> {
    pub fn alternative(&self) -> ValidVariableAssignmentStatement {
        match self.inner() {
            VariableAssignmentStatementSyntax::SimpleVariableAssignment(inner) => {
                ValidVariableAssignmentStatement::SimpleVariableAssignment(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
            VariableAssignmentStatementSyntax::ConditionalVariableAssignment(inner) => {
                ValidVariableAssignmentStatement::ConditionalVariableAssignment(
                    Valid::new_unchecked(inner.clone()),
                )
            }
            VariableAssignmentStatementSyntax::SelectedVariableAssignment(inner) => {
                ValidVariableAssignmentStatement::SelectedVariableAssignment(Valid::new_unchecked(
                    inner.clone(),
                ))
            }
        }
    }
}
impl Valid<VariableDeclarationSyntax> {
    pub fn shared_token(&self) -> Option<SyntaxToken> {
        self.inner().shared_token()
    }
    pub fn variable_token(&self) -> SyntaxToken {
        self.inner().variable_token().expect("node must be valid")
    }
    pub fn identifier_list(&self) -> Valid<IdentifierListSyntax> {
        Valid::new_unchecked(self.inner().identifier_list().expect("node must be valid"))
    }
    pub fn colon_token(&self) -> SyntaxToken {
        self.inner().colon_token().expect("node must be valid")
    }
    pub fn subtype_indication(&self) -> Valid<SubtypeIndicationSyntax> {
        Valid::new_unchecked(
            self.inner()
                .subtype_indication()
                .expect("node must be valid"),
        )
    }
    pub fn initial_value(&self) -> Option<Valid<InitialValueSyntax>> {
        self.inner().initial_value().map(Valid::new_unchecked)
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<VerificationUnitBindingSyntax> {
    pub fn verification_unit_binding_indication(
        &self,
    ) -> Valid<VerificationUnitBindingIndicationSyntax> {
        Valid::new_unchecked(
            self.inner()
                .verification_unit_binding_indication()
                .expect("node must be valid"),
        )
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
impl Valid<VerificationUnitBindingIndicationSyntax> {
    pub fn use_token(&self) -> SyntaxToken {
        self.inner().use_token().expect("node must be valid")
    }
    pub fn vunit_token(&self) -> SyntaxToken {
        self.inner().vunit_token().expect("node must be valid")
    }
    pub fn verification_unit_list(&self) -> Valid<VerificationUnitListSyntax> {
        Valid::new_unchecked(
            self.inner()
                .verification_unit_list()
                .expect("node must be valid"),
        )
    }
}
impl Valid<VerificationUnitListSyntax> {
    pub fn names(&self) -> impl Iterator<Item = Valid<NameSyntax>> + use<'_> {
        self.inner().names().map(Valid::new_unchecked)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.inner().comma_token()
    }
}
impl Valid<WaitStatementSyntax> {
    pub fn stmt_label(&self) -> Option<Valid<StmtLabelSyntax>> {
        self.inner().stmt_label().map(Valid::new_unchecked)
    }
    pub fn wait_token(&self) -> SyntaxToken {
        self.inner().wait_token().expect("node must be valid")
    }
    pub fn sensitivity_clause(&self) -> Option<Valid<SensitivityClauseSyntax>> {
        self.inner().sensitivity_clause().map(Valid::new_unchecked)
    }
    pub fn condition_clause(&self) -> Option<Valid<ConditionClauseSyntax>> {
        self.inner().condition_clause().map(Valid::new_unchecked)
    }
    pub fn timeout_clause(&self) -> Option<Valid<TimeoutClauseSyntax>> {
        self.inner().timeout_clause().map(Valid::new_unchecked)
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.inner().semi_colon_token().expect("node must be valid")
    }
}
#[derive(Debug, Clone)]
pub enum ValidWaveform {
    WaveformElements(Valid<WaveformElementsSyntax>),
    UnaffectedWaveform(Valid<UnaffectedWaveformSyntax>),
}
impl Valid<WaveformSyntax> {
    pub fn alternative(&self) -> ValidWaveform {
        match self.inner() {
            WaveformSyntax::WaveformElements(inner) => {
                ValidWaveform::WaveformElements(Valid::new_unchecked(inner.clone()))
            }
            WaveformSyntax::UnaffectedWaveform(inner) => {
                ValidWaveform::UnaffectedWaveform(Valid::new_unchecked(inner.clone()))
            }
        }
    }
}
impl Valid<WaveformElementSyntax> {
    pub fn expression(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().expression().expect("node must be valid"))
    }
    pub fn after_clause(&self) -> Option<Valid<AfterClauseSyntax>> {
        self.inner().after_clause().map(Valid::new_unchecked)
    }
}
impl Valid<WaveformElementsSyntax> {
    pub fn waveform_elements(
        &self,
    ) -> impl Iterator<Item = Valid<WaveformElementSyntax>> + use<'_> {
        self.inner().waveform_elements().map(Valid::new_unchecked)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.inner().comma_token()
    }
}
impl Valid<WhenClauseSyntax> {
    pub fn when_token(&self) -> SyntaxToken {
        self.inner().when_token().expect("node must be valid")
    }
    pub fn condition(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().condition().expect("node must be valid"))
    }
}
impl Valid<WhenExpressionSyntax> {
    pub fn expression(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().expression().expect("node must be valid"))
    }
    pub fn when_token(&self) -> SyntaxToken {
        self.inner().when_token().expect("node must be valid")
    }
    pub fn condition(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().condition().expect("node must be valid"))
    }
}
impl Valid<WhenWaveformSyntax> {
    pub fn waveform(&self) -> Valid<WaveformSyntax> {
        Valid::new_unchecked(self.inner().waveform().expect("node must be valid"))
    }
    pub fn when_token(&self) -> SyntaxToken {
        self.inner().when_token().expect("node must be valid")
    }
    pub fn condition(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().condition().expect("node must be valid"))
    }
}
impl Valid<WhileSchemeSyntax> {
    pub fn while_token(&self) -> SyntaxToken {
        self.inner().while_token().expect("node must be valid")
    }
    pub fn condition(&self) -> Valid<ExpressionSyntax> {
        Valid::new_unchecked(self.inner().condition().expect("node must be valid"))
    }
}
