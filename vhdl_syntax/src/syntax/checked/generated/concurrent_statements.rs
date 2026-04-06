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
pub struct CheckedDeclarationStatementSeparator(DeclarationStatementSeparatorSyntax);
impl CheckedNode for CheckedDeclarationStatementSeparator {
    type Syntax = DeclarationStatementSeparatorSyntax;
    fn cast_unchecked(syntax: DeclarationStatementSeparatorSyntax) -> Self {
        CheckedDeclarationStatementSeparator(syntax)
    }
    fn raw(&self) -> DeclarationStatementSeparatorSyntax {
        self.0.clone()
    }
}
impl CheckedDeclarationStatementSeparator {
    pub fn begin_token(&self) -> SyntaxToken {
        self.0.begin_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedSemiColonTerminatedGenericMapAspect(SemiColonTerminatedGenericMapAspectSyntax);
impl CheckedNode for CheckedSemiColonTerminatedGenericMapAspect {
    type Syntax = SemiColonTerminatedGenericMapAspectSyntax;
    fn cast_unchecked(syntax: SemiColonTerminatedGenericMapAspectSyntax) -> Self {
        CheckedSemiColonTerminatedGenericMapAspect(syntax)
    }
    fn raw(&self) -> SemiColonTerminatedGenericMapAspectSyntax {
        self.0.clone()
    }
}
impl CheckedSemiColonTerminatedGenericMapAspect {
    pub fn generic_map_aspect(&self) -> CheckedGenericMapAspect {
        CheckedGenericMapAspect::cast_unchecked(self.0.generic_map_aspect().unwrap())
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedSemiColonTerminatedPortMapAspect(SemiColonTerminatedPortMapAspectSyntax);
impl CheckedNode for CheckedSemiColonTerminatedPortMapAspect {
    type Syntax = SemiColonTerminatedPortMapAspectSyntax;
    fn cast_unchecked(syntax: SemiColonTerminatedPortMapAspectSyntax) -> Self {
        CheckedSemiColonTerminatedPortMapAspect(syntax)
    }
    fn raw(&self) -> SemiColonTerminatedPortMapAspectSyntax {
        self.0.clone()
    }
}
impl CheckedSemiColonTerminatedPortMapAspect {
    pub fn port_map_aspect(&self) -> CheckedPortMapAspect {
        CheckedPortMapAspect::cast_unchecked(self.0.port_map_aspect().unwrap())
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedBlockHeader(BlockHeaderSyntax);
impl CheckedNode for CheckedBlockHeader {
    type Syntax = BlockHeaderSyntax;
    fn cast_unchecked(syntax: BlockHeaderSyntax) -> Self {
        CheckedBlockHeader(syntax)
    }
    fn raw(&self) -> BlockHeaderSyntax {
        self.0.clone()
    }
}
impl CheckedBlockHeader {
    pub fn generic_clause(&self) -> Option<CheckedGenericClause> {
        self.0
            .generic_clause()
            .map(CheckedGenericClause::cast_unchecked)
    }
    pub fn semi_colon_terminated_generic_map_aspect(
        &self,
    ) -> Option<CheckedSemiColonTerminatedGenericMapAspect> {
        self.0
            .semi_colon_terminated_generic_map_aspect()
            .map(CheckedSemiColonTerminatedGenericMapAspect::cast_unchecked)
    }
    pub fn port_clause(&self) -> Option<CheckedPortClause> {
        self.0.port_clause().map(CheckedPortClause::cast_unchecked)
    }
    pub fn semi_colon_terminated_port_map_aspect(
        &self,
    ) -> Option<CheckedSemiColonTerminatedPortMapAspect> {
        self.0
            .semi_colon_terminated_port_map_aspect()
            .map(CheckedSemiColonTerminatedPortMapAspect::cast_unchecked)
    }
}
#[derive(Debug, Clone)]
pub struct CheckedBlockStatement(BlockStatementSyntax);
impl CheckedNode for CheckedBlockStatement {
    type Syntax = BlockStatementSyntax;
    fn cast_unchecked(syntax: BlockStatementSyntax) -> Self {
        CheckedBlockStatement(syntax)
    }
    fn raw(&self) -> BlockStatementSyntax {
        self.0.clone()
    }
}
impl CheckedBlockStatement {
    pub fn block_preamble(&self) -> CheckedBlockPreamble {
        CheckedBlockPreamble::cast_unchecked(self.0.block_preamble().unwrap())
    }
    pub fn block_header(&self) -> Option<CheckedBlockHeader> {
        self.0
            .block_header()
            .map(CheckedBlockHeader::cast_unchecked)
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
    pub fn block_epilogue(&self) -> CheckedBlockEpilogue {
        CheckedBlockEpilogue::cast_unchecked(self.0.block_epilogue().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedParenthesizedExpression(ParenthesizedExpressionSyntax);
impl CheckedNode for CheckedParenthesizedExpression {
    type Syntax = ParenthesizedExpressionSyntax;
    fn cast_unchecked(syntax: ParenthesizedExpressionSyntax) -> Self {
        CheckedParenthesizedExpression(syntax)
    }
    fn raw(&self) -> ParenthesizedExpressionSyntax {
        self.0.clone()
    }
}
impl CheckedParenthesizedExpression {
    pub fn left_par_token(&self) -> SyntaxToken {
        self.0.left_par_token().unwrap()
    }
    pub fn expression(&self) -> CheckedExpression {
        CheckedExpression::cast_unchecked(self.0.expression().unwrap())
    }
    pub fn right_par_token(&self) -> SyntaxToken {
        self.0.right_par_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedBlockPreamble(BlockPreambleSyntax);
impl CheckedNode for CheckedBlockPreamble {
    type Syntax = BlockPreambleSyntax;
    fn cast_unchecked(syntax: BlockPreambleSyntax) -> Self {
        CheckedBlockPreamble(syntax)
    }
    fn raw(&self) -> BlockPreambleSyntax {
        self.0.clone()
    }
}
impl CheckedBlockPreamble {
    pub fn label(&self) -> CheckedLabel {
        CheckedLabel::cast_unchecked(self.0.label().unwrap())
    }
    pub fn block_token(&self) -> SyntaxToken {
        self.0.block_token().unwrap()
    }
    pub fn condition(&self) -> Option<CheckedParenthesizedExpression> {
        self.0
            .condition()
            .map(CheckedParenthesizedExpression::cast_unchecked)
    }
    pub fn is_token(&self) -> Option<SyntaxToken> {
        self.0.is_token()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedBlockEpilogue(BlockEpilogueSyntax);
impl CheckedNode for CheckedBlockEpilogue {
    type Syntax = BlockEpilogueSyntax;
    fn cast_unchecked(syntax: BlockEpilogueSyntax) -> Self {
        CheckedBlockEpilogue(syntax)
    }
    fn raw(&self) -> BlockEpilogueSyntax {
        self.0.clone()
    }
}
impl CheckedBlockEpilogue {
    pub fn end_token(&self) -> SyntaxToken {
        self.0.end_token().unwrap()
    }
    pub fn block_token(&self) -> SyntaxToken {
        self.0.block_token().unwrap()
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0.identifier_token()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedCaseGenerateAlternative(CaseGenerateAlternativeSyntax);
impl CheckedNode for CheckedCaseGenerateAlternative {
    type Syntax = CaseGenerateAlternativeSyntax;
    fn cast_unchecked(syntax: CaseGenerateAlternativeSyntax) -> Self {
        CheckedCaseGenerateAlternative(syntax)
    }
    fn raw(&self) -> CaseGenerateAlternativeSyntax {
        self.0.clone()
    }
}
impl CheckedCaseGenerateAlternative {
    pub fn when_token(&self) -> SyntaxToken {
        self.0.when_token().unwrap()
    }
    pub fn label(&self) -> Option<CheckedLabel> {
        self.0.label().map(CheckedLabel::cast_unchecked)
    }
    pub fn choices(&self) -> Option<CheckedChoices> {
        self.0.choices().map(CheckedChoices::cast_unchecked)
    }
    pub fn right_arrow_token(&self) -> SyntaxToken {
        self.0.right_arrow_token().unwrap()
    }
    pub fn generate_statement_body(&self) -> Option<CheckedGenerateStatementBody> {
        self.0
            .generate_statement_body()
            .map(CheckedGenerateStatementBody::cast_unchecked)
    }
}
#[derive(Debug, Clone)]
pub struct CheckedCaseGenerateStatement(CaseGenerateStatementSyntax);
impl CheckedNode for CheckedCaseGenerateStatement {
    type Syntax = CaseGenerateStatementSyntax;
    fn cast_unchecked(syntax: CaseGenerateStatementSyntax) -> Self {
        CheckedCaseGenerateStatement(syntax)
    }
    fn raw(&self) -> CaseGenerateStatementSyntax {
        self.0.clone()
    }
}
impl CheckedCaseGenerateStatement {
    pub fn case_generate_statement_preamble(&self) -> CheckedCaseGenerateStatementPreamble {
        CheckedCaseGenerateStatementPreamble::cast_unchecked(
            self.0.case_generate_statement_preamble().unwrap(),
        )
    }
    pub fn case_generate_alternatives(
        &self,
    ) -> impl Iterator<Item = CheckedCaseGenerateAlternative> + use<'_> {
        self.0
            .case_generate_alternatives()
            .map(CheckedCaseGenerateAlternative::cast_unchecked)
    }
    pub fn case_generate_statement_epilogue(&self) -> CheckedCaseGenerateStatementEpilogue {
        CheckedCaseGenerateStatementEpilogue::cast_unchecked(
            self.0.case_generate_statement_epilogue().unwrap(),
        )
    }
}
#[derive(Debug, Clone)]
pub struct CheckedCaseGenerateStatementPreamble(CaseGenerateStatementPreambleSyntax);
impl CheckedNode for CheckedCaseGenerateStatementPreamble {
    type Syntax = CaseGenerateStatementPreambleSyntax;
    fn cast_unchecked(syntax: CaseGenerateStatementPreambleSyntax) -> Self {
        CheckedCaseGenerateStatementPreamble(syntax)
    }
    fn raw(&self) -> CaseGenerateStatementPreambleSyntax {
        self.0.clone()
    }
}
impl CheckedCaseGenerateStatementPreamble {
    pub fn label(&self) -> CheckedLabel {
        CheckedLabel::cast_unchecked(self.0.label().unwrap())
    }
    pub fn case_token(&self) -> SyntaxToken {
        self.0.case_token().unwrap()
    }
    pub fn expression(&self) -> CheckedExpression {
        CheckedExpression::cast_unchecked(self.0.expression().unwrap())
    }
    pub fn generate_token(&self) -> SyntaxToken {
        self.0.generate_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedCaseGenerateStatementEpilogue(CaseGenerateStatementEpilogueSyntax);
impl CheckedNode for CheckedCaseGenerateStatementEpilogue {
    type Syntax = CaseGenerateStatementEpilogueSyntax;
    fn cast_unchecked(syntax: CaseGenerateStatementEpilogueSyntax) -> Self {
        CheckedCaseGenerateStatementEpilogue(syntax)
    }
    fn raw(&self) -> CaseGenerateStatementEpilogueSyntax {
        self.0.clone()
    }
}
impl CheckedCaseGenerateStatementEpilogue {
    pub fn end_token(&self) -> SyntaxToken {
        self.0.end_token().unwrap()
    }
    pub fn generate_token(&self) -> SyntaxToken {
        self.0.generate_token().unwrap()
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0.identifier_token()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedComponentInstantiationStatement(ComponentInstantiationStatementSyntax);
impl CheckedNode for CheckedComponentInstantiationStatement {
    type Syntax = ComponentInstantiationStatementSyntax;
    fn cast_unchecked(syntax: ComponentInstantiationStatementSyntax) -> Self {
        CheckedComponentInstantiationStatement(syntax)
    }
    fn raw(&self) -> ComponentInstantiationStatementSyntax {
        self.0.clone()
    }
}
impl CheckedComponentInstantiationStatement {
    pub fn label(&self) -> CheckedLabel {
        CheckedLabel::cast_unchecked(self.0.label().unwrap())
    }
    pub fn instantiated_unit(&self) -> CheckedInstantiatedUnit {
        CheckedInstantiatedUnit::cast_unchecked(self.0.instantiated_unit().unwrap())
    }
    pub fn component_instantiation_items(&self) -> Option<CheckedComponentInstantiationItems> {
        self.0
            .component_instantiation_items()
            .map(CheckedComponentInstantiationItems::cast_unchecked)
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedComponentInstantiationItems(ComponentInstantiationItemsSyntax);
impl CheckedNode for CheckedComponentInstantiationItems {
    type Syntax = ComponentInstantiationItemsSyntax;
    fn cast_unchecked(syntax: ComponentInstantiationItemsSyntax) -> Self {
        CheckedComponentInstantiationItems(syntax)
    }
    fn raw(&self) -> ComponentInstantiationItemsSyntax {
        self.0.clone()
    }
}
impl CheckedComponentInstantiationItems {
    pub fn generic_map_aspect(&self) -> Option<CheckedGenericMapAspect> {
        self.0
            .generic_map_aspect()
            .map(CheckedGenericMapAspect::cast_unchecked)
    }
    pub fn port_map_aspect(&self) -> Option<CheckedPortMapAspect> {
        self.0
            .port_map_aspect()
            .map(CheckedPortMapAspect::cast_unchecked)
    }
}
#[derive(Debug, Clone)]
pub struct CheckedConcurrentProcedureCallOrComponentInstantiationStatement(
    ConcurrentProcedureCallOrComponentInstantiationStatementSyntax,
);
impl CheckedNode for CheckedConcurrentProcedureCallOrComponentInstantiationStatement {
    type Syntax = ConcurrentProcedureCallOrComponentInstantiationStatementSyntax;
    fn cast_unchecked(
        syntax: ConcurrentProcedureCallOrComponentInstantiationStatementSyntax,
    ) -> Self {
        CheckedConcurrentProcedureCallOrComponentInstantiationStatement(syntax)
    }
    fn raw(&self) -> ConcurrentProcedureCallOrComponentInstantiationStatementSyntax {
        self.0.clone()
    }
}
impl CheckedConcurrentProcedureCallOrComponentInstantiationStatement {
    pub fn label(&self) -> Option<CheckedLabel> {
        self.0.label().map(CheckedLabel::cast_unchecked)
    }
    pub fn postponed_token(&self) -> Option<SyntaxToken> {
        self.0.postponed_token()
    }
    pub fn name(&self) -> CheckedName {
        CheckedName::cast_unchecked(self.0.name().unwrap())
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedConcurrentAssertionStatement(ConcurrentAssertionStatementSyntax);
impl CheckedNode for CheckedConcurrentAssertionStatement {
    type Syntax = ConcurrentAssertionStatementSyntax;
    fn cast_unchecked(syntax: ConcurrentAssertionStatementSyntax) -> Self {
        CheckedConcurrentAssertionStatement(syntax)
    }
    fn raw(&self) -> ConcurrentAssertionStatementSyntax {
        self.0.clone()
    }
}
impl CheckedConcurrentAssertionStatement {
    pub fn label(&self) -> Option<CheckedLabel> {
        self.0.label().map(CheckedLabel::cast_unchecked)
    }
    pub fn postponed_token(&self) -> Option<SyntaxToken> {
        self.0.postponed_token()
    }
    pub fn assertion(&self) -> CheckedAssertion {
        CheckedAssertion::cast_unchecked(self.0.assertion().unwrap())
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedConcurrentSimpleSignalAssignment(ConcurrentSimpleSignalAssignmentSyntax);
impl CheckedNode for CheckedConcurrentSimpleSignalAssignment {
    type Syntax = ConcurrentSimpleSignalAssignmentSyntax;
    fn cast_unchecked(syntax: ConcurrentSimpleSignalAssignmentSyntax) -> Self {
        CheckedConcurrentSimpleSignalAssignment(syntax)
    }
    fn raw(&self) -> ConcurrentSimpleSignalAssignmentSyntax {
        self.0.clone()
    }
}
impl CheckedConcurrentSimpleSignalAssignment {
    pub fn label(&self) -> Option<CheckedLabel> {
        self.0.label().map(CheckedLabel::cast_unchecked)
    }
    pub fn postponed_token(&self) -> Option<SyntaxToken> {
        self.0.postponed_token()
    }
    pub fn target(&self) -> CheckedTarget {
        CheckedTarget::cast_unchecked(self.0.target().unwrap())
    }
    pub fn lte_token(&self) -> SyntaxToken {
        self.0.lte_token().unwrap()
    }
    pub fn guarded_token(&self) -> Option<SyntaxToken> {
        self.0.guarded_token()
    }
    pub fn delay_mechanism(&self) -> Option<CheckedDelayMechanism> {
        self.0
            .delay_mechanism()
            .map(CheckedDelayMechanism::cast_unchecked)
    }
    pub fn waveform(&self) -> CheckedWaveform {
        CheckedWaveform::cast_unchecked(self.0.waveform().unwrap())
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedConcurrentConditionalSignalAssignment(
    ConcurrentConditionalSignalAssignmentSyntax,
);
impl CheckedNode for CheckedConcurrentConditionalSignalAssignment {
    type Syntax = ConcurrentConditionalSignalAssignmentSyntax;
    fn cast_unchecked(syntax: ConcurrentConditionalSignalAssignmentSyntax) -> Self {
        CheckedConcurrentConditionalSignalAssignment(syntax)
    }
    fn raw(&self) -> ConcurrentConditionalSignalAssignmentSyntax {
        self.0.clone()
    }
}
impl CheckedConcurrentConditionalSignalAssignment {
    pub fn label(&self) -> Option<CheckedLabel> {
        self.0.label().map(CheckedLabel::cast_unchecked)
    }
    pub fn postponed_token(&self) -> Option<SyntaxToken> {
        self.0.postponed_token()
    }
    pub fn target(&self) -> CheckedTarget {
        CheckedTarget::cast_unchecked(self.0.target().unwrap())
    }
    pub fn lte_token(&self) -> SyntaxToken {
        self.0.lte_token().unwrap()
    }
    pub fn guarded_token(&self) -> Option<SyntaxToken> {
        self.0.guarded_token()
    }
    pub fn delay_mechanism(&self) -> Option<CheckedDelayMechanism> {
        self.0
            .delay_mechanism()
            .map(CheckedDelayMechanism::cast_unchecked)
    }
    pub fn conditional_waveforms(&self) -> CheckedConditionalWaveforms {
        CheckedConditionalWaveforms::cast_unchecked(self.0.conditional_waveforms().unwrap())
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedConcurrentSelectedSignalAssignment(ConcurrentSelectedSignalAssignmentSyntax);
impl CheckedNode for CheckedConcurrentSelectedSignalAssignment {
    type Syntax = ConcurrentSelectedSignalAssignmentSyntax;
    fn cast_unchecked(syntax: ConcurrentSelectedSignalAssignmentSyntax) -> Self {
        CheckedConcurrentSelectedSignalAssignment(syntax)
    }
    fn raw(&self) -> ConcurrentSelectedSignalAssignmentSyntax {
        self.0.clone()
    }
}
impl CheckedConcurrentSelectedSignalAssignment {
    pub fn concurrent_selected_signal_assignment_preamble(
        &self,
    ) -> CheckedConcurrentSelectedSignalAssignmentPreamble {
        CheckedConcurrentSelectedSignalAssignmentPreamble::cast_unchecked(
            self.0
                .concurrent_selected_signal_assignment_preamble()
                .unwrap(),
        )
    }
    pub fn target(&self) -> CheckedTarget {
        CheckedTarget::cast_unchecked(self.0.target().unwrap())
    }
    pub fn lte_token(&self) -> SyntaxToken {
        self.0.lte_token().unwrap()
    }
    pub fn guarded_token(&self) -> Option<SyntaxToken> {
        self.0.guarded_token()
    }
    pub fn delay_mechanism(&self) -> Option<CheckedDelayMechanism> {
        self.0
            .delay_mechanism()
            .map(CheckedDelayMechanism::cast_unchecked)
    }
    pub fn selected_waveforms(&self) -> Option<CheckedSelectedWaveforms> {
        self.0
            .selected_waveforms()
            .map(CheckedSelectedWaveforms::cast_unchecked)
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedConcurrentSelectedSignalAssignmentPreamble(
    ConcurrentSelectedSignalAssignmentPreambleSyntax,
);
impl CheckedNode for CheckedConcurrentSelectedSignalAssignmentPreamble {
    type Syntax = ConcurrentSelectedSignalAssignmentPreambleSyntax;
    fn cast_unchecked(syntax: ConcurrentSelectedSignalAssignmentPreambleSyntax) -> Self {
        CheckedConcurrentSelectedSignalAssignmentPreamble(syntax)
    }
    fn raw(&self) -> ConcurrentSelectedSignalAssignmentPreambleSyntax {
        self.0.clone()
    }
}
impl CheckedConcurrentSelectedSignalAssignmentPreamble {
    pub fn label(&self) -> Option<CheckedLabel> {
        self.0.label().map(CheckedLabel::cast_unchecked)
    }
    pub fn postponed_token(&self) -> Option<SyntaxToken> {
        self.0.postponed_token()
    }
    pub fn with_token(&self) -> SyntaxToken {
        self.0.with_token().unwrap()
    }
    pub fn expression(&self) -> CheckedExpression {
        CheckedExpression::cast_unchecked(self.0.expression().unwrap())
    }
    pub fn select_token(&self) -> SyntaxToken {
        self.0.select_token().unwrap()
    }
    pub fn que_token(&self) -> Option<SyntaxToken> {
        self.0.que_token()
    }
}
#[derive(Debug, Clone)]
pub enum CheckedConcurrentStatement {
    BlockStatement(CheckedBlockStatement),
    ProcessStatement(CheckedProcessStatement),
    ConcurrentAssertionStatement(CheckedConcurrentAssertionStatement),
    ComponentInstantiationStatement(CheckedComponentInstantiationStatement),
    ConcurrentSelectedSignalAssignment(CheckedConcurrentSelectedSignalAssignment),
    ConcurrentConditionalSignalAssignment(CheckedConcurrentConditionalSignalAssignment),
    ConcurrentSimpleSignalAssignment(CheckedConcurrentSimpleSignalAssignment),
    ConcurrentProcedureCallOrComponentInstantiationStatement(
        CheckedConcurrentProcedureCallOrComponentInstantiationStatement,
    ),
    ForGenerateStatement(CheckedForGenerateStatement),
    IfGenerateStatement(CheckedIfGenerateStatement),
    CaseGenerateStatement(CheckedCaseGenerateStatement),
    PslDirective(CheckedPslDirective),
}
impl CheckedNode for CheckedConcurrentStatement {
    type Syntax = ConcurrentStatementSyntax;
    fn cast_unchecked(syntax: ConcurrentStatementSyntax) -> Self {
        match syntax {
            ConcurrentStatementSyntax::BlockStatement(inner) => {
                CheckedConcurrentStatement::BlockStatement(CheckedBlockStatement::cast_unchecked(
                    inner,
                ))
            }
            ConcurrentStatementSyntax::ProcessStatement(inner) => {
                CheckedConcurrentStatement::ProcessStatement(
                    CheckedProcessStatement::cast_unchecked(inner),
                )
            }
            ConcurrentStatementSyntax::ConcurrentAssertionStatement(inner) => {
                CheckedConcurrentStatement::ConcurrentAssertionStatement(
                    CheckedConcurrentAssertionStatement::cast_unchecked(inner),
                )
            }
            ConcurrentStatementSyntax::ComponentInstantiationStatement(inner) => {
                CheckedConcurrentStatement::ComponentInstantiationStatement(
                    CheckedComponentInstantiationStatement::cast_unchecked(inner),
                )
            }
            ConcurrentStatementSyntax::ConcurrentSelectedSignalAssignment(inner) => {
                CheckedConcurrentStatement::ConcurrentSelectedSignalAssignment(
                    CheckedConcurrentSelectedSignalAssignment::cast_unchecked(inner),
                )
            }
            ConcurrentStatementSyntax::ConcurrentConditionalSignalAssignment(inner) => {
                CheckedConcurrentStatement::ConcurrentConditionalSignalAssignment(
                    CheckedConcurrentConditionalSignalAssignment::cast_unchecked(inner),
                )
            }
            ConcurrentStatementSyntax::ConcurrentSimpleSignalAssignment(inner) => {
                CheckedConcurrentStatement::ConcurrentSimpleSignalAssignment(
                    CheckedConcurrentSimpleSignalAssignment::cast_unchecked(inner),
                )
            }
            ConcurrentStatementSyntax::ConcurrentProcedureCallOrComponentInstantiationStatement(
                inner,
            ) => {
                CheckedConcurrentStatement::ConcurrentProcedureCallOrComponentInstantiationStatement(
                    CheckedConcurrentProcedureCallOrComponentInstantiationStatement::cast_unchecked(
                        inner,
                    ),
                )
            }
            ConcurrentStatementSyntax::ForGenerateStatement(inner) => {
                CheckedConcurrentStatement::ForGenerateStatement(
                    CheckedForGenerateStatement::cast_unchecked(inner),
                )
            }
            ConcurrentStatementSyntax::IfGenerateStatement(inner) => {
                CheckedConcurrentStatement::IfGenerateStatement(
                    CheckedIfGenerateStatement::cast_unchecked(inner),
                )
            }
            ConcurrentStatementSyntax::CaseGenerateStatement(inner) => {
                CheckedConcurrentStatement::CaseGenerateStatement(
                    CheckedCaseGenerateStatement::cast_unchecked(inner),
                )
            }
            ConcurrentStatementSyntax::PslDirective(inner) => {
                CheckedConcurrentStatement::PslDirective(CheckedPslDirective::cast_unchecked(inner))
            }
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self { CheckedConcurrentStatement :: BlockStatement (inner) => ConcurrentStatementSyntax :: BlockStatement (inner . raw ()) , CheckedConcurrentStatement :: ProcessStatement (inner) => ConcurrentStatementSyntax :: ProcessStatement (inner . raw ()) , CheckedConcurrentStatement :: ConcurrentAssertionStatement (inner) => ConcurrentStatementSyntax :: ConcurrentAssertionStatement (inner . raw ()) , CheckedConcurrentStatement :: ComponentInstantiationStatement (inner) => ConcurrentStatementSyntax :: ComponentInstantiationStatement (inner . raw ()) , CheckedConcurrentStatement :: ConcurrentSelectedSignalAssignment (inner) => ConcurrentStatementSyntax :: ConcurrentSelectedSignalAssignment (inner . raw ()) , CheckedConcurrentStatement :: ConcurrentConditionalSignalAssignment (inner) => ConcurrentStatementSyntax :: ConcurrentConditionalSignalAssignment (inner . raw ()) , CheckedConcurrentStatement :: ConcurrentSimpleSignalAssignment (inner) => ConcurrentStatementSyntax :: ConcurrentSimpleSignalAssignment (inner . raw ()) , CheckedConcurrentStatement :: ConcurrentProcedureCallOrComponentInstantiationStatement (inner) => ConcurrentStatementSyntax :: ConcurrentProcedureCallOrComponentInstantiationStatement (inner . raw ()) , CheckedConcurrentStatement :: ForGenerateStatement (inner) => ConcurrentStatementSyntax :: ForGenerateStatement (inner . raw ()) , CheckedConcurrentStatement :: IfGenerateStatement (inner) => ConcurrentStatementSyntax :: IfGenerateStatement (inner . raw ()) , CheckedConcurrentStatement :: CaseGenerateStatement (inner) => ConcurrentStatementSyntax :: CaseGenerateStatement (inner . raw ()) , CheckedConcurrentStatement :: PslDirective (inner) => ConcurrentStatementSyntax :: PslDirective (inner . raw ()) , }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedConcurrentStatements(ConcurrentStatementsSyntax);
impl CheckedNode for CheckedConcurrentStatements {
    type Syntax = ConcurrentStatementsSyntax;
    fn cast_unchecked(syntax: ConcurrentStatementsSyntax) -> Self {
        CheckedConcurrentStatements(syntax)
    }
    fn raw(&self) -> ConcurrentStatementsSyntax {
        self.0.clone()
    }
}
impl CheckedConcurrentStatements {
    pub fn concurrent_statements(
        &self,
    ) -> impl Iterator<Item = CheckedConcurrentStatement> + use<'_> {
        self.0
            .concurrent_statements()
            .map(CheckedConcurrentStatement::cast_unchecked)
    }
}
#[derive(Debug, Clone)]
pub struct CheckedForGenerateStatement(ForGenerateStatementSyntax);
impl CheckedNode for CheckedForGenerateStatement {
    type Syntax = ForGenerateStatementSyntax;
    fn cast_unchecked(syntax: ForGenerateStatementSyntax) -> Self {
        CheckedForGenerateStatement(syntax)
    }
    fn raw(&self) -> ForGenerateStatementSyntax {
        self.0.clone()
    }
}
impl CheckedForGenerateStatement {
    pub fn for_generate_statement_preamble(&self) -> CheckedForGenerateStatementPreamble {
        CheckedForGenerateStatementPreamble::cast_unchecked(
            self.0.for_generate_statement_preamble().unwrap(),
        )
    }
    pub fn generate_statement_body(&self) -> Option<CheckedGenerateStatementBody> {
        self.0
            .generate_statement_body()
            .map(CheckedGenerateStatementBody::cast_unchecked)
    }
    pub fn for_generate_statement_epilogue(&self) -> CheckedForGenerateStatementEpilogue {
        CheckedForGenerateStatementEpilogue::cast_unchecked(
            self.0.for_generate_statement_epilogue().unwrap(),
        )
    }
}
#[derive(Debug, Clone)]
pub struct CheckedForGenerateStatementPreamble(ForGenerateStatementPreambleSyntax);
impl CheckedNode for CheckedForGenerateStatementPreamble {
    type Syntax = ForGenerateStatementPreambleSyntax;
    fn cast_unchecked(syntax: ForGenerateStatementPreambleSyntax) -> Self {
        CheckedForGenerateStatementPreamble(syntax)
    }
    fn raw(&self) -> ForGenerateStatementPreambleSyntax {
        self.0.clone()
    }
}
impl CheckedForGenerateStatementPreamble {
    pub fn label(&self) -> Option<CheckedLabel> {
        self.0.label().map(CheckedLabel::cast_unchecked)
    }
    pub fn for_token(&self) -> SyntaxToken {
        self.0.for_token().unwrap()
    }
    pub fn parameter_specification(&self) -> CheckedParameterSpecification {
        CheckedParameterSpecification::cast_unchecked(self.0.parameter_specification().unwrap())
    }
    pub fn generate_token(&self) -> SyntaxToken {
        self.0.generate_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedForGenerateStatementEpilogue(ForGenerateStatementEpilogueSyntax);
impl CheckedNode for CheckedForGenerateStatementEpilogue {
    type Syntax = ForGenerateStatementEpilogueSyntax;
    fn cast_unchecked(syntax: ForGenerateStatementEpilogueSyntax) -> Self {
        CheckedForGenerateStatementEpilogue(syntax)
    }
    fn raw(&self) -> ForGenerateStatementEpilogueSyntax {
        self.0.clone()
    }
}
impl CheckedForGenerateStatementEpilogue {
    pub fn end_token(&self) -> SyntaxToken {
        self.0.end_token().unwrap()
    }
    pub fn generate_token(&self) -> SyntaxToken {
        self.0.generate_token().unwrap()
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0.identifier_token()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedGenerateStatementBody(GenerateStatementBodySyntax);
impl CheckedNode for CheckedGenerateStatementBody {
    type Syntax = GenerateStatementBodySyntax;
    fn cast_unchecked(syntax: GenerateStatementBodySyntax) -> Self {
        CheckedGenerateStatementBody(syntax)
    }
    fn raw(&self) -> GenerateStatementBodySyntax {
        self.0.clone()
    }
}
impl CheckedGenerateStatementBody {
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
    pub fn generate_statement_body_epilogue(&self) -> Option<CheckedGenerateStatementBodyEpilogue> {
        self.0
            .generate_statement_body_epilogue()
            .map(CheckedGenerateStatementBodyEpilogue::cast_unchecked)
    }
}
#[derive(Debug, Clone)]
pub struct CheckedGenerateStatementBodyEpilogue(GenerateStatementBodyEpilogueSyntax);
impl CheckedNode for CheckedGenerateStatementBodyEpilogue {
    type Syntax = GenerateStatementBodyEpilogueSyntax;
    fn cast_unchecked(syntax: GenerateStatementBodyEpilogueSyntax) -> Self {
        CheckedGenerateStatementBodyEpilogue(syntax)
    }
    fn raw(&self) -> GenerateStatementBodyEpilogueSyntax {
        self.0.clone()
    }
}
impl CheckedGenerateStatementBodyEpilogue {
    pub fn end_token(&self) -> SyntaxToken {
        self.0.end_token().unwrap()
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0.identifier_token()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedIfGenerateElsif(IfGenerateElsifSyntax);
impl CheckedNode for CheckedIfGenerateElsif {
    type Syntax = IfGenerateElsifSyntax;
    fn cast_unchecked(syntax: IfGenerateElsifSyntax) -> Self {
        CheckedIfGenerateElsif(syntax)
    }
    fn raw(&self) -> IfGenerateElsifSyntax {
        self.0.clone()
    }
}
impl CheckedIfGenerateElsif {
    pub fn elsif_token(&self) -> SyntaxToken {
        self.0.elsif_token().unwrap()
    }
    pub fn label(&self) -> Option<CheckedLabel> {
        self.0.label().map(CheckedLabel::cast_unchecked)
    }
    pub fn expression(&self) -> CheckedExpression {
        CheckedExpression::cast_unchecked(self.0.expression().unwrap())
    }
    pub fn generate_token(&self) -> SyntaxToken {
        self.0.generate_token().unwrap()
    }
    pub fn generate_statement_body(&self) -> Option<CheckedGenerateStatementBody> {
        self.0
            .generate_statement_body()
            .map(CheckedGenerateStatementBody::cast_unchecked)
    }
}
#[derive(Debug, Clone)]
pub struct CheckedIfGenerateElse(IfGenerateElseSyntax);
impl CheckedNode for CheckedIfGenerateElse {
    type Syntax = IfGenerateElseSyntax;
    fn cast_unchecked(syntax: IfGenerateElseSyntax) -> Self {
        CheckedIfGenerateElse(syntax)
    }
    fn raw(&self) -> IfGenerateElseSyntax {
        self.0.clone()
    }
}
impl CheckedIfGenerateElse {
    pub fn else_token(&self) -> SyntaxToken {
        self.0.else_token().unwrap()
    }
    pub fn label(&self) -> Option<CheckedLabel> {
        self.0.label().map(CheckedLabel::cast_unchecked)
    }
    pub fn generate_token(&self) -> SyntaxToken {
        self.0.generate_token().unwrap()
    }
    pub fn generate_statement_body(&self) -> Option<CheckedGenerateStatementBody> {
        self.0
            .generate_statement_body()
            .map(CheckedGenerateStatementBody::cast_unchecked)
    }
}
#[derive(Debug, Clone)]
pub struct CheckedIfGenerateStatement(IfGenerateStatementSyntax);
impl CheckedNode for CheckedIfGenerateStatement {
    type Syntax = IfGenerateStatementSyntax;
    fn cast_unchecked(syntax: IfGenerateStatementSyntax) -> Self {
        CheckedIfGenerateStatement(syntax)
    }
    fn raw(&self) -> IfGenerateStatementSyntax {
        self.0.clone()
    }
}
impl CheckedIfGenerateStatement {
    pub fn if_generate_statement_preamble(&self) -> CheckedIfGenerateStatementPreamble {
        CheckedIfGenerateStatementPreamble::cast_unchecked(
            self.0.if_generate_statement_preamble().unwrap(),
        )
    }
    pub fn generate_statement_body(&self) -> Option<CheckedGenerateStatementBody> {
        self.0
            .generate_statement_body()
            .map(CheckedGenerateStatementBody::cast_unchecked)
    }
    pub fn if_generate_elsifs(&self) -> impl Iterator<Item = CheckedIfGenerateElsif> + use<'_> {
        self.0
            .if_generate_elsifs()
            .map(CheckedIfGenerateElsif::cast_unchecked)
    }
    pub fn if_generate_else(&self) -> Option<CheckedIfGenerateElse> {
        self.0
            .if_generate_else()
            .map(CheckedIfGenerateElse::cast_unchecked)
    }
    pub fn if_generate_statement_epilogue(&self) -> CheckedIfGenerateStatementEpilogue {
        CheckedIfGenerateStatementEpilogue::cast_unchecked(
            self.0.if_generate_statement_epilogue().unwrap(),
        )
    }
}
#[derive(Debug, Clone)]
pub struct CheckedIfGenerateStatementPreamble(IfGenerateStatementPreambleSyntax);
impl CheckedNode for CheckedIfGenerateStatementPreamble {
    type Syntax = IfGenerateStatementPreambleSyntax;
    fn cast_unchecked(syntax: IfGenerateStatementPreambleSyntax) -> Self {
        CheckedIfGenerateStatementPreamble(syntax)
    }
    fn raw(&self) -> IfGenerateStatementPreambleSyntax {
        self.0.clone()
    }
}
impl CheckedIfGenerateStatementPreamble {
    pub fn label(&self) -> Option<CheckedLabel> {
        self.0.label().map(CheckedLabel::cast_unchecked)
    }
    pub fn if_token(&self) -> SyntaxToken {
        self.0.if_token().unwrap()
    }
    pub fn alternative_label(&self) -> Option<CheckedLabel> {
        self.0.alternative_label().map(CheckedLabel::cast_unchecked)
    }
    pub fn condition(&self) -> CheckedExpression {
        CheckedExpression::cast_unchecked(self.0.condition().unwrap())
    }
    pub fn generate_token(&self) -> SyntaxToken {
        self.0.generate_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedIfGenerateStatementEpilogue(IfGenerateStatementEpilogueSyntax);
impl CheckedNode for CheckedIfGenerateStatementEpilogue {
    type Syntax = IfGenerateStatementEpilogueSyntax;
    fn cast_unchecked(syntax: IfGenerateStatementEpilogueSyntax) -> Self {
        CheckedIfGenerateStatementEpilogue(syntax)
    }
    fn raw(&self) -> IfGenerateStatementEpilogueSyntax {
        self.0.clone()
    }
}
impl CheckedIfGenerateStatementEpilogue {
    pub fn end_token(&self) -> SyntaxToken {
        self.0.end_token().unwrap()
    }
    pub fn generate_token(&self) -> SyntaxToken {
        self.0.generate_token().unwrap()
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0.identifier_token()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub enum CheckedInstantiatedUnit {
    ComponentInstantiatedUnit(CheckedComponentInstantiatedUnit),
    EntityInstantiatedUnit(CheckedEntityInstantiatedUnit),
    ConfigurationInstantiatedUnit(CheckedConfigurationInstantiatedUnit),
}
impl CheckedNode for CheckedInstantiatedUnit {
    type Syntax = InstantiatedUnitSyntax;
    fn cast_unchecked(syntax: InstantiatedUnitSyntax) -> Self {
        match syntax {
            InstantiatedUnitSyntax::ComponentInstantiatedUnit(inner) => {
                CheckedInstantiatedUnit::ComponentInstantiatedUnit(
                    CheckedComponentInstantiatedUnit::cast_unchecked(inner),
                )
            }
            InstantiatedUnitSyntax::EntityInstantiatedUnit(inner) => {
                CheckedInstantiatedUnit::EntityInstantiatedUnit(
                    CheckedEntityInstantiatedUnit::cast_unchecked(inner),
                )
            }
            InstantiatedUnitSyntax::ConfigurationInstantiatedUnit(inner) => {
                CheckedInstantiatedUnit::ConfigurationInstantiatedUnit(
                    CheckedConfigurationInstantiatedUnit::cast_unchecked(inner),
                )
            }
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedInstantiatedUnit::ComponentInstantiatedUnit(inner) => {
                InstantiatedUnitSyntax::ComponentInstantiatedUnit(inner.raw())
            }
            CheckedInstantiatedUnit::EntityInstantiatedUnit(inner) => {
                InstantiatedUnitSyntax::EntityInstantiatedUnit(inner.raw())
            }
            CheckedInstantiatedUnit::ConfigurationInstantiatedUnit(inner) => {
                InstantiatedUnitSyntax::ConfigurationInstantiatedUnit(inner.raw())
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedComponentInstantiatedUnit(ComponentInstantiatedUnitSyntax);
impl CheckedNode for CheckedComponentInstantiatedUnit {
    type Syntax = ComponentInstantiatedUnitSyntax;
    fn cast_unchecked(syntax: ComponentInstantiatedUnitSyntax) -> Self {
        CheckedComponentInstantiatedUnit(syntax)
    }
    fn raw(&self) -> ComponentInstantiatedUnitSyntax {
        self.0.clone()
    }
}
impl CheckedComponentInstantiatedUnit {
    pub fn component_token(&self) -> Option<SyntaxToken> {
        self.0.component_token()
    }
    pub fn name(&self) -> CheckedName {
        CheckedName::cast_unchecked(self.0.name().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedEntityInstantiatedUnit(EntityInstantiatedUnitSyntax);
impl CheckedNode for CheckedEntityInstantiatedUnit {
    type Syntax = EntityInstantiatedUnitSyntax;
    fn cast_unchecked(syntax: EntityInstantiatedUnitSyntax) -> Self {
        CheckedEntityInstantiatedUnit(syntax)
    }
    fn raw(&self) -> EntityInstantiatedUnitSyntax {
        self.0.clone()
    }
}
impl CheckedEntityInstantiatedUnit {
    pub fn entity_token(&self) -> SyntaxToken {
        self.0.entity_token().unwrap()
    }
    pub fn name(&self) -> CheckedName {
        CheckedName::cast_unchecked(self.0.name().unwrap())
    }
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0.left_par_token()
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0.identifier_token()
    }
    pub fn right_par_token(&self) -> Option<SyntaxToken> {
        self.0.right_par_token()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedConfigurationInstantiatedUnit(ConfigurationInstantiatedUnitSyntax);
impl CheckedNode for CheckedConfigurationInstantiatedUnit {
    type Syntax = ConfigurationInstantiatedUnitSyntax;
    fn cast_unchecked(syntax: ConfigurationInstantiatedUnitSyntax) -> Self {
        CheckedConfigurationInstantiatedUnit(syntax)
    }
    fn raw(&self) -> ConfigurationInstantiatedUnitSyntax {
        self.0.clone()
    }
}
impl CheckedConfigurationInstantiatedUnit {
    pub fn configuration_token(&self) -> SyntaxToken {
        self.0.configuration_token().unwrap()
    }
    pub fn name(&self) -> CheckedName {
        CheckedName::cast_unchecked(self.0.name().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedAllSensitivityList(AllSensitivityListSyntax);
impl CheckedNode for CheckedAllSensitivityList {
    type Syntax = AllSensitivityListSyntax;
    fn cast_unchecked(syntax: AllSensitivityListSyntax) -> Self {
        CheckedAllSensitivityList(syntax)
    }
    fn raw(&self) -> AllSensitivityListSyntax {
        self.0.clone()
    }
}
impl CheckedAllSensitivityList {
    pub fn all_token(&self) -> SyntaxToken {
        self.0.all_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedSensitivityList(SensitivityListSyntax);
impl CheckedNode for CheckedSensitivityList {
    type Syntax = SensitivityListSyntax;
    fn cast_unchecked(syntax: SensitivityListSyntax) -> Self {
        CheckedSensitivityList(syntax)
    }
    fn raw(&self) -> SensitivityListSyntax {
        self.0.clone()
    }
}
impl CheckedSensitivityList {
    pub fn names(&self) -> impl Iterator<Item = CheckedName> + use<'_> {
        self.0.names().map(CheckedName::cast_unchecked)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.comma_token()
    }
}
#[derive(Debug, Clone)]
pub enum CheckedProcessSensitivityList {
    AllSensitivityList(CheckedAllSensitivityList),
    SensitivityList(CheckedSensitivityList),
}
impl CheckedNode for CheckedProcessSensitivityList {
    type Syntax = ProcessSensitivityListSyntax;
    fn cast_unchecked(syntax: ProcessSensitivityListSyntax) -> Self {
        match syntax {
            ProcessSensitivityListSyntax::AllSensitivityList(inner) => {
                CheckedProcessSensitivityList::AllSensitivityList(
                    CheckedAllSensitivityList::cast_unchecked(inner),
                )
            }
            ProcessSensitivityListSyntax::SensitivityList(inner) => {
                CheckedProcessSensitivityList::SensitivityList(
                    CheckedSensitivityList::cast_unchecked(inner),
                )
            }
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedProcessSensitivityList::AllSensitivityList(inner) => {
                ProcessSensitivityListSyntax::AllSensitivityList(inner.raw())
            }
            CheckedProcessSensitivityList::SensitivityList(inner) => {
                ProcessSensitivityListSyntax::SensitivityList(inner.raw())
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedProcessStatement(ProcessStatementSyntax);
impl CheckedNode for CheckedProcessStatement {
    type Syntax = ProcessStatementSyntax;
    fn cast_unchecked(syntax: ProcessStatementSyntax) -> Self {
        CheckedProcessStatement(syntax)
    }
    fn raw(&self) -> ProcessStatementSyntax {
        self.0.clone()
    }
}
impl CheckedProcessStatement {
    pub fn process_statement_preamble(&self) -> CheckedProcessStatementPreamble {
        CheckedProcessStatementPreamble::cast_unchecked(
            self.0.process_statement_preamble().unwrap(),
        )
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
    pub fn process_statement_epilogue(&self) -> CheckedProcessStatementEpilogue {
        CheckedProcessStatementEpilogue::cast_unchecked(
            self.0.process_statement_epilogue().unwrap(),
        )
    }
}
#[derive(Debug, Clone)]
pub struct CheckedParenthesizedProcessSensitivityList(ParenthesizedProcessSensitivityListSyntax);
impl CheckedNode for CheckedParenthesizedProcessSensitivityList {
    type Syntax = ParenthesizedProcessSensitivityListSyntax;
    fn cast_unchecked(syntax: ParenthesizedProcessSensitivityListSyntax) -> Self {
        CheckedParenthesizedProcessSensitivityList(syntax)
    }
    fn raw(&self) -> ParenthesizedProcessSensitivityListSyntax {
        self.0.clone()
    }
}
impl CheckedParenthesizedProcessSensitivityList {
    pub fn left_par_token(&self) -> SyntaxToken {
        self.0.left_par_token().unwrap()
    }
    pub fn process_sensitivity_list(&self) -> CheckedProcessSensitivityList {
        CheckedProcessSensitivityList::cast_unchecked(self.0.process_sensitivity_list().unwrap())
    }
    pub fn right_par_token(&self) -> SyntaxToken {
        self.0.right_par_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedProcessStatementPreamble(ProcessStatementPreambleSyntax);
impl CheckedNode for CheckedProcessStatementPreamble {
    type Syntax = ProcessStatementPreambleSyntax;
    fn cast_unchecked(syntax: ProcessStatementPreambleSyntax) -> Self {
        CheckedProcessStatementPreamble(syntax)
    }
    fn raw(&self) -> ProcessStatementPreambleSyntax {
        self.0.clone()
    }
}
impl CheckedProcessStatementPreamble {
    pub fn label(&self) -> CheckedLabel {
        CheckedLabel::cast_unchecked(self.0.label().unwrap())
    }
    pub fn postponed_token(&self) -> Option<SyntaxToken> {
        self.0.postponed_token()
    }
    pub fn process_token(&self) -> SyntaxToken {
        self.0.process_token().unwrap()
    }
    pub fn parenthesized_process_sensitivity_list(
        &self,
    ) -> Option<CheckedParenthesizedProcessSensitivityList> {
        self.0
            .parenthesized_process_sensitivity_list()
            .map(CheckedParenthesizedProcessSensitivityList::cast_unchecked)
    }
    pub fn is_token(&self) -> Option<SyntaxToken> {
        self.0.is_token()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedProcessStatementEpilogue(ProcessStatementEpilogueSyntax);
impl CheckedNode for CheckedProcessStatementEpilogue {
    type Syntax = ProcessStatementEpilogueSyntax;
    fn cast_unchecked(syntax: ProcessStatementEpilogueSyntax) -> Self {
        CheckedProcessStatementEpilogue(syntax)
    }
    fn raw(&self) -> ProcessStatementEpilogueSyntax {
        self.0.clone()
    }
}
impl CheckedProcessStatementEpilogue {
    pub fn end_token(&self) -> SyntaxToken {
        self.0.end_token().unwrap()
    }
    pub fn postponed_token(&self) -> Option<SyntaxToken> {
        self.0.postponed_token()
    }
    pub fn process_token(&self) -> SyntaxToken {
        self.0.process_token().unwrap()
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0.identifier_token()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
