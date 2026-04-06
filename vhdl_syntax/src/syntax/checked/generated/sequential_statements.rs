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
pub struct CheckedAssertion(AssertionSyntax);
impl CheckedNode for CheckedAssertion {
    type Syntax = AssertionSyntax;
    fn cast_unchecked(syntax: AssertionSyntax) -> Self {
        CheckedAssertion(syntax)
    }
    fn raw(&self) -> AssertionSyntax {
        self.0.clone()
    }
}
impl CheckedAssertion {
    pub fn assert_token(&self) -> SyntaxToken {
        self.0.assert_token().unwrap()
    }
    pub fn condition(&self) -> CheckedExpression {
        CheckedExpression::cast_unchecked(self.0.condition().unwrap())
    }
    pub fn report_token(&self) -> Option<SyntaxToken> {
        self.0.report_token()
    }
    pub fn report(&self) -> Option<CheckedExpression> {
        self.0.report().map(CheckedExpression::cast_unchecked)
    }
    pub fn severity_token(&self) -> Option<SyntaxToken> {
        self.0.severity_token()
    }
    pub fn severity(&self) -> Option<CheckedExpression> {
        self.0.severity().map(CheckedExpression::cast_unchecked)
    }
}
#[derive(Debug, Clone)]
pub struct CheckedAssertionStatement(AssertionStatementSyntax);
impl CheckedNode for CheckedAssertionStatement {
    type Syntax = AssertionStatementSyntax;
    fn cast_unchecked(syntax: AssertionStatementSyntax) -> Self {
        CheckedAssertionStatement(syntax)
    }
    fn raw(&self) -> AssertionStatementSyntax {
        self.0.clone()
    }
}
impl CheckedAssertionStatement {
    pub fn label_token(&self) -> Option<SyntaxToken> {
        self.0.label_token()
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0.colon_token()
    }
    pub fn assertion(&self) -> CheckedAssertion {
        CheckedAssertion::cast_unchecked(self.0.assertion().unwrap())
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedCaseStatementAlternative(CaseStatementAlternativeSyntax);
impl CheckedNode for CheckedCaseStatementAlternative {
    type Syntax = CaseStatementAlternativeSyntax;
    fn cast_unchecked(syntax: CaseStatementAlternativeSyntax) -> Self {
        CheckedCaseStatementAlternative(syntax)
    }
    fn raw(&self) -> CaseStatementAlternativeSyntax {
        self.0.clone()
    }
}
impl CheckedCaseStatementAlternative {
    pub fn case_statement_alternative_preamble(&self) -> CheckedCaseStatementAlternativePreamble {
        CheckedCaseStatementAlternativePreamble::cast_unchecked(
            self.0.case_statement_alternative_preamble().unwrap(),
        )
    }
    pub fn sequential_statements(&self) -> Option<CheckedSequentialStatements> {
        self.0
            .sequential_statements()
            .map(CheckedSequentialStatements::cast_unchecked)
    }
}
#[derive(Debug, Clone)]
pub struct CheckedCaseStatementAlternativePreamble(CaseStatementAlternativePreambleSyntax);
impl CheckedNode for CheckedCaseStatementAlternativePreamble {
    type Syntax = CaseStatementAlternativePreambleSyntax;
    fn cast_unchecked(syntax: CaseStatementAlternativePreambleSyntax) -> Self {
        CheckedCaseStatementAlternativePreamble(syntax)
    }
    fn raw(&self) -> CaseStatementAlternativePreambleSyntax {
        self.0.clone()
    }
}
impl CheckedCaseStatementAlternativePreamble {
    pub fn when_token(&self) -> SyntaxToken {
        self.0.when_token().unwrap()
    }
    pub fn choices(&self) -> Option<CheckedChoices> {
        self.0.choices().map(CheckedChoices::cast_unchecked)
    }
    pub fn right_arrow_token(&self) -> SyntaxToken {
        self.0.right_arrow_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedCaseStatement(CaseStatementSyntax);
impl CheckedNode for CheckedCaseStatement {
    type Syntax = CaseStatementSyntax;
    fn cast_unchecked(syntax: CaseStatementSyntax) -> Self {
        CheckedCaseStatement(syntax)
    }
    fn raw(&self) -> CaseStatementSyntax {
        self.0.clone()
    }
}
impl CheckedCaseStatement {
    pub fn case_statement_preamble(&self) -> CheckedCaseStatementPreamble {
        CheckedCaseStatementPreamble::cast_unchecked(self.0.case_statement_preamble().unwrap())
    }
    pub fn case_statement_alternatives(
        &self,
    ) -> impl Iterator<Item = CheckedCaseStatementAlternative> + use<'_> {
        self.0
            .case_statement_alternatives()
            .map(CheckedCaseStatementAlternative::cast_unchecked)
    }
    pub fn case_statement_epilogue(&self) -> CheckedCaseStatementEpilogue {
        CheckedCaseStatementEpilogue::cast_unchecked(self.0.case_statement_epilogue().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedCaseStatementPreamble(CaseStatementPreambleSyntax);
impl CheckedNode for CheckedCaseStatementPreamble {
    type Syntax = CaseStatementPreambleSyntax;
    fn cast_unchecked(syntax: CaseStatementPreambleSyntax) -> Self {
        CheckedCaseStatementPreamble(syntax)
    }
    fn raw(&self) -> CaseStatementPreambleSyntax {
        self.0.clone()
    }
}
impl CheckedCaseStatementPreamble {
    pub fn label(&self) -> CheckedLabel {
        CheckedLabel::cast_unchecked(self.0.label().unwrap())
    }
    pub fn case_token(&self) -> SyntaxToken {
        self.0.case_token().unwrap()
    }
    pub fn que_token(&self) -> Option<SyntaxToken> {
        self.0.que_token()
    }
    pub fn expression(&self) -> Option<CheckedExpression> {
        self.0.expression().map(CheckedExpression::cast_unchecked)
    }
    pub fn is_token(&self) -> SyntaxToken {
        self.0.is_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedCaseStatementEpilogue(CaseStatementEpilogueSyntax);
impl CheckedNode for CheckedCaseStatementEpilogue {
    type Syntax = CaseStatementEpilogueSyntax;
    fn cast_unchecked(syntax: CaseStatementEpilogueSyntax) -> Self {
        CheckedCaseStatementEpilogue(syntax)
    }
    fn raw(&self) -> CaseStatementEpilogueSyntax {
        self.0.clone()
    }
}
impl CheckedCaseStatementEpilogue {
    pub fn end_token(&self) -> SyntaxToken {
        self.0.end_token().unwrap()
    }
    pub fn case_token(&self) -> SyntaxToken {
        self.0.case_token().unwrap()
    }
    pub fn que_token(&self) -> Option<SyntaxToken> {
        self.0.que_token()
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0.identifier_token()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedConditionClause(ConditionClauseSyntax);
impl CheckedNode for CheckedConditionClause {
    type Syntax = ConditionClauseSyntax;
    fn cast_unchecked(syntax: ConditionClauseSyntax) -> Self {
        CheckedConditionClause(syntax)
    }
    fn raw(&self) -> ConditionClauseSyntax {
        self.0.clone()
    }
}
impl CheckedConditionClause {
    pub fn until_token(&self) -> SyntaxToken {
        self.0.until_token().unwrap()
    }
    pub fn expression(&self) -> CheckedExpression {
        CheckedExpression::cast_unchecked(self.0.expression().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedConditionalElseWhenExpression(ConditionalElseWhenExpressionSyntax);
impl CheckedNode for CheckedConditionalElseWhenExpression {
    type Syntax = ConditionalElseWhenExpressionSyntax;
    fn cast_unchecked(syntax: ConditionalElseWhenExpressionSyntax) -> Self {
        CheckedConditionalElseWhenExpression(syntax)
    }
    fn raw(&self) -> ConditionalElseWhenExpressionSyntax {
        self.0.clone()
    }
}
impl CheckedConditionalElseWhenExpression {
    pub fn else_token(&self) -> SyntaxToken {
        self.0.else_token().unwrap()
    }
    pub fn expression(&self) -> CheckedExpression {
        CheckedExpression::cast_unchecked(self.0.expression().unwrap())
    }
    pub fn when_token(&self) -> SyntaxToken {
        self.0.when_token().unwrap()
    }
    pub fn condition(&self) -> CheckedExpression {
        CheckedExpression::cast_unchecked(self.0.condition().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedConditionalElseItem(ConditionalElseItemSyntax);
impl CheckedNode for CheckedConditionalElseItem {
    type Syntax = ConditionalElseItemSyntax;
    fn cast_unchecked(syntax: ConditionalElseItemSyntax) -> Self {
        CheckedConditionalElseItem(syntax)
    }
    fn raw(&self) -> ConditionalElseItemSyntax {
        self.0.clone()
    }
}
impl CheckedConditionalElseItem {
    pub fn else_token(&self) -> SyntaxToken {
        self.0.else_token().unwrap()
    }
    pub fn expression(&self) -> CheckedExpression {
        CheckedExpression::cast_unchecked(self.0.expression().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedConditionalExpressions(ConditionalExpressionsSyntax);
impl CheckedNode for CheckedConditionalExpressions {
    type Syntax = ConditionalExpressionsSyntax;
    fn cast_unchecked(syntax: ConditionalExpressionsSyntax) -> Self {
        CheckedConditionalExpressions(syntax)
    }
    fn raw(&self) -> ConditionalExpressionsSyntax {
        self.0.clone()
    }
}
impl CheckedConditionalExpressions {
    pub fn conditional_expression(&self) -> CheckedConditionalExpression {
        CheckedConditionalExpression::cast_unchecked(self.0.conditional_expression().unwrap())
    }
    pub fn conditional_else_when_expressions(
        &self,
    ) -> impl Iterator<Item = CheckedConditionalElseWhenExpression> + use<'_> {
        self.0
            .conditional_else_when_expressions()
            .map(CheckedConditionalElseWhenExpression::cast_unchecked)
    }
    pub fn conditional_else_item(&self) -> Option<CheckedConditionalElseItem> {
        self.0
            .conditional_else_item()
            .map(CheckedConditionalElseItem::cast_unchecked)
    }
}
#[derive(Debug, Clone)]
pub struct CheckedConditionalExpression(ConditionalExpressionSyntax);
impl CheckedNode for CheckedConditionalExpression {
    type Syntax = ConditionalExpressionSyntax;
    fn cast_unchecked(syntax: ConditionalExpressionSyntax) -> Self {
        CheckedConditionalExpression(syntax)
    }
    fn raw(&self) -> ConditionalExpressionSyntax {
        self.0.clone()
    }
}
impl CheckedConditionalExpression {
    pub fn expression(&self) -> CheckedExpression {
        CheckedExpression::cast_unchecked(self.0.expression().unwrap())
    }
    pub fn when_token(&self) -> SyntaxToken {
        self.0.when_token().unwrap()
    }
    pub fn condition(&self) -> CheckedExpression {
        CheckedExpression::cast_unchecked(self.0.condition().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedConditionalForceAssignment(ConditionalForceAssignmentSyntax);
impl CheckedNode for CheckedConditionalForceAssignment {
    type Syntax = ConditionalForceAssignmentSyntax;
    fn cast_unchecked(syntax: ConditionalForceAssignmentSyntax) -> Self {
        CheckedConditionalForceAssignment(syntax)
    }
    fn raw(&self) -> ConditionalForceAssignmentSyntax {
        self.0.clone()
    }
}
impl CheckedConditionalForceAssignment {
    pub fn target(&self) -> CheckedTarget {
        CheckedTarget::cast_unchecked(self.0.target().unwrap())
    }
    pub fn lte_token(&self) -> SyntaxToken {
        self.0.lte_token().unwrap()
    }
    pub fn force_token(&self) -> SyntaxToken {
        self.0.force_token().unwrap()
    }
    pub fn force_mode(&self) -> Option<ForceModeSyntax> {
        self.0.force_mode()
    }
    pub fn conditional_expressions(&self) -> CheckedConditionalExpressions {
        CheckedConditionalExpressions::cast_unchecked(self.0.conditional_expressions().unwrap())
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub enum CheckedConditionalSignalAssignment {
    ConditionalWaveformAssignment(CheckedConditionalWaveformAssignment),
    ConditionalForceAssignment(CheckedConditionalForceAssignment),
}
impl CheckedNode for CheckedConditionalSignalAssignment {
    type Syntax = ConditionalSignalAssignmentSyntax;
    fn cast_unchecked(syntax: ConditionalSignalAssignmentSyntax) -> Self {
        match syntax {
            ConditionalSignalAssignmentSyntax::ConditionalWaveformAssignment(inner) => {
                CheckedConditionalSignalAssignment::ConditionalWaveformAssignment(
                    CheckedConditionalWaveformAssignment::cast_unchecked(inner),
                )
            }
            ConditionalSignalAssignmentSyntax::ConditionalForceAssignment(inner) => {
                CheckedConditionalSignalAssignment::ConditionalForceAssignment(
                    CheckedConditionalForceAssignment::cast_unchecked(inner),
                )
            }
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedConditionalSignalAssignment::ConditionalWaveformAssignment(inner) => {
                ConditionalSignalAssignmentSyntax::ConditionalWaveformAssignment(inner.raw())
            }
            CheckedConditionalSignalAssignment::ConditionalForceAssignment(inner) => {
                ConditionalSignalAssignmentSyntax::ConditionalForceAssignment(inner.raw())
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedConditionalVariableAssignment(ConditionalVariableAssignmentSyntax);
impl CheckedNode for CheckedConditionalVariableAssignment {
    type Syntax = ConditionalVariableAssignmentSyntax;
    fn cast_unchecked(syntax: ConditionalVariableAssignmentSyntax) -> Self {
        CheckedConditionalVariableAssignment(syntax)
    }
    fn raw(&self) -> ConditionalVariableAssignmentSyntax {
        self.0.clone()
    }
}
impl CheckedConditionalVariableAssignment {
    pub fn target(&self) -> CheckedTarget {
        CheckedTarget::cast_unchecked(self.0.target().unwrap())
    }
    pub fn colon_eq_token(&self) -> SyntaxToken {
        self.0.colon_eq_token().unwrap()
    }
    pub fn conditional_expressions(&self) -> CheckedConditionalExpressions {
        CheckedConditionalExpressions::cast_unchecked(self.0.conditional_expressions().unwrap())
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedConditionalWaveformAssignment(ConditionalWaveformAssignmentSyntax);
impl CheckedNode for CheckedConditionalWaveformAssignment {
    type Syntax = ConditionalWaveformAssignmentSyntax;
    fn cast_unchecked(syntax: ConditionalWaveformAssignmentSyntax) -> Self {
        CheckedConditionalWaveformAssignment(syntax)
    }
    fn raw(&self) -> ConditionalWaveformAssignmentSyntax {
        self.0.clone()
    }
}
impl CheckedConditionalWaveformAssignment {
    pub fn target(&self) -> CheckedTarget {
        CheckedTarget::cast_unchecked(self.0.target().unwrap())
    }
    pub fn lte_token(&self) -> SyntaxToken {
        self.0.lte_token().unwrap()
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
pub struct CheckedConditionalWaveformElseWhenExpression(
    ConditionalWaveformElseWhenExpressionSyntax,
);
impl CheckedNode for CheckedConditionalWaveformElseWhenExpression {
    type Syntax = ConditionalWaveformElseWhenExpressionSyntax;
    fn cast_unchecked(syntax: ConditionalWaveformElseWhenExpressionSyntax) -> Self {
        CheckedConditionalWaveformElseWhenExpression(syntax)
    }
    fn raw(&self) -> ConditionalWaveformElseWhenExpressionSyntax {
        self.0.clone()
    }
}
impl CheckedConditionalWaveformElseWhenExpression {
    pub fn else_token(&self) -> SyntaxToken {
        self.0.else_token().unwrap()
    }
    pub fn waveform(&self) -> CheckedWaveform {
        CheckedWaveform::cast_unchecked(self.0.waveform().unwrap())
    }
    pub fn when_token(&self) -> SyntaxToken {
        self.0.when_token().unwrap()
    }
    pub fn condition(&self) -> CheckedExpression {
        CheckedExpression::cast_unchecked(self.0.condition().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedConditionalWaveformElseItem(ConditionalWaveformElseItemSyntax);
impl CheckedNode for CheckedConditionalWaveformElseItem {
    type Syntax = ConditionalWaveformElseItemSyntax;
    fn cast_unchecked(syntax: ConditionalWaveformElseItemSyntax) -> Self {
        CheckedConditionalWaveformElseItem(syntax)
    }
    fn raw(&self) -> ConditionalWaveformElseItemSyntax {
        self.0.clone()
    }
}
impl CheckedConditionalWaveformElseItem {
    pub fn else_token(&self) -> SyntaxToken {
        self.0.else_token().unwrap()
    }
    pub fn waveform(&self) -> CheckedWaveform {
        CheckedWaveform::cast_unchecked(self.0.waveform().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedConditionalWaveforms(ConditionalWaveformsSyntax);
impl CheckedNode for CheckedConditionalWaveforms {
    type Syntax = ConditionalWaveformsSyntax;
    fn cast_unchecked(syntax: ConditionalWaveformsSyntax) -> Self {
        CheckedConditionalWaveforms(syntax)
    }
    fn raw(&self) -> ConditionalWaveformsSyntax {
        self.0.clone()
    }
}
impl CheckedConditionalWaveforms {
    pub fn conditional_waveform(&self) -> CheckedConditionalWaveform {
        CheckedConditionalWaveform::cast_unchecked(self.0.conditional_waveform().unwrap())
    }
    pub fn conditional_waveform_else_when_expressions(
        &self,
    ) -> impl Iterator<Item = CheckedConditionalWaveformElseWhenExpression> + use<'_> {
        self.0
            .conditional_waveform_else_when_expressions()
            .map(CheckedConditionalWaveformElseWhenExpression::cast_unchecked)
    }
    pub fn conditional_waveform_else_item(&self) -> Option<CheckedConditionalWaveformElseItem> {
        self.0
            .conditional_waveform_else_item()
            .map(CheckedConditionalWaveformElseItem::cast_unchecked)
    }
}
#[derive(Debug, Clone)]
pub struct CheckedConditionalWaveform(ConditionalWaveformSyntax);
impl CheckedNode for CheckedConditionalWaveform {
    type Syntax = ConditionalWaveformSyntax;
    fn cast_unchecked(syntax: ConditionalWaveformSyntax) -> Self {
        CheckedConditionalWaveform(syntax)
    }
    fn raw(&self) -> ConditionalWaveformSyntax {
        self.0.clone()
    }
}
impl CheckedConditionalWaveform {
    pub fn waveform(&self) -> CheckedWaveform {
        CheckedWaveform::cast_unchecked(self.0.waveform().unwrap())
    }
    pub fn when_token(&self) -> SyntaxToken {
        self.0.when_token().unwrap()
    }
    pub fn expression(&self) -> CheckedExpression {
        CheckedExpression::cast_unchecked(self.0.expression().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedTransportDelayMechanism(TransportDelayMechanismSyntax);
impl CheckedNode for CheckedTransportDelayMechanism {
    type Syntax = TransportDelayMechanismSyntax;
    fn cast_unchecked(syntax: TransportDelayMechanismSyntax) -> Self {
        CheckedTransportDelayMechanism(syntax)
    }
    fn raw(&self) -> TransportDelayMechanismSyntax {
        self.0.clone()
    }
}
impl CheckedTransportDelayMechanism {
    pub fn transport_token(&self) -> SyntaxToken {
        self.0.transport_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedInertialDelayMechanism(InertialDelayMechanismSyntax);
impl CheckedNode for CheckedInertialDelayMechanism {
    type Syntax = InertialDelayMechanismSyntax;
    fn cast_unchecked(syntax: InertialDelayMechanismSyntax) -> Self {
        CheckedInertialDelayMechanism(syntax)
    }
    fn raw(&self) -> InertialDelayMechanismSyntax {
        self.0.clone()
    }
}
impl CheckedInertialDelayMechanism {
    pub fn reject_token(&self) -> Option<SyntaxToken> {
        self.0.reject_token()
    }
    pub fn expression(&self) -> Option<CheckedExpression> {
        self.0.expression().map(CheckedExpression::cast_unchecked)
    }
    pub fn inertial_token(&self) -> SyntaxToken {
        self.0.inertial_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub enum CheckedDelayMechanism {
    TransportDelayMechanism(CheckedTransportDelayMechanism),
    InertialDelayMechanism(CheckedInertialDelayMechanism),
}
impl CheckedNode for CheckedDelayMechanism {
    type Syntax = DelayMechanismSyntax;
    fn cast_unchecked(syntax: DelayMechanismSyntax) -> Self {
        match syntax {
            DelayMechanismSyntax::TransportDelayMechanism(inner) => {
                CheckedDelayMechanism::TransportDelayMechanism(
                    CheckedTransportDelayMechanism::cast_unchecked(inner),
                )
            }
            DelayMechanismSyntax::InertialDelayMechanism(inner) => {
                CheckedDelayMechanism::InertialDelayMechanism(
                    CheckedInertialDelayMechanism::cast_unchecked(inner),
                )
            }
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedDelayMechanism::TransportDelayMechanism(inner) => {
                DelayMechanismSyntax::TransportDelayMechanism(inner.raw())
            }
            CheckedDelayMechanism::InertialDelayMechanism(inner) => {
                DelayMechanismSyntax::InertialDelayMechanism(inner.raw())
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedExitStatement(ExitStatementSyntax);
impl CheckedNode for CheckedExitStatement {
    type Syntax = ExitStatementSyntax;
    fn cast_unchecked(syntax: ExitStatementSyntax) -> Self {
        CheckedExitStatement(syntax)
    }
    fn raw(&self) -> ExitStatementSyntax {
        self.0.clone()
    }
}
impl CheckedExitStatement {
    pub fn label_token(&self) -> Option<SyntaxToken> {
        self.0.label_token()
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0.colon_token()
    }
    pub fn exit_token(&self) -> SyntaxToken {
        self.0.exit_token().unwrap()
    }
    pub fn loop_label_token(&self) -> Option<SyntaxToken> {
        self.0.loop_label_token()
    }
    pub fn when_token(&self) -> Option<SyntaxToken> {
        self.0.when_token()
    }
    pub fn expression(&self) -> Option<CheckedExpression> {
        self.0.expression().map(CheckedExpression::cast_unchecked)
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedIfStatementElsif(IfStatementElsifSyntax);
impl CheckedNode for CheckedIfStatementElsif {
    type Syntax = IfStatementElsifSyntax;
    fn cast_unchecked(syntax: IfStatementElsifSyntax) -> Self {
        CheckedIfStatementElsif(syntax)
    }
    fn raw(&self) -> IfStatementElsifSyntax {
        self.0.clone()
    }
}
impl CheckedIfStatementElsif {
    pub fn elsif_token(&self) -> SyntaxToken {
        self.0.elsif_token().unwrap()
    }
    pub fn expression(&self) -> CheckedExpression {
        CheckedExpression::cast_unchecked(self.0.expression().unwrap())
    }
    pub fn then_token(&self) -> SyntaxToken {
        self.0.then_token().unwrap()
    }
    pub fn sequential_statements(&self) -> Option<CheckedSequentialStatements> {
        self.0
            .sequential_statements()
            .map(CheckedSequentialStatements::cast_unchecked)
    }
}
#[derive(Debug, Clone)]
pub struct CheckedIfStatementElse(IfStatementElseSyntax);
impl CheckedNode for CheckedIfStatementElse {
    type Syntax = IfStatementElseSyntax;
    fn cast_unchecked(syntax: IfStatementElseSyntax) -> Self {
        CheckedIfStatementElse(syntax)
    }
    fn raw(&self) -> IfStatementElseSyntax {
        self.0.clone()
    }
}
impl CheckedIfStatementElse {
    pub fn else_token(&self) -> SyntaxToken {
        self.0.else_token().unwrap()
    }
    pub fn sequential_statements(&self) -> Option<CheckedSequentialStatements> {
        self.0
            .sequential_statements()
            .map(CheckedSequentialStatements::cast_unchecked)
    }
}
#[derive(Debug, Clone)]
pub struct CheckedIfStatement(IfStatementSyntax);
impl CheckedNode for CheckedIfStatement {
    type Syntax = IfStatementSyntax;
    fn cast_unchecked(syntax: IfStatementSyntax) -> Self {
        CheckedIfStatement(syntax)
    }
    fn raw(&self) -> IfStatementSyntax {
        self.0.clone()
    }
}
impl CheckedIfStatement {
    pub fn if_statement_preamble(&self) -> CheckedIfStatementPreamble {
        CheckedIfStatementPreamble::cast_unchecked(self.0.if_statement_preamble().unwrap())
    }
    pub fn sequential_statements(&self) -> Option<CheckedSequentialStatements> {
        self.0
            .sequential_statements()
            .map(CheckedSequentialStatements::cast_unchecked)
    }
    pub fn if_statement_elsifs(&self) -> impl Iterator<Item = CheckedIfStatementElsif> + use<'_> {
        self.0
            .if_statement_elsifs()
            .map(CheckedIfStatementElsif::cast_unchecked)
    }
    pub fn if_statement_else(&self) -> Option<CheckedIfStatementElse> {
        self.0
            .if_statement_else()
            .map(CheckedIfStatementElse::cast_unchecked)
    }
    pub fn if_statement_epilogue(&self) -> CheckedIfStatementEpilogue {
        CheckedIfStatementEpilogue::cast_unchecked(self.0.if_statement_epilogue().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedIfStatementPreamble(IfStatementPreambleSyntax);
impl CheckedNode for CheckedIfStatementPreamble {
    type Syntax = IfStatementPreambleSyntax;
    fn cast_unchecked(syntax: IfStatementPreambleSyntax) -> Self {
        CheckedIfStatementPreamble(syntax)
    }
    fn raw(&self) -> IfStatementPreambleSyntax {
        self.0.clone()
    }
}
impl CheckedIfStatementPreamble {
    pub fn if_label_token(&self) -> Option<SyntaxToken> {
        self.0.if_label_token()
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0.colon_token()
    }
    pub fn if_token(&self) -> SyntaxToken {
        self.0.if_token().unwrap()
    }
    pub fn expression(&self) -> CheckedExpression {
        CheckedExpression::cast_unchecked(self.0.expression().unwrap())
    }
    pub fn then_token(&self) -> SyntaxToken {
        self.0.then_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedIfStatementEpilogue(IfStatementEpilogueSyntax);
impl CheckedNode for CheckedIfStatementEpilogue {
    type Syntax = IfStatementEpilogueSyntax;
    fn cast_unchecked(syntax: IfStatementEpilogueSyntax) -> Self {
        CheckedIfStatementEpilogue(syntax)
    }
    fn raw(&self) -> IfStatementEpilogueSyntax {
        self.0.clone()
    }
}
impl CheckedIfStatementEpilogue {
    pub fn end_token(&self) -> SyntaxToken {
        self.0.end_token().unwrap()
    }
    pub fn if_token(&self) -> SyntaxToken {
        self.0.if_token().unwrap()
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0.identifier_token()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedWhileIterationScheme(WhileIterationSchemeSyntax);
impl CheckedNode for CheckedWhileIterationScheme {
    type Syntax = WhileIterationSchemeSyntax;
    fn cast_unchecked(syntax: WhileIterationSchemeSyntax) -> Self {
        CheckedWhileIterationScheme(syntax)
    }
    fn raw(&self) -> WhileIterationSchemeSyntax {
        self.0.clone()
    }
}
impl CheckedWhileIterationScheme {
    pub fn while_token(&self) -> SyntaxToken {
        self.0.while_token().unwrap()
    }
    pub fn expression(&self) -> CheckedExpression {
        CheckedExpression::cast_unchecked(self.0.expression().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedForIterationScheme(ForIterationSchemeSyntax);
impl CheckedNode for CheckedForIterationScheme {
    type Syntax = ForIterationSchemeSyntax;
    fn cast_unchecked(syntax: ForIterationSchemeSyntax) -> Self {
        CheckedForIterationScheme(syntax)
    }
    fn raw(&self) -> ForIterationSchemeSyntax {
        self.0.clone()
    }
}
impl CheckedForIterationScheme {
    pub fn for_token(&self) -> SyntaxToken {
        self.0.for_token().unwrap()
    }
    pub fn parameter_specification(&self) -> CheckedParameterSpecification {
        CheckedParameterSpecification::cast_unchecked(self.0.parameter_specification().unwrap())
    }
}
#[derive(Debug, Clone)]
pub enum CheckedIterationScheme {
    WhileIterationScheme(CheckedWhileIterationScheme),
    ForIterationScheme(CheckedForIterationScheme),
}
impl CheckedNode for CheckedIterationScheme {
    type Syntax = IterationSchemeSyntax;
    fn cast_unchecked(syntax: IterationSchemeSyntax) -> Self {
        match syntax {
            IterationSchemeSyntax::WhileIterationScheme(inner) => {
                CheckedIterationScheme::WhileIterationScheme(
                    CheckedWhileIterationScheme::cast_unchecked(inner),
                )
            }
            IterationSchemeSyntax::ForIterationScheme(inner) => {
                CheckedIterationScheme::ForIterationScheme(
                    CheckedForIterationScheme::cast_unchecked(inner),
                )
            }
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedIterationScheme::WhileIterationScheme(inner) => {
                IterationSchemeSyntax::WhileIterationScheme(inner.raw())
            }
            CheckedIterationScheme::ForIterationScheme(inner) => {
                IterationSchemeSyntax::ForIterationScheme(inner.raw())
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedLoopStatement(LoopStatementSyntax);
impl CheckedNode for CheckedLoopStatement {
    type Syntax = LoopStatementSyntax;
    fn cast_unchecked(syntax: LoopStatementSyntax) -> Self {
        CheckedLoopStatement(syntax)
    }
    fn raw(&self) -> LoopStatementSyntax {
        self.0.clone()
    }
}
impl CheckedLoopStatement {
    pub fn loop_statement_preamble(&self) -> CheckedLoopStatementPreamble {
        CheckedLoopStatementPreamble::cast_unchecked(self.0.loop_statement_preamble().unwrap())
    }
    pub fn sequential_statements(&self) -> Option<CheckedSequentialStatements> {
        self.0
            .sequential_statements()
            .map(CheckedSequentialStatements::cast_unchecked)
    }
    pub fn loop_statement_epilogue(&self) -> CheckedLoopStatementEpilogue {
        CheckedLoopStatementEpilogue::cast_unchecked(self.0.loop_statement_epilogue().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedLoopStatementPreamble(LoopStatementPreambleSyntax);
impl CheckedNode for CheckedLoopStatementPreamble {
    type Syntax = LoopStatementPreambleSyntax;
    fn cast_unchecked(syntax: LoopStatementPreambleSyntax) -> Self {
        CheckedLoopStatementPreamble(syntax)
    }
    fn raw(&self) -> LoopStatementPreambleSyntax {
        self.0.clone()
    }
}
impl CheckedLoopStatementPreamble {
    pub fn label(&self) -> CheckedLabel {
        CheckedLabel::cast_unchecked(self.0.label().unwrap())
    }
    pub fn iteration_scheme(&self) -> Option<CheckedIterationScheme> {
        self.0
            .iteration_scheme()
            .map(CheckedIterationScheme::cast_unchecked)
    }
    pub fn loop_token(&self) -> SyntaxToken {
        self.0.loop_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedLoopStatementEpilogue(LoopStatementEpilogueSyntax);
impl CheckedNode for CheckedLoopStatementEpilogue {
    type Syntax = LoopStatementEpilogueSyntax;
    fn cast_unchecked(syntax: LoopStatementEpilogueSyntax) -> Self {
        CheckedLoopStatementEpilogue(syntax)
    }
    fn raw(&self) -> LoopStatementEpilogueSyntax {
        self.0.clone()
    }
}
impl CheckedLoopStatementEpilogue {
    pub fn end_token(&self) -> SyntaxToken {
        self.0.end_token().unwrap()
    }
    pub fn loop_token(&self) -> SyntaxToken {
        self.0.loop_token().unwrap()
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0.identifier_token()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedNextStatement(NextStatementSyntax);
impl CheckedNode for CheckedNextStatement {
    type Syntax = NextStatementSyntax;
    fn cast_unchecked(syntax: NextStatementSyntax) -> Self {
        CheckedNextStatement(syntax)
    }
    fn raw(&self) -> NextStatementSyntax {
        self.0.clone()
    }
}
impl CheckedNextStatement {
    pub fn label(&self) -> CheckedLabel {
        CheckedLabel::cast_unchecked(self.0.label().unwrap())
    }
    pub fn next_token(&self) -> SyntaxToken {
        self.0.next_token().unwrap()
    }
    pub fn loop_label_token(&self) -> Option<SyntaxToken> {
        self.0.loop_label_token()
    }
    pub fn when_token(&self) -> Option<SyntaxToken> {
        self.0.when_token()
    }
    pub fn expression(&self) -> Option<CheckedExpression> {
        self.0.expression().map(CheckedExpression::cast_unchecked)
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedNullStatement(NullStatementSyntax);
impl CheckedNode for CheckedNullStatement {
    type Syntax = NullStatementSyntax;
    fn cast_unchecked(syntax: NullStatementSyntax) -> Self {
        CheckedNullStatement(syntax)
    }
    fn raw(&self) -> NullStatementSyntax {
        self.0.clone()
    }
}
impl CheckedNullStatement {
    pub fn label(&self) -> CheckedLabel {
        CheckedLabel::cast_unchecked(self.0.label().unwrap())
    }
    pub fn null_token(&self) -> SyntaxToken {
        self.0.null_token().unwrap()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedParameterSpecification(ParameterSpecificationSyntax);
impl CheckedNode for CheckedParameterSpecification {
    type Syntax = ParameterSpecificationSyntax;
    fn cast_unchecked(syntax: ParameterSpecificationSyntax) -> Self {
        CheckedParameterSpecification(syntax)
    }
    fn raw(&self) -> ParameterSpecificationSyntax {
        self.0.clone()
    }
}
impl CheckedParameterSpecification {
    pub fn identifier_token(&self) -> SyntaxToken {
        self.0.identifier_token().unwrap()
    }
    pub fn in_token(&self) -> SyntaxToken {
        self.0.in_token().unwrap()
    }
    pub fn discrete_range(&self) -> CheckedDiscreteRange {
        CheckedDiscreteRange::cast_unchecked(self.0.discrete_range().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedProcedureCallStatement(ProcedureCallStatementSyntax);
impl CheckedNode for CheckedProcedureCallStatement {
    type Syntax = ProcedureCallStatementSyntax;
    fn cast_unchecked(syntax: ProcedureCallStatementSyntax) -> Self {
        CheckedProcedureCallStatement(syntax)
    }
    fn raw(&self) -> ProcedureCallStatementSyntax {
        self.0.clone()
    }
}
impl CheckedProcedureCallStatement {
    pub fn label(&self) -> CheckedLabel {
        CheckedLabel::cast_unchecked(self.0.label().unwrap())
    }
    pub fn name(&self) -> CheckedName {
        CheckedName::cast_unchecked(self.0.name().unwrap())
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedReportStatement(ReportStatementSyntax);
impl CheckedNode for CheckedReportStatement {
    type Syntax = ReportStatementSyntax;
    fn cast_unchecked(syntax: ReportStatementSyntax) -> Self {
        CheckedReportStatement(syntax)
    }
    fn raw(&self) -> ReportStatementSyntax {
        self.0.clone()
    }
}
impl CheckedReportStatement {
    pub fn label(&self) -> CheckedLabel {
        CheckedLabel::cast_unchecked(self.0.label().unwrap())
    }
    pub fn report_token(&self) -> SyntaxToken {
        self.0.report_token().unwrap()
    }
    pub fn report(&self) -> CheckedExpression {
        CheckedExpression::cast_unchecked(self.0.report().unwrap())
    }
    pub fn severity_token(&self) -> Option<SyntaxToken> {
        self.0.severity_token()
    }
    pub fn severity(&self) -> Option<CheckedExpression> {
        self.0.severity().map(CheckedExpression::cast_unchecked)
    }
}
#[derive(Debug, Clone)]
pub struct CheckedReturnStatement(ReturnStatementSyntax);
impl CheckedNode for CheckedReturnStatement {
    type Syntax = ReturnStatementSyntax;
    fn cast_unchecked(syntax: ReturnStatementSyntax) -> Self {
        CheckedReturnStatement(syntax)
    }
    fn raw(&self) -> ReturnStatementSyntax {
        self.0.clone()
    }
}
impl CheckedReturnStatement {
    pub fn label(&self) -> CheckedLabel {
        CheckedLabel::cast_unchecked(self.0.label().unwrap())
    }
    pub fn return_token(&self) -> SyntaxToken {
        self.0.return_token().unwrap()
    }
    pub fn expression(&self) -> Option<CheckedExpression> {
        self.0.expression().map(CheckedExpression::cast_unchecked)
    }
}
#[derive(Debug, Clone)]
pub struct CheckedSelectedExpressionItem(SelectedExpressionItemSyntax);
impl CheckedNode for CheckedSelectedExpressionItem {
    type Syntax = SelectedExpressionItemSyntax;
    fn cast_unchecked(syntax: SelectedExpressionItemSyntax) -> Self {
        CheckedSelectedExpressionItem(syntax)
    }
    fn raw(&self) -> SelectedExpressionItemSyntax {
        self.0.clone()
    }
}
impl CheckedSelectedExpressionItem {
    pub fn expression(&self) -> CheckedExpression {
        CheckedExpression::cast_unchecked(self.0.expression().unwrap())
    }
    pub fn when_token(&self) -> SyntaxToken {
        self.0.when_token().unwrap()
    }
    pub fn choices(&self) -> Option<CheckedChoices> {
        self.0.choices().map(CheckedChoices::cast_unchecked)
    }
}
#[derive(Debug, Clone)]
pub struct CheckedSelectedExpressions(SelectedExpressionsSyntax);
impl CheckedNode for CheckedSelectedExpressions {
    type Syntax = SelectedExpressionsSyntax;
    fn cast_unchecked(syntax: SelectedExpressionsSyntax) -> Self {
        CheckedSelectedExpressions(syntax)
    }
    fn raw(&self) -> SelectedExpressionsSyntax {
        self.0.clone()
    }
}
impl CheckedSelectedExpressions {
    pub fn selected_expression_items(
        &self,
    ) -> impl Iterator<Item = CheckedSelectedExpressionItem> + use<'_> {
        self.0
            .selected_expression_items()
            .map(CheckedSelectedExpressionItem::cast_unchecked)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.comma_token()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedSelectedForceAssignment(SelectedForceAssignmentSyntax);
impl CheckedNode for CheckedSelectedForceAssignment {
    type Syntax = SelectedForceAssignmentSyntax;
    fn cast_unchecked(syntax: SelectedForceAssignmentSyntax) -> Self {
        CheckedSelectedForceAssignment(syntax)
    }
    fn raw(&self) -> SelectedForceAssignmentSyntax {
        self.0.clone()
    }
}
impl CheckedSelectedForceAssignment {
    pub fn selected_assignment_preamble(&self) -> CheckedSelectedAssignmentPreamble {
        CheckedSelectedAssignmentPreamble::cast_unchecked(
            self.0.selected_assignment_preamble().unwrap(),
        )
    }
    pub fn target(&self) -> CheckedTarget {
        CheckedTarget::cast_unchecked(self.0.target().unwrap())
    }
    pub fn lte_token(&self) -> SyntaxToken {
        self.0.lte_token().unwrap()
    }
    pub fn force_token(&self) -> SyntaxToken {
        self.0.force_token().unwrap()
    }
    pub fn force_mode(&self) -> Option<ForceModeSyntax> {
        self.0.force_mode()
    }
    pub fn selected_expressions(&self) -> Option<CheckedSelectedExpressions> {
        self.0
            .selected_expressions()
            .map(CheckedSelectedExpressions::cast_unchecked)
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedSelectedWaveformItem(SelectedWaveformItemSyntax);
impl CheckedNode for CheckedSelectedWaveformItem {
    type Syntax = SelectedWaveformItemSyntax;
    fn cast_unchecked(syntax: SelectedWaveformItemSyntax) -> Self {
        CheckedSelectedWaveformItem(syntax)
    }
    fn raw(&self) -> SelectedWaveformItemSyntax {
        self.0.clone()
    }
}
impl CheckedSelectedWaveformItem {
    pub fn waveform(&self) -> CheckedWaveform {
        CheckedWaveform::cast_unchecked(self.0.waveform().unwrap())
    }
    pub fn when_token(&self) -> SyntaxToken {
        self.0.when_token().unwrap()
    }
    pub fn choices(&self) -> Option<CheckedChoices> {
        self.0.choices().map(CheckedChoices::cast_unchecked)
    }
}
#[derive(Debug, Clone)]
pub struct CheckedSelectedWaveforms(SelectedWaveformsSyntax);
impl CheckedNode for CheckedSelectedWaveforms {
    type Syntax = SelectedWaveformsSyntax;
    fn cast_unchecked(syntax: SelectedWaveformsSyntax) -> Self {
        CheckedSelectedWaveforms(syntax)
    }
    fn raw(&self) -> SelectedWaveformsSyntax {
        self.0.clone()
    }
}
impl CheckedSelectedWaveforms {
    pub fn selected_waveform_items(
        &self,
    ) -> impl Iterator<Item = CheckedSelectedWaveformItem> + use<'_> {
        self.0
            .selected_waveform_items()
            .map(CheckedSelectedWaveformItem::cast_unchecked)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.comma_token()
    }
}
#[derive(Debug, Clone)]
pub enum CheckedSelectedSignalAssignment {
    SelectedWaveformAssignment(CheckedSelectedWaveformAssignment),
    SelectedForceAssignment(CheckedSelectedForceAssignment),
}
impl CheckedNode for CheckedSelectedSignalAssignment {
    type Syntax = SelectedSignalAssignmentSyntax;
    fn cast_unchecked(syntax: SelectedSignalAssignmentSyntax) -> Self {
        match syntax {
            SelectedSignalAssignmentSyntax::SelectedWaveformAssignment(inner) => {
                CheckedSelectedSignalAssignment::SelectedWaveformAssignment(
                    CheckedSelectedWaveformAssignment::cast_unchecked(inner),
                )
            }
            SelectedSignalAssignmentSyntax::SelectedForceAssignment(inner) => {
                CheckedSelectedSignalAssignment::SelectedForceAssignment(
                    CheckedSelectedForceAssignment::cast_unchecked(inner),
                )
            }
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedSelectedSignalAssignment::SelectedWaveformAssignment(inner) => {
                SelectedSignalAssignmentSyntax::SelectedWaveformAssignment(inner.raw())
            }
            CheckedSelectedSignalAssignment::SelectedForceAssignment(inner) => {
                SelectedSignalAssignmentSyntax::SelectedForceAssignment(inner.raw())
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedSelectedVariableAssignment(SelectedVariableAssignmentSyntax);
impl CheckedNode for CheckedSelectedVariableAssignment {
    type Syntax = SelectedVariableAssignmentSyntax;
    fn cast_unchecked(syntax: SelectedVariableAssignmentSyntax) -> Self {
        CheckedSelectedVariableAssignment(syntax)
    }
    fn raw(&self) -> SelectedVariableAssignmentSyntax {
        self.0.clone()
    }
}
impl CheckedSelectedVariableAssignment {
    pub fn selected_assignment_preamble(&self) -> CheckedSelectedAssignmentPreamble {
        CheckedSelectedAssignmentPreamble::cast_unchecked(
            self.0.selected_assignment_preamble().unwrap(),
        )
    }
    pub fn target(&self) -> CheckedTarget {
        CheckedTarget::cast_unchecked(self.0.target().unwrap())
    }
    pub fn colon_eq_token(&self) -> SyntaxToken {
        self.0.colon_eq_token().unwrap()
    }
    pub fn selected_expressions(&self) -> Option<CheckedSelectedExpressions> {
        self.0
            .selected_expressions()
            .map(CheckedSelectedExpressions::cast_unchecked)
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedSelectedWaveformAssignment(SelectedWaveformAssignmentSyntax);
impl CheckedNode for CheckedSelectedWaveformAssignment {
    type Syntax = SelectedWaveformAssignmentSyntax;
    fn cast_unchecked(syntax: SelectedWaveformAssignmentSyntax) -> Self {
        CheckedSelectedWaveformAssignment(syntax)
    }
    fn raw(&self) -> SelectedWaveformAssignmentSyntax {
        self.0.clone()
    }
}
impl CheckedSelectedWaveformAssignment {
    pub fn selected_assignment_preamble(&self) -> CheckedSelectedAssignmentPreamble {
        CheckedSelectedAssignmentPreamble::cast_unchecked(
            self.0.selected_assignment_preamble().unwrap(),
        )
    }
    pub fn target(&self) -> CheckedTarget {
        CheckedTarget::cast_unchecked(self.0.target().unwrap())
    }
    pub fn lte_token(&self) -> SyntaxToken {
        self.0.lte_token().unwrap()
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
pub struct CheckedSelectedAssignmentPreamble(SelectedAssignmentPreambleSyntax);
impl CheckedNode for CheckedSelectedAssignmentPreamble {
    type Syntax = SelectedAssignmentPreambleSyntax;
    fn cast_unchecked(syntax: SelectedAssignmentPreambleSyntax) -> Self {
        CheckedSelectedAssignmentPreamble(syntax)
    }
    fn raw(&self) -> SelectedAssignmentPreambleSyntax {
        self.0.clone()
    }
}
impl CheckedSelectedAssignmentPreamble {
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
pub struct CheckedSensitivityClause(SensitivityClauseSyntax);
impl CheckedNode for CheckedSensitivityClause {
    type Syntax = SensitivityClauseSyntax;
    fn cast_unchecked(syntax: SensitivityClauseSyntax) -> Self {
        CheckedSensitivityClause(syntax)
    }
    fn raw(&self) -> SensitivityClauseSyntax {
        self.0.clone()
    }
}
impl CheckedSensitivityClause {
    pub fn on_token(&self) -> SyntaxToken {
        self.0.on_token().unwrap()
    }
    pub fn name_list(&self) -> Option<CheckedNameList> {
        self.0.name_list().map(CheckedNameList::cast_unchecked)
    }
}
#[derive(Debug, Clone)]
pub enum CheckedSequentialStatement {
    WaitStatement(CheckedWaitStatement),
    AssertionStatement(CheckedAssertionStatement),
    ReportStatement(CheckedReportStatement),
    SignalAssignmentStatement(CheckedSignalAssignmentStatement),
    VariableAssignmentStatement(CheckedVariableAssignmentStatement),
    ProcedureCallStatement(CheckedProcedureCallStatement),
    IfStatement(CheckedIfStatement),
    CaseStatement(CheckedCaseStatement),
    LoopStatement(CheckedLoopStatement),
    NextStatement(CheckedNextStatement),
    ExitStatement(CheckedExitStatement),
    ReturnStatement(CheckedReturnStatement),
    NullStatement(CheckedNullStatement),
}
impl CheckedNode for CheckedSequentialStatement {
    type Syntax = SequentialStatementSyntax;
    fn cast_unchecked(syntax: SequentialStatementSyntax) -> Self {
        match syntax {
            SequentialStatementSyntax::WaitStatement(inner) => {
                CheckedSequentialStatement::WaitStatement(CheckedWaitStatement::cast_unchecked(
                    inner,
                ))
            }
            SequentialStatementSyntax::AssertionStatement(inner) => {
                CheckedSequentialStatement::AssertionStatement(
                    CheckedAssertionStatement::cast_unchecked(inner),
                )
            }
            SequentialStatementSyntax::ReportStatement(inner) => {
                CheckedSequentialStatement::ReportStatement(CheckedReportStatement::cast_unchecked(
                    inner,
                ))
            }
            SequentialStatementSyntax::SignalAssignmentStatement(inner) => {
                CheckedSequentialStatement::SignalAssignmentStatement(
                    CheckedSignalAssignmentStatement::cast_unchecked(inner),
                )
            }
            SequentialStatementSyntax::VariableAssignmentStatement(inner) => {
                CheckedSequentialStatement::VariableAssignmentStatement(
                    CheckedVariableAssignmentStatement::cast_unchecked(inner),
                )
            }
            SequentialStatementSyntax::ProcedureCallStatement(inner) => {
                CheckedSequentialStatement::ProcedureCallStatement(
                    CheckedProcedureCallStatement::cast_unchecked(inner),
                )
            }
            SequentialStatementSyntax::IfStatement(inner) => {
                CheckedSequentialStatement::IfStatement(CheckedIfStatement::cast_unchecked(inner))
            }
            SequentialStatementSyntax::CaseStatement(inner) => {
                CheckedSequentialStatement::CaseStatement(CheckedCaseStatement::cast_unchecked(
                    inner,
                ))
            }
            SequentialStatementSyntax::LoopStatement(inner) => {
                CheckedSequentialStatement::LoopStatement(CheckedLoopStatement::cast_unchecked(
                    inner,
                ))
            }
            SequentialStatementSyntax::NextStatement(inner) => {
                CheckedSequentialStatement::NextStatement(CheckedNextStatement::cast_unchecked(
                    inner,
                ))
            }
            SequentialStatementSyntax::ExitStatement(inner) => {
                CheckedSequentialStatement::ExitStatement(CheckedExitStatement::cast_unchecked(
                    inner,
                ))
            }
            SequentialStatementSyntax::ReturnStatement(inner) => {
                CheckedSequentialStatement::ReturnStatement(CheckedReturnStatement::cast_unchecked(
                    inner,
                ))
            }
            SequentialStatementSyntax::NullStatement(inner) => {
                CheckedSequentialStatement::NullStatement(CheckedNullStatement::cast_unchecked(
                    inner,
                ))
            }
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedSequentialStatement::WaitStatement(inner) => {
                SequentialStatementSyntax::WaitStatement(inner.raw())
            }
            CheckedSequentialStatement::AssertionStatement(inner) => {
                SequentialStatementSyntax::AssertionStatement(inner.raw())
            }
            CheckedSequentialStatement::ReportStatement(inner) => {
                SequentialStatementSyntax::ReportStatement(inner.raw())
            }
            CheckedSequentialStatement::SignalAssignmentStatement(inner) => {
                SequentialStatementSyntax::SignalAssignmentStatement(inner.raw())
            }
            CheckedSequentialStatement::VariableAssignmentStatement(inner) => {
                SequentialStatementSyntax::VariableAssignmentStatement(inner.raw())
            }
            CheckedSequentialStatement::ProcedureCallStatement(inner) => {
                SequentialStatementSyntax::ProcedureCallStatement(inner.raw())
            }
            CheckedSequentialStatement::IfStatement(inner) => {
                SequentialStatementSyntax::IfStatement(inner.raw())
            }
            CheckedSequentialStatement::CaseStatement(inner) => {
                SequentialStatementSyntax::CaseStatement(inner.raw())
            }
            CheckedSequentialStatement::LoopStatement(inner) => {
                SequentialStatementSyntax::LoopStatement(inner.raw())
            }
            CheckedSequentialStatement::NextStatement(inner) => {
                SequentialStatementSyntax::NextStatement(inner.raw())
            }
            CheckedSequentialStatement::ExitStatement(inner) => {
                SequentialStatementSyntax::ExitStatement(inner.raw())
            }
            CheckedSequentialStatement::ReturnStatement(inner) => {
                SequentialStatementSyntax::ReturnStatement(inner.raw())
            }
            CheckedSequentialStatement::NullStatement(inner) => {
                SequentialStatementSyntax::NullStatement(inner.raw())
            }
        }
    }
}
#[derive(Debug, Clone)]
pub enum CheckedSignalAssignmentStatement {
    SimpleSignalAssignment(CheckedSimpleSignalAssignment),
    ConditionalSignalAssignment(CheckedConditionalSignalAssignment),
    SelectedSignalAssignment(CheckedSelectedSignalAssignment),
}
impl CheckedNode for CheckedSignalAssignmentStatement {
    type Syntax = SignalAssignmentStatementSyntax;
    fn cast_unchecked(syntax: SignalAssignmentStatementSyntax) -> Self {
        match syntax {
            SignalAssignmentStatementSyntax::SimpleSignalAssignment(inner) => {
                CheckedSignalAssignmentStatement::SimpleSignalAssignment(
                    CheckedSimpleSignalAssignment::cast_unchecked(inner),
                )
            }
            SignalAssignmentStatementSyntax::ConditionalSignalAssignment(inner) => {
                CheckedSignalAssignmentStatement::ConditionalSignalAssignment(
                    CheckedConditionalSignalAssignment::cast_unchecked(inner),
                )
            }
            SignalAssignmentStatementSyntax::SelectedSignalAssignment(inner) => {
                CheckedSignalAssignmentStatement::SelectedSignalAssignment(
                    CheckedSelectedSignalAssignment::cast_unchecked(inner),
                )
            }
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedSignalAssignmentStatement::SimpleSignalAssignment(inner) => {
                SignalAssignmentStatementSyntax::SimpleSignalAssignment(inner.raw())
            }
            CheckedSignalAssignmentStatement::ConditionalSignalAssignment(inner) => {
                SignalAssignmentStatementSyntax::ConditionalSignalAssignment(inner.raw())
            }
            CheckedSignalAssignmentStatement::SelectedSignalAssignment(inner) => {
                SignalAssignmentStatementSyntax::SelectedSignalAssignment(inner.raw())
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedSimpleForceAssignment(SimpleForceAssignmentSyntax);
impl CheckedNode for CheckedSimpleForceAssignment {
    type Syntax = SimpleForceAssignmentSyntax;
    fn cast_unchecked(syntax: SimpleForceAssignmentSyntax) -> Self {
        CheckedSimpleForceAssignment(syntax)
    }
    fn raw(&self) -> SimpleForceAssignmentSyntax {
        self.0.clone()
    }
}
impl CheckedSimpleForceAssignment {
    pub fn label(&self) -> Option<CheckedLabel> {
        self.0.label().map(CheckedLabel::cast_unchecked)
    }
    pub fn target(&self) -> CheckedTarget {
        CheckedTarget::cast_unchecked(self.0.target().unwrap())
    }
    pub fn lte_token(&self) -> SyntaxToken {
        self.0.lte_token().unwrap()
    }
    pub fn force_token(&self) -> SyntaxToken {
        self.0.force_token().unwrap()
    }
    pub fn force_mode(&self) -> Option<ForceModeSyntax> {
        self.0.force_mode()
    }
    pub fn expression(&self) -> CheckedExpression {
        CheckedExpression::cast_unchecked(self.0.expression().unwrap())
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedSimpleReleaseAssignment(SimpleReleaseAssignmentSyntax);
impl CheckedNode for CheckedSimpleReleaseAssignment {
    type Syntax = SimpleReleaseAssignmentSyntax;
    fn cast_unchecked(syntax: SimpleReleaseAssignmentSyntax) -> Self {
        CheckedSimpleReleaseAssignment(syntax)
    }
    fn raw(&self) -> SimpleReleaseAssignmentSyntax {
        self.0.clone()
    }
}
impl CheckedSimpleReleaseAssignment {
    pub fn label(&self) -> Option<CheckedLabel> {
        self.0.label().map(CheckedLabel::cast_unchecked)
    }
    pub fn target(&self) -> CheckedTarget {
        CheckedTarget::cast_unchecked(self.0.target().unwrap())
    }
    pub fn lte_token(&self) -> SyntaxToken {
        self.0.lte_token().unwrap()
    }
    pub fn release_token(&self) -> SyntaxToken {
        self.0.release_token().unwrap()
    }
    pub fn force_mode(&self) -> Option<ForceModeSyntax> {
        self.0.force_mode()
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedSimpleWaveformAssignment(SimpleWaveformAssignmentSyntax);
impl CheckedNode for CheckedSimpleWaveformAssignment {
    type Syntax = SimpleWaveformAssignmentSyntax;
    fn cast_unchecked(syntax: SimpleWaveformAssignmentSyntax) -> Self {
        CheckedSimpleWaveformAssignment(syntax)
    }
    fn raw(&self) -> SimpleWaveformAssignmentSyntax {
        self.0.clone()
    }
}
impl CheckedSimpleWaveformAssignment {
    pub fn label(&self) -> Option<CheckedLabel> {
        self.0.label().map(CheckedLabel::cast_unchecked)
    }
    pub fn target(&self) -> CheckedTarget {
        CheckedTarget::cast_unchecked(self.0.target().unwrap())
    }
    pub fn lte_token(&self) -> SyntaxToken {
        self.0.lte_token().unwrap()
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
pub enum CheckedSimpleSignalAssignment {
    SimpleWaveformAssignment(CheckedSimpleWaveformAssignment),
    SimpleForceAssignment(CheckedSimpleForceAssignment),
    SimpleReleaseAssignment(CheckedSimpleReleaseAssignment),
}
impl CheckedNode for CheckedSimpleSignalAssignment {
    type Syntax = SimpleSignalAssignmentSyntax;
    fn cast_unchecked(syntax: SimpleSignalAssignmentSyntax) -> Self {
        match syntax {
            SimpleSignalAssignmentSyntax::SimpleWaveformAssignment(inner) => {
                CheckedSimpleSignalAssignment::SimpleWaveformAssignment(
                    CheckedSimpleWaveformAssignment::cast_unchecked(inner),
                )
            }
            SimpleSignalAssignmentSyntax::SimpleForceAssignment(inner) => {
                CheckedSimpleSignalAssignment::SimpleForceAssignment(
                    CheckedSimpleForceAssignment::cast_unchecked(inner),
                )
            }
            SimpleSignalAssignmentSyntax::SimpleReleaseAssignment(inner) => {
                CheckedSimpleSignalAssignment::SimpleReleaseAssignment(
                    CheckedSimpleReleaseAssignment::cast_unchecked(inner),
                )
            }
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedSimpleSignalAssignment::SimpleWaveformAssignment(inner) => {
                SimpleSignalAssignmentSyntax::SimpleWaveformAssignment(inner.raw())
            }
            CheckedSimpleSignalAssignment::SimpleForceAssignment(inner) => {
                SimpleSignalAssignmentSyntax::SimpleForceAssignment(inner.raw())
            }
            CheckedSimpleSignalAssignment::SimpleReleaseAssignment(inner) => {
                SimpleSignalAssignmentSyntax::SimpleReleaseAssignment(inner.raw())
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedSimpleVariableAssignment(SimpleVariableAssignmentSyntax);
impl CheckedNode for CheckedSimpleVariableAssignment {
    type Syntax = SimpleVariableAssignmentSyntax;
    fn cast_unchecked(syntax: SimpleVariableAssignmentSyntax) -> Self {
        CheckedSimpleVariableAssignment(syntax)
    }
    fn raw(&self) -> SimpleVariableAssignmentSyntax {
        self.0.clone()
    }
}
impl CheckedSimpleVariableAssignment {
    pub fn target(&self) -> CheckedTarget {
        CheckedTarget::cast_unchecked(self.0.target().unwrap())
    }
    pub fn colon_eq_token(&self) -> SyntaxToken {
        self.0.colon_eq_token().unwrap()
    }
    pub fn expression(&self) -> CheckedExpression {
        CheckedExpression::cast_unchecked(self.0.expression().unwrap())
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedNameTarget(NameTargetSyntax);
impl CheckedNode for CheckedNameTarget {
    type Syntax = NameTargetSyntax;
    fn cast_unchecked(syntax: NameTargetSyntax) -> Self {
        CheckedNameTarget(syntax)
    }
    fn raw(&self) -> NameTargetSyntax {
        self.0.clone()
    }
}
impl CheckedNameTarget {
    pub fn name(&self) -> CheckedName {
        CheckedName::cast_unchecked(self.0.name().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedAggregateTarget(AggregateTargetSyntax);
impl CheckedNode for CheckedAggregateTarget {
    type Syntax = AggregateTargetSyntax;
    fn cast_unchecked(syntax: AggregateTargetSyntax) -> Self {
        CheckedAggregateTarget(syntax)
    }
    fn raw(&self) -> AggregateTargetSyntax {
        self.0.clone()
    }
}
impl CheckedAggregateTarget {
    pub fn aggregate(&self) -> CheckedAggregate {
        CheckedAggregate::cast_unchecked(self.0.aggregate().unwrap())
    }
}
#[derive(Debug, Clone)]
pub enum CheckedTarget {
    NameTarget(CheckedNameTarget),
    AggregateTarget(CheckedAggregateTarget),
}
impl CheckedNode for CheckedTarget {
    type Syntax = TargetSyntax;
    fn cast_unchecked(syntax: TargetSyntax) -> Self {
        match syntax {
            TargetSyntax::NameTarget(inner) => {
                CheckedTarget::NameTarget(CheckedNameTarget::cast_unchecked(inner))
            }
            TargetSyntax::AggregateTarget(inner) => {
                CheckedTarget::AggregateTarget(CheckedAggregateTarget::cast_unchecked(inner))
            }
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedTarget::NameTarget(inner) => TargetSyntax::NameTarget(inner.raw()),
            CheckedTarget::AggregateTarget(inner) => TargetSyntax::AggregateTarget(inner.raw()),
        }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedTimeoutClause(TimeoutClauseSyntax);
impl CheckedNode for CheckedTimeoutClause {
    type Syntax = TimeoutClauseSyntax;
    fn cast_unchecked(syntax: TimeoutClauseSyntax) -> Self {
        CheckedTimeoutClause(syntax)
    }
    fn raw(&self) -> TimeoutClauseSyntax {
        self.0.clone()
    }
}
impl CheckedTimeoutClause {
    pub fn for_token(&self) -> SyntaxToken {
        self.0.for_token().unwrap()
    }
    pub fn expression(&self) -> CheckedExpression {
        CheckedExpression::cast_unchecked(self.0.expression().unwrap())
    }
}
#[derive(Debug, Clone)]
pub enum CheckedVariableAssignmentStatement {
    SimpleVariableAssignment(CheckedSimpleVariableAssignment),
    ConditionalVariableAssignment(CheckedConditionalVariableAssignment),
    SelectedVariableAssignment(CheckedSelectedVariableAssignment),
}
impl CheckedNode for CheckedVariableAssignmentStatement {
    type Syntax = VariableAssignmentStatementSyntax;
    fn cast_unchecked(syntax: VariableAssignmentStatementSyntax) -> Self {
        match syntax {
            VariableAssignmentStatementSyntax::SimpleVariableAssignment(inner) => {
                CheckedVariableAssignmentStatement::SimpleVariableAssignment(
                    CheckedSimpleVariableAssignment::cast_unchecked(inner),
                )
            }
            VariableAssignmentStatementSyntax::ConditionalVariableAssignment(inner) => {
                CheckedVariableAssignmentStatement::ConditionalVariableAssignment(
                    CheckedConditionalVariableAssignment::cast_unchecked(inner),
                )
            }
            VariableAssignmentStatementSyntax::SelectedVariableAssignment(inner) => {
                CheckedVariableAssignmentStatement::SelectedVariableAssignment(
                    CheckedSelectedVariableAssignment::cast_unchecked(inner),
                )
            }
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedVariableAssignmentStatement::SimpleVariableAssignment(inner) => {
                VariableAssignmentStatementSyntax::SimpleVariableAssignment(inner.raw())
            }
            CheckedVariableAssignmentStatement::ConditionalVariableAssignment(inner) => {
                VariableAssignmentStatementSyntax::ConditionalVariableAssignment(inner.raw())
            }
            CheckedVariableAssignmentStatement::SelectedVariableAssignment(inner) => {
                VariableAssignmentStatementSyntax::SelectedVariableAssignment(inner.raw())
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedWaitStatement(WaitStatementSyntax);
impl CheckedNode for CheckedWaitStatement {
    type Syntax = WaitStatementSyntax;
    fn cast_unchecked(syntax: WaitStatementSyntax) -> Self {
        CheckedWaitStatement(syntax)
    }
    fn raw(&self) -> WaitStatementSyntax {
        self.0.clone()
    }
}
impl CheckedWaitStatement {
    pub fn label(&self) -> CheckedLabel {
        CheckedLabel::cast_unchecked(self.0.label().unwrap())
    }
    pub fn wait_token(&self) -> SyntaxToken {
        self.0.wait_token().unwrap()
    }
    pub fn sensitivity_clause(&self) -> Option<CheckedSensitivityClause> {
        self.0
            .sensitivity_clause()
            .map(CheckedSensitivityClause::cast_unchecked)
    }
    pub fn condition_clause(&self) -> Option<CheckedConditionClause> {
        self.0
            .condition_clause()
            .map(CheckedConditionClause::cast_unchecked)
    }
    pub fn timeout_clause(&self) -> Option<CheckedTimeoutClause> {
        self.0
            .timeout_clause()
            .map(CheckedTimeoutClause::cast_unchecked)
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedWaveformElement(WaveformElementSyntax);
impl CheckedNode for CheckedWaveformElement {
    type Syntax = WaveformElementSyntax;
    fn cast_unchecked(syntax: WaveformElementSyntax) -> Self {
        CheckedWaveformElement(syntax)
    }
    fn raw(&self) -> WaveformElementSyntax {
        self.0.clone()
    }
}
impl CheckedWaveformElement {
    pub fn expression(&self) -> CheckedExpression {
        CheckedExpression::cast_unchecked(self.0.expression().unwrap())
    }
    pub fn after_token(&self) -> SyntaxToken {
        self.0.after_token().unwrap()
    }
    pub fn time_expression(&self) -> CheckedExpression {
        CheckedExpression::cast_unchecked(self.0.time_expression().unwrap())
    }
}
#[derive(Debug, Clone)]
pub struct CheckedWaveformElements(WaveformElementsSyntax);
impl CheckedNode for CheckedWaveformElements {
    type Syntax = WaveformElementsSyntax;
    fn cast_unchecked(syntax: WaveformElementsSyntax) -> Self {
        CheckedWaveformElements(syntax)
    }
    fn raw(&self) -> WaveformElementsSyntax {
        self.0.clone()
    }
}
impl CheckedWaveformElements {
    pub fn waveform_elements(&self) -> impl Iterator<Item = CheckedWaveformElement> + use<'_> {
        self.0
            .waveform_elements()
            .map(CheckedWaveformElement::cast_unchecked)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.comma_token()
    }
}
#[derive(Debug, Clone)]
pub struct CheckedUnaffectedWaveform(UnaffectedWaveformSyntax);
impl CheckedNode for CheckedUnaffectedWaveform {
    type Syntax = UnaffectedWaveformSyntax;
    fn cast_unchecked(syntax: UnaffectedWaveformSyntax) -> Self {
        CheckedUnaffectedWaveform(syntax)
    }
    fn raw(&self) -> UnaffectedWaveformSyntax {
        self.0.clone()
    }
}
impl CheckedUnaffectedWaveform {
    pub fn unaffected_token(&self) -> SyntaxToken {
        self.0.unaffected_token().unwrap()
    }
}
#[derive(Debug, Clone)]
pub enum CheckedWaveform {
    WaveformElements(CheckedWaveformElements),
    UnaffectedWaveform(CheckedUnaffectedWaveform),
}
impl CheckedNode for CheckedWaveform {
    type Syntax = WaveformSyntax;
    fn cast_unchecked(syntax: WaveformSyntax) -> Self {
        match syntax {
            WaveformSyntax::WaveformElements(inner) => {
                CheckedWaveform::WaveformElements(CheckedWaveformElements::cast_unchecked(inner))
            }
            WaveformSyntax::UnaffectedWaveform(inner) => CheckedWaveform::UnaffectedWaveform(
                CheckedUnaffectedWaveform::cast_unchecked(inner),
            ),
        }
    }
    fn raw(&self) -> Self::Syntax {
        match self {
            CheckedWaveform::WaveformElements(inner) => {
                WaveformSyntax::WaveformElements(inner.raw())
            }
            CheckedWaveform::UnaffectedWaveform(inner) => {
                WaveformSyntax::UnaffectedWaveform(inner.raw())
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct CheckedSequentialStatements(SequentialStatementsSyntax);
impl CheckedNode for CheckedSequentialStatements {
    type Syntax = SequentialStatementsSyntax;
    fn cast_unchecked(syntax: SequentialStatementsSyntax) -> Self {
        CheckedSequentialStatements(syntax)
    }
    fn raw(&self) -> SequentialStatementsSyntax {
        self.0.clone()
    }
}
impl CheckedSequentialStatements {
    pub fn sequential_statements(
        &self,
    ) -> impl Iterator<Item = CheckedSequentialStatement> + use<'_> {
        self.0
            .sequential_statements()
            .map(CheckedSequentialStatement::cast_unchecked)
    }
}
