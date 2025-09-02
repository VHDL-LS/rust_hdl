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
pub struct AssertionSyntax(pub(crate) SyntaxNode);
impl AstNode for AssertionSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::Assertion => Some(AssertionSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl AssertionSyntax {
    pub fn assert_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Assert))
            .nth(0)
    }
    pub fn condition(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn report_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Report))
            .nth(0)
    }
    pub fn report(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(1)
    }
    pub fn severity_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Severity))
            .nth(0)
    }
    pub fn severity(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(2)
    }
}
#[derive(Debug, Clone)]
pub struct AssertionStatementSyntax(pub(crate) SyntaxNode);
impl AstNode for AssertionStatementSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::AssertionStatement => Some(AssertionStatementSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl AssertionStatementSyntax {
    pub fn label_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Identifier)
            .nth(0)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == Colon).nth(0)
    }
    pub fn assertion(&self) -> Option<AssertionSyntax> {
        self.0.children().filter_map(AssertionSyntax::cast).nth(0)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct CaseStatementAlternativeSyntax(pub(crate) SyntaxNode);
impl AstNode for CaseStatementAlternativeSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::CaseStatementAlternative => Some(CaseStatementAlternativeSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl CaseStatementAlternativeSyntax {
    pub fn when_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::When))
            .nth(0)
    }
    pub fn choices(&self) -> Option<ChoicesSyntax> {
        self.0.children().filter_map(ChoicesSyntax::cast).nth(0)
    }
    pub fn right_arrow_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == RightArrow)
            .nth(0)
    }
    pub fn sequential_statements(&self) -> Option<SequentialStatementsSyntax> {
        self.0
            .children()
            .filter_map(SequentialStatementsSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct CaseStatementSyntax(pub(crate) SyntaxNode);
impl AstNode for CaseStatementSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::CaseStatement => Some(CaseStatementSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl CaseStatementSyntax {
    pub fn label(&self) -> Option<LabelSyntax> {
        self.0.children().filter_map(LabelSyntax::cast).nth(0)
    }
    pub fn case_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Case))
            .nth(0)
    }
    pub fn que_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == Que).nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn is_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Is))
            .nth(0)
    }
    pub fn case_statement_alternatives(
        &self,
    ) -> impl Iterator<Item = CaseStatementAlternativeSyntax> + use<'_> {
        self.0
            .children()
            .filter_map(CaseStatementAlternativeSyntax::cast)
    }
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::End))
            .nth(0)
    }
    pub fn trailing_case_token_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Case))
            .nth(1)
    }
    pub fn trailing_que_token_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == Que).nth(1)
    }
    pub fn trailing_case_label_token(&self) -> Option<SyntaxToken> {
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
pub struct ConditionClauseSyntax(pub(crate) SyntaxNode);
impl AstNode for ConditionClauseSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ConditionClause => Some(ConditionClauseSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConditionClauseSyntax {
    pub fn until_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Until))
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ConditionalElseWhenExpressionSyntax(pub(crate) SyntaxNode);
impl AstNode for ConditionalElseWhenExpressionSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ConditionalElseWhenExpression => {
                Some(ConditionalElseWhenExpressionSyntax(node))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConditionalElseWhenExpressionSyntax {
    pub fn else_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Else))
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn when_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::When))
            .nth(0)
    }
    pub fn condition(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(1)
    }
}
#[derive(Debug, Clone)]
pub struct ConditionalElseItemSyntax(pub(crate) SyntaxNode);
impl AstNode for ConditionalElseItemSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ConditionalElseItem => Some(ConditionalElseItemSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConditionalElseItemSyntax {
    pub fn else_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Else))
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ConditionalExpressionsSyntax(pub(crate) SyntaxNode);
impl AstNode for ConditionalExpressionsSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ConditionalExpressions => Some(ConditionalExpressionsSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConditionalExpressionsSyntax {
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn when_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::When))
            .nth(0)
    }
    pub fn condition(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(1)
    }
    pub fn conditional_else_when_expressions(
        &self,
    ) -> impl Iterator<Item = ConditionalElseWhenExpressionSyntax> + use<'_> {
        self.0
            .children()
            .filter_map(ConditionalElseWhenExpressionSyntax::cast)
    }
    pub fn conditional_else_item(&self) -> Option<ConditionalElseItemSyntax> {
        self.0
            .children()
            .filter_map(ConditionalElseItemSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ConditionalForceAssignmentSyntax(pub(crate) SyntaxNode);
impl AstNode for ConditionalForceAssignmentSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ConditionalForceAssignment => Some(ConditionalForceAssignmentSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConditionalForceAssignmentSyntax {
    pub fn target(&self) -> Option<TargetSyntax> {
        self.0.children().filter_map(TargetSyntax::cast).nth(0)
    }
    pub fn lte_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == LTE).nth(0)
    }
    pub fn force_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Force))
            .nth(0)
    }
    pub fn force_mode(&self) -> Option<ForceModeSyntax> {
        self.0.tokens().filter_map(ForceModeSyntax::cast).nth(0)
    }
    pub fn conditional_expressions(&self) -> Option<ConditionalExpressionsSyntax> {
        self.0
            .children()
            .filter_map(ConditionalExpressionsSyntax::cast)
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
pub enum ConditionalSignalAssignmentSyntax {
    ConditionalWaveformAssignment(ConditionalWaveformAssignmentSyntax),
    ConditionalForceAssignment(ConditionalForceAssignmentSyntax),
}
impl AstNode for ConditionalSignalAssignmentSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ConditionalWaveformAssignment => Some(
                ConditionalSignalAssignmentSyntax::ConditionalWaveformAssignment(
                    ConditionalWaveformAssignmentSyntax::cast(node).unwrap(),
                ),
            ),
            NodeKind::ConditionalForceAssignment => Some(
                ConditionalSignalAssignmentSyntax::ConditionalForceAssignment(
                    ConditionalForceAssignmentSyntax::cast(node).unwrap(),
                ),
            ),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            ConditionalSignalAssignmentSyntax::ConditionalWaveformAssignment(inner) => inner.raw(),
            ConditionalSignalAssignmentSyntax::ConditionalForceAssignment(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ConditionalVariableAssignmentSyntax(pub(crate) SyntaxNode);
impl AstNode for ConditionalVariableAssignmentSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ConditionalVariableAssignment => {
                Some(ConditionalVariableAssignmentSyntax(node))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConditionalVariableAssignmentSyntax {
    pub fn target(&self) -> Option<TargetSyntax> {
        self.0.children().filter_map(TargetSyntax::cast).nth(0)
    }
    pub fn colon_eq_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == ColonEq)
            .nth(0)
    }
    pub fn conditional_expressions(&self) -> Option<ConditionalExpressionsSyntax> {
        self.0
            .children()
            .filter_map(ConditionalExpressionsSyntax::cast)
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
pub struct ConditionalWaveformAssignmentSyntax(pub(crate) SyntaxNode);
impl AstNode for ConditionalWaveformAssignmentSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ConditionalWaveformAssignment => {
                Some(ConditionalWaveformAssignmentSyntax(node))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConditionalWaveformAssignmentSyntax {
    pub fn target(&self) -> Option<TargetSyntax> {
        self.0.children().filter_map(TargetSyntax::cast).nth(0)
    }
    pub fn lte_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == LTE).nth(0)
    }
    pub fn delay_mechanism(&self) -> Option<DelayMechanismSyntax> {
        self.0
            .children()
            .filter_map(DelayMechanismSyntax::cast)
            .nth(0)
    }
    pub fn conditional_waveforms(&self) -> Option<ConditionalWaveformsSyntax> {
        self.0
            .children()
            .filter_map(ConditionalWaveformsSyntax::cast)
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
pub struct ConditionalWaveformElseWhenExpressionSyntax(pub(crate) SyntaxNode);
impl AstNode for ConditionalWaveformElseWhenExpressionSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ConditionalWaveformElseWhenExpression => {
                Some(ConditionalWaveformElseWhenExpressionSyntax(node))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConditionalWaveformElseWhenExpressionSyntax {
    pub fn else_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Else))
            .nth(0)
    }
    pub fn waveform(&self) -> Option<WaveformSyntax> {
        self.0.children().filter_map(WaveformSyntax::cast).nth(0)
    }
    pub fn when_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::When))
            .nth(0)
    }
    pub fn condition(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ConditionalWaveformElseItemSyntax(pub(crate) SyntaxNode);
impl AstNode for ConditionalWaveformElseItemSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ConditionalWaveformElseItem => Some(ConditionalWaveformElseItemSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConditionalWaveformElseItemSyntax {
    pub fn else_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Else))
            .nth(0)
    }
    pub fn waveform(&self) -> Option<WaveformSyntax> {
        self.0.children().filter_map(WaveformSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ConditionalWaveformsSyntax(pub(crate) SyntaxNode);
impl AstNode for ConditionalWaveformsSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ConditionalWaveforms => Some(ConditionalWaveformsSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConditionalWaveformsSyntax {
    pub fn waveform(&self) -> Option<WaveformSyntax> {
        self.0.children().filter_map(WaveformSyntax::cast).nth(0)
    }
    pub fn when_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::When))
            .nth(0)
    }
    pub fn condition(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn conditional_waveform_else_when_expressions(
        &self,
    ) -> impl Iterator<Item = ConditionalWaveformElseWhenExpressionSyntax> + use<'_> {
        self.0
            .children()
            .filter_map(ConditionalWaveformElseWhenExpressionSyntax::cast)
    }
    pub fn conditional_waveform_else_item(&self) -> Option<ConditionalWaveformElseItemSyntax> {
        self.0
            .children()
            .filter_map(ConditionalWaveformElseItemSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct TransportDelayMechanismSyntax(pub(crate) SyntaxNode);
impl AstNode for TransportDelayMechanismSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::TransportDelayMechanism => Some(TransportDelayMechanismSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl TransportDelayMechanismSyntax {
    pub fn transport_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Transport))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct InertialDelayMechanismSyntax(pub(crate) SyntaxNode);
impl AstNode for InertialDelayMechanismSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::InertialDelayMechanism => Some(InertialDelayMechanismSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InertialDelayMechanismSyntax {
    pub fn reject_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Reject))
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn inertial_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Inertial))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum DelayMechanismSyntax {
    TransportDelayMechanism(TransportDelayMechanismSyntax),
    InertialDelayMechanism(InertialDelayMechanismSyntax),
}
impl AstNode for DelayMechanismSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::TransportDelayMechanism => {
                Some(DelayMechanismSyntax::TransportDelayMechanism(
                    TransportDelayMechanismSyntax::cast(node).unwrap(),
                ))
            }
            NodeKind::InertialDelayMechanism => Some(DelayMechanismSyntax::InertialDelayMechanism(
                InertialDelayMechanismSyntax::cast(node).unwrap(),
            )),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            DelayMechanismSyntax::TransportDelayMechanism(inner) => inner.raw(),
            DelayMechanismSyntax::InertialDelayMechanism(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExitStatementSyntax(pub(crate) SyntaxNode);
impl AstNode for ExitStatementSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ExitStatement => Some(ExitStatementSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ExitStatementSyntax {
    pub fn label_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Identifier)
            .nth(0)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == Colon).nth(0)
    }
    pub fn exit_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Exit))
            .nth(0)
    }
    pub fn loop_label_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Identifier)
            .nth(1)
    }
    pub fn when_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::When))
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
pub enum ForceModeSyntax {
    In(SyntaxToken),
    Out(SyntaxToken),
}
impl ForceModeSyntax {
    pub fn cast(token: SyntaxToken) -> Option<Self> {
        match token.kind() {
            Keyword(Kw::In) => Some(ForceModeSyntax::In(token)),
            Keyword(Kw::Out) => Some(ForceModeSyntax::Out(token)),
            _ => None,
        }
    }
    pub fn raw(&self) -> SyntaxToken {
        match self {
            ForceModeSyntax::In(token) => token.clone(),
            ForceModeSyntax::Out(token) => token.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct IfStatementElsifSyntax(pub(crate) SyntaxNode);
impl AstNode for IfStatementElsifSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::IfStatementElsif => Some(IfStatementElsifSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl IfStatementElsifSyntax {
    pub fn elsif_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Elsif))
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn then_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Then))
            .nth(0)
    }
    pub fn sequential_statements(&self) -> Option<SequentialStatementsSyntax> {
        self.0
            .children()
            .filter_map(SequentialStatementsSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct IfStatementElseSyntax(pub(crate) SyntaxNode);
impl AstNode for IfStatementElseSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::IfStatementElse => Some(IfStatementElseSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl IfStatementElseSyntax {
    pub fn else_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Else))
            .nth(0)
    }
    pub fn sequential_statements(&self) -> Option<SequentialStatementsSyntax> {
        self.0
            .children()
            .filter_map(SequentialStatementsSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct IfStatementSyntax(pub(crate) SyntaxNode);
impl AstNode for IfStatementSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::IfStatement => Some(IfStatementSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl IfStatementSyntax {
    pub fn if_label_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Identifier)
            .nth(0)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == Colon).nth(0)
    }
    pub fn if_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::If))
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn then_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Then))
            .nth(0)
    }
    pub fn sequential_statements(&self) -> Option<SequentialStatementsSyntax> {
        self.0
            .children()
            .filter_map(SequentialStatementsSyntax::cast)
            .nth(0)
    }
    pub fn if_statement_elsifs(&self) -> impl Iterator<Item = IfStatementElsifSyntax> + use<'_> {
        self.0.children().filter_map(IfStatementElsifSyntax::cast)
    }
    pub fn if_statement_else(&self) -> Option<IfStatementElseSyntax> {
        self.0
            .children()
            .filter_map(IfStatementElseSyntax::cast)
            .nth(0)
    }
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::End))
            .nth(0)
    }
    pub fn trailing_if_token_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::If))
            .nth(1)
    }
    pub fn trailing_if_label_token(&self) -> Option<SyntaxToken> {
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
pub struct WhileIterationSchemeSyntax(pub(crate) SyntaxNode);
impl AstNode for WhileIterationSchemeSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::WhileIterationScheme => Some(WhileIterationSchemeSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl WhileIterationSchemeSyntax {
    pub fn while_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::While))
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ForIterationSchemeSyntax(pub(crate) SyntaxNode);
impl AstNode for ForIterationSchemeSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ForIterationScheme => Some(ForIterationSchemeSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ForIterationSchemeSyntax {
    pub fn for_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::For))
            .nth(0)
    }
    pub fn parameter_specification(&self) -> Option<ParameterSpecificationSyntax> {
        self.0
            .children()
            .filter_map(ParameterSpecificationSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum IterationSchemeSyntax {
    WhileIterationScheme(WhileIterationSchemeSyntax),
    ForIterationScheme(ForIterationSchemeSyntax),
}
impl AstNode for IterationSchemeSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::WhileIterationScheme => Some(IterationSchemeSyntax::WhileIterationScheme(
                WhileIterationSchemeSyntax::cast(node).unwrap(),
            )),
            NodeKind::ForIterationScheme => Some(IterationSchemeSyntax::ForIterationScheme(
                ForIterationSchemeSyntax::cast(node).unwrap(),
            )),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            IterationSchemeSyntax::WhileIterationScheme(inner) => inner.raw(),
            IterationSchemeSyntax::ForIterationScheme(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LoopStatementSyntax(pub(crate) SyntaxNode);
impl AstNode for LoopStatementSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::LoopStatement => Some(LoopStatementSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl LoopStatementSyntax {
    pub fn label(&self) -> Option<LabelSyntax> {
        self.0.children().filter_map(LabelSyntax::cast).nth(0)
    }
    pub fn iteration_scheme(&self) -> Option<IterationSchemeSyntax> {
        self.0
            .children()
            .filter_map(IterationSchemeSyntax::cast)
            .nth(0)
    }
    pub fn loop_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Loop))
            .nth(0)
    }
    pub fn sequential_statements(&self) -> Option<SequentialStatementsSyntax> {
        self.0
            .children()
            .filter_map(SequentialStatementsSyntax::cast)
            .nth(0)
    }
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::End))
            .nth(0)
    }
    pub fn trailing_loop_token_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Loop))
            .nth(1)
    }
    pub fn trailing_loop_label_token(&self) -> Option<SyntaxToken> {
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
pub struct NextStatementSyntax(pub(crate) SyntaxNode);
impl AstNode for NextStatementSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::NextStatement => Some(NextStatementSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl NextStatementSyntax {
    pub fn label(&self) -> Option<LabelSyntax> {
        self.0.children().filter_map(LabelSyntax::cast).nth(0)
    }
    pub fn next_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Next))
            .nth(0)
    }
    pub fn loop_label_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Identifier)
            .nth(0)
    }
    pub fn when_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::When))
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
pub struct NullStatementSyntax(pub(crate) SyntaxNode);
impl AstNode for NullStatementSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::NullStatement => Some(NullStatementSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl NullStatementSyntax {
    pub fn label(&self) -> Option<LabelSyntax> {
        self.0.children().filter_map(LabelSyntax::cast).nth(0)
    }
    pub fn null_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Null))
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
pub struct ParameterSpecificationSyntax(pub(crate) SyntaxNode);
impl AstNode for ParameterSpecificationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ParameterSpecification => Some(ParameterSpecificationSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ParameterSpecificationSyntax {
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Identifier)
            .nth(0)
    }
    pub fn in_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::In))
            .nth(0)
    }
    pub fn discrete_range(&self) -> Option<DiscreteRangeSyntax> {
        self.0
            .children()
            .filter_map(DiscreteRangeSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ProcedureCallStatementSyntax(pub(crate) SyntaxNode);
impl AstNode for ProcedureCallStatementSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ProcedureCallStatement => Some(ProcedureCallStatementSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ProcedureCallStatementSyntax {
    pub fn label(&self) -> Option<LabelSyntax> {
        self.0.children().filter_map(LabelSyntax::cast).nth(0)
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ReportStatementSyntax(pub(crate) SyntaxNode);
impl AstNode for ReportStatementSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ReportStatement => Some(ReportStatementSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ReportStatementSyntax {
    pub fn label(&self) -> Option<LabelSyntax> {
        self.0.children().filter_map(LabelSyntax::cast).nth(0)
    }
    pub fn report_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Report))
            .nth(0)
    }
    pub fn report(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn severity_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Severity))
            .nth(0)
    }
    pub fn severity(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(1)
    }
}
#[derive(Debug, Clone)]
pub struct ReturnStatementSyntax(pub(crate) SyntaxNode);
impl AstNode for ReturnStatementSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ReturnStatement => Some(ReturnStatementSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ReturnStatementSyntax {
    pub fn label(&self) -> Option<LabelSyntax> {
        self.0.children().filter_map(LabelSyntax::cast).nth(0)
    }
    pub fn return_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Return))
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct SelectedExpressionItemSyntax(pub(crate) SyntaxNode);
impl AstNode for SelectedExpressionItemSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SelectedExpressionItem => Some(SelectedExpressionItemSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SelectedExpressionItemSyntax {
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn when_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::When))
            .nth(0)
    }
    pub fn choices(&self) -> Option<ChoicesSyntax> {
        self.0.children().filter_map(ChoicesSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct SelectedExpressionsSyntax(pub(crate) SyntaxNode);
impl AstNode for SelectedExpressionsSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SelectedExpressions => Some(SelectedExpressionsSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SelectedExpressionsSyntax {
    pub fn selected_expression_items(
        &self,
    ) -> impl Iterator<Item = SelectedExpressionItemSyntax> + use<'_> {
        self.0
            .children()
            .filter_map(SelectedExpressionItemSyntax::cast)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.tokens().filter(|token| token.kind() == Comma)
    }
}
#[derive(Debug, Clone)]
pub struct SelectedForceAssignmentSyntax(pub(crate) SyntaxNode);
impl AstNode for SelectedForceAssignmentSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SelectedForceAssignment => Some(SelectedForceAssignmentSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SelectedForceAssignmentSyntax {
    pub fn with_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::With))
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn select_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Select))
            .nth(0)
    }
    pub fn que_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == Que).nth(0)
    }
    pub fn target(&self) -> Option<TargetSyntax> {
        self.0.children().filter_map(TargetSyntax::cast).nth(0)
    }
    pub fn lte_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == LTE).nth(0)
    }
    pub fn force_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Force))
            .nth(0)
    }
    pub fn force_mode(&self) -> Option<ForceModeSyntax> {
        self.0.tokens().filter_map(ForceModeSyntax::cast).nth(0)
    }
    pub fn selected_expressions(&self) -> Option<SelectedExpressionsSyntax> {
        self.0
            .children()
            .filter_map(SelectedExpressionsSyntax::cast)
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
pub struct SelectedWaveformItemSyntax(pub(crate) SyntaxNode);
impl AstNode for SelectedWaveformItemSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SelectedWaveformItem => Some(SelectedWaveformItemSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SelectedWaveformItemSyntax {
    pub fn waveform(&self) -> Option<WaveformSyntax> {
        self.0.children().filter_map(WaveformSyntax::cast).nth(0)
    }
    pub fn when_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::When))
            .nth(0)
    }
    pub fn choices(&self) -> Option<ChoicesSyntax> {
        self.0.children().filter_map(ChoicesSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct SelectedWaveformsSyntax(pub(crate) SyntaxNode);
impl AstNode for SelectedWaveformsSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SelectedWaveforms => Some(SelectedWaveformsSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SelectedWaveformsSyntax {
    pub fn selected_waveform_items(
        &self,
    ) -> impl Iterator<Item = SelectedWaveformItemSyntax> + use<'_> {
        self.0
            .children()
            .filter_map(SelectedWaveformItemSyntax::cast)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.tokens().filter(|token| token.kind() == Comma)
    }
}
#[derive(Debug, Clone)]
pub enum SelectedSignalAssignmentSyntax {
    SelectedWaveformAssignment(SelectedWaveformAssignmentSyntax),
    SelectedForceAssignment(SelectedForceAssignmentSyntax),
}
impl AstNode for SelectedSignalAssignmentSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SelectedWaveformAssignment => {
                Some(SelectedSignalAssignmentSyntax::SelectedWaveformAssignment(
                    SelectedWaveformAssignmentSyntax::cast(node).unwrap(),
                ))
            }
            NodeKind::SelectedForceAssignment => {
                Some(SelectedSignalAssignmentSyntax::SelectedForceAssignment(
                    SelectedForceAssignmentSyntax::cast(node).unwrap(),
                ))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            SelectedSignalAssignmentSyntax::SelectedWaveformAssignment(inner) => inner.raw(),
            SelectedSignalAssignmentSyntax::SelectedForceAssignment(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SelectedVariableAssignmentSyntax(pub(crate) SyntaxNode);
impl AstNode for SelectedVariableAssignmentSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SelectedVariableAssignment => Some(SelectedVariableAssignmentSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SelectedVariableAssignmentSyntax {
    pub fn with_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::With))
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn select_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Select))
            .nth(0)
    }
    pub fn que_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == Que).nth(0)
    }
    pub fn target(&self) -> Option<TargetSyntax> {
        self.0.children().filter_map(TargetSyntax::cast).nth(0)
    }
    pub fn colon_eq_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == ColonEq)
            .nth(0)
    }
    pub fn selected_expressions(&self) -> Option<SelectedExpressionsSyntax> {
        self.0
            .children()
            .filter_map(SelectedExpressionsSyntax::cast)
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
pub struct SelectedWaveformAssignmentSyntax(pub(crate) SyntaxNode);
impl AstNode for SelectedWaveformAssignmentSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SelectedWaveformAssignment => Some(SelectedWaveformAssignmentSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SelectedWaveformAssignmentSyntax {
    pub fn with_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::With))
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn select_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Select))
            .nth(0)
    }
    pub fn que_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == Que).nth(0)
    }
    pub fn target(&self) -> Option<TargetSyntax> {
        self.0.children().filter_map(TargetSyntax::cast).nth(0)
    }
    pub fn lte_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == LTE).nth(0)
    }
    pub fn delay_mechanism(&self) -> Option<DelayMechanismSyntax> {
        self.0
            .children()
            .filter_map(DelayMechanismSyntax::cast)
            .nth(0)
    }
    pub fn selected_waveforms(&self) -> Option<SelectedWaveformsSyntax> {
        self.0
            .children()
            .filter_map(SelectedWaveformsSyntax::cast)
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
pub struct SensitivityClauseSyntax(pub(crate) SyntaxNode);
impl AstNode for SensitivityClauseSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SensitivityClause => Some(SensitivityClauseSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SensitivityClauseSyntax {
    pub fn on_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::On))
            .nth(0)
    }
    pub fn name_list(&self) -> Option<NameListSyntax> {
        self.0.children().filter_map(NameListSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct SequentialStatementsSyntax(pub(crate) SyntaxNode);
impl AstNode for SequentialStatementsSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SequentialStatements => Some(SequentialStatementsSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SequentialStatementsSyntax {
    pub fn sequential_statements(
        &self,
    ) -> impl Iterator<Item = SequentialStatementSyntax> + use<'_> {
        self.0
            .children()
            .filter_map(SequentialStatementSyntax::cast)
    }
}
#[derive(Debug, Clone)]
pub enum SequentialStatementSyntax {
    WaitStatement(WaitStatementSyntax),
    AssertionStatement(AssertionStatementSyntax),
    ReportStatement(ReportStatementSyntax),
    SignalAssignmentStatement(SignalAssignmentStatementSyntax),
    VariableAssignmentStatement(VariableAssignmentStatementSyntax),
    ProcedureCallStatement(ProcedureCallStatementSyntax),
    IfStatement(IfStatementSyntax),
    CaseStatement(CaseStatementSyntax),
    LoopStatement(LoopStatementSyntax),
    NextStatement(NextStatementSyntax),
    ExitStatement(ExitStatementSyntax),
    ReturnStatement(ReturnStatementSyntax),
    NullStatement(NullStatementSyntax),
}
impl AstNode for SequentialStatementSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::WaitStatement => Some(SequentialStatementSyntax::WaitStatement(
                WaitStatementSyntax::cast(node).unwrap(),
            )),
            NodeKind::AssertionStatement => Some(SequentialStatementSyntax::AssertionStatement(
                AssertionStatementSyntax::cast(node).unwrap(),
            )),
            NodeKind::ReportStatement => Some(SequentialStatementSyntax::ReportStatement(
                ReportStatementSyntax::cast(node).unwrap(),
            )),
            NodeKind::SignalAssignmentStatement => {
                Some(SequentialStatementSyntax::SignalAssignmentStatement(
                    SignalAssignmentStatementSyntax::cast(node).unwrap(),
                ))
            }
            NodeKind::VariableAssignmentStatement => {
                Some(SequentialStatementSyntax::VariableAssignmentStatement(
                    VariableAssignmentStatementSyntax::cast(node).unwrap(),
                ))
            }
            NodeKind::ProcedureCallStatement => {
                Some(SequentialStatementSyntax::ProcedureCallStatement(
                    ProcedureCallStatementSyntax::cast(node).unwrap(),
                ))
            }
            NodeKind::IfStatement => Some(SequentialStatementSyntax::IfStatement(
                IfStatementSyntax::cast(node).unwrap(),
            )),
            NodeKind::CaseStatement => Some(SequentialStatementSyntax::CaseStatement(
                CaseStatementSyntax::cast(node).unwrap(),
            )),
            NodeKind::LoopStatement => Some(SequentialStatementSyntax::LoopStatement(
                LoopStatementSyntax::cast(node).unwrap(),
            )),
            NodeKind::NextStatement => Some(SequentialStatementSyntax::NextStatement(
                NextStatementSyntax::cast(node).unwrap(),
            )),
            NodeKind::ExitStatement => Some(SequentialStatementSyntax::ExitStatement(
                ExitStatementSyntax::cast(node).unwrap(),
            )),
            NodeKind::ReturnStatement => Some(SequentialStatementSyntax::ReturnStatement(
                ReturnStatementSyntax::cast(node).unwrap(),
            )),
            NodeKind::NullStatement => Some(SequentialStatementSyntax::NullStatement(
                NullStatementSyntax::cast(node).unwrap(),
            )),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            SequentialStatementSyntax::WaitStatement(inner) => inner.raw(),
            SequentialStatementSyntax::AssertionStatement(inner) => inner.raw(),
            SequentialStatementSyntax::ReportStatement(inner) => inner.raw(),
            SequentialStatementSyntax::SignalAssignmentStatement(inner) => inner.raw(),
            SequentialStatementSyntax::VariableAssignmentStatement(inner) => inner.raw(),
            SequentialStatementSyntax::ProcedureCallStatement(inner) => inner.raw(),
            SequentialStatementSyntax::IfStatement(inner) => inner.raw(),
            SequentialStatementSyntax::CaseStatement(inner) => inner.raw(),
            SequentialStatementSyntax::LoopStatement(inner) => inner.raw(),
            SequentialStatementSyntax::NextStatement(inner) => inner.raw(),
            SequentialStatementSyntax::ExitStatement(inner) => inner.raw(),
            SequentialStatementSyntax::ReturnStatement(inner) => inner.raw(),
            SequentialStatementSyntax::NullStatement(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SignalAssignmentStatementSyntax(pub(crate) SyntaxNode);
impl AstNode for SignalAssignmentStatementSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SignalAssignmentStatement => Some(SignalAssignmentStatementSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SignalAssignmentStatementSyntax {
    pub fn label(&self) -> Option<LabelSyntax> {
        self.0.children().filter_map(LabelSyntax::cast).nth(0)
    }
    pub fn simple_signal_assignment(&self) -> Option<SimpleSignalAssignmentSyntax> {
        self.0
            .children()
            .filter_map(SimpleSignalAssignmentSyntax::cast)
            .nth(0)
    }
    pub fn conditional_signal_assignment(&self) -> Option<ConditionalSignalAssignmentSyntax> {
        self.0
            .children()
            .filter_map(ConditionalSignalAssignmentSyntax::cast)
            .nth(0)
    }
    pub fn selected_signal_assignment(&self) -> Option<SelectedSignalAssignmentSyntax> {
        self.0
            .children()
            .filter_map(SelectedSignalAssignmentSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct SimpleForceAssignmentSyntax(pub(crate) SyntaxNode);
impl AstNode for SimpleForceAssignmentSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SimpleForceAssignment => Some(SimpleForceAssignmentSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SimpleForceAssignmentSyntax {
    pub fn target(&self) -> Option<TargetSyntax> {
        self.0.children().filter_map(TargetSyntax::cast).nth(0)
    }
    pub fn lte_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == LTE).nth(0)
    }
    pub fn force_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Force))
            .nth(0)
    }
    pub fn force_mode(&self) -> Option<ForceModeSyntax> {
        self.0.tokens().filter_map(ForceModeSyntax::cast).nth(0)
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
pub struct SimpleReleaseAssignmentSyntax(pub(crate) SyntaxNode);
impl AstNode for SimpleReleaseAssignmentSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SimpleReleaseAssignment => Some(SimpleReleaseAssignmentSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SimpleReleaseAssignmentSyntax {
    pub fn target(&self) -> Option<TargetSyntax> {
        self.0.children().filter_map(TargetSyntax::cast).nth(0)
    }
    pub fn lte_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == LTE).nth(0)
    }
    pub fn release_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Release))
            .nth(0)
    }
    pub fn force_mode(&self) -> Option<ForceModeSyntax> {
        self.0.tokens().filter_map(ForceModeSyntax::cast).nth(0)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct SimpleWaveformAssignmentSyntax(pub(crate) SyntaxNode);
impl AstNode for SimpleWaveformAssignmentSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SimpleWaveformAssignment => Some(SimpleWaveformAssignmentSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SimpleWaveformAssignmentSyntax {
    pub fn target(&self) -> Option<TargetSyntax> {
        self.0.children().filter_map(TargetSyntax::cast).nth(0)
    }
    pub fn lte_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == LTE).nth(0)
    }
    pub fn delay_mechanism(&self) -> Option<DelayMechanismSyntax> {
        self.0
            .children()
            .filter_map(DelayMechanismSyntax::cast)
            .nth(0)
    }
    pub fn waveform(&self) -> Option<WaveformSyntax> {
        self.0.children().filter_map(WaveformSyntax::cast).nth(0)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum SimpleSignalAssignmentSyntax {
    SimpleWaveformAssignment(SimpleWaveformAssignmentSyntax),
    SimpleForceAssignment(SimpleForceAssignmentSyntax),
    SimpleReleaseAssignment(SimpleReleaseAssignmentSyntax),
}
impl AstNode for SimpleSignalAssignmentSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SimpleWaveformAssignment => {
                Some(SimpleSignalAssignmentSyntax::SimpleWaveformAssignment(
                    SimpleWaveformAssignmentSyntax::cast(node).unwrap(),
                ))
            }
            NodeKind::SimpleForceAssignment => {
                Some(SimpleSignalAssignmentSyntax::SimpleForceAssignment(
                    SimpleForceAssignmentSyntax::cast(node).unwrap(),
                ))
            }
            NodeKind::SimpleReleaseAssignment => {
                Some(SimpleSignalAssignmentSyntax::SimpleReleaseAssignment(
                    SimpleReleaseAssignmentSyntax::cast(node).unwrap(),
                ))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            SimpleSignalAssignmentSyntax::SimpleWaveformAssignment(inner) => inner.raw(),
            SimpleSignalAssignmentSyntax::SimpleForceAssignment(inner) => inner.raw(),
            SimpleSignalAssignmentSyntax::SimpleReleaseAssignment(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SimpleVariableAssignmentSyntax(pub(crate) SyntaxNode);
impl AstNode for SimpleVariableAssignmentSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SimpleVariableAssignment => Some(SimpleVariableAssignmentSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SimpleVariableAssignmentSyntax {
    pub fn target(&self) -> Option<TargetSyntax> {
        self.0.children().filter_map(TargetSyntax::cast).nth(0)
    }
    pub fn colon_eq_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == ColonEq)
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
pub enum TargetSyntax {
    Name(NameSyntax),
    Aggregate(AggregateSyntax),
}
impl AstNode for TargetSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::Name => Some(TargetSyntax::Name(NameSyntax::cast(node).unwrap())),
            NodeKind::Aggregate => Some(TargetSyntax::Aggregate(
                AggregateSyntax::cast(node).unwrap(),
            )),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            TargetSyntax::Name(inner) => inner.raw(),
            TargetSyntax::Aggregate(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TimeoutClauseSyntax(pub(crate) SyntaxNode);
impl AstNode for TimeoutClauseSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::TimeoutClause => Some(TimeoutClauseSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl TimeoutClauseSyntax {
    pub fn for_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::For))
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum VariableAssignmentStatementSyntax {
    SimpleVariableAssignment(SimpleVariableAssignmentSyntax),
    ConditionalVariableAssignment(ConditionalVariableAssignmentSyntax),
    SelectedVariableAssignment(SelectedVariableAssignmentSyntax),
}
impl AstNode for VariableAssignmentStatementSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SimpleVariableAssignment => {
                Some(VariableAssignmentStatementSyntax::SimpleVariableAssignment(
                    SimpleVariableAssignmentSyntax::cast(node).unwrap(),
                ))
            }
            NodeKind::ConditionalVariableAssignment => Some(
                VariableAssignmentStatementSyntax::ConditionalVariableAssignment(
                    ConditionalVariableAssignmentSyntax::cast(node).unwrap(),
                ),
            ),
            NodeKind::SelectedVariableAssignment => Some(
                VariableAssignmentStatementSyntax::SelectedVariableAssignment(
                    SelectedVariableAssignmentSyntax::cast(node).unwrap(),
                ),
            ),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            VariableAssignmentStatementSyntax::SimpleVariableAssignment(inner) => inner.raw(),
            VariableAssignmentStatementSyntax::ConditionalVariableAssignment(inner) => inner.raw(),
            VariableAssignmentStatementSyntax::SelectedVariableAssignment(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct WaitStatementSyntax(pub(crate) SyntaxNode);
impl AstNode for WaitStatementSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::WaitStatement => Some(WaitStatementSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl WaitStatementSyntax {
    pub fn label(&self) -> Option<LabelSyntax> {
        self.0.children().filter_map(LabelSyntax::cast).nth(0)
    }
    pub fn wait_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Wait))
            .nth(0)
    }
    pub fn sensitivity_clause(&self) -> Option<SensitivityClauseSyntax> {
        self.0
            .children()
            .filter_map(SensitivityClauseSyntax::cast)
            .nth(0)
    }
    pub fn condition_clause(&self) -> Option<ConditionClauseSyntax> {
        self.0
            .children()
            .filter_map(ConditionClauseSyntax::cast)
            .nth(0)
    }
    pub fn timeout_clause(&self) -> Option<TimeoutClauseSyntax> {
        self.0
            .children()
            .filter_map(TimeoutClauseSyntax::cast)
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
pub struct WaveformElementSyntax(pub(crate) SyntaxNode);
impl AstNode for WaveformElementSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::WaveformElement => Some(WaveformElementSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl WaveformElementSyntax {
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn after_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::After))
            .nth(0)
    }
    pub fn time_expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(1)
    }
}
#[derive(Debug, Clone)]
pub struct WaveformElementsSyntax(pub(crate) SyntaxNode);
impl AstNode for WaveformElementsSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::WaveformElements => Some(WaveformElementsSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl WaveformElementsSyntax {
    pub fn waveform_elements(&self) -> impl Iterator<Item = WaveformElementSyntax> + use<'_> {
        self.0.children().filter_map(WaveformElementSyntax::cast)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.tokens().filter(|token| token.kind() == Comma)
    }
}
#[derive(Debug, Clone)]
pub struct UnaffectedWaveformSyntax(pub(crate) SyntaxNode);
impl AstNode for UnaffectedWaveformSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::UnaffectedWaveform => Some(UnaffectedWaveformSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl UnaffectedWaveformSyntax {
    pub fn unaffected_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Unaffected))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum WaveformSyntax {
    WaveformElements(WaveformElementsSyntax),
    UnaffectedWaveform(UnaffectedWaveformSyntax),
}
impl AstNode for WaveformSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::WaveformElements => Some(WaveformSyntax::WaveformElements(
                WaveformElementsSyntax::cast(node).unwrap(),
            )),
            NodeKind::UnaffectedWaveform => Some(WaveformSyntax::UnaffectedWaveform(
                UnaffectedWaveformSyntax::cast(node).unwrap(),
            )),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            WaveformSyntax::WaveformElements(inner) => inner.raw(),
            WaveformSyntax::UnaffectedWaveform(inner) => inner.raw(),
        }
    }
}
