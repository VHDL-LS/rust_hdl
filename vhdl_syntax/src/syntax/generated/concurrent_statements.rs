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
pub struct SemiColonTerminatedGenericMapAspectSyntax(pub(crate) SyntaxNode);
impl AstNode for SemiColonTerminatedGenericMapAspectSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SemiColonTerminatedGenericMapAspect => {
                Some(SemiColonTerminatedGenericMapAspectSyntax(node))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SemiColonTerminatedGenericMapAspectSyntax {
    pub fn generic_map_aspect(&self) -> Option<GenericMapAspectSyntax> {
        self.0
            .children()
            .filter_map(GenericMapAspectSyntax::cast)
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
pub struct SemiColonTerminatedPortMapAspectSyntax(pub(crate) SyntaxNode);
impl AstNode for SemiColonTerminatedPortMapAspectSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SemiColonTerminatedPortMapAspect => {
                Some(SemiColonTerminatedPortMapAspectSyntax(node))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SemiColonTerminatedPortMapAspectSyntax {
    pub fn port_map_aspect(&self) -> Option<PortMapAspectSyntax> {
        self.0
            .children()
            .filter_map(PortMapAspectSyntax::cast)
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
pub struct BlockHeaderSyntax(pub(crate) SyntaxNode);
impl AstNode for BlockHeaderSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::BlockHeader => Some(BlockHeaderSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl BlockHeaderSyntax {
    pub fn generic_clause(&self) -> Option<GenericClauseSyntax> {
        self.0
            .children()
            .filter_map(GenericClauseSyntax::cast)
            .nth(0)
    }
    pub fn generic_map_aspect(&self) -> Option<SemiColonTerminatedGenericMapAspectSyntax> {
        self.0
            .children()
            .filter_map(SemiColonTerminatedGenericMapAspectSyntax::cast)
            .nth(0)
    }
    pub fn port_clause(&self) -> Option<PortClauseSyntax> {
        self.0.children().filter_map(PortClauseSyntax::cast).nth(0)
    }
    pub fn port_map_aspect(&self) -> Option<SemiColonTerminatedPortMapAspectSyntax> {
        self.0
            .children()
            .filter_map(SemiColonTerminatedPortMapAspectSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ParenthesizedExpressionSyntax(pub(crate) SyntaxNode);
impl AstNode for ParenthesizedExpressionSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ParenthesizedExpression => Some(ParenthesizedExpressionSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ParenthesizedExpressionSyntax {
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == LeftPar)
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn right_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == RightPar)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct BlockStatementSyntax(pub(crate) SyntaxNode);
impl AstNode for BlockStatementSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::BlockStatement => Some(BlockStatementSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl BlockStatementSyntax {
    pub fn label(&self) -> Option<LabelSyntax> {
        self.0.children().filter_map(LabelSyntax::cast).nth(0)
    }
    pub fn block_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Block))
            .nth(0)
    }
    pub fn condition(&self) -> Option<ParenthesizedExpressionSyntax> {
        self.0
            .children()
            .filter_map(ParenthesizedExpressionSyntax::cast)
            .nth(0)
    }
    pub fn is_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Is))
            .nth(0)
    }
    pub fn block_header(&self) -> Option<BlockHeaderSyntax> {
        self.0.children().filter_map(BlockHeaderSyntax::cast).nth(0)
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
    pub fn trailing_block_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Block))
            .nth(1)
    }
    pub fn trailing_label_token(&self) -> Option<SyntaxToken> {
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
pub struct CaseGenerateAlternativeSyntax(pub(crate) SyntaxNode);
impl AstNode for CaseGenerateAlternativeSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::CaseGenerateAlternative => Some(CaseGenerateAlternativeSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl CaseGenerateAlternativeSyntax {
    pub fn when_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::When))
            .nth(0)
    }
    pub fn label(&self) -> Option<LabelSyntax> {
        self.0.children().filter_map(LabelSyntax::cast).nth(0)
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
    pub fn generate_statement_body(&self) -> Option<GenerateStatementBodySyntax> {
        self.0
            .children()
            .filter_map(GenerateStatementBodySyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct CaseGenerateStatementSyntax(pub(crate) SyntaxNode);
impl AstNode for CaseGenerateStatementSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::CaseGenerateStatement => Some(CaseGenerateStatementSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl CaseGenerateStatementSyntax {
    pub fn label(&self) -> Option<LabelSyntax> {
        self.0.children().filter_map(LabelSyntax::cast).nth(0)
    }
    pub fn case_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Case))
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn generate_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Generate))
            .nth(0)
    }
    pub fn case_generate_alternatives(
        &self,
    ) -> impl Iterator<Item = CaseGenerateAlternativeSyntax> + use<'_> {
        self.0
            .children()
            .filter_map(CaseGenerateAlternativeSyntax::cast)
    }
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::End))
            .nth(0)
    }
    pub fn trailing_generate_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Generate))
            .nth(1)
    }
    pub fn trailing_generate_label_token(&self) -> Option<SyntaxToken> {
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
pub struct ComponentInstantiationStatementSyntax(pub(crate) SyntaxNode);
impl AstNode for ComponentInstantiationStatementSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ComponentInstantiationStatement => {
                Some(ComponentInstantiationStatementSyntax(node))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ComponentInstantiationStatementSyntax {
    pub fn label(&self) -> Option<LabelSyntax> {
        self.0.children().filter_map(LabelSyntax::cast).nth(0)
    }
    pub fn instantiated_unit(&self) -> Option<InstantiatedUnitSyntax> {
        self.0
            .children()
            .filter_map(InstantiatedUnitSyntax::cast)
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
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ConcurrentProcedureCallOrComponentInstantiationStatementSyntax(pub(crate) SyntaxNode);
impl AstNode for ConcurrentProcedureCallOrComponentInstantiationStatementSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ConcurrentProcedureCallOrComponentInstantiationStatement => {
                Some(ConcurrentProcedureCallOrComponentInstantiationStatementSyntax(node))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConcurrentProcedureCallOrComponentInstantiationStatementSyntax {
    pub fn label(&self) -> Option<LabelSyntax> {
        self.0.children().filter_map(LabelSyntax::cast).nth(0)
    }
    pub fn postponed_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Postponed))
            .nth(0)
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
pub struct ConcurrentAssertionStatementSyntax(pub(crate) SyntaxNode);
impl AstNode for ConcurrentAssertionStatementSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ConcurrentAssertionStatement => {
                Some(ConcurrentAssertionStatementSyntax(node))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConcurrentAssertionStatementSyntax {
    pub fn label(&self) -> Option<LabelSyntax> {
        self.0.children().filter_map(LabelSyntax::cast).nth(0)
    }
    pub fn postponed_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Postponed))
            .nth(0)
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
pub struct ConcurrentSimpleSignalAssignmentSyntax(pub(crate) SyntaxNode);
impl AstNode for ConcurrentSimpleSignalAssignmentSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ConcurrentSimpleSignalAssignment => {
                Some(ConcurrentSimpleSignalAssignmentSyntax(node))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConcurrentSimpleSignalAssignmentSyntax {
    pub fn label(&self) -> Option<LabelSyntax> {
        self.0.children().filter_map(LabelSyntax::cast).nth(0)
    }
    pub fn postponed_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Postponed))
            .nth(0)
    }
    pub fn target(&self) -> Option<TargetSyntax> {
        self.0.children().filter_map(TargetSyntax::cast).nth(0)
    }
    pub fn lte_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == LTE).nth(0)
    }
    pub fn guarded_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Guarded))
            .nth(0)
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
pub struct ConcurrentConditionalSignalAssignmentSyntax(pub(crate) SyntaxNode);
impl AstNode for ConcurrentConditionalSignalAssignmentSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ConcurrentConditionalSignalAssignment => {
                Some(ConcurrentConditionalSignalAssignmentSyntax(node))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConcurrentConditionalSignalAssignmentSyntax {
    pub fn label(&self) -> Option<LabelSyntax> {
        self.0.children().filter_map(LabelSyntax::cast).nth(0)
    }
    pub fn postponed_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Postponed))
            .nth(0)
    }
    pub fn target(&self) -> Option<TargetSyntax> {
        self.0.children().filter_map(TargetSyntax::cast).nth(0)
    }
    pub fn lte_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == LTE).nth(0)
    }
    pub fn guarded_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Guarded))
            .nth(0)
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
pub struct ConcurrentSelectedSignalAssignmentSyntax(pub(crate) SyntaxNode);
impl AstNode for ConcurrentSelectedSignalAssignmentSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ConcurrentSelectedSignalAssignment => {
                Some(ConcurrentSelectedSignalAssignmentSyntax(node))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConcurrentSelectedSignalAssignmentSyntax {
    pub fn label(&self) -> Option<LabelSyntax> {
        self.0.children().filter_map(LabelSyntax::cast).nth(0)
    }
    pub fn postponed_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Postponed))
            .nth(0)
    }
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
    pub fn guarded_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Guarded))
            .nth(0)
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
pub enum ConcurrentStatementSyntax {
    BlockStatement(BlockStatementSyntax),
    ProcessStatement(ProcessStatementSyntax),
    ConcurrentAssertionStatement(ConcurrentAssertionStatementSyntax),
    ComponentInstantiationStatement(ComponentInstantiationStatementSyntax),
    ConcurrentSelectedSignalAssignment(ConcurrentSelectedSignalAssignmentSyntax),
    ConcurrentConditionalSignalAssignment(ConcurrentConditionalSignalAssignmentSyntax),
    ConcurrentSimpleSignalAssignment(ConcurrentSimpleSignalAssignmentSyntax),
    ConcurrentProcedureCallOrComponentInstantiationStatement(
        ConcurrentProcedureCallOrComponentInstantiationStatementSyntax,
    ),
    ForGenerateStatement(ForGenerateStatementSyntax),
    IfGenerateStatement(IfGenerateStatementSyntax),
    CaseGenerateStatement(CaseGenerateStatementSyntax),
    PslDirective(PslDirectiveSyntax),
}
impl AstNode for ConcurrentStatementSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::BlockStatement => Some(ConcurrentStatementSyntax::BlockStatement(
                BlockStatementSyntax::cast(node).unwrap(),
            )),
            NodeKind::ProcessStatement => Some(ConcurrentStatementSyntax::ProcessStatement(
                ProcessStatementSyntax::cast(node).unwrap(),
            )),
            NodeKind::ConcurrentAssertionStatement => {
                Some(ConcurrentStatementSyntax::ConcurrentAssertionStatement(
                    ConcurrentAssertionStatementSyntax::cast(node).unwrap(),
                ))
            }
            NodeKind::ComponentInstantiationStatement => {
                Some(ConcurrentStatementSyntax::ComponentInstantiationStatement(
                    ComponentInstantiationStatementSyntax::cast(node).unwrap(),
                ))
            }
            NodeKind::ConcurrentSelectedSignalAssignment => Some(
                ConcurrentStatementSyntax::ConcurrentSelectedSignalAssignment(
                    ConcurrentSelectedSignalAssignmentSyntax::cast(node).unwrap(),
                ),
            ),
            NodeKind::ConcurrentConditionalSignalAssignment => Some(
                ConcurrentStatementSyntax::ConcurrentConditionalSignalAssignment(
                    ConcurrentConditionalSignalAssignmentSyntax::cast(node).unwrap(),
                ),
            ),
            NodeKind::ConcurrentSimpleSignalAssignment => {
                Some(ConcurrentStatementSyntax::ConcurrentSimpleSignalAssignment(
                    ConcurrentSimpleSignalAssignmentSyntax::cast(node).unwrap(),
                ))
            }
            NodeKind::ConcurrentProcedureCallOrComponentInstantiationStatement => Some(
                ConcurrentStatementSyntax::ConcurrentProcedureCallOrComponentInstantiationStatement(
                    ConcurrentProcedureCallOrComponentInstantiationStatementSyntax::cast(node)
                        .unwrap(),
                ),
            ),
            NodeKind::ForGenerateStatement => {
                Some(ConcurrentStatementSyntax::ForGenerateStatement(
                    ForGenerateStatementSyntax::cast(node).unwrap(),
                ))
            }
            NodeKind::IfGenerateStatement => Some(ConcurrentStatementSyntax::IfGenerateStatement(
                IfGenerateStatementSyntax::cast(node).unwrap(),
            )),
            NodeKind::CaseGenerateStatement => {
                Some(ConcurrentStatementSyntax::CaseGenerateStatement(
                    CaseGenerateStatementSyntax::cast(node).unwrap(),
                ))
            }
            NodeKind::PslDirective => Some(ConcurrentStatementSyntax::PslDirective(
                PslDirectiveSyntax::cast(node).unwrap(),
            )),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            ConcurrentStatementSyntax::BlockStatement(inner) => inner.raw(),
            ConcurrentStatementSyntax::ProcessStatement(inner) => inner.raw(),
            ConcurrentStatementSyntax::ConcurrentAssertionStatement(inner) => inner.raw(),
            ConcurrentStatementSyntax::ComponentInstantiationStatement(inner) => inner.raw(),
            ConcurrentStatementSyntax::ConcurrentSelectedSignalAssignment(inner) => inner.raw(),
            ConcurrentStatementSyntax::ConcurrentConditionalSignalAssignment(inner) => inner.raw(),
            ConcurrentStatementSyntax::ConcurrentSimpleSignalAssignment(inner) => inner.raw(),
            ConcurrentStatementSyntax::ConcurrentProcedureCallOrComponentInstantiationStatement(
                inner,
            ) => inner.raw(),
            ConcurrentStatementSyntax::ForGenerateStatement(inner) => inner.raw(),
            ConcurrentStatementSyntax::IfGenerateStatement(inner) => inner.raw(),
            ConcurrentStatementSyntax::CaseGenerateStatement(inner) => inner.raw(),
            ConcurrentStatementSyntax::PslDirective(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ForGenerateStatementSyntax(pub(crate) SyntaxNode);
impl AstNode for ForGenerateStatementSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ForGenerateStatement => Some(ForGenerateStatementSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ForGenerateStatementSyntax {
    pub fn label(&self) -> Option<LabelSyntax> {
        self.0.children().filter_map(LabelSyntax::cast).nth(0)
    }
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
    pub fn generate_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Generate))
            .nth(0)
    }
    pub fn generate_statement_body(&self) -> Option<GenerateStatementBodySyntax> {
        self.0
            .children()
            .filter_map(GenerateStatementBodySyntax::cast)
            .nth(0)
    }
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::End))
            .nth(0)
    }
    pub fn trailing_generate_token_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Generate))
            .nth(1)
    }
    pub fn trailing_generate_label_token(&self) -> Option<SyntaxToken> {
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
pub struct GenerateStatementBodySyntax(pub(crate) SyntaxNode);
impl AstNode for GenerateStatementBodySyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::GenerateStatementBody => Some(GenerateStatementBodySyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl GenerateStatementBodySyntax {
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
    pub fn alternative_label_token(&self) -> Option<SyntaxToken> {
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
pub struct IfGenerateElsifSyntax(pub(crate) SyntaxNode);
impl AstNode for IfGenerateElsifSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::IfGenerateElsif => Some(IfGenerateElsifSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl IfGenerateElsifSyntax {
    pub fn elsif_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Elsif))
            .nth(0)
    }
    pub fn label(&self) -> Option<LabelSyntax> {
        self.0.children().filter_map(LabelSyntax::cast).nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn generate_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Generate))
            .nth(0)
    }
    pub fn generate_statement_body(&self) -> Option<GenerateStatementBodySyntax> {
        self.0
            .children()
            .filter_map(GenerateStatementBodySyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct IfGenerateElseSyntax(pub(crate) SyntaxNode);
impl AstNode for IfGenerateElseSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::IfGenerateElse => Some(IfGenerateElseSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl IfGenerateElseSyntax {
    pub fn else_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Else))
            .nth(0)
    }
    pub fn label(&self) -> Option<LabelSyntax> {
        self.0.children().filter_map(LabelSyntax::cast).nth(0)
    }
    pub fn generate_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Generate))
            .nth(0)
    }
    pub fn generate_statement_body(&self) -> Option<GenerateStatementBodySyntax> {
        self.0
            .children()
            .filter_map(GenerateStatementBodySyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct IfGenerateStatementSyntax(pub(crate) SyntaxNode);
impl AstNode for IfGenerateStatementSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::IfGenerateStatement => Some(IfGenerateStatementSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl IfGenerateStatementSyntax {
    pub fn label(&self) -> Option<LabelSyntax> {
        self.0.children().filter_map(LabelSyntax::cast).nth(0)
    }
    pub fn if_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::If))
            .nth(0)
    }
    pub fn alternative_label(&self) -> Option<LabelSyntax> {
        self.0.children().filter_map(LabelSyntax::cast).nth(1)
    }
    pub fn condition(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn generate_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Generate))
            .nth(0)
    }
    pub fn generate_statement_body(&self) -> Option<GenerateStatementBodySyntax> {
        self.0
            .children()
            .filter_map(GenerateStatementBodySyntax::cast)
            .nth(0)
    }
    pub fn if_generate_elsifs(&self) -> impl Iterator<Item = IfGenerateElsifSyntax> + use<'_> {
        self.0.children().filter_map(IfGenerateElsifSyntax::cast)
    }
    pub fn if_generate_else(&self) -> Option<IfGenerateElseSyntax> {
        self.0
            .children()
            .filter_map(IfGenerateElseSyntax::cast)
            .nth(0)
    }
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::End))
            .nth(0)
    }
    pub fn trailing_generate_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Generate))
            .nth(1)
    }
    pub fn trailing_generate_label_token(&self) -> Option<SyntaxToken> {
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
pub enum InstantiatedUnitSyntax {
    ComponentInstantiatedUnit(ComponentInstantiatedUnitSyntax),
    EntityInstantiatedUnit(EntityInstantiatedUnitSyntax),
    ConfigurationInstantiatedUnit(ConfigurationInstantiatedUnitSyntax),
}
impl AstNode for InstantiatedUnitSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ComponentInstantiatedUnit => {
                Some(InstantiatedUnitSyntax::ComponentInstantiatedUnit(
                    ComponentInstantiatedUnitSyntax::cast(node).unwrap(),
                ))
            }
            NodeKind::EntityInstantiatedUnit => {
                Some(InstantiatedUnitSyntax::EntityInstantiatedUnit(
                    EntityInstantiatedUnitSyntax::cast(node).unwrap(),
                ))
            }
            NodeKind::ConfigurationInstantiatedUnit => {
                Some(InstantiatedUnitSyntax::ConfigurationInstantiatedUnit(
                    ConfigurationInstantiatedUnitSyntax::cast(node).unwrap(),
                ))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            InstantiatedUnitSyntax::ComponentInstantiatedUnit(inner) => inner.raw(),
            InstantiatedUnitSyntax::EntityInstantiatedUnit(inner) => inner.raw(),
            InstantiatedUnitSyntax::ConfigurationInstantiatedUnit(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ComponentInstantiatedUnitSyntax(pub(crate) SyntaxNode);
impl AstNode for ComponentInstantiatedUnitSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ComponentInstantiatedUnit => Some(ComponentInstantiatedUnitSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ComponentInstantiatedUnitSyntax {
    pub fn component_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Component))
            .nth(0)
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct EntityInstantiatedUnitSyntax(pub(crate) SyntaxNode);
impl AstNode for EntityInstantiatedUnitSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::EntityInstantiatedUnit => Some(EntityInstantiatedUnitSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl EntityInstantiatedUnitSyntax {
    pub fn entity_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Entity))
            .nth(0)
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == LeftPar)
            .nth(0)
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Identifier)
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
pub struct ConfigurationInstantiatedUnitSyntax(pub(crate) SyntaxNode);
impl AstNode for ConfigurationInstantiatedUnitSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ConfigurationInstantiatedUnit => {
                Some(ConfigurationInstantiatedUnitSyntax(node))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConfigurationInstantiatedUnitSyntax {
    pub fn configuration_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Configuration))
            .nth(0)
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct AllSensitivityListSyntax(pub(crate) SyntaxNode);
impl AstNode for AllSensitivityListSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::AllSensitivityList => Some(AllSensitivityListSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl AllSensitivityListSyntax {
    pub fn all_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::All))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum ProcessSensitivityListSyntax {
    AllSensitivityList(AllSensitivityListSyntax),
    NameList(NameListSyntax),
}
impl AstNode for ProcessSensitivityListSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::AllSensitivityList => Some(ProcessSensitivityListSyntax::AllSensitivityList(
                AllSensitivityListSyntax::cast(node).unwrap(),
            )),
            NodeKind::NameList => Some(ProcessSensitivityListSyntax::NameList(
                NameListSyntax::cast(node).unwrap(),
            )),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            ProcessSensitivityListSyntax::AllSensitivityList(inner) => inner.raw(),
            ProcessSensitivityListSyntax::NameList(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParenthesizedProcessSensitivityListSyntax(pub(crate) SyntaxNode);
impl AstNode for ParenthesizedProcessSensitivityListSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ParenthesizedProcessSensitivityList => {
                Some(ParenthesizedProcessSensitivityListSyntax(node))
            }
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ParenthesizedProcessSensitivityListSyntax {
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == LeftPar)
            .nth(0)
    }
    pub fn process_sensitivity_list(&self) -> Option<ProcessSensitivityListSyntax> {
        self.0
            .children()
            .filter_map(ProcessSensitivityListSyntax::cast)
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
pub struct ProcessStatementSyntax(pub(crate) SyntaxNode);
impl AstNode for ProcessStatementSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ProcessStatement => Some(ProcessStatementSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ProcessStatementSyntax {
    pub fn label(&self) -> Option<LabelSyntax> {
        self.0.children().filter_map(LabelSyntax::cast).nth(0)
    }
    pub fn postponed_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Postponed))
            .nth(0)
    }
    pub fn process_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Process))
            .nth(0)
    }
    pub fn parenthesized_process_sensitivity_list(
        &self,
    ) -> Option<ParenthesizedProcessSensitivityListSyntax> {
        self.0
            .children()
            .filter_map(ParenthesizedProcessSensitivityListSyntax::cast)
            .nth(0)
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
    pub fn trailing_postponed_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Postponed))
            .nth(1)
    }
    pub fn trailing_process_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Process))
            .nth(1)
    }
    pub fn trailing_process_label_token(&self) -> Option<SyntaxToken> {
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
