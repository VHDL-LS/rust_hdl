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
pub struct DeclarationStatementSeparatorSyntax(pub(crate) SyntaxNode);
impl AstNode for DeclarationStatementSeparatorSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::DeclarationStatementSeparator => {
                Some(DeclarationStatementSeparatorSyntax(node))
            }
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::DeclarationStatementSeparator)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl DeclarationStatementSeparatorSyntax {
    pub fn begin_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Begin))
            .nth(0)
    }
}
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::SemiColonTerminatedGenericMapAspect)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::SemiColonTerminatedPortMapAspect)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::BlockHeader)
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
    pub fn semi_colon_terminated_generic_map_aspect(
        &self,
    ) -> Option<SemiColonTerminatedGenericMapAspectSyntax> {
        self.0
            .children()
            .filter_map(SemiColonTerminatedGenericMapAspectSyntax::cast)
            .nth(0)
    }
    pub fn port_clause(&self) -> Option<PortClauseSyntax> {
        self.0.children().filter_map(PortClauseSyntax::cast).nth(0)
    }
    pub fn semi_colon_terminated_port_map_aspect(
        &self,
    ) -> Option<SemiColonTerminatedPortMapAspectSyntax> {
        self.0
            .children()
            .filter_map(SemiColonTerminatedPortMapAspectSyntax::cast)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::BlockStatement)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl BlockStatementSyntax {
    pub fn block_preamble(&self) -> Option<BlockPreambleSyntax> {
        self.0
            .children()
            .filter_map(BlockPreambleSyntax::cast)
            .nth(0)
    }
    pub fn block_header(&self) -> Option<BlockHeaderSyntax> {
        self.0.children().filter_map(BlockHeaderSyntax::cast).nth(0)
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
    pub fn block_epilogue(&self) -> Option<BlockEpilogueSyntax> {
        self.0
            .children()
            .filter_map(BlockEpilogueSyntax::cast)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ParenthesizedExpression)
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
pub struct BlockPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for BlockPreambleSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::BlockPreamble => Some(BlockPreambleSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::BlockPreamble)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl BlockPreambleSyntax {
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
}
#[derive(Debug, Clone)]
pub struct BlockEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for BlockEpilogueSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::BlockEpilogue => Some(BlockEpilogueSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::BlockEpilogue)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl BlockEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::End))
            .nth(0)
    }
    pub fn block_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Block))
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
pub struct CaseGenerateAlternativeSyntax(pub(crate) SyntaxNode);
impl AstNode for CaseGenerateAlternativeSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::CaseGenerateAlternative => Some(CaseGenerateAlternativeSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::CaseGenerateAlternative)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::CaseGenerateStatement)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl CaseGenerateStatementSyntax {
    pub fn case_generate_statement_preamble(&self) -> Option<CaseGenerateStatementPreambleSyntax> {
        self.0
            .children()
            .filter_map(CaseGenerateStatementPreambleSyntax::cast)
            .nth(0)
    }
    pub fn case_generate_alternatives(
        &self,
    ) -> impl Iterator<Item = CaseGenerateAlternativeSyntax> + use<'_> {
        self.0
            .children()
            .filter_map(CaseGenerateAlternativeSyntax::cast)
    }
    pub fn case_generate_statement_epilogue(&self) -> Option<CaseGenerateStatementEpilogueSyntax> {
        self.0
            .children()
            .filter_map(CaseGenerateStatementEpilogueSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct CaseGenerateStatementPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for CaseGenerateStatementPreambleSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::CaseGenerateStatementPreamble => {
                Some(CaseGenerateStatementPreambleSyntax(node))
            }
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::CaseGenerateStatementPreamble)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl CaseGenerateStatementPreambleSyntax {
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
}
#[derive(Debug, Clone)]
pub struct CaseGenerateStatementEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for CaseGenerateStatementEpilogueSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::CaseGenerateStatementEpilogue => {
                Some(CaseGenerateStatementEpilogueSyntax(node))
            }
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::CaseGenerateStatementEpilogue)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl CaseGenerateStatementEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::End))
            .nth(0)
    }
    pub fn generate_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Generate))
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ComponentInstantiationStatement)
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
    pub fn component_instantiation_items(&self) -> Option<ComponentInstantiationItemsSyntax> {
        self.0
            .children()
            .filter_map(ComponentInstantiationItemsSyntax::cast)
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
pub struct ComponentInstantiationItemsSyntax(pub(crate) SyntaxNode);
impl AstNode for ComponentInstantiationItemsSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ComponentInstantiationItems => Some(ComponentInstantiationItemsSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ComponentInstantiationItems)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ComponentInstantiationItemsSyntax {
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(
            node.kind(),
            NodeKind::ConcurrentProcedureCallOrComponentInstantiationStatement
        )
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ConcurrentAssertionStatement)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ConcurrentSimpleSignalAssignment)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ConcurrentConditionalSignalAssignment)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ConcurrentSelectedSignalAssignment)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConcurrentSelectedSignalAssignmentSyntax {
    pub fn concurrent_selected_signal_assignment_preamble(
        &self,
    ) -> Option<ConcurrentSelectedSignalAssignmentPreambleSyntax> {
        self.0
            .children()
            .filter_map(ConcurrentSelectedSignalAssignmentPreambleSyntax::cast)
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
pub struct ConcurrentSelectedSignalAssignmentPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for ConcurrentSelectedSignalAssignmentPreambleSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ConcurrentSelectedSignalAssignmentPreamble => {
                Some(ConcurrentSelectedSignalAssignmentPreambleSyntax(node))
            }
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(
            node.kind(),
            NodeKind::ConcurrentSelectedSignalAssignmentPreamble
        )
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConcurrentSelectedSignalAssignmentPreambleSyntax {
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
        if BlockStatementSyntax::can_cast(&node) {
            return Some(ConcurrentStatementSyntax::BlockStatement(
                BlockStatementSyntax::cast(node).unwrap(),
            ));
        };
        if ProcessStatementSyntax::can_cast(&node) {
            return Some(ConcurrentStatementSyntax::ProcessStatement(
                ProcessStatementSyntax::cast(node).unwrap(),
            ));
        };
        if ConcurrentAssertionStatementSyntax::can_cast(&node) {
            return Some(ConcurrentStatementSyntax::ConcurrentAssertionStatement(
                ConcurrentAssertionStatementSyntax::cast(node).unwrap(),
            ));
        };
        if ComponentInstantiationStatementSyntax::can_cast(&node) {
            return Some(ConcurrentStatementSyntax::ComponentInstantiationStatement(
                ComponentInstantiationStatementSyntax::cast(node).unwrap(),
            ));
        };
        if ConcurrentSelectedSignalAssignmentSyntax::can_cast(&node) {
            return Some(
                ConcurrentStatementSyntax::ConcurrentSelectedSignalAssignment(
                    ConcurrentSelectedSignalAssignmentSyntax::cast(node).unwrap(),
                ),
            );
        };
        if ConcurrentConditionalSignalAssignmentSyntax::can_cast(&node) {
            return Some(
                ConcurrentStatementSyntax::ConcurrentConditionalSignalAssignment(
                    ConcurrentConditionalSignalAssignmentSyntax::cast(node).unwrap(),
                ),
            );
        };
        if ConcurrentSimpleSignalAssignmentSyntax::can_cast(&node) {
            return Some(ConcurrentStatementSyntax::ConcurrentSimpleSignalAssignment(
                ConcurrentSimpleSignalAssignmentSyntax::cast(node).unwrap(),
            ));
        };
        if ConcurrentProcedureCallOrComponentInstantiationStatementSyntax::can_cast(&node) {
            return Some(
                ConcurrentStatementSyntax::ConcurrentProcedureCallOrComponentInstantiationStatement(
                    ConcurrentProcedureCallOrComponentInstantiationStatementSyntax::cast(node)
                        .unwrap(),
                ),
            );
        };
        if ForGenerateStatementSyntax::can_cast(&node) {
            return Some(ConcurrentStatementSyntax::ForGenerateStatement(
                ForGenerateStatementSyntax::cast(node).unwrap(),
            ));
        };
        if IfGenerateStatementSyntax::can_cast(&node) {
            return Some(ConcurrentStatementSyntax::IfGenerateStatement(
                IfGenerateStatementSyntax::cast(node).unwrap(),
            ));
        };
        if CaseGenerateStatementSyntax::can_cast(&node) {
            return Some(ConcurrentStatementSyntax::CaseGenerateStatement(
                CaseGenerateStatementSyntax::cast(node).unwrap(),
            ));
        };
        if PslDirectiveSyntax::can_cast(&node) {
            return Some(ConcurrentStatementSyntax::PslDirective(
                PslDirectiveSyntax::cast(node).unwrap(),
            ));
        };
        None
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        BlockStatementSyntax::can_cast(node)
            || ProcessStatementSyntax::can_cast(node)
            || ConcurrentAssertionStatementSyntax::can_cast(node)
            || ComponentInstantiationStatementSyntax::can_cast(node)
            || ConcurrentSelectedSignalAssignmentSyntax::can_cast(node)
            || ConcurrentConditionalSignalAssignmentSyntax::can_cast(node)
            || ConcurrentSimpleSignalAssignmentSyntax::can_cast(node)
            || ConcurrentProcedureCallOrComponentInstantiationStatementSyntax::can_cast(node)
            || ForGenerateStatementSyntax::can_cast(node)
            || IfGenerateStatementSyntax::can_cast(node)
            || CaseGenerateStatementSyntax::can_cast(node)
            || PslDirectiveSyntax::can_cast(node)
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
pub struct ConcurrentStatementsSyntax(pub(crate) SyntaxNode);
impl AstNode for ConcurrentStatementsSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ConcurrentStatements => Some(ConcurrentStatementsSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ConcurrentStatements)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConcurrentStatementsSyntax {
    pub fn concurrent_statements(
        &self,
    ) -> impl Iterator<Item = ConcurrentStatementSyntax> + use<'_> {
        self.0
            .children()
            .filter_map(ConcurrentStatementSyntax::cast)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ForGenerateStatement)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ForGenerateStatementSyntax {
    pub fn for_generate_statement_preamble(&self) -> Option<ForGenerateStatementPreambleSyntax> {
        self.0
            .children()
            .filter_map(ForGenerateStatementPreambleSyntax::cast)
            .nth(0)
    }
    pub fn generate_statement_body(&self) -> Option<GenerateStatementBodySyntax> {
        self.0
            .children()
            .filter_map(GenerateStatementBodySyntax::cast)
            .nth(0)
    }
    pub fn for_generate_statement_epilogue(&self) -> Option<ForGenerateStatementEpilogueSyntax> {
        self.0
            .children()
            .filter_map(ForGenerateStatementEpilogueSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ForGenerateStatementPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for ForGenerateStatementPreambleSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ForGenerateStatementPreamble => {
                Some(ForGenerateStatementPreambleSyntax(node))
            }
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ForGenerateStatementPreamble)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ForGenerateStatementPreambleSyntax {
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
}
#[derive(Debug, Clone)]
pub struct ForGenerateStatementEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for ForGenerateStatementEpilogueSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ForGenerateStatementEpilogue => {
                Some(ForGenerateStatementEpilogueSyntax(node))
            }
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ForGenerateStatementEpilogue)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ForGenerateStatementEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::End))
            .nth(0)
    }
    pub fn generate_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Generate))
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
pub struct GenerateStatementBodySyntax(pub(crate) SyntaxNode);
impl AstNode for GenerateStatementBodySyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::GenerateStatementBody => Some(GenerateStatementBodySyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::GenerateStatementBody)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl GenerateStatementBodySyntax {
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
    pub fn generate_statement_body_epilogue(&self) -> Option<GenerateStatementBodyEpilogueSyntax> {
        self.0
            .children()
            .filter_map(GenerateStatementBodyEpilogueSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct GenerateStatementBodyEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for GenerateStatementBodyEpilogueSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::GenerateStatementBodyEpilogue => {
                Some(GenerateStatementBodyEpilogueSyntax(node))
            }
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::GenerateStatementBodyEpilogue)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl GenerateStatementBodyEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::End))
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
pub struct IfGenerateElsifSyntax(pub(crate) SyntaxNode);
impl AstNode for IfGenerateElsifSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::IfGenerateElsif => Some(IfGenerateElsifSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::IfGenerateElsif)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::IfGenerateElse)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::IfGenerateStatement)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl IfGenerateStatementSyntax {
    pub fn if_generate_statement_preamble(&self) -> Option<IfGenerateStatementPreambleSyntax> {
        self.0
            .children()
            .filter_map(IfGenerateStatementPreambleSyntax::cast)
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
    pub fn if_generate_statement_epilogue(&self) -> Option<IfGenerateStatementEpilogueSyntax> {
        self.0
            .children()
            .filter_map(IfGenerateStatementEpilogueSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct IfGenerateStatementPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for IfGenerateStatementPreambleSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::IfGenerateStatementPreamble => Some(IfGenerateStatementPreambleSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::IfGenerateStatementPreamble)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl IfGenerateStatementPreambleSyntax {
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
}
#[derive(Debug, Clone)]
pub struct IfGenerateStatementEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for IfGenerateStatementEpilogueSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::IfGenerateStatementEpilogue => Some(IfGenerateStatementEpilogueSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::IfGenerateStatementEpilogue)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl IfGenerateStatementEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::End))
            .nth(0)
    }
    pub fn generate_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Generate))
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
pub enum InstantiatedUnitSyntax {
    ComponentInstantiatedUnit(ComponentInstantiatedUnitSyntax),
    EntityInstantiatedUnit(EntityInstantiatedUnitSyntax),
    ConfigurationInstantiatedUnit(ConfigurationInstantiatedUnitSyntax),
}
impl AstNode for InstantiatedUnitSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        if ComponentInstantiatedUnitSyntax::can_cast(&node) {
            return Some(InstantiatedUnitSyntax::ComponentInstantiatedUnit(
                ComponentInstantiatedUnitSyntax::cast(node).unwrap(),
            ));
        };
        if EntityInstantiatedUnitSyntax::can_cast(&node) {
            return Some(InstantiatedUnitSyntax::EntityInstantiatedUnit(
                EntityInstantiatedUnitSyntax::cast(node).unwrap(),
            ));
        };
        if ConfigurationInstantiatedUnitSyntax::can_cast(&node) {
            return Some(InstantiatedUnitSyntax::ConfigurationInstantiatedUnit(
                ConfigurationInstantiatedUnitSyntax::cast(node).unwrap(),
            ));
        };
        None
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        ComponentInstantiatedUnitSyntax::can_cast(node)
            || EntityInstantiatedUnitSyntax::can_cast(node)
            || ConfigurationInstantiatedUnitSyntax::can_cast(node)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ComponentInstantiatedUnit)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::EntityInstantiatedUnit)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ConfigurationInstantiatedUnit)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::AllSensitivityList)
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
pub struct SensitivityListSyntax(pub(crate) SyntaxNode);
impl AstNode for SensitivityListSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SensitivityList => Some(SensitivityListSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::SensitivityList)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SensitivityListSyntax {
    pub fn names(&self) -> impl Iterator<Item = NameSyntax> + use<'_> {
        self.0.children().filter_map(NameSyntax::cast)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.tokens().filter(|token| token.kind() == Comma)
    }
}
#[derive(Debug, Clone)]
pub enum ProcessSensitivityListSyntax {
    AllSensitivityList(AllSensitivityListSyntax),
    SensitivityList(SensitivityListSyntax),
}
impl AstNode for ProcessSensitivityListSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        if AllSensitivityListSyntax::can_cast(&node) {
            return Some(ProcessSensitivityListSyntax::AllSensitivityList(
                AllSensitivityListSyntax::cast(node).unwrap(),
            ));
        };
        if SensitivityListSyntax::can_cast(&node) {
            return Some(ProcessSensitivityListSyntax::SensitivityList(
                SensitivityListSyntax::cast(node).unwrap(),
            ));
        };
        None
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        AllSensitivityListSyntax::can_cast(node) || SensitivityListSyntax::can_cast(node)
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            ProcessSensitivityListSyntax::AllSensitivityList(inner) => inner.raw(),
            ProcessSensitivityListSyntax::SensitivityList(inner) => inner.raw(),
        }
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ProcessStatement)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ProcessStatementSyntax {
    pub fn process_statement_preamble(&self) -> Option<ProcessStatementPreambleSyntax> {
        self.0
            .children()
            .filter_map(ProcessStatementPreambleSyntax::cast)
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
    pub fn process_statement_epilogue(&self) -> Option<ProcessStatementEpilogueSyntax> {
        self.0
            .children()
            .filter_map(ProcessStatementEpilogueSyntax::cast)
            .nth(0)
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
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ParenthesizedProcessSensitivityList)
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
pub struct ProcessStatementPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for ProcessStatementPreambleSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ProcessStatementPreamble => Some(ProcessStatementPreambleSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ProcessStatementPreamble)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ProcessStatementPreambleSyntax {
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
}
#[derive(Debug, Clone)]
pub struct ProcessStatementEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for ProcessStatementEpilogueSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ProcessStatementEpilogue => Some(ProcessStatementEpilogueSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ProcessStatementEpilogue)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ProcessStatementEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::End))
            .nth(0)
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
