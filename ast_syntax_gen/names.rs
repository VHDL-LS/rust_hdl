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
pub struct AbsolutePathnameSyntax(pub(crate) SyntaxNode);
impl AstNode for AbsolutePathnameSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::AbsolutePathname => Some(AbsolutePathnameSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl AbsolutePathnameSyntax {
    pub fn dot_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == Dot).nth(0)
    }
    pub fn partial_pathname(&self) -> Option<PartialPathnameSyntax> {
        self.0
            .children()
            .filter_map(PartialPathnameSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct AttributeNameSyntax(pub(crate) SyntaxNode);
impl AstNode for AttributeNameSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::AttributeName => Some(AttributeNameSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl AttributeNameSyntax {
    pub fn left_square_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == LeftSquare)
            .nth(0)
    }
    pub fn signature(&self) -> Option<SignatureSyntax> {
        self.0.children().filter_map(SignatureSyntax::cast).nth(0)
    }
    pub fn right_square_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == RightSquare)
            .nth(0)
    }
    pub fn tick_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == Tick).nth(0)
    }
    pub fn attribute_designator_token_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Identifier)
            .nth(0)
    }
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
pub enum ExternalNameSyntax {
    ExternalConstantName(ExternalConstantNameSyntax),
    ExternalSignalName(ExternalSignalNameSyntax),
    ExternalVariableName(ExternalVariableNameSyntax),
}
impl AstNode for ExternalNameSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ExternalConstantName => Some(ExternalNameSyntax::ExternalConstantName(
                ExternalConstantNameSyntax::cast(node).unwrap(),
            )),
            NodeKind::ExternalSignalName => Some(ExternalNameSyntax::ExternalSignalName(
                ExternalSignalNameSyntax::cast(node).unwrap(),
            )),
            NodeKind::ExternalVariableName => Some(ExternalNameSyntax::ExternalVariableName(
                ExternalVariableNameSyntax::cast(node).unwrap(),
            )),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            ExternalNameSyntax::ExternalConstantName(inner) => inner.raw(),
            ExternalNameSyntax::ExternalSignalName(inner) => inner.raw(),
            ExternalNameSyntax::ExternalVariableName(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExternalConstantNameSyntax(pub(crate) SyntaxNode);
impl AstNode for ExternalConstantNameSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ExternalConstantName => Some(ExternalConstantNameSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ExternalConstantNameSyntax {
    pub fn lt_lt_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == LtLt).nth(0)
    }
    pub fn constant_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Constant))
            .nth(0)
    }
    pub fn external_path_name(&self) -> Option<ExternalPathNameSyntax> {
        self.0
            .children()
            .filter_map(ExternalPathNameSyntax::cast)
            .nth(0)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == Colon).nth(0)
    }
    pub fn subtype_indication(&self) -> Option<SubtypeIndicationSyntax> {
        self.0
            .children()
            .filter_map(SubtypeIndicationSyntax::cast)
            .nth(0)
    }
    pub fn gt_gt_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == GtGt).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ExternalSignalNameSyntax(pub(crate) SyntaxNode);
impl AstNode for ExternalSignalNameSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ExternalSignalName => Some(ExternalSignalNameSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ExternalSignalNameSyntax {
    pub fn lt_lt_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == LtLt).nth(0)
    }
    pub fn signal_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Signal))
            .nth(0)
    }
    pub fn external_path_name(&self) -> Option<ExternalPathNameSyntax> {
        self.0
            .children()
            .filter_map(ExternalPathNameSyntax::cast)
            .nth(0)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == Colon).nth(0)
    }
    pub fn subtype_indication(&self) -> Option<SubtypeIndicationSyntax> {
        self.0
            .children()
            .filter_map(SubtypeIndicationSyntax::cast)
            .nth(0)
    }
    pub fn gt_gt_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == GtGt).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ExternalVariableNameSyntax(pub(crate) SyntaxNode);
impl AstNode for ExternalVariableNameSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ExternalVariableName => Some(ExternalVariableNameSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ExternalVariableNameSyntax {
    pub fn lt_lt_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == LtLt).nth(0)
    }
    pub fn variable_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Variable))
            .nth(0)
    }
    pub fn external_path_name(&self) -> Option<ExternalPathNameSyntax> {
        self.0
            .children()
            .filter_map(ExternalPathNameSyntax::cast)
            .nth(0)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == Colon).nth(0)
    }
    pub fn subtype_indication(&self) -> Option<SubtypeIndicationSyntax> {
        self.0
            .children()
            .filter_map(SubtypeIndicationSyntax::cast)
            .nth(0)
    }
    pub fn gt_gt_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == GtGt).nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum ExternalPathNameSyntax {
    PackagePathname(PackagePathnameSyntax),
    AbsolutePathname(AbsolutePathnameSyntax),
    RelativePathname(RelativePathnameSyntax),
}
impl AstNode for ExternalPathNameSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::PackagePathname => Some(ExternalPathNameSyntax::PackagePathname(
                PackagePathnameSyntax::cast(node).unwrap(),
            )),
            NodeKind::AbsolutePathname => Some(ExternalPathNameSyntax::AbsolutePathname(
                AbsolutePathnameSyntax::cast(node).unwrap(),
            )),
            NodeKind::RelativePathname => Some(ExternalPathNameSyntax::RelativePathname(
                RelativePathnameSyntax::cast(node).unwrap(),
            )),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            ExternalPathNameSyntax::PackagePathname(inner) => inner.raw(),
            ExternalPathNameSyntax::AbsolutePathname(inner) => inner.raw(),
            ExternalPathNameSyntax::RelativePathname(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct PackagePathnameSyntax(pub(crate) SyntaxNode);
impl AstNode for PackagePathnameSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::PackagePathname => Some(PackagePathnameSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PackagePathnameSyntax {
    pub fn comm_at_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == CommAt)
            .nth(0)
    }
    pub fn dot_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.tokens().filter(|token| token.kind() == Dot)
    }
    pub fn simple_name_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.tokens().filter(|token| token.kind() == Identifier)
    }
}
#[derive(Debug, Clone)]
pub struct RelativePathnameSyntax(pub(crate) SyntaxNode);
impl AstNode for RelativePathnameSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::RelativePathname => Some(RelativePathnameSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl RelativePathnameSyntax {
    pub fn circ_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.tokens().filter(|token| token.kind() == Circ)
    }
    pub fn dot_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.tokens().filter(|token| token.kind() == Dot)
    }
    pub fn partial_pathname(&self) -> Option<PartialPathnameSyntax> {
        self.0
            .children()
            .filter_map(PartialPathnameSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct PartialPathnameSyntax(pub(crate) SyntaxNode);
impl AstNode for PartialPathnameSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::PartialPathname => Some(PartialPathnameSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PartialPathnameSyntax {
    pub fn parenthesized_expression_or_aggregates(
        &self,
    ) -> impl Iterator<Item = ParenthesizedExpressionOrAggregateSyntax> + use<'_> {
        self.0
            .children()
            .filter_map(ParenthesizedExpressionOrAggregateSyntax::cast)
    }
    pub fn dot_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.tokens().filter(|token| token.kind() == Dot)
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Identifier)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct NameListSyntax(pub(crate) SyntaxNode);
impl AstNode for NameListSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::NameList => Some(NameListSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl NameListSyntax {
    pub fn names(&self) -> impl Iterator<Item = NameSyntax> + use<'_> {
        self.0.children().filter_map(NameSyntax::cast)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.tokens().filter(|token| token.kind() == Comma)
    }
}
#[derive(Debug, Clone)]
pub struct LabelSyntax(pub(crate) SyntaxNode);
impl AstNode for LabelSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::Label => Some(LabelSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl LabelSyntax {
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Identifier)
            .nth(0)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == Colon).nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum DesignatorSyntax {
    Identifier(SyntaxToken),
    StringLiteral(SyntaxToken),
}
impl DesignatorSyntax {
    pub fn cast(token: SyntaxToken) -> Option<Self> {
        match token.kind() {
            Identifier => Some(DesignatorSyntax::Identifier(token)),
            StringLiteral => Some(DesignatorSyntax::StringLiteral(token)),
            _ => None,
        }
    }
    pub fn raw(&self) -> SyntaxToken {
        match self {
            DesignatorSyntax::Identifier(token) => token.clone(),
            DesignatorSyntax::StringLiteral(token) => token.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum NameDesignatorSyntax {
    Identifier(SyntaxToken),
    StringLiteral(SyntaxToken),
    CharacterLiteral(SyntaxToken),
}
impl NameDesignatorSyntax {
    pub fn cast(token: SyntaxToken) -> Option<Self> {
        match token.kind() {
            Identifier => Some(NameDesignatorSyntax::Identifier(token)),
            StringLiteral => Some(NameDesignatorSyntax::StringLiteral(token)),
            CharacterLiteral => Some(NameDesignatorSyntax::CharacterLiteral(token)),
            _ => None,
        }
    }
    pub fn raw(&self) -> SyntaxToken {
        match self {
            NameDesignatorSyntax::Identifier(token) => token.clone(),
            NameDesignatorSyntax::StringLiteral(token) => token.clone(),
            NameDesignatorSyntax::CharacterLiteral(token) => token.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct NameSyntax(pub(crate) SyntaxNode);
impl AstNode for NameSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::Name => Some(NameSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl NameSyntax {
    pub fn name_prefix(&self) -> Option<NamePrefixSyntax> {
        self.0.children().filter_map(NamePrefixSyntax::cast).nth(0)
    }
    pub fn name_tails(&self) -> impl Iterator<Item = NameTailSyntax> + use<'_> {
        self.0.children().filter_map(NameTailSyntax::cast)
    }
}
#[derive(Debug, Clone)]
pub struct NameDesignatorPrefixSyntax(pub(crate) SyntaxNode);
impl AstNode for NameDesignatorPrefixSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::NameDesignatorPrefix => Some(NameDesignatorPrefixSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl NameDesignatorPrefixSyntax {
    pub fn name_designator(&self) -> Option<NameDesignatorSyntax> {
        self.0
            .tokens()
            .filter_map(NameDesignatorSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum NamePrefixSyntax {
    ExternalName(ExternalNameSyntax),
    NameDesignatorPrefix(NameDesignatorPrefixSyntax),
}
impl AstNode for NamePrefixSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ExternalName => Some(NamePrefixSyntax::ExternalName(
                ExternalNameSyntax::cast(node).unwrap(),
            )),
            NodeKind::NameDesignatorPrefix => Some(NamePrefixSyntax::NameDesignatorPrefix(
                NameDesignatorPrefixSyntax::cast(node).unwrap(),
            )),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            NamePrefixSyntax::ExternalName(inner) => inner.raw(),
            NamePrefixSyntax::NameDesignatorPrefix(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum SuffixSyntax {
    Identifier(SyntaxToken),
    StringLiteral(SyntaxToken),
    CharacterLiteral(SyntaxToken),
    All(SyntaxToken),
}
impl SuffixSyntax {
    pub fn cast(token: SyntaxToken) -> Option<Self> {
        match token.kind() {
            Identifier => Some(SuffixSyntax::Identifier(token)),
            StringLiteral => Some(SuffixSyntax::StringLiteral(token)),
            CharacterLiteral => Some(SuffixSyntax::CharacterLiteral(token)),
            Keyword(Kw::All) => Some(SuffixSyntax::All(token)),
            _ => None,
        }
    }
    pub fn raw(&self) -> SyntaxToken {
        match self {
            SuffixSyntax::Identifier(token) => token.clone(),
            SuffixSyntax::StringLiteral(token) => token.clone(),
            SuffixSyntax::CharacterLiteral(token) => token.clone(),
            SuffixSyntax::All(token) => token.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SelectedNameSyntax(pub(crate) SyntaxNode);
impl AstNode for SelectedNameSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SelectedName => Some(SelectedNameSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SelectedNameSyntax {
    pub fn dot_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().filter(|token| token.kind() == Dot).nth(0)
    }
    pub fn suffix(&self) -> Option<SuffixSyntax> {
        self.0.tokens().filter_map(SuffixSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum NameTailSyntax {
    SelectedName(SelectedNameSyntax),
    RawTokens(RawTokensSyntax),
    AttributeName(AttributeNameSyntax),
}
impl AstNode for NameTailSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SelectedName => Some(NameTailSyntax::SelectedName(
                SelectedNameSyntax::cast(node).unwrap(),
            )),
            NodeKind::RawTokens => Some(NameTailSyntax::RawTokens(
                RawTokensSyntax::cast(node).unwrap(),
            )),
            NodeKind::AttributeName => Some(NameTailSyntax::AttributeName(
                AttributeNameSyntax::cast(node).unwrap(),
            )),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            NameTailSyntax::SelectedName(inner) => inner.raw(),
            NameTailSyntax::RawTokens(inner) => inner.raw(),
            NameTailSyntax::AttributeName(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct RawTokensSyntax(pub(crate) SyntaxNode);
impl AstNode for RawTokensSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::RawTokens => Some(RawTokensSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl RawTokensSyntax {}
