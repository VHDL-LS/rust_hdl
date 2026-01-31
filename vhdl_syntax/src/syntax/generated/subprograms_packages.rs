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
pub struct SubprogramDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for SubprogramDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SubprogramDeclaration => Some(SubprogramDeclarationSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::SubprogramDeclaration)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SubprogramDeclarationSyntax {
    pub fn subprogram_specification(&self) -> Option<SubprogramSpecificationSyntax> {
        self.0
            .children()
            .filter_map(SubprogramSpecificationSyntax::cast)
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
pub enum SubprogramSpecificationSyntax {
    ProcedureSpecification(ProcedureSpecificationSyntax),
    FunctionSpecification(FunctionSpecificationSyntax),
}
impl AstNode for SubprogramSpecificationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        if ProcedureSpecificationSyntax::can_cast(&node) {
            return Some(SubprogramSpecificationSyntax::ProcedureSpecification(
                ProcedureSpecificationSyntax::cast(node).unwrap(),
            ));
        };
        if FunctionSpecificationSyntax::can_cast(&node) {
            return Some(SubprogramSpecificationSyntax::FunctionSpecification(
                FunctionSpecificationSyntax::cast(node).unwrap(),
            ));
        };
        None
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        ProcedureSpecificationSyntax::can_cast(node) || FunctionSpecificationSyntax::can_cast(node)
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            SubprogramSpecificationSyntax::ProcedureSpecification(inner) => inner.raw(),
            SubprogramSpecificationSyntax::FunctionSpecification(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ProcedureSpecificationSyntax(pub(crate) SyntaxNode);
impl AstNode for ProcedureSpecificationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ProcedureSpecification => Some(ProcedureSpecificationSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ProcedureSpecification)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ProcedureSpecificationSyntax {
    pub fn procedure_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Procedure))
            .nth(0)
    }
    pub fn designator(&self) -> Option<DesignatorSyntax> {
        self.0.tokens().filter_map(DesignatorSyntax::cast).nth(0)
    }
    pub fn subprogram_header(&self) -> Option<SubprogramHeaderSyntax> {
        self.0
            .children()
            .filter_map(SubprogramHeaderSyntax::cast)
            .nth(0)
    }
    pub fn parameter_list(&self) -> Option<ParameterListSyntax> {
        self.0
            .children()
            .filter_map(ParameterListSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct FunctionSpecificationSyntax(pub(crate) SyntaxNode);
impl AstNode for FunctionSpecificationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::FunctionSpecification => Some(FunctionSpecificationSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::FunctionSpecification)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl FunctionSpecificationSyntax {
    pub fn function_purity(&self) -> Option<FunctionPuritySyntax> {
        self.0
            .tokens()
            .filter_map(FunctionPuritySyntax::cast)
            .nth(0)
    }
    pub fn function_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Function))
            .nth(0)
    }
    pub fn designator(&self) -> Option<DesignatorSyntax> {
        self.0.tokens().filter_map(DesignatorSyntax::cast).nth(0)
    }
    pub fn subprogram_header(&self) -> Option<SubprogramHeaderSyntax> {
        self.0
            .children()
            .filter_map(SubprogramHeaderSyntax::cast)
            .nth(0)
    }
    pub fn parameter_list(&self) -> Option<ParameterListSyntax> {
        self.0
            .children()
            .filter_map(ParameterListSyntax::cast)
            .nth(0)
    }
    pub fn return_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Return))
            .nth(0)
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ParenthesizedInterfaceListSyntax(pub(crate) SyntaxNode);
impl AstNode for ParenthesizedInterfaceListSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ParenthesizedInterfaceList => Some(ParenthesizedInterfaceListSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ParenthesizedInterfaceList)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ParenthesizedInterfaceListSyntax {
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == LeftPar)
            .nth(0)
    }
    pub fn interface_list(&self) -> Option<InterfaceListSyntax> {
        self.0
            .children()
            .filter_map(InterfaceListSyntax::cast)
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
pub struct ParameterListSyntax(pub(crate) SyntaxNode);
impl AstNode for ParameterListSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ParameterList => Some(ParameterListSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ParameterList)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ParameterListSyntax {
    pub fn parameter_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Parameter))
            .nth(0)
    }
    pub fn parenthesized_interface_list(&self) -> Option<ParenthesizedInterfaceListSyntax> {
        self.0
            .children()
            .filter_map(ParenthesizedInterfaceListSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum FunctionPuritySyntax {
    Pure(SyntaxToken),
    Impure(SyntaxToken),
}
impl FunctionPuritySyntax {
    pub fn cast(token: SyntaxToken) -> Option<Self> {
        match token.kind() {
            Keyword(Kw::Pure) => Some(FunctionPuritySyntax::Pure(token)),
            Keyword(Kw::Impure) => Some(FunctionPuritySyntax::Impure(token)),
            _ => None,
        }
    }
    pub fn raw(&self) -> SyntaxToken {
        match self {
            FunctionPuritySyntax::Pure(token) => token.clone(),
            FunctionPuritySyntax::Impure(token) => token.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SubprogramHeaderSyntax(pub(crate) SyntaxNode);
impl AstNode for SubprogramHeaderSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SubprogramHeader => Some(SubprogramHeaderSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::SubprogramHeader)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SubprogramHeaderSyntax {
    pub fn subprogram_header_generic_clause(&self) -> Option<SubprogramHeaderGenericClauseSyntax> {
        self.0
            .children()
            .filter_map(SubprogramHeaderGenericClauseSyntax::cast)
            .nth(0)
    }
    pub fn generic_map_aspect(&self) -> Option<GenericMapAspectSyntax> {
        self.0
            .children()
            .filter_map(GenericMapAspectSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct SubprogramHeaderGenericClauseSyntax(pub(crate) SyntaxNode);
impl AstNode for SubprogramHeaderGenericClauseSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SubprogramHeaderGenericClause => {
                Some(SubprogramHeaderGenericClauseSyntax(node))
            }
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::SubprogramHeaderGenericClause)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SubprogramHeaderGenericClauseSyntax {
    pub fn generic_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Generic))
            .nth(0)
    }
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == LeftPar)
            .nth(0)
    }
    pub fn interface_list(&self) -> Option<InterfaceListSyntax> {
        self.0
            .children()
            .filter_map(InterfaceListSyntax::cast)
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
pub struct SubprogramBodySyntax(pub(crate) SyntaxNode);
impl AstNode for SubprogramBodySyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SubprogramBody => Some(SubprogramBodySyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::SubprogramBody)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SubprogramBodySyntax {
    pub fn subprogram_body_preamble(&self) -> Option<SubprogramBodyPreambleSyntax> {
        self.0
            .children()
            .filter_map(SubprogramBodyPreambleSyntax::cast)
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
    pub fn subprogram_body_epilogue(&self) -> Option<SubprogramBodyEpilogueSyntax> {
        self.0
            .children()
            .filter_map(SubprogramBodyEpilogueSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct SubprogramBodyPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for SubprogramBodyPreambleSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SubprogramBodyPreamble => Some(SubprogramBodyPreambleSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::SubprogramBodyPreamble)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SubprogramBodyPreambleSyntax {
    pub fn subprogram_specification(&self) -> Option<SubprogramSpecificationSyntax> {
        self.0
            .children()
            .filter_map(SubprogramSpecificationSyntax::cast)
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
pub struct SubprogramBodyEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for SubprogramBodyEpilogueSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SubprogramBodyEpilogue => Some(SubprogramBodyEpilogueSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::SubprogramBodyEpilogue)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SubprogramBodyEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::End))
            .nth(0)
    }
    pub fn subprogram_kind(&self) -> Option<SubprogramKindSyntax> {
        self.0
            .tokens()
            .filter_map(SubprogramKindSyntax::cast)
            .nth(0)
    }
    pub fn designator(&self) -> Option<DesignatorSyntax> {
        self.0.tokens().filter_map(DesignatorSyntax::cast).nth(0)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum SubprogramKindSyntax {
    Procedure(SyntaxToken),
    Function(SyntaxToken),
}
impl SubprogramKindSyntax {
    pub fn cast(token: SyntaxToken) -> Option<Self> {
        match token.kind() {
            Keyword(Kw::Procedure) => Some(SubprogramKindSyntax::Procedure(token)),
            Keyword(Kw::Function) => Some(SubprogramKindSyntax::Function(token)),
            _ => None,
        }
    }
    pub fn raw(&self) -> SyntaxToken {
        match self {
            SubprogramKindSyntax::Procedure(token) => token.clone(),
            SubprogramKindSyntax::Function(token) => token.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SubprogramInstantiationDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for SubprogramInstantiationDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SubprogramInstantiationDeclaration => {
                Some(SubprogramInstantiationDeclarationSyntax(node))
            }
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::SubprogramInstantiationDeclaration)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SubprogramInstantiationDeclarationSyntax {
    pub fn subprogram_instantiation_declaration_preamble(
        &self,
    ) -> Option<SubprogramInstantiationDeclarationPreambleSyntax> {
        self.0
            .children()
            .filter_map(SubprogramInstantiationDeclarationPreambleSyntax::cast)
            .nth(0)
    }
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
pub struct SubprogramInstantiationDeclarationPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for SubprogramInstantiationDeclarationPreambleSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SubprogramInstantiationDeclarationPreamble => {
                Some(SubprogramInstantiationDeclarationPreambleSyntax(node))
            }
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(
            node.kind(),
            NodeKind::SubprogramInstantiationDeclarationPreamble
        )
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SubprogramInstantiationDeclarationPreambleSyntax {
    pub fn subprogram_kind(&self) -> Option<SubprogramKindSyntax> {
        self.0
            .tokens()
            .filter_map(SubprogramKindSyntax::cast)
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
    pub fn new_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::New))
            .nth(0)
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
    pub fn signature(&self) -> Option<SignatureSyntax> {
        self.0.children().filter_map(SignatureSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct PackageSyntax(pub(crate) SyntaxNode);
impl AstNode for PackageSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::Package => Some(PackageSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::Package)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PackageSyntax {
    pub fn package_preamble(&self) -> Option<PackagePreambleSyntax> {
        self.0
            .children()
            .filter_map(PackagePreambleSyntax::cast)
            .nth(0)
    }
    pub fn package_header(&self) -> Option<PackageHeaderSyntax> {
        self.0
            .children()
            .filter_map(PackageHeaderSyntax::cast)
            .nth(0)
    }
    pub fn declarations(&self) -> Option<DeclarationsSyntax> {
        self.0
            .children()
            .filter_map(DeclarationsSyntax::cast)
            .nth(0)
    }
    pub fn package_epilogue(&self) -> Option<PackageEpilogueSyntax> {
        self.0
            .children()
            .filter_map(PackageEpilogueSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct PackagePreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for PackagePreambleSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::PackagePreamble => Some(PackagePreambleSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::PackagePreamble)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PackagePreambleSyntax {
    pub fn package_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Package))
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
pub struct PackageEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for PackageEpilogueSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::PackageEpilogue => Some(PackageEpilogueSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::PackageEpilogue)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PackageEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::End))
            .nth(0)
    }
    pub fn package_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Package))
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
pub struct PackageHeaderSyntax(pub(crate) SyntaxNode);
impl AstNode for PackageHeaderSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::PackageHeader => Some(PackageHeaderSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::PackageHeader)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PackageHeaderSyntax {
    pub fn generic_clause(&self) -> Option<GenericClauseSyntax> {
        self.0
            .children()
            .filter_map(GenericClauseSyntax::cast)
            .nth(0)
    }
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
pub struct PackageBodySyntax(pub(crate) SyntaxNode);
impl AstNode for PackageBodySyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::PackageBody => Some(PackageBodySyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::PackageBody)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PackageBodySyntax {
    pub fn package_body_preamble(&self) -> Option<PackageBodyPreambleSyntax> {
        self.0
            .children()
            .filter_map(PackageBodyPreambleSyntax::cast)
            .nth(0)
    }
    pub fn declarations(&self) -> Option<DeclarationsSyntax> {
        self.0
            .children()
            .filter_map(DeclarationsSyntax::cast)
            .nth(0)
    }
    pub fn package_body_epilogue(&self) -> Option<PackageBodyEpilogueSyntax> {
        self.0
            .children()
            .filter_map(PackageBodyEpilogueSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct PackageBodyPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for PackageBodyPreambleSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::PackageBodyPreamble => Some(PackageBodyPreambleSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::PackageBodyPreamble)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PackageBodyPreambleSyntax {
    pub fn package_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Package))
            .nth(0)
    }
    pub fn body_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Body))
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
pub struct PackageBodyEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for PackageBodyEpilogueSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::PackageBodyEpilogue => Some(PackageBodyEpilogueSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::PackageBodyEpilogue)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PackageBodyEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::End))
            .nth(0)
    }
    pub fn package_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Package))
            .nth(0)
    }
    pub fn body_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Body))
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
pub struct PackageInstantiationSyntax(pub(crate) SyntaxNode);
impl AstNode for PackageInstantiationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::PackageInstantiation => Some(PackageInstantiationSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::PackageInstantiation)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PackageInstantiationSyntax {
    pub fn package_instantiation_preamble(&self) -> Option<PackageInstantiationPreambleSyntax> {
        self.0
            .children()
            .filter_map(PackageInstantiationPreambleSyntax::cast)
            .nth(0)
    }
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
pub struct PackageInstantiationPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for PackageInstantiationPreambleSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::PackageInstantiationPreamble => {
                Some(PackageInstantiationPreambleSyntax(node))
            }
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::PackageInstantiationPreamble)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PackageInstantiationPreambleSyntax {
    pub fn package_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Package))
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
    pub fn new_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::New))
            .nth(0)
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct SignatureSyntax(pub(crate) SyntaxNode);
impl AstNode for SignatureSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::Signature => Some(SignatureSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::Signature)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SignatureSyntax {
    pub fn left_square_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == LeftSquare)
            .nth(0)
    }
    pub fn names(&self) -> impl Iterator<Item = NameSyntax> + use<'_> {
        self.0.children().filter_map(NameSyntax::cast)
    }
    pub fn return_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Return))
            .nth(0)
    }
    pub fn return_type(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(1)
    }
    pub fn right_square_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == RightSquare)
            .nth(0)
    }
}
