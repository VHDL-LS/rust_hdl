// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2026, Lukas Scheller lukasscheller@icloud.com
use super::*;
use crate::syntax::meta::{Choice, Layout, LayoutItem, LayoutItemKind, Sequence};
use crate::syntax::node::{SyntaxNode, SyntaxToken};
use crate::syntax::node_kind::NodeKind;
use crate::syntax::AstNode;
use crate::tokens::Keyword as Kw;
use crate::tokens::TokenKind;
#[derive(Debug, Clone)]
pub struct SubprogramDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for SubprogramDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SubprogramDeclaration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "subprogram_specification",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::ProcedureSpecification,
                    NodeKind::FunctionSpecification,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "semi_colon",
                kind: LayoutItemKind::Token(TokenKind::SemiColon),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        SubprogramDeclarationSyntax(node)
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
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum SubprogramSpecificationSyntax {
    ProcedureSpecification(ProcedureSpecificationSyntax),
    FunctionSpecification(FunctionSpecificationSyntax),
}
impl AstNode for SubprogramSpecificationSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::ProcedureSpecification,
            NodeKind::FunctionSpecification,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if ProcedureSpecificationSyntax::can_cast(&node) {
            return SubprogramSpecificationSyntax::ProcedureSpecification(
                ProcedureSpecificationSyntax::cast_unchecked(node),
            );
        }
        if FunctionSpecificationSyntax::can_cast(&node) {
            return SubprogramSpecificationSyntax::FunctionSpecification(
                FunctionSpecificationSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ProcedureSpecification,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "procedure",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Procedure)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "designator",
                kind: LayoutItemKind::TokenChoice(&[
                    TokenKind::Identifier,
                    TokenKind::StringLiteral,
                ]),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "subprogram_header",
                kind: LayoutItemKind::Node(NodeKind::SubprogramHeader),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "parameter_list",
                kind: LayoutItemKind::Node(NodeKind::ParameterList),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ProcedureSpecificationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ProcedureSpecificationSyntax {
    pub fn procedure_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Procedure))
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::FunctionSpecification,
        items: &[
            LayoutItem {
                optional: true,
                repeated: false,
                name: "function_purity",
                kind: LayoutItemKind::TokenChoice(&[
                    TokenKind::Keyword(Kw::Pure),
                    TokenKind::Keyword(Kw::Impure),
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "function",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Function)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "designator",
                kind: LayoutItemKind::TokenChoice(&[
                    TokenKind::Identifier,
                    TokenKind::StringLiteral,
                ]),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "subprogram_header",
                kind: LayoutItemKind::Node(NodeKind::SubprogramHeader),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "parameter_list",
                kind: LayoutItemKind::Node(NodeKind::ParameterList),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "return",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Return)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "name",
                kind: LayoutItemKind::Node(NodeKind::Name),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        FunctionSpecificationSyntax(node)
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
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Function))
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
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Return))
            .nth(0)
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ParenthesizedInterfaceListSyntax(pub(crate) SyntaxNode);
impl AstNode for ParenthesizedInterfaceListSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ParenthesizedInterfaceList,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "left_par",
                kind: LayoutItemKind::Token(TokenKind::LeftPar),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "interface_list",
                kind: LayoutItemKind::Node(NodeKind::InterfaceList),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "right_par",
                kind: LayoutItemKind::Token(TokenKind::RightPar),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ParenthesizedInterfaceListSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ParenthesizedInterfaceListSyntax {
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::LeftPar)
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
            .filter(|token| token.kind() == TokenKind::RightPar)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ParameterListSyntax(pub(crate) SyntaxNode);
impl AstNode for ParameterListSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ParameterList,
        items: &[
            LayoutItem {
                optional: true,
                repeated: false,
                name: "parameter",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Parameter)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "parenthesized_interface_list",
                kind: LayoutItemKind::Node(NodeKind::ParenthesizedInterfaceList),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ParameterListSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ParameterListSyntax {
    pub fn parameter_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Parameter))
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
            TokenKind::Keyword(Kw::Pure) => Some(FunctionPuritySyntax::Pure(token)),
            TokenKind::Keyword(Kw::Impure) => Some(FunctionPuritySyntax::Impure(token)),
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SubprogramHeader,
        items: &[
            LayoutItem {
                optional: true,
                repeated: false,
                name: "subprogram_header_generic_clause",
                kind: LayoutItemKind::Node(NodeKind::SubprogramHeaderGenericClause),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "generic_map_aspect",
                kind: LayoutItemKind::Node(NodeKind::GenericMapAspect),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        SubprogramHeaderSyntax(node)
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SubprogramHeaderGenericClause,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "generic",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Generic)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "left_par",
                kind: LayoutItemKind::Token(TokenKind::LeftPar),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "interface_list",
                kind: LayoutItemKind::Node(NodeKind::InterfaceList),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "right_par",
                kind: LayoutItemKind::Token(TokenKind::RightPar),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        SubprogramHeaderGenericClauseSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SubprogramHeaderGenericClauseSyntax {
    pub fn generic_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Generic))
            .nth(0)
    }
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::LeftPar)
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
            .filter(|token| token.kind() == TokenKind::RightPar)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct SubprogramBodySyntax(pub(crate) SyntaxNode);
impl AstNode for SubprogramBodySyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SubprogramBody,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "subprogram_body_preamble",
                kind: LayoutItemKind::Node(NodeKind::SubprogramBodyPreamble),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "declarations",
                kind: LayoutItemKind::Node(NodeKind::Declarations),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "declaration_statement_separator",
                kind: LayoutItemKind::Node(NodeKind::DeclarationStatementSeparator),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "concurrent_statements",
                kind: LayoutItemKind::Node(NodeKind::ConcurrentStatements),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "subprogram_body_epilogue",
                kind: LayoutItemKind::Node(NodeKind::SubprogramBodyEpilogue),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        SubprogramBodySyntax(node)
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SubprogramBodyPreamble,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "subprogram_specification",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::ProcedureSpecification,
                    NodeKind::FunctionSpecification,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "is",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Is)),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        SubprogramBodyPreambleSyntax(node)
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
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Is))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct SubprogramBodyEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for SubprogramBodyEpilogueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SubprogramBodyEpilogue,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "end",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::End)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "subprogram_kind",
                kind: LayoutItemKind::TokenChoice(&[
                    TokenKind::Keyword(Kw::Procedure),
                    TokenKind::Keyword(Kw::Function),
                ]),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "designator",
                kind: LayoutItemKind::TokenChoice(&[
                    TokenKind::Identifier,
                    TokenKind::StringLiteral,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "semi_colon",
                kind: LayoutItemKind::Token(TokenKind::SemiColon),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        SubprogramBodyEpilogueSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SubprogramBodyEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::End))
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
            .filter(|token| token.kind() == TokenKind::SemiColon)
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
            TokenKind::Keyword(Kw::Procedure) => Some(SubprogramKindSyntax::Procedure(token)),
            TokenKind::Keyword(Kw::Function) => Some(SubprogramKindSyntax::Function(token)),
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SubprogramInstantiationDeclaration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "subprogram_instantiation_declaration_preamble",
                kind: LayoutItemKind::Node(NodeKind::SubprogramInstantiationDeclarationPreamble),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "generic_map_aspect",
                kind: LayoutItemKind::Node(NodeKind::GenericMapAspect),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "semi_colon",
                kind: LayoutItemKind::Token(TokenKind::SemiColon),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        SubprogramInstantiationDeclarationSyntax(node)
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
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct SubprogramInstantiationDeclarationPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for SubprogramInstantiationDeclarationPreambleSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SubprogramInstantiationDeclarationPreamble,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "subprogram_kind",
                kind: LayoutItemKind::TokenChoice(&[
                    TokenKind::Keyword(Kw::Procedure),
                    TokenKind::Keyword(Kw::Function),
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "name",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "is",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Is)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "new",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::New)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "name",
                kind: LayoutItemKind::Node(NodeKind::Name),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "signature",
                kind: LayoutItemKind::Node(NodeKind::Signature),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        SubprogramInstantiationDeclarationPreambleSyntax(node)
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
            .filter(|token| token.kind() == TokenKind::Identifier)
            .nth(0)
    }
    pub fn is_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Is))
            .nth(0)
    }
    pub fn new_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::New))
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::Package,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "package_preamble",
                kind: LayoutItemKind::Node(NodeKind::PackagePreamble),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "package_header",
                kind: LayoutItemKind::Node(NodeKind::PackageHeader),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "declarations",
                kind: LayoutItemKind::Node(NodeKind::Declarations),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "package_epilogue",
                kind: LayoutItemKind::Node(NodeKind::PackageEpilogue),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        PackageSyntax(node)
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PackagePreamble,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "package",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Package)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "name",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "is",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Is)),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        PackagePreambleSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PackagePreambleSyntax {
    pub fn package_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Package))
            .nth(0)
    }
    pub fn name_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Identifier)
            .nth(0)
    }
    pub fn is_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Is))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct PackageEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for PackageEpilogueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PackageEpilogue,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "end",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::End)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "package",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Package)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "identifier",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "semi_colon",
                kind: LayoutItemKind::Token(TokenKind::SemiColon),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        PackageEpilogueSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PackageEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::End))
            .nth(0)
    }
    pub fn package_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Package))
            .nth(0)
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Identifier)
            .nth(0)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct PackageHeaderSyntax(pub(crate) SyntaxNode);
impl AstNode for PackageHeaderSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PackageHeader,
        items: &[
            LayoutItem {
                optional: true,
                repeated: false,
                name: "generic_clause",
                kind: LayoutItemKind::Node(NodeKind::GenericClause),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "generic_map_aspect",
                kind: LayoutItemKind::Node(NodeKind::GenericMapAspect),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "semi_colon",
                kind: LayoutItemKind::Token(TokenKind::SemiColon),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        PackageHeaderSyntax(node)
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
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct PackageBodySyntax(pub(crate) SyntaxNode);
impl AstNode for PackageBodySyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PackageBody,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "package_body_preamble",
                kind: LayoutItemKind::Node(NodeKind::PackageBodyPreamble),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "declarations",
                kind: LayoutItemKind::Node(NodeKind::Declarations),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "package_body_epilogue",
                kind: LayoutItemKind::Node(NodeKind::PackageBodyEpilogue),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        PackageBodySyntax(node)
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PackageBodyPreamble,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "package",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Package)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "body",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Body)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "name",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "is",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Is)),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        PackageBodyPreambleSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PackageBodyPreambleSyntax {
    pub fn package_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Package))
            .nth(0)
    }
    pub fn body_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Body))
            .nth(0)
    }
    pub fn name_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Identifier)
            .nth(0)
    }
    pub fn is_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Is))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct PackageBodyEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for PackageBodyEpilogueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PackageBodyEpilogue,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "end",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::End)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "package",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Package)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "body",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Body)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "identifier",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "semi_colon",
                kind: LayoutItemKind::Token(TokenKind::SemiColon),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        PackageBodyEpilogueSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PackageBodyEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::End))
            .nth(0)
    }
    pub fn package_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Package))
            .nth(0)
    }
    pub fn body_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Body))
            .nth(0)
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Identifier)
            .nth(0)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct PackageInstantiationSyntax(pub(crate) SyntaxNode);
impl AstNode for PackageInstantiationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PackageInstantiation,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "package_instantiation_preamble",
                kind: LayoutItemKind::Node(NodeKind::PackageInstantiationPreamble),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "generic_map_aspect",
                kind: LayoutItemKind::Node(NodeKind::GenericMapAspect),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "semi_colon",
                kind: LayoutItemKind::Token(TokenKind::SemiColon),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        PackageInstantiationSyntax(node)
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
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct PackageInstantiationPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for PackageInstantiationPreambleSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PackageInstantiationPreamble,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "package",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Package)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "name",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "is",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Is)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "new",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::New)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "name",
                kind: LayoutItemKind::Node(NodeKind::Name),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        PackageInstantiationPreambleSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PackageInstantiationPreambleSyntax {
    pub fn package_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Package))
            .nth(0)
    }
    pub fn name_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Identifier)
            .nth(0)
    }
    pub fn is_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Is))
            .nth(0)
    }
    pub fn new_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::New))
            .nth(0)
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct SignatureSyntax(pub(crate) SyntaxNode);
impl AstNode for SignatureSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::Signature,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "left_square",
                kind: LayoutItemKind::Token(TokenKind::LeftSquare),
            },
            LayoutItem {
                optional: false,
                repeated: true,
                name: "names",
                kind: LayoutItemKind::Node(NodeKind::Name),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "return",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Return)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "return_type",
                kind: LayoutItemKind::Node(NodeKind::Name),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "right_square",
                kind: LayoutItemKind::Token(TokenKind::RightSquare),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        SignatureSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SignatureSyntax {
    pub fn left_square_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::LeftSquare)
            .nth(0)
    }
    pub fn names(&self) -> impl Iterator<Item = NameSyntax> + use<'_> {
        self.0.children().filter_map(NameSyntax::cast)
    }
    pub fn return_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Return))
            .nth(0)
    }
    pub fn return_type(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(1)
    }
    pub fn right_square_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::RightSquare)
            .nth(0)
    }
}
