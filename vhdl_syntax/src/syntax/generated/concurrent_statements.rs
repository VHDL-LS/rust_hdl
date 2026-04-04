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
pub struct DeclarationStatementSeparatorSyntax(pub(crate) SyntaxNode);
impl AstNode for DeclarationStatementSeparatorSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::DeclarationStatementSeparator,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "begin",
            kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Begin)),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        DeclarationStatementSeparatorSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl DeclarationStatementSeparatorSyntax {
    pub fn begin_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Begin))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct SemiColonTerminatedGenericMapAspectSyntax(pub(crate) SyntaxNode);
impl AstNode for SemiColonTerminatedGenericMapAspectSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SemiColonTerminatedGenericMapAspect,
        items: &[
            LayoutItem {
                optional: false,
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
        SemiColonTerminatedGenericMapAspectSyntax(node)
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
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct SemiColonTerminatedPortMapAspectSyntax(pub(crate) SyntaxNode);
impl AstNode for SemiColonTerminatedPortMapAspectSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SemiColonTerminatedPortMapAspect,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "port_map_aspect",
                kind: LayoutItemKind::Node(NodeKind::PortMapAspect),
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
        SemiColonTerminatedPortMapAspectSyntax(node)
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
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct BlockHeaderSyntax(pub(crate) SyntaxNode);
impl AstNode for BlockHeaderSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::BlockHeader,
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
                name: "semi_colon_terminated_generic_map_aspect",
                kind: LayoutItemKind::Node(NodeKind::SemiColonTerminatedGenericMapAspect),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "port_clause",
                kind: LayoutItemKind::Node(NodeKind::PortClause),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "semi_colon_terminated_port_map_aspect",
                kind: LayoutItemKind::Node(NodeKind::SemiColonTerminatedPortMapAspect),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        BlockHeaderSyntax(node)
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::BlockStatement,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "block_preamble",
                kind: LayoutItemKind::Node(NodeKind::BlockPreamble),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "block_header",
                kind: LayoutItemKind::Node(NodeKind::BlockHeader),
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
                name: "block_epilogue",
                kind: LayoutItemKind::Node(NodeKind::BlockEpilogue),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        BlockStatementSyntax(node)
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ParenthesizedExpression,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "left_par",
                kind: LayoutItemKind::Token(TokenKind::LeftPar),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "expression",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::LiteralExpression,
                    NodeKind::PhysicalLiteralExpression,
                    NodeKind::UnaryExpression,
                    NodeKind::BinaryExpression,
                    NodeKind::ParenthesizedExpressionOrAggregate,
                    NodeKind::SubtypeIndicationAllocator,
                    NodeKind::ExpressionAllocator,
                    NodeKind::QualifiedExpression,
                    NodeKind::TypeConversion,
                    NodeKind::NameExpression,
                ]),
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
        ParenthesizedExpressionSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ParenthesizedExpressionSyntax {
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::LeftPar)
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn right_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::RightPar)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct BlockPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for BlockPreambleSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::BlockPreamble,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "label",
                kind: LayoutItemKind::Node(NodeKind::Label),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "block",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Block)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "condition",
                kind: LayoutItemKind::Node(NodeKind::ParenthesizedExpression),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "is",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Is)),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        BlockPreambleSyntax(node)
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
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Block))
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
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Is))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct BlockEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for BlockEpilogueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::BlockEpilogue,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "end",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::End)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "block",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Block)),
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
        BlockEpilogueSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl BlockEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::End))
            .nth(0)
    }
    pub fn block_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Block))
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
pub struct CaseGenerateAlternativeSyntax(pub(crate) SyntaxNode);
impl AstNode for CaseGenerateAlternativeSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::CaseGenerateAlternative,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "when",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::When)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "label",
                kind: LayoutItemKind::Node(NodeKind::Label),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "choices",
                kind: LayoutItemKind::Node(NodeKind::Choices),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "right_arrow",
                kind: LayoutItemKind::Token(TokenKind::RightArrow),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "generate_statement_body",
                kind: LayoutItemKind::Node(NodeKind::GenerateStatementBody),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        CaseGenerateAlternativeSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl CaseGenerateAlternativeSyntax {
    pub fn when_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::When))
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
            .filter(|token| token.kind() == TokenKind::RightArrow)
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::CaseGenerateStatement,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "case_generate_statement_preamble",
                kind: LayoutItemKind::Node(NodeKind::CaseGenerateStatementPreamble),
            },
            LayoutItem {
                optional: false,
                repeated: true,
                name: "case_generate_alternatives",
                kind: LayoutItemKind::Node(NodeKind::CaseGenerateAlternative),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "case_generate_statement_epilogue",
                kind: LayoutItemKind::Node(NodeKind::CaseGenerateStatementEpilogue),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        CaseGenerateStatementSyntax(node)
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::CaseGenerateStatementPreamble,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "label",
                kind: LayoutItemKind::Node(NodeKind::Label),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "case",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Case)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "expression",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::LiteralExpression,
                    NodeKind::PhysicalLiteralExpression,
                    NodeKind::UnaryExpression,
                    NodeKind::BinaryExpression,
                    NodeKind::ParenthesizedExpressionOrAggregate,
                    NodeKind::SubtypeIndicationAllocator,
                    NodeKind::ExpressionAllocator,
                    NodeKind::QualifiedExpression,
                    NodeKind::TypeConversion,
                    NodeKind::NameExpression,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "generate",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Generate)),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        CaseGenerateStatementPreambleSyntax(node)
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
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Case))
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn generate_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Generate))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct CaseGenerateStatementEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for CaseGenerateStatementEpilogueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::CaseGenerateStatementEpilogue,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "end",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::End)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "generate",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Generate)),
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
        CaseGenerateStatementEpilogueSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl CaseGenerateStatementEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::End))
            .nth(0)
    }
    pub fn generate_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Generate))
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
pub struct ComponentInstantiationStatementSyntax(pub(crate) SyntaxNode);
impl AstNode for ComponentInstantiationStatementSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ComponentInstantiationStatement,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "label",
                kind: LayoutItemKind::Node(NodeKind::Label),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "instantiated_unit",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::ComponentInstantiatedUnit,
                    NodeKind::EntityInstantiatedUnit,
                    NodeKind::ConfigurationInstantiatedUnit,
                ]),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "component_instantiation_items",
                kind: LayoutItemKind::Node(NodeKind::ComponentInstantiationItems),
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
        ComponentInstantiationStatementSyntax(node)
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
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ComponentInstantiationItemsSyntax(pub(crate) SyntaxNode);
impl AstNode for ComponentInstantiationItemsSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ComponentInstantiationItems,
        items: &[
            LayoutItem {
                optional: true,
                repeated: false,
                name: "generic_map_aspect",
                kind: LayoutItemKind::Node(NodeKind::GenericMapAspect),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "port_map_aspect",
                kind: LayoutItemKind::Node(NodeKind::PortMapAspect),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ComponentInstantiationItemsSyntax(node)
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ConcurrentProcedureCallOrComponentInstantiationStatement,
        items: &[
            LayoutItem {
                optional: true,
                repeated: false,
                name: "label",
                kind: LayoutItemKind::Node(NodeKind::Label),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "postponed",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Postponed)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "name",
                kind: LayoutItemKind::Node(NodeKind::Name),
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
        ConcurrentProcedureCallOrComponentInstantiationStatementSyntax(node)
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
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Postponed))
            .nth(0)
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ConcurrentAssertionStatementSyntax(pub(crate) SyntaxNode);
impl AstNode for ConcurrentAssertionStatementSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ConcurrentAssertionStatement,
        items: &[
            LayoutItem {
                optional: true,
                repeated: false,
                name: "label",
                kind: LayoutItemKind::Node(NodeKind::Label),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "postponed",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Postponed)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "assertion",
                kind: LayoutItemKind::Node(NodeKind::Assertion),
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
        ConcurrentAssertionStatementSyntax(node)
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
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Postponed))
            .nth(0)
    }
    pub fn assertion(&self) -> Option<AssertionSyntax> {
        self.0.children().filter_map(AssertionSyntax::cast).nth(0)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ConcurrentSimpleSignalAssignmentSyntax(pub(crate) SyntaxNode);
impl AstNode for ConcurrentSimpleSignalAssignmentSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ConcurrentSimpleSignalAssignment,
        items: &[
            LayoutItem {
                optional: true,
                repeated: false,
                name: "label",
                kind: LayoutItemKind::Node(NodeKind::Label),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "postponed",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Postponed)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "target",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::NameTarget,
                    NodeKind::AggregateTarget,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "lte",
                kind: LayoutItemKind::Token(TokenKind::LTE),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "guarded",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Guarded)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "delay_mechanism",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::TransportDelayMechanism,
                    NodeKind::InertialDelayMechanism,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "waveform",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::WaveformElements,
                    NodeKind::UnaffectedWaveform,
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
        ConcurrentSimpleSignalAssignmentSyntax(node)
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
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Postponed))
            .nth(0)
    }
    pub fn target(&self) -> Option<TargetSyntax> {
        self.0.children().filter_map(TargetSyntax::cast).nth(0)
    }
    pub fn lte_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::LTE)
            .nth(0)
    }
    pub fn guarded_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Guarded))
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
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ConcurrentConditionalSignalAssignmentSyntax(pub(crate) SyntaxNode);
impl AstNode for ConcurrentConditionalSignalAssignmentSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ConcurrentConditionalSignalAssignment,
        items: &[
            LayoutItem {
                optional: true,
                repeated: false,
                name: "label",
                kind: LayoutItemKind::Node(NodeKind::Label),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "postponed",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Postponed)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "target",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::NameTarget,
                    NodeKind::AggregateTarget,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "lte",
                kind: LayoutItemKind::Token(TokenKind::LTE),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "guarded",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Guarded)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "delay_mechanism",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::TransportDelayMechanism,
                    NodeKind::InertialDelayMechanism,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "conditional_waveforms",
                kind: LayoutItemKind::Node(NodeKind::ConditionalWaveforms),
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
        ConcurrentConditionalSignalAssignmentSyntax(node)
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
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Postponed))
            .nth(0)
    }
    pub fn target(&self) -> Option<TargetSyntax> {
        self.0.children().filter_map(TargetSyntax::cast).nth(0)
    }
    pub fn lte_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::LTE)
            .nth(0)
    }
    pub fn guarded_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Guarded))
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
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ConcurrentSelectedSignalAssignmentSyntax(pub(crate) SyntaxNode);
impl AstNode for ConcurrentSelectedSignalAssignmentSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ConcurrentSelectedSignalAssignment,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "concurrent_selected_signal_assignment_preamble",
                kind: LayoutItemKind::Node(NodeKind::ConcurrentSelectedSignalAssignmentPreamble),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "target",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::NameTarget,
                    NodeKind::AggregateTarget,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "lte",
                kind: LayoutItemKind::Token(TokenKind::LTE),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "guarded",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Guarded)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "delay_mechanism",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::TransportDelayMechanism,
                    NodeKind::InertialDelayMechanism,
                ]),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "selected_waveforms",
                kind: LayoutItemKind::Node(NodeKind::SelectedWaveforms),
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
        ConcurrentSelectedSignalAssignmentSyntax(node)
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
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::LTE)
            .nth(0)
    }
    pub fn guarded_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Guarded))
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
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ConcurrentSelectedSignalAssignmentPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for ConcurrentSelectedSignalAssignmentPreambleSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ConcurrentSelectedSignalAssignmentPreamble,
        items: &[
            LayoutItem {
                optional: true,
                repeated: false,
                name: "label",
                kind: LayoutItemKind::Node(NodeKind::Label),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "postponed",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Postponed)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "with",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::With)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "expression",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::LiteralExpression,
                    NodeKind::PhysicalLiteralExpression,
                    NodeKind::UnaryExpression,
                    NodeKind::BinaryExpression,
                    NodeKind::ParenthesizedExpressionOrAggregate,
                    NodeKind::SubtypeIndicationAllocator,
                    NodeKind::ExpressionAllocator,
                    NodeKind::QualifiedExpression,
                    NodeKind::TypeConversion,
                    NodeKind::NameExpression,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "select",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Select)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "que",
                kind: LayoutItemKind::Token(TokenKind::Que),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ConcurrentSelectedSignalAssignmentPreambleSyntax(node)
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
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Postponed))
            .nth(0)
    }
    pub fn with_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::With))
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn select_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Select))
            .nth(0)
    }
    pub fn que_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Que)
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
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::BlockStatement,
            NodeKind::ProcessStatement,
            NodeKind::ConcurrentAssertionStatement,
            NodeKind::ComponentInstantiationStatement,
            NodeKind::ConcurrentSelectedSignalAssignment,
            NodeKind::ConcurrentConditionalSignalAssignment,
            NodeKind::ConcurrentSimpleSignalAssignment,
            NodeKind::ConcurrentProcedureCallOrComponentInstantiationStatement,
            NodeKind::ForGenerateStatement,
            NodeKind::IfGenerateStatement,
            NodeKind::CaseGenerateStatement,
            NodeKind::PslDirective,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if BlockStatementSyntax::can_cast(&node) {
            return ConcurrentStatementSyntax::BlockStatement(
                BlockStatementSyntax::cast_unchecked(node),
            );
        }
        if ProcessStatementSyntax::can_cast(&node) {
            return ConcurrentStatementSyntax::ProcessStatement(
                ProcessStatementSyntax::cast_unchecked(node),
            );
        }
        if ConcurrentAssertionStatementSyntax::can_cast(&node) {
            return ConcurrentStatementSyntax::ConcurrentAssertionStatement(
                ConcurrentAssertionStatementSyntax::cast_unchecked(node),
            );
        }
        if ComponentInstantiationStatementSyntax::can_cast(&node) {
            return ConcurrentStatementSyntax::ComponentInstantiationStatement(
                ComponentInstantiationStatementSyntax::cast_unchecked(node),
            );
        }
        if ConcurrentSelectedSignalAssignmentSyntax::can_cast(&node) {
            return ConcurrentStatementSyntax::ConcurrentSelectedSignalAssignment(
                ConcurrentSelectedSignalAssignmentSyntax::cast_unchecked(node),
            );
        }
        if ConcurrentConditionalSignalAssignmentSyntax::can_cast(&node) {
            return ConcurrentStatementSyntax::ConcurrentConditionalSignalAssignment(
                ConcurrentConditionalSignalAssignmentSyntax::cast_unchecked(node),
            );
        }
        if ConcurrentSimpleSignalAssignmentSyntax::can_cast(&node) {
            return ConcurrentStatementSyntax::ConcurrentSimpleSignalAssignment(
                ConcurrentSimpleSignalAssignmentSyntax::cast_unchecked(node),
            );
        }
        if ConcurrentProcedureCallOrComponentInstantiationStatementSyntax::can_cast(&node) {
            return ConcurrentStatementSyntax :: ConcurrentProcedureCallOrComponentInstantiationStatement (ConcurrentProcedureCallOrComponentInstantiationStatementSyntax :: cast_unchecked (node)) ;
        }
        if ForGenerateStatementSyntax::can_cast(&node) {
            return ConcurrentStatementSyntax::ForGenerateStatement(
                ForGenerateStatementSyntax::cast_unchecked(node),
            );
        }
        if IfGenerateStatementSyntax::can_cast(&node) {
            return ConcurrentStatementSyntax::IfGenerateStatement(
                IfGenerateStatementSyntax::cast_unchecked(node),
            );
        }
        if CaseGenerateStatementSyntax::can_cast(&node) {
            return ConcurrentStatementSyntax::CaseGenerateStatement(
                CaseGenerateStatementSyntax::cast_unchecked(node),
            );
        }
        if PslDirectiveSyntax::can_cast(&node) {
            return ConcurrentStatementSyntax::PslDirective(PslDirectiveSyntax::cast_unchecked(
                node,
            ));
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ConcurrentStatements,
        items: &[LayoutItem {
            optional: false,
            repeated: true,
            name: "concurrent_statements",
            kind: LayoutItemKind::NodeChoice(&[
                NodeKind::BlockStatement,
                NodeKind::ProcessStatement,
                NodeKind::ConcurrentAssertionStatement,
                NodeKind::ComponentInstantiationStatement,
                NodeKind::ConcurrentSelectedSignalAssignment,
                NodeKind::ConcurrentConditionalSignalAssignment,
                NodeKind::ConcurrentSimpleSignalAssignment,
                NodeKind::ConcurrentProcedureCallOrComponentInstantiationStatement,
                NodeKind::ForGenerateStatement,
                NodeKind::IfGenerateStatement,
                NodeKind::CaseGenerateStatement,
                NodeKind::PslDirective,
            ]),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ConcurrentStatementsSyntax(node)
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ForGenerateStatement,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "for_generate_statement_preamble",
                kind: LayoutItemKind::Node(NodeKind::ForGenerateStatementPreamble),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "generate_statement_body",
                kind: LayoutItemKind::Node(NodeKind::GenerateStatementBody),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "for_generate_statement_epilogue",
                kind: LayoutItemKind::Node(NodeKind::ForGenerateStatementEpilogue),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ForGenerateStatementSyntax(node)
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ForGenerateStatementPreamble,
        items: &[
            LayoutItem {
                optional: true,
                repeated: false,
                name: "label",
                kind: LayoutItemKind::Node(NodeKind::Label),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "for",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::For)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "parameter_specification",
                kind: LayoutItemKind::Node(NodeKind::ParameterSpecification),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "generate",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Generate)),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ForGenerateStatementPreambleSyntax(node)
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
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::For))
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
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Generate))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ForGenerateStatementEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for ForGenerateStatementEpilogueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ForGenerateStatementEpilogue,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "end",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::End)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "generate",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Generate)),
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
        ForGenerateStatementEpilogueSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ForGenerateStatementEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::End))
            .nth(0)
    }
    pub fn generate_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Generate))
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
pub struct GenerateStatementBodySyntax(pub(crate) SyntaxNode);
impl AstNode for GenerateStatementBodySyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::GenerateStatementBody,
        items: &[
            LayoutItem {
                optional: true,
                repeated: false,
                name: "declarations",
                kind: LayoutItemKind::Node(NodeKind::Declarations),
            },
            LayoutItem {
                optional: true,
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
                optional: true,
                repeated: false,
                name: "generate_statement_body_epilogue",
                kind: LayoutItemKind::Node(NodeKind::GenerateStatementBodyEpilogue),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        GenerateStatementBodySyntax(node)
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::GenerateStatementBodyEpilogue,
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
        GenerateStatementBodyEpilogueSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl GenerateStatementBodyEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::End))
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
pub struct IfGenerateElsifSyntax(pub(crate) SyntaxNode);
impl AstNode for IfGenerateElsifSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::IfGenerateElsif,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "elsif",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Elsif)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "label",
                kind: LayoutItemKind::Node(NodeKind::Label),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "expression",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::LiteralExpression,
                    NodeKind::PhysicalLiteralExpression,
                    NodeKind::UnaryExpression,
                    NodeKind::BinaryExpression,
                    NodeKind::ParenthesizedExpressionOrAggregate,
                    NodeKind::SubtypeIndicationAllocator,
                    NodeKind::ExpressionAllocator,
                    NodeKind::QualifiedExpression,
                    NodeKind::TypeConversion,
                    NodeKind::NameExpression,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "generate",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Generate)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "generate_statement_body",
                kind: LayoutItemKind::Node(NodeKind::GenerateStatementBody),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        IfGenerateElsifSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl IfGenerateElsifSyntax {
    pub fn elsif_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Elsif))
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
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Generate))
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::IfGenerateElse,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "else",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Else)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "label",
                kind: LayoutItemKind::Node(NodeKind::Label),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "generate",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Generate)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "generate_statement_body",
                kind: LayoutItemKind::Node(NodeKind::GenerateStatementBody),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        IfGenerateElseSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl IfGenerateElseSyntax {
    pub fn else_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Else))
            .nth(0)
    }
    pub fn label(&self) -> Option<LabelSyntax> {
        self.0.children().filter_map(LabelSyntax::cast).nth(0)
    }
    pub fn generate_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Generate))
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::IfGenerateStatement,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "if_generate_statement_preamble",
                kind: LayoutItemKind::Node(NodeKind::IfGenerateStatementPreamble),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "generate_statement_body",
                kind: LayoutItemKind::Node(NodeKind::GenerateStatementBody),
            },
            LayoutItem {
                optional: false,
                repeated: true,
                name: "if_generate_elsifs",
                kind: LayoutItemKind::Node(NodeKind::IfGenerateElsif),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "if_generate_else",
                kind: LayoutItemKind::Node(NodeKind::IfGenerateElse),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "if_generate_statement_epilogue",
                kind: LayoutItemKind::Node(NodeKind::IfGenerateStatementEpilogue),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        IfGenerateStatementSyntax(node)
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::IfGenerateStatementPreamble,
        items: &[
            LayoutItem {
                optional: true,
                repeated: false,
                name: "label",
                kind: LayoutItemKind::Node(NodeKind::Label),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "if",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::If)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "alternative_label",
                kind: LayoutItemKind::Node(NodeKind::Label),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "condition",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::LiteralExpression,
                    NodeKind::PhysicalLiteralExpression,
                    NodeKind::UnaryExpression,
                    NodeKind::BinaryExpression,
                    NodeKind::ParenthesizedExpressionOrAggregate,
                    NodeKind::SubtypeIndicationAllocator,
                    NodeKind::ExpressionAllocator,
                    NodeKind::QualifiedExpression,
                    NodeKind::TypeConversion,
                    NodeKind::NameExpression,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "generate",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Generate)),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        IfGenerateStatementPreambleSyntax(node)
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
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::If))
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
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Generate))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct IfGenerateStatementEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for IfGenerateStatementEpilogueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::IfGenerateStatementEpilogue,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "end",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::End)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "generate",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Generate)),
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
        IfGenerateStatementEpilogueSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl IfGenerateStatementEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::End))
            .nth(0)
    }
    pub fn generate_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Generate))
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
pub enum InstantiatedUnitSyntax {
    ComponentInstantiatedUnit(ComponentInstantiatedUnitSyntax),
    EntityInstantiatedUnit(EntityInstantiatedUnitSyntax),
    ConfigurationInstantiatedUnit(ConfigurationInstantiatedUnitSyntax),
}
impl AstNode for InstantiatedUnitSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::ComponentInstantiatedUnit,
            NodeKind::EntityInstantiatedUnit,
            NodeKind::ConfigurationInstantiatedUnit,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if ComponentInstantiatedUnitSyntax::can_cast(&node) {
            return InstantiatedUnitSyntax::ComponentInstantiatedUnit(
                ComponentInstantiatedUnitSyntax::cast_unchecked(node),
            );
        }
        if EntityInstantiatedUnitSyntax::can_cast(&node) {
            return InstantiatedUnitSyntax::EntityInstantiatedUnit(
                EntityInstantiatedUnitSyntax::cast_unchecked(node),
            );
        }
        if ConfigurationInstantiatedUnitSyntax::can_cast(&node) {
            return InstantiatedUnitSyntax::ConfigurationInstantiatedUnit(
                ConfigurationInstantiatedUnitSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ComponentInstantiatedUnit,
        items: &[
            LayoutItem {
                optional: true,
                repeated: false,
                name: "component",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Component)),
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
        ComponentInstantiatedUnitSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ComponentInstantiatedUnitSyntax {
    pub fn component_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Component))
            .nth(0)
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct EntityInstantiatedUnitSyntax(pub(crate) SyntaxNode);
impl AstNode for EntityInstantiatedUnitSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::EntityInstantiatedUnit,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "entity",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Entity)),
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
                name: "left_par",
                kind: LayoutItemKind::Token(TokenKind::LeftPar),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "identifier",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "right_par",
                kind: LayoutItemKind::Token(TokenKind::RightPar),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        EntityInstantiatedUnitSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl EntityInstantiatedUnitSyntax {
    pub fn entity_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Entity))
            .nth(0)
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::LeftPar)
            .nth(0)
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Identifier)
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
pub struct ConfigurationInstantiatedUnitSyntax(pub(crate) SyntaxNode);
impl AstNode for ConfigurationInstantiatedUnitSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ConfigurationInstantiatedUnit,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "configuration",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Configuration)),
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
        ConfigurationInstantiatedUnitSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConfigurationInstantiatedUnitSyntax {
    pub fn configuration_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Configuration))
            .nth(0)
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct AllSensitivityListSyntax(pub(crate) SyntaxNode);
impl AstNode for AllSensitivityListSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::AllSensitivityList,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "all",
            kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::All)),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        AllSensitivityListSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl AllSensitivityListSyntax {
    pub fn all_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::All))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct SensitivityListSyntax(pub(crate) SyntaxNode);
impl AstNode for SensitivityListSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SensitivityList,
        items: &[
            LayoutItem {
                optional: false,
                repeated: true,
                name: "names",
                kind: LayoutItemKind::Node(NodeKind::Name),
            },
            LayoutItem {
                optional: false,
                repeated: true,
                name: "comma",
                kind: LayoutItemKind::Token(TokenKind::Comma),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        SensitivityListSyntax(node)
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
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Comma)
    }
}
#[derive(Debug, Clone)]
pub enum ProcessSensitivityListSyntax {
    AllSensitivityList(AllSensitivityListSyntax),
    SensitivityList(SensitivityListSyntax),
}
impl AstNode for ProcessSensitivityListSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[NodeKind::AllSensitivityList, NodeKind::SensitivityList],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if AllSensitivityListSyntax::can_cast(&node) {
            return ProcessSensitivityListSyntax::AllSensitivityList(
                AllSensitivityListSyntax::cast_unchecked(node),
            );
        }
        if SensitivityListSyntax::can_cast(&node) {
            return ProcessSensitivityListSyntax::SensitivityList(
                SensitivityListSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ProcessStatement,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "process_statement_preamble",
                kind: LayoutItemKind::Node(NodeKind::ProcessStatementPreamble),
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
                name: "process_statement_epilogue",
                kind: LayoutItemKind::Node(NodeKind::ProcessStatementEpilogue),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ProcessStatementSyntax(node)
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ParenthesizedProcessSensitivityList,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "left_par",
                kind: LayoutItemKind::Token(TokenKind::LeftPar),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "process_sensitivity_list",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::AllSensitivityList,
                    NodeKind::SensitivityList,
                ]),
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
        ParenthesizedProcessSensitivityListSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ParenthesizedProcessSensitivityListSyntax {
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::LeftPar)
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
            .filter(|token| token.kind() == TokenKind::RightPar)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ProcessStatementPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for ProcessStatementPreambleSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ProcessStatementPreamble,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "label",
                kind: LayoutItemKind::Node(NodeKind::Label),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "postponed",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Postponed)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "process",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Process)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "parenthesized_process_sensitivity_list",
                kind: LayoutItemKind::Node(NodeKind::ParenthesizedProcessSensitivityList),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "is",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Is)),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ProcessStatementPreambleSyntax(node)
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
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Postponed))
            .nth(0)
    }
    pub fn process_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Process))
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
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Is))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ProcessStatementEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for ProcessStatementEpilogueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ProcessStatementEpilogue,
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
                name: "postponed",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Postponed)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "process",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Process)),
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
        ProcessStatementEpilogueSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ProcessStatementEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::End))
            .nth(0)
    }
    pub fn postponed_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Postponed))
            .nth(0)
    }
    pub fn process_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Process))
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
