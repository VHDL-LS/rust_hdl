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
pub struct AssertionSyntax(pub(crate) SyntaxNode);
impl AstNode for AssertionSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::Assertion,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Assert)),
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
                optional: true,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Report)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "report",
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
                optional: true,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Severity)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "severity",
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
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        AssertionSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl AssertionSyntax {
    pub fn assert_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Assert))
            .nth(0)
    }
    pub fn condition(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn report_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Report))
            .nth(0)
    }
    pub fn report(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(1)
    }
    pub fn severity_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Severity))
            .nth(0)
    }
    pub fn severity(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(2)
    }
}
#[derive(Debug, Clone)]
pub struct AssertionStatementSyntax(pub(crate) SyntaxNode);
impl AstNode for AssertionStatementSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::AssertionStatement,
        items: &[
            LayoutItem {
                optional: true,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Colon),
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
                name: "",
                kind: LayoutItemKind::Token(TokenKind::SemiColon),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        AssertionStatementSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl AssertionStatementSyntax {
    pub fn label_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Identifier)
            .nth(0)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Colon)
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
pub struct CaseStatementAlternativeSyntax(pub(crate) SyntaxNode);
impl AstNode for CaseStatementAlternativeSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::CaseStatementAlternative,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "case_statement_alternative_preamble",
                kind: LayoutItemKind::Node(NodeKind::CaseStatementAlternativePreamble),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "sequential_statements",
                kind: LayoutItemKind::Node(NodeKind::SequentialStatements),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        CaseStatementAlternativeSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl CaseStatementAlternativeSyntax {
    pub fn case_statement_alternative_preamble(
        &self,
    ) -> Option<CaseStatementAlternativePreambleSyntax> {
        self.0
            .children()
            .filter_map(CaseStatementAlternativePreambleSyntax::cast)
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
pub struct CaseStatementAlternativePreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for CaseStatementAlternativePreambleSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::CaseStatementAlternativePreamble,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::When)),
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
                name: "",
                kind: LayoutItemKind::Token(TokenKind::RightArrow),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        CaseStatementAlternativePreambleSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl CaseStatementAlternativePreambleSyntax {
    pub fn when_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::When))
            .nth(0)
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
}
#[derive(Debug, Clone)]
pub struct CaseStatementSyntax(pub(crate) SyntaxNode);
impl AstNode for CaseStatementSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::CaseStatement,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "case_statement_preamble",
                kind: LayoutItemKind::Node(NodeKind::CaseStatementPreamble),
            },
            LayoutItem {
                optional: false,
                repeated: true,
                name: "case_statement_alternatives",
                kind: LayoutItemKind::Node(NodeKind::CaseStatementAlternative),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "case_statement_epilogue",
                kind: LayoutItemKind::Node(NodeKind::CaseStatementEpilogue),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        CaseStatementSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl CaseStatementSyntax {
    pub fn case_statement_preamble(&self) -> Option<CaseStatementPreambleSyntax> {
        self.0
            .children()
            .filter_map(CaseStatementPreambleSyntax::cast)
            .nth(0)
    }
    pub fn case_statement_alternatives(
        &self,
    ) -> impl Iterator<Item = CaseStatementAlternativeSyntax> + use<'_> {
        self.0
            .children()
            .filter_map(CaseStatementAlternativeSyntax::cast)
    }
    pub fn case_statement_epilogue(&self) -> Option<CaseStatementEpilogueSyntax> {
        self.0
            .children()
            .filter_map(CaseStatementEpilogueSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct CaseStatementPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for CaseStatementPreambleSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::CaseStatementPreamble,
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
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Case)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Que),
            },
            LayoutItem {
                optional: true,
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
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Is)),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        CaseStatementPreambleSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl CaseStatementPreambleSyntax {
    pub fn label(&self) -> Option<LabelSyntax> {
        self.0.children().filter_map(LabelSyntax::cast).nth(0)
    }
    pub fn case_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Case))
            .nth(0)
    }
    pub fn que_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Que)
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn is_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Is))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct CaseStatementEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for CaseStatementEpilogueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::CaseStatementEpilogue,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::End)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Case)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Que),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::SemiColon),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        CaseStatementEpilogueSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl CaseStatementEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::End))
            .nth(0)
    }
    pub fn case_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Case))
            .nth(0)
    }
    pub fn que_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Que)
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
pub struct ConditionClauseSyntax(pub(crate) SyntaxNode);
impl AstNode for ConditionClauseSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ConditionClause,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Until)),
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
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ConditionClauseSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConditionClauseSyntax {
    pub fn until_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Until))
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ConditionalElseWhenExpressionSyntax(pub(crate) SyntaxNode);
impl AstNode for ConditionalElseWhenExpressionSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ConditionalElseWhenExpression,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Else)),
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
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::When)),
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
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ConditionalElseWhenExpressionSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConditionalElseWhenExpressionSyntax {
    pub fn else_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Else))
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn when_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::When))
            .nth(0)
    }
    pub fn condition(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(1)
    }
}
#[derive(Debug, Clone)]
pub struct ConditionalElseItemSyntax(pub(crate) SyntaxNode);
impl AstNode for ConditionalElseItemSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ConditionalElseItem,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Else)),
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
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ConditionalElseItemSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConditionalElseItemSyntax {
    pub fn else_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Else))
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ConditionalExpressionsSyntax(pub(crate) SyntaxNode);
impl AstNode for ConditionalExpressionsSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ConditionalExpressions,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "conditional_expression",
                kind: LayoutItemKind::Node(NodeKind::ConditionalExpression),
            },
            LayoutItem {
                optional: false,
                repeated: true,
                name: "conditional_else_when_expressions",
                kind: LayoutItemKind::Node(NodeKind::ConditionalElseWhenExpression),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "conditional_else_item",
                kind: LayoutItemKind::Node(NodeKind::ConditionalElseItem),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ConditionalExpressionsSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConditionalExpressionsSyntax {
    pub fn conditional_expression(&self) -> Option<ConditionalExpressionSyntax> {
        self.0
            .children()
            .filter_map(ConditionalExpressionSyntax::cast)
            .nth(0)
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
pub struct ConditionalExpressionSyntax(pub(crate) SyntaxNode);
impl AstNode for ConditionalExpressionSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ConditionalExpression,
        items: &[
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
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::When)),
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
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ConditionalExpressionSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConditionalExpressionSyntax {
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn when_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::When))
            .nth(0)
    }
    pub fn condition(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(1)
    }
}
#[derive(Debug, Clone)]
pub struct ConditionalForceAssignmentSyntax(pub(crate) SyntaxNode);
impl AstNode for ConditionalForceAssignmentSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ConditionalForceAssignment,
        items: &[
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
                name: "",
                kind: LayoutItemKind::Token(TokenKind::LTE),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Force)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "force_mode",
                kind: LayoutItemKind::TokenGroup(&[
                    TokenKind::Keyword(Kw::In),
                    TokenKind::Keyword(Kw::Out),
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "conditional_expressions",
                kind: LayoutItemKind::Node(NodeKind::ConditionalExpressions),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::SemiColon),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ConditionalForceAssignmentSyntax(node)
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
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::LTE)
            .nth(0)
    }
    pub fn force_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Force))
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
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum ConditionalSignalAssignmentSyntax {
    ConditionalWaveformAssignment(ConditionalWaveformAssignmentSyntax),
    ConditionalForceAssignment(ConditionalForceAssignmentSyntax),
}
impl AstNode for ConditionalSignalAssignmentSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::ConditionalWaveformAssignment,
            NodeKind::ConditionalForceAssignment,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if ConditionalWaveformAssignmentSyntax::can_cast(&node) {
            return ConditionalSignalAssignmentSyntax::ConditionalWaveformAssignment(
                ConditionalWaveformAssignmentSyntax::cast_unchecked(node),
            );
        }
        if ConditionalForceAssignmentSyntax::can_cast(&node) {
            return ConditionalSignalAssignmentSyntax::ConditionalForceAssignment(
                ConditionalForceAssignmentSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ConditionalVariableAssignment,
        items: &[
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
                name: "",
                kind: LayoutItemKind::Token(TokenKind::ColonEq),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "conditional_expressions",
                kind: LayoutItemKind::Node(NodeKind::ConditionalExpressions),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::SemiColon),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ConditionalVariableAssignmentSyntax(node)
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
            .filter(|token| token.kind() == TokenKind::ColonEq)
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
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ConditionalWaveformAssignmentSyntax(pub(crate) SyntaxNode);
impl AstNode for ConditionalWaveformAssignmentSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ConditionalWaveformAssignment,
        items: &[
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
                name: "",
                kind: LayoutItemKind::Token(TokenKind::LTE),
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
                name: "",
                kind: LayoutItemKind::Token(TokenKind::SemiColon),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ConditionalWaveformAssignmentSyntax(node)
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
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::LTE)
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
pub struct ConditionalWaveformElseWhenExpressionSyntax(pub(crate) SyntaxNode);
impl AstNode for ConditionalWaveformElseWhenExpressionSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ConditionalWaveformElseWhenExpression,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Else)),
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
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::When)),
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
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ConditionalWaveformElseWhenExpressionSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConditionalWaveformElseWhenExpressionSyntax {
    pub fn else_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Else))
            .nth(0)
    }
    pub fn waveform(&self) -> Option<WaveformSyntax> {
        self.0.children().filter_map(WaveformSyntax::cast).nth(0)
    }
    pub fn when_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::When))
            .nth(0)
    }
    pub fn condition(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ConditionalWaveformElseItemSyntax(pub(crate) SyntaxNode);
impl AstNode for ConditionalWaveformElseItemSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ConditionalWaveformElseItem,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Else)),
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
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ConditionalWaveformElseItemSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConditionalWaveformElseItemSyntax {
    pub fn else_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Else))
            .nth(0)
    }
    pub fn waveform(&self) -> Option<WaveformSyntax> {
        self.0.children().filter_map(WaveformSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ConditionalWaveformsSyntax(pub(crate) SyntaxNode);
impl AstNode for ConditionalWaveformsSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ConditionalWaveforms,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "conditional_waveform",
                kind: LayoutItemKind::Node(NodeKind::ConditionalWaveform),
            },
            LayoutItem {
                optional: false,
                repeated: true,
                name: "conditional_waveform_else_when_expressions",
                kind: LayoutItemKind::Node(NodeKind::ConditionalWaveformElseWhenExpression),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "conditional_waveform_else_item",
                kind: LayoutItemKind::Node(NodeKind::ConditionalWaveformElseItem),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ConditionalWaveformsSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConditionalWaveformsSyntax {
    pub fn conditional_waveform(&self) -> Option<ConditionalWaveformSyntax> {
        self.0
            .children()
            .filter_map(ConditionalWaveformSyntax::cast)
            .nth(0)
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
pub struct ConditionalWaveformSyntax(pub(crate) SyntaxNode);
impl AstNode for ConditionalWaveformSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ConditionalWaveform,
        items: &[
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
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::When)),
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
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ConditionalWaveformSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConditionalWaveformSyntax {
    pub fn waveform(&self) -> Option<WaveformSyntax> {
        self.0.children().filter_map(WaveformSyntax::cast).nth(0)
    }
    pub fn when_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::When))
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct TransportDelayMechanismSyntax(pub(crate) SyntaxNode);
impl AstNode for TransportDelayMechanismSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::TransportDelayMechanism,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "",
            kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Transport)),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        TransportDelayMechanismSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl TransportDelayMechanismSyntax {
    pub fn transport_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Transport))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct InertialDelayMechanismSyntax(pub(crate) SyntaxNode);
impl AstNode for InertialDelayMechanismSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::InertialDelayMechanism,
        items: &[
            LayoutItem {
                optional: true,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Reject)),
            },
            LayoutItem {
                optional: true,
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
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Inertial)),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        InertialDelayMechanismSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InertialDelayMechanismSyntax {
    pub fn reject_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Reject))
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn inertial_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Inertial))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum DelayMechanismSyntax {
    TransportDelayMechanism(TransportDelayMechanismSyntax),
    InertialDelayMechanism(InertialDelayMechanismSyntax),
}
impl AstNode for DelayMechanismSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::TransportDelayMechanism,
            NodeKind::InertialDelayMechanism,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if TransportDelayMechanismSyntax::can_cast(&node) {
            return DelayMechanismSyntax::TransportDelayMechanism(
                TransportDelayMechanismSyntax::cast_unchecked(node),
            );
        }
        if InertialDelayMechanismSyntax::can_cast(&node) {
            return DelayMechanismSyntax::InertialDelayMechanism(
                InertialDelayMechanismSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ExitStatement,
        items: &[
            LayoutItem {
                optional: true,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Colon),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Exit)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::When)),
            },
            LayoutItem {
                optional: true,
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
                name: "",
                kind: LayoutItemKind::Token(TokenKind::SemiColon),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ExitStatementSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ExitStatementSyntax {
    pub fn label_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Identifier)
            .nth(0)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Colon)
            .nth(0)
    }
    pub fn exit_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Exit))
            .nth(0)
    }
    pub fn loop_label_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Identifier)
            .nth(1)
    }
    pub fn when_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::When))
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::SemiColon)
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
            TokenKind::Keyword(Kw::In) => Some(ForceModeSyntax::In(token)),
            TokenKind::Keyword(Kw::Out) => Some(ForceModeSyntax::Out(token)),
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::IfStatementElsif,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Elsif)),
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
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Then)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "sequential_statements",
                kind: LayoutItemKind::Node(NodeKind::SequentialStatements),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        IfStatementElsifSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl IfStatementElsifSyntax {
    pub fn elsif_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Elsif))
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn then_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Then))
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::IfStatementElse,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Else)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "sequential_statements",
                kind: LayoutItemKind::Node(NodeKind::SequentialStatements),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        IfStatementElseSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl IfStatementElseSyntax {
    pub fn else_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Else))
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::IfStatement,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "if_statement_preamble",
                kind: LayoutItemKind::Node(NodeKind::IfStatementPreamble),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "sequential_statements",
                kind: LayoutItemKind::Node(NodeKind::SequentialStatements),
            },
            LayoutItem {
                optional: false,
                repeated: true,
                name: "if_statement_elsifs",
                kind: LayoutItemKind::Node(NodeKind::IfStatementElsif),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "if_statement_else",
                kind: LayoutItemKind::Node(NodeKind::IfStatementElse),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "if_statement_epilogue",
                kind: LayoutItemKind::Node(NodeKind::IfStatementEpilogue),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        IfStatementSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl IfStatementSyntax {
    pub fn if_statement_preamble(&self) -> Option<IfStatementPreambleSyntax> {
        self.0
            .children()
            .filter_map(IfStatementPreambleSyntax::cast)
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
    pub fn if_statement_epilogue(&self) -> Option<IfStatementEpilogueSyntax> {
        self.0
            .children()
            .filter_map(IfStatementEpilogueSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct IfStatementPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for IfStatementPreambleSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::IfStatementPreamble,
        items: &[
            LayoutItem {
                optional: true,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Colon),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::If)),
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
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Then)),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        IfStatementPreambleSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl IfStatementPreambleSyntax {
    pub fn if_label_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Identifier)
            .nth(0)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Colon)
            .nth(0)
    }
    pub fn if_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::If))
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn then_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Then))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct IfStatementEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for IfStatementEpilogueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::IfStatementEpilogue,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::End)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::If)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::SemiColon),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        IfStatementEpilogueSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl IfStatementEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::End))
            .nth(0)
    }
    pub fn if_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::If))
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
pub struct WhileIterationSchemeSyntax(pub(crate) SyntaxNode);
impl AstNode for WhileIterationSchemeSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::WhileIterationScheme,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::While)),
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
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        WhileIterationSchemeSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl WhileIterationSchemeSyntax {
    pub fn while_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::While))
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ForIterationSchemeSyntax(pub(crate) SyntaxNode);
impl AstNode for ForIterationSchemeSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ForIterationScheme,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::For)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "parameter_specification",
                kind: LayoutItemKind::Node(NodeKind::ParameterSpecification),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ForIterationSchemeSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ForIterationSchemeSyntax {
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
}
#[derive(Debug, Clone)]
pub enum IterationSchemeSyntax {
    WhileIterationScheme(WhileIterationSchemeSyntax),
    ForIterationScheme(ForIterationSchemeSyntax),
}
impl AstNode for IterationSchemeSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[NodeKind::WhileIterationScheme, NodeKind::ForIterationScheme],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if WhileIterationSchemeSyntax::can_cast(&node) {
            return IterationSchemeSyntax::WhileIterationScheme(
                WhileIterationSchemeSyntax::cast_unchecked(node),
            );
        }
        if ForIterationSchemeSyntax::can_cast(&node) {
            return IterationSchemeSyntax::ForIterationScheme(
                ForIterationSchemeSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::LoopStatement,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "loop_statement_preamble",
                kind: LayoutItemKind::Node(NodeKind::LoopStatementPreamble),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "sequential_statements",
                kind: LayoutItemKind::Node(NodeKind::SequentialStatements),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "loop_statement_epilogue",
                kind: LayoutItemKind::Node(NodeKind::LoopStatementEpilogue),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        LoopStatementSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl LoopStatementSyntax {
    pub fn loop_statement_preamble(&self) -> Option<LoopStatementPreambleSyntax> {
        self.0
            .children()
            .filter_map(LoopStatementPreambleSyntax::cast)
            .nth(0)
    }
    pub fn sequential_statements(&self) -> Option<SequentialStatementsSyntax> {
        self.0
            .children()
            .filter_map(SequentialStatementsSyntax::cast)
            .nth(0)
    }
    pub fn loop_statement_epilogue(&self) -> Option<LoopStatementEpilogueSyntax> {
        self.0
            .children()
            .filter_map(LoopStatementEpilogueSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct LoopStatementPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for LoopStatementPreambleSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::LoopStatementPreamble,
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
                name: "iteration_scheme",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::WhileIterationScheme,
                    NodeKind::ForIterationScheme,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Loop)),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        LoopStatementPreambleSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl LoopStatementPreambleSyntax {
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
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Loop))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct LoopStatementEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for LoopStatementEpilogueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::LoopStatementEpilogue,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::End)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Loop)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::SemiColon),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        LoopStatementEpilogueSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl LoopStatementEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::End))
            .nth(0)
    }
    pub fn loop_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Loop))
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
pub struct NextStatementSyntax(pub(crate) SyntaxNode);
impl AstNode for NextStatementSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::NextStatement,
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
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Next)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::When)),
            },
            LayoutItem {
                optional: true,
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
                name: "",
                kind: LayoutItemKind::Token(TokenKind::SemiColon),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        NextStatementSyntax(node)
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
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Next))
            .nth(0)
    }
    pub fn loop_label_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Identifier)
            .nth(0)
    }
    pub fn when_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::When))
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct NullStatementSyntax(pub(crate) SyntaxNode);
impl AstNode for NullStatementSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::NullStatement,
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
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Null)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::SemiColon),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        NullStatementSyntax(node)
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
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Null))
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
pub struct ParameterSpecificationSyntax(pub(crate) SyntaxNode);
impl AstNode for ParameterSpecificationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ParameterSpecification,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::In)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "discrete_range",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::SubtypeIndicationDiscreteDiscreteRange,
                    NodeKind::SubtypeIndicationDiscreteRange,
                    NodeKind::OpenDiscreteRange,
                ]),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ParameterSpecificationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ParameterSpecificationSyntax {
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Identifier)
            .nth(0)
    }
    pub fn in_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::In))
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ProcedureCallStatement,
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
                name: "name",
                kind: LayoutItemKind::Node(NodeKind::Name),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::SemiColon),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ProcedureCallStatementSyntax(node)
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
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ReportStatementSyntax(pub(crate) SyntaxNode);
impl AstNode for ReportStatementSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ReportStatement,
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
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Report)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "report",
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
                optional: true,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Severity)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "severity",
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
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ReportStatementSyntax(node)
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
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Report))
            .nth(0)
    }
    pub fn report(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn severity_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Severity))
            .nth(0)
    }
    pub fn severity(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(1)
    }
}
#[derive(Debug, Clone)]
pub struct ReturnStatementSyntax(pub(crate) SyntaxNode);
impl AstNode for ReturnStatementSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ReturnStatement,
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
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Return)),
            },
            LayoutItem {
                optional: true,
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
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ReturnStatementSyntax(node)
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
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Return))
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct SelectedExpressionItemSyntax(pub(crate) SyntaxNode);
impl AstNode for SelectedExpressionItemSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SelectedExpressionItem,
        items: &[
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
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::When)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "choices",
                kind: LayoutItemKind::Node(NodeKind::Choices),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        SelectedExpressionItemSyntax(node)
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
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::When))
            .nth(0)
    }
    pub fn choices(&self) -> Option<ChoicesSyntax> {
        self.0.children().filter_map(ChoicesSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct SelectedExpressionsSyntax(pub(crate) SyntaxNode);
impl AstNode for SelectedExpressionsSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SelectedExpressions,
        items: &[
            LayoutItem {
                optional: false,
                repeated: true,
                name: "selected_expression_items",
                kind: LayoutItemKind::Node(NodeKind::SelectedExpressionItem),
            },
            LayoutItem {
                optional: false,
                repeated: true,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Comma),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        SelectedExpressionsSyntax(node)
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
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Comma)
    }
}
#[derive(Debug, Clone)]
pub struct SelectedForceAssignmentSyntax(pub(crate) SyntaxNode);
impl AstNode for SelectedForceAssignmentSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SelectedForceAssignment,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "selected_assignment_preamble",
                kind: LayoutItemKind::Node(NodeKind::SelectedAssignmentPreamble),
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
                name: "",
                kind: LayoutItemKind::Token(TokenKind::LTE),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Force)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "force_mode",
                kind: LayoutItemKind::TokenGroup(&[
                    TokenKind::Keyword(Kw::In),
                    TokenKind::Keyword(Kw::Out),
                ]),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "selected_expressions",
                kind: LayoutItemKind::Node(NodeKind::SelectedExpressions),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::SemiColon),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        SelectedForceAssignmentSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SelectedForceAssignmentSyntax {
    pub fn selected_assignment_preamble(&self) -> Option<SelectedAssignmentPreambleSyntax> {
        self.0
            .children()
            .filter_map(SelectedAssignmentPreambleSyntax::cast)
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
    pub fn force_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Force))
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
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct SelectedWaveformItemSyntax(pub(crate) SyntaxNode);
impl AstNode for SelectedWaveformItemSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SelectedWaveformItem,
        items: &[
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
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::When)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "choices",
                kind: LayoutItemKind::Node(NodeKind::Choices),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        SelectedWaveformItemSyntax(node)
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
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::When))
            .nth(0)
    }
    pub fn choices(&self) -> Option<ChoicesSyntax> {
        self.0.children().filter_map(ChoicesSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct SelectedWaveformsSyntax(pub(crate) SyntaxNode);
impl AstNode for SelectedWaveformsSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SelectedWaveforms,
        items: &[
            LayoutItem {
                optional: false,
                repeated: true,
                name: "selected_waveform_items",
                kind: LayoutItemKind::Node(NodeKind::SelectedWaveformItem),
            },
            LayoutItem {
                optional: false,
                repeated: true,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Comma),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        SelectedWaveformsSyntax(node)
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
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Comma)
    }
}
#[derive(Debug, Clone)]
pub enum SelectedSignalAssignmentSyntax {
    SelectedWaveformAssignment(SelectedWaveformAssignmentSyntax),
    SelectedForceAssignment(SelectedForceAssignmentSyntax),
}
impl AstNode for SelectedSignalAssignmentSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::SelectedWaveformAssignment,
            NodeKind::SelectedForceAssignment,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if SelectedWaveformAssignmentSyntax::can_cast(&node) {
            return SelectedSignalAssignmentSyntax::SelectedWaveformAssignment(
                SelectedWaveformAssignmentSyntax::cast_unchecked(node),
            );
        }
        if SelectedForceAssignmentSyntax::can_cast(&node) {
            return SelectedSignalAssignmentSyntax::SelectedForceAssignment(
                SelectedForceAssignmentSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SelectedVariableAssignment,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "selected_assignment_preamble",
                kind: LayoutItemKind::Node(NodeKind::SelectedAssignmentPreamble),
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
                name: "",
                kind: LayoutItemKind::Token(TokenKind::ColonEq),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "selected_expressions",
                kind: LayoutItemKind::Node(NodeKind::SelectedExpressions),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::SemiColon),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        SelectedVariableAssignmentSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SelectedVariableAssignmentSyntax {
    pub fn selected_assignment_preamble(&self) -> Option<SelectedAssignmentPreambleSyntax> {
        self.0
            .children()
            .filter_map(SelectedAssignmentPreambleSyntax::cast)
            .nth(0)
    }
    pub fn target(&self) -> Option<TargetSyntax> {
        self.0.children().filter_map(TargetSyntax::cast).nth(0)
    }
    pub fn colon_eq_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::ColonEq)
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
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct SelectedWaveformAssignmentSyntax(pub(crate) SyntaxNode);
impl AstNode for SelectedWaveformAssignmentSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SelectedWaveformAssignment,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "selected_assignment_preamble",
                kind: LayoutItemKind::Node(NodeKind::SelectedAssignmentPreamble),
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
                name: "",
                kind: LayoutItemKind::Token(TokenKind::LTE),
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
                name: "",
                kind: LayoutItemKind::Token(TokenKind::SemiColon),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        SelectedWaveformAssignmentSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SelectedWaveformAssignmentSyntax {
    pub fn selected_assignment_preamble(&self) -> Option<SelectedAssignmentPreambleSyntax> {
        self.0
            .children()
            .filter_map(SelectedAssignmentPreambleSyntax::cast)
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
pub struct SelectedAssignmentPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for SelectedAssignmentPreambleSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SelectedAssignmentPreamble,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
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
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Select)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Que),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        SelectedAssignmentPreambleSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SelectedAssignmentPreambleSyntax {
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
pub struct SensitivityClauseSyntax(pub(crate) SyntaxNode);
impl AstNode for SensitivityClauseSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SensitivityClause,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::On)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "name_list",
                kind: LayoutItemKind::Node(NodeKind::NameList),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        SensitivityClauseSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SensitivityClauseSyntax {
    pub fn on_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::On))
            .nth(0)
    }
    pub fn name_list(&self) -> Option<NameListSyntax> {
        self.0.children().filter_map(NameListSyntax::cast).nth(0)
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
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::WaitStatement,
            NodeKind::AssertionStatement,
            NodeKind::ReportStatement,
            NodeKind::SimpleWaveformAssignment,
            NodeKind::SimpleForceAssignment,
            NodeKind::SimpleReleaseAssignment,
            NodeKind::ConditionalWaveformAssignment,
            NodeKind::ConditionalForceAssignment,
            NodeKind::SelectedWaveformAssignment,
            NodeKind::SelectedForceAssignment,
            NodeKind::SimpleVariableAssignment,
            NodeKind::ConditionalVariableAssignment,
            NodeKind::SelectedVariableAssignment,
            NodeKind::ProcedureCallStatement,
            NodeKind::IfStatement,
            NodeKind::CaseStatement,
            NodeKind::LoopStatement,
            NodeKind::NextStatement,
            NodeKind::ExitStatement,
            NodeKind::ReturnStatement,
            NodeKind::NullStatement,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if WaitStatementSyntax::can_cast(&node) {
            return SequentialStatementSyntax::WaitStatement(WaitStatementSyntax::cast_unchecked(
                node,
            ));
        }
        if AssertionStatementSyntax::can_cast(&node) {
            return SequentialStatementSyntax::AssertionStatement(
                AssertionStatementSyntax::cast_unchecked(node),
            );
        }
        if ReportStatementSyntax::can_cast(&node) {
            return SequentialStatementSyntax::ReportStatement(
                ReportStatementSyntax::cast_unchecked(node),
            );
        }
        if SignalAssignmentStatementSyntax::can_cast(&node) {
            return SequentialStatementSyntax::SignalAssignmentStatement(
                SignalAssignmentStatementSyntax::cast_unchecked(node),
            );
        }
        if VariableAssignmentStatementSyntax::can_cast(&node) {
            return SequentialStatementSyntax::VariableAssignmentStatement(
                VariableAssignmentStatementSyntax::cast_unchecked(node),
            );
        }
        if ProcedureCallStatementSyntax::can_cast(&node) {
            return SequentialStatementSyntax::ProcedureCallStatement(
                ProcedureCallStatementSyntax::cast_unchecked(node),
            );
        }
        if IfStatementSyntax::can_cast(&node) {
            return SequentialStatementSyntax::IfStatement(IfStatementSyntax::cast_unchecked(node));
        }
        if CaseStatementSyntax::can_cast(&node) {
            return SequentialStatementSyntax::CaseStatement(CaseStatementSyntax::cast_unchecked(
                node,
            ));
        }
        if LoopStatementSyntax::can_cast(&node) {
            return SequentialStatementSyntax::LoopStatement(LoopStatementSyntax::cast_unchecked(
                node,
            ));
        }
        if NextStatementSyntax::can_cast(&node) {
            return SequentialStatementSyntax::NextStatement(NextStatementSyntax::cast_unchecked(
                node,
            ));
        }
        if ExitStatementSyntax::can_cast(&node) {
            return SequentialStatementSyntax::ExitStatement(ExitStatementSyntax::cast_unchecked(
                node,
            ));
        }
        if ReturnStatementSyntax::can_cast(&node) {
            return SequentialStatementSyntax::ReturnStatement(
                ReturnStatementSyntax::cast_unchecked(node),
            );
        }
        if NullStatementSyntax::can_cast(&node) {
            return SequentialStatementSyntax::NullStatement(NullStatementSyntax::cast_unchecked(
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
pub enum SignalAssignmentStatementSyntax {
    SimpleSignalAssignment(SimpleSignalAssignmentSyntax),
    ConditionalSignalAssignment(ConditionalSignalAssignmentSyntax),
    SelectedSignalAssignment(SelectedSignalAssignmentSyntax),
}
impl AstNode for SignalAssignmentStatementSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::SimpleWaveformAssignment,
            NodeKind::SimpleForceAssignment,
            NodeKind::SimpleReleaseAssignment,
            NodeKind::ConditionalWaveformAssignment,
            NodeKind::ConditionalForceAssignment,
            NodeKind::SelectedWaveformAssignment,
            NodeKind::SelectedForceAssignment,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if SimpleSignalAssignmentSyntax::can_cast(&node) {
            return SignalAssignmentStatementSyntax::SimpleSignalAssignment(
                SimpleSignalAssignmentSyntax::cast_unchecked(node),
            );
        }
        if ConditionalSignalAssignmentSyntax::can_cast(&node) {
            return SignalAssignmentStatementSyntax::ConditionalSignalAssignment(
                ConditionalSignalAssignmentSyntax::cast_unchecked(node),
            );
        }
        if SelectedSignalAssignmentSyntax::can_cast(&node) {
            return SignalAssignmentStatementSyntax::SelectedSignalAssignment(
                SelectedSignalAssignmentSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            SignalAssignmentStatementSyntax::SimpleSignalAssignment(inner) => inner.raw(),
            SignalAssignmentStatementSyntax::ConditionalSignalAssignment(inner) => inner.raw(),
            SignalAssignmentStatementSyntax::SelectedSignalAssignment(inner) => inner.raw(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct SimpleForceAssignmentSyntax(pub(crate) SyntaxNode);
impl AstNode for SimpleForceAssignmentSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SimpleForceAssignment,
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
                name: "target",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::NameTarget,
                    NodeKind::AggregateTarget,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::LTE),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Force)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "force_mode",
                kind: LayoutItemKind::TokenGroup(&[
                    TokenKind::Keyword(Kw::In),
                    TokenKind::Keyword(Kw::Out),
                ]),
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
                name: "",
                kind: LayoutItemKind::Token(TokenKind::SemiColon),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        SimpleForceAssignmentSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SimpleForceAssignmentSyntax {
    pub fn label(&self) -> Option<LabelSyntax> {
        self.0.children().filter_map(LabelSyntax::cast).nth(0)
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
    pub fn force_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Force))
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
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct SimpleReleaseAssignmentSyntax(pub(crate) SyntaxNode);
impl AstNode for SimpleReleaseAssignmentSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SimpleReleaseAssignment,
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
                name: "target",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::NameTarget,
                    NodeKind::AggregateTarget,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::LTE),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Release)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "force_mode",
                kind: LayoutItemKind::TokenGroup(&[
                    TokenKind::Keyword(Kw::In),
                    TokenKind::Keyword(Kw::Out),
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::SemiColon),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        SimpleReleaseAssignmentSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SimpleReleaseAssignmentSyntax {
    pub fn label(&self) -> Option<LabelSyntax> {
        self.0.children().filter_map(LabelSyntax::cast).nth(0)
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
    pub fn release_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Release))
            .nth(0)
    }
    pub fn force_mode(&self) -> Option<ForceModeSyntax> {
        self.0.tokens().filter_map(ForceModeSyntax::cast).nth(0)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct SimpleWaveformAssignmentSyntax(pub(crate) SyntaxNode);
impl AstNode for SimpleWaveformAssignmentSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SimpleWaveformAssignment,
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
                name: "target",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::NameTarget,
                    NodeKind::AggregateTarget,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::LTE),
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
                name: "",
                kind: LayoutItemKind::Token(TokenKind::SemiColon),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        SimpleWaveformAssignmentSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SimpleWaveformAssignmentSyntax {
    pub fn label(&self) -> Option<LabelSyntax> {
        self.0.children().filter_map(LabelSyntax::cast).nth(0)
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
pub enum SimpleSignalAssignmentSyntax {
    SimpleWaveformAssignment(SimpleWaveformAssignmentSyntax),
    SimpleForceAssignment(SimpleForceAssignmentSyntax),
    SimpleReleaseAssignment(SimpleReleaseAssignmentSyntax),
}
impl AstNode for SimpleSignalAssignmentSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::SimpleWaveformAssignment,
            NodeKind::SimpleForceAssignment,
            NodeKind::SimpleReleaseAssignment,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if SimpleWaveformAssignmentSyntax::can_cast(&node) {
            return SimpleSignalAssignmentSyntax::SimpleWaveformAssignment(
                SimpleWaveformAssignmentSyntax::cast_unchecked(node),
            );
        }
        if SimpleForceAssignmentSyntax::can_cast(&node) {
            return SimpleSignalAssignmentSyntax::SimpleForceAssignment(
                SimpleForceAssignmentSyntax::cast_unchecked(node),
            );
        }
        if SimpleReleaseAssignmentSyntax::can_cast(&node) {
            return SimpleSignalAssignmentSyntax::SimpleReleaseAssignment(
                SimpleReleaseAssignmentSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SimpleVariableAssignment,
        items: &[
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
                name: "",
                kind: LayoutItemKind::Token(TokenKind::ColonEq),
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
                name: "",
                kind: LayoutItemKind::Token(TokenKind::SemiColon),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        SimpleVariableAssignmentSyntax(node)
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
            .filter(|token| token.kind() == TokenKind::ColonEq)
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct NameTargetSyntax(pub(crate) SyntaxNode);
impl AstNode for NameTargetSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::NameTarget,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "name",
            kind: LayoutItemKind::Node(NodeKind::Name),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        NameTargetSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl NameTargetSyntax {
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct AggregateTargetSyntax(pub(crate) SyntaxNode);
impl AstNode for AggregateTargetSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::AggregateTarget,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "aggregate",
            kind: LayoutItemKind::Node(NodeKind::Aggregate),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        AggregateTargetSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl AggregateTargetSyntax {
    pub fn aggregate(&self) -> Option<AggregateSyntax> {
        self.0.children().filter_map(AggregateSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum TargetSyntax {
    NameTarget(NameTargetSyntax),
    AggregateTarget(AggregateTargetSyntax),
}
impl AstNode for TargetSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[NodeKind::NameTarget, NodeKind::AggregateTarget],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if NameTargetSyntax::can_cast(&node) {
            return TargetSyntax::NameTarget(NameTargetSyntax::cast_unchecked(node));
        }
        if AggregateTargetSyntax::can_cast(&node) {
            return TargetSyntax::AggregateTarget(AggregateTargetSyntax::cast_unchecked(node));
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            TargetSyntax::NameTarget(inner) => inner.raw(),
            TargetSyntax::AggregateTarget(inner) => inner.raw(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct TimeoutClauseSyntax(pub(crate) SyntaxNode);
impl AstNode for TimeoutClauseSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::TimeoutClause,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::For)),
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
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        TimeoutClauseSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl TimeoutClauseSyntax {
    pub fn for_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::For))
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
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::SimpleVariableAssignment,
            NodeKind::ConditionalVariableAssignment,
            NodeKind::SelectedVariableAssignment,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if SimpleVariableAssignmentSyntax::can_cast(&node) {
            return VariableAssignmentStatementSyntax::SimpleVariableAssignment(
                SimpleVariableAssignmentSyntax::cast_unchecked(node),
            );
        }
        if ConditionalVariableAssignmentSyntax::can_cast(&node) {
            return VariableAssignmentStatementSyntax::ConditionalVariableAssignment(
                ConditionalVariableAssignmentSyntax::cast_unchecked(node),
            );
        }
        if SelectedVariableAssignmentSyntax::can_cast(&node) {
            return VariableAssignmentStatementSyntax::SelectedVariableAssignment(
                SelectedVariableAssignmentSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::WaitStatement,
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
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Wait)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "sensitivity_clause",
                kind: LayoutItemKind::Node(NodeKind::SensitivityClause),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "condition_clause",
                kind: LayoutItemKind::Node(NodeKind::ConditionClause),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "timeout_clause",
                kind: LayoutItemKind::Node(NodeKind::TimeoutClause),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::SemiColon),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        WaitStatementSyntax(node)
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
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Wait))
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
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct WaveformElementSyntax(pub(crate) SyntaxNode);
impl AstNode for WaveformElementSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::WaveformElement,
        items: &[
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
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::After)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "time_expression",
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
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        WaveformElementSyntax(node)
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
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::After))
            .nth(0)
    }
    pub fn time_expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(1)
    }
}
#[derive(Debug, Clone)]
pub struct WaveformElementsSyntax(pub(crate) SyntaxNode);
impl AstNode for WaveformElementsSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::WaveformElements,
        items: &[
            LayoutItem {
                optional: false,
                repeated: true,
                name: "waveform_elements",
                kind: LayoutItemKind::Node(NodeKind::WaveformElement),
            },
            LayoutItem {
                optional: false,
                repeated: true,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Comma),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        WaveformElementsSyntax(node)
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
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Comma)
    }
}
#[derive(Debug, Clone)]
pub struct UnaffectedWaveformSyntax(pub(crate) SyntaxNode);
impl AstNode for UnaffectedWaveformSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::UnaffectedWaveform,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "",
            kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Unaffected)),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        UnaffectedWaveformSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl UnaffectedWaveformSyntax {
    pub fn unaffected_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Unaffected))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum WaveformSyntax {
    WaveformElements(WaveformElementsSyntax),
    UnaffectedWaveform(UnaffectedWaveformSyntax),
}
impl AstNode for WaveformSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[NodeKind::WaveformElements, NodeKind::UnaffectedWaveform],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if WaveformElementsSyntax::can_cast(&node) {
            return WaveformSyntax::WaveformElements(WaveformElementsSyntax::cast_unchecked(node));
        }
        if UnaffectedWaveformSyntax::can_cast(&node) {
            return WaveformSyntax::UnaffectedWaveform(UnaffectedWaveformSyntax::cast_unchecked(
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
            WaveformSyntax::WaveformElements(inner) => inner.raw(),
            WaveformSyntax::UnaffectedWaveform(inner) => inner.raw(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct SequentialStatementsSyntax(pub(crate) SyntaxNode);
impl AstNode for SequentialStatementsSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SequentialStatements,
        items: &[LayoutItem {
            optional: false,
            repeated: true,
            name: "sequential_statements",
            kind: LayoutItemKind::NodeChoice(&[
                NodeKind::WaitStatement,
                NodeKind::AssertionStatement,
                NodeKind::ReportStatement,
                NodeKind::SimpleWaveformAssignment,
                NodeKind::SimpleForceAssignment,
                NodeKind::SimpleReleaseAssignment,
                NodeKind::ConditionalWaveformAssignment,
                NodeKind::ConditionalForceAssignment,
                NodeKind::SelectedWaveformAssignment,
                NodeKind::SelectedForceAssignment,
                NodeKind::SimpleVariableAssignment,
                NodeKind::ConditionalVariableAssignment,
                NodeKind::SelectedVariableAssignment,
                NodeKind::ProcedureCallStatement,
                NodeKind::IfStatement,
                NodeKind::CaseStatement,
                NodeKind::LoopStatement,
                NodeKind::NextStatement,
                NodeKind::ExitStatement,
                NodeKind::ReturnStatement,
                NodeKind::NullStatement,
            ]),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        SequentialStatementsSyntax(node)
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
