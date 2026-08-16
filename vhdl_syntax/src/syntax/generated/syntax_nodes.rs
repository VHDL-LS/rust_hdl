// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2026, Lukas Scheller lukasscheller@icloud.com
use super::*;
use crate::syntax::meta::{Choice, Layout, LayoutItem, LayoutItemKind, List, Sequence};
use crate::syntax::node::{SyntaxNode, SyntaxToken};
use crate::syntax::node_kind::NodeKind;
use crate::syntax::AstNode;
use crate::tokens::Keyword as Kw;
use crate::tokens::TokenKind;
#[derive(Debug, Clone)]
pub struct AbsolutePathnameSyntax(pub(crate) SyntaxNode);
impl AstNode for AbsolutePathnameSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::AbsolutePathname,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "dot",
                kind: LayoutItemKind::Token(TokenKind::Dot),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "partial_pathname",
                kind: LayoutItemKind::Node(NodeKind::PartialPathname),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        AbsolutePathnameSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl AbsolutePathnameSyntax {
    pub fn dot_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Dot)
            .nth(0)
    }
    pub fn partial_pathname(&self) -> Option<PartialPathnameSyntax> {
        self.0
            .children()
            .filter_map(PartialPathnameSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct AccessTypeDefinitionSyntax(pub(crate) SyntaxNode);
impl AstNode for AccessTypeDefinitionSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::AccessTypeDefinition,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "access",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Access)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "subtype_indication",
                kind: LayoutItemKind::Node(NodeKind::SubtypeIndication),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        AccessTypeDefinitionSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl AccessTypeDefinitionSyntax {
    pub fn access_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Access))
            .nth(0)
    }
    pub fn subtype_indication(&self) -> Option<SubtypeIndicationSyntax> {
        self.0
            .children()
            .filter_map(SubtypeIndicationSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ActualPartSyntax(pub(crate) SyntaxNode);
impl AstNode for ActualPartSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ActualPart,
        items: &[
            LayoutItem {
                optional: true,
                repeated: false,
                name: "inertial",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Inertial)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "actual_part_body",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::ActualPartExpression,
                    NodeKind::ActualPartSubtypeIndication,
                    NodeKind::ActualPartOpen,
                ]),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ActualPartSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ActualPartSyntax {
    pub fn inertial_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Inertial))
            .nth(0)
    }
    pub fn actual_part_body(&self) -> Option<ActualPartBodySyntax> {
        self.0
            .children()
            .filter_map(ActualPartBodySyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum ActualPartBodySyntax {
    ActualPartExpression(ActualPartExpressionSyntax),
    ActualPartSubtypeIndication(ActualPartSubtypeIndicationSyntax),
    ActualPartOpen(ActualPartOpenSyntax),
}
impl AstNode for ActualPartBodySyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::ActualPartExpression,
            NodeKind::ActualPartSubtypeIndication,
            NodeKind::ActualPartOpen,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if ActualPartExpressionSyntax::can_cast(&node) {
            return ActualPartBodySyntax::ActualPartExpression(
                ActualPartExpressionSyntax::cast_unchecked(node),
            );
        }
        if ActualPartSubtypeIndicationSyntax::can_cast(&node) {
            return ActualPartBodySyntax::ActualPartSubtypeIndication(
                ActualPartSubtypeIndicationSyntax::cast_unchecked(node),
            );
        }
        if ActualPartOpenSyntax::can_cast(&node) {
            return ActualPartBodySyntax::ActualPartOpen(ActualPartOpenSyntax::cast_unchecked(
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
            ActualPartBodySyntax::ActualPartExpression(inner) => inner.raw(),
            ActualPartBodySyntax::ActualPartSubtypeIndication(inner) => inner.raw(),
            ActualPartBodySyntax::ActualPartOpen(inner) => inner.raw(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct ActualPartExpressionSyntax(pub(crate) SyntaxNode);
impl AstNode for ActualPartExpressionSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ActualPartExpression,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "expression",
            kind: LayoutItemKind::NodeChoice(&[
                NodeKind::LiteralExpression,
                NodeKind::PhysicalLiteralExpression,
                NodeKind::UnaryExpression,
                NodeKind::BinaryExpression,
                NodeKind::ParenthesizedExpressionOrAggregate,
                NodeKind::Allocator,
                NodeKind::NameExpression,
                NodeKind::QualifiedExpression,
            ]),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ActualPartExpressionSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ActualPartExpressionSyntax {
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ActualPartOpenSyntax(pub(crate) SyntaxNode);
impl AstNode for ActualPartOpenSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ActualPartOpen,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "open",
            kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Open)),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ActualPartOpenSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ActualPartOpenSyntax {
    pub fn open_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Open))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ActualPartSubtypeIndicationSyntax(pub(crate) SyntaxNode);
impl AstNode for ActualPartSubtypeIndicationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ActualPartSubtypeIndication,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "subtype_indication",
            kind: LayoutItemKind::Node(NodeKind::SubtypeIndication),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ActualPartSubtypeIndicationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ActualPartSubtypeIndicationSyntax {
    pub fn subtype_indication(&self) -> Option<SubtypeIndicationSyntax> {
        self.0
            .children()
            .filter_map(SubtypeIndicationSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct AfterClauseSyntax(pub(crate) SyntaxNode);
impl AstNode for AfterClauseSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::AfterClause,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "after",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::After)),
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
                    NodeKind::Allocator,
                    NodeKind::NameExpression,
                    NodeKind::QualifiedExpression,
                ]),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        AfterClauseSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl AfterClauseSyntax {
    pub fn after_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::After))
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct AggregateSyntax(pub(crate) SyntaxNode);
impl AstNode for AggregateSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::Aggregate,
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
                name: "element_association_list",
                kind: LayoutItemKind::Node(NodeKind::ElementAssociationList),
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
        AggregateSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl AggregateSyntax {
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::LeftPar)
            .nth(0)
    }
    pub fn element_association_list(&self) -> Option<ElementAssociationListSyntax> {
        self.0
            .children()
            .filter_map(ElementAssociationListSyntax::cast)
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
pub struct AliasDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for AliasDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::AliasDeclaration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "alias",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Alias)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "alias_designator",
                kind: LayoutItemKind::TokenChoice(&[
                    TokenKind::Identifier,
                    TokenKind::CharacterLiteral,
                    TokenKind::StringLiteral,
                ]),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "alias_subtype",
                kind: LayoutItemKind::Node(NodeKind::AliasSubtype),
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
                name: "name",
                kind: LayoutItemKind::Node(NodeKind::Name),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "signature",
                kind: LayoutItemKind::Node(NodeKind::Signature),
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
        AliasDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl AliasDeclarationSyntax {
    pub fn alias_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Alias))
            .nth(0)
    }
    pub fn alias_designator(&self) -> Option<AliasDesignatorSyntax> {
        self.0
            .tokens()
            .filter_map(AliasDesignatorSyntax::cast)
            .nth(0)
    }
    pub fn alias_subtype(&self) -> Option<AliasSubtypeSyntax> {
        self.0
            .children()
            .filter_map(AliasSubtypeSyntax::cast)
            .nth(0)
    }
    pub fn is_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Is))
            .nth(0)
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
    pub fn signature(&self) -> Option<SignatureSyntax> {
        self.0.children().filter_map(SignatureSyntax::cast).nth(0)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum AliasDesignatorSyntax {
    Identifier(SyntaxToken),
    CharacterLiteral(SyntaxToken),
    StringLiteral(SyntaxToken),
}
impl AliasDesignatorSyntax {
    pub fn cast(token: SyntaxToken) -> Option<Self> {
        match token.kind() {
            TokenKind::Identifier => Some(AliasDesignatorSyntax::Identifier(token)),
            TokenKind::CharacterLiteral => Some(AliasDesignatorSyntax::CharacterLiteral(token)),
            TokenKind::StringLiteral => Some(AliasDesignatorSyntax::StringLiteral(token)),
            _ => None,
        }
    }
    pub fn raw(&self) -> SyntaxToken {
        match self {
            AliasDesignatorSyntax::Identifier(token) => token.clone(),
            AliasDesignatorSyntax::CharacterLiteral(token) => token.clone(),
            AliasDesignatorSyntax::StringLiteral(token) => token.clone(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct AliasSubtypeSyntax(pub(crate) SyntaxNode);
impl AstNode for AliasSubtypeSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::AliasSubtype,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "colon",
                kind: LayoutItemKind::Token(TokenKind::Colon),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "subtype_indication",
                kind: LayoutItemKind::Node(NodeKind::SubtypeIndication),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        AliasSubtypeSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl AliasSubtypeSyntax {
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Colon)
            .nth(0)
    }
    pub fn subtype_indication(&self) -> Option<SubtypeIndicationSyntax> {
        self.0
            .children()
            .filter_map(SubtypeIndicationSyntax::cast)
            .nth(0)
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
pub struct AllocatorSyntax(pub(crate) SyntaxNode);
impl AstNode for AllocatorSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::Allocator,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "new",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::New)),
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
                    NodeKind::Allocator,
                    NodeKind::NameExpression,
                    NodeKind::QualifiedExpression,
                ]),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        AllocatorSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl AllocatorSyntax {
    pub fn new_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::New))
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ArchitectureBodySyntax(pub(crate) SyntaxNode);
impl AstNode for ArchitectureBodySyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ArchitectureBody,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "architecture_preamble",
                kind: LayoutItemKind::Node(NodeKind::ArchitecturePreamble),
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
                name: "architecture_epilogue",
                kind: LayoutItemKind::Node(NodeKind::ArchitectureEpilogue),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ArchitectureBodySyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ArchitectureBodySyntax {
    pub fn architecture_preamble(&self) -> Option<ArchitecturePreambleSyntax> {
        self.0
            .children()
            .filter_map(ArchitecturePreambleSyntax::cast)
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
    pub fn architecture_epilogue(&self) -> Option<ArchitectureEpilogueSyntax> {
        self.0
            .children()
            .filter_map(ArchitectureEpilogueSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ArchitectureEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for ArchitectureEpilogueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ArchitectureEpilogue,
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
                name: "architecture",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Architecture)),
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
        ArchitectureEpilogueSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ArchitectureEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::End))
            .nth(0)
    }
    pub fn architecture_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Architecture))
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
pub struct ArchitecturePreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for ArchitecturePreambleSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ArchitecturePreamble,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "architecture",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Architecture)),
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
                name: "of",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Of)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "entity_name",
                kind: LayoutItemKind::Node(NodeKind::Name),
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
        ArchitecturePreambleSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ArchitecturePreambleSyntax {
    pub fn architecture_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Architecture))
            .nth(0)
    }
    pub fn name_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Identifier)
            .nth(0)
    }
    pub fn of_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Of))
            .nth(0)
    }
    pub fn entity_name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
    pub fn is_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Is))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum ArrayTypeDefinitionSyntax {
    UnboundedArrayDefinition(UnboundedArrayDefinitionSyntax),
    ConstrainedArrayDefinition(ConstrainedArrayDefinitionSyntax),
}
impl AstNode for ArrayTypeDefinitionSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::UnboundedArrayDefinition,
            NodeKind::ConstrainedArrayDefinition,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if UnboundedArrayDefinitionSyntax::can_cast(&node) {
            return ArrayTypeDefinitionSyntax::UnboundedArrayDefinition(
                UnboundedArrayDefinitionSyntax::cast_unchecked(node),
            );
        }
        if ConstrainedArrayDefinitionSyntax::can_cast(&node) {
            return ArrayTypeDefinitionSyntax::ConstrainedArrayDefinition(
                ConstrainedArrayDefinitionSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            ArrayTypeDefinitionSyntax::UnboundedArrayDefinition(inner) => inner.raw(),
            ArrayTypeDefinitionSyntax::ConstrainedArrayDefinition(inner) => inner.raw(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct AssertionSyntax(pub(crate) SyntaxNode);
impl AstNode for AssertionSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::Assertion,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "assert",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Assert)),
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
                    NodeKind::Allocator,
                    NodeKind::NameExpression,
                    NodeKind::QualifiedExpression,
                ]),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "report_clause",
                kind: LayoutItemKind::Node(NodeKind::ReportClause),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "severity_clause",
                kind: LayoutItemKind::Node(NodeKind::SeverityClause),
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
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn report_clause(&self) -> Option<ReportClauseSyntax> {
        self.0
            .children()
            .filter_map(ReportClauseSyntax::cast)
            .nth(0)
    }
    pub fn severity_clause(&self) -> Option<SeverityClauseSyntax> {
        self.0
            .children()
            .filter_map(SeverityClauseSyntax::cast)
            .nth(0)
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
                name: "label",
                kind: LayoutItemKind::Node(NodeKind::Label),
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
        AssertionStatementSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl AssertionStatementSyntax {
    pub fn label(&self) -> Option<LabelSyntax> {
        self.0.children().filter_map(LabelSyntax::cast).nth(0)
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
pub struct AssociationElementSyntax(pub(crate) SyntaxNode);
impl AstNode for AssociationElementSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::AssociationElement,
        items: &[
            LayoutItem {
                optional: true,
                repeated: false,
                name: "formal",
                kind: LayoutItemKind::Node(NodeKind::Formal),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "actual_part",
                kind: LayoutItemKind::Node(NodeKind::ActualPart),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        AssociationElementSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl AssociationElementSyntax {
    pub fn formal(&self) -> Option<FormalSyntax> {
        self.0.children().filter_map(FormalSyntax::cast).nth(0)
    }
    pub fn actual_part(&self) -> Option<ActualPartSyntax> {
        self.0.children().filter_map(ActualPartSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct AssociationListSyntax(pub(crate) SyntaxNode);
impl AstNode for AssociationListSyntax {
    const META: &'static Layout = &Layout::List(List {
        kind: NodeKind::AssociationList,
        element: &LayoutItem {
            optional: false,
            repeated: true,
            name: "association_elements",
            kind: LayoutItemKind::Node(NodeKind::AssociationElement),
        },
        separator: &LayoutItem {
            optional: false,
            repeated: true,
            name: "comma",
            kind: LayoutItemKind::Token(TokenKind::Comma),
        },
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        AssociationListSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl AssociationListSyntax {
    pub fn association_elements(&self) -> impl Iterator<Item = AssociationElementSyntax> + use<'_> {
        self.0.children().filter_map(AssociationElementSyntax::cast)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Comma)
    }
}
#[derive(Debug, Clone)]
pub struct AttributeDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for AttributeDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::AttributeDeclaration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "attribute",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Attribute)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "identifier",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "colon",
                kind: LayoutItemKind::Token(TokenKind::Colon),
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
        AttributeDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl AttributeDeclarationSyntax {
    pub fn attribute_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Attribute))
            .nth(0)
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
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
pub enum AttributeDesignatorSyntax {
    Identifier(SyntaxToken),
    Range(SyntaxToken),
    Subtype(SyntaxToken),
}
impl AttributeDesignatorSyntax {
    pub fn cast(token: SyntaxToken) -> Option<Self> {
        match token.kind() {
            TokenKind::Identifier => Some(AttributeDesignatorSyntax::Identifier(token)),
            TokenKind::Keyword(Kw::Range) => Some(AttributeDesignatorSyntax::Range(token)),
            TokenKind::Keyword(Kw::Subtype) => Some(AttributeDesignatorSyntax::Subtype(token)),
            _ => None,
        }
    }
    pub fn raw(&self) -> SyntaxToken {
        match self {
            AttributeDesignatorSyntax::Identifier(token) => token.clone(),
            AttributeDesignatorSyntax::Range(token) => token.clone(),
            AttributeDesignatorSyntax::Subtype(token) => token.clone(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct AttributeNameSyntax(pub(crate) SyntaxNode);
impl AstNode for AttributeNameSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::AttributeName,
        items: &[
            LayoutItem {
                optional: true,
                repeated: false,
                name: "signature",
                kind: LayoutItemKind::Node(NodeKind::Signature),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "tick",
                kind: LayoutItemKind::Token(TokenKind::Tick),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "attribute_designator_token",
                kind: LayoutItemKind::TokenChoice(&[
                    TokenKind::Identifier,
                    TokenKind::Keyword(Kw::Range),
                    TokenKind::Keyword(Kw::Subtype),
                ]),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        AttributeNameSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl AttributeNameSyntax {
    pub fn signature(&self) -> Option<SignatureSyntax> {
        self.0.children().filter_map(SignatureSyntax::cast).nth(0)
    }
    pub fn tick_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Tick)
            .nth(0)
    }
    pub fn attribute_designator_token(&self) -> Option<AttributeDesignatorSyntax> {
        self.0
            .tokens()
            .filter_map(AttributeDesignatorSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct AttributeSpecificationSyntax(pub(crate) SyntaxNode);
impl AstNode for AttributeSpecificationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::AttributeSpecification,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "attribute",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Attribute)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "attribute_designator_token",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "of",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Of)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "entity_specification",
                kind: LayoutItemKind::Node(NodeKind::EntitySpecification),
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
                name: "expression",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::LiteralExpression,
                    NodeKind::PhysicalLiteralExpression,
                    NodeKind::UnaryExpression,
                    NodeKind::BinaryExpression,
                    NodeKind::ParenthesizedExpressionOrAggregate,
                    NodeKind::Allocator,
                    NodeKind::NameExpression,
                    NodeKind::QualifiedExpression,
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
        AttributeSpecificationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl AttributeSpecificationSyntax {
    pub fn attribute_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Attribute))
            .nth(0)
    }
    pub fn attribute_designator_token_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Identifier)
            .nth(0)
    }
    pub fn of_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Of))
            .nth(0)
    }
    pub fn entity_specification(&self) -> Option<EntitySpecificationSyntax> {
        self.0
            .children()
            .filter_map(EntitySpecificationSyntax::cast)
            .nth(0)
    }
    pub fn is_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Is))
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
pub struct BinaryExpressionSyntax(pub(crate) SyntaxNode);
impl AstNode for BinaryExpressionSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::BinaryExpression,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "lhs",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::LiteralExpression,
                    NodeKind::PhysicalLiteralExpression,
                    NodeKind::UnaryExpression,
                    NodeKind::BinaryExpression,
                    NodeKind::ParenthesizedExpressionOrAggregate,
                    NodeKind::Allocator,
                    NodeKind::NameExpression,
                    NodeKind::QualifiedExpression,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "op",
                kind: LayoutItemKind::TokenChoice(&[
                    TokenKind::Keyword(Kw::And),
                    TokenKind::Keyword(Kw::Or),
                    TokenKind::Keyword(Kw::Nand),
                    TokenKind::Keyword(Kw::Nor),
                    TokenKind::Keyword(Kw::Xor),
                    TokenKind::Keyword(Kw::Xnor),
                    TokenKind::EQ,
                    TokenKind::NE,
                    TokenKind::LT,
                    TokenKind::LTE,
                    TokenKind::GT,
                    TokenKind::GTE,
                    TokenKind::QueEQ,
                    TokenKind::QueNE,
                    TokenKind::QueLTE,
                    TokenKind::QueLT,
                    TokenKind::QueGT,
                    TokenKind::QueGTE,
                    TokenKind::Keyword(Kw::Sll),
                    TokenKind::Keyword(Kw::Srl),
                    TokenKind::Keyword(Kw::Sla),
                    TokenKind::Keyword(Kw::Sra),
                    TokenKind::Keyword(Kw::Rol),
                    TokenKind::Keyword(Kw::Ror),
                    TokenKind::Plus,
                    TokenKind::Minus,
                    TokenKind::Concat,
                    TokenKind::Times,
                    TokenKind::Div,
                    TokenKind::Keyword(Kw::Mod),
                    TokenKind::Keyword(Kw::Rem),
                    TokenKind::Pow,
                    TokenKind::Keyword(Kw::To),
                    TokenKind::Keyword(Kw::Downto),
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "rhs",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::LiteralExpression,
                    NodeKind::PhysicalLiteralExpression,
                    NodeKind::UnaryExpression,
                    NodeKind::BinaryExpression,
                    NodeKind::ParenthesizedExpressionOrAggregate,
                    NodeKind::Allocator,
                    NodeKind::NameExpression,
                    NodeKind::QualifiedExpression,
                ]),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        BinaryExpressionSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl BinaryExpressionSyntax {
    pub fn lhs(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn op(&self) -> Option<BinaryOperatorSyntax> {
        self.0
            .tokens()
            .filter_map(BinaryOperatorSyntax::cast)
            .nth(0)
    }
    pub fn rhs(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(1)
    }
}
#[derive(Debug, Clone)]
pub enum BinaryOperatorSyntax {
    And(SyntaxToken),
    Or(SyntaxToken),
    Nand(SyntaxToken),
    Nor(SyntaxToken),
    Xor(SyntaxToken),
    Xnor(SyntaxToken),
    Eq(SyntaxToken),
    Ne(SyntaxToken),
    Lt(SyntaxToken),
    Lte(SyntaxToken),
    Gt(SyntaxToken),
    Gte(SyntaxToken),
    QueEq(SyntaxToken),
    QueNe(SyntaxToken),
    QueLte(SyntaxToken),
    QueLt(SyntaxToken),
    QueGt(SyntaxToken),
    QueGte(SyntaxToken),
    Sll(SyntaxToken),
    Srl(SyntaxToken),
    Sla(SyntaxToken),
    Sra(SyntaxToken),
    Rol(SyntaxToken),
    Ror(SyntaxToken),
    Plus(SyntaxToken),
    Minus(SyntaxToken),
    Concat(SyntaxToken),
    Times(SyntaxToken),
    Div(SyntaxToken),
    Mod(SyntaxToken),
    Rem(SyntaxToken),
    Pow(SyntaxToken),
    To(SyntaxToken),
    Downto(SyntaxToken),
}
impl BinaryOperatorSyntax {
    pub fn cast(token: SyntaxToken) -> Option<Self> {
        match token.kind() {
            TokenKind::Keyword(Kw::And) => Some(BinaryOperatorSyntax::And(token)),
            TokenKind::Keyword(Kw::Or) => Some(BinaryOperatorSyntax::Or(token)),
            TokenKind::Keyword(Kw::Nand) => Some(BinaryOperatorSyntax::Nand(token)),
            TokenKind::Keyword(Kw::Nor) => Some(BinaryOperatorSyntax::Nor(token)),
            TokenKind::Keyword(Kw::Xor) => Some(BinaryOperatorSyntax::Xor(token)),
            TokenKind::Keyword(Kw::Xnor) => Some(BinaryOperatorSyntax::Xnor(token)),
            TokenKind::EQ => Some(BinaryOperatorSyntax::Eq(token)),
            TokenKind::NE => Some(BinaryOperatorSyntax::Ne(token)),
            TokenKind::LT => Some(BinaryOperatorSyntax::Lt(token)),
            TokenKind::LTE => Some(BinaryOperatorSyntax::Lte(token)),
            TokenKind::GT => Some(BinaryOperatorSyntax::Gt(token)),
            TokenKind::GTE => Some(BinaryOperatorSyntax::Gte(token)),
            TokenKind::QueEQ => Some(BinaryOperatorSyntax::QueEq(token)),
            TokenKind::QueNE => Some(BinaryOperatorSyntax::QueNe(token)),
            TokenKind::QueLTE => Some(BinaryOperatorSyntax::QueLte(token)),
            TokenKind::QueLT => Some(BinaryOperatorSyntax::QueLt(token)),
            TokenKind::QueGT => Some(BinaryOperatorSyntax::QueGt(token)),
            TokenKind::QueGTE => Some(BinaryOperatorSyntax::QueGte(token)),
            TokenKind::Keyword(Kw::Sll) => Some(BinaryOperatorSyntax::Sll(token)),
            TokenKind::Keyword(Kw::Srl) => Some(BinaryOperatorSyntax::Srl(token)),
            TokenKind::Keyword(Kw::Sla) => Some(BinaryOperatorSyntax::Sla(token)),
            TokenKind::Keyword(Kw::Sra) => Some(BinaryOperatorSyntax::Sra(token)),
            TokenKind::Keyword(Kw::Rol) => Some(BinaryOperatorSyntax::Rol(token)),
            TokenKind::Keyword(Kw::Ror) => Some(BinaryOperatorSyntax::Ror(token)),
            TokenKind::Plus => Some(BinaryOperatorSyntax::Plus(token)),
            TokenKind::Minus => Some(BinaryOperatorSyntax::Minus(token)),
            TokenKind::Concat => Some(BinaryOperatorSyntax::Concat(token)),
            TokenKind::Times => Some(BinaryOperatorSyntax::Times(token)),
            TokenKind::Div => Some(BinaryOperatorSyntax::Div(token)),
            TokenKind::Keyword(Kw::Mod) => Some(BinaryOperatorSyntax::Mod(token)),
            TokenKind::Keyword(Kw::Rem) => Some(BinaryOperatorSyntax::Rem(token)),
            TokenKind::Pow => Some(BinaryOperatorSyntax::Pow(token)),
            TokenKind::Keyword(Kw::To) => Some(BinaryOperatorSyntax::To(token)),
            TokenKind::Keyword(Kw::Downto) => Some(BinaryOperatorSyntax::Downto(token)),
            _ => None,
        }
    }
    pub fn raw(&self) -> SyntaxToken {
        match self {
            BinaryOperatorSyntax::And(token) => token.clone(),
            BinaryOperatorSyntax::Or(token) => token.clone(),
            BinaryOperatorSyntax::Nand(token) => token.clone(),
            BinaryOperatorSyntax::Nor(token) => token.clone(),
            BinaryOperatorSyntax::Xor(token) => token.clone(),
            BinaryOperatorSyntax::Xnor(token) => token.clone(),
            BinaryOperatorSyntax::Eq(token) => token.clone(),
            BinaryOperatorSyntax::Ne(token) => token.clone(),
            BinaryOperatorSyntax::Lt(token) => token.clone(),
            BinaryOperatorSyntax::Lte(token) => token.clone(),
            BinaryOperatorSyntax::Gt(token) => token.clone(),
            BinaryOperatorSyntax::Gte(token) => token.clone(),
            BinaryOperatorSyntax::QueEq(token) => token.clone(),
            BinaryOperatorSyntax::QueNe(token) => token.clone(),
            BinaryOperatorSyntax::QueLte(token) => token.clone(),
            BinaryOperatorSyntax::QueLt(token) => token.clone(),
            BinaryOperatorSyntax::QueGt(token) => token.clone(),
            BinaryOperatorSyntax::QueGte(token) => token.clone(),
            BinaryOperatorSyntax::Sll(token) => token.clone(),
            BinaryOperatorSyntax::Srl(token) => token.clone(),
            BinaryOperatorSyntax::Sla(token) => token.clone(),
            BinaryOperatorSyntax::Sra(token) => token.clone(),
            BinaryOperatorSyntax::Rol(token) => token.clone(),
            BinaryOperatorSyntax::Ror(token) => token.clone(),
            BinaryOperatorSyntax::Plus(token) => token.clone(),
            BinaryOperatorSyntax::Minus(token) => token.clone(),
            BinaryOperatorSyntax::Concat(token) => token.clone(),
            BinaryOperatorSyntax::Times(token) => token.clone(),
            BinaryOperatorSyntax::Div(token) => token.clone(),
            BinaryOperatorSyntax::Mod(token) => token.clone(),
            BinaryOperatorSyntax::Rem(token) => token.clone(),
            BinaryOperatorSyntax::Pow(token) => token.clone(),
            BinaryOperatorSyntax::To(token) => token.clone(),
            BinaryOperatorSyntax::Downto(token) => token.clone(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct BindingSyntax(pub(crate) SyntaxNode);
impl AstNode for BindingSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::Binding,
        items: &[
            LayoutItem {
                optional: true,
                repeated: false,
                name: "binding_indication",
                kind: LayoutItemKind::Node(NodeKind::BindingIndication),
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
        BindingSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl BindingSyntax {
    pub fn binding_indication(&self) -> Option<BindingIndicationSyntax> {
        self.0
            .children()
            .filter_map(BindingIndicationSyntax::cast)
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
pub struct BindingIndicationSyntax(pub(crate) SyntaxNode);
impl AstNode for BindingIndicationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::BindingIndication,
        items: &[
            LayoutItem {
                optional: true,
                repeated: false,
                name: "binding_use_clause",
                kind: LayoutItemKind::Node(NodeKind::BindingUseClause),
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
                name: "port_map_aspect",
                kind: LayoutItemKind::Node(NodeKind::PortMapAspect),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        BindingIndicationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl BindingIndicationSyntax {
    pub fn binding_use_clause(&self) -> Option<BindingUseClauseSyntax> {
        self.0
            .children()
            .filter_map(BindingUseClauseSyntax::cast)
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
}
#[derive(Debug, Clone)]
pub struct BindingUseClauseSyntax(pub(crate) SyntaxNode);
impl AstNode for BindingUseClauseSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::BindingUseClause,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "use",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Use)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "entity_aspect",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::EntityEntityAspect,
                    NodeKind::EntityConfigurationAspect,
                    NodeKind::EntityOpenAspect,
                ]),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        BindingUseClauseSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl BindingUseClauseSyntax {
    pub fn use_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Use))
            .nth(0)
    }
    pub fn entity_aspect(&self) -> Option<EntityAspectSyntax> {
        self.0
            .children()
            .filter_map(EntityAspectSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct BlockConfigurationSyntax(pub(crate) SyntaxNode);
impl AstNode for BlockConfigurationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::BlockConfiguration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "block_configuration_preamble",
                kind: LayoutItemKind::Node(NodeKind::BlockConfigurationPreamble),
            },
            LayoutItem {
                optional: false,
                repeated: true,
                name: "use_clauses",
                kind: LayoutItemKind::Node(NodeKind::UseClause),
            },
            LayoutItem {
                optional: false,
                repeated: true,
                name: "configuration_items",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::BlockConfigurationItem,
                    NodeKind::ComponentConfiguration,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "block_configuration_epilogue",
                kind: LayoutItemKind::Node(NodeKind::BlockConfigurationEpilogue),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        BlockConfigurationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl BlockConfigurationSyntax {
    pub fn block_configuration_preamble(&self) -> Option<BlockConfigurationPreambleSyntax> {
        self.0
            .children()
            .filter_map(BlockConfigurationPreambleSyntax::cast)
            .nth(0)
    }
    pub fn use_clauses(&self) -> impl Iterator<Item = UseClauseSyntax> + use<'_> {
        self.0.children().filter_map(UseClauseSyntax::cast)
    }
    pub fn configuration_items(&self) -> impl Iterator<Item = ConfigurationItemSyntax> + use<'_> {
        self.0.children().filter_map(ConfigurationItemSyntax::cast)
    }
    pub fn block_configuration_epilogue(&self) -> Option<BlockConfigurationEpilogueSyntax> {
        self.0
            .children()
            .filter_map(BlockConfigurationEpilogueSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct BlockConfigurationEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for BlockConfigurationEpilogueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::BlockConfigurationEpilogue,
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
                name: "for",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::For)),
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
        BlockConfigurationEpilogueSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl BlockConfigurationEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::End))
            .nth(0)
    }
    pub fn for_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::For))
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
pub struct BlockConfigurationItemSyntax(pub(crate) SyntaxNode);
impl AstNode for BlockConfigurationItemSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::BlockConfigurationItem,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "block_configuration",
            kind: LayoutItemKind::Node(NodeKind::BlockConfiguration),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        BlockConfigurationItemSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl BlockConfigurationItemSyntax {
    pub fn block_configuration(&self) -> Option<BlockConfigurationSyntax> {
        self.0
            .children()
            .filter_map(BlockConfigurationSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct BlockConfigurationPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for BlockConfigurationPreambleSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::BlockConfigurationPreamble,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "for",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::For)),
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
        BlockConfigurationPreambleSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl BlockConfigurationPreambleSyntax {
    pub fn for_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::For))
            .nth(0)
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
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
pub struct BlockHeaderSyntax(pub(crate) SyntaxNode);
impl AstNode for BlockHeaderSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::BlockHeader,
        items: &[
            LayoutItem {
                optional: true,
                repeated: false,
                name: "generic_part",
                kind: LayoutItemKind::Node(NodeKind::GenericPart),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "port_part",
                kind: LayoutItemKind::Node(NodeKind::PortPart),
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
    pub fn generic_part(&self) -> Option<GenericPartSyntax> {
        self.0.children().filter_map(GenericPartSyntax::cast).nth(0)
    }
    pub fn port_part(&self) -> Option<PortPartSyntax> {
        self.0.children().filter_map(PortPartSyntax::cast).nth(0)
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
                name: "block",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Block)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "parenthesized_expression",
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
    pub fn block_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Block))
            .nth(0)
    }
    pub fn parenthesized_expression(&self) -> Option<ParenthesizedExpressionSyntax> {
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
pub struct BlockStatementSyntax(pub(crate) SyntaxNode);
impl AstNode for BlockStatementSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::BlockStatement,
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
    pub fn label(&self) -> Option<LabelSyntax> {
        self.0.children().filter_map(LabelSyntax::cast).nth(0)
    }
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
                optional: false,
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
pub struct CaseGeneratePreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for CaseGeneratePreambleSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::CaseGeneratePreamble,
        items: &[
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
                    NodeKind::Allocator,
                    NodeKind::NameExpression,
                    NodeKind::QualifiedExpression,
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
        CaseGeneratePreambleSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl CaseGeneratePreambleSyntax {
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
pub struct CaseGenerateStatementSyntax(pub(crate) SyntaxNode);
impl AstNode for CaseGenerateStatementSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::CaseGenerateStatement,
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
                name: "case_generate_preamble",
                kind: LayoutItemKind::Node(NodeKind::CaseGeneratePreamble),
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
                name: "generate_epilogue",
                kind: LayoutItemKind::Node(NodeKind::GenerateEpilogue),
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
    pub fn label(&self) -> Option<LabelSyntax> {
        self.0.children().filter_map(LabelSyntax::cast).nth(0)
    }
    pub fn case_generate_preamble(&self) -> Option<CaseGeneratePreambleSyntax> {
        self.0
            .children()
            .filter_map(CaseGeneratePreambleSyntax::cast)
            .nth(0)
    }
    pub fn case_generate_alternatives(
        &self,
    ) -> impl Iterator<Item = CaseGenerateAlternativeSyntax> + use<'_> {
        self.0
            .children()
            .filter_map(CaseGenerateAlternativeSyntax::cast)
    }
    pub fn generate_epilogue(&self) -> Option<GenerateEpilogueSyntax> {
        self.0
            .children()
            .filter_map(GenerateEpilogueSyntax::cast)
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
                name: "when",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::When)),
            },
            LayoutItem {
                optional: false,
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
pub struct CaseStatementEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for CaseStatementEpilogueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::CaseStatementEpilogue,
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
                name: "case",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Case)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "que",
                kind: LayoutItemKind::Token(TokenKind::Que),
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
pub struct CaseStatementPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for CaseStatementPreambleSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::CaseStatementPreamble,
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
                name: "case",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Case)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "que",
                kind: LayoutItemKind::Token(TokenKind::Que),
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
                    NodeKind::Allocator,
                    NodeKind::NameExpression,
                    NodeKind::QualifiedExpression,
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
pub enum ChoiceSyntax {
    ExpressionChoice(ExpressionChoiceSyntax),
    OthersChoice(OthersChoiceSyntax),
}
impl AstNode for ChoiceSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[NodeKind::ExpressionChoice, NodeKind::OthersChoice],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if ExpressionChoiceSyntax::can_cast(&node) {
            return ChoiceSyntax::ExpressionChoice(ExpressionChoiceSyntax::cast_unchecked(node));
        }
        if OthersChoiceSyntax::can_cast(&node) {
            return ChoiceSyntax::OthersChoice(OthersChoiceSyntax::cast_unchecked(node));
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            ChoiceSyntax::ExpressionChoice(inner) => inner.raw(),
            ChoiceSyntax::OthersChoice(inner) => inner.raw(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct ChoicesSyntax(pub(crate) SyntaxNode);
impl AstNode for ChoicesSyntax {
    const META: &'static Layout = &Layout::List(List {
        kind: NodeKind::Choices,
        element: &LayoutItem {
            optional: false,
            repeated: true,
            name: "choices",
            kind: LayoutItemKind::NodeChoice(&[NodeKind::ExpressionChoice, NodeKind::OthersChoice]),
        },
        separator: &LayoutItem {
            optional: false,
            repeated: true,
            name: "bar",
            kind: LayoutItemKind::Token(TokenKind::Bar),
        },
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ChoicesSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ChoicesSyntax {
    pub fn choices(&self) -> impl Iterator<Item = ChoiceSyntax> + use<'_> {
        self.0.children().filter_map(ChoiceSyntax::cast)
    }
    pub fn bar_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Bar)
    }
}
#[derive(Debug, Clone)]
pub struct ComponentConfigurationSyntax(pub(crate) SyntaxNode);
impl AstNode for ComponentConfigurationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ComponentConfiguration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "component_configuration_preamble",
                kind: LayoutItemKind::Node(NodeKind::ComponentConfigurationPreamble),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "binding",
                kind: LayoutItemKind::Node(NodeKind::Binding),
            },
            LayoutItem {
                optional: false,
                repeated: true,
                name: "verification_unit_bindings",
                kind: LayoutItemKind::Node(NodeKind::VerificationUnitBinding),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "block_configuration",
                kind: LayoutItemKind::Node(NodeKind::BlockConfiguration),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "component_configuration_epilogue",
                kind: LayoutItemKind::Node(NodeKind::ComponentConfigurationEpilogue),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ComponentConfigurationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ComponentConfigurationSyntax {
    pub fn component_configuration_preamble(&self) -> Option<ComponentConfigurationPreambleSyntax> {
        self.0
            .children()
            .filter_map(ComponentConfigurationPreambleSyntax::cast)
            .nth(0)
    }
    pub fn binding(&self) -> Option<BindingSyntax> {
        self.0.children().filter_map(BindingSyntax::cast).nth(0)
    }
    pub fn verification_unit_bindings(
        &self,
    ) -> impl Iterator<Item = VerificationUnitBindingSyntax> + use<'_> {
        self.0
            .children()
            .filter_map(VerificationUnitBindingSyntax::cast)
    }
    pub fn block_configuration(&self) -> Option<BlockConfigurationSyntax> {
        self.0
            .children()
            .filter_map(BlockConfigurationSyntax::cast)
            .nth(0)
    }
    pub fn component_configuration_epilogue(&self) -> Option<ComponentConfigurationEpilogueSyntax> {
        self.0
            .children()
            .filter_map(ComponentConfigurationEpilogueSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ComponentConfigurationEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for ComponentConfigurationEpilogueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ComponentConfigurationEpilogue,
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
                name: "for",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::For)),
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
        ComponentConfigurationEpilogueSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ComponentConfigurationEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::End))
            .nth(0)
    }
    pub fn for_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::For))
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
pub struct ComponentConfigurationPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for ComponentConfigurationPreambleSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ComponentConfigurationPreamble,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "for",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::For)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "component_specification",
                kind: LayoutItemKind::Node(NodeKind::ComponentSpecification),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ComponentConfigurationPreambleSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ComponentConfigurationPreambleSyntax {
    pub fn for_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::For))
            .nth(0)
    }
    pub fn component_specification(&self) -> Option<ComponentSpecificationSyntax> {
        self.0
            .children()
            .filter_map(ComponentSpecificationSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ComponentDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for ComponentDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ComponentDeclaration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "component_declaration_preamble",
                kind: LayoutItemKind::Node(NodeKind::ComponentDeclarationPreamble),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "generic_clause",
                kind: LayoutItemKind::Node(NodeKind::GenericClause),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "port_clause",
                kind: LayoutItemKind::Node(NodeKind::PortClause),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "component_declaration_epilogue",
                kind: LayoutItemKind::Node(NodeKind::ComponentDeclarationEpilogue),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ComponentDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ComponentDeclarationSyntax {
    pub fn component_declaration_preamble(&self) -> Option<ComponentDeclarationPreambleSyntax> {
        self.0
            .children()
            .filter_map(ComponentDeclarationPreambleSyntax::cast)
            .nth(0)
    }
    pub fn generic_clause(&self) -> Option<GenericClauseSyntax> {
        self.0
            .children()
            .filter_map(GenericClauseSyntax::cast)
            .nth(0)
    }
    pub fn port_clause(&self) -> Option<PortClauseSyntax> {
        self.0.children().filter_map(PortClauseSyntax::cast).nth(0)
    }
    pub fn component_declaration_epilogue(&self) -> Option<ComponentDeclarationEpilogueSyntax> {
        self.0
            .children()
            .filter_map(ComponentDeclarationEpilogueSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ComponentDeclarationEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for ComponentDeclarationEpilogueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ComponentDeclarationEpilogue,
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
                name: "component",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Component)),
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
        ComponentDeclarationEpilogueSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ComponentDeclarationEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::End))
            .nth(0)
    }
    pub fn component_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Component))
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
pub struct ComponentDeclarationPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for ComponentDeclarationPreambleSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ComponentDeclarationPreamble,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "component",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Component)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "name",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
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
        ComponentDeclarationPreambleSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ComponentDeclarationPreambleSyntax {
    pub fn component_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Component))
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
                    NodeKind::InstantiatedComponent,
                    NodeKind::InstantiatedEntity,
                    NodeKind::InstantiatedConfiguration,
                ]),
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
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ComponentSpecificationSyntax(pub(crate) SyntaxNode);
impl AstNode for ComponentSpecificationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ComponentSpecification,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "instantiation_list",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::InstantiationListList,
                    NodeKind::InstantiationListAll,
                    NodeKind::InstantiationListOthers,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "colon",
                kind: LayoutItemKind::Token(TokenKind::Colon),
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
        ComponentSpecificationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ComponentSpecificationSyntax {
    pub fn instantiation_list(&self) -> Option<InstantiationListSyntax> {
        self.0
            .children()
            .filter_map(InstantiationListSyntax::cast)
            .nth(0)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Colon)
            .nth(0)
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum CompositeTypeDefinitionSyntax {
    ArrayTypeDefinition(ArrayTypeDefinitionSyntax),
    RecordTypeDefinition(RecordTypeDefinitionSyntax),
}
impl AstNode for CompositeTypeDefinitionSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::UnboundedArrayDefinition,
            NodeKind::ConstrainedArrayDefinition,
            NodeKind::RecordTypeDefinition,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if ArrayTypeDefinitionSyntax::can_cast(&node) {
            return CompositeTypeDefinitionSyntax::ArrayTypeDefinition(
                ArrayTypeDefinitionSyntax::cast_unchecked(node),
            );
        }
        if RecordTypeDefinitionSyntax::can_cast(&node) {
            return CompositeTypeDefinitionSyntax::RecordTypeDefinition(
                RecordTypeDefinitionSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            CompositeTypeDefinitionSyntax::ArrayTypeDefinition(inner) => inner.raw(),
            CompositeTypeDefinitionSyntax::RecordTypeDefinition(inner) => inner.raw(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct CompoundConfigurationSpecificationSyntax(pub(crate) SyntaxNode);
impl AstNode for CompoundConfigurationSpecificationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::CompoundConfigurationSpecification,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "component_configuration_preamble",
                kind: LayoutItemKind::Node(NodeKind::ComponentConfigurationPreamble),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "binding_indication",
                kind: LayoutItemKind::Node(NodeKind::BindingIndication),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "semi_colon",
                kind: LayoutItemKind::Token(TokenKind::SemiColon),
            },
            LayoutItem {
                optional: false,
                repeated: true,
                name: "verification_unit_bindings",
                kind: LayoutItemKind::Node(NodeKind::VerificationUnitBinding),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "component_configuration_epilogue",
                kind: LayoutItemKind::Node(NodeKind::ComponentConfigurationEpilogue),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        CompoundConfigurationSpecificationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl CompoundConfigurationSpecificationSyntax {
    pub fn component_configuration_preamble(&self) -> Option<ComponentConfigurationPreambleSyntax> {
        self.0
            .children()
            .filter_map(ComponentConfigurationPreambleSyntax::cast)
            .nth(0)
    }
    pub fn binding_indication(&self) -> Option<BindingIndicationSyntax> {
        self.0
            .children()
            .filter_map(BindingIndicationSyntax::cast)
            .nth(0)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
    pub fn verification_unit_bindings(
        &self,
    ) -> impl Iterator<Item = VerificationUnitBindingSyntax> + use<'_> {
        self.0
            .children()
            .filter_map(VerificationUnitBindingSyntax::cast)
    }
    pub fn component_configuration_epilogue(&self) -> Option<ComponentConfigurationEpilogueSyntax> {
        self.0
            .children()
            .filter_map(ComponentConfigurationEpilogueSyntax::cast)
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
pub struct ConcurrentSelectedSignalAssignmentSyntax(pub(crate) SyntaxNode);
impl AstNode for ConcurrentSelectedSignalAssignmentSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ConcurrentSelectedSignalAssignment,
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
    pub fn label(&self) -> Option<LabelSyntax> {
        self.0.children().filter_map(LabelSyntax::cast).nth(0)
    }
    pub fn postponed_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Postponed))
            .nth(0)
    }
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
pub struct ConditionClauseSyntax(pub(crate) SyntaxNode);
impl AstNode for ConditionClauseSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ConditionClause,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "until",
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
                    NodeKind::Allocator,
                    NodeKind::NameExpression,
                    NodeKind::QualifiedExpression,
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
pub struct ConditionalExpressionsSyntax(pub(crate) SyntaxNode);
impl AstNode for ConditionalExpressionsSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ConditionalExpressions,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "when_expression",
                kind: LayoutItemKind::Node(NodeKind::WhenExpression),
            },
            LayoutItem {
                optional: false,
                repeated: true,
                name: "else_when_expressions",
                kind: LayoutItemKind::Node(NodeKind::ElseWhenExpression),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "else_expression",
                kind: LayoutItemKind::Node(NodeKind::ElseExpression),
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
    pub fn when_expression(&self) -> Option<WhenExpressionSyntax> {
        self.0
            .children()
            .filter_map(WhenExpressionSyntax::cast)
            .nth(0)
    }
    pub fn else_when_expressions(
        &self,
    ) -> impl Iterator<Item = ElseWhenExpressionSyntax> + use<'_> {
        self.0.children().filter_map(ElseWhenExpressionSyntax::cast)
    }
    pub fn else_expression(&self) -> Option<ElseExpressionSyntax> {
        self.0
            .children()
            .filter_map(ElseExpressionSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ConditionalForceAssignmentSyntax(pub(crate) SyntaxNode);
impl AstNode for ConditionalForceAssignmentSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ConditionalForceAssignment,
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
                name: "lte",
                kind: LayoutItemKind::Token(TokenKind::LTE),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "force",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Force)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "force_mode",
                kind: LayoutItemKind::TokenChoice(&[
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
                name: "semi_colon",
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
                name: "colon_eq",
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
                name: "semi_colon",
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
                name: "lte",
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
                name: "semi_colon",
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
pub struct ConditionalWaveformsSyntax(pub(crate) SyntaxNode);
impl AstNode for ConditionalWaveformsSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ConditionalWaveforms,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "when_waveform",
                kind: LayoutItemKind::Node(NodeKind::WhenWaveform),
            },
            LayoutItem {
                optional: false,
                repeated: true,
                name: "else_when_waveforms",
                kind: LayoutItemKind::Node(NodeKind::ElseWhenWaveform),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "else_waveform",
                kind: LayoutItemKind::Node(NodeKind::ElseWaveform),
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
    pub fn when_waveform(&self) -> Option<WhenWaveformSyntax> {
        self.0
            .children()
            .filter_map(WhenWaveformSyntax::cast)
            .nth(0)
    }
    pub fn else_when_waveforms(&self) -> impl Iterator<Item = ElseWhenWaveformSyntax> + use<'_> {
        self.0.children().filter_map(ElseWhenWaveformSyntax::cast)
    }
    pub fn else_waveform(&self) -> Option<ElseWaveformSyntax> {
        self.0
            .children()
            .filter_map(ElseWaveformSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ConfigurationDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for ConfigurationDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ConfigurationDeclaration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "configuration_declaration_preamble",
                kind: LayoutItemKind::Node(NodeKind::ConfigurationDeclarationPreamble),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "declarations",
                kind: LayoutItemKind::Node(NodeKind::Declarations),
            },
            LayoutItem {
                optional: false,
                repeated: true,
                name: "verification_unit_bindings",
                kind: LayoutItemKind::Node(NodeKind::VerificationUnitBinding),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "block_configuration",
                kind: LayoutItemKind::Node(NodeKind::BlockConfiguration),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "configuration_declaration_epilogue",
                kind: LayoutItemKind::Node(NodeKind::ConfigurationDeclarationEpilogue),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ConfigurationDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConfigurationDeclarationSyntax {
    pub fn configuration_declaration_preamble(
        &self,
    ) -> Option<ConfigurationDeclarationPreambleSyntax> {
        self.0
            .children()
            .filter_map(ConfigurationDeclarationPreambleSyntax::cast)
            .nth(0)
    }
    pub fn declarations(&self) -> Option<DeclarationsSyntax> {
        self.0
            .children()
            .filter_map(DeclarationsSyntax::cast)
            .nth(0)
    }
    pub fn verification_unit_bindings(
        &self,
    ) -> impl Iterator<Item = VerificationUnitBindingSyntax> + use<'_> {
        self.0
            .children()
            .filter_map(VerificationUnitBindingSyntax::cast)
    }
    pub fn block_configuration(&self) -> Option<BlockConfigurationSyntax> {
        self.0
            .children()
            .filter_map(BlockConfigurationSyntax::cast)
            .nth(0)
    }
    pub fn configuration_declaration_epilogue(
        &self,
    ) -> Option<ConfigurationDeclarationEpilogueSyntax> {
        self.0
            .children()
            .filter_map(ConfigurationDeclarationEpilogueSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ConfigurationDeclarationEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for ConfigurationDeclarationEpilogueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ConfigurationDeclarationEpilogue,
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
                name: "configuration",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Configuration)),
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
        ConfigurationDeclarationEpilogueSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConfigurationDeclarationEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::End))
            .nth(0)
    }
    pub fn configuration_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Configuration))
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
pub struct ConfigurationDeclarationPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for ConfigurationDeclarationPreambleSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ConfigurationDeclarationPreamble,
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
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "of",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Of)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "entity_name",
                kind: LayoutItemKind::Node(NodeKind::Name),
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
        ConfigurationDeclarationPreambleSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConfigurationDeclarationPreambleSyntax {
    pub fn configuration_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Configuration))
            .nth(0)
    }
    pub fn name_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Identifier)
            .nth(0)
    }
    pub fn of_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Of))
            .nth(0)
    }
    pub fn entity_name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
    pub fn is_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Is))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum ConfigurationItemSyntax {
    BlockConfigurationItem(BlockConfigurationItemSyntax),
    ComponentConfiguration(ComponentConfigurationSyntax),
}
impl AstNode for ConfigurationItemSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::BlockConfigurationItem,
            NodeKind::ComponentConfiguration,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if BlockConfigurationItemSyntax::can_cast(&node) {
            return ConfigurationItemSyntax::BlockConfigurationItem(
                BlockConfigurationItemSyntax::cast_unchecked(node),
            );
        }
        if ComponentConfigurationSyntax::can_cast(&node) {
            return ConfigurationItemSyntax::ComponentConfiguration(
                ComponentConfigurationSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            ConfigurationItemSyntax::BlockConfigurationItem(inner) => inner.raw(),
            ConfigurationItemSyntax::ComponentConfiguration(inner) => inner.raw(),
        }
    }
}
#[derive(Debug, Clone)]
pub enum ConfigurationSpecificationSyntax {
    SimpleConfigurationSpecification(SimpleConfigurationSpecificationSyntax),
    CompoundConfigurationSpecification(CompoundConfigurationSpecificationSyntax),
}
impl AstNode for ConfigurationSpecificationSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::SimpleConfigurationSpecification,
            NodeKind::CompoundConfigurationSpecification,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if SimpleConfigurationSpecificationSyntax::can_cast(&node) {
            return ConfigurationSpecificationSyntax::SimpleConfigurationSpecification(
                SimpleConfigurationSpecificationSyntax::cast_unchecked(node),
            );
        }
        if CompoundConfigurationSpecificationSyntax::can_cast(&node) {
            return ConfigurationSpecificationSyntax::CompoundConfigurationSpecification(
                CompoundConfigurationSpecificationSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            ConfigurationSpecificationSyntax::SimpleConfigurationSpecification(inner) => {
                inner.raw()
            }
            ConfigurationSpecificationSyntax::CompoundConfigurationSpecification(inner) => {
                inner.raw()
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct ConstantDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for ConstantDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ConstantDeclaration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "constant",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Constant)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "identifier_list",
                kind: LayoutItemKind::Node(NodeKind::IdentifierList),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "colon",
                kind: LayoutItemKind::Token(TokenKind::Colon),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "subtype_indication",
                kind: LayoutItemKind::Node(NodeKind::SubtypeIndication),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "initial_value",
                kind: LayoutItemKind::Node(NodeKind::InitialValue),
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
        ConstantDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConstantDeclarationSyntax {
    pub fn constant_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Constant))
            .nth(0)
    }
    pub fn identifier_list(&self) -> Option<IdentifierListSyntax> {
        self.0
            .children()
            .filter_map(IdentifierListSyntax::cast)
            .nth(0)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Colon)
            .nth(0)
    }
    pub fn subtype_indication(&self) -> Option<SubtypeIndicationSyntax> {
        self.0
            .children()
            .filter_map(SubtypeIndicationSyntax::cast)
            .nth(0)
    }
    pub fn initial_value(&self) -> Option<InitialValueSyntax> {
        self.0
            .children()
            .filter_map(InitialValueSyntax::cast)
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
pub struct ConstrainedArrayDefinitionSyntax(pub(crate) SyntaxNode);
impl AstNode for ConstrainedArrayDefinitionSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ConstrainedArrayDefinition,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "array",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Array)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "index_constraint",
                kind: LayoutItemKind::Node(NodeKind::IndexConstraint),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "of",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Of)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "subtype_indication",
                kind: LayoutItemKind::Node(NodeKind::SubtypeIndication),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ConstrainedArrayDefinitionSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ConstrainedArrayDefinitionSyntax {
    pub fn array_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Array))
            .nth(0)
    }
    pub fn index_constraint(&self) -> Option<IndexConstraintSyntax> {
        self.0
            .children()
            .filter_map(IndexConstraintSyntax::cast)
            .nth(0)
    }
    pub fn of_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Of))
            .nth(0)
    }
    pub fn subtype_indication(&self) -> Option<SubtypeIndicationSyntax> {
        self.0
            .children()
            .filter_map(SubtypeIndicationSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ContextClauseSyntax(pub(crate) SyntaxNode);
impl AstNode for ContextClauseSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ContextClause,
        items: &[LayoutItem {
            optional: false,
            repeated: true,
            name: "context_items",
            kind: LayoutItemKind::NodeChoice(&[
                NodeKind::LibraryClause,
                NodeKind::UseClauseContextItem,
                NodeKind::ContextReference,
            ]),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ContextClauseSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ContextClauseSyntax {
    pub fn context_items(&self) -> impl Iterator<Item = ContextItemSyntax> + use<'_> {
        self.0.children().filter_map(ContextItemSyntax::cast)
    }
}
#[derive(Debug, Clone)]
pub struct ContextDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for ContextDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ContextDeclaration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "context_declaration_preamble",
                kind: LayoutItemKind::Node(NodeKind::ContextDeclarationPreamble),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "context_clause",
                kind: LayoutItemKind::Node(NodeKind::ContextClause),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "context_declaration_epilogue",
                kind: LayoutItemKind::Node(NodeKind::ContextDeclarationEpilogue),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ContextDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ContextDeclarationSyntax {
    pub fn context_declaration_preamble(&self) -> Option<ContextDeclarationPreambleSyntax> {
        self.0
            .children()
            .filter_map(ContextDeclarationPreambleSyntax::cast)
            .nth(0)
    }
    pub fn context_clause(&self) -> Option<ContextClauseSyntax> {
        self.0
            .children()
            .filter_map(ContextClauseSyntax::cast)
            .nth(0)
    }
    pub fn context_declaration_epilogue(&self) -> Option<ContextDeclarationEpilogueSyntax> {
        self.0
            .children()
            .filter_map(ContextDeclarationEpilogueSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ContextDeclarationEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for ContextDeclarationEpilogueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ContextDeclarationEpilogue,
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
                name: "context",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Context)),
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
        ContextDeclarationEpilogueSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ContextDeclarationEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::End))
            .nth(0)
    }
    pub fn context_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Context))
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
pub struct ContextDeclarationPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for ContextDeclarationPreambleSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ContextDeclarationPreamble,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "context",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Context)),
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
        ContextDeclarationPreambleSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ContextDeclarationPreambleSyntax {
    pub fn context_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Context))
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
pub enum ContextItemSyntax {
    LibraryClause(LibraryClauseSyntax),
    UseClauseContextItem(UseClauseContextItemSyntax),
    ContextReference(ContextReferenceSyntax),
}
impl AstNode for ContextItemSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::LibraryClause,
            NodeKind::UseClauseContextItem,
            NodeKind::ContextReference,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if LibraryClauseSyntax::can_cast(&node) {
            return ContextItemSyntax::LibraryClause(LibraryClauseSyntax::cast_unchecked(node));
        }
        if UseClauseContextItemSyntax::can_cast(&node) {
            return ContextItemSyntax::UseClauseContextItem(
                UseClauseContextItemSyntax::cast_unchecked(node),
            );
        }
        if ContextReferenceSyntax::can_cast(&node) {
            return ContextItemSyntax::ContextReference(ContextReferenceSyntax::cast_unchecked(
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
            ContextItemSyntax::LibraryClause(inner) => inner.raw(),
            ContextItemSyntax::UseClauseContextItem(inner) => inner.raw(),
            ContextItemSyntax::ContextReference(inner) => inner.raw(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct ContextReferenceSyntax(pub(crate) SyntaxNode);
impl AstNode for ContextReferenceSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ContextReference,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "context",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Context)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "name_list",
                kind: LayoutItemKind::Node(NodeKind::NameList),
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
        ContextReferenceSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ContextReferenceSyntax {
    pub fn context_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Context))
            .nth(0)
    }
    pub fn name_list(&self) -> Option<NameListSyntax> {
        self.0.children().filter_map(NameListSyntax::cast).nth(0)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum DeclarationSyntax {
    SubprogramDeclaration(SubprogramDeclarationSyntax),
    SubprogramBody(SubprogramBodySyntax),
    SubprogramInstantiationDeclaration(SubprogramInstantiationDeclarationSyntax),
    PackageDeclaration(PackageDeclarationSyntax),
    PackageBodyDeclaration(PackageBodyDeclarationSyntax),
    PackageInstantiationDeclaration(PackageInstantiationDeclarationSyntax),
    TypeDeclaration(TypeDeclarationSyntax),
    SubtypeDeclaration(SubtypeDeclarationSyntax),
    FileDeclaration(FileDeclarationSyntax),
    AliasDeclaration(AliasDeclarationSyntax),
    ComponentDeclaration(ComponentDeclarationSyntax),
    AttributeDeclaration(AttributeDeclarationSyntax),
    AttributeSpecification(AttributeSpecificationSyntax),
    ConfigurationSpecification(ConfigurationSpecificationSyntax),
    DisconnectionSpecification(DisconnectionSpecificationSyntax),
    UseClauseDeclaration(UseClauseDeclarationSyntax),
    GroupTemplateDeclaration(GroupTemplateDeclarationSyntax),
    GroupDeclaration(GroupDeclarationSyntax),
    ConstantDeclaration(ConstantDeclarationSyntax),
    SignalDeclaration(SignalDeclarationSyntax),
    VariableDeclaration(VariableDeclarationSyntax),
}
impl AstNode for DeclarationSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::SubprogramDeclaration,
            NodeKind::SubprogramBody,
            NodeKind::SubprogramInstantiationDeclaration,
            NodeKind::PackageDeclaration,
            NodeKind::PackageBodyDeclaration,
            NodeKind::PackageInstantiationDeclaration,
            NodeKind::FullTypeDeclaration,
            NodeKind::IncompleteTypeDeclaration,
            NodeKind::SubtypeDeclaration,
            NodeKind::FileDeclaration,
            NodeKind::AliasDeclaration,
            NodeKind::ComponentDeclaration,
            NodeKind::AttributeDeclaration,
            NodeKind::AttributeSpecification,
            NodeKind::SimpleConfigurationSpecification,
            NodeKind::CompoundConfigurationSpecification,
            NodeKind::DisconnectionSpecification,
            NodeKind::UseClauseDeclaration,
            NodeKind::GroupTemplateDeclaration,
            NodeKind::GroupDeclaration,
            NodeKind::ConstantDeclaration,
            NodeKind::SignalDeclaration,
            NodeKind::VariableDeclaration,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if SubprogramDeclarationSyntax::can_cast(&node) {
            return DeclarationSyntax::SubprogramDeclaration(
                SubprogramDeclarationSyntax::cast_unchecked(node),
            );
        }
        if SubprogramBodySyntax::can_cast(&node) {
            return DeclarationSyntax::SubprogramBody(SubprogramBodySyntax::cast_unchecked(node));
        }
        if SubprogramInstantiationDeclarationSyntax::can_cast(&node) {
            return DeclarationSyntax::SubprogramInstantiationDeclaration(
                SubprogramInstantiationDeclarationSyntax::cast_unchecked(node),
            );
        }
        if PackageDeclarationSyntax::can_cast(&node) {
            return DeclarationSyntax::PackageDeclaration(
                PackageDeclarationSyntax::cast_unchecked(node),
            );
        }
        if PackageBodyDeclarationSyntax::can_cast(&node) {
            return DeclarationSyntax::PackageBodyDeclaration(
                PackageBodyDeclarationSyntax::cast_unchecked(node),
            );
        }
        if PackageInstantiationDeclarationSyntax::can_cast(&node) {
            return DeclarationSyntax::PackageInstantiationDeclaration(
                PackageInstantiationDeclarationSyntax::cast_unchecked(node),
            );
        }
        if TypeDeclarationSyntax::can_cast(&node) {
            return DeclarationSyntax::TypeDeclaration(TypeDeclarationSyntax::cast_unchecked(node));
        }
        if SubtypeDeclarationSyntax::can_cast(&node) {
            return DeclarationSyntax::SubtypeDeclaration(
                SubtypeDeclarationSyntax::cast_unchecked(node),
            );
        }
        if FileDeclarationSyntax::can_cast(&node) {
            return DeclarationSyntax::FileDeclaration(FileDeclarationSyntax::cast_unchecked(node));
        }
        if AliasDeclarationSyntax::can_cast(&node) {
            return DeclarationSyntax::AliasDeclaration(AliasDeclarationSyntax::cast_unchecked(
                node,
            ));
        }
        if ComponentDeclarationSyntax::can_cast(&node) {
            return DeclarationSyntax::ComponentDeclaration(
                ComponentDeclarationSyntax::cast_unchecked(node),
            );
        }
        if AttributeDeclarationSyntax::can_cast(&node) {
            return DeclarationSyntax::AttributeDeclaration(
                AttributeDeclarationSyntax::cast_unchecked(node),
            );
        }
        if AttributeSpecificationSyntax::can_cast(&node) {
            return DeclarationSyntax::AttributeSpecification(
                AttributeSpecificationSyntax::cast_unchecked(node),
            );
        }
        if ConfigurationSpecificationSyntax::can_cast(&node) {
            return DeclarationSyntax::ConfigurationSpecification(
                ConfigurationSpecificationSyntax::cast_unchecked(node),
            );
        }
        if DisconnectionSpecificationSyntax::can_cast(&node) {
            return DeclarationSyntax::DisconnectionSpecification(
                DisconnectionSpecificationSyntax::cast_unchecked(node),
            );
        }
        if UseClauseDeclarationSyntax::can_cast(&node) {
            return DeclarationSyntax::UseClauseDeclaration(
                UseClauseDeclarationSyntax::cast_unchecked(node),
            );
        }
        if GroupTemplateDeclarationSyntax::can_cast(&node) {
            return DeclarationSyntax::GroupTemplateDeclaration(
                GroupTemplateDeclarationSyntax::cast_unchecked(node),
            );
        }
        if GroupDeclarationSyntax::can_cast(&node) {
            return DeclarationSyntax::GroupDeclaration(GroupDeclarationSyntax::cast_unchecked(
                node,
            ));
        }
        if ConstantDeclarationSyntax::can_cast(&node) {
            return DeclarationSyntax::ConstantDeclaration(
                ConstantDeclarationSyntax::cast_unchecked(node),
            );
        }
        if SignalDeclarationSyntax::can_cast(&node) {
            return DeclarationSyntax::SignalDeclaration(SignalDeclarationSyntax::cast_unchecked(
                node,
            ));
        }
        if VariableDeclarationSyntax::can_cast(&node) {
            return DeclarationSyntax::VariableDeclaration(
                VariableDeclarationSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            DeclarationSyntax::SubprogramDeclaration(inner) => inner.raw(),
            DeclarationSyntax::SubprogramBody(inner) => inner.raw(),
            DeclarationSyntax::SubprogramInstantiationDeclaration(inner) => inner.raw(),
            DeclarationSyntax::PackageDeclaration(inner) => inner.raw(),
            DeclarationSyntax::PackageBodyDeclaration(inner) => inner.raw(),
            DeclarationSyntax::PackageInstantiationDeclaration(inner) => inner.raw(),
            DeclarationSyntax::TypeDeclaration(inner) => inner.raw(),
            DeclarationSyntax::SubtypeDeclaration(inner) => inner.raw(),
            DeclarationSyntax::FileDeclaration(inner) => inner.raw(),
            DeclarationSyntax::AliasDeclaration(inner) => inner.raw(),
            DeclarationSyntax::ComponentDeclaration(inner) => inner.raw(),
            DeclarationSyntax::AttributeDeclaration(inner) => inner.raw(),
            DeclarationSyntax::AttributeSpecification(inner) => inner.raw(),
            DeclarationSyntax::ConfigurationSpecification(inner) => inner.raw(),
            DeclarationSyntax::DisconnectionSpecification(inner) => inner.raw(),
            DeclarationSyntax::UseClauseDeclaration(inner) => inner.raw(),
            DeclarationSyntax::GroupTemplateDeclaration(inner) => inner.raw(),
            DeclarationSyntax::GroupDeclaration(inner) => inner.raw(),
            DeclarationSyntax::ConstantDeclaration(inner) => inner.raw(),
            DeclarationSyntax::SignalDeclaration(inner) => inner.raw(),
            DeclarationSyntax::VariableDeclaration(inner) => inner.raw(),
        }
    }
}
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
pub struct DeclarationsSyntax(pub(crate) SyntaxNode);
impl AstNode for DeclarationsSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::Declarations,
        items: &[LayoutItem {
            optional: false,
            repeated: true,
            name: "declarations",
            kind: LayoutItemKind::NodeChoice(&[
                NodeKind::SubprogramDeclaration,
                NodeKind::SubprogramBody,
                NodeKind::SubprogramInstantiationDeclaration,
                NodeKind::PackageDeclaration,
                NodeKind::PackageBodyDeclaration,
                NodeKind::PackageInstantiationDeclaration,
                NodeKind::FullTypeDeclaration,
                NodeKind::IncompleteTypeDeclaration,
                NodeKind::SubtypeDeclaration,
                NodeKind::FileDeclaration,
                NodeKind::AliasDeclaration,
                NodeKind::ComponentDeclaration,
                NodeKind::AttributeDeclaration,
                NodeKind::AttributeSpecification,
                NodeKind::SimpleConfigurationSpecification,
                NodeKind::CompoundConfigurationSpecification,
                NodeKind::DisconnectionSpecification,
                NodeKind::UseClauseDeclaration,
                NodeKind::GroupTemplateDeclaration,
                NodeKind::GroupDeclaration,
                NodeKind::ConstantDeclaration,
                NodeKind::SignalDeclaration,
                NodeKind::VariableDeclaration,
            ]),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        DeclarationsSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl DeclarationsSyntax {
    pub fn declarations(&self) -> impl Iterator<Item = DeclarationSyntax> + use<'_> {
        self.0.children().filter_map(DeclarationSyntax::cast)
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
pub struct DesignFileSyntax(pub(crate) SyntaxNode);
impl AstNode for DesignFileSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::DesignFile,
        items: &[
            LayoutItem {
                optional: false,
                repeated: true,
                name: "design_units",
                kind: LayoutItemKind::Node(NodeKind::DesignUnit),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "eof",
                kind: LayoutItemKind::Token(TokenKind::Eof),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        DesignFileSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl DesignFileSyntax {
    pub fn design_units(&self) -> impl Iterator<Item = DesignUnitSyntax> + use<'_> {
        self.0.children().filter_map(DesignUnitSyntax::cast)
    }
    pub fn eof_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Eof)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct DesignUnitSyntax(pub(crate) SyntaxNode);
impl AstNode for DesignUnitSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::DesignUnit,
        items: &[
            LayoutItem {
                optional: true,
                repeated: false,
                name: "context_clause",
                kind: LayoutItemKind::Node(NodeKind::ContextClause),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "library_unit",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::EntityDeclaration,
                    NodeKind::ConfigurationDeclaration,
                    NodeKind::PrimaryUnitPackageDeclaration,
                    NodeKind::PackageInstantiationDeclarationPrimaryUnit,
                    NodeKind::ContextDeclaration,
                    NodeKind::ArchitectureBody,
                    NodeKind::SecondaryUnitPackageBody,
                ]),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        DesignUnitSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl DesignUnitSyntax {
    pub fn context_clause(&self) -> Option<ContextClauseSyntax> {
        self.0
            .children()
            .filter_map(ContextClauseSyntax::cast)
            .nth(0)
    }
    pub fn library_unit(&self) -> Option<LibraryUnitSyntax> {
        self.0.children().filter_map(LibraryUnitSyntax::cast).nth(0)
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
            TokenKind::Identifier => Some(DesignatorSyntax::Identifier(token)),
            TokenKind::StringLiteral => Some(DesignatorSyntax::StringLiteral(token)),
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
pub struct DisconnectionSpecificationSyntax(pub(crate) SyntaxNode);
impl AstNode for DisconnectionSpecificationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::DisconnectionSpecification,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "disconnect",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Disconnect)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "guarded_signal_specification",
                kind: LayoutItemKind::Node(NodeKind::GuardedSignalSpecification),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "after",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::After)),
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
                    NodeKind::Allocator,
                    NodeKind::NameExpression,
                    NodeKind::QualifiedExpression,
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
        DisconnectionSpecificationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl DisconnectionSpecificationSyntax {
    pub fn disconnect_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Disconnect))
            .nth(0)
    }
    pub fn guarded_signal_specification(&self) -> Option<GuardedSignalSpecificationSyntax> {
        self.0
            .children()
            .filter_map(GuardedSignalSpecificationSyntax::cast)
            .nth(0)
    }
    pub fn after_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::After))
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
pub struct ElementAssociationSyntax(pub(crate) SyntaxNode);
impl AstNode for ElementAssociationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ElementAssociation,
        items: &[
            LayoutItem {
                optional: true,
                repeated: false,
                name: "element_choices",
                kind: LayoutItemKind::Node(NodeKind::ElementChoices),
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
                    NodeKind::Allocator,
                    NodeKind::NameExpression,
                    NodeKind::QualifiedExpression,
                ]),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ElementAssociationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ElementAssociationSyntax {
    pub fn element_choices(&self) -> Option<ElementChoicesSyntax> {
        self.0
            .children()
            .filter_map(ElementChoicesSyntax::cast)
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ElementAssociationListSyntax(pub(crate) SyntaxNode);
impl AstNode for ElementAssociationListSyntax {
    const META: &'static Layout = &Layout::List(List {
        kind: NodeKind::ElementAssociationList,
        element: &LayoutItem {
            optional: false,
            repeated: true,
            name: "element_associations",
            kind: LayoutItemKind::Node(NodeKind::ElementAssociation),
        },
        separator: &LayoutItem {
            optional: false,
            repeated: true,
            name: "comma",
            kind: LayoutItemKind::Token(TokenKind::Comma),
        },
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ElementAssociationListSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ElementAssociationListSyntax {
    pub fn element_associations(&self) -> impl Iterator<Item = ElementAssociationSyntax> + use<'_> {
        self.0.children().filter_map(ElementAssociationSyntax::cast)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Comma)
    }
}
#[derive(Debug, Clone)]
pub struct ElementChoicesSyntax(pub(crate) SyntaxNode);
impl AstNode for ElementChoicesSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ElementChoices,
        items: &[
            LayoutItem {
                optional: false,
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
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ElementChoicesSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ElementChoicesSyntax {
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
pub struct ElementDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for ElementDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ElementDeclaration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "identifier_list",
                kind: LayoutItemKind::Node(NodeKind::IdentifierList),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "colon",
                kind: LayoutItemKind::Token(TokenKind::Colon),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "subtype_indication",
                kind: LayoutItemKind::Node(NodeKind::SubtypeIndication),
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
        ElementDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ElementDeclarationSyntax {
    pub fn identifier_list(&self) -> Option<IdentifierListSyntax> {
        self.0
            .children()
            .filter_map(IdentifierListSyntax::cast)
            .nth(0)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Colon)
            .nth(0)
    }
    pub fn subtype_indication(&self) -> Option<SubtypeIndicationSyntax> {
        self.0
            .children()
            .filter_map(SubtypeIndicationSyntax::cast)
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
pub enum ElementResolutionSyntax {
    ResolutionIndicationElementResolution(ResolutionIndicationElementResolutionSyntax),
    RecordResolutionElementResolution(RecordResolutionElementResolutionSyntax),
}
impl AstNode for ElementResolutionSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::ResolutionIndicationElementResolution,
            NodeKind::RecordResolutionElementResolution,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if ResolutionIndicationElementResolutionSyntax::can_cast(&node) {
            return ElementResolutionSyntax::ResolutionIndicationElementResolution(
                ResolutionIndicationElementResolutionSyntax::cast_unchecked(node),
            );
        }
        if RecordResolutionElementResolutionSyntax::can_cast(&node) {
            return ElementResolutionSyntax::RecordResolutionElementResolution(
                RecordResolutionElementResolutionSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            ElementResolutionSyntax::ResolutionIndicationElementResolution(inner) => inner.raw(),
            ElementResolutionSyntax::RecordResolutionElementResolution(inner) => inner.raw(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct ElementResolutionResolutionIndicationSyntax(pub(crate) SyntaxNode);
impl AstNode for ElementResolutionResolutionIndicationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ElementResolutionResolutionIndication,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "element_resolution",
            kind: LayoutItemKind::NodeChoice(&[
                NodeKind::ResolutionIndicationElementResolution,
                NodeKind::RecordResolutionElementResolution,
            ]),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ElementResolutionResolutionIndicationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ElementResolutionResolutionIndicationSyntax {
    pub fn element_resolution(&self) -> Option<ElementResolutionSyntax> {
        self.0
            .children()
            .filter_map(ElementResolutionSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ElseExpressionSyntax(pub(crate) SyntaxNode);
impl AstNode for ElseExpressionSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ElseExpression,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "else",
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
                    NodeKind::Allocator,
                    NodeKind::NameExpression,
                    NodeKind::QualifiedExpression,
                ]),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ElseExpressionSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ElseExpressionSyntax {
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
pub struct ElseWaveformSyntax(pub(crate) SyntaxNode);
impl AstNode for ElseWaveformSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ElseWaveform,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "else",
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
        ElseWaveformSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ElseWaveformSyntax {
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
pub struct ElseWhenExpressionSyntax(pub(crate) SyntaxNode);
impl AstNode for ElseWhenExpressionSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ElseWhenExpression,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "else",
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
                    NodeKind::Allocator,
                    NodeKind::NameExpression,
                    NodeKind::QualifiedExpression,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "when",
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
                    NodeKind::Allocator,
                    NodeKind::NameExpression,
                    NodeKind::QualifiedExpression,
                ]),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ElseWhenExpressionSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ElseWhenExpressionSyntax {
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
pub struct ElseWhenWaveformSyntax(pub(crate) SyntaxNode);
impl AstNode for ElseWhenWaveformSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ElseWhenWaveform,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "else",
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
                name: "when",
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
                    NodeKind::Allocator,
                    NodeKind::NameExpression,
                    NodeKind::QualifiedExpression,
                ]),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ElseWhenWaveformSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ElseWhenWaveformSyntax {
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
pub enum EntityAspectSyntax {
    EntityEntityAspect(EntityEntityAspectSyntax),
    EntityConfigurationAspect(EntityConfigurationAspectSyntax),
    EntityOpenAspect(EntityOpenAspectSyntax),
}
impl AstNode for EntityAspectSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::EntityEntityAspect,
            NodeKind::EntityConfigurationAspect,
            NodeKind::EntityOpenAspect,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if EntityEntityAspectSyntax::can_cast(&node) {
            return EntityAspectSyntax::EntityEntityAspect(
                EntityEntityAspectSyntax::cast_unchecked(node),
            );
        }
        if EntityConfigurationAspectSyntax::can_cast(&node) {
            return EntityAspectSyntax::EntityConfigurationAspect(
                EntityConfigurationAspectSyntax::cast_unchecked(node),
            );
        }
        if EntityOpenAspectSyntax::can_cast(&node) {
            return EntityAspectSyntax::EntityOpenAspect(EntityOpenAspectSyntax::cast_unchecked(
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
            EntityAspectSyntax::EntityEntityAspect(inner) => inner.raw(),
            EntityAspectSyntax::EntityConfigurationAspect(inner) => inner.raw(),
            EntityAspectSyntax::EntityOpenAspect(inner) => inner.raw(),
        }
    }
}
#[derive(Debug, Clone)]
pub enum EntityClassSyntax {
    Entity(SyntaxToken),
    Architecture(SyntaxToken),
    Configuration(SyntaxToken),
    Procedure(SyntaxToken),
    Function(SyntaxToken),
    Package(SyntaxToken),
    Type(SyntaxToken),
    Subtype(SyntaxToken),
    Constant(SyntaxToken),
    Signal(SyntaxToken),
    Variable(SyntaxToken),
    Component(SyntaxToken),
    Label(SyntaxToken),
    Literal(SyntaxToken),
    Units(SyntaxToken),
    Group(SyntaxToken),
    File(SyntaxToken),
    Property(SyntaxToken),
    Sequence(SyntaxToken),
}
impl EntityClassSyntax {
    pub fn cast(token: SyntaxToken) -> Option<Self> {
        match token.kind() {
            TokenKind::Keyword(Kw::Entity) => Some(EntityClassSyntax::Entity(token)),
            TokenKind::Keyword(Kw::Architecture) => Some(EntityClassSyntax::Architecture(token)),
            TokenKind::Keyword(Kw::Configuration) => Some(EntityClassSyntax::Configuration(token)),
            TokenKind::Keyword(Kw::Procedure) => Some(EntityClassSyntax::Procedure(token)),
            TokenKind::Keyword(Kw::Function) => Some(EntityClassSyntax::Function(token)),
            TokenKind::Keyword(Kw::Package) => Some(EntityClassSyntax::Package(token)),
            TokenKind::Keyword(Kw::Type) => Some(EntityClassSyntax::Type(token)),
            TokenKind::Keyword(Kw::Subtype) => Some(EntityClassSyntax::Subtype(token)),
            TokenKind::Keyword(Kw::Constant) => Some(EntityClassSyntax::Constant(token)),
            TokenKind::Keyword(Kw::Signal) => Some(EntityClassSyntax::Signal(token)),
            TokenKind::Keyword(Kw::Variable) => Some(EntityClassSyntax::Variable(token)),
            TokenKind::Keyword(Kw::Component) => Some(EntityClassSyntax::Component(token)),
            TokenKind::Keyword(Kw::Label) => Some(EntityClassSyntax::Label(token)),
            TokenKind::Keyword(Kw::Literal) => Some(EntityClassSyntax::Literal(token)),
            TokenKind::Keyword(Kw::Units) => Some(EntityClassSyntax::Units(token)),
            TokenKind::Keyword(Kw::Group) => Some(EntityClassSyntax::Group(token)),
            TokenKind::Keyword(Kw::File) => Some(EntityClassSyntax::File(token)),
            TokenKind::Keyword(Kw::Property) => Some(EntityClassSyntax::Property(token)),
            TokenKind::Keyword(Kw::Sequence) => Some(EntityClassSyntax::Sequence(token)),
            _ => None,
        }
    }
    pub fn raw(&self) -> SyntaxToken {
        match self {
            EntityClassSyntax::Entity(token) => token.clone(),
            EntityClassSyntax::Architecture(token) => token.clone(),
            EntityClassSyntax::Configuration(token) => token.clone(),
            EntityClassSyntax::Procedure(token) => token.clone(),
            EntityClassSyntax::Function(token) => token.clone(),
            EntityClassSyntax::Package(token) => token.clone(),
            EntityClassSyntax::Type(token) => token.clone(),
            EntityClassSyntax::Subtype(token) => token.clone(),
            EntityClassSyntax::Constant(token) => token.clone(),
            EntityClassSyntax::Signal(token) => token.clone(),
            EntityClassSyntax::Variable(token) => token.clone(),
            EntityClassSyntax::Component(token) => token.clone(),
            EntityClassSyntax::Label(token) => token.clone(),
            EntityClassSyntax::Literal(token) => token.clone(),
            EntityClassSyntax::Units(token) => token.clone(),
            EntityClassSyntax::Group(token) => token.clone(),
            EntityClassSyntax::File(token) => token.clone(),
            EntityClassSyntax::Property(token) => token.clone(),
            EntityClassSyntax::Sequence(token) => token.clone(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct EntityClassEntrySyntax(pub(crate) SyntaxNode);
impl AstNode for EntityClassEntrySyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::EntityClassEntry,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "entity_class",
                kind: LayoutItemKind::TokenChoice(&[
                    TokenKind::Keyword(Kw::Entity),
                    TokenKind::Keyword(Kw::Architecture),
                    TokenKind::Keyword(Kw::Configuration),
                    TokenKind::Keyword(Kw::Procedure),
                    TokenKind::Keyword(Kw::Function),
                    TokenKind::Keyword(Kw::Package),
                    TokenKind::Keyword(Kw::Type),
                    TokenKind::Keyword(Kw::Subtype),
                    TokenKind::Keyword(Kw::Constant),
                    TokenKind::Keyword(Kw::Signal),
                    TokenKind::Keyword(Kw::Variable),
                    TokenKind::Keyword(Kw::Component),
                    TokenKind::Keyword(Kw::Label),
                    TokenKind::Keyword(Kw::Literal),
                    TokenKind::Keyword(Kw::Units),
                    TokenKind::Keyword(Kw::Group),
                    TokenKind::Keyword(Kw::File),
                    TokenKind::Keyword(Kw::Property),
                    TokenKind::Keyword(Kw::Sequence),
                ]),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "box",
                kind: LayoutItemKind::Token(TokenKind::BOX),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        EntityClassEntrySyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl EntityClassEntrySyntax {
    pub fn entity_class(&self) -> Option<EntityClassSyntax> {
        self.0.tokens().filter_map(EntityClassSyntax::cast).nth(0)
    }
    pub fn box_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::BOX)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct EntityClassEntryListSyntax(pub(crate) SyntaxNode);
impl AstNode for EntityClassEntryListSyntax {
    const META: &'static Layout = &Layout::List(List {
        kind: NodeKind::EntityClassEntryList,
        element: &LayoutItem {
            optional: false,
            repeated: true,
            name: "entity_class_entrys",
            kind: LayoutItemKind::Node(NodeKind::EntityClassEntry),
        },
        separator: &LayoutItem {
            optional: false,
            repeated: true,
            name: "comma",
            kind: LayoutItemKind::Token(TokenKind::Comma),
        },
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        EntityClassEntryListSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl EntityClassEntryListSyntax {
    pub fn entity_class_entrys(&self) -> impl Iterator<Item = EntityClassEntrySyntax> + use<'_> {
        self.0.children().filter_map(EntityClassEntrySyntax::cast)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Comma)
    }
}
#[derive(Debug, Clone)]
pub struct EntityConfigurationAspectSyntax(pub(crate) SyntaxNode);
impl AstNode for EntityConfigurationAspectSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::EntityConfigurationAspect,
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
        EntityConfigurationAspectSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl EntityConfigurationAspectSyntax {
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
pub struct EntityDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for EntityDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::EntityDeclaration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "entity_declaration_preamble",
                kind: LayoutItemKind::Node(NodeKind::EntityDeclarationPreamble),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "entity_header",
                kind: LayoutItemKind::Node(NodeKind::EntityHeader),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "declarations",
                kind: LayoutItemKind::Node(NodeKind::Declarations),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "entity_statement_part",
                kind: LayoutItemKind::Node(NodeKind::EntityStatementPart),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "entity_declaration_epilogue",
                kind: LayoutItemKind::Node(NodeKind::EntityDeclarationEpilogue),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        EntityDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl EntityDeclarationSyntax {
    pub fn entity_declaration_preamble(&self) -> Option<EntityDeclarationPreambleSyntax> {
        self.0
            .children()
            .filter_map(EntityDeclarationPreambleSyntax::cast)
            .nth(0)
    }
    pub fn entity_header(&self) -> Option<EntityHeaderSyntax> {
        self.0
            .children()
            .filter_map(EntityHeaderSyntax::cast)
            .nth(0)
    }
    pub fn declarations(&self) -> Option<DeclarationsSyntax> {
        self.0
            .children()
            .filter_map(DeclarationsSyntax::cast)
            .nth(0)
    }
    pub fn entity_statement_part(&self) -> Option<EntityStatementPartSyntax> {
        self.0
            .children()
            .filter_map(EntityStatementPartSyntax::cast)
            .nth(0)
    }
    pub fn entity_declaration_epilogue(&self) -> Option<EntityDeclarationEpilogueSyntax> {
        self.0
            .children()
            .filter_map(EntityDeclarationEpilogueSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct EntityDeclarationEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for EntityDeclarationEpilogueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::EntityDeclarationEpilogue,
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
                name: "entity",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Entity)),
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
        EntityDeclarationEpilogueSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl EntityDeclarationEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::End))
            .nth(0)
    }
    pub fn entity_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Entity))
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
pub struct EntityDeclarationPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for EntityDeclarationPreambleSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::EntityDeclarationPreamble,
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
        EntityDeclarationPreambleSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl EntityDeclarationPreambleSyntax {
    pub fn entity_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Entity))
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
pub struct EntityDesignatorSyntax(pub(crate) SyntaxNode);
impl AstNode for EntityDesignatorSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::EntityDesignator,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "entity_tag",
                kind: LayoutItemKind::TokenChoice(&[
                    TokenKind::Identifier,
                    TokenKind::CharacterLiteral,
                    TokenKind::StringLiteral,
                ]),
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
        EntityDesignatorSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl EntityDesignatorSyntax {
    pub fn entity_tag(&self) -> Option<EntityTagSyntax> {
        self.0.tokens().filter_map(EntityTagSyntax::cast).nth(0)
    }
    pub fn signature(&self) -> Option<SignatureSyntax> {
        self.0.children().filter_map(SignatureSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct EntityDesignatorListSyntax(pub(crate) SyntaxNode);
impl AstNode for EntityDesignatorListSyntax {
    const META: &'static Layout = &Layout::List(List {
        kind: NodeKind::EntityDesignatorList,
        element: &LayoutItem {
            optional: false,
            repeated: true,
            name: "entity_designators",
            kind: LayoutItemKind::Node(NodeKind::EntityDesignator),
        },
        separator: &LayoutItem {
            optional: false,
            repeated: true,
            name: "comma",
            kind: LayoutItemKind::Token(TokenKind::Comma),
        },
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        EntityDesignatorListSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl EntityDesignatorListSyntax {
    pub fn entity_designators(&self) -> impl Iterator<Item = EntityDesignatorSyntax> + use<'_> {
        self.0.children().filter_map(EntityDesignatorSyntax::cast)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Comma)
    }
}
#[derive(Debug, Clone)]
pub struct EntityEntityAspectSyntax(pub(crate) SyntaxNode);
impl AstNode for EntityEntityAspectSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::EntityEntityAspect,
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
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        EntityEntityAspectSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl EntityEntityAspectSyntax {
    pub fn entity_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Entity))
            .nth(0)
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct EntityHeaderSyntax(pub(crate) SyntaxNode);
impl AstNode for EntityHeaderSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::EntityHeader,
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
                name: "port_clause",
                kind: LayoutItemKind::Node(NodeKind::PortClause),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        EntityHeaderSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl EntityHeaderSyntax {
    pub fn generic_clause(&self) -> Option<GenericClauseSyntax> {
        self.0
            .children()
            .filter_map(GenericClauseSyntax::cast)
            .nth(0)
    }
    pub fn port_clause(&self) -> Option<PortClauseSyntax> {
        self.0.children().filter_map(PortClauseSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum EntityNameListSyntax {
    EntityDesignatorList(EntityDesignatorListSyntax),
    EntityNameListAll(EntityNameListAllSyntax),
    EntityNameListOthers(EntityNameListOthersSyntax),
}
impl AstNode for EntityNameListSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::EntityDesignatorList,
            NodeKind::EntityNameListAll,
            NodeKind::EntityNameListOthers,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if EntityDesignatorListSyntax::can_cast(&node) {
            return EntityNameListSyntax::EntityDesignatorList(
                EntityDesignatorListSyntax::cast_unchecked(node),
            );
        }
        if EntityNameListAllSyntax::can_cast(&node) {
            return EntityNameListSyntax::EntityNameListAll(
                EntityNameListAllSyntax::cast_unchecked(node),
            );
        }
        if EntityNameListOthersSyntax::can_cast(&node) {
            return EntityNameListSyntax::EntityNameListOthers(
                EntityNameListOthersSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            EntityNameListSyntax::EntityDesignatorList(inner) => inner.raw(),
            EntityNameListSyntax::EntityNameListAll(inner) => inner.raw(),
            EntityNameListSyntax::EntityNameListOthers(inner) => inner.raw(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct EntityNameListAllSyntax(pub(crate) SyntaxNode);
impl AstNode for EntityNameListAllSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::EntityNameListAll,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "all",
            kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::All)),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        EntityNameListAllSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl EntityNameListAllSyntax {
    pub fn all_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::All))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct EntityNameListOthersSyntax(pub(crate) SyntaxNode);
impl AstNode for EntityNameListOthersSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::EntityNameListOthers,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "others",
            kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Others)),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        EntityNameListOthersSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl EntityNameListOthersSyntax {
    pub fn others_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Others))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct EntityOpenAspectSyntax(pub(crate) SyntaxNode);
impl AstNode for EntityOpenAspectSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::EntityOpenAspect,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "open",
            kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Open)),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        EntityOpenAspectSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl EntityOpenAspectSyntax {
    pub fn open_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Open))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct EntitySpecificationSyntax(pub(crate) SyntaxNode);
impl AstNode for EntitySpecificationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::EntitySpecification,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "entity_name_list",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::EntityDesignatorList,
                    NodeKind::EntityNameListAll,
                    NodeKind::EntityNameListOthers,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "colon",
                kind: LayoutItemKind::Token(TokenKind::Colon),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "entity_class",
                kind: LayoutItemKind::TokenChoice(&[
                    TokenKind::Keyword(Kw::Entity),
                    TokenKind::Keyword(Kw::Architecture),
                    TokenKind::Keyword(Kw::Configuration),
                    TokenKind::Keyword(Kw::Procedure),
                    TokenKind::Keyword(Kw::Function),
                    TokenKind::Keyword(Kw::Package),
                    TokenKind::Keyword(Kw::Type),
                    TokenKind::Keyword(Kw::Subtype),
                    TokenKind::Keyword(Kw::Constant),
                    TokenKind::Keyword(Kw::Signal),
                    TokenKind::Keyword(Kw::Variable),
                    TokenKind::Keyword(Kw::Component),
                    TokenKind::Keyword(Kw::Label),
                    TokenKind::Keyword(Kw::Literal),
                    TokenKind::Keyword(Kw::Units),
                    TokenKind::Keyword(Kw::Group),
                    TokenKind::Keyword(Kw::File),
                    TokenKind::Keyword(Kw::Property),
                    TokenKind::Keyword(Kw::Sequence),
                ]),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        EntitySpecificationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl EntitySpecificationSyntax {
    pub fn entity_name_list(&self) -> Option<EntityNameListSyntax> {
        self.0
            .children()
            .filter_map(EntityNameListSyntax::cast)
            .nth(0)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Colon)
            .nth(0)
    }
    pub fn entity_class(&self) -> Option<EntityClassSyntax> {
        self.0.tokens().filter_map(EntityClassSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct EntityStatementPartSyntax(pub(crate) SyntaxNode);
impl AstNode for EntityStatementPartSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::EntityStatementPart,
        items: &[
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
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        EntityStatementPartSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl EntityStatementPartSyntax {
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
}
#[derive(Debug, Clone)]
pub enum EntityTagSyntax {
    Identifier(SyntaxToken),
    CharacterLiteral(SyntaxToken),
    StringLiteral(SyntaxToken),
}
impl EntityTagSyntax {
    pub fn cast(token: SyntaxToken) -> Option<Self> {
        match token.kind() {
            TokenKind::Identifier => Some(EntityTagSyntax::Identifier(token)),
            TokenKind::CharacterLiteral => Some(EntityTagSyntax::CharacterLiteral(token)),
            TokenKind::StringLiteral => Some(EntityTagSyntax::StringLiteral(token)),
            _ => None,
        }
    }
    pub fn raw(&self) -> SyntaxToken {
        match self {
            EntityTagSyntax::Identifier(token) => token.clone(),
            EntityTagSyntax::CharacterLiteral(token) => token.clone(),
            EntityTagSyntax::StringLiteral(token) => token.clone(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct EnumerationListSyntax(pub(crate) SyntaxNode);
impl AstNode for EnumerationListSyntax {
    const META: &'static Layout = &Layout::List(List {
        kind: NodeKind::EnumerationList,
        element: &LayoutItem {
            optional: false,
            repeated: true,
            name: "enumeration_literals",
            kind: LayoutItemKind::TokenChoice(&[
                TokenKind::Identifier,
                TokenKind::CharacterLiteral,
            ]),
        },
        separator: &LayoutItem {
            optional: false,
            repeated: true,
            name: "comma",
            kind: LayoutItemKind::Token(TokenKind::Comma),
        },
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        EnumerationListSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl EnumerationListSyntax {
    pub fn enumeration_literals(&self) -> impl Iterator<Item = EnumerationLiteralSyntax> + use<'_> {
        self.0.tokens().filter_map(EnumerationLiteralSyntax::cast)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Comma)
    }
}
#[derive(Debug, Clone)]
pub enum EnumerationLiteralSyntax {
    Identifier(SyntaxToken),
    CharacterLiteral(SyntaxToken),
}
impl EnumerationLiteralSyntax {
    pub fn cast(token: SyntaxToken) -> Option<Self> {
        match token.kind() {
            TokenKind::Identifier => Some(EnumerationLiteralSyntax::Identifier(token)),
            TokenKind::CharacterLiteral => Some(EnumerationLiteralSyntax::CharacterLiteral(token)),
            _ => None,
        }
    }
    pub fn raw(&self) -> SyntaxToken {
        match self {
            EnumerationLiteralSyntax::Identifier(token) => token.clone(),
            EnumerationLiteralSyntax::CharacterLiteral(token) => token.clone(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct EnumerationTypeDefinitionSyntax(pub(crate) SyntaxNode);
impl AstNode for EnumerationTypeDefinitionSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::EnumerationTypeDefinition,
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
                name: "enumeration_list",
                kind: LayoutItemKind::Node(NodeKind::EnumerationList),
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
        EnumerationTypeDefinitionSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl EnumerationTypeDefinitionSyntax {
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::LeftPar)
            .nth(0)
    }
    pub fn enumeration_list(&self) -> Option<EnumerationListSyntax> {
        self.0
            .children()
            .filter_map(EnumerationListSyntax::cast)
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
pub struct ExitStatementSyntax(pub(crate) SyntaxNode);
impl AstNode for ExitStatementSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ExitStatement,
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
                name: "exit",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Exit)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "loop_label",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "when_clause",
                kind: LayoutItemKind::Node(NodeKind::WhenClause),
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
        ExitStatementSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ExitStatementSyntax {
    pub fn label(&self) -> Option<LabelSyntax> {
        self.0.children().filter_map(LabelSyntax::cast).nth(0)
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
            .nth(0)
    }
    pub fn when_clause(&self) -> Option<WhenClauseSyntax> {
        self.0.children().filter_map(WhenClauseSyntax::cast).nth(0)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum ExpressionSyntax {
    LiteralExpression(LiteralExpressionSyntax),
    PhysicalLiteralExpression(PhysicalLiteralExpressionSyntax),
    UnaryExpression(UnaryExpressionSyntax),
    BinaryExpression(BinaryExpressionSyntax),
    ParenthesizedExpressionOrAggregate(ParenthesizedExpressionOrAggregateSyntax),
    Allocator(AllocatorSyntax),
    NameExpression(NameExpressionSyntax),
    QualifiedExpression(QualifiedExpressionSyntax),
}
impl AstNode for ExpressionSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::LiteralExpression,
            NodeKind::PhysicalLiteralExpression,
            NodeKind::UnaryExpression,
            NodeKind::BinaryExpression,
            NodeKind::ParenthesizedExpressionOrAggregate,
            NodeKind::Allocator,
            NodeKind::NameExpression,
            NodeKind::QualifiedExpression,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if LiteralExpressionSyntax::can_cast(&node) {
            return ExpressionSyntax::LiteralExpression(LiteralExpressionSyntax::cast_unchecked(
                node,
            ));
        }
        if PhysicalLiteralExpressionSyntax::can_cast(&node) {
            return ExpressionSyntax::PhysicalLiteralExpression(
                PhysicalLiteralExpressionSyntax::cast_unchecked(node),
            );
        }
        if UnaryExpressionSyntax::can_cast(&node) {
            return ExpressionSyntax::UnaryExpression(UnaryExpressionSyntax::cast_unchecked(node));
        }
        if BinaryExpressionSyntax::can_cast(&node) {
            return ExpressionSyntax::BinaryExpression(BinaryExpressionSyntax::cast_unchecked(
                node,
            ));
        }
        if ParenthesizedExpressionOrAggregateSyntax::can_cast(&node) {
            return ExpressionSyntax::ParenthesizedExpressionOrAggregate(
                ParenthesizedExpressionOrAggregateSyntax::cast_unchecked(node),
            );
        }
        if AllocatorSyntax::can_cast(&node) {
            return ExpressionSyntax::Allocator(AllocatorSyntax::cast_unchecked(node));
        }
        if NameExpressionSyntax::can_cast(&node) {
            return ExpressionSyntax::NameExpression(NameExpressionSyntax::cast_unchecked(node));
        }
        if QualifiedExpressionSyntax::can_cast(&node) {
            return ExpressionSyntax::QualifiedExpression(
                QualifiedExpressionSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            ExpressionSyntax::LiteralExpression(inner) => inner.raw(),
            ExpressionSyntax::PhysicalLiteralExpression(inner) => inner.raw(),
            ExpressionSyntax::UnaryExpression(inner) => inner.raw(),
            ExpressionSyntax::BinaryExpression(inner) => inner.raw(),
            ExpressionSyntax::ParenthesizedExpressionOrAggregate(inner) => inner.raw(),
            ExpressionSyntax::Allocator(inner) => inner.raw(),
            ExpressionSyntax::NameExpression(inner) => inner.raw(),
            ExpressionSyntax::QualifiedExpression(inner) => inner.raw(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct ExpressionChoiceSyntax(pub(crate) SyntaxNode);
impl AstNode for ExpressionChoiceSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ExpressionChoice,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "expression",
            kind: LayoutItemKind::NodeChoice(&[
                NodeKind::LiteralExpression,
                NodeKind::PhysicalLiteralExpression,
                NodeKind::UnaryExpression,
                NodeKind::BinaryExpression,
                NodeKind::ParenthesizedExpressionOrAggregate,
                NodeKind::Allocator,
                NodeKind::NameExpression,
                NodeKind::QualifiedExpression,
            ]),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ExpressionChoiceSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ExpressionChoiceSyntax {
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ExpressionListSyntax(pub(crate) SyntaxNode);
impl AstNode for ExpressionListSyntax {
    const META: &'static Layout = &Layout::List(List {
        kind: NodeKind::ExpressionList,
        element: &LayoutItem {
            optional: false,
            repeated: true,
            name: "expressions",
            kind: LayoutItemKind::NodeChoice(&[
                NodeKind::LiteralExpression,
                NodeKind::PhysicalLiteralExpression,
                NodeKind::UnaryExpression,
                NodeKind::BinaryExpression,
                NodeKind::ParenthesizedExpressionOrAggregate,
                NodeKind::Allocator,
                NodeKind::NameExpression,
                NodeKind::QualifiedExpression,
            ]),
        },
        separator: &LayoutItem {
            optional: false,
            repeated: true,
            name: "comma",
            kind: LayoutItemKind::Token(TokenKind::Comma),
        },
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ExpressionListSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ExpressionListSyntax {
    pub fn expressions(&self) -> impl Iterator<Item = ExpressionSyntax> + use<'_> {
        self.0.children().filter_map(ExpressionSyntax::cast)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Comma)
    }
}
#[derive(Debug, Clone)]
pub struct ExternalConstantNameSyntax(pub(crate) SyntaxNode);
impl AstNode for ExternalConstantNameSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ExternalConstantName,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "lt_lt",
                kind: LayoutItemKind::Token(TokenKind::LtLt),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "constant",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Constant)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "external_pathname",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::PackagePathname,
                    NodeKind::AbsolutePathname,
                    NodeKind::RelativePathname,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "colon",
                kind: LayoutItemKind::Token(TokenKind::Colon),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "subtype_indication",
                kind: LayoutItemKind::Node(NodeKind::SubtypeIndication),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "gt_gt",
                kind: LayoutItemKind::Token(TokenKind::GtGt),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ExternalConstantNameSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ExternalConstantNameSyntax {
    pub fn lt_lt_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::LtLt)
            .nth(0)
    }
    pub fn constant_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Constant))
            .nth(0)
    }
    pub fn external_pathname(&self) -> Option<ExternalPathnameSyntax> {
        self.0
            .children()
            .filter_map(ExternalPathnameSyntax::cast)
            .nth(0)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Colon)
            .nth(0)
    }
    pub fn subtype_indication(&self) -> Option<SubtypeIndicationSyntax> {
        self.0
            .children()
            .filter_map(SubtypeIndicationSyntax::cast)
            .nth(0)
    }
    pub fn gt_gt_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::GtGt)
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
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::ExternalConstantName,
            NodeKind::ExternalSignalName,
            NodeKind::ExternalVariableName,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if ExternalConstantNameSyntax::can_cast(&node) {
            return ExternalNameSyntax::ExternalConstantName(
                ExternalConstantNameSyntax::cast_unchecked(node),
            );
        }
        if ExternalSignalNameSyntax::can_cast(&node) {
            return ExternalNameSyntax::ExternalSignalName(
                ExternalSignalNameSyntax::cast_unchecked(node),
            );
        }
        if ExternalVariableNameSyntax::can_cast(&node) {
            return ExternalNameSyntax::ExternalVariableName(
                ExternalVariableNameSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
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
pub enum ExternalPathnameSyntax {
    PackagePathname(PackagePathnameSyntax),
    AbsolutePathname(AbsolutePathnameSyntax),
    RelativePathname(RelativePathnameSyntax),
}
impl AstNode for ExternalPathnameSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::PackagePathname,
            NodeKind::AbsolutePathname,
            NodeKind::RelativePathname,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if PackagePathnameSyntax::can_cast(&node) {
            return ExternalPathnameSyntax::PackagePathname(PackagePathnameSyntax::cast_unchecked(
                node,
            ));
        }
        if AbsolutePathnameSyntax::can_cast(&node) {
            return ExternalPathnameSyntax::AbsolutePathname(
                AbsolutePathnameSyntax::cast_unchecked(node),
            );
        }
        if RelativePathnameSyntax::can_cast(&node) {
            return ExternalPathnameSyntax::RelativePathname(
                RelativePathnameSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            ExternalPathnameSyntax::PackagePathname(inner) => inner.raw(),
            ExternalPathnameSyntax::AbsolutePathname(inner) => inner.raw(),
            ExternalPathnameSyntax::RelativePathname(inner) => inner.raw(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct ExternalSignalNameSyntax(pub(crate) SyntaxNode);
impl AstNode for ExternalSignalNameSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ExternalSignalName,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "lt_lt",
                kind: LayoutItemKind::Token(TokenKind::LtLt),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "signal",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Signal)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "external_pathname",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::PackagePathname,
                    NodeKind::AbsolutePathname,
                    NodeKind::RelativePathname,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "colon",
                kind: LayoutItemKind::Token(TokenKind::Colon),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "subtype_indication",
                kind: LayoutItemKind::Node(NodeKind::SubtypeIndication),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "gt_gt",
                kind: LayoutItemKind::Token(TokenKind::GtGt),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ExternalSignalNameSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ExternalSignalNameSyntax {
    pub fn lt_lt_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::LtLt)
            .nth(0)
    }
    pub fn signal_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Signal))
            .nth(0)
    }
    pub fn external_pathname(&self) -> Option<ExternalPathnameSyntax> {
        self.0
            .children()
            .filter_map(ExternalPathnameSyntax::cast)
            .nth(0)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Colon)
            .nth(0)
    }
    pub fn subtype_indication(&self) -> Option<SubtypeIndicationSyntax> {
        self.0
            .children()
            .filter_map(SubtypeIndicationSyntax::cast)
            .nth(0)
    }
    pub fn gt_gt_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::GtGt)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ExternalVariableNameSyntax(pub(crate) SyntaxNode);
impl AstNode for ExternalVariableNameSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ExternalVariableName,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "lt_lt",
                kind: LayoutItemKind::Token(TokenKind::LtLt),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "variable",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Variable)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "external_pathname",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::PackagePathname,
                    NodeKind::AbsolutePathname,
                    NodeKind::RelativePathname,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "colon",
                kind: LayoutItemKind::Token(TokenKind::Colon),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "subtype_indication",
                kind: LayoutItemKind::Node(NodeKind::SubtypeIndication),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "gt_gt",
                kind: LayoutItemKind::Token(TokenKind::GtGt),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ExternalVariableNameSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ExternalVariableNameSyntax {
    pub fn lt_lt_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::LtLt)
            .nth(0)
    }
    pub fn variable_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Variable))
            .nth(0)
    }
    pub fn external_pathname(&self) -> Option<ExternalPathnameSyntax> {
        self.0
            .children()
            .filter_map(ExternalPathnameSyntax::cast)
            .nth(0)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Colon)
            .nth(0)
    }
    pub fn subtype_indication(&self) -> Option<SubtypeIndicationSyntax> {
        self.0
            .children()
            .filter_map(SubtypeIndicationSyntax::cast)
            .nth(0)
    }
    pub fn gt_gt_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::GtGt)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct FileDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for FileDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::FileDeclaration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "file",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::File)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "identifier_list",
                kind: LayoutItemKind::Node(NodeKind::IdentifierList),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "colon",
                kind: LayoutItemKind::Token(TokenKind::Colon),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "subtype_indication",
                kind: LayoutItemKind::Node(NodeKind::SubtypeIndication),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "file_open_information",
                kind: LayoutItemKind::Node(NodeKind::FileOpenInformation),
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
        FileDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl FileDeclarationSyntax {
    pub fn file_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::File))
            .nth(0)
    }
    pub fn identifier_list(&self) -> Option<IdentifierListSyntax> {
        self.0
            .children()
            .filter_map(IdentifierListSyntax::cast)
            .nth(0)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Colon)
            .nth(0)
    }
    pub fn subtype_indication(&self) -> Option<SubtypeIndicationSyntax> {
        self.0
            .children()
            .filter_map(SubtypeIndicationSyntax::cast)
            .nth(0)
    }
    pub fn file_open_information(&self) -> Option<FileOpenInformationSyntax> {
        self.0
            .children()
            .filter_map(FileOpenInformationSyntax::cast)
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
pub struct FileOpenInformationSyntax(pub(crate) SyntaxNode);
impl AstNode for FileOpenInformationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::FileOpenInformation,
        items: &[
            LayoutItem {
                optional: true,
                repeated: false,
                name: "file_open_kind",
                kind: LayoutItemKind::Node(NodeKind::FileOpenKind),
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
                name: "expression",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::LiteralExpression,
                    NodeKind::PhysicalLiteralExpression,
                    NodeKind::UnaryExpression,
                    NodeKind::BinaryExpression,
                    NodeKind::ParenthesizedExpressionOrAggregate,
                    NodeKind::Allocator,
                    NodeKind::NameExpression,
                    NodeKind::QualifiedExpression,
                ]),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        FileOpenInformationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl FileOpenInformationSyntax {
    pub fn file_open_kind(&self) -> Option<FileOpenKindSyntax> {
        self.0
            .children()
            .filter_map(FileOpenKindSyntax::cast)
            .nth(0)
    }
    pub fn is_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Is))
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct FileOpenKindSyntax(pub(crate) SyntaxNode);
impl AstNode for FileOpenKindSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::FileOpenKind,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "open",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Open)),
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
                    NodeKind::Allocator,
                    NodeKind::NameExpression,
                    NodeKind::QualifiedExpression,
                ]),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        FileOpenKindSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl FileOpenKindSyntax {
    pub fn open_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Open))
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct FileTypeDefinitionSyntax(pub(crate) SyntaxNode);
impl AstNode for FileTypeDefinitionSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::FileTypeDefinition,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "file",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::File)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "of",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Of)),
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
        FileTypeDefinitionSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl FileTypeDefinitionSyntax {
    pub fn file_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::File))
            .nth(0)
    }
    pub fn of_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Of))
            .nth(0)
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ForGeneratePreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for ForGeneratePreambleSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ForGeneratePreamble,
        items: &[
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
        ForGeneratePreambleSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ForGeneratePreambleSyntax {
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
pub struct ForGenerateStatementSyntax(pub(crate) SyntaxNode);
impl AstNode for ForGenerateStatementSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ForGenerateStatement,
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
                name: "for_generate_preamble",
                kind: LayoutItemKind::Node(NodeKind::ForGeneratePreamble),
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
                name: "generate_epilogue",
                kind: LayoutItemKind::Node(NodeKind::GenerateEpilogue),
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
    pub fn label(&self) -> Option<LabelSyntax> {
        self.0.children().filter_map(LabelSyntax::cast).nth(0)
    }
    pub fn for_generate_preamble(&self) -> Option<ForGeneratePreambleSyntax> {
        self.0
            .children()
            .filter_map(ForGeneratePreambleSyntax::cast)
            .nth(0)
    }
    pub fn generate_statement_body(&self) -> Option<GenerateStatementBodySyntax> {
        self.0
            .children()
            .filter_map(GenerateStatementBodySyntax::cast)
            .nth(0)
    }
    pub fn generate_epilogue(&self) -> Option<GenerateEpilogueSyntax> {
        self.0
            .children()
            .filter_map(GenerateEpilogueSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ForSchemeSyntax(pub(crate) SyntaxNode);
impl AstNode for ForSchemeSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ForScheme,
        items: &[
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
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ForSchemeSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ForSchemeSyntax {
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
pub struct FormalSyntax(pub(crate) SyntaxNode);
impl AstNode for FormalSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::Formal,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "formal_part",
                kind: LayoutItemKind::Node(NodeKind::FormalPart),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "right_arrow",
                kind: LayoutItemKind::Token(TokenKind::RightArrow),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        FormalSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl FormalSyntax {
    pub fn formal_part(&self) -> Option<FormalPartSyntax> {
        self.0.children().filter_map(FormalPartSyntax::cast).nth(0)
    }
    pub fn right_arrow_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::RightArrow)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct FormalPartSyntax(pub(crate) SyntaxNode);
impl AstNode for FormalPartSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::FormalPart,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "name",
            kind: LayoutItemKind::Node(NodeKind::Name),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        FormalPartSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl FormalPartSyntax {
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct FullTypeDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for FullTypeDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::FullTypeDeclaration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "type",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Type)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "identifier",
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
                name: "type_definition",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::EnumerationTypeDefinition,
                    NodeKind::NumericTypeDefinition,
                    NodeKind::PhysicalTypeDefinition,
                    NodeKind::UnboundedArrayDefinition,
                    NodeKind::ConstrainedArrayDefinition,
                    NodeKind::RecordTypeDefinition,
                    NodeKind::AccessTypeDefinition,
                    NodeKind::FileTypeDefinition,
                    NodeKind::ProtectedTypeDeclaration,
                    NodeKind::ProtectedTypeBody,
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
        FullTypeDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl FullTypeDeclarationSyntax {
    pub fn type_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Type))
            .nth(0)
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
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
    pub fn type_definition(&self) -> Option<TypeDefinitionSyntax> {
        self.0
            .children()
            .filter_map(TypeDefinitionSyntax::cast)
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
pub struct FunctionSpecificationSyntax(pub(crate) SyntaxNode);
impl AstNode for FunctionSpecificationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::FunctionSpecification,
        items: &[
            LayoutItem {
                optional: true,
                repeated: false,
                name: "purity",
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
    pub fn purity(&self) -> Option<PuritySyntax> {
        self.0.tokens().filter_map(PuritySyntax::cast).nth(0)
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
pub struct GenerateBodyDeclarationsSyntax(pub(crate) SyntaxNode);
impl AstNode for GenerateBodyDeclarationsSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::GenerateBodyDeclarations,
        items: &[
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
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        GenerateBodyDeclarationsSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl GenerateBodyDeclarationsSyntax {
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
}
#[derive(Debug, Clone)]
pub struct GenerateBodyEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for GenerateBodyEpilogueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::GenerateBodyEpilogue,
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
        GenerateBodyEpilogueSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl GenerateBodyEpilogueSyntax {
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
pub struct GenerateEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for GenerateEpilogueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::GenerateEpilogue,
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
        GenerateEpilogueSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl GenerateEpilogueSyntax {
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
                name: "generate_body_declarations",
                kind: LayoutItemKind::Node(NodeKind::GenerateBodyDeclarations),
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
                name: "generate_body_epilogue",
                kind: LayoutItemKind::Node(NodeKind::GenerateBodyEpilogue),
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
    pub fn generate_body_declarations(&self) -> Option<GenerateBodyDeclarationsSyntax> {
        self.0
            .children()
            .filter_map(GenerateBodyDeclarationsSyntax::cast)
            .nth(0)
    }
    pub fn concurrent_statements(&self) -> Option<ConcurrentStatementsSyntax> {
        self.0
            .children()
            .filter_map(ConcurrentStatementsSyntax::cast)
            .nth(0)
    }
    pub fn generate_body_epilogue(&self) -> Option<GenerateBodyEpilogueSyntax> {
        self.0
            .children()
            .filter_map(GenerateBodyEpilogueSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct GenericClauseSyntax(pub(crate) SyntaxNode);
impl AstNode for GenericClauseSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::GenericClause,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "generic_clause_preamble",
                kind: LayoutItemKind::Node(NodeKind::GenericClausePreamble),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "interface_list",
                kind: LayoutItemKind::Node(NodeKind::InterfaceList),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "generic_clause_epilogue",
                kind: LayoutItemKind::Node(NodeKind::GenericClauseEpilogue),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        GenericClauseSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl GenericClauseSyntax {
    pub fn generic_clause_preamble(&self) -> Option<GenericClausePreambleSyntax> {
        self.0
            .children()
            .filter_map(GenericClausePreambleSyntax::cast)
            .nth(0)
    }
    pub fn interface_list(&self) -> Option<InterfaceListSyntax> {
        self.0
            .children()
            .filter_map(InterfaceListSyntax::cast)
            .nth(0)
    }
    pub fn generic_clause_epilogue(&self) -> Option<GenericClauseEpilogueSyntax> {
        self.0
            .children()
            .filter_map(GenericClauseEpilogueSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct GenericClauseEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for GenericClauseEpilogueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::GenericClauseEpilogue,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "right_par",
                kind: LayoutItemKind::Token(TokenKind::RightPar),
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
        GenericClauseEpilogueSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl GenericClauseEpilogueSyntax {
    pub fn right_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::RightPar)
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
pub struct GenericClausePreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for GenericClausePreambleSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::GenericClausePreamble,
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
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        GenericClausePreambleSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl GenericClausePreambleSyntax {
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
}
#[derive(Debug, Clone)]
pub struct GenericMapSyntax(pub(crate) SyntaxNode);
impl AstNode for GenericMapSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::GenericMap,
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
        GenericMapSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl GenericMapSyntax {
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
pub struct GenericMapAspectSyntax(pub(crate) SyntaxNode);
impl AstNode for GenericMapAspectSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::GenericMapAspect,
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
                name: "map",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Map)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "left_par",
                kind: LayoutItemKind::Token(TokenKind::LeftPar),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "association_list",
                kind: LayoutItemKind::Node(NodeKind::AssociationList),
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
        GenericMapAspectSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl GenericMapAspectSyntax {
    pub fn generic_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Generic))
            .nth(0)
    }
    pub fn map_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Map))
            .nth(0)
    }
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::LeftPar)
            .nth(0)
    }
    pub fn association_list(&self) -> Option<AssociationListSyntax> {
        self.0
            .children()
            .filter_map(AssociationListSyntax::cast)
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
pub struct GenericPartSyntax(pub(crate) SyntaxNode);
impl AstNode for GenericPartSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::GenericPart,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "generic_clause",
                kind: LayoutItemKind::Node(NodeKind::GenericClause),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "generic_map",
                kind: LayoutItemKind::Node(NodeKind::GenericMap),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        GenericPartSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl GenericPartSyntax {
    pub fn generic_clause(&self) -> Option<GenericClauseSyntax> {
        self.0
            .children()
            .filter_map(GenericClauseSyntax::cast)
            .nth(0)
    }
    pub fn generic_map(&self) -> Option<GenericMapSyntax> {
        self.0.children().filter_map(GenericMapSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct GroupDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for GroupDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::GroupDeclaration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "group",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Group)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "identifier",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "colon",
                kind: LayoutItemKind::Token(TokenKind::Colon),
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
        GroupDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl GroupDeclarationSyntax {
    pub fn group_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Group))
            .nth(0)
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
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
pub struct GroupTemplateDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for GroupTemplateDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::GroupTemplateDeclaration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "group",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Group)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "identifier",
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
                name: "left_par",
                kind: LayoutItemKind::Token(TokenKind::LeftPar),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "entity_class_entry_list",
                kind: LayoutItemKind::Node(NodeKind::EntityClassEntryList),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "right_par",
                kind: LayoutItemKind::Token(TokenKind::RightPar),
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
        GroupTemplateDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl GroupTemplateDeclarationSyntax {
    pub fn group_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Group))
            .nth(0)
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
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
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::LeftPar)
            .nth(0)
    }
    pub fn entity_class_entry_list(&self) -> Option<EntityClassEntryListSyntax> {
        self.0
            .children()
            .filter_map(EntityClassEntryListSyntax::cast)
            .nth(0)
    }
    pub fn right_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::RightPar)
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
pub struct GuardedSignalSpecificationSyntax(pub(crate) SyntaxNode);
impl AstNode for GuardedSignalSpecificationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::GuardedSignalSpecification,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "signal_list",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::SignalListList,
                    NodeKind::SignalListAll,
                    NodeKind::SignalListOthers,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "colon",
                kind: LayoutItemKind::Token(TokenKind::Colon),
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
        GuardedSignalSpecificationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl GuardedSignalSpecificationSyntax {
    pub fn signal_list(&self) -> Option<SignalListSyntax> {
        self.0.children().filter_map(SignalListSyntax::cast).nth(0)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Colon)
            .nth(0)
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct IdentifierListSyntax(pub(crate) SyntaxNode);
impl AstNode for IdentifierListSyntax {
    const META: &'static Layout = &Layout::List(List {
        kind: NodeKind::IdentifierList,
        element: &LayoutItem {
            optional: false,
            repeated: true,
            name: "identifier",
            kind: LayoutItemKind::Token(TokenKind::Identifier),
        },
        separator: &LayoutItem {
            optional: false,
            repeated: true,
            name: "comma",
            kind: LayoutItemKind::Token(TokenKind::Comma),
        },
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        IdentifierListSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl IdentifierListSyntax {
    pub fn identifier_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Identifier)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Comma)
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
                    NodeKind::Allocator,
                    NodeKind::NameExpression,
                    NodeKind::QualifiedExpression,
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
pub struct IfGenerateIfSyntax(pub(crate) SyntaxNode);
impl AstNode for IfGenerateIfSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::IfGenerateIf,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "if",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::If)),
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
                    NodeKind::Allocator,
                    NodeKind::NameExpression,
                    NodeKind::QualifiedExpression,
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
        IfGenerateIfSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl IfGenerateIfSyntax {
    pub fn if_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::If))
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
pub struct IfGenerateStatementSyntax(pub(crate) SyntaxNode);
impl AstNode for IfGenerateStatementSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::IfGenerateStatement,
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
                name: "if_generate_if",
                kind: LayoutItemKind::Node(NodeKind::IfGenerateIf),
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
                name: "generate_epilogue",
                kind: LayoutItemKind::Node(NodeKind::GenerateEpilogue),
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
    pub fn label(&self) -> Option<LabelSyntax> {
        self.0.children().filter_map(LabelSyntax::cast).nth(0)
    }
    pub fn if_generate_if(&self) -> Option<IfGenerateIfSyntax> {
        self.0
            .children()
            .filter_map(IfGenerateIfSyntax::cast)
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
    pub fn generate_epilogue(&self) -> Option<GenerateEpilogueSyntax> {
        self.0
            .children()
            .filter_map(GenerateEpilogueSyntax::cast)
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
pub struct IfStatementElseSyntax(pub(crate) SyntaxNode);
impl AstNode for IfStatementElseSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::IfStatementElse,
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
pub struct IfStatementElsifSyntax(pub(crate) SyntaxNode);
impl AstNode for IfStatementElsifSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::IfStatementElsif,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "elsif",
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
                    NodeKind::Allocator,
                    NodeKind::NameExpression,
                    NodeKind::QualifiedExpression,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "then",
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
pub struct IfStatementEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for IfStatementEpilogueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::IfStatementEpilogue,
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
                name: "if",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::If)),
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
pub struct IfStatementPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for IfStatementPreambleSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::IfStatementPreamble,
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
                optional: false,
                repeated: false,
                name: "expression",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::LiteralExpression,
                    NodeKind::PhysicalLiteralExpression,
                    NodeKind::UnaryExpression,
                    NodeKind::BinaryExpression,
                    NodeKind::ParenthesizedExpressionOrAggregate,
                    NodeKind::Allocator,
                    NodeKind::NameExpression,
                    NodeKind::QualifiedExpression,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "then",
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
    pub fn label(&self) -> Option<LabelSyntax> {
        self.0.children().filter_map(LabelSyntax::cast).nth(0)
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
pub struct IncompleteTypeDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for IncompleteTypeDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::IncompleteTypeDeclaration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "type",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Type)),
            },
            LayoutItem {
                optional: false,
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
        IncompleteTypeDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl IncompleteTypeDeclarationSyntax {
    pub fn type_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Type))
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
pub struct IndexConstraintSyntax(pub(crate) SyntaxNode);
impl AstNode for IndexConstraintSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::IndexConstraint,
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
                name: "expression_list",
                kind: LayoutItemKind::Node(NodeKind::ExpressionList),
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
        IndexConstraintSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl IndexConstraintSyntax {
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::LeftPar)
            .nth(0)
    }
    pub fn expression_list(&self) -> Option<ExpressionListSyntax> {
        self.0
            .children()
            .filter_map(ExpressionListSyntax::cast)
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
pub struct IndexSubtypeDefinitionSyntax(pub(crate) SyntaxNode);
impl AstNode for IndexSubtypeDefinitionSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::IndexSubtypeDefinition,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "name",
                kind: LayoutItemKind::Node(NodeKind::Name),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "range",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Range)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "box",
                kind: LayoutItemKind::Token(TokenKind::BOX),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        IndexSubtypeDefinitionSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl IndexSubtypeDefinitionSyntax {
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
    pub fn range_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Range))
            .nth(0)
    }
    pub fn box_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::BOX)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct IndexSubtypeDefinitionListSyntax(pub(crate) SyntaxNode);
impl AstNode for IndexSubtypeDefinitionListSyntax {
    const META: &'static Layout = &Layout::List(List {
        kind: NodeKind::IndexSubtypeDefinitionList,
        element: &LayoutItem {
            optional: false,
            repeated: true,
            name: "index_subtype_definitions",
            kind: LayoutItemKind::Node(NodeKind::IndexSubtypeDefinition),
        },
        separator: &LayoutItem {
            optional: false,
            repeated: true,
            name: "comma",
            kind: LayoutItemKind::Token(TokenKind::Comma),
        },
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        IndexSubtypeDefinitionListSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl IndexSubtypeDefinitionListSyntax {
    pub fn index_subtype_definitions(
        &self,
    ) -> impl Iterator<Item = IndexSubtypeDefinitionSyntax> + use<'_> {
        self.0
            .children()
            .filter_map(IndexSubtypeDefinitionSyntax::cast)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Comma)
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
                name: "reject_clause",
                kind: LayoutItemKind::Node(NodeKind::RejectClause),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "inertial",
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
    pub fn reject_clause(&self) -> Option<RejectClauseSyntax> {
        self.0
            .children()
            .filter_map(RejectClauseSyntax::cast)
            .nth(0)
    }
    pub fn inertial_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Inertial))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct InitialValueSyntax(pub(crate) SyntaxNode);
impl AstNode for InitialValueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::InitialValue,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "colon_eq",
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
                    NodeKind::Allocator,
                    NodeKind::NameExpression,
                    NodeKind::QualifiedExpression,
                ]),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        InitialValueSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InitialValueSyntax {
    pub fn colon_eq_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::ColonEq)
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct InstantiatedComponentSyntax(pub(crate) SyntaxNode);
impl AstNode for InstantiatedComponentSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::InstantiatedComponent,
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
        InstantiatedComponentSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InstantiatedComponentSyntax {
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
pub struct InstantiatedConfigurationSyntax(pub(crate) SyntaxNode);
impl AstNode for InstantiatedConfigurationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::InstantiatedConfiguration,
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
        InstantiatedConfigurationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InstantiatedConfigurationSyntax {
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
pub struct InstantiatedEntitySyntax(pub(crate) SyntaxNode);
impl AstNode for InstantiatedEntitySyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::InstantiatedEntity,
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
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        InstantiatedEntitySyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InstantiatedEntitySyntax {
    pub fn entity_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Entity))
            .nth(0)
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum InstantiatedUnitSyntax {
    InstantiatedComponent(InstantiatedComponentSyntax),
    InstantiatedEntity(InstantiatedEntitySyntax),
    InstantiatedConfiguration(InstantiatedConfigurationSyntax),
}
impl AstNode for InstantiatedUnitSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::InstantiatedComponent,
            NodeKind::InstantiatedEntity,
            NodeKind::InstantiatedConfiguration,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if InstantiatedComponentSyntax::can_cast(&node) {
            return InstantiatedUnitSyntax::InstantiatedComponent(
                InstantiatedComponentSyntax::cast_unchecked(node),
            );
        }
        if InstantiatedEntitySyntax::can_cast(&node) {
            return InstantiatedUnitSyntax::InstantiatedEntity(
                InstantiatedEntitySyntax::cast_unchecked(node),
            );
        }
        if InstantiatedConfigurationSyntax::can_cast(&node) {
            return InstantiatedUnitSyntax::InstantiatedConfiguration(
                InstantiatedConfigurationSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            InstantiatedUnitSyntax::InstantiatedComponent(inner) => inner.raw(),
            InstantiatedUnitSyntax::InstantiatedEntity(inner) => inner.raw(),
            InstantiatedUnitSyntax::InstantiatedConfiguration(inner) => inner.raw(),
        }
    }
}
#[derive(Debug, Clone)]
pub enum InstantiationListSyntax {
    InstantiationListList(InstantiationListListSyntax),
    InstantiationListAll(InstantiationListAllSyntax),
    InstantiationListOthers(InstantiationListOthersSyntax),
}
impl AstNode for InstantiationListSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::InstantiationListList,
            NodeKind::InstantiationListAll,
            NodeKind::InstantiationListOthers,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if InstantiationListListSyntax::can_cast(&node) {
            return InstantiationListSyntax::InstantiationListList(
                InstantiationListListSyntax::cast_unchecked(node),
            );
        }
        if InstantiationListAllSyntax::can_cast(&node) {
            return InstantiationListSyntax::InstantiationListAll(
                InstantiationListAllSyntax::cast_unchecked(node),
            );
        }
        if InstantiationListOthersSyntax::can_cast(&node) {
            return InstantiationListSyntax::InstantiationListOthers(
                InstantiationListOthersSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            InstantiationListSyntax::InstantiationListList(inner) => inner.raw(),
            InstantiationListSyntax::InstantiationListAll(inner) => inner.raw(),
            InstantiationListSyntax::InstantiationListOthers(inner) => inner.raw(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct InstantiationListAllSyntax(pub(crate) SyntaxNode);
impl AstNode for InstantiationListAllSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::InstantiationListAll,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "all",
            kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::All)),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        InstantiationListAllSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InstantiationListAllSyntax {
    pub fn all_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::All))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct InstantiationListListSyntax(pub(crate) SyntaxNode);
impl AstNode for InstantiationListListSyntax {
    const META: &'static Layout = &Layout::List(List {
        kind: NodeKind::InstantiationListList,
        element: &LayoutItem {
            optional: false,
            repeated: true,
            name: "identifier",
            kind: LayoutItemKind::Token(TokenKind::Identifier),
        },
        separator: &LayoutItem {
            optional: false,
            repeated: true,
            name: "comma",
            kind: LayoutItemKind::Token(TokenKind::Comma),
        },
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        InstantiationListListSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InstantiationListListSyntax {
    pub fn identifier_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Identifier)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Comma)
    }
}
#[derive(Debug, Clone)]
pub struct InstantiationListOthersSyntax(pub(crate) SyntaxNode);
impl AstNode for InstantiationListOthersSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::InstantiationListOthers,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "others",
            kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Others)),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        InstantiationListOthersSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InstantiationListOthersSyntax {
    pub fn others_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Others))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum InterfaceDeclarationSyntax {
    InterfaceObjectDeclaration(InterfaceObjectDeclarationSyntax),
    InterfaceFileDeclaration(InterfaceFileDeclarationSyntax),
    InterfaceIncompleteTypeDeclaration(InterfaceIncompleteTypeDeclarationSyntax),
    InterfaceSubprogramDeclaration(InterfaceSubprogramDeclarationSyntax),
    InterfacePackageDeclaration(InterfacePackageDeclarationSyntax),
}
impl AstNode for InterfaceDeclarationSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::InterfaceObjectDeclaration,
            NodeKind::InterfaceFileDeclaration,
            NodeKind::InterfaceIncompleteTypeDeclaration,
            NodeKind::InterfaceSubprogramDeclaration,
            NodeKind::InterfacePackageDeclaration,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if InterfaceObjectDeclarationSyntax::can_cast(&node) {
            return InterfaceDeclarationSyntax::InterfaceObjectDeclaration(
                InterfaceObjectDeclarationSyntax::cast_unchecked(node),
            );
        }
        if InterfaceFileDeclarationSyntax::can_cast(&node) {
            return InterfaceDeclarationSyntax::InterfaceFileDeclaration(
                InterfaceFileDeclarationSyntax::cast_unchecked(node),
            );
        }
        if InterfaceIncompleteTypeDeclarationSyntax::can_cast(&node) {
            return InterfaceDeclarationSyntax::InterfaceIncompleteTypeDeclaration(
                InterfaceIncompleteTypeDeclarationSyntax::cast_unchecked(node),
            );
        }
        if InterfaceSubprogramDeclarationSyntax::can_cast(&node) {
            return InterfaceDeclarationSyntax::InterfaceSubprogramDeclaration(
                InterfaceSubprogramDeclarationSyntax::cast_unchecked(node),
            );
        }
        if InterfacePackageDeclarationSyntax::can_cast(&node) {
            return InterfaceDeclarationSyntax::InterfacePackageDeclaration(
                InterfacePackageDeclarationSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            InterfaceDeclarationSyntax::InterfaceObjectDeclaration(inner) => inner.raw(),
            InterfaceDeclarationSyntax::InterfaceFileDeclaration(inner) => inner.raw(),
            InterfaceDeclarationSyntax::InterfaceIncompleteTypeDeclaration(inner) => inner.raw(),
            InterfaceDeclarationSyntax::InterfaceSubprogramDeclaration(inner) => inner.raw(),
            InterfaceDeclarationSyntax::InterfacePackageDeclaration(inner) => inner.raw(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct InterfaceFileDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfaceFileDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::InterfaceFileDeclaration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "file",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::File)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "identifier_list",
                kind: LayoutItemKind::Node(NodeKind::IdentifierList),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "colon",
                kind: LayoutItemKind::Token(TokenKind::Colon),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "subtype_indication",
                kind: LayoutItemKind::Node(NodeKind::SubtypeIndication),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        InterfaceFileDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfaceFileDeclarationSyntax {
    pub fn file_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::File))
            .nth(0)
    }
    pub fn identifier_list(&self) -> Option<IdentifierListSyntax> {
        self.0
            .children()
            .filter_map(IdentifierListSyntax::cast)
            .nth(0)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Colon)
            .nth(0)
    }
    pub fn subtype_indication(&self) -> Option<SubtypeIndicationSyntax> {
        self.0
            .children()
            .filter_map(SubtypeIndicationSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct InterfaceFunctionSpecificationSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfaceFunctionSpecificationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::InterfaceFunctionSpecification,
        items: &[
            LayoutItem {
                optional: true,
                repeated: false,
                name: "purity",
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
        InterfaceFunctionSpecificationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfaceFunctionSpecificationSyntax {
    pub fn purity(&self) -> Option<PuritySyntax> {
        self.0.tokens().filter_map(PuritySyntax::cast).nth(0)
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
pub struct InterfaceIncompleteTypeDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfaceIncompleteTypeDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::InterfaceIncompleteTypeDeclaration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "type",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Type)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "identifier",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        InterfaceIncompleteTypeDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfaceIncompleteTypeDeclarationSyntax {
    pub fn type_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Type))
            .nth(0)
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Identifier)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct InterfaceListSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfaceListSyntax {
    const META: &'static Layout = &Layout::List(List {
        kind: NodeKind::InterfaceList,
        element: &LayoutItem {
            optional: false,
            repeated: true,
            name: "interface_declarations",
            kind: LayoutItemKind::NodeChoice(&[
                NodeKind::InterfaceObjectDeclaration,
                NodeKind::InterfaceFileDeclaration,
                NodeKind::InterfaceIncompleteTypeDeclaration,
                NodeKind::InterfaceSubprogramDeclaration,
                NodeKind::InterfacePackageDeclaration,
            ]),
        },
        separator: &LayoutItem {
            optional: false,
            repeated: true,
            name: "semi_colon",
            kind: LayoutItemKind::Token(TokenKind::SemiColon),
        },
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        InterfaceListSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfaceListSyntax {
    pub fn interface_declarations(
        &self,
    ) -> impl Iterator<Item = InterfaceDeclarationSyntax> + use<'_> {
        self.0
            .children()
            .filter_map(InterfaceDeclarationSyntax::cast)
    }
    pub fn semi_colon_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::SemiColon)
    }
}
#[derive(Debug, Clone)]
pub enum InterfaceObjectClassSyntax {
    Constant(SyntaxToken),
    Signal(SyntaxToken),
    Variable(SyntaxToken),
}
impl InterfaceObjectClassSyntax {
    pub fn cast(token: SyntaxToken) -> Option<Self> {
        match token.kind() {
            TokenKind::Keyword(Kw::Constant) => Some(InterfaceObjectClassSyntax::Constant(token)),
            TokenKind::Keyword(Kw::Signal) => Some(InterfaceObjectClassSyntax::Signal(token)),
            TokenKind::Keyword(Kw::Variable) => Some(InterfaceObjectClassSyntax::Variable(token)),
            _ => None,
        }
    }
    pub fn raw(&self) -> SyntaxToken {
        match self {
            InterfaceObjectClassSyntax::Constant(token) => token.clone(),
            InterfaceObjectClassSyntax::Signal(token) => token.clone(),
            InterfaceObjectClassSyntax::Variable(token) => token.clone(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct InterfaceObjectDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfaceObjectDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::InterfaceObjectDeclaration,
        items: &[
            LayoutItem {
                optional: true,
                repeated: false,
                name: "interface_object_class",
                kind: LayoutItemKind::TokenChoice(&[
                    TokenKind::Keyword(Kw::Constant),
                    TokenKind::Keyword(Kw::Signal),
                    TokenKind::Keyword(Kw::Variable),
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "identifier_list",
                kind: LayoutItemKind::Node(NodeKind::IdentifierList),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "colon",
                kind: LayoutItemKind::Token(TokenKind::Colon),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "mode",
                kind: LayoutItemKind::TokenChoice(&[
                    TokenKind::Keyword(Kw::In),
                    TokenKind::Keyword(Kw::Out),
                    TokenKind::Keyword(Kw::Inout),
                    TokenKind::Keyword(Kw::Buffer),
                    TokenKind::Keyword(Kw::Linkage),
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "subtype_indication",
                kind: LayoutItemKind::Node(NodeKind::SubtypeIndication),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "bus",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Bus)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "initial_value",
                kind: LayoutItemKind::Node(NodeKind::InitialValue),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        InterfaceObjectDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfaceObjectDeclarationSyntax {
    pub fn interface_object_class(&self) -> Option<InterfaceObjectClassSyntax> {
        self.0
            .tokens()
            .filter_map(InterfaceObjectClassSyntax::cast)
            .nth(0)
    }
    pub fn identifier_list(&self) -> Option<IdentifierListSyntax> {
        self.0
            .children()
            .filter_map(IdentifierListSyntax::cast)
            .nth(0)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Colon)
            .nth(0)
    }
    pub fn mode(&self) -> Option<ModeSyntax> {
        self.0.tokens().filter_map(ModeSyntax::cast).nth(0)
    }
    pub fn subtype_indication(&self) -> Option<SubtypeIndicationSyntax> {
        self.0
            .children()
            .filter_map(SubtypeIndicationSyntax::cast)
            .nth(0)
    }
    pub fn bus_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Bus))
            .nth(0)
    }
    pub fn initial_value(&self) -> Option<InitialValueSyntax> {
        self.0
            .children()
            .filter_map(InitialValueSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct InterfacePackageDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfacePackageDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::InterfacePackageDeclaration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "interface_package_declaration_preamble",
                kind: LayoutItemKind::Node(NodeKind::InterfacePackageDeclarationPreamble),
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
                optional: false,
                repeated: false,
                name: "interface_package_generic_map_aspect",
                kind: LayoutItemKind::Node(NodeKind::InterfacePackageGenericMapAspect),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        InterfacePackageDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfacePackageDeclarationSyntax {
    pub fn interface_package_declaration_preamble(
        &self,
    ) -> Option<InterfacePackageDeclarationPreambleSyntax> {
        self.0
            .children()
            .filter_map(InterfacePackageDeclarationPreambleSyntax::cast)
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
    pub fn interface_package_generic_map_aspect(
        &self,
    ) -> Option<InterfacePackageGenericMapAspectSyntax> {
        self.0
            .children()
            .filter_map(InterfacePackageGenericMapAspectSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct InterfacePackageDeclarationPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfacePackageDeclarationPreambleSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::InterfacePackageDeclarationPreamble,
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
                name: "identifier",
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
        InterfacePackageDeclarationPreambleSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfacePackageDeclarationPreambleSyntax {
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
    pub fn is_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Is))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct InterfacePackageGenericMapAspectSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfacePackageGenericMapAspectSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::InterfacePackageGenericMapAspect,
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
                name: "map",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Map)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "left_par",
                kind: LayoutItemKind::Token(TokenKind::LeftPar),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "interface_package_generic_map_aspect_inner",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::InterfacePackageGenericMapAspectBox,
                    NodeKind::InterfacePackageGenericMapAspectDefault,
                    NodeKind::InterfacePackageGenericMapAspectAssociations,
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
        InterfacePackageGenericMapAspectSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfacePackageGenericMapAspectSyntax {
    pub fn generic_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Generic))
            .nth(0)
    }
    pub fn map_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Map))
            .nth(0)
    }
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::LeftPar)
            .nth(0)
    }
    pub fn interface_package_generic_map_aspect_inner(
        &self,
    ) -> Option<InterfacePackageGenericMapAspectInnerSyntax> {
        self.0
            .children()
            .filter_map(InterfacePackageGenericMapAspectInnerSyntax::cast)
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
pub struct InterfacePackageGenericMapAspectAssociationsSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfacePackageGenericMapAspectAssociationsSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::InterfacePackageGenericMapAspectAssociations,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "association_list",
            kind: LayoutItemKind::Node(NodeKind::AssociationList),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        InterfacePackageGenericMapAspectAssociationsSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfacePackageGenericMapAspectAssociationsSyntax {
    pub fn association_list(&self) -> Option<AssociationListSyntax> {
        self.0
            .children()
            .filter_map(AssociationListSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct InterfacePackageGenericMapAspectBoxSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfacePackageGenericMapAspectBoxSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::InterfacePackageGenericMapAspectBox,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "box",
            kind: LayoutItemKind::Token(TokenKind::BOX),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        InterfacePackageGenericMapAspectBoxSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfacePackageGenericMapAspectBoxSyntax {
    pub fn box_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::BOX)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct InterfacePackageGenericMapAspectDefaultSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfacePackageGenericMapAspectDefaultSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::InterfacePackageGenericMapAspectDefault,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "default",
            kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Default)),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        InterfacePackageGenericMapAspectDefaultSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfacePackageGenericMapAspectDefaultSyntax {
    pub fn default_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Default))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum InterfacePackageGenericMapAspectInnerSyntax {
    InterfacePackageGenericMapAspectBox(InterfacePackageGenericMapAspectBoxSyntax),
    InterfacePackageGenericMapAspectDefault(InterfacePackageGenericMapAspectDefaultSyntax),
    InterfacePackageGenericMapAspectAssociations(
        InterfacePackageGenericMapAspectAssociationsSyntax,
    ),
}
impl AstNode for InterfacePackageGenericMapAspectInnerSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::InterfacePackageGenericMapAspectBox,
            NodeKind::InterfacePackageGenericMapAspectDefault,
            NodeKind::InterfacePackageGenericMapAspectAssociations,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if InterfacePackageGenericMapAspectBoxSyntax::can_cast(&node) {
            return InterfacePackageGenericMapAspectInnerSyntax :: InterfacePackageGenericMapAspectBox (InterfacePackageGenericMapAspectBoxSyntax :: cast_unchecked (node)) ;
        }
        if InterfacePackageGenericMapAspectDefaultSyntax::can_cast(&node) {
            return InterfacePackageGenericMapAspectInnerSyntax :: InterfacePackageGenericMapAspectDefault (InterfacePackageGenericMapAspectDefaultSyntax :: cast_unchecked (node)) ;
        }
        if InterfacePackageGenericMapAspectAssociationsSyntax::can_cast(&node) {
            return InterfacePackageGenericMapAspectInnerSyntax :: InterfacePackageGenericMapAspectAssociations (InterfacePackageGenericMapAspectAssociationsSyntax :: cast_unchecked (node)) ;
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
    }
    fn raw(&self) -> SyntaxNode {
        match self { InterfacePackageGenericMapAspectInnerSyntax :: InterfacePackageGenericMapAspectBox (inner) => inner . raw () , InterfacePackageGenericMapAspectInnerSyntax :: InterfacePackageGenericMapAspectDefault (inner) => inner . raw () , InterfacePackageGenericMapAspectInnerSyntax :: InterfacePackageGenericMapAspectAssociations (inner) => inner . raw () , }
    }
}
#[derive(Debug, Clone)]
pub struct InterfaceProcedureSpecificationSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfaceProcedureSpecificationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::InterfaceProcedureSpecification,
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
                name: "parameter_list",
                kind: LayoutItemKind::Node(NodeKind::ParameterList),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        InterfaceProcedureSpecificationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfaceProcedureSpecificationSyntax {
    pub fn procedure_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Procedure))
            .nth(0)
    }
    pub fn designator(&self) -> Option<DesignatorSyntax> {
        self.0.tokens().filter_map(DesignatorSyntax::cast).nth(0)
    }
    pub fn parameter_list(&self) -> Option<ParameterListSyntax> {
        self.0
            .children()
            .filter_map(ParameterListSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct InterfaceSubprogramDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfaceSubprogramDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::InterfaceSubprogramDeclaration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "interface_subprogram_specification",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::InterfaceProcedureSpecification,
                    NodeKind::InterfaceFunctionSpecification,
                ]),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "subprogram_default",
                kind: LayoutItemKind::Node(NodeKind::SubprogramDefault),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        InterfaceSubprogramDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfaceSubprogramDeclarationSyntax {
    pub fn interface_subprogram_specification(
        &self,
    ) -> Option<InterfaceSubprogramSpecificationSyntax> {
        self.0
            .children()
            .filter_map(InterfaceSubprogramSpecificationSyntax::cast)
            .nth(0)
    }
    pub fn subprogram_default(&self) -> Option<SubprogramDefaultSyntax> {
        self.0
            .children()
            .filter_map(SubprogramDefaultSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum InterfaceSubprogramDefaultSyntax {
    InterfaceSubprogramDefaultName(InterfaceSubprogramDefaultNameSyntax),
    InterfaceSubprogramDefaultBox(InterfaceSubprogramDefaultBoxSyntax),
}
impl AstNode for InterfaceSubprogramDefaultSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::InterfaceSubprogramDefaultName,
            NodeKind::InterfaceSubprogramDefaultBox,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if InterfaceSubprogramDefaultNameSyntax::can_cast(&node) {
            return InterfaceSubprogramDefaultSyntax::InterfaceSubprogramDefaultName(
                InterfaceSubprogramDefaultNameSyntax::cast_unchecked(node),
            );
        }
        if InterfaceSubprogramDefaultBoxSyntax::can_cast(&node) {
            return InterfaceSubprogramDefaultSyntax::InterfaceSubprogramDefaultBox(
                InterfaceSubprogramDefaultBoxSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            InterfaceSubprogramDefaultSyntax::InterfaceSubprogramDefaultName(inner) => inner.raw(),
            InterfaceSubprogramDefaultSyntax::InterfaceSubprogramDefaultBox(inner) => inner.raw(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct InterfaceSubprogramDefaultBoxSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfaceSubprogramDefaultBoxSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::InterfaceSubprogramDefaultBox,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "box",
            kind: LayoutItemKind::Token(TokenKind::BOX),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        InterfaceSubprogramDefaultBoxSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfaceSubprogramDefaultBoxSyntax {
    pub fn box_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::BOX)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct InterfaceSubprogramDefaultNameSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfaceSubprogramDefaultNameSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::InterfaceSubprogramDefaultName,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "name",
            kind: LayoutItemKind::Node(NodeKind::Name),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        InterfaceSubprogramDefaultNameSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfaceSubprogramDefaultNameSyntax {
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum InterfaceSubprogramSpecificationSyntax {
    InterfaceProcedureSpecification(InterfaceProcedureSpecificationSyntax),
    InterfaceFunctionSpecification(InterfaceFunctionSpecificationSyntax),
}
impl AstNode for InterfaceSubprogramSpecificationSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::InterfaceProcedureSpecification,
            NodeKind::InterfaceFunctionSpecification,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if InterfaceProcedureSpecificationSyntax::can_cast(&node) {
            return InterfaceSubprogramSpecificationSyntax::InterfaceProcedureSpecification(
                InterfaceProcedureSpecificationSyntax::cast_unchecked(node),
            );
        }
        if InterfaceFunctionSpecificationSyntax::can_cast(&node) {
            return InterfaceSubprogramSpecificationSyntax::InterfaceFunctionSpecification(
                InterfaceFunctionSpecificationSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            InterfaceSubprogramSpecificationSyntax::InterfaceProcedureSpecification(inner) => {
                inner.raw()
            }
            InterfaceSubprogramSpecificationSyntax::InterfaceFunctionSpecification(inner) => {
                inner.raw()
            }
        }
    }
}
#[derive(Debug, Clone)]
pub enum IterationSchemeSyntax {
    WhileScheme(WhileSchemeSyntax),
    ForScheme(ForSchemeSyntax),
}
impl AstNode for IterationSchemeSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[NodeKind::WhileScheme, NodeKind::ForScheme],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if WhileSchemeSyntax::can_cast(&node) {
            return IterationSchemeSyntax::WhileScheme(WhileSchemeSyntax::cast_unchecked(node));
        }
        if ForSchemeSyntax::can_cast(&node) {
            return IterationSchemeSyntax::ForScheme(ForSchemeSyntax::cast_unchecked(node));
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            IterationSchemeSyntax::WhileScheme(inner) => inner.raw(),
            IterationSchemeSyntax::ForScheme(inner) => inner.raw(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct LabelSyntax(pub(crate) SyntaxNode);
impl AstNode for LabelSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::Label,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "identifier",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "colon",
                kind: LayoutItemKind::Token(TokenKind::Colon),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        LabelSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl LabelSyntax {
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
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
}
#[derive(Debug, Clone)]
pub struct LibraryClauseSyntax(pub(crate) SyntaxNode);
impl AstNode for LibraryClauseSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::LibraryClause,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "library",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Library)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "identifier_list",
                kind: LayoutItemKind::Node(NodeKind::IdentifierList),
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
        LibraryClauseSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl LibraryClauseSyntax {
    pub fn library_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Library))
            .nth(0)
    }
    pub fn identifier_list(&self) -> Option<IdentifierListSyntax> {
        self.0
            .children()
            .filter_map(IdentifierListSyntax::cast)
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
pub enum LibraryUnitSyntax {
    PrimaryUnit(PrimaryUnitSyntax),
    SecondaryUnit(SecondaryUnitSyntax),
}
impl AstNode for LibraryUnitSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::EntityDeclaration,
            NodeKind::ConfigurationDeclaration,
            NodeKind::PrimaryUnitPackageDeclaration,
            NodeKind::PackageInstantiationDeclarationPrimaryUnit,
            NodeKind::ContextDeclaration,
            NodeKind::ArchitectureBody,
            NodeKind::SecondaryUnitPackageBody,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if PrimaryUnitSyntax::can_cast(&node) {
            return LibraryUnitSyntax::PrimaryUnit(PrimaryUnitSyntax::cast_unchecked(node));
        }
        if SecondaryUnitSyntax::can_cast(&node) {
            return LibraryUnitSyntax::SecondaryUnit(SecondaryUnitSyntax::cast_unchecked(node));
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            LibraryUnitSyntax::PrimaryUnit(inner) => inner.raw(),
            LibraryUnitSyntax::SecondaryUnit(inner) => inner.raw(),
        }
    }
}
#[derive(Debug, Clone)]
pub enum LiteralSyntax {
    BitStringLiteral(SyntaxToken),
    CharacterLiteral(SyntaxToken),
    StringLiteral(SyntaxToken),
    AbstractLiteral(SyntaxToken),
    Null(SyntaxToken),
}
impl LiteralSyntax {
    pub fn cast(token: SyntaxToken) -> Option<Self> {
        match token.kind() {
            TokenKind::BitStringLiteral => Some(LiteralSyntax::BitStringLiteral(token)),
            TokenKind::CharacterLiteral => Some(LiteralSyntax::CharacterLiteral(token)),
            TokenKind::StringLiteral => Some(LiteralSyntax::StringLiteral(token)),
            TokenKind::AbstractLiteral => Some(LiteralSyntax::AbstractLiteral(token)),
            TokenKind::Keyword(Kw::Null) => Some(LiteralSyntax::Null(token)),
            _ => None,
        }
    }
    pub fn raw(&self) -> SyntaxToken {
        match self {
            LiteralSyntax::BitStringLiteral(token) => token.clone(),
            LiteralSyntax::CharacterLiteral(token) => token.clone(),
            LiteralSyntax::StringLiteral(token) => token.clone(),
            LiteralSyntax::AbstractLiteral(token) => token.clone(),
            LiteralSyntax::Null(token) => token.clone(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct LiteralExpressionSyntax(pub(crate) SyntaxNode);
impl AstNode for LiteralExpressionSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::LiteralExpression,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "literal",
            kind: LayoutItemKind::TokenChoice(&[
                TokenKind::BitStringLiteral,
                TokenKind::CharacterLiteral,
                TokenKind::StringLiteral,
                TokenKind::AbstractLiteral,
                TokenKind::Keyword(Kw::Null),
            ]),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        LiteralExpressionSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl LiteralExpressionSyntax {
    pub fn literal(&self) -> Option<LiteralSyntax> {
        self.0.tokens().filter_map(LiteralSyntax::cast).nth(0)
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
pub struct LoopStatementEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for LoopStatementEpilogueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::LoopStatementEpilogue,
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
                name: "loop",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Loop)),
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
pub struct LoopStatementPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for LoopStatementPreambleSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::LoopStatementPreamble,
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
                name: "iteration_scheme",
                kind: LayoutItemKind::NodeChoice(&[NodeKind::WhileScheme, NodeKind::ForScheme]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "loop",
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
pub enum ModeSyntax {
    In(SyntaxToken),
    Out(SyntaxToken),
    Inout(SyntaxToken),
    Buffer(SyntaxToken),
    Linkage(SyntaxToken),
}
impl ModeSyntax {
    pub fn cast(token: SyntaxToken) -> Option<Self> {
        match token.kind() {
            TokenKind::Keyword(Kw::In) => Some(ModeSyntax::In(token)),
            TokenKind::Keyword(Kw::Out) => Some(ModeSyntax::Out(token)),
            TokenKind::Keyword(Kw::Inout) => Some(ModeSyntax::Inout(token)),
            TokenKind::Keyword(Kw::Buffer) => Some(ModeSyntax::Buffer(token)),
            TokenKind::Keyword(Kw::Linkage) => Some(ModeSyntax::Linkage(token)),
            _ => None,
        }
    }
    pub fn raw(&self) -> SyntaxToken {
        match self {
            ModeSyntax::In(token) => token.clone(),
            ModeSyntax::Out(token) => token.clone(),
            ModeSyntax::Inout(token) => token.clone(),
            ModeSyntax::Buffer(token) => token.clone(),
            ModeSyntax::Linkage(token) => token.clone(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct NameSyntax(pub(crate) SyntaxNode);
impl AstNode for NameSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::Name,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "name_prefix",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::ExternalConstantName,
                    NodeKind::ExternalSignalName,
                    NodeKind::ExternalVariableName,
                    NodeKind::NameDesignatorPrefix,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: true,
                name: "name_tails",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::SelectedName,
                    NodeKind::ParenthesizedName,
                    NodeKind::AttributeName,
                ]),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "range_constraint",
                kind: LayoutItemKind::Node(NodeKind::RangeConstraint),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        NameSyntax(node)
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
    pub fn range_constraint(&self) -> Option<RangeConstraintSyntax> {
        self.0
            .children()
            .filter_map(RangeConstraintSyntax::cast)
            .nth(0)
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
            TokenKind::Identifier => Some(NameDesignatorSyntax::Identifier(token)),
            TokenKind::StringLiteral => Some(NameDesignatorSyntax::StringLiteral(token)),
            TokenKind::CharacterLiteral => Some(NameDesignatorSyntax::CharacterLiteral(token)),
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
pub struct NameDesignatorPrefixSyntax(pub(crate) SyntaxNode);
impl AstNode for NameDesignatorPrefixSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::NameDesignatorPrefix,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "name_designator",
            kind: LayoutItemKind::TokenChoice(&[
                TokenKind::Identifier,
                TokenKind::StringLiteral,
                TokenKind::CharacterLiteral,
            ]),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        NameDesignatorPrefixSyntax(node)
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
pub struct NameExpressionSyntax(pub(crate) SyntaxNode);
impl AstNode for NameExpressionSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::NameExpression,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "name",
            kind: LayoutItemKind::Node(NodeKind::Name),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        NameExpressionSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl NameExpressionSyntax {
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct NameListSyntax(pub(crate) SyntaxNode);
impl AstNode for NameListSyntax {
    const META: &'static Layout = &Layout::List(List {
        kind: NodeKind::NameList,
        element: &LayoutItem {
            optional: false,
            repeated: true,
            name: "names",
            kind: LayoutItemKind::Node(NodeKind::Name),
        },
        separator: &LayoutItem {
            optional: false,
            repeated: true,
            name: "comma",
            kind: LayoutItemKind::Token(TokenKind::Comma),
        },
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        NameListSyntax(node)
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
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Comma)
    }
}
#[derive(Debug, Clone)]
pub enum NamePrefixSyntax {
    ExternalName(ExternalNameSyntax),
    NameDesignatorPrefix(NameDesignatorPrefixSyntax),
}
impl AstNode for NamePrefixSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::ExternalConstantName,
            NodeKind::ExternalSignalName,
            NodeKind::ExternalVariableName,
            NodeKind::NameDesignatorPrefix,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if ExternalNameSyntax::can_cast(&node) {
            return NamePrefixSyntax::ExternalName(ExternalNameSyntax::cast_unchecked(node));
        }
        if NameDesignatorPrefixSyntax::can_cast(&node) {
            return NamePrefixSyntax::NameDesignatorPrefix(
                NameDesignatorPrefixSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            NamePrefixSyntax::ExternalName(inner) => inner.raw(),
            NamePrefixSyntax::NameDesignatorPrefix(inner) => inner.raw(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct NameResolutionIndicationSyntax(pub(crate) SyntaxNode);
impl AstNode for NameResolutionIndicationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::NameResolutionIndication,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "name",
            kind: LayoutItemKind::Node(NodeKind::Name),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        NameResolutionIndicationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl NameResolutionIndicationSyntax {
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum NameTailSyntax {
    SelectedName(SelectedNameSyntax),
    ParenthesizedName(ParenthesizedNameSyntax),
    AttributeName(AttributeNameSyntax),
}
impl AstNode for NameTailSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::SelectedName,
            NodeKind::ParenthesizedName,
            NodeKind::AttributeName,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if SelectedNameSyntax::can_cast(&node) {
            return NameTailSyntax::SelectedName(SelectedNameSyntax::cast_unchecked(node));
        }
        if ParenthesizedNameSyntax::can_cast(&node) {
            return NameTailSyntax::ParenthesizedName(ParenthesizedNameSyntax::cast_unchecked(
                node,
            ));
        }
        if AttributeNameSyntax::can_cast(&node) {
            return NameTailSyntax::AttributeName(AttributeNameSyntax::cast_unchecked(node));
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            NameTailSyntax::SelectedName(inner) => inner.raw(),
            NameTailSyntax::ParenthesizedName(inner) => inner.raw(),
            NameTailSyntax::AttributeName(inner) => inner.raw(),
        }
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
pub struct NextStatementSyntax(pub(crate) SyntaxNode);
impl AstNode for NextStatementSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::NextStatement,
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
                name: "next",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Next)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "loop_label",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "when_clause",
                kind: LayoutItemKind::Node(NodeKind::WhenClause),
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
    pub fn when_clause(&self) -> Option<WhenClauseSyntax> {
        self.0.children().filter_map(WhenClauseSyntax::cast).nth(0)
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
                optional: true,
                repeated: false,
                name: "label",
                kind: LayoutItemKind::Node(NodeKind::Label),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "null",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Null)),
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
pub struct NumericTypeDefinitionSyntax(pub(crate) SyntaxNode);
impl AstNode for NumericTypeDefinitionSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::NumericTypeDefinition,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "range_constraint",
            kind: LayoutItemKind::Node(NodeKind::RangeConstraint),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        NumericTypeDefinitionSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl NumericTypeDefinitionSyntax {
    pub fn range_constraint(&self) -> Option<RangeConstraintSyntax> {
        self.0
            .children()
            .filter_map(RangeConstraintSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct OthersChoiceSyntax(pub(crate) SyntaxNode);
impl AstNode for OthersChoiceSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::OthersChoice,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "others",
            kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Others)),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        OthersChoiceSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl OthersChoiceSyntax {
    pub fn others_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Others))
            .nth(0)
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
pub struct PackageBodyDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for PackageBodyDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PackageBodyDeclaration,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "package_body",
            kind: LayoutItemKind::Node(NodeKind::PackageBody),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        PackageBodyDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PackageBodyDeclarationSyntax {
    pub fn package_body(&self) -> Option<PackageBodySyntax> {
        self.0.children().filter_map(PackageBodySyntax::cast).nth(0)
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
pub struct PackageDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for PackageDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PackageDeclaration,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "package",
            kind: LayoutItemKind::Node(NodeKind::Package),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        PackageDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PackageDeclarationSyntax {
    pub fn package(&self) -> Option<PackageSyntax> {
        self.0.children().filter_map(PackageSyntax::cast).nth(0)
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
                optional: false,
                repeated: false,
                name: "generic_clause",
                kind: LayoutItemKind::Node(NodeKind::GenericClause),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "generic_map",
                kind: LayoutItemKind::Node(NodeKind::GenericMap),
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
    pub fn generic_map(&self) -> Option<GenericMapSyntax> {
        self.0.children().filter_map(GenericMapSyntax::cast).nth(0)
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
pub struct PackageInstantiationDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for PackageInstantiationDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PackageInstantiationDeclaration,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "package_instantiation",
            kind: LayoutItemKind::Node(NodeKind::PackageInstantiation),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        PackageInstantiationDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PackageInstantiationDeclarationSyntax {
    pub fn package_instantiation(&self) -> Option<PackageInstantiationSyntax> {
        self.0
            .children()
            .filter_map(PackageInstantiationSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct PackageInstantiationDeclarationPrimaryUnitSyntax(pub(crate) SyntaxNode);
impl AstNode for PackageInstantiationDeclarationPrimaryUnitSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PackageInstantiationDeclarationPrimaryUnit,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "package_instantiation",
            kind: LayoutItemKind::Node(NodeKind::PackageInstantiation),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        PackageInstantiationDeclarationPrimaryUnitSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PackageInstantiationDeclarationPrimaryUnitSyntax {
    pub fn package_instantiation(&self) -> Option<PackageInstantiationSyntax> {
        self.0
            .children()
            .filter_map(PackageInstantiationSyntax::cast)
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
pub struct PackagePathSyntax(pub(crate) SyntaxNode);
impl AstNode for PackagePathSyntax {
    const META: &'static Layout = &Layout::List(List {
        kind: NodeKind::PackagePath,
        element: &LayoutItem {
            optional: false,
            repeated: true,
            name: "identifier",
            kind: LayoutItemKind::Token(TokenKind::Identifier),
        },
        separator: &LayoutItem {
            optional: false,
            repeated: true,
            name: "dot",
            kind: LayoutItemKind::Token(TokenKind::Dot),
        },
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        PackagePathSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PackagePathSyntax {
    pub fn identifier_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Identifier)
    }
    pub fn dot_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Dot)
    }
}
#[derive(Debug, Clone)]
pub struct PackagePathnameSyntax(pub(crate) SyntaxNode);
impl AstNode for PackagePathnameSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PackagePathname,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "comm_at",
                kind: LayoutItemKind::Token(TokenKind::CommAt),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "package_path",
                kind: LayoutItemKind::Node(NodeKind::PackagePath),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        PackagePathnameSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PackagePathnameSyntax {
    pub fn comm_at_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::CommAt)
            .nth(0)
    }
    pub fn package_path(&self) -> Option<PackagePathSyntax> {
        self.0.children().filter_map(PackagePathSyntax::cast).nth(0)
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
                optional: false,
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
pub struct ParameterSpecificationSyntax(pub(crate) SyntaxNode);
impl AstNode for ParameterSpecificationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ParameterSpecification,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "identifier",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "in",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::In)),
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
                    NodeKind::Allocator,
                    NodeKind::NameExpression,
                    NodeKind::QualifiedExpression,
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
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ParenthesizedElementResolutionSyntax(pub(crate) SyntaxNode);
impl AstNode for ParenthesizedElementResolutionSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ParenthesizedElementResolution,
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
                name: "element_resolution_resolution_indication",
                kind: LayoutItemKind::Node(NodeKind::ElementResolutionResolutionIndication),
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
        ParenthesizedElementResolutionSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ParenthesizedElementResolutionSyntax {
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::LeftPar)
            .nth(0)
    }
    pub fn element_resolution_resolution_indication(
        &self,
    ) -> Option<ElementResolutionResolutionIndicationSyntax> {
        self.0
            .children()
            .filter_map(ElementResolutionResolutionIndicationSyntax::cast)
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
                    NodeKind::Allocator,
                    NodeKind::NameExpression,
                    NodeKind::QualifiedExpression,
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
pub struct ParenthesizedExpressionOrAggregateSyntax(pub(crate) SyntaxNode);
impl AstNode for ParenthesizedExpressionOrAggregateSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ParenthesizedExpressionOrAggregate,
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
                name: "element_association_list",
                kind: LayoutItemKind::Node(NodeKind::ElementAssociationList),
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
        ParenthesizedExpressionOrAggregateSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ParenthesizedExpressionOrAggregateSyntax {
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::LeftPar)
            .nth(0)
    }
    pub fn element_association_list(&self) -> Option<ElementAssociationListSyntax> {
        self.0
            .children()
            .filter_map(ElementAssociationListSyntax::cast)
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
                optional: false,
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
pub struct ParenthesizedNameSyntax(pub(crate) SyntaxNode);
impl AstNode for ParenthesizedNameSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ParenthesizedName,
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
                name: "association_list",
                kind: LayoutItemKind::Node(NodeKind::AssociationList),
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
        ParenthesizedNameSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ParenthesizedNameSyntax {
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::LeftPar)
            .nth(0)
    }
    pub fn association_list(&self) -> Option<AssociationListSyntax> {
        self.0
            .children()
            .filter_map(AssociationListSyntax::cast)
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
pub struct PartialPathnameSyntax(pub(crate) SyntaxNode);
impl AstNode for PartialPathnameSyntax {
    const META: &'static Layout = &Layout::List(List {
        kind: NodeKind::PartialPathname,
        element: &LayoutItem {
            optional: false,
            repeated: true,
            name: "pathname_elements",
            kind: LayoutItemKind::Node(NodeKind::PathnameElement),
        },
        separator: &LayoutItem {
            optional: false,
            repeated: true,
            name: "dot",
            kind: LayoutItemKind::Token(TokenKind::Dot),
        },
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        PartialPathnameSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PartialPathnameSyntax {
    pub fn pathname_elements(&self) -> impl Iterator<Item = PathnameElementSyntax> + use<'_> {
        self.0.children().filter_map(PathnameElementSyntax::cast)
    }
    pub fn dot_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Dot)
    }
}
#[derive(Debug, Clone)]
pub struct PathnameElementSyntax(pub(crate) SyntaxNode);
impl AstNode for PathnameElementSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PathnameElement,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "identifier",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "parenthesized_expression",
                kind: LayoutItemKind::Node(NodeKind::ParenthesizedExpression),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        PathnameElementSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PathnameElementSyntax {
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Identifier)
            .nth(0)
    }
    pub fn parenthesized_expression(&self) -> Option<ParenthesizedExpressionSyntax> {
        self.0
            .children()
            .filter_map(ParenthesizedExpressionSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct PhysicalLiteralSyntax(pub(crate) SyntaxNode);
impl AstNode for PhysicalLiteralSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PhysicalLiteral,
        items: &[
            LayoutItem {
                optional: true,
                repeated: false,
                name: "abstract_literal",
                kind: LayoutItemKind::Token(TokenKind::AbstractLiteral),
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
        PhysicalLiteralSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PhysicalLiteralSyntax {
    pub fn abstract_literal_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::AbstractLiteral)
            .nth(0)
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct PhysicalLiteralExpressionSyntax(pub(crate) SyntaxNode);
impl AstNode for PhysicalLiteralExpressionSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PhysicalLiteralExpression,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "physical_literal",
            kind: LayoutItemKind::Node(NodeKind::PhysicalLiteral),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        PhysicalLiteralExpressionSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PhysicalLiteralExpressionSyntax {
    pub fn physical_literal(&self) -> Option<PhysicalLiteralSyntax> {
        self.0
            .children()
            .filter_map(PhysicalLiteralSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct PhysicalTypeDefinitionSyntax(pub(crate) SyntaxNode);
impl AstNode for PhysicalTypeDefinitionSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PhysicalTypeDefinition,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "range_constraint",
                kind: LayoutItemKind::Node(NodeKind::RangeConstraint),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "unit_declarations",
                kind: LayoutItemKind::Node(NodeKind::UnitDeclarations),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "physical_type_definition_epilogue",
                kind: LayoutItemKind::Node(NodeKind::PhysicalTypeDefinitionEpilogue),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        PhysicalTypeDefinitionSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PhysicalTypeDefinitionSyntax {
    pub fn range_constraint(&self) -> Option<RangeConstraintSyntax> {
        self.0
            .children()
            .filter_map(RangeConstraintSyntax::cast)
            .nth(0)
    }
    pub fn unit_declarations(&self) -> Option<UnitDeclarationsSyntax> {
        self.0
            .children()
            .filter_map(UnitDeclarationsSyntax::cast)
            .nth(0)
    }
    pub fn physical_type_definition_epilogue(
        &self,
    ) -> Option<PhysicalTypeDefinitionEpilogueSyntax> {
        self.0
            .children()
            .filter_map(PhysicalTypeDefinitionEpilogueSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct PhysicalTypeDefinitionEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for PhysicalTypeDefinitionEpilogueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PhysicalTypeDefinitionEpilogue,
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
                name: "units",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Units)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "name",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        PhysicalTypeDefinitionEpilogueSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PhysicalTypeDefinitionEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::End))
            .nth(0)
    }
    pub fn units_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Units))
            .nth(0)
    }
    pub fn name_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Identifier)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct PortClauseSyntax(pub(crate) SyntaxNode);
impl AstNode for PortClauseSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PortClause,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "port_clause_preamble",
                kind: LayoutItemKind::Node(NodeKind::PortClausePreamble),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "interface_list",
                kind: LayoutItemKind::Node(NodeKind::InterfaceList),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "port_clause_epilogue",
                kind: LayoutItemKind::Node(NodeKind::PortClauseEpilogue),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        PortClauseSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PortClauseSyntax {
    pub fn port_clause_preamble(&self) -> Option<PortClausePreambleSyntax> {
        self.0
            .children()
            .filter_map(PortClausePreambleSyntax::cast)
            .nth(0)
    }
    pub fn interface_list(&self) -> Option<InterfaceListSyntax> {
        self.0
            .children()
            .filter_map(InterfaceListSyntax::cast)
            .nth(0)
    }
    pub fn port_clause_epilogue(&self) -> Option<PortClauseEpilogueSyntax> {
        self.0
            .children()
            .filter_map(PortClauseEpilogueSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct PortClauseEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for PortClauseEpilogueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PortClauseEpilogue,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "right_par",
                kind: LayoutItemKind::Token(TokenKind::RightPar),
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
        PortClauseEpilogueSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PortClauseEpilogueSyntax {
    pub fn right_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::RightPar)
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
pub struct PortClausePreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for PortClausePreambleSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PortClausePreamble,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "port",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Port)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "left_par",
                kind: LayoutItemKind::Token(TokenKind::LeftPar),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        PortClausePreambleSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PortClausePreambleSyntax {
    pub fn port_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Port))
            .nth(0)
    }
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::LeftPar)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct PortMapSyntax(pub(crate) SyntaxNode);
impl AstNode for PortMapSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PortMap,
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
        PortMapSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PortMapSyntax {
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
pub struct PortMapAspectSyntax(pub(crate) SyntaxNode);
impl AstNode for PortMapAspectSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PortMapAspect,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "port",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Port)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "map",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Map)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "left_par",
                kind: LayoutItemKind::Token(TokenKind::LeftPar),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "association_list",
                kind: LayoutItemKind::Node(NodeKind::AssociationList),
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
        PortMapAspectSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PortMapAspectSyntax {
    pub fn port_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Port))
            .nth(0)
    }
    pub fn map_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Map))
            .nth(0)
    }
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::LeftPar)
            .nth(0)
    }
    pub fn association_list(&self) -> Option<AssociationListSyntax> {
        self.0
            .children()
            .filter_map(AssociationListSyntax::cast)
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
pub struct PortPartSyntax(pub(crate) SyntaxNode);
impl AstNode for PortPartSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PortPart,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "port_clause",
                kind: LayoutItemKind::Node(NodeKind::PortClause),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "port_map",
                kind: LayoutItemKind::Node(NodeKind::PortMap),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        PortPartSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PortPartSyntax {
    pub fn port_clause(&self) -> Option<PortClauseSyntax> {
        self.0.children().filter_map(PortClauseSyntax::cast).nth(0)
    }
    pub fn port_map(&self) -> Option<PortMapSyntax> {
        self.0.children().filter_map(PortMapSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum PrimaryUnitSyntax {
    EntityDeclaration(EntityDeclarationSyntax),
    ConfigurationDeclaration(ConfigurationDeclarationSyntax),
    PrimaryUnitPackageDeclaration(PrimaryUnitPackageDeclarationSyntax),
    PackageInstantiationDeclarationPrimaryUnit(PackageInstantiationDeclarationPrimaryUnitSyntax),
    ContextDeclaration(ContextDeclarationSyntax),
}
impl AstNode for PrimaryUnitSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::EntityDeclaration,
            NodeKind::ConfigurationDeclaration,
            NodeKind::PrimaryUnitPackageDeclaration,
            NodeKind::PackageInstantiationDeclarationPrimaryUnit,
            NodeKind::ContextDeclaration,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if EntityDeclarationSyntax::can_cast(&node) {
            return PrimaryUnitSyntax::EntityDeclaration(EntityDeclarationSyntax::cast_unchecked(
                node,
            ));
        }
        if ConfigurationDeclarationSyntax::can_cast(&node) {
            return PrimaryUnitSyntax::ConfigurationDeclaration(
                ConfigurationDeclarationSyntax::cast_unchecked(node),
            );
        }
        if PrimaryUnitPackageDeclarationSyntax::can_cast(&node) {
            return PrimaryUnitSyntax::PrimaryUnitPackageDeclaration(
                PrimaryUnitPackageDeclarationSyntax::cast_unchecked(node),
            );
        }
        if PackageInstantiationDeclarationPrimaryUnitSyntax::can_cast(&node) {
            return PrimaryUnitSyntax::PackageInstantiationDeclarationPrimaryUnit(
                PackageInstantiationDeclarationPrimaryUnitSyntax::cast_unchecked(node),
            );
        }
        if ContextDeclarationSyntax::can_cast(&node) {
            return PrimaryUnitSyntax::ContextDeclaration(
                ContextDeclarationSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            PrimaryUnitSyntax::EntityDeclaration(inner) => inner.raw(),
            PrimaryUnitSyntax::ConfigurationDeclaration(inner) => inner.raw(),
            PrimaryUnitSyntax::PrimaryUnitPackageDeclaration(inner) => inner.raw(),
            PrimaryUnitSyntax::PackageInstantiationDeclarationPrimaryUnit(inner) => inner.raw(),
            PrimaryUnitSyntax::ContextDeclaration(inner) => inner.raw(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct PrimaryUnitDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for PrimaryUnitDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PrimaryUnitDeclaration,
        items: &[
            LayoutItem {
                optional: false,
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
        PrimaryUnitDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PrimaryUnitDeclarationSyntax {
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
pub struct PrimaryUnitPackageDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for PrimaryUnitPackageDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PrimaryUnitPackageDeclaration,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "package",
            kind: LayoutItemKind::Node(NodeKind::Package),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        PrimaryUnitPackageDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PrimaryUnitPackageDeclarationSyntax {
    pub fn package(&self) -> Option<PackageSyntax> {
        self.0.children().filter_map(PackageSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ProcedureCallStatementSyntax(pub(crate) SyntaxNode);
impl AstNode for ProcedureCallStatementSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ProcedureCallStatement,
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
pub struct ProcessEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for ProcessEpilogueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ProcessEpilogue,
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
        ProcessEpilogueSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ProcessEpilogueSyntax {
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
#[derive(Debug, Clone)]
pub struct ProcessPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for ProcessPreambleSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ProcessPreamble,
        items: &[
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
        ProcessPreambleSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ProcessPreambleSyntax {
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
                optional: true,
                repeated: false,
                name: "label",
                kind: LayoutItemKind::Node(NodeKind::Label),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "process_preamble",
                kind: LayoutItemKind::Node(NodeKind::ProcessPreamble),
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
                name: "sequential_statements",
                kind: LayoutItemKind::Node(NodeKind::SequentialStatements),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "process_epilogue",
                kind: LayoutItemKind::Node(NodeKind::ProcessEpilogue),
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
    pub fn label(&self) -> Option<LabelSyntax> {
        self.0.children().filter_map(LabelSyntax::cast).nth(0)
    }
    pub fn process_preamble(&self) -> Option<ProcessPreambleSyntax> {
        self.0
            .children()
            .filter_map(ProcessPreambleSyntax::cast)
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
    pub fn sequential_statements(&self) -> Option<SequentialStatementsSyntax> {
        self.0
            .children()
            .filter_map(SequentialStatementsSyntax::cast)
            .nth(0)
    }
    pub fn process_epilogue(&self) -> Option<ProcessEpilogueSyntax> {
        self.0
            .children()
            .filter_map(ProcessEpilogueSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ProtectedPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for ProtectedPreambleSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ProtectedPreamble,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "protected",
            kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Protected)),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ProtectedPreambleSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ProtectedPreambleSyntax {
    pub fn protected_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Protected))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ProtectedTypeBodySyntax(pub(crate) SyntaxNode);
impl AstNode for ProtectedTypeBodySyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ProtectedTypeBody,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "protected_type_body_preamble",
                kind: LayoutItemKind::Node(NodeKind::ProtectedTypeBodyPreamble),
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
                name: "protected_type_body_epilogue",
                kind: LayoutItemKind::Node(NodeKind::ProtectedTypeBodyEpilogue),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ProtectedTypeBodySyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ProtectedTypeBodySyntax {
    pub fn protected_type_body_preamble(&self) -> Option<ProtectedTypeBodyPreambleSyntax> {
        self.0
            .children()
            .filter_map(ProtectedTypeBodyPreambleSyntax::cast)
            .nth(0)
    }
    pub fn declarations(&self) -> Option<DeclarationsSyntax> {
        self.0
            .children()
            .filter_map(DeclarationsSyntax::cast)
            .nth(0)
    }
    pub fn protected_type_body_epilogue(&self) -> Option<ProtectedTypeBodyEpilogueSyntax> {
        self.0
            .children()
            .filter_map(ProtectedTypeBodyEpilogueSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ProtectedTypeBodyEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for ProtectedTypeBodyEpilogueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ProtectedTypeBodyEpilogue,
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
                name: "protected",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Protected)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "body",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Body)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "name",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ProtectedTypeBodyEpilogueSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ProtectedTypeBodyEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::End))
            .nth(0)
    }
    pub fn protected_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Protected))
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
}
#[derive(Debug, Clone)]
pub struct ProtectedTypeBodyPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for ProtectedTypeBodyPreambleSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ProtectedTypeBodyPreamble,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "protected",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Protected)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "body",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Body)),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ProtectedTypeBodyPreambleSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ProtectedTypeBodyPreambleSyntax {
    pub fn protected_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Protected))
            .nth(0)
    }
    pub fn body_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Body))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ProtectedTypeDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for ProtectedTypeDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ProtectedTypeDeclaration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "protected_preamble",
                kind: LayoutItemKind::Node(NodeKind::ProtectedPreamble),
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
                name: "protected_type_declaration_epilogue",
                kind: LayoutItemKind::Node(NodeKind::ProtectedTypeDeclarationEpilogue),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ProtectedTypeDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ProtectedTypeDeclarationSyntax {
    pub fn protected_preamble(&self) -> Option<ProtectedPreambleSyntax> {
        self.0
            .children()
            .filter_map(ProtectedPreambleSyntax::cast)
            .nth(0)
    }
    pub fn declarations(&self) -> Option<DeclarationsSyntax> {
        self.0
            .children()
            .filter_map(DeclarationsSyntax::cast)
            .nth(0)
    }
    pub fn protected_type_declaration_epilogue(
        &self,
    ) -> Option<ProtectedTypeDeclarationEpilogueSyntax> {
        self.0
            .children()
            .filter_map(ProtectedTypeDeclarationEpilogueSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ProtectedTypeDeclarationEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for ProtectedTypeDeclarationEpilogueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ProtectedTypeDeclarationEpilogue,
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
                name: "protected",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Protected)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "name",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ProtectedTypeDeclarationEpilogueSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ProtectedTypeDeclarationEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::End))
            .nth(0)
    }
    pub fn protected_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Protected))
            .nth(0)
    }
    pub fn name_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Identifier)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum ProtectedTypeDefinitionSyntax {
    ProtectedTypeDeclaration(ProtectedTypeDeclarationSyntax),
    ProtectedTypeBody(ProtectedTypeBodySyntax),
}
impl AstNode for ProtectedTypeDefinitionSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::ProtectedTypeDeclaration,
            NodeKind::ProtectedTypeBody,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if ProtectedTypeDeclarationSyntax::can_cast(&node) {
            return ProtectedTypeDefinitionSyntax::ProtectedTypeDeclaration(
                ProtectedTypeDeclarationSyntax::cast_unchecked(node),
            );
        }
        if ProtectedTypeBodySyntax::can_cast(&node) {
            return ProtectedTypeDefinitionSyntax::ProtectedTypeBody(
                ProtectedTypeBodySyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            ProtectedTypeDefinitionSyntax::ProtectedTypeDeclaration(inner) => inner.raw(),
            ProtectedTypeDefinitionSyntax::ProtectedTypeBody(inner) => inner.raw(),
        }
    }
}
#[derive(Debug, Clone)]
pub enum PuritySyntax {
    Pure(SyntaxToken),
    Impure(SyntaxToken),
}
impl PuritySyntax {
    pub fn cast(token: SyntaxToken) -> Option<Self> {
        match token.kind() {
            TokenKind::Keyword(Kw::Pure) => Some(PuritySyntax::Pure(token)),
            TokenKind::Keyword(Kw::Impure) => Some(PuritySyntax::Impure(token)),
            _ => None,
        }
    }
    pub fn raw(&self) -> SyntaxToken {
        match self {
            PuritySyntax::Pure(token) => token.clone(),
            PuritySyntax::Impure(token) => token.clone(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct QualifiedExpressionSyntax(pub(crate) SyntaxNode);
impl AstNode for QualifiedExpressionSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::QualifiedExpression,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "name",
                kind: LayoutItemKind::Node(NodeKind::Name),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "tick",
                kind: LayoutItemKind::Token(TokenKind::Tick),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "parenthesized_expression_or_aggregate",
                kind: LayoutItemKind::Node(NodeKind::ParenthesizedExpressionOrAggregate),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        QualifiedExpressionSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl QualifiedExpressionSyntax {
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
    pub fn tick_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Tick)
            .nth(0)
    }
    pub fn parenthesized_expression_or_aggregate(
        &self,
    ) -> Option<ParenthesizedExpressionOrAggregateSyntax> {
        self.0
            .children()
            .filter_map(ParenthesizedExpressionOrAggregateSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct RangeConstraintSyntax(pub(crate) SyntaxNode);
impl AstNode for RangeConstraintSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::RangeConstraint,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "range",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Range)),
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
                    NodeKind::Allocator,
                    NodeKind::NameExpression,
                    NodeKind::QualifiedExpression,
                ]),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        RangeConstraintSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl RangeConstraintSyntax {
    pub fn range_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Range))
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct RecordElementDeclarationsSyntax(pub(crate) SyntaxNode);
impl AstNode for RecordElementDeclarationsSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::RecordElementDeclarations,
        items: &[LayoutItem {
            optional: false,
            repeated: true,
            name: "element_declarations",
            kind: LayoutItemKind::Node(NodeKind::ElementDeclaration),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        RecordElementDeclarationsSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl RecordElementDeclarationsSyntax {
    pub fn element_declarations(&self) -> impl Iterator<Item = ElementDeclarationSyntax> + use<'_> {
        self.0.children().filter_map(ElementDeclarationSyntax::cast)
    }
}
#[derive(Debug, Clone)]
pub struct RecordElementResolutionSyntax(pub(crate) SyntaxNode);
impl AstNode for RecordElementResolutionSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::RecordElementResolution,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "name",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "resolution_indication",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::NameResolutionIndication,
                    NodeKind::ParenthesizedElementResolution,
                ]),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        RecordElementResolutionSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl RecordElementResolutionSyntax {
    pub fn name_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Identifier)
            .nth(0)
    }
    pub fn resolution_indication(&self) -> Option<ResolutionIndicationSyntax> {
        self.0
            .children()
            .filter_map(ResolutionIndicationSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct RecordResolutionSyntax(pub(crate) SyntaxNode);
impl AstNode for RecordResolutionSyntax {
    const META: &'static Layout = &Layout::List(List {
        kind: NodeKind::RecordResolution,
        element: &LayoutItem {
            optional: false,
            repeated: true,
            name: "record_element_resolutions",
            kind: LayoutItemKind::Node(NodeKind::RecordElementResolution),
        },
        separator: &LayoutItem {
            optional: false,
            repeated: true,
            name: "comma",
            kind: LayoutItemKind::Token(TokenKind::Comma),
        },
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        RecordResolutionSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl RecordResolutionSyntax {
    pub fn record_element_resolutions(
        &self,
    ) -> impl Iterator<Item = RecordElementResolutionSyntax> + use<'_> {
        self.0
            .children()
            .filter_map(RecordElementResolutionSyntax::cast)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Comma)
    }
}
#[derive(Debug, Clone)]
pub struct RecordResolutionElementResolutionSyntax(pub(crate) SyntaxNode);
impl AstNode for RecordResolutionElementResolutionSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::RecordResolutionElementResolution,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "record_resolution",
            kind: LayoutItemKind::Node(NodeKind::RecordResolution),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        RecordResolutionElementResolutionSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl RecordResolutionElementResolutionSyntax {
    pub fn record_resolution(&self) -> Option<RecordResolutionSyntax> {
        self.0
            .children()
            .filter_map(RecordResolutionSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct RecordTypeDefinitionSyntax(pub(crate) SyntaxNode);
impl AstNode for RecordTypeDefinitionSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::RecordTypeDefinition,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "record_type_definition_preamble",
                kind: LayoutItemKind::Node(NodeKind::RecordTypeDefinitionPreamble),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "record_element_declarations",
                kind: LayoutItemKind::Node(NodeKind::RecordElementDeclarations),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "record_type_definition_epilogue",
                kind: LayoutItemKind::Node(NodeKind::RecordTypeDefinitionEpilogue),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        RecordTypeDefinitionSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl RecordTypeDefinitionSyntax {
    pub fn record_type_definition_preamble(&self) -> Option<RecordTypeDefinitionPreambleSyntax> {
        self.0
            .children()
            .filter_map(RecordTypeDefinitionPreambleSyntax::cast)
            .nth(0)
    }
    pub fn record_element_declarations(&self) -> Option<RecordElementDeclarationsSyntax> {
        self.0
            .children()
            .filter_map(RecordElementDeclarationsSyntax::cast)
            .nth(0)
    }
    pub fn record_type_definition_epilogue(&self) -> Option<RecordTypeDefinitionEpilogueSyntax> {
        self.0
            .children()
            .filter_map(RecordTypeDefinitionEpilogueSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct RecordTypeDefinitionEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for RecordTypeDefinitionEpilogueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::RecordTypeDefinitionEpilogue,
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
                name: "record",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Record)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "identifier",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        RecordTypeDefinitionEpilogueSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl RecordTypeDefinitionEpilogueSyntax {
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::End))
            .nth(0)
    }
    pub fn record_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Record))
            .nth(0)
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Identifier)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct RecordTypeDefinitionPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for RecordTypeDefinitionPreambleSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::RecordTypeDefinitionPreamble,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "record",
            kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Record)),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        RecordTypeDefinitionPreambleSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl RecordTypeDefinitionPreambleSyntax {
    pub fn record_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Record))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct RejectClauseSyntax(pub(crate) SyntaxNode);
impl AstNode for RejectClauseSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::RejectClause,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "reject",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Reject)),
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
                    NodeKind::Allocator,
                    NodeKind::NameExpression,
                    NodeKind::QualifiedExpression,
                ]),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        RejectClauseSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl RejectClauseSyntax {
    pub fn reject_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Reject))
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct RelativePathnameSyntax(pub(crate) SyntaxNode);
impl AstNode for RelativePathnameSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::RelativePathname,
        items: &[
            LayoutItem {
                optional: false,
                repeated: true,
                name: "up_levels",
                kind: LayoutItemKind::Node(NodeKind::UpLevel),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "partial_pathname",
                kind: LayoutItemKind::Node(NodeKind::PartialPathname),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        RelativePathnameSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl RelativePathnameSyntax {
    pub fn up_levels(&self) -> impl Iterator<Item = UpLevelSyntax> + use<'_> {
        self.0.children().filter_map(UpLevelSyntax::cast)
    }
    pub fn partial_pathname(&self) -> Option<PartialPathnameSyntax> {
        self.0
            .children()
            .filter_map(PartialPathnameSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ReportClauseSyntax(pub(crate) SyntaxNode);
impl AstNode for ReportClauseSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ReportClause,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "report",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Report)),
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
                    NodeKind::Allocator,
                    NodeKind::NameExpression,
                    NodeKind::QualifiedExpression,
                ]),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ReportClauseSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ReportClauseSyntax {
    pub fn report_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Report))
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ReportStatementSyntax(pub(crate) SyntaxNode);
impl AstNode for ReportStatementSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ReportStatement,
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
                name: "report",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Report)),
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
                    NodeKind::Allocator,
                    NodeKind::NameExpression,
                    NodeKind::QualifiedExpression,
                ]),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "severity_clause",
                kind: LayoutItemKind::Node(NodeKind::SeverityClause),
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
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn severity_clause(&self) -> Option<SeverityClauseSyntax> {
        self.0
            .children()
            .filter_map(SeverityClauseSyntax::cast)
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
pub enum ResolutionIndicationSyntax {
    NameResolutionIndication(NameResolutionIndicationSyntax),
    ParenthesizedElementResolution(ParenthesizedElementResolutionSyntax),
}
impl AstNode for ResolutionIndicationSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::NameResolutionIndication,
            NodeKind::ParenthesizedElementResolution,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if NameResolutionIndicationSyntax::can_cast(&node) {
            return ResolutionIndicationSyntax::NameResolutionIndication(
                NameResolutionIndicationSyntax::cast_unchecked(node),
            );
        }
        if ParenthesizedElementResolutionSyntax::can_cast(&node) {
            return ResolutionIndicationSyntax::ParenthesizedElementResolution(
                ParenthesizedElementResolutionSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            ResolutionIndicationSyntax::NameResolutionIndication(inner) => inner.raw(),
            ResolutionIndicationSyntax::ParenthesizedElementResolution(inner) => inner.raw(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct ResolutionIndicationElementResolutionSyntax(pub(crate) SyntaxNode);
impl AstNode for ResolutionIndicationElementResolutionSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ResolutionIndicationElementResolution,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "resolution_indication",
            kind: LayoutItemKind::NodeChoice(&[
                NodeKind::NameResolutionIndication,
                NodeKind::ParenthesizedElementResolution,
            ]),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ResolutionIndicationElementResolutionSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ResolutionIndicationElementResolutionSyntax {
    pub fn resolution_indication(&self) -> Option<ResolutionIndicationSyntax> {
        self.0
            .children()
            .filter_map(ResolutionIndicationSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ReturnStatementSyntax(pub(crate) SyntaxNode);
impl AstNode for ReturnStatementSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ReturnStatement,
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
                name: "return",
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
                    NodeKind::Allocator,
                    NodeKind::NameExpression,
                    NodeKind::QualifiedExpression,
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
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct ReturnTypeSyntax(pub(crate) SyntaxNode);
impl AstNode for ReturnTypeSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ReturnType,
        items: &[
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
        ReturnTypeSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ReturnTypeSyntax {
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
pub enum ScalarTypeDefinitionSyntax {
    EnumerationTypeDefinition(EnumerationTypeDefinitionSyntax),
    NumericTypeDefinition(NumericTypeDefinitionSyntax),
    PhysicalTypeDefinition(PhysicalTypeDefinitionSyntax),
}
impl AstNode for ScalarTypeDefinitionSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::EnumerationTypeDefinition,
            NodeKind::NumericTypeDefinition,
            NodeKind::PhysicalTypeDefinition,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if EnumerationTypeDefinitionSyntax::can_cast(&node) {
            return ScalarTypeDefinitionSyntax::EnumerationTypeDefinition(
                EnumerationTypeDefinitionSyntax::cast_unchecked(node),
            );
        }
        if NumericTypeDefinitionSyntax::can_cast(&node) {
            return ScalarTypeDefinitionSyntax::NumericTypeDefinition(
                NumericTypeDefinitionSyntax::cast_unchecked(node),
            );
        }
        if PhysicalTypeDefinitionSyntax::can_cast(&node) {
            return ScalarTypeDefinitionSyntax::PhysicalTypeDefinition(
                PhysicalTypeDefinitionSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            ScalarTypeDefinitionSyntax::EnumerationTypeDefinition(inner) => inner.raw(),
            ScalarTypeDefinitionSyntax::NumericTypeDefinition(inner) => inner.raw(),
            ScalarTypeDefinitionSyntax::PhysicalTypeDefinition(inner) => inner.raw(),
        }
    }
}
#[derive(Debug, Clone)]
pub enum SecondaryUnitSyntax {
    ArchitectureBody(ArchitectureBodySyntax),
    SecondaryUnitPackageBody(SecondaryUnitPackageBodySyntax),
}
impl AstNode for SecondaryUnitSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::ArchitectureBody,
            NodeKind::SecondaryUnitPackageBody,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if ArchitectureBodySyntax::can_cast(&node) {
            return SecondaryUnitSyntax::ArchitectureBody(ArchitectureBodySyntax::cast_unchecked(
                node,
            ));
        }
        if SecondaryUnitPackageBodySyntax::can_cast(&node) {
            return SecondaryUnitSyntax::SecondaryUnitPackageBody(
                SecondaryUnitPackageBodySyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            SecondaryUnitSyntax::ArchitectureBody(inner) => inner.raw(),
            SecondaryUnitSyntax::SecondaryUnitPackageBody(inner) => inner.raw(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct SecondaryUnitDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for SecondaryUnitDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SecondaryUnitDeclaration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "identifier",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "eq",
                kind: LayoutItemKind::Token(TokenKind::EQ),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "physical_literal",
                kind: LayoutItemKind::Node(NodeKind::PhysicalLiteral),
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
        SecondaryUnitDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SecondaryUnitDeclarationSyntax {
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Identifier)
            .nth(0)
    }
    pub fn eq_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::EQ)
            .nth(0)
    }
    pub fn physical_literal(&self) -> Option<PhysicalLiteralSyntax> {
        self.0
            .children()
            .filter_map(PhysicalLiteralSyntax::cast)
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
pub struct SecondaryUnitPackageBodySyntax(pub(crate) SyntaxNode);
impl AstNode for SecondaryUnitPackageBodySyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SecondaryUnitPackageBody,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "package_body",
            kind: LayoutItemKind::Node(NodeKind::PackageBody),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        SecondaryUnitPackageBodySyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SecondaryUnitPackageBodySyntax {
    pub fn package_body(&self) -> Option<PackageBodySyntax> {
        self.0.children().filter_map(PackageBodySyntax::cast).nth(0)
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
                    NodeKind::Allocator,
                    NodeKind::NameExpression,
                    NodeKind::QualifiedExpression,
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
                    NodeKind::Allocator,
                    NodeKind::NameExpression,
                    NodeKind::QualifiedExpression,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "when",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::When)),
            },
            LayoutItem {
                optional: false,
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
    const META: &'static Layout = &Layout::List(List {
        kind: NodeKind::SelectedExpressions,
        element: &LayoutItem {
            optional: false,
            repeated: true,
            name: "selected_expression_items",
            kind: LayoutItemKind::Node(NodeKind::SelectedExpressionItem),
        },
        separator: &LayoutItem {
            optional: false,
            repeated: true,
            name: "comma",
            kind: LayoutItemKind::Token(TokenKind::Comma),
        },
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
                optional: true,
                repeated: false,
                name: "label",
                kind: LayoutItemKind::Node(NodeKind::Label),
            },
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
                name: "lte",
                kind: LayoutItemKind::Token(TokenKind::LTE),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "force",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Force)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "force_mode",
                kind: LayoutItemKind::TokenChoice(&[
                    TokenKind::Keyword(Kw::In),
                    TokenKind::Keyword(Kw::Out),
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "selected_expressions",
                kind: LayoutItemKind::Node(NodeKind::SelectedExpressions),
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
        SelectedForceAssignmentSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SelectedForceAssignmentSyntax {
    pub fn label(&self) -> Option<LabelSyntax> {
        self.0.children().filter_map(LabelSyntax::cast).nth(0)
    }
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
pub struct SelectedNameSyntax(pub(crate) SyntaxNode);
impl AstNode for SelectedNameSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SelectedName,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "dot",
                kind: LayoutItemKind::Token(TokenKind::Dot),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "suffix",
                kind: LayoutItemKind::TokenChoice(&[
                    TokenKind::Identifier,
                    TokenKind::StringLiteral,
                    TokenKind::CharacterLiteral,
                    TokenKind::Keyword(Kw::All),
                ]),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        SelectedNameSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SelectedNameSyntax {
    pub fn dot_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Dot)
            .nth(0)
    }
    pub fn suffix(&self) -> Option<SuffixSyntax> {
        self.0.tokens().filter_map(SuffixSyntax::cast).nth(0)
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
                optional: true,
                repeated: false,
                name: "label",
                kind: LayoutItemKind::Node(NodeKind::Label),
            },
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
                name: "colon_eq",
                kind: LayoutItemKind::Token(TokenKind::ColonEq),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "selected_expressions",
                kind: LayoutItemKind::Node(NodeKind::SelectedExpressions),
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
        SelectedVariableAssignmentSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SelectedVariableAssignmentSyntax {
    pub fn label(&self) -> Option<LabelSyntax> {
        self.0.children().filter_map(LabelSyntax::cast).nth(0)
    }
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
                optional: true,
                repeated: false,
                name: "label",
                kind: LayoutItemKind::Node(NodeKind::Label),
            },
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
                name: "lte",
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
        SelectedWaveformAssignmentSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SelectedWaveformAssignmentSyntax {
    pub fn label(&self) -> Option<LabelSyntax> {
        self.0.children().filter_map(LabelSyntax::cast).nth(0)
    }
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
                name: "when",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::When)),
            },
            LayoutItem {
                optional: false,
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
    const META: &'static Layout = &Layout::List(List {
        kind: NodeKind::SelectedWaveforms,
        element: &LayoutItem {
            optional: false,
            repeated: true,
            name: "selected_waveform_items",
            kind: LayoutItemKind::Node(NodeKind::SelectedWaveformItem),
        },
        separator: &LayoutItem {
            optional: false,
            repeated: true,
            name: "comma",
            kind: LayoutItemKind::Token(TokenKind::Comma),
        },
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
pub struct SensitivityClauseSyntax(pub(crate) SyntaxNode);
impl AstNode for SensitivityClauseSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SensitivityClause,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "on",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::On)),
            },
            LayoutItem {
                optional: false,
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
pub struct SensitivityListSyntax(pub(crate) SyntaxNode);
impl AstNode for SensitivityListSyntax {
    const META: &'static Layout = &Layout::List(List {
        kind: NodeKind::SensitivityList,
        element: &LayoutItem {
            optional: false,
            repeated: true,
            name: "names",
            kind: LayoutItemKind::Node(NodeKind::Name),
        },
        separator: &LayoutItem {
            optional: false,
            repeated: true,
            name: "comma",
            kind: LayoutItemKind::Token(TokenKind::Comma),
        },
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
#[derive(Debug, Clone)]
pub struct SeverityClauseSyntax(pub(crate) SyntaxNode);
impl AstNode for SeverityClauseSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SeverityClause,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "severity",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Severity)),
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
                    NodeKind::Allocator,
                    NodeKind::NameExpression,
                    NodeKind::QualifiedExpression,
                ]),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        SeverityClauseSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SeverityClauseSyntax {
    pub fn severity_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Severity))
            .nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
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
pub struct SignalDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for SignalDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SignalDeclaration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "signal",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Signal)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "identifier_list",
                kind: LayoutItemKind::Node(NodeKind::IdentifierList),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "colon",
                kind: LayoutItemKind::Token(TokenKind::Colon),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "subtype_indication",
                kind: LayoutItemKind::Node(NodeKind::SubtypeIndication),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "signal_kind",
                kind: LayoutItemKind::TokenChoice(&[
                    TokenKind::Keyword(Kw::Register),
                    TokenKind::Keyword(Kw::Bus),
                ]),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "initial_value",
                kind: LayoutItemKind::Node(NodeKind::InitialValue),
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
        SignalDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SignalDeclarationSyntax {
    pub fn signal_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Signal))
            .nth(0)
    }
    pub fn identifier_list(&self) -> Option<IdentifierListSyntax> {
        self.0
            .children()
            .filter_map(IdentifierListSyntax::cast)
            .nth(0)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Colon)
            .nth(0)
    }
    pub fn subtype_indication(&self) -> Option<SubtypeIndicationSyntax> {
        self.0
            .children()
            .filter_map(SubtypeIndicationSyntax::cast)
            .nth(0)
    }
    pub fn signal_kind(&self) -> Option<SignalKindSyntax> {
        self.0.tokens().filter_map(SignalKindSyntax::cast).nth(0)
    }
    pub fn initial_value(&self) -> Option<InitialValueSyntax> {
        self.0
            .children()
            .filter_map(InitialValueSyntax::cast)
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
pub enum SignalKindSyntax {
    Register(SyntaxToken),
    Bus(SyntaxToken),
}
impl SignalKindSyntax {
    pub fn cast(token: SyntaxToken) -> Option<Self> {
        match token.kind() {
            TokenKind::Keyword(Kw::Register) => Some(SignalKindSyntax::Register(token)),
            TokenKind::Keyword(Kw::Bus) => Some(SignalKindSyntax::Bus(token)),
            _ => None,
        }
    }
    pub fn raw(&self) -> SyntaxToken {
        match self {
            SignalKindSyntax::Register(token) => token.clone(),
            SignalKindSyntax::Bus(token) => token.clone(),
        }
    }
}
#[derive(Debug, Clone)]
pub enum SignalListSyntax {
    SignalListList(SignalListListSyntax),
    SignalListAll(SignalListAllSyntax),
    SignalListOthers(SignalListOthersSyntax),
}
impl AstNode for SignalListSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::SignalListList,
            NodeKind::SignalListAll,
            NodeKind::SignalListOthers,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if SignalListListSyntax::can_cast(&node) {
            return SignalListSyntax::SignalListList(SignalListListSyntax::cast_unchecked(node));
        }
        if SignalListAllSyntax::can_cast(&node) {
            return SignalListSyntax::SignalListAll(SignalListAllSyntax::cast_unchecked(node));
        }
        if SignalListOthersSyntax::can_cast(&node) {
            return SignalListSyntax::SignalListOthers(SignalListOthersSyntax::cast_unchecked(
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
            SignalListSyntax::SignalListList(inner) => inner.raw(),
            SignalListSyntax::SignalListAll(inner) => inner.raw(),
            SignalListSyntax::SignalListOthers(inner) => inner.raw(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct SignalListAllSyntax(pub(crate) SyntaxNode);
impl AstNode for SignalListAllSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SignalListAll,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "all",
            kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::All)),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        SignalListAllSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SignalListAllSyntax {
    pub fn all_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::All))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct SignalListListSyntax(pub(crate) SyntaxNode);
impl AstNode for SignalListListSyntax {
    const META: &'static Layout = &Layout::List(List {
        kind: NodeKind::SignalListList,
        element: &LayoutItem {
            optional: false,
            repeated: true,
            name: "names",
            kind: LayoutItemKind::Node(NodeKind::Name),
        },
        separator: &LayoutItem {
            optional: false,
            repeated: true,
            name: "comma",
            kind: LayoutItemKind::Token(TokenKind::Comma),
        },
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        SignalListListSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SignalListListSyntax {
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
pub struct SignalListOthersSyntax(pub(crate) SyntaxNode);
impl AstNode for SignalListOthersSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SignalListOthers,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "others",
            kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Others)),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        SignalListOthersSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SignalListOthersSyntax {
    pub fn others_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Others))
            .nth(0)
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
                optional: true,
                repeated: false,
                name: "name_list",
                kind: LayoutItemKind::Node(NodeKind::NameList),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "return_type",
                kind: LayoutItemKind::Node(NodeKind::ReturnType),
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
    pub fn name_list(&self) -> Option<NameListSyntax> {
        self.0.children().filter_map(NameListSyntax::cast).nth(0)
    }
    pub fn return_type(&self) -> Option<ReturnTypeSyntax> {
        self.0.children().filter_map(ReturnTypeSyntax::cast).nth(0)
    }
    pub fn right_square_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::RightSquare)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct SimpleConfigurationSpecificationSyntax(pub(crate) SyntaxNode);
impl AstNode for SimpleConfigurationSpecificationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SimpleConfigurationSpecification,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "component_configuration_preamble",
                kind: LayoutItemKind::Node(NodeKind::ComponentConfigurationPreamble),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "binding_indication",
                kind: LayoutItemKind::Node(NodeKind::BindingIndication),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "semi_colon",
                kind: LayoutItemKind::Token(TokenKind::SemiColon),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "component_configuration_epilogue",
                kind: LayoutItemKind::Node(NodeKind::ComponentConfigurationEpilogue),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        SimpleConfigurationSpecificationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SimpleConfigurationSpecificationSyntax {
    pub fn component_configuration_preamble(&self) -> Option<ComponentConfigurationPreambleSyntax> {
        self.0
            .children()
            .filter_map(ComponentConfigurationPreambleSyntax::cast)
            .nth(0)
    }
    pub fn binding_indication(&self) -> Option<BindingIndicationSyntax> {
        self.0
            .children()
            .filter_map(BindingIndicationSyntax::cast)
            .nth(0)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
    pub fn component_configuration_epilogue(&self) -> Option<ComponentConfigurationEpilogueSyntax> {
        self.0
            .children()
            .filter_map(ComponentConfigurationEpilogueSyntax::cast)
            .nth(0)
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
                name: "lte",
                kind: LayoutItemKind::Token(TokenKind::LTE),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "force",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Force)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "force_mode",
                kind: LayoutItemKind::TokenChoice(&[
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
                    NodeKind::Allocator,
                    NodeKind::NameExpression,
                    NodeKind::QualifiedExpression,
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
                name: "lte",
                kind: LayoutItemKind::Token(TokenKind::LTE),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "release",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Release)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "force_mode",
                kind: LayoutItemKind::TokenChoice(&[
                    TokenKind::Keyword(Kw::In),
                    TokenKind::Keyword(Kw::Out),
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
                name: "colon_eq",
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
                    NodeKind::Allocator,
                    NodeKind::NameExpression,
                    NodeKind::QualifiedExpression,
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
        SimpleVariableAssignmentSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SimpleVariableAssignmentSyntax {
    pub fn label(&self) -> Option<LabelSyntax> {
        self.0.children().filter_map(LabelSyntax::cast).nth(0)
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
                name: "lte",
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
                name: "semi_colon",
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
                name: "sequential_statements",
                kind: LayoutItemKind::Node(NodeKind::SequentialStatements),
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
    pub fn sequential_statements(&self) -> Option<SequentialStatementsSyntax> {
        self.0
            .children()
            .filter_map(SequentialStatementsSyntax::cast)
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
pub struct SubprogramDefaultSyntax(pub(crate) SyntaxNode);
impl AstNode for SubprogramDefaultSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SubprogramDefault,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "is",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Is)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "interface_subprogram_default",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::InterfaceSubprogramDefaultName,
                    NodeKind::InterfaceSubprogramDefaultBox,
                ]),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        SubprogramDefaultSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SubprogramDefaultSyntax {
    pub fn is_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Is))
            .nth(0)
    }
    pub fn interface_subprogram_default(&self) -> Option<InterfaceSubprogramDefaultSyntax> {
        self.0
            .children()
            .filter_map(InterfaceSubprogramDefaultSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct SubprogramHeaderSyntax(pub(crate) SyntaxNode);
impl AstNode for SubprogramHeaderSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SubprogramHeader,
        items: &[
            LayoutItem {
                optional: false,
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
                optional: false,
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
pub struct SubtypeDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for SubtypeDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SubtypeDeclaration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "subtype",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Subtype)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "identifier",
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
                name: "subtype_indication",
                kind: LayoutItemKind::Node(NodeKind::SubtypeIndication),
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
        SubtypeDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SubtypeDeclarationSyntax {
    pub fn subtype_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Subtype))
            .nth(0)
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
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
    pub fn subtype_indication(&self) -> Option<SubtypeIndicationSyntax> {
        self.0
            .children()
            .filter_map(SubtypeIndicationSyntax::cast)
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
pub struct SubtypeIndicationSyntax(pub(crate) SyntaxNode);
impl AstNode for SubtypeIndicationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SubtypeIndication,
        items: &[
            LayoutItem {
                optional: true,
                repeated: false,
                name: "resolution_indication",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::NameResolutionIndication,
                    NodeKind::ParenthesizedElementResolution,
                ]),
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
        SubtypeIndicationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SubtypeIndicationSyntax {
    pub fn resolution_indication(&self) -> Option<ResolutionIndicationSyntax> {
        self.0
            .children()
            .filter_map(ResolutionIndicationSyntax::cast)
            .nth(0)
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
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
            TokenKind::Identifier => Some(SuffixSyntax::Identifier(token)),
            TokenKind::StringLiteral => Some(SuffixSyntax::StringLiteral(token)),
            TokenKind::CharacterLiteral => Some(SuffixSyntax::CharacterLiteral(token)),
            TokenKind::Keyword(Kw::All) => Some(SuffixSyntax::All(token)),
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
                name: "for",
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
                    NodeKind::Allocator,
                    NodeKind::NameExpression,
                    NodeKind::QualifiedExpression,
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
pub struct TransportDelayMechanismSyntax(pub(crate) SyntaxNode);
impl AstNode for TransportDelayMechanismSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::TransportDelayMechanism,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "transport",
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
pub enum TypeDeclarationSyntax {
    FullTypeDeclaration(FullTypeDeclarationSyntax),
    IncompleteTypeDeclaration(IncompleteTypeDeclarationSyntax),
}
impl AstNode for TypeDeclarationSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::FullTypeDeclaration,
            NodeKind::IncompleteTypeDeclaration,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if FullTypeDeclarationSyntax::can_cast(&node) {
            return TypeDeclarationSyntax::FullTypeDeclaration(
                FullTypeDeclarationSyntax::cast_unchecked(node),
            );
        }
        if IncompleteTypeDeclarationSyntax::can_cast(&node) {
            return TypeDeclarationSyntax::IncompleteTypeDeclaration(
                IncompleteTypeDeclarationSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            TypeDeclarationSyntax::FullTypeDeclaration(inner) => inner.raw(),
            TypeDeclarationSyntax::IncompleteTypeDeclaration(inner) => inner.raw(),
        }
    }
}
#[derive(Debug, Clone)]
pub enum TypeDefinitionSyntax {
    ScalarTypeDefinition(ScalarTypeDefinitionSyntax),
    CompositeTypeDefinition(CompositeTypeDefinitionSyntax),
    AccessTypeDefinition(AccessTypeDefinitionSyntax),
    FileTypeDefinition(FileTypeDefinitionSyntax),
    ProtectedTypeDefinition(ProtectedTypeDefinitionSyntax),
}
impl AstNode for TypeDefinitionSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::EnumerationTypeDefinition,
            NodeKind::NumericTypeDefinition,
            NodeKind::PhysicalTypeDefinition,
            NodeKind::UnboundedArrayDefinition,
            NodeKind::ConstrainedArrayDefinition,
            NodeKind::RecordTypeDefinition,
            NodeKind::AccessTypeDefinition,
            NodeKind::FileTypeDefinition,
            NodeKind::ProtectedTypeDeclaration,
            NodeKind::ProtectedTypeBody,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if ScalarTypeDefinitionSyntax::can_cast(&node) {
            return TypeDefinitionSyntax::ScalarTypeDefinition(
                ScalarTypeDefinitionSyntax::cast_unchecked(node),
            );
        }
        if CompositeTypeDefinitionSyntax::can_cast(&node) {
            return TypeDefinitionSyntax::CompositeTypeDefinition(
                CompositeTypeDefinitionSyntax::cast_unchecked(node),
            );
        }
        if AccessTypeDefinitionSyntax::can_cast(&node) {
            return TypeDefinitionSyntax::AccessTypeDefinition(
                AccessTypeDefinitionSyntax::cast_unchecked(node),
            );
        }
        if FileTypeDefinitionSyntax::can_cast(&node) {
            return TypeDefinitionSyntax::FileTypeDefinition(
                FileTypeDefinitionSyntax::cast_unchecked(node),
            );
        }
        if ProtectedTypeDefinitionSyntax::can_cast(&node) {
            return TypeDefinitionSyntax::ProtectedTypeDefinition(
                ProtectedTypeDefinitionSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            TypeDefinitionSyntax::ScalarTypeDefinition(inner) => inner.raw(),
            TypeDefinitionSyntax::CompositeTypeDefinition(inner) => inner.raw(),
            TypeDefinitionSyntax::AccessTypeDefinition(inner) => inner.raw(),
            TypeDefinitionSyntax::FileTypeDefinition(inner) => inner.raw(),
            TypeDefinitionSyntax::ProtectedTypeDefinition(inner) => inner.raw(),
        }
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
            name: "unaffected",
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
pub struct UnaryExpressionSyntax(pub(crate) SyntaxNode);
impl AstNode for UnaryExpressionSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::UnaryExpression,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "op",
                kind: LayoutItemKind::TokenChoice(&[
                    TokenKind::QueQue,
                    TokenKind::Plus,
                    TokenKind::Minus,
                    TokenKind::Keyword(Kw::Abs),
                    TokenKind::Keyword(Kw::Not),
                    TokenKind::Keyword(Kw::And),
                    TokenKind::Keyword(Kw::Or),
                    TokenKind::Keyword(Kw::Nand),
                    TokenKind::Keyword(Kw::Nor),
                    TokenKind::Keyword(Kw::Xor),
                    TokenKind::Keyword(Kw::Xnor),
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
                    NodeKind::Allocator,
                    NodeKind::NameExpression,
                    NodeKind::QualifiedExpression,
                ]),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        UnaryExpressionSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl UnaryExpressionSyntax {
    pub fn op(&self) -> Option<UnaryOperatorSyntax> {
        self.0.tokens().filter_map(UnaryOperatorSyntax::cast).nth(0)
    }
    pub fn expression(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum UnaryOperatorSyntax {
    QueQue(SyntaxToken),
    Plus(SyntaxToken),
    Minus(SyntaxToken),
    Abs(SyntaxToken),
    Not(SyntaxToken),
    And(SyntaxToken),
    Or(SyntaxToken),
    Nand(SyntaxToken),
    Nor(SyntaxToken),
    Xor(SyntaxToken),
    Xnor(SyntaxToken),
}
impl UnaryOperatorSyntax {
    pub fn cast(token: SyntaxToken) -> Option<Self> {
        match token.kind() {
            TokenKind::QueQue => Some(UnaryOperatorSyntax::QueQue(token)),
            TokenKind::Plus => Some(UnaryOperatorSyntax::Plus(token)),
            TokenKind::Minus => Some(UnaryOperatorSyntax::Minus(token)),
            TokenKind::Keyword(Kw::Abs) => Some(UnaryOperatorSyntax::Abs(token)),
            TokenKind::Keyword(Kw::Not) => Some(UnaryOperatorSyntax::Not(token)),
            TokenKind::Keyword(Kw::And) => Some(UnaryOperatorSyntax::And(token)),
            TokenKind::Keyword(Kw::Or) => Some(UnaryOperatorSyntax::Or(token)),
            TokenKind::Keyword(Kw::Nand) => Some(UnaryOperatorSyntax::Nand(token)),
            TokenKind::Keyword(Kw::Nor) => Some(UnaryOperatorSyntax::Nor(token)),
            TokenKind::Keyword(Kw::Xor) => Some(UnaryOperatorSyntax::Xor(token)),
            TokenKind::Keyword(Kw::Xnor) => Some(UnaryOperatorSyntax::Xnor(token)),
            _ => None,
        }
    }
    pub fn raw(&self) -> SyntaxToken {
        match self {
            UnaryOperatorSyntax::QueQue(token) => token.clone(),
            UnaryOperatorSyntax::Plus(token) => token.clone(),
            UnaryOperatorSyntax::Minus(token) => token.clone(),
            UnaryOperatorSyntax::Abs(token) => token.clone(),
            UnaryOperatorSyntax::Not(token) => token.clone(),
            UnaryOperatorSyntax::And(token) => token.clone(),
            UnaryOperatorSyntax::Or(token) => token.clone(),
            UnaryOperatorSyntax::Nand(token) => token.clone(),
            UnaryOperatorSyntax::Nor(token) => token.clone(),
            UnaryOperatorSyntax::Xor(token) => token.clone(),
            UnaryOperatorSyntax::Xnor(token) => token.clone(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct UnboundedArrayDefinitionSyntax(pub(crate) SyntaxNode);
impl AstNode for UnboundedArrayDefinitionSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::UnboundedArrayDefinition,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "array",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Array)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "left_par",
                kind: LayoutItemKind::Token(TokenKind::LeftPar),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "index_subtype_definition_list",
                kind: LayoutItemKind::Node(NodeKind::IndexSubtypeDefinitionList),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "right_par",
                kind: LayoutItemKind::Token(TokenKind::RightPar),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "of",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Of)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "subtype_indication",
                kind: LayoutItemKind::Node(NodeKind::SubtypeIndication),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        UnboundedArrayDefinitionSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl UnboundedArrayDefinitionSyntax {
    pub fn array_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Array))
            .nth(0)
    }
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::LeftPar)
            .nth(0)
    }
    pub fn index_subtype_definition_list(&self) -> Option<IndexSubtypeDefinitionListSyntax> {
        self.0
            .children()
            .filter_map(IndexSubtypeDefinitionListSyntax::cast)
            .nth(0)
    }
    pub fn right_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::RightPar)
            .nth(0)
    }
    pub fn of_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Of))
            .nth(0)
    }
    pub fn subtype_indication(&self) -> Option<SubtypeIndicationSyntax> {
        self.0
            .children()
            .filter_map(SubtypeIndicationSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct UnitDeclarationsSyntax(pub(crate) SyntaxNode);
impl AstNode for UnitDeclarationsSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::UnitDeclarations,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "units",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Units)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "primary_unit_declaration",
                kind: LayoutItemKind::Node(NodeKind::PrimaryUnitDeclaration),
            },
            LayoutItem {
                optional: false,
                repeated: true,
                name: "secondary_unit_declarations",
                kind: LayoutItemKind::Node(NodeKind::SecondaryUnitDeclaration),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        UnitDeclarationsSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl UnitDeclarationsSyntax {
    pub fn units_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Units))
            .nth(0)
    }
    pub fn primary_unit_declaration(&self) -> Option<PrimaryUnitDeclarationSyntax> {
        self.0
            .children()
            .filter_map(PrimaryUnitDeclarationSyntax::cast)
            .nth(0)
    }
    pub fn secondary_unit_declarations(
        &self,
    ) -> impl Iterator<Item = SecondaryUnitDeclarationSyntax> + use<'_> {
        self.0
            .children()
            .filter_map(SecondaryUnitDeclarationSyntax::cast)
    }
}
#[derive(Debug, Clone)]
pub struct UpLevelSyntax(pub(crate) SyntaxNode);
impl AstNode for UpLevelSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::UpLevel,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "circ",
                kind: LayoutItemKind::Token(TokenKind::Circ),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "dot",
                kind: LayoutItemKind::Token(TokenKind::Dot),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        UpLevelSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl UpLevelSyntax {
    pub fn circ_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Circ)
            .nth(0)
    }
    pub fn dot_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Dot)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct UseClauseSyntax(pub(crate) SyntaxNode);
impl AstNode for UseClauseSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::UseClause,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "use",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Use)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "name_list",
                kind: LayoutItemKind::Node(NodeKind::NameList),
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
        UseClauseSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl UseClauseSyntax {
    pub fn use_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Use))
            .nth(0)
    }
    pub fn name_list(&self) -> Option<NameListSyntax> {
        self.0.children().filter_map(NameListSyntax::cast).nth(0)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct UseClauseContextItemSyntax(pub(crate) SyntaxNode);
impl AstNode for UseClauseContextItemSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::UseClauseContextItem,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "use_clause",
            kind: LayoutItemKind::Node(NodeKind::UseClause),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        UseClauseContextItemSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl UseClauseContextItemSyntax {
    pub fn use_clause(&self) -> Option<UseClauseSyntax> {
        self.0.children().filter_map(UseClauseSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct UseClauseDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for UseClauseDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::UseClauseDeclaration,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "use_clause",
            kind: LayoutItemKind::Node(NodeKind::UseClause),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        UseClauseDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl UseClauseDeclarationSyntax {
    pub fn use_clause(&self) -> Option<UseClauseSyntax> {
        self.0.children().filter_map(UseClauseSyntax::cast).nth(0)
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
pub struct VariableDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for VariableDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::VariableDeclaration,
        items: &[
            LayoutItem {
                optional: true,
                repeated: false,
                name: "shared",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Shared)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "variable",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Variable)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "identifier_list",
                kind: LayoutItemKind::Node(NodeKind::IdentifierList),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "colon",
                kind: LayoutItemKind::Token(TokenKind::Colon),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "subtype_indication",
                kind: LayoutItemKind::Node(NodeKind::SubtypeIndication),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "initial_value",
                kind: LayoutItemKind::Node(NodeKind::InitialValue),
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
        VariableDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl VariableDeclarationSyntax {
    pub fn shared_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Shared))
            .nth(0)
    }
    pub fn variable_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Variable))
            .nth(0)
    }
    pub fn identifier_list(&self) -> Option<IdentifierListSyntax> {
        self.0
            .children()
            .filter_map(IdentifierListSyntax::cast)
            .nth(0)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Colon)
            .nth(0)
    }
    pub fn subtype_indication(&self) -> Option<SubtypeIndicationSyntax> {
        self.0
            .children()
            .filter_map(SubtypeIndicationSyntax::cast)
            .nth(0)
    }
    pub fn initial_value(&self) -> Option<InitialValueSyntax> {
        self.0
            .children()
            .filter_map(InitialValueSyntax::cast)
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
pub struct VerificationUnitBindingSyntax(pub(crate) SyntaxNode);
impl AstNode for VerificationUnitBindingSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::VerificationUnitBinding,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "verification_unit_binding_indication",
                kind: LayoutItemKind::Node(NodeKind::VerificationUnitBindingIndication),
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
        VerificationUnitBindingSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl VerificationUnitBindingSyntax {
    pub fn verification_unit_binding_indication(
        &self,
    ) -> Option<VerificationUnitBindingIndicationSyntax> {
        self.0
            .children()
            .filter_map(VerificationUnitBindingIndicationSyntax::cast)
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
pub struct VerificationUnitBindingIndicationSyntax(pub(crate) SyntaxNode);
impl AstNode for VerificationUnitBindingIndicationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::VerificationUnitBindingIndication,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "use",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Use)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "vunit",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Vunit)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "verification_unit_list",
                kind: LayoutItemKind::Node(NodeKind::VerificationUnitList),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        VerificationUnitBindingIndicationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl VerificationUnitBindingIndicationSyntax {
    pub fn use_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Use))
            .nth(0)
    }
    pub fn vunit_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Vunit))
            .nth(0)
    }
    pub fn verification_unit_list(&self) -> Option<VerificationUnitListSyntax> {
        self.0
            .children()
            .filter_map(VerificationUnitListSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct VerificationUnitListSyntax(pub(crate) SyntaxNode);
impl AstNode for VerificationUnitListSyntax {
    const META: &'static Layout = &Layout::List(List {
        kind: NodeKind::VerificationUnitList,
        element: &LayoutItem {
            optional: false,
            repeated: true,
            name: "names",
            kind: LayoutItemKind::Node(NodeKind::Name),
        },
        separator: &LayoutItem {
            optional: false,
            repeated: true,
            name: "comma",
            kind: LayoutItemKind::Token(TokenKind::Comma),
        },
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        VerificationUnitListSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl VerificationUnitListSyntax {
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
pub struct WaitStatementSyntax(pub(crate) SyntaxNode);
impl AstNode for WaitStatementSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::WaitStatement,
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
                name: "wait",
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
                name: "semi_colon",
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
                    NodeKind::Allocator,
                    NodeKind::NameExpression,
                    NodeKind::QualifiedExpression,
                ]),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "after_clause",
                kind: LayoutItemKind::Node(NodeKind::AfterClause),
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
    pub fn after_clause(&self) -> Option<AfterClauseSyntax> {
        self.0.children().filter_map(AfterClauseSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct WaveformElementsSyntax(pub(crate) SyntaxNode);
impl AstNode for WaveformElementsSyntax {
    const META: &'static Layout = &Layout::List(List {
        kind: NodeKind::WaveformElements,
        element: &LayoutItem {
            optional: false,
            repeated: true,
            name: "waveform_elements",
            kind: LayoutItemKind::Node(NodeKind::WaveformElement),
        },
        separator: &LayoutItem {
            optional: false,
            repeated: true,
            name: "comma",
            kind: LayoutItemKind::Token(TokenKind::Comma),
        },
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
pub struct WhenClauseSyntax(pub(crate) SyntaxNode);
impl AstNode for WhenClauseSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::WhenClause,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "when",
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
                    NodeKind::Allocator,
                    NodeKind::NameExpression,
                    NodeKind::QualifiedExpression,
                ]),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        WhenClauseSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl WhenClauseSyntax {
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
pub struct WhenExpressionSyntax(pub(crate) SyntaxNode);
impl AstNode for WhenExpressionSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::WhenExpression,
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
                    NodeKind::Allocator,
                    NodeKind::NameExpression,
                    NodeKind::QualifiedExpression,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "when",
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
                    NodeKind::Allocator,
                    NodeKind::NameExpression,
                    NodeKind::QualifiedExpression,
                ]),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        WhenExpressionSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl WhenExpressionSyntax {
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
pub struct WhenWaveformSyntax(pub(crate) SyntaxNode);
impl AstNode for WhenWaveformSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::WhenWaveform,
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
                name: "when",
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
                    NodeKind::Allocator,
                    NodeKind::NameExpression,
                    NodeKind::QualifiedExpression,
                ]),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        WhenWaveformSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl WhenWaveformSyntax {
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
pub struct WhileSchemeSyntax(pub(crate) SyntaxNode);
impl AstNode for WhileSchemeSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::WhileScheme,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "while",
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
                    NodeKind::Allocator,
                    NodeKind::NameExpression,
                    NodeKind::QualifiedExpression,
                ]),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        WhileSchemeSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl WhileSchemeSyntax {
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
