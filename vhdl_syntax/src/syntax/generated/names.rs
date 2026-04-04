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
                optional: true,
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
pub struct AttributeNameSyntax(pub(crate) SyntaxNode);
impl AstNode for AttributeNameSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::AttributeName,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "left_square",
                kind: LayoutItemKind::Token(TokenKind::LeftSquare),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "signature",
                kind: LayoutItemKind::Node(NodeKind::Signature),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "right_square",
                kind: LayoutItemKind::Token(TokenKind::RightSquare),
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
                kind: LayoutItemKind::Token(TokenKind::Identifier),
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
        AttributeNameSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl AttributeNameSyntax {
    pub fn left_square_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::LeftSquare)
            .nth(0)
    }
    pub fn signature(&self) -> Option<SignatureSyntax> {
        self.0.children().filter_map(SignatureSyntax::cast).nth(0)
    }
    pub fn right_square_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::RightSquare)
            .nth(0)
    }
    pub fn tick_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Tick)
            .nth(0)
    }
    pub fn attribute_designator_token_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Identifier)
            .nth(0)
    }
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
                name: "external_path_name",
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
    pub fn external_path_name(&self) -> Option<ExternalPathNameSyntax> {
        self.0
            .children()
            .filter_map(ExternalPathNameSyntax::cast)
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
                name: "external_path_name",
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
    pub fn external_path_name(&self) -> Option<ExternalPathNameSyntax> {
        self.0
            .children()
            .filter_map(ExternalPathNameSyntax::cast)
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
                name: "external_path_name",
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
    pub fn external_path_name(&self) -> Option<ExternalPathNameSyntax> {
        self.0
            .children()
            .filter_map(ExternalPathNameSyntax::cast)
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
pub enum ExternalPathNameSyntax {
    PackagePathname(PackagePathnameSyntax),
    AbsolutePathname(AbsolutePathnameSyntax),
    RelativePathname(RelativePathnameSyntax),
}
impl AstNode for ExternalPathNameSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::PackagePathname,
            NodeKind::AbsolutePathname,
            NodeKind::RelativePathname,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if PackagePathnameSyntax::can_cast(&node) {
            return ExternalPathNameSyntax::PackagePathname(PackagePathnameSyntax::cast_unchecked(
                node,
            ));
        }
        if AbsolutePathnameSyntax::can_cast(&node) {
            return ExternalPathNameSyntax::AbsolutePathname(
                AbsolutePathnameSyntax::cast_unchecked(node),
            );
        }
        if RelativePathnameSyntax::can_cast(&node) {
            return ExternalPathNameSyntax::RelativePathname(
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
            ExternalPathNameSyntax::PackagePathname(inner) => inner.raw(),
            ExternalPathNameSyntax::AbsolutePathname(inner) => inner.raw(),
            ExternalPathNameSyntax::RelativePathname(inner) => inner.raw(),
        }
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
                repeated: true,
                name: "dot",
                kind: LayoutItemKind::Token(TokenKind::Dot),
            },
            LayoutItem {
                optional: false,
                repeated: true,
                name: "simple_name",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
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
    pub fn dot_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Dot)
    }
    pub fn simple_name_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Identifier)
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
                name: "circ",
                kind: LayoutItemKind::Token(TokenKind::Circ),
            },
            LayoutItem {
                optional: false,
                repeated: true,
                name: "dot",
                kind: LayoutItemKind::Token(TokenKind::Dot),
            },
            LayoutItem {
                optional: true,
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
    pub fn circ_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Circ)
    }
    pub fn dot_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Dot)
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
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PartialPathname,
        items: &[
            LayoutItem {
                optional: false,
                repeated: true,
                name: "identifier",
                kind: LayoutItemKind::Token(TokenKind::Identifier),
            },
            LayoutItem {
                optional: false,
                repeated: true,
                name: "dot",
                kind: LayoutItemKind::Token(TokenKind::Dot),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        PartialPathnameSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PartialPathnameSyntax {
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
pub struct NameListSyntax(pub(crate) SyntaxNode);
impl AstNode for NameListSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::NameList,
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
                    NodeKind::RawTokens,
                    NodeKind::AttributeName,
                ]),
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
pub enum NameTailSyntax {
    SelectedName(SelectedNameSyntax),
    RawTokens(RawTokensSyntax),
    AttributeName(AttributeNameSyntax),
}
impl AstNode for NameTailSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::SelectedName,
            NodeKind::RawTokens,
            NodeKind::AttributeName,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if SelectedNameSyntax::can_cast(&node) {
            return NameTailSyntax::SelectedName(SelectedNameSyntax::cast_unchecked(node));
        }
        if RawTokensSyntax::can_cast(&node) {
            return NameTailSyntax::RawTokens(RawTokensSyntax::cast_unchecked(node));
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
            NameTailSyntax::RawTokens(inner) => inner.raw(),
            NameTailSyntax::AttributeName(inner) => inner.raw(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct RawTokensSyntax(pub(crate) SyntaxNode);
impl AstNode for RawTokensSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::RawTokens,
        items: &[],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        RawTokensSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
