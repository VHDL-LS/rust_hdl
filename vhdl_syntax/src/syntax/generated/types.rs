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
pub struct AccessTypeDefinitionSyntax(pub(crate) SyntaxNode);
impl AstNode for AccessTypeDefinitionSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::AccessTypeDefinition,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
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
pub struct ArrayConstraintSyntax(pub(crate) SyntaxNode);
impl AstNode for ArrayConstraintSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ArrayConstraint,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "index_constraint",
                kind: LayoutItemKind::Node(NodeKind::IndexConstraint),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "constraint",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::RangeConstraintConstraint,
                    NodeKind::ArrayConstraint,
                    NodeKind::RecordConstraint,
                ]),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ArrayConstraintSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ArrayConstraintSyntax {
    pub fn index_constraint(&self) -> Option<IndexConstraintSyntax> {
        self.0
            .children()
            .filter_map(IndexConstraintSyntax::cast)
            .nth(0)
    }
    pub fn constraint(&self) -> Option<ConstraintSyntax> {
        self.0.children().filter_map(ConstraintSyntax::cast).nth(0)
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
pub struct ConstrainedArrayDefinitionSyntax(pub(crate) SyntaxNode);
impl AstNode for ConstrainedArrayDefinitionSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ConstrainedArrayDefinition,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
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
                name: "",
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
pub enum DirectionSyntax {
    To(SyntaxToken),
    Downto(SyntaxToken),
}
impl DirectionSyntax {
    pub fn cast(token: SyntaxToken) -> Option<Self> {
        match token.kind() {
            TokenKind::Keyword(Kw::To) => Some(DirectionSyntax::To(token)),
            TokenKind::Keyword(Kw::Downto) => Some(DirectionSyntax::Downto(token)),
            _ => None,
        }
    }
    pub fn raw(&self) -> SyntaxToken {
        match self {
            DirectionSyntax::To(token) => token.clone(),
            DirectionSyntax::Downto(token) => token.clone(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct SubtypeIndicationDiscreteDiscreteRangeSyntax(pub(crate) SyntaxNode);
impl AstNode for SubtypeIndicationDiscreteDiscreteRangeSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SubtypeIndicationDiscreteDiscreteRange,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "subtype_indication",
            kind: LayoutItemKind::Node(NodeKind::SubtypeIndication),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        SubtypeIndicationDiscreteDiscreteRangeSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SubtypeIndicationDiscreteDiscreteRangeSyntax {
    pub fn subtype_indication(&self) -> Option<SubtypeIndicationSyntax> {
        self.0
            .children()
            .filter_map(SubtypeIndicationSyntax::cast)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct SubtypeIndicationDiscreteRangeSyntax(pub(crate) SyntaxNode);
impl AstNode for SubtypeIndicationDiscreteRangeSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SubtypeIndicationDiscreteRange,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "range",
            kind: LayoutItemKind::NodeChoice(&[
                NodeKind::AttributeRange,
                NodeKind::RangeExpression,
            ]),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        SubtypeIndicationDiscreteRangeSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl SubtypeIndicationDiscreteRangeSyntax {
    pub fn range(&self) -> Option<RangeSyntax> {
        self.0.children().filter_map(RangeSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct OpenDiscreteRangeSyntax(pub(crate) SyntaxNode);
impl AstNode for OpenDiscreteRangeSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::OpenDiscreteRange,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "",
            kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Open)),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        OpenDiscreteRangeSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl OpenDiscreteRangeSyntax {
    pub fn open_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Open))
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum DiscreteRangeSyntax {
    SubtypeIndicationDiscreteDiscreteRange(SubtypeIndicationDiscreteDiscreteRangeSyntax),
    SubtypeIndicationDiscreteRange(SubtypeIndicationDiscreteRangeSyntax),
    OpenDiscreteRange(OpenDiscreteRangeSyntax),
}
impl AstNode for DiscreteRangeSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::SubtypeIndicationDiscreteDiscreteRange,
            NodeKind::SubtypeIndicationDiscreteRange,
            NodeKind::OpenDiscreteRange,
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if SubtypeIndicationDiscreteDiscreteRangeSyntax::can_cast(&node) {
            return DiscreteRangeSyntax::SubtypeIndicationDiscreteDiscreteRange(
                SubtypeIndicationDiscreteDiscreteRangeSyntax::cast_unchecked(node),
            );
        }
        if SubtypeIndicationDiscreteRangeSyntax::can_cast(&node) {
            return DiscreteRangeSyntax::SubtypeIndicationDiscreteRange(
                SubtypeIndicationDiscreteRangeSyntax::cast_unchecked(node),
            );
        }
        if OpenDiscreteRangeSyntax::can_cast(&node) {
            return DiscreteRangeSyntax::OpenDiscreteRange(
                OpenDiscreteRangeSyntax::cast_unchecked(node),
            );
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            DiscreteRangeSyntax::SubtypeIndicationDiscreteDiscreteRange(inner) => inner.raw(),
            DiscreteRangeSyntax::SubtypeIndicationDiscreteRange(inner) => inner.raw(),
            DiscreteRangeSyntax::OpenDiscreteRange(inner) => inner.raw(),
        }
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
                name: "",
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
                name: "",
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
pub struct EnumerationTypeDefinitionSyntax(pub(crate) SyntaxNode);
impl AstNode for EnumerationTypeDefinitionSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::EnumerationTypeDefinition,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::LeftPar),
            },
            LayoutItem {
                optional: false,
                repeated: true,
                name: "discrete_ranges",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::SubtypeIndicationDiscreteDiscreteRange,
                    NodeKind::SubtypeIndicationDiscreteRange,
                    NodeKind::OpenDiscreteRange,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: true,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Comma),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
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
    pub fn discrete_ranges(&self) -> impl Iterator<Item = DiscreteRangeSyntax> + use<'_> {
        self.0.children().filter_map(DiscreteRangeSyntax::cast)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Comma)
    }
    pub fn right_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::RightPar)
            .nth(0)
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
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::File)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
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
pub struct IdentifierListSyntax(pub(crate) SyntaxNode);
impl AstNode for IdentifierListSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::IdentifierList,
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
                kind: LayoutItemKind::Token(TokenKind::Comma),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        IdentifierListSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl IdentifierListSyntax {
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Identifier)
            .nth(0)
    }
    pub fn comma_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Comma)
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
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Type)),
            },
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
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Range)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
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
pub struct PhysicalLiteralSyntax(pub(crate) SyntaxNode);
impl AstNode for PhysicalLiteralSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PhysicalLiteral,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
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
pub struct UnitDeclarationsSyntax(pub(crate) SyntaxNode);
impl AstNode for UnitDeclarationsSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::UnitDeclarations,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
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
pub struct PhysicalTypeDefinitionEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for PhysicalTypeDefinitionEpilogueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PhysicalTypeDefinitionEpilogue,
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
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Units)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
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
pub struct PrimaryUnitDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for PrimaryUnitDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PrimaryUnitDeclaration,
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
pub struct ProtectedTypeBodyPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for ProtectedTypeBodyPreambleSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ProtectedTypeBodyPreamble,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Protected)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
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
pub struct ProtectedTypeBodyEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for ProtectedTypeBodyEpilogueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ProtectedTypeBodyEpilogue,
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
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Protected)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Body)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
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
pub struct ProtectedTypeDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for ProtectedTypeDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ProtectedTypeDeclaration,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "protected_type_declaration_preamble",
                kind: LayoutItemKind::Node(NodeKind::ProtectedTypeDeclarationPreamble),
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
    pub fn protected_type_declaration_preamble(
        &self,
    ) -> Option<ProtectedTypeDeclarationPreambleSyntax> {
        self.0
            .children()
            .filter_map(ProtectedTypeDeclarationPreambleSyntax::cast)
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
pub struct ProtectedTypeDeclarationPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for ProtectedTypeDeclarationPreambleSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ProtectedTypeDeclarationPreamble,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "",
            kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Protected)),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        ProtectedTypeDeclarationPreambleSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ProtectedTypeDeclarationPreambleSyntax {
    pub fn protected_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Keyword(Kw::Protected))
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
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::End)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Protected)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
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
pub struct RangeExpressionSyntax(pub(crate) SyntaxNode);
impl AstNode for RangeExpressionSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::RangeExpression,
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
                name: "direction",
                kind: LayoutItemKind::TokenGroup(&[
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
        RangeExpressionSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl RangeExpressionSyntax {
    pub fn lhs(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(0)
    }
    pub fn direction(&self) -> Option<DirectionSyntax> {
        self.0.tokens().filter_map(DirectionSyntax::cast).nth(0)
    }
    pub fn rhs(&self) -> Option<ExpressionSyntax> {
        self.0.children().filter_map(ExpressionSyntax::cast).nth(1)
    }
}
#[derive(Debug, Clone)]
pub struct AttributeRangeSyntax(pub(crate) SyntaxNode);
impl AstNode for AttributeRangeSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::AttributeRange,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "name",
            kind: LayoutItemKind::Node(NodeKind::Name),
        }],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        AttributeRangeSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl AttributeRangeSyntax {
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub enum RangeSyntax {
    AttributeRange(AttributeRangeSyntax),
    RangeExpression(RangeExpressionSyntax),
}
impl AstNode for RangeSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[NodeKind::AttributeRange, NodeKind::RangeExpression],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        if AttributeRangeSyntax::can_cast(&node) {
            return RangeSyntax::AttributeRange(AttributeRangeSyntax::cast_unchecked(node));
        }
        if RangeExpressionSyntax::can_cast(&node) {
            return RangeSyntax::RangeExpression(RangeExpressionSyntax::cast_unchecked(node));
        }
        unreachable!(
            "cast_unchecked called with unexpected node kind {:?}",
            node.kind()
        )
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            RangeSyntax::AttributeRange(inner) => inner.raw(),
            RangeSyntax::RangeExpression(inner) => inner.raw(),
        }
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
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Range)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "range",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::AttributeRange,
                    NodeKind::RangeExpression,
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
    pub fn range(&self) -> Option<RangeSyntax> {
        self.0.children().filter_map(RangeSyntax::cast).nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct RecordConstraintSyntax(pub(crate) SyntaxNode);
impl AstNode for RecordConstraintSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::RecordConstraint,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::LeftPar),
            },
            LayoutItem {
                optional: false,
                repeated: true,
                name: "record_element_constraints",
                kind: LayoutItemKind::Node(NodeKind::RecordElementConstraint),
            },
            LayoutItem {
                optional: false,
                repeated: true,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Comma),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::RightPar),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        RecordConstraintSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl RecordConstraintSyntax {
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::LeftPar)
            .nth(0)
    }
    pub fn record_element_constraints(
        &self,
    ) -> impl Iterator<Item = RecordElementConstraintSyntax> + use<'_> {
        self.0
            .children()
            .filter_map(RecordElementConstraintSyntax::cast)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Comma)
    }
    pub fn right_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::RightPar)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct RecordElementConstraintSyntax(pub(crate) SyntaxNode);
impl AstNode for RecordElementConstraintSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::RecordElementConstraint,
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
                name: "constraint",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::RangeConstraintConstraint,
                    NodeKind::ArrayConstraint,
                    NodeKind::RecordConstraint,
                ]),
            },
        ],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        RecordElementConstraintSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl RecordElementConstraintSyntax {
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().filter_map(NameSyntax::cast).nth(0)
    }
    pub fn constraint(&self) -> Option<ConstraintSyntax> {
        self.0.children().filter_map(ConstraintSyntax::cast).nth(0)
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
pub struct RecordTypeDefinitionPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for RecordTypeDefinitionPreambleSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::RecordTypeDefinitionPreamble,
        items: &[LayoutItem {
            optional: false,
            repeated: false,
            name: "",
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
pub struct RecordTypeDefinitionEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for RecordTypeDefinitionEpilogueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::RecordTypeDefinitionEpilogue,
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
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Record)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
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
pub struct SecondaryUnitDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for SecondaryUnitDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::SecondaryUnitDeclaration,
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
                name: "",
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
pub struct IndexConstraintSyntax(pub(crate) SyntaxNode);
impl AstNode for IndexConstraintSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::IndexConstraint,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::LeftPar),
            },
            LayoutItem {
                optional: false,
                repeated: true,
                name: "discrete_ranges",
                kind: LayoutItemKind::NodeChoice(&[
                    NodeKind::SubtypeIndicationDiscreteDiscreteRange,
                    NodeKind::SubtypeIndicationDiscreteRange,
                    NodeKind::OpenDiscreteRange,
                ]),
            },
            LayoutItem {
                optional: false,
                repeated: true,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Comma),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
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
    pub fn discrete_ranges(&self) -> impl Iterator<Item = DiscreteRangeSyntax> + use<'_> {
        self.0.children().filter_map(DiscreteRangeSyntax::cast)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::Comma)
    }
    pub fn right_par_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == TokenKind::RightPar)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct IndexSubtypeDefinitionListSyntax(pub(crate) SyntaxNode);
impl AstNode for IndexSubtypeDefinitionListSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::IndexSubtypeDefinitionList,
        items: &[
            LayoutItem {
                optional: false,
                repeated: true,
                name: "index_subtype_definitions",
                kind: LayoutItemKind::Node(NodeKind::IndexSubtypeDefinition),
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
pub struct UnboundedArrayDefinitionSyntax(pub(crate) SyntaxNode);
impl AstNode for UnboundedArrayDefinitionSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::UnboundedArrayDefinition,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Array)),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::LeftPar),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "index_subtype_definition_list",
                kind: LayoutItemKind::Node(NodeKind::IndexSubtypeDefinitionList),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::RightPar),
            },
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
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
