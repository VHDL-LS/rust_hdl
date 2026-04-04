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
pub struct ContextDeclarationPreambleSyntax(pub(crate) SyntaxNode);
impl AstNode for ContextDeclarationPreambleSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ContextDeclarationPreamble,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Context)),
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
pub struct ContextDeclarationEpilogueSyntax(pub(crate) SyntaxNode);
impl AstNode for ContextDeclarationEpilogueSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ContextDeclarationEpilogue,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::End)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Context)),
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
pub struct ContextReferenceSyntax(pub(crate) SyntaxNode);
impl AstNode for ContextReferenceSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::ContextReference,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
                kind: LayoutItemKind::Token(TokenKind::Keyword(Kw::Context)),
            },
            LayoutItem {
                optional: true,
                repeated: false,
                name: "name_list",
                kind: LayoutItemKind::Node(NodeKind::NameList),
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
                name: "",
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
                    NodeKind::PslVerificationUnit,
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
pub struct LibraryClauseSyntax(pub(crate) SyntaxNode);
impl AstNode for LibraryClauseSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::LibraryClause,
        items: &[
            LayoutItem {
                optional: false,
                repeated: false,
                name: "",
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
                name: "",
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
            NodeKind::PslVerificationUnit,
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
pub enum PrimaryUnitSyntax {
    EntityDeclaration(EntityDeclarationSyntax),
    ConfigurationDeclaration(ConfigurationDeclarationSyntax),
    PrimaryUnitPackageDeclaration(PrimaryUnitPackageDeclarationSyntax),
    PackageInstantiationDeclarationPrimaryUnit(PackageInstantiationDeclarationPrimaryUnitSyntax),
    ContextDeclaration(ContextDeclarationSyntax),
    PslVerificationUnit(PslVerificationUnitSyntax),
}
impl AstNode for PrimaryUnitSyntax {
    const META: &'static Layout = &Layout::Choice(Choice {
        options: &[
            NodeKind::EntityDeclaration,
            NodeKind::ConfigurationDeclaration,
            NodeKind::PrimaryUnitPackageDeclaration,
            NodeKind::PackageInstantiationDeclarationPrimaryUnit,
            NodeKind::ContextDeclaration,
            NodeKind::PslVerificationUnit,
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
        if PslVerificationUnitSyntax::can_cast(&node) {
            return PrimaryUnitSyntax::PslVerificationUnit(
                PslVerificationUnitSyntax::cast_unchecked(node),
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
            PrimaryUnitSyntax::PslVerificationUnit(inner) => inner.raw(),
        }
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
