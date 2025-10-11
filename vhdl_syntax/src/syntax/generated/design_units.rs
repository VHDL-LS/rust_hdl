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
pub struct ContextClauseSyntax(pub(crate) SyntaxNode);
impl AstNode for ContextClauseSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ContextClause => Some(ContextClauseSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ContextClause)
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
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ContextDeclaration => Some(ContextDeclarationSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ContextDeclaration)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ContextDeclarationSyntax {
    pub fn context_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Context))
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
    pub fn context_clause(&self) -> Option<ContextClauseSyntax> {
        self.0
            .children()
            .filter_map(ContextClauseSyntax::cast)
            .nth(0)
    }
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::End))
            .nth(0)
    }
    pub fn trailing_context_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Context))
            .nth(1)
    }
    pub fn trailing_name_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Identifier)
            .nth(1)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == SemiColon)
            .nth(0)
    }
}
#[derive(Debug, Clone)]
pub struct UseClauseContextItemSyntax(pub(crate) SyntaxNode);
impl AstNode for UseClauseContextItemSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::UseClauseContextItem => Some(UseClauseContextItemSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::UseClauseContextItem)
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
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ContextReference => Some(ContextReferenceSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::ContextReference)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl ContextReferenceSyntax {
    pub fn context_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Context))
            .nth(0)
    }
    pub fn names(&self) -> impl Iterator<Item = NameSyntax> + use<'_> {
        self.0.children().filter_map(NameSyntax::cast)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.tokens().filter(|token| token.kind() == Comma)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == SemiColon)
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
    fn cast(node: SyntaxNode) -> Option<Self> {
        if LibraryClauseSyntax::can_cast(&node) {
            return Some(ContextItemSyntax::LibraryClause(
                LibraryClauseSyntax::cast(node).unwrap(),
            ));
        };
        if UseClauseContextItemSyntax::can_cast(&node) {
            return Some(ContextItemSyntax::UseClauseContextItem(
                UseClauseContextItemSyntax::cast(node).unwrap(),
            ));
        };
        if ContextReferenceSyntax::can_cast(&node) {
            return Some(ContextItemSyntax::ContextReference(
                ContextReferenceSyntax::cast(node).unwrap(),
            ));
        };
        None
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        LibraryClauseSyntax::can_cast(node)
            || UseClauseContextItemSyntax::can_cast(node)
            || ContextReferenceSyntax::can_cast(node)
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
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::DesignFile => Some(DesignFileSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::DesignFile)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl DesignFileSyntax {
    pub fn design_units(&self) -> impl Iterator<Item = DesignUnitSyntax> + use<'_> {
        self.0.children().filter_map(DesignUnitSyntax::cast)
    }
}
#[derive(Debug, Clone)]
pub struct DesignUnitSyntax(pub(crate) SyntaxNode);
impl AstNode for DesignUnitSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::DesignUnit => Some(DesignUnitSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::DesignUnit)
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
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::LibraryClause => Some(LibraryClauseSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::LibraryClause)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl LibraryClauseSyntax {
    pub fn library_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Library))
            .nth(0)
    }
    pub fn logical_name_list(&self) -> Option<LogicalNameListSyntax> {
        self.0
            .children()
            .filter_map(LogicalNameListSyntax::cast)
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
pub enum LibraryUnitSyntax {
    PrimaryUnit(PrimaryUnitSyntax),
    SecondaryUnit(SecondaryUnitSyntax),
}
impl AstNode for LibraryUnitSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        if PrimaryUnitSyntax::can_cast(&node) {
            return Some(LibraryUnitSyntax::PrimaryUnit(
                PrimaryUnitSyntax::cast(node).unwrap(),
            ));
        };
        if SecondaryUnitSyntax::can_cast(&node) {
            return Some(LibraryUnitSyntax::SecondaryUnit(
                SecondaryUnitSyntax::cast(node).unwrap(),
            ));
        };
        None
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        PrimaryUnitSyntax::can_cast(node) || SecondaryUnitSyntax::can_cast(node)
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            LibraryUnitSyntax::PrimaryUnit(inner) => inner.raw(),
            LibraryUnitSyntax::SecondaryUnit(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LogicalNameListSyntax(pub(crate) SyntaxNode);
impl AstNode for LogicalNameListSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::LogicalNameList => Some(LogicalNameListSyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::LogicalNameList)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl LogicalNameListSyntax {
    pub fn identifier_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.tokens().filter(|token| token.kind() == Identifier)
    }
    pub fn comma_token(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.tokens().filter(|token| token.kind() == Comma)
    }
}
#[derive(Debug, Clone)]
pub struct PrimaryUnitPackageDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for PrimaryUnitPackageDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::PrimaryUnitPackageDeclaration => {
                Some(PrimaryUnitPackageDeclarationSyntax(node))
            }
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::PrimaryUnitPackageDeclaration)
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
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::PackageInstantiationDeclarationPrimaryUnit => {
                Some(PackageInstantiationDeclarationPrimaryUnitSyntax(node))
            }
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(
            node.kind(),
            NodeKind::PackageInstantiationDeclarationPrimaryUnit
        )
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
    fn cast(node: SyntaxNode) -> Option<Self> {
        if EntityDeclarationSyntax::can_cast(&node) {
            return Some(PrimaryUnitSyntax::EntityDeclaration(
                EntityDeclarationSyntax::cast(node).unwrap(),
            ));
        };
        if ConfigurationDeclarationSyntax::can_cast(&node) {
            return Some(PrimaryUnitSyntax::ConfigurationDeclaration(
                ConfigurationDeclarationSyntax::cast(node).unwrap(),
            ));
        };
        if PrimaryUnitPackageDeclarationSyntax::can_cast(&node) {
            return Some(PrimaryUnitSyntax::PrimaryUnitPackageDeclaration(
                PrimaryUnitPackageDeclarationSyntax::cast(node).unwrap(),
            ));
        };
        if PackageInstantiationDeclarationPrimaryUnitSyntax::can_cast(&node) {
            return Some(
                PrimaryUnitSyntax::PackageInstantiationDeclarationPrimaryUnit(
                    PackageInstantiationDeclarationPrimaryUnitSyntax::cast(node).unwrap(),
                ),
            );
        };
        if ContextDeclarationSyntax::can_cast(&node) {
            return Some(PrimaryUnitSyntax::ContextDeclaration(
                ContextDeclarationSyntax::cast(node).unwrap(),
            ));
        };
        if PslVerificationUnitSyntax::can_cast(&node) {
            return Some(PrimaryUnitSyntax::PslVerificationUnit(
                PslVerificationUnitSyntax::cast(node).unwrap(),
            ));
        };
        None
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        EntityDeclarationSyntax::can_cast(node)
            || ConfigurationDeclarationSyntax::can_cast(node)
            || PrimaryUnitPackageDeclarationSyntax::can_cast(node)
            || PackageInstantiationDeclarationPrimaryUnitSyntax::can_cast(node)
            || ContextDeclarationSyntax::can_cast(node)
            || PslVerificationUnitSyntax::can_cast(node)
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
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::SecondaryUnitPackageBody => Some(SecondaryUnitPackageBodySyntax(node)),
            _ => None,
        }
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        matches!(node.kind(), NodeKind::SecondaryUnitPackageBody)
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
    fn cast(node: SyntaxNode) -> Option<Self> {
        if ArchitectureBodySyntax::can_cast(&node) {
            return Some(SecondaryUnitSyntax::ArchitectureBody(
                ArchitectureBodySyntax::cast(node).unwrap(),
            ));
        };
        if SecondaryUnitPackageBodySyntax::can_cast(&node) {
            return Some(SecondaryUnitSyntax::SecondaryUnitPackageBody(
                SecondaryUnitPackageBodySyntax::cast(node).unwrap(),
            ));
        };
        None
    }
    fn can_cast(node: &SyntaxNode) -> bool {
        ArchitectureBodySyntax::can_cast(node) || SecondaryUnitPackageBodySyntax::can_cast(node)
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            SecondaryUnitSyntax::ArchitectureBody(inner) => inner.raw(),
            SecondaryUnitSyntax::SecondaryUnitPackageBody(inner) => inner.raw(),
        }
    }
}
