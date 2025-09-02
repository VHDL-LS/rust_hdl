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
pub enum ContextItemSyntax {
    LibraryClause(LibraryClauseSyntax),
    UseClause(UseClauseSyntax),
    ContextReference(ContextReferenceSyntax),
}
impl AstNode for ContextItemSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::LibraryClause => Some(ContextItemSyntax::LibraryClause(
                LibraryClauseSyntax::cast(node).unwrap(),
            )),
            NodeKind::UseClause => Some(ContextItemSyntax::UseClause(
                UseClauseSyntax::cast(node).unwrap(),
            )),
            NodeKind::ContextReference => Some(ContextItemSyntax::ContextReference(
                ContextReferenceSyntax::cast(node).unwrap(),
            )),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            ContextItemSyntax::LibraryClause(inner) => inner.raw(),
            ContextItemSyntax::UseClause(inner) => inner.raw(),
            ContextItemSyntax::ContextReference(inner) => inner.raw(),
        }
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
pub struct DesignFileSyntax(pub(crate) SyntaxNode);
impl AstNode for DesignFileSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::DesignFile => Some(DesignFileSyntax(node)),
            _ => None,
        }
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
        match node.kind() {
            NodeKind::PrimaryUnit => Some(LibraryUnitSyntax::PrimaryUnit(
                PrimaryUnitSyntax::cast(node).unwrap(),
            )),
            NodeKind::SecondaryUnit => Some(LibraryUnitSyntax::SecondaryUnit(
                SecondaryUnitSyntax::cast(node).unwrap(),
            )),
            _ => None,
        }
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
pub enum PrimaryUnitSyntax {
    EntityDeclaration(EntityDeclarationSyntax),
    ConfigurationDeclaration(ConfigurationDeclarationSyntax),
    PackageDeclaration(PackageDeclarationSyntax),
    PackageInstantiationDeclaration(PackageInstantiationDeclarationSyntax),
    ContextDeclaration(ContextDeclarationSyntax),
    PslVerificationUnit(PslVerificationUnitSyntax),
}
impl AstNode for PrimaryUnitSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::EntityDeclaration => Some(PrimaryUnitSyntax::EntityDeclaration(
                EntityDeclarationSyntax::cast(node).unwrap(),
            )),
            NodeKind::ConfigurationDeclaration => {
                Some(PrimaryUnitSyntax::ConfigurationDeclaration(
                    ConfigurationDeclarationSyntax::cast(node).unwrap(),
                ))
            }
            NodeKind::PackageDeclaration => Some(PrimaryUnitSyntax::PackageDeclaration(
                PackageDeclarationSyntax::cast(node).unwrap(),
            )),
            NodeKind::PackageInstantiationDeclaration => {
                Some(PrimaryUnitSyntax::PackageInstantiationDeclaration(
                    PackageInstantiationDeclarationSyntax::cast(node).unwrap(),
                ))
            }
            NodeKind::ContextDeclaration => Some(PrimaryUnitSyntax::ContextDeclaration(
                ContextDeclarationSyntax::cast(node).unwrap(),
            )),
            NodeKind::PslVerificationUnit => Some(PrimaryUnitSyntax::PslVerificationUnit(
                PslVerificationUnitSyntax::cast(node).unwrap(),
            )),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            PrimaryUnitSyntax::EntityDeclaration(inner) => inner.raw(),
            PrimaryUnitSyntax::ConfigurationDeclaration(inner) => inner.raw(),
            PrimaryUnitSyntax::PackageDeclaration(inner) => inner.raw(),
            PrimaryUnitSyntax::PackageInstantiationDeclaration(inner) => inner.raw(),
            PrimaryUnitSyntax::ContextDeclaration(inner) => inner.raw(),
            PrimaryUnitSyntax::PslVerificationUnit(inner) => inner.raw(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum SecondaryUnitSyntax {
    ArchitectureBody(ArchitectureBodySyntax),
    PackageBody(PackageBodySyntax),
}
impl AstNode for SecondaryUnitSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ArchitectureBody => Some(SecondaryUnitSyntax::ArchitectureBody(
                ArchitectureBodySyntax::cast(node).unwrap(),
            )),
            NodeKind::PackageBody => Some(SecondaryUnitSyntax::PackageBody(
                PackageBodySyntax::cast(node).unwrap(),
            )),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            SecondaryUnitSyntax::ArchitectureBody(inner) => inner.raw(),
            SecondaryUnitSyntax::PackageBody(inner) => inner.raw(),
        }
    }
}
