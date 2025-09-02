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
pub struct PslDirectiveSyntax(pub(crate) SyntaxNode);
impl AstNode for PslDirectiveSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::PslDirective => Some(PslDirectiveSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PslDirectiveSyntax {}
#[derive(Debug, Clone)]
pub struct PslPropertyDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for PslPropertyDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::PslPropertyDeclaration => Some(PslPropertyDeclarationSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PslPropertyDeclarationSyntax {}
#[derive(Debug, Clone)]
pub struct PslSequenceDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for PslSequenceDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::PslSequenceDeclaration => Some(PslSequenceDeclarationSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PslSequenceDeclarationSyntax {}
#[derive(Debug, Clone)]
pub struct PslClockDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for PslClockDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::PslClockDeclaration => Some(PslClockDeclarationSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PslClockDeclarationSyntax {}
#[derive(Debug, Clone)]
pub struct PslVerificationUnitSyntax(pub(crate) SyntaxNode);
impl AstNode for PslVerificationUnitSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::PslVerificationUnit => Some(PslVerificationUnitSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PslVerificationUnitSyntax {}
