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
pub struct PslDirectiveSyntax(pub(crate) SyntaxNode);
impl AstNode for PslDirectiveSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PslDirective,
        items: &[],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        PslDirectiveSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PslDirectiveSyntax {}
#[derive(Debug, Clone)]
pub struct PslPropertyDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for PslPropertyDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PslPropertyDeclaration,
        items: &[],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        PslPropertyDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PslPropertyDeclarationSyntax {}
#[derive(Debug, Clone)]
pub struct PslSequenceDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for PslSequenceDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PslSequenceDeclaration,
        items: &[],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        PslSequenceDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PslSequenceDeclarationSyntax {}
#[derive(Debug, Clone)]
pub struct PslClockDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for PslClockDeclarationSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PslClockDeclaration,
        items: &[],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        PslClockDeclarationSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PslClockDeclarationSyntax {}
#[derive(Debug, Clone)]
pub struct PslVerificationUnitSyntax(pub(crate) SyntaxNode);
impl AstNode for PslVerificationUnitSyntax {
    const META: &'static Layout = &Layout::Sequence(Sequence {
        kind: NodeKind::PslVerificationUnit,
        items: &[],
    });
    fn cast_unchecked(node: SyntaxNode) -> Self {
        PslVerificationUnitSyntax(node)
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PslVerificationUnitSyntax {}
