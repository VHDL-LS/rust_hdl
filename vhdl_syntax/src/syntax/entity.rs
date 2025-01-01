// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

use crate::parser::diagnostics::ParserDiagnostic;
use crate::parser::Parser;
use crate::syntax::node::{SyntaxNode, SyntaxToken};
use crate::syntax::node_kind::NodeKind;
use crate::syntax::AstNode;
use crate::tokens::tokenizer::Tokenize;
use crate::tokens::TokenKind::{Identifier, Keyword};
use crate::tokens::{IntoTokenStream, Keyword as Kw};
use std::fmt::{Display, Formatter};
use std::str::FromStr;

pub struct EntityDeclaration(pub(crate) SyntaxNode);

impl AstNode for EntityDeclaration {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::EntityDeclaration => Some(EntityDeclaration(node)),
            _ => None,
        }
    }

    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}

impl FromStr for EntityDeclaration {
    type Err = Vec<ParserDiagnostic>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut parser = Parser::new(s.tokenize().into_token_stream());
        parser.entity();
        let (node, diag) = parser.end();
        if !diag.is_empty() {
            Err(diag)
        } else {
            Ok(EntityDeclaration(SyntaxNode::new_root(node)))
        }
    }
}

impl Display for EntityDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl EntityDeclaration {
    pub fn entity_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .find(|tok| tok.kind() == Keyword(Kw::Entity))
    }

    pub fn identifier(&self) -> Option<SyntaxToken> {
        self.0.tokens().find(|tok| tok.kind() == Identifier)
    }

    pub fn final_identifier(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|tok| tok.kind() == Identifier)
            .nth(1)
    }
}

// TODO: the following nodes are only here because there is no point in creating too many
// TODO: close-to empty files at the moment. At some opportune moment, they should get their own
// TODO: files / modules.
pub struct ConfigurationDeclaration(pub(crate) SyntaxNode);

impl AstNode for ConfigurationDeclaration {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ConfigurationDeclaration => Some(ConfigurationDeclaration(node)),
            _ => None,
        }
    }

    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}

pub struct PackageDeclaration(pub(crate) SyntaxNode);

impl AstNode for PackageDeclaration {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::PackageDeclaration => Some(PackageDeclaration(node)),
            _ => None,
        }
    }

    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}

pub struct PackageInstantiationDeclaration(pub(crate) SyntaxNode);

impl AstNode for PackageInstantiationDeclaration {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::PackageInstantiationDeclaration => {
                Some(PackageInstantiationDeclaration(node))
            }
            _ => None,
        }
    }

    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}

pub struct ContextDeclaration(pub(crate) SyntaxNode);

impl AstNode for ContextDeclaration {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ContextDeclaration => Some(ContextDeclaration(node)),
            _ => None,
        }
    }

    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}

pub struct ArchitectureBody(());

pub struct PackageBody(());
