// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

use crate::parser::diagnostics::ParserDiagnostic;
use crate::parser::Parser;
use crate::syntax::entity::{
    ArchitectureBody, ConfigurationDeclaration, ContextDeclaration, EntityDeclaration, PackageBody,
    PackageDeclaration, PackageInstantiationDeclaration,
};
use crate::syntax::node::SyntaxNode;
use crate::syntax::node_kind::NodeKind;
use crate::syntax::AstNode;
use crate::tokens::tokenizer::Tokenize;
use crate::tokens::IntoTokenStream;
use std::str::FromStr;

pub struct DesignFile(pub(crate) SyntaxNode);

impl DesignFile {
    pub fn design_units(&self) -> impl Iterator<Item = DesignUnit> + use<'_> {
        self.0.children().filter_map(DesignUnit::cast)
    }
}

impl FromStr for DesignFile {
    type Err = Vec<ParserDiagnostic>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parser = Parser::new(s.tokenize().into_token_stream());
        let (node, diagnostics) = parser.parse();
        if diagnostics.is_empty() {
            Ok(DesignFile(node))
        } else {
            Err(diagnostics)
        }
    }
}

impl AstNode for DesignFile {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::DesignFile => Some(Self(node)),
            _ => None,
        }
    }

    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}

pub struct DesignUnit(SyntaxNode);

impl AstNode for DesignUnit {
    fn cast(node: SyntaxNode) -> Option<DesignUnit> {
        match node.kind() {
            NodeKind::DesignUnit => Some(DesignUnit(node)),
            _ => None,
        }
    }

    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}

impl DesignUnit {
    pub fn context_clause(&self) -> Option<ContextClause> {
        self.0.children().filter_map(ContextClause::cast).next()
    }

    pub fn library_unit(&self) -> Option<LibraryUnit> {
        self.0.children().filter_map(LibraryUnit::cast).next()
    }
}

pub enum LibraryUnit {
    Primary(PrimaryUnit),
    Secondary(SecondaryUnit),
}

impl AstNode for LibraryUnit {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::EntityDeclaration => Some(LibraryUnit::Primary(PrimaryUnit::Entity(
                EntityDeclaration(node),
            ))),
            NodeKind::ConfigurationDeclaration => Some(LibraryUnit::Primary(
                PrimaryUnit::Configuration(ConfigurationDeclaration(node)),
            )),
            NodeKind::PackageDeclaration => Some(LibraryUnit::Primary(PrimaryUnit::Package(
                PackageDeclaration(node),
            ))),
            NodeKind::PackageInstantiationDeclaration => Some(LibraryUnit::Primary(
                PrimaryUnit::PackageInstantiation(PackageInstantiationDeclaration(node)),
            )),
            NodeKind::ContextDeclaration => Some(LibraryUnit::Primary(PrimaryUnit::Context(
                ContextDeclaration(node),
            ))),
            _ => None,
        }
    }

    fn raw(&self) -> SyntaxNode {
        todo!()
    }
}

pub enum PrimaryUnit {
    Entity(EntityDeclaration),
    Configuration(ConfigurationDeclaration),
    Package(PackageDeclaration),
    PackageInstantiation(PackageInstantiationDeclaration),
    Context(ContextDeclaration),
}

pub enum SecondaryUnit {
    Architecture(ArchitectureBody),
    Package(PackageBody),
}

pub struct ContextClause(pub(crate) SyntaxNode);

impl AstNode for ContextClause {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::ContextClause => Some(ContextClause(node)),
            _ => None,
        }
    }

    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
