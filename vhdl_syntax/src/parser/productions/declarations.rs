// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::Parser;
use crate::tokens::TokenKind::*;
use crate::syntax::NodeKind::*;
use crate::tokens::TokenStream;
use crate::tokens::{Keyword as Kw, TokenKind};

fn is_start_of_declarative_part(token_kind: TokenKind) -> bool {
    matches!(
        token_kind,
        Keyword(
            Kw::Use
                | Kw::Type
                | Kw::Subtype
                | Kw::Shared
                | Kw::Constant
                | Kw::Signal
                | Kw::Variable
                | Kw::File
                | Kw::Component
                | Kw::Attribute
                | Kw::Alias
                | Kw::Impure
                | Kw::Pure
                | Kw::Function
                | Kw::Procedure
                | Kw::Package
                | Kw::For
                | Kw::View
                | Kw::Begin
        )
    )
}

impl<T: TokenStream> Parser<T> {
    pub(crate) fn opt_declarative_part(&mut self) {
        if self.peek_token().is_some_and(is_start_of_declarative_part) {
            self.declarative_part();
        }
    }

    pub(crate) fn declarative_part(&mut self) {
        while let Some(token) = self.peek_token() {
            match token {
                Keyword(Kw::Begin | Kw::End) => break,
                Keyword(Kw::Type) => self.type_declaration(),
                Keyword(Kw::Subtype) => self.subtype_declaration(),
                Keyword(Kw::Component) => self.component_declaration(),
                Keyword(Kw::Impure | Kw::Pure | Kw::Function | Kw::Procedure) => {
                    self.subprogram_declaration_or_body()
                }
                Keyword(Kw::Package) => self.package_instantiation_declaration(),
                Keyword(Kw::For) => self.configuration_specification(),
                Keyword(Kw::File) => self.file_declaration(),
                Keyword(Kw::Shared | Kw::Variable) => self.variable_declaration(),
                Keyword(Kw::Constant) => self.constant_declaration(),
                Keyword(Kw::Signal) => self.signal_declaration(),
                Keyword(Kw::Attribute) => self.attribute_declaration(),
                Keyword(Kw::Use) => self.use_clause_declaration(),
                Keyword(Kw::Alias) => self.alias_declaration(),
                Keyword(Kw::View) => self.view_declaration(),
                _ => {
                    self.skip();
                    self.expect_tokens_err([Keyword(Kw::Type)])
                },
            }
        }
    }

    pub fn use_clause_declaration(&mut self) {
        self.start_node(UseClauseDeclaration);
        self.use_clause();
        self.end_node();
    }

    pub fn package_declaration(&mut self) {
        self.start_node(PackageDeclaration);
        self.package();
        self.end_node();
    }

    pub fn configuration_specification(&mut self) {
        todo!()
    }

    pub fn view_declaration(&mut self) {
        todo!()
    }
}
