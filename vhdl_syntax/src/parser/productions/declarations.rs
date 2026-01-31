// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::Parser;
use crate::syntax::NodeKind::{self, *};
use crate::tokens::TokenKind::*;
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

impl Parser {
    pub(crate) fn opt_declarative_part(&mut self) {
        if self.peek_token().is_some_and(is_start_of_declarative_part) {
            self.declarations();
        }
    }

    pub(crate) fn declarations(&mut self) {
        self.start_node(Declarations);
        while let Some(token) = self.peek_token() {
            match token {
                Keyword(Kw::Begin | Kw::End) => break,
                Keyword(Kw::Type) => self.type_declaration(),
                Keyword(Kw::Subtype) => self.subtype_declaration(),
                Keyword(Kw::Component) => self.component_declaration(),
                Keyword(Kw::Impure | Kw::Pure | Kw::Function | Kw::Procedure) => {
                    // TODO: Brittle
                    if self.next_nth_is(Keyword(Kw::New), 3) {
                        self.subprogram_instantiation_declaration();
                    } else {
                        self.subprogram_declaration_or_body()
                    }
                }
                Keyword(Kw::Package) => self.package_instantiation_declaration(),
                Keyword(Kw::For) => self.configuration_specification(),
                Keyword(Kw::File) => self.file_declaration(),
                Keyword(Kw::Shared | Kw::Variable) => self.variable_declaration(),
                Keyword(Kw::Constant) => self.constant_declaration(),
                Keyword(Kw::Signal) => self.signal_declaration(),
                Keyword(Kw::Attribute) => self.attribute_declaration_or_specification(),
                Keyword(Kw::Use) => self.use_clause_declaration(),
                Keyword(Kw::Alias) => self.alias_declaration(),
                _ => {
                    self.skip();
                    self.expect_tokens_err([Keyword(Kw::Type)])
                }
            }
        }
        self.end_node();
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
        let checkpoint = self.checkpoint();
        self.start_node(ComponentConfigurationPreamble);
        self.expect_kw(Kw::For);
        self.component_specification();
        self.end_node();
        self.binding_indication();
        if self.next_is(Keyword(Kw::Use)) && self.next_nth_is(Keyword(Kw::Vunit), 1) {
            self.start_node_at(checkpoint, NodeKind::CompoundConfigurationSpecification);
            self.start_node(CompoundConfigurationSpecificationItems);
            while self.next_is(Keyword(Kw::Use)) {
                self.start_node(SemiColonTerminatedVerificationUnitBindingIndication);
                self.verification_unit_binding_indication();
                self.expect_token(SemiColon);
                self.end_node();
            }
            self.end_node();
            self.start_node(ComponentConfigurationEpilogue);
            self.expect_tokens([Keyword(Kw::End), Keyword(Kw::For), SemiColon]);
            self.end_node();
        } else {
            self.start_node_at(checkpoint, NodeKind::SimpleConfigurationSpecification);
            self.expect_token(SemiColon);
            if self.next_is(Keyword(Kw::End)) {
                self.start_node(ComponentConfigurationEpilogue);
                self.expect_tokens([Keyword(Kw::End), Keyword(Kw::For), SemiColon]);
                self.end_node();
            }
            self.end_node();
        }
    }

    pub fn component_specification(&mut self) {
        self.start_node(NodeKind::ComponentSpecification);
        match self.peek_token() {
            Some(Keyword(Kw::All)) => {
                self.skip_into_node(NodeKind::InstantiationListAll);
            }
            Some(Keyword(Kw::Others)) => {
                self.skip_into_node(NodeKind::InstantiationListOthers);
            }
            Some(Identifier) => {
                self.start_node(NodeKind::InstantiationListList);
                self.skip();
                while self.next_is(Comma) {
                    self.skip();
                    self.identifier();
                }
                self.end_node();
            }
            _ => self.expect_tokens_err([Keyword(Kw::All), Keyword(Kw::Others), Identifier]),
        }
        self.expect_token(Colon);
        self.name();
        self.end_node();
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{test_utils::to_test_text, Parser};

    #[test]
    fn package_instantiation() {
        insta::assert_snapshot!(to_test_text(
            Parser::package_instantiation_declaration,
            "package ident is new lib.foo.bar;",
        ));
    }

    #[test]
    fn package_instantiation_generic_map() {
        insta::assert_snapshot!(to_test_text(
            Parser::package_instantiation_declaration,
            "\
package ident is new lib.foo.bar
  generic map (
    foo => bar
  );",
        ));
    }
}
