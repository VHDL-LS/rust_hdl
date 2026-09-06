//! Parsing of design files, and abstract design units.
//! The concrete design units (entity, architecture, ...) live in their own file.
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

use crate::parser::error::SyntaxErrKind;
use crate::parser::util::StallGuard;
use crate::parser::Parser;
use crate::syntax::child::Child;
use crate::syntax::node_kind::NodeKind;
use crate::syntax::NodeKind::BindingUseClause;
use crate::tokens::token_kind::Keyword as Kw;
use crate::tokens::token_kind::TokenKind::*;
use crate::tokens::TokenKind;

impl Parser {
    pub(crate) fn design_file(&mut self) {
        let marker = self.start_node(NodeKind::DesignFile);
        if self.next_is(Eof) {
            self.push_err(SyntaxErrKind::Expected(Child::<_, Box<[TokenKind]>>::Node(
                Box::new([NodeKind::DesignUnit]),
            )));
            self.skip();
            marker.complete(self);
            return;
        }
        let mut guard = StallGuard::new();
        while guard.should_continue(self) && self.peek_token() != Eof {
            self.design_unit();
        }
        assert!(self.next_is(Eof), "No EoF token in design file");
        self.skip();
        marker.complete(self);
    }

    pub(crate) fn design_unit(&mut self) {
        self.node(NodeKind::DesignUnit, |p| {
            p.context_clause();
            match_next_token!(p,
                Keyword(Kw::Architecture) => p.architecture(),
                Keyword(Kw::Package) => {
                    if p.next_nth_is(Keyword(Kw::Body), 1) {
                        p.node(NodeKind::SecondaryUnitPackageBody, |p| {
                            p.package_body();
                        });
                    } else if p.next_nth_is(Keyword(Kw::New), 3) {
                        p.node(NodeKind::PackageInstantiationDeclarationPrimaryUnit, |p| {
                            p.package_instantiation();
                        });
                    } else {
                        p.node(NodeKind::PrimaryUnitPackageDeclaration, |p| {
                            p.package();
                        });
                    }
                },
                Keyword(Kw::Entity) => p.entity_declaration(),
                Keyword(Kw::Configuration) => p.configuration_declaration(),
                Keyword(Kw::Context) => p.context_declaration(),
            );
        });
    }

    pub(crate) fn context_declaration(&mut self) {
        self.node(NodeKind::ContextDeclaration, |p| {
            p.context_declaration_preamble();
            p.context_clause();
            p.context_declaration_epilogue();
        });
    }

    pub(crate) fn context_declaration_preamble(&mut self) {
        self.node(NodeKind::ContextDeclarationPreamble, |p| {
            p.expect_kw(Kw::Context);
            p.identifier();
            p.expect_kw(Kw::Is);
        });
    }

    pub(crate) fn context_declaration_epilogue(&mut self) {
        self.node(NodeKind::ContextDeclarationEpilogue, |p| {
            p.expect_kw(Kw::End);
            p.opt_token(Keyword(Kw::Context));
            p.opt_identifier();
            p.expect_token(SemiColon);
        });
    }

    pub(crate) fn binding_indication(&mut self) {
        self.node(NodeKind::BindingIndication, |p| {
            if p.next_is(Keyword(Kw::Use)) {
                p.node(BindingUseClause, |p| {
                    p.skip(); // Use
                    p.entity_aspect();
                });
            }
            if p.next_is(Keyword(Kw::Generic)) {
                p.generic_map_aspect();
            }
            if p.next_is(Keyword(Kw::Port)) {
                p.port_map_aspect();
            }
        });
    }

    pub(crate) fn entity_aspect(&mut self) {
        if self.next_is(Keyword(Kw::Open)) {
            self.skip_into_node(NodeKind::EntityOpenAspect);
        } else if self.next_is(Keyword(Kw::Entity)) {
            self.node(NodeKind::EntityEntityAspect, |p| {
                p.skip();
                p.name();
            });
        } else if self.next_is(Keyword(Kw::Configuration)) {
            self.node(NodeKind::EntityConfigurationAspect, |p| {
                p.skip();
                p.name();
            });
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{test_utils::to_test_text, Parser};

    #[test]
    fn empty_file() {
        assert_recovery_snapshot!("", Parser::design_file);
    }

    #[test]
    fn parse_multiple_entity_declarations() {
        insta::assert_snapshot!(to_test_text(
            Parser::design_file,
            "\
entity myent is
end entity;

entity myent2 is
end entity myent2;

entity myent3 is
end myent3;

entity myent4 is
end;",
        ));
    }

    #[test]
    fn parse_simple_entity() {
        insta::assert_snapshot!(to_test_text(
            Parser::design_file,
            "\
entity my_ent is
begin
end my_ent;

entity my_ent2 is
begin
end entity;
",
        ));
    }

    #[test]
    fn parse_entity_with_context_clause() {
        insta::assert_snapshot!(to_test_text(
            Parser::design_file,
            "\
library ieee;
use ieee.std_logic_1164.all;

entity my_ent is
begin
end my_ent;",
        ));
    }

    #[test]
    fn parse_use_clause() {
        insta::assert_snapshot!(to_test_text(Parser::use_clause, "use lib1.lib2.lib3.all;"));
    }

    #[test]
    fn test_context_clause() {
        insta::assert_snapshot!(to_test_text(
            Parser::context_declaration,
            "\
context ident is
end;"
        ));
        insta::assert_snapshot!(to_test_text(
            Parser::context_declaration,
            "\
context ident is
end context;"
        ));
        insta::assert_snapshot!(to_test_text(
            Parser::context_declaration,
            "\
context ident is
end ident;"
        ));
        insta::assert_snapshot!(to_test_text(
            Parser::context_declaration,
            "\
context ident is
end context ident;"
        ));
    }

    #[test]
    fn test_context_clause_items() {
        insta::assert_snapshot!(to_test_text(
            Parser::context_declaration,
            "\
context ident is
  library foo;
  use foo.bar;
  context foo.ctx;
end context;"
        ));
    }

    #[test]
    fn context_clause_associated_with_design_units() {
        insta::assert_snapshot!(to_test_text(
            Parser::design_file,
            "\
library lib;
use lib.foo;

entity myent is
end entity;"
        ));
    }

    #[test]
    fn parse_package_primary_units() {
        // A top-level package declaration / instantiation must be wrapped in the
        // design-unit variants (`PrimaryUnitPackageDeclaration` /
        // `PackageInstantiationDeclarationPrimaryUnit`), not the declaration
        // variants, so they conform to `DesignUnit`'s `library_unit` choice.
        insta::assert_snapshot!(to_test_text(
            Parser::design_file,
            "\
package pkg is
end package;

package body pkg is
end package body;

package inst is new lib.gen generic map (g => 1);"
        ));
    }

    // MARK: Error recovery

    #[test]
    fn library_clause_missing_semicolon() {
        assert_recovery_snapshot!("library ieee", Parser::design_file);
    }

    #[test]
    fn use_clause_missing_name() {
        assert_recovery_snapshot!("use ;", Parser::design_file);
    }

    #[test]
    fn context_declaration_missing_end() {
        assert_recovery_snapshot!(
            "\
context my_ctx is
  library ieee;",
            Parser::design_file
        );
    }

    #[test]
    fn design_file_top_level_body_keyword() {
        assert_recovery_snapshot!("body", Parser::design_file);
    }
}
