// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::marker::CompletedMarker;
use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind::*;
use crate::syntax::{
    AstNode, ProtectedTypeBodyDeclarativeItemSyntax, ProtectedTypeDeclarativeItemSyntax,
};
use crate::tokens::Keyword as Kw;
use crate::tokens::TokenKind::*;

impl Parser {
    pub(crate) fn type_declaration(&mut self) -> CompletedMarker {
        let unknown = self.start_unknown();
        self.expect_kw(Kw::Type);
        self.identifier();
        if self.opt_token(SemiColon) {
            return unknown.complete(self, IncompleteTypeDeclaration);
        }
        let marker = unknown.resolve(self, FullTypeDeclaration);
        self.expect_kw(Kw::Is);
        self.type_definition();
        self.expect_token(SemiColon);
        marker.complete(self)
    }

    pub(crate) fn type_definition(&mut self) {
        match_next_token!(self,
            Keyword(Kw::Range) => self.numeric_type_definition(),
            Keyword(Kw::Access) => {
                self.node(AccessTypeDefinition, |p| {
                    p.skip();
                    p.subtype_indication();
                });
            },
            Keyword(Kw::Protected) => self.protected_type_definition(),
            Keyword(Kw::File) => self.file_type_definition(),
            Keyword(Kw::Array) => self.array_type_definition(),
            Keyword(Kw::Record) => self.record_type_definition(),
            LeftPar => self.enumeration_type_definition()
        )
    }

    pub(crate) fn protected_type_definition(&mut self) {
        let is_body =
            self.next_is(Keyword(Kw::Protected)) && self.next_nth_is(Keyword(Kw::Body), 1);
        let (definition, preamble, epilogue) = if is_body {
            (
                ProtectedTypeBody,
                ProtectedTypeBodyPreamble,
                ProtectedTypeBodyEpilogue,
            )
        } else {
            (
                ProtectedTypeDeclaration,
                ProtectedPreamble,
                ProtectedTypeDeclarationEpilogue,
            )
        };
        self.node(definition, |p| {
            p.node(preamble, |p| {
                if is_body {
                    p.expect_tokens([Keyword(Kw::Protected), Keyword(Kw::Body)]);
                } else {
                    p.expect_kw(Kw::Protected);
                }
            });
            if is_body {
                p.protected_type_body_declarative_part();
            } else {
                p.protected_type_declarative_part();
            }
            p.node(epilogue, |p| {
                p.expect_tokens([Keyword(Kw::End), Keyword(Kw::Protected)]);
                if is_body {
                    p.expect_token(Keyword(Kw::Body));
                }
                p.opt_identifier();
            });
        });
    }

    pub(crate) fn protected_type_declarative_part(&mut self) {
        self.declarations(
            ProtectedTypeDeclarativePart,
            ProtectedTypeDeclarativeItemSyntax::META,
        );
    }

    pub(crate) fn protected_type_body_declarative_part(&mut self) {
        self.declarations(
            ProtectedTypeBodyDeclarativePart,
            ProtectedTypeBodyDeclarativeItemSyntax::META,
        );
    }

    pub(crate) fn file_type_definition(&mut self) {
        self.node(FileTypeDefinition, |p| {
            p.expect_tokens([Keyword(Kw::File), Keyword(Kw::Of)]);
            p.type_mark();
        });
    }

    pub(crate) fn subtype_declaration(&mut self) -> CompletedMarker {
        self.node(SubtypeDeclaration, |p| {
            p.expect_kw(Kw::Subtype);
            p.identifier();
            p.expect_kw(Kw::Is);
            p.subtype_indication();
            p.expect_token(SemiColon);
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils::to_test_text;
    use crate::parser::Parser;

    #[test]
    fn incomplete_type_declaration() {
        insta::assert_snapshot!(to_test_text(
            Parser::type_declaration,
            "type incomplete_type;"
        ));
    }

    #[test]
    fn file_type_declaration() {
        insta::assert_snapshot!(to_test_text(
            Parser::type_declaration,
            "type IntegerFile is file of integer;"
        ));

        insta::assert_snapshot!(to_test_text(
            Parser::type_declaration,
            "type sl_file is file of ieee.std_logic_1164.std_ulogic;"
        ));
    }

    #[test]
    fn access_type_definition() {
        insta::assert_snapshot!(to_test_text(
            Parser::type_declaration,
            "type str_ptr_t is access string;"
        ));
    }

    #[test]
    fn protected_type_declaration() {
        insta::assert_snapshot!(to_test_text(
            Parser::type_declaration,
            "type p_t is protected end protected;",
        ));

        insta::assert_snapshot!(to_test_text(
            Parser::type_declaration,
            "type p_t is protected end protected p_t;"
        ));
        insta::assert_snapshot!(to_test_text(
            Parser::type_declaration,
            "\
type foo is protected
  procedure proc;
  function fun return ret;
end protected;"
        ));
    }

    #[test]
    fn protected_type_body() {
        insta::assert_snapshot!(to_test_text(
            Parser::type_declaration,
            "type p_t is protected body end protected body;"
        ));
        insta::assert_snapshot!(to_test_text(
            Parser::type_declaration,
            "type p_t is protected body end protected body p_t;"
        ));
        insta::assert_snapshot!(to_test_text(
            Parser::type_declaration,
            "\
type foo is protected body
  variable foo : natural;
  procedure proc is
  begin
  end;
end protected body;"
        ));
    }

    #[test]
    fn test_parse_subtype_declaration() {
        insta::assert_snapshot!(to_test_text(
            Parser::subtype_declaration,
            "subtype vec_t is integer_vector(2-1 downto 0);"
        ));
    }

    // MARK: Error recovery

    #[test]
    fn type_missing_is() {
        assert_recovery_snapshot!(
            "type state_t (idle, running, done);",
            Parser::type_declaration
        );
    }

    #[test]
    fn type_missing_definition() {
        assert_recovery_snapshot!("type state_t is ;", Parser::type_declaration);
    }

    #[test]
    fn subtype_missing_indication() {
        assert_recovery_snapshot!("subtype small_int is ;", Parser::subtype_declaration);
    }
}
