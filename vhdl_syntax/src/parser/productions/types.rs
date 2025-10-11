// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind::*;
use crate::tokens::Keyword as Kw;
use crate::tokens::TokenKind::*;
use crate::tokens::TokenStream;

impl<T: TokenStream> Parser<T> {
    pub fn type_declaration(&mut self) {
        let checkpoint = self.checkpoint();
        self.expect_kw(Kw::Type);
        self.identifier();
        if self.opt_token(SemiColon) {
            self.start_node_at(checkpoint, IncompleteTypeDeclaration);
            self.end_node();
            return;
        }
        self.start_node_at(checkpoint, FullTypeDeclaration);
        self.expect_kw(Kw::Is);
        self.type_definition();
        self.expect_token(SemiColon);
        self.end_node();
    }

    pub fn type_definition(&mut self) {
        match_next_token!(self,
            Keyword(Kw::Range) => self.numeric_type_definition(),
            Keyword(Kw::Access) => {
                self.start_node(AccessTypeDefinition);
                self.skip();
                self.subtype_indication();
                self.end_node();
            },
            Keyword(Kw::Protected) => self.protected_type_definition(),
            Keyword(Kw::File) => self.file_type_definition(),
            Keyword(Kw::Array) => self.array_type_definition(),
            Keyword(Kw::Record) => self.record_type_definition(),
            LeftPar => self.enumeration_type_definition()
        )
    }

    pub fn protected_type_definition(&mut self) {
        let checkpoint = self.checkpoint();
        self.expect_kw(Kw::Protected);
        let is_body = self.opt_token(Keyword(Kw::Body));
        self.declarative_part();
        self.expect_tokens([Keyword(Kw::End), Keyword(Kw::Protected)]);
        if is_body {
            self.expect_token(Keyword(Kw::Body));
        }
        self.opt_identifier();
        if is_body {
            self.start_node_at(checkpoint, ProtectedTypeBody);
        } else {
            self.start_node_at(checkpoint, ProtectedTypeDeclaration);
        }
        self.end_node();
    }

    pub fn file_type_definition(&mut self) {
        self.start_node(FileTypeDefinition);
        self.expect_tokens([Keyword(Kw::File), Keyword(Kw::Of)]);
        self.type_mark();
        self.end_node();
    }

    pub fn access_type_definition(&mut self) {
        self.start_node(AccessTypeDefinition);
        self.expect_kw(Kw::Access);
        self.subtype_indication();
        self.end_node();
    }

    pub fn subtype_declaration(&mut self) {
        self.start_node(SubtypeDeclaration);
        self.expect_kw(Kw::Subtype);
        self.identifier();
        self.expect_kw(Kw::Is);
        self.subtype_indication();
        self.expect_token(SemiColon);
        self.end_node();
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils::check;
    use crate::parser::Parser;

    #[test]
    fn incomplete_type_declaration() {
        check(
            Parser::type_declaration,
            "type incomplete_type;",
            "\
IncompleteTypeDeclaration
  Keyword(Type)
  Identifier 'incomplete_type'
  SemiColon
",
        );
    }

    #[test]
    fn file_type_declaration() {
        check(
            Parser::type_declaration,
            "type IntegerFile is file of integer;",
            "\
FullTypeDeclaration
  Keyword(Type)
  Identifier 'IntegerFile'
  Keyword(Is)
  FileTypeDefinition
    Keyword(File)
    Keyword(Of)
    Name
      Identifier 'integer'
  SemiColon
",
        );

        check(
            Parser::type_declaration,
            "type sl_file is file of ieee.std_logic_1164.std_ulogic;",
            "\
FullTypeDeclaration
  Keyword(Type)
  Identifier 'sl_file'
  Keyword(Is)
  FileTypeDefinition
    Keyword(File)
    Keyword(Of)
    Name
      Identifier 'ieee'
      SelectedName
        Dot
        Identifier 'std_logic_1164'
      SelectedName
        Dot
        Identifier 'std_ulogic'
  SemiColon
",
        );
    }

    #[test]
    fn access_type_definition() {
        check(
            Parser::type_declaration,
            "type str_ptr_t is access string;",
            "\
FullTypeDeclaration
  Keyword(Type)
  Identifier 'str_ptr_t'
  Keyword(Is)
  AccessTypeDefinition
    Keyword(Access)
    Identifier 'string'
  SemiColon
",
        );
    }

    #[test]
    fn protected_type_declaration() {
        check(
            Parser::type_declaration,
            "type p_t is protected end protected;",
            "\
FullTypeDeclaration
  Keyword(Type)
  Identifier 'p_t'
  Keyword(Is)
  ProtectedTypeDeclaration
    Keyword(Protected)
    Keyword(End)
    Keyword(Protected)
  SemiColon
",
        );

        check(
            Parser::type_declaration,
            "type p_t is protected end protected p_t;",
            "\
FullTypeDeclaration
  Keyword(Type)
  Identifier 'p_t'
  Keyword(Is)
  ProtectedTypeDeclaration
    Keyword(Protected)
    Keyword(End)
    Keyword(Protected)
    Identifier 'p_t'
  SemiColon
",
        );

        // TODO: Test protected types with content
    }

    #[test]
    fn protected_type_body() {
        check(
            Parser::type_declaration,
            "type p_t is protected body end protected body;",
            "\
FullTypeDeclaration
  Keyword(Type)
  Identifier 'p_t'
  Keyword(Is)
  ProtectedTypeBody
    Keyword(Protected)
    Keyword(Body)
    Keyword(End)
    Keyword(Protected)
    Keyword(Body)
  SemiColon
",
        );
        check(
            Parser::type_declaration,
            "type p_t is protected body end protected body p_t;",
            "\
FullTypeDeclaration
  Keyword(Type)
  Identifier 'p_t'
  Keyword(Is)
  ProtectedTypeBody
    Keyword(Protected)
    Keyword(Body)
    Keyword(End)
    Keyword(Protected)
    Keyword(Body)
    Identifier 'p_t'
  SemiColon
",
        );

        // TODO: Test protected types with content
    }
}
