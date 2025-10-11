//! Parsing of entity declarations
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com
use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind::*;
use crate::tokens::token_kind::Keyword as Kw;
use crate::tokens::token_kind::TokenKind::*;
use crate::tokens::TokenStream;

impl<T: TokenStream> Parser<T> {
    pub fn entity(&mut self) {
        self.start_node(EntityDeclaration);
        self.expect_token(Keyword(Kw::Entity));
        self.identifier();
        self.expect_token(Keyword(Kw::Is));
        self.entity_header();
        if self.opt_token(Keyword(Kw::Begin)) {
            self.labeled_concurrent_statements();
        }
        self.expect_token(Keyword(Kw::End));
        self.opt_token(Keyword(Kw::Entity));
        self.opt_identifier();
        self.expect_token(SemiColon);
        self.end_node();
    }

    fn entity_header(&mut self) {
        self.start_node(EntityHeader);
        self.opt_generic_clause();
        self.opt_port_clause();
        self.end_node();
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils::check;
    use crate::parser::Parser;

    #[test]
    fn parse_simple_entity() {
        check(
            Parser::entity,
            "\
entity my_ent is
begin
end my_ent;
",
            "\
EntityDeclaration
  Keyword(Entity)
  Identifier 'my_ent'
  Keyword(Is)
  EntityHeader
  Keyword(Begin)
  Keyword(End)
  Identifier 'my_ent'
  SemiColon
",
        );
    }

    #[test]
    fn parse_entity_with_generics() {
        check(
            Parser::entity,
            "\
entity my_ent is
    generic();
begin
end my_ent;
",
            "\
EntityDeclaration
  Keyword(Entity)
  Identifier 'my_ent'
  Keyword(Is)
  EntityHeader
    GenericClause
      Keyword(Generic)
      LeftPar
      InterfaceList
      RightPar
      SemiColon
  Keyword(Begin)
  Keyword(End)
  Identifier 'my_ent'
  SemiColon
",
        );
    }

    #[test]
    fn parse_entity_with_ports() {
        check(
            Parser::entity,
            "\
entity my_ent is
    port();
begin
end my_ent;
",
            "\
EntityDeclaration
  Keyword(Entity)
  Identifier 'my_ent'
  Keyword(Is)
  EntityHeader
    PortClause
      Keyword(Port)
      LeftPar
      InterfaceList
      RightPar
      SemiColon
  Keyword(Begin)
  Keyword(End)
  Identifier 'my_ent'
  SemiColon
",
        );
    }

    #[test]
    fn parse_entity_with_generics_and_ports() {
        check(
            Parser::entity,
            "\
entity my_ent is
    generic();
    port();
begin
end my_ent;
",
            "\
EntityDeclaration
  Keyword(Entity)
  Identifier 'my_ent'
  Keyword(Is)
  EntityHeader
    GenericClause
      Keyword(Generic)
      LeftPar
      InterfaceList
      RightPar
      SemiColon
    PortClause
      Keyword(Port)
      LeftPar
      InterfaceList
      RightPar
      SemiColon
  Keyword(Begin)
  Keyword(End)
  Identifier 'my_ent'
  SemiColon
",
        );
    }
    #[test]
    fn parse_entity_with_filled_generics_and_ports() {
        check(
            Parser::entity,
            "\
entity my_ent is
    generic(constant a: in bit);
    port(
        b, c : out std_logic;
        signal d : linkage boolean
    );
begin
end my_ent;
",
            "\
EntityDeclaration
  Keyword(Entity)
  Identifier 'my_ent'
  Keyword(Is)
  EntityHeader
    GenericClause
      Keyword(Generic)
      LeftPar
      InterfaceList
        InterfaceConstantDeclaration
          Keyword(Constant)
          IdentifierList
            Identifier 'a'
          Colon
          Keyword(In)
          Identifier 'bit'
      RightPar
      SemiColon
    PortClause
      Keyword(Port)
      LeftPar
      InterfaceList
        InterfaceConstantDeclaration
          IdentifierList
            Identifier 'b'
            Comma
            Identifier 'c'
          Colon
          Keyword(Out)
          Identifier 'std_logic'
        SemiColon
        InterfaceSignalDeclaration
          Keyword(Signal)
          IdentifierList
            Identifier 'd'
          Colon
          Keyword(Linkage)
          Identifier 'boolean'
      RightPar
      SemiColon
  Keyword(Begin)
  Keyword(End)
  Identifier 'my_ent'
  SemiColon
",
        );
    }
}
