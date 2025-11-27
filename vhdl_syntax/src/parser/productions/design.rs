//! Parsing of design files, and abstract design units.
//! The concrete design units (entity, architecture, ...) live in their own file.
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com
use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind;
use crate::tokens::token_kind::Keyword as Kw;
use crate::tokens::token_kind::TokenKind::*;
use crate::tokens::TokenStream;

impl<T: TokenStream> Parser<T> {
    pub fn design_file(&mut self) {
        self.start_node(NodeKind::DesignFile);
        loop {
            self.design_unit();
            if self.tokenizer.peek_next().is_none() {
                break;
            }
        }
        self.end_node();
    }

    pub fn design_unit(&mut self) {
        self.start_node(NodeKind::DesignUnit);
        
        self.context_clause();
        match self.peek_token() {
            Some(Keyword(Kw::Architecture)) => self.architecture(),
            Some(Keyword(Kw::Package)) => {
                if self.next_nth_is(Keyword(Kw::Body), 1) {
                    self.start_node(NodeKind::SecondaryUnitPackageBody);
                    self.package_body();
                    self.end_node();
                } else if self.next_nth_is(Keyword(Kw::New), 3) {
                    self.package_instantiation_declaration();
                } else {
                    self.package_declaration();
                }
            }
            Some(Keyword(Kw::Entity)) => self.entity(),
            Some(Keyword(Kw::Configuration)) => todo!(),
            Some(Keyword(Kw::Context)) => self.context_declaration(),
            Some(_) => todo!("token: {:?}", self.tokenizer.peek(0).unwrap()),
            None => {},
        }
        self.end_node();
    }

    pub fn context_declaration(&mut self) {
      self.start_node(NodeKind::ContextDeclaration);
      self.expect_kw(Kw::Context);
      self.identifier();
      self.expect_kw(Kw::Is);
      self.context_clause();
      self.expect_kw(Kw::End);
      self.opt_token(Keyword(Kw::Context));
      self.opt_identifier();
      self.expect_token(SemiColon);
      self.end_node();
    }

    pub fn context_clause(&mut self) {
        self.start_node(NodeKind::ContextClause);
        loop {
            match self.peek_token() {
              Some(Keyword(Kw::Use)) => self.use_clause(),
              Some(Keyword(Kw::Library)) => self.library_clause(),
              Some(Keyword(Kw::Context)) => {
                if !self.next_nth_is(Keyword(Kw::Is), 2) {
                  self.context_reference()
                } else {
                    break;
                }
              },
              _ => break
            }
        }
        self.end_node();
    }

    pub fn library_clause(&mut self) {
        self.start_node(NodeKind::LibraryClause);
        self.expect_kw(Kw::Library);
        self.identifier_list();
        self.expect_token(SemiColon);
        self.end_node();
    }

    pub fn use_clause(&mut self) {
        self.start_node(NodeKind::UseClause);
        self.expect_kw(Kw::Use);
        self.name_list();
        self.expect_token(SemiColon);
        self.end_node();
    }

    pub fn context_reference(&mut self) {
        self.start_node(NodeKind::ContextReference);
        self.expect_kw(Kw::Context);
        self.name_list();
        self.expect_token(SemiColon);
        self.end_node();
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{test_utils::check, Parser};

    #[test]
    fn parse_simple_entity() {
        check(
            Parser::design_file,
            "\
entity my_ent is
begin
end my_ent;

entity my_ent2 is
begin
end entity;
",
            "\
DesignFile
  DesignUnit
    ContextClause
    EntityDeclaration
      Keyword(Entity)
      Identifier 'my_ent'
      Keyword(Is)
      EntityHeader
      Keyword(Begin)
      Keyword(End)
      Identifier 'my_ent'
      SemiColon
  DesignUnit
    ContextClause
    EntityDeclaration
      Keyword(Entity)
      Identifier 'my_ent2'
      Keyword(Is)
      EntityHeader
      Keyword(Begin)
      Keyword(End)
      Keyword(Entity)
      SemiColon
",
        );
    }

    #[test]
    fn parse_entity_with_context_clause() {
        check(
            Parser::design_file,
            "\
            library ieee;
            use ieee.std_logic_1164.all;

            entity my_ent is
            begin
            end my_ent;
        ",
            "\
DesignFile
  DesignUnit
    ContextClause
      LibraryClause
        Keyword(Library)
        IdentifierList
          Identifier 'ieee'
        SemiColon
      UseClause
        Keyword(Use)
        NameList
          Name
            Identifier 'ieee'
            SelectedName
              Dot
              Identifier 'std_logic_1164'
            SelectedName
              Dot
              Keyword(All)
        SemiColon
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
    fn parse_use_clause() {
        check(
            Parser::use_clause,
            "use lib1.lib2.lib3.all;",
            "\
UseClause
  Keyword(Use)
  NameList
    Name
      Identifier 'lib1'
      SelectedName
        Dot
        Identifier 'lib2'
      SelectedName
        Dot
        Identifier 'lib3'
      SelectedName
        Dot
        Keyword(All)
  SemiColon
",
        );
    }
}
