// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com
use crate::parser::Parser;
/// Parsing of design files, and abstract design units.
/// The concrete design units (entity, architecture, ...) live in their own file.
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
        if !self.tokenizer.has_next() {
            self.eof_err();
            return;
        }
        self.start_node(NodeKind::DesignUnit);
        self.context_clause();
        match self.tokenizer.peek_next() {
            Some(tok) => match tok.kind() {
                Keyword(Kw::Entity) => self.entity(),
                Keyword(Kw::Configuration) => todo!(),
                Keyword(Kw::Package) => todo!(),
                Keyword(Kw::Context) => todo!(),
                Keyword(Kw::Architecture) => todo!(),
                _ => self.expect_tokens_err([Keyword(Kw::Entity)]),
            },
            None => self.eof_err(),
        }
        self.end_node();
    }

    pub fn context_clause(&mut self) {}
}

#[cfg(test)]
mod tests {
    use crate::parser::{CanParse, Parser};
    use crate::vhdl;
    use pretty_assertions::assert_eq;

    #[test]
    fn parse_simple_entity() {
        let (entity, _) = vhdl! {
            entity my_ent is
            begin
            end my_ent;

            entity my_ent2 is
            begin
            end entity;
        }
        .parse(Parser::design_file);
        assert_eq!(
            entity.test_text(),
            "\
DesignFile
  DesignUnit
    EntityDeclaration
      Keyword(Entity)
      Identifier 'my_ent'
      Keyword(Is)
      Keyword(Begin)
      Keyword(End)
      Identifier 'my_ent'
      SemiColon
  DesignUnit
    EntityDeclaration
      Keyword(Entity)
      Identifier 'my_ent2'
      Keyword(Is)
      Keyword(Begin)
      Keyword(End)
      Keyword(Entity)
      SemiColon
"
        );
    }
}
