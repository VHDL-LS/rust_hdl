// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com
use crate::parser::Parser;
/// Parsing of entity declarations
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
        // unimplemented
    }
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
        }
        .parse(Parser::entity);
        assert_eq!(
            entity.test_text(),
            "\
EntityDeclaration
  Keyword(Entity)
  Identifier 'my_ent'
  Keyword(Is)
  Keyword(Begin)
  Keyword(End)
  Identifier 'my_ent'
  SemiColon
"
        );
    }
}
