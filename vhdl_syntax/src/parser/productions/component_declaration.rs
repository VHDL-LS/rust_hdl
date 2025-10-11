// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind::*;
use crate::tokens::token_kind::Keyword as Kw;
use crate::tokens::TokenKind::*;
use crate::tokens::TokenStream;

impl<T: TokenStream> Parser<T> {
    pub fn component_declaration(&mut self) {
        self.start_node(ComponentDeclaration);
        self.expect_token(Keyword(Kw::Component));
        self.identifier();
        self.opt_token(Keyword(Kw::Is));
        self.opt_generic_clause();
        self.opt_port_clause();
        self.expect_tokens([Keyword(Kw::End), Keyword(Kw::Component)]);
        self.opt_identifier();
        self.expect_token(SemiColon);
        self.end_node();
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils::check;
    use crate::parser::Parser;

    #[test]
    fn simple_components() {
        check(
            Parser::component_declaration,
            "\
component foo
end component;
",
            "\
ComponentDeclaration
  Keyword(Component)
  Identifier 'foo'
  Keyword(End)
  Keyword(Component)
  SemiColon
            ",
        );
        check(
            Parser::component_declaration,
            "\
component foo is
end component;
",
            "\
ComponentDeclaration
  Keyword(Component)
  Identifier 'foo'
  Keyword(Is)
  Keyword(End)
  Keyword(Component)
  SemiColon
            ",
        );
        check(
            Parser::component_declaration,
            "\
component foo is
end component foo;
",
            "\
ComponentDeclaration
  Keyword(Component)
  Identifier 'foo'
  Keyword(Is)
  Keyword(End)
  Keyword(Component)
  Identifier 'foo'
  SemiColon
            ",
        );
    }

    #[test]
    fn components_with_generics() {
        check(
            Parser::component_declaration,
            "\
component foo is
  generic (
    foo : natural
  );
end component;
",
            "\
ComponentDeclaration
  Keyword(Component)
  Identifier 'foo'
  Keyword(Is)
  GenericClause
    Keyword(Generic)
    LeftPar
    InterfaceList
      InterfaceConstantDeclaration
        IdentifierList
          Identifier 'foo'
        Colon
        Identifier 'natural'
    RightPar
    SemiColon
  Keyword(End)
  Keyword(Component)
  SemiColon
            ",
        );
    }

    #[test]
    fn components_with_port() {
        check(
            Parser::component_declaration,
            "\
component foo is
  port (
    foo : natural
  );
end component;
",
            "\
ComponentDeclaration
  Keyword(Component)
  Identifier 'foo'
  Keyword(Is)
  PortClause
    Keyword(Port)
    LeftPar
    InterfaceList
      InterfaceConstantDeclaration
        IdentifierList
          Identifier 'foo'
        Colon
        Identifier 'natural'
    RightPar
    SemiColon
  Keyword(End)
  Keyword(Component)
  SemiColon
            ",
        );
    }
}
