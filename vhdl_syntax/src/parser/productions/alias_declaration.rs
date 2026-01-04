// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind::*;
use crate::tokens::token_kind::Keyword as Kw;
use crate::tokens::token_kind::TokenKind::*;

impl Parser {
    pub fn alias_declaration(&mut self) {
        self.start_node(AliasDeclaration);
        self.expect_kw(Kw::Alias);
        self.alias_designator();
        if self.opt_token(Colon) {
            self.subtype_indication();
        }
        self.expect_token(Keyword(Kw::Is));
        self.name();
        if self.next_is(LeftSquare) {
            self.signature();
        }
        self.expect_token(SemiColon);
        self.end_node();
    }

    pub fn alias_designator(&mut self) {
        self.expect_one_of_tokens([Identifier, StringLiteral, CharacterLiteral]);
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils::*;
    use crate::parser::Parser;

    #[test]
    fn parse_simple_alias() {
        insta::assert_snapshot!(to_test_text(
            Parser::alias_declaration,
            "alias foo is name;",
        ));
    }

    #[test]
    fn parse_alias_with_subtype_indication() {
        insta::assert_snapshot!(to_test_text(
            Parser::alias_declaration,
            "alias foo : vector(0 to 1) is name;",
        ));
    }

    #[test]
    fn parse_alias_with_signature() {
        insta::assert_snapshot!(to_test_text(
            Parser::alias_declaration,
            "alias foo is name [return natural];",
        ));
    }

    #[test]
    fn parse_alias_with_operator_symbol() {
        insta::assert_snapshot!(to_test_text(
            Parser::alias_declaration,
            "alias \"and\" is name;",
        ));
    }

    #[test]
    fn parse_alias_with_character() {
        insta::assert_snapshot!(to_test_text(Parser::alias_declaration, "alias 'c' is 'b';",));
    }
}
