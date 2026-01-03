// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com
/// Parsing of object declarations (LRM §6.4.2)
use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind::*;
use crate::tokens::token_kind::Keyword as Kw;
use crate::tokens::TokenKind::*;
use crate::tokens::TokenStream;

impl<T: TokenStream> Parser<T> {
    pub fn constant_declaration(&mut self) {
        self.start_node(ConstantDeclaration);
        self.expect_token(Keyword(Kw::Constant));
        self.identifier_list();
        self.expect_token(Colon);
        self.subtype_indication();
        if self.opt_token(ColonEq) {
            self.expression();
        }
        self.expect_token(SemiColon);
        self.end_node();
    }

    pub fn signal_declaration(&mut self) {
        self.start_node(SignalDeclaration);
        self.expect_token(Keyword(Kw::Signal));
        self.identifier_list();
        self.expect_token(Colon);
        self.subtype_indication();
        self.opt_tokens([Keyword(Kw::Register), Keyword(Kw::Bus)]);
        if self.opt_token(ColonEq) {
            self.expression();
        }
        self.expect_token(SemiColon);
        self.end_node();
    }

    pub fn variable_declaration(&mut self) {
        self.start_node(VariableDeclaration);
        self.opt_token(Keyword(Kw::Shared));
        self.expect_token(Keyword(Kw::Variable));
        self.identifier_list();
        self.expect_token(Colon);
        self.subtype_indication();
        if self.opt_token(ColonEq) {
            self.expression();
        }
        self.expect_token(SemiColon);
        self.end_node();
    }

    pub fn file_declaration(&mut self) {
        self.start_node(FileDeclaration);
        self.expect_token(Keyword(Kw::File));
        self.identifier_list();
        self.expect_token(Colon);
        self.subtype_indication();
        self.opt_file_open_information();
        self.expect_token(SemiColon);
        self.end_node();
    }

    fn opt_file_open_information(&mut self) -> bool {
        if !self.next_is(Keyword(Kw::Open)) && !self.next_is(Keyword(Kw::Is)) {
            return false;
        }

        self.start_node(FileOpenInformation);
        if self.opt_token(Keyword(Kw::Open)) {
            self.expression();
        }
        self.expect_token(Keyword(Kw::Is));
        self.expression();
        self.end_node();
        true
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils::to_test_text;
    use crate::parser::Parser;

    #[test]
    fn constant_declaration() {
        insta::assert_snapshot!(to_test_text(
            Parser::constant_declaration,
            "constant C1: std_ulogic;"
        ));
        insta::assert_snapshot!(to_test_text(
            Parser::constant_declaration,
            "constant C1,C2,C3: std_ulogic := '0';"
        ));
    }

    #[test]
    fn signal_declaration() {
        insta::assert_snapshot!(to_test_text(Parser::signal_declaration, "signal s1: bit;"));
        insta::assert_snapshot!(to_test_text(
            Parser::signal_declaration,
            "signal s, ss, sss : natural := 1;"
        ));
        insta::assert_snapshot!(to_test_text(
            Parser::signal_declaration,
            "signal reg_s: bit register;"
        ));
        insta::assert_snapshot!(to_test_text(
            Parser::signal_declaration,
            "signal bus_s: bit bus := '0';"
        ));
    }

    #[test]
    fn variable_declaration() {
        insta::assert_snapshot!(to_test_text(
            Parser::variable_declaration,
            "variable v1: bit;"
        ));
        insta::assert_snapshot!(to_test_text(
            Parser::variable_declaration,
            "variable v, vv, vvv : natural := 1;"
        ));
        insta::assert_snapshot!(to_test_text(
            Parser::variable_declaration,
            "shared variable shared_v: natural;"
        ));
        insta::assert_snapshot!(to_test_text(
            Parser::variable_declaration,
            "shared variable shared_v, shared_vv: natural;"
        ));
    }

    #[test]
    fn file_declaration() {
        insta::assert_snapshot!(to_test_text(Parser::file_declaration, "file v1: bit;"));
        insta::assert_snapshot!(to_test_text(
            Parser::file_declaration,
            "file f, ff, fff : natural;"
        ));
        insta::assert_snapshot!(to_test_text(
            Parser::file_declaration,
            "file f_path: bit is \"./path_to_file.txt\";"
        ));
        insta::assert_snapshot!(to_test_text(
            Parser::file_declaration,
            "file f_path: bit open WRITE_MODE is \"./path_to_file.txt\";"
        ));
    }

    #[test]
    fn parses_optional_expression() {
        insta::assert_snapshot!(to_test_text(
            Parser::constant_declaration,
            "constant foo : natural := 0;"
        ));
    }
}
