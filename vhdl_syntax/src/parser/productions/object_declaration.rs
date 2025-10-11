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
    use crate::parser::test_utils::check;
    use crate::parser::Parser;

    #[test]
    fn constant_declaration() {
        check(
            Parser::constant_declaration,
            "constant C1: std_ulogic;",
            "\
ConstantDeclaration
  Keyword(Constant)
  IdentifierList
    Identifier 'C1'
  Colon
  Identifier 'std_ulogic'
  SemiColon
",
        );
        check(
            Parser::constant_declaration,
            "constant C1,C2,C3: std_ulogic := '0';",
            "\
ConstantDeclaration
  Keyword(Constant)
  IdentifierList
    Identifier 'C1'
    Comma
    Identifier 'C2'
    Comma
    Identifier 'C3'
  Colon
  Identifier 'std_ulogic'
  ColonEq
  LiteralExpression
    CharacterLiteral ''0''
  SemiColon
",
        );
    }

    #[test]
    fn signal_declaration() {
        check(
            Parser::signal_declaration,
            "signal s1: bit;",
            "\
SignalDeclaration
  Keyword(Signal)
  IdentifierList
    Identifier 's1'
  Colon
  Identifier 'bit'
  SemiColon
",
        );
        check(
            Parser::signal_declaration,
            "signal s, ss, sss : natural := 1;",
            "\
SignalDeclaration
  Keyword(Signal)
  IdentifierList
    Identifier 's'
    Comma
    Identifier 'ss'
    Comma
    Identifier 'sss'
  Colon
  Identifier 'natural'
  ColonEq
  LiteralExpression
    AbstractLiteral '1'
  SemiColon
",
        );
        check(
            Parser::signal_declaration,
            "signal reg_s: bit register;",
            "\
SignalDeclaration
  Keyword(Signal)
  IdentifierList
    Identifier 'reg_s'
  Colon
  Identifier 'bit'
  Keyword(Register)
  SemiColon
",
        );
        check(
            Parser::signal_declaration,
            "signal bus_s: bit bus := '0';",
            "\
SignalDeclaration
  Keyword(Signal)
  IdentifierList
    Identifier 'bus_s'
  Colon
  Identifier 'bit'
  Keyword(Bus)
  ColonEq
  LiteralExpression
    CharacterLiteral ''0''
  SemiColon
",
        );
    }

    #[test]
    fn variable_declaration() {
        check(
            Parser::variable_declaration,
            "variable v1: bit;",
            "\
VariableDeclaration
  Keyword(Variable)
  IdentifierList
    Identifier 'v1'
  Colon
  Identifier 'bit'
  SemiColon
",
        );
        check(
            Parser::variable_declaration,
            "variable v, vv, vvv : natural := 1;",
            "\
VariableDeclaration
  Keyword(Variable)
  IdentifierList
    Identifier 'v'
    Comma
    Identifier 'vv'
    Comma
    Identifier 'vvv'
  Colon
  Identifier 'natural'
  ColonEq
  LiteralExpression
    AbstractLiteral '1'
  SemiColon
",
        );
        check(
            Parser::variable_declaration,
            "shared variable shared_v: natural;",
            "\
VariableDeclaration
  Keyword(Shared)
  Keyword(Variable)
  IdentifierList
    Identifier 'shared_v'
  Colon
  Identifier 'natural'
  SemiColon
",
        );
    }

    #[test]
    fn file_declaration() {
        check(
            Parser::file_declaration,
            "file v1: bit;",
            "\
FileDeclaration
  Keyword(File)
  IdentifierList
    Identifier 'v1'
  Colon
  Identifier 'bit'
  SemiColon
",
        );
        check(
            Parser::file_declaration,
            "file f, ff, fff : natural;",
            "\
FileDeclaration
  Keyword(File)
  IdentifierList
    Identifier 'f'
    Comma
    Identifier 'ff'
    Comma
    Identifier 'fff'
  Colon
  Identifier 'natural'
  SemiColon
",
        );
        check(
            Parser::file_declaration,
            "file f_path: bit is \"./path_to_file.txt\";",
            "\
FileDeclaration
  Keyword(File)
  IdentifierList
    Identifier 'f_path'
  Colon
  Identifier 'bit'
  FileOpenInformation
    Keyword(Is)
    LiteralExpression
      StringLiteral '\"./path_to_file.txt\"'
  SemiColon
",
        );
        check(
            Parser::file_declaration,
            "file f_path: bit open WRITE_MODE is \"./path_to_file.txt\";",
            "\
FileDeclaration
  Keyword(File)
  IdentifierList
    Identifier 'f_path'
  Colon
  Identifier 'bit'
  FileOpenInformation
    Keyword(Open)
    NameExpression
      Name
        Identifier 'WRITE_MODE'
    Keyword(Is)
    LiteralExpression
      StringLiteral '\"./path_to_file.txt\"'
  SemiColon
",
        );
    }
}
