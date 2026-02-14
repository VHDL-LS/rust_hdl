// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind::*;
use crate::syntax::NodeKind;
use crate::tokens::Keyword as Kw;
use crate::tokens::TokenKind::*;

impl Parser {
    pub fn subprogram_declaration(&mut self) {
        self.start_node(SubprogramDeclaration);
        self.subprogram_specification();
        self.expect_token(SemiColon);
        self.end_node();
    }

    pub fn subprogram_instantiation_declaration(&mut self) {
        self.start_node(SubprogramInstantiationDeclaration);
        self.subprogram_instantiation_declaration_preamble();
        self.opt_generic_map_aspect();
        self.expect_token(SemiColon);
        self.end_node();
    }

    pub fn subprogram_instantiation_declaration_preamble(&mut self) {
        self.start_node(SubprogramInstantiationDeclarationPreamble);
        self.expect_one_of_tokens([Keyword(Kw::Function), Keyword(Kw::Procedure)]);
        self.identifier();
        self.expect_tokens([Keyword(Kw::Is), Keyword(Kw::New)]);
        self.name();
        if self.next_is(LeftSquare) {
            self.signature();
        }
        self.end_node();
    }

    pub fn subprogram_specification(&mut self) {
        let is_function = if matches!(
            self.peek_token(),
            Keyword(Kw::Pure | Kw::Impure | Kw::Function)
        ) {
            self.start_node(FunctionSpecification);
            self.opt_tokens([Keyword(Kw::Pure), Keyword(Kw::Impure)]);
            self.expect_token(Keyword(Kw::Function));
            true
        } else if self.opt_token(Keyword(Kw::Procedure)) {
            self.start_node(ProcedureSpecification);
            false
        } else {
            self.expect_tokens_err([
                Keyword(Kw::Pure),
                Keyword(Kw::Impure),
                Keyword(Kw::Function),
                Keyword(Kw::Procedure),
            ]);
            return;
        };
        self.designator();
        self.subprogram_header();
        self.opt_parameter_list();
        if is_function {
            self.expect_kw(Kw::Return);
            self.type_mark();
        }
        self.end_node();
    }

    pub(crate) fn opt_parameter_list(&mut self) {
        if self.next_is_one_of([Keyword(Kw::Parameter), LeftPar]) {
            self.parameter_list();
        }
    }

    pub fn parameter_list(&mut self) {
        self.start_node(NodeKind::ParameterList);
        self.opt_token(Keyword(Kw::Parameter));
        self.start_node(ParenthesizedInterfaceList);
        self.expect_token(LeftPar);
        self.interface_list();
        self.expect_token(RightPar);
        self.end_node();
        self.end_node();
    }

    pub fn subprogram_header(&mut self) {
        self.start_node(SubprogramHeader);
        self.opt_subprogram_header_generic_clause();
        self.opt_generic_map_aspect();
        self.end_node();
    }

    fn opt_subprogram_header_generic_clause(&mut self) {
        if self.next_is(Keyword(Kw::Generic)) {
            self.subprogram_header_generic_clause();
        }
    }

    pub fn subprogram_header_generic_clause(&mut self) {
        self.start_node(SubprogramHeaderGenericClause);
        self.expect_kw(Kw::Generic);
        self.expect_token(LeftPar);
        if !(self.next_is(RightPar)) {
            self.interface_list();
        }
        self.expect_token(RightPar);
        self.end_node();
    }

    pub fn subprogram_body(&mut self) {
        self.subprogram_declaration_or_body();
    }

    pub(crate) fn subprogram_declaration_or_body(&mut self) {
        let checkpoint = self.checkpoint();
        self.subprogram_specification();
        if self.opt_token(SemiColon) {
            self.start_node_at(checkpoint, SubprogramDeclaration);
            self.end_node();
            return;
        }
        self.start_node_at(checkpoint, SubprogramBody);
        self.start_node_at(checkpoint, SubprogramBodyPreamble);
        self.expect_kw(Kw::Is);
        self.end_node();
        self.declarations();
        self.start_node(DeclarationStatementSeparator);
        self.expect_kw(Kw::Begin);
        self.end_node();
        self.sequential_statements();
        self.subprogram_body_epilogue();
        self.end_node();
    }

    pub fn subprogram_body_epilogue(&mut self) {
        self.start_node(SubprogramBodyEpilogue);
        self.expect_kw(Kw::End);
        self.subprogram_kind();
        self.opt_designator();
        self.expect_token(SemiColon);
        self.end_node();
    }

    pub fn subprogram_kind(&mut self) {
        self.opt_tokens([Keyword(Kw::Function), Keyword(Kw::Procedure)]);
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{test_utils::to_test_text, Parser};

    #[test]
    pub fn parses_procedure_declaration() {
        insta::assert_snapshot!(to_test_text(
            Parser::subprogram_declaration,
            "procedure foo;"
        ));
    }

    #[test]
    pub fn parses_function_specification() {
        insta::assert_snapshot!(to_test_text(
            Parser::subprogram_declaration,
            "function foo return lib.foo.natural;"
        ));
    }

    #[test]
    pub fn parses_function_specification_operator() {
        insta::assert_snapshot!(to_test_text(
            Parser::subprogram_declaration,
            "function \"+\" return lib.foo.natural;"
        ));
    }

    #[test]
    pub fn parses_impure_function_specification() {
        insta::assert_snapshot!(to_test_text(
            Parser::subprogram_declaration,
            "impure function foo return lib.foo.natural;"
        ));
    }

    #[test]
    pub fn parses_pure_function_specification() {
        insta::assert_snapshot!(to_test_text(
            Parser::subprogram_declaration,
            "pure function foo return lib.foo.natural;"
        ));
    }

    #[test]
    pub fn parses_procedure_specification_with_parameters() {
        insta::assert_snapshot!(to_test_text(
            Parser::subprogram_declaration,
            "procedure foo(foo : natural);"
        ));
    }

    #[test]
    pub fn parses_function_specification_with_parameters() {
        insta::assert_snapshot!(to_test_text(
            Parser::subprogram_declaration,
            "function foo(foo : natural) return lib.foo.natural;"
        ));
    }

    #[test]
    pub fn parses_function_specification_with_parameters_and_keyword() {
        insta::assert_snapshot!(to_test_text(
            Parser::subprogram_declaration,
            "function foo parameter (foo : natural) return lib.foo.natural;"
        ));
    }

    #[test]
    pub fn parses_function_specification_with_parameters_keyword_and_header() {
        insta::assert_snapshot!(to_test_text(
            Parser::subprogram_declaration,
            "function foo generic (abc_def: natural) parameter (foo : natural) return lib.foo.natural;"
        ));
    }

    #[test]
    pub fn parses_subprogram_body() {
        insta::assert_snapshot!(to_test_text(
            Parser::subprogram_body,
            "\
function foo(arg : natural) return natural is
  constant foo : natural := 0;
begin
  return foo + arg;
end function;"
        ));
    }

    #[test]
    pub fn parses_subprogram_declaration() {
        insta::assert_snapshot!(to_test_text(
            Parser::subprogram_body,
            "\
function foo(arg : natural) return natural is
begin
end function foo;"
        ));
    }

    #[test]
    pub fn parses_subprogram_body_end_operator_symbol() {
        insta::assert_snapshot!(to_test_text(
            Parser::subprogram_body,
            "\
function \"+\"(arg : natural) return natural is
begin
end function \"+\";"
        ));
    }

    #[test]
    pub fn parse_subprogram_header_no_aspect() {
        insta::assert_snapshot!(to_test_text(
            Parser::subprogram_header,
            "generic (x: natural := 1; y: real)"
        ));
    }

    #[test]
    pub fn parse_subprogram_header_with_aspect() {
        insta::assert_snapshot!(to_test_text(
            Parser::subprogram_header,
            "generic (x: natural := 1; y: real) generic map (x => 2, y => 0.4)"
        ));
    }

    #[test]
    pub fn parse_procedure_spec_with_header_no_aspect() {
        insta::assert_snapshot!(to_test_text(
            Parser::subprogram_declaration,
            "\
procedure my_proc
    generic (x: natural := 4; y: real := 4);"
        ));
    }

    #[test]
    pub fn parse_procedure_spec_with_header_aspect() {
        insta::assert_snapshot!(to_test_text(
            Parser::subprogram_declaration,
            "\
procedure my_proc
    generic (x: natural := 4; y: real := 4)
    generic map (x => 42);"
        ));
    }

    #[test]
    pub fn parse_function_with_header() {
        insta::assert_snapshot!(to_test_text(
            Parser::subprogram_body,
            "\
function foo generic (x: natural := 4) (arg : natural) return natural is
  constant foo : natural := 0;
begin
  return foo + arg;
end function;"
        ));
    }

    #[test]
    pub fn swap_function() {
        insta::assert_snapshot!(to_test_text(
            Parser::subprogram_body,
            "\
procedure swap
  generic ( type T )
  parameter (a, b : inout T) is
  variable temp : T;
begin
  temp := a; a := b; b := temp;
end procedure swap;"
        ));
    }

    #[test]
    pub fn subprogram_instantiation() {
        insta::assert_snapshot!(to_test_text(
            Parser::subprogram_instantiation_declaration,
            "procedure my_proc is new proc;"
        ));
        insta::assert_snapshot!(to_test_text(
            Parser::subprogram_instantiation_declaration,
            "function my_proc is new proc;"
        ));
    }
}
