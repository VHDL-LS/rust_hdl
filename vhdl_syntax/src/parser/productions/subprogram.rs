// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::marker::{CompletedMarker, Precede};
use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind::*;
use crate::syntax::{
    AstNode, NodeKind, SequentialStatementSyntax, SubprogramDeclarativeItemSyntax,
};
use crate::tokens::Keyword as Kw;
use crate::tokens::TokenKind::*;

impl Parser {
    #[cfg(test)]
    pub(crate) fn subprogram_declaration(&mut self) {
        self.node(SubprogramDeclaration, |p| {
            p.subprogram_specification();
            p.expect_token(SemiColon);
        });
    }

    pub(crate) fn subprogram_instantiation_declaration(&mut self) -> CompletedMarker {
        self.node(SubprogramInstantiationDeclaration, |p| {
            p.subprogram_instantiation_declaration_preamble();
            p.opt_generic_map_aspect();
            p.expect_token(SemiColon);
        })
    }

    pub(crate) fn subprogram_instantiation_declaration_preamble(&mut self) {
        self.node(SubprogramInstantiationDeclarationPreamble, |p| {
            p.expect_one_of_tokens([Keyword(Kw::Function), Keyword(Kw::Procedure)]);
            p.identifier();
            p.expect_tokens([Keyword(Kw::Is), Keyword(Kw::New)]);
            p.name();
            if p.next_is(LeftSquare) {
                p.signature();
            }
        });
    }

    pub(crate) fn subprogram_specification(&mut self) -> Option<CompletedMarker> {
        let (marker, is_function) = if matches!(
            self.peek_token(),
            Keyword(Kw::Pure | Kw::Impure | Kw::Function)
        ) {
            let marker = self.start_node(FunctionSpecification);
            self.opt_tokens([Keyword(Kw::Pure), Keyword(Kw::Impure)]);
            self.expect_token(Keyword(Kw::Function));
            (marker, true)
        } else if self.next_is(Keyword(Kw::Procedure)) {
            let marker = self.start_node(ProcedureSpecification);
            self.expect_token(Keyword(Kw::Procedure));
            (marker, false)
        } else {
            self.expect_tokens_recover([
                Keyword(Kw::Pure),
                Keyword(Kw::Impure),
                Keyword(Kw::Function),
                Keyword(Kw::Procedure),
            ]);
            return None;
        };
        self.designator();
        self.subprogram_header();
        self.opt_parameter_list();
        if is_function {
            self.expect_kw(Kw::Return);
            self.type_mark();
        }
        Some(marker.complete(self))
    }

    pub(crate) fn opt_parameter_list(&mut self) {
        if self.next_is_one_of([Keyword(Kw::Parameter), LeftPar]) {
            self.parameter_list();
        }
    }

    pub(crate) fn parameter_list(&mut self) {
        self.node(NodeKind::ParameterList, |p| {
            p.opt_token(Keyword(Kw::Parameter));
            p.node(ParenthesizedInterfaceList, |p| {
                p.expect_token(LeftPar);
                p.interface_list();
                p.expect_token(RightPar);
            });
        });
    }

    pub(crate) fn subprogram_header(&mut self) {
        if !self.next_is(Keyword(Kw::Generic)) {
            return;
        }
        self.node(SubprogramHeader, |p| {
            p.subprogram_header_generic_clause();
            p.opt_generic_map_aspect();
        });
    }

    pub(crate) fn subprogram_header_generic_clause(&mut self) {
        self.node(SubprogramHeaderGenericClause, |p| {
            p.expect_kw(Kw::Generic);
            p.expect_token(LeftPar);
            p.interface_list();
            p.expect_token(RightPar);
        });
    }

    #[cfg(test)]
    pub(crate) fn subprogram_body(&mut self) {
        self.subprogram_declaration_or_body();
    }

    pub(crate) fn subprogram_declaration_or_body(&mut self) -> CompletedMarker {
        let unknown = self.start_unknown();
        let specification = self.subprogram_specification();
        let marker = if self.opt_token(SemiColon) {
            return unknown.complete(self, SubprogramDeclaration);
        } else {
            unknown.resolve(self, SubprogramBody)
        };
        let preamble = specification.precede(self, SubprogramBodyPreamble);
        self.expect_kw(Kw::Is);
        preamble.complete(self);
        self.subprogram_declarative_part();
        self.node(DeclarationStatementSeparator, |p| {
            p.expect_kw(Kw::Begin);
        });
        self.subprogram_statement_part();
        self.subprogram_body_epilogue();
        marker.complete(self)
    }

    pub(crate) fn subprogram_declarative_part(&mut self) {
        self.declarations(
            SubprogramDeclarativePart,
            SubprogramDeclarativeItemSyntax::META,
        );
    }

    pub(crate) fn subprogram_statement_part(&mut self) {
        self.sequential_statements(SubprogramStatementPart, SequentialStatementSyntax::META);
    }

    pub(crate) fn subprogram_body_epilogue(&mut self) {
        self.node(SubprogramBodyEpilogue, |p| {
            p.expect_kw(Kw::End);
            p.subprogram_kind();
            p.opt_designator();
            p.expect_token(SemiColon);
        });
    }

    pub(crate) fn subprogram_kind(&mut self) {
        self.opt_tokens([Keyword(Kw::Function), Keyword(Kw::Procedure)]);
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{test_utils::to_test_text, Parser};

    #[test]
    pub(crate) fn parses_procedure_declaration() {
        insta::assert_snapshot!(to_test_text(
            Parser::subprogram_declaration,
            "procedure foo;"
        ));
    }

    #[test]
    pub(crate) fn parses_function_specification() {
        insta::assert_snapshot!(to_test_text(
            Parser::subprogram_declaration,
            "function foo return lib.foo.natural;"
        ));
    }

    #[test]
    pub(crate) fn parses_function_specification_operator() {
        insta::assert_snapshot!(to_test_text(
            Parser::subprogram_declaration,
            "function \"+\" return lib.foo.natural;"
        ));
    }

    #[test]
    pub(crate) fn parses_impure_function_specification() {
        insta::assert_snapshot!(to_test_text(
            Parser::subprogram_declaration,
            "impure function foo return lib.foo.natural;"
        ));
    }

    #[test]
    pub(crate) fn parses_pure_function_specification() {
        insta::assert_snapshot!(to_test_text(
            Parser::subprogram_declaration,
            "pure function foo return lib.foo.natural;"
        ));
    }

    #[test]
    pub(crate) fn parses_procedure_specification_with_parameters() {
        insta::assert_snapshot!(to_test_text(
            Parser::subprogram_declaration,
            "procedure foo(foo : natural);"
        ));
    }

    #[test]
    pub(crate) fn parses_function_specification_with_parameters() {
        insta::assert_snapshot!(to_test_text(
            Parser::subprogram_declaration,
            "function foo(foo : natural) return lib.foo.natural;"
        ));
    }

    #[test]
    pub(crate) fn parses_function_specification_with_parameters_and_keyword() {
        insta::assert_snapshot!(to_test_text(
            Parser::subprogram_declaration,
            "function foo parameter (foo : natural) return lib.foo.natural;"
        ));
    }

    #[test]
    pub(crate) fn parses_function_specification_with_parameters_keyword_and_header() {
        insta::assert_snapshot!(to_test_text(
            Parser::subprogram_declaration,
            "function foo generic (abc_def: natural) parameter (foo : natural) return lib.foo.natural;"
        ));
    }

    #[test]
    pub(crate) fn parses_subprogram_body() {
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
    pub(crate) fn parses_subprogram_declaration() {
        insta::assert_snapshot!(to_test_text(
            Parser::subprogram_body,
            "\
function foo(arg : natural) return natural is
begin
end function foo;"
        ));
    }

    #[test]
    pub(crate) fn parses_subprogram_body_end_operator_symbol() {
        insta::assert_snapshot!(to_test_text(
            Parser::subprogram_body,
            "\
function \"+\"(arg : natural) return natural is
begin
end function \"+\";"
        ));
    }

    #[test]
    pub(crate) fn parse_subprogram_header_no_aspect() {
        insta::assert_snapshot!(to_test_text(
            Parser::subprogram_header,
            "generic (x: natural := 1; y: real)"
        ));
    }

    #[test]
    pub(crate) fn parse_subprogram_header_with_aspect() {
        insta::assert_snapshot!(to_test_text(
            Parser::subprogram_header,
            "generic (x: natural := 1; y: real) generic map (x => 2, y => 0.4)"
        ));
    }

    #[test]
    pub(crate) fn parse_procedure_spec_with_header_no_aspect() {
        insta::assert_snapshot!(to_test_text(
            Parser::subprogram_declaration,
            "\
procedure my_proc
    generic (x: natural := 4; y: real := 4);"
        ));
    }

    #[test]
    pub(crate) fn parse_procedure_spec_with_header_aspect() {
        insta::assert_snapshot!(to_test_text(
            Parser::subprogram_declaration,
            "\
procedure my_proc
    generic (x: natural := 4; y: real := 4)
    generic map (x => 42);"
        ));
    }

    #[test]
    pub(crate) fn parse_function_with_header() {
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
    pub(crate) fn swap_function() {
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
    pub(crate) fn subprogram_instantiation() {
        insta::assert_snapshot!(to_test_text(
            Parser::subprogram_instantiation_declaration,
            "procedure my_proc is new proc;"
        ));
        insta::assert_snapshot!(to_test_text(
            Parser::subprogram_instantiation_declaration,
            "function my_proc is new proc;"
        ));
    }

    // MARK: Error recovery

    #[test]
    fn function_missing_return_type() {
        assert_recovery_snapshot!(
            "function f(a : integer) return ;",
            Parser::subprogram_declaration
        );
    }

    #[test]
    fn function_missing_return_clause() {
        assert_recovery_snapshot!("function f(a : integer);", Parser::subprogram_declaration);
    }

    #[test]
    fn function_unclosed_parameter_list() {
        assert_recovery_snapshot!(
            "function f(a : integer return integer;",
            Parser::subprogram_declaration
        );
    }
}
