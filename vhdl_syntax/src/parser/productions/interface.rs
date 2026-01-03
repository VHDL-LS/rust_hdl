// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind::*;
use crate::tokens::Keyword as Kw;
use crate::tokens::TokenKind::*;
use crate::tokens::TokenStream;

impl<T: TokenStream> Parser<T> {
    pub(crate) fn opt_generic_clause(&mut self) {
        if self.next_is(Keyword(Kw::Generic)) {
            self.generic_clause();
        }
    }

    pub fn generic_clause(&mut self) {
        self.start_node(GenericClause);
        self.expect_kw(Kw::Generic);
        self.expect_token(LeftPar);
        if !(self.next_is(RightPar)) {
            self.interface_list();
        }
        self.expect_tokens([RightPar, SemiColon]);
        self.end_node();
    }

    pub fn opt_port_clause(&mut self) {
        if self.next_is(Keyword(Kw::Port)) {
            self.port_clause();
        }
    }

    pub fn port_clause(&mut self) {
        self.start_node(PortClause);
        self.expect_kw(Kw::Port);
        self.expect_token(LeftPar);
        if !(self.next_is(RightPar)) {
            self.interface_list();
        }
        self.expect_tokens([RightPar, SemiColon]);
        self.end_node();
    }

    pub fn interface_list(&mut self) {
        self.start_node(InterfaceList);
        self.separated_list(Parser::interface_declaration, SemiColon);
        self.end_node();
    }

    pub fn interface_declaration(&mut self) {
        match self.peek_token() {
            Some(Keyword(Kw::Signal | Kw::Constant | Kw::Variable) | Identifier) => {
                self.interface_object_declaration();
            }
            Some(Keyword(Kw::File)) => self.interface_file_declaration(),
            Some(Keyword(Kw::Type)) => self.interface_type_declaration(),
            Some(Keyword(Kw::Function | Kw::Procedure | Kw::Impure | Kw::Pure)) => {
                self.interface_subprogram_declaration()
            }
            Some(Keyword(Kw::Package)) => self.interface_package_declaration(),
            _ => self.expect_tokens_err([
                Keyword(Kw::Signal),
                Keyword(Kw::Constant),
                Keyword(Kw::Variable),
                Identifier,
                Keyword(Kw::File),
                Keyword(Kw::Type),
                Keyword(Kw::Function),
                Keyword(Kw::Procedure),
                Keyword(Kw::Impure),
                Keyword(Kw::Pure),
            ]),
        }
    }

    pub fn interface_file_declaration(&mut self) {
        self.start_node(InterfaceFileDeclaration);
        self.expect_kw(Kw::File);
        self.identifier_list();
        self.expect_token(Colon);
        self.subtype_indication();
        self.end_node();
    }

    pub fn interface_type_declaration(&mut self) {
        self.start_node(InterfaceIncompleteTypeDeclaration);
        self.expect_kw(Kw::Type);
        self.identifier();
        self.end_node();
    }

    pub fn interface_subprogram_declaration(&mut self) {
        self.start_node(InterfaceSubprogramDeclaration);
        self.interface_subprogram_specification();
        if self.opt_token(Keyword(Kw::Is)) {
            self.interface_subprogram_default();
        }
        self.end_node();
    }

    pub fn interface_subprogram_default(&mut self) {
        if self.next_is(BOX) {
            self.skip_into_node(InterfaceSubprogramDefaultBox);
        } else {
            self.start_node(InterfaceSubprogramDefaultName);
            self.name();
            self.end_node();
        }
    }

    pub fn interface_subprogram_specification(&mut self) {
        if self.next_is(Keyword(Kw::Procedure)) {
            self.interface_procedure_specification();
        } else {
            self.interface_function_specification();
        }
    }

    pub fn interface_procedure_specification(&mut self) {
        self.start_node(InterfaceProcedureSpecification);
        self.end_node();
    }

    pub fn interface_function_specification(&mut self) {
        self.start_node(InterfaceFunctionSpecification);
        self.opt_function_purity();
        self.expect_kw(Kw::Function);
        self.designator();
        self.opt_parameter_list();
        self.expect_kw(Kw::Return);
        self.name();
        self.end_node();
    }

    fn opt_parameter_list(&mut self) {
        if self.next_is_one_of([Keyword(Kw::Parameter), LeftPar]) {
            self.parameter_list();
        }
    }

    pub fn parameter_list(&mut self) {
        self.start_node(ParameterList);
        self.opt_token(Keyword(Kw::Parameter));
        self.expect_token(LeftPar);
        self.interface_list();
        self.expect_token(RightPar);
        self.expect_kw(Kw::Return);
        self.type_mark();
        self.end_node();
    }

    pub fn opt_function_purity(&mut self) {
        self.opt_tokens([Keyword(Kw::Pure), Keyword(Kw::Impure)]);
    }

    pub fn interface_package_declaration(&mut self) {
        self.start_node(InterfacePackageDeclaration);
        self.expect_kw(Kw::Package);
        self.identifier();
        self.expect_kw(Kw::Is);
        self.expect_kw(Kw::New);
        self.name();
        self.interface_package_generic_map_aspect();
        self.end_node();
    }

    pub fn interface_package_generic_map_aspect(&mut self) {
        self.start_node(InterfacePackageGenericMapAspect);
        self.expect_kw(Kw::Generic);
        self.expect_kw(Kw::Map);
        self.expect_token(LeftPar);
        if self.next_is(BOX) {
            self.skip_into_node(InterfacePackageGenericMapAspectBox);
        } else if self.next_is(Keyword(Kw::Default)) {
            self.skip_into_node(InterfacePackageGenericMapAspectDefault);
        } else {
            self.start_node(InterfacePackageGenericMapAspectAssociations);
            self.association_list();
            self.end_node();
        }
        self.expect_token(RightPar);
        self.end_node();
    }

    pub fn interface_object_declaration(&mut self) {
        let checkpoint = self.checkpoint();
        let tok = self.opt_tokens([
            Keyword(Kw::Signal),
            Keyword(Kw::Constant),
            Keyword(Kw::Variable),
        ]);
        match tok {
            Some(Keyword(Kw::Signal)) => self.start_node_at(checkpoint, InterfaceSignalDeclaration),
            Some(Keyword(Kw::Variable)) => {
                self.start_node_at(checkpoint, InterfaceVariableDeclaration)
            }
            _ => self.start_node_at(checkpoint, InterfaceConstantDeclaration),
        }
        self.identifier_list();
        self.expect_token(Colon);
        self.opt_mode();
        self.subtype_indication();
        self.opt_token(Keyword(Kw::Bus));
        if self.opt_token(ColonEq) {
            self.expression();
        }
        self.end_node();
    }

    pub fn opt_mode(&mut self) {
        self.opt_tokens([
            Keyword(Kw::In),
            Keyword(Kw::Out),
            Keyword(Kw::Inout),
            Keyword(Kw::Buffer),
            Keyword(Kw::Linkage),
        ]);
    }

    pub fn association_list(&mut self) {
        self.association_list_bounded(usize::MAX);
    }

    fn association_list_bounded(&mut self, max_index: usize) {
        self.start_node(AssociationList);
        self.separated_list(
            |parser| {
                let end_of_element_idx =
                    match parser.lookahead_max_token_index(max_index, [Comma, RightPar]) {
                        Ok((_, idx)) => idx,
                        Err((_, idx)) => idx,
                    };
                parser.association_element_bounded(end_of_element_idx);
            },
            Comma,
        );
        self.end_node();
    }

    fn association_element_bounded(&mut self, max_index: usize) {
        self.start_node(AssociationElement);

        // TODO: Error handling is done at a bare minimum.
        if self
            .lookahead_max_token_index(max_index, [RightArrow])
            .is_ok()
        {
            self.formal_part();
            self.expect_token(RightArrow);
        }
        self.actual_part_bounded(max_index);

        self.end_node();
    }

    pub fn formal_part(&mut self) {
        self.start_node(FormalPart);
        self.name();
        // Note: `self.name()` will already consume any trailing parenthesized names!
        self.end_node();
    }

    fn actual_part_bounded(&mut self, max_index: usize) {
        self.start_node(ActualPart);
        // Parsing of `actual_part` would boil down to `name | expression | subtype_indication`
        self.skip_to(max_index);
        self.end_node();
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils::to_test_text;
    use crate::parser::Parser;

    #[test]
    fn association_list() {
        insta::assert_snapshot!(to_test_text(Parser::association_list, "arg1, arg2",));

        insta::assert_snapshot!(to_test_text(
            Parser::association_list,
            "p1 => 1, std_ulogic(p2)=>     sl_sig"
        ));
    }

    #[test]
    fn interface_lists() {
        insta::assert_snapshot!(to_test_text(
            Parser::interface_list,
            "\
constant foo : std_logic;
bar : natural"
        ));
        insta::assert_snapshot!(to_test_text(
            Parser::interface_list,
            "\
signal foo : in std_logic;
bar : natural"
        ));
        insta::assert_snapshot!(to_test_text(
            Parser::interface_list,
            "\
signal foo : in std_logic;
constant bar : natural;
variable xyz : var"
        ));
    }

    #[test]
    fn empty_generic_clause() {
        insta::assert_snapshot!(to_test_text(Parser::generic_clause, "generic();",));
    }

    #[test]
    fn empty_port_clause() {
        insta::assert_snapshot!(to_test_text(Parser::port_clause, "port();",));
    }

    #[test]
    fn object_declaration() {
        insta::assert_snapshot!(to_test_text(
            Parser::interface_declaration,
            "a : in std_logic",
        ));
        insta::assert_snapshot!(to_test_text(
            Parser::interface_declaration,
            "a : out std_logic"
        ));
        insta::assert_snapshot!(to_test_text(
            Parser::interface_declaration,
            "signal a : out std_logic"
        ));
        insta::assert_snapshot!(to_test_text(
            Parser::interface_declaration,
            "constant a : out std_logic"
        ));
        insta::assert_snapshot!(to_test_text(
            Parser::interface_declaration,
            "a : inout std_logic"
        ));
        insta::assert_snapshot!(to_test_text(
            Parser::interface_declaration,
            "a : linkage std_logic",
        ));
        insta::assert_snapshot!(to_test_text(
            Parser::interface_declaration,
            "a : buffer std_logic"
        ));
        insta::assert_snapshot!(to_test_text(
            Parser::interface_declaration,
            "a, b, c : in std_logic"
        ));
    }

    #[test]
    fn subtype_indication_in_interface_declaration() {
        insta::assert_snapshot!(to_test_text(
            Parser::interface_declaration,
            "a : std_ulogic_vector(31 downto 0)",
        ));
    }

    #[test]
    fn parses_interface_type() {
        insta::assert_snapshot!(to_test_text(Parser::interface_declaration, "type name"));
    }

    #[test]
    fn parses_interface_identifier_list() {
        insta::assert_snapshot!(to_test_text(
            Parser::interface_declaration,
            "constant foo, bar : natural",
        ));
    }

    #[test]
    fn parses_interface_file_declaration() {
        insta::assert_snapshot!(to_test_text(
            Parser::interface_declaration,
            "file foo : text open read_mode",
        ));
    }

    #[test]
    fn parses_interface_file_declaration_no_file_name() {
        insta::assert_snapshot!(to_test_text(
            Parser::interface_declaration,
            "file foo : text is \"file_name\"",
        ));
    }

    #[test]
    fn parses_interface_subprogram() {
        insta::assert_snapshot!(to_test_text(
            Parser::interface_declaration,
            "function foo return bar"
        ));
    }

    #[test]
    fn parses_interface_subprogram_default() {
        insta::assert_snapshot!(to_test_text(
            Parser::interface_declaration,
            "function foo return bar is lib.name"
        ));

        insta::assert_snapshot!(to_test_text(
            Parser::interface_declaration,
            "function foo return bar is <>"
        ));
    }

    #[test]
    fn interface_package_generic_map_aspect() {
        insta::assert_snapshot!(to_test_text(
            Parser::interface_declaration,
            "\
package foo is new lib.pkg
    generic map (foo => bar)"
        ));
    }

    #[test]
    fn interface_package_generic_map_box() {
        insta::assert_snapshot!(to_test_text(
            Parser::interface_declaration,
            "\
package foo is new lib.pkg
     generic map (<>)"
        ));
    }

    #[test]
    fn interface_package_generic_map_default() {
        insta::assert_snapshot!(to_test_text(
            Parser::interface_declaration,
            "\
package foo is new lib.pkg
     generic map (default)"
        ));
    }
}
