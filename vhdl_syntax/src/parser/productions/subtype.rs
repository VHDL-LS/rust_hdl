// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::Parser;
use crate::syntax::NodeKind::*;
use crate::tokens::Keyword as Kw;
use crate::tokens::TokenKind::*;

fn is_start_of_name(parser: &Parser) -> bool {
    parser.next_is_one_of([LtLt, Identifier, StringLiteral, CharacterLiteral])
}

impl Parser {
    fn resolution_indication(&mut self) {
        if self.next_is(LeftPar) {
            self.start_node(ParenthesizedElementResolutionResolutionIndication);
            self.skip();
            self.element_resolution();
            self.expect_token(RightPar);
            self.end_node();
        } else {
            self.start_node(NameResolutionIndication);
            self.name();
            self.end_node();
        }
    }

    pub fn element_resolution(&mut self) {
        self.start_node(ElementResolutionResolutionIndication);
        if self.next_is(Identifier)
            && (matches!(
                self.peek_nth_token(1),
                LtLt | Identifier | StringLiteral | CharacterLiteral | LeftPar
            ))
        {
            self.start_node(RecordResolutionElementResolution);
            self.record_resolution();
            self.end_node();
        } else {
            self.start_node(ResolutionIndicationElementResolution);
            self.resolution_indication();
            self.end_node();
        }
        self.end_node();
    }

    pub fn record_element_resolution(&mut self) {
        self.start_node(RecordElementResolution);
        self.identifier();
        self.resolution_indication();
        self.end_node();
    }

    pub fn record_resolution(&mut self) {
        self.start_node(RecordResolution);
        self.separated_list(Parser::record_element_resolution, Comma);
        self.end_node();
    }

    pub fn subtype_indication(&mut self) {
        self.start_node(SubtypeIndication);
        if self.next_is(LeftPar) {
            self.resolution_indication();
        } else {
            self.name();
        }
        if is_start_of_name(self) {
            self.name();
        }
        self.opt_constraint();
        self.end_node();
    }

    fn opt_constraint(&mut self) {
        if self.next_is(Keyword(Kw::Range)) {
            self.start_node(RangeConstraint);
            self.skip();
            self.range();
            self.end_node();
        }
        // all other constraints are handled by `name`
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{test_utils::to_test_text, Parser};

    #[test]
    fn parse_subtype_indication_without_constraint() {
        insta::assert_snapshot!(to_test_text(Parser::subtype_indication, "std_logic"));
    }

    #[test]
    fn parse_subtype_indication_with_resolution_function() {
        insta::assert_snapshot!(to_test_text(
            Parser::subtype_indication,
            "resolve std_logic"
        ));
    }

    #[test]
    fn parse_subtype_indication_with_array_element_resolution_function() {
        insta::assert_snapshot!(to_test_text(
            Parser::subtype_indication,
            "(resolve) integer_vector"
        ));
    }

    #[test]
    fn parse_subtype_indication_with_record_element_resolution_function() {
        insta::assert_snapshot!(to_test_text(
            Parser::subtype_indication,
            "(elem resolve) rec_t"
        ));
    }

    #[test]
    fn parse_subtype_indication_with_record_element_resolution_function_many() {
        insta::assert_snapshot!(to_test_text(
            Parser::subtype_indication,
            "(elem1 (resolve1), elem2 resolve2, elem3 (sub_elem sub_resolve)) rec_t"
        ));
    }

    #[test]
    fn parse_subtype_indication_with_resolution_function_selected_name() {
        insta::assert_snapshot!(to_test_text(
            Parser::subtype_indication,
            "lib.foo.resolve std_logic"
        ));
    }

    #[test]
    fn parse_subtype_indication_without_selected_name() {
        insta::assert_snapshot!(to_test_text(Parser::subtype_indication, "lib.foo.bar"));
    }

    #[test]
    fn parse_subtype_indication_with_range() {
        insta::assert_snapshot!(to_test_text(
            Parser::subtype_indication,
            "integer range 0 to 2-1"
        ));
    }

    #[test]
    fn parse_subtype_indication_with_range_attribute() {
        insta::assert_snapshot!(to_test_text(
            Parser::subtype_indication,
            "integer range lib.foo.bar'range"
        ));
    }

    #[test]
    fn parse_subtype_indication_with_array_constraint_range() {
        insta::assert_snapshot!(to_test_text(
            Parser::subtype_indication,
            "integer_vector(2-1 downto 0)"
        ));
    }

    #[test]
    fn parse_subtype_indication_with_array_constraint_discrete() {
        insta::assert_snapshot!(to_test_text(
            Parser::subtype_indication,
            "integer_vector(lib.foo.bar)"
        ));
    }

    #[test]
    fn parse_subtype_indication_with_array_constraint_attribute() {
        insta::assert_snapshot!(to_test_text(
            Parser::subtype_indication,
            "integer_vector(lib.pkg.bar'range)"
        ));
    }

    #[test]
    fn parse_subtype_indication_with_array_constraint_open() {
        insta::assert_snapshot!(to_test_text(
            Parser::subtype_indication,
            "integer_vector(open)"
        ));
    }

    #[test]
    fn parse_subtype_indication_with_multi_dim_array_constraints() {
        insta::assert_snapshot!(to_test_text(
            Parser::subtype_indication,
            "integer_vector(2-1 downto 0, 11 to 14)"
        ));
    }

    #[test]
    fn parse_subtype_indication_with_array_element_constraint() {
        insta::assert_snapshot!(to_test_text(
            Parser::subtype_indication,
            "integer_vector(2-1 downto 0, 11 to 14)(foo to bar)"
        ));
    }

    #[test]
    fn parse_subtype_indication_with_record_constraint() {
        insta::assert_snapshot!(to_test_text(
            Parser::subtype_indication,
            "axi_m2s_t(tdata(2-1 downto 0), tuser(3 to 5))"
        ));
    }

    #[test]
    fn test_subtype_indication_with_subtype_attribute() {
        insta::assert_snapshot!(to_test_text(Parser::subtype_indication, "obj'subtype"));
    }
}
