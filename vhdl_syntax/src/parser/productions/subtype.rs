// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::marker::Precede;
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
            self.node(ParenthesizedElementResolution, |p| {
                p.skip();
                p.element_resolution();
                p.expect_token(RightPar);
            });
        } else {
            self.node(NameResolutionIndication, |p| {
                p.name();
            });
        }
    }

    pub(crate) fn element_resolution(&mut self) {
        self.node(ElementResolutionResolutionIndication, |p| {
            if p.next_is(Identifier)
                && (matches!(
                    p.peek_nth_token(1),
                    LtLt | Identifier | StringLiteral | CharacterLiteral | LeftPar
                ))
            {
                p.node(RecordResolutionElementResolution, |p| {
                    p.record_resolution();
                });
            } else {
                p.resolution_indication();
            }
        });
    }

    pub(crate) fn record_element_resolution(&mut self) {
        self.node(RecordElementResolution, |p| {
            p.identifier();
            p.resolution_indication();
        });
    }

    pub(crate) fn record_resolution(&mut self) {
        self.separated_list(RecordResolution, Parser::record_element_resolution, Comma);
    }

    pub(crate) fn subtype_indication(&mut self) {
        // subtype_indication ::= [resolution_indication] name
        // Constraints (range/index/record) are now `NameTail`s on the type-mark
        // `Name`, so no separate constraint slot is needed here.
        self.node(SubtypeIndication, |p| {
            if p.next_is(LeftPar) {
                p.resolution_indication();
                p.name();
            } else {
                // Bare-name `resolution_indication` is only detectable when a
                // second name follows. Mark the position, parse the first name,
                // and if another name follows wrap the first retroactively as a
                // `NameResolutionIndication`.
                let name = p.name();
                if is_start_of_name(p) {
                    name.precede(p, NameResolutionIndication).complete(p);
                    p.name();
                }
            }
        });
    }

    /// `range_constraint ::= "range" expression`. `to`/`downto` are binary
    /// operators in the expression grammar, so the old `range` production is
    /// gone and the constraint body is just an expression.
    pub(crate) fn range_constraint(&mut self) {
        self.node(RangeConstraint, |p| {
            p.expect_kw(Kw::Range);
            p.expression();
        });
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
    fn parse_subtype_indication_with_nested_array_element_resolution_function() {
        insta::assert_snapshot!(to_test_text(
            Parser::subtype_indication,
            "((resolved)) unresolved_slv_array"
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
