// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::marker::CompletedMarker;
use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind::*;
use crate::tokens::Keyword as Kw;
use crate::tokens::TokenKind::*;

fn is_start_of_attribute_name(parser: &mut Parser) -> bool {
    // Checking for `LeftSquare || Tick` will result in ambiguities with other grammar rules where a signature is possible right after a name.
    // Those rules can be `alias_declaration` (LRM §6.6.1) and `subprogram_instantiation_declaration` (LRM §4.4).
    // By checking whether the closing square bracket is followed by a `Tick` this ambiguity is resolved
    match parser.peek_token() {
        Tick => {
            matches!(parser.peek_nth_token(1), Identifier | Keyword(_))
        }
        LeftSquare => {
            let mut idx = 1;
            let mut bracket_count = 1;

            while bracket_count > 0 {
                match parser.peek_nth_token(idx) {
                    LeftSquare => bracket_count += 1,
                    RightSquare => bracket_count -= 1,
                    Eof => {
                        return false;
                    }
                    _ => {}
                }

                idx += 1;
            }

            parser.next_nth_is(Tick, idx)
        }
        _ => false,
    }
}

impl Parser {
    pub(crate) fn name(&mut self) -> CompletedMarker {
        // (Based on) LRM §8.1
        // The LRM grammar rules for names were transformed to avoid left recursion.

        // In contrast to the LRM, this parsing routine is greedy. Meaning, it will consume trailing parenthesized
        // expressions even if the belong to an outer grammar rule!
        self.node(Name, |p| {
            if p.next_is(LtLt) {
                p.external_name();
            } else {
                p.node(NameDesignatorPrefix, |p| {
                    p.expect_one_of_tokens([Identifier, StringLiteral, CharacterLiteral]);
                });
            }

            while p.opt_name_tail() {}

            // Ambiguity: `range <>` is the tail of an index subtype definition.
            // This wires through name due to the starting `type_mark`.
            // TODO: consider alternative: broaden language to make "<>" a valid expression.
            // Creates less ambiguity here.
            if p.next_is(Keyword(Kw::Range)) && !p.next_nth_is(BOX, 1) {
                p.range_constraint();
            }
        })
    }

    pub(crate) fn type_mark(&mut self) -> CompletedMarker {
        self.name()
    }

    pub(crate) fn opt_designator(&mut self) {
        self.opt_tokens([Identifier, StringLiteral]);
    }

    pub(crate) fn designator(&mut self) {
        self.expect_one_of_tokens([Identifier, StringLiteral]);
    }

    pub(crate) fn label(&mut self) {
        self.node(StmtLabel, |p| {
            p.expect_tokens([Identifier, Colon]);
        });
    }

    pub(crate) fn opt_label(&mut self) {
        if self.next_is(Identifier) && self.next_nth_is(Colon, 1) {
            self.node(StmtLabel, |p| {
                p.skip_n(2);
            });
        }
    }

    pub(crate) fn name_list(&mut self) -> CompletedMarker {
        self.separated_list(NameList, Parser::name, Comma)
    }

    fn suffix(&mut self) {
        // LRM §8.3
        // suffix ::= identifier | string_literal | character_literal | `all` ;
        self.expect_one_of_tokens([
            Identifier,
            StringLiteral,
            CharacterLiteral,
            Keyword(Kw::All),
        ]);
    }

    fn opt_name_tail(&mut self) -> bool {
        match self.peek_token() {
            Dot => {
                self.node(SelectedName, |p| {
                    p.expect_token(Dot);
                    p.suffix();
                });
                true
            }
            LeftPar => {
                self.node(ParenthesizedName, |p| {
                    p.expect_token(LeftPar);
                    p.association_list();
                    p.expect_token(RightPar);
                });
                true
            }
            _ => {
                if is_start_of_attribute_name(self) {
                    self.node(AttributeName, |p| {
                        if p.next_is(LeftSquare) {
                            p.signature();
                        }
                        p.expect_token(Tick);
                        // Either an identifier or a keyword (e.g., `range`, `subtype`).
                        if matches!(p.peek_token(), Keyword(_) | Identifier) {
                            p.skip();
                        }
                    });
                    true
                } else {
                    false
                }
            }
        }
    }

    pub(crate) fn external_name(&mut self) {
        // LRM §8.7
        let unknown = self.start_unknown();
        self.expect_token(LtLt);

        let tok = self.expect_one_of_tokens([
            Keyword(Kw::Constant),
            Keyword(Kw::Signal),
            Keyword(Kw::Variable),
        ]);
        let marker = unknown.resolve(
            self,
            match tok {
                Some(Keyword(Kw::Signal)) => ExternalSignalName,
                Some(Keyword(Kw::Variable)) => ExternalVariableName,
                _ => ExternalConstantName,
            },
        );
        self.external_pathname();
        self.expect_token(Colon);
        self.subtype_indication();

        self.expect_token(GtGt);
        marker.complete(self);
    }

    fn external_pathname(&mut self) {
        // LRM §8.7
        // No node is opened on the recovery path, hence the `Option`.
        let marker = match_next_token!(self,
        CommAt => {
            let marker = self.start_node(PackagePathname);
            self.skip();
            self.separated_list(PackagePath, Parser::identifier, Dot);
            Some(marker)
        },
        Dot => {
            let marker = self.start_node(AbsolutePathname);
            self.skip();
            self.partial_pathname();
            Some(marker)
        },
        Circ, Identifier => {
            let marker = self.start_node(RelativePathname);
            while self.next_is(Circ) {
                self.node(UpLevel, |p| {
                    p.skip(); // Circ
                    p.expect_token(Dot);
                });
            }
            self.partial_pathname();
            Some(marker)
        });
        if let Some(marker) = marker {
            marker.complete(self);
        }
    }

    fn partial_pathname(&mut self) {
        // LRM §8.7
        // partial_pathname ::= { pathname_element `.` } object_simple_name ;
        self.separated_list(PartialPathname, Parser::pathname_element, Dot);
    }

    fn pathname_element(&mut self) {
        self.node(PathnameElement, |p| {
            p.identifier();
            if p.next_is(LeftPar) {
                p.node(ParenthesizedExpression, |p| {
                    p.expect_token(LeftPar);
                    p.expression();
                    p.expect_token(RightPar);
                });
            }
        });
    }

    pub(crate) fn choices(&mut self) {
        self.separated_list(Choices, Parser::choice, Bar);
    }

    pub(crate) fn choice(&mut self) {
        if self.next_is(Keyword(Kw::Others)) {
            self.node(OthersChoice, |p| {
                p.skip();
            });
            return;
        }
        // `expression` now subsumes the old `range` (`to`/`downto` are binary
        // operators); `choice = expression | discrete_range | others` collapses
        // to "either an expression or `others`" at the parser level.
        self.node(ExpressionChoice, |p| {
            p.expression();
        });
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{test_utils::to_test_text, Parser};

    fn name_to_test_text(code: &str) -> String {
        to_test_text(Parser::name, code)
    }

    #[test]
    fn test_identifier_list() {
        insta::assert_snapshot!(to_test_text(Parser::identifier_list, "foo, bar, baz"));
    }

    #[test]
    fn test_simple_name() {
        insta::assert_snapshot!(name_to_test_text("foo"));
    }

    #[test]
    fn test_characer_name() {
        insta::assert_snapshot!(name_to_test_text("'a'"));
    }

    #[test]
    fn test_operator_symbol() {
        insta::assert_snapshot!(name_to_test_text("\"+\""));
        insta::assert_snapshot!(name_to_test_text("\"AND\""));
        insta::assert_snapshot!(name_to_test_text("\"and\""));
    }

    #[test]
    fn test_parse_selected_name_multiple() {
        insta::assert_snapshot!(name_to_test_text("foo.bar.baz"));
    }

    #[test]
    fn test_parse_selected_name_all() {
        insta::assert_snapshot!(name_to_test_text("foo.all"));
    }

    #[test]
    fn test_slice_name_range() {
        insta::assert_snapshot!(name_to_test_text("prefix(0 to 3)"));
        insta::assert_snapshot!(name_to_test_text("prefix(3 downto 0)"));
    }

    #[test]
    fn test_slice_range_attribute() {
        insta::assert_snapshot!(name_to_test_text("prefix(foo(0)'range)"));
    }

    #[test]
    fn test_attribute_name() {
        insta::assert_snapshot!(name_to_test_text("prefix'foo"));
        insta::assert_snapshot!(name_to_test_text("prefix'range"));
        insta::assert_snapshot!(name_to_test_text("prefix'subtype"));
        insta::assert_snapshot!(name_to_test_text("prefix'element"));
    }

    #[test]
    fn test_attribute_name_expression() {
        insta::assert_snapshot!(name_to_test_text("prefix'foo(expr+1)"));
    }

    #[test]
    fn test_attribute_name_signature_expression() {
        insta::assert_snapshot!(name_to_test_text("prefix[return natural]'foo(expr+1)"));
    }

    #[test]
    fn test_function_call_no_formal() {
        insta::assert_snapshot!(name_to_test_text("foo(0)"));
    }

    #[test]
    fn test_function_call_many() {
        insta::assert_snapshot!(name_to_test_text("prefix(0, 1)(3).suffix"));
    }

    #[test]
    fn test_function_call() {
        insta::assert_snapshot!(name_to_test_text("foo(arg => 0)"));
    }

    #[test]
    fn test_external_name_implicit_relative() {
        insta::assert_snapshot!(name_to_test_text("<< signal dut.foo : std_logic >>"));
    }

    #[test]
    fn test_external_name_explicit_relative() {
        insta::assert_snapshot!(name_to_test_text("<< signal ^.dut.gen : std_logic >>"));
    }

    #[test]
    fn test_external_name_explicit_relative_multiple_levels() {
        insta::assert_snapshot!(name_to_test_text("<< signal ^.^.^.dut.gen : std_logic >>"));
    }

    #[test]
    fn test_external_name_absolute() {
        insta::assert_snapshot!(name_to_test_text("<< signal .dut.gen : std_logic >>"));
    }

    #[test]
    fn test_external_name_package() {
        insta::assert_snapshot!(name_to_test_text("<< signal @lib.pkg : std_logic >>"));
    }

    #[test]
    fn test_external_name_object_classes() {
        insta::assert_snapshot!(name_to_test_text("<< constant dut.foo : std_logic >>"));
        insta::assert_snapshot!(name_to_test_text("<< signal dut.foo : std_logic >>"));
        insta::assert_snapshot!(name_to_test_text("<< variable dut.foo : std_logic >>"));
    }

    #[test]
    fn empty_association_list() {
        assert_recovery_snapshot!("foo()", Parser::name);
    }
}
