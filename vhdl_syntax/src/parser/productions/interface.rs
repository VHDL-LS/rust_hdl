// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::marker::Precede;
use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind::*;
use crate::syntax::NodeKind;
use crate::tokens::Keyword as Kw;
use crate::tokens::TokenKind::*;

struct PortOrGenericSpec {
    pub node_kind: NodeKind,
    pub keyword: Kw,
}

const PORT_SPEC: PortOrGenericSpec = PortOrGenericSpec {
    node_kind: PortClause,
    keyword: Kw::Port,
};

const GENERIC_SPEC: PortOrGenericSpec = PortOrGenericSpec {
    node_kind: GenericClause,
    keyword: Kw::Generic,
};

impl Parser {
    pub(crate) fn opt_generic_clause(&mut self) {
        if self.next_is(Keyword(Kw::Generic)) {
            self.generic_clause();
        }
    }

    pub(crate) fn generic_clause(&mut self) {
        self.port_or_generic_clause(GENERIC_SPEC);
    }

    pub(crate) fn opt_port_clause(&mut self) {
        if self.next_is(Keyword(Kw::Port)) {
            self.port_clause();
        }
    }

    pub(crate) fn port_clause(&mut self) {
        self.port_or_generic_clause(PORT_SPEC);
    }

    fn port_or_generic_clause(&mut self, spec: PortOrGenericSpec) {
        self.node(spec.node_kind, |p| {
            p.expect_kw(spec.keyword);
            p.expect_token(LeftPar);
            p.interface_list();
            p.expect_tokens([RightPar, SemiColon]);
        });
    }

    pub(crate) fn interface_list(&mut self) {
        self.separated_list(InterfaceList, Parser::interface_declaration, SemiColon);
    }

    pub(crate) fn interface_declaration(&mut self) {
        match_next_token!(self,
            Keyword(Kw::Signal), Keyword(Kw::Constant), Keyword(Kw::Variable), Identifier => {
                self.interface_object_declaration();
            },
            Keyword(Kw::File) => self.interface_file_declaration(),
            Keyword(Kw::Type) => self.interface_type_declaration(),
            Keyword(Kw::Function), Keyword(Kw::Procedure), Keyword(Kw::Impure), Keyword(Kw::Pure) => {
                self.interface_subprogram_declaration()
            },
            Keyword(Kw::Package) => self.interface_package_declaration(),
        );
    }

    pub(crate) fn interface_file_declaration(&mut self) {
        self.node(InterfaceFileDeclaration, |p| {
            p.expect_kw(Kw::File);
            p.identifier_list();
            p.expect_token(Colon);
            p.subtype_indication();
        });
    }

    pub(crate) fn interface_type_declaration(&mut self) {
        self.node(InterfaceIncompleteTypeDeclaration, |p| {
            p.expect_kw(Kw::Type);
            p.identifier();
        });
    }

    pub(crate) fn interface_subprogram_declaration(&mut self) {
        self.node(InterfaceSubprogramDeclaration, |p| {
            p.interface_subprogram_specification();
            if p.next_is(Keyword(Kw::Is)) {
                p.node(SubprogramDefault, |p| {
                    p.skip(); // Kw::Is
                    p.interface_subprogram_default();
                });
            }
        });
    }

    pub(crate) fn interface_subprogram_default(&mut self) {
        if self.next_is(BOX) {
            self.skip_into_node(InterfaceSubprogramDefaultBox);
        } else {
            self.node(InterfaceSubprogramDefaultName, |p| {
                p.name();
            });
        }
    }

    pub(crate) fn interface_subprogram_specification(&mut self) {
        if self.next_is(Keyword(Kw::Procedure)) {
            self.interface_procedure_specification();
        } else {
            self.interface_function_specification();
        }
    }

    pub(crate) fn interface_procedure_specification(&mut self) {
        self.node(InterfaceProcedureSpecification, |p| {
            p.expect_kw(Kw::Procedure);
            p.designator();
            p.opt_parameter_list();
        });
    }

    pub(crate) fn interface_function_specification(&mut self) {
        self.node(InterfaceFunctionSpecification, |p| {
            p.opt_function_purity();
            p.expect_kw(Kw::Function);
            p.designator();
            p.opt_parameter_list();
            p.expect_kw(Kw::Return);
            p.type_mark();
        });
    }

    pub(crate) fn opt_function_purity(&mut self) {
        self.opt_tokens([Keyword(Kw::Pure), Keyword(Kw::Impure)]);
    }

    pub(crate) fn interface_package_declaration(&mut self) {
        self.node(InterfacePackageDeclaration, |p| {
            p.interface_package_declaration_preamble();
            p.expect_kw(Kw::New);
            p.name();
            p.interface_package_generic_map_aspect();
        });
    }

    pub(crate) fn interface_package_declaration_preamble(&mut self) {
        self.node(InterfacePackageDeclarationPreamble, |p| {
            p.expect_kw(Kw::Package);
            p.identifier();
            p.expect_kw(Kw::Is);
        });
    }

    pub(crate) fn interface_package_generic_map_aspect(&mut self) {
        self.node(InterfacePackageGenericMapAspect, |p| {
            p.expect_kw(Kw::Generic);
            p.expect_kw(Kw::Map);
            p.expect_token(LeftPar);
            if p.next_is(BOX) {
                p.skip_into_node(InterfacePackageGenericMapAspectBox);
            } else if p.next_is(Keyword(Kw::Default)) {
                p.skip_into_node(InterfacePackageGenericMapAspectDefault);
            } else {
                p.node(InterfacePackageGenericMapAspectAssociations, |p| {
                    p.association_list();
                });
            }
            p.expect_token(RightPar);
        });
    }

    pub(crate) fn interface_object_declaration(&mut self) {
        // The object class (constant/signal/variable) is optional and not
        // reliably distinguishable here, so a single node covers all three; the
        // explicit class keyword, when present, is kept as a child.
        self.node(InterfaceObjectDeclaration, |p| {
            p.opt_tokens([
                Keyword(Kw::Signal),
                Keyword(Kw::Constant),
                Keyword(Kw::Variable),
            ]);
            p.identifier_list();
            p.expect_token(Colon);
            p.opt_mode();
            p.subtype_indication();
            p.opt_token(Keyword(Kw::Bus));
            if p.next_is(ColonEq) {
                p.node(InitialValue, |p| {
                    p.skip(); // ColonEq
                    p.expression();
                });
            }
        });
    }

    pub(crate) fn opt_mode(&mut self) {
        self.opt_tokens([
            Keyword(Kw::In),
            Keyword(Kw::Out),
            Keyword(Kw::Inout),
            Keyword(Kw::Buffer),
            Keyword(Kw::Linkage),
        ]);
    }

    pub(crate) fn association_list(&mut self) {
        self.separated_list(
            AssociationList,
            |parser| {
                let end_of_element_idx =
                    match parser.lookahead_max_token_index(usize::MAX, [Comma, RightPar]) {
                        Ok((_, idx)) => idx,
                        Err((_, idx)) => idx,
                    };
                parser.association_element_bounded(end_of_element_idx);
            },
            Comma,
        );
    }

    fn association_element_bounded(&mut self, max_index: usize) {
        self.node(AssociationElement, |p| {
            // TODO: Error handling is done at a bare minimum.
            if p.lookahead_max_token_index(max_index, [RightArrow]).is_ok() {
                p.node(Formal, |p| {
                    p.formal_part();
                    p.expect_token(RightArrow);
                });
            }
            p.actual_part();
        });
    }

    pub(crate) fn formal_part(&mut self) {
        // Note: `self.name()` will already consume any trailing parenthesized names!
        self.name();
    }

    pub(crate) fn actual_part(&mut self) {
        // actual_part       ::= actual_designator | name "(" actual_designator ")"
        //                     | type_mark "(" actual_designator ")"
        // actual_designator ::= [inertial] expression | subtype_indication | open
        //
        // Dispatch between `Expression` and `SubtypeIndication` is decided by
        // a single lookahead: the only syntactic signal that a subtype is
        // intended is a second name following the first (either the bare
        // `resolution_function_name type_mark` form or the
        // `(element_resolution) type_mark` form). The LRM-wrapped forms
        // `name "(" actual_designator ")"` collapse into `Expression`
        // because `Name` accepts a `ParenthesizedName` tail; analysis sorts
        // them out later.
        self.node(ActualPart, |p| {
            p.opt_token(Keyword(Kw::Inertial));

            if p.next_is(Keyword(Kw::Open)) {
                p.node(ActualPartOpen, |p| {
                    p.skip();
                });
            } else if p.next_is(LeftPar) {
                // Look past the matching `)`. If a name-starter follows, the
                // parenthesized group is an `(element_resolution)` and we are in
                // a subtype_indication; otherwise it is a parenthesized
                // expression / aggregate.
                let is_subtype = match p.lookahead_skip_n(1, [RightPar]) {
                    Ok((_, end_index)) => matches!(
                        p.peek_nth_token(end_index - p.token_index() + 1),
                        Identifier | StringLiteral | CharacterLiteral | LtLt
                    ),
                    Err(_) => false,
                };
                if is_subtype {
                    p.node(ActualPartSubtypeIndication, |p| {
                        p.subtype_indication();
                    });
                } else {
                    p.node(ActualPartExpression, |p| {
                        p.expression();
                    });
                }
            } else if matches!(
                p.peek_token(),
                Identifier | StringLiteral | CharacterLiteral | LtLt
            ) {
                // The actual_part starts with a name. Parse it greedily; if a
                // second name follows, the first was a `resolution_function_name`
                // and we promote to subtype_indication retroactively. Otherwise
                // the name is the leading primary of an expression.
                let name = p.name();
                if matches!(
                    p.peek_token(),
                    Identifier | StringLiteral | CharacterLiteral | LtLt
                ) {
                    let resolution = name.precede(p, NameResolutionIndication).complete(p);
                    let subtype = resolution.precede(p, SubtypeIndication);
                    p.name(); // type_mark
                    let subtype = subtype.complete(p);
                    subtype.precede(p, ActualPartSubtypeIndication).complete(p);
                } else {
                    let primary = p.continue_primary_after_name(name);
                    let expression = p.expression_from_primary(primary);
                    expression.precede(p, ActualPartExpression).complete(p);
                }
            } else {
                p.node(ActualPartExpression, |p| {
                    p.expression();
                });
            }
        });
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
    fn actual_part_open() {
        insta::assert_snapshot!(to_test_text(Parser::actual_part, "open"));
    }

    #[test]
    fn actual_part_inertial_open() {
        // `inertial` only meaningfully binds to an expression per the LRM;
        // we accept it before any body and let analysis reject misuse.
        insta::assert_snapshot!(to_test_text(Parser::actual_part, "inertial open"));
    }

    #[test]
    fn actual_part_expression() {
        insta::assert_snapshot!(to_test_text(Parser::actual_part, "42"));
        insta::assert_snapshot!(to_test_text(Parser::actual_part, "lib.pkg.sig"));
        insta::assert_snapshot!(to_test_text(Parser::actual_part, "a + b * 2"));
        insta::assert_snapshot!(to_test_text(Parser::actual_part, "-foo"));
        insta::assert_snapshot!(to_test_text(Parser::actual_part, "(foo) + 1"));
        insta::assert_snapshot!(to_test_text(Parser::actual_part, "(a, b, c)"));
    }

    #[test]
    fn actual_part_expression_type_conversion_shape() {
        // `std_ulogic(p)` — the LRM `type_mark "(" actual_designator ")"`
        // wrapped form. Broadened into an Expression whose Name has a
        // ParenthesizedName tail; analysis recovers the conversion.
        insta::assert_snapshot!(to_test_text(Parser::actual_part, "std_ulogic(p)"));
    }

    #[test]
    fn actual_part_expression_inertial() {
        insta::assert_snapshot!(to_test_text(Parser::actual_part, "inertial a + 1"));
    }

    #[test]
    fn actual_part_expression_qualified() {
        insta::assert_snapshot!(to_test_text(Parser::actual_part, "T'(1, 2, 3)"));
    }

    #[test]
    fn actual_part_subtype_resolution_function_name() {
        // `resolve std_logic` — two adjacent names ⇒ subtype_indication
        // with a `NameResolutionIndication`.
        insta::assert_snapshot!(to_test_text(Parser::actual_part, "resolve std_logic"));
    }

    #[test]
    fn actual_part_subtype_resolution_function_selected_name() {
        insta::assert_snapshot!(to_test_text(
            Parser::actual_part,
            "lib.pkg.resolve std_logic"
        ));
    }

    #[test]
    fn actual_part_subtype_paren_array_resolution() {
        // `(resolve) bit_vector` — `(element_resolution)` followed by a
        // type_mark name ⇒ subtype_indication.
        insta::assert_snapshot!(to_test_text(Parser::actual_part, "(resolve) bit_vector"));
    }

    #[test]
    fn actual_part_subtype_paren_record_resolution() {
        // Record element resolution — `(field fn)` followed by a type_mark.
        insta::assert_snapshot!(to_test_text(Parser::actual_part, "(elem resolve) rec_t"));
    }

    #[test]
    fn actual_part_subtype_with_range_constraint() {
        // `integer range 0 to 7` — bare type_mark with a range constraint.
        // `name()` greedily consumes the trailing range, so this is
        // broadened into an `ActualPartExpression` whose `Name` carries a
        // `RangeConstraint` tail; analysis recognizes the subtype shape.
        insta::assert_snapshot!(to_test_text(Parser::actual_part, "integer range 0 to 7"));
    }

    #[test]
    fn actual_part_subtype_with_array_constraint_shape() {
        // `bit_vector(7 downto 0)` — bare type_mark with an array
        // constraint, indistinguishable from an indexed-name expression at
        // parse time and emitted as `ActualPartExpression`.
        insta::assert_snapshot!(to_test_text(Parser::actual_part, "bit_vector(7 downto 0)"));
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
    fn parses_interface_subprogram_with_parameters() {
        insta::assert_snapshot!(to_test_text(
            Parser::interface_declaration,
            "function Match(Actual : ActualType) return boolean"
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

    // MARK: Error recovery

    #[test]
    fn port_missing_colon() {
        assert_recovery_snapshot!("port (clk in std_logic);", Parser::port_clause);
    }

    #[test]
    fn port_clause_unclosed_paren() {
        assert_recovery_snapshot!("port (clk : in std_logic;", Parser::port_clause);
    }

    #[test]
    fn generic_clause_unclosed_paren() {
        assert_recovery_snapshot!("generic (width : integer", Parser::generic_clause);
    }

    #[test]
    fn empty_generic_clause() {
        assert_recovery_snapshot!("generic();", Parser::generic_clause);
    }

    #[test]
    fn empty_port_clause() {
        assert_recovery_snapshot!("port();", Parser::port_clause);
    }

    #[test]
    fn interface_list_doubled_separator() {
        assert_recovery_snapshot!(
            "port (clk : in std_logic;; rst : in std_logic);",
            Parser::port_clause
        );
    }

    #[ignore = "missing list separator is silently mis-parsed (resolution-indication \
        ambiguity + unanchored separated_list); needs more resilient parsing"]
    #[test]
    fn interface_list_missing_separator() {
        assert_recovery_snapshot!(
            "clk : in std_logic rst : in std_logic",
            Parser::interface_list
        );
    }
}
