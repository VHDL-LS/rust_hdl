// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com
/// Parsing of scalar types (LRM §5.2)
use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind::*;
use crate::tokens::token_kind::Keyword as Kw;
use crate::tokens::TokenKind::*;

impl Parser {
    pub(crate) fn numeric_type_definition(&mut self) {
        let unknown = self.start_unknown();
        self.range_constraint();
        if self.next_is(Keyword(Kw::Units)) {
            let marker = unknown.resolve(self, PhysicalTypeDefinition);
            self.node(UnitDeclarations, |p| {
                p.expect_token(Keyword(Kw::Units));
                p.primary_unit_declaration();
                while p.next_is(Identifier) {
                    p.secondary_unit_declaration()
                }
            });
            self.physical_type_definition_epilogue();
            marker.complete(self);
        } else {
            unknown.complete(self, NumericTypeDefinition);
        }
    }

    pub(crate) fn physical_type_definition_epilogue(&mut self) {
        self.node(PhysicalTypeDefinitionEpilogue, |p| {
            p.expect_tokens([Keyword(Kw::End), Keyword(Kw::Units)]);
            p.opt_identifier();
        });
    }

    pub(crate) fn enumeration_type_definition(&mut self) {
        self.node(EnumerationTypeDefinition, |p| {
            p.expect_token(LeftPar);
            p.separated_list(EnumerationList, Parser::enumeration_literal, Comma);
            p.expect_token(RightPar);
        });
    }

    pub(crate) fn enumeration_literal(&mut self) {
        self.expect_one_of_tokens([Identifier, CharacterLiteral]);
    }

    pub(crate) fn primary_unit_declaration(&mut self) {
        self.node(PrimaryUnitDeclaration, |p| {
            p.identifier();
            p.expect_token(SemiColon);
        });
    }

    pub(crate) fn secondary_unit_declaration(&mut self) {
        self.node(SecondaryUnitDeclaration, |p| {
            p.identifier();
            p.expect_token(EQ);
            p.physical_literal();
            p.expect_token(SemiColon);
        });
    }

    pub(crate) fn physical_literal(&mut self) {
        self.node(PhysicalLiteral, |p| {
            p.opt_token(AbstractLiteral);
            p.name();
        });
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils::to_test_text;
    use crate::parser::Parser;

    #[test]
    fn integer_type_declaration() {
        insta::assert_snapshot!(to_test_text(
            Parser::type_declaration,
            "type positive_t is range 0 to C_MAX;"
        ));
    }

    #[test]
    fn floating_type_declaration() {
        insta::assert_snapshot!(to_test_text(
            Parser::type_declaration,
            "type some_float_t is range C_MAX downto 3.141592654;"
        ));
    }

    #[test]
    fn physical_type_declaration() {
        insta::assert_snapshot!(to_test_text(
            Parser::type_declaration,
            "\
type dec_t is range 0 to 1e10 units
    prim;
    sec        = 2 prim;
    ter        = 3 prim;
    alias_prim =   prim;
end units;"
        ));

        insta::assert_snapshot!(to_test_text(
            Parser::type_declaration,
            "\
type distance_t is range 0 to 10 units
    m;
end units;"
        ));
    }

    #[test]
    fn test_physical_type_declaration_implicit_secondary_units() {
        insta::assert_snapshot!(to_test_text(
            Parser::type_declaration,
            "\
type phys is range 0 to 15 units
   primary_unit;
   secondary_unit = primary_unit;
end units;"
        ));
    }

    #[test]
    fn enumeration_type_declaration() {
        insta::assert_snapshot!(to_test_text(
            Parser::type_declaration,
            "type enum_t is (A);"
        ));

        insta::assert_snapshot!(to_test_text(
            Parser::type_declaration,
            "type enum_2_t is (S1, S2, S3);"
        ));

        insta::assert_snapshot!(to_test_text(
            Parser::type_declaration,
            "type chars_t is ('A', 'B');"
        ));

        insta::assert_snapshot!(to_test_text(
            Parser::type_declaration,
            "type chars_t is ('A', B);"
        ));
    }

    #[test]
    fn range_expression() {
        // `to`/`downto` are binary operators; ranges are now plain expressions.
        insta::assert_snapshot!(to_test_text(Parser::expression, "100 downto 10"));
        insta::assert_snapshot!(to_test_text(Parser::expression, "0 to 0"));
        insta::assert_snapshot!(to_test_text(Parser::expression, "slv32_t'range"));
    }
}
