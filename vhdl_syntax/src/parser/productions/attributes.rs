// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::marker::CompletedMarker;
use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind::*;
use crate::tokens::token_kind::Keyword as Kw;
use crate::tokens::TokenKind::*;

impl Parser {
    pub(crate) fn attribute_specification(&mut self) -> CompletedMarker {
        self.node(AttributeSpecification, |p| {
            p.expect_kw(Kw::Attribute);
            p.identifier();
            p.expect_token(Keyword(Kw::Of));
            p.entity_specification();
            p.expect_token(Keyword(Kw::Is));
            p.expression();
            p.expect_token(SemiColon);
        })
    }

    pub(crate) fn entity_specification(&mut self) {
        self.node(EntitySpecification, |p| {
            p.entity_name_list();
            p.expect_token(Colon);
            p.entity_class();
        });
    }

    pub(crate) fn entity_name_list(&mut self) -> Option<CompletedMarker> {
        match_next_token!(self,
            Keyword(Kw::All) => Some(self.skip_into_node(EntityNameListAll)),
            Keyword(Kw::Others) => Some(self.skip_into_node(EntityNameListOthers)),
            Identifier, StringLiteral, CharacterLiteral => {
                Some(self.separated_list(EntityDesignatorList, Parser::entity_designator, Comma))
            }
        )
    }

    pub(crate) fn entity_class(&mut self) {
        self.expect_one_of_tokens([
            Keyword(Kw::Entity),
            Keyword(Kw::Architecture),
            Keyword(Kw::Configuration),
            Keyword(Kw::Procedure),
            Keyword(Kw::Function),
            Keyword(Kw::Package),
            Keyword(Kw::Type),
            Keyword(Kw::Subtype),
            Keyword(Kw::Constant),
            Keyword(Kw::Signal),
            Keyword(Kw::Variable),
            Keyword(Kw::Component),
            Keyword(Kw::Label),
            Keyword(Kw::Literal),
            Keyword(Kw::Units),
            Keyword(Kw::Group),
            Keyword(Kw::File),
            Keyword(Kw::Property),
            Keyword(Kw::Sequence),
        ]);
    }

    pub(crate) fn entity_designator(&mut self) {
        self.node(EntityDesignator, |p| {
            p.entity_tag();
            if p.peek_token() == LeftSquare {
                p.signature();
            }
        });
    }

    pub(crate) fn entity_tag(&mut self) {
        self.expect_one_of_tokens([Identifier, CharacterLiteral, StringLiteral]);
    }

    pub(crate) fn attribute_declaration_or_specification(&mut self) -> CompletedMarker {
        if self.next_nth_is(Keyword(Kw::Of), 2) {
            self.attribute_specification()
        } else {
            self.attribute_declaration()
        }
    }

    pub(crate) fn attribute_declaration(&mut self) -> CompletedMarker {
        self.node(AttributeDeclaration, |p| {
            p.expect_kw(Kw::Attribute);
            p.identifier();
            p.expect_token(Colon);
            p.type_mark();
            p.expect_token(SemiColon);
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{test_utils::to_test_text, Parser};

    #[test]
    fn parse_simple_attribute_declaration() {
        insta::assert_snapshot!(to_test_text(
            Parser::attribute_declaration,
            "attribute foo : lib.name;"
        ));
    }

    #[test]
    fn parse_simple_attribute_specification() {
        insta::assert_snapshot!(to_test_text(
            Parser::attribute_specification,
            "attribute attr_name of foo : signal is 0+1;",
        ));
    }

    #[test]
    fn simple_attribute_specification_operator_symbol() {
        insta::assert_snapshot!(to_test_text(
            Parser::attribute_specification,
            "attribute attr_name of \"**\" : function is 0+1;",
        ));
    }

    #[test]
    fn attribute_specification_list() {
        insta::assert_snapshot!(to_test_text(
            Parser::attribute_specification,
            "attribute attr_name of foo, bar : signal is 0+1;",
        ));
    }

    #[test]
    fn attribute_specification_all() {
        insta::assert_snapshot!(to_test_text(
            Parser::attribute_specification,
            "attribute attr_name of all : signal is 0+1;",
        ));
    }

    #[test]
    fn attribute_specification_others() {
        insta::assert_snapshot!(to_test_text(
            Parser::attribute_specification,
            "attribute attr_name of others : signal is 0+1;",
        ));
    }

    #[test]
    fn attribute_specification_with_signature() {
        insta::assert_snapshot!(to_test_text(
            Parser::attribute_specification,
            "attribute attr_name of foo[return natural] : function is 0+1;",
        ));
    }
}
