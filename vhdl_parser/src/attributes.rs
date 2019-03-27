// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use crate::ast::{
    Attribute, AttributeDeclaration, AttributeSpecification, Designator, EntityClass, EntityName,
    EntityTag,
};
use crate::expression::parse_expression;
use crate::message::ParseResult;
use crate::names::parse_selected_name;
use crate::subprogram::parse_signature;
use crate::tokenizer::Kind::*;
use crate::tokenstream::TokenStream;

fn parse_entity_class(stream: &mut TokenStream) -> ParseResult<EntityClass> {
    let token = stream.expect()?;
    Ok(try_token_kind!(
        token,
        Entity => EntityClass::Entity,
        Architecture => EntityClass::Architecture,
        Configuration => EntityClass::Configuration,
        Package => EntityClass::Package,
        Signal => EntityClass::Signal,
        Variable => EntityClass::Variable,
        Procedure => EntityClass::Procedure,
        Function => EntityClass::Function,
        Component => EntityClass::Component
    ))
}

pub fn parse_entity_name_list(stream: &mut TokenStream) -> ParseResult<Vec<EntityName>> {
    let token = stream.peek_expect()?;
    Ok(try_token_kind!(
        token,
        Identifier | StringLiteral => {
            let mut entity_name_list = Vec::new();
            loop {
                let designator_token = stream.expect()?;
                let designator = try_token_kind!(
                    designator_token,
                    Identifier => designator_token.expect_ident()?.map_into(Designator::Identifier),
                    StringLiteral => designator_token.expect_string()?.map_into(Designator::OperatorSymbol));

                let signature = {
                    if stream.peek_kind()? == Some(LeftSquare) {
                        Some(parse_signature(stream)?)
                    } else {
                        None
                    }
                };

                entity_name_list.push(EntityName::Name(EntityTag {
                    designator,
                    signature,
                }));

                let sep_token = stream.peek_expect()?;

                try_token_kind!(
                    sep_token,

                    Comma => {
                        stream.move_after(&sep_token);
                    },
                    Colon => {
                        break entity_name_list;
                    }
                )
            }
        },
        Others => {
            stream.move_after(&token);
            vec![EntityName::Others]
        },
        All => {
            stream.move_after(&token);
            vec![EntityName::All]
        }
    ))
}

pub fn parse_attribute(stream: &mut TokenStream) -> ParseResult<Vec<Attribute>> {
    stream.expect_kind(Attribute)?;
    let ident = stream.expect_ident()?;
    let token = stream.expect()?;

    Ok(try_token_kind!(
        token,
        Colon => {
            let type_mark = parse_selected_name(stream)?;
            stream.expect_kind(SemiColon)?;
            vec![Attribute::Declaration(AttributeDeclaration {
                ident,
                type_mark,
            })]
        },
        Of => {
            let entity_names = parse_entity_name_list(stream)?;
            stream.expect_kind(Colon)?;
            let entity_class = parse_entity_class(stream)?;
            stream.expect_kind(Is)?;
            let expr = parse_expression(stream)?;
            stream.expect_kind(SemiColon)?;

            entity_names
                .into_iter()
                .map(|entity_name| {
                    Attribute::Specification(AttributeSpecification {
                        ident: ident.clone(),
                        entity_name: entity_name.clone(),
                        entity_class: entity_class,
                        expr: expr.clone(),
                    })
                }).collect()
        }
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::Code;

    #[test]
    fn parse_simple_attribute_declaration() {
        let code = Code::new("attribute foo : lib.name;");
        assert_eq!(
            code.with_stream(parse_attribute),
            vec![Attribute::Declaration(AttributeDeclaration {
                ident: code.s1("foo").ident(),
                type_mark: code.s1("lib.name").selected_name()
            })]
        )
    }

    #[test]
    fn parse_simple_attribute_specification() {
        let code = Code::new("attribute attr_name of foo : signal is 0+1;");
        assert_eq!(
            code.with_stream(parse_attribute),
            vec![Attribute::Specification(AttributeSpecification {
                ident: code.s1("attr_name").ident(),
                entity_name: EntityName::Name(EntityTag {
                    designator: code.s1("foo").designator(),
                    signature: None
                }),
                entity_class: EntityClass::Signal,
                expr: code.s1("0+1").expr()
            })]
        )
    }

    #[test]
    fn parse_simple_attribute_specification_operator_symbol() {
        let code = Code::new("attribute attr_name of \"**\" : function is 0+1;");
        assert_eq!(
            code.with_stream(parse_attribute),
            vec![Attribute::Specification(AttributeSpecification {
                ident: code.s1("attr_name").ident(),
                entity_name: EntityName::Name(EntityTag {
                    designator: code.s1("\"**\"").designator(),
                    signature: None
                }),
                entity_class: EntityClass::Function,
                expr: code.s1("0+1").expr()
            })]
        )
    }

    #[test]
    fn parse_attribute_specification_list() {
        let code = Code::new("attribute attr_name of foo, bar : signal is 0+1;");
        assert_eq!(
            code.with_stream(parse_attribute),
            vec![
                Attribute::Specification(AttributeSpecification {
                    ident: code.s1("attr_name").ident(),
                    entity_name: EntityName::Name(EntityTag {
                        designator: code.s1("foo").designator(),
                        signature: None
                    }),
                    entity_class: EntityClass::Signal,
                    expr: code.s1("0+1").expr()
                }),
                Attribute::Specification(AttributeSpecification {
                    ident: code.s1("attr_name").ident(),
                    entity_name: EntityName::Name(EntityTag {
                        designator: code.s1("bar").designator(),
                        signature: None
                    }),
                    entity_class: EntityClass::Signal,
                    expr: code.s1("0+1").expr()
                })
            ]
        )
    }

    #[test]
    fn parse_attribute_specification_all() {
        let code = Code::new("attribute attr_name of all : signal is 0+1;");
        assert_eq!(
            code.with_stream(parse_attribute),
            vec![Attribute::Specification(AttributeSpecification {
                ident: code.s1("attr_name").ident(),
                entity_name: EntityName::All,
                entity_class: EntityClass::Signal,
                expr: code.s1("0+1").expr()
            })]
        )
    }

    #[test]
    fn parse_attribute_specification_others() {
        let code = Code::new("attribute attr_name of others : signal is 0+1;");
        assert_eq!(
            code.with_stream(parse_attribute),
            vec![Attribute::Specification(AttributeSpecification {
                ident: code.s1("attr_name").ident(),
                entity_name: EntityName::Others,
                entity_class: EntityClass::Signal,
                expr: code.s1("0+1").expr()
            })]
        )
    }

    #[test]
    fn parse_attribute_specification_with_signature() {
        let code = Code::new("attribute attr_name of foo[return natural] : function is 0+1;");
        assert_eq!(
            code.with_stream(parse_attribute),
            vec![Attribute::Specification(AttributeSpecification {
                ident: code.s1("attr_name").ident(),
                entity_name: EntityName::Name(EntityTag {
                    designator: code.s1("foo").designator(),
                    signature: Some(code.s1("[return natural]").signature())
                }),
                entity_class: EntityClass::Function,
                expr: code.s1("0+1").expr()
            })]
        )
    }

}
