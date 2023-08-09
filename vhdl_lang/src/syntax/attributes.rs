// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::common::ParseResult;
use super::expression::parse_expression;
use super::names::parse_type_mark;
use super::subprogram::parse_signature;
use super::tokens::{Kind::*, TokenStream};
use crate::ast::{
    Attribute, AttributeDeclaration, AttributeSpecification, Designator, EntityClass, EntityName,
    EntityTag, WithRef,
};

fn parse_entity_class(stream: &TokenStream) -> ParseResult<EntityClass> {
    Ok(expect_token!(stream, token,
        Entity => EntityClass::Entity,
        Architecture => EntityClass::Architecture,
        Configuration => EntityClass::Configuration,
        Package => EntityClass::Package,
        Signal => EntityClass::Signal,
        Variable => EntityClass::Variable,
        Procedure => EntityClass::Procedure,
        Function => EntityClass::Function,
        Component => EntityClass::Component,
        Constant => EntityClass::Constant,
        Type => EntityClass::Type,
        Label => EntityClass::Label
    ))
}

pub fn parse_entity_name_list(stream: &TokenStream) -> ParseResult<Vec<EntityName>> {
    Ok(expect_token!(stream, token,
        Identifier | StringLiteral => {
            let mut entity_name_list = Vec::new();
            let mut token = token;
            loop {

                let designator = match token.kind {
                    Identifier => token.to_identifier_value()?.map_into(Designator::Identifier),
                    StringLiteral => token.to_operator_symbol()?.map_into(Designator::OperatorSymbol),
                    _ => unreachable!(""),
                };

                let signature = {
                    if stream.peek_kind() == Some(LeftSquare) {
                        Some(parse_signature(stream)?)
                    } else {
                        None
                    }
                };

                entity_name_list.push(EntityName::Name(EntityTag {
                    designator: designator.map_into(WithRef::new),
                    signature,
                }));

                if stream.skip_if_kind(Comma) {
                    token = expect_token!(stream, token, Identifier | StringLiteral => token);
                } else {
                    break entity_name_list;
                }
            }
        },
        Others => {
            vec![EntityName::Others]
        },
        All => {
            vec![EntityName::All]
        }
    ))
}

pub fn parse_attribute(stream: &TokenStream) -> ParseResult<Vec<Attribute>> {
    stream.expect_kind(Attribute)?;
    let ident = stream.expect_ident()?;
    Ok(expect_token!(stream, token,
        Colon => {
            let type_mark = parse_type_mark(stream)?;
            stream.expect_kind(SemiColon)?;
            vec![Attribute::Declaration(AttributeDeclaration {
                ident: ident.into(),
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
                        ident: WithRef::new(ident.clone()),
                        entity_name,
                        entity_class,
                        expr: expr.clone(),
                    })
                }).collect()
        }
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::test::Code;

    #[test]
    fn parse_simple_attribute_declaration() {
        let code = Code::new("attribute foo : lib.name;");
        assert_eq!(
            code.with_stream(parse_attribute),
            vec![Attribute::Declaration(AttributeDeclaration {
                ident: code.s1("foo").decl_ident(),
                type_mark: code.s1("lib.name").type_mark()
            })]
        )
    }

    #[test]
    fn parse_simple_attribute_specification() {
        let code = Code::new("attribute attr_name of foo : signal is 0+1;");
        assert_eq!(
            code.with_stream(parse_attribute),
            vec![Attribute::Specification(AttributeSpecification {
                ident: WithRef::new(code.s1("attr_name").ident()),
                entity_name: EntityName::Name(EntityTag {
                    designator: code.s1("foo").ref_designator(),
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
                ident: WithRef::new(code.s1("attr_name").ident()),
                entity_name: EntityName::Name(EntityTag {
                    designator: code.s1("\"**\"").ref_designator(),
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
                    ident: WithRef::new(code.s1("attr_name").ident()),
                    entity_name: EntityName::Name(EntityTag {
                        designator: code.s1("foo").ref_designator(),
                        signature: None
                    }),
                    entity_class: EntityClass::Signal,
                    expr: code.s1("0+1").expr()
                }),
                Attribute::Specification(AttributeSpecification {
                    ident: WithRef::new(code.s1("attr_name").ident()),
                    entity_name: EntityName::Name(EntityTag {
                        designator: code.s1("bar").ref_designator(),
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
                ident: WithRef::new(code.s1("attr_name").ident()),
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
                ident: WithRef::new(code.s1("attr_name").ident()),
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
                ident: WithRef::new(code.s1("attr_name").ident()),
                entity_name: EntityName::Name(EntityTag {
                    designator: code.s1("foo").ref_designator(),
                    signature: Some(code.s1("[return natural]").signature())
                }),
                entity_class: EntityClass::Function,
                expr: code.s1("0+1").expr()
            })]
        )
    }
}
