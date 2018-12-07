// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use ast::{
    AbstractLiteral, ArrayIndex, ElementDeclaration, EnumerationLiteral, Ident, Literal,
    PhysicalTypeDeclaration, ProtectedTypeBody, ProtectedTypeDeclaration,
    ProtectedTypeDeclarativeItem, Range, TypeDeclaration, TypeDefinition,
};
use common::error_on_end_identifier_mismatch;
use declarative_part::parse_declarative_part;
use message::{push_some, MessageHandler, ParseResult};
use names::{parse_identifier_list, parse_selected_name};
use range::{parse_array_index_constraint, parse_range};
use subprogram::parse_subprogram_declaration;
use subtype_indication::parse_subtype_indication;
use tokenizer::Kind::*;
use tokenstream::TokenStream;

/// LRM 5.2.2 Enumeration types
fn parse_enumeration_type_definition(stream: &mut TokenStream) -> ParseResult<TypeDefinition> {
    let mut enum_literals = Vec::new();
    loop {
        let literal_token = stream.expect()?;

        try_token_kind!(
            literal_token,
            Identifier | Character => {
                let enum_literal = try_token_kind!(
                    literal_token,
                    Identifier => literal_token.expect_ident()?.map_into(EnumerationLiteral::Identifier),
                    Character => literal_token.expect_character()?.map_into(EnumerationLiteral::Character)
                );
                enum_literals.push(enum_literal);

                try_token_kind!(
                    stream.expect()?,
                    RightPar => {
                        stream.expect_kind(SemiColon)?;
                        break;
                    },
                    Comma => {}
                );
            }
        );
    }

    Ok(TypeDefinition::Enumeration(enum_literals))
}

fn parse_array_index_constraints(stream: &mut TokenStream) -> ParseResult<Vec<ArrayIndex>> {
    stream.expect_kind(LeftPar)?;
    let mut indexes = Vec::new();
    loop {
        indexes.push(parse_array_index_constraint(stream)?);

        try_token_kind!(
            stream.expect()?,
            RightPar => {
                return Ok(indexes);
            },
            Comma => {}
        )
    }
}

/// LRM 5.3.2 Array types
fn parse_array_type_definition(stream: &mut TokenStream) -> ParseResult<TypeDefinition> {
    let index_constraints = parse_array_index_constraints(stream)?;
    stream.expect_kind(Of)?;
    let element_subtype = parse_subtype_indication(stream)?;
    stream.expect_kind(SemiColon)?;
    Ok(TypeDefinition::Array(index_constraints, element_subtype))
}

/// LRM 5.3.3 Record types
fn parse_record_type_definition(
    stream: &mut TokenStream,
) -> ParseResult<(TypeDefinition, Option<Ident>)> {
    let mut elem_decls = Vec::new();

    loop {
        let token = stream.peek_expect()?;
        if token.kind == End {
            stream.move_after(&token); // End
            stream.pop_if_kind(Record)?;
            let end_ident = stream.pop_optional_ident()?;
            stream.expect_kind(SemiColon)?;
            return Ok((TypeDefinition::Record(elem_decls), end_ident));
        };

        let idents = parse_identifier_list(stream)?;
        stream.expect_kind(Colon)?;
        let subtype = parse_subtype_indication(stream)?;
        for ident in idents {
            elem_decls.push(ElementDeclaration {
                ident,
                subtype: subtype.clone(),
            });
        }
        stream.expect_kind(SemiColon)?;
    }
}

pub fn parse_subtype_declaration(stream: &mut TokenStream) -> ParseResult<TypeDeclaration> {
    stream.expect_kind(Subtype)?;
    let ident = stream.expect_ident()?;
    stream.expect_kind(Is)?;
    let subtype_indication = parse_subtype_indication(stream)?;
    stream.expect_kind(SemiColon)?;
    Ok(TypeDeclaration {
        ident,
        def: TypeDefinition::Subtype(subtype_indication),
    })
}

/// LRM 5.6.2 Protected type declarations
pub fn parse_protected_type_declaration(
    stream: &mut TokenStream,
    messages: &mut MessageHandler,
) -> ParseResult<(ProtectedTypeDeclaration, Option<Ident>)> {
    let mut items = Vec::new();

    loop {
        let token = stream.peek_expect()?;

        try_token_kind!(
            token,
            Impure | Function | Procedure => items.push(ProtectedTypeDeclarativeItem::Subprogram(
                parse_subprogram_declaration(stream, messages)?,
            )),
            End => {
                stream.move_after(&token);
                break;
            }
        );
    }
    stream.expect_kind(Protected)?;
    let end_ident = stream.pop_optional_ident()?;
    Ok((ProtectedTypeDeclaration { items }, end_ident))
}

/// LRM 5.2.4 Physical types
fn parse_physical_type_definition(
    stream: &mut TokenStream,
    range: Range,
) -> ParseResult<(TypeDefinition, Option<Ident>)> {
    let primary_unit = stream.expect_ident()?;
    stream.expect_kind(SemiColon)?;

    let mut secondary_units = Vec::new();

    loop {
        let token = stream.peek_expect()?;
        try_token_kind!(
            token,
            End => {
                break;
            },
            Identifier => {
                stream.move_after(&token);
                let ident = token.expect_ident()?;
                stream.expect_kind(EQ)?;
                let literal = {
                    let value_token = stream.expect()?;
                    try_token_kind!(
                        value_token,
                        AbstractLiteral => {
                            let value = value_token.expect_abstract_literal()?.item;
                            let unit = stream.expect_ident()?;
                            Literal::Physical(value, unit.item)
                        },
                        Identifier => {
                            let unit = value_token.expect_ident()?;
                            Literal::Physical(AbstractLiteral::Integer(1), unit.item)
                        }
                    )
                };

                secondary_units.push((ident, literal));
                stream.expect_kind(SemiColon)?;
            }
        )
    }

    stream.expect_kind(End)?;
    stream.expect_kind(Units)?;
    let end_ident = stream.pop_optional_ident()?;
    stream.expect_kind(SemiColon)?;

    Ok((
        TypeDefinition::Physical(PhysicalTypeDeclaration {
            range,
            primary_unit,
            secondary_units,
        }),
        end_ident,
    ))
}

/// LRM 6.2
pub fn parse_type_declaration(
    stream: &mut TokenStream,
    messages: &mut MessageHandler,
) -> ParseResult<TypeDeclaration> {
    let token = stream.peek_expect()?;
    try_token_kind!(
        token,
        Subtype => {
            return parse_subtype_declaration(stream);
        },
        Type => {
            stream.move_after(&token);
        }
    );

    let ident = stream.expect_ident()?;

    try_token_kind!(
        stream.expect()?,
        Is => {},
        SemiColon => {
            return Ok(TypeDeclaration {
                ident,
                def: TypeDefinition::Incomplete,
            });
        }
    );

    let def = try_token_kind!(
        stream.expect()?,
        // Integer
        Range => {
            let constraint = parse_range(stream)?.item;
            try_token_kind!(
                stream.expect()?,
                SemiColon => TypeDefinition::Integer(constraint),
                Units => {
                    let (def, end_ident) = parse_physical_type_definition(stream, constraint)?;
                    push_some(messages, error_on_end_identifier_mismatch(&ident, &end_ident));
                    def
                }
            )
        },

        Access => {
            let subtype_indication = parse_subtype_indication(stream)?;
            stream.expect_kind(SemiColon)?;
            TypeDefinition::Access(subtype_indication)
        },

        Protected => {
            if stream.skip_if_kind(Body)? {
                let decl = parse_declarative_part(stream, messages, false)?;
                stream.expect_kind(Protected)?;
                stream.expect_kind(Body)?;
                // @TODO check name
                stream.pop_if_kind(Identifier)?;
                stream.expect_kind(SemiColon)?;
                TypeDefinition::ProtectedBody(ProtectedTypeBody {decl})
            } else {
                let (protected_type_decl, end_ident) = parse_protected_type_declaration(stream, messages)?;
                push_some(messages, error_on_end_identifier_mismatch(&ident, &end_ident));
                stream.expect_kind(SemiColon)?;
                TypeDefinition::Protected(protected_type_decl)
            }
        },
        File => {
            stream.expect_kind(Of)?;
            let selected_name = parse_selected_name(stream)?;
            stream.expect_kind(SemiColon)?;
            TypeDefinition::File(selected_name)
        },
        Array => parse_array_type_definition(stream)?,
        Record =>  {
            let (def, end_ident) = parse_record_type_definition(stream)?;
            push_some(messages, error_on_end_identifier_mismatch(&ident, &end_ident));
            def
        },
        // Enumeration
        LeftPar => parse_enumeration_type_definition(stream)?
    );

    Ok(TypeDeclaration { ident, def })
}

#[cfg(test)]
mod tests {
    use super::*;

    use ast::{DiscreteRange, Ident};
    use test_util::Code;

    #[test]
    fn parse_integer_scalar_type_definition() {
        let code = Code::new("type foo is range 0 to 1;");

        let type_decl = TypeDeclaration {
            ident: code.s1("foo").ident(),
            def: TypeDefinition::Integer(code.s1("0 to 1").range()),
        };
        assert_eq!(
            code.with_stream_no_messages(parse_type_declaration),
            type_decl
        );
    }

    #[test]
    fn parse_enumeration_scalar_type_definition() {
        let code = Code::new("type foo is (alpha, beta);");

        let type_decl = TypeDeclaration {
            ident: code.s1("foo").ident(),
            def: TypeDefinition::Enumeration(vec![
                code.s1("alpha")
                    .ident()
                    .map_into(EnumerationLiteral::Identifier),
                code.s1("beta")
                    .ident()
                    .map_into(EnumerationLiteral::Identifier),
            ]),
        };
        assert_eq!(
            code.with_stream_no_messages(parse_type_declaration),
            type_decl
        );
    }

    #[test]
    fn parse_enumeration_scalar_type_definition_character() {
        let code = Code::new("type foo is ('a', 'b');");

        let type_decl = TypeDeclaration {
            ident: code.s1("foo").ident(),
            def: TypeDefinition::Enumeration(vec![
                code.s1("'a'")
                    .character()
                    .map_into(EnumerationLiteral::Character),
                code.s1("'b'")
                    .character()
                    .map_into(EnumerationLiteral::Character),
            ]),
        };
        assert_eq!(
            code.with_stream_no_messages(parse_type_declaration),
            type_decl
        );
    }

    #[test]
    fn mixing_identifier_and_scalar_in_enumerations() {
        let code = Code::new("type foo is (ident, 'b');");

        let type_decl = TypeDeclaration {
            ident: code.s1("foo").ident(),
            def: TypeDefinition::Enumeration(vec![
                code.s1("ident")
                    .ident()
                    .map_into(EnumerationLiteral::Identifier),
                code.s1("'b'")
                    .character()
                    .map_into(EnumerationLiteral::Character),
            ]),
        };
        assert_eq!(
            code.with_stream_no_messages(parse_type_declaration),
            type_decl
        );
    }

    #[test]
    fn parse_array_type_definition_with_index_subtype_definition() {
        let code = Code::new("type foo is array (natural range <>) of boolean;");

        let type_decl = TypeDeclaration {
            ident: code.s1("foo").ident(),
            def: TypeDefinition::Array(
                vec![ArrayIndex::IndexSubtypeDefintion(
                    code.s1("natural").selected_name(),
                )],
                code.s1("boolean").subtype_indication(),
            ),
        };

        assert_eq!(
            code.with_stream_no_messages(parse_type_declaration),
            type_decl
        );
    }

    #[test]
    fn parse_array_type_definition_with_discrete_subtype_definition() {
        let code = Code::new("type foo is array (natural) of boolean;");

        let type_decl = TypeDeclaration {
            ident: code.s1("foo").ident(),
            def: TypeDefinition::Array(
                vec![ArrayIndex::Discrete(DiscreteRange::Discrete(
                    code.s1("natural").selected_name(),
                    None,
                ))],
                code.s1("boolean").subtype_indication(),
            ),
        };

        assert_eq!(
            code.with_stream_no_messages(parse_type_declaration),
            type_decl
        );
    }

    #[test]
    fn parse_array_type_definition_with_selected_name() {
        let code = Code::new("type foo is array (lib.pkg.foo) of boolean;");

        let type_decl = TypeDeclaration {
            ident: code.s1("foo").ident(),
            def: TypeDefinition::Array(
                vec![ArrayIndex::Discrete(DiscreteRange::Discrete(
                    code.s1("lib.pkg.foo").selected_name(),
                    None,
                ))],
                code.s1("boolean").subtype_indication(),
            ),
        };

        assert_eq!(
            code.with_stream_no_messages(parse_type_declaration),
            type_decl
        );
    }

    #[test]
    fn parse_array_type_definition_with_range_attribute_name() {
        let code = Code::new("type foo is array (arr_t'range) of boolean;");

        let type_decl = TypeDeclaration {
            ident: code.s1("foo").ident(),
            def: TypeDefinition::Array(
                vec![ArrayIndex::Discrete(DiscreteRange::Range(
                    code.s1("arr_t'range").range(),
                ))],
                code.s1("boolean").subtype_indication(),
            ),
        };

        assert_eq!(
            code.with_stream_no_messages(parse_type_declaration),
            type_decl
        );
    }

    #[test]
    fn parse_array_type_definition_with_constraint() {
        let code = Code::new("type foo is array (2-1 downto 0) of boolean;");

        let index = ArrayIndex::Discrete(DiscreteRange::Range(code.s1("2-1 downto 0").range()));

        let type_decl = TypeDeclaration {
            ident: code.s1("foo").ident(),
            def: TypeDefinition::Array(vec![index], code.s1("boolean").subtype_indication()),
        };

        assert_eq!(
            code.with_stream_no_messages(parse_type_declaration),
            type_decl
        );
    }

    #[test]
    fn parse_array_type_definition_mixed() {
        let code = Code::new("type foo is array (2-1 downto 0, integer range <>) of boolean;");

        let index0 = ArrayIndex::Discrete(DiscreteRange::Range(code.s1("2-1 downto 0").range()));

        let index1 = ArrayIndex::IndexSubtypeDefintion(code.s1("integer").selected_name());

        let type_decl = TypeDeclaration {
            ident: code.s1("foo").ident(),
            def: TypeDefinition::Array(
                vec![index0, index1],
                code.s1("boolean").subtype_indication(),
            ),
        };

        assert_eq!(
            code.with_stream_no_messages(parse_type_declaration),
            type_decl
        );
    }

    #[test]
    fn parse_record_type_definition() {
        let code = Code::new(
            "\
            type foo is record
  element : boolean;
end record;",
        );

        let elem_decl = ElementDeclaration {
            ident: code.s1("element").ident(),
            subtype: code.s1("boolean").subtype_indication(),
        };

        let type_decl = TypeDeclaration {
            ident: code.s1("foo").ident(),
            def: TypeDefinition::Record(vec![elem_decl]),
        };

        assert_eq!(
            code.with_stream_no_messages(parse_type_declaration),
            type_decl
        );
    }

    #[test]
    fn parse_record_type_definition_many() {
        let code = Code::new(
            "\
            type foo is record
  element, field : boolean;
  other_element : std_logic_vector(0 to 1);
end foo;",
        );

        let elem_decl0a = ElementDeclaration {
            ident: code.s1("element").ident(),
            subtype: code.s1("boolean").subtype_indication(),
        };

        let elem_decl0b = ElementDeclaration {
            ident: code.s1("field").ident(),
            subtype: code.s1("boolean").subtype_indication(),
        };

        let elem_decl1 = ElementDeclaration {
            ident: code.s1("other_element").ident(),
            subtype: code.s1("std_logic_vector(0 to 1)").subtype_indication(),
        };

        let type_decl = TypeDeclaration {
            ident: code.s1("foo").ident(),
            def: TypeDefinition::Record(vec![elem_decl0a, elem_decl0b, elem_decl1]),
        };

        assert_eq!(
            code.with_stream_no_messages(parse_type_declaration),
            type_decl
        );
    }

    #[test]
    fn test_parse_subtype_declaration() {
        let code = Code::new("subtype vec_t is integer_vector(2-1 downto 0);");

        assert_eq!(
            code.with_stream_no_messages(parse_type_declaration),
            TypeDeclaration {
                ident: code.s1("vec_t").ident(),
                def: TypeDefinition::Subtype(
                    code.s1("integer_vector(2-1 downto 0)").subtype_indication()
                )
            }
        );
    }

    #[test]
    fn test_parse_access_type_declaration() {
        let code = Code::new("type ptr_t is access integer_vector(2-1 downto 0);");

        assert_eq!(
            code.with_stream_no_messages(parse_type_declaration),
            TypeDeclaration {
                ident: code.s1("ptr_t").ident(),
                def: TypeDefinition::Access(
                    code.s1("integer_vector(2-1 downto 0)").subtype_indication()
                )
            }
        );
    }

    #[test]
    fn test_incomplete_type_declaration() {
        let code = Code::new("type incomplete;");

        assert_eq!(
            code.with_stream_no_messages(parse_type_declaration),
            TypeDeclaration {
                ident: code.s1("incomplete").ident(),
                def: TypeDefinition::Incomplete
            }
        );
    }

    #[test]
    fn test_file_type_declaration() {
        let code = Code::new("type foo is file of character;");

        assert_eq!(
            code.with_stream_no_messages(parse_type_declaration),
            TypeDeclaration {
                ident: code.s1("foo").ident(),
                def: TypeDefinition::File(code.s1("character").selected_name())
            }
        );
    }

    fn protected_decl(ident: Ident, items: Vec<ProtectedTypeDeclarativeItem>) -> TypeDeclaration {
        TypeDeclaration {
            ident,
            def: TypeDefinition::Protected(ProtectedTypeDeclaration { items }),
        }
    }

    #[test]
    fn test_protected_type_declaration() {
        let code = Code::new(
            "\
type foo is protected
end protected;
",
        );
        assert_eq!(
            code.with_stream_no_messages(parse_type_declaration),
            protected_decl(code.s1("foo").ident(), vec![])
        )
    }

    #[test]
    fn test_protected_type_declaration_simple_name_suffix() {
        let code = Code::new(
            "\
type foo is protected
end protected foo;
",
        );
        assert_eq!(
            code.with_stream_no_messages(parse_type_declaration),
            protected_decl(code.s1("foo").ident(), vec![])
        )
    }

    #[test]
    fn test_protected_type_declaration_with_subprograms() {
        let code = Code::new(
            "\
type foo is protected
  procedure proc;
  function fun return ret;
end protected;
",
        );
        let items = vec![
            ProtectedTypeDeclarativeItem::Subprogram(code.s1("procedure proc").subprogram_decl()),
            ProtectedTypeDeclarativeItem::Subprogram(
                code.s1("function fun return ret").subprogram_decl(),
            ),
        ];

        assert_eq!(
            code.with_stream_no_messages(parse_type_declaration),
            protected_decl(code.s1("foo").ident(), items)
        )
    }

    #[test]
    fn test_protected_type_body() {
        let code = Code::new(
            "\
type foo is protected body
  variable foo : natural;
  procedure proc is
  begin
  end;
end protected body;
",
        );

        let decl = code
            .s1("\
  variable foo : natural;
  procedure proc is
  begin
  end;").declarative_part();
        assert_eq!(
            code.with_stream_no_messages(parse_type_declaration),
            TypeDeclaration {
                ident: code.s1("foo").ident(),
                def: TypeDefinition::ProtectedBody(ProtectedTypeBody { decl }),
            }
        )
    }

    #[test]
    fn test_physical_type_declaration() {
        let code = Code::new(
            "\
type phys is range 0 to 15 units
   primary_unit;
end units phys;
",
        );

        assert_eq!(
            code.with_stream_no_messages(parse_type_declaration),
            TypeDeclaration {
                ident: code.s1("phys").ident(),
                def: TypeDefinition::Physical(PhysicalTypeDeclaration {
                    range: code.s1("0 to 15").range(),
                    primary_unit: code.s1("primary_unit").ident(),
                    secondary_units: vec![]
                })
            }
        )
    }

    #[test]
    fn test_physical_type_declaration_secondary_units() {
        let code = Code::new(
            "\
type phys is range 0 to 15 units
   primary_unit;
   secondary_unit = 5 primary_unit;
end units;
",
        );

        assert_eq!(
            code.with_stream_no_messages(parse_type_declaration),
            TypeDeclaration {
                ident: code.s1("phys").ident(),
                def: TypeDefinition::Physical(PhysicalTypeDeclaration {
                    range: code.s1("0 to 15").range(),
                    primary_unit: code.s1("primary_unit").ident(),
                    secondary_units: vec![(
                        code.s1("secondary_unit").ident(),
                        Literal::Physical(AbstractLiteral::Integer(5), code.symbol("primary_unit"))
                    ),]
                })
            }
        )
    }

    #[test]
    fn test_physical_type_declaration_implicit_secondary_units() {
        let code = Code::new(
            "\
type phys is range 0 to 15 units
   primary_unit;
   secondary_unit = primary_unit;
end units;
",
        );

        assert_eq!(
            code.with_stream_no_messages(parse_type_declaration),
            TypeDeclaration {
                ident: code.s1("phys").ident(),
                def: TypeDefinition::Physical(PhysicalTypeDeclaration {
                    range: code.s1("0 to 15").range(),
                    primary_unit: code.s1("primary_unit").ident(),
                    secondary_units: vec![(
                        code.s1("secondary_unit").ident(),
                        Literal::Physical(AbstractLiteral::Integer(1), code.symbol("primary_unit"))
                    ),]
                })
            }
        )
    }

}
