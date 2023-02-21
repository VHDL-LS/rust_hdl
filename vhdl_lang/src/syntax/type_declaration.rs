// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::common::check_end_identifier_mismatch;
use super::common::ParseResult;
use super::declarative_part::parse_declarative_part;
use super::names::parse_identifier_list;
use super::range::{parse_array_index_constraint, parse_range};
use super::subprogram::parse_subprogram_declaration;
use super::subtype_indication::parse_subtype_indication;
use super::tokens::{Kind::*, TokenStream};
use crate::ast::*;
use crate::ast::{AbstractLiteral, Range};
use crate::data::DiagnosticHandler;
use crate::syntax::names::parse_type_mark;

/// LRM 5.2.2 Enumeration types
fn parse_enumeration_type_definition(stream: &TokenStream) -> ParseResult<TypeDefinition> {
    let mut enum_literals = Vec::new();
    loop {
        expect_token!(stream,
            literal_token,
            Identifier | Character => {
                let enum_literal = match literal_token.kind {
                    Identifier => literal_token.to_identifier_value()?.map_into(EnumerationLiteral::Identifier),
                    Character => literal_token.to_character_value()?.map_into(EnumerationLiteral::Character),
                    _ => unreachable!()
                };
                enum_literals.push(WithDecl::new(enum_literal));

                expect_token!(stream, token,
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

fn parse_array_index_constraints(stream: &TokenStream) -> ParseResult<Vec<ArrayIndex>> {
    stream.expect_kind(LeftPar)?;
    let mut indexes = Vec::new();
    loop {
        indexes.push(parse_array_index_constraint(stream)?);

        expect_token!(stream, token,
            RightPar => {
                return Ok(indexes);
            },
            Comma => {}
        )
    }
}

/// LRM 5.3.2 Array types
fn parse_array_type_definition(stream: &TokenStream) -> ParseResult<TypeDefinition> {
    let index_constraints = parse_array_index_constraints(stream)?;
    stream.expect_kind(Of)?;
    let element_subtype = parse_subtype_indication(stream)?;
    stream.expect_kind(SemiColon)?;
    Ok(TypeDefinition::Array(index_constraints, element_subtype))
}

/// LRM 5.3.3 Record types
fn parse_record_type_definition(
    stream: &TokenStream,
) -> ParseResult<(TypeDefinition, Option<Ident>)> {
    let mut elem_decls = Vec::new();

    loop {
        if stream.skip_if_kind(End) {
            stream.pop_if_kind(Record);
            let end_ident = stream.pop_optional_ident();
            stream.expect_kind(SemiColon)?;
            return Ok((TypeDefinition::Record(elem_decls), end_ident));
        };

        let idents = parse_identifier_list(stream)?;
        stream.expect_kind(Colon)?;
        let subtype = parse_subtype_indication(stream)?;
        for ident in idents {
            elem_decls.push(ElementDeclaration {
                ident: ident.into(),
                subtype: subtype.clone(),
            });
        }
        stream.expect_kind(SemiColon)?;
    }
}

pub fn parse_subtype_declaration(stream: &TokenStream) -> ParseResult<TypeDeclaration> {
    stream.expect_kind(Subtype)?;
    let ident = stream.expect_ident()?;
    stream.expect_kind(Is)?;
    let subtype_indication = parse_subtype_indication(stream)?;
    stream.expect_kind(SemiColon)?;
    Ok(TypeDeclaration {
        ident: ident.into(),
        def: TypeDefinition::Subtype(subtype_indication),
        end_ident_pos: None,
    })
}

/// LRM 5.6.2 Protected type declarations
pub fn parse_protected_type_declaration(
    stream: &TokenStream,
    diagnostics: &mut dyn DiagnosticHandler,
) -> ParseResult<(ProtectedTypeDeclaration, Option<Ident>)> {
    let mut items = Vec::new();

    loop {
        let token = stream.peek_expect()?;

        try_init_token_kind!(
            token,
            Impure | Function | Procedure => items.push(ProtectedTypeDeclarativeItem::Subprogram(
                parse_subprogram_declaration(stream, diagnostics)?,
            )),
            End => {
                stream.skip();
                break;
            }
        );
    }
    stream.expect_kind(Protected)?;
    let end_ident = stream.pop_optional_ident();
    Ok((ProtectedTypeDeclaration { items }, end_ident))
}

/// LRM 5.2.4 Physical types
fn parse_physical_type_definition(
    stream: &TokenStream,
    range: Range,
) -> ParseResult<(TypeDefinition, Option<Ident>)> {
    let primary_unit = WithDecl::new(stream.expect_ident()?);
    stream.expect_kind(SemiColon)?;

    let mut secondary_units = Vec::new();

    loop {
        peek_token!(
            stream, token,
            End => {
                break;
            },
            Identifier => {
                stream.skip();
                let ident = WithDecl::new(token.to_identifier_value()?);
                stream.expect_kind(EQ)?;
                let literal = {
                    expect_token!(stream,
                        value_token,
                        AbstractLiteral => {
                            let value = value_token.to_abstract_literal()?.item;
                            let unit = stream.expect_ident()?;
                            PhysicalLiteral {value, unit: unit.into_ref()}
                        },
                        Identifier => {
                            let unit = value_token.to_identifier_value()?;
                            PhysicalLiteral {value: AbstractLiteral::Integer(1), unit: unit.into_ref()}
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
    let end_ident = stream.pop_optional_ident();
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
    stream: &TokenStream,
    diagnostics: &mut dyn DiagnosticHandler,
) -> ParseResult<TypeDeclaration> {
    peek_token!(
        stream, token,
        Subtype => {
            return parse_subtype_declaration(stream);
        },
        Type => {
            stream.skip();
        }
    );

    let ident = WithDecl::new(stream.expect_ident()?);
    let mut end_ident_pos = None;

    expect_token!(
        stream, token,
        Is => {},
        SemiColon => {
            return Ok(TypeDeclaration {
                ident,
                def: TypeDefinition::Incomplete(Reference::default()),
                end_ident_pos
            });
        }
    );

    let def = expect_token!(
        stream, token,
        // Integer
        Range => {
            let constraint = parse_range(stream)?.item;
            expect_token!(
                stream, token,
                SemiColon => TypeDefinition::Numeric(constraint),
                Units => {
                    let (def, end_ident) = parse_physical_type_definition(stream, constraint)?;
                    end_ident_pos = check_end_identifier_mismatch(&ident.tree, end_ident, diagnostics);
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
            if stream.skip_if_kind(Body) {
                let decl = parse_declarative_part(stream, diagnostics)?;
                stream.expect_kind(End)?;
                stream.expect_kind(Protected)?;
                stream.expect_kind(Body)?;
                let end_ident = stream.pop_optional_ident();
                stream.expect_kind(SemiColon)?;
                end_ident_pos = check_end_identifier_mismatch(&ident.tree, end_ident, diagnostics);

                TypeDefinition::ProtectedBody(ProtectedTypeBody {decl})
            } else {
                let (protected_type_decl, end_ident) = parse_protected_type_declaration(stream, diagnostics)?;
                end_ident_pos = check_end_identifier_mismatch(&ident.tree, end_ident, diagnostics);
                stream.expect_kind(SemiColon)?;
                TypeDefinition::Protected(protected_type_decl)
            }
        },
        File => {
            stream.expect_kind(Of)?;
            let type_mark = parse_type_mark(stream)?;
            stream.expect_kind(SemiColon)?;
            TypeDefinition::File(type_mark)
        },
        Array => parse_array_type_definition(stream)?,
        Record =>  {
            let (def, end_ident) = parse_record_type_definition(stream)?;
            end_ident_pos = check_end_identifier_mismatch(&ident.tree, end_ident, diagnostics);
            def
        },
        // Enumeration
        LeftPar => parse_enumeration_type_definition(stream)?
    );

    Ok(TypeDeclaration {
        ident,
        def,
        end_ident_pos,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::ast::{DiscreteRange, Ident};
    use crate::syntax::test::Code;
    use crate::SrcPos;

    #[test]
    fn parse_integer_scalar_type_definition() {
        let code = Code::new("type foo is range 0 to 1;");

        let type_decl = TypeDeclaration {
            ident: code.s1("foo").decl_ident(),
            def: TypeDefinition::Numeric(code.s1("0 to 1").range()),
            end_ident_pos: None,
        };
        assert_eq!(
            code.with_stream_no_diagnostics(parse_type_declaration),
            type_decl
        );
    }

    #[test]
    fn parse_enumeration_scalar_type_definition() {
        let code = Code::new("type foo is (alpha, beta);");

        let type_decl = TypeDeclaration {
            ident: code.s1("foo").decl_ident(),
            def: TypeDefinition::Enumeration(vec![
                code.s1("alpha")
                    .ident()
                    .map_into(EnumerationLiteral::Identifier)
                    .into(),
                code.s1("beta")
                    .ident()
                    .map_into(EnumerationLiteral::Identifier)
                    .into(),
            ]),
            end_ident_pos: None,
        };
        assert_eq!(
            code.with_stream_no_diagnostics(parse_type_declaration),
            type_decl
        );
    }

    #[test]
    fn parse_enumeration_scalar_type_definition_character() {
        let code = Code::new("type foo is ('a', 'b');");

        let type_decl = TypeDeclaration {
            ident: code.s1("foo").decl_ident(),
            def: TypeDefinition::Enumeration(vec![
                code.s1("'a'")
                    .character()
                    .map_into(EnumerationLiteral::Character)
                    .into(),
                code.s1("'b'")
                    .character()
                    .map_into(EnumerationLiteral::Character)
                    .into(),
            ]),
            end_ident_pos: None,
        };
        assert_eq!(
            code.with_stream_no_diagnostics(parse_type_declaration),
            type_decl
        );
    }

    #[test]
    fn mixing_identifier_and_scalar_in_enumerations() {
        let code = Code::new("type foo is (ident, 'b');");

        let type_decl = TypeDeclaration {
            ident: code.s1("foo").decl_ident(),
            def: TypeDefinition::Enumeration(vec![
                code.s1("ident")
                    .ident()
                    .map_into(EnumerationLiteral::Identifier)
                    .into(),
                code.s1("'b'")
                    .character()
                    .map_into(EnumerationLiteral::Character)
                    .into(),
            ]),
            end_ident_pos: None,
        };
        assert_eq!(
            code.with_stream_no_diagnostics(parse_type_declaration),
            type_decl
        );
    }

    #[test]
    fn parse_array_type_definition_with_index_subtype_definition() {
        let code = Code::new("type foo is array (natural range <>) of boolean;");

        let type_decl = TypeDeclaration {
            ident: code.s1("foo").decl_ident(),
            def: TypeDefinition::Array(
                vec![ArrayIndex::IndexSubtypeDefintion(
                    code.s1("natural").type_mark(),
                )],
                code.s1("boolean").subtype_indication(),
            ),
            end_ident_pos: None,
        };

        assert_eq!(
            code.with_stream_no_diagnostics(parse_type_declaration),
            type_decl
        );
    }

    #[test]
    fn parse_array_type_definition_with_discrete_subtype_definition() {
        let code = Code::new("type foo is array (natural) of boolean;");

        let type_decl = TypeDeclaration {
            ident: code.s1("foo").decl_ident(),
            def: TypeDefinition::Array(
                vec![ArrayIndex::Discrete(DiscreteRange::Discrete(
                    code.s1("natural").type_mark(),
                    None,
                ))],
                code.s1("boolean").subtype_indication(),
            ),
            end_ident_pos: None,
        };

        assert_eq!(
            code.with_stream_no_diagnostics(parse_type_declaration),
            type_decl
        );
    }

    #[test]
    fn parse_array_type_definition_with_selected_name() {
        let code = Code::new("type foo is array (lib.pkg.foo) of boolean;");

        let type_decl = TypeDeclaration {
            ident: code.s1("foo").decl_ident(),
            def: TypeDefinition::Array(
                vec![ArrayIndex::Discrete(DiscreteRange::Discrete(
                    code.s1("lib.pkg.foo").type_mark(),
                    None,
                ))],
                code.s1("boolean").subtype_indication(),
            ),
            end_ident_pos: None,
        };

        assert_eq!(
            code.with_stream_no_diagnostics(parse_type_declaration),
            type_decl
        );
    }

    #[test]
    fn parse_array_type_definition_with_range_attribute_name() {
        let code = Code::new("type foo is array (arr_t'range) of boolean;");

        let type_decl = TypeDeclaration {
            ident: code.s1("foo").decl_ident(),
            def: TypeDefinition::Array(
                vec![ArrayIndex::Discrete(DiscreteRange::Range(
                    code.s1("arr_t'range").range(),
                ))],
                code.s1("boolean").subtype_indication(),
            ),
            end_ident_pos: None,
        };

        assert_eq!(
            code.with_stream_no_diagnostics(parse_type_declaration),
            type_decl
        );
    }

    #[test]
    fn parse_array_type_definition_with_constraint() {
        let code = Code::new("type foo is array (2-1 downto 0) of boolean;");

        let index = ArrayIndex::Discrete(DiscreteRange::Range(code.s1("2-1 downto 0").range()));

        let type_decl = TypeDeclaration {
            ident: code.s1("foo").decl_ident(),
            def: TypeDefinition::Array(vec![index], code.s1("boolean").subtype_indication()),
            end_ident_pos: None,
        };

        assert_eq!(
            code.with_stream_no_diagnostics(parse_type_declaration),
            type_decl
        );
    }

    #[test]
    fn parse_array_type_definition_mixed() {
        let code = Code::new("type foo is array (2-1 downto 0, integer range <>) of boolean;");

        let index0 = ArrayIndex::Discrete(DiscreteRange::Range(code.s1("2-1 downto 0").range()));

        let index1 = ArrayIndex::IndexSubtypeDefintion(code.s1("integer").type_mark());

        let type_decl = TypeDeclaration {
            ident: code.s1("foo").decl_ident(),
            def: TypeDefinition::Array(
                vec![index0, index1],
                code.s1("boolean").subtype_indication(),
            ),
            end_ident_pos: None,
        };

        assert_eq!(
            code.with_stream_no_diagnostics(parse_type_declaration),
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
            ident: code.s1("element").decl_ident(),
            subtype: code.s1("boolean").subtype_indication(),
        };

        let type_decl = TypeDeclaration {
            ident: code.s1("foo").decl_ident(),
            def: TypeDefinition::Record(vec![elem_decl]),
            end_ident_pos: None,
        };

        assert_eq!(
            code.with_stream_no_diagnostics(parse_type_declaration),
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
            ident: code.s1("element").decl_ident(),
            subtype: code.s1("boolean").subtype_indication(),
        };

        let elem_decl0b = ElementDeclaration {
            ident: code.s1("field").decl_ident(),
            subtype: code.s1("boolean").subtype_indication(),
        };

        let elem_decl1 = ElementDeclaration {
            ident: code.s1("other_element").decl_ident(),
            subtype: code.s1("std_logic_vector(0 to 1)").subtype_indication(),
        };

        let type_decl = TypeDeclaration {
            ident: code.s1("foo").decl_ident(),
            def: TypeDefinition::Record(vec![elem_decl0a, elem_decl0b, elem_decl1]),
            end_ident_pos: Some(code.s("foo", 2).pos()),
        };

        assert_eq!(
            code.with_stream_no_diagnostics(parse_type_declaration),
            type_decl
        );
    }

    #[test]
    fn test_parse_subtype_declaration() {
        let code = Code::new("subtype vec_t is integer_vector(2-1 downto 0);");

        assert_eq!(
            code.with_stream_no_diagnostics(parse_type_declaration),
            TypeDeclaration {
                ident: code.s1("vec_t").decl_ident(),
                def: TypeDefinition::Subtype(
                    code.s1("integer_vector(2-1 downto 0)").subtype_indication()
                ),
                end_ident_pos: None,
            }
        );
    }

    #[test]
    fn test_parse_access_type_declaration() {
        let code = Code::new("type ptr_t is access integer_vector(2-1 downto 0);");

        assert_eq!(
            code.with_stream_no_diagnostics(parse_type_declaration),
            TypeDeclaration {
                ident: code.s1("ptr_t").decl_ident(),
                def: TypeDefinition::Access(
                    code.s1("integer_vector(2-1 downto 0)").subtype_indication()
                ),
                end_ident_pos: None,
            }
        );
    }

    #[test]
    fn test_incomplete_type_declaration() {
        let code = Code::new("type incomplete;");

        assert_eq!(
            code.with_stream_no_diagnostics(parse_type_declaration),
            TypeDeclaration {
                ident: code.s1("incomplete").decl_ident(),
                def: TypeDefinition::Incomplete(Reference::default()),
                end_ident_pos: None,
            }
        );
    }

    #[test]
    fn test_file_type_declaration() {
        let code = Code::new("type foo is file of character;");

        assert_eq!(
            code.with_stream_no_diagnostics(parse_type_declaration),
            TypeDeclaration {
                ident: code.s1("foo").decl_ident(),
                def: TypeDefinition::File(code.s1("character").type_mark()),
                end_ident_pos: None,
            }
        );
    }

    fn protected_decl(
        ident: Ident,
        items: Vec<ProtectedTypeDeclarativeItem>,
        end_ident_pos: Option<SrcPos>,
    ) -> TypeDeclaration {
        TypeDeclaration {
            ident: ident.into(),
            def: TypeDefinition::Protected(ProtectedTypeDeclaration { items }),
            end_ident_pos,
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
            code.with_stream_no_diagnostics(parse_type_declaration),
            protected_decl(code.s1("foo").ident(), vec![], None)
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
            code.with_stream_no_diagnostics(parse_type_declaration),
            protected_decl(code.s1("foo").ident(), vec![], Some(code.s("foo", 2).pos()))
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
            code.with_stream_no_diagnostics(parse_type_declaration),
            protected_decl(code.s1("foo").ident(), items, None)
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
  end;")
            .declarative_part();

        let ident = code.s1("foo").decl_ident();
        assert_eq!(
            code.with_stream_no_diagnostics(parse_type_declaration),
            TypeDeclaration {
                ident,
                def: TypeDefinition::ProtectedBody(ProtectedTypeBody { decl }),
                end_ident_pos: None,
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
            code.with_stream_no_diagnostics(parse_type_declaration),
            TypeDeclaration {
                ident: code.s1("phys").decl_ident(),
                def: TypeDefinition::Physical(PhysicalTypeDeclaration {
                    range: code.s1("0 to 15").range(),
                    primary_unit: code.s1("primary_unit").decl_ident(),
                    secondary_units: vec![]
                }),
                end_ident_pos: Some(code.s("phys", 2).pos()),
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
            code.with_stream_no_diagnostics(parse_type_declaration),
            TypeDeclaration {
                ident: code.s1("phys").decl_ident(),
                def: TypeDefinition::Physical(PhysicalTypeDeclaration {
                    range: code.s1("0 to 15").range(),
                    primary_unit: code.s1("primary_unit").decl_ident(),
                    secondary_units: vec![(
                        code.s1("secondary_unit").decl_ident(),
                        PhysicalLiteral {
                            value: AbstractLiteral::Integer(5),
                            unit: code.s("primary_unit", 2).ident().into_ref()
                        }
                    ),]
                }),
                end_ident_pos: None,
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
            code.with_stream_no_diagnostics(parse_type_declaration),
            TypeDeclaration {
                ident: code.s1("phys").decl_ident(),
                def: TypeDefinition::Physical(PhysicalTypeDeclaration {
                    range: code.s1("0 to 15").range(),
                    primary_unit: code.s1("primary_unit").decl_ident(),
                    secondary_units: vec![(
                        code.s1("secondary_unit").decl_ident(),
                        PhysicalLiteral {
                            value: AbstractLiteral::Integer(1),
                            unit: code.s("primary_unit", 2).ident().into_ref()
                        }
                    ),]
                }),
                end_ident_pos: None,
            }
        )
    }
}
