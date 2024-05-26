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
use super::tokens::{Kind::*, TokenSpan};
use crate::ast::*;
use crate::ast::{AbstractLiteral, Range};
use crate::named_entity::Reference;
use crate::syntax::names::parse_type_mark;
use vhdl_lang::syntax::parser::ParsingContext;

/// LRM 5.2.2 Enumeration types
fn parse_enumeration_type_definition(ctx: &mut ParsingContext<'_>) -> ParseResult<TypeDefinition> {
    let mut enum_literals = Vec::new();
    loop {
        expect_token!(ctx.stream,
            literal_token,
            literal_token_id,
            Identifier | Character => {
                let enum_literal = match literal_token.kind {
                    Identifier => literal_token.to_identifier_value(literal_token_id)?.map_into(EnumerationLiteral::Identifier),
                    Character => literal_token.to_character_value(literal_token_id)?.map_into(EnumerationLiteral::Character),
                    _ => unreachable!()
                };
                enum_literals.push(WithDecl::new(enum_literal));

                expect_token!(ctx.stream, token,
                    RightPar => { break; },
                    Comma => {}
                );
            }
        );
    }

    Ok(TypeDefinition::Enumeration(enum_literals))
}

fn parse_array_index_constraints(ctx: &mut ParsingContext<'_>) -> ParseResult<Vec<ArrayIndex>> {
    ctx.stream.expect_kind(LeftPar)?;
    let mut indexes = Vec::new();
    loop {
        indexes.push(parse_array_index_constraint(ctx)?);

        expect_token!(ctx.stream, token,
            RightPar => {
                return Ok(indexes);
            },
            Comma => {}
        )
    }
}

/// LRM 5.3.2 Array types
fn parse_array_type_definition(ctx: &mut ParsingContext<'_>) -> ParseResult<TypeDefinition> {
    let index_constraints = parse_array_index_constraints(ctx)?;
    ctx.stream.expect_kind(Of)?;
    let element_subtype = parse_subtype_indication(ctx)?;
    Ok(TypeDefinition::Array(index_constraints, element_subtype))
}

/// LRM 5.3.3 Record types
fn parse_record_type_definition(
    ctx: &mut ParsingContext<'_>,
) -> ParseResult<(TypeDefinition, Option<Ident>)> {
    let mut elem_decls = Vec::new();

    loop {
        if ctx.stream.skip_if_kind(End) {
            ctx.stream.pop_if_kind(Record);
            let end_ident = ctx.stream.pop_optional_ident();
            return Ok((TypeDefinition::Record(elem_decls), end_ident));
        };

        let start_token = ctx.stream.get_current_token_id();

        let idents = parse_identifier_list(ctx)?;
        ctx.stream.expect_kind(Colon)?;
        let subtype = parse_subtype_indication(ctx)?;
        let end_token = ctx.stream.expect_kind(SemiColon)?;
        for ident in idents {
            elem_decls.push(ElementDeclaration {
                ident: ident.into(),
                subtype: subtype.clone(),
                span: TokenSpan::new(start_token, end_token),
            });
        }
    }
}

pub fn parse_subtype_declaration(ctx: &mut ParsingContext<'_>) -> ParseResult<TypeDeclaration> {
    let start_token = ctx.stream.expect_kind(Subtype)?;
    let ident = ctx.stream.expect_ident()?;
    ctx.stream.expect_kind(Is)?;
    let subtype_indication = parse_subtype_indication(ctx)?;
    let end_token = ctx.stream.expect_kind(SemiColon)?;
    Ok(TypeDeclaration {
        span: TokenSpan::new(start_token, end_token),
        ident: ident.into(),
        def: TypeDefinition::Subtype(subtype_indication),
        end_ident_pos: None,
    })
}

/// LRM 5.6.2 Protected type declarations
pub fn parse_protected_type_declaration(
    ctx: &mut ParsingContext<'_>,
) -> ParseResult<(ProtectedTypeDeclaration, Option<Ident>)> {
    let mut items = Vec::new();

    loop {
        let token = ctx.stream.peek_expect()?;

        try_init_token_kind!(
            token,
            Impure | Function | Procedure => items.push(ProtectedTypeDeclarativeItem::Subprogram(
                parse_subprogram_declaration(ctx)?,
            )),
            End => {
                ctx.stream.skip();
                break;
            }
        );
    }
    ctx.stream.expect_kind(Protected)?;
    let end_ident = ctx.stream.pop_optional_ident();
    Ok((ProtectedTypeDeclaration { items }, end_ident))
}

/// LRM 5.2.4 Physical types
fn parse_physical_type_definition(
    ctx: &mut ParsingContext<'_>,
    range: Range,
) -> ParseResult<(TypeDefinition, Option<Ident>)> {
    let primary_unit = WithDecl::new(ctx.stream.expect_ident()?);
    ctx.stream.expect_kind(SemiColon)?;

    let mut secondary_units = Vec::new();

    loop {
        peek_token!(
            ctx.stream, token, token_id,
            End => {
                break;
            },
            Identifier => {
                ctx.stream.skip();
                let ident = WithDecl::new(token.to_identifier_value(token_id)?);
                ctx.stream.expect_kind(EQ)?;
                let literal = {
                    expect_token!(ctx.stream,
                        value_token,
                        value_token_id,
                        AbstractLiteral => {
                            let value = value_token.to_abstract_literal(value_token_id)?.item;
                            let unit = ctx.stream.expect_ident()?;
                            PhysicalLiteral {value, unit: unit.into_ref()}
                        },
                        Identifier => {
                            let unit = value_token.to_identifier_value(value_token_id)?;
                            PhysicalLiteral {value: AbstractLiteral::Integer(1), unit: unit.into_ref()}
                        }
                    )
                };

                secondary_units.push((ident, literal));
                ctx.stream.expect_kind(SemiColon)?;
            }
        )
    }

    ctx.stream.expect_kind(End)?;
    ctx.stream.expect_kind(Units)?;
    let end_ident = ctx.stream.pop_optional_ident();

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
pub fn parse_type_declaration(ctx: &mut ParsingContext<'_>) -> ParseResult<TypeDeclaration> {
    let start_token = ctx.stream.get_current_token_id();
    peek_token!(
        ctx.stream, token,
        Subtype => {
            return parse_subtype_declaration(ctx);
        },
        Type => {
            ctx.stream.skip();
        }
    );

    let ident = WithDecl::new(ctx.stream.expect_ident()?);
    let mut end_ident_pos = None;

    expect_token!(
        ctx.stream, token,
        Is => {},
        SemiColon => {
            return Ok(TypeDeclaration {
                span: TokenSpan::new(start_token, ctx.stream.get_last_token_id()),
                ident,
                def: TypeDefinition::Incomplete(Reference::undefined()),
                end_ident_pos
            });
        }
    );

    let def = expect_token!(
        ctx.stream, token,
        // Integer
        Range => {
            let constraint = parse_range(ctx)?.item;
            expect_token!(
                ctx.stream, token,
                SemiColon => {
                    ctx.stream.back(); // The ';' is consumed at the end of the function
                    TypeDefinition::Numeric(constraint)
                },
                Units => {
                    let (def, end_ident) = parse_physical_type_definition(ctx, constraint)?;
                    end_ident_pos = check_end_identifier_mismatch(ctx, &ident.tree, end_ident);
                    def
                }
            )
        },

        Access => {
            let subtype_indication = parse_subtype_indication(ctx)?;
            TypeDefinition::Access(subtype_indication)
        },

        Protected => {
            if ctx.stream.skip_if_kind(Body) {
                let decl = parse_declarative_part(ctx)?;
                ctx.stream.expect_kind(End)?;
                ctx.stream.expect_kind(Protected)?;
                ctx.stream.expect_kind(Body)?;
                let end_ident = ctx.stream.pop_optional_ident();
                end_ident_pos = check_end_identifier_mismatch(ctx, &ident.tree, end_ident);

                TypeDefinition::ProtectedBody(ProtectedTypeBody {decl})
            } else {
                let (protected_type_decl, end_ident) = parse_protected_type_declaration(ctx)?;
                end_ident_pos = check_end_identifier_mismatch(ctx, &ident.tree, end_ident);
                TypeDefinition::Protected(protected_type_decl)
            }
        },
        File => {
            ctx.stream.expect_kind(Of)?;
            let type_mark = parse_type_mark(ctx)?;
            TypeDefinition::File(type_mark)
        },
        Array => parse_array_type_definition(ctx)?,
        Record =>  {
            let (def, end_ident) = parse_record_type_definition(ctx)?;
            end_ident_pos = check_end_identifier_mismatch(ctx, &ident.tree, end_ident);
            def
        },
        // Enumeration
        LeftPar => parse_enumeration_type_definition(ctx)?
    );

    let end_token = ctx.stream.expect_kind(SemiColon)?;
    Ok(TypeDeclaration {
        span: TokenSpan::new(start_token, end_token),
        ident,
        def,
        end_ident_pos,
    })
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use super::*;

    use crate::{HasTokenSpan, TokenId};

    use crate::ast::{DiscreteRange, Ident};
    use crate::syntax::test::{token_to_string, Code};

    #[test]
    fn parse_integer_scalar_type_definition() {
        let code = Code::new("type foo is range 0 to 1;");

        let type_decl = TypeDeclaration {
            span: code.token_span(),
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
            span: code.token_span(),
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
            span: code.token_span(),
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
            span: code.token_span(),
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
            span: code.token_span(),
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
            span: code.token_span(),
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
            span: code.token_span(),
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
            span: code.token_span(),
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
            span: code.token_span(),
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
            span: code.token_span(),
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
            span: code.s1("element : boolean;").token_span(),
        };

        let type_decl = TypeDeclaration {
            span: code.token_span(),
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
            span: code.s1("element, field : boolean;").token_span(),
        };

        let elem_decl0b = ElementDeclaration {
            ident: code.s1("field").decl_ident(),
            subtype: code.s1("boolean").subtype_indication(),
            span: code.s1("element, field : boolean;").token_span(),
        };

        let elem_decl1 = ElementDeclaration {
            ident: code.s1("other_element").decl_ident(),
            subtype: code.s1("std_logic_vector(0 to 1)").subtype_indication(),
            span: code
                .s1("other_element : std_logic_vector(0 to 1);")
                .token_span(),
        };

        let type_decl = TypeDeclaration {
            span: code.token_span(),
            ident: code.s1("foo").decl_ident(),
            def: TypeDefinition::Record(vec![elem_decl0a, elem_decl0b, elem_decl1]),
            end_ident_pos: Some(code.s("foo", 2).token()),
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
                span: code.token_span(),
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
                span: code.token_span(),
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
                span: code.token_span(),
                ident: code.s1("incomplete").decl_ident(),
                def: TypeDefinition::Incomplete(Reference::undefined()),
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
                span: code.token_span(),
                ident: code.s1("foo").decl_ident(),
                def: TypeDefinition::File(code.s1("character").type_mark()),
                end_ident_pos: None,
            }
        );
    }

    fn protected_decl(
        ident: Ident,
        token_span: TokenSpan,
        items: Vec<ProtectedTypeDeclarativeItem>,
        end_ident_pos: Option<TokenId>,
    ) -> TypeDeclaration {
        TypeDeclaration {
            span: token_span,
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
            protected_decl(code.s1("foo").ident(), code.token_span(), vec![], None)
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
            protected_decl(
                code.s1("foo").ident(),
                code.token_span(),
                vec![],
                Some(code.s("foo", 2).token())
            )
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
            ProtectedTypeDeclarativeItem::Subprogram(code.s1("procedure proc;").subprogram_decl()),
            ProtectedTypeDeclarativeItem::Subprogram(
                code.s1("function fun return ret;").subprogram_decl(),
            ),
        ];

        assert_eq!(
            code.with_stream_no_diagnostics(parse_type_declaration),
            protected_decl(code.s1("foo").ident(), code.token_span(), items, None)
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
                span: code.token_span(),
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
                span: code.token_span(),
                ident: code.s1("phys").decl_ident(),
                def: TypeDefinition::Physical(PhysicalTypeDeclaration {
                    range: code.s1("0 to 15").range(),
                    primary_unit: code.s1("primary_unit").decl_ident(),
                    secondary_units: vec![]
                }),
                end_ident_pos: Some(code.s("phys", 2).token()),
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
                span: code.token_span(),
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
                span: code.token_span(),
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

    #[test]
    pub fn test_token_span() {
        let code = Code::new(
            "\
package pkg is
    subtype negative is integer range -2**31 to -1;
    type incomplete;
    type new_integer is range 100 downto -100;
    type line is access string;
    type prot is
        protected
            procedure proc;
        end protected;
    type IntegerFile is file of INTEGER;
    type arr is array (natural range <>) of positive;
    type dummy_rec is
        record
            dummy: bit;
        end record;
    type enum is (V1, V2, V3);
end package;
",
        );
        let ctx = code.tokenize();
        let pkg = code.package_declaration();
        let type_decls = pkg
            .decl
            .iter()
            .map(|d| {
                if let Declaration::Type(obj) = &d.item {
                    obj
                } else {
                    panic!("Only object declarations are expected!")
                }
            })
            .collect_vec();

        let type_decl_strings: Vec<Vec<String>> = type_decls
            .iter()
            .map(|decl| {
                decl.get_token_slice(&ctx)
                    .iter()
                    .map(token_to_string)
                    .collect()
            })
            .collect_vec();

        assert_eq!(
            type_decl_strings[0],
            vec![
                "subtype", "negative", "is", "integer", "range", "-", "2", "**", "31", "to", "-",
                "1", ";"
            ],
        );
        assert_eq!(type_decl_strings[1], vec!["type", "incomplete", ";"],);
        assert_eq!(
            type_decl_strings[2],
            vec![
                "type",
                "new_integer",
                "is",
                "range",
                "100",
                "downto",
                "-",
                "100",
                ";"
            ],
        );
        assert_eq!(
            type_decl_strings[3],
            vec!["type", "line", "is", "access", "string", ";"],
        );
        assert_eq!(
            type_decl_strings[4],
            vec![
                "type",
                "prot",
                "is",
                "protected",
                "procedure",
                "proc",
                ";",
                "end",
                "protected",
                ";"
            ],
        );
        assert_eq!(
            type_decl_strings[5],
            vec!["type", "IntegerFile", "is", "file", "of", "INTEGER", ";"],
        );
        assert_eq!(
            type_decl_strings[6],
            vec![
                "type", "arr", "is", "array", "(", "natural", "range", "<>", ")", "of", "positive",
                ";",
            ],
        );
        assert_eq!(
            type_decl_strings[7],
            vec![
                "type",
                "dummy_rec",
                "is",
                "record",
                "dummy",
                ":",
                "bit",
                ";",
                "end",
                "record",
                ";",
            ],
        );
    }
}
