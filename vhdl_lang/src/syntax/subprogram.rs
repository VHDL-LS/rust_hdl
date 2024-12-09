// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::common::{check_end_identifier_mismatch, ParseResult};
use super::declarative_part::parse_declarative_part;
use super::interface_declaration::parse_parameter_interface_list;
use super::names::parse_type_mark;
use super::sequential_statement::parse_labeled_sequential_statements;
use super::tokens::{kinds_error, Kind::*, TokenId, TokenSpan};
use crate::ast::token_range::{WithToken, WithTokenSpan};
use crate::ast::*;
use crate::data::*;
use crate::syntax::concurrent_statement::parse_map_aspect;
use crate::syntax::interface_declaration::parse_generic_interface_list;
use crate::syntax::names::parse_name;
use crate::syntax::recover::expect_semicolon_or_last;
use vhdl_lang::syntax::parser::ParsingContext;

pub fn parse_signature(ctx: &mut ParsingContext<'_>) -> ParseResult<WithTokenSpan<Signature>> {
    let left_square = ctx.stream.expect_kind(LeftSquare)?;
    let mut type_marks = Vec::new();
    let mut return_mark = None;

    let pos = peek_token!(
        ctx.stream, token, token_id,
        Return => {
            ctx.stream.skip();
            return_mark = Some(parse_type_mark(ctx)?);
            let right_square = ctx.stream.expect_kind(RightSquare)?;
            TokenSpan::new(left_square, right_square)
        },
        RightSquare => {
            ctx.stream.skip();
            TokenSpan::new(left_square, token_id)
        },
        Identifier => {
            loop {
                let token = ctx.stream.peek_expect()?;

                match token.kind {
                    Identifier => {
                        type_marks.push(parse_type_mark(ctx)?);
                        expect_token!(
                            ctx.stream,
                            sep_token,
                            sep_token_id,
                            Comma => {},
                            RightSquare => {
                                break TokenSpan::new(left_square, sep_token_id);
                            },
                            Return => {
                                return_mark = Some(parse_type_mark(ctx)?);
                                let right_square = ctx.stream.expect_kind(RightSquare)?;
                                break TokenSpan::new(left_square, right_square);
                            }
                        )
                    }
                    _ => {
                        ctx.stream.skip();
                        return Err(kinds_error(ctx.stream.pos_before(token), &[Identifier]))
                    }
                };
            }
        }
    );

    let signature = match return_mark {
        Some(return_mark) => Signature::Function(type_marks, return_mark),
        None => Signature::Procedure(type_marks),
    };

    Ok(WithTokenSpan::new(signature, pos))
}

fn parse_designator(ctx: &mut ParsingContext<'_>) -> ParseResult<WithToken<SubprogramDesignator>> {
    Ok(expect_token!(
        ctx.stream,
        token,
        token_id,
        Identifier => token.to_identifier_value(token_id)?.map_into(SubprogramDesignator::Identifier),
        StringLiteral => token.to_operator_symbol(token_id)?.map_into(SubprogramDesignator::OperatorSymbol)
    ))
}

/// Parses a subprogram header according of the form
/// subprogram_header ::=
///     [ generic ( generic_list )
///     [ generic_map_aspect ] ]
pub fn parse_optional_subprogram_header(
    ctx: &mut ParsingContext<'_>,
) -> ParseResult<Option<SubprogramHeader>> {
    let Some(generic) = ctx.stream.pop_if_kind(Generic) else {
        return Ok(None);
    };
    let mut generic_list = parse_generic_interface_list(ctx)?;
    generic_list.span.start_token = generic;
    let map_aspect = parse_map_aspect(ctx, Generic)?;

    Ok(Some(SubprogramHeader {
        generic_list,
        map_aspect,
    }))
}

pub fn parse_subprogram_instantiation(
    ctx: &mut ParsingContext<'_>,
) -> ParseResult<SubprogramInstantiation> {
    let start_token = ctx.stream.get_current_token_id();
    let tok = ctx.stream.peek_expect()?;
    let kind = match tok.kind {
        Procedure => SubprogramKind::Procedure,
        Function => SubprogramKind::Function,
        _ => {
            return Err(Diagnostic::syntax_error(
                tok.pos.clone(),
                "Expecting 'function' or 'procedure'",
            ))
        }
    };
    ctx.stream.skip();
    let ident = WithDecl::new(ctx.stream.expect_ident()?);
    ctx.stream.expect_kind(Is)?;
    ctx.stream.expect_kind(New)?;
    let subprogram_name = parse_name(ctx)?;
    let signature = if ctx.stream.next_kind_is(LeftSquare) {
        Some(parse_signature(ctx)?)
    } else {
        None
    };
    let generic_map = parse_map_aspect(ctx, Generic)?;
    let end_token = expect_semicolon_or_last(ctx);
    Ok(SubprogramInstantiation {
        span: TokenSpan::new(start_token, end_token),
        kind,
        ident,
        subprogram_name,
        signature,
        generic_map,
    })
}

pub fn parse_subprogram_specification(
    ctx: &mut ParsingContext<'_>,
) -> ParseResult<SubprogramSpecification> {
    let start_token = ctx.stream.get_current_token_id();
    let (is_function, is_pure) = {
        expect_token!(
            ctx.stream,
            token,
            Procedure => (false, false),
            Function => (true, true),
            Impure => {
                ctx.stream.expect_kind(Function)?;
                (true, false)
            },
            Pure => {
                ctx.stream.expect_kind(Function)?;
                (true, true)
            }
        )
    };

    let designator = parse_designator(ctx)?;

    let header = parse_optional_subprogram_header(ctx)?;

    let param_tok = ctx.stream.pop_if_kind(Parameter);
    let parameter_list = if ctx.stream.peek_kind() == Some(LeftPar) {
        let mut interface_list = parse_parameter_interface_list(ctx)?;
        if let Some(param_tok) = param_tok {
            interface_list.span.start_token = param_tok
        }
        Some(interface_list)
    } else {
        None
    };

    if is_function {
        ctx.stream.expect_kind(Return)?;
        let return_type = parse_type_mark(ctx)?;
        let end_token = ctx.stream.get_last_token_id();
        Ok(SubprogramSpecification::Function(FunctionSpecification {
            pure: is_pure,
            designator: designator.into(),
            header,
            parameter_list,
            return_type,
            span: TokenSpan::new(start_token, end_token),
        }))
    } else {
        let end_token = ctx.stream.get_last_token_id();
        Ok(SubprogramSpecification::Procedure(Box::new(
            ProcedureSpecification {
                designator: designator.into(),
                header,
                parameter_list,
                span: TokenSpan::new(start_token, end_token),
            },
        )))
    }
}

pub fn parse_subprogram_declaration(
    ctx: &mut ParsingContext<'_>,
) -> ParseResult<SubprogramDeclaration> {
    let start_token = ctx.stream.get_current_token_id();
    let specification = parse_subprogram_specification(ctx)?;
    let end_token = expect_semicolon_or_last(ctx);

    Ok(SubprogramDeclaration {
        span: TokenSpan::new(start_token, end_token),
        specification,
    })
}

/// LRM 4.3 Subprogram bodies
pub fn parse_subprogram_body(
    ctx: &mut ParsingContext<'_>,
    specification_start_token: TokenId,
    specification: SubprogramSpecification,
) -> ParseResult<SubprogramBody> {
    let end_kind = {
        match specification {
            SubprogramSpecification::Procedure(..) => Procedure,
            SubprogramSpecification::Function(..) => Function,
        }
    };
    let declarations = parse_declarative_part(ctx)?;
    let begin_token = ctx.stream.expect_kind(Begin)?;

    let statements = parse_labeled_sequential_statements(ctx)?;
    expect_token!(
        ctx.stream,
        end_token,
        end_token_id,
        End => {
            ctx.stream.pop_if_kind(end_kind);

            let end_ident = if matches!(ctx.stream.peek_kind(), Some(Identifier | StringLiteral)) {
                Some(parse_designator(ctx)?)
            } else {
                None
            };
            let semicolon = expect_semicolon_or_last(ctx);

            Ok(SubprogramBody {
                span: TokenSpan::new(specification_start_token, semicolon),
                end_ident_pos: check_end_identifier_mismatch(ctx, specification.subpgm_designator(), end_ident),
                begin_token,
                specification,
                declarations,
                statements,
                end_token: end_token_id
            })
        }
    )
}

pub fn parse_subprogram(ctx: &mut ParsingContext<'_>) -> ParseResult<Declaration> {
    if ctx.stream.next_kinds_are(&[Procedure, Identifier, Is, New])
        || ctx.stream.next_kinds_are(&[Function, Identifier, Is, New])
    {
        return Ok(Declaration::SubprogramInstantiation(
            parse_subprogram_instantiation(ctx)?,
        ));
    }
    let start_token = ctx.stream.get_current_token_id();
    let specification = parse_subprogram_specification(ctx)?;
    expect_token!(
        ctx.stream,
        token,
        Is => {
            Ok(Declaration::SubprogramBody(parse_subprogram_body(ctx, start_token, specification)?))
        },
        SemiColon => {
            Ok(Declaration::SubprogramDeclaration(SubprogramDeclaration{
                span: TokenSpan::new(start_token, ctx.stream.get_last_token_id()),
                specification,
            }))
        }
    )
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use super::*;

    use crate::syntax::test::{token_to_string, Code};
    use crate::{HasTokenSpan, Token};
    use pretty_assertions::assert_eq;

    fn check_token_span(tokens: &[Token], expected_str: &str) {
        assert_eq!(
            tokens.iter().map(token_to_string).collect::<Vec<String>>(),
            expected_str.split(' ').collect::<Vec<&str>>(),
        )
    }

    #[test]
    pub fn parses_procedure_declaration() {
        let code = Code::new(
            "\
procedure foo;
",
        );
        assert_eq!(
            code.with_stream_no_diagnostics(parse_subprogram_declaration),
            SubprogramDeclaration {
                span: code.token_span(),
                specification: SubprogramSpecification::Procedure(Box::new(
                    ProcedureSpecification {
                        designator: code
                            .s1("foo")
                            .ident()
                            .map_into(SubprogramDesignator::Identifier)
                            .into(),
                        header: None,
                        parameter_list: None,
                        span: code.s1("procedure foo").token_span(),
                    }
                ))
            }
        );
    }

    #[test]
    pub fn parses_function_specification() {
        let code = Code::new(
            "\
function foo return lib.foo.natural;
",
        );
        assert_eq!(
            code.with_stream_no_diagnostics(parse_subprogram_declaration),
            SubprogramDeclaration {
                span: code.token_span(),
                specification: SubprogramSpecification::Function(FunctionSpecification {
                    pure: true,
                    designator: code
                        .s1("foo")
                        .ident()
                        .map_into(SubprogramDesignator::Identifier)
                        .into(),
                    header: None,
                    parameter_list: None,
                    return_type: code.s1("lib.foo.natural").type_mark(),
                    span: code.between("function", ".natural").token_span(),
                })
            }
        );
    }

    #[test]
    pub fn parses_function_specification_operator() {
        let code = Code::new(
            "\
function \"+\" return lib.foo.natural;
",
        );
        assert_eq!(
            code.with_stream_no_diagnostics(parse_subprogram_declaration),
            SubprogramDeclaration {
                span: code.token_span(),
                specification: SubprogramSpecification::Function(FunctionSpecification {
                    pure: true,
                    designator: WithToken {
                        item: SubprogramDesignator::OperatorSymbol(Operator::Plus),
                        token: code.s1("\"+\"").token()
                    }
                    .into(),
                    header: None,
                    parameter_list: None,
                    return_type: code.s1("lib.foo.natural").type_mark(),
                    span: code.between("function", ".natural").token_span(),
                })
            }
        );
    }

    #[test]
    pub fn parses_impure_function_specification() {
        let code = Code::new(
            "\
impure function foo return lib.foo.natural;
",
        );
        assert_eq!(
            code.with_stream_no_diagnostics(parse_subprogram_declaration),
            SubprogramDeclaration {
                span: code.token_span(),
                specification: SubprogramSpecification::Function(FunctionSpecification {
                    pure: false,
                    designator: code
                        .s1("foo")
                        .ident()
                        .map_into(SubprogramDesignator::Identifier)
                        .into(),
                    header: None,
                    parameter_list: None,
                    return_type: code.s1("lib.foo.natural").type_mark(),
                    span: code.between("impure", ".natural").token_span(),
                })
            }
        );
    }
    #[test]
    pub fn parses_pure_function_specification() {
        let code = Code::new(
            "\
pure function foo return lib.foo.natural;
",
        );
        assert_eq!(
            code.with_stream_no_diagnostics(parse_subprogram_declaration),
            SubprogramDeclaration {
                span: code.token_span(),
                specification: SubprogramSpecification::Function(FunctionSpecification {
                    pure: true,
                    designator: code
                        .s1("foo")
                        .ident()
                        .map_into(SubprogramDesignator::Identifier)
                        .into(),
                    header: None,
                    parameter_list: None,
                    return_type: code.s1("lib.foo.natural").type_mark(),
                    span: code.between("pure", ".natural").token_span(),
                })
            }
        );
    }

    #[test]
    pub fn parses_procedure_specification_with_parameters() {
        let code = Code::new(
            "\
procedure foo(foo : natural);
",
        );
        assert_eq!(
            code.with_stream_no_diagnostics(parse_subprogram_declaration),
            SubprogramDeclaration {
                span: code.token_span(),
                specification: SubprogramSpecification::Procedure(Box::new(
                    ProcedureSpecification {
                        designator: code
                            .s1("foo")
                            .ident()
                            .map_into(SubprogramDesignator::Identifier)
                            .into(),
                        header: None,
                        parameter_list: Some(InterfaceList {
                            interface_type: InterfaceType::Parameter,
                            items: vec![code.s1("foo : natural").parameter()],
                            span: code.between("(", ")").token_span()
                        }),
                        span: code.between("procedure", "natural)").token_span(),
                    }
                ))
            }
        );
    }

    #[test]
    pub fn parses_function_specification_with_parameters() {
        let code = Code::new(
            "\
function foo(foo : natural) return lib.foo.natural;
",
        );
        assert_eq!(
            code.with_stream_no_diagnostics(parse_subprogram_declaration),
            SubprogramDeclaration {
                span: code.token_span(),
                specification: SubprogramSpecification::Function(FunctionSpecification {
                    pure: true,
                    designator: code
                        .s1("foo")
                        .ident()
                        .map_into(SubprogramDesignator::Identifier)
                        .into(),
                    header: None,
                    parameter_list: Some(InterfaceList {
                        interface_type: InterfaceType::Parameter,
                        items: vec![code.s1("foo : natural").parameter()],
                        span: code.between("(", ")").token_span()
                    }),
                    return_type: code.s1("lib.foo.natural").type_mark(),
                    span: code.between("function", ".natural").token_span(),
                })
            }
        );
    }

    #[test]
    pub fn parses_function_specification_with_parameters_and_keyword() {
        let code = Code::new(
            "\
function foo parameter (foo : natural) return lib.foo.natural;
",
        );
        assert_eq!(
            code.with_stream_no_diagnostics(parse_subprogram_declaration),
            SubprogramDeclaration {
                span: code.token_span(),
                specification: SubprogramSpecification::Function(FunctionSpecification {
                    pure: true,
                    designator: code
                        .s1("foo")
                        .ident()
                        .map_into(SubprogramDesignator::Identifier)
                        .into(),
                    header: None,
                    parameter_list: Some(InterfaceList {
                        interface_type: InterfaceType::Parameter,
                        items: vec![code.s1("foo : natural").parameter()],
                        span: code.between("parameter (", ")").token_span()
                    }),
                    return_type: code.s1("lib.foo.natural").type_mark(),
                    span: code.between("function", ".natural").token_span(),
                })
            }
        );
    }

    #[test]
    pub fn parses_function_specification_with_parameters_keyword_and_header() {
        let code = Code::new(
            "\
function foo generic (abc_def: natural) parameter (foo : natural) return lib.foo.natural;
",
        );
        assert_eq!(
            code.with_stream_no_diagnostics(parse_subprogram_declaration),
            SubprogramDeclaration {
                span: code.token_span(),
                specification: SubprogramSpecification::Function(FunctionSpecification {
                    pure: true,
                    designator: code
                        .s1("foo")
                        .ident()
                        .map_into(SubprogramDesignator::Identifier)
                        .into(),
                    header: Some(SubprogramHeader {
                        generic_list: InterfaceList {
                            interface_type: InterfaceType::Generic,
                            items: vec![code.s1("abc_def: natural").generic()],
                            span: code.s1("generic (abc_def: natural)").token_span()
                        },
                        map_aspect: None,
                    }),
                    parameter_list: Some(InterfaceList {
                        interface_type: InterfaceType::Parameter,
                        items: vec![code.s1("foo : natural").parameter()],
                        span: code.s1("parameter (foo : natural)").token_span()
                    }),
                    return_type: code.s1("lib.foo.natural").type_mark(),
                    span: code.between("function", ".natural").token_span(),
                })
            }
        );
    }

    #[test]
    pub fn parses_function_signature_only_return() {
        let code = Code::new("[return bar.type_mark]");
        assert_eq!(
            code.with_stream(parse_signature),
            WithTokenSpan::new(
                Signature::Function(vec![], code.s1("bar.type_mark").type_mark()),
                code.token_span()
            )
        );
    }

    #[test]
    pub fn parses_function_signature_one_argument() {
        let code = Code::new("[foo.type_mark return bar.type_mark]");
        assert_eq!(
            code.with_stream(parse_signature),
            WithTokenSpan::new(
                Signature::Function(
                    vec![code.s1("foo.type_mark").type_mark()],
                    code.s1("bar.type_mark").type_mark()
                ),
                code.token_span()
            )
        );
    }

    #[test]
    pub fn parses_function_signature_error_on_comma() {
        let code = Code::new("[foo.type_mark, return");
        assert_eq!(
            code.with_stream_err(parse_signature),
            Diagnostic::syntax_error(code.s1("return"), "Expected '{identifier}'"),
        );
    }

    #[test]
    pub fn parses_procedure_signature() {
        let code = Code::new("[foo.type_mark]");
        assert_eq!(
            code.with_stream(parse_signature),
            WithTokenSpan::new(
                Signature::Procedure(vec![code.s1("foo.type_mark").type_mark()]),
                code.token_span()
            )
        );
    }

    #[test]
    pub fn parses_function_signature_many_arguments() {
        let code = Code::new("[foo.type_mark, foo2.type_mark return bar.type_mark]");
        assert_eq!(
            code.with_stream(parse_signature),
            WithTokenSpan::new(
                Signature::Function(
                    vec![
                        code.s1("foo.type_mark").type_mark(),
                        code.s1("foo2.type_mark").type_mark()
                    ],
                    code.s1("bar.type_mark").type_mark()
                ),
                code.token_span()
            )
        );
    }

    #[test]
    pub fn parses_function_signature_many_return_error() {
        let code = Code::new("[return bar.type_mark return");
        assert_eq!(
            code.with_partial_stream(parse_signature),
            Err(Diagnostic::syntax_error(
                code.s("return", 2),
                "Expected ']'"
            ))
        );

        let code = Code::new("[foo return bar.type_mark return");
        assert_eq!(
            code.with_partial_stream(parse_signature),
            Err(Diagnostic::syntax_error(
                code.s("return", 2),
                "Expected ']'"
            ))
        );
    }

    #[test]
    pub fn parses_subprogram_body() {
        let code = Code::new(
            "\
function foo(arg : natural) return natural is
  constant foo : natural := 0;
begin
  return foo + arg;
end function;
",
        );
        let specification = code
            .s1("function foo(arg : natural) return natural")
            .subprogram_specification();
        let declarations = code.s1("constant foo : natural := 0;").declarative_part();
        let statements = vec![code.s1("return foo + arg;").sequential_statement()];
        let body = SubprogramBody {
            span: code.token_span(),
            specification,
            begin_token: code.s1("begin").token(),
            declarations,
            statements,
            end_token: code.s1("end").token(),
            end_ident_pos: None,
        };
        assert_eq!(
            code.with_stream_no_diagnostics(parse_subprogram),
            Declaration::SubprogramBody(body)
        );
    }

    #[test]
    pub fn parses_subprogram_declaration() {
        let code = Code::new(
            "\
function foo(arg : natural) return natural;
",
        );
        let specification = code
            .s1("function foo(arg : natural) return natural;")
            .subprogram_decl();
        assert_eq!(
            code.with_stream_no_diagnostics(parse_subprogram),
            Declaration::SubprogramDeclaration(specification)
        );
    }

    #[test]
    pub fn parses_subprogram_body_end_ident() {
        let code = Code::new(
            "\
function foo(arg : natural) return natural is
begin
end function foo;
",
        );
        let specification = code
            .s1("function foo(arg : natural) return natural")
            .subprogram_specification();
        let body = SubprogramBody {
            span: code.token_span(),
            specification,
            declarations: vec![],
            begin_token: code.s1("begin").token(),
            statements: vec![],
            end_token: code.s1("end").token(),
            end_ident_pos: Some(code.s("foo", 2).token()),
        };
        assert_eq!(
            code.with_stream_no_diagnostics(parse_subprogram),
            Declaration::SubprogramBody(body)
        );
    }

    #[test]
    pub fn parses_subprogram_body_end_operator_symbol() {
        let code = Code::new(
            "\
function \"+\"(arg : natural) return natural is
begin
end function \"+\";
",
        );
        let specification = code
            .s1("function \"+\"(arg : natural) return natural")
            .subprogram_specification();
        let body = SubprogramBody {
            span: code.token_span(),
            specification,
            declarations: vec![],
            begin_token: code.s1("begin").token(),
            statements: vec![],
            end_token: code.s1("end").token(),
            end_ident_pos: Some(code.s("\"+\"", 2).token()),
        };
        assert_eq!(
            code.with_stream_no_diagnostics(parse_subprogram),
            Declaration::SubprogramBody(body)
        );
    }

    #[test]
    pub fn parse_subprogram_header_no_aspect() {
        let code = Code::new("generic (x: natural := 1; y: real)");
        let header = code
            .subprogram_header()
            .expect("Expected subprogram header");
        assert_eq!(
            header,
            SubprogramHeader {
                map_aspect: None,
                generic_list: InterfaceList {
                    interface_type: InterfaceType::Generic,
                    items: vec![
                        code.s1("x: natural := 1").generic(),
                        code.s1("y: real").generic()
                    ],
                    span: code.between("generic (", ")").token_span()
                }
            }
        )
    }

    #[test]
    pub fn parse_subprogram_header_with_aspect() {
        let code = Code::new("generic (x: natural := 1; y: real) generic map (x => 2, y => 0.4)");
        let header = code
            .subprogram_header()
            .expect("Expected subprogram header");
        assert_eq!(
            header,
            SubprogramHeader {
                map_aspect: Some(MapAspect {
                    span: code.s1("generic map (x => 2, y => 0.4)").token_span(),
                    list: SeparatedList {
                        items: vec![
                            code.s1("x => 2").association_element(),
                            code.s1("y => 0.4").association_element()
                        ],
                        tokens: vec![code.s1(",").token()]
                    },
                }),
                generic_list: InterfaceList {
                    interface_type: InterfaceType::Generic,
                    items: vec![
                        code.s1("x: natural := 1").generic(),
                        code.s1("y: real").generic()
                    ],
                    span: code.between("generic (", ")").token_span()
                }
            }
        )
    }

    #[test]
    pub fn parse_function_spec_with_header_no_aspect() {
        let code = Code::new(
            "\
procedure my_proc
    generic (x: natural := 4; y: real := 4);
        ",
        );
        let decl = code.subprogram_decl();
        assert_eq!(
            decl,
            SubprogramDeclaration {
                span: code.token_span(),
                specification: SubprogramSpecification::Procedure(Box::new(
                    ProcedureSpecification {
                        designator: code
                            .s1("my_proc")
                            .ident()
                            .map_into(SubprogramDesignator::Identifier)
                            .into(),
                        header: code
                            .s1("generic (x: natural := 4; y: real := 4)")
                            .subprogram_header(),
                        parameter_list: None,
                        span: code.between("procedure", "4)").token_span(),
                    }
                ))
            }
        );
    }

    #[test]
    pub fn parse_function_spec_with_header_aspect() {
        let code = Code::new(
            "\
procedure my_proc
    generic (x: natural := 4; y: real := 4)
    generic map (x => 42);
        ",
        );
        let decl = code.subprogram_decl();
        assert_eq!(
            decl,
            SubprogramDeclaration {
                span: code.token_span(),
                specification: SubprogramSpecification::Procedure(Box::new(
                    ProcedureSpecification {
                        designator: code
                            .s1("my_proc")
                            .ident()
                            .map_into(SubprogramDesignator::Identifier)
                            .into(),
                        header: code
                            .s1("generic (x: natural := 4; y: real := 4)
    generic map (x => 42)")
                            .subprogram_header(),
                        parameter_list: None,
                        span: code.between("procedure", "42)").token_span(),
                    }
                ))
            }
        );
    }

    #[test]
    pub fn parse_function_with_header() {
        let code = Code::new(
            "\
function foo generic (x: natural := 4) (arg : natural) return natural is
  constant foo : natural := 0;
begin
  return foo + arg;
end function;
        ",
        );
        let specification = code
            .s1("function foo generic (x: natural := 4) (arg : natural) return natural")
            .subprogram_specification();
        let declarations = code.s1("constant foo : natural := 0;").declarative_part();
        let statements = vec![code.s1("return foo + arg;").sequential_statement()];
        let body = SubprogramBody {
            span: code.token_span(),
            specification,
            begin_token: code.s1("begin").token(),
            declarations,
            statements,
            end_token: code.s1("end").token(),
            end_ident_pos: None,
        };
        assert_eq!(
            code.with_stream_no_diagnostics(parse_subprogram),
            Declaration::SubprogramBody(body)
        );
    }

    #[test]
    pub fn swap_function() {
        // From GitHub, Issue #154
        let code = Code::new(
            "\
procedure swap
  generic ( type T )
  parameter (a, b : inout T) is
  variable temp : T;
begin
  temp := a; a := b; b := temp;
end procedure swap;
        ",
        );
        let specification = code
            .s1("procedure swap
  generic ( type T )
  parameter (a, b : inout T)")
            .subprogram_specification();
        let body = SubprogramBody {
            span: code.token_span(),
            specification,
            declarations: code.s1("variable temp : T;").declarative_part(),
            begin_token: code.s1("begin").token(),
            statements: vec![
                code.s1("temp := a;").sequential_statement(),
                code.s1("a := b;").sequential_statement(),
                code.s1(" b := temp;").sequential_statement(),
            ],
            end_token: code.s1("end").token(),
            end_ident_pos: Some(code.s("swap", 2).token()),
        };
        assert_eq!(
            code.with_stream_no_diagnostics(parse_subprogram),
            Declaration::SubprogramBody(body)
        );
    }

    #[test]
    pub fn subprogram_instantiation() {
        let code = Code::new("procedure my_proc is new proc;");
        let inst = code.parse_ok_no_diagnostics(parse_subprogram_instantiation);
        assert_eq!(
            inst,
            SubprogramInstantiation {
                kind: SubprogramKind::Procedure,
                span: code.token_span(),
                ident: code.s1("my_proc").decl_ident(),
                subprogram_name: code.s1("new proc").s1("proc").name(),
                signature: None,
                generic_map: None,
            }
        );

        let code = Code::new("function my_func is new func;");
        let inst = code.parse_ok_no_diagnostics(parse_subprogram_instantiation);
        assert_eq!(
            inst,
            SubprogramInstantiation {
                kind: SubprogramKind::Function,
                span: code.token_span(),
                ident: code.s1("my_func").decl_ident(),
                subprogram_name: code.s1("new func").s1("func").name(),
                signature: None,
                generic_map: None,
            }
        );

        let code = Code::new("function my_func is new func [bit return bit_vector];");
        let inst = code.parse_ok_no_diagnostics(parse_subprogram_instantiation);
        assert_eq!(
            inst,
            SubprogramInstantiation {
                kind: SubprogramKind::Function,
                span: code.token_span(),
                ident: code.s1("my_func").decl_ident(),
                subprogram_name: code.s1("new func").s1("func").name(),
                signature: Some(code.s1("[bit return bit_vector]").signature()),
                generic_map: None,
            }
        );

        let code =
            Code::new("function my_func is new func [bit return bit_vector] generic map (x => x);");
        let inst = code.parse_ok_no_diagnostics(parse_subprogram_instantiation);
        assert_eq!(
            inst,
            SubprogramInstantiation {
                kind: SubprogramKind::Function,
                span: code.token_span(),
                ident: code.s1("my_func").decl_ident(),
                subprogram_name: code.s1("new func").s1("func").name(),
                signature: Some(code.s1("[bit return bit_vector]").signature()),
                generic_map: Some(code.s1("generic map (x => x)").generic_map_aspect()),
            }
        );

        let code = Code::new("function my_func is new func generic map (z => z, x => y);");
        let inst = code.parse_ok_no_diagnostics(parse_subprogram_instantiation);
        assert_eq!(
            inst,
            SubprogramInstantiation {
                kind: SubprogramKind::Function,
                span: code.token_span(),
                ident: code.s1("my_func").decl_ident(),
                subprogram_name: code.s1("new func").s1("func").name(),
                signature: None,
                generic_map: Some(code.s1("generic map (z => z, x => y)").generic_map_aspect()),
            }
        );
    }

    #[test]
    pub fn subprogram_declaration() {
        let code = Code::new("procedure my_proc is new proc;");
        let inst = code.parse_ok_no_diagnostics(parse_subprogram);
        assert_eq!(
            inst,
            Declaration::SubprogramInstantiation(SubprogramInstantiation {
                kind: SubprogramKind::Procedure,
                span: code.token_span(),
                ident: code.s1("my_proc").decl_ident(),
                subprogram_name: code.s1("new proc").s1("proc").name(),
                signature: None,
                generic_map: None,
            })
        );

        let code = Code::new("function my_func is new func;");
        let inst = code.parse_ok_no_diagnostics(parse_subprogram);
        assert_eq!(
            inst,
            Declaration::SubprogramInstantiation(SubprogramInstantiation {
                kind: SubprogramKind::Function,
                span: code.token_span(),
                ident: code.s1("my_func").decl_ident(),
                subprogram_name: code.s1("new func").s1("func").name(),
                signature: None,
                generic_map: None,
            })
        );
    }

    #[test]
    pub fn test_token_span() {
        let code = Code::new(
            "\
package pkg is
    function foo generic (abc_def: natural) parameter (foo : natural) return lib.foo.natural;
    function bar (idx: natural) return boolean;
    procedure proc;
end package;
",
        );
        let ctx = code.tokenize();
        let pkg = code.package_declaration();
        let subprograms = pkg
            .decl
            .iter()
            .map(|d| {
                if let Declaration::SubprogramDeclaration(sub) = &d.item {
                    sub
                } else {
                    panic!("Only subprogram declarations are expected!")
                }
            })
            .collect_vec();

        check_token_span(subprograms[0]
                .get_token_slice(&ctx),
                "function foo generic ( abc_def : natural ) parameter ( foo : natural ) return lib . foo . natural ;");

        check_token_span(
            subprograms[1].get_token_slice(&ctx),
            "function bar ( idx : natural ) return boolean ;",
        );

        check_token_span(subprograms[2].get_token_slice(&ctx), "procedure proc ;");
    }
}
