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
use super::tokens::{kinds_error, Kind::*, TokenAccess, TokenStream};
use crate::ast::*;
use crate::data::*;
use crate::syntax::concurrent_statement::parse_map_aspect;
use crate::syntax::interface_declaration::parse_generic_interface_list;
use crate::syntax::names::parse_name;

pub fn parse_signature(stream: &TokenStream) -> ParseResult<WithPos<Signature>> {
    let left_square = stream.expect_kind(LeftSquare)?;
    let start_pos = stream.get_pos(left_square);
    let mut type_marks = Vec::new();
    let mut return_mark = None;

    let pos = peek_token!(
        stream, token,
        Return => {
            stream.skip();
            return_mark = Some(parse_type_mark(stream)?);
            let right_square = stream.expect_kind(RightSquare)?;
            start_pos.combine(stream.get_pos(right_square))
        },
        RightSquare => {
            stream.skip();
            start_pos.combine(&token.pos)
        },
        Identifier => {
            loop {
                let token = stream.peek_expect()?;

                match token.kind {
                    Identifier => {
                        type_marks.push(parse_type_mark(stream)?);
                        expect_token!(
                            stream,
                            sep_token,
                            Comma => {},
                            RightSquare => {
                                break start_pos.combine(&sep_token.pos);
                            },
                            Return => {
                                return_mark = Some(parse_type_mark(stream)?);
                                let right_square = stream.expect_kind(RightSquare)?;
                                break start_pos.combine(stream.get_pos(right_square));
                            }
                        )
                    }
                    _ => {
                        stream.skip();
                        return Err(kinds_error(stream.pos_before(token), &[Identifier]))
                    }
                };
            }
        }
    );

    let signature = match return_mark {
        Some(return_mark) => Signature::Function(type_marks, return_mark),
        None => Signature::Procedure(type_marks),
    };

    Ok(WithPos::new(signature, pos))
}

fn parse_designator(stream: &TokenStream) -> ParseResult<WithPos<SubprogramDesignator>> {
    Ok(expect_token!(
        stream,
        token,
        Identifier => token.to_identifier_value()?.map_into(SubprogramDesignator::Identifier),
        StringLiteral => token.to_operator_symbol()?.map_into(SubprogramDesignator::OperatorSymbol)
    ))
}

/// Parses a subprogram header according of the form
/// subprogram_header ::=
///     [ generic ( generic_list )
///     [ generic_map_aspect ] ]
pub fn parse_optional_subprogram_header(
    stream: &TokenStream,
    diagnostics: &mut dyn DiagnosticHandler,
) -> ParseResult<Option<SubprogramHeader>> {
    let Some(generic) = stream.pop_if_kind(Generic) else {
        return Ok(None);
    };
    let generic_list = parse_generic_interface_list(stream, diagnostics)?;
    let map_aspect = parse_map_aspect(stream, Generic, diagnostics)?;

    Ok(Some(SubprogramHeader {
        generic_tok: generic,
        generic_list,
        map_aspect,
    }))
}

pub fn parse_subprogram_instantiation(
    stream: &TokenStream,
    diagnostics: &mut dyn DiagnosticHandler,
) -> ParseResult<SubprogramInstantiation> {
    let id = stream.get_token_id();
    let tok = stream.peek_expect()?;
    let kind = match tok.kind {
        Procedure => WithToken::new(SubprogramKind::Procedure, id),
        Function => WithToken::new(SubprogramKind::Function, id),
        _ => {
            return Err(Diagnostic::error(
                tok.pos.clone(),
                "Expecting 'function' or 'procedure'",
            ))
        }
    };
    stream.skip();
    let ident = WithDecl::new(stream.expect_ident()?);
    stream.expect_kind(Is)?;
    stream.expect_kind(New)?;
    let subprogram_name = parse_name(stream)?;
    let signature = if stream.next_kind_is(LeftSquare) {
        Some(parse_signature(stream)?)
    } else {
        None
    };
    let generic_map = parse_map_aspect(stream, Generic, diagnostics)?;
    let semi = stream.expect_kind(SemiColon)?;
    Ok(SubprogramInstantiation {
        kind,
        ident,
        subprogram_name,
        signature,
        generic_map,
        semi,
    })
}

pub fn parse_subprogram_declaration_no_semi(
    stream: &TokenStream,
    diagnostics: &mut dyn DiagnosticHandler,
) -> ParseResult<SubprogramDeclaration> {
    let (is_function, is_pure) = {
        expect_token!(
            stream,
            token,
            Procedure => (false, false),
            Function => (true, true),
            Impure => {
                stream.expect_kind(Function)?;
                (true, false)
            },
            Pure => {
                stream.expect_kind(Function)?;
                (true, true)
            }
        )
    };

    let designator = parse_designator(stream)?;

    let header = parse_optional_subprogram_header(stream, diagnostics)?;

    let (parameter_list, param_tok) = {
        if let Some(parameter) = stream.pop_if_kind(Parameter) {
            (
                parse_parameter_interface_list(stream, diagnostics)?,
                Some(parameter),
            )
        } else if stream.peek_kind() == Some(LeftPar) {
            (parse_parameter_interface_list(stream, diagnostics)?, None)
        } else {
            (Vec::new(), None)
        }
    };

    if is_function {
        stream.expect_kind(Return)?;
        let return_type = parse_type_mark(stream)?;
        Ok(SubprogramDeclaration::Function(FunctionSpecification {
            pure: is_pure,
            param_tok,
            designator: designator.into(),
            header,
            parameter_list,
            return_type,
        }))
    } else {
        Ok(SubprogramDeclaration::Procedure(ProcedureSpecification {
            designator: designator.into(),
            param_tok,
            header,
            parameter_list,
        }))
    }
}

pub fn parse_subprogram_declaration(
    stream: &TokenStream,
    diagnostics: &mut dyn DiagnosticHandler,
) -> ParseResult<SubprogramDeclaration> {
    let res = parse_subprogram_declaration_no_semi(stream, diagnostics);
    stream.expect_kind(SemiColon)?;
    res
}

/// LRM 4.3 Subprogram bodies
pub fn parse_subprogram_body(
    stream: &TokenStream,
    specification: SubprogramDeclaration,
    diagnostics: &mut dyn DiagnosticHandler,
) -> ParseResult<SubprogramBody> {
    let end_kind = {
        match specification {
            SubprogramDeclaration::Procedure(..) => Procedure,
            SubprogramDeclaration::Function(..) => Function,
        }
    };
    let declarations = parse_declarative_part(stream, diagnostics)?;
    stream.expect_kind(Begin)?;

    let statements = parse_labeled_sequential_statements(stream, diagnostics)?;
    expect_token!(
        stream,
        end_token,
        End => {
            stream.pop_if_kind(end_kind);

            let end_ident = if matches!(stream.peek_kind(), Some(Identifier | StringLiteral)) {
                Some(parse_designator(stream)?)
            } else {
                None
            };
            stream.expect_kind(SemiColon)?;

            Ok(SubprogramBody {
                end_ident_pos: check_end_identifier_mismatch(specification.subpgm_designator(), end_ident, diagnostics),
                specification,
                declarations,
                statements,
            })
        }
    )
}

pub fn parse_subprogram(
    stream: &TokenStream,
    diagnostics: &mut dyn DiagnosticHandler,
) -> ParseResult<Declaration> {
    if stream.next_kinds_are(&[Procedure, Identifier, Is, New])
        || stream.next_kinds_are(&[Function, Identifier, Is, New])
    {
        return Ok(Declaration::SubprogramInstantiation(
            parse_subprogram_instantiation(stream, diagnostics)?,
        ));
    }
    let specification = parse_subprogram_declaration_no_semi(stream, diagnostics)?;
    expect_token!(
        stream,
        token,
        Is => {
            Ok(Declaration::SubprogramBody(parse_subprogram_body(stream, specification, diagnostics)?))
        },
        SemiColon => {
            Ok(Declaration::SubprogramDeclaration(specification))
        }
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::syntax::test::Code;

    #[test]
    pub fn parses_procedure_specification() {
        let code = Code::new(
            "\
procedure foo;
",
        );
        assert_eq!(
            code.with_stream_no_diagnostics(parse_subprogram_declaration),
            SubprogramDeclaration::Procedure(ProcedureSpecification {
                designator: code
                    .s1("foo")
                    .ident()
                    .map_into(SubprogramDesignator::Identifier)
                    .into(),
                param_tok: None,
                header: None,
                parameter_list: Vec::new(),
            })
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
            SubprogramDeclaration::Function(FunctionSpecification {
                pure: true,
                designator: code
                    .s1("foo")
                    .ident()
                    .map_into(SubprogramDesignator::Identifier)
                    .into(),
                header: None,
                param_tok: None,
                parameter_list: Vec::new(),
                return_type: code.s1("lib.foo.natural").type_mark()
            })
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
            SubprogramDeclaration::Function(FunctionSpecification {
                pure: true,
                designator: WithPos {
                    item: SubprogramDesignator::OperatorSymbol(Operator::Plus),
                    pos: code.s1("\"+\"").pos()
                }
                .into(),
                header: None,
                param_tok: None,
                parameter_list: Vec::new(),
                return_type: code.s1("lib.foo.natural").type_mark()
            })
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
            SubprogramDeclaration::Function(FunctionSpecification {
                pure: false,
                designator: code
                    .s1("foo")
                    .ident()
                    .map_into(SubprogramDesignator::Identifier)
                    .into(),
                header: None,
                param_tok: None,
                parameter_list: Vec::new(),
                return_type: code.s1("lib.foo.natural").type_mark()
            })
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
            SubprogramDeclaration::Function(FunctionSpecification {
                pure: true,
                designator: code
                    .s1("foo")
                    .ident()
                    .map_into(SubprogramDesignator::Identifier)
                    .into(),
                header: None,
                param_tok: None,
                parameter_list: Vec::new(),
                return_type: code.s1("lib.foo.natural").type_mark()
            })
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
            SubprogramDeclaration::Procedure(ProcedureSpecification {
                designator: code
                    .s1("foo")
                    .ident()
                    .map_into(SubprogramDesignator::Identifier)
                    .into(),
                header: None,
                param_tok: None,
                parameter_list: vec![code.s1("foo : natural").parameter()],
            })
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
            SubprogramDeclaration::Function(FunctionSpecification {
                pure: true,
                designator: code
                    .s1("foo")
                    .ident()
                    .map_into(SubprogramDesignator::Identifier)
                    .into(),
                header: None,
                param_tok: None,
                parameter_list: vec![code.s1("foo : natural").parameter()],
                return_type: code.s1("lib.foo.natural").type_mark()
            })
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
            SubprogramDeclaration::Function(FunctionSpecification {
                pure: true,
                designator: code
                    .s1("foo")
                    .ident()
                    .map_into(SubprogramDesignator::Identifier)
                    .into(),
                header: None,
                param_tok: Some(code.s1("parameter").token()),
                parameter_list: vec![code.s1("foo : natural").parameter()],
                return_type: code.s1("lib.foo.natural").type_mark()
            })
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
            SubprogramDeclaration::Function(FunctionSpecification {
                pure: true,
                designator: code
                    .s1("foo")
                    .ident()
                    .map_into(SubprogramDesignator::Identifier)
                    .into(),
                header: Some(SubprogramHeader {
                    generic_list: vec![code.s1("abc_def: natural").generic(),],
                    map_aspect: None,
                    generic_tok: code.s1("generic").token()
                }),
                param_tok: Some(code.s1("parameter").token()),
                parameter_list: vec![code.s1("foo : natural").parameter()],
                return_type: code.s1("lib.foo.natural").type_mark()
            })
        );
    }

    #[test]
    pub fn parses_function_signature_only_return() {
        let code = Code::new("[return bar.type_mark]");
        assert_eq!(
            code.with_stream(parse_signature),
            WithPos::new(
                Signature::Function(vec![], code.s1("bar.type_mark").type_mark()),
                code.pos()
            )
        );
    }

    #[test]
    pub fn parses_function_signature_one_argument() {
        let code = Code::new("[foo.type_mark return bar.type_mark]");
        assert_eq!(
            code.with_stream(parse_signature),
            WithPos::new(
                Signature::Function(
                    vec![code.s1("foo.type_mark").type_mark()],
                    code.s1("bar.type_mark").type_mark()
                ),
                code.pos()
            )
        );
    }

    #[test]
    pub fn parses_function_signature_error_on_comma() {
        let code = Code::new("[foo.type_mark, return");
        assert_eq!(
            code.with_stream_err(parse_signature),
            Diagnostic::error(code.s1("return"), "Expected '{identifier}'"),
        );
    }

    #[test]
    pub fn parses_procedure_signature() {
        let code = Code::new("[foo.type_mark]");
        assert_eq!(
            code.with_stream(parse_signature),
            WithPos::new(
                Signature::Procedure(vec![code.s1("foo.type_mark").type_mark()]),
                code.pos()
            )
        );
    }

    #[test]
    pub fn parses_function_signature_many_arguments() {
        let code = Code::new("[foo.type_mark, foo2.type_mark return bar.type_mark]");
        assert_eq!(
            code.with_stream(parse_signature),
            WithPos::new(
                Signature::Function(
                    vec![
                        code.s1("foo.type_mark").type_mark(),
                        code.s1("foo2.type_mark").type_mark()
                    ],
                    code.s1("bar.type_mark").type_mark()
                ),
                code.pos()
            )
        );
    }

    #[test]
    pub fn parses_function_signature_many_return_error() {
        let code = Code::new("[return bar.type_mark return");
        assert_eq!(
            code.with_partial_stream(parse_signature),
            Err(Diagnostic::error(code.s("return", 2), "Expected ']'"))
        );

        let code = Code::new("[foo return bar.type_mark return");
        assert_eq!(
            code.with_partial_stream(parse_signature),
            Err(Diagnostic::error(code.s("return", 2), "Expected ']'"))
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
            .subprogram_decl();
        let declarations = code.s1("constant foo : natural := 0;").declarative_part();
        let statements = vec![code.s1("return foo + arg;").sequential_statement()];
        let body = SubprogramBody {
            specification,
            declarations,
            statements,
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
            .s1("function foo(arg : natural) return natural")
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
            .subprogram_decl();
        let body = SubprogramBody {
            specification,
            declarations: vec![],
            statements: vec![],
            end_ident_pos: Some(code.s("foo", 2).pos()),
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
            .subprogram_decl();
        let body = SubprogramBody {
            specification,
            declarations: vec![],
            statements: vec![],
            end_ident_pos: Some(code.s("\"+\"", 2).pos()),
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
                generic_tok: code.s1("generic").token(),
                map_aspect: None,
                generic_list: vec![
                    code.s1("x: natural := 1").generic(),
                    code.s1("y: real").generic()
                ]
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
                generic_tok: code.s1("generic").token(),
                map_aspect: Some(MapAspect {
                    start: code.s("generic", 2).token(),
                    list: SeparatedList {
                        items: vec![
                            code.s1("x => 2").association_element(),
                            code.s1("y => 0.4").association_element()
                        ],
                        tokens: vec![code.s1(",").token()]
                    },
                    closing_paren: code.s(")", 2).token()
                }),
                generic_list: vec![
                    code.s1("x: natural := 1").generic(),
                    code.s1("y: real").generic()
                ]
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
            SubprogramDeclaration::Procedure(ProcedureSpecification {
                designator: code
                    .s1("my_proc")
                    .ident()
                    .map_into(SubprogramDesignator::Identifier)
                    .into(),
                header: code
                    .s1("generic (x: natural := 4; y: real := 4)")
                    .subprogram_header(),
                param_tok: None,
                parameter_list: vec![],
            })
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
            SubprogramDeclaration::Procedure(ProcedureSpecification {
                designator: code
                    .s1("my_proc")
                    .ident()
                    .map_into(SubprogramDesignator::Identifier)
                    .into(),
                header: code
                    .s1("generic (x: natural := 4; y: real := 4)
    generic map (x => 42)")
                    .subprogram_header(),
                param_tok: None,
                parameter_list: vec![],
            })
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
            .subprogram_decl();
        let declarations = code.s1("constant foo : natural := 0;").declarative_part();
        let statements = vec![code.s1("return foo + arg;").sequential_statement()];
        let body = SubprogramBody {
            specification,
            declarations,
            statements,
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
            .subprogram_decl();
        let body = SubprogramBody {
            specification,
            declarations: code.s1("variable temp : T;").declarative_part(),
            statements: vec![
                code.s1("temp := a;").sequential_statement(),
                code.s1("a := b;").sequential_statement(),
                code.s1(" b := temp;").sequential_statement(),
            ],
            end_ident_pos: Some(code.s("swap", 2).pos()),
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
                kind: WithToken::new(SubprogramKind::Procedure, code.s1("procedure").token()),
                ident: code.s1("my_proc").decl_ident(),
                subprogram_name: code.s1("new proc").s1("proc").name(),
                signature: None,
                generic_map: None,
                semi: code.s1(";").token()
            }
        );

        let code = Code::new("function my_func is new func;");
        let inst = code.parse_ok_no_diagnostics(parse_subprogram_instantiation);
        assert_eq!(
            inst,
            SubprogramInstantiation {
                kind: WithToken::new(SubprogramKind::Function, code.s1("function").token()),
                ident: code.s1("my_func").decl_ident(),
                subprogram_name: code.s1("new func").s1("func").name(),
                signature: None,
                generic_map: None,
                semi: code.s1(";").token()
            }
        );

        let code = Code::new("function my_func is new func [bit return bit_vector];");
        let inst = code.parse_ok_no_diagnostics(parse_subprogram_instantiation);
        assert_eq!(
            inst,
            SubprogramInstantiation {
                kind: WithToken::new(SubprogramKind::Function, code.s1("function").token()),
                ident: code.s1("my_func").decl_ident(),
                subprogram_name: code.s1("new func").s1("func").name(),
                signature: Some(code.s1("[bit return bit_vector]").signature()),
                generic_map: None,
                semi: code.s1(";").token()
            }
        );

        let code =
            Code::new("function my_func is new func [bit return bit_vector] generic map (x => x);");
        let inst = code.parse_ok_no_diagnostics(parse_subprogram_instantiation);
        assert_eq!(
            inst,
            SubprogramInstantiation {
                kind: WithToken::new(SubprogramKind::Function, code.s1("function").token()),
                ident: code.s1("my_func").decl_ident(),
                subprogram_name: code.s1("new func").s1("func").name(),
                signature: Some(code.s1("[bit return bit_vector]").signature()),
                generic_map: Some(code.s1("generic map (x => x)").generic_map_aspect()),
                semi: code.s1(";").token()
            }
        );

        let code = Code::new("function my_func is new func generic map (z => z, x => y);");
        let inst = code.parse_ok_no_diagnostics(parse_subprogram_instantiation);
        assert_eq!(
            inst,
            SubprogramInstantiation {
                kind: WithToken::new(SubprogramKind::Function, code.s1("function").token()),
                ident: code.s1("my_func").decl_ident(),
                subprogram_name: code.s1("new func").s1("func").name(),
                signature: None,
                generic_map: Some(code.s1("generic map (z => z, x => y)").generic_map_aspect()),
                semi: code.s1(";").token()
            }
        );
    }
}
