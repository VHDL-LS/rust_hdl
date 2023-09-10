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
use super::tokens::{kinds_error, Kind::*, TokenStream};
use crate::ast::*;
use crate::data::*;

pub fn parse_signature(stream: &TokenStream) -> ParseResult<WithPos<Signature>> {
    let left_square = stream.expect_kind(LeftSquare)?;
    let start_pos = &left_square.pos;
    let mut type_marks = Vec::new();
    let mut return_mark = None;

    let pos = peek_token!(
        stream, token,
        Return => {
            stream.skip();
            return_mark = Some(parse_type_mark(stream)?);
            start_pos.combine(&stream.expect_kind(RightSquare)?)
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
                                break start_pos.combine(&stream.expect_kind(RightSquare)?);
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

    let parameter_list = {
        if stream.peek_kind() == Some(LeftPar) {
            parse_parameter_interface_list(stream, diagnostics)?
        } else {
            Vec::new()
        }
    };

    if is_function {
        stream.expect_kind(Return)?;
        let return_type = parse_type_mark(stream)?;
        Ok(SubprogramDeclaration::Function(FunctionSpecification {
            pure: is_pure,
            designator: designator.into(),
            parameter_list,
            return_type,
        }))
    } else {
        Ok(SubprogramDeclaration::Procedure(ProcedureSpecification {
            designator: designator.into(),
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
}
