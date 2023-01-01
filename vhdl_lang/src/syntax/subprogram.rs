// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::common::ParseResult;
use super::declarative_part::parse_declarative_part;
use super::interface_declaration::parse_parameter_interface_list;
use super::names::parse_selected_name;
use super::sequential_statement::parse_labeled_sequential_statements;
use super::tokens::{kinds_error, Kind::*, TokenStream};
use crate::ast::*;
use crate::data::*;

pub fn parse_signature(stream: &mut TokenStream) -> ParseResult<WithPos<Signature>> {
    let left_square = stream.expect_kind(LeftSquare)?;
    let start_pos = left_square.pos;
    let mut type_marks = Vec::new();
    let mut return_mark = None;

    let token = stream.peek_expect()?;
    let pos = try_token_kind!(
        token,
        Return => {
            stream.move_after(&token);
            return_mark = Some(parse_selected_name(stream)?);
            start_pos.combine(&stream.expect_kind(RightSquare)?)
        },
        RightSquare => {
            stream.move_after(&token);
            start_pos.combine(&token.pos)
        },
        Identifier => {
            loop {
                let token = stream.peek_expect()?;

                match token.kind {
                    Identifier => {
                        type_marks.push(parse_selected_name(stream)?);
                        let sep_token = stream.expect()?;

                        try_token_kind!(
                            sep_token,
                            Comma => {},
                            RightSquare => {
                                break start_pos.combine(&sep_token.pos);
                            },
                            Return => {
                                return_mark = Some(parse_selected_name(stream)?);
                                break start_pos.combine(&stream.expect_kind(RightSquare)?);
                            }
                        )
                    }
                    _ => {
                        stream.move_after(&token);
                        return Err(kinds_error(token.pos, &[Identifier]))
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

fn parse_designator(stream: &mut TokenStream) -> ParseResult<WithPos<SubprogramDesignator>> {
    let token = stream.expect()?;
    Ok(try_token_kind!(
        token,
        Identifier => token.expect_ident()?.map_into(SubprogramDesignator::Identifier),
        StringLiteral => token.expect_string()?.map_into(SubprogramDesignator::OperatorSymbol)
    ))
}

pub fn parse_subprogram_declaration_no_semi(
    stream: &mut TokenStream,
    diagnostics: &mut dyn DiagnosticHandler,
) -> ParseResult<SubprogramDeclaration> {
    let token = stream.expect()?;

    let (is_function, is_pure) = {
        try_token_kind!(
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
        if stream.peek_kind()? == Some(LeftPar) {
            parse_parameter_interface_list(stream, diagnostics)?
        } else {
            Vec::new()
        }
    };

    if is_function {
        stream.expect_kind(Return)?;
        let return_type = parse_selected_name(stream)?;
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
    stream: &mut TokenStream,
    diagnostics: &mut dyn DiagnosticHandler,
) -> ParseResult<SubprogramDeclaration> {
    let res = parse_subprogram_declaration_no_semi(stream, diagnostics);
    stream.expect_kind(SemiColon)?;
    res
}

/// LRM 4.3 Subprogram bodies
pub fn parse_subprogram_body(
    stream: &mut TokenStream,
    specification: SubprogramDeclaration,
    diagnostics: &mut dyn DiagnosticHandler,
) -> ParseResult<SubprogramBody> {
    let end_kind = {
        match specification {
            SubprogramDeclaration::Procedure(..) => Procedure,
            SubprogramDeclaration::Function(..) => Function,
        }
    };
    let declarations = parse_declarative_part(stream, diagnostics, true)?;

    let (statements, end_token) = parse_labeled_sequential_statements(stream, diagnostics)?;
    try_token_kind!(
        end_token,
        End => {
            stream.pop_if_kind(end_kind)?;
            stream.pop_if_kind(Identifier)?;
            stream.pop_if_kind(StringLiteral)?;
            stream.expect_kind(SemiColon)?;
        }
    );
    Ok(SubprogramBody {
        specification,
        declarations,
        statements,
    })
}

pub fn parse_subprogram(
    stream: &mut TokenStream,
    diagnostics: &mut dyn DiagnosticHandler,
) -> ParseResult<Declaration> {
    let specification = parse_subprogram_declaration_no_semi(stream, diagnostics)?;
    match_token_kind!(
        stream.expect()?,
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

    use crate::data::Latin1String;
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
                return_type: code.s1("lib.foo.natural").selected_name()
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
                    item: SubprogramDesignator::OperatorSymbol(Latin1String::from_utf8_unchecked(
                        "+"
                    )),
                    pos: code.s1("\"+\"").pos()
                }
                .into(),
                parameter_list: Vec::new(),
                return_type: code.s1("lib.foo.natural").selected_name()
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
                return_type: code.s1("lib.foo.natural").selected_name()
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
                return_type: code.s1("lib.foo.natural").selected_name()
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
                return_type: code.s1("lib.foo.natural").selected_name()
            })
        );
    }

    #[test]
    pub fn parses_function_signature_only_return() {
        let code = Code::new("[return bar.type_mark]");
        assert_eq!(
            code.with_stream(parse_signature),
            WithPos::new(
                Signature::Function(vec![], code.s1("bar.type_mark").selected_name()),
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
                    vec![code.s1("foo.type_mark").selected_name()],
                    code.s1("bar.type_mark").selected_name()
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
                Signature::Procedure(vec![code.s1("foo.type_mark").selected_name()]),
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
                        code.s1("foo.type_mark").selected_name(),
                        code.s1("foo2.type_mark").selected_name()
                    ],
                    code.s1("bar.type_mark").selected_name()
                ),
                code.pos()
            )
        );
    }

    #[test]
    pub fn parses_function_signature_many_return_error() {
        let code = Code::new("[return bar.type_mark return");
        assert_eq!(
            code.with_stream_err(parse_signature),
            Diagnostic::error(code.s("return", 2), "Expected ']'")
        );

        let code = Code::new("[foo return bar.type_mark return");
        assert_eq!(
            code.with_stream_err(parse_signature),
            Diagnostic::error(code.s("return", 2), "Expected ']'")
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
}
