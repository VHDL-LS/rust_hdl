// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use crate::ast::{
    Declaration, FunctionSpecification, ProcedureSpecification, Signature, SubprogramBody,
    SubprogramDeclaration, SubprogramDesignator,
};
use crate::declarative_part::parse_declarative_part;
use crate::interface_declaration::parse_parameter_interface_list;
use crate::message::{Message, MessageHandler, ParseResult};
use crate::names::parse_selected_name;
use crate::sequential_statement::parse_labeled_sequential_statements;
use crate::source::WithPos;
use crate::tokenizer::Kind::*;
use crate::tokenstream::TokenStream;

pub fn parse_signature(stream: &mut TokenStream) -> ParseResult<Signature> {
    stream.expect_kind(LeftSquare)?;
    let mut type_marks = Vec::new();
    let mut return_mark = None;
    let mut errmsg = None;
    loop {
        let token = stream.peek_expect()?;

        try_token_kind!(
            token,

            Identifier => {
                type_marks.push(parse_selected_name(stream)?);
                let sep_token = stream.expect()?;

                try_token_kind!(
                    sep_token,
                    Comma => {},
                    RightSquare => {
                        break;
                    },
                    Return => {
                        let new_return_mark = Some(parse_selected_name(stream)?);
                        if return_mark.is_some() {
                            errmsg = Some(Message::error(sep_token, "Duplicate return in signature"));
                        } else {
                            return_mark = new_return_mark;
                        }
                    }
                )
            },
            Return => {
                stream.move_after(&token);
                let new_return_mark = Some(parse_selected_name(stream)?);
                if return_mark.is_some() {
                    errmsg = Some(Message::error(token, "Duplicate return in signature"));
                } else {
                    return_mark = new_return_mark;
                }
            },
            RightSquare => {
                stream.move_after(&token);
                break;
            }
        )
    }
    if let Some(msg) = errmsg {
        // @TODO recoverable error should not return Err
        return Err(msg);
    }

    Ok(match return_mark {
        Some(return_mark) => Signature::Function(type_marks, return_mark),
        None => Signature::Procedure(type_marks),
    })
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
    messages: &mut dyn MessageHandler,
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
            }
        )
    };

    let designator = parse_designator(stream)?;

    let parameter_list = {
        if stream.peek_kind()? == Some(LeftPar) {
            parse_parameter_interface_list(stream, messages)?
        } else {
            Vec::new()
        }
    };

    if is_function {
        stream.expect_kind(Return)?;
        let return_type = parse_selected_name(stream)?;
        Ok(SubprogramDeclaration::Function(FunctionSpecification {
            pure: is_pure,
            designator,
            parameter_list,
            return_type,
        }))
    } else {
        Ok(SubprogramDeclaration::Procedure(ProcedureSpecification {
            designator,
            parameter_list,
        }))
    }
}

pub fn parse_subprogram_declaration(
    stream: &mut TokenStream,
    messages: &mut dyn MessageHandler,
) -> ParseResult<SubprogramDeclaration> {
    let res = parse_subprogram_declaration_no_semi(stream, messages);
    stream.expect_kind(SemiColon)?;
    res
}

/// LRM 4.3 Subprogram bodies
pub fn parse_subprogram_body(
    stream: &mut TokenStream,
    specification: SubprogramDeclaration,
    messages: &mut dyn MessageHandler,
) -> ParseResult<SubprogramBody> {
    let end_kind = {
        match specification {
            SubprogramDeclaration::Procedure(..) => Procedure,
            SubprogramDeclaration::Function(..) => Function,
        }
    };
    let declarations = parse_declarative_part(stream, messages, true)?;

    let (statements, end_token) = parse_labeled_sequential_statements(stream, messages)?;
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
    messages: &mut dyn MessageHandler,
) -> ParseResult<Declaration> {
    let specification = parse_subprogram_declaration_no_semi(stream, messages)?;
    match_token_kind!(
        stream.expect()?,
        Is => {
            Ok(Declaration::SubprogramBody(parse_subprogram_body(stream, specification, messages)?))
        },
        SemiColon => {
            Ok(Declaration::SubprogramDeclaration(specification))
        }
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::latin_1::Latin1String;
    use crate::test_util::Code;

    #[test]
    pub fn parses_procedure_specification() {
        let code = Code::new(
            "\
procedure foo;
",
        );
        assert_eq!(
            code.with_stream_no_messages(parse_subprogram_declaration),
            SubprogramDeclaration::Procedure(ProcedureSpecification {
                designator: code
                    .s1("foo")
                    .ident()
                    .map_into(SubprogramDesignator::Identifier),
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
            code.with_stream_no_messages(parse_subprogram_declaration),
            SubprogramDeclaration::Function(FunctionSpecification {
                pure: true,
                designator: code
                    .s1("foo")
                    .ident()
                    .map_into(SubprogramDesignator::Identifier),
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
            code.with_stream_no_messages(parse_subprogram_declaration),
            SubprogramDeclaration::Function(FunctionSpecification {
                pure: true,
                designator: WithPos {
                    item: SubprogramDesignator::OperatorSymbol(Latin1String::from_utf8_unchecked(
                        "+"
                    )),
                    pos: code.s1("\"+\"").pos()
                },
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
            code.with_stream_no_messages(parse_subprogram_declaration),
            SubprogramDeclaration::Function(FunctionSpecification {
                pure: false,
                designator: code
                    .s1("foo")
                    .ident()
                    .map_into(SubprogramDesignator::Identifier),
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
            code.with_stream_no_messages(parse_subprogram_declaration),
            SubprogramDeclaration::Procedure(ProcedureSpecification {
                designator: code
                    .s1("foo")
                    .ident()
                    .map_into(SubprogramDesignator::Identifier),
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
            code.with_stream_no_messages(parse_subprogram_declaration),
            SubprogramDeclaration::Function(FunctionSpecification {
                pure: true,
                designator: code
                    .s1("foo")
                    .ident()
                    .map_into(SubprogramDesignator::Identifier),
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
            Signature::Function(vec![], code.s1("bar.type_mark").selected_name())
        );
    }

    #[test]
    pub fn parses_function_signature_one_argument() {
        let code = Code::new("[foo.type_mark return bar.type_mark]");
        assert_eq!(
            code.with_stream(parse_signature),
            Signature::Function(
                vec![code.s1("foo.type_mark").selected_name()],
                code.s1("bar.type_mark").selected_name()
            )
        );
    }

    #[test]
    pub fn parses_procedure_signature() {
        let code = Code::new("[foo.type_mark]");
        assert_eq!(
            code.with_stream(parse_signature),
            Signature::Procedure(vec![code.s1("foo.type_mark").selected_name()])
        );
    }

    #[test]
    pub fn parses_function_signature_many_arguments() {
        let code = Code::new("[foo.type_mark, foo2.type_mark return bar.type_mark]");
        assert_eq!(
            code.with_stream(parse_signature),
            Signature::Function(
                vec![
                    code.s1("foo.type_mark").selected_name(),
                    code.s1("foo2.type_mark").selected_name()
                ],
                code.s1("bar.type_mark").selected_name()
            )
        );
    }

    #[test]
    pub fn parses_function_signature_many_return_error() {
        let code = Code::new("[return bar.type_mark return bar2]");
        assert_eq!(
            code.with_stream_err(parse_signature),
            Message::error(code.s("return", 2), "Duplicate return in signature")
        );

        let code = Code::new("[foo return bar.type_mark return bar2]");
        assert_eq!(
            code.with_stream_err(parse_signature),
            Message::error(code.s("return", 2), "Duplicate return in signature")
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
            code.with_stream_no_messages(parse_subprogram),
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
            code.with_stream_no_messages(parse_subprogram),
            Declaration::SubprogramDeclaration(specification)
        );
    }
}
