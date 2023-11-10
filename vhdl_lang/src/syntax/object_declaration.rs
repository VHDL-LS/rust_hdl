// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::common::ParseResult;
use super::expression::parse_expression;
use super::names::parse_identifier_list;
use super::subtype_indication::parse_subtype_indication;
use super::tokens::{Kind::*, TokenSpan, TokenStream};
/// LRM 6.4.2 Object Declarations
use crate::ast::*;
use crate::data::WithPos;
use crate::Diagnostic;

pub fn parse_optional_assignment(stream: &TokenStream) -> ParseResult<Option<WithPos<Expression>>> {
    if stream.pop_if_kind(ColonEq).is_some() {
        let expr = parse_expression(stream)?;
        Ok(Some(expr))
    } else {
        Ok(None)
    }
}

fn parse_object_declaration_kind(
    stream: &TokenStream,
    class: ObjectClass,
) -> ParseResult<Vec<ObjectDeclaration>> {
    let start_token = stream.get_current_token_id();
    match class {
        ObjectClass::Signal => {
            stream.expect_kind(Signal)?;
        }
        ObjectClass::Constant => {
            stream.expect_kind(Constant)?;
        }
        ObjectClass::Variable => {
            stream.expect_kind(Variable)?;
        }
        ObjectClass::SharedVariable => {
            stream.expect_kind(Shared)?;
            stream.expect_kind(Variable)?;
        }
    }

    let idents = parse_identifier_list(stream)?;
    stream.expect_kind(Colon)?;
    let subtype = parse_subtype_indication(stream)?;
    let opt_expression = parse_optional_assignment(stream)?;
    let end_token = stream.expect_kind(SemiColon)?;

    Ok(idents
        .into_iter()
        .map(|ident| ObjectDeclaration {
            span: TokenSpan::new(start_token, end_token),
            class,
            ident: ident.into(),
            subtype_indication: subtype.clone(),
            expression: opt_expression.clone(),
        })
        .collect())
}

pub fn parse_object_declaration(stream: &TokenStream) -> ParseResult<Vec<ObjectDeclaration>> {
    let token = stream.peek_expect()?;
    let result = try_init_token_kind!(
        token,
        Constant => parse_object_declaration_kind(stream, ObjectClass::Constant)?,
        Signal => parse_object_declaration_kind(stream, ObjectClass::Signal)?,
        Variable => parse_object_declaration_kind(stream, ObjectClass::Variable)?,
        Shared => {
            parse_object_declaration_kind(stream, ObjectClass::SharedVariable)?
        }
    );
    Ok(result)
}

pub fn parse_file_declaration(stream: &TokenStream) -> ParseResult<Vec<FileDeclaration>> {
    let start_token = stream.expect_kind(File)?;
    let idents = parse_identifier_list(stream)?;
    stream.expect_kind(Colon)?;
    let subtype = parse_subtype_indication(stream)?;

    let open_info = {
        if stream.skip_if_kind(Open) {
            Some(parse_expression(stream)?)
        } else {
            None
        }
    };

    let file_name = {
        if stream.skip_if_kind(Is) {
            Some(parse_expression(stream)?)
        } else {
            None
        }
    };
    let end_token = stream.expect_kind(SemiColon)?;

    // If the `file_open_information` is present, `file_name` is mandatory
    // LRM 6.4.2.5
    if open_info.is_some() && file_name.is_none() {
        if let Some(ident) = idents.first() {
            return Err(Diagnostic::error(
                ident,
                "file_declaration must have a file name specified if the file open expression is specified as well",
            ));
        }
    }

    Ok(idents
        .into_iter()
        .map(|ident| FileDeclaration {
            span: TokenSpan::new(start_token, end_token),
            ident: ident.into(),
            subtype_indication: subtype.clone(),
            open_info: open_info.clone(),
            file_name: file_name.clone(),
        })
        .collect())
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use super::*;
    use crate::syntax::test::{token_to_string, Code};
    use crate::HasTokenSpan;

    #[test]
    fn parses_constant() {
        let code = Code::new("constant foo : natural;");
        assert_eq!(
            code.with_stream(parse_object_declaration),
            vec![ObjectDeclaration {
                span: code.token_span(),
                class: ObjectClass::Constant,
                ident: code.s1("foo").decl_ident(),
                subtype_indication: code.s1("natural").subtype_indication(),
                expression: None
            }]
        );
    }

    #[test]
    fn parses_signal() {
        let code = Code::new("signal foo : natural;");
        assert_eq!(
            code.with_stream(parse_object_declaration),
            vec![ObjectDeclaration {
                span: code.token_span(),
                class: ObjectClass::Signal,
                ident: code.s1("foo").decl_ident(),
                subtype_indication: code.s1("natural").subtype_indication(),
                expression: None
            }]
        );
    }

    #[test]
    fn parses_variable() {
        let code = Code::new("variable foo : natural;");
        assert_eq!(
            code.with_stream(parse_object_declaration),
            vec![ObjectDeclaration {
                span: code.token_span(),
                class: ObjectClass::Variable,
                ident: code.s1("foo").decl_ident(),
                subtype_indication: code.s1("natural").subtype_indication(),
                expression: None
            }]
        );
    }

    #[test]
    fn parses_shared_variable() {
        let code = Code::new("shared variable foo : natural;");
        assert_eq!(
            code.with_stream(parse_object_declaration),
            vec![ObjectDeclaration {
                span: code.token_span(),
                class: ObjectClass::SharedVariable,
                ident: code.s1("foo").decl_ident(),
                subtype_indication: code.s1("natural").subtype_indication(),
                expression: None
            }]
        );
    }

    #[test]
    fn parses_file() {
        let code = Code::new("file foo : text;");
        assert_eq!(
            code.with_stream(parse_file_declaration),
            vec![FileDeclaration {
                span: code.token_span(),
                ident: code.s1("foo").decl_ident(),
                subtype_indication: code.s1("text").subtype_indication(),
                open_info: None,
                file_name: None
            }]
        );
    }

    #[test]
    fn parses_file_with_file_name() {
        let code = Code::new("file foo : text is \"file_name\";");
        assert_eq!(
            code.with_stream(parse_file_declaration),
            vec![FileDeclaration {
                span: code.token_span(),
                ident: code.s1("foo").decl_ident(),
                subtype_indication: code.s1("text").subtype_indication(),
                open_info: None,
                file_name: Some(code.s1("\"file_name\"").expr())
            }]
        );
    }

    #[test]
    fn parses_file_with_open_information() {
        let code = Code::new("file foo : text open write_mode is \"file_name\";");
        assert_eq!(
            code.with_stream(parse_file_declaration),
            vec![FileDeclaration {
                span: code.token_span(),
                ident: code.s1("foo").decl_ident(),
                subtype_indication: code.s1("text").subtype_indication(),
                open_info: Some(code.s1("write_mode").expr()),
                file_name: Some(code.s1("\"file_name\"").expr())
            }]
        );
    }

    #[test]
    fn parses_file_with_open_information_without_file_name() {
        let code = Code::new("file foo : text open write_mode;");
        assert_eq!(
            code.with_stream_err(parse_file_declaration),
            Diagnostic::error(
                code.s1("foo"),
                "file_declaration must have a file name specified if the file open expression is specified as well",
            )
        );
    }

    #[test]
    fn parses_optional_expression() {
        let code = Code::new("constant foo : natural := 0;");
        assert_eq!(
            code.with_stream(parse_object_declaration),
            vec![ObjectDeclaration {
                span: code.token_span(),
                class: ObjectClass::Constant,
                ident: code.s1("foo").decl_ident(),
                subtype_indication: code.s1("natural").subtype_indication(),
                expression: Some(code.s1("0").expr())
            }]
        );
    }

    #[test]
    fn parses_identifier_list() {
        let code = Code::new("constant foo, bar : natural := 0;");

        let objects = vec![
            ObjectDeclaration {
                span: code.token_span(),
                class: ObjectClass::Constant,
                ident: code.s1("foo").decl_ident(),
                subtype_indication: code.s1("natural").subtype_indication(),
                expression: Some(code.s1("0").expr()),
            },
            ObjectDeclaration {
                span: code.token_span(),
                class: ObjectClass::Constant,
                ident: code.s1("bar").decl_ident(),
                subtype_indication: code.s1("natural").subtype_indication(),
                expression: Some(code.s1("0").expr()),
            },
        ];

        assert_eq!(code.with_stream(parse_object_declaration), objects);
    }

    #[test]
    pub fn test_token_span() {
        let code = Code::new(
            "\
architecture pkg of ent is
    constant PI         : real := 3.141;
    variable bar        : bit;
    shared variable foo : integer;
    signal busy         : std_ulogic := '0';
begin
end architecture;
",
        );
        let ctx = code.tokenize();
        let arch = code.architecture_body();
        let obj_decls = arch
            .decl
            .iter()
            .map(|d| {
                if let Declaration::Object(obj) = d {
                    obj
                } else {
                    panic!("Only object declarations are expected!")
                }
            })
            .collect_vec();

        let obj_decl_strings: Vec<Vec<String>> = obj_decls
            .iter()
            .map(|decl| {
                decl.get_token_slice(&ctx)
                    .iter()
                    .map(token_to_string)
                    .collect()
            })
            .collect_vec();

        assert_eq!(
            obj_decl_strings[0],
            vec!["constant", "PI", ":", "real", ":=", "3.141", ";"],
        );
        assert_eq!(
            obj_decl_strings[1],
            vec!["variable", "bar", ":", "bit", ";"],
        );
        assert_eq!(
            obj_decl_strings[2],
            vec!["shared", "variable", "foo", ":", "integer", ";"],
        );
        assert_eq!(
            obj_decl_strings[3],
            vec!["signal", "busy", ":", "std_ulogic", ":=", "'0'", ";"],
        );
    }
}
