// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

/// LRM 6.4.2 Object Declarations
use crate::ast::{Expression, FileDeclaration, ObjectClass, ObjectDeclaration};
use crate::diagnostic::ParseResult;
use crate::expression::parse_expression;
use crate::names::parse_identifier_list;
use crate::source::WithPos;
use crate::subtype_indication::parse_subtype_indication;
use crate::tokenizer::Kind::*;
use crate::tokenstream::TokenStream;

pub fn parse_optional_assignment(
    stream: &mut TokenStream,
) -> ParseResult<Option<WithPos<Expression>>> {
    if stream.pop_if_kind(ColonEq)?.is_some() {
        let expr = parse_expression(stream)?;
        Ok(Some(expr))
    } else {
        Ok(None)
    }
}

fn parse_object_declaration_kind(
    stream: &mut TokenStream,
    class: ObjectClass,
) -> ParseResult<Vec<ObjectDeclaration>> {
    let idents = parse_identifier_list(stream)?;
    stream.expect_kind(Colon)?;
    let subtype = parse_subtype_indication(stream)?;
    let opt_expression = parse_optional_assignment(stream)?;

    Ok(idents
        .into_iter()
        .map(|ident| ObjectDeclaration {
            class,
            ident,
            subtype_indication: subtype.clone(),
            expression: opt_expression.clone(),
        })
        .collect())
}

pub fn parse_object_declaration(stream: &mut TokenStream) -> ParseResult<Vec<ObjectDeclaration>> {
    let token = stream.expect()?;
    let result = try_token_kind!(
        token,
        Constant => parse_object_declaration_kind(stream, ObjectClass::Constant)?,
        Signal => parse_object_declaration_kind(stream, ObjectClass::Signal)?,
        Variable => parse_object_declaration_kind(stream, ObjectClass::Variable)?,
        Shared => {
            stream.expect_kind(Variable)?;
            parse_object_declaration_kind(stream, ObjectClass::SharedVariable)?
        }
    );
    stream.expect_kind(SemiColon)?;
    Ok(result)
}

pub fn parse_file_declaration_no_semi(
    stream: &mut TokenStream,
) -> ParseResult<Vec<FileDeclaration>> {
    stream.expect_kind(File)?;
    let idents = parse_identifier_list(stream)?;
    stream.expect_kind(Colon)?;
    let subtype = parse_subtype_indication(stream)?;

    let open_info = {
        if stream.skip_if_kind(Open)? {
            Some(parse_expression(stream)?)
        } else {
            None
        }
    };

    let file_name = {
        if stream.skip_if_kind(Is)? {
            Some(parse_expression(stream)?)
        } else {
            None
        }
    };

    Ok(idents
        .into_iter()
        .map(|ident| FileDeclaration {
            ident,
            subtype_indication: subtype.clone(),
            open_info: open_info.clone(),
            file_name: file_name.clone(),
        })
        .collect())
}

pub fn parse_file_declaration(stream: &mut TokenStream) -> ParseResult<Vec<FileDeclaration>> {
    let result = parse_file_declaration_no_semi(stream)?;
    stream.expect_kind(SemiColon)?;
    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::Code;

    #[test]
    fn parses_constant() {
        let code = Code::new("constant foo : natural;");
        assert_eq!(
            code.with_stream(parse_object_declaration),
            vec![ObjectDeclaration {
                class: ObjectClass::Constant,
                ident: code.s1("foo").ident(),
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
                class: ObjectClass::Signal,
                ident: code.s1("foo").ident(),
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
                class: ObjectClass::Variable,
                ident: code.s1("foo").ident(),
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
                class: ObjectClass::SharedVariable,
                ident: code.s1("foo").ident(),
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
                ident: code.s1("foo").ident(),
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
                ident: code.s1("foo").ident(),
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
                ident: code.s1("foo").ident(),
                subtype_indication: code.s1("text").subtype_indication(),
                open_info: Some(code.s1("write_mode").expr()),
                file_name: Some(code.s1("\"file_name\"").expr())
            }]
        );
    }

    #[test]
    fn parses_optional_expression() {
        let code = Code::new("constant foo : natural := 0;");
        assert_eq!(
            code.with_stream(parse_object_declaration),
            vec![ObjectDeclaration {
                class: ObjectClass::Constant,
                ident: code.s1("foo").ident(),
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
                class: ObjectClass::Constant,
                ident: code.s1("foo").ident(),
                subtype_indication: code.s1("natural").subtype_indication(),
                expression: Some(code.s1("0").expr()),
            },
            ObjectDeclaration {
                class: ObjectClass::Constant,
                ident: code.s1("bar").ident(),
                subtype_indication: code.s1("natural").subtype_indication(),
                expression: Some(code.s1("0").expr()),
            },
        ];

        assert_eq!(code.with_stream(parse_object_declaration), objects);
    }
}
