// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

/// LRM 6.4.2 Object Declarations
use ast::{Expression, FileDeclaration, ObjectClass, ObjectDeclaration};
use expression::parse_expression;
use message::ParseResult;
use names::parse_identifier_list;
use source::WithPos;
use subtype_indication::parse_subtype_indication;
use tokenizer::Kind::*;
use tokenstream::TokenStream;

pub fn parse_optional_assignment(
    stream: &mut TokenStream,
) -> ParseResult<Option<WithPos<Expression>>> {
    if let Some(_) = stream.pop_if_kind(ColonEq)? {
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
            class: class,
            ident: ident,
            subtype_indication: subtype.clone(),
            expression: opt_expression.clone(),
        }).collect())
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
            ident: ident,
            subtype_indication: subtype.clone(),
            open_info: open_info.clone(),
            file_name: file_name.clone(),
        }).collect())
}

pub fn parse_file_declaration(stream: &mut TokenStream) -> ParseResult<Vec<FileDeclaration>> {
    let result = parse_file_declaration_no_semi(stream)?;
    stream.expect_kind(SemiColon)?;
    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_util::with_stream;

    #[test]
    fn parses_constant() {
        let (util, object) = with_stream(parse_object_declaration, "constant foo : natural;");
        assert_eq!(
            object,
            vec![ObjectDeclaration {
                class: ObjectClass::Constant,
                ident: util.ident("foo"),
                subtype_indication: util.subtype_indication("natural"),
                expression: None
            }]
        );
    }

    #[test]
    fn parses_signal() {
        let (util, object) = with_stream(parse_object_declaration, "signal foo : natural;");
        assert_eq!(
            object,
            vec![ObjectDeclaration {
                class: ObjectClass::Signal,
                ident: util.ident("foo"),
                subtype_indication: util.subtype_indication("natural"),
                expression: None
            }]
        );
    }

    #[test]
    fn parses_variable() {
        let (util, object) = with_stream(parse_object_declaration, "variable foo : natural;");
        assert_eq!(
            object,
            vec![ObjectDeclaration {
                class: ObjectClass::Variable,
                ident: util.ident("foo"),
                subtype_indication: util.subtype_indication("natural"),
                expression: None
            }]
        );
    }

    #[test]
    fn parses_shared_variable() {
        let (util, object) =
            with_stream(parse_object_declaration, "shared variable foo : natural;");
        assert_eq!(
            object,
            vec![ObjectDeclaration {
                class: ObjectClass::SharedVariable,
                ident: util.ident("foo"),
                subtype_indication: util.subtype_indication("natural"),
                expression: None
            }]
        );
    }

    #[test]
    fn parses_file() {
        let (util, object) = with_stream(parse_file_declaration, "file foo : text;");
        assert_eq!(
            object,
            vec![FileDeclaration {
                ident: util.ident("foo"),
                subtype_indication: util.subtype_indication("text"),
                open_info: None,
                file_name: None
            }]
        );
    }

    #[test]
    fn parses_file_with_file_name() {
        let (util, object) =
            with_stream(parse_file_declaration, "file foo : text is \"file_name\";");
        assert_eq!(
            object,
            vec![FileDeclaration {
                ident: util.ident("foo"),
                subtype_indication: util.subtype_indication("text"),
                open_info: None,
                file_name: Some(util.expr("\"file_name\""))
            }]
        );
    }

    #[test]
    fn parses_file_with_open_information() {
        let (util, object) = with_stream(
            parse_file_declaration,
            "file foo : text open write_mode is \"file_name\";",
        );
        assert_eq!(
            object,
            vec![FileDeclaration {
                ident: util.ident("foo"),
                subtype_indication: util.subtype_indication("text"),
                open_info: Some(util.expr("write_mode")),
                file_name: Some(util.expr("\"file_name\""))
            }]
        );
    }

    #[test]
    fn parses_optional_expression() {
        let (util, object) = with_stream(parse_object_declaration, "constant foo : natural := 0;");
        assert_eq!(
            object,
            vec![ObjectDeclaration {
                class: ObjectClass::Constant,
                ident: util.ident("foo"),
                subtype_indication: util.subtype_indication("natural"),
                expression: Some(util.expr("0"))
            }]
        );
    }

    #[test]
    fn parses_identifier_list() {
        let (util, object) = with_stream(
            parse_object_declaration,
            "constant foo, bar : natural := 0;",
        );

        let objects = vec![
            ObjectDeclaration {
                class: ObjectClass::Constant,
                ident: util.ident("foo"),
                subtype_indication: util.subtype_indication("natural"),
                expression: Some(util.expr("0")),
            },
            ObjectDeclaration {
                class: ObjectClass::Constant,
                ident: util.ident("bar"),
                subtype_indication: util.subtype_indication("natural"),
                expression: Some(util.expr("0")),
            },
        ];

        assert_eq!(object, objects);
    }
}
