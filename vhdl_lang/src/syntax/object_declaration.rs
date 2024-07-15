use itertools::Itertools;
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com
/// LRM 6.4.2 Object Declarations
use super::common::ParseResult;
use super::expression::parse_expression;
use super::names::parse_identifier_list;
use super::subtype_indication::parse_subtype_indication;
use super::tokens::{Kind::*, TokenSpan};
use crate::ast::token_range::WithTokenSpan;
use crate::ast::*;
use crate::syntax::recover::expect_semicolon_or_last;
use crate::Diagnostic;
use vhdl_lang::syntax::parser::ParsingContext;

pub fn parse_optional_assignment(
    ctx: &mut ParsingContext<'_>,
) -> ParseResult<Option<WithTokenSpan<Expression>>> {
    if ctx.stream.pop_if_kind(ColonEq).is_some() {
        let expr = parse_expression(ctx)?;
        Ok(Some(expr))
    } else {
        Ok(None)
    }
}

fn parse_object_declaration_kind(
    ctx: &mut ParsingContext<'_>,
    class: ObjectClass,
) -> ParseResult<WithTokenSpan<ObjectDeclaration>> {
    let start_token = ctx.stream.get_current_token_id();
    match class {
        ObjectClass::Signal => {
            ctx.stream.expect_kind(Signal)?;
        }
        ObjectClass::Constant => {
            ctx.stream.expect_kind(Constant)?;
        }
        ObjectClass::Variable => {
            ctx.stream.expect_kind(Variable)?;
        }
        ObjectClass::SharedVariable => {
            ctx.stream.expect_kind(Shared)?;
            ctx.stream.expect_kind(Variable)?;
        }
    }

    let idents = parse_identifier_list(ctx)?
        .into_iter()
        .map(WithDecl::new)
        .collect_vec();
    ctx.stream.expect_kind(Colon)?;
    let subtype = parse_subtype_indication(ctx)?;
    let opt_expression = parse_optional_assignment(ctx)?;
    let end_token = expect_semicolon_or_last(ctx);
    Ok(WithTokenSpan::new(
        ObjectDeclaration {
            class,
            idents,
            subtype_indication: subtype.clone(),
            expression: opt_expression.clone(),
        },
        TokenSpan::new(start_token, end_token),
    ))
}

pub fn parse_object_declaration(
    ctx: &mut ParsingContext<'_>,
) -> ParseResult<WithTokenSpan<ObjectDeclaration>> {
    let token = ctx.stream.peek_expect()?;
    let result = try_init_token_kind!(
        token,
        Constant => parse_object_declaration_kind(ctx, ObjectClass::Constant)?,
        Signal => parse_object_declaration_kind(ctx, ObjectClass::Signal)?,
        Variable => parse_object_declaration_kind(ctx, ObjectClass::Variable)?,
        Shared => {
            parse_object_declaration_kind(ctx, ObjectClass::SharedVariable)?
        }
    );
    Ok(result)
}

pub fn parse_file_declaration(
    ctx: &mut ParsingContext<'_>,
) -> ParseResult<Vec<WithTokenSpan<FileDeclaration>>> {
    let start_token = ctx.stream.expect_kind(File)?;
    let idents = parse_identifier_list(ctx)?;
    ctx.stream.expect_kind(Colon)?;
    let subtype = parse_subtype_indication(ctx)?;

    let open_info = {
        if ctx.stream.skip_if_kind(Open) {
            Some(parse_expression(ctx)?)
        } else {
            None
        }
    };

    let file_name = {
        if ctx.stream.skip_if_kind(Is) {
            Some(parse_expression(ctx)?)
        } else {
            None
        }
    };
    let end_token = expect_semicolon_or_last(ctx);

    // If the `file_open_information` is present, `file_name` is mandatory
    // LRM 6.4.2.5
    if open_info.is_some() && file_name.is_none() {
        if let Some(ident) = idents.first() {
            return Err(Diagnostic::syntax_error(
                ident.pos(ctx),
                "file_declaration must have a file name specified if the file open expression is specified as well",
            ));
        }
    }

    Ok(idents
        .into_iter()
        .map(|ident| {
            WithTokenSpan::new(
                FileDeclaration {
                    ident: ident.into(),
                    subtype_indication: subtype.clone(),
                    open_info: open_info.clone(),
                    file_name: file_name.clone(),
                },
                TokenSpan::new(start_token, end_token),
            )
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
            WithTokenSpan::new(
                ObjectDeclaration {
                    class: ObjectClass::Constant,
                    idents: vec![code.s1("foo").decl_ident()],
                    subtype_indication: code.s1("natural").subtype_indication(),
                    expression: None
                },
                code.token_span()
            )
        );
    }

    #[test]
    fn parses_signal() {
        let code = Code::new("signal foo : natural;");
        assert_eq!(
            code.with_stream(parse_object_declaration),
            WithTokenSpan::new(
                ObjectDeclaration {
                    class: ObjectClass::Signal,
                    idents: vec![code.s1("foo").decl_ident()],
                    subtype_indication: code.s1("natural").subtype_indication(),
                    expression: None
                },
                code.token_span()
            )
        );
    }

    #[test]
    fn parses_variable() {
        let code = Code::new("variable foo : natural;");
        assert_eq!(
            code.with_stream(parse_object_declaration),
            WithTokenSpan::new(
                ObjectDeclaration {
                    class: ObjectClass::Variable,
                    idents: vec![code.s1("foo").decl_ident()],
                    subtype_indication: code.s1("natural").subtype_indication(),
                    expression: None
                },
                code.token_span()
            )
        );
    }

    #[test]
    fn parses_shared_variable() {
        let code = Code::new("shared variable foo : natural;");
        assert_eq!(
            code.with_stream(parse_object_declaration),
            WithTokenSpan::new(
                ObjectDeclaration {
                    class: ObjectClass::SharedVariable,
                    idents: vec![code.s1("foo").decl_ident()],
                    subtype_indication: code.s1("natural").subtype_indication(),
                    expression: None
                },
                code.token_span()
            )
        );
    }

    #[test]
    fn parses_file() {
        let code = Code::new("file foo : text;");
        assert_eq!(
            code.with_stream(parse_file_declaration),
            vec![WithTokenSpan::new(
                FileDeclaration {
                    ident: code.s1("foo").decl_ident(),
                    subtype_indication: code.s1("text").subtype_indication(),
                    open_info: None,
                    file_name: None
                },
                code.token_span()
            )]
        );
    }

    #[test]
    fn parses_file_with_file_name() {
        let code = Code::new("file foo : text is \"file_name\";");
        assert_eq!(
            code.with_stream(parse_file_declaration),
            vec![WithTokenSpan::new(
                FileDeclaration {
                    ident: code.s1("foo").decl_ident(),
                    subtype_indication: code.s1("text").subtype_indication(),
                    open_info: None,
                    file_name: Some(code.s1("\"file_name\"").expr())
                },
                code.token_span()
            )]
        );
    }

    #[test]
    fn parses_file_with_open_information() {
        let code = Code::new("file foo : text open write_mode is \"file_name\";");
        assert_eq!(
            code.with_stream(parse_file_declaration),
            vec![WithTokenSpan::new(
                FileDeclaration {
                    ident: code.s1("foo").decl_ident(),
                    subtype_indication: code.s1("text").subtype_indication(),
                    open_info: Some(code.s1("write_mode").expr()),
                    file_name: Some(code.s1("\"file_name\"").expr())
                },
                code.token_span()
            )]
        );
    }

    #[test]
    fn parses_file_with_open_information_without_file_name() {
        let code = Code::new("file foo : text open write_mode;");
        assert_eq!(
            code.with_stream_err(parse_file_declaration),
            Diagnostic::syntax_error(
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
            WithTokenSpan::new(
                ObjectDeclaration {
                    class: ObjectClass::Constant,
                    idents: vec![code.s1("foo").decl_ident()],
                    subtype_indication: code.s1("natural").subtype_indication(),
                    expression: Some(code.s1("0").expr())
                },
                code.token_span()
            )
        );
    }

    #[test]
    fn parses_identifier_list() {
        let code = Code::new("constant foo, bar : natural := 0;");

        assert_eq!(
            code.with_stream(parse_object_declaration),
            WithTokenSpan::new(
                ObjectDeclaration {
                    class: ObjectClass::Constant,
                    idents: vec![code.s1("foo").decl_ident(), code.s1("bar").decl_ident()],
                    subtype_indication: code.s1("natural").subtype_indication(),
                    expression: Some(code.s1("0").expr()),
                },
                code.token_span(),
            )
        );
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
                if let Declaration::Object(obj) = &d.item {
                    WithTokenSpan::new(obj, d.span)
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
