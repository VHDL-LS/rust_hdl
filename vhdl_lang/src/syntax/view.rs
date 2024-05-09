// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2024, Olof Kraigher olof.kraigher@gmail.com

use crate::ast::{ElementMode, ModeViewDeclaration, ModeViewElement, WithDecl};
use crate::syntax::common::ParseResult;
use crate::syntax::interface_declaration::parse_optional_mode;
use crate::syntax::names::parse_name;
use crate::syntax::parser::ParsingContext;
use crate::syntax::separated_list::parse_ident_list;
use crate::syntax::subtype_indication::parse_subtype_indication;
use crate::syntax::Kind::*;
use vhdl_lang::syntax::common::check_end_identifier_mismatch;
use vhdl_lang::TokenSpan;

// mode_view_declaration ::=
//   `view` identifier `of` unresolved_record_subtype_indication `is`
//     { mode_view_element_definition }
//   `end` `view` [ mode_view_simple_name ] `;`
pub(crate) fn parse_mode_view_declaration(
    ctx: &mut ParsingContext<'_>,
) -> ParseResult<ModeViewDeclaration> {
    let start_tok = ctx.stream.expect_kind(View)?;
    let ident = WithDecl::new(ctx.stream.expect_ident()?);
    ctx.stream.expect_kind(Of)?;
    let typ = parse_subtype_indication(ctx)?;
    ctx.stream.expect_kind(Is)?;
    let mut elements = Vec::new();
    while ctx.stream.peek_kind() != Some(End) {
        elements.push(parse_mode_view_element_definition(ctx)?);
    }
    ctx.stream.expect_kind(End)?;
    ctx.stream.expect_kind(View)?;
    let end_ident_pos =
        check_end_identifier_mismatch(ctx, &ident.tree, ctx.stream.pop_optional_ident());
    let end_tok = ctx.stream.expect_kind(SemiColon)?;
    Ok(ModeViewDeclaration {
        span: TokenSpan::new(start_tok, end_tok),
        ident,
        typ,
        elements,
        end_ident_pos,
    })
}

// mode_view_element_definition ::= record_element_list : element_mode_indication ;
pub(crate) fn parse_mode_view_element_definition(
    ctx: &mut ParsingContext<'_>,
) -> ParseResult<ModeViewElement> {
    let start = ctx.stream.get_current_token_id();
    let element_list = parse_ident_list(ctx)?;
    ctx.stream.expect_kind(Colon)?;
    let mode = parse_element_mode_indication(ctx)?;
    let end_token = ctx.stream.expect_kind(SemiColon)?;
    Ok(ModeViewElement {
        span: TokenSpan::new(start, end_token),
        mode,
        names: element_list,
    })
}

// element_mode_indication ::= mode | element_mode_view_indication
//
// element_mode_view_indication ::=
//     element_record_mode_view_indication
//     | element_array_mode_view_indication
//
// element_record_mode_view_indication ::= view mode_view_name
//
// element_array_mode_view_indication ::= view ( mode_view_name )
pub(crate) fn parse_element_mode_indication(
    ctx: &mut ParsingContext<'_>,
) -> ParseResult<ElementMode> {
    if let Some(mode) = parse_optional_mode(ctx)? {
        return Ok(ElementMode::Simple(mode));
    }
    ctx.stream.expect_kind(View)?;
    if ctx.stream.skip_if_kind(LeftPar) {
        let name = parse_name(ctx)?;
        ctx.stream.expect_kind(RightPar)?;
        Ok(ElementMode::Array(name))
    } else {
        let name = parse_name(ctx)?;
        Ok(ElementMode::Record(name))
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::token_range::WithToken;
    use crate::ast::{Mode, ModeViewDeclaration, ModeViewElement};
    use crate::data::ErrorCode;
    use crate::syntax::test::{check_diagnostics, Code};
    use crate::syntax::view::{
        parse_element_mode_indication, parse_mode_view_declaration,
        parse_mode_view_element_definition,
    };
    use crate::Diagnostic;
    use crate::VHDLStandard::VHDL2019;
    use vhdl_lang::ast::ElementMode;

    #[test]
    fn element_mode_indication() {
        let code = Code::with_standard("in", VHDL2019);
        assert_eq!(
            code.parse_ok_no_diagnostics(parse_element_mode_indication),
            ElementMode::Simple(WithToken::new(Mode::In, code.s1("in").token()))
        );
        let code = Code::with_standard("out", VHDL2019);
        assert_eq!(
            code.parse_ok_no_diagnostics(parse_element_mode_indication),
            ElementMode::Simple(WithToken::new(Mode::Out, code.s1("out").token()))
        );
        let code = Code::with_standard("inout", VHDL2019);
        assert_eq!(
            code.parse_ok_no_diagnostics(parse_element_mode_indication),
            ElementMode::Simple(WithToken::new(Mode::InOut, code.s1("inout").token()))
        );
        let code = Code::with_standard("buffer", VHDL2019);
        assert_eq!(
            code.parse_ok_no_diagnostics(parse_element_mode_indication),
            ElementMode::Simple(WithToken::new(Mode::Buffer, code.s1("buffer").token()))
        );
        let code = Code::with_standard("linkage", VHDL2019);
        assert_eq!(
            code.parse_ok_no_diagnostics(parse_element_mode_indication),
            ElementMode::Simple(WithToken::new(Mode::Linkage, code.s1("linkage").token()))
        );
    }

    #[test]
    fn record_mode_view_declaration() {
        let code = Code::with_standard("view foo.bar", VHDL2019);
        assert_eq!(
            code.parse_ok_no_diagnostics(parse_element_mode_indication),
            ElementMode::Record(code.s1("foo.bar").name())
        );
    }

    #[test]
    fn array_mode_view_declaration() {
        let code = Code::with_standard("view (foo.bar)", VHDL2019);
        assert_eq!(
            code.parse_ok_no_diagnostics(parse_element_mode_indication),
            ElementMode::Array(code.s1("foo.bar").name())
        );
    }

    #[test]
    fn missing_closing_parenthesis() {
        let code = Code::with_standard("view (foo.bar", VHDL2019);
        let (res, _) = code.parse(parse_element_mode_indication);
        assert_eq!(
            res,
            Err(Diagnostic::new(
                code.eof_pos(),
                "Unexpected EOF, when expecting ')'",
                ErrorCode::SyntaxError
            ))
        );
    }

    #[test]
    fn simple_mode_view_element_definition() {
        let code = Code::with_standard("foo: in ;", VHDL2019);
        assert_eq!(
            code.parse_ok_no_diagnostics(parse_mode_view_element_definition),
            ModeViewElement {
                names: code.s1("foo").ident_list(),
                mode: code.s1("in").element_mode(),
                span: code.token_span(),
            }
        );

        let code = Code::with_standard("foo, bar, baz: linkage;", VHDL2019);
        assert_eq!(
            code.parse_ok_no_diagnostics(parse_mode_view_element_definition),
            ModeViewElement {
                names: code.s1("foo, bar, baz").ident_list(),
                mode: code.s1("linkage").element_mode(),
                span: code.token_span(),
            }
        );

        let code = Code::with_standard("foo: view bar;", VHDL2019);
        assert_eq!(
            code.parse_ok_no_diagnostics(parse_mode_view_element_definition),
            ModeViewElement {
                names: code.s1("foo").ident_list(),
                mode: code.s1("view bar").element_mode(),
                span: code.token_span(),
            }
        );

        let code = Code::with_standard("foo: view (bar_array);", VHDL2019);
        assert_eq!(
            code.parse_ok_no_diagnostics(parse_mode_view_element_definition),
            ModeViewElement {
                names: code.s1("foo").ident_list(),
                mode: code.s1("view (bar_array)").element_mode(),
                span: code.token_span(),
            }
        );
    }

    #[test]
    fn empty_view() {
        let code = Code::with_standard(
            "\
view foo of bar is
end view;
    ",
            VHDL2019,
        );
        assert_eq!(
            code.parse_ok_no_diagnostics(parse_mode_view_declaration),
            ModeViewDeclaration {
                span: code.token_span(),
                ident: code.s1("foo").decl_ident(),
                typ: code.s1("bar").subtype_indication(),
                elements: Vec::new(),
                end_ident_pos: None,
            }
        );

        let code = Code::with_standard(
            "\
view foo of bar is
end view foo;
    ",
            VHDL2019,
        );
        assert_eq!(
            code.parse_ok_no_diagnostics(parse_mode_view_declaration),
            ModeViewDeclaration {
                span: code.token_span(),
                ident: code.s1("foo").decl_ident(),
                typ: code.s1("bar").subtype_indication(),
                elements: Vec::new(),
                end_ident_pos: Some(code.s("foo", 2).token()),
            }
        );

        let code = Code::with_standard(
            "\
view foo of bar is
end view baz;
    ",
            VHDL2019,
        );
        let (res, diagnostics) = code.with_stream_diagnostics(parse_mode_view_declaration);
        assert_eq!(
            res,
            ModeViewDeclaration {
                span: code.token_span(),
                ident: code.s1("foo").decl_ident(),
                typ: code.s1("bar").subtype_indication(),
                elements: Vec::new(),
                end_ident_pos: None
            }
        );
        check_diagnostics(
            diagnostics,
            vec![Diagnostic::new(
                code.s1("baz"),
                "End identifier mismatch, expected foo",
                ErrorCode::SyntaxError,
            )],
        )
    }

    #[test]
    fn views_parse_correctly() {
        let code = Code::with_standard(
            "\
view foo of bar is
    baz: in;
    foo: view some_view;
end view;
    ",
            VHDL2019,
        );
        assert_eq!(
            code.parse_ok_no_diagnostics(parse_mode_view_declaration),
            ModeViewDeclaration {
                span: code.token_span(),
                typ: code.s1("bar").subtype_indication(),
                ident: code.s1("foo").decl_ident(),
                elements: vec![
                    code.s1("baz: in;").mode_view_element(),
                    code.s1("foo: view some_view;").mode_view_element(),
                ],
                end_ident_pos: None,
            }
        )
    }
}
