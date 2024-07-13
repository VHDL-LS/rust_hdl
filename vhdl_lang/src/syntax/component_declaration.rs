// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::common::{check_end_identifier_mismatch, ParseResult};
use super::interface_declaration::{parse_generic_interface_list, parse_port_interface_list};
use super::tokens::{Kind::*, TokenSpan};
use crate::ast::token_range::WithTokenSpan;
use crate::ast::WithDecl;
use crate::ast::{ComponentDeclaration, InterfaceDeclaration};
use crate::data::Diagnostic;
use crate::syntax::recover::expect_semicolon_or_last;
use vhdl_lang::syntax::parser::ParsingContext;
use vhdl_lang::VHDLStandard::VHDL2019;

pub fn parse_optional_generic_list(
    ctx: &mut ParsingContext<'_>,
) -> ParseResult<Option<WithTokenSpan<Vec<InterfaceDeclaration>>>> {
    let mut list = None;
    loop {
        let token = ctx.stream.peek_expect()?;
        match token.kind {
            Generic => {
                let generic_token = ctx.stream.get_current_token_id();
                ctx.stream.skip();
                let new_list = parse_generic_interface_list(ctx)?;
                let semicolon = expect_semicolon_or_last(ctx);
                if list.is_some() {
                    ctx.diagnostics
                        .push(Diagnostic::syntax_error(token, "Duplicate generic clause"));
                } else {
                    list = Some(WithTokenSpan::new(
                        new_list,
                        TokenSpan::new(generic_token, semicolon),
                    ));
                }
            }
            _ => break,
        }
    }

    Ok(list)
}

pub fn parse_optional_port_list(
    ctx: &mut ParsingContext<'_>,
) -> ParseResult<Option<WithTokenSpan<Vec<InterfaceDeclaration>>>> {
    let mut list = None;
    loop {
        let token = ctx.stream.peek_expect()?;
        match token.kind {
            Port => {
                let port_token = ctx.stream.get_current_token_id();
                ctx.stream.skip();
                let new_list = parse_port_interface_list(ctx)?;
                let semicolon = expect_semicolon_or_last(ctx);
                if list.is_some() {
                    ctx.diagnostics
                        .push(Diagnostic::syntax_error(token, "Duplicate port clause"));
                } else {
                    list = Some(WithTokenSpan::new(
                        new_list,
                        TokenSpan::new(port_token, semicolon),
                    ));
                }
            }
            Generic => {
                ctx.stream.skip();
                parse_generic_interface_list(ctx)?;
                ctx.diagnostics.push(Diagnostic::syntax_error(
                    token,
                    "Generic clause must come before port clause",
                ));
            }
            _ => break,
        }
    }

    Ok(list)
}

pub fn parse_component_declaration(
    ctx: &mut ParsingContext<'_>,
) -> ParseResult<ComponentDeclaration> {
    let start_token = ctx.stream.expect_kind(Component)?;
    let ident = WithDecl::new(ctx.stream.expect_ident()?);
    let is_token = ctx.stream.pop_if_kind(Is);

    let generic_list = parse_optional_generic_list(ctx)?;
    let port_list = parse_optional_port_list(ctx)?;
    let end_token = ctx.stream.expect_kind(End)?;
    if ctx.standard < VHDL2019 {
        ctx.stream.expect_kind(Component)?;
    } else {
        ctx.stream.pop_if_kind(Component);
    }
    let end_ident = ctx.stream.pop_optional_ident();
    let semicolon_token = expect_semicolon_or_last(ctx);

    Ok(ComponentDeclaration {
        span: TokenSpan::new(start_token, semicolon_token),
        is_token,
        end_ident_pos: check_end_identifier_mismatch(ctx, &ident.tree, end_ident),
        ident,
        generic_list,
        port_list,
        end_token,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::syntax::test::Code;
    use crate::VHDLStandard::VHDL2019;

    #[test]
    fn test_component() {
        let code = Code::new(
            "\
component foo
end component;
",
        );
        let component = code.with_stream_no_diagnostics(parse_component_declaration);
        assert_eq!(
            component,
            ComponentDeclaration {
                span: code.token_span(),
                is_token: None,
                ident: code.s1("foo").decl_ident(),
                generic_list: None,
                port_list: None,
                end_ident_pos: None,
                end_token: code.s1("end").token(),
            }
        );

        let code = Code::new(
            "\
component foo is
end component;
",
        );
        let component = code.with_stream_no_diagnostics(parse_component_declaration);
        assert_eq!(
            component,
            ComponentDeclaration {
                span: code.token_span(),
                is_token: None,
                ident: code.s1("foo").decl_ident(),
                generic_list: None,
                port_list: None,
                end_ident_pos: None,
                end_token: code.s1("end").token(),
            }
        );

        let code = Code::new(
            "\
component foo is
end component foo;
",
        );
        let component = code.with_stream_no_diagnostics(parse_component_declaration);
        assert_eq!(
            component,
            ComponentDeclaration {
                span: code.token_span(),
                is_token: Some(code.s1("foo").token()),
                ident: code.s1("foo").decl_ident(),
                generic_list: None,
                port_list: None,
                end_ident_pos: Some(code.s("foo", 2).token()),
                end_token: code.s1("end").token(),
            }
        );
    }

    #[test]
    fn test_component_with_generic() {
        let code = Code::new(
            "\
component foo is
  generic (
    foo : natural
  );
end component;
",
        );
        let component = code.with_stream_no_diagnostics(parse_component_declaration);
        assert_eq!(
            component,
            ComponentDeclaration {
                span: code.token_span(),
                is_token: Some(code.s1("foo").token()),
                ident: code.s1("foo").decl_ident(),
                generic_list: Some(WithTokenSpan::new(
                    vec![code.s1("foo : natural").generic()],
                    code.between("generic", ");").token_span()
                )),
                port_list: None,
                end_ident_pos: None,
                end_token: code.s1("end").token(),
            }
        );
    }

    #[test]
    fn test_component_with_port() {
        let code = Code::new(
            "\
component foo is
  port (
    foo : natural
  );
end component;
",
        );
        let component = code.with_stream_no_diagnostics(parse_component_declaration);
        assert_eq!(
            component,
            ComponentDeclaration {
                span: code.token_span(),
                is_token: Some(code.s1("foo").token()),
                ident: code.s1("foo").decl_ident(),
                generic_list: None,
                port_list: Some(WithTokenSpan::new(
                    vec![code.s1("foo : natural").port()],
                    code.between("generic", ");").token_span()
                )),
                end_ident_pos: None,
                end_token: code.s1("end").token(),
            }
        );
    }

    #[test]
    fn error_on_duplicate_generic_clause() {
        let code = Code::new(
            "\
generic (
  foo : natural
);
generic (
  bar : natural
);
end
",
        );

        let (result, diagnostics) =
            code.with_partial_stream_diagnostics(parse_optional_generic_list);
        assert_eq!(
            diagnostics,
            vec![Diagnostic::syntax_error(
                &code.s("generic", 2).pos(),
                "Duplicate generic clause"
            )]
        );
        assert_eq!(
            result,
            Ok(Some(WithTokenSpan::new(
                vec![code.s1("foo : natural").generic()],
                code.between("generic", ");").token_span()
            ))),
        );
    }

    #[test]
    fn error_on_duplicate_port_clause() {
        let code = Code::new(
            "\
port (
  foo : natural
);
port (
  bar : natural
);
end
",
        );
        let (result, diagnostics) = code.with_partial_stream_diagnostics(parse_optional_port_list);
        assert_eq!(
            diagnostics,
            vec![Diagnostic::syntax_error(
                code.s("port", 2),
                "Duplicate port clause"
            )]
        );
        assert_eq!(
            result,
            Ok(Some(WithTokenSpan::new(
                vec![code.s1("foo : natural").port()],
                code.between("port", ";").token_span()
            ))),
        );
    }

    #[test]
    fn error_generic_after_port_clause() {
        let code = Code::new(
            "\
port (
  foo : natural
);
generic (
  bar : natural
);
end
",
        );
        let (result, diagnostics) = code.with_partial_stream_diagnostics(parse_optional_port_list);
        assert_eq!(
            diagnostics,
            vec![Diagnostic::syntax_error(
                code.s1("generic"),
                "Generic clause must come before port clause"
            )]
        );
        assert_eq!(
            result,
            Ok(Some(WithTokenSpan::new(
                vec![code.s1("foo : natural").port()],
                code.between("port", ";").token_span()
            )))
        );
    }

    #[test]
    pub fn component_vhdl2019() {
        Code::with_standard(
            "\
component foo is
end;
        ",
            VHDL2019,
        )
        .parse_ok_no_diagnostics(parse_component_declaration);

        Code::with_standard(
            "\
component foo is
end component;
        ",
            VHDL2019,
        )
        .parse_ok_no_diagnostics(parse_component_declaration);

        Code::with_standard(
            "\
component foo is
end component foo;
        ",
            VHDL2019,
        )
        .parse_ok_no_diagnostics(parse_component_declaration);
    }
}
