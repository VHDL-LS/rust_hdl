// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::common::check_end_identifier_mismatch;
use super::common::ParseResult;
use super::interface_declaration::{parse_generic_interface_list, parse_port_interface_list};
use super::tokens::{Kind::*, TokenStream};
use crate::ast::WithDecl;
use crate::ast::{ComponentDeclaration, InterfaceDeclaration};
use crate::data::{Diagnostic, DiagnosticHandler};

pub fn parse_optional_generic_list(
    stream: &TokenStream,
    diagnostics: &mut dyn DiagnosticHandler,
) -> ParseResult<Option<Vec<InterfaceDeclaration>>> {
    let mut list = None;
    loop {
        let token = stream.peek_expect()?;
        match token.kind {
            Generic => {
                stream.skip();
                let new_list = parse_generic_interface_list(stream, diagnostics)?;
                stream.expect_kind(SemiColon)?;
                if list.is_some() {
                    diagnostics.push(Diagnostic::error(token, "Duplicate generic clause"));
                } else {
                    list = Some(new_list);
                }
            }
            _ => break,
        }
    }

    Ok(list)
}

pub fn parse_optional_port_list(
    stream: &TokenStream,
    diagnostics: &mut dyn DiagnosticHandler,
) -> ParseResult<Option<Vec<InterfaceDeclaration>>> {
    let mut list = None;
    loop {
        let token = stream.peek_expect()?;
        match token.kind {
            Port => {
                stream.skip();
                let new_list = parse_port_interface_list(stream, diagnostics)?;
                stream.expect_kind(SemiColon)?;
                if list.is_some() {
                    diagnostics.push(Diagnostic::error(token, "Duplicate port clause"));
                } else {
                    list = Some(new_list);
                }
            }
            Generic => {
                stream.skip();
                parse_generic_interface_list(stream, diagnostics)?;
                stream.expect_kind(SemiColon)?;
                diagnostics.push(Diagnostic::error(
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
    stream: &TokenStream,
    diagnostics: &mut dyn DiagnosticHandler,
) -> ParseResult<ComponentDeclaration> {
    stream.expect_kind(Component)?;
    let ident = WithDecl::new(stream.expect_ident()?);
    stream.pop_if_kind(Is);

    let generic_list = parse_optional_generic_list(stream, diagnostics)?;
    let port_list = parse_optional_port_list(stream, diagnostics)?;
    stream.expect_kind(End)?;
    stream.expect_kind(Component)?;
    let end_ident = stream.pop_optional_ident();
    stream.expect_kind(SemiColon)?;

    Ok(ComponentDeclaration {
        end_ident_pos: check_end_identifier_mismatch(&ident.tree, end_ident, diagnostics),
        ident,
        generic_list: generic_list.unwrap_or_default(),
        port_list: port_list.unwrap_or_default(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::ast::Ident;
    use crate::syntax::test::Code;
    use crate::SrcPos;

    fn to_component(
        ident: WithDecl<Ident>,
        generic_list: Vec<InterfaceDeclaration>,
        port_list: Vec<InterfaceDeclaration>,
        end_ident_pos: Option<SrcPos>,
    ) -> ComponentDeclaration {
        ComponentDeclaration {
            ident,
            generic_list,
            port_list,
            end_ident_pos,
        }
    }

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
            to_component(code.s1("foo").decl_ident(), vec![], vec![], None)
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
            to_component(code.s1("foo").decl_ident(), vec![], vec![], None)
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
            to_component(
                code.s1("foo").decl_ident(),
                vec![],
                vec![],
                Some(code.s("foo", 2).pos())
            )
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
            to_component(
                code.s1("foo").decl_ident(),
                vec![code.s1("foo : natural").generic()],
                vec![],
                None
            )
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
            to_component(
                code.s1("foo").decl_ident(),
                vec![],
                vec![code.s1("foo : natural").port()],
                None
            )
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
            vec![Diagnostic::error(
                &code.s("generic", 2).pos(),
                "Duplicate generic clause"
            )]
        );
        assert_eq!(result, Ok(Some(vec![code.s1("foo : natural").generic()])),);
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
            vec![Diagnostic::error(
                code.s("port", 2),
                "Duplicate port clause"
            )]
        );
        assert_eq!(result, Ok(Some(vec![code.s1("foo : natural").port()])),);
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
            vec![Diagnostic::error(
                code.s1("generic"),
                "Generic clause must come before port clause"
            )]
        );
        assert_eq!(result, Ok(Some(vec![code.s1("foo : natural").port()])),);
    }
}
