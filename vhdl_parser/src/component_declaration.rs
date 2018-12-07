// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use crate::ast::{ComponentDeclaration, InterfaceDeclaration};
use crate::common::error_on_end_identifier_mismatch;
use crate::interface_declaration::{parse_generic_interface_list, parse_port_interface_list};
use crate::message::{push_some, Message, MessageHandler, ParseResult};
use crate::tokenizer::Kind::*;
use crate::tokenstream::TokenStream;

pub fn parse_optional_generic_list(
    stream: &mut TokenStream,
    messages: &mut MessageHandler,
) -> ParseResult<Option<Vec<InterfaceDeclaration>>> {
    let mut list = None;
    loop {
        let token = stream.peek_expect()?;
        match token.kind {
            Generic => {
                stream.move_after(&token);
                let new_list = parse_generic_interface_list(stream, messages)?;
                stream.expect_kind(SemiColon)?;
                if list.is_some() {
                    messages.push(Message::error(token, "Duplicate generic clause"));
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
    stream: &mut TokenStream,
    messages: &mut MessageHandler,
) -> ParseResult<Option<Vec<InterfaceDeclaration>>> {
    let mut list = None;
    loop {
        let token = stream.peek_expect()?;
        match token.kind {
            Port => {
                stream.move_after(&token);
                let new_list = parse_port_interface_list(stream, messages)?;
                stream.expect_kind(SemiColon)?;
                if list.is_some() {
                    messages.push(Message::error(token, "Duplicate port clause"));
                } else {
                    list = Some(new_list);
                }
            }
            Generic => {
                stream.move_after(&token);
                parse_generic_interface_list(stream, messages)?;
                stream.expect_kind(SemiColon)?;
                messages.push(Message::error(
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
    stream: &mut TokenStream,
    messages: &mut MessageHandler,
) -> ParseResult<ComponentDeclaration> {
    stream.expect_kind(Component)?;
    let ident = stream.expect_ident()?;
    stream.pop_if_kind(Is)?;

    let generic_list = parse_optional_generic_list(stream, messages)?;
    let port_list = parse_optional_port_list(stream, messages)?;
    stream.expect_kind(End)?;
    stream.expect_kind(Component)?;
    if let Some(token) = stream.pop_if_kind(Identifier)? {
        push_some(
            messages,
            error_on_end_identifier_mismatch(&ident, &Some(token.expect_ident()?)),
        );
    }
    stream.expect_kind(SemiColon)?;

    Ok(ComponentDeclaration {
        ident,
        generic_list: generic_list.unwrap_or_default(),
        port_list: port_list.unwrap_or_default(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::ast::Ident;
    use crate::test_util::Code;

    fn to_component(
        ident: Ident,
        generic_list: Vec<InterfaceDeclaration>,
        port_list: Vec<InterfaceDeclaration>,
    ) -> ComponentDeclaration {
        ComponentDeclaration {
            ident,
            generic_list,
            port_list,
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
        let component = code.with_stream_no_messages(parse_component_declaration);
        assert_eq!(
            component,
            to_component(code.s1("foo").ident(), vec![], vec![])
        );

        let code = Code::new(
            "\
component foo is
end component;
",
        );
        let component = code.with_stream_no_messages(parse_component_declaration);
        assert_eq!(
            component,
            to_component(code.s1("foo").ident(), vec![], vec![])
        );

        let code = Code::new(
            "\
component foo is
end component foo;
",
        );
        let component = code.with_stream_no_messages(parse_component_declaration);
        assert_eq!(
            component,
            to_component(code.s1("foo").ident(), vec![], vec![])
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
        let component = code.with_stream_no_messages(parse_component_declaration);
        assert_eq!(
            component,
            to_component(
                code.s1("foo").ident(),
                vec![code.s1("foo : natural").generic()],
                vec![]
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
        let component = code.with_stream_no_messages(parse_component_declaration);
        assert_eq!(
            component,
            to_component(
                code.s1("foo").ident(),
                vec![],
                vec![code.s1("foo : natural").port()]
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

        let (result, messages) = code.with_partial_stream_messages(parse_optional_generic_list);
        assert_eq!(
            messages,
            vec![Message::error(
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
        let (result, messages) = code.with_partial_stream_messages(parse_optional_port_list);
        assert_eq!(
            messages,
            vec![Message::error(code.s("port", 2), "Duplicate port clause")]
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
        let (result, messages) = code.with_partial_stream_messages(parse_optional_port_list);
        assert_eq!(
            messages,
            vec![Message::error(
                code.s1("generic"),
                "Generic clause must come before port clause"
            )]
        );
        assert_eq!(result, Ok(Some(vec![code.s1("foo : natural").port()])),);
    }
}
