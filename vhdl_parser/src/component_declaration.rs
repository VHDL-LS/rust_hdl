// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use ast::{ComponentDeclaration, InterfaceDeclaration};
use common::error_on_end_identifier_mismatch;
use interface_declaration::{parse_generic_interface_list, parse_port_interface_list};
use message::{error, push_some, MessageHandler, ParseResult};
use tokenizer::Kind::*;
use tokenstream::TokenStream;

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
                    messages.push(error(token, "Duplicate generic clause"));
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
                    messages.push(error(token, "Duplicate port clause"));
                } else {
                    list = Some(new_list);
                }
            }
            Generic => {
                stream.move_after(&token);
                parse_generic_interface_list(stream, messages)?;
                stream.expect_kind(SemiColon)?;
                messages.push(error(token, "Generic clause must come before port clause"));
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
        ident: ident,
        generic_list: generic_list.unwrap_or(Vec::new()),
        port_list: port_list.unwrap_or(Vec::new()),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    use ast::Ident;
    use message::error;
    use test_util::{with_partial_stream_messages, with_stream_no_messages};

    fn to_component(
        ident: Ident,
        generic_list: Vec<InterfaceDeclaration>,
        port_list: Vec<InterfaceDeclaration>,
    ) -> ComponentDeclaration {
        ComponentDeclaration {
            ident: ident,
            generic_list: generic_list,
            port_list: port_list,
        }
    }

    #[test]
    fn test_component() {
        let (util, component) = with_stream_no_messages(
            parse_component_declaration,
            "\
component foo
end component;
",
        );
        assert_eq!(component, to_component(util.ident("foo"), vec![], vec![]));

        let (util, component) = with_stream_no_messages(
            parse_component_declaration,
            "\
component foo is
end component;
",
        );
        assert_eq!(component, to_component(util.ident("foo"), vec![], vec![]));

        let (util, component) = with_stream_no_messages(
            parse_component_declaration,
            "\
component foo is
end component foo;
",
        );
        assert_eq!(component, to_component(util.ident("foo"), vec![], vec![]));
    }

    #[test]
    fn test_component_with_generic() {
        let (util, component) = with_stream_no_messages(
            parse_component_declaration,
            "\
component foo is
  generic (
    foo : natural
  );
end component;
",
        );
        assert_eq!(
            component,
            to_component(
                util.ident("foo"),
                vec![util.generic("foo : natural")],
                vec![]
            )
        );
    }

    #[test]
    fn test_component_with_port() {
        let (util, component) = with_stream_no_messages(
            parse_component_declaration,
            "\
component foo is
  port (
    foo : natural
  );
end component;
",
        );
        assert_eq!(
            component,
            to_component(util.ident("foo"), vec![], vec![util.port("foo : natural")])
        );
    }

    #[test]
    fn error_on_duplicate_generic_clause() {
        let (util, result, messages) = with_partial_stream_messages(
            parse_optional_generic_list,
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
        assert_eq!(
            messages,
            vec![error(
                &util.substr_pos("generic", 2),
                "Duplicate generic clause"
            )]
        );
        assert_eq!(result, Ok(Some(vec![util.generic("foo : natural")])),);
    }

    #[test]
    fn error_on_duplicate_port_clause() {
        let (util, result, messages) = with_partial_stream_messages(
            parse_optional_port_list,
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
        assert_eq!(
            messages,
            vec![error(&util.substr_pos("port", 2), "Duplicate port clause")]
        );
        assert_eq!(result, Ok(Some(vec![util.port("foo : natural")])),);
    }

    #[test]
    fn error_generic_after_port_clause() {
        let (util, result, messages) = with_partial_stream_messages(
            parse_optional_port_list,
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
        assert_eq!(
            messages,
            vec![error(
                &util.first_substr_pos("generic"),
                "Generic clause must come before port clause"
            )]
        );
        assert_eq!(result, Ok(Some(vec![util.port("foo : natural")])),);
    }
}
