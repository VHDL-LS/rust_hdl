// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use ast::{ComponentDeclaration, InterfaceDeclaration};
use common::warning_on_end_identifier_mismatch;
use interface_declaration::{parse_generic_interface_list, parse_port_interface_list};
use message::{error, push_some, MessageHandler, ParseResult};
use tokenizer::Kind::*;
use tokenstream::TokenStream;

fn parse_generic_and_port_clauses(
    stream: &mut TokenStream,
    messages: &mut MessageHandler,
) -> ParseResult<(Vec<InterfaceDeclaration>, Vec<InterfaceDeclaration>)> {
    let mut generic_list = None;
    let mut port_list = None;

    loop {
        let token = stream.expect()?;
        try_token_kind!(
            token,
            Generic => {
                let new_generic_list = parse_generic_interface_list(stream, messages)?;
                stream.pop_if_kind(SemiColon)?;
                if generic_list.is_some() {
                    messages.push(error(&token.pos, "Duplicate generic clause"));
                } else {
                    generic_list = Some(new_generic_list);
                }
            },
            Port => {
                let new_port_list = parse_port_interface_list(stream, messages)?;
                stream.pop_if_kind(SemiColon)?;
                if port_list.is_some() {
                    messages.push(error(&token.pos, "Duplicate port clause"));
                } else {
                    port_list = Some(new_port_list);
                }
            },
            End => {
                break;
            }
        )
    }

    Ok((generic_list.unwrap_or(vec![]), port_list.unwrap_or(vec![])))
}

pub fn parse_component_declaration(
    stream: &mut TokenStream,
    messages: &mut MessageHandler,
) -> ParseResult<ComponentDeclaration> {
    stream.expect_kind(Component)?;
    let ident = stream.expect_ident()?;
    stream.pop_if_kind(Is)?;

    let (generic_list, port_list) = parse_generic_and_port_clauses(stream, messages)?;

    stream.expect_kind(Component)?;
    if let Some(token) = stream.pop_if_kind(Identifier)? {
        push_some(
            messages,
            warning_on_end_identifier_mismatch(&ident, &Some(token.expect_ident()?)),
        );
    }
    stream.expect_kind(SemiColon)?;

    Ok(ComponentDeclaration {
        ident: ident,
        generic_list: generic_list,
        port_list: port_list,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    use ast::Ident;
    use message::error;
    use test_util::{with_stream_messages, with_stream_no_messages};

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
    fn test_component_with_generic_duplicate() {
        let (util, component, messages) = with_stream_messages(
            parse_component_declaration,
            "\
component foo is
  generic (
    foo : natural
  );
  generic (
    bar : natural
  );
end component;
",
        );
        assert_eq!(
            messages,
            vec![error(
                &util.substr_pos("generic", 2),
                "Duplicate generic clause"
            )]
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
    fn test_component_with_port_duplicate() {
        let (util, component, messages) = with_stream_messages(
            parse_component_declaration,
            "\
component foo is
  port (
    foo : natural
  );
  port (
    bar : natural
  );
end component;
",
        );
        assert_eq!(
            messages,
            vec![error(&util.substr_pos("port", 2), "Duplicate port clause")]
        );
        assert_eq!(
            component,
            to_component(util.ident("foo"), vec![], vec![util.port("foo : natural")])
        );
    }

}
