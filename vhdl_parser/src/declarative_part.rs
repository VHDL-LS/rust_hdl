// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use crate::alias_declaration::parse_alias_declaration;
use crate::ast::{Declaration, PackageInstantiation};
use crate::attributes::parse_attribute;
use crate::component_declaration::parse_component_declaration;
use crate::configuration::parse_configuration_specification;
use crate::context::parse_use_clause;
use crate::message::{MessageHandler, ParseResult};
use crate::names::{parse_association_list, parse_selected_name};
use crate::object_declaration::{parse_file_declaration, parse_object_declaration};
use crate::subprogram::parse_subprogram;
use crate::tokenizer::{Kind::*, Token};
use crate::tokenstream::TokenStream;
use crate::type_declaration::parse_type_declaration;

pub fn parse_package_instantiation(stream: &mut TokenStream) -> ParseResult<PackageInstantiation> {
    stream.expect_kind(Package)?;
    let ident = stream.expect_ident()?;
    stream.expect_kind(Is)?;
    stream.expect_kind(New)?;
    let package_name = parse_selected_name(stream)?;

    let token = stream.expect()?;
    let generic_map = try_token_kind!(
        token,
        Generic => {
            stream.expect_kind(Map)?;
            let association_list = parse_association_list(stream)?;
            stream.expect_kind(SemiColon)?;
            Some(association_list)
        },
        SemiColon => None);
    Ok(PackageInstantiation {
        ident,
        package_name,
        generic_map,
    })
}

pub fn is_declarative_part(stream: &mut TokenStream, begin_is_end: bool) -> ParseResult<bool> {
    Ok(check_declarative_part(&stream.peek_expect()?, !begin_is_end, begin_is_end).is_ok())
}

fn check_declarative_part(token: &Token, may_end: bool, may_begin: bool) -> ParseResult<()> {
    match token.kind {
        Use | Type | Subtype | Shared | Constant | Signal | Variable | File | Component
        | Attribute | Alias | Impure | Function | Procedure | Package | For => Ok(()),
        Begin if may_begin => Ok(()),
        End if may_end => Ok(()),
        _ => {
            let decl_kinds = [
                Use, Type, Subtype, Shared, Constant, Signal, Variable, File, Component, Attribute,
                Alias, Impure, Function, Procedure, Package, For,
            ];

            Err(token.kinds_error(&decl_kinds))
        }
    }
}
pub fn parse_declarative_part(
    stream: &mut TokenStream,
    messages: &mut dyn MessageHandler,
    begin_is_end: bool,
) -> ParseResult<Vec<Declaration>> {
    let decl = parse_declarative_part_leave_end_token(stream, messages)?;

    if begin_is_end {
        stream.expect_kind(Begin)?;
    } else {
        stream.expect_kind(End)?;
    }
    Ok(decl)
}

pub fn parse_declarative_part_leave_end_token(
    stream: &mut TokenStream,
    messages: &mut dyn MessageHandler,
) -> ParseResult<Vec<Declaration>> {
    let mut declarations: Vec<Declaration> = Vec::new();

    while let Some(token) = stream.peek()? {
        match token.kind {
            Use => declarations.push(Declaration::Use(parse_use_clause(stream)?)),
            Type | Subtype => {
                declarations.push(Declaration::Type(parse_type_declaration(stream, messages)?))
            }
            Shared | Constant | Signal | Variable => {
                for decl in parse_object_declaration(stream)? {
                    declarations.push(Declaration::Object(decl))
                }
            }
            File => {
                for decl in parse_file_declaration(stream)? {
                    declarations.push(Declaration::File(decl))
                }
            }
            Component => declarations.push(Declaration::Component(parse_component_declaration(
                stream, messages,
            )?)),
            Attribute => {
                for decl in parse_attribute(stream)? {
                    declarations.push(Declaration::Attribute(decl))
                }
            }
            Alias => declarations.push(Declaration::Alias(parse_alias_declaration(stream)?)),
            Impure | Function | Procedure => declarations.push(parse_subprogram(stream, messages)?),
            Package => {
                declarations.push(Declaration::Package(parse_package_instantiation(stream)?))
            }
            For => declarations.push(Declaration::Configuration(
                parse_configuration_specification(stream)?,
            )),
            Begin | End => {
                break;
            }
            _ => {
                stream.move_after(&token);
                check_declarative_part(&token, false, false)?;
            }
        }
    }

    Ok(declarations)
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::test_util::Code;

    #[test]
    fn package_instantiation() {
        let code = Code::new(
            "\
package ident is new lib.foo.bar;
",
        );
        assert_eq!(
            code.with_stream(parse_package_instantiation),
            PackageInstantiation {
                ident: code.s1("ident").ident(),
                package_name: code.s1("lib.foo.bar").selected_name(),
                generic_map: None
            }
        );
    }

    #[test]
    fn package_instantiation_generic_map() {
        let code = Code::new(
            "\
package ident is new lib.foo.bar
  generic map (
    foo => bar
  );
",
        );
        assert_eq!(
            code.with_stream(parse_package_instantiation),
            PackageInstantiation {
                ident: code.s1("ident").ident(),
                package_name: code.s1("lib.foo.bar").selected_name(),
                generic_map: Some(
                    code.s1("(
    foo => bar
  )")
                        .association_list()
                )
            }
        );
    }

    #[test]
    fn parse_declarative_part_error() {
        // Just checking that there is not an infinite loop
        let code = Code::new("invalid");
        let (decl, _) = code.with_partial_stream_messages(parse_declarative_part_leave_end_token);
        assert!(decl.is_err());
    }
}
