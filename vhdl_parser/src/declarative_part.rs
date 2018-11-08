// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use alias_declaration::parse_alias_declaration;
use ast::{Declaration, PackageInstantiation};
use attributes::parse_attribute;
use component_declaration::parse_component_declaration;
use context::parse_use_clause;
use message::{MessageHandler, ParseResult};
use names::{parse_association_list, parse_selected_name};
use object_declaration::{parse_file_declaration, parse_object_declaration};
use subprogram::parse_subprogram;
use tokenizer::{Kind::*, Token};
use tokenstream::TokenStream;
use type_declaration::parse_type_declaration;

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
    Ok(check_declarative_part(&stream.peek_expect()?, begin_is_end).is_ok())
}

fn check_declarative_part(token: &Token, begin_is_end: bool) -> ParseResult<()> {
    match token.kind {
        Use | Type | Subtype | Shared | Constant | Signal | Variable | File | Component
        | Attribute | Alias | Impure | Function | Procedure | Package => Ok(()),
        kind => {
            let end_kind = {
                if begin_is_end {
                    Begin
                } else {
                    End
                }
            };
            let decl_kinds = [
                Use, Type, Subtype, Shared, Constant, Signal, Variable, File, Component, Attribute,
                Alias, Impure, Function, Procedure, Package, end_kind,
            ];

            if kind == end_kind {
                Ok(())
            } else {
                Err(token.kinds_error(&decl_kinds))
            }
        }
    }
}

pub fn parse_declarative_part(
    stream: &mut TokenStream,
    messages: &mut MessageHandler,
    begin_is_end: bool,
) -> ParseResult<Vec<Declaration>> {
    let mut declarations: Vec<Declaration> = Vec::new();

    while let Some(token) = stream.peek()? {
        match token.kind {
            Use => match parse_use_clause(stream) {
                Ok(decl) => declarations.push(Declaration::Use(decl)),
                Err(msg) => {
                    messages.push(msg);
                }
            },
            Type | Subtype => match parse_type_declaration(stream, messages) {
                Ok(decl) => declarations.push(Declaration::Type(decl)),
                Err(msg) => {
                    messages.push(msg);
                }
            },
            Shared | Constant | Signal | Variable => match parse_object_declaration(stream) {
                Ok(decls) => {
                    for decl in decls {
                        declarations.push(Declaration::Object(decl))
                    }
                }
                Err(msg) => {
                    messages.push(msg);
                }
            },
            File => match parse_file_declaration(stream) {
                Ok(decls) => {
                    for decl in decls {
                        declarations.push(Declaration::File(decl))
                    }
                }
                Err(msg) => {
                    messages.push(msg);
                }
            },
            Component => match parse_component_declaration(stream, messages) {
                Ok(decl) => declarations.push(Declaration::Component(decl)),
                Err(msg) => {
                    messages.push(msg);
                }
            },
            Attribute => match parse_attribute(stream) {
                Ok(decls) => {
                    for decl in decls {
                        declarations.push(Declaration::Attribute(decl))
                    }
                }
                Err(msg) => {
                    messages.push(msg);
                }
            },
            Alias => match parse_alias_declaration(stream) {
                Ok(decl) => declarations.push(Declaration::Alias(decl)),
                Err(msg) => {
                    messages.push(msg);
                }
            },
            Impure | Function | Procedure => match parse_subprogram(stream, messages) {
                Ok(decl) => declarations.push(decl),
                Err(msg) => {
                    messages.push(msg);
                }
            },
            Package => match parse_package_instantiation(stream) {
                Ok(decl) => declarations.push(Declaration::Package(decl)),
                Err(msg) => {
                    messages.push(msg);
                }
            },
            _ => {
                stream.move_after(&token);

                if let Err(msg) = check_declarative_part(&token, begin_is_end) {
                    messages.push(msg);
                } else {
                    break;
                }
            }
        }
    }

    Ok(declarations)
}

#[cfg(test)]
mod tests {
    use super::*;

    use test_util::with_stream;

    #[test]
    fn package_instantiation() {
        let (util, inst) = with_stream(
            parse_package_instantiation,
            "\
package ident is new lib.foo.bar;
",
        );
        assert_eq!(
            inst,
            PackageInstantiation {
                ident: util.ident("ident"),
                package_name: util.selected_name("lib.foo.bar"),
                generic_map: None
            }
        );
    }

    #[test]
    fn package_instantiation_generic_map() {
        let (util, inst) = with_stream(
            parse_package_instantiation,
            "\
package ident is new lib.foo.bar
  generic map (
    foo => bar
  );
",
        );
        assert_eq!(
            inst,
            PackageInstantiation {
                ident: util.ident("ident"),
                package_name: util.selected_name("lib.foo.bar"),
                generic_map: Some(util.association_list(
                    "(
    foo => bar
  )"
                ))
            }
        );
    }
}
