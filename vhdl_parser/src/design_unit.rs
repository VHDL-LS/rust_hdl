// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use tokenizer::Kind::*;
use tokenstream::TokenStream;

use ast::{
    ArchitectureBody, DesignFile, DesignUnit, EntityDeclaration, LibraryUnit, PackageBody,
    PackageDeclaration,
};
use common::warning_on_end_identifier_mismatch;
use concurrent_statement::parse_labeled_concurrent_statements;
use configuration::parse_configuration_declaration;
use context::{parse_context, parse_library_clause, parse_use_clause, DeclarationOrReference};
use declarative_part::{parse_declarative_part, parse_package_instantiation};
use interface_declaration::{parse_generic_interface_list, parse_port_interface_list};
use message::{error, push_result, MessageHandler, ParseResult};

/// Parse an entity declaration, token is initial entity token
/// If a parse error occurs the stream is consumed until and end entity
fn parse_entity_declaration(
    stream: &mut TokenStream,
    messages: &mut MessageHandler,
) -> ParseResult<EntityDeclaration> {
    stream.expect_kind(Entity)?;

    let mut generic_clause = None;
    let mut port_clause = None;

    let ident = stream.expect_ident()?;
    stream.expect_kind(Is)?;

    loop {
        let token = stream.expect()?;
        try_token_kind!(
            token,
            Generic => {
                let new_generic_clause = parse_generic_interface_list(stream, messages)?;
                stream.expect_kind(SemiColon)?;

                if generic_clause.is_some() {
                    messages.push(error(&token.pos, "Duplicate generic clause"));
                    continue;
                } else {
                    generic_clause = Some(new_generic_clause);
                }

            },

            Port => {
                let new_port_clause = parse_port_interface_list(stream, messages)?;
                stream.expect_kind(SemiColon)?;

                if port_clause.is_some() {
                    messages.push(error(&token.pos, "Duplicate port clause"));
                    continue;
                } else {
                    port_clause = Some(new_port_clause);
                }
            },

            End => {
                stream.pop_if_kind(Entity)?;
                let end_ident = stream.pop_optional_ident()?;
                if let Some(msg) = warning_on_end_identifier_mismatch(&ident, &end_ident) {
                    messages.push(msg);
                }
                stream.expect_kind(SemiColon)?;
                return Ok(EntityDeclaration {
                    ident: ident,
                    generic_clause: generic_clause,
                    port_clause: port_clause,
                    statements: Vec::new(),
                });
            },

            Begin => {
                let statements = parse_labeled_concurrent_statements(stream, messages)?;
                stream.pop_if_kind(Entity)?;
                let end_ident = stream.pop_optional_ident()?;
                if let Some(msg) = warning_on_end_identifier_mismatch(&ident, &end_ident) {
                    messages.push(msg);
                }
                stream.expect_kind(SemiColon)?;
                return Ok(EntityDeclaration {
                    ident: ident,
                    generic_clause: generic_clause,
                    port_clause: port_clause,
                    statements,
                });
            }
        );
    }
}

/// LRM 3.3.1
fn parse_architecture_body(
    stream: &mut TokenStream,
    messages: &mut MessageHandler,
) -> ParseResult<ArchitectureBody> {
    stream.expect_kind(Architecture)?;
    let ident = stream.expect_ident()?;
    stream.expect_kind(Of)?;
    let entity_name = stream.expect_ident()?.item;
    stream.expect_kind(Is)?;

    let decl = parse_declarative_part(stream, messages, true)?;

    let statements = parse_labeled_concurrent_statements(stream, messages)?;
    stream.pop_if_kind(Architecture)?;

    let end_ident = stream.pop_optional_ident()?;
    if let Some(msg) = warning_on_end_identifier_mismatch(&ident, &end_ident) {
        messages.push(msg);
    }

    stream.expect_kind(SemiColon)?;

    Ok(ArchitectureBody {
        ident,
        entity_name,
        decl,
        statements,
    })
}

/// LRM 4.7 Package declarations
fn parse_package_declaration(
    stream: &mut TokenStream,
    messages: &mut MessageHandler,
) -> ParseResult<PackageDeclaration> {
    stream.expect_kind(Package)?;
    let ident = stream.expect_ident()?;

    stream.expect_kind(Is)?;
    let generic_clause = {
        if stream.skip_if_kind(Generic)? {
            let decl = parse_generic_interface_list(stream, messages)?;
            stream.expect_kind(SemiColon)?;
            Some(decl)
        } else {
            None
        }
    };
    let decl = parse_declarative_part(stream, messages, false)?;
    stream.pop_if_kind(Package)?;
    let end_ident = stream.pop_optional_ident()?;
    if let Some(msg) = warning_on_end_identifier_mismatch(&ident, &end_ident) {
        messages.push(msg);
    }
    stream.pop_if_kind(Identifier)?;
    stream.expect_kind(SemiColon)?;
    return Ok(PackageDeclaration {
        ident,
        generic_clause,
        decl,
    });
}

/// LRM 4.8 Package bodies
fn parse_package_body(
    stream: &mut TokenStream,
    messages: &mut MessageHandler,
) -> ParseResult<PackageBody> {
    stream.expect_kind(Package)?;
    stream.expect_kind(Body)?;
    let ident = stream.expect_ident()?;

    stream.expect_kind(Is)?;
    let decl = parse_declarative_part(stream, messages, false)?;
    stream.pop_if_kind(Package)?;
    stream.pop_if_kind(Body)?;
    let end_ident = stream.pop_optional_ident()?;
    if let Some(msg) = warning_on_end_identifier_mismatch(&ident, &end_ident) {
        messages.push(msg);
    }
    stream.expect_kind(SemiColon)?;

    return Ok(PackageBody { ident, decl });
}

pub fn parse_design_file(
    stream: &mut TokenStream,
    messages: &mut MessageHandler,
) -> ParseResult<DesignFile> {
    let mut design_units = vec![];

    while let Some(token) = stream.peek()? {
        try_token_kind!(
            token,
            Library => {
                let decl = parse_library_clause(stream);
                push_result(messages, decl);
            },
            Use => {
                let decl = parse_use_clause(stream);
                push_result(messages, decl);
            },
            Context => match parse_context(stream, messages) {
                Ok(DeclarationOrReference::Declaration(context_decl)) => {
                    design_units.push(DesignUnit {
                        context_clause: vec![],
                        library_unit: LibraryUnit::ContextDeclaration(context_decl),
                    });
                }
                Ok(_) => {}
                Err(msg) => messages.push(msg),
            },
            Entity => match parse_entity_declaration(stream, messages) {
                Ok(entity) => {
                    design_units.push(DesignUnit {
                        context_clause: vec![],
                        library_unit: LibraryUnit::EntityDeclaration(entity),
                    });
                }
                Err(msg) => messages.push(msg),
            },

            Architecture => match parse_architecture_body(stream, messages) {
                Ok(architecture) => {
                    design_units.push(DesignUnit {
                        context_clause: vec![],
                        library_unit: LibraryUnit::Architecture(architecture),
                    });
                }
                Err(msg) => messages.push(msg),
            },

            Configuration => match parse_configuration_declaration(stream, messages) {
                Ok(configuration) => {
                    design_units.push(DesignUnit {
                        context_clause: vec![],
                        library_unit: LibraryUnit::Configuration(configuration),
                    });
                }
                Err(msg) => messages.push(msg),
            },
            Package => {
                if stream.is_peek_kinds(&[Package, Body])? {
                    match parse_package_body(stream, messages) {
                        Ok(package_body) => {
                            design_units.push(DesignUnit {
                                context_clause: vec![],
                                library_unit: LibraryUnit::PackageBody(package_body),
                            });
                        }
                        Err(msg) => messages.push(msg),
                    };
                } else if stream.is_peek_kinds(&[Package, Identifier, Is, New])? {
                    match parse_package_instantiation(stream) {
                        Ok(inst) => design_units.push(DesignUnit {
                            context_clause: vec![],
                            library_unit: LibraryUnit::PackageInstance(inst),
                        }),
                        Err(msg) => messages.push(msg),
                    }
                } else {
                    match parse_package_declaration(stream, messages) {
                        Ok(package) => {
                            design_units.push(DesignUnit {
                                context_clause: vec![],
                                library_unit: LibraryUnit::PackageDeclaration(package),
                            });
                        }
                        Err(msg) => messages.push(msg),
                    };
                }
            }
        );
    }

    Ok(DesignFile { design_units })
}

#[cfg(test)]
mod tests {
    use super::*;

    use ast::Ident;
    use message::Message;
    use symbol_table::Symbol;
    use test_util::{check_no_messages, with_stream, with_stream_no_messages, TestUtil};

    fn parse_str(code: &str) -> (TestUtil, DesignFile, Vec<Message>) {
        let mut messages = vec![];
        let (util, design_file) =
            with_stream(|stream| parse_design_file(stream, &mut messages), code);
        (util, design_file, messages)
    }

    fn parse_ok(code: &str) -> (TestUtil, DesignFile) {
        let (util, designfile, messages) = parse_str(code);
        check_no_messages(&messages);
        (util, designfile)
    }

    fn library_units(design_file: DesignFile) -> Vec<LibraryUnit> {
        design_file
            .design_units
            .into_iter()
            .map(|design_unit| design_unit.library_unit)
            .collect()
    }

    fn to_single_entity(design_file: DesignFile) -> EntityDeclaration {
        match design_file.design_units.as_slice() {
            &[DesignUnit {
                library_unit: LibraryUnit::EntityDeclaration(ref entity),
                ..
            }] => entity.to_owned(),
            _ => panic!("Expected single entity {:?}", design_file),
        }
    }

    #[test]
    fn parse_empty() {
        let (_, design_file) = parse_ok("");
        assert_eq!(design_file.design_units.len(), 0);
    }

    /// An simple entity with only a name
    fn simple_entity(ident: Ident) -> LibraryUnit {
        LibraryUnit::EntityDeclaration(EntityDeclaration {
            ident: ident,
            generic_clause: None,
            port_clause: None,
            statements: vec![],
        })
    }

    #[test]
    fn parse_entity_declaration() {
        let (util, design_file) = parse_ok(
            "
entity myent is
end entity;
",
        );
        assert_eq!(
            library_units(design_file),
            [simple_entity(util.ident("myent"))]
        );

        let (util, design_file) = parse_ok(
            "
entity myent is
end entity myent;
",
        );
        assert_eq!(
            library_units(design_file),
            [simple_entity(util.ident("myent"))]
        );
    }

    #[test]
    fn parse_entity_generic_clause() {
        let (util, design_file) = parse_ok(
            "
entity myent is
  generic ();
end entity;
",
        );
        assert_eq!(
            to_single_entity(design_file),
            EntityDeclaration {
                ident: util.ident("myent"),
                generic_clause: Some(Vec::new()),
                port_clause: None,
                statements: vec![],
            }
        );
    }

    #[test]
    fn parse_entity_generic_clause_with_values() {
        let (util, design_file) = parse_ok(
            "
entity myent is
  generic (
    runner_cfg : string);
end entity;
",
        );

        assert_eq!(
            to_single_entity(design_file),
            EntityDeclaration {
                ident: Ident {
                    item: util.symbol("myent"),
                    pos: util.first_substr_pos("myent")
                },
                generic_clause: Some(vec![util.generic("runner_cfg : string")]),
                port_clause: None,
                statements: vec![],
            }
        );
    }

    #[test]
    fn parse_entity_error_double_generic_clause() {
        let (util, design_file, messages) = parse_str(
            "
entity myent is
  generic ();
  generic ();
end entity;
",
        );

        assert_eq!(
            to_single_entity(design_file),
            EntityDeclaration {
                ident: Ident {
                    item: util.symbol("myent"),
                    pos: util.first_substr_pos("myent")
                },
                generic_clause: Some(vec![]),
                port_clause: None,
                statements: vec![],
            }
        );

        assert_eq!(
            messages,
            [error(
                &util.substr_pos("generic", 2),
                "Duplicate generic clause"
            )]
        );
    }

    #[test]
    fn parse_entity_port_clause() {
        let (util, design_file) = parse_ok(
            "
entity myent is
  port ();
end entity;
",
        );
        assert_eq!(
            to_single_entity(design_file),
            EntityDeclaration {
                ident: util.ident("myent"),
                generic_clause: None,
                port_clause: Some(vec![]),
                statements: vec![],
            }
        );
    }

    #[test]
    fn parse_entity_empty_statements() {
        let (util, design_file) = parse_ok(
            "
entity myent is
begin
end entity;
",
        );
        assert_eq!(
            to_single_entity(design_file),
            EntityDeclaration {
                ident: util.ident("myent"),
                generic_clause: None,
                port_clause: None,
                statements: vec![],
            }
        );
    }

    #[test]
    fn parse_entity_statements() {
        let (util, design_file) = parse_ok(
            "
entity myent is
begin
  check(clk, valid);
end entity;
",
        );
        assert_eq!(
            to_single_entity(design_file),
            EntityDeclaration {
                ident: util.ident("myent"),
                generic_clause: None,
                port_clause: None,
                statements: vec![util.concurrent_statement("check(clk, valid);")],
            }
        );
    }

    #[test]
    fn parse_entity_error_double_port_clause() {
        let (util, design_file, messages) = parse_str(
            "
entity myent is
  port (
    signal clk : std_logic
  );
  port ();
end entity;
",
        );

        assert_eq!(
            to_single_entity(design_file),
            EntityDeclaration {
                ident: Ident {
                    item: util.symbol("myent"),
                    pos: util.first_substr_pos("myent")
                },
                generic_clause: None,
                port_clause: Some(vec![util.port("signal clk : std_logic")]),
                statements: vec![],
            }
        );

        assert_eq!(
            messages,
            [error(&util.substr_pos("port", 2), "Duplicate port clause")]
        );
    }

    #[test]
    fn parse_multiple_entity_declarations() {
        let (util, design_file) = parse_ok(
            "
entity myent is
end entity;

entity myent2 is
end entity myent2;

entity myent3 is
end myent3;

entity myent4 is
end;
",
        );
        assert_eq!(
            library_units(design_file),
            [
                simple_entity(util.ident("myent")),
                simple_entity(util.ident("myent2")),
                simple_entity(util.ident("myent3")),
                simple_entity(util.ident("myent4"))
            ]
        );
    }

    // An simple entity with only a name
    fn simple_architecture(ident: Ident, entity_name: Symbol) -> LibraryUnit {
        LibraryUnit::Architecture(ArchitectureBody {
            ident,
            entity_name,
            decl: Vec::new(),
            statements: vec![],
        })
    }

    #[test]
    fn parse_architecture_body() {
        let (util, design_file) = parse_ok(
            "
architecture arch_name of myent is
begin
end architecture;
",
        );
        assert_eq!(
            library_units(design_file),
            [simple_architecture(
                util.ident("arch_name"),
                util.symbol("myent")
            )]
        );
    }

    #[test]
    fn parse_architecture_body_end_identifier() {
        let (util, design_file) = parse_ok(
            "
architecture arch_name of myent is
begin
end architecture arch_name;
",
        );
        assert_eq!(
            library_units(design_file),
            [simple_architecture(
                util.ident("arch_name"),
                util.symbol("myent")
            )]
        );
    }

    #[test]
    fn parse_architecture_body_end() {
        let (util, design_file) = parse_ok(
            "
architecture arch_name of myent is
begin
end;
",
        );
        assert_eq!(
            library_units(design_file),
            [simple_architecture(
                util.ident("arch_name"),
                util.symbol("myent")
            )]
        );
    }

    #[test]
    fn test_package_declaration() {
        let (util, package) = with_stream_no_messages(
            parse_package_declaration,
            "
package pkg_name is
end package;
",
        );
        assert_eq!(
            package,
            PackageDeclaration {
                ident: util.ident("pkg_name"),
                generic_clause: None,
                decl: vec![],
            }
        );
    }

    #[test]
    fn test_package_declaration_with_declarations() {
        let (util, package) = with_stream_no_messages(
            parse_package_declaration,
            "
package pkg_name is
  type foo;
  constant bar : natural := 0;
end package;
",
        );
        assert_eq!(
            package,
            PackageDeclaration {
                ident: util.ident("pkg_name"),
                generic_clause: None,
                decl: util.declarative_part(
                    "\
  type foo;
  constant bar : natural := 0;
"
                ),
            }
        );
    }

    #[test]
    fn test_package_declaration_generics_clause() {
        let (util, package) = with_stream_no_messages(
            parse_package_declaration,
            "
package pkg_name is
  generic (
    type foo;
    type bar
  );
end package;
",
        );
        assert_eq!(
            package,
            PackageDeclaration {
                ident: util.ident("pkg_name"),
                generic_clause: Some(vec![util.generic("type foo"), util.generic("type bar")]),
                decl: vec![]
            }
        );
    }
}
