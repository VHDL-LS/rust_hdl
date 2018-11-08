// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use symbol_table::Symbol;
use tokenizer::Kind::*;
use tokenstream::TokenStream;

use ast::{DesignFile, DesignUnit, GenericClause, Ident, LibraryUnit, PortClause};
use concurrent_statement::parse_labeled_concurrent_statements;
use configuration::parse_configuration_declaration;
use context::{parse_context, parse_library_clause, parse_use_clause, DeclarationOrReference};
use declarative_part::{parse_declarative_part, parse_package_instantiation};
use interface_declaration::{parse_generic_interface_list, parse_port_interface_list};
use message::{error, push_result, MessageHandler, ParseResult};

/// Parse a generic clause
fn parse_generic_clause(
    stream: &mut TokenStream,
    messages: &mut MessageHandler,
) -> ParseResult<GenericClause> {
    let generic_list = parse_generic_interface_list(stream, messages)?;

    if let Err(err) = stream.expect_kind(SemiColon) {
        messages.push(err);
    }

    Ok(GenericClause {
        generic_list: generic_list,
    })
}

fn parse_port_clause(
    stream: &mut TokenStream,
    messages: &mut MessageHandler,
) -> ParseResult<PortClause> {
    let port_list = parse_port_interface_list(stream, messages)?;

    if let Err(err) = stream.expect_kind(SemiColon) {
        messages.push(err);
    }

    Ok(PortClause {
        port_list: port_list,
    })
}

/// Parse an entity declaration, token is initial entity token
/// If a parse error occurs the stream is consumed until and end entity
fn parse_entity_decl(
    stream: &mut TokenStream,
    messages: &mut MessageHandler,
) -> ParseResult<LibraryUnit> {
    let entity_token = stream.expect_kind(Entity)?;

    let mut generic_clause = None;
    let mut port_clause = None;

    if let Ok(ident) = stream.expect_ident() {
        if stream.expect_kind(Is).is_ok() {
            while let Some(token) = stream.pop()? {
                match token.kind {
                    Generic => {
                        if generic_clause.is_some() {
                            messages.push(error(&token.pos, "Duplicate generic clause"));
                            push_result(messages, stream.skip_until_kind(SemiColon));
                            continue;
                        }

                        if let Ok(generics) = parse_generic_clause(stream, messages) {
                            generic_clause = Some(generics);
                        } else {
                            messages.push(error(&token, "Failed to parse generic clause"));
                        }
                    }

                    // @TODO Skip port list for now
                    Port => {
                        if port_clause.is_some() {
                            messages.push(error(&token.pos, "Duplicate port clause"));
                            push_result(messages, stream.skip_until_kind(SemiColon));
                            continue;
                        }

                        if let Ok(ports) = parse_port_clause(stream, messages) {
                            port_clause = Some(ports);
                        }
                    }

                    End => {
                        stream.pop_if_kind(Entity)?;
                        // @TODO check identifier
                        stream.pop_if_kind(Identifier)?;
                        stream.expect_kind(SemiColon)?;
                        return Ok(LibraryUnit::EntityDeclaration {
                            ident: ident,
                            generic_clause: generic_clause,
                            port_clause: port_clause,
                        });
                    }

                    Begin => {
                        let statements = parse_labeled_concurrent_statements(stream, messages);
                        push_result(messages, statements);
                        stream.pop_if_kind(Entity)?;
                        // @TODO check identifier
                        stream.pop_if_kind(Identifier)?;
                        stream.expect_kind(SemiColon)?;
                        return Ok(LibraryUnit::EntityDeclaration {
                            ident: ident,
                            generic_clause: generic_clause,
                            port_clause: port_clause,
                        });
                    }

                    _ => {
                        messages.push(token.kinds_error(&[Generic, Port, Begin, End]));
                        break;
                    }
                }
            }
        }
    }

    push_result(messages, stream.pop_if_kind(End));
    push_result(messages, stream.skip_until_kind(SemiColon));
    return Err(error(&entity_token, "Failed to parse entity declaration"));
}

/// Parse architecture symbol declaration and entity name
fn parse_architecture_header(stream: &mut TokenStream) -> ParseResult<(Ident, Symbol)> {
    let ident = stream.expect_ident()?;
    stream.expect_kind(Of)?;
    let Ident {
        item: entity_name, ..
    } = stream.expect_ident()?;
    stream.expect_kind(Is)?;
    Ok((ident, entity_name))
}

/// LRM 3.3.1
fn parse_architecture_body(
    stream: &mut TokenStream,
    messages: &mut MessageHandler,
) -> ParseResult<LibraryUnit> {
    let architecture_token = stream.expect_kind(Architecture)?;
    let design_unit = match parse_architecture_header(stream) {
        Ok((ident, entity_name)) => Ok(LibraryUnit::ArchitectureBody {
            ident,
            entity_name,
            decl: parse_declarative_part(stream, messages, true)?,
        }),
        Err(err) => {
            messages.push(err);
            return Err(error(&architecture_token, "Failed to parse architecture"));
        }
    };

    let statements = parse_labeled_concurrent_statements(stream, messages);
    push_result(messages, statements);
    stream.pop_if_kind(Architecture)?;
    // @TODO check end identifier
    stream.pop_if_kind(Identifier)?;
    stream.expect_kind(SemiColon)?;

    if design_unit.is_err() {
        messages.push(error(
            &architecture_token,
            "Failed to parse architecture declaration",
        ));
    }

    return design_unit;
}

/// LRM 4.7 Package declarations
fn parse_package_declaration(
    stream: &mut TokenStream,
    messages: &mut MessageHandler,
) -> ParseResult<LibraryUnit> {
    stream.expect_kind(Package)?;
    let ident = match stream.expect_ident() {
        Ok(ident) => ident,
        Err(err) => {
            return Err(err);
        }
    };

    stream.expect_kind(Is)?;
    if stream.skip_if_kind(Generic)? {
        let decl = parse_generic_interface_list(stream, messages);
        push_result(messages, decl);
        stream.expect_kind(SemiColon)?;
    }
    let decl = parse_declarative_part(stream, messages, false)?;
    stream.pop_if_kind(Package)?;
    // @TODO check end identifier
    stream.pop_if_kind(Identifier)?;
    stream.expect_kind(SemiColon)?;
    return Ok(LibraryUnit::PackageDeclaration { ident, decl });
}

/// LRM 4.8 Package bodies
fn parse_package_body(
    stream: &mut TokenStream,
    messages: &mut MessageHandler,
) -> ParseResult<LibraryUnit> {
    stream.expect_kind(Package)?;
    stream.expect_kind(Body)?;
    let ident = match stream.expect_ident() {
        Ok(ident) => ident,
        Err(err) => {
            return Err(err);
        }
    };

    push_result(messages, stream.expect_kind(Is));
    let decl = parse_declarative_part(stream, messages, false);
    stream.pop_if_kind(Package)?;
    stream.pop_if_kind(Body)?;
    // @TODO check end identifier
    stream.pop_if_kind(Identifier)?;
    stream.expect_kind(SemiColon)?;
    push_result(messages, decl);

    return Ok(LibraryUnit::PackageBody { ident });
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
            Entity => match parse_entity_decl(stream, messages) {
                Ok(library_unit) => {
                    design_units.push(DesignUnit {
                        context_clause: vec![],
                        library_unit,
                    });
                }
                Err(msg) => messages.push(msg),
            },

            Architecture => match parse_architecture_body(stream, messages) {
                Ok(library_unit) => {
                    design_units.push(DesignUnit {
                        context_clause: vec![],
                        library_unit,
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
                        Ok(library_unit) => {
                            design_units.push(DesignUnit {
                                context_clause: vec![],
                                library_unit,
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
                        Ok(library_unit) => {
                            design_units.push(DesignUnit {
                                context_clause: vec![],
                                library_unit,
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

    use ast::{InterfaceDeclaration, InterfaceObjectDeclaration, Mode, ObjectClass};
    use message::Message;
    use test_util::{check_no_messages, with_stream, TestUtil};

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

    #[test]
    fn parse_empty() {
        let (_, design_file) = parse_ok("");
        assert_eq!(design_file.design_units.len(), 0);
    }

    /// An simple entity with only a name
    fn simple_entity(ident: Ident) -> LibraryUnit {
        LibraryUnit::EntityDeclaration {
            ident: ident,
            generic_clause: None,
            port_clause: None,
        }
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
            library_units(design_file),
            [LibraryUnit::EntityDeclaration {
                ident: util.ident("myent"),
                generic_clause: Some(GenericClause {
                    generic_list: Vec::new()
                }),
                port_clause: None
            }]
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

        let mut generic_list = Vec::new();
        generic_list.push(InterfaceDeclaration::Object(InterfaceObjectDeclaration {
            mode: Mode::In,
            class: ObjectClass::Constant,
            ident: util.ident("runner_cfg"),
            subtype_indication: util.subtype_indication("string"),
            expression: None,
        }));

        assert_eq!(
            library_units(design_file),
            [LibraryUnit::EntityDeclaration {
                ident: Ident {
                    item: util.symbol("myent"),
                    pos: util.first_substr_pos("myent")
                },
                generic_clause: Some(GenericClause {
                    generic_list: generic_list
                }),
                port_clause: None
            }]
        );
    }

    #[test]
    fn parse_entity_generic_clause_with_many_values() {
        let (util, design_file) = parse_ok(
            "
entity myent is
  generic (
    -- test that optional constant and in mode keyword works
    constant runner_cfg : in string;
    foo : boolean := false);
end entity;
",
        );

        let mut generic_list = Vec::new();
        generic_list.push(InterfaceDeclaration::Object(InterfaceObjectDeclaration {
            mode: Mode::In,
            class: ObjectClass::Constant,
            ident: util.ident("runner_cfg"),
            subtype_indication: util.subtype_indication("string"),
            expression: None,
        }));

        generic_list.push(InterfaceDeclaration::Object(InterfaceObjectDeclaration {
            mode: Mode::In,
            class: ObjectClass::Constant,
            ident: util.ident("foo"),
            subtype_indication: util.subtype_indication("boolean"),
            expression: Some(util.expr("false")),
        }));

        assert_eq!(
            library_units(design_file),
            [LibraryUnit::EntityDeclaration {
                ident: Ident {
                    item: util.symbol("myent"),
                    pos: util.first_substr_pos("myent")
                },
                generic_clause: Some(GenericClause {
                    generic_list: generic_list
                }),
                port_clause: None
            }]
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
            library_units(design_file),
            [LibraryUnit::EntityDeclaration {
                ident: Ident {
                    item: util.symbol("myent"),
                    pos: util.first_substr_pos("myent")
                },
                generic_clause: Some(GenericClause {
                    generic_list: Vec::new()
                }),
                port_clause: None
            }]
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
            library_units(design_file),
            [LibraryUnit::EntityDeclaration {
                ident: util.ident("myent"),
                generic_clause: None,
                port_clause: Some(PortClause {
                    port_list: Vec::new()
                })
            }]
        );
    }

    #[test]
    fn parse_entity_error_double_port_clause() {
        let (util, design_file, messages) = parse_str(
            "
entity myent is
  port ();
  port ();
end entity;
",
        );

        assert_eq!(
            library_units(design_file),
            [LibraryUnit::EntityDeclaration {
                ident: Ident {
                    item: util.symbol("myent"),
                    pos: util.first_substr_pos("myent")
                },
                generic_clause: None,
                port_clause: Some(PortClause {
                    port_list: Vec::new()
                })
            }]
        );

        assert_eq!(
            messages,
            [error(&util.substr_pos("port", 2), "Duplicate port clause")]
        );
    }

    #[test]
    fn parse_entity_port_clause_with_many_values() {
        let (util, design_file) = parse_ok(
            "
entity myent is
  port (
    -- Check that optional signal keyword can be omitted
    clk : std_logic;
    bar : out bit := '1');
end entity;
",
        );

        let mut port_list = Vec::new();
        port_list.push(InterfaceDeclaration::Object(InterfaceObjectDeclaration {
            mode: Mode::In,
            class: ObjectClass::Signal,
            ident: util.ident("clk"),
            subtype_indication: util.subtype_indication("std_logic"),
            expression: None,
        }));

        port_list.push(InterfaceDeclaration::Object(InterfaceObjectDeclaration {
            mode: Mode::Out,
            class: ObjectClass::Signal,
            ident: util.ident("bar"),
            subtype_indication: util.subtype_indication("bit"),
            expression: Some(util.expr("'1'")),
        }));

        assert_eq!(
            library_units(design_file),
            [LibraryUnit::EntityDeclaration {
                ident: Ident {
                    item: util.symbol("myent"),
                    pos: util.first_substr_pos("myent")
                },
                generic_clause: None,
                port_clause: Some(PortClause {
                    port_list: port_list
                })
            }]
        );
    }

    #[test]
    fn parse_entity_port_clause_with_optional_signal() {
        let (util, design_file) = parse_ok(
            "
entity myent is
  port (
    signal clk : std_logic);
end entity;
",
        );

        let mut port_list = Vec::new();
        port_list.push(InterfaceDeclaration::Object(InterfaceObjectDeclaration {
            mode: Mode::In,
            class: ObjectClass::Signal,
            ident: util.ident("clk"),
            subtype_indication: util.subtype_indication("std_logic"),
            expression: None,
        }));

        assert_eq!(
            library_units(design_file),
            [LibraryUnit::EntityDeclaration {
                ident: Ident {
                    item: util.symbol("myent"),
                    pos: util.first_substr_pos("myent")
                },
                generic_clause: None,
                port_clause: Some(PortClause {
                    port_list: port_list
                })
            }]
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

    #[test]
    fn parse_entity_declaration_errors() {
        let (util, design_file, messages) = parse_str(
            "
entity
",
        );
        assert_eq!(library_units(design_file), []);
        assert_eq!(
            messages,
            [error(
                &util.first_substr_pos("entity"),
                "Failed to parse entity declaration"
            )]
        );

        let (util, design_file, messages) = parse_str(
            "
entity myent
",
        );
        assert_eq!(library_units(design_file), []);
        assert_eq!(
            messages,
            [error(
                &util.first_substr_pos("entity"),
                "Failed to parse entity declaration"
            )]
        );

        let (util, design_file, messages) = parse_str(
            "
entity myent is
",
        );
        assert_eq!(library_units(design_file), []);
        assert_eq!(
            messages,
            [error(
                &util.first_substr_pos("entity"),
                "Failed to parse entity declaration"
            )]
        );
    }

    // An simple entity with only a name
    fn simple_architecture(ident: Ident, entity_name: Symbol) -> LibraryUnit {
        LibraryUnit::ArchitectureBody {
            ident,
            entity_name,
            decl: Vec::new(),
        }
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

    fn simple_package(ident: Ident) -> LibraryUnit {
        LibraryUnit::PackageDeclaration {
            ident,
            decl: vec![],
        }
    }

    #[test]
    fn parse_package_declaration() {
        let (util, design_file) = parse_ok(
            "
package pkg_name is
end package;
",
        );
        assert_eq!(
            library_units(design_file),
            [simple_package(util.ident("pkg_name"),)]
        );
    }
}
