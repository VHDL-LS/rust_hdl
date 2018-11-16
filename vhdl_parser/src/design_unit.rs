// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use tokenizer::Kind::*;
use tokenstream::TokenStream;

use ast::{
    AnyDesignUnit, ArchitectureBody, ContextItem, DesignFile, DesignUnit, EntityDeclaration,
    PackageBody, PackageDeclaration, PrimaryUnit, SecondaryUnit,
};
use common::error_on_end_identifier_mismatch;
use component_declaration::{parse_optional_generic_list, parse_optional_port_list};
use concurrent_statement::parse_labeled_concurrent_statements;
use configuration::parse_configuration_declaration;
use context::{parse_context, parse_library_clause, parse_use_clause, DeclarationOrReference};
use declarative_part::{
    parse_declarative_part, parse_declarative_part_leave_end_token, parse_package_instantiation,
};
use interface_declaration::parse_generic_interface_list;
use message::{Message, MessageHandler, ParseResult};
use source::WithPos;

/// Parse an entity declaration, token is initial entity token
/// If a parse error occurs the stream is consumed until and end entity
pub fn parse_entity_declaration(
    stream: &mut TokenStream,
    messages: &mut MessageHandler,
) -> ParseResult<EntityDeclaration> {
    stream.expect_kind(Entity)?;

    let ident = stream.expect_ident()?;
    stream.expect_kind(Is)?;

    let generic_clause = parse_optional_generic_list(stream, messages)?;
    let port_clause = parse_optional_port_list(stream, messages)?;

    let decl = parse_declarative_part_leave_end_token(stream, messages)?;

    let token = stream.expect()?;
    let statements = try_token_kind!(
        token,
        End => Vec::new(),
        Begin => parse_labeled_concurrent_statements(stream, messages)?
    );
    stream.pop_if_kind(Entity)?;
    let end_ident = stream.pop_optional_ident()?;
    if let Some(msg) = error_on_end_identifier_mismatch(&ident, &end_ident) {
        messages.push(msg);
    }
    stream.expect_kind(SemiColon)?;
    Ok(EntityDeclaration {
        ident: ident,
        generic_clause: generic_clause,
        port_clause: port_clause,
        decl,
        statements,
    })
}

/// LRM 3.3.1
pub fn parse_architecture_body(
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
    if let Some(msg) = error_on_end_identifier_mismatch(&ident, &end_ident) {
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
    if let Some(msg) = error_on_end_identifier_mismatch(&ident, &end_ident) {
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
    if let Some(msg) = error_on_end_identifier_mismatch(&ident, &end_ident) {
        messages.push(msg);
    }
    stream.expect_kind(SemiColon)?;

    return Ok(PackageBody { ident, decl });
}

fn to_primary_design_unit(
    context_clause: &mut Vec<WithPos<ContextItem>>,
    unit: PrimaryUnit,
) -> AnyDesignUnit {
    AnyDesignUnit::Primary(DesignUnit {
        context_clause: std::mem::replace(context_clause, Vec::new()),
        unit,
    })
}

fn to_secondary_design_unit(
    context_clause: &mut Vec<WithPos<ContextItem>>,
    unit: SecondaryUnit,
) -> AnyDesignUnit {
    AnyDesignUnit::Secondary(DesignUnit {
        context_clause: std::mem::replace(context_clause, Vec::new()),
        unit,
    })
}

pub fn parse_design_file(
    stream: &mut TokenStream,
    messages: &mut MessageHandler,
) -> ParseResult<DesignFile> {
    let mut context_clause = vec![];
    let mut design_units = vec![];

    while let Some(token) = stream.peek()? {
        try_token_kind!(
            token,
            Library => {
                match parse_library_clause(stream) {
                    Ok(library) => {
                        context_clause.push(library.map_into(ContextItem::Library));
                    },
                    Err(msg) => messages.push(msg),
                }
            },
            Use => {
                match parse_use_clause(stream) {
                    Ok(use_clause) => {
                        context_clause.push(use_clause.map_into(ContextItem::Use));
                    },
                    Err(msg) => messages.push(msg),
                }
            },
            Context => match parse_context(stream, messages) {
                Ok(DeclarationOrReference::Declaration(context_decl)) => {
                    design_units.push(to_primary_design_unit(&mut context_clause, PrimaryUnit::ContextDeclaration(context_decl)));
                }
                Ok(DeclarationOrReference::Reference(context_ref)) => {
                    context_clause.push(context_ref.map_into(ContextItem::Context));
                }
                Err(msg) => messages.push(msg),
            },
            Entity => match parse_entity_declaration(stream, messages) {
                Ok(entity) => {
                    design_units.push(to_primary_design_unit(&mut context_clause, PrimaryUnit::EntityDeclaration(entity)));
                }
                Err(msg) => messages.push(msg),
            },

            Architecture => match parse_architecture_body(stream, messages) {
                Ok(architecture) => {
                    design_units.push(to_secondary_design_unit(&mut context_clause, SecondaryUnit::Architecture(architecture)));
                }
                Err(msg) => messages.push(msg),
            },

            Configuration => match parse_configuration_declaration(stream, messages) {
                Ok(configuration) => {
                    design_units.push(to_primary_design_unit(&mut context_clause, PrimaryUnit::Configuration(configuration)));
                }
                Err(msg) => messages.push(msg),
            },
            Package => {
                if stream.is_peek_kinds(&[Package, Body])? {
                    match parse_package_body(stream, messages) {
                        Ok(package_body) => {
                            design_units.push(to_secondary_design_unit(&mut context_clause, SecondaryUnit::PackageBody(package_body)));
                        }
                        Err(msg) => messages.push(msg),
                    };
                } else if stream.is_peek_kinds(&[Package, Identifier, Is, New])? {
                    match parse_package_instantiation(stream) {
                        Ok(inst) => design_units.push(to_primary_design_unit(&mut context_clause, PrimaryUnit::PackageInstance(inst))),
                        Err(msg) => messages.push(msg),
                    }
                } else {
                    match parse_package_declaration(stream, messages) {
                        Ok(package) => {
                            design_units.push(to_primary_design_unit(&mut context_clause, PrimaryUnit::PackageDeclaration(package)))
                        }
                        Err(msg) => messages.push(msg),
                    };
                }
            }
        );
    }

    for context_item in context_clause {
        let message = match context_item.item {
            ContextItem::Library(..) => "Library clause not associated with any design unit",
            ContextItem::Use(..) => "Use clause not associated with any design unit",
            ContextItem::Context(..) => "Context clause not associated with any design unit",
        };
        messages.push(Message::warning(context_item, message));
    }

    Ok(DesignFile { design_units })
}

#[cfg(test)]
mod tests {
    use super::*;

    use ast::*;
    use message::Message;
    use symbol_table::Symbol;
    use test_util::{check_no_messages, Code};

    fn parse_str(code: &str) -> (Code, DesignFile, Vec<Message>) {
        let code = Code::new(code);
        let mut messages = vec![];
        let design_file = code.with_stream(|stream| parse_design_file(stream, &mut messages));
        (code, design_file, messages)
    }

    fn parse_ok(code: &str) -> (Code, DesignFile) {
        let (code, design_file, messages) = parse_str(code);
        check_no_messages(&messages);
        (code, design_file)
    }

    fn to_single_entity(design_file: DesignFile) -> EntityDeclaration {
        match design_file.design_units.as_slice() {
            &[AnyDesignUnit::Primary(DesignUnit {
                unit: PrimaryUnit::EntityDeclaration(ref entity),
                ..
            })] => entity.to_owned(),
            _ => panic!("Expected single entity {:?}", design_file),
        }
    }

    #[test]
    fn parse_empty() {
        let (_, design_file) = parse_ok("");
        assert_eq!(design_file.design_units.len(), 0);
    }

    /// An simple entity with only a name
    fn simple_entity(ident: Ident) -> AnyDesignUnit {
        AnyDesignUnit::Primary(DesignUnit {
            context_clause: vec![],
            unit: PrimaryUnit::EntityDeclaration(EntityDeclaration {
                ident: ident,
                generic_clause: None,
                port_clause: None,
                decl: vec![],
                statements: vec![],
            }),
        })
    }

    #[test]
    fn parse_entity_declaration() {
        let (code, design_file) = parse_ok(
            "
entity myent is
end entity;
",
        );
        assert_eq!(
            design_file.design_units,
            [simple_entity(code.s1("myent").ident())]
        );

        let (code, design_file) = parse_ok(
            "
entity myent is
end entity myent;
",
        );
        assert_eq!(
            design_file.design_units,
            [simple_entity(code.s1("myent").ident())]
        );
    }

    #[test]
    fn parse_entity_generic_clause() {
        let (code, design_file) = parse_ok(
            "
entity myent is
  generic ();
end entity;
",
        );
        assert_eq!(
            to_single_entity(design_file),
            EntityDeclaration {
                ident: code.s1("myent").ident(),
                generic_clause: Some(Vec::new()),
                port_clause: None,
                decl: vec![],
                statements: vec![],
            }
        );
    }

    #[test]
    fn parse_entity_generic_clause_with_values() {
        let (code, design_file) = parse_ok(
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
                    item: code.symbol("myent"),
                    pos: code.s1("myent").pos()
                },
                generic_clause: Some(vec![code.s1("runner_cfg : string").generic()]),
                port_clause: None,
                decl: vec![],
                statements: vec![],
            }
        );
    }

    #[test]
    fn parse_entity_port_clause() {
        let (code, design_file) = parse_ok(
            "
entity myent is
  port ();
end entity;
",
        );
        assert_eq!(
            to_single_entity(design_file),
            EntityDeclaration {
                ident: code.s1("myent").ident(),
                generic_clause: None,
                port_clause: Some(vec![]),
                decl: vec![],
                statements: vec![],
            }
        );
    }

    #[test]
    fn parse_entity_empty_statements() {
        let (code, design_file) = parse_ok(
            "
entity myent is
begin
end entity;
",
        );
        assert_eq!(
            to_single_entity(design_file),
            EntityDeclaration {
                ident: code.s1("myent").ident(),
                generic_clause: None,
                port_clause: None,
                decl: vec![],
                statements: vec![],
            }
        );
    }

    #[test]
    fn parse_entity_declarations() {
        let (code, design_file) = parse_ok(
            "
entity myent is
  constant foo : natural := 0;
end entity;
",
        );
        assert_eq!(
            to_single_entity(design_file),
            EntityDeclaration {
                ident: code.s1("myent").ident(),
                generic_clause: None,
                port_clause: None,
                decl: code.s1("constant foo : natural := 0;").declarative_part(),
                statements: vec![],
            }
        );
    }

    #[test]
    fn parse_entity_statements() {
        let (code, design_file) = parse_ok(
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
                ident: code.s1("myent").ident(),
                generic_clause: None,
                port_clause: None,
                decl: vec![],
                statements: vec![code.s1("check(clk, valid);").concurrent_statement()],
            }
        );
    }

    #[test]
    fn parse_multiple_entity_declarations() {
        let (code, design_file) = parse_ok(
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
            design_file.design_units,
            [
                simple_entity(code.s1("myent").ident()),
                simple_entity(code.s1("myent2").ident()),
                simple_entity(code.s1("myent3").ident()),
                simple_entity(code.s1("myent4").ident())
            ]
        );
    }

    // An simple entity with only a name
    fn simple_architecture(ident: Ident, entity_name: Symbol) -> AnyDesignUnit {
        AnyDesignUnit::Secondary(DesignUnit {
            context_clause: vec![],
            unit: SecondaryUnit::Architecture(ArchitectureBody {
                ident,
                entity_name,
                decl: Vec::new(),
                statements: vec![],
            }),
        })
    }

    #[test]
    fn parse_architecture_body() {
        let (code, design_file) = parse_ok(
            "
architecture arch_name of myent is
begin
end architecture;
",
        );
        assert_eq!(
            design_file.design_units,
            [simple_architecture(
                code.s1("arch_name").ident(),
                code.symbol("myent")
            )]
        );
    }

    #[test]
    fn parse_architecture_body_end_identifier() {
        let (code, design_file) = parse_ok(
            "
architecture arch_name of myent is
begin
end architecture arch_name;
",
        );
        assert_eq!(
            design_file.design_units,
            [simple_architecture(
                code.s1("arch_name").ident(),
                code.symbol("myent")
            )]
        );
    }

    #[test]
    fn parse_architecture_body_end() {
        let (code, design_file) = parse_ok(
            "
architecture arch_name of myent is
begin
end;
",
        );
        assert_eq!(
            design_file.design_units,
            [simple_architecture(
                code.s1("arch_name").ident(),
                code.symbol("myent")
            )]
        );
    }

    #[test]
    fn test_package_declaration() {
        let code = Code::new(
            "
package pkg_name is
end package;
",
        );
        assert_eq!(
            code.with_stream_no_messages(parse_package_declaration),
            PackageDeclaration {
                ident: code.s1("pkg_name").ident(),
                generic_clause: None,
                decl: vec![],
            }
        );
    }

    #[test]
    fn test_package_declaration_with_declarations() {
        let code = Code::new(
            "
package pkg_name is
  type foo;
  constant bar : natural := 0;
end package;
",
        );
        assert_eq!(
            code.with_stream_no_messages(parse_package_declaration),
            PackageDeclaration {
                ident: code.s1("pkg_name").ident(),
                generic_clause: None,
                decl: code
                    .s1("\
  type foo;
  constant bar : natural := 0;
").declarative_part(),
            }
        );
    }

    #[test]
    fn test_package_declaration_generics_clause() {
        let code = Code::new(
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
            code.with_stream_no_messages(parse_package_declaration),
            PackageDeclaration {
                ident: code.s1("pkg_name").ident(),
                generic_clause: Some(vec![
                    code.s1("type foo").generic(),
                    code.s1("type bar").generic()
                ]),
                decl: vec![]
            }
        );
    }

    #[test]
    fn context_clause_associated_with_design_units() {
        let (code, design_file) = parse_ok(
            "
library lib;
use lib.foo;

entity myent is
end entity;
",
        );
        assert_eq!(
            design_file,
            DesignFile {
                design_units: vec![AnyDesignUnit::Primary(DesignUnit {
                    context_clause: vec![
                        code.s1("library lib;")
                            .library_clause()
                            .map_into(ContextItem::Library),
                        code.s1("use lib.foo;")
                            .use_clause()
                            .map_into(ContextItem::Use),
                    ],
                    unit: PrimaryUnit::EntityDeclaration(EntityDeclaration {
                        ident: code.s1("myent").ident(),
                        generic_clause: None,
                        port_clause: None,
                        decl: vec![],
                        statements: vec![],
                    })
                })]
            }
        );
    }

    #[test]
    fn warning_on_orphan_context_clause() {
        let code = Code::new(
            "
library lib;
use lib.foo;
context lib.ctx;
    ",
        );
        let (design_file, messages) = code.with_stream_messages(parse_design_file);
        assert_eq!(
            messages,
            vec![
                Message::warning(
                    code.s1("library lib;"),
                    "Library clause not associated with any design unit"
                ),
                Message::warning(
                    code.s1("use lib.foo;"),
                    "Use clause not associated with any design unit"
                ),
                Message::warning(
                    code.s1("context lib.ctx;"),
                    "Context clause not associated with any design unit"
                ),
            ]
        );
        assert_eq!(
            design_file,
            DesignFile {
                design_units: vec![]
            }
        );
    }
}
