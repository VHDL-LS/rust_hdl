// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::tokens::{Kind::*, TokenStream};

use super::common::check_end_identifier_mismatch;
use super::common::ParseResult;
use super::component_declaration::{parse_optional_generic_list, parse_optional_port_list};
use super::concurrent_statement::parse_labeled_concurrent_statements;
use super::configuration::parse_configuration_declaration;
use super::context::{
    parse_context, parse_library_clause, parse_use_clause, DeclarationOrReference,
};
use super::declarative_part::{parse_declarative_part, parse_package_instantiation};
use super::interface_declaration::parse_generic_interface_list;
use crate::ast::*;
use crate::data::*;

/// Parse an entity declaration, token is initial entity token
/// If a parse error occurs the stream is consumed until and end entity
pub fn parse_entity_declaration(
    stream: &TokenStream,
    diagnostics: &mut dyn DiagnosticHandler,
) -> ParseResult<EntityDeclaration> {
    stream.expect_kind(Entity)?;

    let ident = WithDecl::new(stream.expect_ident()?);
    stream.expect_kind(Is)?;

    let generic_clause = parse_optional_generic_list(stream, diagnostics)?;
    let port_clause = parse_optional_port_list(stream, diagnostics)?;

    let decl = parse_declarative_part(stream, diagnostics)?;

    let statements = if stream.skip_if_kind(Begin) {
        parse_labeled_concurrent_statements(stream, diagnostics)?
    } else {
        Vec::new()
    };
    stream.pop_if_kind(End);
    stream.pop_if_kind(Entity);
    let end_ident = stream.pop_optional_ident();
    stream.expect_kind(SemiColon)?;
    Ok(EntityDeclaration {
        context_clause: ContextClause::default(),
        end_ident_pos: check_end_identifier_mismatch(&ident.tree, end_ident, diagnostics),
        ident,
        generic_clause,
        port_clause,
        decl,
        statements,
    })
}

/// LRM 3.3.1
pub fn parse_architecture_body(
    stream: &TokenStream,
    diagnostics: &mut dyn DiagnosticHandler,
) -> ParseResult<ArchitectureBody> {
    stream.expect_kind(Architecture)?;
    let ident = WithDecl::new(stream.expect_ident()?);
    stream.expect_kind(Of)?;
    let entity_name = stream.expect_ident()?;
    stream.expect_kind(Is)?;

    let decl = parse_declarative_part(stream, diagnostics)?;
    stream.expect_kind(Begin)?;

    let statements = parse_labeled_concurrent_statements(stream, diagnostics)?;
    stream.expect_kind(End)?;
    stream.pop_if_kind(Architecture);

    let end_ident = stream.pop_optional_ident();
    stream.expect_kind(SemiColon)?;

    Ok(ArchitectureBody {
        context_clause: ContextClause::default(),
        end_ident_pos: check_end_identifier_mismatch(&ident.tree, end_ident, diagnostics),
        ident,
        entity_name: entity_name.into_ref(),
        decl,
        statements,
    })
}

/// LRM 4.7 Package declarations
pub fn parse_package_declaration(
    stream: &TokenStream,
    diagnostics: &mut dyn DiagnosticHandler,
) -> ParseResult<PackageDeclaration> {
    stream.expect_kind(Package)?;
    let ident = WithDecl::new(stream.expect_ident()?);

    stream.expect_kind(Is)?;
    let generic_clause = {
        if stream.skip_if_kind(Generic) {
            let decl = parse_generic_interface_list(stream, diagnostics)?;
            stream.expect_kind(SemiColon)?;
            Some(decl)
        } else {
            None
        }
    };
    let decl = parse_declarative_part(stream, diagnostics)?;
    stream.expect_kind(End)?;
    stream.pop_if_kind(Package);
    let end_ident = stream.pop_optional_ident();
    stream.expect_kind(SemiColon)?;
    Ok(PackageDeclaration {
        context_clause: ContextClause::default(),
        end_ident_pos: check_end_identifier_mismatch(&ident.tree, end_ident, diagnostics),
        ident,
        generic_clause,
        decl,
    })
}

/// LRM 4.8 Package bodies
pub fn parse_package_body(
    stream: &TokenStream,
    diagnostics: &mut dyn DiagnosticHandler,
) -> ParseResult<PackageBody> {
    stream.expect_kind(Package)?;
    stream.expect_kind(Body)?;
    let ident = stream.expect_ident()?;

    stream.expect_kind(Is)?;
    let decl = parse_declarative_part(stream, diagnostics)?;
    stream.expect_kind(End)?;
    if stream.skip_if_kind(Package) {
        stream.expect_kind(Body)?;
    }
    let end_ident = stream.pop_optional_ident();
    stream.expect_kind(SemiColon)?;

    Ok(PackageBody {
        context_clause: ContextClause::default(),
        decl,
        end_ident_pos: check_end_identifier_mismatch(&ident, end_ident, diagnostics),
        ident: ident.into(),
    })
}

fn take_context_clause(context_clause: &mut ContextClause) -> ContextClause {
    std::mem::take(context_clause)
}

fn context_item_message(context_item: &ContextItem, message: impl AsRef<str>) -> String {
    let prefix = match context_item {
        ContextItem::Library(..) => "Library clause",
        ContextItem::Use(..) => "Use clause",
        ContextItem::Context(..) => "Context reference",
    };

    format!("{} {}", prefix, message.as_ref())
}

pub fn parse_design_file(
    stream: &TokenStream,
    diagnostics: &mut dyn DiagnosticHandler,
) -> ParseResult<DesignFile> {
    let mut context_clause = vec![];
    let mut design_units = vec![];

    while let Some(token) = stream.peek() {
        try_init_token_kind!(
            token,
            Library => {
                match parse_library_clause(stream) {
                    Ok(library) => {
                        context_clause.push(library.map_into(ContextItem::Library));
                    },
                    Err(diagnostic) => diagnostics.push(diagnostic),
                }
            },
            Use => {
                match parse_use_clause(stream) {
                    Ok(use_clause) => {
                        context_clause.push(use_clause.map_into(ContextItem::Use));
                    },
                    Err(diagnostic) => diagnostics.push(diagnostic),
                }
            },
            Context => match parse_context(stream, diagnostics) {
                Ok(DeclarationOrReference::Declaration(context_decl)) => {
                    if !context_clause.is_empty() {
                        let mut diagnostic = Diagnostic::error(&context_decl.ident, "Context declaration may not be preceeded by a context clause");

                        for context_item in context_clause.iter() {
                            diagnostic.add_related(context_item, context_item_message(&context_item.item, "may not come before context declaration"));
                        }

                        diagnostics.push(diagnostic);
                        context_clause.clear();
                    }

                    design_units.push(AnyDesignUnit::Primary(AnyPrimaryUnit::Context(context_decl)));
                }
                Ok(DeclarationOrReference::Reference(context_ref)) => {
                    context_clause.push(context_ref.map_into(ContextItem::Context));
                }
                Err(diagnostic) => diagnostics.push(diagnostic),
            },
            Entity => match parse_entity_declaration(stream, diagnostics) {
                Ok(mut entity) => {
                    entity.context_clause = take_context_clause(&mut context_clause);
                    design_units.push(AnyDesignUnit::Primary(AnyPrimaryUnit::Entity(entity)));
                }
                Err(diagnostic) => diagnostics.push(diagnostic),
            },

            Architecture => match parse_architecture_body(stream, diagnostics) {
                Ok(mut architecture) => {
                    architecture.context_clause = take_context_clause(&mut context_clause);
                    design_units.push(AnyDesignUnit::Secondary(AnySecondaryUnit::Architecture(architecture)));
                }
                Err(diagnostic) => diagnostics.push(diagnostic),
            },

            Configuration => match parse_configuration_declaration(stream, diagnostics) {
                Ok(mut configuration) => {
                    configuration.context_clause = take_context_clause(&mut context_clause);
                    design_units.push(AnyDesignUnit::Primary(AnyPrimaryUnit::Configuration(configuration)));
                }
                Err(diagnostic) => diagnostics.push(diagnostic),
            },
            Package => {
                if stream.next_kinds_are(&[Package, Body]) {
                    match parse_package_body(stream, diagnostics) {
                        Ok(mut package_body) => {
                            package_body.context_clause = take_context_clause(&mut context_clause);
                            design_units.push(AnyDesignUnit::Secondary(AnySecondaryUnit::PackageBody(package_body)));
                        }
                        Err(diagnostic) => diagnostics.push(diagnostic),
                    };
                } else if stream.next_kinds_are(&[Package, Identifier, Is, New]) {
                    match parse_package_instantiation(stream) {
                        Ok(mut inst) => {
                            inst.context_clause = take_context_clause(&mut context_clause);
                            design_units.push(AnyDesignUnit::Primary(AnyPrimaryUnit::PackageInstance(inst)))
                        },
                        Err(diagnostic) => diagnostics.push(diagnostic),
                    }
                } else {
                    match parse_package_declaration(stream, diagnostics) {
                        Ok(mut package) => {
                            package.context_clause = take_context_clause(&mut context_clause);
                            design_units.push(AnyDesignUnit::Primary(AnyPrimaryUnit::Package(package)))
                        }
                        Err(diagnostic) => diagnostics.push(diagnostic),
                    };
                }
            }
        );
    }

    for context_item in context_clause {
        diagnostics.push(Diagnostic::warning(
            &context_item,
            context_item_message(&context_item.item, "not associated with any design unit"),
        ));
    }

    Ok(DesignFile { design_units })
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::data::Diagnostic;
    use crate::syntax::test::{check_diagnostics, check_no_diagnostics, Code};

    fn parse_str(code: &str) -> (Code, DesignFile, Vec<Diagnostic>) {
        let code = Code::new(code);
        let mut diagnostics = vec![];
        let design_file = code.with_stream(|stream| parse_design_file(stream, &mut diagnostics));
        (code, design_file, diagnostics)
    }

    fn parse_ok(code: &str) -> (Code, DesignFile) {
        let (code, design_file, diagnostics) = parse_str(code);
        check_no_diagnostics(&diagnostics);
        (code, design_file)
    }

    fn to_single_entity(design_file: DesignFile) -> EntityDeclaration {
        match design_file.design_units.as_slice() {
            [AnyDesignUnit::Primary(AnyPrimaryUnit::Entity(ref entity))] => entity.to_owned(),
            _ => panic!("Expected single entity {design_file:?}"),
        }
    }

    #[test]
    fn parse_empty() {
        let (_, design_file) = parse_ok("");
        assert_eq!(design_file.design_units.len(), 0);
    }

    /// An simple entity with only a name
    fn simple_entity(ident: Ident, end_ident_pos: Option<SrcPos>) -> AnyDesignUnit {
        AnyDesignUnit::Primary(AnyPrimaryUnit::Entity(EntityDeclaration {
            context_clause: ContextClause::default(),
            ident: ident.into(),
            generic_clause: None,
            port_clause: None,
            decl: vec![],
            statements: vec![],
            end_ident_pos,
        }))
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
            [simple_entity(code.s1("myent").ident(), None)]
        );

        let (code, design_file) = parse_ok(
            "
entity myent is
end entity myent;
",
        );
        assert_eq!(
            design_file.design_units,
            [simple_entity(
                code.s1("myent").ident(),
                Some(code.s("myent", 2).pos())
            )]
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
                context_clause: ContextClause::default(),
                ident: code.s1("myent").decl_ident(),
                generic_clause: Some(Vec::new()),
                port_clause: None,
                decl: vec![],
                statements: vec![],
                end_ident_pos: None,
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
                context_clause: ContextClause::default(),
                ident: code.s1("myent").decl_ident(),
                generic_clause: Some(vec![code.s1("runner_cfg : string").generic()]),
                port_clause: None,
                decl: vec![],
                statements: vec![],
                end_ident_pos: None,
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
                context_clause: ContextClause::default(),
                ident: code.s1("myent").decl_ident(),
                generic_clause: None,
                port_clause: Some(vec![]),
                decl: vec![],
                statements: vec![],
                end_ident_pos: None,
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
                context_clause: ContextClause::default(),
                ident: code.s1("myent").decl_ident(),
                generic_clause: None,
                port_clause: None,
                decl: vec![],
                statements: vec![],
                end_ident_pos: None,
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
                context_clause: ContextClause::default(),
                ident: code.s1("myent").decl_ident(),
                generic_clause: None,
                port_clause: None,
                decl: code.s1("constant foo : natural := 0;").declarative_part(),
                statements: vec![],
                end_ident_pos: None,
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
                context_clause: ContextClause::default(),
                ident: code.s1("myent").decl_ident(),
                generic_clause: None,
                port_clause: None,
                decl: vec![],
                statements: vec![code.s1("check(clk, valid);").concurrent_statement()],
                end_ident_pos: None,
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
                simple_entity(code.s1("myent").ident(), None),
                simple_entity(code.s1("myent2").ident(), Some(code.s("myent2", 2).pos())),
                simple_entity(code.s1("myent3").ident(), Some(code.s("myent3", 2).pos())),
                simple_entity(code.s1("myent4").ident(), None)
            ]
        );
    }

    // An simple entity with only a name
    fn simple_architecture(
        ident: WithDecl<Ident>,
        entity_name: Ident,
        end_ident_pos: Option<SrcPos>,
    ) -> AnyDesignUnit {
        AnyDesignUnit::Secondary(AnySecondaryUnit::Architecture(ArchitectureBody {
            context_clause: ContextClause::default(),
            ident,
            entity_name: entity_name.into_ref(),
            decl: Vec::new(),
            statements: vec![],
            end_ident_pos,
        }))
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
                WithDecl::new(code.s1("arch_name").ident()),
                code.s1("myent").ident(),
                None,
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
                WithDecl::new(code.s1("arch_name").ident()),
                code.s1("myent").ident(),
                Some(code.s("arch_name", 2).pos())
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
                WithDecl::new(code.s1("arch_name").ident()),
                code.s1("myent").ident(),
                None
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
            code.with_stream_no_diagnostics(parse_package_declaration),
            PackageDeclaration {
                context_clause: ContextClause::default(),
                ident: code.s1("pkg_name").decl_ident(),
                generic_clause: None,
                decl: vec![],
                end_ident_pos: None
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
            code.with_stream_no_diagnostics(parse_package_declaration),
            PackageDeclaration {
                context_clause: ContextClause::default(),
                ident: code.s1("pkg_name").decl_ident(),
                generic_clause: None,
                decl: code
                    .s1("\
  type foo;
  constant bar : natural := 0;
")
                    .declarative_part(),
                end_ident_pos: None
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
            code.with_stream_no_diagnostics(parse_package_declaration),
            PackageDeclaration {
                context_clause: ContextClause::default(),
                ident: code.s1("pkg_name").decl_ident(),
                generic_clause: Some(vec![
                    code.s1("type foo").generic(),
                    code.s1("type bar").generic()
                ]),
                decl: vec![],
                end_ident_pos: None
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
                design_units: vec![AnyDesignUnit::Primary(AnyPrimaryUnit::Entity(
                    EntityDeclaration {
                        context_clause: vec![
                            code.s1("library lib;")
                                .library_clause()
                                .map_into(ContextItem::Library),
                            code.s1("use lib.foo;")
                                .use_clause()
                                .map_into(ContextItem::Use),
                        ],
                        ident: code.s1("myent").decl_ident(),
                        generic_clause: None,
                        port_clause: None,
                        decl: vec![],
                        statements: vec![],
                        end_ident_pos: None
                    }
                ))]
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
        let (design_file, diagnostics) = code.with_stream_diagnostics(parse_design_file);
        check_diagnostics(
            diagnostics,
            vec![
                Diagnostic::warning(
                    code.s1("library lib;"),
                    "Library clause not associated with any design unit",
                ),
                Diagnostic::warning(
                    code.s1("use lib.foo;"),
                    "Use clause not associated with any design unit",
                ),
                Diagnostic::warning(
                    code.s1("context lib.ctx;"),
                    "Context reference not associated with any design unit",
                ),
            ],
        );
        assert_eq!(
            design_file,
            DesignFile {
                design_units: vec![]
            }
        );
    }

    #[test]
    fn error_on_context_clause_before_context_declaration() {
        let code = Code::new(
            "
library lib;
use lib.pkg;
context lib.c;

context ctx is
end context;

entity ent is
end entity;
    ",
        );
        let (design_file, diagnostics) = code.with_stream_diagnostics(parse_design_file);
        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code.s1("ctx"),
                "Context declaration may not be preceeded by a context clause",
            )
            .related(
                code.s1("library lib;"),
                "Library clause may not come before context declaration",
            )
            .related(
                code.s1("use lib.pkg;"),
                "Use clause may not come before context declaration",
            )
            .related(
                code.s1("context lib.c;"),
                "Context reference may not come before context declaration",
            )],
        );

        match design_file.design_units.get(1).unwrap() {
            AnyDesignUnit::Primary(AnyPrimaryUnit::Entity(entity)) => {
                assert_eq!(entity.context_clause.len(), 0);
            }
            _ => panic!("Expected entity"),
        }
    }
}
