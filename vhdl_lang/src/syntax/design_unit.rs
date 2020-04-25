// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::tokens::{Kind::*, TokenStream};

use super::common::error_on_end_identifier_mismatch;
use super::common::ParseResult;
use super::component_declaration::{parse_optional_generic_list, parse_optional_port_list};
use super::concurrent_statement::parse_labeled_concurrent_statements;
use super::configuration::parse_configuration_declaration;
use super::context::{
    parse_context, parse_library_clause, parse_use_clause, DeclarationOrReference,
};
use super::declarative_part::{
    parse_declarative_part, parse_declarative_part_leave_end_token, parse_package_instantiation,
};
use super::interface_declaration::parse_generic_interface_list;
use crate::ast::*;
use crate::data::*;

/// Parse an entity declaration, token is initial entity token
/// If a parse error occurs the stream is consumed until and end entity
pub fn parse_entity_declaration(
    stream: &mut TokenStream,
    diagnostics: &mut dyn DiagnosticHandler,
) -> ParseResult<EntityDeclaration> {
    let entity_token = stream.expect_kind(Entity)?;

    let ident = stream.expect_ident()?;
    stream.expect_kind(Is)?;

    let generic_clause = parse_optional_generic_list(stream, diagnostics)?;
    let port_clause = parse_optional_port_list(stream, diagnostics)?;

    let decl = parse_declarative_part_leave_end_token(stream, diagnostics)?;

    let token = stream.expect()?;
    let statements = try_token_kind!(
        token,
        End => Vec::new(),
        Begin => parse_labeled_concurrent_statements(stream, diagnostics)?
    );
    stream.pop_if_kind(Entity)?;
    let end_ident = stream.pop_optional_ident()?;
    if let Some(diagnostic) = error_on_end_identifier_mismatch(&ident, &end_ident) {
        diagnostics.push(diagnostic);
    }
    let semi_token = stream.expect_kind(SemiColon)?;
    Ok(EntityDeclaration {
        context_clause: ContextClause::default(),
        ident,
        generic_clause,
        port_clause,
        decl,
        statements,
        source_range: entity_token.pos.combine_into(&semi_token),
    })
}

/// LRM 3.3.1
pub fn parse_architecture_body(
    stream: &mut TokenStream,
    diagnostics: &mut dyn DiagnosticHandler,
) -> ParseResult<ArchitectureBody> {
    let architecture_token = stream.expect_kind(Architecture)?;
    let ident = stream.expect_ident()?;
    stream.expect_kind(Of)?;
    let entity_name = stream.expect_ident()?;
    stream.expect_kind(Is)?;

    let decl = parse_declarative_part(stream, diagnostics, true)?;

    let statements = parse_labeled_concurrent_statements(stream, diagnostics)?;
    stream.pop_if_kind(Architecture)?;

    let end_ident = stream.pop_optional_ident()?;
    if let Some(diagnostic) = error_on_end_identifier_mismatch(&ident, &end_ident) {
        diagnostics.push(diagnostic);
    }

    let semi_token = stream.expect_kind(SemiColon)?;

    Ok(ArchitectureBody {
        context_clause: ContextClause::default(),
        ident,
        entity_name: entity_name.into_ref(),
        decl,
        statements,
        source_range: architecture_token.pos.combine_into(&semi_token),
    })
}

/// LRM 4.7 Package declarations
pub fn parse_package_declaration(
    stream: &mut TokenStream,
    diagnostics: &mut dyn DiagnosticHandler,
) -> ParseResult<PackageDeclaration> {
    let package_token = stream.expect_kind(Package)?;
    let ident = stream.expect_ident()?;

    stream.expect_kind(Is)?;
    let generic_clause = {
        if stream.skip_if_kind(Generic)? {
            let decl = parse_generic_interface_list(stream, diagnostics)?;
            stream.expect_kind(SemiColon)?;
            Some(decl)
        } else {
            None
        }
    };
    let decl = parse_declarative_part(stream, diagnostics, false)?;
    stream.pop_if_kind(Package)?;
    let end_ident = stream.pop_optional_ident()?;
    if let Some(diagnostic) = error_on_end_identifier_mismatch(&ident, &end_ident) {
        diagnostics.push(diagnostic);
    }
    stream.pop_if_kind(Identifier)?;
    let semi_token = stream.expect_kind(SemiColon)?;
    Ok(PackageDeclaration {
        context_clause: ContextClause::default(),
        ident,
        generic_clause,
        decl,
        source_range: package_token.pos.combine_into(&semi_token),
    })
}

/// LRM 4.8 Package bodies
pub fn parse_package_body(
    stream: &mut TokenStream,
    diagnostics: &mut dyn DiagnosticHandler,
) -> ParseResult<PackageBody> {
    let package_token = stream.expect_kind(Package)?;
    stream.expect_kind(Body)?;
    let ident = stream.expect_ident()?;

    stream.expect_kind(Is)?;
    let decl = parse_declarative_part(stream, diagnostics, false)?;
    if stream.skip_if_kind(Package)? {
        stream.expect_kind(Body)?;
    }
    let end_ident = stream.pop_optional_ident()?;
    if let Some(diagnostic) = error_on_end_identifier_mismatch(&ident, &end_ident) {
        diagnostics.push(diagnostic);
    }
    let semi_token = stream.expect_kind(SemiColon)?;

    Ok(PackageBody {
        context_clause: ContextClause::default(),
        ident: ident.into_ref(),
        decl,
        source_range: package_token.pos.combine_into(&semi_token),
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
    stream: &mut TokenStream,
    diagnostics: &mut dyn DiagnosticHandler,
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
                            diagnostic.add_related(&context_item, context_item_message(&context_item.item, "may not come before context declaration"));
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
                    if let Some(ref context_item) = context_clause.first() {
                        entity.source_range = entity.source_range.combine_into(context_item)
                    }
                    entity.context_clause = take_context_clause(&mut context_clause);
                    design_units.push(AnyDesignUnit::Primary(AnyPrimaryUnit::Entity(entity)));
                }
                Err(diagnostic) => diagnostics.push(diagnostic),
            },

            Architecture => match parse_architecture_body(stream, diagnostics) {
                Ok(mut architecture) => {
                    if let Some(ref context_item) = context_clause.first() {
                        architecture.source_range = architecture.source_range.combine_into(context_item)
                    }
                    architecture.context_clause = take_context_clause(&mut context_clause);
                    design_units.push(AnyDesignUnit::Secondary(AnySecondaryUnit::Architecture(architecture)));
                }
                Err(diagnostic) => diagnostics.push(diagnostic),
            },

            Configuration => match parse_configuration_declaration(stream, diagnostics) {
                Ok(mut configuration) => {
                    if let Some(ref context_item) = context_clause.first() {
                        configuration.source_range = configuration.source_range.combine_into(context_item)
                    }
                    configuration.context_clause = take_context_clause(&mut context_clause);
                    design_units.push(AnyDesignUnit::Primary(AnyPrimaryUnit::Configuration(configuration)));
                }
                Err(diagnostic) => diagnostics.push(diagnostic),
            },
            Package => {
                if stream.next_kinds_are(&[Package, Body])? {
                    match parse_package_body(stream, diagnostics) {
                        Ok(mut package_body) => {
                            if let Some(ref context_item) = context_clause.first() {
                                package_body.source_range = package_body.source_range.combine_into(context_item)
                            }
                            package_body.context_clause = take_context_clause(&mut context_clause);
                            design_units.push(AnyDesignUnit::Secondary(AnySecondaryUnit::PackageBody(package_body)));
                        }
                        Err(diagnostic) => diagnostics.push(diagnostic),
                    };
                } else if stream.next_kinds_are(&[Package, Identifier, Is, New])? {
                    match parse_package_instantiation(stream) {
                        Ok(mut inst) => {
                            if let Some(ref context_item) = context_clause.first() {
                                inst.source_range = inst.source_range.combine_into(context_item)
                            }
                            inst.context_clause = take_context_clause(&mut context_clause);
                            design_units.push(AnyDesignUnit::Primary(AnyPrimaryUnit::PackageInstance(inst)))
                        },
                        Err(diagnostic) => diagnostics.push(diagnostic),
                    }
                } else {
                    match parse_package_declaration(stream, diagnostics) {
                        Ok(mut package) => {
                            if let Some(ref context_item) = context_clause.first() {
                                package.source_range = package.source_range.combine_into(context_item)
                            }
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
    use crate::syntax::test::{check_diagnostics, check_no_diagnostics, source_range, Code};
    use pretty_assertions::assert_eq;

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
            &[AnyDesignUnit::Primary(AnyPrimaryUnit::Entity(ref entity))] => entity.to_owned(),
            _ => panic!("Expected single entity {:?}", design_file),
        }
    }

    #[test]
    fn parse_empty() {
        let (_, design_file) = parse_ok("");
        assert_eq!(design_file.design_units.len(), 0);
    }

    /// An simple entity with only a name
    fn simple_entity(ident: Ident, source_range: SrcPos) -> AnyDesignUnit {
        AnyDesignUnit::Primary(AnyPrimaryUnit::Entity(EntityDeclaration {
            context_clause: ContextClause::default(),
            ident,
            generic_clause: None,
            port_clause: None,
            decl: vec![],
            statements: vec![],
            source_range: source_range,
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
            [simple_entity(
                code.s1("myent").ident(),
                source_range(&code, (1, 0), (2, 11))
            )]
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
                source_range(&code, (1, 0), (2, 17))
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
                ident: code.s1("myent").ident(),
                generic_clause: Some(Vec::new()),
                port_clause: None,
                decl: vec![],
                statements: vec![],
                source_range: source_range(&code, (1, 0), (3, 11)),
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
                ident: Ident {
                    item: code.symbol("myent"),
                    pos: code.s1("myent").pos()
                },
                generic_clause: Some(vec![code.s1("runner_cfg : string").generic()]),
                port_clause: None,
                decl: vec![],
                statements: vec![],
                source_range: source_range(&code, (1, 0), (4, 11)),
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
                ident: code.s1("myent").ident(),
                generic_clause: None,
                port_clause: Some(vec![]),
                decl: vec![],
                statements: vec![],
                source_range: source_range(&code, (1, 0), (3, 11)),
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
                ident: code.s1("myent").ident(),
                generic_clause: None,
                port_clause: None,
                decl: vec![],
                statements: vec![],
                source_range: source_range(&code, (1, 0), (3, 11)),
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
                ident: code.s1("myent").ident(),
                generic_clause: None,
                port_clause: None,
                decl: code.s1("constant foo : natural := 0;").declarative_part(),
                statements: vec![],
                source_range: source_range(&code, (1, 0), (3, 11)),
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
                ident: code.s1("myent").ident(),
                generic_clause: None,
                port_clause: None,
                decl: vec![],
                statements: vec![code.s1("check(clk, valid);").concurrent_statement()],
                source_range: source_range(&code, (1, 0), (4, 11)),
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
                simple_entity(
                    code.s1("myent").ident(),
                    source_range(&code, (1, 0), (2, 11))
                ),
                simple_entity(
                    code.s1("myent2").ident(),
                    source_range(&code, (4, 0), (5, 18))
                ),
                simple_entity(
                    code.s1("myent3").ident(),
                    source_range(&code, (7, 0), (8, 11))
                ),
                simple_entity(
                    code.s1("myent4").ident(),
                    source_range(&code, (10, 0), (11, 4))
                )
            ]
        );
    }

    // An simple entity with only a name
    fn simple_architecture(
        ident: Ident,
        entity_name: Ident,
        source_range: SrcPos,
    ) -> AnyDesignUnit {
        AnyDesignUnit::Secondary(AnySecondaryUnit::Architecture(ArchitectureBody {
            context_clause: ContextClause::default(),
            ident,
            entity_name: entity_name.into_ref(),
            decl: Vec::new(),
            statements: vec![],
            source_range: source_range,
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
                code.s1("arch_name").ident(),
                code.s1("myent").ident(),
                source_range(&code, (1, 0), (3, 17))
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
                code.s1("myent").ident(),
                source_range(&code, (1, 0), (3, 27))
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
                code.s1("myent").ident(),
                source_range(&code, (1, 0), (3, 4))
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
                ident: code.s1("pkg_name").ident(),
                generic_clause: None,
                decl: vec![],
                source_range: source_range(&code, (1, 0), (2, 12)),
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
                ident: code.s1("pkg_name").ident(),
                generic_clause: None,
                decl: code
                    .s1("\
  type foo;
  constant bar : natural := 0;
")
                    .declarative_part(),
                source_range: source_range(&code, (1, 0), (4, 12)),
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
                ident: code.s1("pkg_name").ident(),
                generic_clause: Some(vec![
                    code.s1("type foo").generic(),
                    code.s1("type bar").generic()
                ]),
                decl: vec![],
                source_range: source_range(&code, (1, 0), (6, 12)),
            }
        );
    }

    #[test]
    fn context_clause_associated_with_entity_declaration() {
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
                        ident: code.s1("myent").ident(),
                        generic_clause: None,
                        port_clause: None,
                        decl: vec![],
                        statements: vec![],
                        source_range: source_range(&code, (1, 0), (5, 11)),
                    }
                ))]
            }
        );
    }

    #[test]
    fn context_clause_associated_with_architecture_body() {
        let (code, design_file) = parse_ok(
            "
library lib;
use lib.foo;

architecture rtl of myent is
begin
end;

",
        );
        assert_eq!(
            design_file,
            DesignFile {
                design_units: vec![AnyDesignUnit::Secondary(AnySecondaryUnit::Architecture(
                    ArchitectureBody {
                        context_clause: vec![
                            code.s1("library lib;")
                                .library_clause()
                                .map_into(ContextItem::Library),
                            code.s1("use lib.foo;")
                                .use_clause()
                                .map_into(ContextItem::Use),
                        ],
                        ident: code.s1("rtl").ident(),
                        entity_name: code.s1("myent").ident().into_ref(),
                        decl: Vec::new(),
                        statements: vec![],
                        source_range: source_range(&code, (1, 0), (6, 4)),
                    }
                ))],
            }
        );
    }

    #[test]
    fn context_clause_associated_with_package_declaration() {
        let (code, design_file) = parse_ok(
            "
library lib;
use lib.foo;

package pkg_name is
end;

",
        );
        assert_eq!(
            design_file,
            DesignFile {
                design_units: vec![AnyDesignUnit::Primary(AnyPrimaryUnit::Package(
                    PackageDeclaration {
                        context_clause: vec![
                            code.s1("library lib;")
                                .library_clause()
                                .map_into(ContextItem::Library),
                            code.s1("use lib.foo;")
                                .use_clause()
                                .map_into(ContextItem::Use),
                        ],
                        ident: code.s1("pkg_name").ident(),
                        generic_clause: None,
                        decl: vec![],
                        source_range: source_range(&code, (1, 0), (5, 4)),
                    }
                ))],
            }
        );
    }

    #[test]
    fn context_clause_associated_with_package_body() {
        let (code, design_file) = parse_ok(
            "
library lib;
use lib.foo;

package body pkg_name is
end;

",
        );
        assert_eq!(
            design_file,
            DesignFile {
                design_units: vec![AnyDesignUnit::Secondary(AnySecondaryUnit::PackageBody(
                    PackageBody {
                        context_clause: vec![
                            code.s1("library lib;")
                                .library_clause()
                                .map_into(ContextItem::Library),
                            code.s1("use lib.foo;")
                                .use_clause()
                                .map_into(ContextItem::Use),
                        ],
                        ident: code.s1("pkg_name").ident().into_ref(),
                        decl: vec![],
                        source_range: source_range(&code, (1, 0), (5, 4)),
                    }
                ))],
            }
        );
    }

    #[test]
    fn context_clause_associated_with_configuration_declaration() {
        let (code, design_file) = parse_ok(
            "
library lib;
use lib.foo;

configuration cfg of entity_name is
  for rtl(0)
  end for;
end;
",
        );
        assert_eq!(
            design_file,
            DesignFile {
                design_units: vec![AnyDesignUnit::Primary(AnyPrimaryUnit::Configuration(
                    ConfigurationDeclaration {
                        context_clause: vec![
                            code.s1("library lib;")
                                .library_clause()
                                .map_into(ContextItem::Library),
                            code.s1("use lib.foo;")
                                .use_clause()
                                .map_into(ContextItem::Use),
                        ],
                        ident: code.s1("cfg").ident(),
                        entity_name: code.s1("entity_name").selected_name(),
                        decl: vec![],
                        vunit_bind_inds: Vec::new(),
                        block_config: BlockConfiguration {
                            block_spec: code.s1("rtl(0)").name(),
                            use_clauses: vec![],
                            items: vec![],
                        },
                        source_range: source_range(&code, (1, 0), (7, 4))
                    }
                ))],
            }
        );
    }

    #[test]
    fn context_clause_associated_with_package_instantiation() {
        let (code, design_file) = parse_ok(
            "
library lib;
use lib.foo;

package ident is new lib.foo.bar;
",
        );
        assert_eq!(
            design_file,
            DesignFile {
                design_units: vec![AnyDesignUnit::Primary(AnyPrimaryUnit::PackageInstance(
                    PackageInstantiation {
                        context_clause: vec![
                            code.s1("library lib;")
                                .library_clause()
                                .map_into(ContextItem::Library),
                            code.s1("use lib.foo;")
                                .use_clause()
                                .map_into(ContextItem::Use),
                        ],
                        ident: code.s1("ident").ident(),
                        package_name: code.s1("lib.foo.bar").selected_name(),
                        generic_map: None,
                        source_range: source_range(&code, (1, 0), (4, 33)),
                    }
                ))],
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
