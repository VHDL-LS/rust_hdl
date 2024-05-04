// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::tokens::{HasTokenSpan, Kind::*, TokenSpan};
use vhdl_lang::syntax::parser::ParsingContext;

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
use crate::data::error_codes::ErrorCode;
use crate::data::*;

/// Parse an entity declaration, token is initial entity token
/// If a parse error occurs the stream is consumed until and end entity
pub fn parse_entity_declaration(ctx: &mut ParsingContext<'_>) -> ParseResult<EntityDeclaration> {
    let start_token = ctx.stream.expect_kind(Entity)?;

    let ident = WithDecl::new(ctx.stream.expect_ident()?);
    ctx.stream.expect_kind(Is)?;

    let generic_clause = parse_optional_generic_list(ctx)?;
    let port_clause = parse_optional_port_list(ctx)?;

    let decl = parse_declarative_part(ctx)?;

    let statements = if ctx.stream.skip_if_kind(Begin) {
        parse_labeled_concurrent_statements(ctx)?
    } else {
        Vec::new()
    };
    ctx.stream.pop_if_kind(End);
    ctx.stream.pop_if_kind(Entity);
    let end_ident = ctx.stream.pop_optional_ident();
    let end_token = ctx.stream.expect_kind(SemiColon)?;
    Ok(EntityDeclaration {
        span: TokenSpan::new(start_token, end_token),
        context_clause: ContextClause::default(),
        end_ident_pos: check_end_identifier_mismatch(ctx, &ident.tree, end_ident),
        ident,
        generic_clause,
        port_clause,
        decl,
        statements,
    })
}

/// LRM 3.3.1
pub fn parse_architecture_body(ctx: &mut ParsingContext<'_>) -> ParseResult<ArchitectureBody> {
    let start_token = ctx.stream.expect_kind(Architecture)?;
    let ident = WithDecl::new(ctx.stream.expect_ident()?);
    ctx.stream.expect_kind(Of)?;
    let entity_name = ctx.stream.expect_ident()?;
    ctx.stream.expect_kind(Is)?;

    let decl = parse_declarative_part(ctx)?;
    let begin_token = ctx.stream.expect_kind(Begin)?;

    let statements = parse_labeled_concurrent_statements(ctx)?;
    ctx.stream.expect_kind(End)?;
    ctx.stream.pop_if_kind(Architecture);

    let end_ident = ctx.stream.pop_optional_ident();
    let end_token = ctx.stream.expect_kind(SemiColon)?;

    Ok(ArchitectureBody {
        span: TokenSpan::new(start_token, end_token),
        context_clause: ContextClause::default(),
        end_ident_pos: check_end_identifier_mismatch(ctx, &ident.tree, end_ident),
        begin_token,
        ident,
        entity_name: entity_name.into_ref(),
        decl,
        statements,
    })
}

/// LRM 4.7 Package declarations
pub fn parse_package_declaration(ctx: &mut ParsingContext<'_>) -> ParseResult<PackageDeclaration> {
    let start_token = ctx.stream.expect_kind(Package)?;
    let ident = WithDecl::new(ctx.stream.expect_ident()?);

    ctx.stream.expect_kind(Is)?;
    let generic_clause = {
        if ctx.stream.skip_if_kind(Generic) {
            let decl = parse_generic_interface_list(ctx)?;
            ctx.stream.expect_kind(SemiColon)?;
            Some(decl)
        } else {
            None
        }
    };
    let decl = parse_declarative_part(ctx)?;
    ctx.stream.expect_kind(End)?;
    ctx.stream.pop_if_kind(Package);
    let end_ident = ctx.stream.pop_optional_ident();
    let end_token = ctx.stream.expect_kind(SemiColon)?;
    Ok(PackageDeclaration {
        span: TokenSpan::new(start_token, end_token),
        context_clause: ContextClause::default(),
        end_ident_pos: check_end_identifier_mismatch(ctx, &ident.tree, end_ident),
        ident,
        generic_clause,
        decl,
    })
}

/// LRM 4.8 Package bodies
pub fn parse_package_body(ctx: &mut ParsingContext<'_>) -> ParseResult<PackageBody> {
    let start_token = ctx.stream.expect_kind(Package)?;
    ctx.stream.expect_kind(Body)?;
    let ident = ctx.stream.expect_ident()?;

    ctx.stream.expect_kind(Is)?;
    let decl = parse_declarative_part(ctx)?;
    ctx.stream.expect_kind(End)?;
    if ctx.stream.skip_if_kind(Package) {
        ctx.stream.expect_kind(Body)?;
    }
    let end_ident = ctx.stream.pop_optional_ident();
    let end_token = ctx.stream.expect_kind(SemiColon)?;

    Ok(PackageBody {
        span: TokenSpan::new(start_token, end_token),
        context_clause: ContextClause::default(),
        decl,
        end_ident_pos: check_end_identifier_mismatch(ctx, &ident, end_ident),
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

pub fn parse_design_file(ctx: &mut ParsingContext<'_>) -> ParseResult<DesignFile> {
    let mut context_clause = vec![];
    let mut design_units = vec![];

    while let Some(token) = ctx.stream.peek() {
        try_init_token_kind!(
            token,
            Library => {
                match parse_library_clause(ctx) {
                    Ok(library) => {
                        context_clause.push(ContextItem::Library(library));
                    },
                    Err(diagnostic) => ctx.diagnostics.push(diagnostic),
                }
            },
            Use => {
                match parse_use_clause(ctx) {
                    Ok(use_clause) => {
                        context_clause.push(ContextItem::Use(use_clause));
                    },
                    Err(diagnostic) => ctx.diagnostics.push(diagnostic),
                }
            },
            Context => match parse_context(ctx) {
                Ok(DeclarationOrReference::Declaration(context_decl)) => {
                    if !context_clause.is_empty() {
                        let mut diagnostic = Diagnostic::syntax_error(context_decl.ident.pos(ctx), "Context declaration may not be preceeded by a context clause");

                        for context_item in context_clause.iter() {
                            diagnostic.add_related(context_item.get_pos(ctx.stream), context_item_message(context_item, "may not come before context declaration"));
                        }

                        ctx.diagnostics.push(diagnostic);
                        context_clause.clear();
                    }

                    let tokens = ctx.stream.slice_tokens();

                    design_units.push((tokens, AnyDesignUnit::Primary(AnyPrimaryUnit::Context(context_decl))));
                }
                Ok(DeclarationOrReference::Reference(context_ref)) => {
                    context_clause.push(ContextItem::Context(context_ref));
                }
                Err(diagnostic) => ctx.diagnostics.push(diagnostic),
            },
            Entity => match parse_entity_declaration(ctx) {
                Ok(mut entity) => {
                    let tokens = ctx.stream.slice_tokens();
                    entity.context_clause = take_context_clause(&mut context_clause);
                    design_units.push((tokens, AnyDesignUnit::Primary(AnyPrimaryUnit::Entity(entity))));
                }
                Err(diagnostic) => ctx.diagnostics.push(diagnostic),
            },

            Architecture => match parse_architecture_body(ctx) {
                Ok(mut architecture) => {
                    let tokens = ctx.stream.slice_tokens();
                    architecture.context_clause = take_context_clause(&mut context_clause);
                    design_units.push((tokens, AnyDesignUnit::Secondary(AnySecondaryUnit::Architecture(architecture))));
                }
                Err(diagnostic) => ctx.diagnostics.push(diagnostic),
            },

            Configuration => match parse_configuration_declaration(ctx) {
                Ok(mut configuration) => {
                    let tokens = ctx.stream.slice_tokens();
                    configuration.context_clause = take_context_clause(&mut context_clause);
                    design_units.push((tokens, AnyDesignUnit::Primary(AnyPrimaryUnit::Configuration(configuration))));
                }
                Err(diagnostic) => ctx.diagnostics.push(diagnostic),
            },
            Package => {
                if ctx.stream.next_kinds_are(&[Package, Body]) {
                    match parse_package_body(ctx) {
                        Ok(mut package_body) => {
                            let tokens = ctx.stream.slice_tokens();
                            package_body.context_clause = take_context_clause(&mut context_clause);
                            design_units.push((tokens, AnyDesignUnit::Secondary(AnySecondaryUnit::PackageBody(package_body))));
                        }
                        Err(diagnostic) => ctx.diagnostics.push(diagnostic),
                    };
                } else if ctx.stream.next_kinds_are(&[Package, Identifier, Is, New]) {
                    match parse_package_instantiation(ctx) {
                        Ok(mut inst) => {
                            let tokens = ctx.stream.slice_tokens();
                            inst.context_clause = take_context_clause(&mut context_clause);
                            design_units.push((tokens, AnyDesignUnit::Primary(AnyPrimaryUnit::PackageInstance(inst))));
                        },
                        Err(diagnostic) => ctx.diagnostics.push(diagnostic),
                    }
                } else {
                    match parse_package_declaration(ctx) {
                        Ok(mut package) => {
                            let tokens = ctx.stream.slice_tokens();
                            package.context_clause = take_context_clause(&mut context_clause);
                            design_units.push((tokens, AnyDesignUnit::Primary(AnyPrimaryUnit::Package(package))));
                        }
                        Err(diagnostic) => ctx.diagnostics.push(diagnostic),
                    };
                }
            }
        );
    }

    for context_item in context_clause {
        ctx.diagnostics.add(
            context_item.get_pos(ctx.stream),
            context_item_message(&context_item, "not associated with any design unit"),
            ErrorCode::UnassociatedContext,
        );
    }

    Ok(DesignFile { design_units })
}

#[cfg(test)]
mod tests {
    use super::*;
    use itertools::Itertools;
    use vhdl_lang::TokenId;

    use crate::data::Diagnostic;
    use crate::syntax::test::{check_diagnostics, check_no_diagnostics, Code};
    use crate::syntax::{HasTokenSpan, TokenAccess};
    use pretty_assertions::assert_eq;

    fn parse_str(code: &str) -> (Code, DesignFile, Vec<Diagnostic>) {
        let code = Code::new(code);
        let (design_file, diagnostics) = code.with_stream_diagnostics(parse_design_file);
        (code, design_file, diagnostics)
    }

    fn parse_ok(code: &str) -> (Code, DesignFile) {
        let (code, design_file, diagnostics) = parse_str(code);
        check_no_diagnostics(&diagnostics);
        (code, design_file)
    }

    fn to_single_entity(design_file: DesignFile) -> EntityDeclaration {
        match design_file.design_units.as_slice() {
            [(_, AnyDesignUnit::Primary(AnyPrimaryUnit::Entity(ref entity)))] => entity.to_owned(),
            _ => panic!("Expected single entity {design_file:?}"),
        }
    }

    #[test]
    fn parse_empty() {
        let (_, design_file) = parse_ok("");
        assert_eq!(design_file.design_units.len(), 0);
    }

    /// An simple entity with only a name
    fn simple_entity(
        ident: Ident,
        span: TokenSpan,
        end_ident_pos: Option<TokenId>,
    ) -> AnyDesignUnit {
        AnyDesignUnit::Primary(AnyPrimaryUnit::Entity(EntityDeclaration {
            span,
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
            [(
                code.tokenize(),
                simple_entity(code.s1("myent").ident(), code.token_span(), None)
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
            [(
                code.tokenize(),
                simple_entity(
                    code.s1("myent").ident(),
                    code.token_span(),
                    Some(code.s("myent", 2).token()),
                )
            )]
        );
    }

    #[test]
    fn parse_entity_generic_clause() {
        let (code, design_file, diagnostics) = parse_str(
            "
entity myent is
  generic ();
end entity;
",
        );
        check_diagnostics(
            diagnostics,
            vec![Diagnostic::syntax_error(
                code.s1("()"),
                "Interface list must not be empty",
            )],
        );
        assert_eq!(
            to_single_entity(design_file),
            EntityDeclaration {
                span: code.token_span(),
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
                span: code.token_span(),
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
        let (code, design_file, diagnostics) = parse_str(
            "
entity myent is
  port ();
end entity;
",
        );
        check_diagnostics(
            diagnostics,
            vec![Diagnostic::syntax_error(
                code.s1("()"),
                "Interface list must not be empty",
            )],
        );
        assert_eq!(
            to_single_entity(design_file),
            EntityDeclaration {
                span: code.token_span(),
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
                span: code.token_span(),
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
                span: code.token_span(),
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
                span: code.token_span(),
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
        let tokens = code.tokenize();
        let substreams = tokens
            .split_inclusive(|tok| tok.kind == SemiColon)
            .map(Vec::from)
            .collect_vec();

        let expected_streams = vec![
            vec![Entity, Identifier, Is, End, Entity, SemiColon],
            vec![Entity, Identifier, Is, End, Entity, Identifier, SemiColon],
            vec![Entity, Identifier, Is, End, Identifier, SemiColon],
            vec![Entity, Identifier, Is, End, SemiColon],
        ];

        assert_eq!(
            substreams
                .iter()
                .map(|it| it.iter().map(|tok| tok.kind).collect_vec())
                .collect_vec(),
            expected_streams
        );

        let code_myent = code.s1_from_start(";");
        let code_myent2 = code.s1_to_end("entity myent2").s1_from_start(";");
        let code_myent3 = code.s1_to_end("entity myent3").s1_from_start(";");
        let code_myent4 = code.s1_to_end("entity myent4").s1_from_start(";");

        assert_eq!(
            design_file.design_units[0],
            (
                substreams[0].clone(),
                simple_entity(code.s1("myent").ident(), code_myent.token_span(), None,)
            )
        );
        assert_eq!(
            design_file.design_units[1],
            (
                substreams[1].clone(),
                simple_entity(
                    code.s1("myent2").ident(),
                    code_myent2
                        .token_span()
                        .apply_offset(code.s1("entity myent2").token()),
                    Some(code.s("myent2", 2).token()),
                )
            )
        );
        assert_eq!(
            design_file.design_units[2],
            (
                substreams[2].clone(),
                simple_entity(
                    code.s1("myent3").ident(),
                    code_myent3
                        .token_span()
                        .apply_offset(code.s1("entity myent3").token()),
                    Some(code.s("myent3", 2).token()),
                )
            ),
        );
        assert_eq!(
            design_file.design_units[3],
            (
                substreams[3].clone(),
                simple_entity(
                    code.s1("myent4").ident(),
                    code_myent4
                        .token_span()
                        .apply_offset(code.s1("entity myent4").token()),
                    None
                )
            )
        );
        assert_eq!(design_file.design_units.len(), 4);
    }

    // An simple entity with only a name
    fn simple_architecture(
        ident: WithDecl<Ident>,
        entity_name: Ident,
        span: TokenSpan,
        begin_token: TokenId,
        end_ident_pos: Option<TokenId>,
    ) -> AnyDesignUnit {
        AnyDesignUnit::Secondary(AnySecondaryUnit::Architecture(ArchitectureBody {
            span,
            context_clause: ContextClause::default(),
            ident,
            begin_token,
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
            [(
                code.tokenize(),
                simple_architecture(
                    WithDecl::new(code.s1("arch_name").ident()),
                    code.s1("myent").ident(),
                    code.token_span(),
                    code.s1("begin").token(),
                    None,
                )
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
            [(
                code.tokenize(),
                simple_architecture(
                    WithDecl::new(code.s1("arch_name").ident()),
                    code.s1("myent").ident(),
                    code.token_span(),
                    code.s1("begin").token(),
                    Some(code.s("arch_name", 2).token()),
                )
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
            [(
                code.tokenize(),
                simple_architecture(
                    WithDecl::new(code.s1("arch_name").ident()),
                    code.s1("myent").ident(),
                    code.token_span(),
                    code.s1("begin").token(),
                    None,
                )
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
                span: code.token_span(),
                context_clause: ContextClause::default(),
                ident: code.s1("pkg_name").decl_ident(),
                generic_clause: None,
                decl: vec![],
                end_ident_pos: None,
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
                span: code.token_span(),
                context_clause: ContextClause::default(),
                ident: code.s1("pkg_name").decl_ident(),
                generic_clause: None,
                decl: code
                    .s1("\
  type foo;
  constant bar : natural := 0;
")
                    .declarative_part(),
                end_ident_pos: None,
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
                span: code.token_span(),
                context_clause: ContextClause::default(),
                ident: code.s1("pkg_name").decl_ident(),
                generic_clause: Some(vec![
                    code.s1("type foo").generic(),
                    code.s1("type bar").generic(),
                ]),
                decl: vec![],
                end_ident_pos: None,
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
                design_units: vec![(
                    code.tokenize(),
                    AnyDesignUnit::Primary(AnyPrimaryUnit::Entity(EntityDeclaration {
                        span: code.s1_to_end("entity").token_span(),
                        context_clause: vec![
                            ContextItem::Library(code.s1("library lib;").library_clause()),
                            ContextItem::Use(code.s1("use lib.foo;").use_clause()),
                        ],
                        ident: code.s1("myent").decl_ident(),
                        generic_clause: None,
                        port_clause: None,
                        decl: vec![],
                        statements: vec![],
                        end_ident_pos: None,
                    }))
                )]
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
                Diagnostic::new(
                    code.s1("library lib;"),
                    "Library clause not associated with any design unit",
                    ErrorCode::UnassociatedContext,
                ),
                Diagnostic::new(
                    code.s1("use lib.foo;"),
                    "Use clause not associated with any design unit",
                    ErrorCode::UnassociatedContext,
                ),
                Diagnostic::new(
                    code.s1("context lib.ctx;"),
                    "Context reference not associated with any design unit",
                    ErrorCode::UnassociatedContext,
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
            vec![Diagnostic::syntax_error(
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
            (_, AnyDesignUnit::Primary(AnyPrimaryUnit::Entity(entity))) => {
                assert_eq!(entity.context_clause.len(), 0);
            }
            _ => panic!("Expected entity"),
        }
    }

    #[test]
    fn index_tokens_from_different_design_units() {
        let code = Code::new(
            "\
library ieee;
use ieee.std_logic_1164.all;

entity my_ent is
generic (
    N : natural := 4
);
end my_ent;

architecture arch of my_ent is
begin

end arch;

context my_context;

entity y is
end entity y;
        ",
        );

        let file = code.design_file();
        let (tokens, unit) = &file.design_units[0];
        let ent = unit.expect_entity();
        let lib = ent.context_clause[0].expect_library_clause();
        let tok = tokens.get_token(lib.get_start_token());
        assert_eq!(tok.kind, Library);
        assert_eq!(tok.pos, code.s1("library").pos());

        let (tokens, unit) = &file.design_units[2];
        let ent = unit.expect_entity();
        let ctx_ref = ent.context_clause[0].expect_context_reference();
        let tok = tokens.get_token(ctx_ref.get_start_token());
        assert_eq!(tok.kind, Context);
        assert_eq!(tok.pos, code.s1("context").pos());
    }
}
