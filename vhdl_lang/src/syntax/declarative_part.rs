// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::alias_declaration::parse_alias_declaration;
use super::attributes::parse_attribute;
use super::common::ParseResult;
use super::component_declaration::parse_component_declaration;
use super::configuration::parse_configuration_specification;
use super::context::parse_use_clause;
use super::design_unit::{parse_package_body, parse_package_declaration};
use super::names::parse_selected_name;
use super::object_declaration::{parse_file_declaration, parse_object_declaration};
use super::subprogram::parse_subprogram;
use super::tokens::{Kind::*, *};
use super::type_declaration::parse_type_declaration;
use crate::ast::token_range::WithTokenSpan;
use crate::ast::{ContextClause, Declaration, PackageInstantiation};
use crate::syntax::concurrent_statement::parse_map_aspect;
use crate::syntax::disconnection::parse_disconnection_specification;
use crate::syntax::view::parse_mode_view_declaration;
use vhdl_lang::syntax::parser::ParsingContext;

pub fn parse_package_instantiation(
    ctx: &mut ParsingContext<'_>,
) -> ParseResult<PackageInstantiation> {
    let start_token = ctx.stream.expect_kind(Package)?;
    let ident = ctx.stream.expect_ident()?;
    ctx.stream.expect_kind(Is)?;
    ctx.stream.expect_kind(New)?;
    let package_name = parse_selected_name(ctx)?;
    let generic_map = parse_map_aspect(ctx, Generic)?;
    let end_token = ctx.stream.expect_kind(SemiColon)?;

    Ok(PackageInstantiation {
        span: TokenSpan::new(start_token, end_token),
        context_clause: ContextClause::default(),
        ident: ident.into(),
        package_name,
        generic_map,
    })
}

pub fn is_declarative_part(ctx: &mut ParsingContext) -> ParseResult<bool> {
    Ok(matches!(
        ctx.stream.peek_expect()?.kind,
        Use | Type
            | Subtype
            | Shared
            | Constant
            | Signal
            | Variable
            | File
            | Component
            | Attribute
            | Alias
            | Impure
            | Pure
            | Function
            | Procedure
            | Package
            | For
            | View
            | Begin
            | Disconnect
    ))
}

pub fn is_recover_token(kind: Kind) -> bool {
    matches!(
        kind,
        Type | Subtype
            | Component
            | Impure
            | Pure
            | Function
            | Procedure
            | Package
            | For
            | File
            | Shared
            | Constant
            | Signal
            | Variable
            | Attribute
            | View
            | Use
            | Alias
            | Begin
            | End
            | Disconnect
    )
}

pub fn parse_declarative_part(
    ctx: &mut ParsingContext<'_>,
) -> ParseResult<Vec<WithTokenSpan<Declaration>>> {
    let mut declarations: Vec<WithTokenSpan<Declaration>> = Vec::new();

    while let Some(token) = ctx.stream.peek() {
        let start_token = ctx.stream.get_current_token_id();
        match token.kind {
            Begin | End => break,
            Type | Subtype | Component | Impure | Pure | Function | Procedure | Package | For => {
                let decl = match token.kind {
                    Type | Subtype => parse_type_declaration(ctx).map(Declaration::Type)?,
                    Component => parse_component_declaration(ctx).map(Declaration::Component)?,
                    Impure | Pure | Function | Procedure => parse_subprogram(ctx)?,
                    Package => {
                        if ctx.stream.next_kinds_are(&[Package, Identifier, Is, New]) {
                            parse_package_instantiation(ctx).map(Declaration::Package)?
                        } else if ctx.stream.next_kinds_are(&[Package, Body]) {
                            parse_package_body(ctx).map(Declaration::PackageBody)?
                        } else {
                            parse_package_declaration(ctx).map(Declaration::PackageDeclaration)?
                        }
                    }
                    For => {
                        parse_configuration_specification(ctx).map(Declaration::Configuration)?
                    }
                    _ => unreachable!(),
                };
                let end_token = ctx.stream.get_last_token_id();
                declarations.push(WithTokenSpan::new(
                    decl,
                    TokenSpan::new(start_token, end_token),
                ));
            }

            File | Shared | Constant | Signal | Variable | Attribute => {
                let decls: ParseResult<Vec<WithTokenSpan<Declaration>>> = match token.kind {
                    File => parse_file_declaration(ctx).map(|decls| {
                        decls
                            .into_iter()
                            .map(|decl| decl.map_into(Declaration::File))
                            .collect()
                    }),
                    Shared | Constant | Signal | Variable => {
                        parse_object_declaration(ctx).map(|decls| {
                            decls
                                .into_iter()
                                .map(|decl| decl.map_into(Declaration::Object))
                                .collect()
                        })
                    }
                    Attribute => parse_attribute(ctx).map(|decls| {
                        decls
                            .into_iter()
                            .map(|decl| decl.map_into(Declaration::Attribute))
                            .collect()
                    }),
                    _ => unreachable!(),
                };
                match decls.or_recover_until(ctx, is_recover_token) {
                    Ok(ref mut decls) => declarations.append(decls),
                    Err(err) => {
                        ctx.diagnostics.push(err);
                        continue;
                    }
                }
            }

            Use | Alias => {
                let decl: ParseResult<WithTokenSpan<Declaration>> = match token.kind {
                    Use => parse_use_clause(ctx).map(|decl| decl.map_into(Declaration::Use)),
                    Alias => {
                        parse_alias_declaration(ctx).map(|decl| decl.map_into(Declaration::Alias))
                    }
                    _ => unreachable!(),
                };
                match decl.or_recover_until(ctx, is_recover_token) {
                    Ok(decl) => declarations.push(decl),
                    Err(err) => {
                        ctx.diagnostics.push(err);
                        continue;
                    }
                }
            }

            View => {
                match parse_mode_view_declaration(ctx).or_recover_until(ctx, is_recover_token) {
                    Ok(decl) => declarations.push(decl.map_into(Declaration::View)),
                    Err(err) => {
                        ctx.diagnostics.push(err);
                        continue;
                    }
                }
            }

            Disconnect => {
                let decls: ParseResult<Vec<WithTokenSpan<Declaration>>> =
                    parse_disconnection_specification(ctx).map(|decls| {
                        decls
                            .into_iter()
                            .map(|decl| decl.map_into(Declaration::Disconnection))
                            .collect()
                    });
                match decls.or_recover_until(ctx, is_recover_token) {
                    Ok(ref mut decls) => declarations.append(decls),
                    Err(err) => {
                        ctx.diagnostics.push(err);
                        continue;
                    }
                }
            }

            _ => {
                use crate::VHDLStandard::*;
                let expected: &[Kind] = match ctx.standard {
                    VHDL2008 | VHDL1993 => &[
                        Type, Subtype, Component, Impure, Pure, Function, Procedure, Package, For,
                        File, Shared, Constant, Signal, Variable, Attribute, Use, Alias,
                        Disconnect,
                    ],
                    VHDL2019 => &[
                        Type, Subtype, Component, Impure, Pure, Function, Procedure, Package, For,
                        File, Shared, Constant, Signal, Variable, Attribute, Use, Alias, View,
                        Disconnect,
                    ],
                };
                ctx.diagnostics.push(token.kinds_error(expected));
                ctx.stream.skip_until(is_recover_token)?;
                continue;
            }
        }
    }

    Ok(declarations)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{ObjectClass, ObjectDeclaration};
    use crate::data::Diagnostic;
    use crate::syntax::test::{check_diagnostics, Code};
    use crate::VHDLStandard::VHDL2019;

    #[test]
    fn package_instantiation() {
        let code = Code::new(
            "\
package ident is new lib.foo.bar;
",
        );
        assert_eq!(
            code.with_stream_no_diagnostics(parse_package_instantiation),
            PackageInstantiation {
                span: code.token_span(),
                context_clause: ContextClause::default(),
                ident: code.s1("ident").decl_ident(),
                package_name: code.s1("lib.foo.bar").name(),
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
            code.with_stream_no_diagnostics(parse_package_instantiation),
            PackageInstantiation {
                span: code.token_span(),
                context_clause: ContextClause::default(),
                ident: code.s1("ident").decl_ident(),
                package_name: code.s1("lib.foo.bar").name(),
                generic_map: Some(
                    code.s1("generic map (
    foo => bar
  )")
                        .generic_map_aspect()
                )
            }
        );
    }

    #[test]
    fn parse_declarative_part_recover() {
        let code = Code::new(
            "\
var invalid: broken;
constant x: natural := 5;
",
        );
        let (decls, msgs) = code.with_partial_stream_diagnostics(parse_declarative_part);
        assert_eq!(
            decls,
            Ok(vec![WithTokenSpan::new(
                Declaration::Object(ObjectDeclaration {
                    class: ObjectClass::Constant,
                    ident: code.s1("x").decl_ident(),
                    subtype_indication: code.s1("natural").subtype_indication(),
                    expression: Some(code.s1("5").expr())
                }),
                code.s1("constant x: natural := 5;").token_span()
            )])
        );

        assert_eq!(
            msgs,
            vec![Diagnostic::syntax_error(
                code.s1("var").pos(),
                "Expected 'type', 'subtype', 'component', 'impure', 'pure', \
                 'function', 'procedure', 'package', 'for', 'file', \
                 'shared', 'constant', 'signal', 'variable', 'attribute', \
                 'use', 'alias' or 'disconnect'"
            )]
        );
    }

    #[test]
    fn parse_declarative_part_error() {
        // Just checking that there is not an infinite loop
        let code = Code::new("invalid");
        let (decl, _) = code.with_partial_stream_diagnostics(parse_declarative_part);
        assert!(decl.is_err());
    }

    #[test]
    fn parse_declarative_part_vhdl2008_vs_vhdl2019() {
        let code = Code::new(
            "\
var not_a_var: broken;
",
        );
        let (_, diag) = code.with_partial_stream_diagnostics(parse_declarative_part);
        check_diagnostics(
            diag,
            vec![Diagnostic::syntax_error(
                code.s1("var").pos(),
                "Expected 'type', 'subtype', 'component', 'impure', 'pure', \
                 'function', 'procedure', 'package', 'for', 'file', \
                 'shared', 'constant', 'signal', 'variable', 'attribute', \
                 'use', 'alias' or 'disconnect'",
            )],
        );

        let code = Code::with_standard(
            "\
var not_a_var: broken;
",
            VHDL2019,
        );
        let (_, diag) = code.with_partial_stream_diagnostics(parse_declarative_part);
        check_diagnostics(
            diag,
            vec![Diagnostic::syntax_error(
                code.s1("var").pos(),
                "Expected 'type', 'subtype', 'component', 'impure', 'pure', \
                 'function', 'procedure', 'package', 'for', 'file', \
                 'shared', 'constant', 'signal', 'variable', 'attribute', \
                 'use', 'alias', 'view' or 'disconnect'",
            )],
        )
    }
}
