// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::common::ParseResult;
use super::expression::parse_expression;
use super::tokens::{Kind::*, TokenSpan};
use crate::ast::token_range::WithTokenSpan;
use crate::ast::*;
use crate::syntax::subtype_indication::parse_subtype_indication;
use vhdl_lang::syntax::parser::ParsingContext;

/// LRM 7.4 Disconnection Specification
pub fn parse_disconnection_specification(
    ctx: &mut ParsingContext<'_>,
) -> ParseResult<Vec<WithTokenSpan<DisconnectionSpecification>>> {
    let start_token = ctx.stream.expect_kind(Disconnect)?;
    let mut idents: Vec<GuardedSignalList> = Vec::new();
    if let Some(token) = ctx.stream.peek() {
        match token.kind {
            All => {
                idents.push(GuardedSignalList::All);
                ctx.stream.skip();
            }
            Others => {
                idents.push(GuardedSignalList::Others);
                ctx.stream.skip();
            }
            _ => loop {
                idents.push(GuardedSignalList::Ident(WithDecl::new(
                    ctx.stream.expect_ident()?,
                )));
                if ctx.stream.next_kind_is(Comma) {
                    ctx.stream.skip();
                } else {
                    break;
                }
            },
        }
    }

    ctx.stream.expect_kind(Colon)?;
    let subtype_indication = parse_subtype_indication(ctx)?;
    ctx.stream.expect_kind(After)?;
    let expression = parse_expression(ctx)?;
    let end_token = ctx.stream.expect_kind(SemiColon)?;

    Ok(idents
        .into_iter()
        .map(|ident| {
            WithTokenSpan::new(
                DisconnectionSpecification {
                    ident,
                    subtype_indication: subtype_indication.clone(),
                    expression: expression.clone(),
                },
                TokenSpan::new(start_token, end_token),
            )
        })
        .collect())
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::syntax::test::Code;
    #[test]
    fn disconnect_spec_with_scalar_or_composite_signal() {
        let code = Code::new(
            "\
    disconnect S: T after 42 ms;
    ",
        );
        assert_eq!(
            code.with_stream_no_diagnostics(parse_disconnection_specification)[0],
            WithTokenSpan::new(
                DisconnectionSpecification {
                    ident: GuardedSignalList::Ident(WithDecl::new(code.s1("S").ident())),
                    subtype_indication: code.s1("T").subtype_indication(),
                    expression: code.s1("42 ms").expr(),
                },
                code.token_span()
            )
        )
    }

    #[test]
    fn disconnect_spec_with_scalar_or_composite_signal_variant() {
        let code = Code::new(
            "\
    disconnect foobar : integer after 100*CLK_PERIOD;
    ",
        );
        assert_eq!(
            code.with_stream_no_diagnostics(parse_disconnection_specification)[0],
            WithTokenSpan::new(
                DisconnectionSpecification {
                    ident: GuardedSignalList::Ident(WithDecl::new(code.s1("foobar").ident())),
                    subtype_indication: code.s1("integer").subtype_indication(),
                    expression: code.s1("100*CLK_PERIOD").expr(),
                },
                code.token_span()
            )
        )
    }

    #[test]
    fn disconnect_spec_explicit_with_others() {
        let code = Code::new(
            "\
    disconnect others: std_logic after bar;
    ",
        );
        assert_eq!(
            code.with_stream_no_diagnostics(parse_disconnection_specification)[0],
            WithTokenSpan::new(
                DisconnectionSpecification {
                    ident: GuardedSignalList::Others,
                    subtype_indication: code.s1("std_logic").subtype_indication(),
                    expression: code.s1("bar").expr(),
                },
                code.token_span()
            )
        )
    }

    #[test]
    fn disconnect_spec_explicit_with_all() {
        let code = Code::new(
            "\
    disconnect all : unsigned(3 downto 0) after bar;
    ",
        );
        assert_eq!(
            code.with_stream_no_diagnostics(parse_disconnection_specification)[0],
            WithTokenSpan::new(
                DisconnectionSpecification {
                    ident: GuardedSignalList::All,
                    subtype_indication: code.s1("unsigned(3 downto 0)").subtype_indication(),
                    expression: code.s1("bar").expr(),
                },
                code.token_span()
            )
        )
    }

    #[test]
    fn disconnect_spec_syntax_error_1() {
        let code = Code::new(
            "\
disconnect ; T after 42 ms;
",
        );
        let (decl, _) = code.with_partial_stream_diagnostics(parse_disconnection_specification);
        assert!(decl.is_err());
    }

    #[test]
    fn disconnect_spec_syntax_error_2() {
        let code = Code::new(
            "\
disconnect 7 after 42 ms;
",
        );
        let (decl, _) = code.with_partial_stream_diagnostics(parse_disconnection_specification);
        assert!(decl.is_err());
    }

    #[test]
    fn disconnect_spec_syntax_error_3() {
        let code = Code::new(
            "\
disconnect foo after 42 ms;
",
        );
        let (decl, _) = code.with_partial_stream_diagnostics(parse_disconnection_specification);
        assert!(decl.is_err());
    }

    #[test]
    fn disconnect_spec_syntax_error_4() {
        let code = Code::new(
            "\
disconnect foo : std_logic afterrr 42 ms;
",
        );
        let (decl, _) = code.with_partial_stream_diagnostics(parse_disconnection_specification);
        assert!(decl.is_err());
    }

    #[test]
    fn disconnect_spec_syntax_error_5() {
        let code = Code::new(
            "\
disconnect foo : after 42 ms bar;
",
        );
        let (decl, _) = code.with_partial_stream_diagnostics(parse_disconnection_specification);
        assert!(decl.is_err());
    }

    #[test]
    fn disconnect_spec_syntax_error_6() {
        let code = Code::new(
            "\
diconnect foo : after 42 ms;
",
        );
        let (decl, _) = code.with_partial_stream_diagnostics(parse_disconnection_specification);
        assert!(decl.is_err());
    }
}
