// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::common::ParseResult;
use super::expression::parse_expression;
//use super::separated_list::parse_ident_list;
use super::tokens::{Kind::*, TokenSpan};
use crate::ast::token_range::WithTokenSpan;
use crate::ast::*;
use crate::syntax::declarative_part::is_recover_token;
use crate::syntax::subtype_indication::parse_subtype_indication;
use crate::syntax::Kind;
use vhdl_lang::syntax::parser::ParsingContext;

/// LRM 7.4 Disconnection Specification
pub fn parse_disconnection_specification(
    ctx: &mut ParsingContext<'_>,
) -> ParseResult<WithTokenSpan<DisconnectionSpecification>> {
    let start_token = ctx.stream.expect_kind(Disconnect)?;

    let ident = GuardedSignalList::All;
    if let Some(token) = ctx.stream.peek() {
        match token.kind {
            All => {
                ctx.stream.skip();
            }
            Others => {
                let ident = GuardedSignalList::Others;
                ctx.stream.skip();
            }
            _ => { // Identifier
                let ident = WithDecl::new(ctx.stream.expect_ident()?);
            }
        }
    }

    ctx.stream.expect_kind(Colon)?;
    let subtype_indication = parse_subtype_indication(ctx)?;
    ctx.stream.expect_kind(After)?;
    let expression = parse_expression(ctx)?;
    let end_token = ctx.stream.expect_kind(SemiColon)?;
    Ok(WithTokenSpan::new(
        DisconnectionSpecification {
            ident,
            subtype_indication,
            expression,
        },
        TokenSpan::new(start_token, end_token),
    ))
}

//#[cfg(test)]
//mod tests {
//    use super::*;
//
//    use crate::data::Diagnostic;
//    use crate::syntax::test::{check_diagnostics, Code};
//    #[test]
//    fn disconnect_spec_with_scalar_or_composite_signal() {
//        let code = Code::new(
//            "\
//disconnect S: T after 42 ms;
//",
//        );
//        assert_eq!(
//            code.with_stream_no_diagnostics(parse_disconnection_specification),
//            WithTokenSpan::new(
//                DisconnectionSpecification {
//                    ident: code.s1("S").decl_ident(),
//                    subtype_indication: Some(code.s1("T").subtype_indication()),
//                    expression: Some(code.s1("42 ms").expr()),
//                },
//                code.token_span()
//            )
//        )
//    }
//
//    #[test]
//    fn disconnect_spec_with_scalar_or_composite_signal_variant() {
//        let code = Code::new(
//            "\
//disconnect foobar : integer after 100*CLK_PERIOD;
//",
//        );
//        assert_eq!(
//            code.with_stream_no_diagnostics(parse_disconnection_specification),
//            WithTokenSpan::new(
//                DisconnectionSpecification {
//                    ident: Some(code.s1("foobar").decl_ident()),
//                    subtype_indication: Some(code.s1("integer").subtype_indication()),
//                    expression: Some(code.s1("100*CLK_PERIOD").expr()),
//                },
//                code.token_span()
//            )
//        )
//    }
//
//    #[test]
//    fn disconnect_spec_explicit_with_others() {
//        let code = Code::new(
//            "\
//disconnect others: std_logic after bar;
//",
//        );
//        assert_eq!(
//            code.with_stream_no_diagnostics(parse_disconnection_specification),
//            WithTokenSpan::new(
//                DisconnectionSpecification {
//                    ident: None,
//                    subtype_indication: Some(code.s1("std_logic").subtype_indication()),
//                    expression: Some(code.s1("bar").expr()),
//                },
//                code.token_span()
//            )
//        )
//    }
//
//    #[test]
//    fn disconnect_spec_explicit_with_all() {
//        let code = Code::new(
//            "\
//disconnect all : unsigned(3 downto 0) after bar;
//",
//        );
//        assert_eq!(
//            code.with_stream_no_diagnostics(parse_disconnection_specification),
//            WithTokenSpan::new(
//                DisconnectionSpecification {
//                    ident: None,
//                    subtype_indication: Some(code.s1("unsigned(3 downto 0)").subtype_indication()),
//                    expression: Some(code.s1("bar").expr()),
//                },
//                code.token_span()
//            )
//        )
//    }
//
//    #[test]
//    fn disconnect_spec_syntax_error_1() {
//        let code = Code::new(
//            "\
//disconnect ; T after 42 ms;
//",
//        );
//        let (_, diag) = code.with_partial_stream_diagnostics(parse_disconnection_specification);
//        check_diagnostics(
//            diag,
//            vec![Diagnostic::syntax_error(
//                code.s1(";").pos(),
//                "Expected '{identifier}', 'all' or 'others'",
//            )],
//        );
//    }
//
//    #[test]
//    fn disconnect_spec_syntax_error_2() {
//        let code = Code::new(
//            "\
//disconnect 7 after 42 ms;
//",
//        );
//        let (_, diag) = code.with_partial_stream_diagnostics(parse_disconnection_specification);
//        check_diagnostics(
//            diag,
//            vec![Diagnostic::syntax_error(
//                code.s1("7").pos(),
//                "Expected '{identifier}', 'all' or 'others'",
//            )],
//        );
//    }
//
//    #[test]
//    fn disconnect_spec_syntax_error_3() {
//        let code = Code::new(
//            "\
//disconnect foo after 42 ms;
//",
//        );
//        let (decl, _) = code.with_partial_stream_diagnostics(parse_disconnection_specification);
//        assert!(decl.is_err());
//    }
//
//    #[test]
//    fn disconnect_spec_syntax_error_4() {
//        let code = Code::new(
//            "\
//disconnect foo : std_logic afterrr 42 ms;
//",
//        );
//        let (decl, _) = code.with_partial_stream_diagnostics(parse_disconnection_specification);
//        assert!(decl.is_err());
//    }
//
//    #[test]
//    fn disconnect_spec_syntax_error_5() {
//        let code = Code::new(
//            "\
//disconnect foo : after 42 ms bar;
//",
//        );
//        let (decl, _) = code.with_partial_stream_diagnostics(parse_disconnection_specification);
//        assert!(decl.is_err());
//    }
//
//    #[test]
//    fn disconnect_spec_syntax_error_6() {
//        let code = Code::new(
//            "\
//diconnect foo : after 42 ms;
//",
//        );
//        let (decl, _) = code.with_partial_stream_diagnostics(parse_disconnection_specification);
//        assert!(decl.is_err());
//    }
//}
