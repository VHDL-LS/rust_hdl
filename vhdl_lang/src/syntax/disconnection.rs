// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::common::ParseResult;
use super::expression::parse_expression;
use super::names::parse_name;
use super::separated_list::parse_ident_list;
use super::tokens::{Kind::*, TokenSpan};
use crate::ast::token_range::WithTokenSpan;
use crate::ast::*;
use vhdl_lang::syntax::parser::ParsingContext;

/// LRM 7.4 Disconnection Specification
pub fn parse_disconnection_specification(
    ctx: &mut ParsingContext<'_>,
) -> ParseResult<WithTokenSpan<DisconnectionSpecification>> {
    let start_token = ctx.stream.expect_kind(Disconnect)?;
    if ctx.stream.next_kind_is(Identifier) {
        let _ = parse_ident_list(ctx);
    } else if ctx.stream.next_kind_is(All) {
        ctx.stream.expect_kind(All)?;
    } else {
        ctx.stream.expect_kind(Others)?;
    }
    ctx.stream.expect_kind(Colon)?;
    let _ = parse_name(ctx);
    ctx.stream.expect_kind(After)?;
    let _ = parse_expression(ctx);
    let end_token = ctx.stream.expect_kind(SemiColon)?;
    Ok(WithTokenSpan::new(
        DisconnectionSpecification {},
        TokenSpan::new(start_token, end_token),
    ))
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
            code.with_stream_no_diagnostics(parse_disconnection_specification),
            WithTokenSpan::new(DisconnectionSpecification {}, code.token_span())
        );
    }

    #[test]
    fn disconnect_spec_with_scalar_or_composite_signal_variant() {
        let code = Code::new(
            "\
disconnect foo, bar, foobar : unsigned(3 downto 0) after CLK_PERIOD;
",
        );
        assert_eq!(
            code.with_stream_no_diagnostics(parse_disconnection_specification),
            WithTokenSpan::new(DisconnectionSpecification {}, code.token_span())
        );
    }

    #[test]
    fn disconnect_spec_explicit_with_others() {
        let code = Code::new(
            "\
disconnect others: std_logic after 42 ms;
",
        );
        assert_eq!(
            code.with_stream_no_diagnostics(parse_disconnection_specification),
            WithTokenSpan::new(DisconnectionSpecification {}, code.token_span())
        );
    }

    #[test]
    fn disconnect_spec_explicit_with_all() {
        let code = Code::new(
            "\
disconnect all : T after 42 ms;
",
        );
        assert_eq!(
            code.with_stream_no_diagnostics(parse_disconnection_specification),
            WithTokenSpan::new(DisconnectionSpecification {}, code.token_span())
        );
    }
}
