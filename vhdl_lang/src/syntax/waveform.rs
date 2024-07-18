// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use crate::ast::token_range::WithTokenSpan;
use crate::ast::{DelayMechanism, Waveform, WaveformElement};
use crate::syntax::parser::ParsingContext;
use vhdl_lang::TokenSpan;

use super::common::{parse_optional, ParseResult};
use super::expression::parse_expression;
use super::tokens::Kind::*;

/// LRM 10.5 Signal assignment statement
pub fn parse_delay_mechanism(
    ctx: &mut ParsingContext<'_>,
) -> ParseResult<Option<WithTokenSpan<DelayMechanism>>> {
    let token = ctx.stream.peek_expect()?;
    let start_token = ctx.stream.get_current_token_id();
    match token.kind {
        Transport => {
            ctx.stream.skip();
            let span = TokenSpan::new(start_token, start_token);
            Ok(Some(WithTokenSpan::new(DelayMechanism::Transport, span)))
        }
        Inertial => {
            ctx.stream.skip();
            let span = TokenSpan::new(start_token, start_token);
            Ok(Some(WithTokenSpan::new(
                DelayMechanism::Inertial { reject: None },
                span,
            )))
        }
        Reject => {
            ctx.stream.skip();
            let reject = Some(parse_expression(ctx)?);
            let end_token = ctx.stream.expect_kind(Inertial)?;
            let span = TokenSpan::new(start_token, end_token);
            Ok(Some(WithTokenSpan::new(
                DelayMechanism::Inertial { reject },
                span,
            )))
        }
        _ => Ok(None),
    }
}

/// LRM 10.5 Signal assignment statement
pub fn parse_waveform(ctx: &mut ParsingContext<'_>) -> ParseResult<Waveform> {
    if let Some(token) = ctx.stream.pop_if_kind(Unaffected) {
        return Ok(Waveform::Unaffected(token));
    }

    let mut elems = Vec::new();

    loop {
        let value = parse_expression(ctx)?;
        let after = parse_optional(ctx, After, parse_expression)?;

        elems.push(WaveformElement { value, after });

        if !ctx.stream.skip_if_kind(Comma) {
            break;
        }
    }
    Ok(Waveform::Elements(elems))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::test::Code;

    #[test]
    fn test_transport_delay_mechanism() {
        let code = Code::new("transport");
        assert_eq!(
            code.with_stream(parse_delay_mechanism),
            Some(WithTokenSpan::new(
                DelayMechanism::Transport,
                code.token_span()
            ))
        );
    }

    #[test]
    fn test_intertial_delay_mechanism() {
        let code = Code::new("inertial");
        assert_eq!(
            code.with_stream(parse_delay_mechanism),
            Some(WithTokenSpan::new(
                DelayMechanism::Inertial { reject: None },
                code.token_span()
            ))
        );
    }

    #[test]
    fn test_reject_intertial_delay_mechanism() {
        let code = Code::new("reject 2 ns inertial");
        assert_eq!(
            code.with_stream(parse_delay_mechanism),
            Some(WithTokenSpan::new(
                DelayMechanism::Inertial {
                    reject: Some(code.s1("2 ns").expr())
                },
                code.token_span()
            ))
        );
    }

    #[test]
    fn test_waveform() {
        let code = Code::new("bar(1 to 3)");
        assert_eq!(
            code.with_stream(parse_waveform),
            Waveform::Elements(vec![WaveformElement {
                value: code.s1("bar(1 to 3)").expr(),
                after: None
            }])
        );
    }

    #[test]
    fn test_waveform_after() {
        let code = Code::new("bar(1 to 3) after 2 ns");
        assert_eq!(
            code.with_stream(parse_waveform),
            Waveform::Elements(vec![WaveformElement {
                value: code.s1("bar(1 to 3)").expr(),
                after: Some(code.s1("2 ns").expr())
            }])
        );
    }

    #[test]
    fn test_waveform_after_many() {
        let code = Code::new("bar(1 to 3) after 2 ns, expr after 1 ns");
        assert_eq!(
            code.with_stream(parse_waveform),
            Waveform::Elements(vec![
                WaveformElement {
                    value: code.s1("bar(1 to 3)").expr(),
                    after: Some(code.s1("2 ns").expr()),
                },
                WaveformElement {
                    value: code.s1("expr").expr(),
                    after: Some(code.s1("1 ns").expr()),
                },
            ])
        );
    }

    #[test]
    fn test_unaffected_waveform() {
        let code = Code::new("unaffected");
        assert_eq!(
            code.with_stream(parse_waveform),
            Waveform::Unaffected(code.token())
        );
    }
}
