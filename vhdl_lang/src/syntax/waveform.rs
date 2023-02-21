// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use crate::ast::{DelayMechanism, Waveform, WaveformElement};

use super::common::{parse_optional, ParseResult};
use super::expression::parse_expression;
use super::tokens::{Kind::*, TokenStream};

/// LRM 10.5 Signal assignment statement
pub fn parse_delay_mechanism(stream: &TokenStream) -> ParseResult<Option<DelayMechanism>> {
    let token = stream.peek_expect()?;
    match token.kind {
        Transport => {
            stream.skip();
            Ok(Some(DelayMechanism::Transport))
        }
        Inertial => {
            stream.skip();
            Ok(Some(DelayMechanism::Inertial { reject: None }))
        }
        Reject => {
            stream.skip();
            let reject = Some(parse_expression(stream)?);
            stream.expect_kind(Inertial)?;
            Ok(Some(DelayMechanism::Inertial { reject }))
        }
        _ => Ok(None),
    }
}

/// LRM 10.5 Signal assignment statement
pub fn parse_waveform(stream: &TokenStream) -> ParseResult<Waveform> {
    if stream.skip_if_kind(Unaffected) {
        return Ok(Waveform::Unaffected);
    }

    let mut elems = Vec::new();

    loop {
        let value = parse_expression(stream)?;
        let after = parse_optional(stream, After, parse_expression)?;

        elems.push(WaveformElement { value, after });

        if !stream.skip_if_kind(Comma) {
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
            Some(DelayMechanism::Transport)
        );
    }

    #[test]
    fn test_intertial_delay_mechanism() {
        let code = Code::new("inertial");
        assert_eq!(
            code.with_stream(parse_delay_mechanism),
            Some(DelayMechanism::Inertial { reject: None })
        );
    }

    #[test]
    fn test_reject_intertial_delay_mechanism() {
        let code = Code::new("reject 2 ns inertial");
        assert_eq!(
            code.with_stream(parse_delay_mechanism),
            Some(DelayMechanism::Inertial {
                reject: Some(code.s1("2 ns").expr())
            })
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
        assert_eq!(code.with_stream(parse_waveform), Waveform::Unaffected);
    }
}
