// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use ast::{DelayMechanism, Waveform, WaveformElement};

use common::parse_optional;
use expression::parse_expression;
use message::ParseResult;
use tokenizer::Kind::*;
use tokenstream::TokenStream;

/// LRM 10.5 Signal assignment statement
pub fn parse_delay_mechanism(stream: &mut TokenStream) -> ParseResult<Option<DelayMechanism>> {
    let token = stream.peek_expect()?;
    match token.kind {
        Transport => {
            stream.move_after(&token);
            Ok(Some(DelayMechanism::Transport))
        }
        Inertial => {
            stream.move_after(&token);
            Ok(Some(DelayMechanism::Inertial { reject: None }))
        }
        Reject => {
            stream.move_after(&token);
            let reject = Some(parse_expression(stream)?);
            stream.expect_kind(Inertial)?;
            Ok(Some(DelayMechanism::Inertial { reject }))
        }
        _ => Ok(None),
    }
}

/// LRM 10.5 Signal assignment statement
pub fn parse_waveform(stream: &mut TokenStream) -> ParseResult<Waveform> {
    if stream.skip_if_kind(Unaffected)? {
        return Ok(Waveform::Unaffected);
    }

    let mut elems = Vec::new();

    loop {
        let value = parse_expression(stream)?;
        let after = parse_optional(stream, After, parse_expression)?;

        elems.push(WaveformElement { value, after });

        if !stream.skip_if_kind(Comma)? {
            break;
        }
    }
    Ok(Waveform::Elements(elems))
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_util::with_stream;

    #[test]
    fn test_transport_delay_mechanism() {
        let (_, delay) = with_stream(parse_delay_mechanism, "transport");
        assert_eq!(delay, Some(DelayMechanism::Transport));
    }

    #[test]
    fn test_intertial_delay_mechanism() {
        let (_, delay) = with_stream(parse_delay_mechanism, "inertial");
        assert_eq!(delay, Some(DelayMechanism::Inertial { reject: None }));
    }

    #[test]
    fn test_reject_intertial_delay_mechanism() {
        let (util, delay) = with_stream(parse_delay_mechanism, "reject 2 ns inertial");
        assert_eq!(
            delay,
            Some(DelayMechanism::Inertial {
                reject: Some(util.expr("2 ns"))
            })
        );
    }

    #[test]
    fn test_waveform() {
        let (util, wave) = with_stream(parse_waveform, "bar(1 to 3)");
        assert_eq!(
            wave,
            Waveform::Elements(vec![WaveformElement {
                value: util.expr("bar(1 to 3)"),
                after: None
            }])
        );
    }

    #[test]
    fn test_waveform_after() {
        let (util, wave) = with_stream(parse_waveform, "bar(1 to 3) after 2 ns");
        assert_eq!(
            wave,
            Waveform::Elements(vec![WaveformElement {
                value: util.expr("bar(1 to 3)"),
                after: Some(util.expr("2 ns"))
            }])
        );
    }

    #[test]
    fn test_waveform_after_many() {
        let (util, wave) = with_stream(parse_waveform, "bar(1 to 3) after 2 ns, expr after 1 ns");
        assert_eq!(
            wave,
            Waveform::Elements(vec![
                WaveformElement {
                    value: util.expr("bar(1 to 3)"),
                    after: Some(util.expr("2 ns")),
                },
                WaveformElement {
                    value: util.expr("expr"),
                    after: Some(util.expr("1 ns")),
                },
            ])
        );
    }

    #[test]
    fn test_unaffected_waveform() {
        let (_, wave) = with_stream(parse_waveform, "unaffected");
        assert_eq!(wave, Waveform::Unaffected);
    }
}
