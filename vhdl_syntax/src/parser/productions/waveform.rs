// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::marker::CompletedMarker;
use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind::*;
use crate::tokens::token_kind::Keyword as Kw;
use crate::tokens::TokenKind::{Comma, Keyword};

impl Parser {
    pub(crate) fn opt_delay_mechanism(&mut self) {
        if self.next_is_one_of([
            Keyword(Kw::Transport),
            Keyword(Kw::Inertial),
            Keyword(Kw::Reject),
        ]) {
            self.delay_mechanism();
        }
    }

    pub(crate) fn delay_mechanism(&mut self) -> Option<CompletedMarker> {
        match_next_token!(self,
            Keyword(Kw::Transport) => Some(self.skip_into_node(TransportDelayMechanism)),
            Keyword(Kw::Inertial) => Some(self.skip_into_node(InertialDelayMechanism)),
            Keyword(Kw::Reject) => {
                Some(self.node(InertialDelayMechanism, |p| {
                    p.node(RejectClause, |p| {
                        p.skip(); // Kw::Reject
                        p.expression();
                    });
                    p.expect_kw(Kw::Inertial);
                }))
            }
        )
    }

    pub(crate) fn selected_waveforms(&mut self) {
        self.separated_list(SelectedWaveforms, Parser::selected_waveform, Comma);
    }

    fn selected_waveform(&mut self) {
        self.node(SelectedWaveformItem, |p| {
            p.waveform();
            p.expect_kw(Kw::When);
            p.choices();
        });
    }

    pub(crate) fn waveform_elements(&mut self) -> CompletedMarker {
        self.separated_list(WaveformElements, Parser::waveform_element, Comma)
    }

    pub(crate) fn waveform(&mut self) -> CompletedMarker {
        if self.next_is(Keyword(Kw::Unaffected)) {
            self.skip_into_node(UnaffectedWaveform)
        } else {
            self.waveform_elements()
        }
    }

    pub(crate) fn waveform_element(&mut self) {
        self.node(WaveformElement, |p| {
            p.expression();
            if p.next_is(Keyword(Kw::After)) {
                p.node(AfterClause, |p| {
                    p.skip(); // Kw::After
                    p.expression();
                });
            }
        });
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils::to_test_text;
    use crate::parser::Parser;

    #[test]
    fn transport_delay_mechanism() {
        insta::assert_snapshot!(to_test_text(Parser::delay_mechanism, "transport"))
    }

    #[test]
    fn intertial_delay_mechanism() {
        insta::assert_snapshot!(to_test_text(Parser::delay_mechanism, "inertial"))
    }

    #[test]
    fn reject_intertial_delay_mechanism() {
        insta::assert_snapshot!(to_test_text(
            Parser::delay_mechanism,
            "reject 2 ns inertial"
        ))
    }

    #[test]
    fn waveform() {
        insta::assert_snapshot!(to_test_text(Parser::waveform, "bar(1 to 3)"))
    }

    #[test]
    fn waveform_after() {
        insta::assert_snapshot!(to_test_text(Parser::waveform, "bar(1 to 3) after 2 ns"))
    }

    #[test]
    fn waveform_after_many() {
        insta::assert_snapshot!(to_test_text(
            Parser::waveform,
            "bar(1 to 3) after 2 ns, expr after 1 ns"
        ))
    }

    #[test]
    fn unaffected_waveform() {
        insta::assert_snapshot!(to_test_text(Parser::waveform, "unaffected"))
    }
}
