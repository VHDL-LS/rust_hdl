// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind::*;
use crate::tokens::token_kind::Keyword as Kw;
use crate::tokens::TokenKind::{Comma, Keyword};
use crate::tokens::TokenStream;

impl<T: TokenStream> Parser<T> {
    pub(crate) fn opt_delay_mechanism(&mut self) {
        if self.next_is_one_of([
            Keyword(Kw::Transport),
            Keyword(Kw::Inertial),
            Keyword(Kw::Reject),
        ]) {
            self.delay_mechanism();
        }
    }

    pub fn delay_mechanism(&mut self) {
        let checkpoint = self.checkpoint();
        match_next_token_consume!(self,
            Keyword(Kw::Transport) => {
                self.start_node_at(checkpoint, TransportDelayMechanism);
            },
            Keyword(Kw::Inertial) => {
                self.start_node_at(checkpoint, InertialDelayMechanism);
            },
            Keyword(Kw::Reject) => {
                self.start_node_at(checkpoint, InertialDelayMechanism);
                self.expression();
                self.expect_kw(Kw::Inertial);
            }
        );
        self.end_node();
    }

    pub fn selected_waveforms(&mut self) {
        self.start_node(SelectedWaveforms);
        self.separated_list(Parser::selected_waveform, Comma);
        self.end_node();
    }

    fn selected_waveform(&mut self) {
        self.start_node(SelectedWaveformItem);
        self.waveform();
        self.expect_kw(Kw::When);
        self.choices();
        self.end_node();
    }

    pub fn waveform_elements(&mut self) {
        self.start_node(WaveformElements);
        self.separated_list(Parser::waveform_element, Comma);
        self.end_node();
    }

    pub fn waveform(&mut self) {
        if self.next_is(Keyword(Kw::Unaffected)) {
            self.skip_into_node(UnaffectedWaveform);
        } else {
            self.waveform_elements();
        }
    }

    pub fn waveform_element(&mut self) {
        self.start_node(WaveformElement);
        self.expression();
        if self.opt_token(Keyword(Kw::After)) {
            self.expression();
        }
        self.end_node()
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
