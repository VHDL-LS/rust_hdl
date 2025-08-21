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
    use crate::parser::test_utils::check;
    use crate::parser::Parser;

    #[test]
    fn transport_delay_mechanism() {
        check(
            Parser::delay_mechanism,
            "transport",
            "\
DelayMechanism
  Keyword(Transport)
        ",
        )
    }

    #[test]
    fn intertial_delay_mechanism() {
        check(
            Parser::delay_mechanism,
            "inertial",
            "\
DelayMechanism
  Keyword(Inertial)
        ",
        )
    }

    #[test]
    fn reject_intertial_delay_mechanism() {
        check(
            Parser::delay_mechanism,
            "reject 2 ns inertial",
            "\
DelayMechanism
  Keyword(Reject)
  PhysicalLiteral
    AbstractLiteral '2'
    Name
      Identifier 'ns'
  Keyword(Inertial)
        ",
        )
    }

    #[test]
    fn waveform() {
        check(
            Parser::waveform,
            "bar(1 to 3)",
            "\
Waveform
  WaveformElement
    Name
      Identifier 'bar'
      RawTokens
        LeftPar
        AbstractLiteral '1'
        Keyword(To)
        AbstractLiteral '3'
        RightPar
        ",
        )
    }

    #[test]
    fn waveform_after() {
        check(
            Parser::waveform,
            "bar(1 to 3) after 2 ns",
            "\
Waveform
  WaveformElement
    Name
      Identifier 'bar'
      RawTokens
        LeftPar
        AbstractLiteral '1'
        Keyword(To)
        AbstractLiteral '3'
        RightPar
    Keyword(After)
    PhysicalLiteral
      AbstractLiteral '2'
      Name
        Identifier 'ns'
        ",
        )
    }

    #[test]
    fn waveform_after_many() {
        check(
            Parser::waveform,
            "bar(1 to 3) after 2 ns, expr after 1 ns",
            "\
Waveform
  WaveformElement
    Name
      Identifier 'bar'
      RawTokens
        LeftPar
        AbstractLiteral '1'
        Keyword(To)
        AbstractLiteral '3'
        RightPar
    Keyword(After)
    PhysicalLiteral
      AbstractLiteral '2'
      Name
        Identifier 'ns'
  Comma
  WaveformElement
    Name
      Identifier 'expr'
    Keyword(After)
    PhysicalLiteral
      AbstractLiteral '1'
      Name
        Identifier 'ns'
        ",
        )
    }

    #[test]
    fn unaffected_waveform() {
        check(
            Parser::waveform,
            "unaffected",
            "\
Waveform
  Keyword(Unaffected)
        ",
        )
    }
}
