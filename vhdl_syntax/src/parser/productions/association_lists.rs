// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind::{GenericMapAspect, PortMapAspect};
use crate::tokens::Keyword as Kw;
use crate::tokens::TokenKind::*;

impl Parser {
    fn map_aspect(&mut self, kind: Kw) {
        match kind {
            Kw::Generic => self.start_node(GenericMapAspect),
            Kw::Port => self.start_node(PortMapAspect),
            _ => unreachable!(),
        }
        self.expect_tokens([Keyword(kind), Keyword(Kw::Map), LeftPar]);
        self.association_list();
        self.expect_token(RightPar);
        self.end_node();
    }

    fn opt_map_aspect(&mut self, kind: Kw) -> bool {
        if self.next_is(Keyword(kind)) && self.next_nth_is(Keyword(Kw::Map), 1) {
            self.map_aspect(kind);
            true
        } else {
            false
        }
    }

    pub fn port_map_aspect(&mut self) {
        self.map_aspect(Kw::Port)
    }

    pub(crate) fn opt_port_map_aspect(&mut self) -> bool {
        self.opt_map_aspect(Kw::Port)
    }

    pub fn generic_map_aspect(&mut self) {
        self.map_aspect(Kw::Generic)
    }

    pub(crate) fn opt_generic_map_aspect(&mut self) -> bool {
        self.opt_map_aspect(Kw::Generic)
    }
}
