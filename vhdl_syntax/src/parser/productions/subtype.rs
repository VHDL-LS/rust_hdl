// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::Parser;
use crate::tokens::TokenStream;
use crate::syntax::NodeKind::*;
use crate::tokens::TokenKind::*;
use crate::tokens::Keyword as Kw;

impl<T: TokenStream> Parser<T> {
    pub fn subtype_indication(&mut self) {
        self.start_node(SubtypeIndication);
        if self.next_is(LeftPar) {
            todo!()
        } else {
            self.name();
        }
        self.opt_constraint();
        // TODO: constraints
        self.end_node();
    }

    fn opt_constraint(&mut self) {
        if self.next_is(Keyword(Kw::Range)) {
            self.start_node(RangeConstraint);
            self.skip();
            self.range();
            self.end_node();
        }
        // all other constraints are handled by `name`
    }
}
