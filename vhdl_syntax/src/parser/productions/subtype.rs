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

fn is_start_of_name<T: TokenStream>(parser: & Parser<T>) -> bool {
    return parser.next_is_one_of([LtLt, Identifier, StringLiteral, CharacterLiteral]);
}

impl<T: TokenStream> Parser<T> {
    fn resolution_indication(&mut self) {
        if self.next_is(LeftPar) {
            self.skip();
            self.element_resolution();
            self.expect_token(RightPar);
        } else {
            self.name();
        }
    }

    fn element_resolution(&mut self) {
        // TODO: This should be prettier (or solved diferently)
        if self.next_is(LeftPar) {
            self.resolution_indication();
        } else {
            let checkpoint = self.checkpoint();
            self.name();
            if is_start_of_name(self) {
                self.resolution_indication();
                self.start_node_at(checkpoint, RecordElementResolution);
            } else {
                return;
            }
            while self.next_is(Comma) {
                self.start_node(RecordElementResolution);
                self.identifier();
                self.resolution_indication();
                self.end_node();
            }
        }
    }

    pub fn subtype_indication(&mut self) {
        self.start_node(SubtypeIndication);
        if self.next_is(LeftPar) {
            self.resolution_indication();
        } else {
            self.name();
        }
        if is_start_of_name(self) {
            self.name();
        } 
        self.opt_constraint();
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
