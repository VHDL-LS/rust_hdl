// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::tokens::token_kind::Keyword as Kw;
use crate::tokens::TokenKind::*;
use crate::{parser::Parser, syntax::NodeKind};

impl Parser {
    pub fn verification_unit_binding_indication(&mut self) {
        self.start_node(NodeKind::VerificationUnitBindingIndication);
        self.expect_tokens([Keyword(Kw::Use), Keyword(Kw::Vunit)]);
        self.verification_unit_list();
        self.end_node();
    }

    pub fn verification_unit_list(&mut self) {
        self.start_node(NodeKind::VerificationUnitList);
        self.separated_list(Parser::name, Comma);
        self.end_node();
    }
}
