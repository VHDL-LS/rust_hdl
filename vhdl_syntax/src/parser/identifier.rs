// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com
/// Parsing of identifiers
use crate::parser::Parser;
use crate::tokens::TokenKind;
use crate::tokens::TokenStream;

impl<T: TokenStream> Parser<T> {
    pub fn identifier(&mut self) {
        self.expect_token(TokenKind::Identifier)
    }

    pub fn opt_identifier(&mut self) -> bool {
        self.opt_token(TokenKind::Identifier)
    }
}
