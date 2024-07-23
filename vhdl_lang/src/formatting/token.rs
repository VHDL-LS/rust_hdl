// This Source Code Form is subject to the terms of the Mozilla Public
// Lic// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// This Source Code Form is subject to the terms of the Mozilla Public
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2024, Olof Kraigher olof.kraigher@gmail.com

use crate::ast::WithDecl;
use crate::formatting::buffer::Buffer;
use crate::formatting::VHDLFormatter;
use crate::{TokenAccess, TokenId};
use std::cmp::max;
use vhdl_lang::ast::Ident;
use vhdl_lang::TokenSpan;

impl VHDLFormatter<'_> {
    pub(crate) fn format_token_id(&self, id: TokenId, buffer: &mut Buffer) {
        buffer.push_token(self.tokens.get_token(id));
    }

    pub(crate) fn format_token_span(&self, span: TokenSpan, buffer: &mut Buffer) {
        for (index, id) in span.iter().enumerate() {
            self.format_token_id(id, buffer);
            if index < span.len() - 1 {
                buffer.push_whitespace();
            }
        }
    }

    pub(crate) fn join_token_span(&self, span: TokenSpan, buffer: &mut Buffer) {
        for id in span.iter() {
            self.format_token_id(id, buffer);
        }
    }

    pub(crate) fn format_ident(&self, ident: &WithDecl<Ident>, buffer: &mut Buffer) {
        self.format_token_id(ident.tree.token, buffer)
    }

    pub(crate) fn line_break_preserve_whitespace(&self, token_id: TokenId, buffer: &mut Buffer) {
        let current_line = self.tokens.get_pos(token_id).end().line;
        // TODO: token_id + 1 might panic
        let next_line = self.tokens.get_token(token_id + 1).full_range().start.line;
        let numbers_of_whitespaces = max(next_line - current_line, 1);
        buffer.line_breaks(numbers_of_whitespaces)
    }
}
