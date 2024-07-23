// This Source Code Form is subject to the terms of the Mozilla Public
// Lic// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// This Source Code Form is subject to the terms of the Mozilla Public
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2024, Olof Kraigher olof.kraigher@gmail.com

use crate::ast::token_range::WithTokenSpan;
use crate::ast::Expression;
use crate::formatting::buffer::Buffer;
use crate::VHDLFormatter;

impl VHDLFormatter<'_> {
    pub(crate) fn format_opt_severity(
        &self,
        severity: Option<&WithTokenSpan<Expression>>,
        buffer: &mut Buffer,
    ) {
        if let Some(severity) = &severity {
            buffer.push_whitespace();
            self.format_token_id(severity.span.start_token - 1, buffer);
            buffer.push_whitespace();
            self.format_expression(severity.as_ref(), buffer);
        }
    }
}
