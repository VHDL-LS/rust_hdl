// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::{CanParse, Parser};
use crate::tokens::Token;
use std::collections::VecDeque;

/// Returns the AST text for snapshot assertions.
pub fn to_test_text(func: impl FnOnce(&mut Parser<VecDeque<Token>>), input: &str) -> String {
    let (entity, diagnostics) = input.parse_syntax(func);
    assert!(diagnostics.is_empty(), "got diagnostics: {:?}", diagnostics);
    entity.test_text()
}
