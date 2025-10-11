// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::{CanParse, Parser};
use crate::tokens::Token;
use pretty_assertions::assert_eq;
use std::collections::VecDeque;

pub fn check(func: impl FnOnce(&mut Parser<VecDeque<Token>>), input: &str, output: &str) {
    let (entity, diagnostics) = input.parse_syntax(func);
    assert!(diagnostics.is_empty(), "got diagnostics: {:?}", diagnostics);
    assert_eq!(output.trim(), entity.test_text().trim());
}
