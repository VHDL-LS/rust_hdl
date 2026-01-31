// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::{parse_syntax, Parser};

/// Returns the AST text for snapshot assertions.
pub fn to_test_text(func: impl FnOnce(&mut Parser), input: &str) -> String {
    let (entity, diagnostics) = parse_syntax(input, func);
    assert!(diagnostics.is_empty(), "got diagnostics: {:?}", diagnostics);
    entity.test_text()
}
