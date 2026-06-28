// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::error::SyntaxErr;
use crate::parser::{parse_syntax, parse_syntax_with_standard, Parser};
use crate::standard::VHDLStandard;
use crate::syntax::node::{SyntaxElement, SyntaxNode};

/// Returns the AST text for snapshot assertions.
pub fn to_test_text(func: impl FnOnce(&mut Parser), input: &str) -> String {
    let (entity, diagnostics) = parse_syntax(input, func);
    assert!(diagnostics.is_empty(), "got diagnostics: {:?}", diagnostics);
    if let Err(err) = entity.validate() {
        println!("Parser <-> AST validation failed: {err}");
        for missing in err.missing() {
            println!(
                "  missing {:?} in {:?}",
                missing.kind(),
                missing.parent().kind()
            );
        }
        for extraneous in err.extraneous() {
            match extraneous {
                SyntaxElement::Node(node) => println!("  extraneous node {:?}", node.kind()),
                SyntaxElement::Token(token) => println!("  extraneous token {:?}", token.kind()),
            }
        }
        panic!();
    }
    entity.test_text()
}

pub fn diagnostics_test_text(
    diags: &[SyntaxErr],
    source: &crate::syntax::node::SyntaxNode,
) -> String {
    use crate::fmt::encoding::Utf8Encoder;
    use crate::text::{char_encoding::Utf8, source_loc::SourceLocConverter};
    use std::fmt::Write;

    let conv = SourceLocConverter::new::<Utf8Encoder, Utf8>(source).expect("Expected valid UTF-8");
    let mut out = String::new();
    for d in diags {
        let s = conv.source_loc(d.span().start);
        let e = conv.source_loc(d.span().end);
        if s == e {
            writeln!(out, "{}:{} {:?}", s.line + 1, s.col + 1, d.err())
        } else {
            writeln!(
                out,
                "{}:{}..{}:{} {:?}",
                s.line + 1,
                s.col + 1,
                e.line + 1,
                e.col + 1,
                d.err()
            )
        }
        .unwrap();
    }
    out
}

pub fn render_recovery(code: &str, tree: &SyntaxNode, diags: &[SyntaxErr]) -> String {
    format!(
        "== source ==\n{code}\n\n== tree ==\n{}\n== diagnostics ==\n{}",
        tree.test_text(),
        diagnostics_test_text(diags, tree),
    )
}

macro_rules! assert_recovery_snapshot {
    ($code:expr, $parser:expr $(,)?) => {{
        let code = $code;
        let (tree, diags) = $crate::parser::parse_syntax(code, $parser);
        insta::assert_snapshot!($crate::parser::test_utils::render_recovery(
            code, &tree, &diags
        ));
    }};
}

/// Returns the AST text for snapshot assertions, tokenizing and parsing under `standard`.
pub fn to_test_text_with_standard(
    standard: VHDLStandard,
    func: impl FnOnce(&mut Parser),
    input: &str,
) -> String {
    let (entity, diagnostics) = parse_syntax_with_standard(standard, input.bytes(), func);
    assert!(diagnostics.is_empty(), "got diagnostics: {:?}", diagnostics);
    entity.test_text()
}
