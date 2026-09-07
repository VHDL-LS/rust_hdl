// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2026, Lukas Scheller lukasscheller@icloud.com
#![no_main]

use arbitrary::{Arbitrary, Unstructured};
use libfuzzer_sys::fuzz_target;
use std::fmt::Debug;
use std::str::FromStr;
use std::sync::LazyLock;
use vhdl_syntax::parser::parse;
use vhdl_syntax::tokens::{TokenKind, TokenStream, Tokenizer, Trivia};

/// Path of the grammar, relative to this crate's manifest directory.
const UNGRAMMAR_PATH: &str = "../xtask/doc/vhdl-08.ungram";

fn get_ungrammar() -> ungrammar::Grammar {
    // Embedded at compile time so that the fuzz target does not depend on the
    // working directory it happens to be run from.
    let grammar_str = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../xtask/doc/vhdl-08.ungram"
    ));
    ungrammar::Grammar::from_str(grammar_str).unwrap_or_else(|err| panic!("{UNGRAMMAR_PATH}:{err}"))
}

static DESIGN_FILE: LazyLock<ungrammar::Grammar> = LazyLock::new(get_ungrammar);

struct Design {
    values: Vec<vhdl_syntax::tokens::Token>,
}

impl Debug for Design {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, val) in self.values.iter().enumerate() {
            if i != 0 {
                write!(f, " ")?;
            }
            write!(f, "{}", val.text())?;
        }
        Ok(())
    }
}

fn get_node(name: &str) -> ungrammar::Node {
    DESIGN_FILE
        .iter()
        .find(|node| DESIGN_FILE[*node].name == name)
        .unwrap()
}

fn map_token(token: &ungrammar::Token) -> vhdl_syntax::tokens::Token {
    let tok = DESIGN_FILE[*token].name.clone();
    if tok.starts_with('#') {
        return match &tok[1..] {
            "identifier" => {
                vhdl_syntax::tokens::Token::new(TokenKind::Identifier, b"i", Trivia::default())
            }
            "character_literal" => vhdl_syntax::tokens::Token::new(
                TokenKind::CharacterLiteral,
                b"'c'",
                Trivia::default(),
            ),
            "string_literal" => vhdl_syntax::tokens::Token::new(
                TokenKind::StringLiteral,
                b"\"s\"",
                Trivia::default(),
            ),
            "abstract_literal" => {
                vhdl_syntax::tokens::Token::new(TokenKind::AbstractLiteral, b"1", Trivia::default())
            }
            "bit_string_literal" => vhdl_syntax::tokens::Token::new(
                TokenKind::BitStringLiteral,
                b"b\"0101\"",
                Trivia::default(),
            ),
            _ => panic!("unhandled token kind {}", tok),
        };
    }
    let (tok, err) = Tokenizer::new(tok.bytes()).next().unwrap();
    assert!(err.is_none());
    tok
}

/// Beyond this nesting depth, `Opt` and `Rep` stop expanding so that recursive
/// productions get a chance to bottom out.
const SOFT_DEPTH_LIMIT: usize = 20;
/// Hard cap on nesting depth. The grammar is left-recursive in places, and a
/// production can recurse without consuming any data from `Unstructured`
/// (e.g. a `Seq` containing only a `Node`), so the recursion has to be cut off
/// explicitly. Exceeding it rejects the input instead of overflowing the stack.
const HARD_DEPTH_LIMIT: usize = 40;
/// Upper bound on the number of repetitions generated for a `Rep` rule.
const MAX_REPETITIONS: usize = 8;

fn choose_rule(
    rule: &ungrammar::Rule,
    u: &mut Unstructured<'_>,
    depth: usize,
) -> arbitrary::Result<Design> {
    match rule {
        ungrammar::Rule::Labeled { label: _, rule } => choose_rule(rule.as_ref(), u, depth),
        ungrammar::Rule::Node(node) => choose_node(*node, u, depth),
        ungrammar::Rule::Token(token) => Ok(Design {
            values: vec![map_token(token)],
        }),
        ungrammar::Rule::Seq(rules) => {
            let mut design = Vec::new();
            for rule in rules {
                let mut res = choose_rule(rule, u, depth)?;
                design.append(&mut res.values);
            }
            Ok(Design { values: design })
        }
        ungrammar::Rule::Alt(rules) => {
            let chosen = u.choose(rules)?;
            choose_rule(chosen, u, depth)
        }
        ungrammar::Rule::Opt(rule) => {
            if depth < SOFT_DEPTH_LIMIT && *u.choose(&[true, false])? {
                choose_rule(rule, u, depth)
            } else {
                Ok(Design { values: vec![] })
            }
        }
        ungrammar::Rule::Rep(rule) => {
            let len = if depth < SOFT_DEPTH_LIMIT {
                u.arbitrary_len::<Design>()?.min(MAX_REPETITIONS)
            } else {
                0
            };
            let mut design = Vec::new();
            for _ in 0..len {
                let mut res = choose_rule(rule, u, depth)?;
                design.append(&mut res.values);
            }
            Ok(Design { values: design })
        }
    }
}

fn choose_node(
    node: ungrammar::Node,
    u: &mut Unstructured<'_>,
    depth: usize,
) -> arbitrary::Result<Design> {
    if DESIGN_FILE[node].name == "Name" {
        return Ok(Design {
            values: vec![vhdl_syntax::tokens::Token::new(
                TokenKind::Identifier,
                b"name",
                Trivia::default(),
            )],
        });
    } else if DESIGN_FILE[node].name == "SubtypeIndication" {
        return Ok(Design {
            values: vec![vhdl_syntax::tokens::Token::new(
                TokenKind::Identifier,
                b"subtype_indication",
                Trivia::default(),
            )],
        });
    }
    if depth >= HARD_DEPTH_LIMIT {
        return Err(arbitrary::Error::IncorrectFormat);
    }
    choose_rule(&DESIGN_FILE[node].rule, u, depth + 1)
}

impl Arbitrary<'_> for Design {
    fn arbitrary(u: &mut Unstructured<'_>) -> arbitrary::Result<Self> {
        let root = get_node("DesignFile");
        choose_node(root, u, 0)
    }
}

fuzz_target!(|data: Design| {
    let (_file, diagnostics) = parse(TokenStream::from_tokens(data.values));
    assert!(
        diagnostics.is_empty(),
        "got diagnostics:\n{:?}",
        diagnostics
    )
});
