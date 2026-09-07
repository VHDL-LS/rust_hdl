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

static GRAMMAR: LazyLock<ungrammar::Grammar> = LazyLock::new(get_ungrammar);

struct Tokens {
    values: Vec<vhdl_syntax::tokens::Token>,
}

impl Debug for Tokens {
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
    GRAMMAR
        .iter()
        .find(|node| GRAMMAR[*node].name == name)
        .unwrap()
}

fn map_token(token: &ungrammar::Token) -> vhdl_syntax::tokens::Token {
    let tok = GRAMMAR[*token].name.clone();
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
/// Upper bound on the number of repetitions generated for a `Rep` rule.
const MAX_REPETITIONS: usize = 8;

fn choose_rule(
    rule: &ungrammar::Rule,
    u: &mut Unstructured<'_>,
    depth: usize,
) -> arbitrary::Result<Tokens> {
    match rule {
        ungrammar::Rule::Labeled { label: _, rule } => choose_rule(rule.as_ref(), u, depth),
        ungrammar::Rule::Node(node) => choose_node(*node, u, depth),
        ungrammar::Rule::Token(token) => Ok(Tokens {
            values: vec![map_token(token)],
        }),
        ungrammar::Rule::Seq(rules) => {
            let mut design = Vec::new();
            for rule in rules {
                let mut res = choose_rule(rule, u, depth)?;
                design.append(&mut res.values);
            }
            Ok(Tokens { values: design })
        }
        ungrammar::Rule::Alt(rules) => {
            let chosen = u.choose(rules)?;
            choose_rule(chosen, u, depth)
        }
        ungrammar::Rule::Opt(rule) => {
            if depth < SOFT_DEPTH_LIMIT && *u.choose(&[false, true])? {
                choose_rule(rule, u, depth)
            } else {
                Ok(Tokens { values: vec![] })
            }
        }
        ungrammar::Rule::Rep(rule) => {
            let len = u.int_in_range(0..=MAX_REPETITIONS)?;
            let mut design = Vec::new();
            for _ in 0..len {
                let mut res = choose_rule(rule, u, depth)?;
                design.append(&mut res.values);
            }
            Ok(Tokens { values: design })
        }
    }
}

fn choose_node(
    node: ungrammar::Node,
    u: &mut Unstructured<'_>,
    depth: usize,
) -> arbitrary::Result<Tokens> {
    // Names and subtype indications are skipped:
    // they incorporate behaviour in vhdl_ls from the LRM that is not reflected
    // in the grammar
    if GRAMMAR[node].name == "Name" {
        return Ok(Tokens {
            values: vec![vhdl_syntax::tokens::Token::new(
                TokenKind::Identifier,
                b"name",
                Trivia::default(),
            )],
        });
    } else if GRAMMAR[node].name == "SubtypeIndication" {
        return Ok(Tokens {
            values: vec![vhdl_syntax::tokens::Token::new(
                TokenKind::Identifier,
                b"subtype_indication",
                Trivia::default(),
            )],
        });
    }
    choose_rule(&GRAMMAR[node].rule, u, depth + 1)
}

impl Arbitrary<'_> for Tokens {
    fn arbitrary(u: &mut Unstructured<'_>) -> arbitrary::Result<Self> {
        let root = get_node("DesignFile");
        let mut result = choose_node(root, u, 0)?;
        result.values.push(vhdl_syntax::tokens::Token::new(
            TokenKind::Eof,
            b"",
            Trivia::default(),
        ));
        Ok(result)
    }
}

fuzz_target!(|data: Tokens| {
    let (_file, diagnostics) = parse(TokenStream::from_tokens(data.values));
    assert!(
        diagnostics.is_empty(),
        "got diagnostics:\n{:?}",
        diagnostics
    )
});
