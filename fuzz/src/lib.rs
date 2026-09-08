// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2026, Lukas Scheller lukasscheller@icloud.com

use arbitrary::{Arbitrary, Unstructured};
use std::collections::HashMap;
use std::fmt::Debug;
use std::marker::PhantomData;
use std::str::FromStr;
use vhdl_syntax::tokens::{Token, TokenKind, Tokenizer, Trivia};

/// Upper bound on the number of repetitions generated for a `Rep` rule.
const MAX_REPETITIONS: usize = 8;

pub struct Grammar {
    grammar: ungrammar::Grammar,
    root: ungrammar::Node,
    terminals: HashMap<ungrammar::Token, Token>,
    opaque: HashMap<ungrammar::Node, Token>,
}

impl Grammar {
    /// Parse `source` and resolve to a `Grammar`.
    pub fn new(path: &str, source: &str, opaque: &[(&str, &[u8])]) -> Grammar {
        let grammar =
            ungrammar::Grammar::from_str(source).unwrap_or_else(|err| panic!("{path}: {err}"));

        let node_by_name = |name: &str| {
            grammar
                .iter()
                .find(|node| grammar[*node].name == name)
                .unwrap_or_else(|| panic!("{path}: no node named `{name}`"))
        };

        let root = node_by_name("DesignFile");
        let terminals = grammar
            .tokens()
            .map(|token| (token, terminal_token(path, &grammar[token].name)))
            .collect();
        let opaque = opaque
            .iter()
            .map(|(name, text)| {
                let token = Token::new(TokenKind::Identifier, *text, Trivia::default());
                (node_by_name(name), token)
            })
            .collect();

        Grammar {
            grammar,
            root,
            terminals,
            opaque,
        }
    }
}

/// Build the token a grammar terminal stands for.
fn terminal_token(path: &str, name: &str) -> Token {
    let literal = |kind, text: &[u8]| Token::new(kind, text, Trivia::default());
    if let Some(class) = name.strip_prefix('#') {
        return match class {
            "identifier" => literal(TokenKind::Identifier, b"i"),
            "character_literal" => literal(TokenKind::CharacterLiteral, b"'c'"),
            "string_literal" => literal(TokenKind::StringLiteral, b"\"s\""),
            "abstract_literal" => literal(TokenKind::AbstractLiteral, b"1"),
            "bit_string_literal" => literal(TokenKind::BitStringLiteral, b"b\"0101\""),
            "eof" => literal(TokenKind::Eof, b""),
            _ => panic!("{path}: unhandled token class `{name}`"),
        };
    }
    let (token, err) = Tokenizer::new(name.bytes())
        .next()
        .unwrap_or_else(|| panic!("{path}: terminal `{name}` tokenizes to nothing"));
    assert!(
        err.is_none(),
        "{path}: terminal `{name}` does not tokenize cleanly: {err:?}"
    );
    token
}

/// One of the grammars a [`Design`] can be generated from.
pub trait GrammarSource: 'static {
    fn grammar() -> &'static Grammar;
}

/// A design file generated from `G`, as the tokens the parser would receive.
pub struct Design<G: GrammarSource> {
    pub tokens: Vec<Token>,
    grammar: PhantomData<G>,
}

impl<G: GrammarSource> Debug for Design<G> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, token) in self.tokens.iter().enumerate() {
            if i != 0 {
                write!(f, " ")?;
            }
            write!(f, "{}", token.text())?;
        }
        Ok(())
    }
}

impl<G: GrammarSource> Arbitrary<'_> for Design<G> {
    fn arbitrary(u: &mut Unstructured<'_>) -> arbitrary::Result<Self> {
        let grammar = G::grammar();
        let mut generator = Generator {
            grammar,
            tokens: Vec::new(),
        };
        generator.node(grammar.root, u, 0)?;
        if generator.tokens.last().map(Token::kind) != Some(TokenKind::Eof) {
            generator
                .tokens
                .push(Token::new(TokenKind::Eof, b"", Trivia::default()));
        }
        Ok(Design {
            tokens: generator.tokens,
            grammar: PhantomData,
        })
    }
}

struct Generator<'g> {
    grammar: &'g Grammar,
    tokens: Vec<Token>,
}

impl Generator<'_> {
    fn rule(
        &mut self,
        rule: &ungrammar::Rule,
        u: &mut Unstructured<'_>,
        depth: usize,
    ) -> arbitrary::Result<()> {
        match rule {
            ungrammar::Rule::Labeled { label: _, rule } => self.rule(rule, u, depth),
            ungrammar::Rule::Node(node) => self.node(*node, u, depth),
            ungrammar::Rule::Token(token) => {
                self.tokens.push(self.grammar.terminals[token].clone());
                Ok(())
            }
            ungrammar::Rule::Seq(rules) => {
                for rule in rules {
                    self.rule(rule, u, depth)?;
                }
                Ok(())
            }
            ungrammar::Rule::Alt(rules) => {
                let chosen = u.choose(rules)?;
                self.rule(chosen, u, depth)
            }
            ungrammar::Rule::Opt(rule) => {
                if *u.choose(&[false, true])? {
                    self.rule(rule, u, depth)?;
                }
                Ok(())
            }
            ungrammar::Rule::Rep(rule) => {
                let len = u.int_in_range(0..=MAX_REPETITIONS)?;
                for _ in 0..len {
                    self.rule(rule, u, depth)?;
                }
                Ok(())
            }
        }
    }

    fn node(
        &mut self,
        node: ungrammar::Node,
        u: &mut Unstructured<'_>,
        depth: usize,
    ) -> arbitrary::Result<()> {
        if let Some(token) = self.grammar.opaque.get(&node) {
            self.tokens.push(token.clone());
            return Ok(());
        }
        let rule = &self.grammar.grammar[node].rule;
        self.rule(rule, u, depth + 1)
    }
}

pub fn assert_parses<G: GrammarSource>(design: Design<G>) {
    let (_file, diagnostics) =
        vhdl_syntax::parser::parse(vhdl_syntax::tokens::TokenStream::from_tokens(design.tokens));
    assert!(diagnostics.is_empty(), "got diagnostics:\n{diagnostics:?}");
}
