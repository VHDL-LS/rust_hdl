// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2025, Lukas Scheller lukasscheller@icloud.com

pub mod node;
pub mod token;

pub use node::*;
pub use token::*;

use convert_case::{Case, Casing};
use std::path::Path;
use std::str::FromStr;

pub fn load_model(file: &Path) -> Model {
    let mut model = Model::default();

    let grammar_str = std::fs::read_to_string(file)
        .unwrap_or_else(|err| panic!("Cannot read {}: {err}", file.display()));
    let grammar = ungrammar::Grammar::from_str(&grammar_str)
        .unwrap_or_else(|err| panic!("{}:{err}", file.display()));

    for node in grammar.iter() {
        let data = &grammar[node];
        let node = map_rule(data.name.clone(), &data.rule, &grammar);
        model.push_node(node);
    }

    model.fixup_empty_capable_optional_markers();
    model.do_checks();
    model.do_postprocessing();

    model
}

/// Maps a single grammar item (a node or token reference, possibly labelled, optional or
/// repeated) onto the model.
fn map_single(
    production: &str,
    rule: &ungrammar::Rule,
    grammar: &ungrammar::Grammar,
) -> Field {
    match rule {
        ungrammar::Rule::Labeled { label, rule } => {
            map_single(production, rule, grammar).with_name(label)
        }
        ungrammar::Rule::Node(node) => Field::node(grammar[*node].name.clone()),
        ungrammar::Rule::Token(token) => {
            let mut name = grammar[*token].name.as_str();
            if name.starts_with('#') {
                name = &name[1..];
            }
            let kind = str_to_token_kind(name)
                .or_else(|_| {
                    Keyword::from_str(&name.to_case(Case::UpperCamel)).map(TokenKind::Keyword)
                })
                .unwrap_or_else(|_| panic!("Invalid token kind {name} in production {production}"));
            Field::token(kind)
        }
        ungrammar::Rule::Opt(rule) => {
            map_single(production, rule, grammar).make_optional()
        }
        ungrammar::Rule::Rep(rule) => {
            map_single(production, rule, grammar).make_repeated()
        }
        // A group is a `Seq` or `Alt` nested inside another rule, which the model cannot
        // represent: every item of a production is a single node or token reference.
        ungrammar::Rule::Seq(_) => panic!(
            "Production {production} contains a nested sequence, e.g. `(A B)?` or `(A B)*`. \
             The model has no group item; give the group its own named production and \
             reference that instead."
        ),
        ungrammar::Rule::Alt(_) => panic!(
            "Production {production} contains a nested alternation, e.g. `A (B | C)`. \
             The model only supports an alternation as the entire body of a production; \
             give the alternation its own named production and reference that instead."
        ),
    }
}

fn map_rule(name: String, rule: &ungrammar::Rule, grammar: &ungrammar::Grammar) -> Node {
    match rule {
        ungrammar::Rule::Labeled { .. } => {
            panic!("Production {name} is a single labelled item; drop the label, the production name is the label")
        }
        ungrammar::Rule::Node(_)
        | ungrammar::Rule::Token(_)
        | ungrammar::Rule::Rep(_)
        | ungrammar::Rule::Opt(_) => {
            let mapped = map_single(&name, rule, grammar);
            Node::Items(SequenceNode {
                name,
                items: vec![mapped],
            })
        }
        ungrammar::Rule::Seq(rules) => {
            let mut mapped = Vec::new();
            for rule in rules {
                let mut next = map_single(&name, rule, grammar);
                let nth = mapped
                    .iter()
                    .filter(|el: &&Field| el.kind == next.kind)
                    .count();
                next.nth = nth;
                mapped.push(next);
            }
            Node::Items(SequenceNode {
                name,
                items: mapped,
            })
        }
        ungrammar::Rule::Alt(rules) => {
            let mapped = rules
                .iter()
                .map(|rule| map_single(&name, rule, grammar))
                .collect::<Vec<_>>();
            let result: NodesOrTokens = if mapped.iter().all(|rule| rule.as_node_kind().is_some()) {
                mapped
                    .into_iter()
                    .map(|rule| {
                        let kind = rule.as_node_kind().expect("checked above");
                        assert!(
                            rule.name == kind && !rule.optional && !rule.repeated,
                            "Alternative {kind} of production {name} is labelled, optional or \
                             repeated; an alternative must be a bare node reference."
                        );
                        kind.to_owned()
                    })
                    .collect()
            } else if mapped.iter().all(|rule| rule.as_token_kind().is_some()) {
                mapped
                    .into_iter()
                    .map(|rule| rule.into_token().expect("checked above"))
                    .collect()
            } else {
                panic!("Alternations must be either all nodes or all tokens, not a mix. Offending production: {name}");
            };
            Node::Choices(ChoiceNode {
                name,
                items: result,
            })
        }
    }
}
