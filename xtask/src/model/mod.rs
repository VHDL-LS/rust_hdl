// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2025, Lukas Scheller lukasscheller@icloud.com

pub mod node;
pub mod token;

pub use node::*;
pub use token::*;

use crate::config::yaml;
use convert_case::{Case, Casing};
use std::fs::File;
use std::path::Path;
use std::str::FromStr;

/// Read all `.yaml` files from `definitions_dir`, deserialize, convert, validate,
/// postprocess, and return a [Model].
pub fn load_model(definitions_dir: &Path) -> Model {
    let mut model = Model::default();

    let mut entries: Vec<_> = std::fs::read_dir(definitions_dir)
        .unwrap_or_else(|err| {
            panic!(
                "cannot read definitions dir {}: {err}",
                definitions_dir.display()
            )
        })
        .filter_map(|e| e.ok())
        .filter(|e| {
            e.path()
                .extension()
                .map(|ext| ext == "yaml")
                .unwrap_or(false)
        })
        .collect();

    // Sort for deterministic section ordering
    entries.sort_by_key(|e| e.path());

    for entry in entries {
        let path = entry.path();
        let file =
            File::open(&path).unwrap_or_else(|err| panic!("cannot open {}: {err}", path.display()));
        let nodes: yaml::Nodes = serde_yml::from_reader(file)
            .unwrap_or_else(|err| panic!("{err} while processing {}", path.display()));
        model.insert_ser_nodes(nodes);
    }

    model.fixup_empty_capable_optional_markers();
    model.do_checks();
    model.do_postprocessing();
    model
}

pub fn load_model_ungramar(file: &Path) -> Model {
    let mut model = Model::default();

    let grammar_str = std::fs::read_to_string(file).unwrap();
    let grammar = ungrammar::Grammar::from_str(&grammar_str).unwrap();

    for node in grammar.iter() {
        let data = &grammar[node];
        let node = map_rule(data.name.clone(), &data.rule, &grammar);
        model.push_node(node);
    }

    add_empty_psl_nodes(&mut model);

    model.fixup_empty_capable_optional_markers();
    model.do_checks();
    model.do_postprocessing();

    model
}

/// The empty PSL placeholder nodes, paired with the choice node each is an alternative of.
///
/// PSL is not supported yet; the parser only produces placeholder nodes for it.
const EMPTY_PSL_NODES: [(&str, &str); 4] = [
    ("PslDirective", "ConcurrentStatement"),
    ("PslPropertyDeclaration", "Declaration"),
    ("PslSequenceDeclaration", "Declaration"),
    ("PslClockDeclaration", "Declaration"),
];

/// Adds the empty PSL placeholder nodes and their use sites to the model.
///
/// These are `!Sequence [ ]` in the YAML definitions, but an ungrammar rule body must contain
/// at least one item, so they cannot be written in the `.ungram` file and are patched in here
/// instead. Every other PSL node is expressible in ungrammar and lives in the grammar file.
///
/// Must run before [`Model::fixup_empty_capable_optional_markers`]: an empty node is
/// empty-capable, which makes the choice nodes above empty-capable too.
fn add_empty_psl_nodes(model: &mut Model) {
    for (name, choice) in EMPTY_PSL_NODES {
        model.push_node(SequenceNode::new(name, vec![]));
        model.push_choice_alternative(choice, NodeRef::from(name.to_owned()));
    }
}

fn map_single(rule: &ungrammar::Rule, grammar: &ungrammar::Grammar) -> TokenOrNode {
    match rule {
        ungrammar::Rule::Labeled { label, rule } => {
            let mut inner = map_single(rule, grammar);
            match &mut inner {
                TokenOrNode::Node(node_ref) => {
                    node_ref.name = label.clone();
                }
                TokenOrNode::Token(token) => {
                    token.name = label.clone();
                }
            }
            inner
        }
        ungrammar::Rule::Node(node) => {
            let name = &grammar[*node].name;
            TokenOrNode::Node(NodeRef {
                kind: name.clone(),
                nth: 0,
                builtin: false,
                repeated: false,
                name: name.clone(),
                optional: false,
            })
        }
        ungrammar::Rule::Token(token) => {
            let mut name = grammar[*token].name.as_str();
            if name.starts_with('#') {
                name = &name[1..];
            }
            let kind = str_to_token_kind(name)
                .or_else(|_| {
                    Keyword::from_str(&name.to_case(Case::UpperCamel)).map(TokenKind::Keyword)
                })
                .expect(&format!("Invalid token kind {}", name));
            let name = match kind {
                TokenKind::Keyword(kw) => kw.to_string(),
                other => other.to_string(),
            };
            TokenOrNode::Token(Token {
                kind,
                name: name,
                nth: 0,
                repeated: false,
                optional: false,
            })
        }
        ungrammar::Rule::Opt(rule) => {
            let mut inner = map_single(rule, grammar);
            match &mut inner {
                TokenOrNode::Node(node_ref) => {
                    node_ref.optional = true;
                }
                TokenOrNode::Token(token) => {
                    token.optional = true;
                }
            }
            inner
        }
        ungrammar::Rule::Rep(rule) => {
            let mut inner = map_single(rule, grammar);
            match &mut inner {
                TokenOrNode::Node(node_ref) => {
                    node_ref.repeated = true;
                    node_ref.name.push('s');
                }
                TokenOrNode::Token(token) => {
                    token.repeated = true;
                }
            }
            inner
        }
        ungrammar::Rule::Seq(_) => unreachable!("map single"),
        ungrammar::Rule::Alt(_) => unreachable!("map single"),
    }
}

fn map_rule(name: String, rule: &ungrammar::Rule, grammar: &ungrammar::Grammar) -> Node {
    match rule {
        ungrammar::Rule::Labeled { .. } => unreachable!("Labeled at top level"),
        ungrammar::Rule::Node(_)
        | ungrammar::Rule::Token(_)
        | ungrammar::Rule::Rep(_)
        | ungrammar::Rule::Opt(_) => {
            let mapped = map_single(rule, grammar);
            Node::Items(SequenceNode {
                name,
                items: vec![mapped],
            })
        }
        ungrammar::Rule::Seq(rules) => {
            let mut mapped = Vec::new();
            for rule in rules {
                let mut next = map_single(rule, grammar);
                let nth = mapped
                    .iter()
                    .filter(|el| match (el, &next) {
                        (TokenOrNode::Node(prev_ref), TokenOrNode::Node(curr_rev)) => {
                            prev_ref.kind == curr_rev.kind
                        }
                        (TokenOrNode::Token(prev_token), TokenOrNode::Token(curr_token)) => {
                            prev_token.kind == curr_token.kind
                        }
                        _ => false,
                    })
                    .count();
                match &mut next {
                    TokenOrNode::Node(node_ref) => {
                        node_ref.nth = nth;
                    }
                    TokenOrNode::Token(token) => {
                        token.nth = nth;
                    }
                }
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
                .map(|rule| map_single(rule, grammar))
                .collect::<Vec<_>>();
            let result: NodesOrTokens = if mapped
                .iter()
                .all(|rule| matches!(rule, TokenOrNode::Node(_)))
            {
                mapped
                    .into_iter()
                    .map(|rule| match rule {
                        TokenOrNode::Node(node) => node,
                        _ => unreachable!(),
                    })
                    .collect()
            } else if mapped
                .iter()
                .all(|rule| matches!(rule, TokenOrNode::Token(_)))
            {
                mapped
                    .into_iter()
                    .map(|rule| match rule {
                        TokenOrNode::Node(_) => unreachable!(),
                        TokenOrNode::Token(tok) => tok,
                    })
                    .collect()
            } else {
                panic!("Choices must be either all tokens or all choices. Offending rule: {name}");
            };
            Node::Choices(ChoiceNode {
                name,
                items: result,
            })
        }
    }
}
