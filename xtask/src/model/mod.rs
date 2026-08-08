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
/// These are empty sequences, but an ungrammar rule body must contain at least one item, so
/// they cannot be written in the `.ungram` file and are patched in here instead. Every other
/// PSL node is expressible in ungrammar and lives in the grammar file.
///
/// Must run before [`Model::fixup_empty_capable_optional_markers`]: an empty node is
/// empty-capable, which makes the choice nodes above empty-capable too.
fn add_empty_psl_nodes(model: &mut Model) {
    for (name, choice) in EMPTY_PSL_NODES {
        model.push_node(SequenceNode::new(name, vec![]));
        model.push_choice_alternative(choice, NodeRef::from(name.to_owned()));
    }
}

/// Maps a single grammar item (a node or token reference, possibly labelled, optional or
/// repeated) onto the model.
fn map_single(
    production: &str,
    rule: &ungrammar::Rule,
    grammar: &ungrammar::Grammar,
) -> TokenOrNode {
    match rule {
        ungrammar::Rule::Labeled { label, rule } => {
            let mut inner = map_single(production, rule, grammar);
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
                .unwrap_or_else(|_| panic!("Invalid token kind {name} in production {production}"));
            let name = match kind {
                TokenKind::Keyword(kw) => kw.to_string(),
                other => other.to_string(),
            };
            TokenOrNode::Token(Token {
                kind,
                name,
                nth: 0,
                repeated: false,
                optional: false,
            })
        }
        ungrammar::Rule::Opt(rule) => {
            let mut inner = map_single(production, rule, grammar);
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
            let mut inner = map_single(production, rule, grammar);
            match &mut inner {
                TokenOrNode::Node(node_ref) => {
                    node_ref.repeated = true;
                    // The accessor for a repeated node is plural. Naive, but the only names it
                    // has to handle are the node names in the grammar file.
                    if !node_ref.name.ends_with('s') {
                        node_ref.name.push('s');
                    }
                }
                TokenOrNode::Token(token) => {
                    token.repeated = true;
                }
            }
            inner
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
                .map(|rule| map_single(&name, rule, grammar))
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
                panic!("Alternations must be either all nodes or all tokens, not a mix. Offending production: {name}");
            };
            Node::Choices(ChoiceNode {
                name,
                items: result,
            })
        }
    }
}
