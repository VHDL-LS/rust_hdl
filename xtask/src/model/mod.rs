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
        let node = map_rule(
            NodeKind::from(data.name.clone()),
            &data.rule,
            &grammar,
            &mut model,
        );
        model.push_node(node);
    }

    model.fixup_empty_capable_optional_markers();
    model.do_checks();
    model.do_postprocessing();

    model
}

/// The `?` / `*` that a labelled group carries
#[derive(Clone, Copy)]
enum GroupMarker {
    None,
    Optional,
    Repeated,
}

impl GroupMarker {
    /// Applies the marker to the field
    fn apply(self, field: Field) -> Field {
        match self {
            GroupMarker::None => field,
            GroupMarker::Optional => field.make_optional(),
            GroupMarker::Repeated => field.make_repeated(),
        }
    }
}

/// Maps a single grammar item (a node or token reference, possibly labelled, optional or
/// repeated) onto the model.
fn map_single(
    production: &str,
    rule: &ungrammar::Rule,
    grammar: &ungrammar::Grammar,
    model: &mut Model,
) -> Field {
    match rule {
        ungrammar::Rule::Labeled { label, rule } => {
            let (inner, marker) = match rule.as_ref() {
                ungrammar::Rule::Opt(inner) => (inner, GroupMarker::Optional),
                ungrammar::Rule::Rep(inner) => (inner, GroupMarker::Repeated),
                _ => (rule, GroupMarker::None),
            };
            let mut node = map_rule(label.to_case(Case::Pascal).into(), inner, grammar, model);
            // Collapse single items `name:Production`
            // TODO: revisit this decision
            if let Node::Items(sequence_node) = &mut node {
                if sequence_node.items.len() == 1 {
                    let field = sequence_node.items.pop().unwrap();
                    return marker.apply(field).with_name(label);
                }
            }
            let field = marker.apply(Field::node(node.name()));
            model.push_node(node);
            field
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
        ungrammar::Rule::Opt(rule) => map_single(production, rule, grammar, model).make_optional(),
        ungrammar::Rule::Rep(rule) => map_single(production, rule, grammar, model).make_repeated(),
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

/// An alternation stores only the kind of each alternative, so anything else the grammar could
/// attach to it (a label, `?`, `*`) would be silently dropped. Reject it instead.
fn assert_bare_alternative(production: &str, kind: &str, item: &Field) {
    assert!(
        item.name == kind && !item.may_be_absent(),
        "Alternative {kind} of production {production} is labelled, optional or repeated; \
         an alternative must be a bare node or token reference."
    );
}

fn map_rule(
    name: NodeKind,
    rule: &ungrammar::Rule,
    grammar: &ungrammar::Grammar,
    model: &mut Model,
) -> Node {
    match rule {
        ungrammar::Rule::Labeled { .. } => {
            panic!("Production {name} is a single labelled item; drop the label, the production name is the label")
        }
        ungrammar::Rule::Node(_)
        | ungrammar::Rule::Token(_)
        | ungrammar::Rule::Rep(_)
        | ungrammar::Rule::Opt(_) => {
            let mapped = map_single(name.as_str(), rule, grammar, model);
            Node::Items(SequenceNode {
                name,
                items: vec![mapped],
            })
        }
        ungrammar::Rule::Seq(rules) => {
            let mut mapped = Vec::new();
            for rule in rules {
                let next = map_single(name.as_str(), rule, grammar, model);
                let nth = mapped
                    .iter()
                    .filter(|el: &&Field| el.kind == next.kind)
                    .count();
                mapped.push(next.with_nth(nth));
            }
            Node::Items(SequenceNode {
                name,
                items: mapped,
            })
        }
        ungrammar::Rule::Alt(rules) => {
            let mapped = rules
                .iter()
                .map(|rule| map_single(name.as_str(), rule, grammar, model))
                .collect::<Vec<_>>();
            let result: NodesOrTokens = if mapped.iter().all(|rule| rule.as_node_kind().is_some()) {
                mapped
                    .iter()
                    .map(|rule| {
                        let kind = rule.as_node_kind().expect("checked above");
                        assert_bare_alternative(name.as_str(), kind.as_str(), rule);
                        kind.to_owned()
                    })
                    .collect()
            } else if mapped.iter().all(|rule| rule.as_token_kind().is_some()) {
                mapped
                    .iter()
                    .map(|rule| {
                        let kind = *rule.as_token_kind().expect("checked above");
                        assert_bare_alternative(name.as_str(), &kind.default_name(), rule);
                        kind
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
