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
use ungrammar::Rule;

pub fn load_model(file: &Path) -> Model {
    let grammar_str = std::fs::read_to_string(file)
        .unwrap_or_else(|err| panic!("Cannot read {}: {err}", file.display()));
    let grammar = ungrammar::Grammar::from_str(&grammar_str)
        .unwrap_or_else(|err| panic!("{}:{err}", file.display()));
    map_grammar(&grammar)
}

/// Maps a parsed ungrammar onto the model, checked and postprocessed.
fn map_grammar(grammar: &ungrammar::Grammar) -> Model {
    let mut model = Model::default();

    for node in grammar.iter() {
        let data = &grammar[node];
        let node = map_rule(
            NodeKind::from(data.name.clone()),
            &data.rule,
            grammar,
            &mut model,
        );
        model.push_node(node);
    }

    model.fixup_nth_by_resolved_kind();
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
                ungrammar::Rule::Token(_) | ungrammar::Rule::Node(_) => {
                    let kind: NodeKind = label.to_case(Case::Pascal).into();
                    let node = Node::Items(SequenceNode {
                        name: kind.clone(),
                        items: vec![map_single(production, rule, grammar, model)],
                    });
                    model.push_node(node);
                    return Field::node(kind);
                }
                _ => (rule, GroupMarker::None),
            };
            let node = map_rule(label.to_case(Case::Pascal).into(), inner, grammar, model);
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
        // `Foo = Bar` introduces no structure of its own: it gives `Bar` a second name, which is
        // what a choice needs to name its alternatives (`ActualPartOpen = 'open'`). The tree
        // holds a `Bar`, never a `Foo`.
        ungrammar::Rule::Node(_) | ungrammar::Rule::Token(_) => {
            let aliased = map_single(name.as_str(), rule, grammar, model);
            Node::Alias(AliasNode::new(name, aliased.kind))
        }
        // `Foo = Bar?` / `Foo = Bar*` do: the node is what carries the "absent" and the
        // "repeated" of the reference.
        ungrammar::Rule::Rep(_) | ungrammar::Rule::Opt(_) => {
            let mapped = map_single(name.as_str(), rule, grammar, model);
            Node::Items(SequenceNode {
                name,
                items: vec![mapped],
            })
        }
        ungrammar::Rule::Seq(rules) => {
            if let [element @ Rule::Node(_) | element @ Rule::Token(_), Rule::Rep(subrule)] =
                &rules[..]
            {
                if let Rule::Seq(rules) = subrule.as_ref() {
                    match &rules[..] {
                        [sep @ Rule::Token(_), element2] if element2 == element => {
                            let element_field =
                                map_single(name.as_str(), element, grammar, model).make_repeated();
                            let separator_field =
                                map_single(name.as_str(), sep, grammar, model).make_repeated();
                            return Node::List(ListNode {
                                kind: name,
                                element: element_field,
                                separator: separator_field,
                            });
                        }
                        _ => {}
                    }
                }
            }
            // Every item is left at ordinal 0; `Model::fixup_nth_by_resolved_kind` numbers them
            // once the whole model is known, since two spellings can be one kind in the tree.
            let mapped = rules
                .iter()
                .map(|rule| map_single(name.as_str(), rule, grammar, model))
                .collect();
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

#[cfg(test)]
mod tests {
    use super::*;

    fn model_of(grammar: &str) -> Model {
        map_grammar(&ungrammar::Grammar::from_str(grammar).expect("test grammar does not parse"))
    }

    /// `Foo = Bar` introduces a name, not a node.
    #[test]
    fn a_production_that_is_a_single_reference_becomes_an_alias() {
        let model = model_of(
            "
            DesignFile = Condition OthersChoice
            Condition = Expression
            OthersChoice = 'others'
            Expression = '#identifier' ';'
            ",
        );
        assert_eq!(
            model.node(&"Condition".into()),
            Some(&Node::Alias(AliasNode::node("Condition", "Expression")))
        );
        assert_eq!(
            model.node(&"OthersChoice".into()),
            Some(&Node::Alias(AliasNode::token(
                "OthersChoice",
                str_to_token_kind("others").unwrap()
            )))
        );
        // Neither is a node in the tree; the getters reach the aliased kinds directly.
        assert!(!model
            .collect_all_materialized_node_kinds()
            .contains("Condition"));
        assert!(!model
            .collect_all_materialized_node_kinds()
            .contains("OthersChoice"));
    }

    /// An alias and the node it renames are one kind in the tree, so the ordinals run across
    /// both: `expression()` reaches the first child, `condition()` the second.
    #[test]
    fn an_alias_and_its_target_are_numbered_as_one_kind() {
        let model = model_of(
            "
            DesignFile = ';' else_when_expression:('else' Expression 'when' Condition)
            Condition = Expression
            Expression = '#identifier' ';'
            ",
        );
        let Some(Node::Items(seq)) = model.node(&"ElseWhenExpression".into()) else {
            panic!("ElseWhenExpression should be a sequence node")
        };
        let ordinals: Vec<(String, Cardinality)> = seq
            .items
            .iter()
            .map(|item| (item.getter_name(), item.cardinality))
            .collect();
        assert_eq!(
            ordinals,
            [
                ("else_token".to_owned(), Cardinality::Required { nth: 0 }),
                ("expression".to_owned(), Cardinality::Required { nth: 0 }),
                ("when_token".to_owned(), Cardinality::Required { nth: 0 }),
                ("condition".to_owned(), Cardinality::Required { nth: 1 }),
            ]
        );
    }

    /// `Foo = Bar?` and `Foo = Bar*` still need a node: it is what carries the cardinality.
    #[test]
    fn a_production_that_is_an_optional_or_repeated_reference_stays_a_node() {
        let model = model_of(
            "
            DesignFile = Maybe Many
            Maybe = ';'?
            Many = ','*
            ",
        );
        for kind in ["Maybe", "Many"] {
            assert!(
                matches!(model.node(&kind.into()), Some(Node::Items(_))),
                "{kind} should still be a sequence node"
            );
        }
    }

    /// A label on a single reference names that reference, which is exactly what an alias is.
    #[test]
    fn a_labelled_single_reference_becomes_an_alias() {
        let model = model_of(
            "
            DesignFile = ';' condition:Expression
            Expression = '#identifier' ';'
            ",
        );
        assert_eq!(
            model.node(&"Condition".into()),
            Some(&Node::Alias(AliasNode::node("Condition", "Expression")))
        );
        let Some(Node::Items(design_file)) = model.node(&"DesignFile".into()) else {
            panic!("DesignFile should be a sequence node")
        };
        assert_eq!(design_file.items[1].getter_name(), "condition");
        assert_eq!(
            design_file.items[1].kind,
            NodeOrTokenKind::Node("Condition".into()),
            "the field references the alias; the alias is what resolves to Expression"
        );
    }
}
