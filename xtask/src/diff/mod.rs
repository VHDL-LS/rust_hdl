// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2025, Lukas Scheller lukasscheller@icloud.com

//! Compares the unmodified LRM grammar (`vhdl-08.ungram`) with the grammar that
//! `vhdl_syntax` actually models (`vhdl-08-modified.ungram`).
//!
//! Two things the modified grammar carries are not deviations from the standard, and
//! the comparison erases both so that what remains is a real difference in the
//! accepted language:
//!
//! * **Labels.** `generic_part:(GenericClause ...)?` and `(GenericClause ...)?` are
//!   the same rule.
//! * **Unmarked groups** (in the default [`Nesting::Flattened`] mode). A group that
//!   carries no `?` or `*` denotes exactly the concatenation of its elements, so
//!   `A (B C) D` and `A B C D` accept the same token sequences. The modified grammar
//!   has such groups only to give the codegen a node to hang error recovery,
//!   preambles/epilogues and `Parenthesized*` wrappers off.
//!
//! The erasure stops there, and deliberately. A group that *is* marked is never
//! flattened -- `(A B)?` is not `A? B?` -- and neither is an alternation, whose
//! nesting changes the language outright. Aliases (`Condition = Expression`),
//! alternative ordering and optional markers are likewise left alone: each of those
//! would hide a real divergence. Pass [`Nesting::Exact`] to compare the group nesting
//! as well.

use anyhow::{Context, Result};
use similar::{ChangeTag, TextDiff};
use std::collections::BTreeMap;
use std::fmt::Write as _;
use std::path::Path;
use std::str::FromStr;

/// Whether the comparison takes the nesting of unmarked groups into account.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Nesting {
    /// Compare the rules as written, so a group the modified grammar adds for scoping
    /// is itself a difference.
    Exact,
    /// Splice unmarked groups into their parent sequence before comparing.
    Flattened,
}

/// A grammar rule with labels erased.
#[derive(PartialEq, Eq, Clone)]
enum Rule {
    Node(String),
    Token(String),
    Seq(Vec<Rule>),
    Alt(Vec<Rule>),
    Opt(Box<Rule>),
    Rep(Box<Rule>),
}

impl Rule {
    fn from_ungrammar(rule: &ungrammar::Rule, grammar: &ungrammar::Grammar) -> Rule {
        let convert_all = |rules: &[ungrammar::Rule]| {
            rules
                .iter()
                .map(|r| Rule::from_ungrammar(r, grammar))
                .collect()
        };
        match rule {
            // The whole point of the diff: a label is not part of the grammar.
            ungrammar::Rule::Labeled { rule, .. } => Rule::from_ungrammar(rule, grammar),
            ungrammar::Rule::Node(node) => Rule::Node(grammar[*node].name.clone()),
            ungrammar::Rule::Token(token) => Rule::Token(grammar[*token].name.clone()),
            ungrammar::Rule::Seq(rules) => Rule::Seq(convert_all(rules)),
            ungrammar::Rule::Alt(rules) => Rule::Alt(convert_all(rules)),
            ungrammar::Rule::Opt(rule) => Rule::Opt(Box::new(Rule::from_ungrammar(rule, grammar))),
            ungrammar::Rule::Rep(rule) => Rule::Rep(Box::new(Rule::from_ungrammar(rule, grammar))),
        }
    }

    /// Splices every unmarked group into its parent sequence.
    ///
    /// Only a sequence nested directly in another sequence is spliced, because only
    /// there is the group unmarked and concatenation associative. The body of a `?`,
    /// a `*` or an alternative keeps its own parentheses -- dropping those would
    /// change the language -- but is descended into, so an unmarked group nested
    /// inside one is still flattened.
    fn flattened(&self) -> Rule {
        match self {
            Rule::Node(_) | Rule::Token(_) => self.clone(),
            Rule::Seq(rules) => {
                let mut flat = Vec::with_capacity(rules.len());
                for rule in rules {
                    match rule.flattened() {
                        Rule::Seq(inner) => flat.extend(inner),
                        other => flat.push(other),
                    }
                }
                Rule::Seq(flat)
            }
            Rule::Alt(rules) => Rule::Alt(rules.iter().map(Rule::flattened).collect()),
            Rule::Opt(rule) => Rule::Opt(Box::new(rule.flattened())),
            Rule::Rep(rule) => Rule::Rep(Box::new(rule.flattened())),
        }
    }

    /// The rule as the given mode compares it.
    fn as_compared(&self, nesting: Nesting) -> Rule {
        match nesting {
            Nesting::Exact => self.clone(),
            Nesting::Flattened => self.flattened(),
        }
    }

    /// Renders the rule on a single line, parenthesizing where the nesting would
    /// otherwise be lost.
    fn write_inline(&self, out: &mut String) {
        match self {
            Rule::Node(name) => out.push_str(name),
            Rule::Token(name) => {
                let _ = write!(out, "'{name}'");
            }
            Rule::Seq(rules) => {
                for (i, rule) in rules.iter().enumerate() {
                    if i > 0 {
                        out.push(' ');
                    }
                    rule.write_seq_element(out);
                }
            }
            Rule::Alt(rules) => {
                for (i, rule) in rules.iter().enumerate() {
                    if i > 0 {
                        out.push_str(" | ");
                    }
                    rule.write_inline(out);
                }
            }
            Rule::Opt(rule) => {
                rule.write_atom(out);
                out.push('?');
            }
            Rule::Rep(rule) => {
                rule.write_atom(out);
                out.push('*');
            }
        }
    }

    /// An element of a sequence: a nested sequence or alternation keeps its parentheses.
    fn write_seq_element(&self, out: &mut String) {
        match self {
            Rule::Seq(_) | Rule::Alt(_) => self.write_parenthesized(out),
            _ => self.write_inline(out),
        }
    }

    /// The operand of a `?` or `*`, which must be parenthesized unless it is a single item.
    fn write_atom(&self, out: &mut String) {
        match self {
            Rule::Node(_) | Rule::Token(_) => self.write_inline(out),
            _ => self.write_parenthesized(out),
        }
    }

    fn write_parenthesized(&self, out: &mut String) {
        out.push('(');
        self.write_inline(out);
        out.push(')');
    }

    /// Renders the rule as the body of a production, one element per line so that a
    /// line diff points at the element that changed.
    fn to_body_lines(&self) -> String {
        let mut out = String::new();
        match self {
            Rule::Seq(rules) => {
                for rule in rules {
                    out.push_str("  ");
                    rule.write_seq_element(&mut out);
                    out.push('\n');
                }
            }
            Rule::Alt(rules) => {
                for (i, rule) in rules.iter().enumerate() {
                    out.push_str(if i == 0 { "  " } else { "| " });
                    rule.write_inline(&mut out);
                    out.push('\n');
                }
            }
            _ => {
                out.push_str("  ");
                self.write_inline(&mut out);
                out.push('\n');
            }
        }
        out
    }
}

/// All productions of one grammar file, keyed by name.
fn load(file: &Path) -> Result<BTreeMap<String, Rule>> {
    let text =
        std::fs::read_to_string(file).with_context(|| format!("cannot read {}", file.display()))?;
    let grammar = ungrammar::Grammar::from_str(&text)
        .map_err(|err| anyhow::anyhow!("{}:{err}", file.display()))?;

    Ok(grammar
        .iter()
        .map(|node| {
            let data = &grammar[node];
            (
                data.name.clone(),
                Rule::from_ungrammar(&data.rule, &grammar),
            )
        })
        .collect())
}

/// Prints the diff of the two grammars.
pub fn diff_grammar(
    lrm_file: &Path,
    modified_file: &Path,
    filter: Option<&str>,
    nesting: Nesting,
) -> Result<()> {
    let lrm = load(lrm_file)?;
    let modified = load(modified_file)?;

    let mut differing = Vec::new();
    let mut identical = 0usize;
    // Identical only because the grouping was flattened away -- reported separately, so
    // that a relaxation of the comparison never passes for an exact match.
    let mut identical_when_flattened = 0usize;

    for (name, lrm_rule) in &lrm {
        if filter.is_some_and(|filter| filter != name) {
            continue;
        }
        let Some(modified_rule) = modified.get(name) else {
            continue;
        };
        let (lrm_rule, modified_rule) = (
            lrm_rule.as_compared(nesting),
            modified_rule.as_compared(nesting),
        );
        if lrm_rule == modified_rule {
            identical += 1;
            if lrm.get(name) != modified.get(name) {
                identical_when_flattened += 1;
            }
        } else {
            differing.push((name, lrm_rule, modified_rule));
        }
    }

    for (name, lrm_rule, modified_rule) in &differing {
        println!("~~ {name} ~~");
        let lrm_body = lrm_rule.to_body_lines();
        let modified_body = modified_rule.to_body_lines();
        for change in TextDiff::from_lines(&lrm_body, &modified_body).iter_all_changes() {
            let sign = match change.tag() {
                ChangeTag::Delete => '-',
                ChangeTag::Insert => '+',
                ChangeTag::Equal => ' ',
            };
            print!("{sign}{change}");
        }
        println!();
    }

    if let Some(filter) = filter {
        match (lrm.contains_key(filter), modified.contains_key(filter)) {
            (false, false) => println!("no production named `{filter}` in either grammar"),
            (true, false) => println!("`{filter}` exists only in {}", file_name(lrm_file)),
            (false, true) => println!("`{filter}` exists only in {}", file_name(modified_file)),
            (true, true) if identical_when_flattened > 0 => {
                println!("`{filter}` is identical once unmarked groups are flattened")
            }
            (true, true) if identical > 0 => println!("`{filter}` is identical"),
            (true, true) => {}
        }
        return Ok(());
    }

    let lrm_only: Vec<_> = lrm
        .keys()
        .filter(|name| !modified.contains_key(*name))
        .collect();
    let modified_only: Vec<_> = modified
        .keys()
        .filter(|name| !lrm.contains_key(*name))
        .collect();

    print_only_in(lrm_file, &lrm_only);
    print_only_in(modified_file, &modified_only);

    let flattened_note = if identical_when_flattened > 0 {
        format!(" ({identical_when_flattened} only after flattening unmarked groups)")
    } else {
        String::new()
    };
    println!(
        "{} differing, {identical} identical{flattened_note}, {} only in {}, {} only in {}",
        differing.len(),
        lrm_only.len(),
        file_name(lrm_file),
        modified_only.len(),
        file_name(modified_file),
    );

    Ok(())
}

fn print_only_in(file: &Path, names: &[&String]) {
    if names.is_empty() {
        return;
    }
    println!("only in {} ({}):", file_name(file), names.len());
    for name in names {
        println!("  {name}");
    }
    println!();
}

fn file_name(file: &Path) -> String {
    file.file_name()
        .unwrap_or_default()
        .to_string_lossy()
        .into_owned()
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The body of `Production` in `grammar`, rendered back on one line.
    fn rule_of(grammar: &str, production: &str) -> Rule {
        let grammar = ungrammar::Grammar::from_str(grammar).expect("test grammar does not parse");
        let node = grammar
            .iter()
            .find(|node| grammar[*node].name == production)
            .unwrap_or_else(|| panic!("no production named {production}"));
        Rule::from_ungrammar(&grammar[node].rule, &grammar)
    }

    fn flattened(grammar: &str, production: &str) -> String {
        let mut out = String::new();
        rule_of(grammar, production)
            .flattened()
            .write_inline(&mut out);
        out
    }

    #[test]
    fn an_unmarked_group_is_spliced_into_its_parent() {
        let grammar = "
            X = 'a' preamble:('b' 'c') 'd'
        ";
        assert_eq!(flattened(grammar, "X"), "'a' 'b' 'c' 'd'");
    }

    #[test]
    fn an_optional_group_keeps_its_parentheses() {
        let grammar = "
            X = 'a' epilogue:('b' 'c')? 'd'
        ";
        assert_eq!(flattened(grammar, "X"), "'a' ('b' 'c')? 'd'");
    }

    #[test]
    fn a_repeated_group_keeps_its_parentheses() {
        let grammar = "
            X = 'a' elsif:('b' 'c')*
        ";
        assert_eq!(flattened(grammar, "X"), "'a' ('b' 'c')*");
    }

    #[test]
    fn an_alternation_is_not_spliced() {
        let grammar = "
            X = lhs:('a' 'b') | rhs:('c' 'd')
        ";
        assert_eq!(flattened(grammar, "X"), "'a' 'b' | 'c' 'd'");
    }

    #[test]
    fn an_unmarked_group_inside_a_marked_one_is_still_spliced() {
        let grammar = "
            X = elsif:('a' inner:('b' 'c') 'd')*
        ";
        assert_eq!(flattened(grammar, "X"), "('a' 'b' 'c' 'd')*");
    }

    #[test]
    fn a_parenthesized_list_group_flattens_to_the_lrm_spelling() {
        let grammar = "
            Aggregate = '(' items:(Element (',' Element)*) ')'
            Element = '#identifier'
        ";
        assert_eq!(
            flattened(grammar, "Aggregate"),
            "'(' Element (',' Element)* ')'"
        );
    }

    #[test]
    fn a_flat_rule_is_unchanged() {
        let grammar = "
            X = 'a' Y? 'b'* ('c' | 'd')
            Y = '#identifier'
        ";
        let mut written = String::new();
        rule_of(grammar, "X").write_inline(&mut written);
        assert_eq!(flattened(grammar, "X"), written);
        assert!(rule_of(grammar, "X").flattened().flattened() == rule_of(grammar, "X").flattened());
    }
}
