// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2025, Lukas Scheller lukasscheller@icloud.com

//! Compares the unmodified LRM grammar (`vhdl-08.ungram`) with the grammar that
//! `vhdl_syntax` actually models (`vhdl-08-modified.ungram`).
//!
//! Labels are not part of the comparison: `generic_part:(GenericClause ...)?` and
//! `(GenericClause ...)?` are the same rule. Everything else -- in particular the
//! nesting of groups -- is significant, so a group that the modified grammar adds
//! for scoping (a `*_preamble`, a `Parenthesized*` wrapper) does show up as a
//! difference.

use anyhow::{Context, Result};
use similar::{ChangeTag, TextDiff};
use std::collections::BTreeMap;
use std::fmt::Write as _;
use std::path::Path;
use std::str::FromStr;

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
pub fn diff_grammar(lrm_file: &Path, modified_file: &Path, filter: Option<&str>) -> Result<()> {
    let lrm = load(lrm_file)?;
    let modified = load(modified_file)?;

    let mut differing = Vec::new();
    let mut identical = 0usize;

    for (name, lrm_rule) in &lrm {
        if filter.is_some_and(|filter| filter != name) {
            continue;
        }
        let Some(modified_rule) = modified.get(name) else {
            continue;
        };
        if lrm_rule == modified_rule {
            identical += 1;
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

    println!(
        "{} differing, {identical} identical, {} only in {}, {} only in {}",
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
