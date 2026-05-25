//! Alphabetically sorts and deduplicates `use` clauses inside a design unit's context clause using
//! the [`Rewriter`](vhdl_syntax::syntax::rewrite) API.
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2026, Lukas Scheller lukasscheller@icloud.com
use std::collections::HashMap;
use vhdl_syntax::fmt::write::FormatToExt;
use vhdl_syntax::parser;
use vhdl_syntax::syntax::node::{SyntaxElement, SyntaxNode};
use vhdl_syntax::syntax::rewrite::RewriteAction;
use vhdl_syntax::syntax::AstNode;
use vhdl_syntax::syntax::{ContextItemSyntax, UseClauseContextItemSyntax};

fn main() {
    // Out-of-order imports with one duplicate (`std_logic_1164` appears twice).
    let vhdl = "\
library ieee;
use ieee.math_real.all;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;

entity foo is
end foo;
";
    let (file, diagnostics) = parser::parse(vhdl);
    assert!(diagnostics.is_empty());

    // 1) Collect every `use`-clause context item from each design unit, in document order.
    let originals: Vec<UseClauseContextItemSyntax> = file
        .design_units()
        .filter_map(|du| du.context_clause())
        .flat_map(|cc| cc.context_items().collect::<Vec<_>>())
        .filter_map(|ci| match ci {
            ContextItemSyntax::UseClauseContextItem(u) => Some(u),
            _ => None,
        })
        .collect();

    // 2) Build the sorted, deduplicated target list.
    let mut sorted: Vec<UseClauseContextItemSyntax> = originals.clone();
    sorted.sort_by_key(sort_key);
    sorted.dedup_by_key(|u| sort_key(u));

    // 3) Map each original slot (by its offset in the source) to either the next
    //    sorted replacement or `None` (= remove this slot, it was a duplicate).
    let mut plan: HashMap<usize, Option<SyntaxNode>> = HashMap::new();
    let mut replacements = sorted.into_iter().map(|u| u.raw());
    for orig in &originals {
        plan.insert(orig.raw().offset(), replacements.next());
    }

    // 4) Single rewrite pass: every visited UseClauseContextItem is either swapped to
    //    its sorted replacement (`Change`) or dropped entirely (`Remove`).
    let new_file = file.raw().rewrite(|el| match el {
        SyntaxElement::Node(n) => match plan.get(&n.offset()) {
            Some(Some(replacement)) => {
                RewriteAction::Change(SyntaxElement::Node(replacement.clone()))
            }
            Some(None) => RewriteAction::Remove,
            None => RewriteAction::Leave,
        },
        SyntaxElement::Token(_) => RewriteAction::Leave,
    });

    assert_eq!(
        format!("{}", new_file.display()),
        "\
library ieee;
use ieee.math_real.all;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;

entity foo is
end foo;
"
    );
}

/// Alphabetical sort key — just the displayed text without surrounding whitespace.
/// Good enough for this example; a real tool would compare the parsed name segments.
fn sort_key(item: &UseClauseContextItemSyntax) -> String {
    item.raw().display().to_string().trim().to_lowercase()
}
