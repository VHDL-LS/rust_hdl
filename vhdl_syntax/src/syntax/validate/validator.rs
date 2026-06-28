// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2026, Lukas Scheller lukasscheller@icloud.com

//! Per-node matching of children against a node's declared [`Layout`].

use crate::syntax::layout_of;
use crate::syntax::meta::{Layout, LayoutItem, LayoutItemKind};
use crate::syntax::node::{SyntaxElement, SyntaxNode};
use crate::syntax::node_kind::NodeKind;
use crate::syntax::validate::error::{Missing, ValidationError};

/// Check a single node against its declared layout, recording any divergence in
/// `err`. Does not recurse; [`super::check`] walks the whole tree.
pub(crate) fn check_node(node: &SyntaxNode, err: &mut ValidationError) {
    match layout_of(node.kind()) {
        Layout::Sequence(seq) => {
            debug_assert_eq!(node.kind(), seq.kind);
            match_children(node, seq.items, err);
        }
        // A `Choice` kind is an abstract alternative: the parser is expected to
        // materialize one of the concrete options directly, never a node tagged
        // with the choice kind itself. This is a parser/builder invariant, not a
        // property of (possibly malformed) input, so it is a programming error
        // rather than a validation finding. It cannot arise from parsing any
        // input, so the release path simply skips the node.
        Layout::Choice(_) => debug_assert!(
            false,
            "{:?} has a choice layout but was materialized as a node",
            node.kind()
        ),
    }
}

/// Match the node's children against `items` with a monotonically advancing
/// cursor: each child must be accepted by some item at or after the cursor.
/// `repeated` items keep the cursor in place so they can match again; a
/// required item the cursor never matched is reported as [`Missing`].
fn match_children(node: &SyntaxNode, items: &[LayoutItem], err: &mut ValidationError) {
    let mut pos = 0usize;
    // The last child that landed in each slot (last wins within a repeated run).
    // Used both to tell whether a required item matched and to reconstruct the
    // insertion anchor (`previous`) for the items that did not.
    let mut filled: Vec<Option<SyntaxElement>> = vec![None; items.len()];

    for child in node.children_with_tokens() {
        match (pos..items.len()).find(|&j| accepts(&items[j], &child)) {
            Some(k) => {
                // A separated list `X (sep X)*` is encoded as a run of adjacent
                // `repeated` items (`[X*, sep*]`) whose children interleave. So
                // matching a repeated item rewinds the cursor to the start of its
                // run, keeping every member of the run matchable; a non-repeated
                // item advances past it.
                pos = if items[k].repeated {
                    repeated_run_start(items, k)
                } else {
                    k + 1
                };
                filled[k] = Some(child);
            }
            None => err.push_extraneous(child),
        }
    }

    for (i, item) in items.iter().enumerate() {
        if !item.optional && !item.repeated && filled[i].is_none() {
            // The anchor is the nearest preceding slot that actually matched a
            // child; `None` if the gap is at the start of the parent.
            let previous = (0..i).rev().find_map(|j| filled[j].clone());
            err.push_missing(Missing::new(previous, node.clone(), *item));
        }
    }
}

/// The first index of the maximal run of consecutive `repeated` items that
/// contains `k`.
fn repeated_run_start(items: &[LayoutItem], k: usize) -> usize {
    let mut start = k;
    while start > 0 && items[start - 1].repeated {
        start -= 1;
    }
    start
}

/// Whether `item` accepts `child`.
fn accepts(item: &LayoutItem, child: &SyntaxElement) -> bool {
    match (child, item.kind) {
        (SyntaxElement::Token(t), LayoutItemKind::Token(k)) => t.kind() == k,
        (SyntaxElement::Token(t), LayoutItemKind::TokenChoice(ks)) => ks.contains(&t.kind()),
        (SyntaxElement::Node(n), LayoutItemKind::Node(k)) => node_satisfies(n.kind(), k),
        (SyntaxElement::Node(n), LayoutItemKind::NodeChoice(ks)) => {
            ks.iter().any(|&k| node_satisfies(n.kind(), k))
        }
        _ => false,
    }
}

/// Whether a node of kind `actual` satisfies an item expecting `expected`:
/// a direct kind match, or `expected` is an abstract choice whose (recursively
/// expanded) options include `actual`.
fn node_satisfies(actual: NodeKind, expected: NodeKind) -> bool {
    if actual == expected {
        return true;
    }
    match layout_of(expected) {
        Layout::Choice(choice) => choice
            .options
            .iter()
            .any(|&option| node_satisfies(actual, option)),
        Layout::Sequence(_) => false,
    }
}
