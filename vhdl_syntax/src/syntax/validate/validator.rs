// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2026, Lukas Scheller lukasscheller@icloud.com

//! Per-node matching of children against a node's declared [`Layout`].

use crate::syntax::layout_of;
use crate::syntax::meta::{Layout, LayoutItem, LayoutItemKind, List};
use crate::syntax::node::{SyntaxElement, SyntaxNode};
use crate::syntax::node_kind::NodeKind;
use crate::syntax::validate::error::{Missing, ValidationError};

/// Check a single node against its declared layout, recording any divergence in
/// `err`. Does not recurse; [`SyntaxNode::validate`] walks the whole tree.
pub(crate) fn check_node(node: &SyntaxNode, err: &mut ValidationError) {
    match layout_of(node.kind()) {
        Layout::Sequence(seq) => {
            debug_assert_eq!(node.kind(), seq.kind);
            match_children(node, seq.items, err);
        }
        Layout::List(list) => match_list(node, list, err),
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

/// Match the children of a separated list against `element (separator element)*`.
fn match_list(node: &SyntaxNode, list: &List, err: &mut ValidationError) {
    // Which slot the next child has to fill for the alternation to hold.
    let mut expect_element = true;
    let mut previous: Option<SyntaxElement> = None;
    let mut saw_any = false;

    for child in node.children_with_tokens() {
        let is_element = accepts(list.element, &child);
        let is_separator = accepts(list.separator, &child);

        if !is_element && !is_separator {
            err.push_extraneous(child);
            continue;
        }

        // A child that could fill either slot is read as the one the alternation wants.
        let as_element = if expect_element {
            is_element
        } else {
            !is_separator
        };

        if as_element != expect_element {
            // The slot the alternation wanted was skipped over.
            let expected = if expect_element {
                list.element
            } else {
                list.separator
            };
            err.push_missing(Missing::new(previous.clone(), node.clone(), *expected));
        }

        // An element is followed by a separator and vice versa.
        expect_element = !as_element;
        previous = Some(child);
        saw_any = true;
    }

    if !saw_any {
        err.push_missing(Missing::new(None, node.clone(), *list.element));
    } else if expect_element {
        // The last child was a separator, so an element is still owed.
        err.push_missing(Missing::new(previous, node.clone(), *list.element));
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
                // A repeated item holds the cursor in place so it can match again;
                // anything else advances past it. Two adjacent repeated items are
                // therefore still matched in grammar order — `A* B*` accepts
                // `a a b b` but not `b a`, which is what the layout says.
                pos = if items[k].repeated { k } else { k + 1 };
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
        // Both are concrete kinds, so the direct comparison above was the only chance.
        Layout::Sequence(_) | Layout::List(_) => false,
    }
}
