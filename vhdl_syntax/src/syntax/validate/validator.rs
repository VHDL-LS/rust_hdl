// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2026, Lukas Scheller lukasscheller@icloud.com

//! Per-node matching of children against a node's declared [`Layout`].

use crate::syntax::child::ChildKind;
use crate::syntax::layout_of;
use crate::syntax::meta::{Layout, LayoutItem, LayoutItemKind, List};
use crate::syntax::node::{SyntaxElement, SyntaxNode};
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
        let is_element = accepts(list.element, child.kind());
        let is_separator = accepts(list.separator, child.kind());

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

fn match_children(node: &SyntaxNode, items: &[LayoutItem], err: &mut ValidationError) {
    let mut children = node.children_with_tokens();
    let mut items = items.iter();
    // The last child that matched an item
    let mut previous: Option<SyntaxElement> = None;
    // Whether the current (repeated) item has matched at least once
    let mut matched_current = false;

    while let Some(item) = items.as_slice().first() {
        let advance = match children.kind_at(0) {
            Some(kind) if accepts(item, kind) => {
                previous = children.next();
                matched_current = true;
                // A repeated item stays in place so it can match again.
                !item.repeated
            }
            // Optional, or a repeated run that already matched once: move on.
            _ if item.optional || (item.repeated && matched_current) => true,
            // A stray child: the item wants the child after it
            Some(_) if children.kind_at(1).is_some_and(|k| accepts(item, k)) => {
                err.push_extraneous(children.next().unwrap());
                false
            }
            // The child belongs to a later item, so this one is missing
            Some(kind) if items.as_slice().iter().any(|later| accepts(later, kind)) => {
                err.push_missing(Missing::new(previous.clone(), node.clone(), *item));
                true
            }
            // No item accepts the child: report it and retry the same item.
            Some(_) => {
                err.push_extraneous(children.next().unwrap());
                false
            }
            None => {
                err.push_missing(Missing::new(previous.clone(), node.clone(), *item));
                true
            }
        };
        if advance {
            items.next();
            matched_current = false;
        }
    }

    children.for_each(|child| err.push_extraneous(child));
}

/// Whether `item` accepts `child`.
fn accepts(item: &LayoutItem, child: ChildKind) -> bool {
    match (child, item.kind) {
        (ChildKind::Token(kind), LayoutItemKind::Token(k)) => kind == k,
        (ChildKind::Token(kind), LayoutItemKind::TokenChoice(ks)) => ks.contains(&kind),
        (ChildKind::Node(kind), LayoutItemKind::Node(k)) => kind == k,
        (ChildKind::Node(kind), LayoutItemKind::NodeChoice(ks)) => ks.contains(&kind),
        _ => false,
    }
}
