// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2026, Lukas Scheller lukasscheller@icloud.com

//! Error types produced by [structural validation](super::check).

use crate::syntax::meta::{LayoutItem, LayoutItemKind};
use crate::syntax::node::{SyntaxElement, SyntaxNode};
use std::fmt;

/// The ways in which a tree diverges from its declared [`Layout`](crate::syntax::meta::Layout).
///
/// A non-empty `ValidationError` always has at least one missing or extraneous
/// element. The two categories are kept apart because they call for different
/// fixes: a `missing` item wants something inserted, an `extraneous` element
/// wants something removed.
#[derive(Clone, Debug, Default)]
pub struct ValidationError {
    /// Required items that were expected by the layout but never matched a child.
    missing: Vec<Missing>,
    /// Children present in the tree that no layout item accepts.
    extraneous: Vec<SyntaxElement>,
}

impl ValidationError {
    pub(crate) fn new() -> ValidationError {
        ValidationError::default()
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.missing.is_empty() && self.extraneous.is_empty()
    }

    pub(crate) fn push_missing(&mut self, missing: Missing) {
        self.missing.push(missing);
    }

    pub(crate) fn push_extraneous(&mut self, extraneous: SyntaxElement) {
        self.extraneous.push(extraneous);
    }

    pub(crate) fn into_result(self) -> Result<(), Self> {
        if self.is_empty() {
            Ok(())
        } else {
            Err(self)
        }
    }

    /// Items expected by the layout but missing from the tree.
    pub fn missing(&self) -> &[Missing] {
        &self.missing
    }

    /// Children present in the tree that no layout item accepts.
    pub fn extraneous(&self) -> &[SyntaxElement] {
        &self.extraneous
    }
}

impl fmt::Display for ValidationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match (self.missing.len(), self.extraneous.len()) {
            (m, 0) => write!(f, "{m} missing element(s)"),
            (0, e) => write!(f, "{e} extraneous element(s)"),
            (m, e) => write!(f, "{m} missing and {e} extraneous element(s)"),
        }
    }
}

impl std::error::Error for ValidationError {}

/// A required item that the layout expected but the tree did not provide.
#[derive(Clone, Debug)]
pub struct Missing {
    /// The element immediately before the gap — the natural insertion anchor.
    /// `None` when the gap is at the start of the parent.
    previous: Option<SyntaxElement>,
    /// The node in which the item was expected.
    parent: SyntaxNode,
    /// The layout item that was missing (carries both its `name` and `kind`).
    expected: LayoutItem,
}

impl Missing {
    pub(crate) fn new(
        previous: Option<SyntaxElement>,
        parent: SyntaxNode,
        expected: LayoutItem,
    ) -> Missing {
        Missing {
            previous,
            parent,
            expected,
        }
    }

    /// The element immediately before the gap, if any.
    pub fn previous(&self) -> Option<&SyntaxElement> {
        self.previous.as_ref()
    }

    /// The node in which the item was expected.
    pub fn parent(&self) -> &SyntaxNode {
        &self.parent
    }

    /// What kind of element was missing.
    pub fn kind(&self) -> LayoutItemKind {
        self.expected.kind
    }

    /// The layout item's declared name (a human-readable field label).
    pub fn name(&self) -> &'static str {
        self.expected.name
    }
}
