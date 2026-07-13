// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2026, Lukas Scheller lukasscheller@icloud.com

//! Error types produced by [structural validation](super::validator).

use crate::syntax::meta::{LayoutItem, LayoutItemKind};
use crate::syntax::node::{SyntaxElement, SyntaxNode};
use std::fmt;

#[derive(Clone, Debug)]
pub enum Validation {
    Missing(Missing),
    Extraneous(SyntaxElement),
}

fn format_expected(expected: &LayoutItem, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match expected.kind {
        LayoutItemKind::Token(token_kind) => write!(f, "{token_kind:?}"),
        LayoutItemKind::Node(node_kind) => write!(f, "{node_kind:?}"),
        LayoutItemKind::NodeChoice(node_kinds) => write!(f, "one of {node_kinds:?}"),
        LayoutItemKind::TokenChoice(token_kinds) => write!(f, "one of {token_kinds:?}"),
    }
}

impl fmt::Display for Validation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Validation::Missing(missing) => {
                write!(f, "{}: missing ", missing.offset(),)?;
                format_expected(&missing.expected, f)
            }
            Validation::Extraneous(child) => {
                write!(
                    f,
                    "{}-{}: extraneous element",
                    child.offset(),
                    child.offset() + child.byte_len()
                )
            }
        }
    }
}

/// The ways in which a tree diverges from its declared [`Layout`](crate::syntax::meta::Layout).
///
/// A non-empty `ValidationError` holds at least one [`Validation`] finding.
/// Each finding is either a `Missing` item (something the layout expected but
/// the tree did not provide — wants an insertion) or an `Extraneous` element
/// (a child no layout item accepts — wants a removal).
#[derive(Clone, Debug, Default)]
pub struct ValidationError(Vec<Validation>);

impl ValidationError {
    pub(crate) fn new() -> ValidationError {
        ValidationError::default()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub(crate) fn push_missing(&mut self, missing: Missing) {
        self.0.push(Validation::Missing(missing));
    }

    pub(crate) fn push_extraneous(&mut self, extraneous: SyntaxElement) {
        self.0.push(Validation::Extraneous(extraneous));
    }

    pub(crate) fn into_result(self) -> Result<(), Self> {
        if self.is_empty() {
            Ok(())
        } else {
            Err(self)
        }
    }

    pub fn items(&self) -> &[Validation] {
        &self.0
    }
}

impl fmt::Display for ValidationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} validation error(s)", self.0.len())
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

    pub fn offset(&self) -> usize {
        self.previous
            .as_ref()
            .map_or(0, |el| el.offset() + el.byte_len())
    }
}
