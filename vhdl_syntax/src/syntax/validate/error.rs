use crate::syntax::{
    meta::LayoutItemKind,
    node::{SyntaxElement, SyntaxNode},
};
use std::fmt;

/// Errors that may appear as part of the validation process.
/// Validation errors always have at least one element (i.e., at least one
/// missing or at least one extraneous)
#[derive(Clone, Debug, Default)]
pub struct ValidationError {
    /// Any missing nodes. Those were expected to be in the tree, but weren't found.
    missing: Vec<Missing>,
    /// Any extraneous nodes. Those are in the tree, but shouldn't be.
    extraneous: Vec<SyntaxElement>,
}

impl ValidationError {
    pub(crate) fn new() -> ValidationError {
        ValidationError::default()
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.missing.is_empty() && self.extraneous.is_empty()
    }

    pub(crate) fn append(&mut self, other: &mut ValidationError) {
        self.extraneous.append(&mut other.extraneous);
        self.missing.append(&mut other.missing);
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

    /// Returns elements that are expected to be in the tree, but were missing
    pub fn missing(&self) -> &[Missing] {
        &self.missing
    }

    /// Returns elements that are in the tree, but weren't expected
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

/// Describes a node that was missing
#[derive(Clone, Debug)]
pub struct Missing {
    /// The node before this one
    previous: Option<SyntaxElement>,
    /// The parent node (where the node was missing)
    parent: SyntaxNode,
    /// What was missing
    kind: LayoutItemKind,
}

impl Missing {
    pub(crate) fn new(
        previous: Option<SyntaxElement>,
        parent: SyntaxNode,
        kind: LayoutItemKind,
    ) -> Missing {
        Missing {
            previous,
            parent,
            kind,
        }
    }

    pub fn previous(&self) -> Option<&SyntaxElement> {
        self.previous.as_ref()
    }

    pub fn parent(&self) -> &SyntaxNode {
        &self.parent
    }

    pub fn kind(&self) -> &LayoutItemKind {
        &self.kind
    }
}
