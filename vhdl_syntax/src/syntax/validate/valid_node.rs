use std::ops::Deref;

use crate::syntax::{validate::ValidationError, AstNode};

/// An AstNode that is guaranteed to be valid
#[derive(Debug, Clone)]
pub struct Valid<T>(T);

impl<T> Valid<T> {
    pub fn inner(&self) -> &T {
        &self.0
    }

    pub fn into_inner(self) -> T {
        self.0
    }
}

impl<T> Valid<T>
where
    T: AstNode,
{
    /// Creates a new valid node, failing if the node isn't valid.
    pub fn new(node: T) -> Result<Self, ValidationError> {
        node.validate().map(|_| Valid(node))
    }

    /// Casts a plain AstNode node to a valid node without checking.
    ///
    /// The accessors of this function will panic, if the node wasn't actually valid.
    pub fn new_unchecked(node: T) -> Self {
        debug_assert!(node.validate().is_ok(), "new_unchecked got an invalid node");
        Valid(node)
    }
}

impl<T> Deref for Valid<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.inner()
    }
}
