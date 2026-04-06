use crate::syntax::{validate::{self, ValidationError}, AstNode};

pub mod generated;
pub use generated::*;

/// A layer on top of an [`AstNode`] that guarantees there are no missing or
/// extraneous elements.
pub trait CheckedNode: Sized {
    type Syntax: AstNode + Clone;

    /// Wrap `syntax` without re-validating its children.  Caller must ensure
    /// the node was already validated (e.g. came from a successful `cast` call
    /// on a checked parent or was manually validated).
    fn cast_unchecked(syntax: Self::Syntax) -> Self;

    /// Return the underlying AST Node.
    fn raw(&self) -> Self::Syntax;

    /// Validate `syntax` and, on success, wrap it in the checked type.
    fn cast(syntax: Self::Syntax) -> Result<Self, ValidationError> {
        let node = syntax.raw();
        validate::check(&node)?;
        Ok(Self::cast_unchecked(syntax))
    }
}
