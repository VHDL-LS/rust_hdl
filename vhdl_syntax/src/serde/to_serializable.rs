//! Provides traits for converting nodes to serializable representations.

use crate::{
    serde::{flags::SerdeFlags, serializable::Serializable},
    syntax::node::SyntaxNode,
};

/// Trait for types that can be converted to a serializable view.
pub trait ToSerializable
where
    Self: Sized,
{
    /// Get a serializable view of this node using custom [SerdeFlags]
    fn serialize_with<'a>(&'a self, serde_flags: SerdeFlags) -> Serializable<'a, Self>;

    /// Get a serializable view of this node using the default [SerdeFlags]
    fn serialize<'a>(&'a self) -> Serializable<'a, Self> {
        self.serialize_with(SerdeFlags::default())
    }
}

impl ToSerializable for SyntaxNode {
    fn serialize_with<'a>(&'a self, serde_flags: SerdeFlags) -> Serializable<'a, Self> {
        Serializable::new(self, serde_flags)
    }
}
