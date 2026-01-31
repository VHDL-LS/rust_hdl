use std::rc::Rc;

use crate::serde::flags::SerdeFlags;

/// Utility helper to serialize Syntax elements (i.e., nodes, tokens, ...).
/// Usually constructed via [ToSerializable](crate::serde::ToSerializable)
pub struct Serializable<'a, T> {
    /// The actual data to serialize
    pub(crate) inner: &'a T,
    /// Flags relevant when serializing
    pub(crate) flags: Rc<SerdeFlags>,
}

impl<'a, T> Serializable<'a, T> {
    pub fn new(inner: &'a T, flags: impl Into<Rc<SerdeFlags>>) -> Serializable<'a, T> {
        Serializable { inner, flags: flags.into() }
    }

    pub fn new_default(inner: &'a T) -> Serializable<'a, T> {
        Self::new(inner, SerdeFlags::default())
    }

    pub fn new_with_same_flags<'b, U>(&self, new_inner: &'b U) -> Serializable<'b, U> {
        Serializable::new(new_inner, self.flags.clone())
    }
}
