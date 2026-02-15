//! Support for serializing and de-serializing syntax elements.

pub mod flags;
pub mod serialize_impl;
pub use flags::SerdeFlags;
pub mod serializable;
pub use serializable::Serializable;
pub mod to_serializable;
pub use to_serializable::ToSerializable;
