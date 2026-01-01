//! Serialization implementations for syntax tree types.

use serde::{Serialize, ser::SerializeStruct};

use crate::{serde::serializable::Serializable, syntax::{child::Child, green::{GreenChild, GreenNode, GreenToken}, node::SyntaxNode}, tokens::Token};


impl<'a> Serialize for Serializable<'a, GreenChild> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self.inner {
            Child::Node((offset, node)) => {
                if self.flags.includes_loc() {
                    serializer.serialize_newtype_variant(
                        "Child",
                        0,
                        "Node",
                        &(offset, self.new_with_same_flags(node)),
                    )
                } else {
                    serializer.serialize_newtype_variant(
                        "Child",
                        0,
                        "Node",
                        &self.new_with_same_flags(node),
                    )
                }
            }
            Child::Token((offset, token)) => {
                if self.flags.includes_loc() {
                    serializer.serialize_newtype_variant(
                        "Child",
                        1,
                        "Token",
                        &(offset, self.new_with_same_flags(token)),
                    )
                } else {
                    serializer.serialize_newtype_variant(
                        "Child",
                        1,
                        "Token",
                        &self.new_with_same_flags(token),
                    )
                }
            }
        }
    }
}

impl<'a> Serialize for Serializable<'a, GreenNode> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut node = serializer.serialize_struct("Node", 2)?;
        node.serialize_field("kind", &self.inner.kind())?;
        node.serialize_field(
            "children",
            &self
                .inner
                .children()
                .map(|child| self.new_with_same_flags(child))
                .collect::<Vec<_>>(),
        )?;
        node.end()
    }
}

impl<'a> Serialize for Serializable<'a, Token> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let field_count = 2 + if self.flags.includes_trivia() { 2 } else { 0 };

        let mut token = serializer.serialize_struct("Token", field_count)?;
        token.serialize_field("kind", &self.inner.kind())?;
        token.serialize_field("text", &self.inner.text())?;

        if self.flags.includes_trivia() {
            token.serialize_field("leading_trivia", &self.inner.leading_trivia())?;
            token.serialize_field("trailing_trivia", &self.inner.trailing_trivia())?;
        }

        token.end()
    }
}

impl<'a> Serialize for Serializable<'a, GreenToken> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.new_with_same_flags(self.inner.token())
            .serialize(serializer)
    }
}

impl<'a> Serialize for Serializable<'a, SyntaxNode> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.new_with_same_flags(self.inner.green())
            .serialize(serializer)
    }
}
