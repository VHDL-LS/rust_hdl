//! Serialization implementations for syntax tree types.

use serde::ser::{self, Serialize, SerializeSeq, SerializeStruct};

use crate::{
    serde::{flags::CommentEncoding, serializable::Serializable},
    syntax::{
        child::Child,
        green::{GreenChild, GreenNode, GreenToken},
        node::SyntaxNode,
    },
    tokens::{Token, Trivia, TriviaPiece, trivia_piece::Comment},
};

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
        token.serialize_field("text", &self.inner.text().to_string())?;

        if self.flags.includes_trivia() {
            token.serialize_field("leading_trivia", &self.new_with_same_flags(self.inner.leading_trivia()))?;
            token.serialize_field("trailing_trivia", &self.new_with_same_flags(self.inner.trailing_trivia()))?;
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

impl<'a> Serialize for Serializable<'a, TriviaPiece> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self.inner {
            TriviaPiece::HorizontalTabs(n) => {
                serializer.serialize_newtype_variant("TriviaPiece", 0, "HorizontalTabs", n)
            }
            TriviaPiece::VerticalTabs(n) => {
                serializer.serialize_newtype_variant("TriviaPiece", 1, "VerticalTabs", n)
            }
            TriviaPiece::CarriageReturns(n) => {
                serializer.serialize_newtype_variant("TriviaPiece", 2, "CarriageReturns", n)
            }
            TriviaPiece::CarriageReturnLineFeeds(n) => serializer.serialize_newtype_variant(
                "TriviaPiece",
                3,
                "CarriageReturnLineFeeds",
                n,
            ),
            TriviaPiece::LineFeeds(n) => serializer.serialize_newtype_variant(
                "TriviaPiece",
                4,
                "LineFeeds",
                n,
            ),
            TriviaPiece::FormFeeds(n) => serializer.serialize_newtype_variant(
                "TriviaPiece",
                5,
                "FormFeeds",
                n,
            ),
            TriviaPiece::LineComment(comment) => serializer.serialize_newtype_variant(
                "TriviaPiece",
                6,
                "LineComment",
                &self.new_with_same_flags(comment),
            ),
            TriviaPiece::BlockComment(comment) => serializer.serialize_newtype_variant(
                "TriviaPiece",
                7,
                "BlockComment",
                &self.new_with_same_flags(comment),
            ),
            TriviaPiece::Spaces(n) => serializer.serialize_newtype_variant(
                "TriviaPiece",
                8,
                "Spaces",
                n,
            ),
            TriviaPiece::NonBreakingSpaces(n) => serializer.serialize_newtype_variant(
                "TriviaPiece",
                9,
                "NonBreakingSpaces",
                n,
            ),
            TriviaPiece::Unexpected(items) => serializer.serialize_newtype_variant(
                "TriviaPiece",
                10,
                "Unexpected",
                items,
            ),
        }
    }
}

impl<'a> Serialize for Serializable<'a, Trivia> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(self.inner.len()))?;
        for el in self.inner.iter() {
            seq.serialize_element(&self.new_with_same_flags(el))?;
        }
        seq.end()
    }
}

impl<'a> Serialize for Serializable<'a, Comment> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self.flags.comment_encoding() {
            CommentEncoding::Utf8 => serializer.serialize_str(
                self.inner
                    .as_utf8()
                    .map_err(|e| ser::Error::custom(format!("Invalid UTF-8 in comments: {}", e)))?,
            ),
            // TODO: This is still UTF-8 (with the special characters being mapped). There may be a better solution...
            CommentEncoding::Latin1 => {
                serializer.serialize_str(&self.inner.as_latin1().to_string())
            }
            CommentEncoding::None => serializer.serialize_bytes(self.inner.as_bytes()),
        }
    }
}
