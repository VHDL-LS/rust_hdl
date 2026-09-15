// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2026, Lukas Scheller lukasscheller@icloud.com

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

#[cfg(test)]
mod tests {
    use crate::parser::{parse, parse_syntax, Parser};
    use crate::syntax::builder::RawNodeBuilder;
    use crate::syntax::validate::valid_node::Valid;
    use crate::syntax::{AstNode, EntityDeclarationPreambleSyntax, NodeKind};
    use crate::tokens::{Keyword, Token, TokenKind, Trivia};

    fn node<T: AstNode>(input: &str, production: fn(&mut Parser)) -> T {
        let (node, diagnostics) = parse_syntax(input, production);
        assert!(diagnostics.is_empty());
        T::cast(node).expect("the production builds this node kind")
    }

    #[test]
    fn new_accepts_a_well_formed_node() {
        let preamble: EntityDeclarationPreambleSyntax =
            node("entity foo is", Parser::entity_declaration_preamble);
        assert!(Valid::new(preamble).is_ok());
    }

    #[test]
    fn new_rejects_a_node_with_missing_children() {
        // Layout: entity (req) · identifier (req) · is (req) — the last two are absent.
        let preamble = RawNodeBuilder::<EntityDeclarationPreambleSyntax>::new()
            .push_token(Token::new(
                TokenKind::Keyword(Keyword::Entity),
                b"entity",
                Trivia::default(),
            ))
            .finish();

        Valid::new(preamble).expect_err("two required tokens are missing");
    }

    #[test]
    fn new_rejects_a_node_whose_child_is_invalid() {
        let file = parse(
            r#"
entity foo is
end entity foo
            "#,
        )
        .0;
        assert!(Valid::new(file).is_err());
    }

    #[test]
    fn deref_reaches_the_underlying_syntax_node() {
        let preamble: Valid<EntityDeclarationPreambleSyntax> =
            Valid::new(node("entity foo is", Parser::entity_declaration_preamble)).unwrap();

        assert_eq!(preamble.kind(), NodeKind::EntityDeclarationPreamble);
        assert_eq!(
            preamble.into_inner().raw().kind(),
            NodeKind::EntityDeclarationPreamble
        );
    }

    #[test]
    #[cfg(debug_assertions)]
    #[should_panic(expected = "new_unchecked got an invalid node")]
    fn new_unchecked_on_an_invalid_node_panics_in_debug() {
        let preamble = RawNodeBuilder::<EntityDeclarationPreambleSyntax>::new()
            .push_token(Token::new(
                TokenKind::Keyword(Keyword::Entity),
                b"entity",
                Trivia::default(),
            ))
            .finish();

        Valid::new_unchecked(preamble);
    }
}
