//! Validate raw syntax nodes.
// Improvements:
// - Add configurable tree recursion depth (currently, entire tree must is searched).
//   This could be needed by analysis (don't want to analyze the entire tree), but bit-by-bit
// - "Error tolerant" / "smart": Currently, `architecture arch 1 of foo is ...` would return 4 incorrect elements
//   ("1 of foo is"). Instead, it should be one extraneous element only.

use crate::syntax::{node::SyntaxNode, validate::validator::Validator};

pub mod error;
mod validator;
pub use error::{Missing, ValidationError};

/// Check a node for missing and extraneous elements.
pub fn check(node: &SyntaxNode) -> Result<(), ValidationError> {
    Validator::new(node).validate().into_result()
}

#[cfg(test)]
mod tests {
    use crate::{
        parser::{builder::NodeBuilder, parse_syntax, Parser},
        syntax::{
            meta::LayoutItemKind,
            node::{SyntaxElement, SyntaxNode},
            node_kind::NodeKind,
            validate::check,
        },
        tokens::{Keyword, Token, TokenKind, Trivia},
    };

    fn tok(kind: TokenKind, text: &[u8]) -> Token {
        Token::new(kind, text, Trivia::default())
    }

    /// Build an `EntityDeclarationPreamble` node from the given raw tokens.
    /// Layout: entity (req) · name-identifier (req) · is (req)
    fn build_preamble(tokens: impl IntoIterator<Item = Token>) -> SyntaxNode {
        let mut b = NodeBuilder::new();
        b.start_node(NodeKind::EntityDeclarationPreamble);
        for t in tokens {
            b.push(t);
        }
        b.end_node();
        SyntaxNode::new_root(b.end())
    }

    // --- happy-path tests ---

    #[test]
    fn valid_preamble_passes() {
        let (node, diagnostics) =
            parse_syntax("entity foo is", Parser::entity_declaration_preamble);
        assert!(diagnostics.is_empty());
        assert!(check(&node).is_ok());
    }

    #[test]
    fn optional_absent_passes() {
        // EntityDeclarationEpilogue: end (req) · entity (opt) · identifier (opt) · ; (req)
        // Both optional items omitted — must still pass.
        let (node, diagnostics) = parse_syntax("end;", Parser::entity_declaration_epilogue);
        assert!(diagnostics.is_empty());
        assert!(check(&node).is_ok());
    }

    #[test]
    fn optional_present_passes() {
        let (node, diagnostics) =
            parse_syntax("end entity foo;", Parser::entity_declaration_epilogue);
        assert!(diagnostics.is_empty());
        assert!(check(&node).is_ok());
    }

    #[test]
    fn repeated_with_zero_occurrences_passes() {
        // DesignFile: DesignUnit* (repeated, zero here) · EOF (req)
        let (node, _) = parse_syntax("", Parser::design_file);
        assert!(check(&node).is_ok());
    }

    #[test]
    fn repeated_with_multiple_occurrences_passes() {
        let (node, diagnostics) =
            parse_syntax("entity a is end; entity b is end;", Parser::design_file);
        assert!(diagnostics.is_empty());
        assert!(check(&node).is_ok());
    }

    // --- missing-element tests ---

    #[test]
    fn missing_required_tokens_are_reported() {
        // EntityDeclarationPreamble with only 'entity'; 'name' and 'is' are absent.
        let node = build_preamble([tok(TokenKind::Keyword(Keyword::Entity), b"entity")]);
        let err = check(&node).unwrap_err();

        assert_eq!(err.missing().len(), 2);
        assert_eq!(err.extraneous().len(), 0);

        // First missing: the entity-name identifier.
        assert!(matches!(
            err.missing()[0].kind(),
            LayoutItemKind::Token(TokenKind::Identifier)
        ));
        // The entity keyword was the last consumed element before the gap.
        assert!(err.missing()[0].previous().is_some());
        assert_eq!(
            err.missing()[0].parent().kind(),
            NodeKind::EntityDeclarationPreamble
        );

        // Second missing: the 'is' keyword.
        assert!(matches!(
            err.missing()[1].kind(),
            LayoutItemKind::Token(TokenKind::Keyword(Keyword::Is))
        ));
        assert_eq!(
            err.missing()[1].parent().kind(),
            NodeKind::EntityDeclarationPreamble
        );
    }

    #[test]
    fn missing_required_after_optional_is_reported() {
        // EntityDeclarationEpilogue with only 'end'; the mandatory ';' is absent.
        let mut b = NodeBuilder::new();
        b.start_node(NodeKind::EntityDeclarationEpilogue);
        b.push(tok(TokenKind::Keyword(Keyword::End), b"end"));
        b.end_node();
        let node = SyntaxNode::new_root(b.end());

        let err = check(&node).unwrap_err();

        assert_eq!(err.missing().len(), 1);
        assert!(matches!(
            err.missing()[0].kind(),
            LayoutItemKind::Token(TokenKind::SemiColon)
        ));
        // 'end' was consumed before the missing ';'.
        assert!(err.missing()[0].previous().is_some());
    }

    // --- extraneous-element test ---

    #[test]
    fn extraneous_token_is_reported() {
        // All required tokens present, plus one surplus identifier at the end.
        let node = build_preamble([
            tok(TokenKind::Keyword(Keyword::Entity), b"entity"),
            tok(TokenKind::Identifier, b"foo"),
            tok(TokenKind::Keyword(Keyword::Is), b"is"),
            tok(TokenKind::Identifier, b"extra"),
        ]);
        let err = check(&node).unwrap_err();

        assert_eq!(err.missing().len(), 0);
        assert_eq!(err.extraneous().len(), 1);
        assert!(matches!(err.extraneous()[0], SyntaxElement::Token(_)));
    }

    // --- recursive propagation test ---

    #[test]
    fn child_errors_propagate_to_parent() {
        // EntityDeclarationPreamble with only 'entity' — 'name' and 'is' are missing.
        let bad_preamble = build_preamble([tok(TokenKind::Keyword(Keyword::Entity), b"entity")]);

        // Minimal valid epilogue: "end ;"
        let mut b = NodeBuilder::new();
        b.start_node(NodeKind::EntityDeclarationEpilogue);
        b.push(tok(TokenKind::Keyword(Keyword::End), b"end"));
        b.push(tok(TokenKind::SemiColon, b";"));
        b.end_node();
        let epilogue = SyntaxNode::new_root(b.end());

        // Assemble an EntityDeclaration from the two children above.
        // All intermediate optional children (EntityHeader, Declarations, …) are absent.
        let mut b = NodeBuilder::new();
        b.start_node(NodeKind::EntityDeclaration);
        b.push_node(bad_preamble.green().clone());
        b.push_node(epilogue.green().clone());
        b.end_node();
        let entity = SyntaxNode::new_root(b.end());

        let err = check(&entity).unwrap_err();

        // The two missing items from the preamble must bubble up through the parent check.
        assert_eq!(err.missing().len(), 2);
        assert_eq!(err.extraneous().len(), 0);
        // The errors are attributed to the child (preamble), not the entity root.
        assert_eq!(
            err.missing()[0].parent().kind(),
            NodeKind::EntityDeclarationPreamble
        );
    }

    #[test]
    fn missing_fields_are_correct() {
        // Build a preamble that contains only the identifier, so:
        //   - 'entity' (required, first) is missing  → previous must be None
        //   - 'is'     (required, last)  is missing  → previous must be the identifier token
        let node = build_preamble([tok(TokenKind::Identifier, b"foo")]);
        let err = check(&node).unwrap_err();

        assert_eq!(err.missing().len(), 2);

        // First gap: 'entity' keyword, nothing consumed before it.
        let first = &err.missing()[0];
        assert!(matches!(
            first.kind(),
            LayoutItemKind::Token(TokenKind::Keyword(Keyword::Entity))
        ));
        assert_eq!(first.parent().kind(), NodeKind::EntityDeclarationPreamble);
        assert!(first.previous().is_none());

        // Second gap: 'is' keyword, the identifier was the last consumed token.
        let second = &err.missing()[1];
        assert!(matches!(
            second.kind(),
            LayoutItemKind::Token(TokenKind::Keyword(Keyword::Is))
        ));
        assert_eq!(second.parent().kind(), NodeKind::EntityDeclarationPreamble);
        let prev = second
            .previous()
            .and_then(|el| el.as_token())
            .expect("previous should be the identifier token");
        assert_eq!(prev.kind(), TokenKind::Identifier);
    }
}
