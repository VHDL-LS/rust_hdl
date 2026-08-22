// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2026, Lukas Scheller lukasscheller@icloud.com

//! Structural validation of a syntax tree against the generated
//! [`Layout`](crate::syntax::meta::Layout) meta.
//!
//! [`SyntaxNode::validate`] walks the tree and reports, per node, any child that no layout
//! item accepts ([`Extraneous`](error::Validation::Extraneous)) and any required
//! item that no child satisfied ([`Missing`](error::Validation::Missing)).

use crate::syntax::node::SyntaxNode;
use crate::syntax::validate::validator::check_node;
use crate::syntax::visitor::{Preorder, WalkEvent};

pub mod error;
pub mod validator;

pub use error::{Missing, ValidationError};

impl SyntaxNode {
    /// Validate a node and its entire subtree for missing and extraneous elements.
    ///
    /// Returns `Ok(())` for a structurally well-formed tree, or a
    /// [`ValidationError`] aggregating every divergence found in the subtree. Each
    /// finding is attributed to the node in which it occurs, so a child's problems
    /// are reported against the child, not the root.
    pub fn validate(&self) -> Result<(), ValidationError> {
        let mut err = ValidationError::new();
        for event in Preorder::new(self.clone()) {
            if let WalkEvent::Enter(node) = event {
                check_node(&node, &mut err);
            }
        }
        err.into_result()
    }
}

#[cfg(test)]
mod tests {
    use crate::fmt::write::FormatToExt;
    use crate::parser::builder::NodeBuilder;
    use crate::parser::{parse, parse_syntax, Parser};
    use crate::syntax::meta::LayoutItemKind;
    use crate::syntax::node::{SyntaxElement, SyntaxNode};
    use crate::syntax::node_kind::NodeKind;
    use crate::syntax::validate::error::Validation;
    use crate::syntax::validate::validator::check_node;
    use crate::syntax::AstNode;
    use crate::syntax::{InterfaceDeclarationSyntax, InterfaceListBuilder};
    use crate::tokens::{Keyword, Token, TokenKind, Trivia};

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
        assert!(&node.validate().is_ok());
    }

    #[test]
    fn optional_absent_passes() {
        // EntityDeclarationEpilogue: end (req) · entity (opt) · identifier (opt) · ; (req)
        // Both optional items omitted — must still pass.
        let (node, diagnostics) = parse_syntax("end;", Parser::entity_declaration_epilogue);
        assert!(diagnostics.is_empty());
        assert!(&node.validate().is_ok());
    }

    #[test]
    fn optional_present_passes() {
        let (node, diagnostics) =
            parse_syntax("end entity foo;", Parser::entity_declaration_epilogue);
        assert!(diagnostics.is_empty());
        assert!(&node.validate().is_ok());
    }

    #[test]
    fn repeated_with_multiple_occurrences_passes() {
        let (node, diagnostics) =
            parse_syntax("entity a is end; entity b is end;", Parser::design_file);
        assert!(diagnostics.is_empty());
        assert!(&node.validate().is_ok());
    }

    #[test]
    fn separated_list_with_multiple_elements_passes() {
        // A port list `(clk : in bit; rst : in bit)` is an `InterfaceList`, which has a
        // `Layout::List` and is therefore matched by `match_list`, not `match_children`.
        let (node, diagnostics) = parse(
            r#"
entity foo is
    port (
        clk : in bit;
        rst : in bit
    );
end entity foo;
            "#,
        );
        assert!(diagnostics.is_empty());
        assert!(&node.raw().validate().is_ok());
    }

    #[test]
    fn adjacent_repeated_items_in_grammar_order_pass() {
        let (node, diagnostics) = parse(
            r#"
configuration cfg of ent is
    for rtl
        use work.pkg.all;
        use work.other.all;
        for inst : comp
            use entity work.e;
        end for;
        for other_inst : comp
            use entity work.f;
        end for;
    end for;
end configuration cfg;
            "#,
        );
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
        assert!(node.raw().validate().is_ok());
    }

    // --- separated-list tests ---

    /// Build an `InterfaceList` from a compact spec: `e` is an element, `;` a separator,
    /// `x` a token that neither slot accepts. The elements are deliberately not valid
    /// `InterfaceObjectDeclaration`s — these tests drive [`check_node`] directly so only
    /// the list's own alternation is under test.
    fn interface_list(spec: &str) -> SyntaxNode {
        let mut b = NodeBuilder::new();
        b.start_node(NodeKind::InterfaceList);
        for (i, ch) in spec.chars().enumerate() {
            match ch {
                'e' => {
                    let mut inner = NodeBuilder::new();
                    inner.start_node(NodeKind::InterfaceObjectDeclaration);
                    inner.push(tok(TokenKind::Identifier, format!("x{i}").as_bytes()));
                    inner.end_node();
                    b.push_node(inner.end());
                }
                ';' => b.push(tok(TokenKind::SemiColon, b";")),
                'x' => b.push(tok(TokenKind::Comma, b",")),
                other => panic!("bad spec char {other}"),
            }
        }
        b.end_node();
        SyntaxNode::new_root(b.end())
    }

    fn list_findings(spec: &str) -> Vec<Validation> {
        let mut err = crate::syntax::validate::error::ValidationError::new();
        check_node(&interface_list(spec), &mut err);
        err.items().to_vec()
    }

    #[test]
    fn well_formed_list_passes() {
        assert!(list_findings("e").is_empty());
        assert!(list_findings("e;e").is_empty());
        assert!(list_findings("e;e;e").is_empty());
    }

    /// The whole point of the `List` layout: two adjacent repeated items could not tell
    /// `a; b` from `a b`, because both are "some elements and some separators".
    #[test]
    fn adjacent_elements_report_a_missing_separator() {
        let findings = list_findings("ee");
        assert_eq!(findings.len(), 1);
        match &findings[0] {
            Validation::Missing(missing) => assert!(matches!(
                missing.kind(),
                LayoutItemKind::Token(TokenKind::SemiColon)
            )),
            other => panic!("expected a missing separator, got {other:?}"),
        }
    }

    #[test]
    fn adjacent_separators_report_a_missing_element() {
        let findings = list_findings("e;;e");
        assert_eq!(findings.len(), 1);
        match &findings[0] {
            Validation::Missing(missing) => {
                assert!(matches!(missing.kind(), LayoutItemKind::NodeChoice(_)))
            }
            other => panic!("expected a missing element, got {other:?}"),
        }
    }

    #[test]
    fn trailing_separator_reports_a_missing_element() {
        let findings = list_findings("e;e;");
        assert_eq!(findings.len(), 1);
        match &findings[0] {
            Validation::Missing(missing) => {
                assert!(matches!(missing.kind(), LayoutItemKind::NodeChoice(_)));
                // The anchor is the dangling separator.
                match missing.previous().expect("anchored on the separator") {
                    SyntaxElement::Token(t) => assert_eq!(t.kind(), TokenKind::SemiColon),
                    other => panic!("expected the separator token, got {other:?}"),
                }
            }
            other => panic!("expected a missing element, got {other:?}"),
        }
    }

    /// An empty list node never reaches the tree: `NodeBuilder::end_node` drops any node
    /// that gained no children, so an empty-capable list shows up as *absent* from its
    /// parent rather than as an empty node.
    #[test]
    fn empty_list_node_is_dropped_rather_than_built() {
        let mut b = NodeBuilder::new();
        b.start_node(NodeKind::InterfaceList);
        b.end_node();
        assert!(
            std::panic::catch_unwind(move || b.end()).is_err(),
            "an empty list node should not have been produced"
        );
    }

    #[test]
    fn foreign_child_in_a_list_is_extraneous() {
        let findings = list_findings("e;ex");
        assert_eq!(findings.len(), 1);
        assert!(matches!(
            &findings[0],
            Validation::Extraneous(SyntaxElement::Token(_))
        ));
    }

    /// The generated list builder interleaves separators itself, so a caller cannot
    /// produce the `a b ;` ordering the old two-repeated-fields builder emitted.
    #[test]
    fn generated_list_builder_interleaves_separators() {
        let (declaration, diagnostics) =
            parse_syntax("clk : in bit", Parser::interface_declaration);
        assert!(diagnostics.is_empty());
        let element = InterfaceDeclarationSyntax::cast(declaration).expect("an interface decl");

        let list = InterfaceListBuilder::new(element.clone())
            .push(element)
            .build();

        assert_eq!(
            list.raw().display().to_string(),
            "clk : in bit;clk : in bit"
        );
        assert!(list.raw().validate().is_ok());
    }

    // --- missing-element tests ---

    #[test]
    fn missing_required_tokens_are_reported() {
        // EntityDeclarationPreamble with only 'entity'; 'name' and 'is' are absent.
        let node = build_preamble([tok(TokenKind::Keyword(Keyword::Entity), b"entity")]);
        let err = node.validate().unwrap_err();

        assert_eq!(err.len(), 2);

        match &err.items()[0] {
            Validation::Missing(missing) => {
                assert!(matches!(
                    missing.kind(),
                    LayoutItemKind::Token(TokenKind::Identifier)
                ));
                assert!(missing.previous().is_some());
                assert_eq!(missing.parent().kind(), NodeKind::EntityDeclarationPreamble);
            }
            _ => panic!("Expected missing"),
        };

        match &err.items()[1] {
            Validation::Missing(missing) => {
                assert!(matches!(
                    missing.kind(),
                    LayoutItemKind::Token(TokenKind::Keyword(Keyword::Is))
                ));
                assert_eq!(missing.parent().kind(), NodeKind::EntityDeclarationPreamble);
            }
            _ => panic!("Expected missing"),
        };
    }

    #[test]
    fn missing_required_after_optional_is_reported() {
        // EntityDeclarationEpilogue with only 'end'; the mandatory ';' is absent.
        let mut b = NodeBuilder::new();
        b.start_node(NodeKind::EntityDeclarationEpilogue);
        b.push(tok(TokenKind::Keyword(Keyword::End), b"end"));
        b.end_node();
        let node = SyntaxNode::new_root(b.end());

        let err = node.validate().unwrap_err();

        assert_eq!(err.len(), 1);
        match &err.items()[0] {
            Validation::Missing(missing) => {
                assert!(matches!(
                    missing.kind(),
                    LayoutItemKind::Token(TokenKind::SemiColon)
                ));
                // 'end' was consumed before the missing ';'.
                assert!(missing.previous().is_some());
            }
            _ => panic!("Expected missing"),
        }
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
        let err = node.validate().unwrap_err();

        assert_eq!(err.len(), 1);
        match &err.items()[0] {
            Validation::Extraneous(extraneous) => {
                assert!(matches!(extraneous, SyntaxElement::Token(_)));
            }
            _ => panic!("Expected extraneous"),
        }
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

        let err = entity.validate().unwrap_err();

        // The two missing items from the preamble must bubble up through the walk.
        assert_eq!(err.len(), 2);
        match &err.items()[0] {
            Validation::Missing(missing) => {
                // The errors are attributed to the child (preamble), not the entity root.
                assert_eq!(missing.parent().kind(), NodeKind::EntityDeclarationPreamble);
            }
            _ => panic!("Expected missing"),
        }
        match &err.items()[1] {
            Validation::Missing(missing) => {
                assert_eq!(missing.parent().kind(), NodeKind::EntityDeclarationPreamble);
            }
            _ => panic!("Expected missing"),
        }
    }

    #[test]
    fn missing_fields_are_correct() {
        // Build a preamble that contains only the identifier, so:
        //   - 'entity' (required, first) is missing  → previous must be None
        //   - 'is'     (required, last)  is missing  → previous must be the identifier token
        let node = build_preamble([tok(TokenKind::Identifier, b"foo")]);
        let err = node.validate().unwrap_err();

        assert_eq!(err.len(), 2);

        // First gap: 'entity' keyword, nothing consumed before it.
        match &err.items()[0] {
            Validation::Missing(first) => {
                assert!(matches!(
                    first.kind(),
                    LayoutItemKind::Token(TokenKind::Keyword(Keyword::Entity))
                ));
                assert_eq!(first.parent().kind(), NodeKind::EntityDeclarationPreamble);
                assert!(first.previous().is_none());
            }
            _ => panic!("Expected missing"),
        }

        // Second gap: 'is' keyword, the identifier was the last consumed token.
        match &err.items()[1] {
            Validation::Missing(second) => {
                assert!(matches!(
                    second.kind(),
                    LayoutItemKind::Token(TokenKind::Keyword(Keyword::Is))
                ));
                assert_eq!(second.parent().kind(), NodeKind::EntityDeclarationPreamble);
                match second
                    .previous()
                    .expect("previous should be the identifier token")
                {
                    SyntaxElement::Token(t) => assert_eq!(t.kind(), TokenKind::Identifier),
                    other => panic!("expected the identifier token, got {other:?}"),
                }
            }
            _ => panic!("Expected missing"),
        }
    }
}
