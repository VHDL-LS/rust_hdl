//! Facilities to rewrite a [SyntaxNode]
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::syntax::green::{GreenChild, GreenNode, GreenNodeData};
use crate::syntax::node::{SyntaxElement, SyntaxNode, SyntaxToken};

pub enum RewriteAction {
    /// Leave the node as-is
    Leave,
    /// Change the node to a different one
    Change(SyntaxElement),
    /// Remove the node
    Remove,
}

/// Facility to rewrite a `SyntaxNode` by re-building it while changing individual nodes.
///
/// Note that when calling `Rewrite` on a node, the node itself cannot change, only it's children
/// may.
pub struct Rewriter<R: Fn(&SyntaxElement) -> RewriteAction> {
    rewrite_action: R,
}

impl<R: Fn(&SyntaxElement) -> RewriteAction> Rewriter<R> {
    pub fn new(rewrite_action: R) -> Self {
        Rewriter { rewrite_action }
    }

    pub fn rewrite(&self, syntax_node: SyntaxNode) -> SyntaxNode {
        SyntaxNode::new_root(self.rewrite_node_to_green(syntax_node))
    }

    fn rewrite_node_to_green(&self, syntax_node: SyntaxNode) -> GreenNode {
        let mut new_green_node = GreenNodeData::new(syntax_node.kind());
        for child in syntax_node.children_with_tokens() {
            match (self.rewrite_action)(&child) {
                RewriteAction::Leave => match child {
                    SyntaxElement::Node(node) => {
                        new_green_node
                            .push(GreenChild::Node(self.rewrite_node_to_green(node)));
                    }
                    SyntaxElement::Token(token) => {
                        new_green_node.push(GreenChild::Token(token.green().clone()));
                    }
                },
                RewriteAction::Change(node) => {
                    new_green_node.push(node.green());
                }
                RewriteAction::Remove => {}
            }
        }
        GreenNode::new(new_green_node)
    }
}

pub struct TokenRewriter<R: TokenRewrite> {
    rewrite: R,
}

pub enum TokenRewriteAction {
    Keep,
    Replace(SyntaxToken),
}

pub trait TokenRewrite {
    /// Called when traversal enters a node.
    fn enter(&mut self, _node: &SyntaxNode) {}

    /// Called for each token in traversal order.
    fn token(&mut self, token: &SyntaxToken) -> TokenRewriteAction;

    /// Called when traversal exits a node.
    fn exit(&mut self, _node: &SyntaxNode) {}
}

impl<R: TokenRewrite> TokenRewriter<R> {
    pub fn new(rewrite: R) -> Self {
        TokenRewriter { rewrite }
    }

    pub fn rewrite(&mut self, syntax_node: SyntaxNode) -> SyntaxNode {
        SyntaxNode::new_root(self.rewrite_node_to_green(syntax_node))
    }

    fn rewrite_node_to_green(&mut self, syntax_node: SyntaxNode) -> GreenNode {
        self.rewrite.enter(&syntax_node);

        let mut new_green_node = GreenNodeData::new(syntax_node.kind());
        for child in syntax_node.children_with_tokens() {
            match child {
                SyntaxElement::Node(node) => {
                    new_green_node.push(
                        GreenChild::Node(self.rewrite_node_to_green(node)),
                    );
                }
                SyntaxElement::Token(tok) => match self.rewrite.token(&tok) {
                    TokenRewriteAction::Keep => {}
                    TokenRewriteAction::Replace(syntax_token) => {
                        new_green_node.push(
                            GreenChild::Token(syntax_token.green().clone()),
                        );
                    }
                },
            }
        }
        self.rewrite.exit(&syntax_node);
        GreenNode::new(new_green_node)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser;
    use crate::syntax::child::Child;
    use crate::syntax::node_kind::NodeKind;
    use crate::syntax::AstNode;
    use pretty_assertions::assert_eq;

    fn parse_root(src: &str) -> SyntaxNode {
        let (file, diagnostics) = parser::parse(src);
        assert!(diagnostics.is_empty(), "got diagnostics: {:?}", diagnostics);
        file.raw()
    }

    const MULTI_ENTITY: &str = "\
entity myent is
end entity;

entity myent2 is
end entity myent2;

entity myent3 is
end myent3;
";

    #[test]
    fn leave_round_trips() {
        let root = parse_root(MULTI_ENTITY);
        let new_root = root.rewrite(|_| RewriteAction::Leave);
        assert_eq!(format!("{}", new_root), MULTI_ENTITY);
    }

    #[test]
    fn change_single_token() {
        let root = parse_root(MULTI_ENTITY);
        let new_root = root.rewrite(|el| match el {
            SyntaxElement::Token(tok)
                if tok.kind() == crate::tokens::TokenKind::Identifier
                    && tok.text() == "myent2" =>
            {
                RewriteAction::Change(SyntaxElement::Token(tok.clone_with_text(b"myentX")))
            }
            _ => RewriteAction::Leave,
        });
        assert_eq!(
            format!("{}", new_root),
            MULTI_ENTITY.replace("myent2", "myentX")
        );
    }

    fn remove_design_unit_at(index: usize, src: &str) -> SyntaxNode {
        let root = parse_root(src);
        let mut seen = 0usize;
        let target = std::cell::Cell::new(None::<usize>);
        for child in root.children_with_tokens() {
            if let Child::Node(n) = child {
                if n.kind() == NodeKind::DesignUnit {
                    if seen == index {
                        target.set(Some(n.offset()));
                        break;
                    }
                    seen += 1;
                }
            }
        }
        let target_offset = target.get().expect("design unit index out of range");
        root.rewrite(|el| match el {
            SyntaxElement::Node(n)
                if n.kind() == NodeKind::DesignUnit && n.offset() == target_offset =>
            {
                RewriteAction::Remove
            }
            _ => RewriteAction::Leave,
        })
    }

    #[test]
    fn remove_first_design_unit() {
        let new_root = remove_design_unit_at(0, MULTI_ENTITY);
        assert_eq!(
            format!("{}", new_root).trim(),
            "\
entity myent2 is
end entity myent2;

entity myent3 is
end myent3;"
        );
    }

    #[test]
    fn remove_middle_design_unit() {
        let new_root = remove_design_unit_at(1, MULTI_ENTITY);
        assert_eq!(
            format!("{}", new_root).trim(),
            "\
entity myent is
end entity;

entity myent3 is
end myent3;"
        );
    }

    #[test]
    fn remove_last_design_unit() {
        let new_root = remove_design_unit_at(2, MULTI_ENTITY);
        assert_eq!(
            format!("{}", new_root).trim(),
            "\
entity myent is
end entity;

entity myent2 is
end entity myent2;"
        );
    }

    #[test]
    fn remove_all_design_units() {
        let root = parse_root(MULTI_ENTITY);
        let new_root = root.rewrite(|el| match el {
            SyntaxElement::Node(n) if n.kind() == NodeKind::DesignUnit => RewriteAction::Remove,
            _ => RewriteAction::Leave,
        });
        let mut remaining_kinds = new_root.green().children().map(|c| match c {
            GreenChild::Node(n) => Child::Node(n.kind()),
            GreenChild::Token(t) => Child::Token(t.kind()),
        });
        // Only the trailing Eof token survives.
        assert!(matches!(
            remaining_kinds.next(),
            Some(Child::Token(crate::tokens::TokenKind::Eof))
        ));
        assert!(remaining_kinds.next().is_none());
    }

    #[test]
    fn idempotent_when_no_match() {
        let root = parse_root(MULTI_ENTITY);
        let once = root.rewrite(|_| RewriteAction::Leave);
        let twice = once.rewrite(|_| RewriteAction::Leave);
        assert_eq!(once.green(), twice.green());
    }
}
