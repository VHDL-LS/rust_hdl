// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2024, Lukas Scheller lukasscheller@icloud.com

//! Traversal of syntax trees.
//!
//! Note that the iterators in this module are rarely constructed directly.
//! Instead, they are usually obtained from the nodes directly using
//! [SyntaxNode::walk], [SyntaxNode::descendants],
//! [SyntaxNode::descendants_with_tokens] or [SyntaxNode::visit_tokens].

use crate::syntax::{
    node::{SyntaxElement, SyntaxNode},
    SyntaxToken,
};

/// An event that is emitted when walking a tree.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WalkEvent {
    /// A node was entered.
    Enter(SyntaxNode),
    /// A node was left; all of its children have been visited.
    Leave(SyntaxNode),
    /// A token was visited.
    Token(SyntaxToken),
}

/// Iterator that visits the nodes and tokens of a subtree in textual order,
/// and that allows skipping subtrees.
pub struct PreorderWithTokens {
    start: SyntaxNode,
    next: Option<WalkEvent>,
    last: Option<WalkEvent>,
    skip_subtree: bool,
}

impl PreorderWithTokens {
    /// Creates a new `PreorderWithTokens` that traverses `start`.
    pub fn new(start: SyntaxNode) -> PreorderWithTokens {
        let next = Some(WalkEvent::Enter(start.clone()));
        PreorderWithTokens {
            start,
            next,
            last: None,
            skip_subtree: false,
        }
    }

    /// Stops the traversal from descending any further and continues after the element that
    /// was yielded last. What exactly is skipped depends on that element:
    ///
    /// - After a [WalkEvent::Enter], the children of the entered node are skipped;
    ///   the next event is the [WalkEvent::Leave] of that same node.
    /// - After a [WalkEvent::Token], the siblings that follow the token are skipped;
    ///   the next event is the [WalkEvent::Leave] of the token's parent.
    /// - After a [WalkEvent::Leave], nothing is skipped, since the subtree that was left is
    ///   already fully visited. The same holds before the first event was yielded.
    pub fn skip_subtree(&mut self) {
        self.skip_subtree = true
    }

    fn do_skip(&mut self) {
        self.next = self.next.take().map(|next| match next {
            WalkEvent::Enter(node) => WalkEvent::Leave(
                node.parent()
                    .expect("a descended-into node always has a parent"),
            ),
            WalkEvent::Token(token) => WalkEvent::Leave(token.parent()),
            WalkEvent::Leave(node) => WalkEvent::Leave(node),
        })
    }
}

impl Iterator for PreorderWithTokens {
    type Item = WalkEvent;

    fn next(&mut self) -> Option<Self::Item> {
        if self.skip_subtree {
            self.skip_subtree = false;
            // After a `Leave`, and before the first event, there is nothing left to skip.
            if matches!(self.last, Some(WalkEvent::Enter(_) | WalkEvent::Token(_))) {
                self.do_skip();
            }
        }
        let next = self.next.take();
        self.last = next.clone();
        self.next = next.as_ref().and_then(|next| {
            Some(match next {
                WalkEvent::Enter(el) => match el.first_child_or_token() {
                    SyntaxElement::Node(node) => WalkEvent::Enter(node),
                    SyntaxElement::Token(token) => WalkEvent::Token(token),
                },
                WalkEvent::Leave(node) if node == &self.start => return None,
                WalkEvent::Leave(node) => match node.next_sibling_or_token() {
                    Some(SyntaxElement::Node(sibling)) => WalkEvent::Enter(sibling),
                    Some(SyntaxElement::Token(sibling)) => WalkEvent::Token(sibling),
                    None => WalkEvent::Leave(node.parent()?),
                },
                WalkEvent::Token(token) => match token.next_sibling_or_token() {
                    Some(SyntaxElement::Node(sibling)) => WalkEvent::Enter(sibling),
                    Some(SyntaxElement::Token(sibling)) => WalkEvent::Token(sibling),
                    None => WalkEvent::Leave(token.parent()),
                },
            })
        });
        next
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser;
    use crate::syntax::node_kind::NodeKind;
    use crate::syntax::AstNode;
    use pretty_assertions::assert_eq;

    const SOURCE: &str = "\
entity foo is
  generic (g : integer := 3);
  port (a : in bit; b : out bit);
end entity foo;

architecture rtl of foo is
  signal s : bit;
begin
  b <= a and s;
end architecture rtl;
";

    fn parse_root(src: &str) -> SyntaxNode {
        let (file, diagnostics) = parser::parse(src);
        assert!(
            diagnostics.is_empty(),
            "got diagnostics:\n{}",
            parser::error::display_errors(&diagnostics)
        );
        file.raw()
    }

    /// Reference implementation of a pre-order traversal that [`PreorderWithTokens`] must agree
    /// with.
    fn nodes_recursive(node: &SyntaxNode, out: &mut Vec<NodeKind>) {
        nodes_recursive_pruned(node, None, out)
    }

    /// Like [`nodes_recursive`], but does not descend into nodes of kind `prune`.
    /// This is the reference for a traversal that calls
    /// [`skip_subtree`](PreorderWithTokens::skip_subtree) on every such node.
    fn nodes_recursive_pruned(node: &SyntaxNode, prune: Option<NodeKind>, out: &mut Vec<NodeKind>) {
        out.push(node.kind());
        if Some(node.kind()) == prune {
            return;
        }
        for child in node.children() {
            nodes_recursive_pruned(&child, prune, out);
        }
    }

    fn visited(node: &SyntaxNode) -> Vec<NodeKind> {
        node.descendants().map(|node| node.kind()).collect()
    }

    /// Returns the first descendant of `node` with the given kind.
    fn find(node: &SyntaxNode, kind: NodeKind) -> SyntaxNode {
        node.descendants()
            .find(|node| node.kind() == kind)
            .unwrap_or_else(|| panic!("no {kind:?} in the test source"))
    }

    #[test]
    fn preorder_visits_every_node_once() {
        let root = parse_root(SOURCE);
        let mut expected = Vec::new();
        nodes_recursive(&root, &mut expected);
        assert_eq!(visited(&root), expected);
    }

    #[test]
    fn preorder_stays_within_the_start_node() {
        let root = parse_root(SOURCE);
        for node in root.descendants() {
            let mut expected = Vec::new();
            nodes_recursive(&node, &mut expected);
            assert_eq!(
                visited(&node),
                expected,
                "traversal starting at {:?} left its subtree",
                node.kind()
            );
        }
    }

    #[test]
    fn preorder_with_tokens_covers_the_whole_tree() {
        let root = parse_root(SOURCE);
        let mut depth = 0usize;
        let mut nodes = Vec::new();
        let mut text = String::new();
        for event in root.walk() {
            match event {
                WalkEvent::Enter(node) => {
                    nodes.push(node.kind());
                    depth += 1;
                }
                WalkEvent::Leave(_) => depth -= 1,
                WalkEvent::Token(token) => {
                    assert!(depth > 0, "token outside of any node");
                    text.push_str(&token.text().to_string());
                }
            }
        }
        assert_eq!(depth, 0, "unbalanced enter/leave events");

        let mut expected = Vec::new();
        nodes_recursive(&root, &mut expected);
        assert_eq!(nodes, expected);

        // `visit_tokens` must be exactly the token events of `walk`.
        let tokens: String = root
            .visit_tokens()
            .map(|token| token.text().to_string())
            .collect();
        assert_eq!(tokens, text);
    }

    #[test]
    fn preorder_with_tokens_visits_a_node_that_contains_only_tokens() {
        let root = parse_root(SOURCE);
        let node = find(&root, NodeKind::NameDesignatorPrefix);
        let events = node.walk().collect::<Vec<_>>();
        assert_eq!(events.len(), 3, "expected enter, token, leave: {events:#?}");
        assert_eq!(events[0], WalkEvent::Enter(node.clone()));
        assert!(matches!(events[1], WalkEvent::Token(_)));
        assert_eq!(events[2], WalkEvent::Leave(node));
    }

    #[test]
    fn walk_interleaves_nodes_and_tokens_without_loss() {
        let root = parse_root(SOURCE);
        let elements = root
            .walk()
            .filter_map(|event| match event {
                WalkEvent::Enter(node) => Some(SyntaxElement::Node(node)),
                WalkEvent::Token(token) => Some(SyntaxElement::Token(token)),
                WalkEvent::Leave(_) => None,
            })
            .collect::<Vec<_>>();

        assert_eq!(
            elements.first().and_then(|el| el.as_node()),
            Some(root.clone())
        );
        assert!(elements
            .iter()
            .filter_map(|el| el.as_node())
            .eq(root.descendants()));
        assert!(elements
            .iter()
            .filter_map(|el| el.as_token())
            .eq(root.visit_tokens()));
        assert_eq!(
            elements.len(),
            root.descendants().count() + root.visit_tokens().count()
        );
    }

    fn walk_skipping(
        start: &SyntaxNode,
        skip_on: impl Fn(&WalkEvent) -> bool,
    ) -> (Vec<NodeKind>, String) {
        let mut nodes = Vec::new();
        let mut text = String::new();
        let mut depth = 0usize;
        let mut walk = start.walk();
        while let Some(event) = walk.next() {
            match &event {
                WalkEvent::Enter(node) => {
                    nodes.push(node.kind());
                    depth += 1;
                }
                WalkEvent::Leave(_) => depth -= 1,
                WalkEvent::Token(token) => text.push_str(&token.text().to_string()),
            }
            if skip_on(&event) {
                walk.skip_subtree();
            }
        }
        assert_eq!(depth, 0, "skipping left enter/leave events unbalanced");
        (nodes, text)
    }

    #[test]
    fn skip_subtree_prunes_the_entered_subtree() {
        let root = parse_root(SOURCE);
        const PRUNE: NodeKind = NodeKind::SubtypeIndication;

        let (nodes, text) = walk_skipping(
            &root,
            |event| matches!(event, WalkEvent::Enter(node) if node.kind() == PRUNE),
        );

        let mut expected = Vec::new();
        nodes_recursive_pruned(&root, Some(PRUNE), &mut expected);
        assert_eq!(nodes, expected);
        assert!(nodes.contains(&PRUNE), "the skipped node itself is visited");

        assert!(!text.contains("integer"), "visited a token below {PRUNE:?}");
        assert!(!text.contains("bit"), "visited a token below {PRUNE:?}");
        assert!(
            text.contains("entity"),
            "unrelated tokens are still visited"
        );
    }

    #[test]
    fn skip_subtree_on_the_start_node_ends_the_walk() {
        let root = parse_root(SOURCE);
        let mut walk = root.walk();
        assert_eq!(walk.next(), Some(WalkEvent::Enter(root.clone())));
        walk.skip_subtree();
        assert_eq!(walk.next(), Some(WalkEvent::Leave(root)));
        assert_eq!(walk.next(), None);
    }

    #[test]
    fn skip_subtree_after_a_token_skips_the_remaining_siblings() {
        let root = parse_root(SOURCE);
        let mut walk = root.walk();
        let token = loop {
            match walk.next().expect("the source contains such a token") {
                WalkEvent::Token(token) if token.next_sibling_or_token().is_some() => break token,
                _ => {}
            }
        };
        walk.skip_subtree();
        assert_eq!(walk.next(), Some(WalkEvent::Leave(token.parent())));
    }

    #[test]
    fn skip_subtree_after_leave_is_a_no_op() {
        let root = parse_root(SOURCE);
        let (nodes, text) = walk_skipping(&root, |event| matches!(event, WalkEvent::Leave(_)));

        let mut expected = Vec::new();
        nodes_recursive(&root, &mut expected);
        assert_eq!(nodes, expected);

        let expected_text: String = root
            .visit_tokens()
            .map(|token| token.text().to_string())
            .collect();
        assert_eq!(text, expected_text);
    }

    #[test]
    fn skip_subtree_before_the_first_event_is_a_no_op() {
        let root = parse_root(SOURCE);
        let mut walk = root.walk();
        walk.skip_subtree();
        let nodes = walk
            .filter_map(|event| match event {
                WalkEvent::Enter(node) => Some(node.kind()),
                _ => None,
            })
            .collect::<Vec<_>>();

        let mut expected = Vec::new();
        nodes_recursive(&root, &mut expected);
        assert_eq!(nodes, expected);
    }
}
