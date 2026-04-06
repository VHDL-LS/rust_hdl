use crate::syntax::{
    layout_of,
    meta::{Layout, LayoutItemKind, Sequence},
    node::{SyntaxElement, SyntaxNode},
    validate::error::{Missing, ValidationError},
};
use std::collections::VecDeque;

// OPTIMIZATION:
// There are a few things one could optimize here:
// - Use a bitmap to check which nodes are present / missing (cheap; requires no construction and no / small allocation)
// - make recursive algorithm non-recursive, e.g., by using tree walking
// - Collect everything into one error; don't use multiples (reduces allocation again for deep traversal)

/// Internal helper to validate nodes for missing or extraneous elements.
pub(crate) struct Validator {
    nodes: VecDeque<SyntaxElement>,
    err: ValidationError,
    parent: SyntaxNode,
    last: Option<SyntaxElement>,
}

fn validate_child(child: &SyntaxElement) -> ValidationError {
    match child {
        SyntaxElement::Node(node) => Validator::new(node).validate(),
        _ => ValidationError::new(),
    }
}

impl Validator {
    pub(crate) fn new(node: &SyntaxNode) -> Validator {
        Validator {
            nodes: node.children_with_tokens().collect(),
            err: ValidationError::new(),
            parent: node.clone(),
            last: None,
        }
    }

    pub(crate) fn validate(mut self) -> ValidationError {
        let layout = layout_of(self.parent.kind());
        match layout {
            Layout::Sequence(sequence) => {
                self.validate_sequence_children(sequence);
                for node in self.nodes {
                    let mut errs = validate_child(&node);
                    self.err.append(&mut errs);
                    self.err.push_extraneous(node);
                }
                self.err
            }
            Layout::Choice(_) => {
                debug_assert!(
                    false,
                    "layout_of(node.kind()) should never return a choice node"
                );
                ValidationError::new()
            }
        }
    }

    fn pop_front(&mut self) {
        self.last = self.nodes.pop_front();
        if let Some(last) = &self.last {
            let mut child_errs = validate_child(last);
            self.err.append(&mut child_errs);
        };
    }

    fn push_missing(&mut self, kind: LayoutItemKind) {
        self.err
            .push_missing(Missing::new(self.last.clone(), self.parent.clone(), kind));
    }

    fn next_matches_layout(&self, kind: &LayoutItemKind) -> bool {
        self.nodes.front().is_some_and(|node| match node {
            SyntaxElement::Node(node) => match kind {
                LayoutItemKind::Token(_) | LayoutItemKind::TokenChoice(_) => false,
                LayoutItemKind::Node(node_kind) => &node.kind() == node_kind,
                LayoutItemKind::NodeChoice(node_kinds) => node_kinds.contains(&node.kind()),
            },
            SyntaxElement::Token(token) => match kind {
                LayoutItemKind::Token(token_kind) => &token.kind() == token_kind,
                LayoutItemKind::Node(_) | LayoutItemKind::NodeChoice(_) => false,
                LayoutItemKind::TokenChoice(token_kinds) => token_kinds.contains(&token.kind()),
            },
        })
    }

    fn require(&mut self, kind: &LayoutItemKind) {
        if self.next_matches_layout(kind) {
            self.pop_front();
        } else {
            self.push_missing(*kind);
        }
    }

    fn optional(&mut self, kind: &LayoutItemKind) {
        if self.next_matches_layout(kind) {
            self.pop_front();
        }
    }

    fn consume_repeated(&mut self, kind: &LayoutItemKind) {
        while self.next_matches_layout(kind) {
            self.pop_front();
        }
    }

    fn validate_sequence_children(&mut self, sequence: &Sequence) {
        for node in sequence.items {
            if node.repeated {
                self.consume_repeated(&node.kind);
            } else if node.optional {
                self.optional(&node.kind);
            } else {
                self.require(&node.kind);
            }
        }
    }
}
