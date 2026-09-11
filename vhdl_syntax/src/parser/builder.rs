// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

use std::num::NonZeroUsize;

use crate::parser::error::{SyntaxErr, SyntaxErrKind};
use crate::parser::marker::{Marker, UnknownMarker};
use crate::syntax::child::Child;
use crate::syntax::green::{GreenChild, GreenNode, GreenNodeData, GreenToken};
use crate::syntax::meta::is_empty_capable;
use crate::syntax::node_kind::NodeKind;
use crate::tokens::tokenizer::LexErr;
use crate::tokens::{Token, TokenKind};

pub(crate) enum Event {
    Start {
        kind: Option<NodeKind>,
        forward_parent: Option<NonZeroUsize>,
    },
    /// End a node
    End,
    /// Push a token with an optional associated lexer error
    Push(Token, Option<LexErr>),
    Ignore,
    /// Emit a syntax error that is associated to the last token or node pushed
    Error(SyntaxErrKind),
}

fn first_token(node: &GreenNode) -> &Token {
    match node.children().next().expect("empty nodes are dropped") {
        GreenChild::Token(token) => token.token(),
        GreenChild::Node(node) => first_token(node),
    }
}

/// Internal builder used to create nodes when parsing.
pub(crate) struct NodeBuilder {
    token_index: usize,
    events: Vec<Event>,
}

impl NodeBuilder {
    pub fn new() -> NodeBuilder {
        NodeBuilder {
            token_index: 0,
            events: Vec::new(),
        }
    }

    pub fn push_event(&mut self, event: Event) -> usize {
        let len = self.events.len();
        self.events.push(event);
        len
    }

    pub fn fix_node(&mut self, index: usize, node: NodeKind) {
        match &mut self.events[index] {
            Event::Start { kind, .. } => assert!(kind.replace(node).is_none()),
            _ => unreachable!(),
        }
    }

    pub fn fix_forward_parent(&mut self, index: usize, distance: NonZeroUsize) {
        match &mut self.events[index] {
            Event::Start { forward_parent, .. } => {
                assert!(forward_parent.replace(distance).is_none())
            }
            _ => unreachable!(),
        }
    }

    pub fn push(&mut self, token: Token, err: Option<LexErr>) {
        self.token_index += 1;
        self.push_event(Event::Push(token, err));
    }

    pub fn start_node(&mut self, kind: NodeKind) -> Marker {
        Marker::new(
            self.push_event(Event::Start {
                kind: Some(kind),
                forward_parent: None,
            }),
            kind,
        )
    }

    pub fn end_node(&mut self) {
        self.push_event(Event::End);
    }

    pub fn end(mut self) -> (GreenNode, Vec<SyntaxErr>) {
        struct Parent {
            kind: NodeKind,
            // index of its first child
            first_child: usize,
            // number of errors recorded while started
            first_error: usize,
        }
        let mut parents: Vec<Parent> = Vec::new();
        let mut children: Vec<GreenChild> = Vec::new();
        let mut errors: Vec<SyntaxErr> = Vec::new();
        // Byte offset just past the last token's text, i.e. before the leading
        // trivia of the next one. This is where a missing token would go.
        let mut text_pos = 0usize;

        for index in 0..self.events.len() {
            match std::mem::replace(&mut self.events[index], Event::Ignore) {
                Event::Ignore => {}
                Event::Start {
                    kind,
                    mut forward_parent,
                } => {
                    // This node plus every node retroactively inserted above
                    // it, innermost first.
                    let mut kinds = vec![kind.expect("start_unknown without resolve")];
                    let mut parent_index = index;
                    while let Some(distance) = forward_parent {
                        parent_index += distance.get();
                        let Event::Start {
                            kind,
                            forward_parent: next,
                        } = std::mem::replace(&mut self.events[parent_index], Event::Ignore)
                        else {
                            unreachable!("a forward parent must be a node start")
                        };
                        kinds.push(kind.expect("start_unknown without resolve"));
                        forward_parent = next;
                    }
                    for kind in kinds.into_iter().rev() {
                        parents.push(Parent {
                            kind,
                            first_child: children.len(),
                            first_error: errors.len(),
                        });
                    }
                }
                Event::End => {
                    let Parent {
                        kind,
                        first_child,
                        first_error,
                    } = parents.pop().expect("end_node without start_node");
                    // Drop empty nodes and merge multiple errors into one
                    if children.len() == first_child {
                        if errors.len() > first_error && !is_empty_capable(kind) {
                            errors.truncate(first_error);
                            let folded = SyntaxErrKind::Expected(
                                Child::<_, Box<[TokenKind]>>::Node(Box::new([kind])),
                            );
                            errors.push(SyntaxErr::new(text_pos..text_pos, folded));
                        }
                        continue;
                    }
                    let data = GreenNodeData::new(kind, children.drain(first_child..))
                        .expect("empty nodes are dropped above");
                    children.push(GreenChild::Node(GreenNode::new(data)));
                }
                Event::Push(token, err) => {
                    if let Some(err) = err {
                        errors.push(SyntaxErr::from_lex_err(err, &token, text_pos));
                    }
                    text_pos += token.byte_len();
                    children.push(GreenChild::Token(GreenToken::new(token)));
                }
                Event::Error(kind) => match kind {
                    SyntaxErrKind::Expected(_) => {
                        errors.push(SyntaxErr::new(text_pos..text_pos, kind))
                    }
                    SyntaxErrKind::Unexpected(Child::Node(_)) => {
                        let Some(GreenChild::Node(node)) = children.last() else {
                            panic!("{kind:?} does not follow a node");
                        };
                        let start = text_pos - node.byte_len()
                            + first_token(node).leading_trivia().byte_len();
                        errors.push(SyntaxErr::new(start..text_pos, kind));
                    }
                    SyntaxErrKind::Unexpected(Child::Token(_)) => {
                        let Some(GreenChild::Token(token)) = children.last() else {
                            panic!("{kind:?} does not follow a token");
                        };
                        let start = text_pos - token.text().len();
                        errors.push(SyntaxErr::new(start..text_pos, kind));
                    }
                    SyntaxErrKind::Unterminated(_) => unreachable!("handled by push(Token)"),
                },
            }
        }

        assert_eq!(children.len(), 1, "expected exactly one root node");
        let root = match children.pop().unwrap() {
            GreenChild::Node(node) => node,
            GreenChild::Token(_) => panic!("the root must be a node, not a token"),
        };
        (root, errors)
    }

    pub fn start_unknown(&mut self) -> UnknownMarker {
        UnknownMarker::new(self.push_event(Event::Start {
            kind: None,
            forward_parent: None,
        }))
    }

    /// Record an error.
    /// The meaning of this is encoded in the `kind`:
    /// SyntaxErrKind::Unexpected is attached to the last token or node
    /// SyntaxErrKind::Expected is not attached and signifies that something was missing
    pub fn push_err(&mut self, kind: SyntaxErrKind) {
        self.push_event(Event::Error(kind));
    }

    pub fn current_token_index(&self) -> usize {
        self.token_index
    }
}

#[cfg(test)]
impl NodeBuilder {
    /// Open a node and return the index of its `Start`, the way a [`Marker`]
    /// records it. These tests drive the builder without a `Parser`, so there
    /// is nothing to hand a marker to; forgetting it skips the drop bomb that
    /// would otherwise report the node as never completed.
    fn start(&mut self, kind: NodeKind) -> usize {
        let pos = self.events.len();
        std::mem::forget(self.start_node(kind));
        pos
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::error::Span;
    use crate::syntax::child::ChildKind;
    use crate::tokens::trivia_piece::TriviaPiece;
    use crate::tokens::Trivia;

    const ROOT: NodeKind = NodeKind::DesignFile;
    const OUTER: NodeKind = NodeKind::DesignUnit;
    const INNER: NodeKind = NodeKind::Name;
    const WRAPPER: NodeKind = NodeKind::BinaryExpression;
    /// A node that may legally hold nothing (`ArchitectureDeclarativePart = BlockDeclarativeItem*`).
    const EMPTY_CAPABLE: NodeKind = NodeKind::ArchitectureDeclarativePart;

    /// A token preceded by `spaces` spaces of leading trivia.
    fn tok(text: &'static [u8], spaces: usize) -> Token {
        let trivia = if spaces == 0 {
            Trivia::new()
        } else {
            Trivia::from([TriviaPiece::Spaces(spaces)])
        };
        Token::new(TokenKind::Identifier, text, trivia)
    }

    fn expected_token() -> SyntaxErrKind {
        SyntaxErrKind::Expected(Child::<Box<[NodeKind]>, _>::Token(Box::new([
            TokenKind::SemiColon,
        ])))
    }

    fn spans(errors: &[SyntaxErr]) -> Vec<Span> {
        errors.iter().map(|err| err.span().clone()).collect()
    }

    /// Insert a node of kind `kind` above the node that starts at `child`, the
    /// way [`Precede`](crate::parser::marker::Precede) does.
    /// Returns the new parent's own index, so that it can be preceded in turn.
    fn precede(builder: &mut NodeBuilder, child: usize, kind: NodeKind) -> usize {
        let parent = builder.start(kind);
        builder.fix_forward_parent(child, NonZeroUsize::new(parent - child).unwrap());
        parent
    }

    #[test]
    fn expected_is_zero_width_at_the_end_of_the_preceding_token() {
        // `ab cd`: the error sits between the two tokens and must hug `ab`
        // rather than point past the space.
        let mut builder = NodeBuilder::new();
        builder.start(ROOT);
        builder.push(tok(b"ab", 0), None);
        builder.push_err(expected_token());
        builder.push(tok(b"cd", 1), None);
        builder.end_node();

        let (_, errors) = builder.end();
        assert_eq!(spans(&errors), vec![2..2]);
    }

    #[test]
    fn expected_at_the_start_of_the_input_is_zero_width_at_zero() {
        let mut builder = NodeBuilder::new();
        builder.start(ROOT);
        builder.push_err(expected_token());
        builder.push(tok(b"ab", 0), None);
        builder.end_node();

        let (_, errors) = builder.end();
        assert_eq!(spans(&errors), vec![0..0]);
    }

    #[test]
    fn unexpected_node_is_measured_without_its_leading_trivia() {
        // `  ab`: the node covers only the token's text, not the two spaces
        // that precede it.
        let mut builder = NodeBuilder::new();
        builder.start(ROOT);
        builder.start(INNER);
        builder.push(tok(b"ab", 2), None);
        builder.end_node();
        builder.push_err(SyntaxErrKind::Unexpected(ChildKind::Node(INNER)));
        builder.end_node();

        let (_, errors) = builder.end();
        assert_eq!(spans(&errors), vec![2..4]);
    }

    #[test]
    fn separate_unexpected_runs_do_not_merge() {
        // A good token between two runs closes the first one.
        let mut builder = NodeBuilder::new();
        builder.start(ROOT);
        builder.push(tok(b"ab", 0), None);
        builder.push_err(SyntaxErrKind::Unexpected(ChildKind::Token(
            TokenKind::Identifier,
        )));
        builder.push(tok(b"cd", 1), None);
        builder.push(tok(b"ef", 1), None);
        builder.push_err(SyntaxErrKind::Unexpected(ChildKind::Token(
            TokenKind::Identifier,
        )));
        builder.end_node();

        let (_, errors) = builder.end();
        assert_eq!(spans(&errors), vec![0..2, 6..8]);
    }

    #[test]
    fn errors_in_an_empty_node_fold_into_one_naming_the_node() {
        let mut builder = NodeBuilder::new();
        builder.start(ROOT);
        builder.push(tok(b"ab", 0), None);
        builder.start(INNER);
        builder.push_err(expected_token());
        builder.push_err(expected_token());
        builder.end_node();
        builder.end_node();

        let (root, errors) = builder.end();
        assert_eq!(errors.len(), 1);
        assert!(
            matches!(errors[0].err(), SyntaxErrKind::Expected(Child::Node(kinds)) if **kinds == [INNER])
        );
        assert_eq!(*errors[0].span(), 2..2);
        // The empty node is not in the tree.
        assert!(root.children().count() == 1);
    }

    #[test]
    fn nested_empty_nodes_fold_to_the_outermost() {
        let mut builder = NodeBuilder::new();
        builder.start(ROOT);
        builder.push(tok(b"ab", 0), None);
        builder.start(OUTER);
        builder.start(INNER);
        builder.push_err(expected_token());
        builder.end_node();
        builder.end_node();
        builder.end_node();

        let (_, errors) = builder.end();
        assert_eq!(errors.len(), 1);
        assert!(
            matches!(errors[0].err(), SyntaxErrKind::Expected(Child::Node(kinds)) if **kinds == [OUTER])
        );
    }

    #[test]
    fn folding_stops_at_an_empty_capable_node() {
        let mut builder = NodeBuilder::new();
        builder.start(ROOT);
        builder.push(tok(b"ab", 0), None);
        builder.start(EMPTY_CAPABLE);
        builder.start(OUTER);
        builder.start(INNER);
        builder.push_err(expected_token());
        builder.end_node();
        builder.end_node();
        builder.end_node();
        builder.end_node();

        let (_, errors) = builder.end();
        assert_eq!(errors.len(), 1);
        assert!(
            matches!(errors[0].err(), SyntaxErrKind::Expected(Child::Node(kinds)) if **kinds == [OUTER])
        );
    }

    #[test]
    fn an_empty_capable_node_keeps_its_own_errors_unfolded() {
        let mut builder = NodeBuilder::new();
        builder.start(ROOT);
        builder.push(tok(b"ab", 0), None);
        builder.start(EMPTY_CAPABLE);
        builder.push_err(expected_token());
        builder.push_err(expected_token());
        builder.end_node();
        builder.end_node();

        let (root, errors) = builder.end();
        assert_eq!(errors.len(), 2);
        assert!(errors
            .iter()
            .all(|err| matches!(err.err(), SyntaxErrKind::Expected(Child::Token(_)))));
        // The empty node is still dropped from the tree.
        assert_eq!(root.children().count(), 1);
    }

    #[test]
    fn precede_wraps_only_the_node_it_names() {
        // `children` holds the enclosing node's children too, so the node
        // pushed last is not necessarily the one being preceded. Here `INNER`
        // is a sibling that the wrapper must leave alone.
        let mut builder = NodeBuilder::new();
        builder.start(ROOT);
        builder.start(INNER);
        builder.push(tok(b"ab", 0), None);
        builder.end_node();
        let inner = builder.start(INNER);
        builder.push(tok(b"cd", 1), None);
        builder.end_node();
        precede(&mut builder, inner, WRAPPER);
        builder.push(tok(b"ef", 1), None);
        builder.end_node(); // WRAPPER
        builder.end_node(); // ROOT

        let (root, _) = builder.end();
        let mut children = root.children();
        assert!(matches!(children.next(), Some(GreenChild::Node(node)) if node.kind() == INNER));
        let Some(GreenChild::Node(wrapper)) = children.next() else {
            panic!("the second child is the wrapper");
        };
        assert_eq!(wrapper.kind(), WRAPPER);
        // The second `INNER` plus `ef`
        assert_eq!(wrapper.children().count(), 2);
        assert!(children.next().is_none());
    }

    #[test]
    fn a_chain_of_precedes_nests_outermost_last() {
        // What `a + b + c` builds: the second `precede` wraps the node the
        // first one produced.
        let mut builder = NodeBuilder::new();
        builder.start(ROOT);
        let inner = builder.start(INNER);
        builder.push(tok(b"a", 0), None);
        builder.end_node();
        let wrapper = precede(&mut builder, inner, WRAPPER);
        builder.push(tok(b"b", 1), None);
        builder.end_node();
        precede(&mut builder, wrapper, OUTER);
        builder.push(tok(b"c", 1), None);
        builder.end_node();
        builder.end_node(); // ROOT

        let (root, _) = builder.end();
        let Some(GreenChild::Node(outer)) = root.children().next() else {
            panic!("the root holds the outermost wrapper");
        };
        assert_eq!(outer.kind(), OUTER);
        let Some(GreenChild::Node(wrapper)) = outer.children().next() else {
            panic!("the outermost wrapper holds the inner one");
        };
        assert_eq!(wrapper.kind(), WRAPPER);
        assert!(
            matches!(wrapper.children().next(), Some(GreenChild::Node(node)) if node.kind() == INNER)
        );
    }

    #[test]
    fn a_node_that_holds_a_token_is_kept_with_its_errors_in_place() {
        let mut builder = NodeBuilder::new();
        builder.start(ROOT);
        builder.start(INNER);
        builder.push_err(expected_token());
        builder.push(tok(b"ab", 1), None);
        builder.end_node();
        builder.end_node();

        let (root, errors) = builder.end();
        assert_eq!(spans(&errors), vec![0..0]);
        assert!(matches!(
            root.children().next(),
            Some(GreenChild::Node(node)) if node.kind() == INNER
        ));
    }
}
