// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

use crate::syntax::green::{GreenNode, GreenNodeData};
use crate::syntax::node_kind::NodeKind;
use crate::tokens::Token;

/// Internal builder used to create nodes when parsing.
pub(crate) struct NodeBuilder {
    stack: Vec<(usize, GreenNodeData)>,
    rel_offset: usize,
    text_len: usize,
}

impl NodeBuilder {
    pub fn new() -> NodeBuilder {
        NodeBuilder {
            stack: Vec::default(),
            rel_offset: 0,
            text_len: 0,
        }
    }

    fn current(&mut self) -> &mut GreenNodeData {
        &mut self.stack.last_mut().unwrap().1
    }

    pub fn push(&mut self, token: Token) {
        let tok_text_len = token.byte_len();
        let offset = self.rel_offset;
        self.current().push_token(offset, token);
        self.rel_offset += tok_text_len;
    }

    pub fn start_node(&mut self, kind: NodeKind) {
        self.stack.push((self.rel_offset, GreenNodeData::new(kind)));
        self.rel_offset = 0;
    }

    pub fn end_node(&mut self) {
        if self.stack.len() == 1 {
            return;
        }
        let (offset, node) = self.stack.pop().expect("Unbalanced start_node / end_node");
        self.current().push_node(offset, node)
    }

    pub fn end(mut self) -> GreenNode {
        GreenNode::new(
            self.stack
                .pop()
                .expect("Unbalanced start_node / end_node")
                .1,
        )
    }

    pub fn current_pos(&self) -> usize {
        self.text_len
    }
}
