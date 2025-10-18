//! Facilities for parsing an input file or string into a [SyntaxNode]
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com
use crate::parser::diagnostics::ParserDiagnostic;
use crate::syntax::node::SyntaxNode;
use crate::tokens::{Token, TokenStream, Tokenize};
use std::collections::VecDeque;

mod builder;
pub mod diagnostics;
#[cfg(test)]
mod test_utils;
#[macro_use]
mod util;
mod list;
mod productions;

/// The parser turns a token stream, produced by a [TokenStream] into
/// [Syntax Nodes](crate::node::SyntaxNode) that form a Concrete Syntax Tree.
/// These syntax nodes can be converted to [AST Nodes](crate::syntax::AstNode)
/// to manipulate and traverse the syntax tree.
pub struct Parser<T: TokenStream> {
    tokenizer: T,
    builder: builder::NodeBuilder,
    diagnostics: Vec<diagnostics::ParserDiagnostic>,
    unexpected_eof: bool,
}

impl<T: TokenStream> Parser<T> {
    pub fn new(tokenizer: T) -> Parser<T> {
        Parser {
            tokenizer,
            builder: builder::NodeBuilder::new(),
            diagnostics: Vec::default(),
            unexpected_eof: false,
        }
    }

    pub fn diagnostics(&self) -> &[diagnostics::ParserDiagnostic] {
        &self.diagnostics
    }

    pub fn token_index(&self) -> usize {
        self.builder.current_token_index()
    }

    /// Parses a design file and returns the root node.
    pub(crate) fn parse(mut self) -> (SyntaxNode, Vec<diagnostics::ParserDiagnostic>) {
        self.design_file();
        let (green, diagnostics) = self.end();
        (SyntaxNode::new_root(green), diagnostics)
    }
}

/// generic trait for all entities that can parse into a node.
/// The generic function commonly is a reference to a parser function
/// such as [Parser::entity] that defines the content of the node.
pub trait CanParse<T> {
    fn parse_syntax(self, func: impl FnOnce(&mut Parser<T>))
        -> (SyntaxNode, Vec<ParserDiagnostic>);
}

impl<T> CanParse<T> for T
where
    T: TokenStream,
{
    fn parse_syntax(
        self,
        func: impl FnOnce(&mut Parser<T>),
    ) -> (SyntaxNode, Vec<ParserDiagnostic>) {
        let mut parser = Parser::new(self);
        func(&mut parser);
        let (green, diagnostics) = parser.end();
        (SyntaxNode::new_root(green), diagnostics)
    }
}

impl CanParse<VecDeque<Token>> for &str {
    fn parse_syntax(
        self,
        func: impl FnOnce(&mut Parser<VecDeque<Token>>),
    ) -> (SyntaxNode, Vec<ParserDiagnostic>) {
        let mut parser = Parser::new(VecDeque::from_iter(self.tokenize()));
        func(&mut parser);
        let (green, diagnostics) = parser.end();
        (SyntaxNode::new_root(green), diagnostics)
    }
}
