//! Facilities for parsing an input file or string into a [SyntaxNode]
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com
use crate::parser::diagnostics::ParserDiagnostic;
use crate::syntax::node::SyntaxNode;
use crate::tokens::TokenStream;

mod builder;
pub mod design;
pub mod diagnostics;
pub mod entity;
pub mod identifier;
pub mod statements;
mod util;

/// The parser turns a token stram, produced by a [TokenStream] into
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

    /// Parses a design file and returns the root node.
    pub(crate) fn parse(mut self) -> (SyntaxNode, Vec<diagnostics::ParserDiagnostic>) {
        self.design_file();
        (SyntaxNode::new_root(self.builder.end()), self.diagnostics)
    }
}

/// generic trait for all entities that can parse into a node.
/// The generic function commonly is a reference to a parser function
/// such as [Parser::entity] that defines the content of the node.
pub trait CanParse<T> {
    fn parse(self, func: impl FnOnce(&mut Parser<T>)) -> (SyntaxNode, Vec<ParserDiagnostic>);
}

impl<T> CanParse<T> for T
where
    T: TokenStream,
{
    fn parse(self, func: impl FnOnce(&mut Parser<T>)) -> (SyntaxNode, Vec<ParserDiagnostic>) {
        let mut parser = Parser::new(self);
        func(&mut parser);
        let (green, diagnostics) = parser.end();
        (SyntaxNode::new_root(green), diagnostics)
    }
}
