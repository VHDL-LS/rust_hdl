//! Facilities for parsing an input file or string into a [SyntaxNode]
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com
use crate::syntax::node::SyntaxNode;
use crate::syntax::{DesignFileSyntax, NodeKind};
use crate::tokens::TokenStream;

mod builder;
pub mod diagnostics;
#[cfg(test)]
mod test_utils;
#[macro_use]
mod util;
mod list;
pub mod productions;

/// The parser turns a token stream, produced by a [TokenStream] into
/// [Syntax Nodes](crate::syntax::node::SyntaxNode) that form a Concrete Syntax Tree.
/// These syntax nodes can be converted to [AST Nodes](crate::syntax::AstNode)
/// to manipulate and traverse the syntax tree.
pub struct Parser {
    token_stream: TokenStream,
    builder: builder::NodeBuilder,
    diagnostics: Vec<diagnostics::ParserDiagnostic>,
    unexpected_eof: bool,
}

impl Parser {
    pub fn new(token_stream: TokenStream) -> Parser {
        Parser {
            token_stream,
            builder: builder::NodeBuilder::new(),
            diagnostics: Vec::default(),
            unexpected_eof: false,
        }
    }

    pub fn into_root(self) -> (SyntaxNode, Vec<diagnostics::ParserDiagnostic>) {
        let (green, diagnostics) = self.end();
        (SyntaxNode::new_root(green), diagnostics)
    }
}

/// Parse and return a VHDL file.
pub fn parse(
    token_stream: impl Into<TokenStream>,
) -> (DesignFileSyntax, Vec<diagnostics::ParserDiagnostic>) {
    let mut parser: Parser = Parser::new(token_stream.into());
    parser.design_file();
    let (syntax_node, diagnostics) = parser.into_root();
    debug_assert!(syntax_node.kind() == NodeKind::DesignFile);
    (DesignFileSyntax(syntax_node), diagnostics)
}

#[cfg(test)]
pub(crate) fn parse_syntax(
    token_stream: impl Into<TokenStream>,
    parser_fn: impl FnOnce(&mut Parser),
) -> (SyntaxNode, Vec<diagnostics::ParserDiagnostic>) {
    let mut parser: Parser = Parser::new(token_stream.into());
    parser_fn(&mut parser);
    let (green, diagnostics) = parser.end();
    (SyntaxNode::new_root(green), diagnostics)
}
