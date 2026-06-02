//! Facilities for parsing an input file or string into a [SyntaxNode]
use crate::parser::error_recovery::RecoveryState;
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com
use crate::standard::VHDLStandard;
use crate::syntax::node::SyntaxNode;
use crate::syntax::{DesignFileSyntax, NodeKind};
use crate::tokens::TokenStream;
use crate::tokens::Tokenizer;

pub(crate) mod builder;
pub mod error;
#[cfg(test)]
mod test_utils;
#[macro_use]
mod util;
mod error_recovery;
mod list;
pub mod productions;

/// The parser turns a token stream, produced by a [TokenStream] into
/// [Syntax Nodes](crate::syntax::node::SyntaxNode) that form a Concrete Syntax Tree.
/// These syntax nodes can be converted to [AST Nodes](crate::syntax::AstNode)
/// to manipulate and traverse the syntax tree.
pub struct Parser {
    token_stream: TokenStream,
    builder: builder::NodeBuilder,
    errors: Vec<error::SyntaxErr>,
    unexpected_eof: bool,
    standard: VHDLStandard,
    recovery: RecoveryState,
    /// Builder position at the last `expect_tokens_recover` call that made no
    /// progress.  When recovery is invoked a second time at the same position,
    /// we force-skip a token to guarantee forward progress.
    last_recovery_pos: Option<usize>,
}

impl Parser {
    pub(crate) fn new(token_stream: TokenStream, standard: VHDLStandard) -> Parser {
        Parser {
            token_stream,
            builder: builder::NodeBuilder::new(),
            errors: Vec::default(),
            unexpected_eof: false,
            standard,
            recovery: RecoveryState::new(),
            last_recovery_pos: None,
        }
    }

    pub fn standard(&self) -> VHDLStandard {
        self.standard
    }

    pub fn into_root(self) -> (SyntaxNode, Vec<error::SyntaxErr>) {
        let (green, diagnostics) = self.end();
        (SyntaxNode::new_root(green), diagnostics)
    }
}

/// Parse and return a VHDL file using the default VHDL standard.
///
/// Use [`parse_with_standard`] to use a non-default VHDL standard.
pub fn parse(token_stream: impl Into<TokenStream>) -> (DesignFileSyntax, Vec<error::SyntaxErr>) {
    let mut parser = Parser::new(token_stream.into(), VHDLStandard::default());
    parser.design_file();
    let (syntax_node, diagnostics) = parser.into_root();
    debug_assert!(syntax_node.kind() == NodeKind::DesignFile);
    (DesignFileSyntax(syntax_node), diagnostics)
}

/// Parse and return a VHDL file, tokenizing and parsing under the given `standard`.
pub fn parse_with_standard(
    standard: VHDLStandard,
    input: impl IntoIterator<Item = u8>,
) -> (DesignFileSyntax, Vec<error::SyntaxErr>) {
    let token_stream: TokenStream = Tokenizer::with_standard(standard, input.into_iter()).collect();
    let mut parser = Parser::new(token_stream, standard);
    parser.design_file();
    let (syntax_node, diagnostics) = parser.into_root();
    debug_assert!(syntax_node.kind() == NodeKind::DesignFile);
    (DesignFileSyntax(syntax_node), diagnostics)
}

#[cfg(test)]
pub(crate) fn parse_syntax(
    token_stream: impl Into<TokenStream>,
    parser_fn: impl FnOnce(&mut Parser),
) -> (SyntaxNode, Vec<error::SyntaxErr>) {
    let mut parser = Parser::new(token_stream.into(), VHDLStandard::default());
    parser_fn(&mut parser);
    let (green, diagnostics) = parser.end();
    (SyntaxNode::new_root(green), diagnostics)
}

#[cfg(test)]
pub(crate) fn parse_syntax_with_standard(
    standard: VHDLStandard,
    input: impl IntoIterator<Item = u8>,
    parser_fn: impl FnOnce(&mut Parser),
) -> (SyntaxNode, Vec<error::SyntaxErr>) {
    let token_stream: TokenStream = Tokenizer::with_standard(standard, input.into_iter()).collect();
    let mut parser = Parser::new(token_stream, standard);
    parser_fn(&mut parser);
    let (green, diagnostics) = parser.end();
    (SyntaxNode::new_root(green), diagnostics)
}
