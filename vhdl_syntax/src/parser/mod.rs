// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

//! Parsing tokens into a syntax tree.
//!
//! The main entry point is the [`parse`] function that takes some input and produces a tree
//! covering the whole input plus a list of [`SyntaxErr`](error::SyntaxErr)s.
//!
//! ```
//! use vhdl_syntax::parser;
//! use vhdl_syntax::syntax::{AstNode, NodeKind};
//!
//! // The architecture is missing its name.
//! let (design, errors) = parser::parse("\
//! architecture of foo is -- Note: missing architecture name
//! begin
//! end;
//! ");
//!
//! assert_eq!(parser::error::display_errors(&errors), "12..12 expected Identifier");
//!
//! // The tree still contains an architecture
//! let kinds: Vec<_> = design
//!     .raw()
//!     .children()
//!     .filter_map(|unit| unit.first_child().map(|node| node.kind()))
//!     .collect();
//! assert_eq!(kinds, [NodeKind::ArchitectureBody]);
//! ```
//!
//! # Standards
//!
//! [`parse`] currently only works for VHDL-2008 without Property Specification Language (PSL) statements.
//! Supporting more revisions is forseen in the future.
use crate::parser::error_recovery::RecoveryState;
use crate::standard::VHDLStandard;
use crate::syntax::node::SyntaxNode;
use crate::syntax::{DesignFileSyntax, NodeKind};
use crate::tokens::TokenStream;

pub(crate) mod builder;
pub mod error;
pub(crate) mod marker;
#[cfg(test)]
#[macro_use]
mod test_utils;
#[macro_use]
mod util;
mod error_recovery;
mod list;
pub(crate) mod productions;

/// The parser turns a token stream, produced by a [TokenStream] into
/// [Syntax Nodes](crate::syntax::node::SyntaxNode) that form a Concrete Syntax Tree.
/// These syntax nodes can be converted to [AST Nodes](crate::syntax::AstNode)
/// to manipulate and traverse the syntax tree.
pub(crate) struct Parser {
    token_stream: TokenStream,
    builder: builder::NodeBuilder,
    standard: VHDLStandard,
    recovery: RecoveryState,
}

impl Parser {
    pub(crate) fn new(token_stream: TokenStream, standard: VHDLStandard) -> Parser {
        Parser {
            token_stream,
            builder: builder::NodeBuilder::new(),
            standard,
            recovery: RecoveryState::new(),
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
    parse_with_standard(VHDLStandard::default(), token_stream)
}

/// Parse and return a VHDL file, tokenizing and parsing under the given `standard`.
///
/// **Note**: This is mostly a placeholder right now. Currently, not much changes
/// for anything other than VHDL-2008.
pub fn parse_with_standard(
    standard: VHDLStandard,
    input: impl Into<TokenStream>,
) -> (DesignFileSyntax, Vec<error::SyntaxErr>) {
    let mut parser = Parser::new(input.into(), standard);
    parser.design_file();
    let (syntax_node, diagnostics) = parser.into_root();
    debug_assert!(syntax_node.kind() == NodeKind::DesignFile);
    (DesignFileSyntax(syntax_node), diagnostics)
}

#[cfg(test)]
pub(crate) fn parse_syntax<T>(
    token_stream: impl Into<TokenStream>,
    parser_fn: impl FnOnce(&mut Parser) -> T,
) -> (SyntaxNode, Vec<error::SyntaxErr>) {
    let mut parser = Parser::new(token_stream.into(), VHDLStandard::default());
    parser_fn(&mut parser);
    let (green, diagnostics) = parser.end();
    (SyntaxNode::new_root(green), diagnostics)
}

#[cfg(test)]
pub(crate) fn parse_syntax_with_standard<T>(
    standard: VHDLStandard,
    input: impl IntoIterator<Item = u8>,
    parser_fn: impl FnOnce(&mut Parser) -> T,
) -> (SyntaxNode, Vec<error::SyntaxErr>) {
    let token_stream: TokenStream =
        crate::tokens::Tokenizer::with_standard(standard, input.into_iter()).collect();
    let mut parser = Parser::new(token_stream, standard);
    parser_fn(&mut parser);
    let (green, diagnostics) = parser.end();
    (SyntaxNode::new_root(green), diagnostics)
}
