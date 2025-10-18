// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

#![cfg(test)]

use crate::parser::{CanParse, Parser};
use crate::syntax::node::{SyntaxNode, SyntaxToken};
use crate::syntax::AstNode;
use crate::tokens::Token;
use std::collections::VecDeque;

mod concurrent_statements;
mod declarations;

fn node<T: AstNode>(func: impl FnOnce(&mut Parser<VecDeque<Token>>), input: &str) -> T {
    let (entity, diagnostics) = input.parse_syntax(func);
    assert!(diagnostics.is_empty(), "got diagnostics: {:?}", diagnostics);
    T::cast(entity).unwrap()
}

trait TestToString {
    fn debug_to_string(&self) -> String;
}

impl TestToString for SyntaxToken {
    fn debug_to_string(&self) -> String {
        self.to_string()
    }
}

impl TestToString for SyntaxNode {
    fn debug_to_string(&self) -> String {
        self.to_string()
    }
}

impl<T> TestToString for T
where
    T: AstNode,
{
    fn debug_to_string(&self) -> String {
        self.raw().debug_to_string()
    }
}

impl<T> TestToString for Option<T>
where
    T: TestToString,
{
    fn debug_to_string(&self) -> String {
        self.as_ref().unwrap().debug_to_string()
    }
}

fn check_nodes<T: AstNode>(nodes: impl Iterator<Item = T>, expected: &str) {
    assert_eq!(
        nodes
            .map(|node| node.raw().to_string())
            .collect::<String>()
            .trim(),
        expected
    );
}

#[macro_export]
macro_rules! check {
    // Base case: single field checking for None
    ($node:expr, $field:ident => None) => {
        assert!($node.$field().is_none());
    };

    // Base case: single field-value pair with string literal
    ($node:expr, $field:ident => $expected:expr) => {
        assert!($node.$field().is_some(), "{} is not present", stringify!($node.$field()));
        assert_eq!($crate::syntax::tests::TestToString::debug_to_string(&$node.$field()).trim(), $expected);
    };

    // Recursive case: field checking for None, followed by more pairs
    ($node:expr, $field:ident => None, $($rest_field:ident => $rest_expected:tt),+ $(,)?) => {
        check!($node, $field => None);
        check!($node, $($rest_field => $rest_expected),+);
    };

    // Recursive case: field with string literal, followed by more pairs
    ($node:expr, $field:ident => $expected:expr, $($rest_field:ident => $rest_expected:tt),+ $(,)?) => {
        check!($node, $field => $expected);
        check!($node, $($rest_field => $rest_expected),+);
    };
}
