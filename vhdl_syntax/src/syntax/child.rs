// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

/// A child that is either a Token or a Node.
///
/// This enum is generic to accommodate for the different kinds of childs
/// (for example, the internal `GreenChild` or the public `SyntaxElement`)
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Child<N, T> {
    Node(N),
    Token(T),
}

impl<N, T> Child<N, T> {
    pub fn as_ref(&self) -> Child<&N, &T> {
        match self {
            Child::Token(token) => Child::Token(token),
            Child::Node(node) => Child::Node(node),
        }
    }

    pub fn is_node(&self) -> bool {
        matches!(self, Child::Node(_))
    }

    pub fn is_token(&self) -> bool {
        matches!(self, Child::Token(_))
    }
}

impl<N, T> Child<N, T>
where
    N: Clone,
    T: Clone,
{
    pub fn as_token(&self) -> Option<T> {
        match self {
            Child::Token(token) => Some(token.clone()),
            Child::Node(_) => None,
        }
    }

    pub fn as_node(&self) -> Option<N> {
        match self {
            Child::Token(_) => None,
            Child::Node(node) => Some(node.clone()),
        }
    }
}
