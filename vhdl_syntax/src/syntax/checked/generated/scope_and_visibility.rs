// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2026, Lukas Scheller lukasscheller@icloud.com
use super::*;
use crate::syntax::checked::CheckedNode;
use crate::syntax::node::SyntaxToken;
use crate::syntax::*;
#[derive(Debug, Clone)]
pub struct CheckedUseClause(UseClauseSyntax);
impl CheckedNode for CheckedUseClause {
    type Syntax = UseClauseSyntax;
    fn cast_unchecked(syntax: UseClauseSyntax) -> Self {
        CheckedUseClause(syntax)
    }
    fn raw(&self) -> UseClauseSyntax {
        self.0.clone()
    }
}
impl CheckedUseClause {
    pub fn use_token(&self) -> SyntaxToken {
        self.0.use_token().unwrap()
    }
    pub fn name_list(&self) -> Option<CheckedNameList> {
        self.0.name_list().map(CheckedNameList::cast_unchecked)
    }
    pub fn semi_colon_token(&self) -> SyntaxToken {
        self.0.semi_colon_token().unwrap()
    }
}
