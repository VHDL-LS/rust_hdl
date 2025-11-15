// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind::*;
use crate::tokens::Keyword as Kw;
use crate::tokens::TokenKind::*;
use crate::tokens::TokenStream;

impl<T: TokenStream> Parser<T> {
    pub fn subprogram_declaration(&mut self) {
        self.start_node(SubprogramDeclaration);
        self.subprogram_specification();
        self.expect_token(SemiColon);
        self.end_node();
    }

    pub fn subprogram_specification(&mut self) {
        let is_function = if matches!(
            self.peek_token(),
            Some(Keyword(Kw::Pure | Kw::Impure | Kw::Function))
        ) {
            self.start_node(FunctionSpecification);
            self.opt_tokens([Keyword(Kw::Pure), Keyword(Kw::Impure)]);
            self.expect_token(Keyword(Kw::Function));
            true
        } else if self.opt_token(Keyword(Kw::Procedure)) {
            self.start_node(ProcedureSpecification);
            false
        } else {
            self.expect_tokens_err([
                Keyword(Kw::Pure),
                Keyword(Kw::Impure),
                Keyword(Kw::Function),
                Keyword(Kw::Procedure),
            ]);
            return;
        };
        self.designator();
        self.subprogram_header();
        if self.opt_token(Keyword(Kw::Parameter)) {
            self.subprogram_params_with_parens();
        }
        if self.next_is(LeftPar) {
            self.subprogram_params_with_parens();
        }
        if is_function {
            self.expect_kw(Kw::Return);
            self.type_mark();
        }
        self.end_node();
    }

    fn subprogram_params_with_parens(&mut self) {
        self.expect_token(LeftPar);
        self.interface_list();
        self.expect_token(RightPar);
    }

    pub fn subprogram_header(&mut self) {
        self.opt_generic_clause();
        self.opt_generic_map_aspect();
    }

    pub(crate) fn subprogram_declaration_or_body(&mut self) {
        let checkpoint = self.checkpoint();
        self.subprogram_specification();
        if self.opt_token(SemiColon) {
            self.start_node_at(checkpoint, SubprogramDeclaration);
            self.end_node();
            return;
        }
        self.start_node_at(checkpoint, SubprogramBody);
        self.expect_kw(Kw::Is);
        self.declarative_part();
        self.expect_kw(Kw::Begin);
        self.sequence_of_statements();
        self.expect_kw(Kw::End);
        self.subprogram_kind();
        self.opt_designator();
        self.expect_token(SemiColon);
        self.end_node();
    }

    pub fn subprogram_kind(&mut self) {
        self.opt_tokens([Keyword(Kw::Function), Keyword(Kw::Procedure)]);
    }
}
