// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::util::LookaheadError;
use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind::*;
use crate::tokens::Keyword as Kw;
use crate::tokens::TokenKind::*;
use crate::tokens::TokenStream;

fn is_start_of_attribute_name<T: TokenStream>(parser: &mut Parser<T>) -> bool {
    // Checking for `LeftSquare || Tick` will result in ambiguities with other grammar rules where a signature is possible right after a name.
    // Those rules can be `alias_declaration` (LRM §6.6.1) and `subprogram_instantiation_declaration` (LRM §4.4).
    // By checking whether the closing square bracket is followed by a `Tick` this ambiguity is resolved
    match parser.peek_token() {
        Some(Tick) => true,
        Some(LeftSquare) => {
            let mut idx = 1;
            let mut bracket_count = 1;

            while bracket_count > 0 {
                match parser.peek_nth_token(idx) {
                    Some(LeftSquare) => bracket_count += 1,
                    Some(RightSquare) => bracket_count -= 1,
                    Some(_) => {}
                    None => {
                        return false;
                    }
                }

                idx += 1;
            }

            parser.next_nth_is(Tick, idx)
        }
        Some(_) | None => false,
    }
}

impl<T: TokenStream> Parser<T> {
    pub fn name(&mut self) {
        self.name_bounded(usize::MAX);
    }

    pub(crate) fn name_bounded(&mut self, max_index: usize) {
        // (Based on) LRM §8.1
        // The LRM grammar rules for names were transformed to avoid left recursion.

        // In contrast to the LRM, this parsing routine is greedy. Meaning, it will consume trailing parenthesized
        // expressions even if the belong to an outer grammar rule!
        self.start_node(Name);

        if self.next_is(LtLt) {
            self.external_name();
        } else {
            self.expect_one_of_tokens([Identifier, StringLiteral, CharacterLiteral]);
        }

        self.opt_name_tail_bounded(max_index);
        self.end_node();
    }

    pub fn type_mark(&mut self) {
        self.name()
    }

    pub(crate) fn opt_designator(&mut self) {
        self.opt_tokens([Identifier, StringLiteral]);
    }

    pub(crate) fn designator(&mut self) {
        // TODO: That designator is not fully LRM compliant
        self.expect_one_of_tokens([Identifier, StringLiteral, CharacterLiteral]);
    }

    pub(crate) fn opt_label(&mut self) {
        if self.next_is(Identifier) && self.next_nth_is(Colon, 1) {
            self.start_node(Label);
            self.skip_n(2);
            self.end_node();
        }
    }

    pub(crate) fn name_list(&mut self) {
        self.start_node(NameList);
        self.separated_list(Parser::name, Comma);
        self.end_node();
    }

    fn suffix(&mut self) {
        // LRM §8.3
        // suffix ::= identifier | string_literal | character_literal | `all` ;
        self.expect_one_of_tokens([
            Identifier,
            StringLiteral,
            CharacterLiteral,
            Keyword(Kw::All),
        ]);
    }

    fn opt_name_tail_bounded(&mut self, max_index: usize) -> bool {
        // name             ::= prefix [ name_tail ] ;
        // name_tail        ::= selected_name | attribute_name | indexed_name | slice_name | function_name ;
        // selected_name    ::= `.` suffix [ name_tail ] ;
        // attribute_name   ::= [ signature ] `'` identifier [ `(` expression `)` ] [ name_tail ] ;
        // function_name    ::= `(` association_list `)` [ name_tail ] ;
        // indexed_name     ::= `(` expression { `,` expression } `)` [ name_tail ] ;
        // slice_name       ::= `(` discrete_range `)` [ name_tail ] ;

        if self.next_is(Dot) {
            self.start_node(SelectedName);
            self.expect_token(Dot);
            self.suffix();
            self.end_node();
            self.opt_name_tail_bounded(max_index)
        } else if self.next_is(LeftPar) {
            // Instead of trying to differentiate between `subtype_indication`, `association_list`, a list of `expression`s and a `discrete_range`
            // put all tokens inside the parenthesis in a `RawTokens` node.
            let end_index_opt =
                match self.lookahead_max_token_index_skip_n(max_index, 1, [RightPar]) {
                    Ok((_, end_index)) => Some(end_index),
                    Err((LookaheadError::MaxIndexReached, _)) => None,
                    // Skip parsing of the parenthesized group, if EOF is reached
                    Err((LookaheadError::Eof, _)) => None,
                    // This error is only possible, when a `RightPar` is found before any token in `kinds`.
                    // Since `RightPar` is in `kinds` that's not possible!
                    Err((LookaheadError::TokenKindNotFound, _)) => unreachable!(),
                };

            if let Some(end_index) = end_index_opt {
                self.start_node(RawTokens);
                self.expect_token(LeftPar);
                self.skip_to(end_index);
                self.expect_token(RightPar);
                self.end_node();

                self.opt_name_tail_bounded(max_index)
            } else {
                false
            }
        } else if is_start_of_attribute_name(self) {
            self.start_node(AttributeName);
            if self.next_is(LeftSquare) {
                self.signature();
            }
            self.expect_token(Tick);

            // Enable qualified expressions
            if !self.next_is(LeftPar) {
                // `range` is a keyword, but may appear as an `attribute_name`
                if !self.opt_identifier() {
                    self.expect_kw(Kw::Range);
                }
            }

            if self.next_is(LeftPar) {
                let end_index_opt = match self.lookahead_max_token_index(max_index, [RightPar]) {
                    Ok((_, end_index)) => Some(end_index),
                    Err((LookaheadError::MaxIndexReached, _)) => None,
                    // Skip parsing of the parenthesized group, if EOF is reached
                    Err((LookaheadError::Eof, _)) => None,
                    // This error is only possible, when a `RightPar` is found before any token in `kinds`.
                    // Since `RightPar` is in `kinds` that's not possible!
                    Err((LookaheadError::TokenKindNotFound, _)) => unreachable!(),
                };
                if let Some(end_index) = end_index_opt {
                    self.start_node(ParenthesizedExpressionOrAggregate);
                    self.expect_token(LeftPar);
                    self.association_list();
                    self.expect_token(RightPar);
                    self.end_node();
                }
                // TODO: Error
            }
            self.end_node();
            self.opt_name_tail_bounded(max_index)
        } else {
            false
        }
    }

    pub fn external_name(&mut self) {
        // LRM §8.7
        let checkpoint = self.checkpoint();
        self.expect_token(LtLt);

        let tok = self.expect_one_of_tokens([
            Keyword(Kw::Constant),
            Keyword(Kw::Signal),
            Keyword(Kw::Variable),
        ]);
        match tok {
            Some(Keyword(Kw::Signal)) => self.start_node_at(checkpoint, ExternalSignalName),
            Some(Keyword(Kw::Variable)) => self.start_node_at(checkpoint, ExternalVariableName),
            _ => self.start_node_at(checkpoint, ExternalConstantName),
        }
        self.external_pathname();
        self.expect_token(Colon);
        self.subtype_indication();

        self.expect_token(GtGt);
        self.end_node();
    }

    fn external_pathname(&mut self) {
        // LRM §8.7
        match_next_token!(self,
        CommAt => {
            self.start_node(PackagePathname);
            self.expect_token(CommAt);
            self.identifier();
            self.expect_token(Dot);
            self.identifier();
            self.expect_token(Dot);
            self.identifier();
            while self.opt_token(Dot) {
                self.identifier();
            }
        },
        Dot => {
            self.start_node(AbsolutePathname);
            self.expect_token(Dot);
            self.partial_pathname();
        },
        Circ, Identifier => {
            self.start_node(RelativePathname);
            while self.opt_token(Circ) {
                self.expect_token(Dot);
            }
            self.partial_pathname();
        });
        self.end_node();
    }

    fn partial_pathname(&mut self) {
        // LRM §8.7
        // partial_pathname ::= { identifier [ `(` expression `)` ] `.` } identifier ;
        self.start_node(PartialPathname);
        self.identifier();
        loop {
            if self.next_is(LeftPar) {
                self.start_node(ParenthesizedExpressionOrAggregate);
                self.expect_token(LeftPar);
                self.expression();
                self.expect_token(RightPar);
                self.end_node();
                self.expect_token(Dot);
            } else if !self.opt_token(Dot) {
                break;
            }
            self.identifier();
        }
        self.end_node();
    }

    pub fn choices(&mut self) {
        self.start_node(Choices);
        self.separated_list(Parser::choice, Bar);
        self.end_node();
    }

    pub fn choice(&mut self) {
        if self.opt_token(Keyword(Kw::Others)) {
            return;
        }
        let checkpoint = self.checkpoint();
        self.expression(); // TODO: can also be discrete range
        if self.next_is_one_of([Keyword(Kw::To), Keyword(Kw::Downto)]) {
            self.skip();
            self.start_node_at(checkpoint, RangeExpression);
            self.expression();
            self.end_node();
        }
    }
}
