// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind;
use crate::syntax::node_kind::NodeKind::*;
use crate::tokens::token_kind::Keyword as Kw;
use crate::tokens::TokenKind::*;
use crate::tokens::TokenStream;

impl<T: TokenStream> Parser<T> {
    fn any_sequential_statement(
        &mut self,
        kind: NodeKind,
        statement_inner: impl FnOnce(&mut Parser<T>),
    ) {
        self.start_node(kind);
        self.opt_label();
        statement_inner(self);
        self.expect_token(SemiColon);
        self.end_node();
    }

    pub fn wait_statement(&mut self) {
        self.any_sequential_statement(WaitStatement, Parser::wait_statement_inner);
    }

    fn wait_statement_inner(&mut self) {
        self.expect_kw(Kw::Wait);
        if self.next_is(Keyword(Kw::On)) {
            self.start_node(SensitivityClause);
            self.skip();
            self.name_list();
            self.end_node();
        }
        if self.next_is(Keyword(Kw::Until)) {
            self.start_node(ConditionClause);
            self.skip();
            self.expression();
            self.end_node();
        }
        if self.next_is(Keyword(Kw::For)) {
            self.start_node(TimeoutClause);
            self.skip();
            self.expression();
            self.end_node();
        }
    }

    pub fn assert_statement(&mut self) {
        self.any_sequential_statement(AssertionStatement, Parser::assert_statement_inner);
    }

    fn assert_statement_inner(&mut self) {
        self.expect_kw(Kw::Assert);
        self.condition();
        if self.next_is(Keyword(Kw::Report)) {
            self.skip();
            self.expression();
        }
        if self.next_is(Keyword(Kw::Severity)) {
            self.skip();
            self.expression();
        }
    }

    pub fn report_statement(&mut self) {
        self.any_sequential_statement(ReportStatement, Parser::report_statement_inner);
    }

    fn report_statement_inner(&mut self) {
        self.expect_kw(Kw::Report);
        self.expression();
        if self.next_is(Keyword(Kw::Severity)) {
            self.skip();
            self.expression();
        }
    }

    fn next_or_exit_statement(&mut self, kw: crate::tokens::token_kind::Keyword, kind: NodeKind) {
        self.any_sequential_statement(kind, |parser| parser.next_or_exit_statement_inner(kw));
    }

    fn next_or_exit_statement_inner(&mut self, kw: crate::tokens::token_kind::Keyword) {
        self.expect_kw(kw);
        self.opt_identifier();
        if self.next_is(Keyword(Kw::When)) {
            self.skip();
            self.expression();
        }
    }

    pub fn next_statement(&mut self) {
        self.next_or_exit_statement(Kw::Next, NextStatement);
    }

    pub fn exit_statement(&mut self) {
        self.next_or_exit_statement(Kw::Exit, ExitStatement);
    }

    pub fn return_statement(&mut self) {
        self.any_sequential_statement(ReturnStatement, Parser::return_statement_inner);
    }

    fn return_statement_inner(&mut self) {
        self.expect_kw(Kw::Return);
        if !self.next_is(SemiColon) {
            self.expression();
        }
    }

    pub fn null_statement(&mut self) {
        self.any_sequential_statement(NullStatement, Parser::null_statement_inner);
    }

    fn null_statement_inner(&mut self) {
        self.expect_kw(Kw::Null);
    }

    fn if_statement_inner(&mut self) {
        self.expect_kw(Kw::If);
        self.condition();
        self.expect_kw(Kw::Then);
        self.sequence_of_statements();
        while self.next_is(Keyword(Kw::Elsif)) {
            self.start_node(IfStatementElsif);
            self.skip();
            self.condition();
            self.expect_kw(Kw::Then);
            self.sequence_of_statements();
            self.end_node();
        }
        if self.next_is(Keyword(Kw::Else)) {
            self.start_node(IfStatementElse);
            self.skip();
            self.sequence_of_statements();
            self.end_node();
        }
        self.expect_tokens([Keyword(Kw::End), Keyword(Kw::If)]);
        self.opt_identifier();
    }

    pub fn if_statement(&mut self) {
        self.any_sequential_statement(IfStatement, Parser::if_statement_inner);
    }

    fn case_statement_inner(&mut self) {
        self.expect_kw(Kw::Case);
        self.opt_token(Que);
        self.expression();
        self.expect_kw(Kw::Is);
        while self.next_is(Keyword(Kw::When)) {
            self.case_statement_alternative();
        }
        self.expect_tokens([Keyword(Kw::End), Keyword(Kw::Case)]);
        self.opt_token(Que);
        self.opt_label();
    }

    pub fn case_statement(&mut self) {
        self.any_sequential_statement(CaseStatement, Parser::case_statement_inner);
    }

    pub fn case_statement_alternative(&mut self) {
        self.start_node(CaseStatementAlternative);
        self.expect_kw(Kw::When);
        self.choices();
        self.expect_token(RightArrow);
        self.sequence_of_statements();
        self.end_node();
    }

    pub fn aggregate(&mut self) {
        self.start_node(Aggregate);
        self.aggregate_inner();
        self.end_node();
    }

    pub(crate) fn aggregate_inner(&mut self) {
        self.expect_token(LeftPar);
        self.separated_list(Parser::element_association, Comma);
        self.expect_token(RightPar);
    }

    pub fn element_association(&mut self) {
        let has_choices = matches!(
            self.lookahead_max_token_index(usize::MAX, [RightArrow, Comma]),
            Ok((RightArrow, _))
        );
        if has_choices {
            self.start_node(ElementAssociation);
            self.choices();
            self.expect_token(RightArrow)
        }
        self.expression();
        if has_choices {
            self.end_node();
        }
    }

    fn loop_statement_inner(&mut self) {
        self.opt_iteration_scheme();
        self.expect_kw(Kw::Loop);
        self.sequence_of_statements();
        self.expect_tokens([Keyword(Kw::End), Keyword(Kw::Loop)]);
        self.opt_identifier();
    }

    pub fn loop_statement(&mut self) {
        self.any_sequential_statement(LoopStatement, Parser::loop_statement_inner)
    }

    fn opt_iteration_scheme(&mut self) {
        if self.next_is(Keyword(Kw::While)) {
            self.start_node(WhileIterationScheme);
            self.skip();
            self.condition();
            self.end_node();
        } else if self.next_is(Keyword(Kw::For)) {
            self.start_node(ForIterationScheme);
            self.skip();
            self.parameter_specification();
            self.end_node();
        }
    }

    pub fn iteration_scheme(&mut self) {
        if !self.next_is_one_of([Keyword(Kw::While), Keyword(Kw::For)]) {
            self.expect_tokens_err([Keyword(Kw::While), Keyword(Kw::For)]);
            return;
        }
        self.opt_iteration_scheme();
    }

    pub fn sequence_of_statements(&mut self) {
        self.start_node(SequentialStatements);
        loop {
            match self.peek_token() {
                None | Some(Keyword(Kw::End | Kw::Else | Kw::Elsif | Kw::When)) => break,
                _ => self.sequential_statement(),
            }
        }
        self.end_node();
    }

    fn opt_force_mode(&mut self) {
        self.opt_tokens([Keyword(Kw::In), Keyword(Kw::Out)]);
    }

    pub fn selected_expressions(&mut self) {
        self.start_node(SelectedExpressions);
        self.separated_list(Parser::selected_expression, Comma);
        self.end_node();
    }

    fn selected_expression(&mut self) {
        self.start_node(SelectedExpressionItem);
        self.expression();
        self.expect_kw(Kw::When);
        self.choices();
        self.end_node();
    }

    pub fn sequential_statement(&mut self) {
        let checkpoint = self.checkpoint();
        self.opt_label();
        match self.peek_token() {
            Some(Keyword(Kw::Wait)) => {
                self.start_node_at(checkpoint, WaitStatement);
                self.wait_statement_inner()
            }
            Some(Keyword(Kw::Assert)) => {
                self.start_node_at(checkpoint, AssertionStatement);
                self.assert_statement_inner()
            }
            Some(Keyword(Kw::Report)) => {
                self.start_node_at(checkpoint, ReportStatement);
                self.report_statement_inner()
            }
            Some(Keyword(Kw::If)) => {
                self.start_node_at(checkpoint, IfStatement);
                self.if_statement_inner()
            }
            Some(Keyword(Kw::Case)) => {
                self.start_node_at(checkpoint, CaseStatement);
                self.case_statement_inner()
            }
            Some(Keyword(Kw::For | Kw::Loop | Kw::While)) => {
                self.start_node_at(checkpoint, LoopStatement);
                self.loop_statement_inner()
            }
            Some(Keyword(Kw::Next)) => {
                self.start_node_at(checkpoint, NextStatement);
                self.next_or_exit_statement_inner(Kw::Next);
            }
            Some(Keyword(Kw::Exit)) => {
                self.start_node_at(checkpoint, ExitStatement);
                self.next_or_exit_statement_inner(Kw::Exit);
            }
            Some(Keyword(Kw::Return)) => {
                self.start_node_at(checkpoint, ReturnStatement);
                self.next_or_exit_statement_inner(Kw::Return);
            }
            Some(Keyword(Kw::Null)) => {
                self.start_node_at(checkpoint, NullStatement);
                self.null_statement_inner()
            }
            Some(Keyword(Kw::With)) => {
                self.skip();
                self.expression();
                self.expect_kw(Kw::Select);
                self.opt_token(Que);
                self.target();
                match self.peek_token() {
                    Some(LTE) => {
                        if self.next_nth_is(Keyword(Kw::Force), 1) {
                            self.start_node_at(checkpoint, SelectedForceAssignment);
                            self.skip_n(2);
                            self.opt_force_mode();
                            self.selected_expressions();
                        } else {
                            self.start_node_at(checkpoint, SelectedWaveformAssignment);
                            self.skip();
                            self.opt_delay_mechanism();
                            self.selected_waveforms();
                        }
                    }
                    Some(ColonEq) => {
                        self.start_node_at(checkpoint, SelectedVariableAssignment);
                        self.skip();
                        self.selected_expressions();
                    }
                    _ => {
                        todo!()
                    }
                }
            }
            Some(Identifier | LeftPar | LtLt) => {
                self.target();
                match self.peek_token() {
                    Some(ColonEq) => {
                        self.skip();
                        self.expression();
                        if self.next_is(Keyword(Kw::When)) {
                            self.start_node_at(checkpoint, ConditionalVariableAssignment);
                            self.skip();
                            self.condition();
                            self.conditional_else(
                                Parser::expression,
                                ConditionalElseWhenExpression,
                                ConditionalElseItem,
                            );
                        } else {
                            self.start_node_at(checkpoint, SimpleVariableAssignment);
                        }
                    }
                    Some(LTE) => {
                        if self.next_nth_is(Keyword(Kw::Force), 1) {
                            self.skip_n(2);
                            self.opt_force_mode();
                            self.expression();
                            if self.next_is(Keyword(Kw::When)) {
                                self.start_node_at(checkpoint, ConditionalForceAssignment);
                                self.skip();
                                self.condition();
                                self.conditional_else(
                                    Parser::waveform,
                                    ConditionalElseWhenExpression,
                                    ConditionalElseItem,
                                );
                            } else {
                                self.start_node_at(checkpoint, SimpleForceAssignment);
                            }
                        } else if self.next_nth_is(Keyword(Kw::Release), 1) {
                            self.start_node_at(checkpoint, SimpleReleaseAssignment);
                            self.skip_n(2);
                            self.opt_force_mode();
                        } else {
                            self.skip();
                            self.opt_delay_mechanism();
                            self.waveform();
                            if self.next_is(Keyword(Kw::When)) {
                                self.start_node_at(checkpoint, ConditionalWaveformAssignment);
                                self.skip();
                                self.condition();
                                self.conditional_else(
                                    Parser::waveform,
                                    ConditionalWaveformElseWhenExpression,
                                    ConditionalWaveformElseItem,
                                );
                            } else {
                                self.start_node_at(checkpoint, SimpleWaveformAssignment);
                            }
                        }
                    }
                    Some(SemiColon) => {
                        self.start_node_at(checkpoint, ProcedureCallStatement);
                    }
                    _ => todo!(),
                }
            }
            _ => todo!(),
        }
        self.expect_token(SemiColon);
        self.end_node();
    }

    fn conditional_else(
        &mut self,
        item: impl Fn(&mut Parser<T>),
        else_when_node: NodeKind,
        else_node: NodeKind,
    ) {
        while self.next_is(Keyword(Kw::Else)) {
            let local_checkpoint = self.checkpoint();
            self.skip();
            item(self);
            if self.next_is(Keyword(Kw::When)) {
                self.start_node_at(local_checkpoint, else_when_node);
                self.skip();
                self.condition();
                self.end_node();
            } else {
                self.start_node_at(local_checkpoint, else_node);
                self.end_node();
                break;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils::check;
    use crate::parser::Parser;

    #[test]
    fn simple_wait_statement() {
        check(
            Parser::wait_statement,
            "wait;",
            "\
WaitStatement
  Keyword(Wait)
  SemiColon
        ",
        );
    }

    #[test]
    fn simple_wait_statement_with_label() {
        check(
            Parser::wait_statement,
            "foo: wait;",
            "\
WaitStatement
  Label
    Identifier 'foo'
    Colon
  Keyword(Wait)
  SemiColon
        ",
        );
    }

    #[test]
    fn wait_statement_with_sensitivity_list() {
        check(
            Parser::wait_statement,
            "wait on foo, bar;",
            "\
WaitStatement
  Keyword(Wait)
  SensitivityClause
    Keyword(On)
    NameList
      Name
        Identifier 'foo'
      Comma
      Name
        Identifier 'bar'
  SemiColon
        ",
        );
    }

    #[test]
    fn wait_statement_with_condition() {
        check(
            Parser::wait_statement,
            "wait until a = b;",
            "\
WaitStatement
  Keyword(Wait)
  ConditionClause
    Keyword(Until)
    BinaryExpression
      NameExpression
        Name
          Identifier 'a'
      EQ
      NameExpression
        Name
          Identifier 'b'
  SemiColon
        ",
        );
    }

    #[test]
    fn wait_statement_with_timeout() {
        check(
            Parser::wait_statement,
            "wait for 2 ns;",
            "\
WaitStatement
  Keyword(Wait)
  TimeoutClause
    Keyword(For)
    PhysicalLiteral
      AbstractLiteral '2'
      Name
        Identifier 'ns'
  SemiColon
        ",
        );
    }

    #[test]
    fn wait_statement_with_all_parts() {
        check(
            Parser::wait_statement,
            "wait on foo until bar for 2 ns;",
            "\
WaitStatement
  Keyword(Wait)
  SensitivityClause
    Keyword(On)
    NameList
      Name
        Identifier 'foo'
  ConditionClause
    Keyword(Until)
    NameExpression
      Name
        Identifier 'bar'
  TimeoutClause
    Keyword(For)
    PhysicalLiteral
      AbstractLiteral '2'
      Name
        Identifier 'ns'
  SemiColon
        ",
        );
    }

    #[test]
    fn simple_assert() {
        check(
            Parser::assert_statement,
            "assert false;",
            "\
AssertionStatement
  Keyword(Assert)
  NameExpression
    Name
      Identifier 'false'
  SemiColon",
        )
    }

    #[test]
    fn full_assert() {
        check(
            Parser::assert_statement,
            "assert false report \"message\" severity error;",
            "\
AssertionStatement
  Keyword(Assert)
  NameExpression
    Name
      Identifier 'false'
  Keyword(Report)
  LiteralExpression
    StringLiteral '\"message\"'
  Keyword(Severity)
  NameExpression
    Name
      Identifier 'error'
  SemiColon",
        )
    }

    #[test]
    fn report_statement() {
        check(
            Parser::report_statement,
            "report \"message\" severity error;",
            "\
ReportStatement
  Keyword(Report)
  LiteralExpression
    StringLiteral '\"message\"'
  Keyword(Severity)
  NameExpression
    Name
      Identifier 'error'
  SemiColon",
        )
    }

    #[test]
    fn next_statement() {
        check(
            Parser::next_statement,
            "next;",
            "\
NextStatement
  Keyword(Next)
  SemiColon
        ",
        );
    }

    #[test]
    fn next_statement_loop_label() {
        check(
            Parser::next_statement,
            "next foo;",
            "\
NextStatement
  Keyword(Next)
  Identifier 'foo'
  SemiColon
        ",
        );
    }

    #[test]
    fn next_statement_condition() {
        check(
            Parser::next_statement,
            "next when condition;",
            "\
NextStatement
  Keyword(Next)
  Keyword(When)
  NameExpression
    Name
      Identifier 'condition'
  SemiColon
        ",
        );
    }

    #[test]
    fn next_statement_loop_label_condition() {
        check(
            Parser::next_statement,
            "next foo when condition;",
            "\
NextStatement
  Keyword(Next)
  Identifier 'foo'
  Keyword(When)
  NameExpression
    Name
      Identifier 'condition'
  SemiColon
        ",
        );
    }

    #[test]
    fn exit_statement() {
        check(
            Parser::exit_statement,
            "exit;",
            "\
ExitStatement
  Keyword(Exit)
  SemiColon
        ",
        );
    }

    #[test]
    fn exit_statement_loop_label() {
        check(
            Parser::exit_statement,
            "exit foo;",
            "\
ExitStatement
  Keyword(Exit)
  Identifier 'foo'
  SemiColon
        ",
        );
    }

    #[test]
    fn exit_statement_condition() {
        check(
            Parser::exit_statement,
            "exit when condition;",
            "\
ExitStatement
  Keyword(Exit)
  Keyword(When)
  NameExpression
    Name
      Identifier 'condition'
  SemiColon
        ",
        );
    }

    #[test]
    fn exit_statement_loop_label_condition() {
        check(
            Parser::exit_statement,
            "exit foo when condition;",
            "\
ExitStatement
  Keyword(Exit)
  Identifier 'foo'
  Keyword(When)
  NameExpression
    Name
      Identifier 'condition'
  SemiColon
        ",
        );
    }

    #[test]
    fn return_statement() {
        check(
            Parser::return_statement,
            "return;",
            "\
ReturnStatement
  Keyword(Return)
  SemiColon
        ",
        );
    }

    #[test]
    fn return_statement_expression() {
        check(
            Parser::return_statement,
            "return 1 + 2;",
            "\
ReturnStatement
  Keyword(Return)
  BinaryExpression
    LiteralExpression
      AbstractLiteral '1'
    Plus
    LiteralExpression
      AbstractLiteral '2'
  SemiColon
        ",
        );
    }

    #[test]
    fn null_statement() {
        check(
            Parser::null_statement,
            "null;",
            "\
NullStatement
  Keyword(Null)
  SemiColon
        ",
        );
    }

    #[test]
    fn empty_if_statement() {
        check(
            Parser::if_statement,
            "\
if cond = true then
end if;",
            "\
IfStatement
  Keyword(If)
  BinaryExpression
    NameExpression
      Name
        Identifier 'cond'
    EQ
    NameExpression
      Name
        Identifier 'true'
  Keyword(Then)
  SequentialStatements
  Keyword(End)
  Keyword(If)
  SemiColon
        ",
        );
    }

    #[test]
    fn simple_if_statement() {
        check(
            Parser::if_statement,
            "\
if cond = true then
   foo(1,2);
   x := 1;
end if;",
            "\
IfStatement
  Keyword(If)
  BinaryExpression
    NameExpression
      Name
        Identifier 'cond'
    EQ
    NameExpression
      Name
        Identifier 'true'
  Keyword(Then)
  SequentialStatements
    ProcedureCallStatement
      NameTarget
        Name
          Identifier 'foo'
          RawTokens
            LeftPar
            AbstractLiteral '1'
            Comma
            AbstractLiteral '2'
            RightPar
      SemiColon
    SimpleVariableAssignment
      NameTarget
        Name
          Identifier 'x'
      ColonEq
      LiteralExpression
        AbstractLiteral '1'
      SemiColon
  Keyword(End)
  Keyword(If)
  SemiColon
        ",
        );
    }

    #[test]
    fn labeled_if_statement() {
        check(
            Parser::if_statement,
            "\
mylabel: if cond = true then
   foo(1,2);
   x := 1;
end if mylabel;",
            "\
IfStatement
  Label
    Identifier 'mylabel'
    Colon
  Keyword(If)
  BinaryExpression
    NameExpression
      Name
        Identifier 'cond'
    EQ
    NameExpression
      Name
        Identifier 'true'
  Keyword(Then)
  SequentialStatements
    ProcedureCallStatement
      NameTarget
        Name
          Identifier 'foo'
          RawTokens
            LeftPar
            AbstractLiteral '1'
            Comma
            AbstractLiteral '2'
            RightPar
      SemiColon
    SimpleVariableAssignment
      NameTarget
        Name
          Identifier 'x'
      ColonEq
      LiteralExpression
        AbstractLiteral '1'
      SemiColon
  Keyword(End)
  Keyword(If)
  Identifier 'mylabel'
  SemiColon
        ",
        );
    }

    #[test]
    fn if_else_statement() {
        check(
            Parser::if_statement,
            "\
if cond = true then
   foo(1,2);
else
   x := 1;
end if;",
            "\
IfStatement
  Keyword(If)
  BinaryExpression
    NameExpression
      Name
        Identifier 'cond'
    EQ
    NameExpression
      Name
        Identifier 'true'
  Keyword(Then)
  SequentialStatements
    ProcedureCallStatement
      NameTarget
        Name
          Identifier 'foo'
          RawTokens
            LeftPar
            AbstractLiteral '1'
            Comma
            AbstractLiteral '2'
            RightPar
      SemiColon
  IfStatementElse
    Keyword(Else)
    SequentialStatements
      SimpleVariableAssignment
        NameTarget
          Name
            Identifier 'x'
        ColonEq
        LiteralExpression
          AbstractLiteral '1'
        SemiColon
  Keyword(End)
  Keyword(If)
  SemiColon
        ",
        );
    }

    #[test]
    fn labeled_if_else_statement() {
        check(
            Parser::if_statement,
            "\
mylabel: if cond = true then
   foo(1,2);
else
   x := 1;
end if mylabel;",
            "\
IfStatement
  Label
    Identifier 'mylabel'
    Colon
  Keyword(If)
  BinaryExpression
    NameExpression
      Name
        Identifier 'cond'
    EQ
    NameExpression
      Name
        Identifier 'true'
  Keyword(Then)
  SequentialStatements
    ProcedureCallStatement
      NameTarget
        Name
          Identifier 'foo'
          RawTokens
            LeftPar
            AbstractLiteral '1'
            Comma
            AbstractLiteral '2'
            RightPar
      SemiColon
  IfStatementElse
    Keyword(Else)
    SequentialStatements
      SimpleVariableAssignment
        NameTarget
          Name
            Identifier 'x'
        ColonEq
        LiteralExpression
          AbstractLiteral '1'
        SemiColon
  Keyword(End)
  Keyword(If)
  Identifier 'mylabel'
  SemiColon
        ",
        );
    }

    #[test]
    fn if_elsif_else_statement() {
        check(
            Parser::if_statement,
            "\
if cond = true then
   foo(1,2);
elsif cond2 = false then
   y := 2;
else
   x := 1;
end if;",
            "\
IfStatement
  Keyword(If)
  BinaryExpression
    NameExpression
      Name
        Identifier 'cond'
    EQ
    NameExpression
      Name
        Identifier 'true'
  Keyword(Then)
  SequentialStatements
    ProcedureCallStatement
      NameTarget
        Name
          Identifier 'foo'
          RawTokens
            LeftPar
            AbstractLiteral '1'
            Comma
            AbstractLiteral '2'
            RightPar
      SemiColon
  IfStatementElsif
    Keyword(Elsif)
    BinaryExpression
      NameExpression
        Name
          Identifier 'cond2'
      EQ
      NameExpression
        Name
          Identifier 'false'
    Keyword(Then)
    SequentialStatements
      SimpleVariableAssignment
        NameTarget
          Name
            Identifier 'y'
        ColonEq
        LiteralExpression
          AbstractLiteral '2'
        SemiColon
  IfStatementElse
    Keyword(Else)
    SequentialStatements
      SimpleVariableAssignment
        NameTarget
          Name
            Identifier 'x'
        ColonEq
        LiteralExpression
          AbstractLiteral '1'
        SemiColon
  Keyword(End)
  Keyword(If)
  SemiColon
        ",
        );
    }

    #[test]
    fn labeled_if_elsif_else_statement() {
        check(
            Parser::if_statement,
            "\
mylabel: if cond = true then
   foo(1,2);
elsif cond2 = false then
   y := 2;
else
   x := 1;
end if mylabel;",
            "\
IfStatement
  Label
    Identifier 'mylabel'
    Colon
  Keyword(If)
  BinaryExpression
    NameExpression
      Name
        Identifier 'cond'
    EQ
    NameExpression
      Name
        Identifier 'true'
  Keyword(Then)
  SequentialStatements
    ProcedureCallStatement
      NameTarget
        Name
          Identifier 'foo'
          RawTokens
            LeftPar
            AbstractLiteral '1'
            Comma
            AbstractLiteral '2'
            RightPar
      SemiColon
  IfStatementElsif
    Keyword(Elsif)
    BinaryExpression
      NameExpression
        Name
          Identifier 'cond2'
      EQ
      NameExpression
        Name
          Identifier 'false'
    Keyword(Then)
    SequentialStatements
      SimpleVariableAssignment
        NameTarget
          Name
            Identifier 'y'
        ColonEq
        LiteralExpression
          AbstractLiteral '2'
        SemiColon
  IfStatementElse
    Keyword(Else)
    SequentialStatements
      SimpleVariableAssignment
        NameTarget
          Name
            Identifier 'x'
        ColonEq
        LiteralExpression
          AbstractLiteral '1'
        SemiColon
  Keyword(End)
  Keyword(If)
  Identifier 'mylabel'
  SemiColon
        ",
        );
    }

    #[test]
    fn case_statement() {
        check(
            Parser::case_statement,
            "\
case foo(1) is
  when 1 | 2 =>
    stmt1;
    stmt2;
  when others =>
    stmt3;
    stmt4;
end case;",
            "\
CaseStatement
  Keyword(Case)
  NameExpression
    Name
      Identifier 'foo'
      RawTokens
        LeftPar
        AbstractLiteral '1'
        RightPar
  Keyword(Is)
  CaseStatementAlternative
    Keyword(When)
    Choices
      LiteralExpression
        AbstractLiteral '1'
      Bar
      LiteralExpression
        AbstractLiteral '2'
    RightArrow
    SequentialStatements
      ProcedureCallStatement
        NameTarget
          Name
            Identifier 'stmt1'
        SemiColon
      ProcedureCallStatement
        NameTarget
          Name
            Identifier 'stmt2'
        SemiColon
  CaseStatementAlternative
    Keyword(When)
    Choices
      Keyword(Others)
    RightArrow
    SequentialStatements
      ProcedureCallStatement
        NameTarget
          Name
            Identifier 'stmt3'
        SemiColon
      ProcedureCallStatement
        NameTarget
          Name
            Identifier 'stmt4'
        SemiColon
  Keyword(End)
  Keyword(Case)
  SemiColon
            ",
        );
    }

    #[test]
    fn matching_case_statement() {
        check(
            Parser::case_statement,
            "\
case? foo(1) is
  when others => null;
end case?;",
            "\
CaseStatement
  Keyword(Case)
  Que
  NameExpression
    Name
      Identifier 'foo'
      RawTokens
        LeftPar
        AbstractLiteral '1'
        RightPar
  Keyword(Is)
  CaseStatementAlternative
    Keyword(When)
    Choices
      Keyword(Others)
    RightArrow
    SequentialStatements
      NullStatement
        Keyword(Null)
        SemiColon
  Keyword(End)
  Keyword(Case)
  Que
  SemiColon
            ",
        );
    }

    #[test]
    fn loop_statement() {
        check(
            Parser::loop_statement,
            "\
lbl: loop
  stmt1;
  stmt2;
end loop lbl;",
            "\
LoopStatement
  Label
    Identifier 'lbl'
    Colon
  Keyword(Loop)
  SequentialStatements
    ProcedureCallStatement
      NameTarget
        Name
          Identifier 'stmt1'
      SemiColon
    ProcedureCallStatement
      NameTarget
        Name
          Identifier 'stmt2'
      SemiColon
  Keyword(End)
  Keyword(Loop)
  Identifier 'lbl'
  SemiColon
            ",
        );
    }

    #[test]
    fn while_loop_statement() {
        check(
            Parser::loop_statement,
            "\
while foo = true loop
  stmt1;
  stmt2;
end loop;",
            "\
LoopStatement
  WhileIterationScheme
    Keyword(While)
    BinaryExpression
      NameExpression
        Name
          Identifier 'foo'
      EQ
      NameExpression
        Name
          Identifier 'true'
  Keyword(Loop)
  SequentialStatements
    ProcedureCallStatement
      NameTarget
        Name
          Identifier 'stmt1'
      SemiColon
    ProcedureCallStatement
      NameTarget
        Name
          Identifier 'stmt2'
      SemiColon
  Keyword(End)
  Keyword(Loop)
  SemiColon
            ",
        );
    }

    #[test]
    fn for_loop_statement() {
        check(
            Parser::loop_statement,
            "\
for idx in 0 to 3 loop
  stmt1;
  stmt2;
end loop;",
            "\
LoopStatement
  ForIterationScheme
    Keyword(For)
    ParameterSpecification
      Identifier 'idx'
      Keyword(In)
      RangeExpression
        LiteralExpression
          AbstractLiteral '0'
        Keyword(To)
        LiteralExpression
          AbstractLiteral '3'
  Keyword(Loop)
  SequentialStatements
    ProcedureCallStatement
      NameTarget
        Name
          Identifier 'stmt1'
      SemiColon
    ProcedureCallStatement
      NameTarget
        Name
          Identifier 'stmt2'
      SemiColon
  Keyword(End)
  Keyword(Loop)
  SemiColon
            ",
        );
    }

    #[test]
    fn simple_signal_assignment() {
        check(
            Parser::sequential_statement,
            "foo(0) <= bar(1,2) after 2 ns;",
            "\
SimpleWaveformAssignment
  NameTarget
    Name
      Identifier 'foo'
      RawTokens
        LeftPar
        AbstractLiteral '0'
        RightPar
  LTE
  WaveformElements
    WaveformElement
      NameExpression
        Name
          Identifier 'bar'
          RawTokens
            LeftPar
            AbstractLiteral '1'
            Comma
            AbstractLiteral '2'
            RightPar
      Keyword(After)
      PhysicalLiteral
        AbstractLiteral '2'
        Name
          Identifier 'ns'
  SemiColon
            ",
        );
    }

    #[test]
    fn simple_signal_force_assignment() {
        check(
            Parser::sequential_statement,
            "foo(0) <= force bar(1,2);",
            "\
SimpleForceAssignment
  NameTarget
    Name
      Identifier 'foo'
      RawTokens
        LeftPar
        AbstractLiteral '0'
        RightPar
  LTE
  Keyword(Force)
  NameExpression
    Name
      Identifier 'bar'
      RawTokens
        LeftPar
        AbstractLiteral '1'
        Comma
        AbstractLiteral '2'
        RightPar
  SemiColon",
        );
    }

    #[test]
    fn simple_signal_release_assignment() {
        check(
            Parser::sequential_statement,
            "foo(0) <= release;",
            "\
SimpleReleaseAssignment
  NameTarget
    Name
      Identifier 'foo'
      RawTokens
        LeftPar
        AbstractLiteral '0'
        RightPar
  LTE
  Keyword(Release)
  SemiColon",
        );
    }

    #[test]
    fn signal_assignment_external_name() {
        check(
            Parser::sequential_statement,
            "<< signal dut.foo : boolean  >> <= bar(1,2);",
            "\
SimpleWaveformAssignment
  NameTarget
    Name
      ExternalSignalName
        LtLt
        Keyword(Signal)
        RelativePathname
          PartialPathname
            Identifier 'dut'
            Dot
            Identifier 'foo'
        Colon
        Identifier 'boolean'
        GtGt
  LTE
  WaveformElements
    WaveformElement
      NameExpression
        Name
          Identifier 'bar'
          RawTokens
            LeftPar
            AbstractLiteral '1'
            Comma
            AbstractLiteral '2'
            RightPar
  SemiColon",
        );
    }

    #[test]
    fn simple_signal_assignment_delay_mechanism() {
        check(
            Parser::sequential_statement,
            "foo(0) <= transport bar(1,2);",
            "\
SimpleWaveformAssignment
  NameTarget
    Name
      Identifier 'foo'
      RawTokens
        LeftPar
        AbstractLiteral '0'
        RightPar
  LTE
  TransportDelayMechanism
    Keyword(Transport)
  WaveformElements
    WaveformElement
      NameExpression
        Name
          Identifier 'bar'
          RawTokens
            LeftPar
            AbstractLiteral '1'
            Comma
            AbstractLiteral '2'
            RightPar
  SemiColon",
        );
    }

    #[test]
    fn simple_variable_assignment() {
        check(
            Parser::sequential_statement,
            "foo(0) := bar(1,2);",
            "\
SimpleVariableAssignment
  NameTarget
    Name
      Identifier 'foo'
      RawTokens
        LeftPar
        AbstractLiteral '0'
        RightPar
  ColonEq
  NameExpression
    Name
      Identifier 'bar'
      RawTokens
        LeftPar
        AbstractLiteral '1'
        Comma
        AbstractLiteral '2'
        RightPar
  SemiColon",
        );
    }

    #[test]
    fn variable_assignment_external_name() {
        check(
            Parser::sequential_statement,
            "<< variable dut.foo : boolean >> := bar(1,2);",
            "\
SimpleVariableAssignment
  NameTarget
    Name
      ExternalVariableName
        LtLt
        Keyword(Variable)
        RelativePathname
          PartialPathname
            Identifier 'dut'
            Dot
            Identifier 'foo'
        Colon
        Identifier 'boolean'
        GtGt
  ColonEq
  NameExpression
    Name
      Identifier 'bar'
      RawTokens
        LeftPar
        AbstractLiteral '1'
        Comma
        AbstractLiteral '2'
        RightPar
  SemiColon
            ",
        );
    }

    #[test]
    fn simple_aggregate_variable_assignment() {
        check(
            Parser::sequential_statement,
            "(foo, 1 => bar) := bar;",
            "\
SimpleVariableAssignment
  AggregateTarget
    Aggregate
      LeftPar
      NameExpression
        Name
          Identifier 'foo'
      Comma
      ElementAssociation
        Choices
          LiteralExpression
            AbstractLiteral '1'
        RightArrow
        NameExpression
          Name
            Identifier 'bar'
      RightPar
  ColonEq
  NameExpression
    Name
      Identifier 'bar'
  SemiColon",
        );
    }

    #[test]
    fn labeled_aggregate_variable_assignment() {
        check(
            Parser::sequential_statement,
            "name: (foo, 1 => bar) := bar;",
            "\
SimpleVariableAssignment
  Label
    Identifier 'name'
    Colon
  AggregateTarget
    Aggregate
      LeftPar
      NameExpression
        Name
          Identifier 'foo'
      Comma
      ElementAssociation
        Choices
          LiteralExpression
            AbstractLiteral '1'
        RightArrow
        NameExpression
          Name
            Identifier 'bar'
      RightPar
  ColonEq
  NameExpression
    Name
      Identifier 'bar'
  SemiColon",
        );
    }

    #[test]
    fn labeled_simple_variable_assignment() {
        check(
            Parser::sequential_statement,
            "name: foo(0) := bar(1,2);",
            "\
SimpleVariableAssignment
  Label
    Identifier 'name'
    Colon
  NameTarget
    Name
      Identifier 'foo'
      RawTokens
        LeftPar
        AbstractLiteral '0'
        RightPar
  ColonEq
  NameExpression
    Name
      Identifier 'bar'
      RawTokens
        LeftPar
        AbstractLiteral '1'
        Comma
        AbstractLiteral '2'
        RightPar
  SemiColon
            ",
        );
    }

    #[test]
    fn selected_variable_assignment() {
        check(
            Parser::sequential_statement,
            "\
with x(0) + 1 select
   foo(0) := bar(1,2) when 0|1,
             def when others;
        ",
            "\
SelectedVariableAssignment
  Keyword(With)
  BinaryExpression
    NameExpression
      Name
        Identifier 'x'
        RawTokens
          LeftPar
          AbstractLiteral '0'
          RightPar
    Plus
    LiteralExpression
      AbstractLiteral '1'
  Keyword(Select)
  NameTarget
    Name
      Identifier 'foo'
      RawTokens
        LeftPar
        AbstractLiteral '0'
        RightPar
  ColonEq
  SelectedExpressions
    SelectedExpressionItem
      NameExpression
        Name
          Identifier 'bar'
          RawTokens
            LeftPar
            AbstractLiteral '1'
            Comma
            AbstractLiteral '2'
            RightPar
      Keyword(When)
      Choices
        LiteralExpression
          AbstractLiteral '0'
        Bar
        LiteralExpression
          AbstractLiteral '1'
    Comma
    SelectedExpressionItem
      NameExpression
        Name
          Identifier 'def'
      Keyword(When)
      Choices
        Keyword(Others)
  SemiColon
            ",
        );
    }

    #[test]
    fn conditional_variable_assignment() {
        check(
            Parser::sequential_statement,
            "\
foo(0) := bar(1,2) when cond = true;
        ",
            "\
ConditionalVariableAssignment
  NameTarget
    Name
      Identifier 'foo'
      RawTokens
        LeftPar
        AbstractLiteral '0'
        RightPar
  ColonEq
  NameExpression
    Name
      Identifier 'bar'
      RawTokens
        LeftPar
        AbstractLiteral '1'
        Comma
        AbstractLiteral '2'
        RightPar
  Keyword(When)
  BinaryExpression
    NameExpression
      Name
        Identifier 'cond'
    EQ
    NameExpression
      Name
        Identifier 'true'
  SemiColon",
        );
    }

    #[test]
    fn conditional_signal_force_assignment() {
        check(
            Parser::sequential_statement,
            "\
foo(0) <= force bar(1,2) when cond;
        ",
            "\
ConditionalForceAssignment
  NameTarget
    Name
      Identifier 'foo'
      RawTokens
        LeftPar
        AbstractLiteral '0'
        RightPar
  LTE
  Keyword(Force)
  NameExpression
    Name
      Identifier 'bar'
      RawTokens
        LeftPar
        AbstractLiteral '1'
        Comma
        AbstractLiteral '2'
        RightPar
  Keyword(When)
  NameExpression
    Name
      Identifier 'cond'
  SemiColon",
        );
    }

    #[test]
    fn conditional_variable_assignment_several() {
        check(
            Parser::sequential_statement,
            "\
foo(0) := bar(1,2) when cond = true else expr2 when cond2;
        ",
            "\
ConditionalVariableAssignment
  NameTarget
    Name
      Identifier 'foo'
      RawTokens
        LeftPar
        AbstractLiteral '0'
        RightPar
  ColonEq
  NameExpression
    Name
      Identifier 'bar'
      RawTokens
        LeftPar
        AbstractLiteral '1'
        Comma
        AbstractLiteral '2'
        RightPar
  Keyword(When)
  BinaryExpression
    NameExpression
      Name
        Identifier 'cond'
    EQ
    NameExpression
      Name
        Identifier 'true'
  ConditionalElseWhenExpression
    Keyword(Else)
    NameExpression
      Name
        Identifier 'expr2'
    Keyword(When)
    NameExpression
      Name
        Identifier 'cond2'
  SemiColon",
        );
    }

    #[test]
    fn conditional_variable_assignment_else() {
        check(
            Parser::sequential_statement,
            "\
foo(0) := bar(1,2) when cond = true else expr2;
        ",
            "\
ConditionalVariableAssignment
  NameTarget
    Name
      Identifier 'foo'
      RawTokens
        LeftPar
        AbstractLiteral '0'
        RightPar
  ColonEq
  NameExpression
    Name
      Identifier 'bar'
      RawTokens
        LeftPar
        AbstractLiteral '1'
        Comma
        AbstractLiteral '2'
        RightPar
  Keyword(When)
  BinaryExpression
    NameExpression
      Name
        Identifier 'cond'
    EQ
    NameExpression
      Name
        Identifier 'true'
  ConditionalElseItem
    Keyword(Else)
    NameExpression
      Name
        Identifier 'expr2'
  SemiColon",
        );
    }

    #[test]
    fn conditional_signal_assignment() {
        check(
            Parser::sequential_statement,
            "\
foo(0) <= bar(1,2) after 2 ns when cond;
        ",
            "\
ConditionalWaveformAssignment
  NameTarget
    Name
      Identifier 'foo'
      RawTokens
        LeftPar
        AbstractLiteral '0'
        RightPar
  LTE
  WaveformElements
    WaveformElement
      NameExpression
        Name
          Identifier 'bar'
          RawTokens
            LeftPar
            AbstractLiteral '1'
            Comma
            AbstractLiteral '2'
            RightPar
      Keyword(After)
      PhysicalLiteral
        AbstractLiteral '2'
        Name
          Identifier 'ns'
  Keyword(When)
  NameExpression
    Name
      Identifier 'cond'
  SemiColon",
        );
    }

    #[test]
    fn selected_signal_assignment() {
        check(
            Parser::sequential_statement,
            "\
with x(0) + 1 select
   foo(0) <= transport bar(1,2) after 2 ns when 0|1,
                       def when others;
        ",
            "\
SelectedWaveformAssignment
  Keyword(With)
  BinaryExpression
    NameExpression
      Name
        Identifier 'x'
        RawTokens
          LeftPar
          AbstractLiteral '0'
          RightPar
    Plus
    LiteralExpression
      AbstractLiteral '1'
  Keyword(Select)
  NameTarget
    Name
      Identifier 'foo'
      RawTokens
        LeftPar
        AbstractLiteral '0'
        RightPar
  LTE
  TransportDelayMechanism
    Keyword(Transport)
  SelectedWaveforms
    SelectedWaveformItem
      WaveformElements
        WaveformElement
          NameExpression
            Name
              Identifier 'bar'
              RawTokens
                LeftPar
                AbstractLiteral '1'
                Comma
                AbstractLiteral '2'
                RightPar
          Keyword(After)
          PhysicalLiteral
            AbstractLiteral '2'
            Name
              Identifier 'ns'
      Keyword(When)
      Choices
        LiteralExpression
          AbstractLiteral '0'
        Bar
        LiteralExpression
          AbstractLiteral '1'
    Comma
    SelectedWaveformItem
      WaveformElements
        WaveformElement
          NameExpression
            Name
              Identifier 'def'
      Keyword(When)
      Choices
        Keyword(Others)
  SemiColon
            ",
        );
    }

    #[test]
    fn selected_signal_force_assignment() {
        check(
            Parser::sequential_statement,
            "\
with x(0) + 1 select
   foo(0) <= force bar(1,2) when 0|1,
                       def when others;",
            "\
SelectedForceAssignment
  Keyword(With)
  BinaryExpression
    NameExpression
      Name
        Identifier 'x'
        RawTokens
          LeftPar
          AbstractLiteral '0'
          RightPar
    Plus
    LiteralExpression
      AbstractLiteral '1'
  Keyword(Select)
  NameTarget
    Name
      Identifier 'foo'
      RawTokens
        LeftPar
        AbstractLiteral '0'
        RightPar
  LTE
  Keyword(Force)
  SelectedExpressions
    SelectedExpressionItem
      NameExpression
        Name
          Identifier 'bar'
          RawTokens
            LeftPar
            AbstractLiteral '1'
            Comma
            AbstractLiteral '2'
            RightPar
      Keyword(When)
      Choices
        LiteralExpression
          AbstractLiteral '0'
        Bar
        LiteralExpression
          AbstractLiteral '1'
    Comma
    SelectedExpressionItem
      NameExpression
        Name
          Identifier 'def'
      Keyword(When)
      Choices
        Keyword(Others)
  SemiColon
            ",
        );
    }

    #[test]
    fn procedure_call_statement() {
        check(
            Parser::sequential_statement,
            "foo(1, 2);",
            "\
ProcedureCallStatement
  NameTarget
    Name
      Identifier 'foo'
      RawTokens
        LeftPar
        AbstractLiteral '1'
        Comma
        AbstractLiteral '2'
        RightPar
  SemiColon
        ",
        );
    }

    #[test]
    fn procedure_call_statement_no_args() {
        check(
            Parser::sequential_statement,
            "foo;",
            "\
ProcedureCallStatement
  NameTarget
    Name
      Identifier 'foo'
  SemiColon
        ",
        );
    }
}
