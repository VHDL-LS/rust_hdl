// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::marker::{CompletedMarker, Precede};
use crate::parser::util::{choice_options, StallGuard};
use crate::parser::Parser;
use crate::syntax::meta::Layout;
use crate::syntax::node_kind::NodeKind;
use crate::syntax::node_kind::NodeKind::*;
use crate::syntax::{AstNode, SequentialStatementSyntax};
use crate::tokens::token_kind::Keyword as Kw;
use crate::tokens::TokenKind::{self, *};

impl Parser {
    pub(crate) fn wait_statement(&mut self) -> CompletedMarker {
        self.node(WaitStatement, |p| {
            p.opt_label();
            p.expect_kw(Kw::Wait);
            if p.next_is(Keyword(Kw::On)) {
                p.node(SensitivityClause, |p| {
                    p.skip();
                    p.sensitivity_list();
                });
            }
            if p.next_is(Keyword(Kw::Until)) {
                p.node(ConditionClause, |p| {
                    p.skip();
                    p.expression();
                });
            }
            if p.next_is(Keyword(Kw::For)) {
                p.node(TimeoutClause, |p| {
                    p.skip();
                    p.expression();
                });
            }
            p.expect_token(SemiColon);
        })
    }

    pub(crate) fn assert_statement(&mut self) -> CompletedMarker {
        self.node(AssertionStatement, |p| {
            p.opt_label();
            p.assertion();
            p.expect_token(SemiColon);
        })
    }

    pub(crate) fn report_statement(&mut self) -> CompletedMarker {
        self.node(ReportStatement, |p| {
            p.opt_label();
            p.expect_kw(Kw::Report);
            p.expression();
            if p.next_is(Keyword(Kw::Severity)) {
                p.node(SeverityClause, |p| {
                    p.skip(); // Kw::Severity
                    p.expression();
                });
            }
            p.expect_token(SemiColon);
        })
    }

    pub(crate) fn next_statement(&mut self) -> CompletedMarker {
        self.node(NextStatement, |p| {
            p.opt_label();
            p.expect_kw(Kw::Next);
            p.opt_identifier();
            if p.next_is(Keyword(Kw::When)) {
                p.node(WhenClause, |p| {
                    p.skip(); // Kw::When
                    p.expression();
                });
            }
            p.expect_token(SemiColon);
        })
    }

    pub(crate) fn exit_statement(&mut self) -> CompletedMarker {
        self.node(ExitStatement, |p| {
            p.opt_label();
            p.expect_kw(Kw::Exit);
            p.opt_identifier();
            if p.next_is(Keyword(Kw::When)) {
                p.node(WhenClause, |p| {
                    p.skip(); // Kw::When
                    p.expression();
                });
            }
            p.expect_token(SemiColon);
        })
    }

    pub(crate) fn return_statement(&mut self) -> CompletedMarker {
        self.node(ReturnStatement, |p| {
            p.opt_label();
            p.expect_kw(Kw::Return);
            if !p.next_is(SemiColon) {
                p.expression();
            }
            p.expect_token(SemiColon);
        })
    }

    pub(crate) fn null_statement(&mut self) -> CompletedMarker {
        self.node(NullStatement, |p| {
            p.opt_label();
            p.expect_kw(Kw::Null);
            p.expect_token(SemiColon);
        })
    }

    pub(crate) fn if_statement(&mut self) -> CompletedMarker {
        self.node(IfStatement, |p| {
            p.if_statement_preamble();
            p.sequence_of_statements();
            while p.next_is(Keyword(Kw::Elsif)) {
                p.node(IfStatementElsif, |p| {
                    p.skip();
                    p.condition();
                    p.expect_kw(Kw::Then);
                    p.sequence_of_statements();
                });
            }
            if p.next_is(Keyword(Kw::Else)) {
                p.node(IfStatementElse, |p| {
                    p.skip();
                    p.sequence_of_statements();
                });
            }
            p.if_statement_epilogue();
        })
    }

    pub(crate) fn if_statement_preamble(&mut self) {
        self.node(IfStatementPreamble, |p| {
            p.opt_label();
            p.expect_kw(Kw::If);
            p.condition();
            p.expect_kw(Kw::Then);
        });
    }

    pub(crate) fn if_statement_epilogue(&mut self) {
        self.node(IfStatementEpilogue, |p| {
            p.expect_tokens([Keyword(Kw::End), Keyword(Kw::If)]);
            p.opt_identifier();
            p.expect_token(SemiColon);
        });
    }

    pub(crate) fn case_statement(&mut self) -> CompletedMarker {
        self.node(CaseStatement, |p| {
            p.case_statement_preamble();
            p.case_statement_alternative();
            while p.next_is(Keyword(Kw::When)) {
                p.case_statement_alternative();
            }
            p.case_statement_epilogue();
        })
    }

    pub(crate) fn case_statement_preamble(&mut self) {
        self.node(CaseStatementPreamble, |p| {
            p.opt_label();
            p.expect_kw(Kw::Case);
            p.opt_token(Que);
            p.expression();
            p.expect_kw(Kw::Is);
        });
    }

    pub(crate) fn case_statement_epilogue(&mut self) {
        self.node(CaseStatementEpilogue, |p| {
            p.expect_tokens([Keyword(Kw::End), Keyword(Kw::Case)]);
            p.opt_token(Que);
            p.opt_identifier();
            p.expect_token(SemiColon);
        });
    }

    pub(crate) fn case_statement_alternative(&mut self) {
        self.node(CaseStatementAlternative, |p| {
            p.case_statement_alternative_preamble();
            p.sequence_of_statements();
        });
    }

    pub(crate) fn case_statement_alternative_preamble(&mut self) {
        self.node(CaseStatementAlternativePreamble, |p| {
            p.expect_kw(Kw::When);
            p.choices();
            p.expect_token(RightArrow);
        });
    }

    pub(crate) fn aggregate(&mut self) {
        self.node(Aggregate, |p| {
            p.aggregate_inner();
        });
    }

    pub(crate) fn aggregate_inner(&mut self) {
        self.expect_token(LeftPar);
        self.separated_list(ElementAssociationList, Parser::element_association, Comma);
        self.expect_token(RightPar);
    }

    pub(crate) fn element_association(&mut self) {
        self.node(ElementAssociation, |p| {
            let has_choices = matches!(
                p.lookahead_max_token_index(usize::MAX, [RightArrow, Comma]),
                Ok((RightArrow, _))
            );
            if has_choices {
                p.node(ElementChoices, |p| {
                    p.choices();
                    p.expect_token(RightArrow);
                });
            }
            p.expression();
        });
    }

    pub(crate) fn loop_statement(&mut self) -> CompletedMarker {
        self.node(LoopStatement, |p| {
            p.loop_statement_preamble();
            p.sequence_of_statements();
            p.loop_statement_epilogue();
        })
    }

    pub(crate) fn loop_statement_preamble(&mut self) {
        self.node(LoopStatementPreamble, |p| {
            p.opt_label();
            p.opt_iteration_scheme();
            p.expect_kw(Kw::Loop);
        });
    }

    pub(crate) fn loop_statement_epilogue(&mut self) {
        self.node(LoopStatementEpilogue, |p| {
            p.expect_tokens([Keyword(Kw::End), Keyword(Kw::Loop)]);
            p.opt_identifier();
            p.expect_token(SemiColon);
        });
    }

    fn opt_iteration_scheme(&mut self) {
        if self.next_is(Keyword(Kw::While)) {
            self.node(WhileScheme, |p| {
                p.skip();
                p.condition();
            });
        } else if self.next_is(Keyword(Kw::For)) {
            self.node(ForScheme, |p| {
                p.skip();
                p.parameter_specification();
            });
        }
    }

    pub(crate) fn sequential_statements(&mut self, node_kind: NodeKind, layout: &Layout) {
        let allowed_nodes = choice_options(layout);
        self.node(node_kind, |p| {
            let mut guard = StallGuard::new();
            while guard.should_continue(p) {
                match p.peek_token() {
                    Eof | Keyword(Kw::End | Kw::Else | Kw::Elsif | Kw::When) => break,
                    _ => {
                        if let Some(stmt) = p.sequential_statement() {
                            p.check_node_is_allowed(&stmt, allowed_nodes);
                        }
                    }
                }
            }
        });
    }

    pub(crate) fn sequence_of_statements(&mut self) {
        self.sequential_statements(SequenceOfStatements, SequentialStatementSyntax::META);
    }

    fn opt_force_mode(&mut self) {
        self.opt_tokens([Keyword(Kw::In), Keyword(Kw::Out)]);
    }

    pub(crate) fn selected_expressions(&mut self) {
        self.separated_list(SelectedExpressions, Parser::selected_expression, Comma);
    }

    fn selected_expression(&mut self) {
        self.node(SelectedExpressionItem, |p| {
            p.expression();
            p.expect_kw(Kw::When);
            p.choices();
        });
    }

    fn sequential_statement_start(&mut self) -> TokenKind {
        if self.next_is(Identifier) && self.next_nth_is(Colon, 1) {
            self.peek_nth_token(2)
        } else {
            self.peek_token()
        }
    }

    pub(crate) fn sequential_statement(&mut self) -> Option<CompletedMarker> {
        match self.sequential_statement_start() {
            Keyword(Kw::Wait) => Some(self.wait_statement()),
            Keyword(Kw::Assert) => Some(self.assert_statement()),
            Keyword(Kw::Report) => Some(self.report_statement()),
            Keyword(Kw::If) => Some(self.if_statement()),
            Keyword(Kw::Case) => Some(self.case_statement()),
            Keyword(Kw::For | Kw::Loop | Kw::While) => Some(self.loop_statement()),
            Keyword(Kw::Next) => Some(self.next_statement()),
            Keyword(Kw::Exit) => Some(self.exit_statement()),
            Keyword(Kw::Return) => Some(self.return_statement()),
            Keyword(Kw::Null) => Some(self.null_statement()),
            Keyword(Kw::With) => {
                let unknown = self.start_unknown();
                self.opt_label();
                self.selected_assignment_preamble();
                self.target();
                let marker = match self.peek_token() {
                    LTE => {
                        if self.next_nth_is(Keyword(Kw::Force), 1) {
                            let marker = unknown.resolve(self, SelectedForceAssignment);
                            self.skip_n(2);
                            self.opt_force_mode();
                            self.selected_expressions();
                            marker
                        } else {
                            let marker = unknown.resolve(self, SelectedWaveformAssignment);
                            self.skip();
                            self.opt_delay_mechanism();
                            self.selected_waveforms();
                            marker
                        }
                    }
                    ColonEq => {
                        let marker = unknown.resolve(self, SelectedVariableAssignment);
                        self.skip();
                        self.selected_expressions();
                        marker
                    }
                    _ => {
                        let marker = unknown.resolve(self, SelectedWaveformAssignment);
                        self.expect_tokens_recover([LTE, ColonEq]);
                        marker
                    }
                };
                self.expect_token(SemiColon);
                Some(marker.complete(self))
            }
            Identifier | LeftPar | LtLt => {
                let unknown = self.start_unknown();
                self.opt_label();
                // A procedure call's callee is a plain `Name`; an assignment's
                // left-hand side is a `Target`. They are told apart by the
                // operator that follows (`:=`/`<=` assign, `;` call), and an
                // aggregate is only ever an assignment target. So parse the name
                // bare and wrap it in `NameTarget` only when an assignment
                // operator follows.
                if self.next_is(LeftPar) {
                    self.node(AggregateTarget, |p| {
                        p.aggregate();
                    });
                } else {
                    let name = self.name();
                    if self.next_is_one_of([ColonEq, LTE]) {
                        name.precede(self, NameTarget).complete(self);
                    }
                }
                let marker = match self.peek_token() {
                    ColonEq => {
                        self.skip();
                        let expression = self.expression();
                        if self.next_is(Keyword(Kw::When)) {
                            let marker = unknown.resolve(self, ConditionalVariableAssignment);
                            let when = expression.precede(self, WhenExpression);
                            self.skip();
                            self.expression();
                            let when_expression = when.complete(self);
                            let expressions = when_expression.precede(self, ConditionalExpressions);
                            self.conditional_else(
                                Parser::expression,
                                ElseWhenExpression,
                                ElseExpression,
                            );
                            expressions.complete(self);
                            marker
                        } else {
                            unknown.resolve(self, SimpleVariableAssignment)
                        }
                    }
                    LTE => {
                        if self.next_nth_is(Keyword(Kw::Force), 1) {
                            self.skip_n(2);
                            self.opt_force_mode();
                            let expression = self.expression();
                            if self.next_is(Keyword(Kw::When)) {
                                let marker = unknown.resolve(self, ConditionalForceAssignment);
                                let when = expression.precede(self, WhenExpression);
                                self.skip();
                                self.expression();
                                let when_expression = when.complete(self);
                                let expressions =
                                    when_expression.precede(self, ConditionalExpressions);
                                self.conditional_else(
                                    Parser::expression,
                                    ElseWhenExpression,
                                    ElseExpression,
                                );
                                expressions.complete(self);
                                marker
                            } else {
                                unknown.resolve(self, SimpleForceAssignment)
                            }
                        } else if self.next_nth_is(Keyword(Kw::Release), 1) {
                            let marker = unknown.resolve(self, SimpleReleaseAssignment);
                            self.skip_n(2);
                            self.opt_force_mode();
                            marker
                        } else {
                            self.skip();
                            self.opt_delay_mechanism();
                            let waveform = self.waveform();
                            if self.next_is(Keyword(Kw::When)) {
                                let marker = unknown.resolve(self, ConditionalWaveformAssignment);
                                let when = waveform.precede(self, WhenWaveform);
                                self.skip();
                                self.expression();
                                let when_waveform = when.complete(self);
                                let waveforms = when_waveform.precede(self, ConditionalWaveforms);
                                self.conditional_else(
                                    Parser::waveform,
                                    ElseWhenWaveform,
                                    ElseWaveform,
                                );
                                waveforms.complete(self);
                                marker
                            } else {
                                unknown.resolve(self, SimpleWaveformAssignment)
                            }
                        }
                    }
                    SemiColon => unknown.resolve(self, ProcedureCallStatement),
                    _ => {
                        let marker = unknown.resolve(self, ProcedureCallStatement);
                        self.expect_tokens_recover([LTE, ColonEq, SemiColon]);
                        marker
                    }
                };
                self.expect_token(SemiColon);
                Some(marker.complete(self))
            }
            _ => {
                // consume label for error recovery
                self.opt_label();
                self.expect_tokens_recover([
                    Keyword(Kw::Wait),
                    Keyword(Kw::Assert),
                    Keyword(Kw::Report),
                    Keyword(Kw::If),
                    Keyword(Kw::Case),
                    Keyword(Kw::For),
                    Keyword(Kw::Loop),
                    Keyword(Kw::While),
                    Keyword(Kw::Next),
                    Keyword(Kw::Exit),
                    Keyword(Kw::Return),
                    Keyword(Kw::Null),
                    Keyword(Kw::With),
                    Identifier,
                    LeftPar,
                    LtLt,
                ]);
                None
            }
        }
    }

    pub(crate) fn conditional_else<T>(
        &mut self,
        item: impl Fn(&mut Parser) -> T,
        else_when_node: NodeKind,
        else_node: NodeKind,
    ) {
        while self.next_is(Keyword(Kw::Else)) {
            let unknown = self.start_unknown();
            self.skip();
            item(self);
            if self.next_is(Keyword(Kw::When)) {
                let marker = unknown.resolve(self, else_when_node);
                self.skip();
                self.condition();
                marker.complete(self);
            } else {
                unknown.complete(self, else_node);
                break;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils::to_test_text;
    use crate::parser::Parser;

    #[test]
    fn simple_wait_statement() {
        insta::assert_snapshot!(to_test_text(Parser::wait_statement, "wait;"));
    }

    #[test]
    fn simple_wait_statement_with_label() {
        insta::assert_snapshot!(to_test_text(Parser::wait_statement, "foo: wait;"));
    }

    #[test]
    fn wait_statement_with_sensitivity_list() {
        insta::assert_snapshot!(to_test_text(Parser::wait_statement, "wait on foo, bar;"));
    }

    #[test]
    fn wait_statement_with_condition() {
        insta::assert_snapshot!(to_test_text(Parser::wait_statement, "wait until a = b;"));
    }

    #[test]
    fn wait_statement_with_timeout() {
        insta::assert_snapshot!(to_test_text(Parser::wait_statement, "wait for 2 ns;"));
    }

    #[test]
    fn wait_statement_with_all_parts() {
        insta::assert_snapshot!(to_test_text(
            Parser::wait_statement,
            "wait on foo until bar for 2 ns;"
        ));
    }

    #[test]
    fn simple_assert() {
        insta::assert_snapshot!(to_test_text(Parser::assert_statement, "assert false;"))
    }

    #[test]
    fn full_assert() {
        insta::assert_snapshot!(to_test_text(
            Parser::assert_statement,
            "assert false report \"message\" severity error;"
        ))
    }

    #[test]
    fn report_statement() {
        insta::assert_snapshot!(to_test_text(
            Parser::report_statement,
            "report \"message\" severity error;"
        ))
    }

    #[test]
    fn next_statement() {
        insta::assert_snapshot!(to_test_text(Parser::next_statement, "next;"));
    }

    #[test]
    fn next_statement_loop_label() {
        insta::assert_snapshot!(to_test_text(Parser::next_statement, "next foo;"));
    }

    #[test]
    fn next_statement_condition() {
        insta::assert_snapshot!(to_test_text(Parser::next_statement, "next when condition;"));
    }

    #[test]
    fn next_statement_loop_label_condition() {
        insta::assert_snapshot!(to_test_text(
            Parser::next_statement,
            "next foo when condition;"
        ));
    }

    #[test]
    fn exit_statement() {
        insta::assert_snapshot!(to_test_text(Parser::exit_statement, "exit;"));
    }

    #[test]
    fn exit_statement_loop_label() {
        insta::assert_snapshot!(to_test_text(Parser::exit_statement, "exit foo;"));
    }

    #[test]
    fn exit_statement_condition() {
        insta::assert_snapshot!(to_test_text(Parser::exit_statement, "exit when condition;"));
    }

    #[test]
    fn exit_statement_loop_label_condition() {
        insta::assert_snapshot!(to_test_text(
            Parser::exit_statement,
            "exit foo when condition;"
        ));
    }

    #[test]
    fn return_statement() {
        insta::assert_snapshot!(to_test_text(Parser::return_statement, "return;"));
    }

    #[test]
    fn return_statement_expression() {
        insta::assert_snapshot!(to_test_text(Parser::return_statement, "return 1 + 2;"));
    }

    #[test]
    fn null_statement() {
        insta::assert_snapshot!(to_test_text(Parser::null_statement, "null;"));
    }

    #[test]
    fn empty_if_statement() {
        insta::assert_snapshot!(to_test_text(
            Parser::if_statement,
            "\
if cond = true then
end if;"
        ));
    }

    #[test]
    fn simple_if_statement() {
        insta::assert_snapshot!(to_test_text(
            Parser::if_statement,
            "\
if cond = true then
   foo(1,2);
   x := 1;
end if;"
        ));
    }

    #[test]
    fn labeled_if_statement() {
        insta::assert_snapshot!(to_test_text(
            Parser::if_statement,
            "\
mylabel: if cond = true then
   foo(1,2);
   x := 1;
end if mylabel;"
        ));
    }

    #[test]
    fn if_else_statement() {
        insta::assert_snapshot!(to_test_text(
            Parser::if_statement,
            "\
if cond = true then
   foo(1,2);
else
   x := 1;
end if;"
        ));
    }

    #[test]
    fn labeled_if_else_statement() {
        insta::assert_snapshot!(to_test_text(
            Parser::if_statement,
            "\
mylabel: if cond = true then
   foo(1,2);
else
   x := 1;
end if mylabel;"
        ));
    }

    #[test]
    fn if_elsif_else_statement() {
        insta::assert_snapshot!(to_test_text(
            Parser::if_statement,
            "\
if cond = true then
   foo(1,2);
elsif cond2 = false then
   y := 2;
else
   x := 1;
end if;"
        ));
    }

    #[test]
    fn labeled_if_elsif_else_statement() {
        insta::assert_snapshot!(to_test_text(
            Parser::if_statement,
            "\
mylabel: if cond = true then
   foo(1,2);
elsif cond2 = false then
   y := 2;
else
   x := 1;
end if mylabel;"
        ));
    }

    #[test]
    fn case_statement() {
        insta::assert_snapshot!(to_test_text(
            Parser::case_statement,
            "\
case foo(1) is
  when 1 | 2 =>
    stmt1;
    stmt2;
  when others =>
    stmt3;
    stmt4;
end case;"
        ));
    }

    #[test]
    fn matching_case_statement() {
        insta::assert_snapshot!(to_test_text(
            Parser::case_statement,
            "\
case? foo(1) is
  when others => null;
end case?;"
        ));
    }

    #[test]
    fn loop_statement() {
        insta::assert_snapshot!(to_test_text(
            Parser::loop_statement,
            "\
lbl: loop
  stmt1;
  stmt2;
end loop lbl;"
        ));
    }

    #[test]
    fn while_loop_statement() {
        insta::assert_snapshot!(to_test_text(
            Parser::loop_statement,
            "\
while foo = true loop
  stmt1;
  stmt2;
end loop;"
        ));
    }

    #[test]
    fn for_loop_statement() {
        insta::assert_snapshot!(to_test_text(
            Parser::loop_statement,
            "\
for idx in 0 to 3 loop
  stmt1;
  stmt2;
end loop;"
        ));
    }

    fn stmt_to_test_text(input: &str) -> String {
        to_test_text(Parser::sequential_statement, input)
    }

    #[test]
    fn simple_signal_assignment() {
        insta::assert_snapshot!(stmt_to_test_text("foo(0) <= bar(1,2) after 2 ns;"));
    }

    #[test]
    fn simple_signal_force_assignment() {
        insta::assert_snapshot!(stmt_to_test_text("foo(0) <= force bar(1,2);"));
    }

    #[test]
    fn simple_signal_release_assignment() {
        insta::assert_snapshot!(stmt_to_test_text("foo(0) <= release;"));
    }

    #[test]
    fn signal_assignment_external_name() {
        insta::assert_snapshot!(stmt_to_test_text(
            "<< signal dut.foo : boolean  >> <= bar(1,2);"
        ));
    }

    #[test]
    fn simple_signal_assignment_delay_mechanism() {
        insta::assert_snapshot!(stmt_to_test_text("foo(0) <= transport bar(1,2);"));
    }

    #[test]
    fn simple_variable_assignment() {
        insta::assert_snapshot!(stmt_to_test_text("foo(0) := bar(1,2);"));
    }

    #[test]
    fn variable_assignment_external_name() {
        insta::assert_snapshot!(stmt_to_test_text(
            "<< variable dut.foo : boolean >> := bar(1,2);"
        ));
    }

    #[test]
    fn simple_aggregate_variable_assignment() {
        insta::assert_snapshot!(stmt_to_test_text("(foo, 1 => bar) := bar;"));
    }

    #[test]
    fn labeled_aggregate_variable_assignment() {
        insta::assert_snapshot!(stmt_to_test_text("name: (foo, 1 => bar) := bar;"));
    }

    #[test]
    fn labeled_simple_variable_assignment() {
        insta::assert_snapshot!(stmt_to_test_text("name: foo(0) := bar(1,2);"));
    }

    #[test]
    fn selected_variable_assignment() {
        insta::assert_snapshot!(stmt_to_test_text(
            "\
with x(0) + 1 select
   foo(0) := bar(1,2) when 0|1,
             def when others;
        "
        ));
    }

    #[test]
    fn labeled_selected_variable_assignment() {
        insta::assert_snapshot!(stmt_to_test_text(
            "\
lbl: with x select
   foo := bar when others;
        "
        ));
    }

    #[test]
    fn conditional_variable_assignment() {
        insta::assert_snapshot!(stmt_to_test_text(
            "\
foo(0) := bar(1,2) when cond = true;
        "
        ));
    }

    #[test]
    fn conditional_signal_force_assignment() {
        insta::assert_snapshot!(stmt_to_test_text(
            "\
foo(0) <= force bar(1,2) when cond;
        "
        ));
    }

    #[test]
    fn conditional_variable_assignment_several() {
        insta::assert_snapshot!(stmt_to_test_text(
            "\
foo(0) := bar(1,2) when cond = true else expr2 when cond2;
        "
        ));
    }

    #[test]
    fn conditional_variable_assignment_else() {
        insta::assert_snapshot!(stmt_to_test_text(
            "\
foo(0) := bar(1,2) when cond = true else expr2;
        "
        ));
    }

    #[test]
    fn conditional_signal_assignment() {
        insta::assert_snapshot!(stmt_to_test_text(
            "\
foo(0) <= bar(1,2) after 2 ns when cond;
        "
        ));
    }

    #[test]
    fn conditional_waveform_assignment_else() {
        insta::assert_snapshot!(stmt_to_test_text(
            "\
data_processing_state <= processing when processing_data else idle;"
        ));
    }

    #[test]
    fn selected_signal_assignment() {
        insta::assert_snapshot!(stmt_to_test_text(
            "\
with x(0) + 1 select
   foo(0) <= transport bar(1,2) after 2 ns when 0|1,
                       def when others;
        "
        ));
    }

    #[test]
    fn selected_signal_force_assignment() {
        insta::assert_snapshot!(stmt_to_test_text(
            "\
with x(0) + 1 select
   foo(0) <= force bar(1,2) when 0|1,
                       def when others;"
        ));
    }

    #[test]
    fn labeled_selected_signal_assignment() {
        insta::assert_snapshot!(stmt_to_test_text(
            "\
lbl: with x select
   foo <= bar when others;
        "
        ));
    }

    #[test]
    fn labeled_selected_signal_force_assignment() {
        insta::assert_snapshot!(stmt_to_test_text(
            "\
lbl: with x select
   foo <= force bar when others;
        "
        ));
    }

    #[test]
    fn procedure_call_statement() {
        insta::assert_snapshot!(stmt_to_test_text("foo(1, 2);"));
    }

    #[test]
    fn procedure_call_statement_no_args() {
        insta::assert_snapshot!(stmt_to_test_text("foo;"));
    }

    #[test]
    fn procedure_call_with_qualified_expression() {
        insta::assert_snapshot!(stmt_to_test_text("foo(l, string'(\"L: \"));"));
    }

    // MARK: Error recovery

    #[test]
    fn if_missing_then() {
        assert_recovery_snapshot!(
            "\
if cond
  x := 1;
end if;",
            Parser::if_statement
        );
    }

    #[test]
    fn if_missing_end_if() {
        assert_recovery_snapshot!(
            "\
if cond then
  x := 1;",
            Parser::if_statement
        );
    }

    #[test]
    fn empty_case_statement() {
        assert_recovery_snapshot!(
            "\
case sel is
end case;",
            Parser::case_statement
        );
    }

    #[test]
    fn case_missing_is() {
        assert_recovery_snapshot!(
            "\
case sel
  when 0 => null;
end case;",
            Parser::case_statement
        );
    }

    #[test]
    fn loop_missing_end() {
        assert_recovery_snapshot!(
            "\
loop
  x := 1;",
            Parser::loop_statement
        );
    }

    #[test]
    fn wait_missing_semicolon() {
        assert_recovery_snapshot!("wait", Parser::wait_statement);
    }

    #[test]
    fn sequential_statement_stall() {
        assert_recovery_snapshot!(
            "\
function f return integer is
begin
    use work.all;      -- `use` in SubprogramBody follow
end;
        ",
            Parser::subprogram_body
        );
    }

    #[test]
    fn sequential_statement_lone_label() {
        assert_recovery_snapshot!(
            "\
function f return integer is
begin
    p:
end;
        ",
            Parser::subprogram_body
        );
    }
}
