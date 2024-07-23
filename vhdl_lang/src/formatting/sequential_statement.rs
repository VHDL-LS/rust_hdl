// This Source Code Form is subject to the terms of the Mozilla Public
// Lic// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// This Source Code Form is subject to the terms of the Mozilla Public
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2024, Olof Kraigher olof.kraigher@gmail.com

use crate::ast::token_range::WithTokenSpan;
use crate::ast::{
    AssignmentRightHand, CaseStatement, Choice, DelayMechanism, Expression, Ident, IterationScheme,
    LabeledSequentialStatement, LoopStatement, ReportStatement, SequentialStatement,
    SignalAssignment, WaitStatement, WithRef,
};
use crate::formatting::buffer::Buffer;
use crate::{HasTokenSpan, TokenSpan};
use vhdl_lang::ast::{
    ExitStatement, IfStatement, NextStatement, SignalForceAssignment, SignalReleaseAssignment,
    VariableAssignment,
};
use vhdl_lang::formatting::VHDLFormatter;
use vhdl_lang::indented;

impl VHDLFormatter<'_> {
    pub fn format_sequential_statements(
        &self,
        statements: &[LabeledSequentialStatement],
        buffer: &mut Buffer,
    ) {
        if statements.is_empty() {
            return;
        }
        indented!(buffer, {
            buffer.line_break();
            for (i, item) in statements.iter().enumerate() {
                self.format_labeled_sequential_statement(item, buffer);
                if i < statements.len() - 1 {
                    self.line_break_preserve_whitespace(item.statement.get_end_token(), buffer);
                }
            }
        });
    }

    pub fn format_labeled_sequential_statement(
        &self,
        statement: &LabeledSequentialStatement,
        buffer: &mut Buffer,
    ) {
        self.format_optional_label(statement.label.tree.as_ref(), buffer);
        self.format_sequential_statement(&statement.statement, buffer);
    }

    pub fn format_sequential_statement(
        &self,
        statement: &WithTokenSpan<SequentialStatement>,
        buffer: &mut Buffer,
    ) {
        use SequentialStatement::*;
        let span = statement.span;
        match &statement.item {
            Wait(wait_statement) => self.format_wait_statement(wait_statement, span, buffer),
            Assert(assert) => {
                self.format_token_id(span.start_token, buffer);
                buffer.push_whitespace();
                self.format_assert_statement(assert, buffer);
                self.format_token_id(span.end_token, buffer);
            }
            Report(report) => self.format_report_statement(report, span, buffer),
            VariableAssignment(variable_assignment) => {
                self.format_variable_assignment(variable_assignment, span, buffer)
            }
            SignalAssignment(signal_assignment) => {
                self.format_signal_assignment(signal_assignment, span, buffer)
            }
            SignalForceAssignment(signal_assignment) => {
                self.format_signal_force_assignment(signal_assignment, span, buffer)
            }
            SignalReleaseAssignment(signal_assignment) => {
                self.format_signal_release_assignment(signal_assignment, span, buffer)
            }
            ProcedureCall(call_or_indexed) => {
                self.format_call_or_indexed(&call_or_indexed.item, call_or_indexed.span, buffer);
                self.format_token_id(span.end_token, buffer);
            }
            If(if_statement) => {
                self.format_if_statement(if_statement, span, buffer);
            }
            Case(case_statement) => {
                self.format_case_statement(case_statement, span, buffer);
            }
            Loop(loop_statement) => {
                self.format_loop_statement(loop_statement, span, buffer);
            }
            Next(next_statement) => self.format_next_statement(next_statement, span, buffer),
            Exit(exit_statement) => self.format_exit_statement(exit_statement, span, buffer),

            Return(stmt) => {
                self.format_token_id(span.start_token, buffer);
                if let Some(expr) = &stmt.expression {
                    buffer.push_whitespace();
                    self.format_expression(expr.as_ref(), buffer);
                }
                self.format_token_id(span.end_token, buffer);
            }
            Null => self.join_token_span(span, buffer),
        }
    }

    pub fn format_wait_statement(
        &self,
        statement: &WaitStatement,
        span: TokenSpan,
        buffer: &mut Buffer,
    ) {
        // wait
        self.format_token_id(span.start_token, buffer);
        if let Some(name_list) = &statement.sensitivity_clause {
            buffer.push_whitespace();
            // on
            self.format_token_id(span.start_token + 1, buffer);
            buffer.push_whitespace();
            for (i, item) in name_list.items.iter().enumerate() {
                self.format_name(item.as_ref(), buffer);
                if let Some(token) = name_list.tokens.get(i) {
                    self.format_token_id(*token, buffer);
                    buffer.push_whitespace();
                }
            }
        }
        if let Some(condition_clause) = &statement.condition_clause {
            buffer.push_whitespace();
            self.format_token_id(condition_clause.span.start_token - 1, buffer);
            buffer.push_whitespace();
            self.format_expression(condition_clause.as_ref(), buffer);
        }
        if let Some(timeout_clause) = &statement.timeout_clause {
            buffer.push_whitespace();
            self.format_token_id(timeout_clause.span.start_token - 1, buffer);
            buffer.push_whitespace();
            self.format_expression(timeout_clause.as_ref(), buffer);
        }

        // ;
        self.format_token_id(span.end_token, buffer);
    }

    pub fn format_report_statement(
        &self,
        report: &ReportStatement,
        span: TokenSpan,
        buffer: &mut Buffer,
    ) {
        // report
        self.format_token_id(span.start_token, buffer);
        buffer.push_whitespace();
        self.format_expression(report.report.as_ref(), buffer);
        self.format_opt_severity(report.severity.as_ref(), buffer);
        self.format_token_id(span.end_token, buffer);
    }

    pub fn format_variable_assignment(
        &self,
        assignment: &VariableAssignment,
        span: TokenSpan,
        buffer: &mut Buffer,
    ) {
        if let AssignmentRightHand::Selected(selected) = &assignment.rhs {
            // with
            self.format_token_id(selected.expression.span.start_token - 1, buffer);
            buffer.push_whitespace();
            self.format_expression(selected.expression.as_ref(), buffer);
            buffer.push_whitespace();
            // select
            self.format_token_id(selected.expression.span.end_token + 1, buffer);
            buffer.push_whitespace();
        }
        self.format_target(&assignment.target, buffer);
        buffer.push_whitespace();
        self.format_token_id(assignment.target.span.end_token + 1, buffer);
        buffer.push_whitespace();
        self.format_assignment_right_hand(
            &assignment.rhs,
            |formatter, expr, buffer| formatter.format_expression(expr.as_ref(), buffer),
            buffer,
        );
        self.format_token_id(span.end_token, buffer);
    }

    pub fn format_signal_assignment(
        &self,
        assignment: &SignalAssignment,
        span: TokenSpan,
        buffer: &mut Buffer,
    ) {
        self.format_target(&assignment.target, buffer);
        buffer.push_whitespace();
        self.format_token_id(assignment.target.span.end_token + 1, buffer);
        buffer.push_whitespace();
        if let Some(delay_mechanism) = &assignment.delay_mechanism {
            self.format_delay_mechanism(delay_mechanism, buffer);
            buffer.push_whitespace();
        }
        self.format_assignment_right_hand(&assignment.rhs, Self::format_waveform, buffer);
        self.format_token_id(span.end_token, buffer);
    }

    pub fn format_delay_mechanism(
        &self,
        delay_mechanism: &WithTokenSpan<DelayMechanism>,
        buffer: &mut Buffer,
    ) {
        match &delay_mechanism.item {
            DelayMechanism::Transport => {
                self.format_token_span(delay_mechanism.span, buffer);
            }
            DelayMechanism::Inertial { reject } => {
                if let Some(reject) = reject {
                    self.format_token_id(delay_mechanism.span.start_token, buffer);
                    buffer.push_whitespace();
                    self.format_expression(reject.as_ref(), buffer);
                    buffer.push_whitespace();
                }
                // inertial
                self.format_token_id(delay_mechanism.span.end_token, buffer);
            }
        }
    }

    pub fn format_signal_force_assignment(
        &self,
        assignment: &SignalForceAssignment,
        span: TokenSpan,
        buffer: &mut Buffer,
    ) {
        self.format_target(&assignment.target, buffer);
        buffer.push_whitespace();
        // <=
        self.format_token_id(assignment.target.span.end_token + 1, buffer);
        buffer.push_whitespace();
        // force
        self.format_token_id(assignment.target.span.end_token + 2, buffer);
        buffer.push_whitespace();
        if assignment.force_mode.is_some() {
            self.format_token_id(assignment.target.span.end_token + 3, buffer);
            buffer.push_whitespace();
        }
        self.format_assignment_right_hand(
            &assignment.rhs,
            |formatter, expr, buffer| formatter.format_expression(expr.as_ref(), buffer),
            buffer,
        );
        self.format_token_id(span.end_token, buffer);
    }

    pub fn format_signal_release_assignment(
        &self,
        assignment: &SignalReleaseAssignment,
        span: TokenSpan,
        buffer: &mut Buffer,
    ) {
        self.format_target(&assignment.target, buffer);
        buffer.push_whitespace();
        // <=
        self.format_token_id(assignment.target.span.end_token + 1, buffer);
        buffer.push_whitespace();
        // release
        self.format_token_id(assignment.target.span.end_token + 2, buffer);
        if assignment.force_mode.is_some() {
            buffer.push_whitespace();
            self.format_token_id(assignment.target.span.end_token + 3, buffer);
        }
        self.format_token_id(span.end_token, buffer);
    }

    pub fn format_if_statement(
        &self,
        statement: &IfStatement,
        span: TokenSpan,
        buffer: &mut Buffer,
    ) {
        for cond in &statement.conds.conditionals {
            let condition = &cond.condition;
            // if | elsif
            self.format_token_id(condition.span.start_token - 1, buffer);
            buffer.push_whitespace();
            self.format_expression(condition.as_ref(), buffer);
            buffer.push_whitespace();
            // then
            self.format_token_id(condition.span.end_token + 1, buffer);
            self.format_sequential_statements(&cond.item, buffer);
            buffer.line_break();
        }
        if let Some((statements, token)) = &statement.conds.else_item {
            self.format_token_id(*token, buffer);
            self.format_sequential_statements(statements, buffer);
            buffer.line_break();
        }
        if statement.end_label_pos.is_some() {
            // end if <label>
            self.format_token_span(
                TokenSpan::new(span.end_token - 3, span.end_token - 1),
                buffer,
            )
        } else {
            // end if
            self.format_token_span(
                TokenSpan::new(span.end_token - 2, span.end_token - 1),
                buffer,
            )
        }
        self.format_token_id(span.end_token, buffer);
    }

    pub fn format_choice(&self, choice: &WithTokenSpan<Choice>, buffer: &mut Buffer) {
        match &choice.item {
            Choice::Expression(expr) => {
                self.format_expression(WithTokenSpan::new(expr, choice.span), buffer)
            }
            Choice::DiscreteRange(range) => self.format_discrete_range(range, buffer),
            Choice::Others => self.format_token_span(choice.span, buffer),
        }
    }

    pub fn format_case_statement(
        &self,
        statement: &CaseStatement,
        span: TokenSpan,
        buffer: &mut Buffer,
    ) {
        // case
        self.format_token_id(span.start_token, buffer);
        if statement.is_matching {
            // ?
            self.format_token_id(span.start_token + 1, buffer);
        }
        buffer.push_whitespace();
        self.format_expression(statement.expression.as_ref(), buffer);
        buffer.push_whitespace();
        // is
        self.format_token_id(statement.expression.span.end_token + 1, buffer);
        indented!(buffer, {
            for alternative in &statement.alternatives {
                buffer.line_break();
                for (i, choice) in alternative.choices.iter().enumerate() {
                    if i == 0 {
                        // when
                        self.format_token_id(choice.span.start_token - 1, buffer);
                        buffer.push_whitespace();
                    }
                    self.format_choice(choice, buffer);
                    if i < alternative.choices.len() - 1 {
                        buffer.push_whitespace();
                        // |
                        self.format_token_id(choice.span.end_token + 1, buffer);
                        buffer.push_whitespace();
                    }
                    if i == alternative.choices.len() - 1 {
                        buffer.push_whitespace();
                        // =>
                        self.format_token_id(choice.span.end_token + 1, buffer);
                    }
                }
                self.format_sequential_statements(&alternative.item, buffer);
            }
        });
        buffer.line_break();
        if statement.is_matching {
            self.format_token_span(
                TokenSpan::new(statement.end_token, span.end_token - 2),
                buffer,
            );
            self.format_token_id(span.end_token - 1, buffer);
        } else {
            self.format_token_span(
                TokenSpan::new(statement.end_token, span.end_token - 1),
                buffer,
            );
        }
        self.format_token_id(span.end_token, buffer);
    }

    pub fn format_loop_statement(
        &self,
        statement: &LoopStatement,
        span: TokenSpan,
        buffer: &mut Buffer,
    ) {
        if let Some(scheme) = &statement.iteration_scheme {
            match scheme {
                IterationScheme::While(expression) => {
                    // while
                    self.format_token_id(expression.span.start_token - 1, buffer);
                    buffer.push_whitespace();
                    self.format_expression(expression.as_ref(), buffer);
                    buffer.push_whitespace();
                }
                IterationScheme::For(ident, range) => {
                    // for <ident> in
                    self.format_token_span(
                        TokenSpan::new(ident.tree.token - 1, ident.tree.token + 1),
                        buffer,
                    );
                    buffer.push_whitespace();
                    self.format_discrete_range(range, buffer);
                    buffer.push_whitespace();
                }
            }
        }
        self.format_token_id(statement.loop_token, buffer);
        self.format_sequential_statements(&statement.statements, buffer);
        buffer.line_break();
        self.format_token_span(
            TokenSpan::new(statement.end_token, span.end_token - 1),
            buffer,
        );
        self.format_token_id(span.end_token, buffer);
    }

    pub fn format_next_statement(
        &self,
        statement: &NextStatement,
        span: TokenSpan,
        buffer: &mut Buffer,
    ) {
        // next
        self.format_token_id(span.start_token, buffer);
        self.format_opt_loop_label(statement.loop_label.as_ref(), buffer);
        self.format_opt_condition(statement.condition.as_ref(), buffer);
        self.format_token_id(span.end_token, buffer);
    }

    pub fn format_exit_statement(
        &self,
        statement: &ExitStatement,
        span: TokenSpan,
        buffer: &mut Buffer,
    ) {
        // next
        self.format_token_id(span.start_token, buffer);
        self.format_opt_loop_label(statement.loop_label.as_ref(), buffer);
        self.format_opt_condition(statement.condition.as_ref(), buffer);
        self.format_token_id(span.end_token, buffer);
    }

    fn format_opt_loop_label(&self, loop_label: Option<&WithRef<Ident>>, buffer: &mut Buffer) {
        if let Some(label) = loop_label {
            buffer.push_whitespace();
            self.format_token_id(label.item.token, buffer);
        }
    }

    fn format_opt_condition(
        &self,
        condition: Option<&WithTokenSpan<Expression>>,
        buffer: &mut Buffer,
    ) {
        if let Some(condition) = &condition {
            buffer.push_whitespace();
            // when
            self.format_token_id(condition.span.start_token - 1, buffer);
            buffer.push_whitespace();
            self.format_expression(condition.as_ref(), buffer);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::formatting::test_utils::check_formatted;
    use crate::syntax::test::Code;

    fn check_statement(input: &str) {
        check_formatted(
            input,
            input,
            Code::sequential_statement,
            |formatter, ast, buffer| formatter.format_labeled_sequential_statement(ast, buffer),
        )
    }

    fn check_statements(inputs: &[&str]) {
        for input in inputs {
            check_statement(input)
        }
    }

    #[test]
    fn null_statement() {
        check_statement("null;");
    }

    #[test]
    fn return_statement() {
        check_statement("return;");
        check_statement("return 2 + 2;");
    }

    #[test]
    fn calls() {
        check_statement("something;");
        check_statement("something(arg);");
    }

    #[test]
    fn assertions() {
        check_statement("assert x;");
        check_statement("assert x report y;");
        check_statement("assert x report y severity NOTE;");
    }

    #[test]
    fn wait_statement() {
        check_statement("wait;");
        check_statement("foo: wait;");
        check_statement("wait on foo, bar;");
        check_statement("wait until a = b;");
        check_statement("wait for 2 ns;");
        check_statement("wait on foo until bar for 2 ns;");
    }

    #[test]
    fn report_statement() {
        check_statements(&["report \"message\" severity error;", "report \"message\";"]);
    }

    #[test]
    fn variable_assignment() {
        check_statements(&["foo(0) := bar(1, 2);", "name: foo(0) := bar(1, 2);"]);
    }

    #[test]
    fn signal_assignment() {
        check_statements(&["foo(0) <= bar(1, 2);", "name: foo(0) <= bar(1, 2);"]);
    }

    #[test]
    fn signal_force_assignment() {
        check_statements(&[
            "foo(0) <= force bar(1, 2);",
            "foo(0) <= force out bar(1, 2);",
            "foo(0) <= force in bar(1, 2);",
        ]);
    }

    #[test]
    fn signal_release_assignment() {
        check_statements(&[
            "foo(0) <= release;",
            "foo(0) <= release out;",
            "foo(0) <= release in;",
        ]);
    }

    #[test]
    fn if_statements() {
        check_statements(&[
            "\
if cond = true then
    foo(1, 2);
    x := 1;
end if;",
            "\
mylabel: if cond = true then
    foo(1, 2);
    x := 1;
end if mylabel;",
            "\
if cond = true then
    foo(1, 2);
else
    x := 1;
end if;",
            "\
mylabel: if cond = true then
    foo(1, 2);
else
    x := 1;
end if mylabel;",
            "\
if cond = true then
    foo(1, 2);
elsif cond2 = false then
    y := 2;
else
    x := 1;
end if;",
            "\
mylabel: if cond = true then
    foo(1, 2);
elsif cond2 = false then
    y := 2;
else
    x := 1;
end if mylabel;",
        ]);
    }

    #[test]
    fn case_statements() {
        check_statements(&[
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
case? foo(1) is
    when others =>
        null;
end case?;",
        ])
    }

    #[test]
    fn check_loop() {
        check_statements(&[
            "\
lbl: loop
end loop lbl;",
            "\
lbl: loop
    stmt1;
    stmt2;
end loop lbl;",
            "\
while foo = true loop
    stmt1;
    stmt2;
end loop;",
            "\
for idx in 0 to 3 loop
    stmt1;
    stmt2;
end loop;",
        ]);
    }

    #[test]
    fn check_next_statement() {
        check_statements(&[
            "next;",
            "next foo;",
            "next when condition;",
            "next foo when condition;",
        ]);
    }

    #[test]
    fn check_exit_statement() {
        check_statements(&[
            "exit;",
            "exit foo;",
            "exit when condition;",
            "exit foo when condition;",
        ]);
    }

    #[test]
    fn check_delay_mechanisms() {
        check_statements(&[
            "foo(0) <= transport bar(1, 2);",
            "bar <= reject 2 ns inertial bar(1, 2);",
            "bar <= inertial bar(1, 2);",
        ]);
    }

    #[test]
    fn format_selected_assignments() {
        check_statement(
            "\
with x(0) + 1 select foo(0) := bar(1, 2) when 0 | 1, def when others;",
        );
    }
}
