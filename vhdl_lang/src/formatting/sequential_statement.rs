use crate::ast::token_range::WithTokenSpan;
use crate::ast::{
    CaseStatement, Choice, LabeledSequentialStatement, ReportStatement, SequentialStatement,
    SignalAssignment, WaitStatement,
};
use crate::{TokenAccess, TokenSpan};
use vhdl_lang::ast::{
    IfStatement, SignalForceAssignment, SignalReleaseAssignment, VariableAssignment,
};
use vhdl_lang::formatting::DesignUnitFormatter;

impl DesignUnitFormatter<'_> {
    pub fn format_sequential_statements(
        &self,
        statements: &[LabeledSequentialStatement],
        buffer: &mut String,
    ) {
        self.join_on_newline(
            statements,
            Self::format_labeled_sequential_statement,
            buffer,
        );
    }
    pub fn format_labeled_sequential_statement(
        &self,
        statement: &LabeledSequentialStatement,
        buffer: &mut String,
    ) {
        self.format_optional_label(statement.label.tree.as_ref(), buffer);
        self.format_sequential_statement(&statement.statement, buffer);
    }

    pub fn format_sequential_statement(
        &self,
        statement: &WithTokenSpan<SequentialStatement>,
        buffer: &mut String,
    ) {
        use SequentialStatement::*;
        let span = statement.span;
        match &statement.item {
            Wait(wait_statement) => self.format_wait_statement(wait_statement, span, buffer),
            Assert(assert) => {
                self.format_token_id(span.start_token, buffer);
                buffer.push(' ');
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
            Return(stmt) => {
                self.format_token_id(span.start_token, buffer);
                if let Some(expr) = &stmt.expression {
                    buffer.push(' ');
                    self.format_expression(&expr.item, expr.span, buffer);
                }
                self.format_token_id(span.end_token, buffer);
            }
            Null => self.join_token_span(span, buffer),
            _ => unimplemented!(),
        }
    }

    pub fn format_wait_statement(
        &self,
        statement: &WaitStatement,
        span: TokenSpan,
        buffer: &mut String,
    ) {
        // wait
        self.format_token_id(span.start_token, buffer);
        if let Some(name_list) = &statement.sensitivity_clause {
            buffer.push(' ');
            // on
            self.format_token_id(span.start_token + 1, buffer);
            buffer.push(' ');
            for (i, item) in name_list.items.iter().enumerate() {
                self.format_name(&item.item, item.span, buffer);
                if let Some(token) = name_list.tokens.get(i) {
                    self.format_token_id(*token, buffer);
                    buffer.push(' ');
                }
            }
        }
        if let Some(condition_clause) = &statement.condition_clause {
            buffer.push(' ');
            self.format_token_id(condition_clause.span.start_token - 1, buffer);
            buffer.push(' ');
            self.format_expression(&condition_clause.item, condition_clause.span, buffer);
        }
        if let Some(timeout_clause) = &statement.timeout_clause {
            buffer.push(' ');
            self.format_token_id(timeout_clause.span.start_token - 1, buffer);
            buffer.push(' ');
            self.format_expression(&timeout_clause.item, timeout_clause.span, buffer);
        }

        // ;
        self.format_token_id(span.end_token, buffer);
    }

    pub fn format_report_statement(
        &self,
        report: &ReportStatement,
        span: TokenSpan,
        buffer: &mut String,
    ) {
        // report
        self.format_token_id(span.start_token, buffer);
        buffer.push(' ');
        self.format_expression(&report.report.item, report.report.span, buffer);
        if let Some(severity) = &report.severity {
            buffer.push(' ');
            self.format_token_id(severity.span.start_token - 1, buffer);
            buffer.push(' ');
            self.format_expression(&severity.item, severity.span, buffer);
        }
        self.format_token_id(span.end_token, buffer);
    }

    pub fn format_variable_assignment(
        &self,
        assignment: &VariableAssignment,
        span: TokenSpan,
        buffer: &mut String,
    ) {
        self.format_target(&assignment.target, buffer);
        buffer.push(' ');
        self.format_token_id(assignment.target.span.end_token + 1, buffer);
        buffer.push(' ');
        self.format_assignment_right_hand(
            &assignment.rhs,
            |formatter, expr, buffer| formatter.format_expression(&expr.item, expr.span, buffer),
            buffer,
        );
        self.format_token_id(span.end_token, buffer);
    }

    pub fn format_signal_assignment(
        &self,
        assignment: &SignalAssignment,
        span: TokenSpan,
        buffer: &mut String,
    ) {
        self.format_target(&assignment.target, buffer);
        buffer.push(' ');
        self.format_token_id(assignment.target.span.end_token + 1, buffer);
        buffer.push(' ');
        if let Some(_delay_mechanism) = &assignment.delay_mechanism {
            unimplemented!()
        }
        self.format_assignment_right_hand(&assignment.rhs, Self::format_waveform, buffer);
        self.format_token_id(span.end_token, buffer);
    }

    pub fn format_signal_force_assignment(
        &self,
        assignment: &SignalForceAssignment,
        span: TokenSpan,
        buffer: &mut String,
    ) {
        self.format_target(&assignment.target, buffer);
        buffer.push(' ');
        // <=
        self.format_token_id(assignment.target.span.end_token + 1, buffer);
        buffer.push(' ');
        // force
        self.format_token_id(assignment.target.span.end_token + 2, buffer);
        buffer.push(' ');
        if assignment.force_mode.is_some() {
            self.format_token_id(assignment.target.span.end_token + 3, buffer);
            buffer.push(' ');
        }
        self.format_assignment_right_hand(
            &assignment.rhs,
            |formatter, expr, buffer| formatter.format_expression(&expr.item, expr.span, buffer),
            buffer,
        );
        self.format_token_id(span.end_token, buffer);
    }

    pub fn format_signal_release_assignment(
        &self,
        assignment: &SignalReleaseAssignment,
        span: TokenSpan,
        buffer: &mut String,
    ) {
        self.format_target(&assignment.target, buffer);
        buffer.push(' ');
        // <=
        self.format_token_id(assignment.target.span.end_token + 1, buffer);
        buffer.push(' ');
        // release
        self.format_token_id(assignment.target.span.end_token + 2, buffer);
        if assignment.force_mode.is_some() {
            buffer.push(' ');
            self.format_token_id(assignment.target.span.end_token + 3, buffer);
        }
        self.format_token_id(span.end_token, buffer);
    }

    pub fn format_if_statement(
        &self,
        statement: &IfStatement,
        span: TokenSpan,
        buffer: &mut String,
    ) {
        for cond in &statement.conds.conditionals {
            let condition = &cond.condition;
            // if | elsif
            self.format_token_id(condition.span.start_token - 1, buffer);
            buffer.push(' ');
            self.format_expression(&condition.item, condition.span, buffer);
            buffer.push(' ');
            // then
            self.format_token_id(condition.span.end_token + 1, buffer);
            self.increase_indentation();
            self.format_sequential_statements(&cond.item, buffer);
            self.decrease_indentation();
            self.newline(buffer);
        }
        if let Some((statements, token)) = &statement.conds.else_item {
            self.format_token_id(*token, buffer);
            self.increase_indentation();
            self.format_sequential_statements(statements, buffer);
            self.decrease_indentation();
            self.newline(buffer);
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

    pub fn format_choice(&self, choice: &WithTokenSpan<Choice>, buffer: &mut String) {
        match &choice.item {
            Choice::Expression(expr) => self.format_expression(expr, choice.span, buffer),
            Choice::DiscreteRange(range) => self.format_discrete_range(range, buffer),
            Choice::Others => self.format_token_span(choice.span, buffer),
        }
    }

    pub fn format_case_statement(
        &self,
        statement: &CaseStatement,
        span: TokenSpan,
        buffer: &mut String,
    ) {
        // case
        self.format_token_id(span.start_token, buffer);
        if statement.is_matching {
            // ?
            self.format_token_id(span.start_token + 1, buffer);
        }
        buffer.push(' ');
        self.format_expression(
            &statement.expression.item,
            statement.expression.span,
            buffer,
        );
        buffer.push(' ');
        // is
        self.format_token_id(statement.expression.span.end_token + 1, buffer);
        self.increase_indentation();
        for alternative in &statement.alternatives {
            self.newline(buffer);
            for (i, choice) in alternative.choices.iter().enumerate() {
                if i == 0 {
                    // when
                    self.format_token_id(choice.span.start_token - 1, buffer);
                    buffer.push(' ');
                }
                self.format_choice(choice, buffer);
                if i < alternative.choices.len() - 1 {
                    buffer.push(' ');
                    // |
                    self.format_token_id(choice.span.end_token + 1, buffer);
                    buffer.push(' ');
                }
                if i == alternative.choices.len() - 1 {
                    buffer.push(' ');
                    // =>
                    self.format_token_id(choice.span.end_token + 1, buffer);
                }
            }
            self.increase_indentation();
            self.format_sequential_statements(&alternative.item, buffer);
            self.decrease_indentation();
        }
        self.decrease_indentation();
        self.newline(buffer);
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
}
