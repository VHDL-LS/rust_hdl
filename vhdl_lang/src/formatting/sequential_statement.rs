use crate::ast::token_range::WithTokenSpan;
use crate::ast::{LabeledSequentialStatement, ReportStatement, SequentialStatement, WaitStatement};
use crate::TokenSpan;
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
            ProcedureCall(call_or_indexed) => {
                self.format_call_or_indexed(&call_or_indexed.item, call_or_indexed.span, buffer);
                self.format_token_id(span.end_token, buffer);
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
        check_statement("report \"message\" severity error;");
        check_statement("report \"message\";");
    }
}
