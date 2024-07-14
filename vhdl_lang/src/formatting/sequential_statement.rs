use crate::ast::token_range::WithTokenSpan;
use crate::ast::{LabeledSequentialStatement, SequentialStatement};
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
            Assert(assert) => {
                self.format_token_id(span.start_token, buffer);
                buffer.push(' ');
                self.format_assert_statement(assert, buffer);
                self.format_token_id(span.end_token, buffer);
            }
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
}
