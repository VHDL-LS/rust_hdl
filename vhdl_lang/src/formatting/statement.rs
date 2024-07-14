use crate::ast::token_range::WithTokenSpan;
use crate::ast::{BlockStatement, CallOrIndexed, ConcurrentStatement, LabeledConcurrentStatement};
use crate::syntax::Kind;
use crate::TokenAccess;
use vhdl_lang::ast::ConcurrentProcedureCall;
use vhdl_lang::TokenSpan;

impl crate::formatting::DesignUnitFormatter<'_> {
    pub fn format_concurrent_statements(
        &self,
        statements: &[LabeledConcurrentStatement],
        buffer: &mut String,
    ) {
        for (i, statement) in statements.iter().enumerate() {
            self.format_labeled_concurrent_statement(statement, buffer);
            if i < statements.len() - 1 {
                self.newline(buffer);
            }
        }
    }

    pub fn format_labeled_concurrent_statement(
        &self,
        statement: &LabeledConcurrentStatement,
        buffer: &mut String,
    ) {
        if let Some(label) = &statement.label.tree {
            self.format_token_id(label.token, buffer);
            // :
            self.format_token_id(label.token + 1, buffer);
            buffer.push(' ');
        }
        self.format_concurrent_statement(&statement.statement, buffer);
        self.format_token_id(statement.statement.span.end_token, buffer);
    }

    pub fn format_concurrent_statement(
        &self,
        statement: &WithTokenSpan<ConcurrentStatement>,
        buffer: &mut String,
    ) {
        use ConcurrentStatement::*;
        let span = statement.span;
        match &statement.item {
            ProcedureCall(call) => self.format_procedure_call(call, span, buffer),
            Block(block) => self.format_block(block, span, buffer),
            _ => unimplemented!(),
        }
    }

    pub fn format_procedure_call(
        &self,
        call: &ConcurrentProcedureCall,
        span: TokenSpan,
        buffer: &mut String,
    ) {
        if call.postponed {
            self.format_token_id(span.start_token, buffer);
            buffer.push(' ');
        }
        self.format_call_or_indexed(&call.call, buffer);
    }

    pub fn format_block(&self, block: &BlockStatement, span: TokenSpan, buffer: &mut String) {}

    pub fn format_call_or_indexed(&self, call: &WithTokenSpan<CallOrIndexed>, buffer: &mut String) {
        self.format_name(&call.item.name.item, call.item.name.span, buffer);
        let open_paren = call.item.name.span.end_token + 1;
        if self.tokens.get_token(open_paren).kind == Kind::LeftPar {
            self.format_token_id(open_paren, buffer);
        }
        for (i, parameter) in call.item.parameters.items.iter().enumerate() {
            self.format_association_element(parameter, buffer);
            if let Some(token) = call.item.parameters.tokens.get(i) {
                self.format_token_id(*token, buffer);
                buffer.push(' ');
            }
        }
        let close_paren = call.span.end_token;
        if self.tokens.get_token(close_paren).kind == Kind::RightPar {
            self.format_token_id(close_paren, buffer);
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
            Code::concurrent_statement,
            |formatter, ast, buffer| formatter.format_labeled_concurrent_statement(ast, buffer),
        )
    }

    #[test]
    fn procedure_calls() {
        check_statement("foo;");
        check_statement("foo(clk);");
        check_statement("foo(clk, bar);");
        check_statement("foo(0);");
        check_statement("foo(arg => 0);");
    }
}
