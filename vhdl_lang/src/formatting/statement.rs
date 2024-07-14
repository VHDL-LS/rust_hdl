use crate::ast::token_range::WithTokenSpan;
use crate::ast::{
    AssignmentRightHand, BlockStatement, ConcurrentAssertStatement, ConcurrentSignalAssignment,
    ConcurrentStatement, LabeledConcurrentStatement, LabeledSequentialStatement, ProcessStatement,
    SensitivityList, Target, Waveform, WaveformElement,
};
use crate::formatting::DesignUnitFormatter;
use crate::syntax::Kind;
use crate::{HasTokenSpan, TokenAccess};
use vhdl_lang::ast::{AssertStatement, ConcurrentProcedureCall};
use vhdl_lang::TokenSpan;

impl DesignUnitFormatter<'_> {
    pub fn join_on_newline<T>(
        &self,
        items: &[T],
        joiner: impl Fn(&Self, &T, &mut String),
        buffer: &mut String,
    ) {
        for item in items {
            self.newline(buffer);
            joiner(self, item, buffer);
        }
    }

    pub fn format_concurrent_statements(
        &self,
        statements: &[LabeledConcurrentStatement],
        buffer: &mut String,
    ) {
        self.join_on_newline(
            statements,
            Self::format_labeled_concurrent_statement,
            buffer,
        );
    }

    pub fn format_sequential_statements(
        &self,
        statements: &[LabeledSequentialStatement],
        buffer: &mut String,
    ) {
        self.join_on_newline(statements, Self::format_sequential_statement, buffer);
    }

    pub fn format_sequential_statement(
        &self,
        statement: &LabeledSequentialStatement,
        buffer: &mut String,
    ) {
        unimplemented!()
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
            Block(block) => self.format_block_statement(block, span, buffer),
            Process(process) => self.format_process_statement(process, span, buffer),
            Assert(assert) => self.format_concurrent_assert_statement(assert, span, buffer),
            Assignment(assignment) => self.format_assignment_statement(assignment, span, buffer),
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
        self.format_call_or_indexed(&call.call.item, call.call.span, buffer);
        // ;
        self.format_token_id(span.end_token, buffer);
    }

    pub fn format_block_statement(
        &self,
        block: &BlockStatement,
        span: TokenSpan,
        buffer: &mut String,
    ) {
        // block
        self.format_token_id(span.start_token, buffer);
        if let Some(guard_condition) = &block.guard_condition {
            self.format_token_id(guard_condition.span.start_token - 1, buffer);
            self.format_expression(&guard_condition.item, guard_condition.span, buffer);
            self.format_token_id(guard_condition.span.end_token + 1, buffer);
        }
        if let Some(is_token) = block.is_token {
            buffer.push(' ');
            self.format_token_id(is_token, buffer);
        }
        self.increase_indentation();
        if let Some(generic_clause) = &block.header.generic_clause {
            self.newline(buffer);
            self.format_interface_list(generic_clause, buffer);
        }
        if let Some(generic_map) = &block.header.generic_map {
            self.newline(buffer);
            self.format_map_aspect(generic_map, buffer);
            // ;
            self.format_token_id(generic_map.span.end_token + 1, buffer);
        }
        if let Some(port_clause) = &block.header.port_clause {
            self.newline(buffer);
            self.format_interface_list(port_clause, buffer);
        }
        if let Some(port_map) = &block.header.port_map {
            self.newline(buffer);
            self.format_map_aspect(port_map, buffer);
            // ;
            self.format_token_id(port_map.span.end_token + 1, buffer);
        }
        self.format_declarations(&block.decl, buffer);
        self.decrease_indentation();
        self.newline(buffer);
        self.format_token_id(block.begin_token, buffer);
        self.newline(buffer);
        self.increase_indentation();
        self.format_concurrent_statements(&block.statements, buffer);
        self.decrease_indentation();
        self.format_token_span(
            TokenSpan::new(block.end_token, block.span.end_token - 1),
            buffer,
        );
        // ;
        self.format_token_id(block.span.end_token, buffer);
    }

    pub fn format_process_statement(
        &self,
        process: &ProcessStatement,
        span: TokenSpan,
        buffer: &mut String,
    ) {
        if self.tokens.get_token(span.start_token).kind == Kind::Postponed {
            // postponed process
            self.format_token_span(
                TokenSpan::new(span.start_token, span.start_token + 1),
                buffer,
            );
        } else {
            // process
            self.format_token_id(span.start_token, buffer);
        }
        if let Some(sensitivity_list) = &process.sensitivity_list {
            match &sensitivity_list.item {
                SensitivityList::Names(names) => {
                    self.format_token_id(sensitivity_list.span.start_token, buffer);
                    for (i, name) in names.iter().enumerate() {
                        self.format_name(&name.item, name.span, buffer);
                        if i < names.len() - 1 {
                            self.format_token_id(name.span.end_token + 1, buffer);
                            buffer.push(' ');
                        }
                    }
                    self.format_token_id(sensitivity_list.span.end_token, buffer);
                }
                SensitivityList::All => self.join_token_span(sensitivity_list.span, buffer),
            }
        }
        if let Some(is_token) = process.is_token {
            buffer.push(' ');
            self.format_token_id(is_token, buffer);
        }
        self.increase_indentation();
        self.format_declarations(&process.decl, buffer);
        self.decrease_indentation();
        self.newline(buffer);
        self.format_token_id(process.begin_token, buffer);
        self.newline(buffer);
        self.increase_indentation();
        self.format_sequential_statements(&process.statements, buffer);
        self.decrease_indentation();
        self.format_token_span(
            TokenSpan::new(process.end_token, process.span.end_token - 1),
            buffer,
        );
        // ;
        self.format_token_id(process.span.end_token, buffer);
    }

    pub fn format_concurrent_assert_statement(
        &self,
        statement: &ConcurrentAssertStatement,
        span: TokenSpan,
        buffer: &mut String,
    ) {
        if self.tokens.get_token(span.start_token).kind == Kind::Postponed {
            // postponed assert
            self.format_token_span(
                TokenSpan::new(span.start_token, span.start_token + 1),
                buffer,
            );
        } else {
            // assert
            self.format_token_id(span.start_token, buffer);
        }
        buffer.push(' ');
        self.format_assert_statement(&statement.statement, buffer);
        // ;
        self.format_token_id(span.end_token, buffer);
    }

    pub fn format_assert_statement(&self, assert_statement: &AssertStatement, bufer: &mut String) {
        self.format_expression(
            &assert_statement.condition.item,
            assert_statement.condition.span,
            bufer,
        );
        if let Some(report) = &assert_statement.report {
            bufer.push(' ');
            self.format_token_id(report.span.start_token - 1, bufer);
            bufer.push(' ');
            self.format_expression(&report.item, report.span, bufer);
        }
        if let Some(severity) = &assert_statement.severity {
            bufer.push(' ');
            self.format_token_id(severity.span.start_token - 1, bufer);
            bufer.push(' ');
            self.format_expression(&severity.item, severity.span, bufer);
        }
    }

    pub fn format_assignment_statement(
        &self,
        assignment_statement: &ConcurrentSignalAssignment,
        span: TokenSpan,
        buffer: &mut String,
    ) {
        self.format_target(&assignment_statement.target, buffer);
        buffer.push(' ');
        // <=
        self.format_token_id(assignment_statement.target.span.end_token + 1, buffer);
        buffer.push(' ');
        if let Some(_mechanism) = &assignment_statement.delay_mechanism {
            unimplemented!()
        }
        self.format_assignment_right_hand(&assignment_statement.rhs, Self::format_waveform, buffer);
        self.format_token_id(span.end_token, buffer);
    }

    pub fn format_assignment_right_hand<T>(
        &self,
        right_hand: &AssignmentRightHand<T>,
        formatter: impl Fn(&Self, &T, &mut String),
        buffer: &mut String,
    ) {
        match right_hand {
            AssignmentRightHand::Simple(simple) => formatter(self, simple, buffer),
            AssignmentRightHand::Conditional(_) => unimplemented!(),
            AssignmentRightHand::Selected(_) => unimplemented!(),
        }
    }

    pub fn format_waveform(&self, waveform: &Waveform, buffer: &mut String) {
        match waveform {
            Waveform::Elements(elements) => {
                for (i, element) in elements.iter().enumerate() {
                    self.format_waveform_element(element, buffer);
                    if i < elements.len() - 1 {
                        self.format_token_id(element.get_end_token() + 1, buffer);
                        buffer.push(' ');
                    }
                }
            }
            Waveform::Unaffected(token) => self.format_token_id(*token, buffer),
        }
    }

    pub fn format_waveform_element(&self, element: &WaveformElement, buffer: &mut String) {
        self.format_expression(&element.value.item, element.value.span, buffer);
        if let Some(after) = &element.after {
            buffer.push(' ');
            self.format_token_id(after.get_start_token() - 1, buffer);
            buffer.push(' ');
            self.format_expression(&after.item, after.span, buffer);
        }
    }

    pub fn format_target(&self, target: &WithTokenSpan<Target>, buffer: &mut String) {
        match &target.item {
            Target::Name(name) => self.format_name(name, target.span, buffer),
            Target::Aggregate(_) => unimplemented!(),
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

    #[test]
    fn blocks() {
        check_statement(
            "\
name: block
begin
end block name;",
        );
        check_statement(
            "\
name: block is
begin
end block name;",
        );
        check_statement(
            "\
name: block(cond = true)
begin
end block;",
        );
        check_statement(
            "\
name: block(cond = true) is
begin
end block;",
        );
        check_statement(
            "\
block(cond = true) is
begin
end block;",
        );
        check_statement(
            "\
name: block is
    generic (
        gen: integer := 1
    );
    generic map (
        gen => 1
    );
    port (
        prt: integer := 1
    );
    port map (
        prt => 2
    );
begin
end block;",
        );
    }

    #[test]
    fn check_processes() {
        check_statement(
            "\
process
begin
end process;",
        );
        check_statement(
            "\
name: process is
begin
end process name;",
        );
        check_statement(
            "\
postponed process
begin
end process;",
        );
        check_statement(
            "\
postponed process
begin
end postponed process;",
        );
        check_statement(
            "\
process(clk, vec(1)) is
begin
end process;",
        );
        check_statement(
            "\
process(all) is
    variable foo: boolean;
begin
end process;",
        );
    }

    #[test]
    fn check_assert() {
        check_statement("assert false;");
        check_statement("assert cond = true;");
        check_statement("postponed assert cond = true;");
        check_statement("assert false report \"message\" severity error;");
    }

    #[test]
    fn check_signal_assignment() {
        check_statement("foo <= bar(2 to 3);");
        check_statement("x <= bar(1 to 3) after 2 ns;");
        check_statement("foo <= bar(1 to 3) after 2 ns, expr after 1 ns;");
    }
}
