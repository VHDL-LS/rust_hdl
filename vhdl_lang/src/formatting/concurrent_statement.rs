use crate::ast::token_range::WithTokenSpan;
use crate::ast::{
    AssignmentRightHand, BlockStatement, CaseGenerateStatement, ConcurrentAssertStatement,
    ConcurrentSignalAssignment, ConcurrentStatement, Conditionals, ElementAssociation,
    ForGenerateStatement, GenerateBody, Ident, IfGenerateStatement, InstantiatedUnit,
    InstantiationStatement, LabeledConcurrentStatement, ProcessStatement, SensitivityList, Target,
    Waveform, WaveformElement,
};
use crate::formatting::VHDLFormatter;
use crate::syntax::Kind;
use crate::{HasTokenSpan, TokenAccess};
use vhdl_lang::ast::{AssertStatement, ConcurrentProcedureCall, Selection};
use vhdl_lang::TokenSpan;

impl VHDLFormatter<'_> {
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

    pub fn format_optional_label(&self, label: Option<&Ident>, buffer: &mut String) {
        if let Some(label) = label {
            self.format_token_id(label.token, buffer);
            // :
            self.format_token_id(label.token + 1, buffer);
            buffer.push(' ');
        }
    }

    pub fn format_labeled_concurrent_statement(
        &self,
        statement: &LabeledConcurrentStatement,
        buffer: &mut String,
    ) {
        self.format_optional_label(statement.label.tree.as_ref(), buffer);
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
            Instance(instantiation_statement) => {
                self.format_instantiation_statement(instantiation_statement, span, buffer)
            }
            ForGenerate(for_generate) => {
                self.format_for_generate_statement(for_generate, span, buffer)
            }
            IfGenerate(if_generate) => self.format_if_generate_statement(if_generate, span, buffer),
            CaseGenerate(case_generate) => {
                self.format_case_generate_statement(case_generate, span, buffer)
            }
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
            self.format_expression(guard_condition.as_ref(), buffer);
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
                        self.format_name(name.as_ref(), buffer);
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
        self.increase_indentation();
        self.format_sequential_statements(&process.statements, buffer);
        self.decrease_indentation();
        self.newline(buffer);
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
        self.format_expression(assert_statement.condition.as_ref(), bufer);
        if let Some(report) = &assert_statement.report {
            bufer.push(' ');
            self.format_token_id(report.span.start_token - 1, bufer);
            bufer.push(' ');
            self.format_expression(report.as_ref(), bufer);
        }
        if let Some(severity) = &assert_statement.severity {
            bufer.push(' ');
            self.format_token_id(severity.span.start_token - 1, bufer);
            bufer.push(' ');
            self.format_expression(severity.as_ref(), bufer);
        }
    }

    pub fn format_assignment_statement(
        &self,
        assignment_statement: &ConcurrentSignalAssignment,
        span: TokenSpan,
        buffer: &mut String,
    ) {
        self.format_target(&assignment_statement.assignment.target, buffer);
        buffer.push(' ');
        // <=
        self.format_token_id(
            assignment_statement.assignment.target.span.end_token + 1,
            buffer,
        );
        buffer.push(' ');
        if let Some(mechanism) = &assignment_statement.assignment.delay_mechanism {
            self.format_delay_mechanism(mechanism, buffer);
            buffer.push(' ');
        }
        self.format_assignment_right_hand(
            &assignment_statement.assignment.rhs,
            Self::format_waveform,
            buffer,
        );
        self.format_token_id(span.end_token, buffer);
    }

    pub fn format_assignment_right_hand<T>(
        &self,
        right_hand: &AssignmentRightHand<T>,
        formatter: impl Fn(&Self, &T, &mut String),
        buffer: &mut String,
    ) {
        use AssignmentRightHand::*;
        match right_hand {
            Simple(simple) => formatter(self, simple, buffer),
            Conditional(conditionals) => {
                self.format_assignment_right_hand_conditionals(conditionals, formatter, buffer)
            }
            Selected(_) => unimplemented!(),
        }
    }

    pub fn format_assignment_right_hand_conditionals<T>(
        &self,
        conds: &Conditionals<T>,
        formatter: impl Fn(&Self, &T, &mut String),
        buffer: &mut String,
    ) {
        for cond in &conds.conditionals {
            // item
            formatter(self, &cond.item, buffer);
            let condition = &cond.condition;
            buffer.push(' ');
            // when
            self.format_token_id(condition.span.start_token - 1, buffer);
            buffer.push(' ');
            self.format_expression(condition.as_ref(), buffer);
            // [else]
            if self
                .tokens
                .get_token(cond.condition.span.end_token + 1)
                .kind
                == Kind::Else
            {
                buffer.push(' ');
                self.format_token_id(cond.condition.span.end_token + 1, buffer);
                buffer.push(' ');
            }
        }
        if let Some((statements, _)) = &conds.else_item {
            // else handled above
            formatter(self, statements, buffer);
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
        self.format_expression(element.value.as_ref(), buffer);
        if let Some(after) = &element.after {
            buffer.push(' ');
            self.format_token_id(after.get_start_token() - 1, buffer);
            buffer.push(' ');
            self.format_expression(after.as_ref(), buffer);
        }
    }

    pub fn format_target(&self, target: &WithTokenSpan<Target>, buffer: &mut String) {
        match &target.item {
            Target::Name(name) => self.format_name(WithTokenSpan::new(name, target.span), buffer),
            Target::Aggregate(associations) => {
                self.format_target_aggregate(associations, target.span, buffer)
            }
        }
    }

    pub fn format_target_aggregate(
        &self,
        associations: &[WithTokenSpan<ElementAssociation>],
        span: TokenSpan,
        buffer: &mut String,
    ) {
        // (
        self.format_token_id(span.start_token, buffer);
        self.format_element_associations(associations, buffer);
        // )
        self.format_token_id(span.end_token, buffer);
    }

    pub fn format_instantiation_statement(
        &self,
        statement: &InstantiationStatement,
        span: TokenSpan,
        buffer: &mut String,
    ) {
        if matches!(
            self.tokens.get_token(span.start_token).kind,
            Kind::Component | Kind::Entity | Kind::Configuration
        ) {
            self.format_token_id(span.start_token, buffer);
            buffer.push(' ');
        }
        match &statement.unit {
            InstantiatedUnit::Component(name) | InstantiatedUnit::Configuration(name) => {
                self.format_name(name.as_ref(), buffer);
            }
            InstantiatedUnit::Entity(name, architecture) => {
                self.format_name(name.as_ref(), buffer);
                if let Some(arch) = architecture {
                    self.join_token_span(
                        TokenSpan::new(arch.item.token - 1, arch.item.token + 1),
                        buffer,
                    )
                }
            }
        }
        if let Some(generic_map) = &statement.generic_map {
            self.increase_indentation();
            self.newline(buffer);
            self.format_map_aspect(generic_map, buffer);
            self.decrease_indentation();
        }
        if let Some(port_map) = &statement.port_map {
            self.increase_indentation();
            self.newline(buffer);
            self.format_map_aspect(port_map, buffer);
            self.decrease_indentation();
        }
        self.format_token_id(span.end_token, buffer);
    }

    pub fn format_for_generate_statement(
        &self,
        statement: &ForGenerateStatement,
        span: TokenSpan,
        buffer: &mut String,
    ) {
        // for
        self.format_token_id(span.start_token, buffer);
        buffer.push(' ');
        // index
        self.format_ident(&statement.index_name, buffer);
        buffer.push(' ');
        // in
        self.format_token_id(statement.index_name.tree.token + 1, buffer);
        buffer.push(' ');
        self.format_discrete_range(&statement.discrete_range, buffer);
        buffer.push(' ');
        self.format_token_id(statement.generate_token, buffer);
        self.format_generate_body(&statement.body, buffer);
        self.newline(buffer);
        self.format_token_span(
            TokenSpan::new(statement.end_token, span.end_token - 1),
            buffer,
        );
        self.format_token_id(span.end_token, buffer);
    }

    pub fn format_if_generate_statement(
        &self,
        statement: &IfGenerateStatement,
        span: TokenSpan,
        buffer: &mut String,
    ) {
        for cond in &statement.conds.conditionals {
            let condition = &cond.condition;
            if let Some(label) = &cond.item.alternative_label {
                // if | elsif
                self.format_token_id(label.tree.token - 1, buffer);
                buffer.push(' ');
                // label
                self.format_token_id(label.tree.token, buffer);
                // :
                self.format_token_id(label.tree.token + 1, buffer);
                buffer.push(' ');
            } else {
                self.format_token_id(condition.span.start_token - 1, buffer);
                buffer.push(' ');
            }
            self.format_expression(condition.as_ref(), buffer);
            buffer.push(' ');
            // generate
            self.format_token_id(condition.span.end_token + 1, buffer);
            self.format_generate_body(&cond.item, buffer);
            self.newline(buffer);
        }
        if let Some((statements, token)) = &statement.conds.else_item {
            if let Some(label) = &statements.alternative_label {
                // else
                self.format_token_id(label.tree.token - 1, buffer);
                buffer.push(' ');
                // label
                self.format_token_id(label.tree.token, buffer);
                // :
                self.format_token_id(label.tree.token + 1, buffer);
                buffer.push(' ');
                // generate
                self.format_token_id(label.tree.token + 2, buffer);
            } else {
                // else
                self.format_token_id(*token, buffer);
                buffer.push(' ');
                // generate
                self.format_token_id(*token + 1, buffer);
            }
            self.format_generate_body(statements, buffer);
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

    pub fn format_case_generate_statement(
        &self,
        statement: &CaseGenerateStatement,
        span: TokenSpan,
        buffer: &mut String,
    ) {
        unimplemented!()
    }

    pub fn format_generate_body(&self, generate_body: &GenerateBody, buffer: &mut String) {
        self.increase_indentation();
        if let Some((decl, begin_token)) = &generate_body.decl {
            self.format_declarations(decl, buffer);
            self.decrease_indentation();
            self.newline(buffer);
            self.format_token_id(*begin_token, buffer);
            self.increase_indentation();
        }
        self.format_concurrent_statements(&generate_body.statements, buffer);
        self.decrease_indentation();
        if let Some(end_token) = generate_body.end_token {
            self.newline(buffer);
            self.format_token_id(end_token, buffer);
            if let Some(token) = generate_body.end_label {
                buffer.push(' ');
                self.format_token_id(token, buffer);
                // ;
                self.format_token_id(token + 1, buffer);
            } else {
                // ;
                self.format_token_id(end_token + 1, buffer);
            }
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

    #[test]
    fn check_simple_instantiation_statement() {
        check_statement("inst: component lib.foo.bar;");
        check_statement("inst: configuration lib.foo.bar;");
        check_statement("inst: entity lib.foo.bar;");
        check_statement("inst: entity lib.foo.bar(arch);");
    }

    #[test]
    fn check_instantiation_statement_generic_map() {
        check_statement(
            "\
inst: component lib.foo.bar
    generic map (
        const => 1
    );",
        );
        check_statement(
            "\
inst: component lib.foo.bar
    port map (
        clk => clk_foo
    );",
        );
        check_statement(
            "\
inst: component lib.foo.bar
    generic map (
        const => 1
    )
    port map (
        clk => clk_foo
    );",
        );
    }

    #[test]
    fn format_for_generate_statement() {
        check_statement(
            "\
gen: for idx in 0 to 1 generate
end generate;",
        );
        check_statement(
            "\
gen: for idx in 0 to 1 generate
    foo <= bar;
end generate;",
        );
        check_statement(
            "\
gen: for idx in 0 to 1 generate
begin
    foo <= bar;
end generate;",
        );
        check_statement(
            "\
gen: for idx in 0 to 1 generate
begin
    foo <= bar;
end;
end generate;",
        );
        check_statement(
            "\
gen: for idx in 0 to 1 generate
    signal foo: natural;
begin
    foo <= bar;
end generate;",
        );
        check_statement(
            "\
gen: for idx in 0 to 1 generate
    signal foo: natural;
begin
    foo <= bar;
end;
end generate;",
        );
    }

    #[test]
    fn format_if_generate_statement() {
        check_statement(
            "\
gen: if cond = true generate
end generate;",
        );
        check_statement(
            "\
gen: if cond = true generate
begin
end generate;",
        );
        check_statement(
            "\
gen: if cond = true generate
elsif cond2 = true generate
else generate
end generate;",
        );
        check_statement(
            "\
gen: if cond = true generate
    variable v1: boolean;
begin
    foo1(clk);
elsif cond2 = true generate
    variable v2: boolean;
begin
    foo2(clk);
else generate
    variable v3: boolean;
begin
    foo3(clk);
end generate;",
        );
        check_statement(
            "\
gen: if alt1: cond = true generate
end alt1;
elsif alt2: cond2 = true generate
end alt2;
else alt3: generate
end alt3;
end generate;",
        );
    }

    #[test]
    fn format_conditional_assignment() {
        check_statement("foo(0) <= bar(1, 2) when cond = true;");
        check_statement("foo(0) <= bar(1, 2) when cond = true else expr2 when cond2;");
        check_statement("foo(0) <= bar(1, 2) when cond = true else expr2;");
        check_statement("foo(0) <= bar(1, 2) after 2 ns when cond;");
    }

    #[test]
    fn format_aggregate_assignments() {
        check_statement("(foo, 1 => bar) <= integer_vector'(1, 2);");
    }
}
