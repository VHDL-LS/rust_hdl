// This Source Code Form is subject to the terms of the Mozilla Public
// Lic// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// This Source Code Form is subject to the terms of the Mozilla Public
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2024, Olof Kraigher olof.kraigher@gmail.com

use crate::ast::token_range::WithTokenSpan;
use crate::ast::{
    AssignmentRightHand, BlockStatement, CaseGenerateStatement, ConcurrentAssertStatement,
    ConcurrentSignalAssignment, ConcurrentStatement, Conditionals, ElementAssociation,
    ForGenerateStatement, GenerateBody, Ident, IfGenerateStatement, InstantiatedUnit,
    InstantiationStatement, LabeledConcurrentStatement, ProcessStatement, SensitivityList, Target,
    Waveform, WaveformElement,
};
use crate::formatting::buffer::Buffer;
use crate::formatting::VHDLFormatter;
use crate::syntax::Kind;
use crate::{HasTokenSpan, TokenAccess};
use vhdl_lang::ast::{Alternative, AssertStatement, ConcurrentProcedureCall};
use vhdl_lang::{indented, TokenSpan};

impl VHDLFormatter<'_> {
    pub fn join_on_newline<T>(
        &self,
        items: &[T],
        joiner: impl Fn(&Self, &T, &mut Buffer),
        buffer: &mut Buffer,
    ) {
        for item in items {
            buffer.line_break();
            joiner(self, item, buffer);
        }
    }

    pub fn format_concurrent_statements(
        &self,
        statements: &[LabeledConcurrentStatement],
        buffer: &mut Buffer,
    ) {
        if statements.is_empty() {
            return;
        }
        buffer.line_break();
        for (i, item) in statements.iter().enumerate() {
            self.format_labeled_concurrent_statement(item, buffer);
            if i < statements.len() - 1 {
                self.line_break_preserve_whitespace(item.statement.get_end_token(), buffer);
            }
        }
    }

    pub fn format_optional_label(&self, label: Option<&Ident>, buffer: &mut Buffer) {
        if let Some(label) = label {
            self.format_token_id(label.token, buffer);
            // :
            self.format_token_id(label.token + 1, buffer);
            buffer.push_whitespace();
        }
    }

    pub fn format_labeled_concurrent_statement(
        &self,
        statement: &LabeledConcurrentStatement,
        buffer: &mut Buffer,
    ) {
        self.format_optional_label(statement.label.tree.as_ref(), buffer);
        self.format_concurrent_statement(&statement.statement, buffer);
    }

    pub fn format_concurrent_statement(
        &self,
        statement: &WithTokenSpan<ConcurrentStatement>,
        buffer: &mut Buffer,
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
        buffer: &mut Buffer,
    ) {
        if call.postponed {
            self.format_token_id(span.start_token, buffer);
            buffer.push_whitespace();
        }
        self.format_call_or_indexed(&call.call.item, call.call.span, buffer);
        // ;
        self.format_token_id(span.end_token, buffer);
    }

    pub fn format_block_statement(
        &self,
        block: &BlockStatement,
        span: TokenSpan,
        buffer: &mut Buffer,
    ) {
        // block
        self.format_token_id(span.start_token, buffer);
        if let Some(guard_condition) = &block.guard_condition {
            self.format_token_id(guard_condition.span.start_token - 1, buffer);
            self.format_expression(guard_condition.as_ref(), buffer);
            self.format_token_id(guard_condition.span.end_token + 1, buffer);
        }
        if let Some(is_token) = block.is_token {
            buffer.push_whitespace();
            self.format_token_id(is_token, buffer);
        }
        indented!(buffer, {
            if let Some(generic_clause) = &block.header.generic_clause {
                buffer.line_break();
                self.format_interface_list(generic_clause, buffer);
            }
            if let Some(generic_map) = &block.header.generic_map {
                buffer.line_break();
                self.format_map_aspect(generic_map, buffer);
                // ;
                self.format_token_id(generic_map.span.end_token + 1, buffer);
            }
            if let Some(port_clause) = &block.header.port_clause {
                buffer.line_break();
                self.format_interface_list(port_clause, buffer);
            }
            if let Some(port_map) = &block.header.port_map {
                buffer.line_break();
                self.format_map_aspect(port_map, buffer);
                // ;
                self.format_token_id(port_map.span.end_token + 1, buffer);
            }
            self.format_declarations(&block.decl, buffer);
        });
        buffer.line_break();
        self.format_token_id(block.begin_token, buffer);
        indented!(buffer, {
            self.format_concurrent_statements(&block.statements, buffer)
        });
        buffer.line_break();
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
        buffer: &mut Buffer,
    ) {
        self.token_with_opt_postponed(span, buffer);
        if let Some(sensitivity_list) = &process.sensitivity_list {
            match &sensitivity_list.item {
                SensitivityList::Names(names) => {
                    self.format_token_id(sensitivity_list.span.start_token, buffer);
                    self.format_name_list(buffer, names);
                    self.format_token_id(sensitivity_list.span.end_token, buffer);
                }
                SensitivityList::All => self.join_token_span(sensitivity_list.span, buffer),
            }
        }
        if let Some(is_token) = process.is_token {
            buffer.push_whitespace();
            self.format_token_id(is_token, buffer);
        }
        indented!(buffer, { self.format_declarations(&process.decl, buffer) });
        buffer.line_break();
        self.format_token_id(process.begin_token, buffer);
        self.format_sequential_statements(&process.statements, buffer);
        buffer.line_break();
        self.format_token_span(
            TokenSpan::new(process.end_token, process.span.end_token - 1),
            buffer,
        );
        // ;
        self.format_token_id(process.span.end_token, buffer);
    }

    fn token_with_opt_postponed(&self, span: TokenSpan, buffer: &mut Buffer) {
        if self.tokens.index(span.start_token).kind == Kind::Postponed {
            // postponed <x>
            self.format_token_span(
                TokenSpan::new(span.start_token, span.start_token + 1),
                buffer,
            );
        } else {
            // <x>
            self.format_token_id(span.start_token, buffer);
        }
    }

    pub fn format_concurrent_assert_statement(
        &self,
        statement: &ConcurrentAssertStatement,
        span: TokenSpan,
        buffer: &mut Buffer,
    ) {
        self.token_with_opt_postponed(span, buffer);
        buffer.push_whitespace();
        self.format_assert_statement(&statement.statement, buffer);
        // ;
        self.format_token_id(span.end_token, buffer);
    }

    pub fn format_assert_statement(&self, assert_statement: &AssertStatement, buffer: &mut Buffer) {
        self.format_expression(assert_statement.condition.as_ref(), buffer);
        if let Some(report) = &assert_statement.report {
            buffer.push_whitespace();
            self.format_token_id(report.span.start_token - 1, buffer);
            buffer.push_whitespace();
            self.format_expression(report.as_ref(), buffer);
        }
        self.format_opt_severity(assert_statement.severity.as_ref(), buffer);
    }

    pub fn format_assignment_statement(
        &self,
        assignment_statement: &ConcurrentSignalAssignment,
        span: TokenSpan,
        buffer: &mut Buffer,
    ) {
        if let AssignmentRightHand::Selected(selected) = &assignment_statement.assignment.rhs {
            // with
            self.format_token_id(selected.expression.span.start_token - 1, buffer);
            buffer.push_whitespace();
            self.format_expression(selected.expression.as_ref(), buffer);
            buffer.push_whitespace();
            // select
            self.format_token_id(selected.expression.span.end_token + 1, buffer);
            buffer.push_whitespace();
        }
        self.format_target(&assignment_statement.assignment.target, buffer);
        buffer.push_whitespace();
        // <=
        self.format_token_id(
            assignment_statement.assignment.target.span.end_token + 1,
            buffer,
        );
        buffer.push_whitespace();
        if let Some(mechanism) = &assignment_statement.assignment.delay_mechanism {
            self.format_delay_mechanism(mechanism, buffer);
            buffer.push_whitespace();
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
        formatter: impl Fn(&Self, &T, &mut Buffer),
        buffer: &mut Buffer,
    ) {
        use AssignmentRightHand::*;
        match right_hand {
            Simple(simple) => formatter(self, simple, buffer),
            Conditional(conditionals) => {
                self.format_assignment_right_hand_conditionals(conditionals, formatter, buffer)
            }
            Selected(selection) => {
                for alternative in &selection.alternatives {
                    self.format_alternative(alternative, &formatter, buffer);
                    if self
                        .tokens
                        .get_token(alternative.span.end_token + 1)
                        .is_some_and(|token| token.kind == Kind::Comma)
                    {
                        self.format_token_id(alternative.span.end_token + 1, buffer);
                        buffer.push_whitespace();
                    }
                }
            }
        }
    }

    pub fn format_alternative<T>(
        &self,
        alternative: &Alternative<T>,
        formatter: &impl Fn(&Self, &T, &mut Buffer),
        buffer: &mut Buffer,
    ) {
        formatter(self, &alternative.item, buffer);
        buffer.push_whitespace();
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
        }
    }

    pub fn format_assignment_right_hand_conditionals<T>(
        &self,
        conditionals: &Conditionals<T>,
        formatter: impl Fn(&Self, &T, &mut Buffer),
        buffer: &mut Buffer,
    ) {
        for cond in &conditionals.conditionals {
            // item
            formatter(self, &cond.item, buffer);
            let condition = &cond.condition;
            buffer.push_whitespace();
            // when
            self.format_token_id(condition.span.start_token - 1, buffer);
            buffer.push_whitespace();
            self.format_expression(condition.as_ref(), buffer);
            // [else]
            if self
                .tokens
                .get_token(cond.condition.span.end_token + 1)
                .is_some_and(|token| token.kind == Kind::Else)
            {
                buffer.push_whitespace();
                self.format_token_id(cond.condition.span.end_token + 1, buffer);
                buffer.push_whitespace();
            }
        }
        if let Some((statements, _)) = &conditionals.else_item {
            // else handled above
            formatter(self, statements, buffer);
        }
    }

    pub fn format_waveform(&self, waveform: &Waveform, buffer: &mut Buffer) {
        match waveform {
            Waveform::Elements(elements) => {
                for (i, element) in elements.iter().enumerate() {
                    self.format_waveform_element(element, buffer);
                    if i < elements.len() - 1 {
                        self.format_token_id(element.get_end_token() + 1, buffer);
                        buffer.push_whitespace();
                    }
                }
            }
            Waveform::Unaffected(token) => self.format_token_id(*token, buffer),
        }
    }

    pub fn format_waveform_element(&self, element: &WaveformElement, buffer: &mut Buffer) {
        self.format_expression(element.value.as_ref(), buffer);
        if let Some(after) = &element.after {
            buffer.push_whitespace();
            self.format_token_id(after.get_start_token() - 1, buffer);
            buffer.push_whitespace();
            self.format_expression(after.as_ref(), buffer);
        }
    }

    pub fn format_target(&self, target: &WithTokenSpan<Target>, buffer: &mut Buffer) {
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
        buffer: &mut Buffer,
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
        buffer: &mut Buffer,
    ) {
        if matches!(
            self.tokens.index(span.start_token).kind,
            Kind::Component | Kind::Entity | Kind::Configuration
        ) {
            self.format_token_id(span.start_token, buffer);
            buffer.push_whitespace();
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
            indented!(buffer, {
                buffer.line_break();
                self.format_map_aspect(generic_map, buffer);
            });
        }
        if let Some(port_map) = &statement.port_map {
            indented!(buffer, {
                buffer.line_break();
                self.format_map_aspect(port_map, buffer);
            });
        }
        self.format_token_id(span.end_token, buffer);
    }

    pub fn format_for_generate_statement(
        &self,
        statement: &ForGenerateStatement,
        span: TokenSpan,
        buffer: &mut Buffer,
    ) {
        // for
        self.format_token_id(span.start_token, buffer);
        buffer.push_whitespace();
        // index
        self.format_ident(&statement.index_name, buffer);
        buffer.push_whitespace();
        // in
        self.format_token_id(statement.index_name.tree.token + 1, buffer);
        buffer.push_whitespace();
        self.format_discrete_range(&statement.discrete_range, buffer);
        buffer.push_whitespace();
        self.format_token_id(statement.generate_token, buffer);
        self.format_generate_body(&statement.body, buffer);
        buffer.line_break();
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
        buffer: &mut Buffer,
    ) {
        for cond in &statement.conds.conditionals {
            let condition = &cond.condition;
            if let Some(label) = &cond.item.alternative_label {
                // if | elsif
                self.format_token_id(label.tree.token - 1, buffer);
                buffer.push_whitespace();
                // label
                self.format_token_id(label.tree.token, buffer);
                // :
                self.format_token_id(label.tree.token + 1, buffer);
                buffer.push_whitespace();
            } else {
                self.format_token_id(condition.span.start_token - 1, buffer);
                buffer.push_whitespace();
            }
            self.format_expression(condition.as_ref(), buffer);
            buffer.push_whitespace();
            // generate
            self.format_token_id(condition.span.end_token + 1, buffer);
            self.format_generate_body(&cond.item, buffer);
            buffer.line_break();
        }
        if let Some((statements, token)) = &statement.conds.else_item {
            if let Some(label) = &statements.alternative_label {
                // else
                self.format_token_id(label.tree.token - 1, buffer);
                buffer.push_whitespace();
                // label
                self.format_token_id(label.tree.token, buffer);
                // :
                self.format_token_id(label.tree.token + 1, buffer);
                buffer.push_whitespace();
                // generate
                self.format_token_id(label.tree.token + 2, buffer);
            } else {
                // else
                self.format_token_id(*token, buffer);
                buffer.push_whitespace();
                // generate
                self.format_token_id(*token + 1, buffer);
            }
            self.format_generate_body(statements, buffer);
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

    pub fn format_case_generate_statement(
        &self,
        statement: &CaseGenerateStatement,
        span: TokenSpan,
        buffer: &mut Buffer,
    ) {
        // case
        self.format_token_id(span.start_token, buffer);
        buffer.push_whitespace();
        self.format_expression(statement.sels.expression.as_ref(), buffer);
        buffer.push_whitespace();
        // generate
        self.format_token_id(statement.sels.expression.span.end_token + 1, buffer);
        indented!(buffer, {
            for alternative in &statement.sels.alternatives {
                buffer.line_break();
                for (i, choice) in alternative.choices.iter().enumerate() {
                    if i == 0 {
                        if let Some(label) = &alternative.item.alternative_label {
                            // when
                            self.format_token_id(label.tree.token - 1, buffer);
                            buffer.push_whitespace();
                            // <ident>
                            self.format_token_id(label.tree.token, buffer);
                            // :
                            self.format_token_id(label.tree.token + 1, buffer);
                        } else {
                            // when
                            self.format_token_id(choice.span.start_token - 1, buffer);
                        }
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
                self.format_generate_body(&alternative.item, buffer);
            }
        });
        buffer.line_break();
        self.format_token_span(
            TokenSpan::new(statement.end_token, span.end_token - 1),
            buffer,
        );
        self.format_token_id(span.end_token, buffer);
    }

    pub fn format_generate_body(&self, generate_body: &GenerateBody, buffer: &mut Buffer) {
        if let Some((decl, begin_token)) = &generate_body.decl {
            indented!(buffer, { self.format_declarations(decl, buffer) });
            buffer.line_break();
            self.format_token_id(*begin_token, buffer);
        }
        indented!(buffer, {
            self.format_concurrent_statements(&generate_body.statements, buffer)
        });
        if let Some(end_token) = generate_body.end_token {
            buffer.line_break();
            self.format_token_id(end_token, buffer);
            if let Some(token) = generate_body.end_label {
                buffer.push_whitespace();
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

    #[test]
    fn format_selected_assignments() {
        check_statement(
            "\
with x(0) + 1 select foo(0) <= bar(1, 2) when 0 | 1, def when others;",
        );
        check_statement(
            "\
with x(0) + 1 select foo(0) <= transport bar(1, 2) after 2 ns when 0 | 1, def when others;",
        );
    }

    #[test]
    fn format_case_generate_statements() {
        check_statement(
            "\
gen: case expr(0) + 2 generate
    when 1 | 2 =>
        sig <= value;
    when others =>
        foo(clk);
end generate;",
        );
        check_statement(
            "\
gen1: case expr(0) + 2 generate
    when alt1: 1 | 2 =>
        sig <= value;
    when alt2: others =>
        foo(clk);
end generate gen1;",
        );
    }
}
