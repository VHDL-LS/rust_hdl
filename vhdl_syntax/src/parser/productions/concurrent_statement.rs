// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind::*;
use crate::tokens::token_kind::Keyword as Kw;
use crate::tokens::TokenKind::*;
use crate::tokens::TokenStream;

impl<T: TokenStream> Parser<T> {
    fn block_statement_inner(&mut self) {
        self.expect_kw(Kw::Block);
        if self.next_is(LeftPar) {
            self.start_node(ParenthesizedExpression);
            self.skip();
            self.condition();
            self.expect_token(RightPar);
            self.end_node();
        }
        self.opt_token(Keyword(Kw::Is));
        self.block_header();
        self.block_declarative_part();
        self.expect_kw(Kw::Begin);
        self.concurrent_statements();
        self.expect_tokens([Keyword(Kw::End), Keyword(Kw::Block)]);
        self.opt_identifier();
    }

    pub fn block_header(&mut self) {
        self.start_node(BlockHeader);
        self.opt_generic_clause();
        let checkpoint = self.checkpoint();
        if self.opt_generic_map_aspect() {
            self.start_node_at(checkpoint, SemiColonTerminatedGenericMapAspect);
            self.expect_token(SemiColon);
            self.end_node();
        }
        self.opt_port_clause();
        let checkpoint = self.checkpoint();
        if self.opt_port_map_aspect() {
            self.start_node_at(checkpoint, SemiColonTerminatedPortMapAspect);
            self.expect_token(SemiColon);
            self.end_node();
        }
        self.end_node();
    }

    pub fn block_declarative_part(&mut self) {
        self.declarative_part()
    }

    pub(crate) fn concurrent_statements(&mut self) {
        loop {
            match self.peek_token() {
                Some(Keyword(Kw::End | Kw::Elsif | Kw::Else | Kw::When)) | None => {
                    return;
                }
                _ => self.concurrent_statement(),
            }
        }
    }

    pub(crate) fn component_instantiated_unit(&mut self) {
        self.start_node(ComponentInstantiatedUnit);
        self.opt_token(Keyword(Kw::Component));
        self.name();
        self.end_node();
    }

    pub(crate) fn entity_instantiated_unit(&mut self) {
        self.start_node(EntityInstantiatedUnit);
        self.expect_kw(Kw::Entity);
        self.name();
        self.end_node();
    }

    pub(crate) fn configuration_instantiated_unit(&mut self) {
        self.start_node(ConfigurationInstantiatedUnit);
        self.expect_kw(Kw::Configuration);
        self.name();
        self.end_node();
    }

    pub(crate) fn concurrent_statement(&mut self) {
        let checkpoint = self.checkpoint();
        self.opt_label();
        self.opt_token(Keyword(Kw::Postponed));
        match self.peek_token() {
            Some(Keyword(Kw::Block)) => {
                self.start_node_at(checkpoint, BlockStatement);
                self.block_statement_inner();
            }
            Some(Keyword(Kw::Process)) => {
                self.start_node_at(checkpoint, ProcessStatement);
                self.process_statement_inner();
            }
            Some(Keyword(Kw::Component)) => {
                self.start_node_at(checkpoint, ComponentInstantiationStatement);
                self.component_instantiated_unit();
                self.instantiation_statement_inner();
            }
            Some(Keyword(Kw::Configuration)) => {
                self.start_node_at(checkpoint, ComponentInstantiationStatement);
                self.configuration_instantiated_unit();
                self.instantiation_statement_inner();
            }
            Some(Keyword(Kw::Entity)) => {
                self.start_node_at(checkpoint, ComponentInstantiationStatement);
                self.entity_instantiated_unit();
                self.instantiation_statement_inner();
            }
            Some(Keyword(Kw::For)) => {
                self.start_node_at(checkpoint, ForGenerateStatement);
                self.for_generate_statement_inner();
            }
            Some(Keyword(Kw::If)) => {
                self.start_node_at(checkpoint, IfGenerateStatement);
                self.if_generate_statement_inner();
            }
            Some(Keyword(Kw::Case)) => {
                self.start_node_at(checkpoint, CaseGenerateStatement);
                self.case_generate_statement_inner();
            }
            Some(Keyword(Kw::Assert)) => {
                self.start_node_at(checkpoint, ConcurrentAssertionStatement);
                self.assertion();
            }
            Some(Keyword(Kw::With)) => {
                self.start_node_at(checkpoint, ConcurrentSelectedSignalAssignment);
                self.concurrent_selected_signal_assignment_inner();
            }
            Some(Identifier | LtLt | StringLiteral | CharacterLiteral) => {
                let checkpoint2 = self.checkpoint();
                self.name();
                match self.peek_token() {
                    Some(LTE) => {
                        self.start_node_at(checkpoint2, NameTarget);
                        self.end_node();
                        self.skip();
                        self.opt_token(Keyword(Kw::Guarded));
                        self.opt_delay_mechanism();
                        let waveform_checkpoint = self.checkpoint();
                        self.waveform();
                        if self.opt_token(Keyword(Kw::When)) {
                            self.start_node_at(checkpoint, ConcurrentConditionalSignalAssignment);
                            self.start_node_at(waveform_checkpoint, ConditionalWaveforms);
                            self.conditional_waveforms_after_first_when();
                            self.end_node();
                        } else {
                            self.start_node_at(checkpoint, ConcurrentSimpleSignalAssignment);
                        }
                    }
                    Some(Keyword(Kw::Port | Kw::Generic)) => {
                        self.start_node_at(checkpoint2, ComponentInstantiatedUnit);
                        self.end_node();
                        self.start_node_at(checkpoint, ComponentInstantiationStatement);
                        self.instantiation_statement_inner();
                    }
                    // Could be an instantiated unit without ports and generics
                    // or a procedure call
                    _ => self.start_node_at(
                        checkpoint,
                        ConcurrentProcedureCallOrComponentInstantiationStatement,
                    ),
                }
            }
            _ => self.expect_tokens_err([Keyword(Kw::Block)]),
        }
        self.expect_token(SemiColon);
        self.end_node();
    }

    /// Parse conditional waveforms, assuming the first `waveform when` is already parsed
    fn conditional_waveforms_after_first_when(&mut self) {
        self.expression();
        while self.next_is(Keyword(Kw::Else)) {
            let checkpoint = self.checkpoint();
            self.expect_kw(Kw::Else);
            self.waveform();
            if self.opt_token(Keyword(Kw::When)) {
                self.start_node_at(checkpoint, ConditionalWaveformElseWhenExpression);
                self.expression();
                self.end_node();
            } else {
                self.start_node_at(checkpoint, ConditionalWaveformElseItem);
                self.end_node();
                break;
            }
        }
    }

    fn concurrent_selected_signal_assignment_inner(&mut self) {
        self.opt_token(Keyword(Kw::Postponed));
        self.expect_kw(Kw::With);
        self.expression();
        self.expect_kw(Kw::Select);
        self.opt_token(Que);
        self.target();
        self.expect_token(LTE);
        self.opt_token(Keyword(Kw::Guarded));
        self.opt_delay_mechanism();
        self.selected_waveforms();
    }

    pub fn target(&mut self) {
        if self.next_is(LeftPar) {
            self.start_node(AggregateTarget);
            self.aggregate();
            self.end_node();
        } else {
            self.start_node(NameTarget);
            self.name();
            self.end_node();
        }
    }

    pub fn assertion(&mut self) {
        self.start_node(Assertion);
        self.expect_kw(Kw::Assert);
        self.condition();
        if self.opt_token(Keyword(Kw::Report)) {
            self.expression();
        }
        if self.opt_token(Keyword(Kw::Severity)) {
            self.expression();
        }
        self.end_node();
    }

    fn case_generate_statement_inner(&mut self) {
        self.expect_kw(Kw::Case);
        self.condition();
        self.expect_kw(Kw::Generate);
        while self.next_is(Keyword(Kw::When)) {
            self.case_generate_alternative();
        }
        self.expect_tokens([Keyword(Kw::End), Keyword(Kw::Generate)]);
        self.opt_identifier();
    }

    pub fn case_generate_alternative(&mut self) {
        self.start_node(CaseGenerateAlternative);
        self.expect_kw(Kw::When);
        self.opt_label();
        self.choices();
        self.expect_token(RightArrow);
        self.generate_statement_body();
        self.end_node();
    }

    fn for_generate_statement_inner(&mut self) {
        self.expect_kw(Kw::For);
        self.parameter_specification();
        self.expect_kw(Kw::Generate);
        self.generate_statement_body();
        self.expect_tokens([Keyword(Kw::End), Keyword(Kw::Generate)]);
        self.opt_identifier();
    }

    pub fn if_generate_elsif(&mut self) {
        self.start_node(IfGenerateElsif);
        self.skip();
        self.opt_label();
        self.condition();
        self.expect_kw(Kw::Generate);
        self.generate_statement_body();
        self.end_node();
    }

    pub fn if_generate_else(&mut self) {
        self.start_node(IfGenerateElse);
        self.skip();
        self.opt_label();
        self.expect_kw(Kw::Generate);
        self.generate_statement_body();
        self.end_node();
    }

    fn if_generate_statement_inner(&mut self) {
        self.expect_kw(Kw::If);
        self.opt_label();
        self.condition();
        self.expect_kw(Kw::Generate);
        self.generate_statement_body();
        while self.next_is(Keyword(Kw::Elsif)) {
            self.if_generate_elsif();
        }
        if self.next_is(Keyword(Kw::Else)) {
            self.if_generate_else();
        }
        self.expect_tokens([Keyword(Kw::End), Keyword(Kw::Generate)]);
        self.opt_identifier();
    }

    pub fn generate_statement_body(&mut self) {
        self.start_node(GenerateStatementBody);
        self.opt_declarative_part();
        self.opt_token(Keyword(Kw::Begin));
        self.concurrent_statements();
        if self.next_is(Keyword(Kw::End)) && !self.next_nth_is(Keyword(Kw::Generate), 1) {
            self.skip();
            self.opt_identifier();
            self.expect_token(SemiColon);
        }
        self.end_node();
    }

    pub fn parameter_specification(&mut self) {
        self.start_node(ParameterSpecification);
        self.identifier();
        self.expect_kw(Kw::In);
        self.discrete_range();
        self.end_node();
    }

    fn instantiation_statement_inner(&mut self) {
        self.opt_generic_map_aspect();
        self.opt_port_map_aspect();
    }

    fn process_statement_inner(&mut self) {
        self.opt_token(Keyword(Kw::Postponed));
        self.expect_token(Keyword(Kw::Process));
        if self.next_is(LeftPar) {
            self.process_sensitivity_list();
        }
        self.opt_token(Keyword(Kw::Is));
        self.declarative_part();
        self.expect_token(Keyword(Kw::Begin));
        self.sequence_of_statements();
        self.expect_kw(Kw::End);
        self.opt_token(Keyword(Kw::Postponed));
        self.expect_token(Keyword(Kw::Process));
        self.opt_identifier();
    }

    pub fn process_sensitivity_list(&mut self) {
        self.start_node(ParenthesizedProcessSensitivityList);
        self.expect_token(LeftPar);
        if self.next_is(RightPar) {
            // This is illegal, but considered only at the analysis stage
            self.skip();
            self.end_node();
            return;
        }
        if self.next_is(Keyword(Kw::All)) {
            self.skip_into_node(AllSensitivityList);
        } else {
            self.name_list();
        }
        self.expect_token(RightPar);
        self.end_node();
    }

    pub fn sensitivity_list(&mut self) {
        self.separated_list(Parser::name, Comma);
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils::to_test_text;
    use crate::parser::Parser;

    fn stmt_to_test_text(input: &str) -> String {
        to_test_text(Parser::concurrent_statement, input)
    }

    #[test]
    fn concurrent_procedure() {
        insta::assert_snapshot!(stmt_to_test_text("foo(clk);",));
    }

    #[test]
    fn postponed_concurrent_procedure() {
        insta::assert_snapshot!(stmt_to_test_text("postponed foo(clk);",));
    }

    #[test]
    fn labeled_concurrent_procedure() {
        insta::assert_snapshot!(stmt_to_test_text("name: foo(clk);",));
    }

    #[test]
    fn concurrent_procedure_no_args() {
        insta::assert_snapshot!(stmt_to_test_text("foo;",));
    }

    #[test]
    fn block() {
        insta::assert_snapshot!(stmt_to_test_text(
            "\
name : block
  constant const : natural := 0;
begin
  name2: foo(clk);
end block;",
        ));
    }

    #[test]
    fn block_variant() {
        insta::assert_snapshot!(stmt_to_test_text(
            "\
name : block is
begin
end block name;",
        ));
    }

    #[test]
    fn guarded_block() {
        insta::assert_snapshot!(stmt_to_test_text(
            "\
name : block (cond = true)
begin
end block;",
        ));
    }

    #[test]
    fn guarded_block_variant() {
        insta::assert_snapshot!(stmt_to_test_text(
            "\
name : block (cond = true) is
begin
end block;",
        ));
    }

    #[test]
    fn block_header() {
        insta::assert_snapshot!(stmt_to_test_text(
            "\
name: block is
  generic(gen: integer := 1);
  generic map(gen => 1);
  port(prt: integer := 1);
  port map(prt => 2);
begin
end block;",
        ));
    }

    #[test]
    fn process_statement() {
        insta::assert_snapshot!(stmt_to_test_text(
            "\
process
begin
end process;",
        ))
    }

    #[test]
    fn test_process_statement_variant() {
        insta::assert_snapshot!(stmt_to_test_text(
            "\
name : process is
begin
end process name;",
        ))
    }

    #[test]
    fn postponed_statement() {
        insta::assert_snapshot!(stmt_to_test_text(
            "\
postponed process
begin
end process;",
        ))
    }

    #[test]
    fn postponed_process_statement_end_postponed() {
        insta::assert_snapshot!(stmt_to_test_text(
            "\
postponed process
begin
end postponed process;",
        ))
    }

    #[test]
    fn process_statement_end_postponed() {
        insta::assert_snapshot!(stmt_to_test_text(
            "\
process is
begin
end postponed process;",
        ))
    }

    #[test]
    fn process_statement_sensitivity() {
        insta::assert_snapshot!(stmt_to_test_text(
            "\
process (clk, vec(1)) is
begin
end process;",
        ))
    }

    #[test]
    fn process_empty_sensitivity() {
        insta::assert_snapshot!(stmt_to_test_text(
            "\
process () is
begin
end process;",
        ))
    }

    #[test]
    fn process_statement_full() {
        insta::assert_snapshot!(stmt_to_test_text(
            "\
process (all) is
  variable foo : boolean;
begin
  foo <= true;
  wait;
end process;",
        ));
    }

    #[test]
    fn concurrent_assert() {
        insta::assert_snapshot!(stmt_to_test_text("assert cond = true;",));
    }

    #[test]
    fn postponed_concurrent_assert() {
        insta::assert_snapshot!(stmt_to_test_text("postponed assert cond = true;",));
    }

    #[test]
    fn concurrent_signal_assignment() {
        insta::assert_snapshot!(stmt_to_test_text("foo <= bar(2 to 3);",));
    }

    #[test]
    fn concurrent_signal_assignment_external_name() {
        insta::assert_snapshot!(stmt_to_test_text(
            "<< signal dut.foo : std_logic >> <= bar(2 to 3);",
        ));
    }

    #[test]
    fn selected_signal_assignment() {
        insta::assert_snapshot!(stmt_to_test_text(
            "with x(0) + 1 select
   foo(0) <= transport bar(1,2) after 2 ns when 0|1;",
        ));
    }

    #[test]
    fn component_instantiation() {
        insta::assert_snapshot!(stmt_to_test_text("inst: component lib.foo.bar;",));
    }

    #[test]
    fn configuration_instantiation() {
        insta::assert_snapshot!(stmt_to_test_text("inst: configuration lib.foo.bar;",));
    }

    #[test]
    fn entity_instantiation() {
        insta::assert_snapshot!(stmt_to_test_text("inst: entity lib.foo.bar;",));
    }

    #[test]
    fn entity_instantiation_architecture() {
        // Note: the architecture is part of the name to simplify
        insta::assert_snapshot!(stmt_to_test_text("inst: entity lib.foo.bar(arch);",));
    }

    #[test]
    fn component_aspect_maps() {
        insta::assert_snapshot!(stmt_to_test_text(
            "\
inst: component lib.foo.bar
  generic map (
   const => 1
  )
  port map (
   clk => clk_foo
  );",
        ));
    }

    #[test]
    fn component_no_keyword_port_aspect_map() {
        insta::assert_snapshot!(stmt_to_test_text(
            "\
inst: lib.foo.bar
  port map (
   clk => clk_foo
  );",
        ));
    }

    #[test]
    fn component_no_keyword_generic_aspect_map() {
        insta::assert_snapshot!(stmt_to_test_text(
            "\
inst: lib.foo.bar
  generic map (
   const => 1
  );",
        ));
    }

    #[test]
    fn for_generate_empty() {
        insta::assert_snapshot!(stmt_to_test_text(
            "\
gen: for idx in 0 to 1 generate
end generate;",
        ));
    }

    #[test]
    fn for_generate() {
        insta::assert_snapshot!(stmt_to_test_text(
            "\
gen: for idx in 0 to 1 generate
  foo <= bar;
end generate;",
        ));
    }

    #[test]
    fn for_generate_empty_declarations() {
        insta::assert_snapshot!(stmt_to_test_text(
            "\
gen: for idx in 0 to 1 generate
begin
  foo <= bar;
end generate;",
        ));

        insta::assert_snapshot!(stmt_to_test_text(
            "\
gen: for idx in 0 to 1 generate
  foo <= bar;
end generate;",
        ));

        insta::assert_snapshot!(stmt_to_test_text(
            "\
gen: for idx in 0 to 1 generate
begin
  foo <= bar;
end;
end generate;",
        ));
    }

    #[test]
    fn for_generate_declarations() {
        insta::assert_snapshot!(stmt_to_test_text(
            "\
gen: for idx in 0 to 1 generate
  signal foo : natural;
begin
  foo <= bar;
end generate;",
        ));

        insta::assert_snapshot!(stmt_to_test_text(
            "\
gen: for idx in 0 to 1 generate
  signal foo : natural;
begin
  foo <= bar;
end;
end generate;",
        ));
    }

    #[test]
    fn if_generate_empty() {
        insta::assert_snapshot!(stmt_to_test_text(
            "\
gen: if cond = true generate
end generate;",
        ));
    }

    #[test]
    fn if_generate_declarative_region() {
        insta::assert_snapshot!(stmt_to_test_text(
            "\
gen: if cond = true generate
begin
end generate;",
        ));
    }

    #[test]
    fn if_elseif_else_generate_empty() {
        insta::assert_snapshot!(stmt_to_test_text(
            "\
gen: if cond = true generate
elsif cond2 = true generate
else generate
end generate;",
        ));
    }

    #[test]
    fn test_if_elseif_else_generate() {
        insta::assert_snapshot!(stmt_to_test_text(
            "\
gen: if cond = true generate
  variable v1 : boolean;
begin
  foo1(clk);
elsif cond2 = true generate
  variable v2 : boolean;
begin
  foo2(clk);
else generate
  variable v3 : boolean;
begin
  foo3(clk);
end generate;",
        ));
    }

    #[test]
    fn if_elseif_else_generate_alternative_label() {
        insta::assert_snapshot!(stmt_to_test_text(
            "\
gen: if alt1: cond = true generate
elsif cond2 = true generate
end alt2;
else alt3: generate
end alt4;
end generate;",
        ))
    }

    #[test]
    fn if_elseif_else_generate_inner_end() {
        insta::assert_snapshot!(stmt_to_test_text(
            "\
gen: if alt1: cond = true generate
end alt1;
elsif alt2: cond2 = true generate
end alt2;
else alt3: generate
end alt3;
end generate;",
        ))
    }

    #[test]
    fn case_generate() {
        insta::assert_snapshot!(stmt_to_test_text(
            "\
gen: case expr(0) + 2 generate
  when 1 | 2 =>
    sig <= value;
  when others =>
    foo(clk);
end generate;",
        ));
    }

    #[test]
    fn case_generate_alternative_label() {
        insta::assert_snapshot!(stmt_to_test_text(
            "\
gen1: case expr(0) + 2 generate
  when alt1: 1 | 2 =>
    sig <= value;
  when alt2: others =>
    foo(clk);
end generate gen1;",
        ));
    }
}
