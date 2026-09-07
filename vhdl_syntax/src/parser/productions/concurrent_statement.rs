// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::marker::{CompletedMarker, Marker, Precede, UnknownMarker};
use crate::parser::productions::declarations::is_start_of_declarative_part;
use crate::parser::util::{choice_options, StallGuard};
use crate::parser::Parser;
use crate::syntax::meta::Layout;
use crate::syntax::node_kind::NodeKind::*;
use crate::syntax::{
    AstNode, BlockDeclarativeItemSyntax, ConcurrentStatementSyntax, NodeKind,
    ProcessDeclarativeItemSyntax, SequentialStatementSyntax,
};
use crate::tokens::token_kind::Keyword as Kw;
use crate::tokens::TokenKind::{self, *};

impl Parser {
    pub(crate) fn block_statement(&mut self) -> CompletedMarker {
        self.node(BlockStatement, |p| {
            p.label();
            p.block_preamble();
            p.block_header();
            p.block_declarative_part();
            p.node(DeclarationStatementSeparator, |p| {
                p.expect_kw(Kw::Begin);
            });
            p.block_statement_part();
            p.block_epilogue();
        })
    }

    pub(crate) fn block_preamble(&mut self) {
        self.node(BlockPreamble, |p| {
            p.expect_kw(Kw::Block);
            if p.next_is(LeftPar) {
                p.node(ParenthesizedCondition, |p| {
                    p.skip(); // LeftPar
                    p.expression();
                    p.expect_token(RightPar);
                });
            }
            p.opt_token(Keyword(Kw::Is));
        });
    }

    pub(crate) fn block_epilogue(&mut self) {
        self.node(BlockEpilogue, |p| {
            p.expect_tokens([Keyword(Kw::End), Keyword(Kw::Block)]);
            p.opt_identifier();
            p.expect_token(SemiColon);
        });
    }

    pub(crate) fn block_declarative_part(&mut self) {
        self.declarations(BlockDeclarativePart, BlockDeclarativeItemSyntax::META);
    }

    pub(crate) fn block_statement_part(&mut self) {
        self.concurrent_statements(BlockStatementPart, ConcurrentStatementSyntax::META);
    }

    pub(crate) fn block_header(&mut self) {
        self.node(BlockHeader, |p| {
            if p.next_is(Keyword(Kw::Generic)) {
                p.node(GenericPart, |p| {
                    p.generic_clause();
                    if p.next_is(Keyword(Kw::Generic)) {
                        p.node(GenericMap, |p| {
                            p.opt_generic_map_aspect();
                            p.expect_token(SemiColon);
                        });
                    }
                });
            }

            if p.next_is(Keyword(Kw::Port)) {
                p.node(PortPart, |p| {
                    p.port_clause();
                    if p.next_is(Keyword(Kw::Port)) {
                        p.node(PortMap, |p| {
                            p.opt_port_map_aspect();
                            p.expect_token(SemiColon);
                        });
                    }
                });
            }
        });
    }

    pub(crate) fn concurrent_statements(&mut self, node_kind: NodeKind, layout: &Layout) {
        self.node(node_kind, |p| {
            p.concurrent_statement_list(choice_options(layout));
        });
    }

    fn concurrent_statement_list(&mut self, allowed_nodes: &[NodeKind]) {
        let mut guard = StallGuard::new();
        while guard.should_continue(self) {
            match self.peek_token() {
                Keyword(Kw::End | Kw::Elsif | Kw::Else | Kw::When) | Eof => {
                    break;
                }
                _ => {
                    if let Some(statement) = self.concurrent_statement() {
                        self.check_node_is_allowed(&statement, allowed_nodes);
                    }
                }
            }
        }
    }

    pub(crate) fn component_instantiated_unit(&mut self) {
        self.node(InstantiatedComponent, |p| {
            p.opt_token(Keyword(Kw::Component));
            p.name();
        });
    }

    pub(crate) fn entity_instantiated_unit(&mut self) {
        self.node(InstantiatedEntity, |p| {
            p.expect_kw(Kw::Entity);
            p.name();
        });
    }

    pub(crate) fn configuration_instantiated_unit(&mut self) {
        self.node(InstantiatedConfiguration, |p| {
            p.expect_kw(Kw::Configuration);
            p.name();
        });
    }

    fn peek_concurrent_statement_kind(&mut self) -> TokenKind {
        // Has label?
        let mut peek_idx = 0usize;
        if self.next_is(Identifier) && self.next_nth_is(Colon, 1) {
            peek_idx = 2;
        }
        // Has 'postpooned' keyword?
        if self.next_nth_is(Keyword(Kw::Postponed), peek_idx) {
            peek_idx += 1;
        }
        self.peek_nth_token(peek_idx)
    }

    pub(crate) fn instantiated_unit(&mut self) {
        match self.peek_token() {
            Keyword(Kw::Entity) => self.entity_instantiated_unit(),
            Keyword(Kw::Configuration) => self.configuration_instantiated_unit(),
            _ => self.component_instantiated_unit(),
        }
    }

    pub(crate) fn component_instantiation_statement(&mut self) -> CompletedMarker {
        self.node(ComponentInstantiationStatement, |p| {
            p.label();
            p.instantiated_unit();
            p.instantiation_statement_inner();
            p.expect_token(SemiColon);
        })
    }

    pub(crate) fn concurrent_assertion_statement(&mut self) -> CompletedMarker {
        self.node(ConcurrentAssertionStatement, |p| {
            p.opt_label();
            p.opt_token(Keyword(Kw::Postponed));
            p.assertion();
            p.expect_token(SemiColon);
        })
    }

    // Assumes `<=` (LTE) has already been parsed
    fn signal_assignment_after_lte(&mut self, unknown: UnknownMarker) -> Marker {
        self.opt_token(Keyword(Kw::Guarded));
        self.opt_delay_mechanism();
        let waveform = self.waveform();
        if self.next_is(Keyword(Kw::When)) {
            let marker = unknown.resolve(self, ConcurrentConditionalSignalAssignment);
            let when = waveform.precede(self, WhenWaveform);
            self.skip();
            self.expression();
            let when_waveform = when.complete(self);
            let waveforms = when_waveform.precede(self, ConditionalWaveforms);
            self.conditional_else(Parser::waveform, ElseWhenWaveform, ElseWaveform);
            waveforms.complete(self);
            marker
        } else {
            unknown.resolve(self, ConcurrentSimpleSignalAssignment)
        }
    }

    pub(crate) fn concurrent_statement(&mut self) -> Option<CompletedMarker> {
        match self.peek_concurrent_statement_kind() {
            Keyword(Kw::Block) => Some(self.block_statement()),
            Keyword(Kw::Process) => Some(self.process_statement()),
            Keyword(Kw::Component | Kw::Configuration | Kw::Entity) => {
                Some(self.component_instantiation_statement())
            }
            Keyword(Kw::For) => Some(self.for_generate_statement()),
            Keyword(Kw::If) => Some(self.if_generate_statement()),
            Keyword(Kw::Case) => Some(self.case_generate_statement()),
            Keyword(Kw::Assert) => Some(self.concurrent_assertion_statement()),
            Keyword(Kw::With) => Some(self.concurrent_selected_signal_assignment()),
            LeftPar => {
                let unknown = self.start_unknown();
                self.opt_label();
                self.opt_token(Keyword(Kw::Postponed));
                self.node(AggregateTarget, Parser::aggregate);
                self.expect_token(LTE);
                let marker = self.signal_assignment_after_lte(unknown);
                self.expect_token(SemiColon);
                Some(marker.complete(self))
            }
            Identifier | LtLt | StringLiteral | CharacterLiteral => {
                let unknown = self.start_unknown();
                self.opt_label();
                self.opt_token(Keyword(Kw::Postponed));
                let name = self.name();
                let marker = match self.peek_token() {
                    LTE => {
                        name.precede(self, NameTarget).complete(self);
                        self.skip();
                        self.signal_assignment_after_lte(unknown)
                    }
                    Keyword(Kw::Port | Kw::Generic) => {
                        name.precede(self, InstantiatedComponent).complete(self);
                        let marker = unknown.resolve(self, ComponentInstantiationStatement);
                        self.instantiation_statement_inner();
                        marker
                    }
                    // Could be an instantiated unit without ports and generics
                    // or a procedure call
                    _ => unknown.resolve(
                        self,
                        ConcurrentProcedureCallOrComponentInstantiationStatement,
                    ),
                };
                self.expect_token(SemiColon);
                Some(marker.complete(self))
            }
            _ => {
                // Consume label and postponed keyword for error recovery
                self.opt_label();
                self.opt_token(Keyword(Kw::Postponed));
                self.expect_tokens_recover([
                    Keyword(Kw::Block),
                    Keyword(Kw::Process),
                    Keyword(Kw::Component),
                    Keyword(Kw::Configuration),
                    Keyword(Kw::Entity),
                    Keyword(Kw::For),
                    Keyword(Kw::If),
                    Keyword(Kw::Case),
                    Keyword(Kw::Assert),
                    Keyword(Kw::With),
                    Identifier,
                    LtLt,
                    StringLiteral,
                    CharacterLiteral,
                ]);
                None
            }
        }
    }

    pub(crate) fn concurrent_selected_signal_assignment(&mut self) -> CompletedMarker {
        self.node(ConcurrentSelectedSignalAssignment, |p| {
            p.opt_label();
            p.opt_token(Keyword(Kw::Postponed));
            p.selected_assignment_preamble();
            p.target();
            p.expect_token(LTE);
            p.opt_token(Keyword(Kw::Guarded));
            p.opt_delay_mechanism();
            p.selected_waveforms();
            p.expect_token(SemiColon);
        })
    }

    pub(crate) fn selected_assignment_preamble(&mut self) {
        self.node(SelectedAssignmentPreamble, |p| {
            p.expect_kw(Kw::With);
            p.expression();
            p.expect_kw(Kw::Select);
            p.opt_token(Que);
        });
    }

    pub(crate) fn target(&mut self) {
        if self.next_is(LeftPar) {
            self.node(AggregateTarget, |p| {
                p.aggregate();
            });
        } else {
            self.node(NameTarget, |p| {
                p.name();
            });
        }
    }

    pub(crate) fn assertion(&mut self) {
        self.node(Assertion, |p| {
            p.expect_kw(Kw::Assert);
            p.condition();
            if p.next_is(Keyword(Kw::Report)) {
                p.node(ReportClause, |p| {
                    p.skip(); // Kw::Report
                    p.expression();
                });
            }
            if p.next_is(Keyword(Kw::Severity)) {
                p.node(SeverityClause, |p| {
                    p.skip(); // Kw::Severity
                    p.expression();
                });
            }
        });
    }

    pub(crate) fn case_generate_statement(&mut self) -> CompletedMarker {
        self.node(CaseGenerateStatement, |p| {
            p.label();
            p.case_generate_preamble();
            p.case_generate_alternative();
            while p.next_is(Keyword(Kw::When)) {
                p.case_generate_alternative();
            }
            p.generate_epilogue();
        })
    }

    pub(crate) fn case_generate_preamble(&mut self) {
        self.node(CaseGeneratePreamble, |p| {
            p.expect_kw(Kw::Case);
            p.expression();
            p.expect_kw(Kw::Generate);
        });
    }

    pub(crate) fn case_generate_alternative(&mut self) {
        self.node(CaseGenerateAlternative, |p| {
            p.expect_kw(Kw::When);
            p.opt_label();
            p.choices();
            p.expect_token(RightArrow);
            p.generate_statement_body();
        });
    }

    pub(crate) fn for_generate_statement(&mut self) -> CompletedMarker {
        self.node(ForGenerateStatement, |p| {
            p.label();
            p.for_generate_preamble();
            p.generate_statement_body();
            p.generate_epilogue();
        })
    }

    pub(crate) fn for_generate_preamble(&mut self) {
        self.node(ForGeneratePreamble, |p| {
            p.expect_kw(Kw::For);
            p.parameter_specification();
            p.expect_kw(Kw::Generate);
        });
    }

    pub(crate) fn generate_epilogue(&mut self) {
        self.node(GenerateEpilogue, |p| {
            p.expect_tokens([Keyword(Kw::End), Keyword(Kw::Generate)]);
            p.opt_identifier();
            p.expect_token(SemiColon);
        });
    }

    pub(crate) fn if_generate_if(&mut self) {
        self.node(IfGenerateIf, |p| {
            p.expect_kw(Kw::If);
            p.opt_label();
            p.expression();
            p.expect_kw(Kw::Generate);
            p.generate_statement_body();
        });
    }

    pub(crate) fn if_generate_elsif(&mut self) {
        self.node(IfGenerateElsif, |p| {
            p.skip();
            p.opt_label();
            p.condition();
            p.expect_kw(Kw::Generate);
            p.generate_statement_body();
        });
    }

    pub(crate) fn if_generate_else(&mut self) {
        self.node(IfGenerateElse, |p| {
            p.skip();
            p.opt_label();
            p.expect_kw(Kw::Generate);
            p.generate_statement_body();
        });
    }

    pub(crate) fn if_generate_statement(&mut self) -> CompletedMarker {
        self.node(IfGenerateStatement, |p| {
            p.label();
            p.if_generate_if();
            while p.next_is(Keyword(Kw::Elsif)) {
                p.if_generate_elsif();
            }
            if p.next_is(Keyword(Kw::Else)) {
                p.if_generate_else();
            }
            p.generate_epilogue();
        })
    }

    pub(crate) fn generate_statement_body(&mut self) {
        self.node(GenerateStatementBody, |p| {
            if is_start_of_declarative_part(p.peek_token()) || p.next_is(Keyword(Kw::Begin)) {
                p.node(GenerateBodyDeclarations, |p| {
                    p.block_declarative_part();
                    p.node(DeclarationStatementSeparator, |p| {
                        p.expect_kw(Kw::Begin);
                    });
                });
            }
            p.concurrent_statement_list(choice_options(ConcurrentStatementSyntax::META));
            if p.next_is(Keyword(Kw::End)) && !p.next_nth_is(Keyword(Kw::Generate), 1) {
                p.generate_statement_body_epilogue();
            }
        });
    }

    pub(crate) fn generate_statement_body_epilogue(&mut self) {
        self.node(GenerateBodyEpilogue, |p| {
            p.expect_kw(Kw::End);
            p.opt_identifier();
            p.expect_token(SemiColon);
        });
    }

    pub(crate) fn parameter_specification(&mut self) {
        self.node(ParameterSpecification, |p| {
            p.identifier();
            p.expect_kw(Kw::In);
            p.expression();
        });
    }

    fn instantiation_statement_inner(&mut self) {
        self.opt_generic_map_aspect();
        self.opt_port_map_aspect();
    }

    pub(crate) fn process_statement(&mut self) -> CompletedMarker {
        self.node(ProcessStatement, |p| {
            p.opt_label();
            p.process_preamble();
            p.process_declarative_part();
            p.node(DeclarationStatementSeparator, |p| {
                p.expect_kw(Kw::Begin);
            });
            p.process_statement_part();
            p.process_epilogue();
        })
    }

    pub(crate) fn process_declarative_part(&mut self) {
        self.declarations(ProcessDeclarativePart, ProcessDeclarativeItemSyntax::META);
    }

    pub(crate) fn process_statement_part(&mut self) {
        self.sequential_statements(ProcessStatementPart, SequentialStatementSyntax::META);
    }

    pub(crate) fn process_preamble(&mut self) {
        self.node(ProcessPreamble, |p| {
            p.opt_token(Keyword(Kw::Postponed));
            p.expect_token(Keyword(Kw::Process));
            if p.next_is(LeftPar) {
                p.process_sensitivity_list();
            }
            p.opt_token(Keyword(Kw::Is));
        });
    }

    pub(crate) fn process_epilogue(&mut self) {
        self.node(ProcessEpilogue, |p| {
            p.expect_kw(Kw::End);
            p.opt_token(Keyword(Kw::Postponed));
            p.expect_token(Keyword(Kw::Process));
            p.opt_identifier();
            p.expect_token(SemiColon);
        });
    }

    pub(crate) fn process_sensitivity_list(&mut self) {
        self.node(ParenthesizedProcessSensitivityList, |p| {
            p.expect_token(LeftPar);
            if p.next_is(Keyword(Kw::All)) {
                p.skip_into_node(AllSensitivityList);
            } else {
                p.sensitivity_list();
            }
            p.expect_token(RightPar);
        });
    }

    pub(crate) fn sensitivity_list(&mut self) {
        self.separated_list(SensitivityList, Parser::name, Comma);
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
    fn process_statement_with_empty_sensitivity_list() {
        assert_recovery_snapshot!(
            "\
process()
begin
end process;",
            Parser::concurrent_statement
        );
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
    fn process_statement_with_conditional_waveform_assignment() {
        insta::assert_snapshot!(stmt_to_test_text(
            "\
main: process is
begin
    data_processing_state <= processing when processing_data else idle;
end process main;",
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
    fn aggregate_signal_asignment() {
        insta::assert_snapshot!(stmt_to_test_text("(foo) <= bar;"));
        insta::assert_snapshot!(stmt_to_test_text("(foo, bar) <= baz;",));
    }

    #[test]
    fn concurrent_signal_assignment_external_name() {
        insta::assert_snapshot!(stmt_to_test_text(
            "<< signal dut.foo : std_logic >> <= bar(2 to 3);",
        ));
    }

    #[test]
    fn concurrent_conditional_signal_assignment() {
        // The first `waveform when condition` must be wrapped in a
        // `WhenWaveform` inside `ConditionalWaveforms`, like the
        // sequential form.
        insta::assert_snapshot!(stmt_to_test_text("foo <= a when sel else b;",));
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
    fn empty_case_generate() {
        assert_recovery_snapshot!(
            "\
gen: case expr(0) + 2 generate
end generate;",
            Parser::case_generate_statement
        );
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

    // MARK: Error recovery

    #[test]
    #[ignore = "currently produces spurious error messages since declarations and statements are not clearly separated"]
    fn process_missing_begin() {
        assert_recovery_snapshot!(
            "\
process (clk)
  variable count : integer := 0;
  count := count + 1;
end process;",
            Parser::process_statement
        );
    }

    #[test]
    fn process_missing_end() {
        assert_recovery_snapshot!(
            "\
process (clk)
begin
  q <= d;",
            Parser::process_statement
        );
    }

    #[test]
    fn for_generate_missing_end() {
        assert_recovery_snapshot!(
            "\
gen: for i in 0 to 7 generate
  buf(i) <= data(i);",
            Parser::for_generate_statement
        );
    }

    #[test]
    fn instantiation_missing_semicolon() {
        assert_recovery_snapshot!(
            "\
u_cpu: entity work.cpu
  port map (
    clk => clk
  )",
            Parser::component_instantiation_statement
        );
    }

    // concurrent statement loop. Could loop endlessly
    #[test]
    fn architecture_misplaced_use() {
        assert_recovery_snapshot!(
            "\
architecture a of e is
  begin
    use work.all;
  end architecture;",
            Parser::architecture
        );
    }
}
