// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind::*;
use crate::syntax::ConcurrentConditionalSignalAssignmentSyntax;
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
                self.skip();
                self.name();
                self.instantiation_statement_inner();
            }
            Some(Keyword(Kw::Configuration)) => {
                self.start_node_at(checkpoint, ConfigurationInstantiatedUnit);
                self.skip();
                self.name();
                self.instantiation_statement_inner();
            }
            Some(Keyword(Kw::Entity)) => {
                self.start_node_at(checkpoint, EntityInstantiatedUnit);
                self.skip();
                self.name();
                if self.opt_token(LeftPar) {
                    self.identifier();
                    self.expect_token(RightPar);
                }
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
            tok => todo!("token_kind={tok:?}"),
        }
        self.expect_token(SemiColon);
        self.end_node();
    }

    /// Parse conditional waveforms, assuming the first `waveform when` is already parsed
    fn conditional_waveforms_after_first_when(&mut self) {
        self.expression();
        while self.next_is(Keyword(Kw::Else)) {
            todo!()
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
            self.aggregate();
        } else {
            self.name()
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

    fn if_generate_statement_inner(&mut self) {
        self.expect_kw(Kw::If);
        self.opt_label();
        self.condition();
        self.expect_kw(Kw::Generate);
        self.generate_statement_body();
        while self.next_is(Keyword(Kw::Elsif)) {
            self.start_node(IfGenerateElsif);
            self.skip();
            self.opt_label();
            self.condition();
            self.expect_kw(Kw::Generate);
            self.generate_statement_body();
            self.end_node();
        }
        if self.next_is(Keyword(Kw::Else)) {
            self.start_node(IfGenerateElse);
            self.skip();
            self.opt_label();
            self.expect_kw(Kw::Generate);
            self.generate_statement_body();
            self.end_node();
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
        self.concurrent_statements();
        self.expect_kw(Kw::End);
        self.opt_token(Keyword(Kw::Postponed));
        self.expect_token(Keyword(Kw::Process));
        self.opt_identifier();
    }

    pub fn process_sensitivity_list(&mut self) {
        self.start_node(ProcessSensitivityList);
        self.expect_token(LeftPar);
        if self.next_is(Keyword(Kw::All)) {
            self.skip();
        }
        if self.next_is(RightPar) {
            // noop
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
    use crate::parser::test_utils::check;
    use crate::parser::Parser;

    fn check_stmt(input: &str, output: &str) {
        check(Parser::concurrent_statement, input, output);
    }

    #[test]
    fn concurrent_procedure() {
        check_stmt(
            "foo(clk);",
            "\
ProcedureCallStatement
  Name
    Identifier 'foo'
    RawTokens
      LeftPar
      Identifier 'clk'
      RightPar
  SemiColon
        ",
        );
    }

    #[test]
    fn postponed_concurrent_procedure() {
        check_stmt(
            "postponed foo(clk);",
            "\
ProcedureCallStatement
  Keyword(Postponed)
  Name
    Identifier 'foo'
    RawTokens
      LeftPar
      Identifier 'clk'
      RightPar
  SemiColon
        ",
        );
    }

    #[test]
    fn labeled_concurrent_procedure() {
        check_stmt(
            "name: foo(clk);",
            "\
ProcedureCallStatement
  Label
    Identifier 'name'
    Colon
  Name
    Identifier 'foo'
    RawTokens
      LeftPar
      Identifier 'clk'
      RightPar
  SemiColon
        ",
        );
    }

    #[test]
    fn concurrent_procedure_no_args() {
        check_stmt(
            "foo;",
            "\
ProcedureCallStatement
  Name
    Identifier 'foo'
  SemiColon
        ",
        );
    }

    #[test]
    fn block() {
        check_stmt(
            "\
name : block
  constant const : natural := 0;
begin
  name2: foo(clk);
end block;",
            "\
BlockStatement
  Label
    Identifier 'name'
    Colon
  Keyword(Block)
  BlockHeader
  ConstantDeclaration
    Keyword(Constant)
    IdentifierList
      Identifier 'const'
    Colon
    Identifier 'natural'
    ColonEq
    Literal
      AbstractLiteral '0'
    SemiColon
  Keyword(Begin)
  ProcedureCallStatement
    Label
      Identifier 'name2'
      Colon
    Name
      Identifier 'foo'
      RawTokens
        LeftPar
        Identifier 'clk'
        RightPar
    SemiColon
  Keyword(End)
  Keyword(Block)
  SemiColon

        ",
        );
    }

    #[test]
    fn block_variant() {
        check_stmt(
            "\
name : block is
begin
end block name;",
            "\
BlockStatement
  Label
    Identifier 'name'
    Colon
  Keyword(Block)
  Keyword(Is)
  BlockHeader
  Keyword(Begin)
  Keyword(End)
  Keyword(Block)
  Identifier 'name'
  SemiColon
        ",
        );
    }

    #[test]
    fn guarded_block() {
        check_stmt(
            "\
name : block (cond = true)
begin
end block;",
            "\
BlockStatement
  Label
    Identifier 'name'
    Colon
  Keyword(Block)
  ParenthesizedExpression
    LeftPar
    BinaryExpression
      Name
        Identifier 'cond'
      EQ
      Name
        Identifier 'true'
    RightPar
  BlockHeader
  Keyword(Begin)
  Keyword(End)
  Keyword(Block)
  SemiColon
        ",
        );
    }

    #[test]
    fn guarded_block_variant() {
        check_stmt(
            "\
name : block (cond = true) is
begin
end block;",
            "\
BlockStatement
  Label
    Identifier 'name'
    Colon
  Keyword(Block)
  ParenthesizedExpression
    LeftPar
    BinaryExpression
      Name
        Identifier 'cond'
      EQ
      Name
        Identifier 'true'
    RightPar
  Keyword(Is)
  BlockHeader
  Keyword(Begin)
  Keyword(End)
  Keyword(Block)
  SemiColon
        ",
        );
    }

    #[test]
    fn block_header() {
        check_stmt(
            "\
name: block is
  generic(gen: integer := 1);
  generic map(gen => 1);
  port(prt: integer := 1);
  port map(prt => 2);
begin
end block;",
            "\
BlockStatement
  Label
    Identifier 'name'
    Colon
  Keyword(Block)
  Keyword(Is)
  BlockHeader
    GenericClause
      Keyword(Generic)
      LeftPar
      InterfaceList
        InterfaceObjectDeclaration
          IdentifierList
            Identifier 'gen'
          Colon
          Identifier 'integer'
          ColonEq
          Literal
            AbstractLiteral '1'
      RightPar
      SemiColon
    GenericMapAspect
      Keyword(Generic)
      Keyword(Map)
      LeftPar
      AssociationList
        AssociationElement
          FormalPart
            Name
              Identifier 'gen'
          RightArrow
          ActualPart
            RawTokens
              AbstractLiteral '1'
      RightPar
    SemiColon
    PortClause
      Keyword(Port)
      LeftPar
      InterfaceList
        InterfaceObjectDeclaration
          IdentifierList
            Identifier 'prt'
          Colon
          Identifier 'integer'
          ColonEq
          Literal
            AbstractLiteral '1'
      RightPar
      SemiColon
    PortMapAspect
      Keyword(Port)
      Keyword(Map)
      LeftPar
      AssociationList
        AssociationElement
          FormalPart
            Name
              Identifier 'prt'
          RightArrow
          ActualPart
            RawTokens
              AbstractLiteral '2'
      RightPar
    SemiColon
  Keyword(Begin)
  Keyword(End)
  Keyword(Block)
  SemiColon
        ",
        );
    }

    #[test]
    fn process_statement() {
        check_stmt(
            "\
process
begin
end process;",
            "\
ProcessStatement
  Keyword(Process)
  Keyword(Begin)
  Keyword(End)
  Keyword(Process)
  SemiColon
        ",
        )
    }

    #[test]
    fn test_process_statement_variant() {
        check_stmt(
            "\
name : process is
begin
end process name;",
            "\
ProcessStatement
  Label
    Identifier 'name'
    Colon
  Keyword(Process)
  Keyword(Is)
  Keyword(Begin)
  Keyword(End)
  Keyword(Process)
  Identifier 'name'
  SemiColon
        ",
        )
    }

    #[test]
    fn postponed_statement() {
        check_stmt(
            "\
postponed process
begin
end process;",
            "\
ProcessStatement
  Keyword(Postponed)
  Keyword(Process)
  Keyword(Begin)
  Keyword(End)
  Keyword(Process)
  SemiColon
        ",
        )
    }

    #[test]
    fn postponed_process_statement_end_postponed() {
        check_stmt(
            "\
postponed process
begin
end postponed process;",
            "\
ProcessStatement
  Keyword(Postponed)
  Keyword(Process)
  Keyword(Begin)
  Keyword(End)
  Keyword(Postponed)
  Keyword(Process)
  SemiColon
        ",
        )
    }

    #[test]
    fn process_statement_end_postponed() {
        check_stmt(
            "\
process is
begin
end postponed process;",
            "\
ProcessStatement
  Keyword(Process)
  Keyword(Is)
  Keyword(Begin)
  Keyword(End)
  Keyword(Postponed)
  Keyword(Process)
  SemiColon
        ",
        )
    }

    #[test]
    fn process_statement_sensitivity() {
        check_stmt(
            "\
process (clk, vec(1)) is
begin
end process;",
            "\
ProcessStatement
  Keyword(Process)
  ProcessSensitivityList
    LeftPar
    NameList
      Name
        Identifier 'clk'
      Comma
      Name
        Identifier 'vec'
        RawTokens
          LeftPar
          AbstractLiteral '1'
          RightPar
    RightPar
  Keyword(Is)
  Keyword(Begin)
  Keyword(End)
  Keyword(Process)
  SemiColon",
        )
    }

    #[test]
    fn process_empty_sensitivity() {
        check_stmt(
            "\
process () is
begin
end process;",
            "\
ProcessStatement
  Keyword(Process)
  ProcessSensitivityList
    LeftPar
    RightPar
  Keyword(Is)
  Keyword(Begin)
  Keyword(End)
  Keyword(Process)
  SemiColon",
        )
    }

    #[test]
    #[ignore]
    fn process_statement_full() {
        check_stmt(
            "\
process (all) is
  variable foo : boolean;
begin
  foo <= true;
  wait;
end process;",
            "TODO",
        );
    }

    #[test]
    fn concurrent_assert() {
        check_stmt(
            "assert cond = true;",
            "\
ConcurrentAssertionStatement
  Assertion
    Keyword(Assert)
    BinaryExpression
      Name
        Identifier 'cond'
      EQ
      Name
        Identifier 'true'
  SemiColon
        ",
        );
    }

    #[test]
    fn postponed_concurrent_assert() {
        check_stmt(
            "postponed assert cond = true;",
            "\
ConcurrentAssertionStatement
  Keyword(Postponed)
  Assertion
    Keyword(Assert)
    BinaryExpression
      Name
        Identifier 'cond'
      EQ
      Name
        Identifier 'true'
  SemiColon
        ",
        );
    }

    #[test]
    fn concurrent_signal_assignment() {
        check_stmt(
            "foo <= bar(2 to 3);",
            "\
ConcurrentSimpleSignalAssignment
  Name
    Identifier 'foo'
  LTE
  Waveform
    WaveformElement
      Name
        Identifier 'bar'
        RawTokens
          LeftPar
          AbstractLiteral '2'
          Keyword(To)
          AbstractLiteral '3'
          RightPar
  SemiColon
        ",
        );
    }

    #[test]
    fn concurrent_signal_assignment_external_name() {
        check_stmt(
            "<< signal dut.foo : std_logic >> <= bar(2 to 3);",
            "\
ConcurrentSimpleSignalAssignment
  Name
    ExternalName
      LtLt
      Keyword(Signal)
      ExternalPathName
        PartialPathname
          Identifier 'dut'
          Dot
          Identifier 'foo'
      Colon
      Identifier 'std_logic'
      GtGt
  LTE
  Waveform
    WaveformElement
      Name
        Identifier 'bar'
        RawTokens
          LeftPar
          AbstractLiteral '2'
          Keyword(To)
          AbstractLiteral '3'
          RightPar
  SemiColon
        ",
        );
    }

    #[test]
    fn selected_signal_assignment() {
        check_stmt(
            "with x(0) + 1 select
   foo(0) <= transport bar(1,2) after 2 ns when 0|1;",
            "\
ConcurrentSelectedSignalAssignment
  Keyword(With)
  BinaryExpression
    Name
      Identifier 'x'
      RawTokens
        LeftPar
        AbstractLiteral '0'
        RightPar
    Plus
    Literal
      AbstractLiteral '1'
  Keyword(Select)
  Name
    Identifier 'foo'
    RawTokens
      LeftPar
      AbstractLiteral '0'
      RightPar
  LTE
  DelayMechanism
    Keyword(Transport)
  SelectedWaveforms
    SelectedWaveformItem
      Waveform
        WaveformElement
          Name
            Identifier 'bar'
            RawTokens
              LeftPar
              AbstractLiteral '1'
              Comma
              AbstractLiteral '2'
              RightPar
          Keyword(After)
          PhysicalLiteral
            AbstractLiteral '2'
            Name
              Identifier 'ns'
      Keyword(When)
      Choices
        Literal
          AbstractLiteral '0'
        Bar
        Literal
          AbstractLiteral '1'
  SemiColon
        ",
        );
    }

    #[test]
    fn component_instantiation() {
        check_stmt(
            "inst: component lib.foo.bar;",
            "\
ComponentInstantiationStatement
  Label
    Identifier 'inst'
    Colon
  Keyword(Component)
  Name
    Identifier 'lib'
    SelectedName
      Dot
      Identifier 'foo'
    SelectedName
      Dot
      Identifier 'bar'
  SemiColon",
        );
    }

    #[test]
    fn configuration_instantiation() {
        check_stmt(
            "inst: configuration lib.foo.bar;",
            "\
ConfigurationInstantiatedUnit
  Label
    Identifier 'inst'
    Colon
  Keyword(Configuration)
  Name
    Identifier 'lib'
    SelectedName
      Dot
      Identifier 'foo'
    SelectedName
      Dot
      Identifier 'bar'
  SemiColon
        ",
        );
    }

    #[test]
    fn entity_instantiation() {
        check_stmt(
            "inst: entity lib.foo.bar;",
            "\
EntityInstantiatedUnit
  Label
    Identifier 'inst'
    Colon
  Keyword(Entity)
  Name
    Identifier 'lib'
    SelectedName
      Dot
      Identifier 'foo'
    SelectedName
      Dot
      Identifier 'bar'
  SemiColon
        ",
        );
    }

    #[test]
    fn entity_instantiation_architecture() {
        // Note: the architecture is part of the name to simplify
        check_stmt(
            "inst: entity lib.foo.bar(arch);",
            "\
EntityInstantiatedUnit
  Label
    Identifier 'inst'
    Colon
  Keyword(Entity)
  Name
    Identifier 'lib'
    SelectedName
      Dot
      Identifier 'foo'
    SelectedName
      Dot
      Identifier 'bar'
    RawTokens
      LeftPar
      Identifier 'arch'
      RightPar
  SemiColon
        ",
        );
    }

    #[test]
    fn component_aspect_maps() {
        check_stmt(
            "\
inst: component lib.foo.bar
  generic map (
   const => 1
  )
  port map (
   clk => clk_foo
  );",
            "\
ComponentInstantiationStatement
  Label
    Identifier 'inst'
    Colon
  Keyword(Component)
  Name
    Identifier 'lib'
    SelectedName
      Dot
      Identifier 'foo'
    SelectedName
      Dot
      Identifier 'bar'
  GenericMapAspect
    Keyword(Generic)
    Keyword(Map)
    LeftPar
    AssociationList
      AssociationElement
        FormalPart
          Name
            Identifier 'const'
        RightArrow
        ActualPart
          RawTokens
            AbstractLiteral '1'
    RightPar
  PortMapAspect
    Keyword(Port)
    Keyword(Map)
    LeftPar
    AssociationList
      AssociationElement
        FormalPart
          Name
            Identifier 'clk'
        RightArrow
        ActualPart
          RawTokens
            Identifier 'clk_foo'
    RightPar
  SemiColon",
        );
    }

    #[test]
    fn component_no_keyword_port_aspect_map() {
        check_stmt(
            "\
inst: lib.foo.bar
  port map (
   clk => clk_foo
  );",
            "\
ComponentInstantiationStatement
  Label
    Identifier 'inst'
    Colon
  Name
    Identifier 'lib'
    SelectedName
      Dot
      Identifier 'foo'
    SelectedName
      Dot
      Identifier 'bar'
  PortMapAspect
    Keyword(Port)
    Keyword(Map)
    LeftPar
    AssociationList
      AssociationElement
        FormalPart
          Name
            Identifier 'clk'
        RightArrow
        ActualPart
          RawTokens
            Identifier 'clk_foo'
    RightPar
  SemiColon",
        );
    }

    #[test]
    fn component_no_keyword_generic_aspect_map() {
        check_stmt(
            "\
inst: lib.foo.bar
  generic map (
   const => 1
  );",
            "\
ComponentInstantiationStatement
  Label
    Identifier 'inst'
    Colon
  Name
    Identifier 'lib'
    SelectedName
      Dot
      Identifier 'foo'
    SelectedName
      Dot
      Identifier 'bar'
  GenericMapAspect
    Keyword(Generic)
    Keyword(Map)
    LeftPar
    AssociationList
      AssociationElement
        FormalPart
          Name
            Identifier 'const'
        RightArrow
        ActualPart
          RawTokens
            AbstractLiteral '1'
    RightPar
  SemiColon",
        );
    }

    #[test]
    fn for_generate_empty() {
        check_stmt(
            "\
gen: for idx in 0 to 1 generate
end generate;",
            "\
ForGenerateStatement
  Label
    Identifier 'gen'
    Colon
  Keyword(For)
  ParameterSpecification
    Identifier 'idx'
    Keyword(In)
    Range
      Literal
        AbstractLiteral '0'
      Keyword(To)
      Literal
        AbstractLiteral '1'
  Keyword(Generate)
  GenerateStatementBody
  Keyword(End)
  Keyword(Generate)
  SemiColon
  ",
        );
    }

    #[test]
    fn for_generate() {
        check_stmt(
            "\
gen: for idx in 0 to 1 generate
  foo <= bar;
end generate;",
            "\
ForGenerateStatement
  Label
    Identifier 'gen'
    Colon
  Keyword(For)
  ParameterSpecification
    Identifier 'idx'
    Keyword(In)
    Range
      Literal
        AbstractLiteral '0'
      Keyword(To)
      Literal
        AbstractLiteral '1'
  Keyword(Generate)
  GenerateStatementBody
    ConcurrentSimpleSignalAssignment
      Name
        Identifier 'foo'
      LTE
      Waveform
        WaveformElement
          Name
            Identifier 'bar'
      SemiColon
  Keyword(End)
  Keyword(Generate)
  SemiColon
  ",
        );
    }

    #[test]
    fn for_generate_empty_declarations() {
        check_stmt(
            "\
gen: for idx in 0 to 1 generate
begin
  foo <= bar;
end generate;",
            "\
ForGenerateStatement
  Label
    Identifier 'gen'
    Colon
  Keyword(For)
  ParameterSpecification
    Identifier 'idx'
    Keyword(In)
    Range
      Literal
        AbstractLiteral '0'
      Keyword(To)
      Literal
        AbstractLiteral '1'
  Keyword(Generate)
  GenerateStatementBody
    Keyword(Begin)
    ConcurrentSimpleSignalAssignment
      Name
        Identifier 'foo'
      LTE
      Waveform
        WaveformElement
          Name
            Identifier 'bar'
      SemiColon
  Keyword(End)
  Keyword(Generate)
  SemiColon
  ",
        );

        check_stmt(
            "\
gen: for idx in 0 to 1 generate
  foo <= bar;
end generate;",
            "\
ForGenerateStatement
  Label
    Identifier 'gen'
    Colon
  Keyword(For)
  ParameterSpecification
    Identifier 'idx'
    Keyword(In)
    Range
      Literal
        AbstractLiteral '0'
      Keyword(To)
      Literal
        AbstractLiteral '1'
  Keyword(Generate)
  GenerateStatementBody
    ConcurrentSimpleSignalAssignment
      Name
        Identifier 'foo'
      LTE
      Waveform
        WaveformElement
          Name
            Identifier 'bar'
      SemiColon
  Keyword(End)
  Keyword(Generate)
  SemiColon
  ",
        );

        check_stmt(
            "\
gen: for idx in 0 to 1 generate
begin
  foo <= bar;
end;
end generate;",
            "\
ForGenerateStatement
  Label
    Identifier 'gen'
    Colon
  Keyword(For)
  ParameterSpecification
    Identifier 'idx'
    Keyword(In)
    Range
      Literal
        AbstractLiteral '0'
      Keyword(To)
      Literal
        AbstractLiteral '1'
  Keyword(Generate)
  GenerateStatementBody
    Keyword(Begin)
    ConcurrentSimpleSignalAssignment
      Name
        Identifier 'foo'
      LTE
      Waveform
        WaveformElement
          Name
            Identifier 'bar'
      SemiColon
    Keyword(End)
    SemiColon
  Keyword(End)
  Keyword(Generate)
  SemiColon
  ",
        );
    }

    #[test]
    #[ignore]
    fn for_generate_declarations() {
        check_stmt(
            "\
gen: for idx in 0 to 1 generate
  signal foo : natural;
begin
  foo <= bar;
end generate;",
            "TODO",
        );

        check_stmt(
            "\
gen: for idx in 0 to 1 generate
  signal foo : natural;
begin
  foo <= bar;
end;
end generate;",
            "TODO",
        );
    }

    #[test]
    fn if_generate_empty() {
        check_stmt(
            "\
gen: if cond = true generate
end generate;",
            "\
IfGenerateStatement
  Label
    Identifier 'gen'
    Colon
  Keyword(If)
  BinaryExpression
    Name
      Identifier 'cond'
    EQ
    Name
      Identifier 'true'
  Keyword(Generate)
  GenerateStatementBody
  Keyword(End)
  Keyword(Generate)
  SemiColon
  ",
        );
    }

    #[test]
    fn if_generate_declarative_region() {
        check_stmt(
            "\
gen: if cond = true generate
begin
end generate;",
            "\
IfGenerateStatement
  Label
    Identifier 'gen'
    Colon
  Keyword(If)
  BinaryExpression
    Name
      Identifier 'cond'
    EQ
    Name
      Identifier 'true'
  Keyword(Generate)
  GenerateStatementBody
    Keyword(Begin)
  Keyword(End)
  Keyword(Generate)
  SemiColon
  ",
        );
    }

    #[test]
    fn if_elseif_else_generate_empty() {
        check_stmt(
            "\
gen: if cond = true generate
elsif cond2 = true generate
else generate
end generate;",
            "\
IfGenerateStatement
  Label
    Identifier 'gen'
    Colon
  Keyword(If)
  BinaryExpression
    Name
      Identifier 'cond'
    EQ
    Name
      Identifier 'true'
  Keyword(Generate)
  GenerateStatementBody
  IfGenerateElsif
    Keyword(Elsif)
    BinaryExpression
      Name
        Identifier 'cond2'
      EQ
      Name
        Identifier 'true'
    Keyword(Generate)
    GenerateStatementBody
  IfGenerateElse
    Keyword(Else)
    Keyword(Generate)
    GenerateStatementBody
  Keyword(End)
  Keyword(Generate)
  SemiColon
  ",
        );
    }

    #[test]
    #[ignore]
    fn test_if_elseif_else_generate() {
        check_stmt(
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
            "\
IfGenerateStatement
  Label
    Identifier 'gen'
    Colon
  Keyword(If)
  BinaryExpression
    Name
      Identifier 'cond'
    EQ
    Name
      Identifier 'true'
  Keyword(Generate)
  GenerateStatementBody
    TODO
  ElsifGenerateBranch
    Keyword(Elsif)
    BinaryExpression
      Name
        Identifier 'cond2'
      EQ
      Name
        Identifier 'true'
    Keyword(Generate)
    GenerateStatementBody
      TODO
  ElseGenerateBranch
    Keyword(Else)
    Keyword(Generate)
    GenerateStatementBody
      TODO
  Keyword(End)
  Keyword(Generate)
  SemiColon",
        );
    }

    #[test]
    fn if_elseif_else_generate_alternative_label() {
        check_stmt(
            "\
gen: if alt1: cond = true generate
elsif cond2 = true generate
end alt2;
else alt3: generate
end alt4;
end generate;",
            "\
IfGenerateStatement
  Label
    Identifier 'gen'
    Colon
  Keyword(If)
  Label
    Identifier 'alt1'
    Colon
  BinaryExpression
    Name
      Identifier 'cond'
    EQ
    Name
      Identifier 'true'
  Keyword(Generate)
  GenerateStatementBody
  IfGenerateElsif
    Keyword(Elsif)
    BinaryExpression
      Name
        Identifier 'cond2'
      EQ
      Name
        Identifier 'true'
    Keyword(Generate)
    GenerateStatementBody
      Keyword(End)
      Identifier 'alt2'
      SemiColon
  IfGenerateElse
    Keyword(Else)
    Label
      Identifier 'alt3'
      Colon
    Keyword(Generate)
    GenerateStatementBody
      Keyword(End)
      Identifier 'alt4'
      SemiColon
  Keyword(End)
  Keyword(Generate)
  SemiColon",
        )
    }

    #[test]
    fn if_elseif_else_generate_inner_end() {
        check_stmt(
            "\
gen: if alt1: cond = true generate
end alt1;
elsif alt2: cond2 = true generate
end alt2;
else alt3: generate
end alt3;
end generate;",
            "\
IfGenerateStatement
  Label
    Identifier 'gen'
    Colon
  Keyword(If)
  Label
    Identifier 'alt1'
    Colon
  BinaryExpression
    Name
      Identifier 'cond'
    EQ
    Name
      Identifier 'true'
  Keyword(Generate)
  GenerateStatementBody
    Keyword(End)
    Identifier 'alt1'
    SemiColon
  IfGenerateElsif
    Keyword(Elsif)
    Label
      Identifier 'alt2'
      Colon
    BinaryExpression
      Name
        Identifier 'cond2'
      EQ
      Name
        Identifier 'true'
    Keyword(Generate)
    GenerateStatementBody
      Keyword(End)
      Identifier 'alt2'
      SemiColon
  IfGenerateElse
    Keyword(Else)
    Label
      Identifier 'alt3'
      Colon
    Keyword(Generate)
    GenerateStatementBody
      Keyword(End)
      Identifier 'alt3'
      SemiColon
  Keyword(End)
  Keyword(Generate)
  SemiColon",
        )
    }

    #[test]
    fn case_generate() {
        check_stmt(
            "\
gen: case expr(0) + 2 generate
  when 1 | 2 =>
    sig <= value;
  when others =>
    foo(clk);
end generate;",
            "\
CaseGenerateStatement
  Label
    Identifier 'gen'
    Colon
  Keyword(Case)
  BinaryExpression
    Name
      Identifier 'expr'
      RawTokens
        LeftPar
        AbstractLiteral '0'
        RightPar
    Plus
    Literal
      AbstractLiteral '2'
  Keyword(Generate)
  CaseGenerateAlternative
    Keyword(When)
    Choices
      Literal
        AbstractLiteral '1'
      Bar
      Literal
        AbstractLiteral '2'
    RightArrow
    GenerateStatementBody
      ConcurrentSimpleSignalAssignment
        Name
          Identifier 'sig'
        LTE
        Waveform
          WaveformElement
            Name
              Identifier 'value'
        SemiColon
  CaseGenerateAlternative
    Keyword(When)
    Choices
      Keyword(Others)
    RightArrow
    GenerateStatementBody
      ProcedureCallStatement
        Name
          Identifier 'foo'
          RawTokens
            LeftPar
            Identifier 'clk'
            RightPar
        SemiColon
  Keyword(End)
  Keyword(Generate)
  SemiColon",
        );
    }

    #[test]
    fn case_generate_alternative_label() {
        check_stmt(
            "\
gen1: case expr(0) + 2 generate
  when alt1: 1 | 2 =>
    sig <= value;
  when alt2: others =>
    foo(clk);
end generate gen1;",
            "\
CaseGenerateStatement
  Label
    Identifier 'gen1'
    Colon
  Keyword(Case)
  BinaryExpression
    Name
      Identifier 'expr'
      RawTokens
        LeftPar
        AbstractLiteral '0'
        RightPar
    Plus
    Literal
      AbstractLiteral '2'
  Keyword(Generate)
  CaseGenerateAlternative
    Keyword(When)
    Label
      Identifier 'alt1'
      Colon
    Choices
      Literal
        AbstractLiteral '1'
      Bar
      Literal
        AbstractLiteral '2'
    RightArrow
    GenerateStatementBody
      ConcurrentSimpleSignalAssignment
        Name
          Identifier 'sig'
        LTE
        Waveform
          WaveformElement
            Name
              Identifier 'value'
        SemiColon
  CaseGenerateAlternative
    Keyword(When)
    Label
      Identifier 'alt2'
      Colon
    Choices
      Keyword(Others)
    RightArrow
    GenerateStatementBody
      ProcedureCallStatement
        Name
          Identifier 'foo'
          RawTokens
            LeftPar
            Identifier 'clk'
            RightPar
        SemiColon
  Keyword(End)
  Keyword(Generate)
  Identifier 'gen1'
  SemiColon",
        );
    }
}
