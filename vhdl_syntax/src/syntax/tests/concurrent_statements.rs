// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::check;
use crate::parser::Parser;
use crate::syntax::concurrent_statements::*;
use crate::syntax::tests::{check_nodes, node};

#[test]
fn block_header() {
    let syntax_node = node::<BlockHeaderSyntax>(
        Parser::block_header,
        r#"
generic (foo: integer);
generic map (foo => 0);
port (bar: integer);
port map (bar => 1);
    "#,
    );
    check!(syntax_node,
        generic_clause     => "generic (foo: integer);",
        generic_map_aspect => "generic map (foo => 0);",
        port_clause        => "port (bar: integer);",
        port_map_aspect    => "port map (bar => 1);"
    );

    let syntax_node = node::<BlockHeaderSyntax>(
        Parser::block_header,
        r#"
generic (foo: integer);
port (bar: integer);
port map (bar => 1);
    "#,
    );
    check!(syntax_node,
        generic_clause => "generic (foo: integer);",
        generic_map_aspect => None,
        port_clause => "port (bar: integer);",
        port_map_aspect => "port map (bar => 1);"
    );

    let syntax_node = node::<BlockHeaderSyntax>(
        Parser::block_header,
        r#"
generic (foo: integer);
generic map (foo => 0);
port (bar: integer);
    "#,
    );
    check!(syntax_node,
        generic_clause => "generic (foo: integer);",
        generic_map_aspect => "generic map (foo => 0);",
        port_clause => "port (bar: integer);",
        port_map_aspect => None
    );

    let syntax_node = node::<BlockHeaderSyntax>(
        Parser::block_header,
        r#"
generic (foo: integer);
port (bar: integer);
    "#,
    );
    check!(syntax_node,
        generic_clause => "generic (foo: integer);",
        generic_map_aspect => None,
        port_clause => "port (bar: integer);",
        port_map_aspect => None
    );
}

#[test]
fn block_statement() {
    let syntax_node = node::<BlockStatementSyntax>(
        Parser::concurrent_statement,
        r#"
name: block (foo = true) is
    generic (bar: natural);
    constant baz: integer := 2;
begin
    x <= '2';
end block name;
    "#,
    );
    check!(syntax_node,
        label => "name:",
        block_token => "block",
        condition => "(foo = true)",
        is_token => "is",
        block_header => "generic (bar: natural);",
        begin_token => "begin",
        end_token => "end",
        trailing_block_token => "block",
        trailing_label_token => "name",
        semi_colon_token => ";"
    );
    check_nodes(syntax_node.declarations(), "constant baz: integer := 2;");
    check_nodes(syntax_node.concurrent_statements(), "x <= '2';");
}

#[test]
fn case_generate_alternative() {
    let syntax_node = node::<CaseGenerateAlternativeSyntax>(
        Parser::case_generate_alternative,
        r#"
        when alt_label: 1 | 2 =>
            foo <= 1;
    "#,
    );
    check!(syntax_node,
        when_token => "when",
        label => "alt_label:",
        choices => "1 | 2",
        right_arrow_token => "=>",
        generate_statement_body => "foo <= 1;"
    );

    let syntax_node = node::<CaseGenerateAlternativeSyntax>(
        Parser::case_generate_alternative,
        r#"
        when 1 | 2 =>
            foo <= 1;
    "#,
    );
    check!(syntax_node,
        when_token => "when",
        label => None,
        choices => "1 | 2",
        right_arrow_token => "=>",
        generate_statement_body => "foo <= 1;"
    );
}

#[test]
fn case_generate_statement() {
    let syntax_node = node::<CaseGenerateStatementSyntax>(
        Parser::concurrent_statement,
        r#"
        foo: case bar generate
          when 1 => x <= 2;
        end generate foo;
    "#,
    );
    check!(syntax_node,
        label => "foo:",
        case_token => "case",
        expression => "bar",
        generate_token => "generate",
        trailing_generate_token => "generate",
        trailing_generate_label_token => "foo",
        semi_colon_token => ";"
    );
    check_nodes(
        syntax_node.case_generate_alternatives(),
        "when 1 => x <= 2;",
    );

    let syntax_node = node::<CaseGenerateStatementSyntax>(
        Parser::concurrent_statement,
        r#"
        foo: case bar generate
          when 1 => x <= 2;
        end generate;
    "#,
    );
    check!(syntax_node,
        label => "foo:",
        case_token => "case",
        expression => "bar",
        generate_token => "generate",
        trailing_generate_token => "generate",
        trailing_generate_label_token => None,
        semi_colon_token => ";"
    );
    check_nodes(
        syntax_node.case_generate_alternatives(),
        "when 1 => x <= 2;",
    );
}

#[test]
fn component_instantiation_statement() {
    let syntax_node = node::<ComponentInstantiationStatementSyntax>(
        Parser::concurrent_statement,
        r#"
        foo: bar
            generic map (baz => 1)
            port map (foobar => '1');
    "#,
    );
    check!(syntax_node,
        label => "foo:",
        instantiated_unit => "bar",
        generic_map_aspect => "generic map (baz => 1)",
        port_map_aspect => "port map (foobar => '1')",
        semi_colon_token => ";"
    );

    let syntax_node = node::<ComponentInstantiationStatementSyntax>(
        Parser::concurrent_statement,
        r#"
        foo: bar
            port map (foobar => '1');
    "#,
    );
    check!(syntax_node,
        label => "foo:",
        instantiated_unit => "bar",
        generic_map_aspect => None,
        port_map_aspect => "port map (foobar => '1')",
        semi_colon_token => ";"
    );

    let syntax_node = node::<ConcurrentProcedureCallOrComponentInstantiationStatementSyntax>(
        Parser::concurrent_statement,
        r#"
        foo: bar;
    "#,
    );
    check!(syntax_node,
        label => "foo:",
        name => "bar",
        semi_colon_token => ";"
    );
}

#[test]
fn assertion_statement() {
    let syntax_node = node::<ConcurrentAssertionStatementSyntax>(
        Parser::concurrent_statement,
        "foo: postponed assert false;",
    );
    check!(syntax_node,
        label => "foo:",
        postponed_token => "postponed",
        assertion => "assert false",
        semi_colon_token => ";"
    );

    let syntax_node = node::<ConcurrentAssertionStatementSyntax>(
        Parser::concurrent_statement,
        "postponed assert false;",
    );
    check!(syntax_node,
        label => None,
        postponed_token => "postponed",
        assertion => "assert false",
        semi_colon_token => ";"
    );

    let syntax_node = node::<ConcurrentAssertionStatementSyntax>(
        Parser::concurrent_statement,
        "foo: assert false;",
    );
    check!(syntax_node,
        label => "foo:",
        postponed_token => None,
        assertion => "assert false",
        semi_colon_token => ";"
    );

    let syntax_node =
        node::<ConcurrentAssertionStatementSyntax>(Parser::concurrent_statement, "assert false;");
    check!(syntax_node,
        label => None,
        postponed_token => None,
        assertion => "assert false",
        semi_colon_token => ";"
    );
}

#[test]
fn concurrent_simple_signal_assignment() {
    let syntax_node = node::<ConcurrentSimpleSignalAssignmentSyntax>(
        Parser::concurrent_statement,
        "foo: postponed foo <= guarded transport bar;",
    );
    check!(syntax_node,
        label => "foo:",
        postponed_token => "postponed",
        target => "foo",
        lte_token => "<=",
        guarded_token => "guarded",
        delay_mechanism => "transport",
        waveform => "bar",
        semi_colon_token => ";"
    );

    let syntax_node = node::<ConcurrentSimpleSignalAssignmentSyntax>(
        Parser::concurrent_statement,
        "postponed foo <= guarded transport bar;",
    );
    check!(syntax_node,
        label => None,
        postponed_token => "postponed",
        target => "foo",
        lte_token => "<=",
        guarded_token => "guarded",
        delay_mechanism => "transport",
        waveform => "bar",
        semi_colon_token => ";"
    );

    let syntax_node = node::<ConcurrentSimpleSignalAssignmentSyntax>(
        Parser::concurrent_statement,
        "foo <= guarded transport bar;",
    );
    check!(syntax_node,
        label => None,
        postponed_token => None,
        target => "foo",
        lte_token => "<=",
        guarded_token => "guarded",
        delay_mechanism => "transport",
        waveform => "bar",
        semi_colon_token => ";"
    );

    let syntax_node = node::<ConcurrentSimpleSignalAssignmentSyntax>(
        Parser::concurrent_statement,
        "foo <= transport bar;",
    );
    check!(syntax_node,
        label => None,
        postponed_token => None,
        target => "foo",
        lte_token => "<=",
        guarded_token => None,
        delay_mechanism => "transport",
        waveform => "bar",
        semi_colon_token => ";"
    );

    let syntax_node =
        node::<ConcurrentSimpleSignalAssignmentSyntax>(Parser::concurrent_statement, "foo <= bar;");
    check!(syntax_node,
        label => None,
        postponed_token => None,
        target => "foo",
        lte_token => "<=",
        guarded_token => None,
        delay_mechanism => None,
        waveform => "bar",
        semi_colon_token => ";"
    );
}

#[test]
fn concurrent_conditional_signal_assignment() {
    let syntax_node = node::<ConcurrentConditionalSignalAssignmentSyntax>(
        Parser::concurrent_statement,
        "foo: postponed bar <= guarded inertial '1' when condition;",
    );
    check!(syntax_node,
        label => "foo:",
        postponed_token => "postponed",
        target => "bar",
        lte_token => "<=",
        guarded_token => "guarded",
        delay_mechanism => "inertial",
        conditional_waveforms => "'1' when condition",
        semi_colon_token => ";",
    );

    let syntax_node = node::<ConcurrentConditionalSignalAssignmentSyntax>(
        Parser::concurrent_statement,
        "postponed bar <= guarded inertial '1' when condition;",
    );
    check!(syntax_node,
        label => None,
        postponed_token => "postponed",
        target => "bar",
        lte_token => "<=",
        guarded_token => "guarded",
        delay_mechanism => "inertial",
        conditional_waveforms => "'1' when condition",
        semi_colon_token => ";",
    );

    let syntax_node = node::<ConcurrentConditionalSignalAssignmentSyntax>(
        Parser::concurrent_statement,
        "foo: bar <= inertial '1' when condition;",
    );
    check!(syntax_node,
        label => "foo:",
        postponed_token => None,
        target => "bar",
        lte_token => "<=",
        guarded_token => None,
        delay_mechanism => "inertial",
        conditional_waveforms => "'1' when condition",
        semi_colon_token => ";",
    );

    let syntax_node = node::<ConcurrentConditionalSignalAssignmentSyntax>(
        Parser::concurrent_statement,
        "bar <= '1' when condition;",
    );
    check!(syntax_node,
        label => None,
        postponed_token => None,
        target => "bar",
        lte_token => "<=",
        guarded_token => None,
        delay_mechanism => None,
        conditional_waveforms => "'1' when condition",
        semi_colon_token => ";",
    );
}

// TODO: target can be name or aggregate; conflicts with expression
#[test]
fn concurrent_selected_signal_assignment() {
    let syntax_node = node::<ConcurrentSelectedSignalAssignmentSyntax>(
        Parser::concurrent_statement,
        "foo: postponed with expr select? bar <= guarded transport '1' when 0;",
    );
    check!(syntax_node,
        label => "foo:",
        postponed_token => "postponed",
        with_token => "with",
        expression => "expr",
        select_token => "select",
        que_token => "?",
        target => "bar",
        lte_token => "<=",
        guarded_token => "guarded",
        delay_mechanism => "transport",
        selected_waveforms => "'1' when 0",
        semi_colon_token => ";"
    );

    let syntax_node = node::<ConcurrentSelectedSignalAssignmentSyntax>(
        Parser::concurrent_statement,
        "postponed with expr select bar <= guarded transport '1' when 0, '0' when others;",
    );
    check!(syntax_node,
        label => None,
        postponed_token => "postponed",
        with_token => "with",
        expression => "expr",
        select_token => "select",
        que_token => None,
        target => "bar",
        lte_token => "<=",
        guarded_token => "guarded",
        delay_mechanism => "transport",
        selected_waveforms => "'1' when 0, '0' when others",
        semi_colon_token => ";"
    );

    let syntax_node = node::<ConcurrentSelectedSignalAssignmentSyntax>(
        Parser::concurrent_statement,
        "foo: with expr select bar <= transport '1' when 0, '0' when others;",
    );
    check!(syntax_node,
        label => "foo:",
        postponed_token => None,
        with_token => "with",
        expression => "expr",
        select_token => "select",
        que_token => None,
        target => "bar",
        lte_token => "<=",
        guarded_token => None,
        delay_mechanism => "transport",
        selected_waveforms => "'1' when 0, '0' when others",
        semi_colon_token => ";"
    );

    let syntax_node = node::<ConcurrentSelectedSignalAssignmentSyntax>(
        Parser::concurrent_statement,
        "with expr select bar <= '1' when 0, '0' when others;",
    );
    check!(syntax_node,
        label => None,
        postponed_token => None,
        with_token => "with",
        expression => "expr",
        select_token => "select",
        que_token => None,
        target => "bar",
        lte_token => "<=",
        guarded_token => None,
        delay_mechanism => None,
        selected_waveforms => "'1' when 0, '0' when others",
        semi_colon_token => ";"
    );
}

#[test]
fn for_generate_statement() {
    let syntax_node = node::<ForGenerateStatementSyntax>(
        Parser::concurrent_statement,
        r#"
        foo: for i in 0 to 5 generate
            bar <= '1';
        end generate foo;
        "#,
    );
    check!(syntax_node,
        label => "foo:",
        for_token => "for",
        parameter_specification => "i in 0 to 5",
        generate_token => "generate",
        generate_statement_body => "bar <= '1';",
        end_token => "end",
        trailing_generate_token_token => "generate",
        trailing_generate_label_token => "foo",
        semi_colon_token => ";"
    );

    let syntax_node = node::<ForGenerateStatementSyntax>(
        Parser::concurrent_statement,
        r#"
        foo: for i in 0 to 5 generate
            bar <= '1';
        end generate;
        "#,
    );
    check!(syntax_node,
        label => "foo:",
        for_token => "for",
        parameter_specification => "i in 0 to 5",
        generate_token => "generate",
        generate_statement_body => "bar <= '1';",
        end_token => "end",
        trailing_generate_token_token => "generate",
        trailing_generate_label_token => None,
        semi_colon_token => ";"
    );
}

#[test]
fn generate_statement_body() {
    let syntax_node = node::<GenerateStatementBodySyntax>(
        Parser::generate_statement_body,
        r#"
          signal bar : boolean;
        begin
          bar <= '1';
        end foo;
        "#,
    );

    check!(syntax_node,
        begin_token => "begin",
        end_token => "end",
        alternative_label_token => "foo",
        semi_colon_token => ";",
    );
    check_nodes(syntax_node.declarations(), "signal bar : boolean;");
    check_nodes(syntax_node.concurrent_statements(), "bar <= '1';");

    let syntax_node = node::<GenerateStatementBodySyntax>(
        Parser::generate_statement_body,
        r#"
          signal bar : boolean;
        end foo;
        "#,
    );

    check!(syntax_node,
        begin_token => None,
        end_token => "end",
        alternative_label_token => "foo",
        semi_colon_token => ";",
    );
    check_nodes(syntax_node.declarations(), "signal bar : boolean;");

    let syntax_node = node::<GenerateStatementBodySyntax>(
        Parser::generate_statement_body,
        r#"
          signal bar : boolean;
        "#,
    );

    check!(syntax_node,
        begin_token => None,
        end_token => None,
        alternative_label_token => None,
        semi_colon_token => None,
    );
    check_nodes(syntax_node.declarations(), "signal bar : boolean;");
}

#[test]
fn if_generate_elsif() {
    let syntax_node = node::<IfGenerateElsifSyntax>(
        Parser::if_generate_elsif,
        r#"
          elsif alt_label: foo generate
              baz <= '1';
        "#,
    );

    check!(syntax_node,
        elsif_token => "elsif",
        label => "alt_label:",
        expression => "foo",
        generate_token => "generate",
        generate_statement_body => "baz <= '1';"
    );

    let syntax_node = node::<IfGenerateElsifSyntax>(
        Parser::if_generate_elsif,
        r#"
          elsif foo generate
              baz <= '1';
        "#,
    );

    check!(syntax_node,
        elsif_token => "elsif",
        label => None,
        expression => "foo",
        generate_token => "generate",
        generate_statement_body => "baz <= '1';"
    );
}

#[test]
fn if_generate_else() {
    let syntax_node = node::<IfGenerateElseSyntax>(
        Parser::if_generate_else,
        r#"
          else alt_label: generate
              baz <= '1';
        "#,
    );
    check!(syntax_node,
        else_token => "else",
        label => "alt_label:",
        generate_token => "generate",
        generate_statement_body => "baz <= '1';"
    );

    let syntax_node = node::<IfGenerateElseSyntax>(
        Parser::if_generate_else,
        r#"
          else generate
              baz <= '1';
        "#,
    );

    check!(syntax_node,
        else_token => "else",
        label => None,
        generate_token => "generate",
        generate_statement_body => "baz <= '1';"
    );
}

#[test]
fn if_generate() {
    let syntax_node = node::<IfGenerateStatementSyntax>(
        Parser::concurrent_statement,
        r#"
        label1: if alt_label1: true generate foo <= '0';
        elsif false generate foo <= '1';
        else generate foo <= 'X';
        end generate label1;
        "#,
    );
    check!(syntax_node,
        label => "label1:",
        if_token => "if",
        alternative_label => "alt_label1:",
        condition => "true",
        generate_token => "generate",
        generate_statement_body => "foo <= '0';",
        if_generate_else => "else generate foo <= 'X';",
        end_token => "end",
        trailing_generate_token => "generate",
        trailing_generate_label_token => "label1",
        semi_colon_token => ";"
    );
    check_nodes(
        syntax_node.if_generate_elsifs(),
        "elsif false generate foo <= '1';",
    );
}

#[test]
fn component_instantiated_unit() {
    let syntax_node = node::<ComponentInstantiatedUnitSyntax>(
        Parser::component_instantiated_unit,
        "component foo",
    );
    check!(syntax_node,
        component_token => "component",
        name => "foo"
    );

    let syntax_node =
        node::<ComponentInstantiatedUnitSyntax>(Parser::component_instantiated_unit, "foo");
    check!(syntax_node,
        component_token => None,
        name => "foo"
    );
}

#[test]
fn entity_instantiated_unit() {
    let syntax_node =
        node::<EntityInstantiatedUnitSyntax>(Parser::entity_instantiated_unit, "entity foo");
    check!(syntax_node,
        entity_token => "entity",
        name => "foo",
    );

    let syntax_node =
        node::<EntityInstantiatedUnitSyntax>(Parser::entity_instantiated_unit, "entity foo(bar)");
    check!(syntax_node,
        entity_token => "entity",
        name => "foo(bar)"
    );
}

#[test]
fn configuration_instantiated_unit() {
    let syntax_node = node::<ConfigurationInstantiatedUnitSyntax>(
        Parser::configuration_instantiated_unit,
        "configuration foo",
    );
    check!(syntax_node,
        configuration_token => "configuration",
        name => "foo",
    );
}

#[test]
fn process_statement() {
    let syntax_node = node::<ProcessStatementSyntax>(
        Parser::concurrent_statement,
        r#"
process
begin
end process;
    "#,
    );
    check!(syntax_node,
        label => None,
        postponed_token => None,
        process_token => "process",
        parenthesized_process_sensitivity_list => None,
        is_token => None,
        end_token => "end",
        trailing_postponed_token => None,
        trailing_process_token => "process",
        trailing_process_label_token => None,
        semi_colon_token => ";"
    );

    let syntax_node = node::<ProcessStatementSyntax>(
        Parser::concurrent_statement,
        r#"
foo: process
begin
end process foo;
    "#,
    );
    check!(syntax_node,
        label => "foo:",
        postponed_token => None,
        process_token => "process",
        parenthesized_process_sensitivity_list => None,
        is_token => None,
        end_token => "end",
        trailing_postponed_token => None,
        trailing_process_token => "process",
        trailing_process_label_token => "foo",
        semi_colon_token => ";"
    );

    let syntax_node = node::<ProcessStatementSyntax>(
        Parser::concurrent_statement,
        r#"
foo: postponed process
begin
end postponed process;
    "#,
    );
    check!(syntax_node,
        label => "foo:",
        postponed_token => "postponed",
        process_token => "process",
        parenthesized_process_sensitivity_list => None,
        is_token => None,
        end_token => "end",
        trailing_postponed_token => "postponed",
        trailing_process_token => "process",
        trailing_process_label_token => None,
        semi_colon_token => ";"
    );

    let syntax_node = node::<ProcessStatementSyntax>(
        Parser::concurrent_statement,
        r#"
foo: postponed process(all)
begin
end process;
    "#,
    );
    check!(syntax_node,
        label => "foo:",
        postponed_token => "postponed",
        process_token => "process",
        parenthesized_process_sensitivity_list => "(all)",
        is_token => None,
        end_token => "end",
        trailing_postponed_token => None,
        trailing_process_token => "process",
        trailing_process_label_token => None,
        semi_colon_token => ";"
    );

    let syntax_node = node::<ProcessStatementSyntax>(
        Parser::concurrent_statement,
        r#"
foo: postponed process(foo,bar)
begin
end process;
    "#,
    );
    check!(syntax_node,
        label => "foo:",
        postponed_token => "postponed",
        process_token => "process",
        parenthesized_process_sensitivity_list => "(foo,bar)",
        is_token => None,
        end_token => "end",
        trailing_postponed_token => None,
        trailing_process_token => "process",
        trailing_process_label_token => None,
        semi_colon_token => ";"
    );

    let syntax_node = node::<ProcessStatementSyntax>(
        Parser::concurrent_statement,
        r#"
process is
begin
end process;
    "#,
    );
    check!(syntax_node,
        label => None,
        postponed_token => None,
        process_token => "process",
        parenthesized_process_sensitivity_list => None,
        is_token => "is",
        end_token => "end",
        trailing_postponed_token => None,
        trailing_process_token => "process",
        trailing_process_label_token => None,
        semi_colon_token => ";"
    );
}
