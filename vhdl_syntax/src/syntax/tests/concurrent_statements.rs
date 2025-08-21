// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::check;
use crate::parser::Parser;
use crate::syntax::tests::{check_nodes, node};
use crate::syntax::{
    BlockHeaderSyntax, BlockStatementSyntax, CaseGenerateAlternativeSyntax,
    CaseGenerateStatementSyntax, ComponentInstantiationStatementSyntax,
    ConcurrentAssertionStatementSyntax, ConcurrentConditionalSignalAssignmentSyntax,
    ConcurrentProcedureCallOrComponentInstantiationStatementSyntax,
    ConcurrentSelectedSignalAssignmentSyntax, ConcurrentSimpleSignalAssignmentSyntax,
};

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
