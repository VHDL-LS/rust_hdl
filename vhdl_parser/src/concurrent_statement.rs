// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use ast::{
    Alternative, AssignmentRightHand, BlockStatement, CaseGenerateStatement,
    ConcurrentAssertStatement, ConcurrentProcedureCall, ConcurrentSignalAssignment,
    ConcurrentStatement, Conditional, Declaration, ForGenerateStatement, FunctionCall,
    GenerateBody, Ident, IfGenerateStatement, InstantiatedUnit, InstantiationStatement,
    LabeledConcurrentStatement, Name, ProcessStatement, Target,
};
use common::warning_on_end_identifier_mismatch;
use declarative_part::{is_declarative_part, parse_declarative_part};
use expression::parse_aggregate_leftpar_known;
use expression::{parse_choices, parse_expression};
use message::{error, MessageHandler, ParseResult};
use names::{
    expression_to_ident, parse_association_list, parse_name_initial_token, parse_selected_name,
    to_selected_name, to_simple_name,
};
use range::parse_discrete_range;
use sequential_statement::{
    parse_assert_statement_known_keyword, parse_labeled_sequential_statements, parse_selection,
    parse_signal_assignment_right_hand, parse_target,
};
use source::WithPos;
use tokenizer::{Kind::*, Token};
use tokenstream::TokenStream;
use waveform::{parse_delay_mechanism, parse_waveform};

/// LRM 11.2 Block statement
pub fn parse_block_statement(
    stream: &mut TokenStream,
    messages: &mut MessageHandler,
) -> ParseResult<BlockStatement> {
    let token = stream.peek_expect()?;
    let guard_condition = {
        match token.kind {
            Is => {
                stream.move_after(&token);
                None
            }
            LeftPar => {
                stream.move_after(&token);
                let expr = parse_expression(stream)?;
                stream.pop_if_kind(RightPar)?;
                Some(expr)
            }
            _ => None,
        }
    };
    let decl = parse_declarative_part(stream, messages, true)?;
    let statements = parse_labeled_concurrent_statements(stream, messages)?;
    stream.expect_kind(Block)?;
    // @TODO check name
    stream.pop_if_kind(Identifier)?;
    stream.expect_kind(SemiColon)?;
    Ok(BlockStatement {
        guard_condition,
        decl,
        statements,
    })
}

/// LRM 11.3 Process statement
pub fn parse_process_statement(
    stream: &mut TokenStream,
    postponed: bool,
    messages: &mut MessageHandler,
) -> ParseResult<ProcessStatement> {
    let token = stream.peek_expect()?;
    let sensitivity_list = {
        match token.kind {
            LeftPar => {
                stream.move_after(&token);
                let mut names = Vec::with_capacity(1);
                loop {
                    let token = stream.expect()?;
                    match token.kind {
                        RightPar => {
                            break names;
                        }
                        Comma => {}
                        _ => {
                            names.push(parse_name_initial_token(stream, token)?);
                        }
                    };
                }
            }
            _ => Vec::new(),
        }
    };
    stream.pop_if_kind(Is)?;
    let decl = parse_declarative_part(stream, messages, true)?;
    let (statements, end_token) = parse_labeled_sequential_statements(stream, messages)?;
    try_token_kind!(end_token, End => {});
    stream.expect_kind(Process)?;
    // @TODO check name
    stream.pop_if_kind(Identifier)?;
    stream.expect_kind(SemiColon)?;
    Ok(ProcessStatement {
        postponed,
        sensitivity_list,
        decl,
        statements,
    })
}

fn to_procedure_call(
    target: WithPos<Target>,
    postponed: bool,
) -> ParseResult<ConcurrentProcedureCall> {
    match target.item {
        Target::Name(Name::FunctionCall(call)) => Ok(ConcurrentProcedureCall {
            postponed,
            call: *call,
        }),
        Target::Name(name) => Ok(ConcurrentProcedureCall {
            postponed,
            call: FunctionCall {
                name: WithPos::new(name, target.pos),
                parameters: vec![],
            },
        }),
        Target::Aggregate(..) => {
            return Err(error(target, "Expected procedure call, got aggregate"));
        }
    }
}

fn parse_assignment_or_procedure_call(
    stream: &mut TokenStream,
    token: Token,
    target: WithPos<Target>,
) -> ParseResult<ConcurrentStatement> {
    match_token_kind!(
        token,
        LTE => {
            // @TODO postponed
            let postponed = false;
            // @TODO guarded
            let guarded = false;
            let delay_mechanism = parse_delay_mechanism(stream)?;
            Ok(ConcurrentStatement::Assignment(ConcurrentSignalAssignment {
                postponed,
                guarded,
                target,
                delay_mechanism,
                rhs: parse_signal_assignment_right_hand(stream)?
            }))
        },
        SemiColon => {
            Ok(ConcurrentStatement::ProcedureCall(to_procedure_call(target, false)?))
        })
}

fn parse_selected_signal_assignment(
    stream: &mut TokenStream,
    postponed: bool,
) -> ParseResult<ConcurrentSignalAssignment> {
    let expression = parse_expression(stream)?;
    stream.expect_kind(Select)?;
    let target = parse_target(stream)?;
    stream.expect_kind(LTE)?;
    // @TODO guarded
    let guarded = false;
    let delay_mechanism = parse_delay_mechanism(stream)?;
    let rhs = AssignmentRightHand::Selected(parse_selection(stream, expression, parse_waveform)?);
    Ok(ConcurrentSignalAssignment {
        postponed,
        guarded,
        target,
        delay_mechanism,
        rhs,
    })
}

pub fn parse_concurrent_assert_statement(
    stream: &mut TokenStream,
    postponed: bool,
) -> ParseResult<ConcurrentAssertStatement> {
    Ok(ConcurrentAssertStatement {
        postponed,
        statement: parse_assert_statement_known_keyword(stream)?,
    })
}

pub fn parse_instantiation_statement(
    stream: &mut TokenStream,
    unit: InstantiatedUnit,
) -> ParseResult<InstantiationStatement> {
    let generic_map = {
        if stream.skip_if_kind(Generic)? {
            stream.expect_kind(Map)?;
            parse_association_list(stream)?
        } else {
            Vec::new()
        }
    };
    let port_map = {
        if stream.skip_if_kind(Port)? {
            stream.expect_kind(Map)?;
            parse_association_list(stream)?
        } else {
            Vec::new()
        }
    };
    let inst = InstantiationStatement {
        unit,
        generic_map,
        port_map,
    };
    stream.expect_kind(SemiColon)?;
    Ok(inst)
}

fn parse_optional_declarative_part(
    stream: &mut TokenStream,
    messages: &mut MessageHandler,
) -> ParseResult<Option<Vec<Declaration>>> {
    if is_declarative_part(stream, true)? {
        Ok(Some(parse_declarative_part(stream, messages, true)?))
    } else {
        Ok(None)
    }
}

fn parse_generate_body_end_token(
    stream: &mut TokenStream,
    alternative_label: Option<Ident>,
    messages: &mut MessageHandler,
) -> ParseResult<(GenerateBody, Token)> {
    let decl = parse_optional_declarative_part(stream, messages)?;
    let (statements, mut end_token) =
        parse_labeled_concurrent_statements_end_token(stream, messages)?;

    // Potential inner end [ alternative_label ];
    if end_token.kind == End {
        let token = stream.peek_expect()?;
        try_token_kind!(
            token,
            Generate => {
                // Outer end
            },
            SemiColon => {
                // Inner end no label
                stream.move_after(&token);
                stream.expect_kind(SemiColon)?;
                end_token = stream.expect()?;
            },
            Identifier => {
                // Inner with identifier
                let end_ident = token.expect_ident()?;
                if let Some(ref ident) = alternative_label {
                    warning_on_end_identifier_mismatch(ident, &Some(end_ident));
                };
                stream.move_after(&token);
                stream.expect_kind(SemiColon)?;
                end_token = stream.expect()?;
            });
    }

    let body = GenerateBody {
        alternative_label,
        decl,
        statements,
    };

    Ok((body, end_token))
}

fn parse_generate_body(
    stream: &mut TokenStream,
    alternative_label: Option<Ident>,
    messages: &mut MessageHandler,
) -> ParseResult<GenerateBody> {
    let (body, end_token) = parse_generate_body_end_token(stream, alternative_label, messages)?;
    end_token.expect_kind(End)?;
    Ok(body)
}

/// 11.8 Generate statements
fn parse_for_generate_statement(
    stream: &mut TokenStream,
    messages: &mut MessageHandler,
) -> ParseResult<ForGenerateStatement> {
    let index_name = stream.expect_ident()?;
    stream.expect_kind(In)?;
    let discrete_range = parse_discrete_range(stream)?;
    stream.expect_kind(Generate)?;
    let body = parse_generate_body(stream, None, messages)?;
    stream.expect_kind(Generate)?;
    // @TODO check identifier
    stream.pop_if_kind(Identifier)?;
    stream.expect_kind(SemiColon)?;

    Ok(ForGenerateStatement {
        index_name,
        discrete_range,
        body,
    })
}

/// 11.8 Generate statements
fn parse_if_generate_statement(
    stream: &mut TokenStream,
    messages: &mut MessageHandler,
) -> ParseResult<IfGenerateStatement> {
    let mut conditionals = Vec::new();
    let else_branch;

    loop {
        let mut condition = parse_expression(stream)?;
        let mut alternative_label = None;
        if stream.skip_if_kind(Colon)? {
            alternative_label = Some(expression_to_ident(condition)?);
            condition = parse_expression(stream)?;
        }
        stream.expect_kind(Generate)?;
        let (body, end_token) = parse_generate_body_end_token(stream, alternative_label, messages)?;

        let conditional = Conditional {
            condition,
            item: body,
        };

        conditionals.push(conditional);

        try_token_kind!(
            end_token,
            End => {
                else_branch = None;
                break;
            },
            Elsif => {
                continue;
            },
            Else => {
                let token = stream.expect()?;
                let alternative_label = try_token_kind!(
                    token,
                    Generate => {
                        None
                    },
                    Identifier => {
                        stream.expect_kind(Colon)?;
                        stream.expect_kind(Generate)?;
                        Some(token.expect_ident()?)
                    }
                );
                let body = parse_generate_body(stream, alternative_label, messages)?;
                else_branch = Some(body);
                break;
            }
        );
    }

    stream.expect_kind(Generate)?;
    // @TODO check identifier
    stream.pop_if_kind(Identifier)?;
    stream.expect_kind(SemiColon)?;

    Ok(IfGenerateStatement {
        conditionals: conditionals,
        else_item: else_branch,
    })
}

/// 11.8 Generate statements
fn parse_case_generate_statement(
    stream: &mut TokenStream,
    messages: &mut MessageHandler,
) -> ParseResult<CaseGenerateStatement> {
    let expression = parse_expression(stream)?;
    stream.expect_kind(Generate)?;
    stream.expect_kind(When)?;

    let mut alternatives = Vec::with_capacity(2);
    loop {
        let alternative_label = {
            if stream.is_peek_kinds(&[Identifier, Colon])? {
                let ident = stream.expect_ident()?;
                stream.expect_kind(Colon)?;
                Some(ident)
            } else {
                None
            }
        };
        let choices = parse_choices(stream)?;
        stream.expect_kind(RightArrow)?;
        let (body, end_token) = parse_generate_body_end_token(stream, alternative_label, messages)?;
        alternatives.push(Alternative {
            choices,
            item: body,
        });

        try_token_kind!(
            end_token,
            End => break,
            When => continue
        );
    }

    stream.expect_kind(Generate)?;
    // @TODO check identifier
    stream.pop_if_kind(Identifier)?;
    stream.expect_kind(SemiColon)?;

    Ok(CaseGenerateStatement {
        expression,
        alternatives,
    })
}

pub fn parse_concurrent_statement(
    stream: &mut TokenStream,
    token: Token,
    messages: &mut MessageHandler,
) -> ParseResult<ConcurrentStatement> {
    let statement = {
        try_token_kind!(
            token,
            Block => {
                ConcurrentStatement::Block(parse_block_statement(stream, messages)?)
            },
            Process => {
                ConcurrentStatement::Process(parse_process_statement(stream, false, messages)?)
            },
            Component => {
                let unit = InstantiatedUnit::Component(parse_selected_name(stream)?);
                ConcurrentStatement::Instance(parse_instantiation_statement(stream, unit)?)
            },
            Configuration => {
                let unit = InstantiatedUnit::Configuration(parse_selected_name(stream)?);
                ConcurrentStatement::Instance(parse_instantiation_statement(stream, unit)?)
            },
            Entity => {
                let name = parse_selected_name(stream)?;
                let arch = {
                    if stream.skip_if_kind(LeftPar)? {
                        let ident = stream.expect_ident()?;
                        stream.expect_kind(RightPar)?;
                        Some(ident)
                    } else {
                        None
                    }
                };
                let unit = InstantiatedUnit::Entity(name, arch);
                ConcurrentStatement::Instance(parse_instantiation_statement(stream, unit)?)
            },
            For => ConcurrentStatement::ForGenerate(parse_for_generate_statement(stream, messages)?),
            If => ConcurrentStatement::IfGenerate(parse_if_generate_statement(stream, messages)?),
            Case => ConcurrentStatement::CaseGenerate(parse_case_generate_statement(stream, messages)?),
            Assert => ConcurrentStatement::Assert(parse_concurrent_assert_statement(stream, false)?),
            Postponed => {
                let token = stream.expect()?;
                match token.kind {
                    Process => ConcurrentStatement::Process(parse_process_statement(stream, true, messages)?),
                    Assert => ConcurrentStatement::Assert(parse_concurrent_assert_statement(stream, true)?),
                    With => ConcurrentStatement::Assignment(parse_selected_signal_assignment(stream, true)?),
                    _ => {
                        let target = parse_name_initial_token(stream, token)?.map_into(Target::Name);
                        stream.expect_kind(SemiColon)?;
                        ConcurrentStatement::ProcedureCall(to_procedure_call(target, true)?)
                    }
                }
            },
            With => ConcurrentStatement::Assignment(parse_selected_signal_assignment(stream, false)?),
            Identifier => {
                let name = parse_name_initial_token(stream, token)?;
                let token = stream.peek_expect()?;
                match token.kind {
                    Generic|Port => {
                        let unit = InstantiatedUnit::Component(to_selected_name(&name)?);
                        ConcurrentStatement::Instance(parse_instantiation_statement(stream, unit)?)
                    }
                    _ => {
                        stream.move_after(&token);
                        parse_assignment_or_procedure_call(stream, token, name.map_into(Target::Name))?
                    }
                }
            },
            LeftPar => {
                let target = parse_aggregate_leftpar_known(stream)?.map_into(Target::Aggregate);
                let token = stream.expect()?;
                parse_assignment_or_procedure_call(stream, token, target)?
            }
        )
    };
    Ok(statement)
}

pub fn parse_labeled_concurrent_statements_end_token(
    stream: &mut TokenStream,
    messages: &mut MessageHandler,
) -> ParseResult<(Vec<LabeledConcurrentStatement>, Token)> {
    let mut statements = Vec::new();
    loop {
        let token = stream.expect()?;
        match token.kind {
            End | Elsif | Else | When => {
                break Ok((statements, token));
            }
            _ => {
                statements.push(parse_labeled_concurrent_statement_initial_token(
                    stream, token, messages,
                )?);
            }
        }
    }
}

pub fn parse_labeled_concurrent_statements(
    stream: &mut TokenStream,
    messages: &mut MessageHandler,
) -> ParseResult<Vec<LabeledConcurrentStatement>> {
    let (statement, _) = parse_labeled_concurrent_statements_end_token(stream, messages)?;
    Ok(statement)
}

pub fn parse_labeled_concurrent_statement_initial_token(
    stream: &mut TokenStream,
    token: Token,
    messages: &mut MessageHandler,
) -> ParseResult<LabeledConcurrentStatement> {
    if token.kind == Identifier {
        let name = parse_name_initial_token(stream, token)?;
        let token = stream.expect()?;
        if token.kind == Colon {
            let label = Some(to_simple_name(name)?);
            let token = stream.expect()?;
            let statement = parse_concurrent_statement(stream, token, messages)?;
            Ok(LabeledConcurrentStatement { label, statement })
        } else {
            let target = name.map_into(Target::Name);
            let statement = parse_assignment_or_procedure_call(stream, token, target)?;
            Ok(LabeledConcurrentStatement {
                label: None,
                statement,
            })
        }
    } else {
        let statement = parse_concurrent_statement(stream, token, messages)?;
        Ok(LabeledConcurrentStatement {
            label: None,
            statement,
        })
    }
}

#[cfg(test)]
pub fn parse_labeled_concurrent_statement(
    stream: &mut TokenStream,
    messages: &mut MessageHandler,
) -> ParseResult<LabeledConcurrentStatement> {
    let token = stream.expect()?;
    parse_labeled_concurrent_statement_initial_token(stream, token, messages)
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::{Alternative, AssertStatement, DelayMechanism, Selection};
    use test_util::with_stream_no_messages;

    #[test]
    fn test_concurrent_procedure() {
        let (util, stmt) = with_stream_no_messages(
            parse_labeled_concurrent_statement,
            "\
            foo(clk);
",
        );
        let call = ConcurrentProcedureCall {
            postponed: false,
            call: util.function_call("foo(clk)"),
        };
        assert_eq!(stmt.label, None);
        assert_eq!(stmt.statement, ConcurrentStatement::ProcedureCall(call));
    }

    #[test]
    fn test_postponed_concurrent_procedure() {
        let (util, stmt) = with_stream_no_messages(
            parse_labeled_concurrent_statement,
            "\
            postponed foo(clk);
",
        );
        let call = ConcurrentProcedureCall {
            postponed: true,
            call: util.function_call("foo(clk)"),
        };
        assert_eq!(stmt.label, None);
        assert_eq!(stmt.statement, ConcurrentStatement::ProcedureCall(call));
    }

    #[test]
    fn test_labeled_concurrent_procedure() {
        let (util, stmt) = with_stream_no_messages(
            parse_labeled_concurrent_statement,
            "\
            name: foo(clk);
",
        );
        let call = ConcurrentProcedureCall {
            postponed: false,
            call: util.function_call("foo(clk)"),
        };
        assert_eq!(stmt.label, Some(util.ident("name")));
        assert_eq!(stmt.statement, ConcurrentStatement::ProcedureCall(call));
    }

    #[test]
    fn test_concurrent_procedure_no_args() {
        let (util, stmt) = with_stream_no_messages(
            parse_labeled_concurrent_statement,
            "\
            foo;
",
        );
        let call = ConcurrentProcedureCall {
            postponed: false,
            call: util.function_call("foo"),
        };
        assert_eq!(stmt.label, None);
        assert_eq!(stmt.statement, ConcurrentStatement::ProcedureCall(call));
    }

    #[test]
    fn test_block() {
        let (util, stmt) = with_stream_no_messages(
            parse_labeled_concurrent_statement,
            "\
name : block
  constant const : natural := 0;
begin
  name2: foo(clk);
end block;
",
        );
        let call = ConcurrentProcedureCall {
            postponed: false,
            call: util.function_call("foo(clk)"),
        };

        let block = BlockStatement {
            guard_condition: None,
            decl: util.declarative_part("constant const : natural := 0;"),
            statements: vec![LabeledConcurrentStatement {
                label: Some(util.ident("name2")),
                statement: ConcurrentStatement::ProcedureCall(call),
            }],
        };
        assert_eq!(stmt.label, Some(util.ident("name")));
        assert_eq!(stmt.statement, ConcurrentStatement::Block(block));
    }

    #[test]
    fn test_block_variant() {
        let (util, stmt) = with_stream_no_messages(
            parse_labeled_concurrent_statement,
            "\
name : block is
begin
end block name;
",
        );
        let block = BlockStatement {
            guard_condition: None,
            decl: vec![],
            statements: vec![],
        };
        assert_eq!(stmt.label, Some(util.ident("name")));
        assert_eq!(stmt.statement, ConcurrentStatement::Block(block));
    }

    #[test]
    fn test_guarded_block() {
        let (util, stmt) = with_stream_no_messages(
            parse_labeled_concurrent_statement,
            "\
name : block (cond = true)
begin
end block;
",
        );
        let block = BlockStatement {
            guard_condition: Some(util.expr("cond = true")),
            decl: vec![],
            statements: vec![],
        };
        assert_eq!(stmt.label, Some(util.ident("name")));
        assert_eq!(stmt.statement, ConcurrentStatement::Block(block));
    }

    #[test]
    fn test_process_statement() {
        let (_, stmt) = with_stream_no_messages(
            parse_labeled_concurrent_statement,
            "\
process
begin
end process;
",
        );
        let process = ProcessStatement {
            postponed: false,
            sensitivity_list: vec![],
            decl: vec![],
            statements: vec![],
        };
        assert_eq!(stmt.label, None);
        assert_eq!(stmt.statement, ConcurrentStatement::Process(process));
    }

    #[test]
    fn test_process_statement_variant() {
        let (util, stmt) = with_stream_no_messages(
            parse_labeled_concurrent_statement,
            "\
name : process is
begin
end process name;
",
        );
        let process = ProcessStatement {
            postponed: false,
            sensitivity_list: vec![],
            decl: vec![],
            statements: vec![],
        };
        assert_eq!(stmt.label, Some(util.ident("name")));
        assert_eq!(stmt.statement, ConcurrentStatement::Process(process));
    }

    #[test]
    fn test_postponed_process_statement() {
        let (_, stmt) = with_stream_no_messages(
            parse_labeled_concurrent_statement,
            "\
postponed process
begin
end process;
",
        );
        let process = ProcessStatement {
            postponed: true,
            sensitivity_list: vec![],
            decl: vec![],
            statements: vec![],
        };
        assert_eq!(stmt.label, None);
        assert_eq!(stmt.statement, ConcurrentStatement::Process(process));
    }

    #[test]
    fn test_process_statement_sensitivity() {
        let (util, stmt) = with_stream_no_messages(
            parse_labeled_concurrent_statement,
            "\
process (clk, vec(1)) is
begin
end process;
",
        );
        let process = ProcessStatement {
            postponed: false,
            sensitivity_list: vec![util.name("clk"), util.name("vec(1)")],
            decl: vec![],
            statements: vec![],
        };
        assert_eq!(stmt.label, None);
        assert_eq!(stmt.statement, ConcurrentStatement::Process(process));
    }

    #[test]
    fn test_process_statement_full() {
        let (util, stmt) = with_stream_no_messages(
            parse_labeled_concurrent_statement,
            "\
process (all) is
  variable foo : boolean;
begin
  foo <= true;
  wait;
end process;
",
        );
        let process = ProcessStatement {
            postponed: false,
            sensitivity_list: vec![util.name("all")],
            decl: util.declarative_part("variable foo : boolean;"),
            statements: vec![
                util.sequential_statement("foo <= true;"),
                util.sequential_statement("wait;"),
            ],
        };
        assert_eq!(stmt.label, None);
        assert_eq!(stmt.statement, ConcurrentStatement::Process(process));
    }

    #[test]
    fn test_concurrent_assert() {
        let (util, stmt) = with_stream_no_messages(
            parse_labeled_concurrent_statement,
            "\
assert cond = true;
",
        );
        let assert = ConcurrentAssertStatement {
            postponed: false,
            statement: AssertStatement {
                condition: util.expr("cond = true"),
                report: None,
                severity: None,
            },
        };
        assert_eq!(stmt.label, None);
        assert_eq!(stmt.statement, ConcurrentStatement::Assert(assert));
    }

    #[test]
    fn test_postponed_concurrent_assert() {
        let (util, stmt) = with_stream_no_messages(
            parse_labeled_concurrent_statement,
            "\
postponed assert cond = true;
",
        );
        let assert = ConcurrentAssertStatement {
            postponed: true,
            statement: AssertStatement {
                condition: util.expr("cond = true"),
                report: None,
                severity: None,
            },
        };
        assert_eq!(stmt.label, None);
        assert_eq!(stmt.statement, ConcurrentStatement::Assert(assert));
    }

    #[test]
    fn test_concurrent_signal_assignment() {
        let (util, stmt) = with_stream_no_messages(
            parse_labeled_concurrent_statement,
            "\
foo <= bar(2 to 3);
",
        );
        let assign = ConcurrentSignalAssignment {
            postponed: false,
            guarded: false,
            target: util.name("foo").map_into(Target::Name),
            delay_mechanism: None,
            rhs: AssignmentRightHand::Simple(util.waveform("bar(2 to 3)")),
        };
        assert_eq!(stmt.label, None);
        assert_eq!(stmt.statement, ConcurrentStatement::Assignment(assign));
    }

    #[test]
    fn parse_selected_signal_assignment() {
        let (util, statement) = with_stream_no_messages(
            parse_labeled_concurrent_statement,
            "\
with x(0) + 1 select
   foo(0) <= transport bar(1,2) after 2 ns when 0|1;",
        );

        let selection = Selection {
            expression: util.expr("x(0) + 1"),
            alternatives: vec![Alternative {
                choices: util.choices("0|1"),
                item: util.waveform("bar(1,2) after 2 ns"),
            }],
        };

        assert_eq!(statement.label, None);
        assert_eq!(
            statement.statement,
            ConcurrentStatement::Assignment(ConcurrentSignalAssignment {
                postponed: false,
                guarded: false,
                target: util.name("foo(0)").map_into(Target::Name),
                delay_mechanism: Some(DelayMechanism::Transport),
                rhs: AssignmentRightHand::Selected(selection)
            })
        );
    }

    #[test]
    fn test_component_instantiation() {
        let (util, stmt) = with_stream_no_messages(
            parse_labeled_concurrent_statement,
            "\
inst: component lib.foo.bar;
",
        );

        let inst = InstantiationStatement {
            unit: InstantiatedUnit::Component(util.selected_name("lib.foo.bar")),
            generic_map: vec![],
            port_map: vec![],
        };
        assert_eq!(stmt.label, Some(util.ident("inst")));
        assert_eq!(stmt.statement, ConcurrentStatement::Instance(inst));
    }

    #[test]
    fn test_configuration_instantiation() {
        let (util, stmt) = with_stream_no_messages(
            parse_labeled_concurrent_statement,
            "\
inst: configuration lib.foo.bar;
",
        );

        let inst = InstantiationStatement {
            unit: InstantiatedUnit::Configuration(util.selected_name("lib.foo.bar")),
            generic_map: vec![],
            port_map: vec![],
        };
        assert_eq!(stmt.label, Some(util.ident("inst")));
        assert_eq!(stmt.statement, ConcurrentStatement::Instance(inst));
    }

    #[test]
    fn test_entity_instantiation() {
        let (util, stmt) = with_stream_no_messages(
            parse_labeled_concurrent_statement,
            "\
inst: entity lib.foo.bar;
",
        );

        let inst = InstantiationStatement {
            unit: InstantiatedUnit::Entity(util.selected_name("lib.foo.bar"), None),
            generic_map: vec![],
            port_map: vec![],
        };
        assert_eq!(stmt.label, Some(util.ident("inst")));
        assert_eq!(stmt.statement, ConcurrentStatement::Instance(inst));
    }

    #[test]
    fn test_entity_architecture_instantiation() {
        let (util, stmt) = with_stream_no_messages(
            parse_labeled_concurrent_statement,
            "\
inst: entity lib.foo.bar(arch);
",
        );

        let inst = InstantiationStatement {
            unit: InstantiatedUnit::Entity(
                util.selected_name("lib.foo.bar"),
                Some(util.ident("arch")),
            ),
            generic_map: vec![],
            port_map: vec![],
        };
        assert_eq!(stmt.label, Some(util.ident("inst")));
        assert_eq!(stmt.statement, ConcurrentStatement::Instance(inst));
    }

    #[test]
    fn test_component_aspect_maps() {
        let (util, stmt) = with_stream_no_messages(
            parse_labeled_concurrent_statement,
            "\
inst: component lib.foo.bar
  generic map (
   const => 1
  )
  port map (
   clk => clk_foo
  );
",
        );

        let inst = InstantiationStatement {
            unit: InstantiatedUnit::Component(util.selected_name("lib.foo.bar")),
            generic_map: util.association_list(
                "(
   const => 1
  )",
            ),
            port_map: util.association_list(
                "(
   clk => clk_foo
  )",
            ),
        };
        assert_eq!(stmt.label, Some(util.ident("inst")));
        assert_eq!(stmt.statement, ConcurrentStatement::Instance(inst));
    }

    #[test]
    fn test_component_no_keyword_port_aspect_map() {
        let (util, stmt) = with_stream_no_messages(
            parse_labeled_concurrent_statement,
            "\
inst: lib.foo.bar
  port map (
   clk => clk_foo
  );
",
        );

        let inst = InstantiationStatement {
            unit: InstantiatedUnit::Component(util.selected_name("lib.foo.bar")),
            generic_map: vec![],
            port_map: util.association_list(
                "(
   clk => clk_foo
  )",
            ),
        };
        assert_eq!(stmt.label, Some(util.ident("inst")));
        assert_eq!(stmt.statement, ConcurrentStatement::Instance(inst));
    }

    #[test]
    fn test_component_no_keyword_generic_aspect_map() {
        let (util, stmt) = with_stream_no_messages(
            parse_labeled_concurrent_statement,
            "\
            inst: lib.foo.bar
  generic map (
   const => 1
  );
",
        );

        let inst = InstantiationStatement {
            unit: InstantiatedUnit::Component(util.selected_name("lib.foo.bar")),
            generic_map: util.association_list(
                "(
   const => 1
  )",
            ),
            port_map: vec![],
        };
        assert_eq!(stmt.label, Some(util.ident("inst")));
        assert_eq!(stmt.statement, ConcurrentStatement::Instance(inst));
    }

    #[test]
    fn test_for_generate_empty() {
        let (util, stmt) = with_stream_no_messages(
            parse_labeled_concurrent_statement,
            "\
gen: for idx in 0 to 1 generate
end generate;
",
        );
        let gen = ForGenerateStatement {
            index_name: util.ident("idx"),
            discrete_range: util.discrete_range("0 to 1"),
            body: GenerateBody {
                alternative_label: None,
                decl: None,
                statements: vec![],
            },
        };
        assert_eq!(stmt.label, Some(util.ident("gen")));
        assert_eq!(stmt.statement, ConcurrentStatement::ForGenerate(gen));
    }

    #[test]
    fn test_for_generate() {
        let (util, stmt) = with_stream_no_messages(
            parse_labeled_concurrent_statement,
            "\
gen: for idx in 0 to 1 generate
  foo <= bar;
end generate;
",
        );
        let gen = ForGenerateStatement {
            index_name: util.ident("idx"),
            discrete_range: util.discrete_range("0 to 1"),
            body: GenerateBody {
                alternative_label: None,
                decl: None,
                statements: vec![util.concurrent_statement("foo <= bar;")],
            },
        };
        assert_eq!(stmt.label, Some(util.ident("gen")));
        assert_eq!(stmt.statement, ConcurrentStatement::ForGenerate(gen));
    }

    #[test]
    fn test_for_generate_empty_declarations() {
        let (util, stmt) = with_stream_no_messages(
            parse_labeled_concurrent_statement,
            "\
gen: for idx in 0 to 1 generate
begin
  foo <= bar;
end generate;
",
        );
        let gen = ForGenerateStatement {
            index_name: util.ident("idx"),
            discrete_range: util.discrete_range("0 to 1"),
            body: GenerateBody {
                alternative_label: None,
                decl: Some(vec![]),
                statements: vec![util.concurrent_statement("foo <= bar;")],
            },
        };
        assert_eq!(stmt.label, Some(util.ident("gen")));
        assert_eq!(stmt.statement, ConcurrentStatement::ForGenerate(gen));
    }

    #[test]
    fn test_for_generate_declarations() {
        let (util, stmt) = with_stream_no_messages(
            parse_labeled_concurrent_statement,
            "\
gen: for idx in 0 to 1 generate
  signal foo : natural;
begin
  foo <= bar;
end generate;
",
        );
        let gen = ForGenerateStatement {
            index_name: util.ident("idx"),
            discrete_range: util.discrete_range("0 to 1"),
            body: GenerateBody {
                alternative_label: None,
                decl: Some(util.declarative_part("signal foo : natural;")),
                statements: vec![util.concurrent_statement("foo <= bar;")],
            },
        };
        assert_eq!(stmt.label, Some(util.ident("gen")));
        assert_eq!(stmt.statement, ConcurrentStatement::ForGenerate(gen));
    }

    #[test]
    fn test_if_generate_empty() {
        let (util, stmt) = with_stream_no_messages(
            parse_labeled_concurrent_statement,
            "\
gen: if cond = true generate
end generate;
",
        );
        let gen = IfGenerateStatement {
            conditionals: vec![Conditional {
                condition: util.expr("cond = true"),
                item: GenerateBody {
                    alternative_label: None,
                    decl: None,
                    statements: vec![],
                },
            }],
            else_item: None,
        };
        assert_eq!(stmt.label, Some(util.ident("gen")));
        assert_eq!(stmt.statement, ConcurrentStatement::IfGenerate(gen));
    }

    #[test]
    fn test_if_generate_declarative_region() {
        let (util, stmt) = with_stream_no_messages(
            parse_labeled_concurrent_statement,
            "\
gen: if cond = true generate
begin
end generate;
",
        );
        let gen = IfGenerateStatement {
            conditionals: vec![Conditional {
                condition: util.expr("cond = true"),
                item: GenerateBody {
                    alternative_label: None,
                    decl: Some(vec![]),
                    statements: vec![],
                },
            }],
            else_item: None,
        };
        assert_eq!(stmt.label, Some(util.ident("gen")));
        assert_eq!(stmt.statement, ConcurrentStatement::IfGenerate(gen));
    }

    #[test]
    fn test_if_elseif_else_generate_empty() {
        let (util, stmt) = with_stream_no_messages(
            parse_labeled_concurrent_statement,
            "\
gen: if cond = true generate
elsif cond2 = true generate
else generate
end generate;
",
        );
        let gen = IfGenerateStatement {
            conditionals: vec![
                Conditional {
                    condition: util.expr("cond = true"),
                    item: GenerateBody {
                        alternative_label: None,
                        decl: None,
                        statements: vec![],
                    },
                },
                Conditional {
                    condition: util.expr("cond2 = true"),
                    item: GenerateBody {
                        alternative_label: None,
                        decl: None,
                        statements: vec![],
                    },
                },
            ],
            else_item: Some(GenerateBody {
                alternative_label: None,
                decl: None,
                statements: vec![],
            }),
        };
        assert_eq!(stmt.label, Some(util.ident("gen")));
        assert_eq!(stmt.statement, ConcurrentStatement::IfGenerate(gen));
    }
    #[test]
    fn test_if_elseif_else_generate() {
        let (util, stmt) = with_stream_no_messages(
            parse_labeled_concurrent_statement,
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
end generate;
",
        );
        let gen = IfGenerateStatement {
            conditionals: vec![
                Conditional {
                    condition: util.expr("cond = true"),
                    item: GenerateBody {
                        alternative_label: None,
                        decl: Some(util.declarative_part("variable v1 : boolean;")),
                        statements: vec![util.concurrent_statement("foo1(clk);")],
                    },
                },
                Conditional {
                    condition: util.expr("cond2 = true"),
                    item: GenerateBody {
                        alternative_label: None,
                        decl: Some(util.declarative_part("variable v2 : boolean;")),
                        statements: vec![util.concurrent_statement("foo2(clk);")],
                    },
                },
            ],
            else_item: Some(GenerateBody {
                alternative_label: None,
                decl: Some(util.declarative_part("variable v3 : boolean;")),
                statements: vec![util.concurrent_statement("foo3(clk);")],
            }),
        };
        assert_eq!(stmt.label, Some(util.ident("gen")));
        assert_eq!(stmt.statement, ConcurrentStatement::IfGenerate(gen));
    }

    #[test]
    fn test_if_elseif_else_generate_alternative_label() {
        let (util, stmt) = with_stream_no_messages(
            parse_labeled_concurrent_statement,
            "\
gen: if alt1: cond = true generate
elsif alt2: cond2 = true generate
else alt3: generate
end generate;
",
        );
        let gen = IfGenerateStatement {
            conditionals: vec![
                Conditional {
                    condition: util.expr("cond = true"),
                    item: GenerateBody {
                        alternative_label: Some(util.ident("alt1")),
                        decl: None,
                        statements: vec![],
                    },
                },
                Conditional {
                    condition: util.expr("cond2 = true"),
                    item: GenerateBody {
                        alternative_label: Some(util.ident("alt2")),
                        decl: None,
                        statements: vec![],
                    },
                },
            ],
            else_item: Some(GenerateBody {
                alternative_label: Some(util.ident("alt3")),
                decl: None,
                statements: vec![],
            }),
        };
        assert_eq!(stmt.label, Some(util.ident("gen")));
        assert_eq!(stmt.statement, ConcurrentStatement::IfGenerate(gen));
    }
    #[test]
    fn test_if_elseif_else_generate_inner_end() {
        let (util, stmt) = with_stream_no_messages(
            parse_labeled_concurrent_statement,
            "\
gen: if alt1: cond = true generate
end alt1;
elsif alt2: cond2 = true generate
end alt2;
else alt3: generate
end alt3;
end generate;
",
        );
        let gen = IfGenerateStatement {
            conditionals: vec![
                Conditional {
                    condition: util.expr("cond = true"),
                    item: GenerateBody {
                        alternative_label: Some(util.ident("alt1")),
                        decl: None,
                        statements: vec![],
                    },
                },
                Conditional {
                    condition: util.expr("cond2 = true"),
                    item: GenerateBody {
                        alternative_label: Some(util.ident("alt2")),
                        decl: None,
                        statements: vec![],
                    },
                },
            ],
            else_item: Some(GenerateBody {
                alternative_label: Some(util.ident("alt3")),
                decl: None,
                statements: vec![],
            }),
        };
        assert_eq!(stmt.label, Some(util.ident("gen")));
        assert_eq!(stmt.statement, ConcurrentStatement::IfGenerate(gen));
    }

    #[test]
    fn test_case_generate() {
        let (util, stmt) = with_stream_no_messages(
            parse_labeled_concurrent_statement,
            "\
gen: case expr(0) + 2 generate
  when 1 | 2 =>
    sig <= value;
  when others =>
    foo(clk);
end generate;
",
        );
        let gen = CaseGenerateStatement {
            expression: util.expr("expr(0) + 2"),
            alternatives: vec![
                Alternative {
                    choices: util.choices("1 | 2"),
                    item: GenerateBody {
                        alternative_label: None,
                        decl: None,
                        statements: vec![util.concurrent_statement("sig <= value;")],
                    },
                },
                Alternative {
                    choices: util.choices("others"),
                    item: GenerateBody {
                        alternative_label: None,
                        decl: None,
                        statements: vec![util.concurrent_statement("foo(clk);")],
                    },
                },
            ],
        };
        assert_eq!(stmt.label, Some(util.ident("gen")));
        assert_eq!(stmt.statement, ConcurrentStatement::CaseGenerate(gen));
    }

    #[test]
    fn test_case_alternative_label() {
        let (util, stmt) = with_stream_no_messages(
            parse_labeled_concurrent_statement,
            "\
gen: case expr(0) + 2 generate
  when alt1: 1 | 2 =>
    sig <= value;
  when alt2: others =>
    foo(clk);
end generate;
",
        );
        let gen = CaseGenerateStatement {
            expression: util.expr("expr(0) + 2"),
            alternatives: vec![
                Alternative {
                    choices: util.choices("1 | 2"),
                    item: GenerateBody {
                        alternative_label: Some(util.ident("alt1")),
                        decl: None,
                        statements: vec![util.concurrent_statement("sig <= value;")],
                    },
                },
                Alternative {
                    choices: util.choices("others"),
                    item: GenerateBody {
                        alternative_label: Some(util.ident("alt2")),
                        decl: None,
                        statements: vec![util.concurrent_statement("foo(clk);")],
                    },
                },
            ],
        };
        assert_eq!(stmt.label, Some(util.ident("gen")));
        assert_eq!(stmt.statement, ConcurrentStatement::CaseGenerate(gen));
    }
}
