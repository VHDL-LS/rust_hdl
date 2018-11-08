// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use ast::{
    Alternative, AssertStatement, AssignmentRightHand, CaseStatement, Conditional, Conditionals,
    ExitStatement, Expression, FunctionCall, IfStatement, IterationScheme,
    LabeledSequentialStatement, LoopStatement, Name, NextStatement, ReportStatement,
    ReturnStatement, Selection, SequentialStatement, SignalAssignment, Target, VariableAssignment,
    WaitStatement, Waveform,
};
use common::parse_optional;
use expression::{parse_aggregate_leftpar_known, parse_choices, parse_expression};
use message::{error, MessageHandler, ParseResult};
use names::{parse_name, parse_name_initial_token, to_simple_name};
use range::parse_discrete_range;
use source::WithPos;
use tokenizer::{Kind::*, Token};
use tokenstream::TokenStream;
use waveform::{parse_delay_mechanism, parse_waveform};

/// LRM 10.2 Wait statement
fn parse_wait_statement_known_keyword(stream: &mut TokenStream) -> ParseResult<WaitStatement> {
    let mut sensitivity_clause = vec![];
    if stream.skip_if_kind(On)? {
        loop {
            sensitivity_clause.push(parse_name(stream)?);
            if !stream.skip_if_kind(Comma)? {
                break;
            }
        }
    }

    let condition_clause = parse_optional(stream, Until, parse_expression)?;
    let timeout_clause = parse_optional(stream, For, parse_expression)?;

    stream.expect_kind(SemiColon)?;
    Ok(WaitStatement {
        sensitivity_clause,
        condition_clause,
        timeout_clause,
    })
}

/// LRM 10.3 Assertion statement
pub fn parse_assert_statement_known_keyword(
    stream: &mut TokenStream,
) -> ParseResult<AssertStatement> {
    let condition = parse_expression(stream)?;
    let report = parse_optional(stream, Report, parse_expression)?;
    let severity = parse_optional(stream, Severity, parse_expression)?;

    stream.expect_kind(SemiColon)?;
    Ok(AssertStatement {
        condition,
        report,
        severity,
    })
}

/// LRM 10.4 Report statement
fn parse_report_statement_known_keyword(stream: &mut TokenStream) -> ParseResult<ReportStatement> {
    let report = parse_expression(stream)?;
    let severity = parse_optional(stream, Severity, parse_expression)?;

    stream.expect_kind(SemiColon)?;
    Ok(ReportStatement { report, severity })
}

pub fn parse_labeled_sequential_statements(
    stream: &mut TokenStream,
    messages: &mut MessageHandler,
) -> ParseResult<(Vec<LabeledSequentialStatement>, Token)> {
    let mut statements = Vec::new();
    loop {
        let token = stream.expect()?;
        match token.kind {
            End | Else | Elsif | When => {
                break Ok((statements, token));
            }
            _ => {
                statements.push(parse_sequential_statement_initial_token(
                    stream, token, messages,
                )?);
            }
        }
    }
}

/// LRM 10.8 If statement
fn parse_if_statement_known_keyword(
    stream: &mut TokenStream,
    messages: &mut MessageHandler,
) -> ParseResult<IfStatement> {
    let mut conditionals = Vec::new();
    let mut else_branch = None;

    loop {
        let condition = parse_expression(stream)?;
        stream.expect_kind(Then)?;
        let (statements, end_token) = parse_labeled_sequential_statements(stream, messages)?;

        let conditional = Conditional {
            condition,
            item: statements,
        };

        try_token_kind!(
            end_token,
            Elsif => {
                conditionals.push(conditional);
                continue;
            },

            Else => {
                conditionals.push(conditional);
                let (statements, end_token) = parse_labeled_sequential_statements(stream, messages)?;

                try_token_kind!(
                    end_token,
                    End => {
                        stream.expect_kind(If)?;
                        // @TODO check end label
                        stream.pop_if_kind(Identifier)?;
                        else_branch = Some(statements);
                        break;
                    }
                );
            },
            End => {
                stream.expect_kind(If)?;
                conditionals.push(conditional);
                break;
            }
        );
    }

    stream.expect_kind(SemiColon)?;
    Ok(IfStatement {
        conditionals,
        else_item: else_branch,
    })
}

/// LRM 10.9 Case statement
fn parse_case_statement_known_keyword(
    stream: &mut TokenStream,
    messages: &mut MessageHandler,
) -> ParseResult<CaseStatement> {
    let expression = parse_expression(stream)?;
    stream.expect_kind(Is)?;
    stream.expect_kind(When)?;
    let mut alternatives = Vec::new();

    loop {
        let choices = parse_choices(stream)?;
        stream.expect_kind(RightArrow)?;
        let (statements, end_token) = parse_labeled_sequential_statements(stream, messages)?;
        let alternative = Alternative {
            choices,
            item: statements,
        };
        try_token_kind!(
            end_token,
            When => {
                alternatives.push(alternative);
                continue;
            },
            End => {
                stream.expect_kind(Case)?;
                // @TODO check end label
                stream.pop_if_kind(Identifier)?;
                alternatives.push(alternative);
                break;
            }
        );
    }

    stream.expect_kind(SemiColon)?;
    Ok(CaseStatement {
        expression,
        alternatives,
    })
}

/// LRM 10.10 Loop statement
fn parse_loop_statement_initial_token(
    stream: &mut TokenStream,
    token: Token,
    messages: &mut MessageHandler,
) -> ParseResult<LoopStatement> {
    let iteration_scheme = {
        try_token_kind!(
            token,
            Loop => None,
            While => {
                let expression = parse_expression(stream)?;
                stream.expect_kind(Loop)?;
                Some(IterationScheme::While(expression))
            },
            For => {
                let ident = stream.expect_ident()?;
                stream.expect_kind(In)?;
                let discrete_range = parse_discrete_range(stream)?;
                stream.expect_kind(Loop)?;
                Some(IterationScheme::For(ident, discrete_range))
            }
        )
    };

    let (statements, end_token) = parse_labeled_sequential_statements(stream, messages)?;
    try_token_kind!(
        end_token,
        End => {
            stream.expect_kind(Loop)?;
            // @TODO check end label
            stream.pop_if_kind(Identifier)?;
        }
    );

    stream.expect_kind(SemiColon)?;
    Ok(LoopStatement {
        iteration_scheme,
        statements,
    })
}

/// LRM 10.11 Next statement
fn parse_next_statement_known_keyword(stream: &mut TokenStream) -> ParseResult<NextStatement> {
    let loop_label = {
        if stream.peek_kind()? == Some(Identifier) {
            Some(stream.expect_ident()?)
        } else {
            None
        }
    };
    let condition = parse_optional(stream, When, parse_expression)?;
    stream.expect_kind(SemiColon)?;
    Ok(NextStatement {
        loop_label,
        condition,
    })
}

/// LRM 10.12 Exit statement
fn parse_exit_statement_known_keyword(stream: &mut TokenStream) -> ParseResult<ExitStatement> {
    let loop_label = {
        if stream.peek_kind()? == Some(Identifier) {
            Some(stream.expect_ident()?)
        } else {
            None
        }
    };
    let condition = parse_optional(stream, When, parse_expression)?;
    stream.expect_kind(SemiColon)?;
    Ok(ExitStatement {
        loop_label,
        condition,
    })
}

/// LRM 10.13 Return statement
fn parse_return_statement_known_keyword(stream: &mut TokenStream) -> ParseResult<ReturnStatement> {
    let expression = {
        if stream.peek_kind()? == Some(SemiColon) {
            None
        } else {
            Some(parse_expression(stream)?)
        }
    };
    stream.expect_kind(SemiColon)?;
    Ok(ReturnStatement { expression })
}

/// LRM 10.5 Signal assignment statement
pub fn parse_signal_assignment_right_hand(
    stream: &mut TokenStream,
) -> ParseResult<AssignmentRightHand<Waveform>> {
    parse_assignment_right_hand(stream, parse_waveform)
}

/// LRM 10.6 Variable assignment statement
fn parse_variable_assignment_right_hand(
    stream: &mut TokenStream,
) -> ParseResult<AssignmentRightHand<WithPos<Expression>>> {
    parse_assignment_right_hand(stream, parse_expression)
}

fn parse_assignment_right_hand<T, F>(
    stream: &mut TokenStream,
    parse_item: F,
) -> ParseResult<AssignmentRightHand<T>>
where
    F: Fn(&mut TokenStream) -> ParseResult<T>,
{
    let item = parse_item(stream)?;

    let token = stream.expect()?;
    match_token_kind!(
        token,
        When => {
            Ok(AssignmentRightHand::Conditional(parse_conditonals(stream, item, parse_item)?))

        },
        SemiColon => {
            Ok(AssignmentRightHand::Simple(item))
        }
    )
}

fn parse_conditonals<T, F>(
    stream: &mut TokenStream,
    initial_item: T,
    parse_item: F,
) -> ParseResult<Conditionals<T>>
where
    F: Fn(&mut TokenStream) -> ParseResult<T>,
{
    let condition = parse_expression(stream)?;
    let cond_expr = Conditional {
        condition,
        item: initial_item,
    };

    let mut conditionals = vec![cond_expr];
    let mut else_item = None;

    loop {
        let token = stream.expect()?;
        try_token_kind!(
            token,
            SemiColon => {
                break;
            },
            Else => {
                let item = parse_item(stream)?;
                let token = stream.expect()?;
                try_token_kind!(
                    token,
                    SemiColon =>  {
                        else_item = Some(item);
                        break;
                    },
                    When => {
                        let condition = parse_expression(stream)?;
                        let conditional = Conditional {
                            condition,
                            item
                        };
                        conditionals.push(conditional);
                    }
                );
            }
        )
    }

    Ok(Conditionals {
        conditionals,
        else_item,
    })
}

pub fn parse_selection<T, F>(
    stream: &mut TokenStream,
    expression: WithPos<Expression>,
    parse_item: F,
) -> ParseResult<Selection<T>>
where
    F: Fn(&mut TokenStream) -> ParseResult<T>,
{
    let mut alternatives = Vec::with_capacity(2);

    loop {
        let item = parse_item(stream)?;
        stream.expect_kind(When)?;
        let choices = parse_choices(stream)?;
        alternatives.push(Alternative { choices, item });

        let token = stream.expect()?;
        try_token_kind!(
            token,
            Comma => {},
            SemiColon => break
        )
    }

    Ok(Selection {
        expression,
        alternatives,
    })
}

fn parse_assignment_or_procedure_call(
    stream: &mut TokenStream,
    token: Token,
    target: WithPos<Target>,
) -> ParseResult<SequentialStatement> {
    Ok(try_token_kind!(
        token,
        ColonEq => {
            SequentialStatement::VariableAssignment(VariableAssignment {
                target,
                rhs: parse_variable_assignment_right_hand(stream)?
            })
        },
        LTE => {
            let delay_mechanism = parse_delay_mechanism(stream)?;
            SequentialStatement::SignalAssignment(SignalAssignment {
                target,
                delay_mechanism,
                rhs: parse_signal_assignment_right_hand(stream)?
            })
        },
        SemiColon => {
            match target.item {
                Target::Name(Name::FunctionCall(call)) => {
                    SequentialStatement::ProcedureCall(*call)
                }
                Target::Name(name) => {
                    SequentialStatement::ProcedureCall(
                        FunctionCall {
                            name: WithPos::new(name, target.pos),
                            parameters: vec![]
                        })
                }
                Target::Aggregate(..) => {
                    return Err(error(target, "Expected procedure call, got aggregate"));
                }
            }
        }
    ))
}

fn parse_target_initial_token(
    stream: &mut TokenStream,
    token: Token,
) -> ParseResult<WithPos<Target>> {
    if token.kind == Identifier {
        Ok(parse_name_initial_token(stream, token)?.map_into(Target::Name))
    } else {
        Ok(parse_aggregate_leftpar_known(stream)?.map_into(Target::Aggregate))
    }
}

pub fn parse_target(stream: &mut TokenStream) -> ParseResult<WithPos<Target>> {
    let token = stream.expect()?;
    parse_target_initial_token(stream, token)
}

fn parse_selected_assignment(stream: &mut TokenStream) -> ParseResult<SequentialStatement> {
    let expression = parse_expression(stream)?;
    stream.expect_kind(Select)?;
    let target = parse_target(stream)?;
    let token = stream.expect()?;
    match_token_kind!(
        token,
        ColonEq => {
            let rhs = AssignmentRightHand::Selected(parse_selection(stream, expression, parse_expression)?);
            Ok(SequentialStatement::VariableAssignment(VariableAssignment {
                target,
                rhs
            }))
        },
        LTE => {
            let delay_mechanism = parse_delay_mechanism(stream)?;
            let rhs = AssignmentRightHand::Selected(parse_selection(stream, expression, parse_waveform)?);
            Ok(SequentialStatement::SignalAssignment(SignalAssignment {
                target,
                delay_mechanism,
                rhs
            }))
        }
    )
}

fn parse_unlabeled_sequential_statement(
    stream: &mut TokenStream,
    token: Token,
    messages: &mut MessageHandler,
) -> ParseResult<SequentialStatement> {
    let statement = {
        try_token_kind!(
            token,
            Wait => SequentialStatement::Wait(parse_wait_statement_known_keyword(stream)?),
            Assert => SequentialStatement::Assert(parse_assert_statement_known_keyword(stream)?),
            Report => SequentialStatement::Report(parse_report_statement_known_keyword(stream)?),
            If => SequentialStatement::If(parse_if_statement_known_keyword(stream, messages)?),
            Case => SequentialStatement::Case(parse_case_statement_known_keyword(stream, messages)?),
            For | Loop | While => {
                SequentialStatement::Loop(parse_loop_statement_initial_token(stream, token, messages)?)
            },
            Next => SequentialStatement::Next(parse_next_statement_known_keyword(stream)?),
            Exit => SequentialStatement::Exit(parse_exit_statement_known_keyword(stream)?),
            Return => SequentialStatement::Return(parse_return_statement_known_keyword(stream)?),
            Null => {
                stream.expect_kind(SemiColon)?;
                SequentialStatement::Null
            },
            With => {
                parse_selected_assignment(stream)?
            },
            Identifier|LeftPar => {
                let target = parse_target_initial_token(stream, token)?;
                let token = stream.expect()?;
                parse_assignment_or_procedure_call(stream, token, target)?
            }
        )
    };
    Ok(statement)
}

#[cfg(test)]
pub fn parse_sequential_statement(
    stream: &mut TokenStream,
    messages: &mut MessageHandler,
) -> ParseResult<LabeledSequentialStatement> {
    let token = stream.expect()?;
    parse_sequential_statement_initial_token(stream, token, messages)
}

pub fn parse_sequential_statement_initial_token(
    stream: &mut TokenStream,
    token: Token,
    messages: &mut MessageHandler,
) -> ParseResult<LabeledSequentialStatement> {
    if token.kind == Identifier {
        let name = parse_name_initial_token(stream, token)?;
        let token = stream.expect()?;
        if token.kind == Colon {
            let label = Some(to_simple_name(name)?);
            let token = stream.expect()?;
            let statement = parse_unlabeled_sequential_statement(stream, token, messages)?;
            Ok(LabeledSequentialStatement { label, statement })
        } else {
            let target = name.map_into(Target::Name);
            let statement = parse_assignment_or_procedure_call(stream, token, target)?;
            Ok(LabeledSequentialStatement {
                label: None,
                statement,
            })
        }
    } else {
        let statement = parse_unlabeled_sequential_statement(stream, token, messages)?;
        Ok(LabeledSequentialStatement {
            label: None,
            statement,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::{DelayMechanism, Ident};

    use test_util::{with_stream_no_messages, TestUtil};

    fn parse(code: &str) -> (TestUtil, LabeledSequentialStatement) {
        with_stream_no_messages(parse_sequential_statement, code)
    }

    fn with_label(
        label: Option<Ident>,
        statement: SequentialStatement,
    ) -> LabeledSequentialStatement {
        LabeledSequentialStatement { label, statement }
    }

    #[test]
    fn parse_simple_wait_statement() {
        let (_, statement) = parse("wait;");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::Wait(WaitStatement {
                    sensitivity_clause: vec![],
                    condition_clause: None,
                    timeout_clause: None
                })
            )
        );
    }

    #[test]
    fn parse_wait_statement_with_label() {
        let (util, statement) = parse("foo: wait;");
        assert_eq!(
            statement,
            with_label(
                Some(util.ident("foo")),
                SequentialStatement::Wait(WaitStatement {
                    sensitivity_clause: vec![],
                    condition_clause: None,
                    timeout_clause: None
                })
            )
        );
    }

    #[test]
    fn parse_wait_statement_with_sensitivity_list() {
        let (util, statement) = parse("wait on foo, bar;");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::Wait(WaitStatement {
                    sensitivity_clause: vec![util.name("foo"), util.name("bar")],
                    condition_clause: None,
                    timeout_clause: None
                })
            )
        );
    }

    #[test]
    fn parse_wait_statement_with_condition() {
        let (util, statement) = parse("wait until a = b;");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::Wait(WaitStatement {
                    sensitivity_clause: vec![],
                    condition_clause: Some(util.expr("a = b")),
                    timeout_clause: None
                })
            )
        );
    }

    #[test]
    fn parse_wait_statement_with_timeout() {
        let (util, statement) = parse("wait for 2 ns;");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::Wait(WaitStatement {
                    sensitivity_clause: vec![],
                    condition_clause: None,
                    timeout_clause: Some(util.expr("2 ns"))
                })
            )
        );
    }

    #[test]
    fn parse_wait_statement_with_all_parts() {
        let (util, statement) = parse("wait on foo until bar for 2 ns;");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::Wait(WaitStatement {
                    sensitivity_clause: vec![util.name("foo")],
                    condition_clause: Some(util.expr("bar")),
                    timeout_clause: Some(util.expr("2 ns"))
                })
            )
        );
    }

    #[test]
    fn parse_simple_assert_statement() {
        let (util, statement) = parse("assert false;");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::Assert(AssertStatement {
                    condition: util.expr("false"),
                    report: None,
                    severity: None
                })
            )
        );
    }

    #[test]
    fn parse_assert_statement() {
        let (util, statement) = parse("assert false report \"message\" severity error;");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::Assert(AssertStatement {
                    condition: util.expr("false"),
                    report: Some(util.expr("\"message\"")),
                    severity: Some(util.expr("error"))
                })
            )
        );
    }

    #[test]
    fn parse_report_statement() {
        let (util, statement) = parse("report \"message\" severity error;");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::Report(ReportStatement {
                    report: util.expr("\"message\""),
                    severity: Some(util.expr("error"))
                })
            )
        );
    }

    #[test]
    fn parse_simple_signal_assignment() {
        let (util, statement) = parse("foo(0) <= bar(1,2) after 2 ns;");

        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::SignalAssignment(SignalAssignment {
                    target: util.name("foo(0)").map_into(Target::Name),
                    delay_mechanism: None,
                    rhs: AssignmentRightHand::Simple(util.waveform("bar(1,2) after 2 ns"))
                })
            )
        );
    }

    #[test]
    fn parse_simple_signal_assignment_delay_mechanism() {
        let (util, statement) = parse("foo(0) <= transport bar(1,2);");

        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::SignalAssignment(SignalAssignment {
                    target: util.name("foo(0)").map_into(Target::Name),
                    delay_mechanism: Some(DelayMechanism::Transport),
                    rhs: AssignmentRightHand::Simple(util.waveform("bar(1,2)"))
                })
            )
        );
    }

    #[test]
    fn parse_simple_variable_assignment() {
        let (util, statement) = parse("foo(0) := bar(1,2);");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::VariableAssignment(VariableAssignment {
                    target: util.name("foo(0)").map_into(Target::Name),
                    rhs: AssignmentRightHand::Simple(util.expr("bar(1,2)"))
                })
            )
        );
    }

    #[test]
    fn parse_simple_aggregate_variable_assignment() {
        let (util, statement) = parse("(foo, 1 => bar) := integer_vector'(1, 2);");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::VariableAssignment(VariableAssignment {
                    target: util
                        .aggregate("(foo, 1 => bar)")
                        .map_into(Target::Aggregate),
                    rhs: AssignmentRightHand::Simple(util.expr("integer_vector'(1, 2)"))
                })
            )
        );
    }

    #[test]
    fn parse_labeled_aggregate_variable_assignment() {
        let (util, statement) = parse("name: (foo, 1 => bar) := integer_vector'(1, 2);");
        assert_eq!(
            statement,
            with_label(
                Some(util.ident("name")),
                SequentialStatement::VariableAssignment(VariableAssignment {
                    target: util
                        .aggregate("(foo, 1 => bar)")
                        .map_into(Target::Aggregate),
                    rhs: AssignmentRightHand::Simple(util.expr("integer_vector'(1, 2)"))
                })
            )
        );
    }

    #[test]
    fn parse_labeled_simple_variable_assignment() {
        let (util, statement) = parse("name: foo(0) := bar(1,2);");
        assert_eq!(
            statement,
            with_label(
                Some(util.ident("name")),
                SequentialStatement::VariableAssignment(VariableAssignment {
                    target: util.name("foo(0)").map_into(Target::Name),
                    rhs: AssignmentRightHand::Simple(util.expr("bar(1,2)"))
                })
            )
        );
    }

    #[test]
    fn parse_conditional_variable_assignment() {
        let (util, statement) = parse("foo(0) := bar(1,2) when cond = true;");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::VariableAssignment(VariableAssignment {
                    target: util.name("foo(0)").map_into(Target::Name),
                    rhs: AssignmentRightHand::Conditional(Conditionals {
                        conditionals: vec![Conditional {
                            condition: util.expr("cond = true"),
                            item: util.expr("bar(1,2)")
                        }],
                        else_item: None
                    })
                })
            )
        );
    }

    #[test]
    fn parse_selected_variable_assignment() {
        let (util, statement) = parse(
            "\
with x(0) + 1 select
   foo(0) := bar(1,2) when 0|1,
             def when others;",
        );

        let selection = Selection {
            expression: util.expr("x(0) + 1"),
            alternatives: vec![
                Alternative {
                    choices: util.choices("0|1"),
                    item: util.expr("bar(1,2)"),
                },
                Alternative {
                    choices: util.choices("others"),
                    item: util.expr("def"),
                },
            ],
        };

        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::VariableAssignment(VariableAssignment {
                    target: util.name("foo(0)").map_into(Target::Name),
                    rhs: AssignmentRightHand::Selected(selection)
                })
            )
        );
    }

    #[test]
    fn parse_conditional_variable_assignment_several() {
        let (util, statement) = parse("foo(0) := bar(1,2) when cond = true else expr2 when cond2;");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::VariableAssignment(VariableAssignment {
                    target: util.name("foo(0)").map_into(Target::Name),
                    rhs: AssignmentRightHand::Conditional(Conditionals {
                        conditionals: vec![
                            Conditional {
                                condition: util.expr("cond = true"),
                                item: util.expr("bar(1,2)")
                            },
                            Conditional {
                                condition: util.expr("cond2"),
                                item: util.expr("expr2")
                            }
                        ],
                        else_item: None
                    })
                })
            )
        );
    }
    #[test]
    fn parse_conditional_variable_assignment_else() {
        let (util, statement) = parse("foo(0) := bar(1,2) when cond = true else expr2;");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::VariableAssignment(VariableAssignment {
                    target: util.name("foo(0)").map_into(Target::Name),
                    rhs: AssignmentRightHand::Conditional(Conditionals {
                        conditionals: vec![Conditional {
                            condition: util.expr("cond = true"),
                            item: util.expr("bar(1,2)")
                        }],
                        else_item: Some(util.expr("expr2"))
                    })
                })
            )
        );
    }

    #[test]
    fn parse_conditional_signal_assignment() {
        let (util, statement) = parse("foo(0) <= bar(1,2) after 2 ns when cond;");

        let conditionals = Conditionals {
            conditionals: vec![Conditional {
                condition: util.expr("cond"),
                item: util.waveform("bar(1,2) after 2 ns"),
            }],
            else_item: None,
        };

        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::SignalAssignment(SignalAssignment {
                    target: util.name("foo(0)").map_into(Target::Name),
                    delay_mechanism: None,
                    rhs: AssignmentRightHand::Conditional(conditionals)
                })
            )
        );
    }

    #[test]
    fn parse_selected_signal_assignment() {
        let (util, statement) = parse(
            "\
with x(0) + 1 select
   foo(0) <= transport bar(1,2) after 2 ns when 0|1,
                       def when others;",
        );

        let selection = Selection {
            expression: util.expr("x(0) + 1"),
            alternatives: vec![
                Alternative {
                    choices: util.choices("0|1"),
                    item: util.waveform("bar(1,2) after 2 ns"),
                },
                Alternative {
                    choices: util.choices("others"),
                    item: util.waveform("def"),
                },
            ],
        };

        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::SignalAssignment(SignalAssignment {
                    target: util.name("foo(0)").map_into(Target::Name),
                    delay_mechanism: Some(DelayMechanism::Transport),
                    rhs: AssignmentRightHand::Selected(selection)
                })
            )
        );
    }

    #[test]
    fn parse_procedure_call_statement() {
        let (util, statement) = parse("foo(1,2);");

        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::ProcedureCall(util.function_call("foo(1,2)"))
            )
        );
    }

    #[test]
    fn parse_procedure_call_no_args() {
        let (util, statement) = parse("foo;");

        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::ProcedureCall(util.function_call("foo"))
            )
        );
    }

    #[test]
    fn parse_simple_if_statement() {
        let (util, statement) = parse(
            "\
if cond = true then
   foo(1,2);
   x := 1;
end if;
",
        );
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::If(IfStatement {
                    conditionals: vec![Conditional {
                        condition: util.expr("cond = true"),
                        item: vec![
                            util.sequential_statement("foo(1,2);"),
                            util.sequential_statement("x := 1;")
                        ]
                    }],
                    else_item: None
                })
            )
        );
    }

    #[test]
    fn parse_if_else_statement() {
        let (util, statement) = parse(
            "\
if cond = true then
   foo(1,2);
else
   x := 1;
end if;
",
        );
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::If(IfStatement {
                    conditionals: vec![Conditional {
                        condition: util.expr("cond = true"),
                        item: vec![util.sequential_statement("foo(1,2);")]
                    }],
                    else_item: Some(vec![util.sequential_statement("x := 1;")])
                })
            )
        );
    }

    #[test]
    fn parse_if_elsif_else_statement() {
        let (util, statement) = parse(
            "\
if cond = true then
   foo(1,2);
elsif cond2 = false then
   y := 2;
else
   x := 1;
end if;
",
        );
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::If(IfStatement {
                    conditionals: vec![
                        Conditional {
                            condition: util.expr("cond = true"),
                            item: vec![util.sequential_statement("foo(1,2);")]
                        },
                        Conditional {
                            condition: util.expr("cond2 = false"),
                            item: vec![util.sequential_statement("y := 2;")]
                        }
                    ],
                    else_item: Some(vec![util.sequential_statement("x := 1;")])
                })
            )
        );
    }

    #[test]
    fn parse_case_statement() {
        let (util, statement) = parse(
            "\
case foo(1) is
  when 1 | 2 =>
    stmt1;
    stmt2;
  when others =>
    stmt3;
    stmt4;
end case;
",
        );
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::Case(CaseStatement {
                    expression: util.expr("foo(1)"),
                    alternatives: vec![
                        Alternative {
                            choices: util.choices("1 | 2"),
                            item: vec![
                                util.sequential_statement("stmt1;"),
                                util.sequential_statement("stmt2;")
                            ]
                        },
                        Alternative {
                            choices: util.choices("others"),
                            item: vec![
                                util.sequential_statement("stmt3;"),
                                util.sequential_statement("stmt4;"),
                            ]
                        }
                    ],
                })
            )
        );
    }

    #[test]
    fn parse_loop_statement() {
        let (util, statement) = parse(
            "\
loop
  stmt1;
  stmt2;
end loop;
",
        );
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::Loop(LoopStatement {
                    iteration_scheme: None,
                    statements: vec![
                        util.sequential_statement("stmt1;"),
                        util.sequential_statement("stmt2;")
                    ],
                })
            )
        );
    }

    #[test]
    fn parse_while_loop_statement() {
        let (util, statement) = parse(
            "\
while foo = true loop
  stmt1;
  stmt2;
end loop;
",
        );
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::Loop(LoopStatement {
                    iteration_scheme: Some(IterationScheme::While(util.expr("foo = true"))),
                    statements: vec![
                        util.sequential_statement("stmt1;"),
                        util.sequential_statement("stmt2;")
                    ],
                })
            )
        );
    }
    #[test]
    fn parse_for_loop_statement() {
        let (util, statement) = parse(
            "\
for idx in 0 to 3 loop
  stmt1;
  stmt2;
end loop;
",
        );
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::Loop(LoopStatement {
                    iteration_scheme: Some(IterationScheme::For(
                        util.ident("idx"),
                        util.discrete_range("0 to 3")
                    )),
                    statements: vec![
                        util.sequential_statement("stmt1;"),
                        util.sequential_statement("stmt2;")
                    ],
                })
            )
        );
    }

    #[test]
    fn parse_next_statement() {
        let (_, statement) = parse("next;");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::Next(NextStatement {
                    loop_label: None,
                    condition: None,
                })
            )
        );
    }

    #[test]
    fn parse_next_statement_loop_label() {
        let (util, statement) = parse("next foo;");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::Next(NextStatement {
                    loop_label: Some(util.ident("foo")),
                    condition: None,
                })
            )
        );
    }

    #[test]
    fn parse_next_statement_condition() {
        let (util, statement) = parse("next when condition;");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::Next(NextStatement {
                    loop_label: None,
                    condition: Some(util.expr("condition")),
                })
            )
        );
    }

    #[test]
    fn parse_next_statement_loop_label_condition() {
        let (util, statement) = parse("next foo when condition;");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::Next(NextStatement {
                    loop_label: Some(util.ident("foo")),
                    condition: Some(util.expr("condition")),
                })
            )
        );
    }

    #[test]
    fn parse_exit_statement() {
        let (_, statement) = parse("exit;");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::Exit(ExitStatement {
                    loop_label: None,
                    condition: None,
                })
            )
        );
    }

    #[test]
    fn parse_exit_statement_loop_label() {
        let (util, statement) = parse("exit foo;");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::Exit(ExitStatement {
                    loop_label: Some(util.ident("foo")),
                    condition: None,
                })
            )
        );
    }

    #[test]
    fn parse_exit_statement_condition() {
        let (util, statement) = parse("exit when condition;");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::Exit(ExitStatement {
                    loop_label: None,
                    condition: Some(util.expr("condition")),
                })
            )
        );
    }

    #[test]
    fn parse_exit_statement_loop_label_condition() {
        let (util, statement) = parse("exit foo when condition;");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::Exit(ExitStatement {
                    loop_label: Some(util.ident("foo")),
                    condition: Some(util.expr("condition")),
                })
            )
        );
    }

    #[test]
    fn parse_return_statement() {
        let (_, statement) = parse("return;");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::Return(ReturnStatement { expression: None })
            )
        );
    }

    #[test]
    fn parse_return_statement_expression() {
        let (util, statement) = parse("return 1 + 2;");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::Return(ReturnStatement {
                    expression: Some(util.expr("1 + 2")),
                })
            )
        );
    }

    #[test]
    fn parse_null_statement() {
        let (_, statement) = parse("null;");
        assert_eq!(statement, with_label(None, SequentialStatement::Null));
    }
}
