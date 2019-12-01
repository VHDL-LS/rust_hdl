// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use crate::ast::{
    to_simple_name, Alternative, AssertStatement, AssignmentRightHand, CaseStatement, Conditional,
    Conditionals, ExitStatement, Expression, ForceMode, FunctionCall, IfStatement, IterationScheme,
    LabeledSequentialStatement, LoopStatement, Name, NextStatement, ReportStatement,
    ReturnStatement, Selection, SequentialStatement, SignalAssignment, SignalForceAssignment,
    SignalReleaseAssignment, Target, VariableAssignment, WaitStatement, Waveform,
};
use crate::common::parse_optional;
use crate::diagnostic::{Diagnostic, DiagnosticHandler, ParseResult};
use crate::expression::{parse_aggregate_leftpar_known, parse_choices, parse_expression};
use crate::names::{parse_name, parse_name_initial_token};
use crate::range::parse_discrete_range;
use crate::source::WithPos;
use crate::tokenizer::{Kind::*, Token};
use crate::tokenstream::TokenStream;
use crate::waveform::{parse_delay_mechanism, parse_waveform};

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
    diagnostics: &mut dyn DiagnosticHandler,
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
                    stream,
                    token,
                    diagnostics,
                )?);
            }
        }
    }
}

/// LRM 10.8 If statement
fn parse_if_statement_known_keyword(
    stream: &mut TokenStream,
    diagnostics: &mut dyn DiagnosticHandler,
) -> ParseResult<IfStatement> {
    let mut conditionals = Vec::new();
    let mut else_branch = None;

    loop {
        let condition = parse_expression(stream)?;
        stream.expect_kind(Then)?;
        let (statements, end_token) = parse_labeled_sequential_statements(stream, diagnostics)?;

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
                let (statements, end_token) = parse_labeled_sequential_statements(stream, diagnostics)?;

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
    diagnostics: &mut dyn DiagnosticHandler,
) -> ParseResult<CaseStatement> {
    let expression = parse_expression(stream)?;
    stream.expect_kind(Is)?;
    stream.expect_kind(When)?;
    let mut alternatives = Vec::new();

    loop {
        let choices = parse_choices(stream)?;
        stream.expect_kind(RightArrow)?;
        let (statements, end_token) = parse_labeled_sequential_statements(stream, diagnostics)?;
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
    token: &Token,
    diagnostics: &mut dyn DiagnosticHandler,
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

    let (statements, end_token) = parse_labeled_sequential_statements(stream, diagnostics)?;
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

fn parse_optional_force_mode(stream: &mut TokenStream) -> ParseResult<Option<ForceMode>> {
    let token = stream.peek_expect()?;
    let optional_force_mode = match token.kind {
        In => {
            stream.move_after(&token);
            Some(ForceMode::In)
        }
        Out => {
            stream.move_after(&token);
            Some(ForceMode::Out)
        }
        _ => None,
    };

    Ok(optional_force_mode)
}

fn parse_assignment_or_procedure_call(
    stream: &mut TokenStream,
    token: &Token,
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
            let token = stream.peek_expect()?;
            match token.kind {
                Force => {
                    stream.move_after(&token);
                    SequentialStatement::SignalForceAssignment(SignalForceAssignment {
                        target,
                        force_mode: parse_optional_force_mode(stream)?,
                        rhs: parse_variable_assignment_right_hand(stream)?
                    })
                },
                Release => {
                    stream.move_after(&token);
                    let force_mode = parse_optional_force_mode(stream)?;
                    stream.expect_kind(SemiColon)?;

                    SequentialStatement::SignalReleaseAssignment(SignalReleaseAssignment {
                        target,
                        force_mode
                    })
                }
                _ => {
                    let delay_mechanism = parse_delay_mechanism(stream)?;
                    SequentialStatement::SignalAssignment(SignalAssignment {
                        target,
                        delay_mechanism,
                        rhs: parse_signal_assignment_right_hand(stream)?
                    })
                }
            }
        },
        SemiColon => {
            match target.item {
                Target::Name(Name::FunctionCall(call)) => {
                    SequentialStatement::ProcedureCall(*call)
                }
                Target::Name(name) => {
                    SequentialStatement::ProcedureCall(
                        FunctionCall {
                            name: WithPos::from(name, target.pos),
                            parameters: vec![]
                        })
                }
                Target::Aggregate(..) => {
                    return Err(Diagnostic::error(target, "Expected procedure call, got aggregate"));
                }
            }
        }
    ))
}

fn parse_target_initial_token(
    stream: &mut TokenStream,
    token: Token,
) -> ParseResult<WithPos<Target>> {
    if token.kind == LeftPar {
        Ok(parse_aggregate_leftpar_known(stream)?.map_into(Target::Aggregate))
    } else {
        Ok(parse_name_initial_token(stream, token)?.map_into(Target::Name))
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
            if stream.skip_if_kind(Force)? {
                Ok(SequentialStatement::SignalForceAssignment(SignalForceAssignment {
                    target,
                    force_mode: parse_optional_force_mode(stream)?,
                    rhs: AssignmentRightHand::Selected(parse_selection(stream, expression, parse_expression)?)
                }))
            } else {
                Ok(SequentialStatement::SignalAssignment(SignalAssignment {
                    target,
                    delay_mechanism: parse_delay_mechanism(stream)?,
                    rhs: AssignmentRightHand::Selected(parse_selection(stream, expression, parse_waveform)?)
            }))
        }
        }
    )
}

fn parse_unlabeled_sequential_statement(
    stream: &mut TokenStream,
    token: Token,
    diagnostics: &mut dyn DiagnosticHandler,
) -> ParseResult<SequentialStatement> {
    let statement = {
        try_token_kind!(
            token,
            Wait => SequentialStatement::Wait(parse_wait_statement_known_keyword(stream)?),
            Assert => SequentialStatement::Assert(parse_assert_statement_known_keyword(stream)?),
            Report => SequentialStatement::Report(parse_report_statement_known_keyword(stream)?),
            If => SequentialStatement::If(parse_if_statement_known_keyword(stream, diagnostics)?),
            Case => SequentialStatement::Case(parse_case_statement_known_keyword(stream, diagnostics)?),
            For | Loop | While => {
                SequentialStatement::Loop(parse_loop_statement_initial_token(stream, &token, diagnostics)?)
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
            Identifier|LeftPar|LtLt => {
                let target = parse_target_initial_token(stream, token)?;
                let token = stream.expect()?;
                parse_assignment_or_procedure_call(stream, &token, target)?
            }
        )
    };
    Ok(statement)
}

#[cfg(test)]
pub fn parse_sequential_statement(
    stream: &mut TokenStream,
    diagnostics: &mut dyn DiagnosticHandler,
) -> ParseResult<LabeledSequentialStatement> {
    let token = stream.expect()?;
    parse_sequential_statement_initial_token(stream, token, diagnostics)
}

pub fn parse_sequential_statement_initial_token(
    stream: &mut TokenStream,
    token: Token,
    diagnostics: &mut dyn DiagnosticHandler,
) -> ParseResult<LabeledSequentialStatement> {
    if token.kind == Identifier {
        let name = parse_name_initial_token(stream, token)?;
        let token = stream.expect()?;
        if token.kind == Colon {
            let label = Some(to_simple_name(name)?);
            let token = stream.expect()?;
            let statement = parse_unlabeled_sequential_statement(stream, token, diagnostics)?;
            Ok(LabeledSequentialStatement { label, statement })
        } else {
            let target = name.map_into(Target::Name);
            let statement = parse_assignment_or_procedure_call(stream, &token, target)?;
            Ok(LabeledSequentialStatement {
                label: None,
                statement,
            })
        }
    } else {
        let statement = parse_unlabeled_sequential_statement(stream, token, diagnostics)?;
        Ok(LabeledSequentialStatement {
            label: None,
            statement,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{DelayMechanism, Ident};

    use crate::test_util::Code;

    fn parse(code: &str) -> (Code, LabeledSequentialStatement) {
        let code = Code::new(code);
        let stmt = code.with_stream_no_diagnostics(parse_sequential_statement);
        (code, stmt)
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
        let (code, statement) = parse("foo: wait;");
        assert_eq!(
            statement,
            with_label(
                Some(code.s1("foo").ident()),
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
        let (code, statement) = parse("wait on foo, bar;");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::Wait(WaitStatement {
                    sensitivity_clause: vec![code.s1("foo").name(), code.s1("bar").name()],
                    condition_clause: None,
                    timeout_clause: None
                })
            )
        );
    }

    #[test]
    fn parse_wait_statement_with_condition() {
        let (code, statement) = parse("wait until a = b;");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::Wait(WaitStatement {
                    sensitivity_clause: vec![],
                    condition_clause: Some(code.s1("a = b").expr()),
                    timeout_clause: None
                })
            )
        );
    }

    #[test]
    fn parse_wait_statement_with_timeout() {
        let (code, statement) = parse("wait for 2 ns;");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::Wait(WaitStatement {
                    sensitivity_clause: vec![],
                    condition_clause: None,
                    timeout_clause: Some(code.s1("2 ns").expr())
                })
            )
        );
    }

    #[test]
    fn parse_wait_statement_with_all_parts() {
        let (code, statement) = parse("wait on foo until bar for 2 ns;");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::Wait(WaitStatement {
                    sensitivity_clause: vec![code.s1("foo").name()],
                    condition_clause: Some(code.s1("bar").expr()),
                    timeout_clause: Some(code.s1("2 ns").expr())
                })
            )
        );
    }

    #[test]
    fn parse_simple_assert_statement() {
        let (code, statement) = parse("assert false;");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::Assert(AssertStatement {
                    condition: code.s1("false").expr(),
                    report: None,
                    severity: None
                })
            )
        );
    }

    #[test]
    fn parse_assert_statement() {
        let (code, statement) = parse("assert false report \"message\" severity error;");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::Assert(AssertStatement {
                    condition: code.s1("false").expr(),
                    report: Some(code.s1("\"message\"").expr()),
                    severity: Some(code.s1("error").expr())
                })
            )
        );
    }

    #[test]
    fn parse_report_statement() {
        let (code, statement) = parse("report \"message\" severity error;");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::Report(ReportStatement {
                    report: code.s1("\"message\"").expr(),
                    severity: Some(code.s1("error").expr())
                })
            )
        );
    }

    #[test]
    fn parse_simple_signal_assignment() {
        let (code, statement) = parse("foo(0) <= bar(1,2) after 2 ns;");

        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::SignalAssignment(SignalAssignment {
                    target: code.s1("foo(0)").name().map_into(Target::Name),
                    delay_mechanism: None,
                    rhs: AssignmentRightHand::Simple(code.s1("bar(1,2) after 2 ns").waveform())
                })
            )
        );
    }

    #[test]
    fn parse_simple_signal_force_assignment() {
        let (code, statement) = parse("foo(0) <= force bar(1,2);");

        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::SignalForceAssignment(SignalForceAssignment {
                    target: code.s1("foo(0)").name().map_into(Target::Name),
                    force_mode: None,
                    rhs: AssignmentRightHand::Simple(code.s1("bar(1,2)").expr())
                })
            )
        );

        // Fore mode in
        let (code, statement) = parse("foo(0) <= force in bar(1,2);");

        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::SignalForceAssignment(SignalForceAssignment {
                    target: code.s1("foo(0)").name().map_into(Target::Name),
                    force_mode: Some(ForceMode::In),
                    rhs: AssignmentRightHand::Simple(code.s1("bar(1,2)").expr())
                })
            )
        );

        // Fore mode out
        let (code, statement) = parse("foo(0) <= force out bar(1,2);");

        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::SignalForceAssignment(SignalForceAssignment {
                    target: code.s1("foo(0)").name().map_into(Target::Name),
                    force_mode: Some(ForceMode::Out),
                    rhs: AssignmentRightHand::Simple(code.s1("bar(1,2)").expr())
                })
            )
        );
    }

    #[test]
    fn parse_simple_signal_release_assignment() {
        let (code, statement) = parse("foo(0) <= release;");

        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::SignalReleaseAssignment(SignalReleaseAssignment {
                    target: code.s1("foo(0)").name().map_into(Target::Name),
                    force_mode: None
                })
            )
        );
    }

    #[test]
    fn parse_signal_assignment_external_name() {
        let (code, statement) = parse("<< signal dut.foo : boolean  >> <= bar(1,2);");

        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::SignalAssignment(SignalAssignment {
                    target: code
                        .s1("<< signal dut.foo : boolean  >>")
                        .name()
                        .map_into(Target::Name),
                    delay_mechanism: None,
                    rhs: AssignmentRightHand::Simple(code.s1("bar(1,2)").waveform())
                })
            )
        );
    }

    #[test]
    fn parse_simple_signal_assignment_delay_mechanism() {
        let (code, statement) = parse("foo(0) <= transport bar(1,2);");

        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::SignalAssignment(SignalAssignment {
                    target: code.s1("foo(0)").name().map_into(Target::Name),
                    delay_mechanism: Some(DelayMechanism::Transport),
                    rhs: AssignmentRightHand::Simple(code.s1("bar(1,2)").waveform())
                })
            )
        );
    }

    #[test]
    fn parse_simple_variable_assignment() {
        let (code, statement) = parse("foo(0) := bar(1,2);");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::VariableAssignment(VariableAssignment {
                    target: code.s1("foo(0)").name().map_into(Target::Name),
                    rhs: AssignmentRightHand::Simple(code.s1("bar(1,2)").expr())
                })
            )
        );
    }

    #[test]
    fn parse_variable_assignment_external_name() {
        let (code, statement) = parse("<< variable dut.foo : boolean >> := bar(1,2);");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::VariableAssignment(VariableAssignment {
                    target: code
                        .s1("<< variable dut.foo : boolean >>")
                        .name()
                        .map_into(Target::Name),
                    rhs: AssignmentRightHand::Simple(code.s1("bar(1,2)").expr())
                })
            )
        );
    }

    #[test]
    fn parse_simple_aggregate_variable_assignment() {
        let (code, statement) = parse("(foo, 1 => bar) := integer_vector'(1, 2);");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::VariableAssignment(VariableAssignment {
                    target: code
                        .s1("(foo, 1 => bar)")
                        .aggregate()
                        .map_into(Target::Aggregate),
                    rhs: AssignmentRightHand::Simple(code.s1("integer_vector'(1, 2)").expr())
                })
            )
        );
    }

    #[test]
    fn parse_labeled_aggregate_variable_assignment() {
        let (code, statement) = parse("name: (foo, 1 => bar) := integer_vector'(1, 2);");
        assert_eq!(
            statement,
            with_label(
                Some(code.s1("name").ident()),
                SequentialStatement::VariableAssignment(VariableAssignment {
                    target: code
                        .s1("(foo, 1 => bar)")
                        .aggregate()
                        .map_into(Target::Aggregate),
                    rhs: AssignmentRightHand::Simple(code.s1("integer_vector'(1, 2)").expr())
                })
            )
        );
    }

    #[test]
    fn parse_labeled_simple_variable_assignment() {
        let (code, statement) = parse("name: foo(0) := bar(1,2);");
        assert_eq!(
            statement,
            with_label(
                Some(code.s1("name").ident()),
                SequentialStatement::VariableAssignment(VariableAssignment {
                    target: code.s1("foo(0)").name().map_into(Target::Name),
                    rhs: AssignmentRightHand::Simple(code.s1("bar(1,2)").expr())
                })
            )
        );
    }

    #[test]
    fn parse_conditional_variable_assignment() {
        let (code, statement) = parse("foo(0) := bar(1,2) when cond = true;");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::VariableAssignment(VariableAssignment {
                    target: code.s1("foo(0)").name().map_into(Target::Name),
                    rhs: AssignmentRightHand::Conditional(Conditionals {
                        conditionals: vec![Conditional {
                            condition: code.s1("cond = true").expr(),
                            item: code.s1("bar(1,2)").expr()
                        }],
                        else_item: None
                    })
                })
            )
        );
    }

    #[test]
    fn parse_selected_variable_assignment() {
        let (code, statement) = parse(
            "\
with x(0) + 1 select
   foo(0) := bar(1,2) when 0|1,
             def when others;",
        );

        let selection = Selection {
            expression: code.s1("x(0) + 1").expr(),
            alternatives: vec![
                Alternative {
                    choices: code.s1("0|1").choices(),
                    item: code.s1("bar(1,2)").expr(),
                },
                Alternative {
                    choices: code.s1("others").choices(),
                    item: code.s1("def").expr(),
                },
            ],
        };

        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::VariableAssignment(VariableAssignment {
                    target: code.s1("foo(0)").name().map_into(Target::Name),
                    rhs: AssignmentRightHand::Selected(selection)
                })
            )
        );
    }

    #[test]
    fn parse_conditional_variable_assignment_several() {
        let (code, statement) = parse("foo(0) := bar(1,2) when cond = true else expr2 when cond2;");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::VariableAssignment(VariableAssignment {
                    target: code.s1("foo(0)").name().map_into(Target::Name),
                    rhs: AssignmentRightHand::Conditional(Conditionals {
                        conditionals: vec![
                            Conditional {
                                condition: code.s1("cond = true").expr(),
                                item: code.s1("bar(1,2)").expr()
                            },
                            Conditional {
                                condition: code.s1("cond2").expr(),
                                item: code.s1("expr2").expr()
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
        let (code, statement) = parse("foo(0) := bar(1,2) when cond = true else expr2;");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::VariableAssignment(VariableAssignment {
                    target: code.s1("foo(0)").name().map_into(Target::Name),
                    rhs: AssignmentRightHand::Conditional(Conditionals {
                        conditionals: vec![Conditional {
                            condition: code.s1("cond = true").expr(),
                            item: code.s1("bar(1,2)").expr()
                        }],
                        else_item: Some(code.s1("expr2").expr())
                    })
                })
            )
        );
    }

    #[test]
    fn parse_conditional_signal_assignment() {
        let (code, statement) = parse("foo(0) <= bar(1,2) after 2 ns when cond;");

        let conditionals = Conditionals {
            conditionals: vec![Conditional {
                condition: code.s1("cond").expr(),
                item: code.s1("bar(1,2) after 2 ns").waveform(),
            }],
            else_item: None,
        };

        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::SignalAssignment(SignalAssignment {
                    target: code.s1("foo(0)").name().map_into(Target::Name),
                    delay_mechanism: None,
                    rhs: AssignmentRightHand::Conditional(conditionals)
                })
            )
        );
    }

    #[test]
    fn parse_conditional_signal_force_assignment() {
        let (code, statement) = parse("foo(0) <= force bar(1,2) when cond;");

        let conditionals = Conditionals {
            conditionals: vec![Conditional {
                condition: code.s1("cond").expr(),
                item: code.s1("bar(1,2)").expr(),
            }],
            else_item: None,
        };

        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::SignalForceAssignment(SignalForceAssignment {
                    target: code.s1("foo(0)").name().map_into(Target::Name),
                    force_mode: None,
                    rhs: AssignmentRightHand::Conditional(conditionals)
                })
            )
        );
    }

    #[test]
    fn parse_selected_signal_assignment() {
        let (code, statement) = parse(
            "\
with x(0) + 1 select
   foo(0) <= transport bar(1,2) after 2 ns when 0|1,
                       def when others;",
        );

        let selection = Selection {
            expression: code.s1("x(0) + 1").expr(),
            alternatives: vec![
                Alternative {
                    choices: code.s1("0|1").choices(),
                    item: code.s1("bar(1,2) after 2 ns").waveform(),
                },
                Alternative {
                    choices: code.s1("others").choices(),
                    item: code.s1("def").waveform(),
                },
            ],
        };

        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::SignalAssignment(SignalAssignment {
                    target: code.s1("foo(0)").name().map_into(Target::Name),
                    delay_mechanism: Some(DelayMechanism::Transport),
                    rhs: AssignmentRightHand::Selected(selection)
                })
            )
        );
    }

    #[test]
    fn parse_selected_signal_force_assignment() {
        let (code, statement) = parse(
            "\
with x(0) + 1 select
   foo(0) <= force bar(1,2) when 0|1,
                       def when others;",
        );

        let selection = Selection {
            expression: code.s1("x(0) + 1").expr(),
            alternatives: vec![
                Alternative {
                    choices: code.s1("0|1").choices(),
                    item: code.s1("bar(1,2)").expr(),
                },
                Alternative {
                    choices: code.s1("others").choices(),
                    item: code.s1("def").expr(),
                },
            ],
        };

        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::SignalForceAssignment(SignalForceAssignment {
                    target: code.s1("foo(0)").name().map_into(Target::Name),
                    force_mode: None,
                    rhs: AssignmentRightHand::Selected(selection)
                })
            )
        );
    }

    #[test]
    fn parse_procedure_call_statement() {
        let (code, statement) = parse("foo(1,2);");

        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::ProcedureCall(code.s1("foo(1,2)").function_call())
            )
        );
    }

    #[test]
    fn parse_procedure_call_no_args() {
        let (code, statement) = parse("foo;");

        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::ProcedureCall(code.s1("foo").function_call())
            )
        );
    }

    #[test]
    fn parse_simple_if_statement() {
        let (code, statement) = parse(
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
                        condition: code.s1("cond = true").expr(),
                        item: vec![
                            code.s1("foo(1,2);").sequential_statement(),
                            code.s1("x := 1;").sequential_statement()
                        ]
                    }],
                    else_item: None
                })
            )
        );
    }

    #[test]
    fn parse_if_else_statement() {
        let (code, statement) = parse(
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
                        condition: code.s1("cond = true").expr(),
                        item: vec![code.s1("foo(1,2);").sequential_statement()]
                    }],
                    else_item: Some(vec![code.s1("x := 1;").sequential_statement()])
                })
            )
        );
    }

    #[test]
    fn parse_if_elsif_else_statement() {
        let (code, statement) = parse(
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
                            condition: code.s1("cond = true").expr(),
                            item: vec![code.s1("foo(1,2);").sequential_statement()]
                        },
                        Conditional {
                            condition: code.s1("cond2 = false").expr(),
                            item: vec![code.s1("y := 2;").sequential_statement()]
                        }
                    ],
                    else_item: Some(vec![code.s1("x := 1;").sequential_statement()])
                })
            )
        );
    }

    #[test]
    fn parse_case_statement() {
        let (code, statement) = parse(
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
                    expression: code.s1("foo(1)").expr(),
                    alternatives: vec![
                        Alternative {
                            choices: code.s1("1 | 2").choices(),
                            item: vec![
                                code.s1("stmt1;").sequential_statement(),
                                code.s1("stmt2;").sequential_statement()
                            ]
                        },
                        Alternative {
                            choices: code.s1("others").choices(),
                            item: vec![
                                code.s1("stmt3;").sequential_statement(),
                                code.s1("stmt4;").sequential_statement(),
                            ]
                        }
                    ],
                })
            )
        );
    }

    #[test]
    fn parse_loop_statement() {
        let (code, statement) = parse(
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
                        code.s1("stmt1;").sequential_statement(),
                        code.s1("stmt2;").sequential_statement()
                    ],
                })
            )
        );
    }

    #[test]
    fn parse_while_loop_statement() {
        let (code, statement) = parse(
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
                    iteration_scheme: Some(IterationScheme::While(code.s1("foo = true").expr())),
                    statements: vec![
                        code.s1("stmt1;").sequential_statement(),
                        code.s1("stmt2;").sequential_statement()
                    ],
                })
            )
        );
    }
    #[test]
    fn parse_for_loop_statement() {
        let (code, statement) = parse(
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
                        code.s1("idx").ident(),
                        code.s1("0 to 3").discrete_range()
                    )),
                    statements: vec![
                        code.s1("stmt1;").sequential_statement(),
                        code.s1("stmt2;").sequential_statement()
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
        let (code, statement) = parse("next foo;");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::Next(NextStatement {
                    loop_label: Some(code.s1("foo").ident()),
                    condition: None,
                })
            )
        );
    }

    #[test]
    fn parse_next_statement_condition() {
        let (code, statement) = parse("next when condition;");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::Next(NextStatement {
                    loop_label: None,
                    condition: Some(code.s1("condition").expr()),
                })
            )
        );
    }

    #[test]
    fn parse_next_statement_loop_label_condition() {
        let (code, statement) = parse("next foo when condition;");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::Next(NextStatement {
                    loop_label: Some(code.s1("foo").ident()),
                    condition: Some(code.s1("condition").expr()),
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
        let (code, statement) = parse("exit foo;");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::Exit(ExitStatement {
                    loop_label: Some(code.s1("foo").ident()),
                    condition: None,
                })
            )
        );
    }

    #[test]
    fn parse_exit_statement_condition() {
        let (code, statement) = parse("exit when condition;");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::Exit(ExitStatement {
                    loop_label: None,
                    condition: Some(code.s1("condition").expr()),
                })
            )
        );
    }

    #[test]
    fn parse_exit_statement_loop_label_condition() {
        let (code, statement) = parse("exit foo when condition;");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::Exit(ExitStatement {
                    loop_label: Some(code.s1("foo").ident()),
                    condition: Some(code.s1("condition").expr()),
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
        let (code, statement) = parse("return 1 + 2;");
        assert_eq!(
            statement,
            with_label(
                None,
                SequentialStatement::Return(ReturnStatement {
                    expression: Some(code.s1("1 + 2").expr()),
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
