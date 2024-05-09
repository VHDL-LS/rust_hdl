// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use super::common::parse_optional;
use super::common::ParseResult;
use super::expression::parse_aggregate;
use super::expression::{parse_choices, parse_expression};
use super::names::parse_name;
use super::range::parse_discrete_range;
use super::tokens::Kind::*;
use super::waveform::{parse_delay_mechanism, parse_waveform};
use crate::ast::token_range::WithTokenSpan;
use crate::ast::*;
use crate::data::*;
use crate::syntax::common::check_label_identifier_mismatch;
use crate::HasTokenSpan;
use vhdl_lang::syntax::parser::ParsingContext;
use vhdl_lang::TokenSpan;

/// LRM 10.2 Wait statement
fn parse_wait_statement(ctx: &mut ParsingContext<'_>) -> ParseResult<WaitStatement> {
    ctx.stream.expect_kind(Wait)?;
    let mut sensitivity_clause = vec![];
    if ctx.stream.skip_if_kind(On) {
        loop {
            sensitivity_clause.push(parse_name(ctx)?);
            if !ctx.stream.skip_if_kind(Comma) {
                break;
            }
        }
    }

    let condition_clause = parse_optional(ctx, Until, parse_expression)?;
    let timeout_clause = parse_optional(ctx, For, parse_expression)?;

    ctx.stream.expect_kind(SemiColon)?;
    Ok(WaitStatement {
        sensitivity_clause,
        condition_clause,
        timeout_clause,
    })
}

/// LRM 10.3 Assertion statement
pub fn parse_assert_statement(ctx: &mut ParsingContext<'_>) -> ParseResult<AssertStatement> {
    ctx.stream.expect_kind(Assert)?;
    let condition = parse_expression(ctx)?;
    let report = parse_optional(ctx, Report, parse_expression)?;
    let severity = parse_optional(ctx, Severity, parse_expression)?;

    ctx.stream.expect_kind(SemiColon)?;
    Ok(AssertStatement {
        condition,
        report,
        severity,
    })
}

/// LRM 10.4 Report statement
fn parse_report_statement(ctx: &mut ParsingContext<'_>) -> ParseResult<ReportStatement> {
    ctx.stream.expect_kind(Report)?;
    let report = parse_expression(ctx)?;
    let severity = parse_optional(ctx, Severity, parse_expression)?;

    ctx.stream.expect_kind(SemiColon)?;
    Ok(ReportStatement { report, severity })
}

pub fn parse_labeled_sequential_statements(
    ctx: &mut ParsingContext<'_>,
) -> ParseResult<Vec<LabeledSequentialStatement>> {
    let mut statements = Vec::new();
    loop {
        let token = ctx.stream.peek_expect()?;
        match token.kind {
            End | Else | Elsif | When => {
                break Ok(statements);
            }
            _ => match parse_sequential_statement(ctx) {
                Ok(stmt) => statements.push(stmt),
                Err(diag) => {
                    ctx.diagnostics.push(diag);
                    let _ = ctx
                        .stream
                        .skip_until(|kind| matches!(kind, End | Else | Elsif | When));
                }
            },
        }
    }
}

/// LRM 10.8 If statement
fn parse_if_statement(
    ctx: &mut ParsingContext<'_>,
    label: Option<&Ident>,
) -> ParseResult<IfStatement> {
    ctx.stream.expect_kind(If)?;
    let mut conditionals = Vec::new();
    let mut else_branch = None;
    loop {
        let condition = parse_expression(ctx)?;
        ctx.stream.expect_kind(Then)?;
        let statements = parse_labeled_sequential_statements(ctx)?;

        let conditional = Conditional {
            condition,
            item: statements,
        };

        expect_token!(
            ctx.stream,
            end_token,
            Elsif => {
                conditionals.push(conditional);
                continue;
            },

            Else => {
                conditionals.push(conditional);
                let statements = parse_labeled_sequential_statements(ctx)?;

                expect_token!(
                    ctx.stream,
                    end_token,
                    End => {
                        ctx.stream.expect_kind(If)?;
                        else_branch = Some(statements);
                        break;
                    }
                );
            },
            End => {
                ctx.stream.expect_kind(If)?;
                conditionals.push(conditional);
                break;
            }
        );
    }

    let end_label_pos =
        check_label_identifier_mismatch(ctx, label, ctx.stream.pop_optional_ident());
    ctx.stream.expect_kind(SemiColon)?;
    Ok(IfStatement {
        conds: Conditionals {
            conditionals,
            else_item: else_branch,
        },
        end_label_pos,
    })
}

/// LRM 10.9 Case statement
fn parse_case_statement(
    ctx: &mut ParsingContext<'_>,
    label: Option<&Ident>,
) -> ParseResult<CaseStatement> {
    ctx.stream.expect_kind(Case)?;
    let is_matching = ctx.stream.pop_if_kind(Que).is_some();
    let expression = parse_expression(ctx)?;
    ctx.stream.expect_kind(Is)?;
    ctx.stream.expect_kind(When)?;
    let mut alternatives = Vec::new();

    loop {
        let choices = parse_choices(ctx)?;
        ctx.stream.expect_kind(RightArrow)?;
        let statements = parse_labeled_sequential_statements(ctx)?;
        let alternative = Alternative {
            choices,
            item: statements,
        };

        expect_token!(
            ctx.stream,
            end_token,
            When => {
                alternatives.push(alternative);
                continue;
            },
            End => {
                ctx.stream.expect_kind(Case)?;
                if is_matching {
                    ctx.stream.expect_kind(Que)?;
                }
                let end_label_pos = check_label_identifier_mismatch(ctx, label, ctx.stream.pop_optional_ident());
                alternatives.push(alternative);
                ctx.stream.expect_kind(SemiColon)?;
                return Ok(CaseStatement {
                    is_matching,
                    expression,
                    alternatives,
                    end_label_pos,
                });
            }
        );
    }
}

/// LRM 10.10 Loop statement
fn parse_loop_statement(
    ctx: &mut ParsingContext<'_>,
    label: Option<&Ident>,
) -> ParseResult<LoopStatement> {
    let iteration_scheme = {
        expect_token!(
            ctx.stream, token,
            Loop => None,
            While => {
                let expression = parse_expression(ctx)?;
                ctx.stream.expect_kind(Loop)?;
                Some(IterationScheme::While(expression))
            },
            For => {
                let ident = ctx.stream.expect_ident()?;
                ctx.stream.expect_kind(In)?;
                let discrete_range = parse_discrete_range(ctx)?;
                ctx.stream.expect_kind(Loop)?;
                Some(IterationScheme::For(ident.into(), discrete_range))
            }
        )
    };

    let statements = parse_labeled_sequential_statements(ctx)?;

    expect_token!(
        ctx.stream,
        end_token,
        End => {
            ctx.stream.expect_kind(Loop)?;
            let end_label_pos = check_label_identifier_mismatch(ctx, label, ctx.stream.pop_optional_ident());
            ctx.stream.expect_kind(SemiColon)?;
            Ok(LoopStatement {
                iteration_scheme,
                statements,
                end_label_pos,
            })
        }
    )
}

/// LRM 10.11 Next statement
fn parse_next_statement(ctx: &mut ParsingContext<'_>) -> ParseResult<NextStatement> {
    ctx.stream.expect_kind(Next)?;
    let loop_label = ctx.stream.pop_optional_ident();
    let condition = parse_optional(ctx, When, parse_expression)?;
    ctx.stream.expect_kind(SemiColon)?;
    Ok(NextStatement {
        loop_label: loop_label.map(WithRef::new),
        condition,
    })
}

/// LRM 10.12 Exit statement
fn parse_exit_statement(ctx: &mut ParsingContext<'_>) -> ParseResult<ExitStatement> {
    ctx.stream.expect_kind(Exit)?;
    let loop_label = ctx.stream.pop_optional_ident();
    let condition = parse_optional(ctx, When, parse_expression)?;
    ctx.stream.expect_kind(SemiColon)?;
    Ok(ExitStatement {
        loop_label: loop_label.map(WithRef::new),
        condition,
    })
}

/// LRM 10.13 Return statement
fn parse_return_statement(ctx: &mut ParsingContext<'_>) -> ParseResult<ReturnStatement> {
    ctx.stream.expect_kind(Return)?;
    let expression = {
        if ctx.stream.peek_kind() == Some(SemiColon) {
            None
        } else {
            Some(parse_expression(ctx)?)
        }
    };
    ctx.stream.expect_kind(SemiColon)?;
    Ok(ReturnStatement { expression })
}

/// LRM 10.5 Signal assignment statement
pub fn parse_signal_assignment_right_hand(
    ctx: &mut ParsingContext<'_>,
) -> ParseResult<AssignmentRightHand<Waveform>> {
    parse_assignment_right_hand(ctx, parse_waveform)
}

/// LRM 10.6 Variable assignment statement
fn parse_variable_assignment_right_hand(
    ctx: &mut ParsingContext<'_>,
) -> ParseResult<AssignmentRightHand<WithTokenSpan<Expression>>> {
    parse_assignment_right_hand(ctx, parse_expression)
}

fn parse_assignment_right_hand<T, F>(
    ctx: &mut ParsingContext<'_>,
    parse_item: F,
) -> ParseResult<AssignmentRightHand<T>>
where
    F: Fn(&mut ParsingContext<'_>) -> ParseResult<T>,
{
    let item = parse_item(ctx)?;

    expect_token!(
        ctx.stream,
        token,
        When => {
            Ok(AssignmentRightHand::Conditional(parse_conditonals(ctx, item, parse_item)?))

        },
        SemiColon => {
            Ok(AssignmentRightHand::Simple(item))
        }
    )
}

fn parse_conditonals<T, F>(
    ctx: &mut ParsingContext<'_>,
    initial_item: T,
    parse_item: F,
) -> ParseResult<Conditionals<T>>
where
    F: Fn(&mut ParsingContext<'_>) -> ParseResult<T>,
{
    let condition = parse_expression(ctx)?;
    let cond_expr = Conditional {
        condition,
        item: initial_item,
    };

    let mut conditionals = vec![cond_expr];
    let mut else_item = None;

    loop {
        expect_token!(
            ctx.stream,
            token,
            SemiColon => {
                break;
            },
            Else => {
                let item = parse_item(ctx)?;
                expect_token!(
                    ctx.stream,
                    token,
                    SemiColon =>  {
                        else_item = Some(item);
                        break;
                    },
                    When => {
                        let condition = parse_expression(ctx)?;
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
    ctx: &mut ParsingContext<'_>,
    expression: WithTokenSpan<Expression>,
    parse_item: F,
) -> ParseResult<Selection<T>>
where
    F: Fn(&mut ParsingContext<'_>) -> ParseResult<T>,
{
    let mut alternatives = Vec::with_capacity(2);

    loop {
        let item = parse_item(ctx)?;
        ctx.stream.expect_kind(When)?;
        let choices = parse_choices(ctx)?;
        alternatives.push(Alternative { choices, item });

        expect_token!(
            ctx.stream,
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

fn parse_optional_force_mode(ctx: &mut ParsingContext<'_>) -> ParseResult<Option<ForceMode>> {
    let token = ctx.stream.peek_expect()?;
    let optional_force_mode = match token.kind {
        In => {
            ctx.stream.skip();
            Some(ForceMode::In)
        }
        Out => {
            ctx.stream.skip();
            Some(ForceMode::Out)
        }
        _ => None,
    };

    Ok(optional_force_mode)
}

fn parse_assignment_or_procedure_call(
    ctx: &mut ParsingContext<'_>,
    target: WithTokenSpan<Target>,
) -> ParseResult<SequentialStatement> {
    let target_span = target.span();
    Ok(expect_token!(
        ctx.stream,
        token,
        ColonEq => {
            let rhs = parse_variable_assignment_right_hand(ctx)?;
            SequentialStatement::VariableAssignment(VariableAssignment {
                target,
                rhs,
            })
        },
        LTE => {
            let token = ctx.stream.peek_expect()?;
            match token.kind {
                Force => {
                    ctx.stream.skip();
                    let force_mode = parse_optional_force_mode(ctx)?;
                    let rhs = parse_variable_assignment_right_hand(ctx)?;
                    ctx.stream.get_last_token_id();
                    SequentialStatement::SignalForceAssignment(SignalForceAssignment {
                        target,
                        force_mode,
                        rhs,
                    })
                },
                Release => {
                    ctx.stream.skip();
                    let force_mode = parse_optional_force_mode(ctx)?;
                    let end_token = ctx.stream.expect_kind(SemiColon)?;

                    SequentialStatement::SignalReleaseAssignment(SignalReleaseAssignment {
                        target,
                        force_mode,
                        span: target_span.end_with(end_token)
                    })
                }
                _ => {
                    let delay_mechanism = parse_delay_mechanism(ctx)?;
                    let rhs = parse_signal_assignment_right_hand(ctx)?;
                    ctx.stream.get_last_token_id();
                    SequentialStatement::SignalAssignment(SignalAssignment {
                        target,
                        delay_mechanism,
                        rhs,
                    })
                }
            }
        },
        SemiColon => {
            match target.item {
                Target::Name(Name::CallOrIndexed(call)) => {
                    SequentialStatement::ProcedureCall(WithTokenSpan::from(*call, target.span))
                }
                Target::Name(name) => {
                    SequentialStatement::ProcedureCall(
                        WithTokenSpan::from(CallOrIndexed {
                            name: WithTokenSpan::from(name, target.span),
                            parameters: vec![]
                        }, target.span))
                }
                Target::Aggregate(..) => {
                    return Err(Diagnostic::syntax_error(target.pos(ctx), "Expected procedure call, got aggregate"));
                }
            }
        }
    ))
}

pub fn parse_target(ctx: &mut ParsingContext<'_>) -> ParseResult<WithTokenSpan<Target>> {
    if ctx.stream.next_kind_is(LeftPar) {
        Ok(parse_aggregate(ctx)?.map_into(Target::Aggregate))
    } else {
        Ok(parse_name(ctx)?.map_into(Target::Name))
    }
}

fn parse_selected_assignment(ctx: &mut ParsingContext<'_>) -> ParseResult<SequentialStatement> {
    let expression = parse_expression(ctx)?;
    ctx.stream.expect_kind(Select)?;
    let target = parse_target(ctx)?;
    expect_token!(
        ctx.stream,
        token,
        ColonEq => {
            let rhs = AssignmentRightHand::Selected(parse_selection(ctx, expression, parse_expression)?);
            Ok(SequentialStatement::VariableAssignment(VariableAssignment {
                target,
                rhs,
            }))
        },
        LTE => {
            if ctx.stream.skip_if_kind(Force) {
                let force_mode = parse_optional_force_mode(ctx)?;
                let rhs = AssignmentRightHand::Selected(parse_selection(ctx, expression, parse_expression)?);
                ctx.stream.get_last_token_id();
                Ok(SequentialStatement::SignalForceAssignment(SignalForceAssignment {
                    target,
                    force_mode,
                    rhs,
                }))
            } else {
                let delay_mechanism = parse_delay_mechanism(ctx)?;
                let rhs = AssignmentRightHand::Selected(parse_selection(ctx, expression, parse_waveform)?);
                ctx.stream.get_last_token_id();
                Ok(SequentialStatement::SignalAssignment(SignalAssignment {
                    target,
                    delay_mechanism,
                    rhs,
            }))
        }
        }
    )
}

fn parse_unlabeled_sequential_statement(
    ctx: &mut ParsingContext<'_>,
    label: Option<&Ident>,
) -> ParseResult<SequentialStatement> {
    let token = ctx.stream.peek_expect()?;
    ctx.stream.get_current_token_id();
    let statement = {
        try_init_token_kind!(
            token,
            Wait => SequentialStatement::Wait(parse_wait_statement(ctx)?),
            Assert => SequentialStatement::Assert(parse_assert_statement(ctx)?),
            Report => SequentialStatement::Report(parse_report_statement(ctx)?),
            If => SequentialStatement::If(parse_if_statement(ctx, label)?),
            Case => SequentialStatement::Case(parse_case_statement(ctx, label)?),
            For | Loop | While => {
                SequentialStatement::Loop(parse_loop_statement(ctx, label)?)
            },
            Next => SequentialStatement::Next(parse_next_statement(ctx)?),
            Exit => SequentialStatement::Exit(parse_exit_statement(ctx)?),
            Return => SequentialStatement::Return(parse_return_statement(ctx)?),
            Null => {
                ctx.stream.skip();
                ctx.stream.expect_kind(SemiColon)?;
                SequentialStatement::Null
            },
            With => {
                ctx.stream.skip();
                parse_selected_assignment(ctx)?
            },
            Identifier|LeftPar|LtLt => {
                let target = parse_target(ctx)?;
                parse_assignment_or_procedure_call(ctx, target)?
            }
        )
    };
    Ok(statement)
}

pub fn parse_sequential_statement(
    ctx: &mut ParsingContext<'_>,
) -> ParseResult<LabeledSequentialStatement> {
    let start_idx = ctx.stream.get_current_token_id();

    if ctx.stream.next_kind_is(Identifier) {
        let name = parse_name(ctx)?;
        if ctx.stream.skip_if_kind(Colon) {
            let label = Some(to_simple_name(ctx, name)?);
            let start_idx = ctx.stream.get_current_token_id();
            let statement = parse_unlabeled_sequential_statement(ctx, label.as_ref())?;
            let end = ctx.stream.get_last_token_id();
            Ok(LabeledSequentialStatement {
                label: WithDecl::new(label),
                statement: WithTokenSpan::from(statement, TokenSpan::new(start_idx, end)),
            })
        } else {
            let target = name.map_into(Target::Name);
            let statement = parse_assignment_or_procedure_call(ctx, target)?;
            let end = ctx.stream.get_last_token_id();
            Ok(LabeledSequentialStatement {
                label: WithDecl::new(None),
                statement: WithTokenSpan::from(statement, TokenSpan::new(start_idx, end)),
            })
        }
    } else {
        let statement = parse_unlabeled_sequential_statement(ctx, None)?;
        let end = ctx.stream.get_last_token_id();
        Ok(LabeledSequentialStatement {
            label: WithDecl::new(None),
            statement: WithTokenSpan::from(statement, TokenSpan::new(start_idx, end)),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{DelayMechanism, Ident};
    use pretty_assertions::assert_eq;

    use crate::syntax::test::Code;

    fn parse(code: &str) -> (Code, LabeledSequentialStatement) {
        let code = Code::new(code);
        let stmt = code.with_stream_no_diagnostics(parse_sequential_statement);
        (code, stmt)
    }

    fn parse_stmt(code: &Code) -> LabeledSequentialStatement {
        code.with_stream_no_diagnostics(parse_sequential_statement)
    }

    fn with_label(
        label: Option<Ident>,
        statement: WithTokenSpan<SequentialStatement>,
    ) -> LabeledSequentialStatement {
        LabeledSequentialStatement {
            label: WithDecl::new(label),
            statement,
        }
    }

    #[test]
    fn parse_simple_wait_statement() {
        let (code, statement) = parse("wait;");
        assert_eq!(
            statement,
            with_label(
                None,
                WithTokenSpan::from(
                    SequentialStatement::Wait(WaitStatement {
                        sensitivity_clause: vec![],
                        condition_clause: None,
                        timeout_clause: None,
                    }),
                    code.token_span()
                )
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
                WithTokenSpan::new(
                    SequentialStatement::Wait(WaitStatement {
                        sensitivity_clause: vec![],
                        condition_clause: None,
                        timeout_clause: None,
                    }),
                    code.pos_after("foo: ").token_span()
                )
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
                WithTokenSpan::new(
                    SequentialStatement::Wait(WaitStatement {
                        sensitivity_clause: vec![code.s1("foo").name(), code.s1("bar").name()],
                        condition_clause: None,
                        timeout_clause: None,
                    }),
                    code.token_span()
                )
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
                WithTokenSpan::new(
                    SequentialStatement::Wait(WaitStatement {
                        sensitivity_clause: vec![],
                        condition_clause: Some(code.s1("a = b").expr()),
                        timeout_clause: None,
                    }),
                    code.token_span()
                )
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
                WithTokenSpan::new(
                    SequentialStatement::Wait(WaitStatement {
                        sensitivity_clause: vec![],
                        condition_clause: None,
                        timeout_clause: Some(code.s1("2 ns").expr()),
                    }),
                    code.token_span()
                )
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
                WithTokenSpan::new(
                    SequentialStatement::Wait(WaitStatement {
                        sensitivity_clause: vec![code.s1("foo").name()],
                        condition_clause: Some(code.s1("bar").expr()),
                        timeout_clause: Some(code.s1("2 ns").expr()),
                    }),
                    code.token_span()
                )
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
                WithTokenSpan::new(
                    SequentialStatement::Assert(AssertStatement {
                        condition: code.s1("false").expr(),
                        report: None,
                        severity: None,
                    }),
                    code.token_span()
                )
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
                WithTokenSpan::new(
                    SequentialStatement::Assert(AssertStatement {
                        condition: code.s1("false").expr(),
                        report: Some(code.s1("\"message\"").expr()),
                        severity: Some(code.s1("error").expr()),
                    }),
                    code.token_span()
                )
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
                WithTokenSpan::new(
                    SequentialStatement::Report(ReportStatement {
                        report: code.s1("\"message\"").expr(),
                        severity: Some(code.s1("error").expr()),
                    }),
                    code.token_span()
                )
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
                WithTokenSpan::new(
                    SequentialStatement::SignalAssignment(SignalAssignment {
                        target: code.s1("foo(0)").name().map_into(Target::Name),
                        delay_mechanism: None,
                        rhs: AssignmentRightHand::Simple(code.s1("bar(1,2) after 2 ns").waveform()),
                    }),
                    code.token_span()
                )
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
                WithTokenSpan::new(
                    SequentialStatement::SignalForceAssignment(SignalForceAssignment {
                        target: code.s1("foo(0)").name().map_into(Target::Name),
                        force_mode: None,
                        rhs: AssignmentRightHand::Simple(code.s1("bar(1,2)").expr()),
                    }),
                    code.token_span()
                )
            )
        );

        // Fore mode in
        let (code, statement) = parse("foo(0) <= force in bar(1,2);");

        assert_eq!(
            statement,
            with_label(
                None,
                WithTokenSpan::new(
                    SequentialStatement::SignalForceAssignment(SignalForceAssignment {
                        target: code.s1("foo(0)").name().map_into(Target::Name),
                        force_mode: Some(ForceMode::In),
                        rhs: AssignmentRightHand::Simple(code.s1("bar(1,2)").expr()),
                    }),
                    code.token_span()
                )
            )
        );

        // Fore mode out
        let (code, statement) = parse("foo(0) <= force out bar(1,2);");

        assert_eq!(
            statement,
            with_label(
                None,
                WithTokenSpan::new(
                    SequentialStatement::SignalForceAssignment(SignalForceAssignment {
                        target: code.s1("foo(0)").name().map_into(Target::Name),
                        force_mode: Some(ForceMode::Out),
                        rhs: AssignmentRightHand::Simple(code.s1("bar(1,2)").expr()),
                    }),
                    code.token_span()
                )
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
                WithTokenSpan::new(
                    SequentialStatement::SignalReleaseAssignment(SignalReleaseAssignment {
                        target: code.s1("foo(0)").name().map_into(Target::Name),
                        force_mode: None,
                        span: code.token_span(),
                    }),
                    code.token_span()
                )
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
                WithTokenSpan::new(
                    SequentialStatement::SignalAssignment(SignalAssignment {
                        target: code
                            .s1("<< signal dut.foo : boolean  >>")
                            .name()
                            .map_into(Target::Name),
                        delay_mechanism: None,
                        rhs: AssignmentRightHand::Simple(code.s1("bar(1,2)").waveform()),
                    }),
                    code.token_span()
                )
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
                WithTokenSpan::new(
                    SequentialStatement::SignalAssignment(SignalAssignment {
                        target: code.s1("foo(0)").name().map_into(Target::Name),
                        delay_mechanism: Some(DelayMechanism::Transport),
                        rhs: AssignmentRightHand::Simple(code.s1("bar(1,2)").waveform()),
                    }),
                    code.token_span()
                )
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
                WithTokenSpan::new(
                    SequentialStatement::VariableAssignment(VariableAssignment {
                        target: code.s1("foo(0)").name().map_into(Target::Name),
                        rhs: AssignmentRightHand::Simple(code.s1("bar(1,2)").expr()),
                    }),
                    code.token_span()
                )
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
                WithTokenSpan::new(
                    SequentialStatement::VariableAssignment(VariableAssignment {
                        target: code
                            .s1("<< variable dut.foo : boolean >>")
                            .name()
                            .map_into(Target::Name),
                        rhs: AssignmentRightHand::Simple(code.s1("bar(1,2)").expr()),
                    }),
                    code.token_span()
                )
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
                WithTokenSpan::new(
                    SequentialStatement::VariableAssignment(VariableAssignment {
                        target: code
                            .s1("(foo, 1 => bar)")
                            .aggregate()
                            .map_into(Target::Aggregate),
                        rhs: AssignmentRightHand::Simple(code.s1("integer_vector'(1, 2)").expr()),
                    }),
                    code.token_span()
                )
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
                WithTokenSpan::new(
                    SequentialStatement::VariableAssignment(VariableAssignment {
                        target: code
                            .s1("(foo, 1 => bar)")
                            .aggregate()
                            .map_into(Target::Aggregate),
                        rhs: AssignmentRightHand::Simple(code.s1("integer_vector'(1, 2)").expr()),
                    }),
                    code.pos_after("name: ").token_span()
                )
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
                WithTokenSpan::new(
                    SequentialStatement::VariableAssignment(VariableAssignment {
                        target: code.s1("foo(0)").name().map_into(Target::Name),
                        rhs: AssignmentRightHand::Simple(code.s1("bar(1,2)").expr()),
                    }),
                    code.pos_after("name: ").token_span()
                )
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
                WithTokenSpan::new(
                    SequentialStatement::VariableAssignment(VariableAssignment {
                        target: code.s1("foo(0)").name().map_into(Target::Name),
                        rhs: AssignmentRightHand::Conditional(Conditionals {
                            conditionals: vec![Conditional {
                                condition: code.s1("cond = true").expr(),
                                item: code.s1("bar(1,2)").expr()
                            }],
                            else_item: None
                        }),
                    }),
                    code.token_span()
                )
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
                WithTokenSpan::new(
                    SequentialStatement::VariableAssignment(VariableAssignment {
                        target: code.s1("foo(0)").name().map_into(Target::Name),
                        rhs: AssignmentRightHand::Selected(selection),
                    }),
                    code.token_span()
                )
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
                WithTokenSpan::new(
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
                        }),
                    }),
                    code.token_span()
                )
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
                WithTokenSpan::new(
                    SequentialStatement::VariableAssignment(VariableAssignment {
                        target: code.s1("foo(0)").name().map_into(Target::Name),
                        rhs: AssignmentRightHand::Conditional(Conditionals {
                            conditionals: vec![Conditional {
                                condition: code.s1("cond = true").expr(),
                                item: code.s1("bar(1,2)").expr()
                            }],
                            else_item: Some(code.s1("expr2").expr())
                        }),
                    }),
                    code.token_span()
                )
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
                WithTokenSpan::new(
                    SequentialStatement::SignalAssignment(SignalAssignment {
                        target: code.s1("foo(0)").name().map_into(Target::Name),
                        delay_mechanism: None,
                        rhs: AssignmentRightHand::Conditional(conditionals),
                    }),
                    code.token_span()
                )
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
                WithTokenSpan::new(
                    SequentialStatement::SignalForceAssignment(SignalForceAssignment {
                        target: code.s1("foo(0)").name().map_into(Target::Name),
                        force_mode: None,
                        rhs: AssignmentRightHand::Conditional(conditionals),
                    }),
                    code.token_span()
                )
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
                WithTokenSpan::new(
                    SequentialStatement::SignalAssignment(SignalAssignment {
                        target: code.s1("foo(0)").name().map_into(Target::Name),
                        delay_mechanism: Some(DelayMechanism::Transport),
                        rhs: AssignmentRightHand::Selected(selection),
                    }),
                    code.token_span()
                )
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
                WithTokenSpan::new(
                    SequentialStatement::SignalForceAssignment(SignalForceAssignment {
                        target: code.s1("foo(0)").name().map_into(Target::Name),
                        force_mode: None,
                        rhs: AssignmentRightHand::Selected(selection),
                    }),
                    code.token_span()
                )
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
                WithTokenSpan::new(
                    SequentialStatement::ProcedureCall(code.s1("foo(1,2)").function_call()),
                    code.token_span()
                )
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
                WithTokenSpan::new(
                    SequentialStatement::ProcedureCall(code.s1("foo").function_call()),
                    code.token_span()
                )
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
end if;",
        );
        assert_eq!(
            statement,
            with_label(
                None,
                WithTokenSpan::new(
                    SequentialStatement::If(IfStatement {
                        conds: Conditionals {
                            conditionals: vec![Conditional {
                                condition: code.s1("cond = true").expr(),
                                item: vec![
                                    code.s1("foo(1,2);").sequential_statement(),
                                    code.s1("x := 1;").sequential_statement()
                                ]
                            }],
                            else_item: None
                        },
                        end_label_pos: None,
                    }),
                    code.token_span()
                )
            )
        );
    }
    #[test]
    fn parse_labeled_if_statement() {
        let (code, statement) = parse(
            "\
mylabel: if cond = true then
   foo(1,2);
   x := 1;
end if mylabel;",
        );
        assert_eq!(
            statement,
            with_label(
                Some(code.s1("mylabel").ident()),
                WithTokenSpan::new(
                    SequentialStatement::If(IfStatement {
                        conds: Conditionals {
                            conditionals: vec![Conditional {
                                condition: code.s1("cond = true").expr(),
                                item: vec![
                                    code.s1("foo(1,2);").sequential_statement(),
                                    code.s1("x := 1;").sequential_statement()
                                ]
                            }],
                            else_item: None
                        },
                        end_label_pos: Some(code.s("mylabel", 2).pos()),
                    }),
                    code.pos_after("mylabel: ").token_span()
                )
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
end if;",
        );
        assert_eq!(
            statement,
            with_label(
                None,
                WithTokenSpan::new(
                    SequentialStatement::If(IfStatement {
                        conds: Conditionals {
                            conditionals: vec![Conditional {
                                condition: code.s1("cond = true").expr(),
                                item: vec![code.s1("foo(1,2);").sequential_statement()]
                            }],
                            else_item: Some(vec![code.s1("x := 1;").sequential_statement()])
                        },
                        end_label_pos: None,
                    }),
                    code.token_span()
                )
            )
        );
    }

    #[test]
    fn parse_labeled_if_else_statement() {
        let (code, statement) = parse(
            "\
mylabel: if cond = true then
   foo(1,2);
else
   x := 1;
end if mylabel;",
        );
        assert_eq!(
            statement,
            with_label(
                Some(code.s1("mylabel").ident()),
                WithTokenSpan::new(
                    SequentialStatement::If(IfStatement {
                        conds: Conditionals {
                            conditionals: vec![Conditional {
                                condition: code.s1("cond = true").expr(),
                                item: vec![code.s1("foo(1,2);").sequential_statement()]
                            }],
                            else_item: Some(vec![code.s1("x := 1;").sequential_statement()])
                        },
                        end_label_pos: Some(code.s("mylabel", 2).pos()),
                    }),
                    code.pos_after("mylabel: ").token_span()
                )
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
end if;",
        );
        assert_eq!(
            statement,
            with_label(
                None,
                WithTokenSpan::new(
                    SequentialStatement::If(IfStatement {
                        conds: Conditionals {
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
                        },
                        end_label_pos: None,
                    }),
                    code.token_span()
                )
            )
        );
    }
    #[test]
    fn parse_labeled_if_elsif_else_statement() {
        let (code, statement) = parse(
            "\
mylabel: if cond = true then
   foo(1,2);
elsif cond2 = false then
   y := 2;
else
   x := 1;
end if mylabel;",
        );
        assert_eq!(
            statement,
            with_label(
                Some(code.s1("mylabel").ident()),
                WithTokenSpan::new(
                    SequentialStatement::If(IfStatement {
                        conds: Conditionals {
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
                        },
                        end_label_pos: Some(code.s("mylabel", 2).pos()),
                    }),
                    code.pos_after("mylabel: ").token_span()
                )
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
end case;",
        );
        assert_eq!(
            statement,
            with_label(
                None,
                WithTokenSpan::new(
                    SequentialStatement::Case(CaseStatement {
                        is_matching: false,
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
                        end_label_pos: None,
                    }),
                    code.token_span()
                )
            )
        );
    }

    #[test]
    fn parse_matching_case_statement() {
        let (code, statement) = parse(
            "\
case? foo(1) is
  when others => null;
end case?;",
        );
        assert_eq!(
            statement,
            with_label(
                None,
                WithTokenSpan::new(
                    SequentialStatement::Case(CaseStatement {
                        is_matching: true,
                        expression: code.s1("foo(1)").expr(),
                        alternatives: vec![Alternative {
                            choices: code.s1("others").choices(),
                            item: vec![code.s1("null;").sequential_statement(),]
                        }],
                        end_label_pos: None,
                    }),
                    code.token_span()
                )
            )
        );
    }

    #[test]
    fn parse_loop_statement() {
        let (code, statement) = parse(
            "\
lbl: loop
  stmt1;
  stmt2;
end loop lbl;",
        );
        assert_eq!(
            statement,
            with_label(
                Some(code.s1("lbl").ident()),
                WithTokenSpan::new(
                    SequentialStatement::Loop(LoopStatement {
                        iteration_scheme: None,
                        statements: vec![
                            code.s1("stmt1;").sequential_statement(),
                            code.s1("stmt2;").sequential_statement()
                        ],
                        end_label_pos: Some(code.s("lbl", 2).pos()),
                    }),
                    code.pos_after("lbl: ").token_span()
                )
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
end loop;",
        );
        assert_eq!(
            statement,
            with_label(
                None,
                WithTokenSpan::new(
                    SequentialStatement::Loop(LoopStatement {
                        iteration_scheme: Some(IterationScheme::While(
                            code.s1("foo = true").expr()
                        )),
                        statements: vec![
                            code.s1("stmt1;").sequential_statement(),
                            code.s1("stmt2;").sequential_statement()
                        ],
                        end_label_pos: None,
                    }),
                    code.token_span()
                )
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
end loop;",
        );
        assert_eq!(
            statement,
            with_label(
                None,
                WithTokenSpan::new(
                    SequentialStatement::Loop(LoopStatement {
                        iteration_scheme: Some(IterationScheme::For(
                            code.s1("idx").decl_ident(),
                            code.s1("0 to 3").discrete_range()
                        )),
                        statements: vec![
                            code.s1("stmt1;").sequential_statement(),
                            code.s1("stmt2;").sequential_statement()
                        ],
                        end_label_pos: None,
                    }),
                    code.token_span()
                )
            )
        );
    }

    #[test]
    fn parse_next_statement() {
        let (code, statement) = parse("next;");
        assert_eq!(
            statement,
            with_label(
                None,
                WithTokenSpan::new(
                    SequentialStatement::Next(NextStatement {
                        loop_label: None,
                        condition: None,
                    }),
                    code.token_span()
                )
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
                WithTokenSpan::new(
                    SequentialStatement::Next(NextStatement {
                        loop_label: Some(code.s1("foo").ident().into_ref()),
                        condition: None,
                    }),
                    code.token_span()
                )
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
                WithTokenSpan::new(
                    SequentialStatement::Next(NextStatement {
                        loop_label: None,
                        condition: Some(code.s1("condition").expr()),
                    }),
                    code.token_span()
                )
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
                WithTokenSpan::new(
                    SequentialStatement::Next(NextStatement {
                        loop_label: Some(code.s1("foo").ident().into_ref()),
                        condition: Some(code.s1("condition").expr()),
                    }),
                    code.token_span()
                )
            )
        );
    }

    #[test]
    fn parse_exit_statement() {
        let (code, statement) = parse("exit;");
        assert_eq!(
            statement,
            with_label(
                None,
                WithTokenSpan::new(
                    SequentialStatement::Exit(ExitStatement {
                        loop_label: None,
                        condition: None,
                    }),
                    code.token_span()
                )
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
                WithTokenSpan::new(
                    SequentialStatement::Exit(ExitStatement {
                        loop_label: Some(code.s1("foo").ident().into_ref()),
                        condition: None,
                    }),
                    code.token_span()
                )
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
                WithTokenSpan::new(
                    SequentialStatement::Exit(ExitStatement {
                        loop_label: None,
                        condition: Some(code.s1("condition").expr()),
                    }),
                    code.token_span()
                )
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
                WithTokenSpan::new(
                    SequentialStatement::Exit(ExitStatement {
                        loop_label: Some(code.s1("foo").ident().into_ref()),
                        condition: Some(code.s1("condition").expr()),
                    }),
                    code.token_span()
                )
            )
        );
    }

    #[test]
    fn parse_return_statement() {
        let code = Code::new("return;");

        let statement = parse_stmt(&code);
        assert_eq!(
            statement,
            with_label(
                None,
                WithTokenSpan::new(
                    SequentialStatement::Return(ReturnStatement { expression: None }),
                    code.token_span()
                )
            )
        );
    }

    #[test]
    fn parse_return_statement_expression() {
        let code = Code::new("return 1 + 2;");
        let statement = parse_stmt(&code);
        assert_eq!(
            statement,
            with_label(
                None,
                WithTokenSpan::new(
                    SequentialStatement::Return(ReturnStatement {
                        expression: Some(code.s1("1 + 2").expr()),
                    }),
                    code.token_span()
                )
            )
        );
    }

    #[test]
    fn parse_null_statement() {
        let (code, statement) = parse("null;");
        assert_eq!(
            statement,
            with_label(
                None,
                WithTokenSpan::new(SequentialStatement::Null, code.token_span())
            )
        );
    }
}
