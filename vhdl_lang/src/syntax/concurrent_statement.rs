// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use super::common::check_label_identifier_mismatch;
use super::common::ParseResult;
use super::declarative_part::{is_declarative_part, parse_declarative_part};
use super::expression::parse_aggregate;
use super::expression::{parse_choices, parse_expression};
use super::interface_declaration::{parse_generic_interface_list, parse_port_interface_list};
use super::names::parse_name;
use super::names::{expression_to_ident, parse_association_list, parse_selected_name};
use super::range::parse_discrete_range;
use super::sequential_statement::{
    parse_assert_statement, parse_labeled_sequential_statements, parse_selection,
    parse_signal_assignment_right_hand, parse_target,
};
use super::tokens::Kind::*;
use super::waveform::{parse_delay_mechanism, parse_waveform};
use crate::ast::*;
use crate::data::*;
use crate::syntax::{Kind, TokenAccess};
use crate::TokenId;
use vhdl_lang::syntax::parser::ParsingContext;
use vhdl_lang::TokenSpan;

/// LRM 11.2 Block statement
pub fn parse_block_statement(
    ctx: &mut ParsingContext<'_>,
    label: Option<&Ident>,
) -> ParseResult<BlockStatement> {
    let start_tok = ctx.stream.expect_kind(Block)?;
    let token = ctx.stream.peek_expect()?;
    let guard_condition = {
        match token.kind {
            LeftPar => {
                ctx.stream.skip();
                let expr = parse_expression(ctx)?;
                ctx.stream.pop_if_kind(RightPar);
                Some(expr)
            }
            _ => None,
        }
    };
    ctx.stream.pop_if_kind(Is);
    let header = parse_block_header(ctx)?;
    let decl = parse_declarative_part(ctx)?;
    ctx.stream.expect_kind(Begin)?;
    let statements = parse_labeled_concurrent_statements(ctx)?;
    ctx.stream.expect_kind(End)?;
    ctx.stream.expect_kind(Block)?;
    let end_ident = ctx.stream.pop_optional_ident();
    let end_tok = ctx.stream.expect_kind(SemiColon)?;
    Ok(BlockStatement {
        guard_condition,
        header,
        decl,
        statements,
        end_label_pos: check_label_identifier_mismatch(ctx, label, end_ident),
        span: TokenSpan::new(start_tok, end_tok),
    })
}

fn parse_block_header(ctx: &mut ParsingContext<'_>) -> ParseResult<BlockHeader> {
    let mut generic_clause = None;
    let mut generic_map = None;
    let mut port_clause = None;
    let mut port_map = None;

    loop {
        let token_id = ctx.stream.get_current_token_id();
        let token = ctx.stream.peek_expect()?;
        match token.kind {
            Generic => {
                ctx.stream.skip();
                if let Some(map_token) = ctx.stream.pop_if_kind(Map) {
                    if port_clause.is_some() || port_map.is_some() {
                        ctx.diagnostics.push(Diagnostic::syntax_error(
                            ctx.stream.get_token(map_token),
                            "Generic map must come before port clause and port map",
                        ));
                    } else if generic_clause.is_none() {
                        ctx.diagnostics.push(Diagnostic::syntax_error(
                            ctx.stream.get_token(map_token),
                            "Generic map declared without preceding generic clause",
                        ));
                    } else if generic_map.is_some() {
                        ctx.diagnostics.push(Diagnostic::syntax_error(
                            ctx.stream.get_token(map_token),
                            "Duplicate generic map",
                        ));
                    }
                    let (list, closing_paren) = parse_association_list(ctx)?;
                    ctx.stream.expect_kind(SemiColon)?;
                    if generic_map.is_none() {
                        generic_map = Some(MapAspect {
                            start: token_id,
                            list,
                            closing_paren,
                        });
                    }
                } else {
                    if generic_map.is_some() {
                        ctx.diagnostics.push(Diagnostic::syntax_error(
                            token,
                            "Generic clause must come before generic map",
                        ));
                    } else if generic_clause.is_some() {
                        ctx.diagnostics
                            .push(Diagnostic::syntax_error(token, "Duplicate generic clause"));
                    }
                    let parsed_generic_list = parse_generic_interface_list(ctx)?;
                    ctx.stream.expect_kind(SemiColon)?;
                    if generic_clause.is_none() {
                        generic_clause = Some(parsed_generic_list);
                    }
                }
            }
            Port => {
                ctx.stream.skip();
                if let Some(map_token) = ctx.stream.pop_if_kind(Map) {
                    if port_clause.is_none() {
                        ctx.diagnostics.push(Diagnostic::syntax_error(
                            ctx.stream.get_token(map_token),
                            "Port map declared without preceeding port clause",
                        ));
                    } else if port_map.is_some() {
                        ctx.diagnostics.push(Diagnostic::syntax_error(
                            ctx.stream.get_token(map_token),
                            "Duplicate port map",
                        ));
                    }
                    let (list, closing_paren) = parse_association_list(ctx)?;
                    ctx.stream.expect_kind(SemiColon)?;
                    if port_map.is_none() {
                        port_map = Some(MapAspect {
                            start: token_id,
                            list,
                            closing_paren,
                        });
                    }
                } else {
                    if port_map.is_some() {
                        ctx.diagnostics.push(Diagnostic::syntax_error(
                            token,
                            "Port clause declared after port map",
                        ));
                    } else if port_clause.is_some() {
                        ctx.diagnostics
                            .push(Diagnostic::syntax_error(token, "Duplicate port clause"));
                    }
                    let parsed_port_list = parse_port_interface_list(ctx)?;
                    ctx.stream.expect_kind(SemiColon)?;
                    if port_clause.is_none() {
                        port_clause = Some(parsed_port_list);
                    }
                }
            }
            _ => break,
        }
    }

    Ok(BlockHeader {
        generic_clause,
        generic_map,
        port_clause,
        port_map,
    })
}

/// LRM 11.3 Process statement
pub fn parse_process_statement(
    ctx: &mut ParsingContext<'_>,
    label: Option<&Ident>,
    postponed: Option<TokenId>,
) -> ParseResult<ProcessStatement> {
    let process_token = ctx.stream.expect_kind(Process)?;
    let sensitivity_list = if ctx.stream.skip_if_kind(LeftPar) {
        peek_token!(ctx.stream, token,
        All => {
            ctx.stream.skip();
            ctx.stream.expect_kind(RightPar)?;
            Some(SensitivityList::All)
        },
        RightPar => {
            ctx.stream.skip();
            ctx.diagnostics.push(
                Diagnostic::syntax_error(token, "Processes with sensitivity lists must contain at least one element.")
            );
            Some(SensitivityList::Names(Vec::new()))
        },
        Identifier => {
            let mut names = Vec::with_capacity(1);
            loop {
                names.push(parse_name(ctx)?);
                peek_token!(ctx.stream, token,
                    RightPar => {
                        ctx.stream.skip();
                        break Some(SensitivityList::Names(names));
                    },
                    Comma => {
                        ctx.stream.skip();
                    },
                    Identifier => {
                    }
                );

            }
        })
    } else {
        None
    };

    ctx.stream.pop_if_kind(Is);
    let decl = parse_declarative_part(ctx)?;
    ctx.stream.expect_kind(Begin)?;
    let statements = parse_labeled_sequential_statements(ctx)?;
    ctx.stream.expect_kind(End)?;

    if let Some(token) = ctx.stream.pop_if_kind(Postponed) {
        if postponed.is_none() {
            ctx.diagnostics.push(Diagnostic::syntax_error(
                ctx.stream.get_token(token),
                "'postponed' at the end of non-postponed process.",
            ));
        }
    }
    ctx.stream.expect_kind(Process)?;
    let end_ident = ctx.stream.pop_optional_ident();
    let end_tok = ctx.stream.expect_kind(SemiColon)?;
    Ok(ProcessStatement {
        postponed: postponed.is_some(),
        sensitivity_list,
        decl,
        statements,
        end_label_pos: check_label_identifier_mismatch(ctx, label, end_ident),
        span: TokenSpan::new(postponed.unwrap_or(process_token), end_tok),
    })
}

fn to_procedure_call(
    ctx: &mut ParsingContext<'_>,
    target: WithTokenSpan<Target>,
    postponed: bool,
) -> ParseResult<ConcurrentProcedureCall> {
    match target.item {
        Target::Name(Name::CallOrIndexed(call)) => Ok(ConcurrentProcedureCall {
            postponed,
            call: WithTokenSpan::from(*call, target.span),
        }),
        Target::Name(name) => Ok(ConcurrentProcedureCall {
            postponed,
            call: WithTokenSpan::from(
                CallOrIndexed {
                    name: WithTokenSpan::from(name, target.span.clone()),
                    parameters: vec![],
                },
                target.span,
            ),
        }),
        Target::Aggregate(..) => Err(Diagnostic::syntax_error(
            target.to_pos(ctx.stream),
            "Expected procedure call, got aggregate",
        )),
    }
}

/// Assume target and <= is parsed already
fn parse_assignment_known_target(
    ctx: &mut ParsingContext<'_>,
    target: WithTokenSpan<Target>,
) -> ParseResult<ConcurrentStatement> {
    // @TODO postponed
    let postponed = false;
    // @TODO guarded
    let guarded = false;
    let delay_mechanism = parse_delay_mechanism(ctx)?;
    Ok(ConcurrentStatement::Assignment(
        ConcurrentSignalAssignment {
            postponed,
            guarded,
            target,
            delay_mechanism,
            rhs: parse_signal_assignment_right_hand(ctx)?,
        },
    ))
}

fn parse_assignment_or_procedure_call(
    ctx: &mut ParsingContext<'_>,
    target: WithTokenSpan<Target>,
) -> ParseResult<ConcurrentStatement> {
    expect_token!(ctx.stream, token,
    LTE => {
        parse_assignment_known_target(ctx, target)
    },
    SemiColon => {
        Ok(ConcurrentStatement::ProcedureCall(to_procedure_call(ctx, target, false)?))
    })
}

fn parse_selected_signal_assignment(
    ctx: &mut ParsingContext<'_>,
    postponed: bool,
) -> ParseResult<ConcurrentSignalAssignment> {
    ctx.stream.expect_kind(With)?;
    let expression = parse_expression(ctx)?;
    ctx.stream.expect_kind(Select)?;
    let target = parse_target(ctx)?;
    ctx.stream.expect_kind(LTE)?;
    // @TODO guarded
    let guarded = false;
    let delay_mechanism = parse_delay_mechanism(ctx)?;
    let rhs = AssignmentRightHand::Selected(parse_selection(ctx, expression, parse_waveform)?);
    Ok(ConcurrentSignalAssignment {
        postponed,
        guarded,
        target,
        delay_mechanism,
        rhs,
    })
}

pub fn parse_concurrent_assert_statement(
    ctx: &mut ParsingContext<'_>,
    postponed: bool,
) -> ParseResult<ConcurrentAssertStatement> {
    Ok(ConcurrentAssertStatement {
        postponed,
        statement: parse_assert_statement(ctx)?,
    })
}

pub fn parse_map_aspect(
    ctx: &mut ParsingContext<'_>,
    aspect_kind: Kind,
) -> ParseResult<Option<MapAspect>> {
    if let Some(aspect) = ctx.stream.pop_if_kind(aspect_kind) {
        ctx.stream.expect_kind(Map)?;
        let (list, closing_paren) = parse_association_list(ctx)?;
        Ok(Some(MapAspect {
            start: aspect,
            list,
            closing_paren,
        }))
    } else {
        Ok(None)
    }
}

pub fn parse_generic_and_port_map(
    ctx: &mut ParsingContext<'_>,
) -> ParseResult<(Option<MapAspect>, Option<MapAspect>)> {
    let generic_map = parse_map_aspect(ctx, Generic)?;
    let port_map = parse_map_aspect(ctx, Port)?;

    Ok((generic_map, port_map))
}

pub fn parse_instantiation_statement(
    ctx: &mut ParsingContext<'_>,
    start_tok: TokenId,
    unit: InstantiatedUnit,
) -> ParseResult<InstantiationStatement> {
    let (generic_map, port_map) = parse_generic_and_port_map(ctx)?;

    let end_tok = ctx.stream.expect_kind(SemiColon)?;

    Ok(InstantiationStatement {
        unit,
        generic_map,
        port_map,
        span: TokenSpan::new(start_tok, end_tok),
    })
}

fn parse_optional_declarative_part(
    ctx: &mut ParsingContext<'_>,
) -> ParseResult<Option<Vec<Declaration>>> {
    if is_declarative_part(ctx)? {
        let decls = parse_declarative_part(ctx)?;
        ctx.stream.expect_kind(Begin)?;
        Ok(Some(decls))
    } else {
        Ok(None)
    }
}

fn parse_generate_body(
    ctx: &mut ParsingContext<'_>,
    alternative_label: Option<WithDecl<Ident>>,
) -> ParseResult<GenerateBody> {
    let decl = parse_optional_declarative_part(ctx)?;
    let statements = parse_labeled_concurrent_statements(ctx)?;
    let mut end_label_pos = None;

    // Potential inner end [ alternative_label ];
    if ctx.stream.next_kinds_are(&[End, SemiColon]) {
        // Inner end no label
        ctx.stream.skip();
        ctx.stream.skip();
    } else if ctx.stream.next_kinds_are(&[End, Identifier]) {
        ctx.stream.skip();
        // Inner with identifier
        let end_ident = ctx.stream.expect_ident()?;
        end_label_pos = check_label_identifier_mismatch(
            ctx,
            alternative_label.as_ref().map(|label| &label.tree),
            Some(end_ident),
        );
        ctx.stream.expect_kind(SemiColon)?;
    }

    let body = GenerateBody {
        alternative_label,
        decl,
        statements,
        end_label_pos,
    };

    Ok(body)
}

/// 11.8 Generate statements
fn parse_for_generate_statement(
    ctx: &mut ParsingContext<'_>,
    label: Option<&Ident>,
) -> ParseResult<ForGenerateStatement> {
    let start_tok = ctx.stream.expect_kind(For)?;
    let index_name = WithDecl::new(ctx.stream.expect_ident()?);
    ctx.stream.expect_kind(In)?;
    let discrete_range = parse_discrete_range(ctx)?;
    ctx.stream.expect_kind(Generate)?;
    let body = parse_generate_body(ctx, None)?;
    ctx.stream.expect_kind(End)?;
    ctx.stream.expect_kind(Generate)?;
    let end_ident = ctx.stream.pop_optional_ident();
    let end_tok = ctx.stream.expect_kind(SemiColon)?;

    Ok(ForGenerateStatement {
        index_name,
        discrete_range,
        body,
        end_label_pos: check_label_identifier_mismatch(ctx, label, end_ident),
        span: TokenSpan::new(start_tok, end_tok),
    })
}

/// 11.8 Generate statements
fn parse_if_generate_statement(
    ctx: &mut ParsingContext,
    label: Option<&Ident>,
) -> ParseResult<IfGenerateStatement> {
    let mut conditionals = Vec::new();
    let else_branch;

    let start_tok = ctx.stream.expect_kind(If)?;
    loop {
        let mut condition = parse_expression(ctx)?;
        let mut alternative_label = None;
        if ctx.stream.skip_if_kind(Colon) {
            alternative_label = Some(WithDecl::new(expression_to_ident(ctx, condition)?));
            condition = parse_expression(ctx)?;
        }
        ctx.stream.expect_kind(Generate)?;
        let body = parse_generate_body(ctx, alternative_label)?;

        let conditional = Conditional {
            condition,
            item: body,
        };

        conditionals.push(conditional);

        expect_token!(
            ctx.stream, end_token,
            End => {
                else_branch = None;
                break;
            },
            Elsif => {
                continue;
            },
            Else => {
                let alternative_label = expect_token!(
                    ctx.stream,
                    token,
                    Generate => {
                        None
                    },
                    Identifier => {
                        ctx.stream.expect_kind(Colon)?;
                        ctx.stream.expect_kind(Generate)?;
                        Some(WithDecl::new(token.to_identifier_value()?))
                    }
                );
                let body = parse_generate_body(ctx, alternative_label)?;
                ctx.stream.expect_kind(End)?;
                else_branch = Some(body);
                break;
            }
        );
    }

    ctx.stream.expect_kind(Generate)?;
    let end_ident = ctx.stream.pop_optional_ident();
    let end_tok = ctx.stream.expect_kind(SemiColon)?;

    Ok(IfGenerateStatement {
        conds: Conditionals {
            conditionals,
            else_item: else_branch,
        },
        end_label_pos: check_label_identifier_mismatch(ctx, label, end_ident),
        span: TokenSpan::new(start_tok, end_tok),
    })
}

/// 11.8 Generate statements
fn parse_case_generate_statement(
    ctx: &mut ParsingContext<'_>,
    label: Option<&Ident>,
) -> ParseResult<CaseGenerateStatement> {
    let start_tok = ctx.stream.expect_kind(Case)?;
    let expression = parse_expression(ctx)?;
    ctx.stream.expect_kind(Generate)?;
    ctx.stream.expect_kind(When)?;

    let mut alternatives = Vec::with_capacity(2);
    loop {
        let alternative_label = {
            if ctx.stream.next_kinds_are(&[Identifier, Colon]) {
                let ident = ctx.stream.expect_ident()?;
                ctx.stream.expect_kind(Colon)?;
                Some(WithDecl::new(ident))
            } else {
                None
            }
        };
        let choices = parse_choices(ctx)?;
        ctx.stream.expect_kind(RightArrow)?;
        let body = parse_generate_body(ctx, alternative_label)?;

        alternatives.push(Alternative {
            choices,
            item: body,
        });

        expect_token!(
            ctx.stream, end_token,
            End => break,
            When => continue
        );
    }

    ctx.stream.expect_kind(Generate)?;
    let end_ident = ctx.stream.pop_optional_ident();
    let end_tok = ctx.stream.expect_kind(SemiColon)?;

    Ok(CaseGenerateStatement {
        sels: Selection {
            expression,
            alternatives,
        },
        end_label_pos: check_label_identifier_mismatch(ctx, label, end_ident),
        span: TokenSpan::new(start_tok, end_tok),
    })
}

pub fn parse_concurrent_statement(
    ctx: &mut ParsingContext<'_>,
    label: Option<&Ident>,
) -> ParseResult<ConcurrentStatement> {
    let token = ctx.stream.peek_expect()?;
    let start_tok = ctx.stream.get_current_token_id();
    let statement = {
        try_init_token_kind!(
            token,
            Block => {
                ConcurrentStatement::Block(parse_block_statement(ctx, label)?)
            },
            Process => {
                ConcurrentStatement::Process(parse_process_statement(ctx, label, None)?)
            },
            Component => {
                ctx.stream.skip();
                let unit = InstantiatedUnit::Component(parse_selected_name(ctx)?);
                ConcurrentStatement::Instance(parse_instantiation_statement(ctx, start_tok, unit)?)
            },
            Configuration => {
                ctx.stream.skip();
                let unit = InstantiatedUnit::Configuration(parse_selected_name(ctx)?);
                ConcurrentStatement::Instance(parse_instantiation_statement(ctx, start_tok, unit)?)
            },
            Entity => {
                ctx.stream.skip();
                let name = parse_selected_name(ctx)?;
                let arch = {
                    if ctx.stream.skip_if_kind(LeftPar) {
                        let ident = ctx.stream.expect_ident()?;
                        ctx.stream.expect_kind(RightPar)?;
                        Some(ident)
                    } else {
                        None
                    }
                };
                let unit = InstantiatedUnit::Entity(name, arch.map(WithRef::new));
                ConcurrentStatement::Instance(parse_instantiation_statement(ctx, start_tok, unit)?)
            },
            For => ConcurrentStatement::ForGenerate(parse_for_generate_statement(ctx, label)?),
            If => ConcurrentStatement::IfGenerate(parse_if_generate_statement(ctx, label)?),
            Case => ConcurrentStatement::CaseGenerate(parse_case_generate_statement(ctx, label)?),
            Assert => ConcurrentStatement::Assert(parse_concurrent_assert_statement(ctx, false)?),
            Postponed => {
                let tok = ctx.stream.get_current_token_id();
                ctx.stream.skip();
                let token = ctx.stream.peek_expect()?;
                match token.kind {
                    Process => ConcurrentStatement::Process(parse_process_statement(ctx, label, Some(tok))?),
                    Assert => ConcurrentStatement::Assert(parse_concurrent_assert_statement(ctx, true)?),
                    With => ConcurrentStatement::Assignment(parse_selected_signal_assignment(ctx, true)?),
                    _ => {
                        let target = parse_name(ctx)?.map_into(Target::Name);
                        ctx.stream.expect_kind(SemiColon)?;
                        ConcurrentStatement::ProcedureCall(to_procedure_call(ctx, target, true)?)
                    }
                }
            },
            With => ConcurrentStatement::Assignment(parse_selected_signal_assignment(ctx, false)?),
            Identifier => {
                let name = parse_name(ctx)?;
                let token = ctx.stream.peek_expect()?;
                match token.kind {
                    Generic|Port => {
                        name.expect_selected(ctx)?;
                        let unit = InstantiatedUnit::Component(name);
                        ConcurrentStatement::Instance(parse_instantiation_statement(ctx, start_tok, unit)?)
                    }
                    _ => {
                        parse_assignment_or_procedure_call(ctx, name.map_into(Target::Name))?
                    }
                }
            },
            LtLt => {
                let name = parse_name(ctx)?;
                ctx.stream.expect_kind(LTE)?;
                parse_assignment_known_target(ctx, name.map_into(Target::Name))?
            },
            LeftPar => {
                let target = parse_aggregate(ctx)?.map_into(Target::Aggregate);
                parse_assignment_or_procedure_call(ctx, target)?
            }
        )
    };
    Ok(statement)
}

pub fn parse_labeled_concurrent_statements(
    ctx: &mut ParsingContext<'_>,
) -> ParseResult<Vec<LabeledConcurrentStatement>> {
    let mut statements = Vec::new();
    loop {
        let token = ctx.stream.peek_expect()?;
        match token.kind {
            End | Elsif | Else | When => {
                break Ok(statements);
            }
            _ => match parse_labeled_concurrent_statement(ctx) {
                Ok(stmt) => {
                    statements.push(stmt);
                }
                Err(diagnostic) => {
                    ctx.diagnostics.push(diagnostic);
                    ctx.stream.skip_until(|kind| {
                        matches!(kind, SemiColon | End | Process | Block | Assert)
                    })?;
                    ctx.stream.pop_if_kind(SemiColon);
                }
            },
        }
    }
}

pub fn parse_labeled_concurrent_statement(
    ctx: &mut ParsingContext<'_>,
) -> ParseResult<LabeledConcurrentStatement> {
    let start = ctx.stream.peek_expect()?;
    if ctx.stream.next_kind_is(Identifier) {
        let name = parse_name(ctx)?;
        if ctx.stream.skip_if_kind(Colon) {
            let label = Some(to_simple_name(ctx.stream, name)?);

            let start = ctx.stream.peek_expect()?;
            let statement = parse_concurrent_statement(ctx, label.as_ref())?;
            let end = ctx.stream.last().unwrap();

            Ok(LabeledConcurrentStatement {
                label: WithDecl::new(label),
                statement: WithPos::new(statement, start.pos.combine(&end.pos)),
            })
        } else {
            let target = name.map_into(Target::Name);
            let statement = parse_assignment_or_procedure_call(ctx, target)?;
            let end = ctx.stream.last().unwrap();

            Ok(LabeledConcurrentStatement {
                label: WithDecl::new(None),
                statement: WithPos::new(statement, start.pos.combine(&end.pos)),
            })
        }
    } else {
        let statement = parse_concurrent_statement(ctx, None)?;
        let end = ctx.stream.last().unwrap();

        Ok(LabeledConcurrentStatement {
            label: WithDecl::new(None),
            statement: WithPos::new(statement, start.pos.combine(&end.pos)),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analysis::tests::check_diagnostics;
    use crate::ast::{Alternative, AssertStatement, DelayMechanism, Selection};
    use crate::syntax::design_unit::parse_architecture_body;
    use crate::syntax::test::Code;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_concurrent_procedure() {
        let code = Code::new("foo(clk);");
        let call = ConcurrentProcedureCall {
            postponed: false,
            call: code.s1("foo(clk)").function_call(),
        };
        let stmt = code.with_stream_no_diagnostics(parse_labeled_concurrent_statement);
        assert_eq!(stmt.label.tree, None);
        assert_eq!(
            stmt.statement,
            WithPos::new(ConcurrentStatement::ProcedureCall(call), code.pos())
        );
    }

    #[test]
    fn test_postponed_concurrent_procedure() {
        let code = Code::new("postponed foo(clk);");
        let call = ConcurrentProcedureCall {
            postponed: true,
            call: code.s1("foo(clk)").function_call(),
        };
        let stmt = code.with_stream_no_diagnostics(parse_labeled_concurrent_statement);
        assert_eq!(stmt.label.tree, None);
        assert_eq!(
            stmt.statement,
            WithPos::new(ConcurrentStatement::ProcedureCall(call), code.pos())
        );
    }

    #[test]
    fn test_labeled_concurrent_procedure() {
        let code = Code::new("name: foo(clk);");
        let call = ConcurrentProcedureCall {
            postponed: false,
            call: code.s1("foo(clk)").function_call(),
        };
        let stmt = code.with_stream_no_diagnostics(parse_labeled_concurrent_statement);
        assert_eq!(stmt.label.tree, Some(code.s1("name").ident()));
        assert_eq!(
            stmt.statement,
            WithPos::new(
                ConcurrentStatement::ProcedureCall(call),
                code.s1("foo(clk);")
            )
        );
    }

    #[test]
    fn test_concurrent_procedure_no_args() {
        let code = Code::new("foo;");
        let call = ConcurrentProcedureCall {
            postponed: false,
            call: code.s1("foo").function_call(),
        };
        let stmt = code.with_stream_no_diagnostics(parse_labeled_concurrent_statement);
        assert_eq!(stmt.label.tree, None);
        assert_eq!(
            stmt.statement,
            WithPos::new(ConcurrentStatement::ProcedureCall(call), code.pos())
        );
    }

    #[test]
    fn test_block() {
        let code = Code::new(
            "\
name : block
  constant const : natural := 0;
begin
  name2: foo(clk);
end block;",
        );
        let call = ConcurrentProcedureCall {
            postponed: false,
            call: code.s1("foo(clk)").function_call(),
        };

        let block = BlockStatement {
            guard_condition: None,
            header: BlockHeader {
                generic_clause: None,
                generic_map: None,
                port_clause: None,
                port_map: None,
            },
            decl: code.s1("constant const : natural := 0;").declarative_part(),
            statements: vec![LabeledConcurrentStatement {
                label: Some(code.s1("name2").ident()).into(),
                statement: WithPos::new(
                    ConcurrentStatement::ProcedureCall(call),
                    code.s1("foo(clk);").pos(),
                ),
            }],
            end_label_pos: None,
            span: code.token_span().skip_to(code.s1("block").token()),
        };
        let stmt = code.with_stream_no_diagnostics(parse_labeled_concurrent_statement);
        assert_eq!(stmt.label.tree, Some(code.s1("name").ident()));
        assert_eq!(
            stmt.statement,
            WithPos::new(ConcurrentStatement::Block(block), code.pos_after("name : "))
        );
    }

    #[test]
    fn test_block_variant() {
        let code = Code::new(
            "\
name : block is
begin
end block name;",
        );
        let block = BlockStatement {
            guard_condition: None,
            header: BlockHeader {
                generic_clause: None,
                generic_map: None,
                port_clause: None,
                port_map: None,
            },
            decl: vec![],
            statements: vec![],
            end_label_pos: Some(code.s("name", 2).pos()),
            span: code.token_span().skip_to(code.s1("block").token()),
        };
        let stmt = code.with_stream_no_diagnostics(parse_labeled_concurrent_statement);
        assert_eq!(stmt.label.tree, Some(code.s1("name").ident()));
        assert_eq!(
            stmt.statement,
            WithPos::new(ConcurrentStatement::Block(block), code.pos_after("name : "))
        );
    }

    #[test]
    fn test_guarded_block() {
        let code = Code::new(
            "\
name : block (cond = true)
begin
end block;",
        );
        let block = BlockStatement {
            guard_condition: Some(code.s1("cond = true").expr()),
            header: BlockHeader {
                generic_clause: None,
                generic_map: None,
                port_clause: None,
                port_map: None,
            },
            decl: vec![],
            statements: vec![],
            end_label_pos: None,
            span: code.token_span().skip_to(code.s1("block").token()),
        };
        let stmt = code.with_stream_no_diagnostics(parse_labeled_concurrent_statement);
        assert_eq!(stmt.label.tree, Some(code.s1("name").ident()));
        assert_eq!(
            stmt.statement,
            WithPos::new(ConcurrentStatement::Block(block), code.pos_after("name : "))
        );
    }

    #[test]
    fn test_guarded_block_variant() {
        let code = Code::new(
            "\
name : block (cond = true) is
begin
end block;",
        );
        let block = BlockStatement {
            guard_condition: Some(code.s1("cond = true").expr()),
            header: BlockHeader {
                generic_clause: None,
                generic_map: None,
                port_clause: None,
                port_map: None,
            },
            decl: vec![],
            statements: vec![],
            end_label_pos: None,
            span: code.token_span().skip_to(code.s1("block").token()),
        };
        let stmt = code.with_stream_no_diagnostics(parse_labeled_concurrent_statement);
        assert_eq!(stmt.label.tree, Some(code.s1("name").ident()));
        assert_eq!(
            stmt.statement,
            WithPos::new(ConcurrentStatement::Block(block), code.pos_after("name : "))
        );
    }

    #[test]
    fn test_block_header() {
        let code = Code::new(
            "\
name: block is
  generic(gen: integer := 1);
  generic map(gen => 1);
  port(prt: integer := 1);
  port map(prt => 2);
begin
end block;",
        );
        let block = BlockStatement {
            guard_condition: None,
            header: BlockHeader {
                generic_clause: Some(vec![code.s1("gen: integer := 1").generic()]),
                generic_map: Some(code.s1("generic map(gen => 1)").generic_map_aspect()),
                port_clause: Some(vec![code.s1("prt: integer := 1").port()]),
                port_map: Some(code.s1("port map(prt => 2)").port_map_aspect()),
            },
            decl: vec![],
            statements: vec![],
            end_label_pos: None,
            span: code.token_span().skip_to(code.s1("block").token()),
        };
        let stmt = code.with_stream_no_diagnostics(parse_labeled_concurrent_statement);
        assert_eq!(stmt.label.tree, Some(code.s1("name").ident()));
        assert_eq!(
            stmt.statement,
            WithPos::new(ConcurrentStatement::Block(block), code.pos_after("name: "))
        );
    }

    #[test]
    fn test_process_statement() {
        let code = Code::new(
            "\
process
begin
end process;",
        );
        let process = ProcessStatement {
            postponed: false,
            sensitivity_list: None,
            decl: vec![],
            statements: vec![],
            end_label_pos: None,
            span: code.token_span(),
        };
        let stmt = code.with_stream_no_diagnostics(parse_labeled_concurrent_statement);
        assert_eq!(stmt.label.tree, None);
        assert_eq!(
            stmt.statement,
            WithPos::new(ConcurrentStatement::Process(process), code.pos())
        );
    }

    #[test]
    fn test_process_statement_variant() {
        let code = Code::new(
            "\
name : process is
begin
end process name;",
        );
        let process = ProcessStatement {
            postponed: false,
            sensitivity_list: None,
            decl: vec![],
            statements: vec![],
            end_label_pos: Some(code.s("name", 2).pos()),
            span: code.token_span().skip_to(code.s1("process").token()),
        };
        let stmt = code.with_stream_no_diagnostics(parse_labeled_concurrent_statement);
        assert_eq!(stmt.label.tree, Some(code.s1("name").ident()));
        assert_eq!(
            stmt.statement,
            WithPos::new(
                ConcurrentStatement::Process(process),
                code.pos_after("name : ")
            )
        );
    }

    #[test]
    fn test_postponed_process_statement() {
        let code = Code::new(
            "\
postponed process
begin
end process;",
        );
        let process = ProcessStatement {
            postponed: true,
            sensitivity_list: None,
            decl: vec![],
            statements: vec![],
            end_label_pos: None,
            span: code.token_span(),
        };
        let stmt = code.with_stream_no_diagnostics(parse_labeled_concurrent_statement);
        assert_eq!(stmt.label.tree, None);
        assert_eq!(
            stmt.statement,
            WithPos::new(ConcurrentStatement::Process(process), code.pos())
        );
    }

    #[test]
    fn test_postponed_process_statement_end_postponed() {
        let code = Code::new(
            "\
postponed process
begin
end postponed process;",
        );
        let process = ProcessStatement {
            postponed: true,
            sensitivity_list: None,
            decl: vec![],
            statements: vec![],
            end_label_pos: None,
            span: code.token_span(),
        };
        let stmt = code.with_stream_no_diagnostics(parse_labeled_concurrent_statement);
        assert_eq!(stmt.label.tree, None);
        assert_eq!(
            stmt.statement,
            WithPos::new(ConcurrentStatement::Process(process), code.pos())
        );
    }

    #[test]
    fn test_process_statement_end_postponed() {
        let code = Code::new(
            "\
process is
begin
end postponed process;",
        );
        let (stmt, diagnostics) = code.with_stream_diagnostics(parse_labeled_concurrent_statement);
        let process = ProcessStatement {
            postponed: false,
            sensitivity_list: None,
            decl: Vec::new(),
            statements: Vec::new(),
            end_label_pos: None,
            span: code.token_span(),
        };
        assert_eq!(
            diagnostics,
            vec![Diagnostic::syntax_error(
                code.s1("postponed"),
                "'postponed' at the end of non-postponed process."
            )]
        );
        assert_eq!(
            stmt.statement,
            WithPos::new(ConcurrentStatement::Process(process), code.pos())
        );
    }

    #[test]
    fn test_process_statement_sensitivity() {
        let code = Code::new(
            "\
process (clk, vec(1)) is
begin
end process;",
        );
        let process = ProcessStatement {
            postponed: false,
            sensitivity_list: Some(SensitivityList::Names(vec![
                code.s1("clk").name(),
                code.s1("vec(1)").name(),
            ])),
            decl: vec![],
            statements: vec![],
            end_label_pos: None,
            span: code.token_span(),
        };
        let stmt = code.with_stream_no_diagnostics(parse_labeled_concurrent_statement);
        assert_eq!(stmt.label.tree, None);
        assert_eq!(
            stmt.statement,
            WithPos::new(ConcurrentStatement::Process(process), code.pos())
        );
    }

    #[test]
    fn test_process_empty_sensitivity() {
        let code = Code::new(
            "\
process () is
begin
end process;",
        );
        let (stmt, diagnostics) = code.with_stream_diagnostics(parse_labeled_concurrent_statement);
        let process = ProcessStatement {
            postponed: false,
            sensitivity_list: Some(SensitivityList::Names(Vec::new())),
            decl: Vec::new(),
            statements: Vec::new(),
            end_label_pos: None,
            span: code.token_span(),
        };
        assert_eq!(
            diagnostics,
            vec![Diagnostic::syntax_error(
                code.s1(")"),
                "Processes with sensitivity lists must contain at least one element."
            )]
        );
        assert_eq!(
            stmt.statement,
            WithPos::new(ConcurrentStatement::Process(process), code.pos())
        );
    }

    #[test]
    fn test_process_statement_full() {
        let code = Code::new(
            "\
process (all) is
  variable foo : boolean;
begin
  foo <= true;
  wait;
end process;",
        );
        let process = ProcessStatement {
            postponed: false,
            sensitivity_list: Some(SensitivityList::All),
            decl: code.s1("variable foo : boolean;").declarative_part(),
            statements: vec![
                code.s1("foo <= true;").sequential_statement(),
                code.s1("wait;").sequential_statement(),
            ],
            end_label_pos: None,
            span: code.token_span(),
        };
        let stmt = code.with_stream_no_diagnostics(parse_labeled_concurrent_statement);
        assert_eq!(stmt.label.tree, None);
        assert_eq!(
            stmt.statement,
            WithPos::new(ConcurrentStatement::Process(process), code.pos())
        );
    }

    #[test]
    fn test_concurrent_assert() {
        let code = Code::new("assert cond = true;");
        let assert = ConcurrentAssertStatement {
            postponed: false,
            statement: AssertStatement {
                condition: code.s1("cond = true").expr(),
                report: None,
                severity: None,
            },
        };
        let stmt = code.with_stream_no_diagnostics(parse_labeled_concurrent_statement);
        assert_eq!(stmt.label.tree, None);
        assert_eq!(
            stmt.statement,
            WithPos::new(ConcurrentStatement::Assert(assert), code.pos())
        );
    }

    #[test]
    fn test_postponed_concurrent_assert() {
        let code = Code::new("postponed assert cond = true;");
        let assert = ConcurrentAssertStatement {
            postponed: true,
            statement: AssertStatement {
                condition: code.s1("cond = true").expr(),
                report: None,
                severity: None,
            },
        };
        let stmt = code.with_stream_no_diagnostics(parse_labeled_concurrent_statement);
        assert_eq!(stmt.label.tree, None);
        assert_eq!(
            stmt.statement,
            WithPos::new(ConcurrentStatement::Assert(assert), code.pos())
        );
    }

    #[test]
    fn test_concurrent_signal_assignment() {
        let code = Code::new("foo <= bar(2 to 3);");
        let assign = ConcurrentSignalAssignment {
            postponed: false,
            guarded: false,
            target: code.s1("foo").name().map_into(Target::Name),
            delay_mechanism: None,
            rhs: AssignmentRightHand::Simple(code.s1("bar(2 to 3)").waveform()),
        };
        let stmt = code.with_stream_no_diagnostics(parse_labeled_concurrent_statement);
        assert_eq!(stmt.label.tree, None);
        assert_eq!(
            stmt.statement,
            WithPos::new(ConcurrentStatement::Assignment(assign), code.pos())
        );
    }

    #[test]
    fn test_concurrent_signal_assignment_external_name() {
        let code = Code::new("<< signal dut.foo : std_logic >> <= bar(2 to 3);");
        let assign = ConcurrentSignalAssignment {
            postponed: false,
            guarded: false,
            target: code
                .s1("<< signal dut.foo : std_logic >>")
                .name()
                .map_into(Target::Name),
            delay_mechanism: None,
            rhs: AssignmentRightHand::Simple(code.s1("bar(2 to 3)").waveform()),
        };
        let stmt = code.with_stream_no_diagnostics(parse_labeled_concurrent_statement);
        assert_eq!(stmt.label.tree, None);
        assert_eq!(
            stmt.statement,
            WithPos::new(ConcurrentStatement::Assignment(assign), code.pos())
        );
    }

    #[test]
    fn parse_selected_signal_assignment() {
        let code = Code::new(
            "\
with x(0) + 1 select
   foo(0) <= transport bar(1,2) after 2 ns when 0|1;",
        );

        let selection = Selection {
            expression: code.s1("x(0) + 1").expr(),
            alternatives: vec![Alternative {
                choices: code.s1("0|1").choices(),
                item: code.s1("bar(1,2) after 2 ns").waveform(),
            }],
        };

        let stmt = code.with_stream_no_diagnostics(parse_labeled_concurrent_statement);
        assert_eq!(stmt.label.tree, None);
        assert_eq!(
            stmt.statement.item,
            ConcurrentStatement::Assignment(ConcurrentSignalAssignment {
                postponed: false,
                guarded: false,
                target: code.s1("foo(0)").name().map_into(Target::Name),
                delay_mechanism: Some(DelayMechanism::Transport),
                rhs: AssignmentRightHand::Selected(selection)
            })
        );
        assert_eq!(stmt.statement.pos, code.pos());
    }

    #[test]
    fn test_component_instantiation() {
        let code = Code::new("inst: component lib.foo.bar;");

        let inst = InstantiationStatement {
            unit: InstantiatedUnit::Component(code.s1("lib.foo.bar").name()),
            generic_map: None,
            port_map: None,
            span: code.token_span().skip_to(code.s1("component").token()),
        };
        let stmt = code.with_stream_no_diagnostics(parse_labeled_concurrent_statement);
        assert_eq!(stmt.label.tree, Some(code.s1("inst").ident()));
        assert_eq!(
            stmt.statement,
            WithPos::new(
                ConcurrentStatement::Instance(inst),
                code.pos_after("inst: ")
            )
        );
    }

    #[test]
    fn test_configuration_instantiation() {
        let code = Code::new("inst: configuration lib.foo.bar;");

        let inst = InstantiationStatement {
            unit: InstantiatedUnit::Configuration(code.s1("lib.foo.bar").name()),
            generic_map: None,
            port_map: None,
            span: code.token_span().skip_to(code.s1("configuration").token()),
        };
        let stmt = code.with_stream_no_diagnostics(parse_labeled_concurrent_statement);
        assert_eq!(stmt.label.tree, Some(code.s1("inst").ident()));
        assert_eq!(
            stmt.statement,
            WithPos::new(
                ConcurrentStatement::Instance(inst),
                code.pos_after("inst: ")
            )
        );
    }

    #[test]
    fn test_entity_instantiation() {
        let code = Code::new("inst: entity lib.foo.bar;");

        let inst = InstantiationStatement {
            unit: InstantiatedUnit::Entity(code.s1("lib.foo.bar").name(), None),
            generic_map: None,
            port_map: None,
            span: code.token_span().skip_to(code.s1("entity").token()),
        };
        let stmt = code.with_stream_no_diagnostics(parse_labeled_concurrent_statement);
        assert_eq!(stmt.label.tree, Some(code.s1("inst").ident()));
        assert_eq!(
            stmt.statement,
            WithPos::new(
                ConcurrentStatement::Instance(inst),
                code.pos_after("inst: ")
            )
        );
    }

    #[test]
    fn test_entity_architecture_instantiation() {
        let code = Code::new("inst: entity lib.foo.bar(arch);");

        let inst = InstantiationStatement {
            unit: InstantiatedUnit::Entity(
                code.s1("lib.foo.bar").name(),
                Some(WithRef::new(code.s1("arch").ident())),
            ),
            generic_map: None,
            port_map: None,
            span: code.token_span().skip_to(code.s1("entity").token()),
        };
        let stmt = code.with_stream_no_diagnostics(parse_labeled_concurrent_statement);
        assert_eq!(stmt.label.tree, Some(code.s1("inst").ident()));
        assert_eq!(
            stmt.statement,
            WithPos::new(
                ConcurrentStatement::Instance(inst),
                code.pos_after("inst: ")
            )
        );
    }

    #[test]
    fn test_component_aspect_maps() {
        let code = Code::new(
            "\
inst: component lib.foo.bar
  generic map (
   const => 1
  )
  port map (
   clk => clk_foo
  );",
        );

        let inst = InstantiationStatement {
            unit: InstantiatedUnit::Component(code.s1("lib.foo.bar").name()),
            generic_map: Some(
                code.s1("generic map (
   const => 1
  )")
                    .generic_map_aspect(),
            ),
            port_map: Some(
                code.s1("port map (
   clk => clk_foo
  )")
                    .port_map_aspect(),
            ),
            span: code.token_span().skip_to(code.s1("component").token()),
        };
        let stmt = code.with_stream_no_diagnostics(parse_labeled_concurrent_statement);
        assert_eq!(stmt.label.tree, Some(code.s1("inst").ident()));
        assert_eq!(
            stmt.statement,
            WithPos::new(
                ConcurrentStatement::Instance(inst),
                code.pos_after("inst: ")
            )
        );
    }

    #[test]
    fn test_component_no_keyword_port_aspect_map() {
        let code = Code::new(
            "\
inst: lib.foo.bar
  port map (
   clk => clk_foo
  );",
        );

        let inst = InstantiationStatement {
            unit: InstantiatedUnit::Component(code.s1("lib.foo.bar").name()),
            generic_map: None,
            port_map: Some(
                code.s1("port map (
   clk => clk_foo
  )")
                    .port_map_aspect(),
            ),
            span: code.token_span().skip_to(code.s1("lib").token()),
        };
        let stmt = code.with_stream_no_diagnostics(parse_labeled_concurrent_statement);
        assert_eq!(stmt.label.tree, Some(code.s1("inst").ident()));
        assert_eq!(
            stmt.statement,
            WithPos::new(
                ConcurrentStatement::Instance(inst),
                code.pos_after("inst: ")
            )
        );
    }

    #[test]
    fn test_component_no_keyword_generic_aspect_map() {
        let code = Code::new(
            "\
inst: lib.foo.bar
  generic map (
   const => 1
  );",
        );

        let inst = InstantiationStatement {
            unit: InstantiatedUnit::Component(code.s1("lib.foo.bar").name()),
            generic_map: Some(
                code.s1("generic map (
   const => 1
  )")
                    .generic_map_aspect(),
            ),
            port_map: None,
            span: code.token_span().skip_to(code.s1("lib").token()),
        };
        let stmt = code.with_stream_no_diagnostics(parse_labeled_concurrent_statement);
        assert_eq!(stmt.label.tree, Some(code.s1("inst").ident()));
        assert_eq!(
            stmt.statement,
            WithPos::new(
                ConcurrentStatement::Instance(inst),
                code.pos_after("inst: ")
            )
        );
    }

    #[test]
    fn test_for_generate_empty() {
        let code = Code::new(
            "\
gen: for idx in 0 to 1 generate
end generate;",
        );
        let gen = ForGenerateStatement {
            index_name: code.s1("idx").decl_ident(),
            discrete_range: code.s1("0 to 1").discrete_range(),
            body: GenerateBody {
                alternative_label: None,
                decl: None,
                statements: vec![],
                end_label_pos: None,
            },
            end_label_pos: None,
            span: code.token_span().skip_to(code.s1("for").token()),
        };
        let stmt = code.with_stream_no_diagnostics(parse_labeled_concurrent_statement);
        assert_eq!(stmt.label.tree, Some(code.s1("gen").ident()));
        assert_eq!(
            stmt.statement,
            WithPos::new(
                ConcurrentStatement::ForGenerate(gen),
                code.pos_after("gen: ")
            )
        );
    }

    #[test]
    fn test_for_generate() {
        let code = Code::new(
            "\
gen: for idx in 0 to 1 generate
  foo <= bar;
end generate;",
        );
        let gen = ForGenerateStatement {
            index_name: code.s1("idx").decl_ident(),
            discrete_range: code.s1("0 to 1").discrete_range(),
            body: GenerateBody {
                alternative_label: None,
                decl: None,
                statements: vec![code.s1("foo <= bar;").concurrent_statement()],
                end_label_pos: None,
            },
            end_label_pos: None,
            span: code.token_span().skip_to(code.s1("for").token()),
        };
        let stmt = code.with_stream_no_diagnostics(parse_labeled_concurrent_statement);
        assert_eq!(stmt.label.tree, Some(code.s1("gen").ident()));
        assert_eq!(
            stmt.statement,
            WithPos::new(
                ConcurrentStatement::ForGenerate(gen),
                code.pos_after("gen: ")
            )
        );
    }

    #[test]
    fn test_for_generate_empty_declarations() {
        fn test(decl: Option<Vec<Declaration>>, code: Code) {
            let gen = ForGenerateStatement {
                index_name: code.s1("idx").decl_ident(),
                discrete_range: code.s1("0 to 1").discrete_range(),
                body: GenerateBody {
                    alternative_label: None,
                    decl,
                    statements: vec![code.s1("foo <= bar;").concurrent_statement()],
                    end_label_pos: None,
                },
                end_label_pos: None,
                span: code.token_span().skip_to(code.s1("for").token()),
            };
            let stmt = code.with_stream_no_diagnostics(parse_labeled_concurrent_statement);
            assert_eq!(stmt.label.tree, Some(code.s1("gen").ident()));
            assert_eq!(
                stmt.statement,
                WithPos::new(
                    ConcurrentStatement::ForGenerate(gen),
                    code.pos_after("gen: ")
                )
            );
        }
        test(
            Some(vec![]),
            Code::new(
                "\
gen: for idx in 0 to 1 generate
begin
  foo <= bar;
end generate;",
            ),
        );

        test(
            None,
            Code::new(
                "\
gen: for idx in 0 to 1 generate
  foo <= bar;
end generate;",
            ),
        );

        test(
            Some(vec![]),
            Code::new(
                "\
gen: for idx in 0 to 1 generate
begin
  foo <= bar;
end;
end generate;",
            ),
        );
    }

    #[test]
    fn test_for_generate_declarations() {
        fn test(code: Code) {
            let gen = ForGenerateStatement {
                index_name: code.s1("idx").decl_ident(),
                discrete_range: code.s1("0 to 1").discrete_range(),
                body: GenerateBody {
                    alternative_label: None,
                    decl: Some(code.s1("signal foo : natural;").declarative_part()),
                    statements: vec![code.s1("foo <= bar;").concurrent_statement()],
                    end_label_pos: None,
                },
                end_label_pos: None,
                span: code.token_span().skip_to(code.s1("for").token()),
            };
            let stmt = code.with_stream_no_diagnostics(parse_labeled_concurrent_statement);
            assert_eq!(stmt.label.tree, Some(code.s1("gen").ident()));
            assert_eq!(
                stmt.statement,
                WithPos::new(
                    ConcurrentStatement::ForGenerate(gen),
                    code.pos_after("gen: ")
                )
            );
        }

        test(Code::new(
            "\
gen: for idx in 0 to 1 generate
  signal foo : natural;
begin
  foo <= bar;
end generate;",
        ));

        test(Code::new(
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
    fn test_if_generate_empty() {
        let code = Code::new(
            "\
gen: if cond = true generate
end generate;",
        );
        let gen = IfGenerateStatement {
            conds: Conditionals {
                conditionals: vec![Conditional {
                    condition: code.s1("cond = true").expr(),
                    item: GenerateBody {
                        alternative_label: None,
                        decl: None,
                        statements: vec![],
                        end_label_pos: None,
                    },
                }],
                else_item: None,
            },
            end_label_pos: None,
            span: code.token_span().skip_to(code.s1("if").token()),
        };
        let stmt = code.with_stream_no_diagnostics(parse_labeled_concurrent_statement);
        assert_eq!(stmt.label.tree, Some(code.s1("gen").ident()));
        assert_eq!(
            stmt.statement,
            WithPos::new(
                ConcurrentStatement::IfGenerate(gen),
                code.pos_after("gen: ")
            )
        );
    }

    #[test]
    fn test_if_generate_declarative_region() {
        let code = Code::new(
            "\
gen: if cond = true generate
begin
end generate;",
        );
        let gen = IfGenerateStatement {
            conds: Conditionals {
                conditionals: vec![Conditional {
                    condition: code.s1("cond = true").expr(),
                    item: GenerateBody {
                        alternative_label: None,
                        decl: Some(vec![]),
                        statements: vec![],
                        end_label_pos: None,
                    },
                }],
                else_item: None,
            },
            end_label_pos: None,
            span: code.token_span().skip_to(code.s1("if").token()),
        };
        let stmt = code.with_stream_no_diagnostics(parse_labeled_concurrent_statement);
        assert_eq!(stmt.label.tree, Some(code.s1("gen").ident()));
        assert_eq!(
            stmt.statement,
            WithPos::new(
                ConcurrentStatement::IfGenerate(gen),
                code.pos_after("gen: ")
            )
        );
    }

    #[test]
    fn test_if_elseif_else_generate_empty() {
        let code = Code::new(
            "\
gen: if cond = true generate
elsif cond2 = true generate
else generate
end generate;",
        );
        let gen = IfGenerateStatement {
            conds: Conditionals {
                conditionals: vec![
                    Conditional {
                        condition: code.s1("cond = true").expr(),
                        item: GenerateBody {
                            alternative_label: None,
                            decl: None,
                            statements: vec![],
                            end_label_pos: None,
                        },
                    },
                    Conditional {
                        condition: code.s1("cond2 = true").expr(),
                        item: GenerateBody {
                            alternative_label: None,
                            decl: None,
                            statements: vec![],
                            end_label_pos: None,
                        },
                    },
                ],
                else_item: Some(GenerateBody {
                    alternative_label: None,
                    decl: None,
                    statements: vec![],
                    end_label_pos: None,
                }),
            },
            end_label_pos: None,
            span: code.token_span().skip_to(code.s1("if").token()),
        };
        let stmt = code.with_stream_no_diagnostics(parse_labeled_concurrent_statement);
        assert_eq!(stmt.label.tree, Some(code.s1("gen").ident()));
        assert_eq!(
            stmt.statement,
            WithPos::new(
                ConcurrentStatement::IfGenerate(gen),
                code.pos_after("gen: ")
            )
        );
    }
    #[test]
    fn test_if_elseif_else_generate() {
        let code = Code::new(
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
        );
        let gen = IfGenerateStatement {
            conds: Conditionals {
                conditionals: vec![
                    Conditional {
                        condition: code.s1("cond = true").expr(),
                        item: GenerateBody {
                            alternative_label: None,
                            decl: Some(code.s1("variable v1 : boolean;").declarative_part()),
                            statements: vec![code.s1("foo1(clk);").concurrent_statement()],
                            end_label_pos: None,
                        },
                    },
                    Conditional {
                        condition: code.s1("cond2 = true").expr(),
                        item: GenerateBody {
                            alternative_label: None,
                            decl: Some(code.s1("variable v2 : boolean;").declarative_part()),
                            statements: vec![code.s1("foo2(clk);").concurrent_statement()],
                            end_label_pos: None,
                        },
                    },
                ],
                else_item: Some(GenerateBody {
                    alternative_label: None,
                    decl: Some(code.s1("variable v3 : boolean;").declarative_part()),
                    statements: vec![code.s1("foo3(clk);").concurrent_statement()],
                    end_label_pos: None,
                }),
            },
            end_label_pos: None,
            span: code.token_span().skip_to(code.s1("if").token()),
        };
        let stmt = code.with_stream_no_diagnostics(parse_labeled_concurrent_statement);
        assert_eq!(stmt.label.tree, Some(code.s1("gen").ident()));
        assert_eq!(
            stmt.statement,
            WithPos::new(
                ConcurrentStatement::IfGenerate(gen),
                code.pos_after("gen: ")
            )
        );
    }

    #[test]
    fn test_if_elseif_else_generate_alternative_label() {
        let code = Code::new(
            "\
gen: if alt1: cond = true generate
elsif cond2 = true generate
end alt2;
else alt3: generate
end alt4;
end generate;",
        );
        let gen = IfGenerateStatement {
            conds: Conditionals {
                conditionals: vec![
                    Conditional {
                        condition: code.s1("cond = true").expr(),
                        item: GenerateBody {
                            alternative_label: Some(code.s1("alt1").decl_ident()),
                            decl: None,
                            statements: vec![],
                            end_label_pos: None,
                        },
                    },
                    Conditional {
                        condition: code.s1("cond2 = true").expr(),
                        item: GenerateBody {
                            alternative_label: None,
                            decl: None,
                            statements: vec![],
                            end_label_pos: None,
                        },
                    },
                ],
                else_item: Some(GenerateBody {
                    alternative_label: Some(code.s1("alt3").decl_ident()),
                    decl: None,
                    statements: vec![],
                    end_label_pos: None,
                }),
            },
            end_label_pos: None,
            span: code.token_span().skip_to(code.s1("if").token()),
        };
        let (stmt, diagnostics) = code.with_stream_diagnostics(parse_labeled_concurrent_statement);
        assert_eq!(stmt.label.tree, Some(code.s1("gen").ident()));
        assert_eq!(
            stmt.statement,
            WithPos::new(
                ConcurrentStatement::IfGenerate(gen),
                code.pos_after("gen: ")
            )
        );
        assert_eq!(
            diagnostics,
            vec![
                Diagnostic::syntax_error(
                    code.s1("alt2"),
                    "End label 'alt2' found for unlabeled statement"
                ),
                Diagnostic::syntax_error(code.s1("alt4"), "End label mismatch, expected alt3")
            ]
        );
    }
    #[test]
    fn test_if_elseif_else_generate_inner_end() {
        let code = Code::new(
            "\
gen: if alt1: cond = true generate
end alt1;
elsif alt2: cond2 = true generate
end alt2;
else alt3: generate
end alt3;
end generate;",
        );
        let gen = IfGenerateStatement {
            conds: Conditionals {
                conditionals: vec![
                    Conditional {
                        condition: code.s1("cond = true").expr(),
                        item: GenerateBody {
                            alternative_label: Some(code.s1("alt1").decl_ident()),
                            decl: None,
                            statements: vec![],
                            end_label_pos: Some(code.s("alt1", 2).pos()),
                        },
                    },
                    Conditional {
                        condition: code.s1("cond2 = true").expr(),
                        item: GenerateBody {
                            alternative_label: Some(code.s1("alt2").decl_ident()),
                            decl: None,
                            statements: vec![],
                            end_label_pos: Some(code.s("alt2", 2).pos()),
                        },
                    },
                ],
                else_item: Some(GenerateBody {
                    alternative_label: Some(code.s1("alt3").decl_ident()),
                    decl: None,
                    statements: vec![],
                    end_label_pos: Some(code.s("alt3", 2).pos()),
                }),
            },
            end_label_pos: None,
            span: code.token_span().skip_to(code.s1("if").token()),
        };
        let stmt = code.with_stream_no_diagnostics(parse_labeled_concurrent_statement);
        assert_eq!(stmt.label.tree, Some(code.s1("gen").ident()));
        assert_eq!(
            stmt.statement,
            WithPos::new(
                ConcurrentStatement::IfGenerate(gen),
                code.pos_after("gen: ")
            )
        );
    }

    #[test]
    fn test_case_generate() {
        let code = Code::new(
            "\
gen: case expr(0) + 2 generate
  when 1 | 2 =>
    sig <= value;
  when others =>
    foo(clk);
end generate;",
        );
        let gen = CaseGenerateStatement {
            sels: Selection {
                expression: code.s1("expr(0) + 2").expr(),
                alternatives: vec![
                    Alternative {
                        choices: code.s1("1 | 2").choices(),
                        item: GenerateBody {
                            alternative_label: None,
                            decl: None,
                            statements: vec![code.s1("sig <= value;").concurrent_statement()],
                            end_label_pos: None,
                        },
                    },
                    Alternative {
                        choices: code.s1("others").choices(),
                        item: GenerateBody {
                            alternative_label: None,
                            decl: None,
                            statements: vec![code.s1("foo(clk);").concurrent_statement()],
                            end_label_pos: None,
                        },
                    },
                ],
            },
            end_label_pos: None,
            span: code.token_span().skip_to(code.s1("case").token()),
        };
        let stmt = code.with_stream_no_diagnostics(parse_labeled_concurrent_statement);
        assert_eq!(stmt.label.tree, Some(code.s1("gen").ident()));
        assert_eq!(
            stmt.statement,
            WithPos::new(
                ConcurrentStatement::CaseGenerate(gen),
                code.pos_after("gen: ")
            )
        );
    }

    #[test]
    fn test_case_alternative_label() {
        let code = Code::new(
            "\
gen1: case expr(0) + 2 generate
  when alt1: 1 | 2 =>
    sig <= value;
  when alt2: others =>
    foo(clk);
end generate gen1;",
        );
        let gen = CaseGenerateStatement {
            sels: Selection {
                expression: code.s1("expr(0) + 2").expr(),
                alternatives: vec![
                    Alternative {
                        choices: code.s1("1 | 2").choices(),
                        item: GenerateBody {
                            alternative_label: Some(code.s1("alt1").decl_ident()),
                            decl: None,
                            statements: vec![code.s1("sig <= value;").concurrent_statement()],
                            end_label_pos: None,
                        },
                    },
                    Alternative {
                        choices: code.s1("others").choices(),
                        item: GenerateBody {
                            alternative_label: Some(code.s1("alt2").decl_ident()),
                            decl: None,
                            statements: vec![code.s1("foo(clk);").concurrent_statement()],
                            end_label_pos: None,
                        },
                    },
                ],
            },
            end_label_pos: Some(code.s("gen1", 2).pos()),
            span: code.token_span().skip_to(code.s1("case").token()),
        };
        let stmt = code.with_stream_no_diagnostics(parse_labeled_concurrent_statement);
        assert_eq!(stmt.label.tree, Some(code.s1("gen1").ident()));
        assert_eq!(
            stmt.statement,
            WithPos::new(
                ConcurrentStatement::CaseGenerate(gen),
                code.pos_after("gen1: ")
            )
        );
    }

    #[test]
    fn parsing_continues_when_a_statement_is_incorrect() {
        let code = Code::new(
            "\
architecture arch of ent is
begin
foo
b <= c;
bar
end arch;
        ",
        );
        let (_, diag) = code.with_stream_diagnostics(parse_architecture_body);
        check_diagnostics(
            diag,
            vec![
                Diagnostic::syntax_error(code.s1("foo").pos().end_pos(), "Expected '<=' or ';'"),
                Diagnostic::syntax_error(code.s1("bar").pos().end_pos(), "Expected '<=' or ';'"),
            ],
        )
    }
}
