// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

// These fields are better explicit than .. since we are forced to consider if new fields should be searched
#![allow(clippy::unneeded_field_pattern)]

use super::*;
use crate::ast::*;
use crate::data::*;
use analyze::*;
use region::*;
use target::AssignmentType;

impl<'a> AnalyzeContext<'a> {
    fn analyze_sequential_statement(
        &self,
        scope: &Scope<'a>,
        statement: &mut LabeledSequentialStatement,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        if let Some(ref mut label) = statement.label {
            scope.add(self.arena.define(label, AnyEntKind::Label), diagnostics);
        }

        match statement.statement {
            SequentialStatement::Return(ref mut ret) => {
                let ReturnStatement { expression } = ret;
                if let Some(ref mut expression) = expression {
                    self.analyze_expression(scope, expression, diagnostics)?;
                }
            }
            SequentialStatement::Wait(ref mut wait_stmt) => {
                let WaitStatement {
                    sensitivity_clause,
                    condition_clause,
                    timeout_clause,
                } = wait_stmt;
                self.sensitivity_list_check(scope, sensitivity_clause, diagnostics)?;
                if let Some(expr) = condition_clause {
                    self.analyze_expression(scope, expr, diagnostics)?;
                }
                if let Some(expr) = timeout_clause {
                    self.analyze_expression(scope, expr, diagnostics)?;
                }
            }
            SequentialStatement::Assert(ref mut assert_stmt) => {
                let AssertStatement {
                    condition,
                    report,
                    severity,
                } = assert_stmt;
                self.analyze_expression(scope, condition, diagnostics)?;
                if let Some(expr) = report {
                    self.analyze_expression(scope, expr, diagnostics)?;
                }
                if let Some(expr) = severity {
                    self.analyze_expression(scope, expr, diagnostics)?;
                }
            }
            SequentialStatement::Report(ref mut report_stmt) => {
                let ReportStatement { report, severity } = report_stmt;
                self.analyze_expression(scope, report, diagnostics)?;
                if let Some(expr) = severity {
                    self.analyze_expression(scope, expr, diagnostics)?;
                }
            }
            SequentialStatement::Exit(ref mut exit_stmt) => {
                let ExitStatement {
                    condition,
                    // @TODO loop label
                    ..
                } = exit_stmt;

                if let Some(expr) = condition {
                    self.analyze_expression(scope, expr, diagnostics)?;
                }
            }
            SequentialStatement::Next(ref mut next_stmt) => {
                let NextStatement {
                    condition,
                    // @TODO loop label
                    ..
                } = next_stmt;

                if let Some(expr) = condition {
                    self.analyze_expression(scope, expr, diagnostics)?;
                }
            }
            SequentialStatement::If(ref mut ifstmt) => {
                let IfStatement {
                    conditionals,
                    else_item,
                } = ifstmt;

                // @TODO write generic function for this
                for conditional in conditionals {
                    let Conditional { condition, item } = conditional;
                    self.analyze_sequential_part(scope, item, diagnostics)?;
                    self.analyze_expression(scope, condition, diagnostics)?;
                }
                if let Some(else_item) = else_item {
                    self.analyze_sequential_part(scope, else_item, diagnostics)?;
                }
            }
            SequentialStatement::Case(ref mut case_stmt) => {
                let CaseStatement {
                    is_matching: _,
                    expression,
                    alternatives,
                } = case_stmt;
                self.analyze_expression(scope, expression, diagnostics)?;
                for alternative in alternatives.iter_mut() {
                    let Alternative { choices, item } = alternative;
                    self.analyze_choices(scope, choices, diagnostics)?;
                    self.analyze_sequential_part(scope, item, diagnostics)?;
                }
            }
            SequentialStatement::Loop(ref mut loop_stmt) => {
                let LoopStatement {
                    iteration_scheme,
                    statements,
                } = loop_stmt;
                match iteration_scheme {
                    Some(IterationScheme::For(ref mut index, ref mut drange)) => {
                        let typ = as_fatal(self.discrete_range_type(scope, drange, diagnostics))?;
                        let region = scope.nested();
                        region.add(
                            self.arena.define(index, AnyEntKind::LoopParameter(typ)),
                            diagnostics,
                        );
                        self.analyze_sequential_part(&region, statements, diagnostics)?;
                    }
                    Some(IterationScheme::While(ref mut expr)) => {
                        self.analyze_expression(scope, expr, diagnostics)?;
                        self.analyze_sequential_part(scope, statements, diagnostics)?;
                    }
                    None => {
                        self.analyze_sequential_part(scope, statements, diagnostics)?;
                    }
                }
            }
            SequentialStatement::ProcedureCall(ref mut pcall) => {
                self.analyze_procedure_call(scope, pcall, diagnostics)?;
            }
            SequentialStatement::SignalAssignment(ref mut assign) => {
                // @TODO more
                let SignalAssignment { target, rhs, .. } = assign;
                self.analyze_waveform_assignment(
                    scope,
                    target,
                    AssignmentType::Signal,
                    rhs,
                    diagnostics,
                )?;
            }
            SequentialStatement::VariableAssignment(ref mut assign) => {
                let VariableAssignment { target, rhs } = assign;
                self.analyze_expr_assignment(
                    scope,
                    target,
                    AssignmentType::Variable,
                    rhs,
                    diagnostics,
                )?;
            }
            SequentialStatement::SignalForceAssignment(ref mut assign) => {
                let SignalForceAssignment {
                    target,
                    force_mode: _,
                    rhs,
                } = assign;
                self.analyze_expr_assignment(
                    scope,
                    target,
                    AssignmentType::Signal,
                    rhs,
                    diagnostics,
                )?;
            }
            SequentialStatement::SignalReleaseAssignment(ref mut assign) => {
                let SignalReleaseAssignment {
                    target,
                    force_mode: _,
                } = assign;
                as_fatal(self.resolve_target(scope, target, AssignmentType::Signal, diagnostics))?;
            }
            SequentialStatement::Null => {}
        }
        Ok(())
    }

    pub fn analyze_sequential_part(
        &self,
        scope: &Scope<'a>,
        statements: &mut [LabeledSequentialStatement],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        for statement in statements.iter_mut() {
            self.analyze_sequential_statement(scope, statement, diagnostics)?;
        }

        Ok(())
    }
}
