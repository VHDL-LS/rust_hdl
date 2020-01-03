// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

use super::*;
use crate::ast::*;
use crate::data::*;
use analyze::*;
use region::*;

impl<'a> AnalyzeContext<'a> {
    fn analyze_sequential_statement(
        &self,
        parent: &mut Region<'_>,
        statement: &mut LabeledSequentialStatement,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        if let Some(ref label) = statement.label {
            parent.add(label.clone(), AnyDeclaration::Constant, diagnostics);
        }

        match statement.statement {
            SequentialStatement::Return(ref mut ret) => {
                let ReturnStatement { expression } = ret;
                if let Some(ref mut expression) = expression {
                    self.analyze_expression(parent, expression, diagnostics)?;
                }
            }
            SequentialStatement::Wait(ref mut wait_stmt) => {
                let WaitStatement {
                    sensitivity_clause,
                    condition_clause,
                    timeout_clause,
                } = wait_stmt;
                for name in sensitivity_clause.iter_mut() {
                    self.resolve_name(parent, &name.pos, &mut name.item, diagnostics)?;
                }
                if let Some(expr) = condition_clause {
                    self.analyze_expression(parent, expr, diagnostics)?;
                }
                if let Some(expr) = timeout_clause {
                    self.analyze_expression(parent, expr, diagnostics)?;
                }
            }
            SequentialStatement::Assert(ref mut assert_stmt) => {
                let AssertStatement {
                    condition,
                    report,
                    severity,
                } = assert_stmt;
                self.analyze_expression(parent, condition, diagnostics)?;
                if let Some(expr) = report {
                    self.analyze_expression(parent, expr, diagnostics)?;
                }
                if let Some(expr) = severity {
                    self.analyze_expression(parent, expr, diagnostics)?;
                }
            }
            SequentialStatement::Report(ref mut report_stmt) => {
                let ReportStatement { report, severity } = report_stmt;
                self.analyze_expression(parent, report, diagnostics)?;
                if let Some(expr) = severity {
                    self.analyze_expression(parent, expr, diagnostics)?;
                }
            }
            SequentialStatement::Exit(ref mut exit_stmt) => {
                let ExitStatement {
                    condition,
                    // @TODO loop label
                    ..
                } = exit_stmt;

                if let Some(expr) = condition {
                    self.analyze_expression(parent, expr, diagnostics)?;
                }
            }
            SequentialStatement::Next(ref mut next_stmt) => {
                let NextStatement {
                    condition,
                    // @TODO loop label
                    ..
                } = next_stmt;

                if let Some(expr) = condition {
                    self.analyze_expression(parent, expr, diagnostics)?;
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
                    self.analyze_sequential_part(parent, item, diagnostics)?;
                    self.analyze_expression(parent, condition, diagnostics)?;
                }
                if let Some(else_item) = else_item {
                    self.analyze_sequential_part(parent, else_item, diagnostics)?;
                }
            }
            SequentialStatement::Case(ref mut case_stmt) => {
                let Selection {
                    expression,
                    alternatives,
                } = case_stmt;
                self.analyze_expression(parent, expression, diagnostics)?;
                for alternative in alternatives.iter_mut() {
                    let Alternative { choices, item } = alternative;
                    self.analyze_choices(parent, choices, diagnostics)?;
                    self.analyze_sequential_part(parent, item, diagnostics)?;
                }
            }
            SequentialStatement::Loop(ref mut loop_stmt) => {
                let LoopStatement {
                    iteration_scheme,
                    statements,
                } = loop_stmt;
                match iteration_scheme {
                    Some(IterationScheme::For(ref mut index, ref mut drange)) => {
                        self.analyze_discrete_range(parent, drange, diagnostics)?;
                        let mut region = parent.nested();
                        let designator: WithPos<Designator> = index.clone().into();
                        region.add(designator, AnyDeclaration::Constant, diagnostics);
                        self.analyze_sequential_part(&mut region, statements, diagnostics)?;
                    }
                    Some(IterationScheme::While(ref mut expr)) => {
                        self.analyze_expression(parent, expr, diagnostics)?;
                        self.analyze_sequential_part(parent, statements, diagnostics)?;
                    }
                    None => {
                        self.analyze_sequential_part(parent, statements, diagnostics)?;
                    }
                }
            }
            SequentialStatement::ProcedureCall(ref mut pcall) => {
                self.analyze_function_call(parent, pcall, diagnostics)?;
            }
            SequentialStatement::SignalAssignment(ref mut assign) => {
                // @TODO more
                let SignalAssignment { target, rhs, .. } = assign;
                self.analyze_waveform_assignment(parent, target, rhs, diagnostics)?;
            }
            SequentialStatement::VariableAssignment(ref mut assign) => {
                let VariableAssignment { target, rhs } = assign;
                self.analyze_expr_assignment(parent, target, rhs, diagnostics)?;
            }
            SequentialStatement::SignalForceAssignment(ref mut assign) => {
                let SignalForceAssignment {
                    target,
                    force_mode: _,
                    rhs,
                } = assign;
                self.analyze_expr_assignment(parent, target, rhs, diagnostics)?;
            }
            SequentialStatement::SignalReleaseAssignment(ref mut assign) => {
                let SignalReleaseAssignment {
                    target,
                    force_mode: _,
                } = assign;
                self.analyze_target(parent, target, diagnostics)?;
            }
            SequentialStatement::Null => {}
        }
        Ok(())
    }

    pub fn analyze_sequential_part(
        &self,
        parent: &mut Region<'_>,
        statements: &mut [LabeledSequentialStatement],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        for statement in statements.iter_mut() {
            self.analyze_sequential_statement(parent, statement, diagnostics)?;
        }

        Ok(())
    }
}
