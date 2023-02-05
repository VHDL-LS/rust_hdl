// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

// These fields are better explicit than .. since we are forced to consider if new fields should be searched
#![allow(clippy::unneeded_field_pattern)]

use super::named_entity::*;
use super::sequential::SequentialRoot;
use super::*;
use crate::ast::*;
use crate::data::*;
use analyze::*;
use region::*;
use target::AssignmentType;

impl<'a> AnalyzeContext<'a> {
    pub fn analyze_concurrent_part(
        &self,
        scope: &Scope<'a>,
        statements: &mut [LabeledConcurrentStatement],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        for statement in statements.iter_mut() {
            self.analyze_concurrent_statement(scope, statement, diagnostics)?;
        }

        Ok(())
    }

    fn analyze_concurrent_statement(
        &self,
        scope: &Scope<'a>,
        statement: &mut LabeledConcurrentStatement,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        if let Some(ref mut label) = statement.label {
            scope.add(label.define(self.arena, AnyEntKind::Label), diagnostics);
        }

        match statement.statement {
            ConcurrentStatement::Block(ref mut block) => {
                if let Some(ref mut guard_condition) = block.guard_condition {
                    self.boolean_expr(scope, guard_condition, diagnostics)?;
                }
                let nested = scope.nested();
                if let Some(ref mut list) = block.header.generic_clause {
                    self.analyze_interface_list(&nested, list, diagnostics)?;
                }
                if let Some(ref mut list) = block.header.generic_map {
                    self.analyze_assoc_elems(scope, list, diagnostics)?;
                }
                if let Some(ref mut list) = block.header.port_clause {
                    self.analyze_interface_list(&nested, list, diagnostics)?;
                }
                if let Some(ref mut list) = block.header.port_map {
                    self.analyze_assoc_elems(scope, list, diagnostics)?;
                }
                self.analyze_declarative_part(&nested, &mut block.decl, diagnostics)?;
                self.analyze_concurrent_part(&nested, &mut block.statements, diagnostics)?;
            }
            ConcurrentStatement::Process(ref mut process) => {
                let ProcessStatement {
                    postponed: _,
                    sensitivity_list,
                    decl,
                    statements,
                } = process;
                if let Some(sensitivity_list) = sensitivity_list {
                    match sensitivity_list {
                        SensitivityList::Names(names) => {
                            self.sensitivity_list_check(scope, names, diagnostics)?;
                        }
                        SensitivityList::All => {}
                    }
                }
                let nested = scope.nested();
                self.analyze_declarative_part(&nested, decl, diagnostics)?;
                self.analyze_sequential_part(
                    &nested,
                    &SequentialRoot::Process,
                    statements,
                    diagnostics,
                )?;
            }
            ConcurrentStatement::ForGenerate(ref mut gen) => {
                let ForGenerateStatement {
                    index_name,
                    discrete_range,
                    body,
                } = gen;
                let typ = as_fatal(self.drange_type(scope, discrete_range, diagnostics))?;
                let nested = scope.nested();
                nested.add(
                    index_name.define(self.arena, AnyEntKind::LoopParameter(typ)),
                    diagnostics,
                );
                self.analyze_generate_body(&nested, body, diagnostics)?;
            }
            ConcurrentStatement::IfGenerate(ref mut gen) => {
                let Conditionals {
                    conditionals,
                    else_item,
                } = gen;
                for conditional in conditionals.iter_mut() {
                    let Conditional { condition, item } = conditional;
                    self.boolean_expr(scope, condition, diagnostics)?;
                    let nested = scope.nested();
                    self.analyze_generate_body(&nested, item, diagnostics)?;
                }
                if let Some(ref mut else_item) = else_item {
                    let nested = scope.nested();
                    self.analyze_generate_body(&nested, else_item, diagnostics)?;
                }
            }
            ConcurrentStatement::CaseGenerate(ref mut gen) => {
                for alternative in gen.alternatives.iter_mut() {
                    let nested = scope.nested();
                    self.analyze_generate_body(&nested, &mut alternative.item, diagnostics)?;
                }
            }
            ConcurrentStatement::Instance(ref mut instance) => {
                self.analyze_instance(scope, instance, diagnostics)?;
            }
            ConcurrentStatement::Assignment(ref mut assign) => {
                // @TODO more delaymechanism
                let ConcurrentSignalAssignment { target, rhs, .. } = assign;
                self.analyze_waveform_assignment(
                    scope,
                    target,
                    AssignmentType::Signal,
                    rhs,
                    diagnostics,
                )?;
            }
            ConcurrentStatement::ProcedureCall(ref mut pcall) => {
                let ConcurrentProcedureCall { call, .. } = pcall;
                self.analyze_procedure_call(scope, call, diagnostics)?;
            }
            ConcurrentStatement::Assert(ref mut assert) => {
                let ConcurrentAssertStatement {
                    postponed: _postponed,
                    statement:
                        AssertStatement {
                            condition,
                            report,
                            severity,
                        },
                } = assert;
                self.boolean_expr(scope, condition, diagnostics)?;
                if let Some(expr) = report {
                    self.expr_with_ttyp(scope, self.string(), expr, diagnostics)?;
                }
                if let Some(expr) = severity {
                    self.expr_with_ttyp(scope, self.severity_level(), expr, diagnostics)?;
                }
            }
        };
        Ok(())
    }

    fn analyze_generate_body(
        &self,
        scope: &Scope<'a>,
        body: &mut GenerateBody,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        let GenerateBody {
            alternative_label,
            decl,
            statements,
        } = body;
        if let Some(label) = alternative_label {
            scope.add(label.define(self.arena, AnyEntKind::Label), diagnostics);
        }
        if let Some(ref mut decl) = decl {
            self.analyze_declarative_part(scope, decl, diagnostics)?;
        }
        self.analyze_concurrent_part(scope, statements, diagnostics)?;

        Ok(())
    }

    fn analyze_instance(
        &self,
        scope: &Scope<'a>,
        instance: &mut InstantiationStatement,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        match instance.unit {
            // @TODO architecture
            InstantiatedUnit::Entity(ref mut entity_name, ..) => {
                if let Err(err) =
                    self.resolve_selected_name(scope, entity_name)
                        .and_then(|entities| {
                            let expected = "entity";
                            let ent = self.resolve_non_overloaded(
                                entities,
                                entity_name.suffix_pos(),
                                expected,
                            )?;

                            if let AnyEntKind::Design(Design::Entity(_, ent_region)) = ent.kind() {
                                let (generic_region, port_region) = ent_region.to_entity_formal();

                                self.analyze_assoc_elems_with_formal_region(
                                    &entity_name.pos,
                                    &generic_region,
                                    scope,
                                    &mut instance.generic_map,
                                    diagnostics,
                                )?;
                                self.analyze_assoc_elems_with_formal_region(
                                    &entity_name.pos,
                                    &port_region,
                                    scope,
                                    &mut instance.port_map,
                                    diagnostics,
                                )?;
                                Ok(())
                            } else {
                                Err(AnalysisError::NotFatal(
                                    ent.kind_error(entity_name.suffix_pos(), expected),
                                ))
                            }
                        })
                {
                    err.add_to(diagnostics)?;
                }
            }
            InstantiatedUnit::Component(ref mut component_name) => {
                if let Err(err) =
                    self.resolve_selected_name(scope, component_name)
                        .and_then(|entities| {
                            let expected = "component";
                            let ent = self.resolve_non_overloaded(
                                entities,
                                component_name.suffix_pos(),
                                expected,
                            )?;

                            if let AnyEntKind::Component(ent_region) = ent.kind() {
                                let (generic_region, port_region) = ent_region.to_entity_formal();
                                self.analyze_assoc_elems_with_formal_region(
                                    &component_name.pos,
                                    &generic_region,
                                    scope,
                                    &mut instance.generic_map,
                                    diagnostics,
                                )?;
                                self.analyze_assoc_elems_with_formal_region(
                                    &component_name.pos,
                                    &port_region,
                                    scope,
                                    &mut instance.port_map,
                                    diagnostics,
                                )?;
                                Ok(())
                            } else {
                                Err(AnalysisError::NotFatal(
                                    ent.kind_error(component_name.suffix_pos(), expected),
                                ))
                            }
                        })
                {
                    err.add_to(diagnostics)?;
                }
            }
            InstantiatedUnit::Configuration(ref mut config_name) => {
                fn is_configuration(kind: &AnyEntKind) -> bool {
                    matches!(kind, AnyEntKind::Design(Design::Configuration))
                }

                if let Err(err) =
                    self.resolve_selected_name(scope, config_name)
                        .and_then(|entities| {
                            self.resolve_non_overloaded_with_kind(
                                entities,
                                config_name.suffix_pos(),
                                &is_configuration,
                                "configuration",
                            )
                        })
                {
                    err.add_to(diagnostics)?;
                }

                self.analyze_assoc_elems(scope, &mut instance.generic_map, diagnostics)?;
                self.analyze_assoc_elems(scope, &mut instance.port_map, diagnostics)?;
            }
        };

        Ok(())
    }

    pub fn sensitivity_list_check(
        &self,
        scope: &Scope<'a>,
        names: &mut [WithPos<Name>],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        for name in names.iter_mut() {
            if let Some(object_name) = as_fatal(self.resolve_object_name(
                scope,
                &name.pos,
                &mut name.item,
                "is not a signal and cannot be in a sensitivity list",
                diagnostics,
            ))? {
                if object_name.base.class() != ObjectClass::Signal {
                    diagnostics.error(
                        &name.pos,
                        format!(
                            "{} is not a signal and cannot be in a sensitivity list",
                            object_name.base.describe_class()
                        ),
                    )
                } else if object_name.base.mode() == Some(Mode::Out) {
                    diagnostics.error(
                        &name.pos,
                        format!(
                            "{} cannot be in a sensitivity list",
                            object_name.base.describe_class()
                        ),
                    )
                }
            }
        }
        Ok(())
    }
}
