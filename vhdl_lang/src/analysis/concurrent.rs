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
        parent: Option<EntRef<'a>>,
        statements: &mut [LabeledConcurrentStatement],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        for statement in statements.iter_mut() {
            self.analyze_concurrent_statement(scope, parent, statement, diagnostics)?;
        }

        Ok(())
    }

    pub fn define_labels_for_concurrent_part(
        &self,
        scope: &Scope<'a>,
        parent: Option<EntRef<'a>>,
        statements: &mut [LabeledConcurrentStatement],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        for statement in statements.iter_mut() {
            if let Some(ref mut label) = statement.label {
                scope.add(
                    label.define(self.arena, parent, AnyEntKind::Label),
                    diagnostics,
                );
            }
        }

        Ok(())
    }

    fn analyze_concurrent_statement(
        &self,
        scope: &Scope<'a>,
        parent: Option<EntRef<'a>>,
        statement: &mut LabeledConcurrentStatement,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        // @TODO without label use orginal parent, should unlabeled statements be implicitly declared?
        let parent = if let Some(id) = statement.label.as_ref().and_then(|label| label.decl) {
            Some(self.arena.get(id))
        } else {
            parent
        };

        match statement.statement {
            ConcurrentStatement::Block(ref mut block) => {
                if let Some(ref mut guard_condition) = block.guard_condition {
                    self.boolean_expr(scope, guard_condition, diagnostics)?;
                }
                let nested = scope.nested();
                if let Some(ref mut list) = block.header.generic_clause {
                    self.analyze_interface_list(&nested, parent, list, diagnostics)?;
                }
                if let Some(ref mut list) = block.header.generic_map {
                    self.analyze_assoc_elems(scope, list, diagnostics)?;
                }
                if let Some(ref mut list) = block.header.port_clause {
                    self.analyze_interface_list(&nested, parent, list, diagnostics)?;
                }
                if let Some(ref mut list) = block.header.port_map {
                    self.analyze_assoc_elems(scope, list, diagnostics)?;
                }

                self.define_labels_for_concurrent_part(
                    scope,
                    parent,
                    &mut block.statements,
                    diagnostics,
                )?;
                self.analyze_declarative_part(&nested, parent, &mut block.decl, diagnostics)?;
                self.analyze_concurrent_part(&nested, parent, &mut block.statements, diagnostics)?;
            }
            ConcurrentStatement::Process(ref mut process) => {
                let ProcessStatement {
                    postponed: _,
                    sensitivity_list,
                    decl,
                    statements,
                    end_label_pos: _,
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
                self.define_labels_for_sequential_part(scope, parent, statements, diagnostics)?;
                self.analyze_declarative_part(&nested, parent, decl, diagnostics)?;
                self.analyze_sequential_part(
                    &nested,
                    parent,
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
                    end_label_pos: _,
                } = gen;
                let typ = as_fatal(self.drange_type(scope, discrete_range, diagnostics))?;
                let nested = scope.nested();
                nested.add(
                    index_name.define(self.arena, parent, AnyEntKind::LoopParameter(typ)),
                    diagnostics,
                );
                self.analyze_generate_body(&nested, parent, body, diagnostics)?;
            }
            ConcurrentStatement::IfGenerate(ref mut gen) => {
                let Conditionals {
                    conditionals,
                    else_item,
                } = &mut gen.conds;
                for conditional in conditionals.iter_mut() {
                    let Conditional { condition, item } = conditional;
                    self.boolean_expr(scope, condition, diagnostics)?;
                    let nested = scope.nested();
                    self.analyze_generate_body(&nested, parent, item, diagnostics)?;
                }
                if let Some(ref mut else_item) = else_item {
                    let nested = scope.nested();
                    self.analyze_generate_body(&nested, parent, else_item, diagnostics)?;
                }
            }
            ConcurrentStatement::CaseGenerate(ref mut gen) => {
                for alternative in gen.sels.alternatives.iter_mut() {
                    let nested = scope.nested();
                    self.analyze_generate_body(
                        &nested,
                        parent,
                        &mut alternative.item,
                        diagnostics,
                    )?;
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
        parent: Option<EntRef<'a>>,
        body: &mut GenerateBody,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        let GenerateBody {
            alternative_label,
            decl,
            statements,
            end_label_pos: _,
        } = body;

        let mut inner_parent = parent;
        if let Some(label) = alternative_label {
            let ent = label.define(self.arena, parent, AnyEntKind::Label);
            scope.add(ent, diagnostics);
            inner_parent = Some(ent);
        }

        // Pre-declare labels
        self.define_labels_for_concurrent_part(scope, parent, statements, diagnostics)?;

        if let Some(ref mut decl) = decl {
            self.analyze_declarative_part(scope, parent, decl, diagnostics)?;
        }
        self.analyze_concurrent_part(scope, inner_parent, statements, diagnostics)?;

        Ok(())
    }

    fn analyze_instance(
        &self,
        scope: &Scope<'a>,
        instance: &mut InstantiationStatement,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        match instance.unit {
            InstantiatedUnit::Entity(ref mut entity_name, ref mut architecture_name) => {
                if let Err(err) =
                    self.resolve_selected_name(scope, entity_name)
                        .and_then(|entities| {
                            let expected = "entity";
                            let ent = self.resolve_non_overloaded(
                                entities,
                                entity_name.suffix_pos(),
                                expected,
                            )?;

                            if let AnyEntKind::Design(Design::Entity(library_name, _, ent_region)) =
                                ent.kind()
                            {
                                if let Designator::Identifier(entity_ident) = ent.designator() {
                                    if let Some(ref mut architecture_name) = architecture_name {
                                        match self.get_architecture(
                                            library_name,
                                            &architecture_name.item.pos,
                                            entity_ident,
                                            &architecture_name.item.item,
                                        ) {
                                            Ok(arch) => {
                                                architecture_name.set_unique_reference(&arch);
                                            }
                                            Err(err) => {
                                                diagnostics.push(err.into_non_fatal()?);
                                            }
                                        }
                                    }
                                }

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
                } else if object_name.base.mode() == Some(Mode::Out) && !object_name.base.is_port()
                {
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
