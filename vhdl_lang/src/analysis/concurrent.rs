// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

use super::*;
use crate::analysis::names::ResolvedName;
use crate::ast::token_range::WithTokenSpan;
use crate::ast::*;
use crate::data::error_codes::ErrorCode;
use crate::data::*;
use crate::named_entity::*;
use crate::{HasTokenSpan, TokenSpan};
use analyze::*;

impl<'a> AnalyzeContext<'a, '_> {
    pub fn analyze_concurrent_part(
        &self,
        scope: &Scope<'a>,
        parent: EntRef<'a>,
        statements: &mut [LabeledConcurrentStatement],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        for statement in statements.iter_mut() {
            let parent = if let Some(id) = statement.label.decl.get() {
                self.arena.get(id)
            } else {
                parent
            };

            self.analyze_concurrent_statement(scope, parent, statement, diagnostics)?;
        }

        Ok(())
    }

    pub fn define_labels_for_concurrent_part(
        &self,
        scope: &Scope<'a>,
        parent: EntRef<'a>,
        statements: &mut [LabeledConcurrentStatement],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        for statement in statements.iter_mut() {
            let span = statement.span();
            if let Some(ref mut label) = statement.label.tree {
                let ent = self.arena.explicit(
                    label.name(),
                    parent,
                    AnyEntKind::Concurrent(statement.statement.item.label_typ()),
                    Some(label.pos(self.ctx)),
                    span,
                    Some(self.source()),
                );
                statement.label.decl.set(ent.id());
                scope.add(ent, diagnostics);
            } else if statement.statement.item.can_have_label() {
                // Generate an anonymous label if it is not explicitly defined
                let ent = self.arena.alloc(
                    scope.anonymous_designator(),
                    Some(parent),
                    Related::None,
                    AnyEntKind::Concurrent(statement.statement.item.label_typ()),
                    None,
                    span,
                    Some(self.source()),
                );
                statement.label.decl.set(ent.id());
            }
        }

        Ok(())
    }

    fn analyze_concurrent_statement(
        &self,
        scope: &Scope<'a>,
        parent: EntRef<'a>,
        statement: &mut LabeledConcurrentStatement,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        let src_span = statement.span();
        match statement.statement.item {
            ConcurrentStatement::Block(ref mut block) => {
                if let Some(ref mut guard_condition) = block.guard_condition {
                    self.boolean_expr(scope, guard_condition, diagnostics)?;
                }
                let nested = scope.nested();
                if let Some(ref mut list) = block.header.generic_clause {
                    self.analyze_interface_list(&nested, parent, list, diagnostics)?;
                }
                if let Some(ref mut list) = block.header.generic_map {
                    self.analyze_assoc_elems(scope, &mut list.list.items[..], diagnostics)?;
                }
                if let Some(ref mut list) = block.header.port_clause {
                    self.analyze_interface_list(&nested, parent, list, diagnostics)?;
                }
                if let Some(ref mut list) = block.header.port_map {
                    self.analyze_assoc_elems(scope, &mut list.list.items[..], diagnostics)?;
                }

                self.define_labels_for_concurrent_part(
                    &nested,
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
                    ..
                } = process;
                if let Some(sensitivity_list) = sensitivity_list {
                    match &mut sensitivity_list.item {
                        SensitivityList::Names(names) => {
                            self.sensitivity_list_check(scope, names, diagnostics)?;
                        }
                        SensitivityList::All => {}
                    }
                }
                let nested = scope.nested();
                self.define_labels_for_sequential_part(&nested, parent, statements, diagnostics)?;
                self.analyze_declarative_part(&nested, parent, decl, diagnostics)?;
                self.analyze_sequential_part(&nested, parent, statements, diagnostics)?;
            }
            ConcurrentStatement::ForGenerate(ref mut gen) => {
                let ForGenerateStatement {
                    index_name,
                    discrete_range,
                    body,
                    end_label_pos: _,
                    ..
                } = gen;
                let typ = as_fatal(self.drange_type(scope, discrete_range, diagnostics))?;
                let nested = scope.nested();
                nested.add(
                    index_name.define(
                        self.ctx,
                        self.arena,
                        parent,
                        AnyEntKind::LoopParameter(typ),
                        src_span,
                        Some(self.source()),
                    ),
                    diagnostics,
                );
                self.analyze_generate_body(&nested, parent, body, src_span, diagnostics)?;
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
                    self.analyze_generate_body(&nested, parent, item, src_span, diagnostics)?;
                }
                if let Some((ref mut else_item, _)) = else_item {
                    let nested = scope.nested();
                    self.analyze_generate_body(&nested, parent, else_item, src_span, diagnostics)?;
                }
            }
            ConcurrentStatement::CaseGenerate(ref mut gen) => {
                let CaseGenerateStatement {
                    sels:
                        Selection {
                            ref mut expression,
                            ref mut alternatives,
                        },
                    end_label_pos: _,
                    ..
                } = gen;

                let ctyp = as_fatal(self.expr_unambiguous_type(scope, expression, diagnostics))?;
                for alternative in alternatives.iter_mut() {
                    let Alternative {
                        ref mut choices,
                        ref mut item,
                        span: _,
                    } = alternative;
                    self.choices_with_ttyp(scope, ctyp, choices, diagnostics)?;
                    let nested = scope.nested();
                    self.analyze_generate_body(&nested, parent, item, src_span, diagnostics)?;
                }
            }
            ConcurrentStatement::Instance(ref mut instance) => {
                self.analyze_instance(scope, instance, diagnostics)?;
            }
            ConcurrentStatement::Assignment(ref mut assign) => {
                // @TODO more delaymechanism
                let ConcurrentSignalAssignment { assignment, .. } = assign;
                self.analyze_waveform_assignment(scope, assignment, diagnostics)?;
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
        parent: EntRef<'a>,
        body: &mut GenerateBody,
        span: TokenSpan,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        let GenerateBody {
            alternative_label,
            decl,
            statements,
            ..
        } = body;

        let mut inner_parent = parent;
        if let Some(label) = alternative_label {
            let ent = label.define(
                self.ctx,
                self.arena,
                parent,
                AnyEntKind::Concurrent(Some(Concurrent::Generate)),
                span,
                Some(self.source()),
            );
            scope.add(ent, diagnostics);
            inner_parent = ent;
        }

        // Pre-declare labels
        self.define_labels_for_concurrent_part(scope, parent, statements, diagnostics)?;

        if let Some((ref mut decl, _)) = decl {
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
                let Some(resolved) = as_fatal(self.name_resolve(
                    scope,
                    entity_name.span,
                    &mut entity_name.item,
                    diagnostics,
                ))?
                else {
                    return Ok(());
                };
                match resolved {
                    ResolvedName::Design(ent) => match ent.kind() {
                        Design::Entity(_, ent_region) => {
                            if let Designator::Identifier(entity_ident) = ent.designator() {
                                if let Some(library_name) = ent.library_name() {
                                    if let Some(ref mut architecture_name) = architecture_name {
                                        if let Some(arch) = as_fatal(self.get_architecture(
                                            diagnostics,
                                            library_name,
                                            self.ctx.get_pos(architecture_name.item.token),
                                            entity_ident,
                                            &architecture_name.item.item,
                                        ))? {
                                            architecture_name.set_unique_reference(&arch);
                                        }
                                    }
                                }
                            }

                            let (generic_region, port_region) = ent_region.to_entity_formal();

                            self.check_association(
                                &entity_name.pos(self.ctx),
                                &generic_region,
                                scope,
                                instance
                                    .generic_map
                                    .as_mut()
                                    .map(|it| it.list.items.as_mut_slice())
                                    .unwrap_or(&mut []),
                                diagnostics,
                            )?;
                            self.check_association(
                                &entity_name.pos(self.ctx),
                                &port_region,
                                scope,
                                instance
                                    .port_map
                                    .as_mut()
                                    .map(|it| it.list.items.as_mut_slice())
                                    .unwrap_or(&mut []),
                                diagnostics,
                            )?;
                            Ok(())
                        }
                        _ => {
                            diagnostics.push(
                                resolved
                                    .kind_error(entity_name.suffix_pos().pos(self.ctx), "entity"),
                            );
                            Ok(())
                        }
                    },
                    other => {
                        diagnostics.push(
                            other.kind_error(entity_name.suffix_pos().pos(self.ctx), "entity"),
                        );
                        Ok(())
                    }
                }
            }
            InstantiatedUnit::Component(ref mut component_name) => {
                let Some(resolved) = as_fatal(self.name_resolve(
                    scope,
                    component_name.span,
                    &mut component_name.item,
                    diagnostics,
                ))?
                else {
                    return Ok(());
                };

                let ent = match resolved {
                    ResolvedName::Final(ent) => ent,
                    other => {
                        diagnostics.push(
                            other
                                .kind_error(component_name.suffix_pos().pos(self.ctx), "component"),
                        );
                        return Ok(());
                    }
                };

                if let AnyEntKind::Component(ent_region) = ent.kind() {
                    let (generic_region, port_region) = ent_region.to_entity_formal();
                    self.check_association(
                        &component_name.pos(self.ctx),
                        &generic_region,
                        scope,
                        instance
                            .generic_map
                            .as_mut()
                            .map(|it| it.list.items.as_mut_slice())
                            .unwrap_or(&mut []),
                        diagnostics,
                    )?;
                    self.check_association(
                        &component_name.pos(self.ctx),
                        &port_region,
                        scope,
                        instance
                            .port_map
                            .as_mut()
                            .map(|it| it.list.items.as_mut_slice())
                            .unwrap_or(&mut []),
                        diagnostics,
                    )?;
                    Ok(())
                } else {
                    diagnostics.push(
                        resolved.kind_error(component_name.suffix_pos().pos(self.ctx), "component"),
                    );
                    Ok(())
                }
            }
            InstantiatedUnit::Configuration(ref mut config_name) => {
                let Some(resolved) = as_fatal(self.name_resolve(
                    scope,
                    config_name.span,
                    &mut config_name.item,
                    diagnostics,
                ))?
                else {
                    return Ok(());
                };
                match resolved {
                    ResolvedName::Design(ent) if matches!(ent.kind(), Design::Configuration) => {}
                    other => {
                        diagnostics.push(
                            other.kind_error(
                                config_name.suffix_pos().pos(self.ctx),
                                "configuration",
                            ),
                        );
                        return Ok(());
                    }
                }

                self.analyze_map_aspect(scope, &mut instance.generic_map, diagnostics)?;
                self.analyze_map_aspect(scope, &mut instance.port_map, diagnostics)
            }
        }
    }

    pub fn analyze_map_aspect(
        &self,
        scope: &Scope<'a>,
        map: &mut Option<MapAspect>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        let Some(aspect) = map else {
            return Ok(());
        };
        self.analyze_assoc_elems(scope, aspect.list.items.as_mut_slice(), diagnostics)
    }

    pub fn sensitivity_list_check(
        &self,
        scope: &Scope<'a>,
        names: &mut [WithTokenSpan<Name>],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        for name in names.iter_mut() {
            if let Some(object_name) = as_fatal(self.resolve_object_name(
                scope,
                name.span,
                &mut name.item,
                "is not a signal and cannot be in a sensitivity list",
                ErrorCode::DisallowedInSensitivityList,
                diagnostics,
            ))? {
                if object_name.base.class() != ObjectClass::Signal {
                    diagnostics.add(
                        name.pos(self.ctx),
                        format!(
                            "{} is not a signal and cannot be in a sensitivity list",
                            object_name.base.describe_class()
                        ),
                        ErrorCode::DisallowedInSensitivityList,
                    )
                } else if object_name.base.mode() == Some(&InterfaceMode::Simple(Mode::Out))
                    && !object_name.base.is_port()
                {
                    diagnostics.add(
                        name.pos(self.ctx),
                        format!(
                            "{} cannot be in a sensitivity list",
                            object_name.base.describe_class()
                        ),
                        ErrorCode::DisallowedInSensitivityList,
                    )
                }
            }
        }
        Ok(())
    }
}
