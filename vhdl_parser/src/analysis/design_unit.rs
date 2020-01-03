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
use root::*;
use semantic::{uninstantiated_package_prefix_error, LookupResult};
use std::sync::Arc;

impl<'a> AnalyzeContext<'a> {
    pub fn analyze_design_unit(
        &self,
        unit: &mut AnyDesignUnit,
        root_region: &mut Region<'_>,
        region: &mut Region<'_>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        match unit {
            AnyDesignUnit::Primary(unit) => match unit {
                AnyPrimaryUnit::Entity(unit) => {
                    self.analyze_entity(unit, root_region, region, diagnostics)
                }
                AnyPrimaryUnit::Configuration(unit) => {
                    self.analyze_configuration(unit, diagnostics)
                }
                AnyPrimaryUnit::Package(unit) => {
                    self.analyze_package(unit, root_region, region, diagnostics)
                }
                AnyPrimaryUnit::PackageInstance(unit) => {
                    self.analyze_package_instance(unit, root_region, region, diagnostics)
                }
                AnyPrimaryUnit::Context(unit) => {
                    self.analyze_context(unit, root_region, region, diagnostics)
                }
            },
            AnyDesignUnit::Secondary(unit) => match unit {
                AnySecondaryUnit::Architecture(unit) => {
                    self.analyze_architecture(unit, diagnostics)
                }
                AnySecondaryUnit::PackageBody(unit) => self.analyze_package_body(unit, diagnostics),
            },
        }
    }

    fn analyze_entity(
        &self,
        unit: &mut EntityDeclaration,
        root_region: &mut Region<'_>,
        region: &mut Region<'_>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        *root_region = Region::default();
        self.add_implicit_context_clause(root_region)?;
        self.analyze_context_clause(root_region, &mut unit.context_clause, diagnostics)?;

        let mut primary_region = root_region.nested();

        if let Some(ref mut list) = unit.generic_clause {
            self.analyze_interface_list(&mut primary_region, list, diagnostics)?;
        }
        if let Some(ref mut list) = unit.port_clause {
            self.analyze_interface_list(&mut primary_region, list, diagnostics)?;
        }
        self.analyze_declarative_part(&mut primary_region, &mut unit.decl, diagnostics)?;
        self.analyze_concurrent_part(&mut primary_region, &mut unit.statements, diagnostics)?;

        primary_region.close_immediate(diagnostics);
        *region = primary_region.without_parent();

        Ok(())
    }

    fn analyze_configuration(
        &self,
        unit: &mut ConfigurationDeclaration,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        let mut root_region = Region::default();
        self.add_implicit_context_clause(&mut root_region)?;
        self.analyze_context_clause(&mut root_region, &mut unit.context_clause, diagnostics)?;

        match self.lookup_entity_for_configuration(&root_region, unit) {
            Ok((entity_decl, _)) => {
                if let Some(primary_pos) = entity_decl.first_pos() {
                    let secondary_pos = unit.pos();
                    if primary_pos.source == secondary_pos.source
                        && primary_pos.start() > secondary_pos.start()
                    {
                        diagnostics.push(Diagnostic::error(
                            secondary_pos,
                            format!(
                                "Configuration '{}' declared before entity '{}'",
                                &unit.name(),
                                &entity_decl.designator
                            ),
                        ));
                    }
                }
            }
            Err(err) => {
                err.add_to(diagnostics)?;
            }
        };
        Ok(())
    }

    fn analyze_package(
        &self,
        unit: &mut PackageDeclaration,
        root_region: &mut Region<'_>,
        region: &mut Region<'_>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        *root_region = Region::default();
        self.add_implicit_context_clause(root_region)?;
        self.analyze_context_clause(root_region, &mut unit.context_clause, diagnostics)?;

        let mut primary_region = root_region.nested().in_package_declaration();

        if let Some(ref mut list) = unit.generic_clause {
            self.analyze_interface_list(&mut primary_region, list, diagnostics)?;
        }
        self.analyze_declarative_part(&mut primary_region, &mut unit.decl, diagnostics)?;

        if self.has_package_body() {
            primary_region.close_immediate(diagnostics);
        } else {
            primary_region.close_both(diagnostics);
        }

        *region = primary_region.without_parent();

        Ok(())
    }

    fn analyze_package_instance(
        &self,
        unit: &mut PackageInstantiation,
        root_region: &mut Region<'_>,
        region: &mut Region<'_>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        *root_region = Region::default();
        self.add_implicit_context_clause(root_region)?;
        self.analyze_context_clause(root_region, &mut unit.context_clause, diagnostics)?;

        match self.analyze_package_instance_name(root_region, &mut unit.package_name) {
            Ok(package_region) => {
                *region = (*package_region).clone();
                Ok(())
            }
            Err(AnalysisError::NotFatal(diagnostic)) => {
                diagnostics.push(diagnostic);
                Ok(())
            }
            Err(AnalysisError::Fatal(err)) => Err(err),
        }
    }

    fn analyze_context(
        &self,
        unit: &mut ContextDeclaration,
        root_region: &mut Region<'_>,
        region: &mut Region<'_>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        *root_region = Region::default();
        self.add_implicit_context_clause(root_region)?;
        let mut primary_region = root_region.nested();
        self.analyze_context_clause(&mut primary_region, &mut unit.items, diagnostics)?;
        *region = primary_region.without_parent();
        Ok(())
    }

    fn analyze_architecture(
        &self,
        unit: &mut ArchitectureBody,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        unit.entity_name.clear_reference();

        let entity = self.lookup_primary_unit(
            unit.primary_ident(),
            PrimaryKind::Entity,
            unit.pos(),
            diagnostics,
        )?;
        // @TODO maybe add more fatal results
        let entity = if let Some(entity) = entity {
            entity
        } else {
            return Ok(());
        };

        unit.entity_name.set_reference_pos(Some(entity.pos()));

        let mut root_region = Region::extend(&entity.result().root_region, None);
        self.analyze_context_clause(&mut root_region, &mut unit.context_clause, diagnostics)?;
        let mut region = Region::extend(&entity.result().region, Some(&root_region));

        // entity name is visible
        region.make_potentially_visible(VisibleDeclaration::new(
            entity.ident(),
            AnyDeclaration::Constant,
        ));

        self.analyze_declarative_part(&mut region, &mut unit.decl, diagnostics)?;
        self.analyze_concurrent_part(&mut region, &mut unit.statements, diagnostics)?;
        region.close_both(diagnostics);
        Ok(())
    }

    fn analyze_package_body(
        &self,
        unit: &mut PackageBody,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        unit.ident.clear_reference();

        let package_data = self.lookup_primary_unit(
            unit.primary_ident(),
            PrimaryKind::Package,
            unit.pos(),
            diagnostics,
        )?;
        // @TODO maybe add more fatal results
        let package_data = if let Some(package_data) = package_data {
            package_data
        } else {
            return Ok(());
        };

        unit.ident.set_reference_pos(Some(package_data.pos()));
        // @TODO make pattern of primary/secondary extension
        let mut root_region = Region::extend(&package_data.result().root_region, None);
        self.analyze_context_clause(&mut root_region, &mut unit.context_clause, diagnostics)?;

        let mut region = Region::extend(&package_data.result().region, Some(&root_region));

        // Package name is visible in body
        region.make_potentially_visible(VisibleDeclaration::new(
            package_data.ident(),
            AnyDeclaration::Constant,
        ));

        self.analyze_declarative_part(&mut region, &mut unit.decl, diagnostics)?;
        region.close_both(diagnostics);
        Ok(())
    }

    fn lookup_primary_unit(
        &self,
        primary_ident: &Ident,
        primary_kind: PrimaryKind,
        secondary_pos: &SrcPos,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult<Option<UnitReadGuard<'a>>> {
        let unit = {
            if let Some(data) = self.get_primary_analysis(
                primary_ident.pos(),
                self.work_library_name(),
                primary_ident.name(),
                primary_kind,
            ) {
                data?
            } else {
                diagnostics.push(Diagnostic::error(
                    primary_ident.pos(),
                    format!(
                        "No {} '{}' within library '{}'",
                        primary_kind.describe(),
                        primary_ident.name(),
                        self.work_library_name()
                    ),
                ));
                return Ok(None);
            }
        };

        let primary_pos = unit.pos();
        if primary_pos.source == secondary_pos.source && primary_pos.start() > secondary_pos.start()
        {
            diagnostics.push(Diagnostic::error(
                secondary_pos,
                format!(
                    "{} declared before {} '{}'",
                    capitalize(&self.current_unit_id().describe()),
                    primary_kind.describe(),
                    unit.name()
                ),
            ));
        }

        Ok(Some(unit))
    }

    fn lookup_entity_for_configuration(
        &self,
        region: &Region<'_>,
        config: &mut ConfigurationDeclaration,
    ) -> AnalysisResult<(VisibleDeclaration, Arc<Region<'static>>)> {
        let ref mut ent_name = config.entity_name;

        let decl = {
            match ent_name.item {
                // Entitities are implicitly defined for configurations
                // configuration cfg of ent
                SelectedName::Designator(ref mut designator) => {
                    match self.lookup_in_library(
                        self.work_library_name(),
                        &ent_name.pos,
                        &designator.item,
                    ) {
                        Ok(decl) => {
                            designator.set_reference(&decl);
                            Ok(decl)
                        }
                        Err(err) => {
                            designator.clear_reference();
                            Err(err)
                        }
                    }
                }

                // configuration cfg of lib.ent
                _ => self.resolve_selected_name(&region, ent_name),
            }
        }?;

        match decl.first() {
            AnyDeclaration::Entity(ref unit_id, ref entity_region) => {
                if unit_id.library_name() != self.work_library_name() {
                    Err(Diagnostic::error(
                        &ent_name,
                        format!("Configuration must be within the same library '{}' as the corresponding entity", self.work_library_name()),
                    ))?
                } else {
                    Ok((decl.clone(), entity_region.clone()))
                }
            }
            _ => Err(Diagnostic::error(&ent_name, "does not denote an entity"))?,
        }
    }

    fn resolve_context_item_name(
        &self,
        region: &Region<'_>,
        name: &mut WithPos<Name>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> AnalysisResult<LookupResult> {
        self.resolve_name_pos(region, &name.pos, &mut name.item, false, diagnostics)
    }

    pub fn analyze_context_clause(
        &self,
        region: &mut Region<'_>,
        context_clause: &mut [WithPos<ContextItem>],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        for context_item in context_clause.iter_mut() {
            match context_item.item {
                ContextItem::Library(LibraryClause { ref name_list }) => {
                    for library_name in name_list.iter() {
                        if self.work_sym == library_name.item {
                            diagnostics.push(Diagnostic::hint(
                                &library_name,
                                "Library clause not necessary for current working library",
                            ))
                        } else if self.has_library(&library_name.item) {
                            region.make_library_visible(
                                &library_name.item,
                                &library_name.item,
                                Some(library_name.pos().clone()),
                            );
                        } else {
                            diagnostics.push(Diagnostic::error(
                                &library_name,
                                format!("No such library '{}'", library_name.item),
                            ));
                        }
                    }
                }
                ContextItem::Use(ref mut use_clause) => {
                    self.analyze_use_clause(region, use_clause, &context_item.pos, diagnostics)?;
                }
                ContextItem::Context(ContextReference { ref mut name_list }) => {
                    for name in name_list.iter_mut() {
                        match name.item {
                            Name::Selected(..) => {}
                            _ => {
                                diagnostics.push(Diagnostic::error(
                                    &context_item.pos,
                                    "Context reference must be a selected name",
                                ));
                                continue;
                            }
                        }

                        match self.resolve_context_item_name(&region, name, diagnostics) {
                            Ok(LookupResult::Single(visible_decl)) => {
                                match visible_decl.first() {
                                    // OK
                                    AnyDeclaration::Context(_, ref context_region) => {
                                        region.copy_visibility_from(context_region);
                                    }
                                    _ => {
                                        // @TODO maybe lookup should return the source position of the suffix
                                        if let Name::Selected(_, ref suffix) = name.item {
                                            diagnostics.push(Diagnostic::error(
                                                &suffix,
                                                format!(
                                                    "'{}' does not denote a context declaration",
                                                    suffix.designator()
                                                ),
                                            ));
                                        }
                                    }
                                }
                            }
                            Ok(LookupResult::AllWithin(..)) => {
                                // @TODO
                            }
                            Ok(LookupResult::NotSelected) => {
                                diagnostics.push(Diagnostic::error(
                                    &context_item.pos,
                                    "Context reference must be a selected name",
                                ));
                            }
                            Err(err) => {
                                err.add_to(diagnostics)?;
                            }
                        }
                    }
                }
            }
        }

        Ok(())
    }

    pub fn analyze_use_clause(
        &self,
        region: &mut Region<'_>,
        use_clause: &mut UseClause,
        use_pos: &SrcPos,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        for name in use_clause.name_list.iter_mut() {
            match name.item {
                Name::Selected(..) => {}
                Name::SelectedAll(..) => {}
                _ => {
                    diagnostics.push(Diagnostic::error(
                        &use_pos,
                        "Use clause must be a selected name",
                    ));
                    continue;
                }
            }

            match self.resolve_context_item_name(&region, name, diagnostics) {
                Ok(LookupResult::Single(visible_decl)) => {
                    region.make_potentially_visible(visible_decl);
                }
                Ok(LookupResult::AllWithin(visibility_pos, visible_decl)) => {
                    match visible_decl.first() {
                        AnyDeclaration::Library(ref library_name) => {
                            self.use_all_in_library(&name.pos, library_name, region)?;
                        }
                        AnyDeclaration::UninstPackage(ref unit_id, ..) => {
                            diagnostics.push(uninstantiated_package_prefix_error(
                                &visibility_pos,
                                unit_id,
                            ));
                        }
                        AnyDeclaration::Package(_, ref package_region) => {
                            region.make_all_potentially_visible(package_region);
                        }
                        AnyDeclaration::PackageInstance(_, ref package_region) => {
                            region.make_all_potentially_visible(package_region);
                        }
                        AnyDeclaration::LocalPackageInstance(_, ref instance_region) => {
                            region.make_all_potentially_visible(&instance_region);
                        }
                        // @TODO handle others
                        _ => {}
                    }
                }
                Ok(LookupResult::NotSelected) => {
                    diagnostics.push(Diagnostic::error(
                        &use_pos,
                        "Use clause must be a selected name",
                    ));
                }
                Err(err) => {
                    err.add_to(diagnostics)?;
                }
            }
        }

        Ok(())
    }

    /// Returns a reference to the the uninstantiated package
    pub fn analyze_package_instance_name(
        &self,
        region: &Region<'_>,
        package_name: &mut WithPos<SelectedName>,
    ) -> AnalysisResult<Arc<Region<'static>>> {
        let decl = self.resolve_selected_name(region, package_name)?;

        if let AnyDeclaration::UninstPackage(_, ref package_region) = decl.first() {
            Ok(package_region.clone())
        } else {
            Err(Diagnostic::error(
                &package_name.pos,
                format!(
                    "'{}' is not an uninstantiated generic package",
                    &decl.designator
                ),
            ))?
        }
    }
}
