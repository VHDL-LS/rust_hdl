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
use std::ops::Deref;
use std::sync::Arc;

impl<'a> AnalyzeContext<'a> {
    pub fn analyze_design_unit(
        &self,
        id: EntityId,
        unit: &mut AnyDesignUnit,
        root_scope: &mut Scope<'_>,
        region: &mut Region,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        match unit {
            AnyDesignUnit::Primary(unit) => match unit {
                AnyPrimaryUnit::Entity(unit) => {
                    self.analyze_entity(id, unit, root_scope, region, diagnostics)
                }
                AnyPrimaryUnit::Configuration(unit) => {
                    self.analyze_configuration(unit, diagnostics)
                }
                AnyPrimaryUnit::Package(unit) => {
                    self.analyze_package(id, unit, root_scope, region, diagnostics)
                }
                AnyPrimaryUnit::PackageInstance(unit) => {
                    self.analyze_package_instance(unit, root_scope, region, diagnostics)
                }
                AnyPrimaryUnit::Context(unit) => {
                    self.analyze_context(unit, root_scope, region, diagnostics)
                }
            },
            AnyDesignUnit::Secondary(unit) => match unit {
                AnySecondaryUnit::Architecture(unit) => {
                    self.analyze_architecture(id, unit, diagnostics)
                }
                AnySecondaryUnit::PackageBody(unit) => self.analyze_package_body(unit, diagnostics),
            },
        }
    }

    fn analyze_entity(
        &self,
        id: EntityId,
        unit: &mut EntityDeclaration,
        root_scope: &mut Scope<'_>,
        region: &mut Region,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        *root_scope = Scope::default();
        self.add_implicit_context_clause(root_scope)?;
        self.analyze_context_clause(root_scope, &mut unit.context_clause, diagnostics)?;

        let mut primary_scope = root_scope.nested();

        // Entity name is visible
        primary_scope.make_potentially_visible(
            Some(unit.pos()),
            Arc::new(NamedEntity::new_with_id(
                id,
                unit.name().into(),
                NamedEntityKind::Label,
                Some(unit.pos().clone()),
            )),
        );

        if let Some(ref mut list) = unit.generic_clause {
            self.analyze_interface_list(&mut primary_scope, list, diagnostics)?;
        }
        if let Some(ref mut list) = unit.port_clause {
            self.analyze_interface_list(&mut primary_scope, list, diagnostics)?;
        }
        self.analyze_declarative_part(&mut primary_scope, &mut unit.decl, diagnostics)?;
        self.analyze_concurrent_part(&mut primary_scope, &mut unit.statements, diagnostics)?;

        *region = primary_scope.into_region();

        Ok(())
    }

    fn analyze_configuration(
        &self,
        unit: &mut ConfigurationDeclaration,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        let mut root_region = Scope::default();
        self.add_implicit_context_clause(&mut root_region)?;
        self.analyze_context_clause(&mut root_region, &mut unit.context_clause, diagnostics)?;

        match self.lookup_entity_for_configuration(&root_region, unit) {
            Ok(named_entity) => {
                if let Some(primary_pos) = named_entity.decl_pos() {
                    let secondary_pos = unit.pos();
                    if primary_pos.source == secondary_pos.source
                        && primary_pos.start() > secondary_pos.start()
                    {
                        diagnostics.push(Diagnostic::error(
                            secondary_pos,
                            capitalize(&format!(
                                "{} declared before {}",
                                self.current_unit_id().describe(),
                                named_entity.describe()
                            )),
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
        id: EntityId,
        unit: &mut PackageDeclaration,
        root_scope: &mut Scope<'_>,
        region: &mut Region,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        *root_scope = Scope::default();
        self.add_implicit_context_clause(root_scope)?;
        self.analyze_context_clause(root_scope, &mut unit.context_clause, diagnostics)?;

        let mut scope = root_scope.nested().in_package_declaration();

        // Package name is visible
        scope.make_potentially_visible(
            Some(unit.pos()),
            Arc::new(NamedEntity::new_with_id(
                id,
                unit.name().into(),
                NamedEntityKind::Label,
                Some(unit.pos().clone()),
            )),
        );

        if let Some(ref mut list) = unit.generic_clause {
            self.analyze_interface_list(&mut scope, list, diagnostics)?;
        }
        self.analyze_declarative_part(&mut scope, &mut unit.decl, diagnostics)?;

        if !self.has_package_body() {
            scope.close(diagnostics);
        }

        *region = scope.into_region();

        Ok(())
    }

    fn analyze_package_instance(
        &self,
        unit: &mut PackageInstantiation,
        root_scope: &mut Scope<'_>,
        region: &mut Region,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        *root_scope = Scope::default();
        self.add_implicit_context_clause(root_scope)?;
        self.analyze_context_clause(root_scope, &mut unit.context_clause, diagnostics)?;

        match self.analyze_package_instance_name(root_scope, &mut unit.package_name) {
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
        root_scope: &mut Scope<'_>,
        region: &mut Region,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        *root_scope = Scope::default();
        self.add_implicit_context_clause(root_scope)?;
        let mut scope = root_scope.nested();
        self.analyze_context_clause(&mut scope, &mut unit.items, diagnostics)?;
        *region = scope.into_region();
        Ok(())
    }

    fn analyze_architecture(
        &self,
        id: EntityId,
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

        // Set architecture entity name reference to named entity
        if let AnyDesignUnit::Primary(primary) = entity.deref() {
            if let Some(named_entity) = primary.named_entity() {
                unit.entity_name.set_unique_reference(named_entity);
            }
        }

        let parent = Scope::new_borrowed(entity.result().root_region.as_ref());
        let mut root_scope = Scope::default().with_parent(&parent);
        self.analyze_context_clause(&mut root_scope, &mut unit.context_clause, diagnostics)?;
        let mut scope = Scope::extend(&entity.result().region, Some(&root_scope));

        // Architecture name is visible
        scope.make_potentially_visible(
            Some(unit.pos()),
            Arc::new(NamedEntity::new_with_id(
                id,
                unit.name().into(),
                NamedEntityKind::Label,
                Some(unit.pos().clone()),
            )),
        );

        self.analyze_declarative_part(&mut scope, &mut unit.decl, diagnostics)?;
        self.analyze_concurrent_part(&mut scope, &mut unit.statements, diagnostics)?;
        scope.close(diagnostics);
        Ok(())
    }

    fn analyze_package_body(
        &self,
        unit: &mut PackageBody,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        unit.ident.clear_reference();

        let package = self.lookup_primary_unit(
            unit.primary_ident(),
            PrimaryKind::Package,
            unit.pos(),
            diagnostics,
        )?;
        // @TODO maybe add more fatal results
        let package = if let Some(package) = package {
            package
        } else {
            return Ok(());
        };

        // Set package body package name reference to package named entity
        if let AnyDesignUnit::Primary(primary) = package.deref() {
            if let Some(named_entity) = primary.named_entity() {
                unit.ident.set_unique_reference(named_entity);
            }
        }

        // @TODO make pattern of primary/secondary extension
        let parent = Scope::new_borrowed(package.result().root_region.as_ref());
        let mut root_scope = Scope::default().with_parent(&parent);
        self.analyze_context_clause(&mut root_scope, &mut unit.context_clause, diagnostics)?;

        let mut scope = Scope::extend(&package.result().region, Some(&root_scope));

        self.analyze_declarative_part(&mut scope, &mut unit.decl, diagnostics)?;
        scope.close(diagnostics);
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
        scope: &Scope<'_>,
        config: &mut ConfigurationDeclaration,
    ) -> AnalysisResult<Arc<NamedEntity>> {
        let ent_name = &mut config.entity_name;

        match ent_name.item {
            // Entitities are implicitly defined for configurations
            // configuration cfg of ent
            SelectedName::Designator(ref mut designator) => {
                match self.lookup_in_library(
                    self.work_library_name(),
                    &ent_name.pos,
                    &designator.item,
                ) {
                    Ok(ent) => {
                        designator.set_unique_reference(&ent);
                        Ok(ent)
                    }
                    Err(err) => {
                        designator.clear_reference();
                        Err(err)
                    }
                }
            }

            // configuration cfg of lib.ent
            SelectedName::Selected(ref mut prefix, ref mut designator) => {
                designator.clear_reference();

                self.resolve_selected_name(scope, prefix)?
                    .into_non_overloaded()
                    .map_err(|ent|
                             AnalysisError::not_fatal_error(
                                 &prefix.pos,
                                 format!("{} does not denote a library", ent.first().describe()),
                             )
                    )
                    .and_then(|library_ent| match library_ent.kind() {
                        NamedEntityKind::Library => {
                            let library_name = library_ent.designator().expect_identifier();
                            if library_name != self.work_library_name() {
                                Err(AnalysisError::not_fatal_error(
                                    &prefix.pos,
                                    format!("Configuration must be within the same library '{}' as the corresponding entity", self.work_library_name()),
                                ))
                            } else {
                                let primary_ent = self.lookup_in_library(library_name, &designator.pos, designator.designator())?;
                                match primary_ent.kind() {
                                    NamedEntityKind::Design(Design::Entity(..)) => {
                                        designator.set_unique_reference(&primary_ent);
                                        Ok(
                                            primary_ent,
                                        )

                                    }
                                    _ => {
                                        Err(AnalysisError::not_fatal_error(
                                            designator,
                                            format!("{} does not denote an entity", primary_ent.describe()),
                                        ))
                                    }
                                }
                            }

                        }
                        _ => {
                            Err(AnalysisError::not_fatal_error(
                                &prefix.pos,
                                format!("{} does not denote a library", library_ent.describe())
                            ))
                        }
                    })
            }
        }
    }

    fn resolve_context_item_prefix(
        &self,
        scope: &Scope<'_>,
        prefix: &mut WithPos<Name>,
    ) -> AnalysisResult<Arc<NamedEntity>> {
        match self.resolve_context_item_name(scope, prefix)? {
            UsedNames::Single(visible) => visible.into_non_overloaded().map_err(|_| {
                AnalysisError::not_fatal_error(&prefix, "Invalid prefix of a selected name")
            }),
            UsedNames::AllWithin(..) => Err(AnalysisError::not_fatal_error(
                &prefix,
                "'.all' may not be the prefix of a selected name",
            )),
        }
    }

    fn resolve_context_item_name(
        &self,
        scope: &Scope<'_>,
        name: &mut WithPos<Name>,
    ) -> AnalysisResult<UsedNames> {
        match &mut name.item {
            Name::Selected(ref mut prefix, ref mut suffix) => {
                suffix.clear_reference();
                let prefix_ent = self.resolve_context_item_prefix(scope, prefix)?;

                let visible = self.lookup_selected(&prefix.pos, &prefix_ent, suffix)?;
                suffix.set_reference(&visible);
                Ok(UsedNames::Single(visible))
            }

            Name::SelectedAll(prefix) => {
                let prefix_ent = self.resolve_context_item_prefix(scope, prefix)?;
                Ok(UsedNames::AllWithin(prefix.pos.clone(), prefix_ent))
            }
            Name::Designator(designator) => {
                designator.clear_reference();
                let visible = scope.lookup(&name.pos, designator.designator())?;
                designator.set_reference(&visible);
                Ok(UsedNames::Single(visible))
            }
            Name::Indexed(..)
            | Name::Slice(..)
            | Name::Attribute(..)
            | Name::FunctionCall(..)
            | Name::External(..) => Err(AnalysisError::not_fatal_error(
                &name.pos,
                "Invalid selected name",
            )),
        }
    }

    fn analyze_context_clause(
        &self,
        scope: &mut Scope<'_>,
        context_clause: &mut [WithPos<ContextItem>],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        for context_item in context_clause.iter_mut() {
            match context_item.item {
                ContextItem::Library(LibraryClause { ref name_list }) => {
                    for library_name in name_list.iter() {
                        if self.work_sym == library_name.item {
                            diagnostics.push(Diagnostic::hint(
                                library_name,
                                "Library clause not necessary for current working library",
                            ))
                        } else if let Some(library) = self.get_library(&library_name.item) {
                            scope.make_potentially_visible(Some(&library_name.pos), library);
                        } else {
                            diagnostics.push(Diagnostic::error(
                                library_name,
                                format!("No such library '{}'", library_name.item),
                            ));
                        }
                    }
                }
                ContextItem::Use(ref mut use_clause) => {
                    self.analyze_use_clause(scope, use_clause, diagnostics)?;
                }
                ContextItem::Context(ContextReference { ref mut name_list }) => {
                    for name in name_list.iter_mut() {
                        match name.item {
                            Name::Selected(..) => {}
                            _ => {
                                diagnostics.push(Diagnostic::error(
                                    &name.pos,
                                    "Context reference must be a selected name",
                                ));
                                continue;
                            }
                        }

                        match self.resolve_context_item_name(scope, name) {
                            Ok(UsedNames::Single(visible)) => {
                                let ent = visible.first();
                                match ent.kind() {
                                    // OK
                                    NamedEntityKind::Design(Design::Context(
                                        ref context_region,
                                    )) => {
                                        scope.add_context_visibility(
                                            Some(&name.pos),
                                            context_region,
                                        );
                                    }
                                    _ => {
                                        if let Name::Selected(_, ref suffix) = name.item {
                                            diagnostics.push(Diagnostic::error(
                                                suffix,
                                                format!(
                                                    "{} does not denote a context declaration",
                                                    ent.describe()
                                                ),
                                            ));
                                        }
                                    }
                                }
                            }
                            Ok(UsedNames::AllWithin(..)) => {
                                // Handled above
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
        scope: &mut Scope<'_>,
        use_clause: &mut UseClause,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        for name in use_clause.name_list.iter_mut() {
            match name.item {
                Name::Selected(..) => {}
                Name::SelectedAll(..) => {}
                _ => {
                    diagnostics.push(Diagnostic::error(
                        &name.pos,
                        "Use clause must be a selected name",
                    ));
                    continue;
                }
            }

            match self.resolve_context_item_name(scope, name) {
                Ok(UsedNames::Single(visible)) => {
                    visible.make_potentially_visible_in(Some(&name.pos), scope);
                }
                Ok(UsedNames::AllWithin(visibility_pos, named_entity)) => {
                    match named_entity.kind() {
                        NamedEntityKind::Library => {
                            let library_name = named_entity.designator().expect_identifier();
                            self.use_all_in_library(&name.pos, library_name, scope)?;
                        }
                        NamedEntityKind::Design(design) => match design {
                            Design::UninstPackage(..) => {
                                diagnostics.push(Diagnostic::invalid_selected_name_prefix(
                                    &named_entity,
                                    &visibility_pos,
                                ));
                            }
                            Design::Package(ref primary_region)
                            | Design::PackageInstance(ref primary_region)
                            | Design::LocalPackageInstance(ref primary_region) => {
                                scope.make_all_potentially_visible(Some(&name.pos), primary_region);
                            }
                            _ => {
                                diagnostics
                                    .error(visibility_pos, "Invalid prefix for selected name");
                            }
                        },

                        _ => {
                            diagnostics.error(visibility_pos, "Invalid prefix for selected name");
                        }
                    }
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
        scope: &Scope<'_>,
        package_name: &mut WithPos<SelectedName>,
    ) -> AnalysisResult<Arc<Region>> {
        let decl = self.resolve_selected_name(scope, package_name)?;

        if let NamedEntityKind::Design(Design::UninstPackage(ref package_region)) =
            decl.first_kind()
        {
            Ok(package_region.clone())
        } else {
            Err(AnalysisError::not_fatal_error(
                &package_name.pos,
                format!(
                    "'{}' is not an uninstantiated generic package",
                    package_name
                ),
            ))
        }
    }
}

pub enum UsedNames {
    /// A single name was used selected
    Single(NamedEntities),
    /// All names within was selected
    /// @TODO add pos for where declaration was made visible into VisibleDeclaration
    AllWithin(SrcPos, Arc<NamedEntity>),
}
