// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

use super::named_entity::*;
use super::*;
use crate::ast::*;
use crate::data::*;
use analyze::*;
use region::*;

impl<'a> AnalyzeContext<'a> {
    pub fn analyze_primary_unit(
        &self,
        id: EntityId,
        unit: &mut AnyPrimaryUnit,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        match unit {
            AnyPrimaryUnit::Entity(unit) => self.analyze_entity(id, unit, diagnostics),
            AnyPrimaryUnit::Configuration(unit) => self.analyze_configuration(unit, diagnostics),
            AnyPrimaryUnit::Package(unit) => self.analyze_package(id, unit, diagnostics),
            AnyPrimaryUnit::PackageInstance(unit) => {
                self.analyze_package_instance(id, unit, diagnostics)
            }
            AnyPrimaryUnit::Context(unit) => self.analyze_context(unit, diagnostics),
        }
    }

    pub fn analyze_secondary_unit(
        &self,
        unit: &mut AnySecondaryUnit,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        match unit {
            AnySecondaryUnit::Architecture(unit) => self.analyze_architecture(unit, diagnostics),
            AnySecondaryUnit::PackageBody(unit) => self.analyze_package_body(unit, diagnostics),
        }
    }

    fn analyze_entity(
        &self,
        id: EntityId,
        unit: &mut EntityDeclaration,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        let root_scope = Scope::default();
        self.add_implicit_context_clause(&root_scope)?;
        self.analyze_context_clause(&root_scope, &mut unit.context_clause, diagnostics)?;

        let primary_scope = root_scope.nested();

        // Entity name is visible
        primary_scope.make_potentially_visible(Some(unit.pos()), self.arena.get(id));

        if let Some(ref mut list) = unit.generic_clause {
            self.analyze_interface_list(&primary_scope, list, diagnostics)?;
        }
        if let Some(ref mut list) = unit.port_clause {
            self.analyze_interface_list(&primary_scope, list, diagnostics)?;
        }
        self.analyze_declarative_part(&primary_scope, &mut unit.decl, diagnostics)?;
        self.analyze_concurrent_part(&primary_scope, &mut unit.statements, diagnostics)?;

        let region = primary_scope.into_region();
        let visibility = root_scope.into_visibility();

        self.redefine(
            id,
            &mut unit.ident,
            AnyEntKind::Design(Design::Entity(visibility, region)),
        );

        Ok(())
    }

    fn analyze_configuration(
        &self,
        unit: &mut ConfigurationDeclaration,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        let root_region = Scope::default();
        self.add_implicit_context_clause(&root_region)?;
        self.analyze_context_clause(&root_region, &mut unit.context_clause, diagnostics)?;

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

        self.arena
            .define(&mut unit.ident, AnyEntKind::Design(Design::Configuration));

        Ok(())
    }

    fn analyze_package(
        &self,
        id: EntityId,
        unit: &mut PackageDeclaration,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        let root_scope = Scope::default();
        self.add_implicit_context_clause(&root_scope)?;
        self.analyze_context_clause(&root_scope, &mut unit.context_clause, diagnostics)?;

        let scope = root_scope.nested().in_package_declaration();

        // Package name is visible
        scope.make_potentially_visible(Some(unit.pos()), self.arena.get(id));

        if let Some(ref mut list) = unit.generic_clause {
            self.analyze_interface_list(&scope, list, diagnostics)?;
        }
        self.analyze_declarative_part(&scope, &mut unit.decl, diagnostics)?;

        if !self.has_package_body() {
            scope.close(diagnostics);
        }

        let region = scope.into_region();
        let visibility = root_scope.into_visibility();

        self.redefine(
            id,
            &mut unit.ident,
            if unit.generic_clause.is_some() {
                AnyEntKind::Design(Design::UninstPackage(visibility, region))
            } else {
                AnyEntKind::Design(Design::Package(visibility, region))
            },
        );

        Ok(())
    }

    fn analyze_package_instance(
        &self,
        id: EntityId,
        unit: &mut PackageInstantiation,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        let root_scope = Scope::default();
        self.add_implicit_context_clause(&root_scope)?;

        self.analyze_context_clause(&root_scope, &mut unit.context_clause, diagnostics)?;

        if let Some(pkg_region) = self.generic_package_instance(&root_scope, unit, diagnostics)? {
            self.redefine(
                id,
                &mut unit.ident,
                AnyEntKind::Design(Design::PackageInstance(pkg_region)),
            );
        }

        Ok(())
    }

    fn analyze_context(
        &self,
        unit: &mut ContextDeclaration,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        let root_scope = Scope::default();
        self.add_implicit_context_clause(&root_scope)?;
        let scope = root_scope.nested();
        self.analyze_context_clause(&scope, &mut unit.items, diagnostics)?;

        self.arena.define(
            &mut unit.ident,
            AnyEntKind::Design(Design::Context(scope.into_region())),
        );

        Ok(())
    }

    fn analyze_architecture(
        &self,
        unit: &mut ArchitectureBody,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        let primary = match self.lookup_in_library(
            self.work_library_name(),
            &unit.entity_name.item.pos,
            &Designator::Identifier(unit.entity_name.item.item.clone()),
            &mut unit.entity_name.reference,
        ) {
            Ok(primary) => primary,
            Err(err) => {
                diagnostics.push(err.into_non_fatal()?);
                return Ok(());
            }
        };
        self.check_secondary_before_primary(&primary, unit.pos(), diagnostics);

        let (visibility, region) =
            if let Design::Entity(ref visibility, ref region) = primary.kind() {
                (visibility, region)
            } else {
                let mut diagnostic = Diagnostic::error(unit.pos(), "Expected an entity");

                if let Some(pos) = primary.decl_pos() {
                    diagnostic.add_related(pos, format!("Found {}", primary.describe()))
                }

                return Ok(());
            };

        let root_scope = Scope::new(Region::with_visibility(visibility.clone()));
        self.analyze_context_clause(&root_scope, &mut unit.context_clause, diagnostics)?;
        let scope = Scope::extend(region, Some(&root_scope));

        // Architecture name is visible
        scope.make_potentially_visible(
            Some(unit.pos()),
            self.arena
                .explicit(unit.name().clone(), AnyEntKind::Label, Some(unit.pos())),
        );

        self.analyze_declarative_part(&scope, &mut unit.decl, diagnostics)?;
        self.analyze_concurrent_part(&scope, &mut unit.statements, diagnostics)?;
        scope.close(diagnostics);
        Ok(())
    }

    fn analyze_package_body(
        &self,
        unit: &mut PackageBody,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        let primary = match self.lookup_in_library(
            self.work_library_name(),
            &unit.ident.item.pos,
            &Designator::Identifier(unit.ident.item.item.clone()),
            &mut unit.ident.reference,
        ) {
            Ok(primary) => primary,
            Err(err) => {
                diagnostics.push(err.into_non_fatal()?);
                return Ok(());
            }
        };
        self.check_secondary_before_primary(&primary, unit.pos(), diagnostics);

        let (visibility, region) = match primary.kind() {
            Design::Package(ref visibility, ref region)
            | Design::UninstPackage(ref visibility, ref region) => (visibility, region),
            _ => {
                let mut diagnostic = Diagnostic::error(unit.pos(), "Expected a package");

                if let Some(pos) = primary.decl_pos() {
                    diagnostic.add_related(pos, format!("Found {}", primary.describe()))
                }

                return Ok(());
            }
        };

        // @TODO make pattern of primary/secondary extension
        let root_scope = Scope::new(Region::with_visibility(visibility.clone()));

        self.analyze_context_clause(&root_scope, &mut unit.context_clause, diagnostics)?;

        let scope = Scope::extend(region, Some(&root_scope));

        self.analyze_declarative_part(&scope, &mut unit.decl, diagnostics)?;
        scope.close(diagnostics);
        Ok(())
    }

    fn check_secondary_before_primary(
        &self,
        primary: &DesignEnt,
        secondary_pos: &SrcPos,
        diagnostics: &mut dyn DiagnosticHandler,
    ) {
        if let Some(primary_pos) = primary.decl_pos() {
            if primary_pos.source == secondary_pos.source
                && primary_pos.start() > secondary_pos.start()
            {
                diagnostics.push(Diagnostic::error(
                    secondary_pos,
                    format!(
                        "{} declared before {}",
                        capitalize(&self.current_unit_id().describe()),
                        primary.describe(),
                    ),
                ));
            }
        }
    }

    fn lookup_entity_for_configuration(
        &self,
        scope: &Scope<'a>,
        config: &mut ConfigurationDeclaration,
    ) -> AnalysisResult<DesignEnt> {
        let ent_name = &mut config.entity_name;

        match ent_name.item {
            // Entitities are implicitly defined for configurations
            // configuration cfg of ent
            SelectedName::Designator(ref mut designator) => self.lookup_in_library(
                self.work_library_name(),
                &ent_name.pos,
                &designator.item,
                &mut designator.reference,
            ),

            // configuration cfg of lib.ent
            SelectedName::Selected(ref mut prefix, ref mut designator) => {
                self.resolve_selected_name(scope, prefix)?
                    .into_non_overloaded()
                    .map_err(|ent|
                             AnalysisError::not_fatal_error(
                                 &prefix.pos,
                                 format!("{} does not denote a library", ent.first().describe()),
                             )
                    )
                    .and_then(|library_ent| match library_ent.kind() {
                        AnyEntKind::Library => {
                            let library_name = library_ent.designator().expect_identifier();
                            if library_name != self.work_library_name() {
                                Err(AnalysisError::not_fatal_error(
                                    &prefix.pos,
                                    format!("Configuration must be within the same library '{}' as the corresponding entity", self.work_library_name()),
                                ))
                            } else {
                                let primary_ent = self.lookup_in_library(library_name, &designator.pos, &designator.item.item, &mut designator.item.reference)?;
                                match primary_ent.kind() {
                                    Design::Entity(..) => Ok(primary_ent),
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
        scope: &Scope<'a>,
        prefix: &mut WithPos<Name>,
    ) -> AnalysisResult<EntRef<'a>> {
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
        scope: &Scope<'a>,
        name: &mut WithPos<Name>,
    ) -> AnalysisResult<UsedNames<'a>> {
        match &mut name.item {
            Name::Selected(ref mut prefix, ref mut suffix) => {
                let prefix_ent = self.resolve_context_item_prefix(scope, prefix)?;

                let visible = self.lookup_selected(&prefix.pos, prefix_ent, suffix)?;
                suffix.set_reference(&visible);
                Ok(UsedNames::Single(visible))
            }

            Name::SelectedAll(prefix) => {
                let prefix_ent = self.resolve_context_item_prefix(scope, prefix)?;
                Ok(UsedNames::AllWithin(prefix.pos.clone(), prefix_ent))
            }
            Name::Designator(designator) => {
                let visible = scope.lookup(&name.pos, designator.designator())?;
                designator.set_reference(&visible);
                Ok(UsedNames::Single(visible))
            }

            Name::Slice(..)
            | Name::Attribute(..)
            | Name::CallOrIndexed(..)
            | Name::External(..) => Err(AnalysisError::not_fatal_error(
                &name.pos,
                "Invalid selected name",
            )),
        }
    }

    pub(crate) fn analyze_context_clause(
        &self,
        scope: &Scope<'a>,
        context_clause: &mut [WithPos<ContextItem>],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        for context_item in context_clause.iter_mut() {
            match context_item.item {
                ContextItem::Library(LibraryClause { ref mut name_list }) => {
                    for library_name in name_list.iter_mut() {
                        if self.work_sym == library_name.item.item {
                            diagnostics.push(Diagnostic::hint(
                                &library_name.item,
                                "Library clause not necessary for current working library",
                            ))
                        } else if let Some(library) = self.get_library(&library_name.item.item) {
                            library_name.set_unique_reference(library);
                            scope.make_potentially_visible(Some(&library_name.item.pos), library);
                        } else {
                            diagnostics.push(Diagnostic::error(
                                &library_name.item,
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
                                    AnyEntKind::Design(Design::Context(ref context_region)) => {
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
        scope: &Scope<'a>,
        use_clause: &mut UseClause,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
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
                        AnyEntKind::Library => {
                            let library_name = named_entity.designator().expect_identifier();
                            self.use_all_in_library(&name.pos, library_name, scope)?;
                        }
                        AnyEntKind::Design(design) => match design {
                            Design::UninstPackage(..) => {
                                diagnostics.push(Diagnostic::invalid_selected_name_prefix(
                                    named_entity,
                                    &visibility_pos,
                                ));
                            }
                            Design::Package(_, ref primary_region)
                            | Design::PackageInstance(ref primary_region) => {
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
        scope: &Scope<'a>,
        package_name: &mut WithPos<SelectedName>,
    ) -> AnalysisResult<&'a Region<'a>> {
        let decl = self.resolve_selected_name(scope, package_name)?;

        if let AnyEntKind::Design(Design::UninstPackage(_, ref package_region)) = decl.first_kind()
        {
            Ok(package_region)
        } else {
            Err(AnalysisError::not_fatal_error(
                &package_name.pos,
                format!("'{package_name}' is not an uninstantiated generic package"),
            ))
        }
    }
}

pub enum UsedNames<'a> {
    /// A single name was used selected
    Single(NamedEntities<'a>),
    /// All names within was selected
    /// @TODO add pos for where declaration was made visible into VisibleDeclaration
    AllWithin(SrcPos, EntRef<'a>),
}
