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
use crate::HasTokenSpan;
use analyze::*;

impl<'a> AnalyzeContext<'a, '_> {
    pub fn analyze_primary_unit(
        &self,
        unit: &mut AnyPrimaryUnit,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        match unit {
            AnyPrimaryUnit::Entity(unit) => self.analyze_entity(unit, diagnostics),
            AnyPrimaryUnit::Configuration(unit) => self.analyze_configuration(unit, diagnostics),
            AnyPrimaryUnit::Package(unit) => self.analyze_package(unit, diagnostics),
            AnyPrimaryUnit::PackageInstance(unit) => {
                self.analyze_package_instance(unit, diagnostics)
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
        unit: &mut EntityDeclaration,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        // Pre-define entity and overwrite it later
        let ent = self.arena.explicit(
            unit.name().clone(),
            self.work_library(),
            AnyEntKind::Design(Design::Entity(Visibility::default(), Region::default())),
            Some(unit.ident_pos(self.ctx)),
            unit.span(),
            Some(self.source()),
        );

        unit.ident.decl.set(ent.id());
        let root_scope = Scope::default();
        self.add_implicit_context_clause(&root_scope)?;
        self.analyze_context_clause(&root_scope, &mut unit.context_clause, diagnostics)?;

        root_scope.add(ent, diagnostics);
        let primary_scope = root_scope.nested();

        if let Some(ref mut list) = unit.generic_clause {
            self.analyze_interface_list(&primary_scope, ent, list, diagnostics)?;
        }
        if let Some(ref mut list) = unit.port_clause {
            self.analyze_interface_list(&primary_scope, ent, list, diagnostics)?;
        }
        self.define_labels_for_concurrent_part(
            &primary_scope,
            ent,
            &mut unit.statements,
            diagnostics,
        )?;
        self.analyze_declarative_part(&primary_scope, ent, &mut unit.decl, diagnostics)?;
        self.analyze_concurrent_part(&primary_scope, ent, &mut unit.statements, diagnostics)?;

        let region = primary_scope.into_region();
        let visibility = root_scope.into_visibility();

        let kind = AnyEntKind::Design(Design::Entity(visibility, region));
        unsafe { ent.set_kind(kind) }

        Ok(())
    }

    fn analyze_configuration(
        &self,
        unit: &mut ConfigurationDeclaration,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        let src_span = unit.span();
        let root_region = Scope::default();
        self.add_implicit_context_clause(&root_region)?;
        self.analyze_context_clause(&root_region, &mut unit.context_clause, diagnostics)?;

        if let Some(named_entity) =
            as_fatal(self.lookup_entity_for_configuration(&root_region, unit, diagnostics))?
        {
            if let Some(primary_pos) = named_entity.decl_pos() {
                let secondary_pos = unit.ident_pos(self.ctx);
                if primary_pos.source == secondary_pos.source
                    && primary_pos.start() > secondary_pos.start()
                {
                    diagnostics.add(
                        secondary_pos,
                        capitalize(&format!(
                            "{} declared before {}",
                            self.current_unit_id().describe(),
                            named_entity.describe()
                        )),
                        ErrorCode::DeclaredBefore,
                    );
                }
            }
        };

        self.define(
            &mut unit.ident,
            self.work_library(),
            AnyEntKind::Design(Design::Configuration),
            src_span,
        );

        Ok(())
    }

    fn analyze_package(
        &self,
        unit: &mut PackageDeclaration,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        let ent = self.arena.explicit(
            unit.name().clone(),
            self.work_library(),
            AnyEntKind::Design(Design::Package(Visibility::default(), Region::default())),
            Some(unit.ident_pos(self.ctx)),
            unit.span(),
            Some(self.source()),
        );

        unit.ident.decl.set(ent.id());

        let root_scope = Scope::default();
        self.add_implicit_context_clause(&root_scope)?;
        self.analyze_context_clause(&root_scope, &mut unit.context_clause, diagnostics)?;

        root_scope.add(ent, diagnostics);
        let scope = root_scope.nested().in_package_declaration();

        if let Some(ref mut list) = unit.generic_clause {
            self.analyze_interface_list(&scope, ent, list, diagnostics)?;
        }
        self.analyze_declarative_part(&scope, ent, &mut unit.decl, diagnostics)?;

        if !self.has_package_body() {
            scope.close(diagnostics);
        }

        let region = scope.into_region();
        let visibility = root_scope.into_visibility();

        let kind = if unit.generic_clause.is_some() {
            AnyEntKind::Design(Design::UninstPackage(visibility, region))
        } else {
            AnyEntKind::Design(Design::Package(visibility, region))
        };

        unsafe {
            ent.set_kind(kind);
        }

        Ok(())
    }

    fn analyze_package_instance(
        &self,
        unit: &mut PackageInstantiation,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        let ent = self.arena.explicit(
            unit.name().clone(),
            self.work_library(),
            AnyEntKind::Design(Design::PackageInstance(Region::default())),
            Some(unit.ident_pos(self.ctx)),
            unit.span(),
            Some(self.source()),
        );

        unit.ident.decl.set(ent.id());
        let root_scope = Scope::default();
        self.add_implicit_context_clause(&root_scope)?;

        self.analyze_context_clause(&root_scope, &mut unit.context_clause, diagnostics)?;

        if let Some(pkg_region) =
            as_fatal(self.generic_package_instance(&root_scope, ent, unit, diagnostics))?
        {
            let kind = AnyEntKind::Design(Design::PackageInstance(pkg_region));

            unsafe {
                ent.set_kind(kind);
            }
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
        let src_span = unit.span();
        self.analyze_context_clause(&scope, &mut unit.items, diagnostics)?;

        self.define(
            &mut unit.ident,
            self.work_library(),
            AnyEntKind::Design(Design::Context(scope.into_region())),
            src_span,
        );

        Ok(())
    }

    fn analyze_architecture(
        &self,
        unit: &mut ArchitectureBody,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        let src_span = unit.span();
        let Some(primary) = as_fatal(self.lookup_in_library(
            diagnostics,
            self.work_library_name(),
            unit.entity_name.item.pos(self.ctx),
            &Designator::Identifier(unit.entity_name.item.item.clone()),
        ))?
        else {
            return Ok(());
        };
        unit.entity_name.set_unique_reference(primary.into());
        self.check_secondary_before_primary(&primary, unit.ident_pos(self.ctx), diagnostics);

        let (visibility, region) =
            if let Design::Entity(ref visibility, ref region) = primary.kind() {
                (visibility, region)
            } else {
                let mut diagnostic =
                    Diagnostic::mismatched_kinds(unit.ident_pos(self.ctx), "Expected an entity");

                if let Some(pos) = primary.decl_pos() {
                    diagnostic.add_related(pos, format!("Found {}", primary.describe()))
                }

                return Ok(());
            };

        let root_scope = Scope::new(Region::with_visibility(visibility.clone()));
        self.analyze_context_clause(&root_scope, &mut unit.context_clause, diagnostics)?;

        let arch = self.define(
            &mut unit.ident,
            primary.into(),
            AnyEntKind::Design(Design::Architecture(
                Visibility::default(),
                Region::default(),
                primary,
            )),
            src_span,
        );

        root_scope.add(arch, diagnostics);

        let scope = Scope::extend(region, Some(&root_scope));

        // Entity name is visible
        scope.make_potentially_visible(primary.decl_pos(), primary.into());

        self.define_labels_for_concurrent_part(&scope, arch, &mut unit.statements, diagnostics)?;
        self.analyze_declarative_part(&scope, arch, &mut unit.decl, diagnostics)?;
        self.analyze_concurrent_part(&scope, arch, &mut unit.statements, diagnostics)?;
        scope.close(diagnostics);

        let region = scope.into_region();
        let visibility = root_scope.into_visibility();

        unsafe {
            arch.set_kind(AnyEntKind::Design(Design::Architecture(
                visibility, region, primary,
            )))
        }
        Ok(())
    }

    fn analyze_package_body(
        &self,
        unit: &mut PackageBody,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        let Some(primary) = as_fatal(self.lookup_in_library(
            diagnostics,
            self.work_library_name(),
            unit.ident_pos(self.ctx),
            &Designator::Identifier(unit.ident.tree.item.clone()),
        ))?
        else {
            return Ok(());
        };

        let (visibility, region) = match primary.kind() {
            Design::Package(ref visibility, ref region)
            | Design::UninstPackage(ref visibility, ref region) => (visibility, region),
            _ => {
                let mut diagnostic =
                    Diagnostic::mismatched_kinds(unit.ident_pos(self.ctx), "Expected a package");

                if let Some(pos) = primary.decl_pos() {
                    diagnostic.add_related(pos, format!("Found {}", primary.describe()))
                }

                return Ok(());
            }
        };

        let body = self.arena.alloc(
            unit.ident.name().clone().into(),
            Some(self.work_library()),
            Related::DeclaredBy(primary.into()),
            AnyEntKind::Design(Design::PackageBody(
                Visibility::default(),
                Region::default(),
            )),
            Some(unit.ident_pos(self.ctx).clone()),
            unit.span(),
            Some(self.source()),
        );
        unit.ident.decl.set(body.id());

        self.check_secondary_before_primary(&primary, unit.ident_pos(self.ctx), diagnostics);

        // @TODO make pattern of primary/secondary extension
        let root_scope = Scope::new(Region::with_visibility(visibility.clone()));

        self.analyze_context_clause(&root_scope, &mut unit.context_clause, diagnostics)?;

        let scope = Scope::extend(region, Some(&root_scope));

        // Package name is visible
        scope.make_potentially_visible(primary.decl_pos(), primary.into());

        self.analyze_declarative_part(&scope, body, &mut unit.decl, diagnostics)?;
        scope.close(diagnostics);
        let region = scope.into_region();
        let visibility = root_scope.into_visibility();

        unsafe { body.set_kind(AnyEntKind::Design(Design::PackageBody(visibility, region))) }
        Ok(())
    }

    fn check_secondary_before_primary(
        &self,
        primary: &DesignEnt<'_>,
        secondary_pos: &SrcPos,
        diagnostics: &mut dyn DiagnosticHandler,
    ) {
        if let Some(primary_pos) = primary.decl_pos() {
            if primary_pos.source == secondary_pos.source
                && primary_pos.start() > secondary_pos.start()
            {
                diagnostics.add(
                    secondary_pos,
                    format!(
                        "{} declared before {}",
                        capitalize(&self.current_unit_id().describe()),
                        primary.describe(),
                    ),
                    ErrorCode::DeclaredBefore,
                );
            }
        }
    }

    fn lookup_entity_for_configuration(
        &self,
        scope: &Scope<'a>,
        config: &mut ConfigurationDeclaration,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<DesignEnt<'_>> {
        let ent_name = &mut config.entity_name;
        let ent_name_span = ent_name.span;

        match ent_name.item {
            // Entities are implicitly defined for configurations
            // configuration cfg of ent
            Name::Designator(ref mut designator) => Ok(self
                .lookup_in_library(
                    diagnostics,
                    self.work_library_name(),
                    &ent_name_span.pos(self.ctx),
                    &designator.item,
                )
                .inspect(|design| designator.set_unique_reference(design.0))?),

            // configuration cfg of lib.ent
            Name::Selected(ref mut prefix, ref mut designator) => {
                let name = self.name_resolve(scope, prefix.span, &mut prefix.item, diagnostics)?;
                match name {
                    ResolvedName::Library(ref library_name) => {
                        if library_name != self.work_library_name() {
                            diagnostics.add(
                                prefix.pos(self.ctx),
                                format!("Configuration must be within the same library '{}' as the corresponding entity", self.work_library_name()), ErrorCode::ConfigNotInSameLibrary);
                            Err(EvalError::Unknown)
                        } else {
                            let primary_ent = self.lookup_in_library(
                                diagnostics,
                                library_name,
                                designator.pos(self.ctx),
                                &designator.item.item,
                            )?;
                            designator
                                .item
                                .reference
                                .set_unique_reference(primary_ent.into());
                            match primary_ent.kind() {
                                Design::Entity(..) => Ok(primary_ent),
                                _ => {
                                    diagnostics.add(
                                        designator.pos(self.ctx),
                                        format!(
                                            "{} does not denote an entity",
                                            primary_ent.describe()
                                        ),
                                        ErrorCode::MismatchedKinds,
                                    );
                                    Err(EvalError::Unknown)
                                }
                            }
                        }
                    }
                    other => {
                        diagnostics.push(other.kind_error(prefix.pos(self.ctx), "library"));
                        Err(EvalError::Unknown)
                    }
                }
            }
            _ => {
                diagnostics.add(
                    ent_name.pos(self.ctx),
                    "Expected selected name",
                    ErrorCode::MismatchedKinds,
                );
                Err(EvalError::Unknown)
            }
        }
    }

    fn resolve_context_item_prefix(
        &self,
        diagnostics: &mut dyn DiagnosticHandler,
        scope: &Scope<'a>,
        prefix: &mut WithTokenSpan<Name>,
    ) -> EvalResult<EntRef<'a>> {
        match self.resolve_context_item_name(diagnostics, scope, prefix)? {
            UsedNames::Single(visible) => match visible.into_non_overloaded() {
                Ok(ent_ref) => Ok(ent_ref),
                Err(_) => {
                    bail!(
                        diagnostics,
                        Diagnostic::mismatched_kinds(
                            prefix.pos(self.ctx),
                            "Invalid prefix of a selected name",
                        )
                    );
                }
            },
            UsedNames::AllWithin(..) => {
                bail!(
                    diagnostics,
                    Diagnostic::mismatched_kinds(
                        prefix.pos(self.ctx),
                        "'.all' may not be the prefix of a selected name",
                    )
                );
            }
        }
    }

    fn resolve_context_item_name(
        &self,
        diagnostics: &mut dyn DiagnosticHandler,
        scope: &Scope<'a>,
        name: &mut WithTokenSpan<Name>,
    ) -> EvalResult<UsedNames<'a>> {
        match &mut name.item {
            Name::Selected(ref mut prefix, ref mut suffix) => {
                let prefix_ent = self.resolve_context_item_prefix(diagnostics, scope, prefix)?;

                let visible = self.lookup_selected(diagnostics, prefix.span, prefix_ent, suffix)?;
                suffix.set_reference(&visible);
                Ok(UsedNames::Single(visible))
            }

            Name::SelectedAll(prefix) => {
                let prefix_ent = self.resolve_context_item_prefix(diagnostics, scope, prefix)?;
                Ok(UsedNames::AllWithin(prefix.pos(self.ctx), prefix_ent))
            }
            Name::Designator(designator) => {
                let visible = scope
                    .lookup(designator.designator())
                    .map_err(|err| err.into_diagnostic(self.ctx, name.span))
                    .into_eval_result(diagnostics)?;
                designator.set_reference(&visible);
                Ok(UsedNames::Single(visible))
            }

            Name::Slice(..)
            | Name::Attribute(..)
            | Name::CallOrIndexed(..)
            | Name::External(..) => {
                bail!(
                    diagnostics,
                    Diagnostic::mismatched_kinds(name.pos(self.ctx), "Invalid selected name",)
                );
            }
        }
    }

    pub(crate) fn analyze_context_clause(
        &self,
        scope: &Scope<'a>,
        context_clause: &mut [ContextItem],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        for context_item in context_clause.iter_mut() {
            match context_item {
                ContextItem::Library(LibraryClause {
                    ref mut name_list, ..
                }) => {
                    for library_name in name_list.iter_mut() {
                        if self.work_sym == library_name.item.item {
                            library_name.set_unique_reference(self.work_library());
                            diagnostics.add(
                                library_name.item.pos(self.ctx),
                                "Library clause not necessary for current working library",
                                ErrorCode::UnnecessaryWorkLibrary,
                            )
                        } else if let Some(library) = self.get_library(&library_name.item.item) {
                            library_name.set_unique_reference(library);
                            scope.make_potentially_visible(
                                Some(library_name.item.pos(self.ctx)),
                                library,
                            );
                        } else {
                            diagnostics.add(
                                library_name.item.pos(self.ctx),
                                format!("No such library '{}'", library_name.item),
                                ErrorCode::Unresolved,
                            );
                        }
                    }
                }
                ContextItem::Use(ref mut use_clause) => {
                    self.analyze_use_clause(scope, use_clause, diagnostics)?;
                }
                ContextItem::Context(ContextReference {
                    ref mut name_list, ..
                }) => {
                    for name in name_list.iter_mut() {
                        match name.item {
                            Name::Selected(..) => {}
                            _ => {
                                diagnostics.add(
                                    name.pos(self.ctx),
                                    "Context reference must be a selected name",
                                    ErrorCode::MismatchedKinds,
                                );
                                continue;
                            }
                        }

                        let Some(context_item) =
                            as_fatal(self.resolve_context_item_name(diagnostics, scope, name))?
                        else {
                            continue;
                        };

                        match context_item {
                            UsedNames::Single(visible) => {
                                let ent = visible.first();
                                match ent.kind() {
                                    // OK
                                    AnyEntKind::Design(Design::Context(ref context_region)) => {
                                        scope.add_context_visibility(
                                            Some(&name.pos(self.ctx)),
                                            context_region,
                                        );
                                    }
                                    _ => {
                                        if let Name::Selected(_, ref suffix) = name.item {
                                            diagnostics.add(
                                                suffix.pos(self.ctx),
                                                format!(
                                                    "{} does not denote a context declaration",
                                                    ent.describe()
                                                ),
                                                ErrorCode::MismatchedKinds,
                                            );
                                        }
                                    }
                                }
                            }
                            UsedNames::AllWithin(..) => {
                                // Handled above
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
                    diagnostics.add(
                        name.pos(self.ctx),
                        "Use clause must be a selected name",
                        ErrorCode::MismatchedKinds,
                    );
                    // We still want to resolve the name,
                    // so that it is available for completion purposes.
                    // We ignore the errors here, since there is already a diagnostic at that position.
                    let mut empty_diag = Vec::new();
                    let _ = self.name_resolve(scope, name.span(), &mut name.item, &mut empty_diag);
                    continue;
                }
            }

            let Some(context_item) =
                as_fatal(self.resolve_context_item_name(diagnostics, scope, name))?
            else {
                continue;
            };
            match context_item {
                UsedNames::Single(visible) => {
                    visible.make_potentially_visible_in(Some(&name.pos(self.ctx)), scope);
                }
                UsedNames::AllWithin(visibility_pos, named_entity) => match named_entity.kind() {
                    AnyEntKind::Library => {
                        let library_name = named_entity.designator().expect_identifier();
                        self.use_all_in_library(&name.pos(self.ctx), library_name, scope)?;
                    }
                    AnyEntKind::Design(design) => match design {
                        Design::UninstPackage(..) => {
                            diagnostics.push(Diagnostic::invalid_selected_name_prefix(
                                named_entity,
                                &visibility_pos,
                            ));
                        }
                        Design::Package(_, ref primary_region)
                        | Design::PackageInstance(ref primary_region)
                        | Design::InterfacePackageInstance(ref primary_region) => {
                            scope.make_all_potentially_visible(
                                Some(&name.pos(self.ctx)),
                                primary_region,
                            );
                        }
                        _ => {
                            diagnostics.add(
                                visibility_pos,
                                "Invalid prefix for selected name",
                                ErrorCode::MismatchedKinds,
                            );
                        }
                    },

                    _ => {
                        diagnostics.add(
                            visibility_pos,
                            "Invalid prefix for selected name",
                            ErrorCode::MismatchedKinds,
                        );
                    }
                },
            }
        }

        Ok(())
    }

    /// Returns a reference to the uninstantiated package
    pub fn analyze_package_instance_name(
        &self,
        scope: &Scope<'a>,
        package_name: &mut WithTokenSpan<Name>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<Region<'a>> {
        let name = self.name_resolve(
            scope,
            package_name.span,
            &mut package_name.item,
            diagnostics,
        )?;
        if let ResolvedName::Design(ref unit) = name {
            if let AnyEntKind::Design(Design::UninstPackage(_, package_region)) = &unit.kind {
                return Ok(package_region.clone());
            }
        }
        diagnostics.add(
            package_name.pos(self.ctx),
            format!("'{package_name}' is not an uninstantiated generic package"),
            ErrorCode::MismatchedKinds,
        );
        Err(EvalError::Unknown)
    }
}

pub enum UsedNames<'a> {
    /// A single name was used selected
    Single(NamedEntities<'a>),
    /// All names within was selected
    /// @TODO add pos for where declaration was made visible into VisibleDeclaration
    AllWithin(SrcPos, EntRef<'a>),
}
