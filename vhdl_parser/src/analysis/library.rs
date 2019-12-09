// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use self::fnv::FnvHashMap;
use fnv;
use std::collections::hash_map::Entry;

use super::declarative_region::{AnyDeclaration, DeclarativeRegion};
use super::lock::AnalysisLock;
use crate::ast::*;
use crate::diagnostic::{Diagnostic, DiagnosticHandler};

/// A design unit with design unit data
#[cfg_attr(test, derive(PartialEq, Debug, Clone))]
pub struct AnalysisData<T> {
    pub diagnostics: Vec<Diagnostic>,
    pub region: DeclarativeRegion<'static>,
    pub ast: T,
}

impl<T> AnalysisData<T> {
    fn new(ast: T) -> AnalysisData<T> {
        AnalysisData {
            diagnostics: Vec::new(),
            region: DeclarativeRegion::default(),
            ast,
        }
    }

    /// Clear data for new analysis, keeping ast
    fn reset(&mut self) {
        // Clear region and diagnostics
        self.region = DeclarativeRegion::default();
        self.diagnostics = Vec::new();
        // Keep ast
    }
}

pub trait HasUnit<T> {
    fn unit(&self) -> &T;
}

impl<T> HasUnit<T> for DesignUnit<T> {
    fn unit(&self) -> &T {
        &self.unit
    }
}

impl HasUnit<ContextDeclaration> for ContextDeclaration {
    fn unit(&self) -> &ContextDeclaration {
        &self
    }
}

impl<T, U: HasUnit<T>> HasUnit<T> for AnalysisData<U> {
    fn unit(&self) -> &T {
        self.ast.unit()
    }
}

pub type LockedData<T> = AnalysisLock<AnalysisData<T>>;

/// Reset analysis state of unit
fn reset_data<T>(lock: &mut AnalysisLock<AnalysisData<T>>) {
    lock.reset(&|data| data.reset());
}

impl<T: HasIdent> HasIdent for AnalysisData<T> {
    fn ident(&self) -> &Ident {
        self.ast.ident()
    }
}

pub struct PrimaryUnit<T> {
    ident: Ident,
    pub data: LockedData<T>,
}

impl<T: HasIdent> PrimaryUnit<T> {
    fn new(unit: T) -> PrimaryUnit<T> {
        PrimaryUnit {
            ident: unit.ident().clone(),
            data: AnalysisLock::new(AnalysisData::new(unit)),
        }
    }
}

impl<T> HasIdent for PrimaryUnit<T> {
    fn ident(&self) -> &Ident {
        &self.ident
    }
}

pub struct SecondaryUnit<T> {
    ident: Ident,
    primary_ident: Ident,
    pub data: LockedData<T>,
}

impl<T: HasIdent + HasPrimaryIdent> SecondaryUnit<T> {
    fn new(unit: T) -> SecondaryUnit<T> {
        SecondaryUnit {
            ident: unit.ident().clone(),
            primary_ident: unit.primary_ident().clone(),
            data: AnalysisLock::new(AnalysisData::new(unit)),
        }
    }
}

impl<T> HasIdent for SecondaryUnit<T> {
    fn ident(&self) -> &Ident {
        &self.ident
    }
}

impl<T> HasPrimaryIdent for SecondaryUnit<T> {
    fn primary_ident(&self) -> &Ident {
        &self.primary_ident
    }
}

pub type EntityData = AnalysisData<EntityUnit>;
pub type ArchitectureData = AnalysisData<ArchitectureUnit>;
pub type ConfigurationData = AnalysisData<ConfigurationUnit>;
pub type ContextData = AnalysisData<ContextDeclaration>;
pub type PackageData = AnalysisData<PackageUnit>;
pub type PackageBodyData = AnalysisData<PackageBodyUnit>;
pub type PackageInstanceData = AnalysisData<PackageInstanceUnit>;

pub type Entity = PrimaryUnit<EntityUnit>;
pub type Architecture = SecondaryUnit<ArchitectureUnit>;
pub type Configuration = PrimaryUnit<ConfigurationUnit>;
pub type Context = PrimaryUnit<ContextDeclaration>;
pub type Package = PrimaryUnit<PackageUnit>;
pub type PackageBody = SecondaryUnit<PackageBodyUnit>;
pub type PackageInstance = PrimaryUnit<PackageInstanceUnit>;

pub type SymbolMap<T> = FnvHashMap<Symbol, T>;

pub struct Library {
    pub name: Symbol,
    pub region: DeclarativeRegion<'static>,
    primary_names: SymbolMap<SrcPos>,
    entities: SymbolMap<Entity>,
    architectures: SymbolMap<SymbolMap<Architecture>>,
    configurations: SymbolMap<Configuration>,
    packages: SymbolMap<Package>,
    package_bodies: SymbolMap<PackageBody>,
    uninst_packages: SymbolMap<Package>,
    package_instances: SymbolMap<PackageInstance>,
    contexts: SymbolMap<Context>,

    /// Design units which were not added since they were duplicates
    /// They need to be kept for later refresh which might make them not duplicates
    duplicates: Vec<(SrcPos, AnyDesignUnit)>,
}

impl<'a> Library {
    fn new(name: Symbol) -> Library {
        Library {
            name,
            region: DeclarativeRegion::default(),
            primary_names: SymbolMap::default(),
            entities: SymbolMap::default(),
            architectures: SymbolMap::default(),
            configurations: SymbolMap::default(),
            packages: SymbolMap::default(),
            package_bodies: SymbolMap::default(),
            uninst_packages: SymbolMap::default(),
            package_instances: SymbolMap::default(),
            contexts: SymbolMap::default(),
            duplicates: Vec::new(),
        }
    }

    fn add_primary_unit(&mut self, primary_unit: AnyPrimaryUnit) {
        match self.primary_names.entry(primary_unit.name().clone()) {
            Entry::Occupied(entry) => {
                self.duplicates.push((
                    entry.get().pos().clone(),
                    AnyDesignUnit::Primary(primary_unit),
                ));
                return;
            }
            Entry::Vacant(entry) => {
                entry.insert(primary_unit.pos().clone());
            }
        }

        match primary_unit {
            AnyPrimaryUnit::EntityDeclaration(entity) => {
                // Ensure entity without architecure has an entry with empty architecture map
                self.architectures
                    .entry(entity.name().clone())
                    .or_insert_with(|| SymbolMap::default());
                self.entities
                    .insert(entity.name().clone(), PrimaryUnit::new(entity));
            }
            AnyPrimaryUnit::PackageDeclaration(package) => {
                let map = if package.unit.generic_clause.is_some() {
                    &mut self.uninst_packages
                } else {
                    &mut self.packages
                };
                map.insert(package.name().clone(), PrimaryUnit::new(package));
            }
            AnyPrimaryUnit::PackageInstance(inst) => {
                self.package_instances
                    .insert(inst.name().clone(), PrimaryUnit::new(inst));
            }
            AnyPrimaryUnit::ContextDeclaration(ctx) => {
                self.contexts
                    .insert(ctx.name().clone(), PrimaryUnit::new(ctx));
            }

            AnyPrimaryUnit::Configuration(config) => {
                self.configurations
                    .insert(config.name().clone(), PrimaryUnit::new(config));
            }
        }
    }

    fn add_secondary_unit(&mut self, secondary_unit: AnySecondaryUnit) {
        match secondary_unit {
            AnySecondaryUnit::Architecture(architecture) => {
                match self
                    .architectures
                    .entry(architecture.primary_name().clone())
                {
                    Entry::Occupied(map_entry) => {
                        let map = map_entry.into_mut();
                        match map.entry(architecture.name().clone()) {
                            Entry::Occupied(arch_entry) => {
                                self.duplicates.push((
                                    arch_entry.get().pos().clone(),
                                    AnyDesignUnit::Secondary(AnySecondaryUnit::Architecture(
                                        architecture,
                                    )),
                                ));
                            }
                            Entry::Vacant(entry) => {
                                entry.insert(SecondaryUnit::new(architecture));
                            }
                        }
                    }
                    Entry::Vacant(entry) => {
                        let mut map = SymbolMap::default();
                        map.insert(
                            architecture.name().clone(),
                            SecondaryUnit::new(architecture),
                        );
                        entry.insert(map);
                    }
                }
            }
            AnySecondaryUnit::PackageBody(body) => {
                match self.package_bodies.entry(body.name().clone()) {
                    Entry::Occupied(entry) => {
                        self.duplicates.push((
                            entry.get().pos().clone(),
                            AnyDesignUnit::Secondary(AnySecondaryUnit::PackageBody(body)),
                        ));
                    }
                    Entry::Vacant(entry) => {
                        entry.insert(SecondaryUnit::new(body));
                    }
                }
            }
        };
    }

    pub fn add_design_unit(&mut self, design_unit: AnyDesignUnit) {
        match design_unit {
            AnyDesignUnit::Primary(primary) => self.add_primary_unit(primary),
            AnyDesignUnit::Secondary(secondary) => self.add_secondary_unit(secondary),
        };
    }

    pub fn add_design_file(&mut self, design_file: DesignFile) {
        for design_unit in design_file.design_units {
            self.add_design_unit(design_unit);
        }
    }

    /// Validate library after removing or adding new design units
    pub fn refresh(&mut self, diagnostics: &mut dyn DiagnosticHandler) {
        self.append_duplicate_diagnostics(diagnostics);
        self.validate_package_body(diagnostics);
        self.validate_entity_architecture(diagnostics);
        self.reset();
        self.rebuild_region();
    }

    /// Reset analysis state of all design units
    fn reset(&mut self) {
        for entity in self.entities.values_mut() {
            reset_data(&mut entity.data);
        }
        for archs in self.architectures.values_mut() {
            for arch in archs.values_mut() {
                reset_data(&mut arch.data);
            }
        }
        for config in self.configurations.values_mut() {
            reset_data(&mut config.data);
        }
        for pkg in self.packages.values_mut() {
            reset_data(&mut pkg.data);
        }
        for pkg in self.uninst_packages.values_mut() {
            reset_data(&mut pkg.data);
        }
        for body in self.package_bodies.values_mut() {
            reset_data(&mut body.data);
        }
        for inst in self.package_instances.values_mut() {
            reset_data(&mut inst.data);
        }
        for ctx in self.contexts.values_mut() {
            reset_data(&mut ctx.data);
        }
    }

    fn append_duplicate_diagnostics(&self, diagnostics: &mut dyn DiagnosticHandler) {
        for (prev_pos, design_unit) in self.duplicates.iter() {
            let diagnostic = match design_unit {
                AnyDesignUnit::Primary(primary_unit) => Diagnostic::error(
                    primary_unit.pos(),
                    format!(
                        "A primary unit has already been declared with name '{}' in library '{}'",
                        primary_unit.name(),
                        &self.name
                    ),
                ),
                AnyDesignUnit::Secondary(secondary_unit) => match secondary_unit {
                    AnySecondaryUnit::Architecture(arch) => Diagnostic::error(
                        &arch.ident(),
                        format!(
                            "Duplicate architecture '{}' of entity '{}'",
                            arch.name(),
                            arch.primary_name(),
                        ),
                    ),
                    AnySecondaryUnit::PackageBody(body) => Diagnostic::error(
                        body.pos(),
                        format!("Duplicate package body of package '{}'", body.name()),
                    ),
                },
            };

            let diagnostic = diagnostic.related(prev_pos, "Previously defined here");
            diagnostics.push(diagnostic);
        }
    }

    fn validate_package_body(&self, diagnostics: &mut dyn DiagnosticHandler) {
        for body in self.package_bodies.values() {
            let package = {
                if let Some(package) = self.packages.get(&body.name()) {
                    package
                } else if let Some(package) = self.uninst_packages.get(&body.name()) {
                    package
                } else {
                    diagnostics.push(Diagnostic::error(
                        &body.ident(),
                        format!(
                            "No package '{}' within library '{}'",
                            &body.name(),
                            &self.name
                        ),
                    ));
                    continue;
                }
            };

            let primary_pos = &package.pos();
            let secondary_pos = &body.pos();
            if primary_pos.source == secondary_pos.source && primary_pos.start > secondary_pos.start
            {
                diagnostics.push(Diagnostic::error(
                    secondary_pos,
                    format!("Package body declared before package '{}'", package.name()),
                ));
            }
        }
    }

    fn validate_entity_architecture(&self, diagnostics: &mut dyn DiagnosticHandler) {
        for (entity_name, architectures) in self.architectures.iter() {
            if self.entities.get(&entity_name).is_none() {
                for architecture in architectures.values() {
                    diagnostics.push(Diagnostic::error(
                        &architecture.primary_pos(),
                        format!(
                            "No entity '{}' within library '{}'",
                            entity_name, &self.name
                        ),
                    ));
                }
            }
        }

        for (entity_name, entity) in self.entities.iter() {
            if let Some(architectures) = self.architectures.get(entity_name) {
                for architecture in architectures.values() {
                    let primary_pos = entity.ident.pos();
                    let secondary_pos = &architecture.pos();
                    if primary_pos.source == secondary_pos.source
                        && primary_pos.start > secondary_pos.start
                    {
                        diagnostics.push(Diagnostic::error(
                            secondary_pos,
                            format!(
                                "Architecture '{}' declared before entity '{}'",
                                &architecture.name(),
                                entity.name()
                            ),
                        ));
                    }
                }
            };
        }
    }

    fn rebuild_region(&mut self) {
        let mut diagnostics = Vec::new();
        self.region = DeclarativeRegion::default();

        for ctx in self.contexts.values() {
            self.region.add(
                ctx.ident(),
                AnyDeclaration::Context(self.name.clone(), ctx.name().clone()),
                &mut diagnostics,
            );
        }

        for config in self.configurations.values() {
            self.region
                .add(config.ident(), AnyDeclaration::Other, &mut diagnostics);
        }

        for pkg in self.packages.values() {
            self.region.add(
                pkg.ident(),
                AnyDeclaration::Package(self.name.clone(), pkg.name().clone()),
                &mut diagnostics,
            );
        }

        for pkg in self.uninst_packages.values() {
            self.region.add(
                pkg.ident(),
                AnyDeclaration::UninstPackage(self.name.clone(), pkg.name().clone()),
                &mut diagnostics,
            );
        }

        for ent in self.entities.values() {
            self.region.add(
                ent.ident(),
                AnyDeclaration::Entity(self.name.clone(), ent.name().clone()),
                &mut diagnostics,
            );
        }

        for pkg in self.package_instances.values() {
            self.region.add(
                pkg.ident(),
                AnyDeclaration::PackageInstance(self.name.clone(), pkg.name().clone()),
                &mut diagnostics,
            );
        }

        assert!(
            diagnostics.is_empty(),
            "Expect no diagnostics when building library region"
        );
    }

    fn remove_source_from<T: HasSource>(map: &mut SymbolMap<T>, source: &Source) {
        map.retain(|_, value| value.source() != source);
    }

    /// Remove all design units defined in source
    /// This is used for incremental analysis where only a single source file is updated
    pub fn remove_source(&mut self, source: &Source) {
        Self::remove_source_from(&mut self.primary_names, source);
        Self::remove_source_from(&mut self.entities, source);
        for architectures in self.architectures.values_mut() {
            Self::remove_source_from(architectures, source);
        }
        // To not leak when both architecture and entity is removed
        self.architectures.retain(|_, value| !value.is_empty());
        Self::remove_source_from(&mut self.configurations, source);
        Self::remove_source_from(&mut self.packages, source);
        Self::remove_source_from(&mut self.package_bodies, source);
        Self::remove_source_from(&mut self.uninst_packages, source);
        Self::remove_source_from(&mut self.package_instances, source);
        Self::remove_source_from(&mut self.contexts, source);

        self.duplicates
            .retain(|(_, value)| value.source() != source);

        // Try to add duplicates that were duplicated by a design unit in the removed file
        let num_duplicates = self.duplicates.len();
        let duplicates =
            std::mem::replace(&mut self.duplicates, Vec::with_capacity(num_duplicates));
        for (prev_pos, design_unit) in duplicates.into_iter() {
            if prev_pos.source() == source {
                self.add_design_unit(design_unit)
            } else {
                self.duplicates.push((prev_pos, design_unit));
            }
        }
    }

    pub fn entity(&'a self, name: &Symbol) -> Option<&'a Entity> {
        self.entities.get(name)
    }

    #[cfg(test)]
    pub fn configuration(&'a self, name: &Symbol) -> Option<&'a Configuration> {
        self.configurations.get(name)
    }

    /// Return a non-generic packate
    #[cfg(test)]
    pub fn package(&'a self, name: &Symbol) -> Option<&'a Package> {
        self.packages.get(name)
    }

    pub fn expect_any_package(&'a self, name: &Symbol) -> &'a Package {
        self.packages
            .get(name)
            .or_else(|| self.uninst_packages.get(name))
            .expect("Package must exist")
    }

    #[cfg(test)]
    pub fn package_instance(&'a self, name: &Symbol) -> Option<&'a PackageInstance> {
        self.package_instances.get(name)
    }

    pub fn expect_package_instance(&'a self, name: &Symbol) -> &'a PackageInstance {
        self.package_instances
            .get(name)
            .expect("Package instance must exist")
    }

    pub fn package_body(&'a self, name: &Symbol) -> Option<&'a PackageBody> {
        self.package_bodies.get(name)
    }

    pub fn context(&'a self, name: &Symbol) -> Option<&'a Context> {
        self.contexts.get(name)
    }

    pub fn entities(&self) -> impl Iterator<Item = &Entity> {
        self.entities.values()
    }

    // @TODO add entity reference wrapper type
    pub fn architectures(&'a self, entity_name: &Symbol) -> &'a SymbolMap<Architecture> {
        self.architectures
            .get(entity_name)
            .expect("Entity must be defined")
    }

    pub fn configurations(&self) -> impl Iterator<Item = &Configuration> {
        self.configurations.values()
    }

    /// Iterate over packages
    pub fn packages(&self) -> impl Iterator<Item = &Package> {
        self.packages.values()
    }

    /// Iterate uninstantiated packages
    pub fn uninst_packages(&self) -> impl Iterator<Item = &Package> {
        self.uninst_packages.values()
    }

    pub fn package_instances(&self) -> impl Iterator<Item = &PackageInstance> {
        self.package_instances.values()
    }

    pub fn contexts(&self) -> impl Iterator<Item = &Context> {
        self.contexts.values()
    }
}

pub struct DesignRoot {
    libraries: SymbolMap<Library>,
}

impl DesignRoot {
    pub fn new() -> DesignRoot {
        DesignRoot {
            libraries: SymbolMap::default(),
        }
    }

    /// Create library if it does not exist or return existing
    pub fn ensure_library(&mut self, name: Symbol) -> &mut Library {
        match self.libraries.entry(name) {
            Entry::Occupied(entry) => entry.into_mut(),
            Entry::Vacant(entry) => {
                let library = Library::new(entry.key().clone());
                entry.insert(library)
            }
        }
    }

    pub fn get_library(&self, library_name: &Symbol) -> Option<&Library> {
        self.libraries.get(library_name)
    }

    pub fn expect_library(&self, library_name: &Symbol) -> &Library {
        self.get_library(library_name)
            .expect("Library must be defined")
    }

    pub fn iter_libraries(&self) -> impl Iterator<Item = &Library> {
        self.libraries.values()
    }

    pub fn iter_libraries_mut(&mut self) -> impl Iterator<Item = &mut Library> {
        self.libraries.values_mut()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::{check_diagnostics, check_no_diagnostics, Code, CodeBuilder};

    fn new_library_with_diagnostics<'a>(code: &Code, name: &str) -> (Library, Vec<Diagnostic>) {
        let mut diagnostics = Vec::new();
        let mut library = Library::new(code.symbol(name));
        library.add_design_file(code.design_file());
        library.refresh(&mut diagnostics);
        (library, diagnostics)
    }

    fn new_library<'a>(code: &Code, name: &str) -> Library {
        let (library, diagnostics) = new_library_with_diagnostics(code, name);
        check_no_diagnostics(&diagnostics);
        library
    }

    #[test]
    fn add_entity() {
        let code = Code::new(
            "
entity ent is
end entity;
",
        );
        let library = new_library(&code, "libname");
        let primary = library.entity(&code.symbol("ent")).unwrap();
        assert_eq!(primary.data.expect_read().unit(), &code.entity());
        assert!(library.architectures(primary.name()).is_empty());
    }

    #[test]
    fn error_on_package_body_without_package() {
        let code = Code::new(
            "
package body pkg is
end package body;
",
        );
        let (library, diagnostics) = new_library_with_diagnostics(&code, "libname");

        assert_eq!(library.packages.len(), 0);
        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code.s1("pkg"),
                "No package 'pkg' within library 'libname'",
            )],
        );
    }

    #[test]
    fn error_on_architecture_without_entity() {
        let code = Code::new(
            "
architecture rtl of ent is
begin
end architecture;
",
        );
        let (library, diagnostics) = new_library_with_diagnostics(&code, "libname");

        assert_eq!(library.entities.len(), 0);
        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code.s1("ent"),
                "No entity 'ent' within library 'libname'",
            )],
        );
    }

    #[test]
    fn error_on_architecture_of_package() {
        let code = Code::new(
            "
package pkg is
end package;

architecture rtl of pkg is
begin
end architecture;
",
        );
        let (library, diagnostics) = new_library_with_diagnostics(&code, "libname");

        assert!(library.package_body(&code.symbol("pkg")).is_none());
        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code.s("pkg", 2),
                "No entity 'pkg' within library 'libname'",
            )],
        );
    }

    #[test]
    fn error_on_package_body_of_entity() {
        let code = Code::new(
            "
entity entname is
end entity;

package body entname is
end package body;
",
        );
        let (library, diagnostics) = new_library_with_diagnostics(&code, "libname");

        assert!(library.architectures(&code.symbol("entname")).is_empty());
        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code.s("entname", 2),
                "No package 'entname' within library 'libname'",
            )],
        );
    }

    #[test]
    fn error_on_duplicate_package_body() {
        let code = Code::new(
            "
package pkg is
end package;

package body pkg is
end package body;

package body pkg is
end package body;
",
        );
        let (library, diagnostics) = new_library_with_diagnostics(&code, "libname");

        assert!(library.package_body(&code.symbol("pkg")).is_some());
        check_diagnostics(
            diagnostics,
            vec![
                Diagnostic::error(code.s("pkg", 3), "Duplicate package body of package 'pkg'")
                    .related(code.s("pkg", 2), "Previously defined here"),
            ],
        );
    }

    #[test]
    fn error_on_duplicate_primary_unit() {
        let code = Code::new(
            "
package pkg is
end package;

entity pkg is
end entity;

entity entname is
end entity;

package entname is
end package;

configuration pkg of entname is
  for rtl
  end for;
end configuration;

package pkg is new gpkg generic map (const => foo);
",
        );
        let (library, diagnostics) = new_library_with_diagnostics(&code, "libname");

        assert_eq!(library.entities.len(), 1);
        assert_eq!(library.packages.len(), 1);
        check_diagnostics(
            diagnostics,
            vec![
                Diagnostic::error(
                    code.s("pkg", 2),
                    "A primary unit has already been declared with name 'pkg' in library 'libname'"
                ).related(code.s("pkg", 1), "Previously defined here"),
                Diagnostic::error(
                    code.s("entname", 2),
                    "A primary unit has already been declared with name 'entname' in library 'libname'"
                ).related(code.s("entname", 1), "Previously defined here"),
                Diagnostic::error(
                    code.s("pkg", 3),
                    "A primary unit has already been declared with name 'pkg' in library 'libname'"
                ).related(code.s("pkg", 1), "Previously defined here"),
                Diagnostic::error(
                    code.s("pkg", 4),
                    "A primary unit has already been declared with name 'pkg' in library 'libname'"
                ).related(code.s("pkg", 1), "Previously defined here"),
            ]
        );
    }

    #[test]
    fn error_on_secondary_before_primary_in_same_file() {
        let code = Code::new(
            "
package body pkg is
end package body;

package pkg is
end package;

architecture rtl of entname is
begin
end architecture;

entity entname is
end entity;
",
        );
        let (library, diagnostics) = new_library_with_diagnostics(&code, "libname");

        // Should still be added as a secondary unit
        assert!(library.package_body(&code.symbol("pkg")).is_some());

        check_diagnostics(
            diagnostics,
            vec![
                Diagnostic::error(
                    code.s("pkg", 1),
                    "Package body declared before package 'pkg'",
                ),
                Diagnostic::error(
                    code.s("rtl", 1),
                    "Architecture 'rtl' declared before entity 'entname'",
                ),
            ],
        );
    }

    #[test]
    fn no_error_on_secondary_before_primary_in_different_files() {
        let builder = CodeBuilder::new();
        let file1 = builder.code(
            "
package body pkg is
end package body;
",
        );

        let file2 = builder.code(
            "
package pkg is
end package;
",
        );

        let mut diagnostics = Vec::new();
        let mut library = Library::new(builder.symbol("libname"));
        library.add_design_file(file1.design_file());
        library.add_design_file(file2.design_file());
        library.refresh(&mut diagnostics);

        // Should still be added as a secondary unit
        assert!(library.package_body(&builder.symbol("pkg")).is_some());

        check_no_diagnostics(&diagnostics);
    }

    #[test]
    fn error_on_duplicate_architecture() {
        let code = Code::new(
            "
entity ent is
end ent;

architecture rtl of ent is
begin
end architecture;

architecture rtl of ent is
begin
end architecture;
",
        );
        let (library, diagnostics) = new_library_with_diagnostics(&code, "libname");

        assert_eq!(library.architectures(&code.symbol("ent")).len(), 1);
        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code.s("rtl", 2),
                "Duplicate architecture 'rtl' of entity 'ent'",
            )
            .related(code.s("rtl", 1), "Previously defined here")],
        );
    }

    #[test]
    fn add_entity_architecture() {
        let code = Code::new(
            "
entity ent is
end entity;

architecture arch0 of ent is
begin
end architecture;

architecture arch1 of ent is
begin
end architecture;
",
        );
        let library = new_library(&code, "libname");

        let entity = code.between("entity", "entity;").entity();
        let architecture0 = code
            .between("architecture arch0 ", "architecture;")
            .architecture();
        let architecture1 = code
            .between("architecture arch1 ", "architecture;")
            .architecture();

        let ent = library.entity(&code.symbol("ent")).unwrap();
        let architectures = library.architectures(entity.name());

        assert_eq!(ent.ident, ent.ident);
        assert_eq!(ent.data.expect_read().unit(), &entity);

        assert_eq!(architectures.len(), 2);
        assert_eq!(
            architectures
                .get(architecture0.name())
                .unwrap()
                .data
                .expect_read()
                .unit(),
            &architecture0
        );
        assert_eq!(
            architectures
                .get(architecture1.name())
                .unwrap()
                .data
                .expect_read()
                .unit(),
            &architecture1
        );
    }

    #[test]
    fn add_package_and_package_body() {
        let code = Code::new(
            "
package pkg is
end package;

package body pkg is
end package body;
",
        );
        let library = new_library(&code, "libname");

        let package = code.between("package", "package;").package();
        let body = code.between("package body", "package body;").package_body();

        let primary = library.package(&code.symbol("pkg")).unwrap();
        assert_eq!(primary.ident(), package.ident());
        assert_eq!(primary.data.expect_read().unit(), &package);

        let secondary = library.package_body(&code.symbol("pkg")).unwrap();
        assert_eq!(secondary.ident(), body.ident());
        assert_eq!(secondary.primary_ident(), body.primary_ident());
        assert_eq!(secondary.data.expect_read().unit(), &body);
    }

    #[test]
    fn add_context_clause() {
        let code = Code::new(
            "
context ctx is
end context;
",
        );
        let library = new_library(&code, "libname");
        let primary = library.context(&code.symbol("ctx")).unwrap();
        assert_eq!(primary.data.expect_read().unit(), &code.context());
    }

    #[test]
    fn add_configuration() {
        let code = Code::new(
            "
entity ent is
end entity;

architecture rtl of ent is
begin
end architecture;

configuration cfg of ent is
  for rtl
  end for;
end configuration;
",
        );
        let library = new_library(&code, "libname");

        let config = code
            .between("configuration cfg", "end configuration;")
            .configuration();

        assert_eq!(
            library
                .configuration(&code.symbol("cfg"))
                .unwrap()
                .data
                .expect_read()
                .unit(),
            &config
        );
        assert_eq!(library.configurations.len(), 1);
    }

    #[test]
    fn error_on_duplicate_configuration() {
        let code = Code::new(
            "
entity ent is
end entity;

configuration cfg of ent is
  for rtl
  end for;
end configuration;

configuration cfg of work.ent is
  for rtl
  end for;
end configuration;
",
        );
        let (library, diagnostics) = new_library_with_diagnostics(&code, "libname");

        let config = code
            .between("configuration cfg", "end configuration;")
            .configuration();

        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code.s("cfg", 2),
                "A primary unit has already been declared with name 'cfg' in library 'libname'",
            )
            .related(code.s1("cfg"), "Previously defined here")],
        );

        let got_cfg = library
            .configuration(&code.symbol("cfg"))
            .unwrap()
            .data
            .expect_read();
        assert_eq!(got_cfg.unit(), &config);

        assert_eq!(library.configurations.len(), 1);
    }

    #[test]
    fn add_package_instance() {
        let code = Code::new(
            "
package ipkg is new work.lib.gpkg generic map (const => 1);
",
        );
        let library = new_library(&code, "libname");
        let instance = code.package_instance();

        let got_instance = library.package_instance(&code.symbol("ipkg")).unwrap();
        assert_eq!(got_instance.data.expect_read().unit(), &instance);
    }
}
