// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

// @TODO add related information to message

extern crate fnv;
use self::fnv::FnvHashMap;
use std::collections::hash_map::Entry;

use crate::ast::{
    has_ident::HasIdent, AnyDesignUnit, ArchitectureBody, ConfigurationDeclaration,
    ContextDeclaration, DesignFile, DesignUnit, EntityDeclaration, Name, PackageBody,
    PackageDeclaration, PackageInstantiation, PrimaryUnit, SecondaryUnit,
};
use crate::message::{Message, MessageHandler};
use crate::source::{SrcPos, WithPos};
use crate::symbol_table::Symbol;

impl EntityDesignUnit {
    fn add_architecture(
        &mut self,
        architecture: DesignUnit<ArchitectureBody>,
        messages: &mut MessageHandler,
    ) {
        match self.architectures.entry(architecture.name().clone()) {
            Entry::Occupied(..) => {
                messages.push(Message::error(
                    &architecture.ident(),
                    format!(
                        "Duplicate architecture '{}' of entity '{}'",
                        &architecture.name(),
                        self.entity.name(),
                    ),
                ));
            }
            Entry::Vacant(entry) => {
                {
                    let primary_pos = &self.entity.pos();
                    let secondary_pos = &architecture.pos();
                    if primary_pos.source == secondary_pos.source
                        && primary_pos.start > secondary_pos.start
                    {
                        messages.push(Message::error(
                            secondary_pos,
                            format!(
                                "Architecture '{}' declared before entity '{}'",
                                &architecture.name(),
                                self.entity.name()
                            ),
                        ));
                    }
                };

                entry.insert(architecture);
            }
        }
    }

    fn add_configuration(
        &mut self,
        configuration: DesignUnit<ConfigurationDeclaration>,
        messages: &mut MessageHandler,
    ) {
        match self.configurations.entry(configuration.name().clone()) {
            Entry::Occupied(..) => {
                messages.push(Message::error(
                    &configuration.ident(),
                    format!(
                        "Duplicate configuration '{}' of entity '{}'",
                        &configuration.name(),
                        self.entity.name(),
                    ),
                ));
            }
            Entry::Vacant(entry) => {
                {
                    let primary_pos = &self.entity.pos();
                    let secondary_pos = &configuration.pos();
                    if primary_pos.source == secondary_pos.source
                        && primary_pos.start > secondary_pos.start
                    {
                        messages.push(Message::error(
                            secondary_pos,
                            format!(
                                "Configuration '{}' declared before entity '{}'",
                                &configuration.name(),
                                self.entity.name()
                            ),
                        ));
                    }
                };

                entry.insert(configuration);
            }
        }
    }

    pub fn configurations(&self) -> impl Iterator<Item = &DesignUnit<ConfigurationDeclaration>> {
        self.configurations.values()
    }
}

impl PackageDesignUnit {
    fn set_body(&mut self, body: DesignUnit<PackageBody>, messages: &mut MessageHandler) {
        if self.body.is_some() {
            messages.push(Message::error(
                body.ident(),
                format!(
                    "Duplicate package body of package '{}'",
                    self.package.name(),
                ),
            ));
        } else {
            {
                let primary_pos = &self.package.pos();
                let secondary_pos = &body.pos();
                if primary_pos.source == secondary_pos.source
                    && primary_pos.start > secondary_pos.start
                {
                    messages.push(Message::error(
                        secondary_pos,
                        format!(
                            "Package body declared before package '{}'",
                            self.package.name()
                        ),
                    ));
                }
            }
            self.body = Some(body);
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct EntityDesignUnit {
    pub entity: DesignUnit<EntityDeclaration>,
    pub architectures: FnvHashMap<Symbol, DesignUnit<ArchitectureBody>>,
    pub configurations: FnvHashMap<Symbol, DesignUnit<ConfigurationDeclaration>>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct PackageDesignUnit {
    /// The declarative region is None when it has not yet been computed
    pub package: DesignUnit<PackageDeclaration>,
    pub body: Option<DesignUnit<PackageBody>>,
}

impl PackageDesignUnit {
    pub fn is_generic(&self) -> bool {
        self.package.unit.generic_clause.is_some()
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Library {
    pub name: Symbol,
    entities: FnvHashMap<Symbol, EntityDesignUnit>,
    // Name to entity name mapping
    cfg_to_entity: FnvHashMap<Symbol, Symbol>,
    packages: FnvHashMap<Symbol, PackageDesignUnit>,
    package_instances: FnvHashMap<Symbol, DesignUnit<PackageInstantiation>>,
    contexts: FnvHashMap<Symbol, ContextDeclaration>,
}

impl<'a> Library {
    pub fn new(
        name: Symbol,
        work_sym: &Symbol,
        design_files: Vec<DesignFile>,
        messages: &mut MessageHandler,
    ) -> Library {
        let mut primary_names: FnvHashMap<Symbol, SrcPos> = FnvHashMap::default();
        let mut entities = FnvHashMap::default();
        let mut packages = FnvHashMap::default();
        let mut package_instances = FnvHashMap::default();
        let mut contexts = FnvHashMap::default();
        let mut architectures = Vec::new();
        let mut package_bodies = Vec::new();
        let mut configurations = Vec::new();

        for design_file in design_files {
            for design_unit in design_file.design_units {
                match design_unit {
                    AnyDesignUnit::Primary(primary) => {
                        let primary_ident = primary.ident().clone();

                        match primary_names.entry(primary_ident.item) {
                            Entry::Occupied(entry) => {
                                let msg = Message::error(
                                    primary_ident.pos,
                                    format!(
                                        "A primary unit has already been declared with name '{}' in library '{}'",
                                        entry.key(),
                                        name
                                    )).related(entry.get(), "Previously defined here");
                                messages.push(msg);
                            }
                            Entry::Vacant(entry) => match primary {
                                PrimaryUnit::EntityDeclaration(entity) => {
                                    entry.insert(entity.unit.ident.pos.clone());
                                    entities.insert(
                                        entity.name().clone(),
                                        EntityDesignUnit {
                                            entity,
                                            architectures: FnvHashMap::default(),
                                            configurations: FnvHashMap::default(),
                                        },
                                    );
                                }
                                PrimaryUnit::PackageDeclaration(package) => {
                                    entry.insert(package.ident().pos.clone());
                                    packages.insert(
                                        package.name().clone(),
                                        PackageDesignUnit {
                                            package,
                                            body: None,
                                        },
                                    );
                                }
                                PrimaryUnit::PackageInstance(inst) => {
                                    entry.insert(inst.ident().pos.clone());
                                    package_instances.insert(inst.name().clone(), inst);
                                }
                                PrimaryUnit::ContextDeclaration(context) => {
                                    entry.insert(context.ident().pos.clone());
                                    contexts.insert(context.name().clone(), context);
                                }

                                PrimaryUnit::Configuration(config) => {
                                    configurations.push(config);
                                }
                            },
                        }
                    }
                    AnyDesignUnit::Secondary(secondary) => match secondary {
                        SecondaryUnit::Architecture(architecture) => {
                            architectures.push(architecture)
                        }
                        SecondaryUnit::PackageBody(body) => package_bodies.push(body),
                    },
                }
            }
        }

        for architecture in architectures {
            if let Some(ref mut entity) = entities.get_mut(&architecture.unit.entity_name.item) {
                entity.add_architecture(architecture, messages)
            } else {
                messages.push(Message::error(
                    &architecture.unit.entity_name.pos,
                    format!(
                        "No entity '{}' within library '{}'",
                        architecture.unit.entity_name.item, name
                    ),
                ));
            }
        }

        for body in package_bodies {
            if let Some(ref mut package) = packages.get_mut(&body.name()) {
                package.set_body(body, messages)
            } else {
                messages.push(Message::error(
                    &body.ident(),
                    format!("No package '{}' within library '{}'", &body.name(), name),
                ));
            }
        }

        for config in configurations {
            let entity = {
                let entname = match config.unit.entity_name.as_slice() {
                    &[ref libname, ref entname] => {
                        if !(libname.item == name || &libname.item == work_sym) {
                            // @TODO use real scope and visibilty rules to resolve entity name
                            // @TODO does not detect missing library clause for libname
                            // @TODO does not detect if libname is shadowed by other use clause
                            messages.push(Message::error(
                                libname,
                                format!("Configuration must be within the same library '{}' as the corresponding entity", name),
                            ));
                            continue;
                        } else {
                            entname
                        }
                    }
                    &[ref entname] => entname,
                    selected_name => {
                        let name: WithPos<Name> = selected_name.to_vec().into();
                        messages.push(Message::error(&name, "Invalid selected name for entity")
                                      .related(&name, "Entity name must be of the form library.entity_name or entity_name"));
                        continue;
                    }
                };
                if let Some(entity) = entities.get_mut(&entname.item) {
                    entity
                } else {
                    messages.push(Message::error(
                        entname,
                        format!("No entity '{}' within library '{}'", entname.item, name),
                    ));
                    continue;
                }
            };

            entity.add_configuration(config, messages);
        }

        let mut cfg_to_entity = FnvHashMap::default();
        for entity in entities.values() {
            for configuration in entity.configurations.values() {
                cfg_to_entity.insert(configuration.name().clone(), entity.entity.name().clone());
            }
        }

        Library {
            name,
            entities,
            cfg_to_entity,
            packages,
            package_instances,
            contexts,
        }
    }

    #[cfg(test)]
    pub fn entity(&'a self, name: &Symbol) -> Option<&'a EntityDesignUnit> {
        self.entities.get(name)
    }

    #[cfg(test)]
    pub fn configuration(
        &'a self,
        name: &Symbol,
    ) -> Option<&'a DesignUnit<ConfigurationDeclaration>> {
        self.cfg_to_entity
            .get(name)
            .and_then(|entity_name| self.entities.get(entity_name))
            .and_then(|entity| entity.configurations.get(name))
    }

    pub fn package(&'a self, name: &Symbol) -> Option<&'a PackageDesignUnit> {
        self.packages.get(name)
    }

    #[cfg(test)]
    pub fn package_instance(
        &'a self,
        name: &Symbol,
    ) -> Option<&'a DesignUnit<PackageInstantiation>> {
        self.package_instances.get(name)
    }

    #[cfg(test)]
    pub fn context(&'a self, name: &Symbol) -> Option<&'a ContextDeclaration> {
        self.contexts.get(name)
    }

    pub fn entities(&self) -> impl Iterator<Item = &EntityDesignUnit> {
        self.entities.values()
    }

    pub fn packages(&'a self) -> impl Iterator<Item = &'a PackageDesignUnit> {
        self.packages.values()
    }

    pub fn package_instances(&self) -> impl Iterator<Item = &DesignUnit<PackageInstantiation>> {
        self.package_instances.values()
    }

    pub fn contexts(&self) -> impl Iterator<Item = &ContextDeclaration> {
        self.contexts.values()
    }
}

pub struct DesignRoot {
    libraries: FnvHashMap<Symbol, Library>,
}

impl DesignRoot {
    pub fn new() -> DesignRoot {
        DesignRoot {
            libraries: FnvHashMap::default(),
        }
    }

    pub fn add_library(&mut self, library: Library) {
        self.libraries.insert(library.name.clone(), library);
    }

    pub fn get_library(&self, library_name: &Symbol) -> Option<&Library> {
        self.libraries.get(library_name)
    }

    pub fn iter_libraries(&self) -> impl Iterator<Item = &Library> {
        self.libraries.values()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::{check_messages, check_no_messages, Code, CodeBuilder};

    fn new_library_with_messages<'a>(code: &Code, name: &str) -> (Library, Vec<Message>) {
        let mut messages = Vec::new();
        let library = Library::new(
            code.symbol(name),
            &code.symbol("work"),
            vec![code.design_file()],
            &mut messages,
        );
        (library, messages)
    }

    fn new_library<'a>(code: &Code, name: &str) -> Library {
        let (library, messages) = new_library_with_messages(code, name);
        check_no_messages(&messages);
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

        assert_eq!(
            library.entity(&code.symbol("ent")),
            Some(&EntityDesignUnit {
                entity: DesignUnit {
                    context_clause: vec![],
                    unit: code.entity()
                },
                architectures: FnvHashMap::default(),
                configurations: FnvHashMap::default()
            })
        );
    }

    #[test]
    fn error_on_package_body_without_package() {
        let code = Code::new(
            "
package body pkg is
end package body;
",
        );
        let (library, messages) = new_library_with_messages(&code, "libname");

        assert_eq!(library.packages.len(), 0);
        check_messages(
            messages,
            vec![Message::error(
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
        let (library, messages) = new_library_with_messages(&code, "libname");

        assert_eq!(library.entities.len(), 0);
        check_messages(
            messages,
            vec![Message::error(
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
        let (library, messages) = new_library_with_messages(&code, "libname");

        assert!(library.package(&code.symbol("pkg")).unwrap().body.is_none());
        check_messages(
            messages,
            vec![Message::error(
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
        let (library, messages) = new_library_with_messages(&code, "libname");

        assert_eq!(
            library
                .entity(&code.symbol("entname"))
                .unwrap()
                .architectures
                .len(),
            0
        );
        check_messages(
            messages,
            vec![Message::error(
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
        let (library, messages) = new_library_with_messages(&code, "libname");

        assert!(library.package(&code.symbol("pkg")).unwrap().body.is_some());
        check_messages(
            messages,
            vec![Message::error(
                code.s("pkg", 3),
                "Duplicate package body of package 'pkg'",
            )],
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
        let (library, messages) = new_library_with_messages(&code, "libname");

        assert_eq!(library.entities.len(), 1);
        assert_eq!(library.packages.len(), 1);
        check_messages(
            messages,
            vec![
                Message::error(
                    code.s("pkg", 2),
                    "A primary unit has already been declared with name 'pkg' in library 'libname'"
                ).related(code.s("pkg", 1), "Previously defined here"),
                Message::error(
                    code.s("entname", 2),
                    "A primary unit has already been declared with name 'entname' in library 'libname'"
                ).related(code.s("entname", 1), "Previously defined here"),
                Message::error(
                    code.s("pkg", 3),
                    "A primary unit has already been declared with name 'pkg' in library 'libname'"
                ).related(code.s("pkg", 1), "Previously defined here"),
                Message::error(
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
        let (library, messages) = new_library_with_messages(&code, "libname");

        // Should still be added as a secondary unit
        assert!(library.package(&code.symbol("pkg")).unwrap().body.is_some());

        check_messages(
            messages,
            vec![
                Message::error(
                    code.s("pkg", 1),
                    "Package body declared before package 'pkg'",
                ),
                Message::error(
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

        let mut messages = Vec::new();
        let library = Library::new(
            builder.symbol("libname"),
            &builder.symbol("work"),
            vec![file1.design_file(), file2.design_file()],
            &mut messages,
        );

        // Should still be added as a secondary unit
        assert!(library
            .package(&builder.symbol("pkg"))
            .unwrap()
            .body
            .is_some());

        check_no_messages(&messages);
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
        let (library, messages) = new_library_with_messages(&code, "libname");

        assert_eq!(
            library
                .entity(&code.symbol("ent"))
                .unwrap()
                .architectures
                .len(),
            1
        );
        check_messages(
            messages,
            vec![Message::error(
                code.s("rtl", 2),
                "Duplicate architecture 'rtl' of entity 'ent'",
            )],
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
        let mut architectures = FnvHashMap::default();

        architectures.insert(
            code.symbol("arch0"),
            DesignUnit {
                context_clause: vec![],
                unit: architecture0,
            },
        );
        architectures.insert(
            code.symbol("arch1"),
            DesignUnit {
                context_clause: vec![],
                unit: architecture1,
            },
        );

        assert_eq!(
            library.entity(&code.symbol("ent")),
            Some(&EntityDesignUnit {
                entity: DesignUnit {
                    context_clause: vec![],
                    unit: entity
                },
                architectures,
                configurations: FnvHashMap::default()
            })
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

        let body = DesignUnit {
            context_clause: vec![],
            unit: body,
        };

        assert_eq!(
            library.package(&code.symbol("pkg")),
            Some(&PackageDesignUnit {
                package: DesignUnit {
                    context_clause: vec![],
                    unit: package
                },
                body: Some(body)
            })
        );
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
        let context = code.context();

        assert_eq!(library.context(&code.symbol("ctx")), Some(&context));
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

configuration cfg1 of ent is
  for rtl
  end for;
end configuration;

configuration cfg2 of work.ent is
  for rtl
  end for;
end configuration;

configuration cfg3 of libname.ent is
  for rtl
  end for;
end configuration;
",
        );
        let library = new_library(&code, "libname");
        let entity = library.entity(&code.symbol("ent")).unwrap();
        let cfg1 = DesignUnit {
            context_clause: vec![],
            unit: code
                .between("configuration cfg1", "end configuration;")
                .configuration(),
        };
        let cfg2 = DesignUnit {
            context_clause: vec![],
            unit: code
                .between("configuration cfg2", "end configuration;")
                .configuration(),
        };
        let cfg3 = DesignUnit {
            context_clause: vec![],
            unit: code
                .between("configuration cfg3", "end configuration;")
                .configuration(),
        };

        let mut configurations = FnvHashMap::default();
        configurations.insert(code.symbol("cfg1"), cfg1.clone());
        configurations.insert(code.symbol("cfg2"), cfg2.clone());
        configurations.insert(code.symbol("cfg3"), cfg3.clone());

        assert_eq!(entity.configurations, configurations);
        assert_eq!(library.configuration(&code.symbol("cfg1")), Some(&cfg1));
        assert_eq!(library.configuration(&code.symbol("cfg2")), Some(&cfg2));
        assert_eq!(library.configuration(&code.symbol("cfg3")), Some(&cfg3));
        assert_eq!(library.configuration(&code.symbol("cfg4")), None);
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
        let (library, messages) = new_library_with_messages(&code, "libname");

        let entity = library.entity(&code.symbol("ent")).unwrap();
        let cfg = code
            .between("configuration cfg", "end configuration;")
            .configuration();
        let mut configurations = FnvHashMap::default();
        configurations.insert(
            code.symbol("cfg"),
            DesignUnit {
                context_clause: vec![],
                unit: cfg,
            },
        );
        check_messages(
            messages,
            vec![Message::error(
                code.s("cfg", 2),
                "Duplicate configuration 'cfg' of entity 'ent'",
            )],
        );
        assert_eq!(entity.configurations, configurations);
    }

    #[test]
    fn error_on_configuration_before_entity_in_same_file() {
        let code = Code::new(
            "
configuration cfg of ent is
  for rtl
  end for;
end configuration;

entity ent is
end entity;
",
        );
        let (library, messages) = new_library_with_messages(&code, "libname");

        let entity = library.entity(&code.symbol("ent")).unwrap();
        let cfg = code
            .between("configuration cfg", "end configuration;")
            .configuration();
        let mut configurations = FnvHashMap::default();
        configurations.insert(
            code.symbol("cfg"),
            DesignUnit {
                context_clause: vec![],
                unit: cfg,
            },
        );
        check_messages(
            messages,
            vec![Message::error(
                code.s("cfg", 1),
                "Configuration 'cfg' declared before entity 'ent'",
            )],
        );
        assert_eq!(entity.configurations, configurations);
    }

    #[test]
    fn error_on_configuration_of_missing_entity() {
        let code = Code::new(
            "
configuration cfg of ent is
  for rtl
  end for;
end configuration;
",
        );
        let (_, messages) = new_library_with_messages(&code, "libname");

        check_messages(
            messages,
            vec![Message::error(
                code.s("ent", 1),
                "No entity 'ent' within library 'libname'",
            )],
        );
    }

    #[test]
    fn error_on_configuration_of_entity_outside_of_library() {
        let code = Code::new(
            "
configuration cfg of lib2.ent is
  for rtl
  end for;
end configuration;
",
        );
        let (_, messages) = new_library_with_messages(&code, "libname");

        check_messages(
            messages,
            vec![Message::error(
                code.s("lib2", 1),
                "Configuration must be within the same library 'libname' as the corresponding entity",
            )],
        );
    }

    #[test]
    fn error_on_configuration_of_with_bad_selected_name() {
        let code = Code::new(
            "
configuration cfg of lib2.pkg.ent is
  for rtl
  end for;
end configuration;
",
        );
        let (_, messages) = new_library_with_messages(&code, "libname");

        check_messages(
            messages,
            vec![
                Message::error(code.s1("lib2.pkg.ent"), "Invalid selected name for entity")
                    .related(
                        code.s1("lib2.pkg.ent"),
                        "Entity name must be of the form library.entity_name or entity_name",
                    ),
            ],
        );
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

        assert_eq!(
            library.package_instance(&code.symbol("ipkg")),
            Some(&DesignUnit {
                context_clause: vec![],
                unit: instance
            })
        );
    }

}
