// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

// @TODO test configuration of missing entity
// @TODO test configuration of entity outside of library
// @TODO add related information to message
// @TODO Capitalize fist letter in errors

extern crate fnv;
use self::fnv::FnvHashMap;
use std::collections::hash_map::Entry;

use ast::{
    AnyDesignUnit, ArchitectureBody, ConfigurationDeclaration, ContextDeclaration, DesignFile,
    DesignUnit, EntityDeclaration, Ident, PackageBody, PackageDeclaration, PackageInstantiation,
    PrimaryUnit, SecondaryUnit,
};
use message::{Message, MessageHandler};
use source::SrcPos;
use symbol_table::Symbol;

trait HasIdent {
    fn ident(&self) -> &Ident;
    fn name(&self) -> &Symbol {
        &self.ident().item
    }
    fn pos(&self) -> &SrcPos {
        &self.ident().pos
    }
}

impl HasIdent for EntityDeclaration {
    fn ident(&self) -> &Ident {
        &self.ident
    }
}

impl HasIdent for PackageDeclaration {
    fn ident(&self) -> &Ident {
        &self.ident
    }
}

impl HasIdent for PackageBody {
    fn ident(&self) -> &Ident {
        &self.ident
    }
}

impl HasIdent for ArchitectureBody {
    fn ident(&self) -> &Ident {
        &self.ident
    }
}

impl HasIdent for PackageInstantiation {
    fn ident(&self) -> &Ident {
        &self.ident
    }
}

impl HasIdent for ContextDeclaration {
    fn ident(&self) -> &Ident {
        &self.ident
    }
}

impl HasIdent for ConfigurationDeclaration {
    fn ident(&self) -> &Ident {
        &self.ident
    }
}

impl HasIdent for PrimaryUnit {
    fn ident(&self) -> &Ident {
        match self {
            PrimaryUnit::EntityDeclaration(ref unit) => &unit.unit.ident,
            PrimaryUnit::Configuration(ref unit) => &unit.unit.ident,
            PrimaryUnit::PackageDeclaration(ref unit) => &unit.unit.ident,
            PrimaryUnit::PackageInstance(ref unit) => &unit.unit.ident,
            PrimaryUnit::ContextDeclaration(ref unit) => &unit.ident,
        }
    }
}

impl<T: HasIdent> HasIdent for DesignUnit<T> {
    fn ident(&self) -> &Ident {
        self.unit.ident()
    }
}

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
                                "architecture '{}' declared before entity '{}'",
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
}

impl PackageDesignUnit {
    fn set_body(&mut self, body: DesignUnit<PackageBody>, messages: &mut MessageHandler) {
        if let Some(_) = self.body {
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
                            "package body declared before package '{}'",
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
}

#[derive(PartialEq, Debug, Clone)]
pub struct PackageDesignUnit {
    pub package: DesignUnit<PackageDeclaration>,
    pub body: Option<DesignUnit<PackageBody>>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Library {
    pub name: Symbol,
    pub entities: FnvHashMap<Symbol, EntityDesignUnit>,
    pub packages: FnvHashMap<Symbol, PackageDesignUnit>,
    pub contexts: FnvHashMap<Symbol, ContextDeclaration>,
}

impl Library {
    pub fn new(
        name: Symbol,
        design_files: Vec<DesignFile>,
        messages: &mut MessageHandler,
    ) -> Library {
        let mut primary_names: FnvHashMap<Symbol, SrcPos> = FnvHashMap::default();
        let mut entities = FnvHashMap::default();
        let mut packages = FnvHashMap::default();
        let mut contexts = FnvHashMap::default();
        let mut architectures = Vec::new();
        let mut package_bodies = Vec::new();

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
                                PrimaryUnit::ContextDeclaration(context) => {
                                    entry.insert(context.ident().pos.clone());
                                    contexts.insert(context.name().clone(), context);
                                }
                                _ => {
                                    // @TODO configuration, package instance
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
                        "No entity '{}' within the library '{}'",
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
                    format!(
                        "No package '{}' within the library '{}'",
                        &body.name(),
                        name
                    ),
                ));
            }
        }

        Library {
            name,
            entities,
            packages,
            contexts,
        }
    }

    #[cfg(test)]
    fn entity<'a>(&'a self, name: &Symbol) -> Option<&'a EntityDesignUnit> {
        self.entities.get(name)
    }

    #[cfg(test)]
    fn package<'a>(&'a self, name: &Symbol) -> Option<&'a PackageDesignUnit> {
        self.packages.get(name)
    }

    #[cfg(test)]
    fn context<'a>(&'a self, name: &Symbol) -> Option<&'a ContextDeclaration> {
        self.contexts.get(name)
    }

    pub fn entities(&self) -> impl Iterator<Item = &EntityDesignUnit> {
        self.entities.values()
    }

    pub fn packages(&self) -> impl Iterator<Item = &PackageDesignUnit> {
        self.packages.values()
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
        // @TODO check for duplicate libraries
        self.libraries.insert(library.name.clone(), library);
    }

    pub fn has_library(&self, library_name: &Symbol) -> bool {
        self.libraries.contains_key(library_name)
    }

    pub fn iter_libraries(&self) -> impl Iterator<Item = &Library> {
        self.libraries.values()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_util::{check_messages, check_no_messages, Code, CodeBuilder};

    fn new_library_with_messages(code: &Code, name: &str) -> (Library, Vec<Message>) {
        let mut messages = Vec::new();
        let library = Library::new(code.symbol(name), vec![code.design_file()], &mut messages);
        (library, messages)
    }

    fn new_library(code: &Code, name: &str) -> Library {
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
                architectures: FnvHashMap::default()
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
                "No package 'pkg' within the library 'libname'",
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
                "No entity 'ent' within the library 'libname'",
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
                "No entity 'pkg' within the library 'libname'",
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
                "No package 'entname' within the library 'libname'",
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
                    "package body declared before package 'pkg'",
                ),
                Message::error(
                    code.s("rtl", 1),
                    "architecture 'rtl' declared before entity 'entname'",
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
            vec![file1.design_file(), file2.design_file()],
            &mut messages,
        );

        // Should still be added as a secondary unit
        assert!(
            library
                .package(&builder.symbol("pkg"))
                .unwrap()
                .body
                .is_some()
        );

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
                architectures
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

}
