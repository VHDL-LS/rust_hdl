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

use ast::{AnyDesignUnit, DesignFile, DesignUnit, Ident, PrimaryUnit, SecondaryUnit};
use message::{Message, MessageHandler};
use source::Source;
use symbol_table::Symbol;

impl DesignUnit<PrimaryUnit> {
    fn ident(&self) -> &Ident {
        match self.unit {
            PrimaryUnit::EntityDeclaration(ref unit) => &unit.ident,
            PrimaryUnit::Configuration(ref unit) => &unit.ident,
            PrimaryUnit::PackageDeclaration(ref unit) => &unit.ident,
            PrimaryUnit::PackageInstance(ref unit) => &unit.ident,
            PrimaryUnit::ContextDeclaration(ref unit) => &unit.ident,
        }
    }

    fn unit_type_str(&self) -> &'static str {
        match self.unit {
            PrimaryUnit::EntityDeclaration(..) => "entity",
            PrimaryUnit::Configuration(..) => "configuration",
            PrimaryUnit::PackageDeclaration(..) => "package",
            PrimaryUnit::PackageInstance(..) => "package instance",
            PrimaryUnit::ContextDeclaration(..) => "context",
        }
    }

    fn unit_str(&self) -> String {
        format!("{} '{}'", self.unit_type_str(), self.name())
    }

    pub fn name(&self) -> &Symbol {
        &self.ident().item
    }

    pub fn source(&self) -> &Source {
        &self.ident().pos.source
    }
}

impl DesignUnit<SecondaryUnit> {
    fn ident(&self) -> &Ident {
        match self.unit {
            SecondaryUnit::Architecture(ref unit) => &unit.ident,
            SecondaryUnit::PackageBody(ref unit) => &unit.ident,
        }
    }

    pub fn name(&self) -> &Symbol {
        &self.ident().item
    }

    fn unit_str(&self) -> String {
        match self.unit {
            SecondaryUnit::Architecture(..) => format!("architecture '{}'", self.name()),
            SecondaryUnit::PackageBody(..) => "package body".to_owned(),
        }
    }

    fn primary_ident(&self) -> &Ident {
        match self.unit {
            SecondaryUnit::Architecture(ref unit) => &unit.entity_name,
            SecondaryUnit::PackageBody(ref unit) => &unit.ident,
        }
    }

    pub fn primary_name(&self) -> &Symbol {
        &self.primary_ident().item
    }

    pub fn source(&self) -> &Source {
        &self.ident().pos.source
    }
}

impl PrimaryDesignUnit {
    /// Returns true if the unit can be a secondary unit of this primary unit
    fn can_be_secondary(&self, secondary_unit: &DesignUnit<SecondaryUnit>) -> bool {
        match (&self.unit.unit, &secondary_unit.unit) {
            (&PrimaryUnit::EntityDeclaration(..), &SecondaryUnit::Architecture(..)) => true,
            (&PrimaryUnit::PackageDeclaration(..), &SecondaryUnit::PackageBody(..)) => true,
            _ => false,
        }
    }

    fn add_secondary_unit(
        &mut self,
        secondary_unit: DesignUnit<SecondaryUnit>,
        messages: &mut MessageHandler,
    ) {
        if !self.can_be_secondary(&secondary_unit) {
            messages.push(Message::error(
                secondary_unit.ident(),
                format!(
                    "{} cannot be a secondary unit of {}",
                    secondary_unit.unit_str(),
                    self.unit.unit_str()
                ),
            ));
        } else {
            match self.secondary.entry(secondary_unit.name().to_owned()) {
                Entry::Occupied(..) => {
                    messages.push(Message::error(
                        secondary_unit.ident(),
                        format!(
                            "Duplicate {} of {}",
                            secondary_unit.unit_str(),
                            self.unit.unit_str(),
                        ),
                    ));
                }
                Entry::Vacant(entry) => {
                    {
                        let primary_pos = &self.unit.ident().pos;
                        let secondary_pos = &secondary_unit.ident().pos;
                        if primary_pos.source == secondary_pos.source
                            && primary_pos.start > secondary_pos.start
                        {
                            messages.push(Message::error(
                                secondary_pos,
                                format!(
                                    "{} declared before {}",
                                    secondary_unit.unit_str(),
                                    self.unit.unit_str()
                                ),
                            ));
                        }
                    };

                    entry.insert(secondary_unit);
                }
            }
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct PrimaryDesignUnit {
    pub unit: DesignUnit<PrimaryUnit>,
    pub secondary: FnvHashMap<Symbol, DesignUnit<SecondaryUnit>>,
}

impl PrimaryDesignUnit {
    pub fn iter_secondary_units(&self) -> impl Iterator<Item = &DesignUnit<SecondaryUnit>> {
        self.secondary.values()
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Library {
    pub name: Symbol,
    pub primary_units: FnvHashMap<Symbol, PrimaryDesignUnit>,
}

impl Library {
    pub fn new(
        name: Symbol,
        design_files: Vec<DesignFile>,
        messages: &mut MessageHandler,
    ) -> Library {
        let mut primary_units = FnvHashMap::default();
        let mut secondary_orphans = Vec::new();

        for design_file in design_files {
            for design_unit in design_file.design_units {
                match design_unit {
                    AnyDesignUnit::Primary(primary) => {
                        let primary_name = primary.name().clone();

                        let primary = PrimaryDesignUnit {
                            unit: primary,
                            secondary: FnvHashMap::default(),
                        };

                        match primary_units.entry(primary_name) {
                            Entry::Occupied(entry) => {
                                let old_unit: &PrimaryDesignUnit = entry.get();
                                let msg = Message::error(
                                    primary.unit.ident(),
                                    format!(
                                        "A primary unit has already been declared with name '{}' in library '{}'",
                                        entry.key(),
                                        name
                                    )).related(old_unit.unit.ident(), "Previously defined here");
                                messages.push(msg);
                            }
                            Entry::Vacant(entry) => {
                                entry.insert(primary);
                            }
                        }
                    }
                    AnyDesignUnit::Secondary(secondary) => {
                        secondary_orphans.push(secondary);
                    }
                }
            }
        }

        for secondary_unit in secondary_orphans {
            if let Some(ref mut primary_unit) = primary_units.get_mut(secondary_unit.primary_name())
            {
                primary_unit.add_secondary_unit(secondary_unit, messages)
            } else {
                match secondary_unit.unit {
                    SecondaryUnit::Architecture(..) => {
                        messages.push(Message::error(
                            secondary_unit.primary_ident(),
                            format!(
                                "No entity '{}' within the library '{}'",
                                secondary_unit.primary_name(),
                                name
                            ),
                        ));
                    }
                    SecondaryUnit::PackageBody(..) => {
                        messages.push(Message::error(
                            secondary_unit.primary_ident(),
                            format!(
                                "No package '{}' within the library '{}'",
                                secondary_unit.primary_name(),
                                name
                            ),
                        ));
                    }
                }
            }
        }

        Library {
            name,
            primary_units,
        }
    }

    #[cfg(test)]
    fn primary_unit<'a>(&'a self, name: &Symbol) -> Option<&'a PrimaryDesignUnit> {
        self.primary_units.get(name)
    }

    pub fn iter_primary_units(&self) -> impl Iterator<Item = &PrimaryDesignUnit> {
        self.primary_units.values()
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
    use test_util::{check_no_messages, Code, CodeBuilder};

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
            library.primary_unit(&code.symbol("ent")),
            Some(&PrimaryDesignUnit {
                unit: DesignUnit {
                    context_clause: vec![],
                    unit: PrimaryUnit::EntityDeclaration(code.entity())
                },
                secondary: FnvHashMap::default()
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

        assert_eq!(library.primary_units.len(), 0);
        assert_eq!(
            messages,
            vec![Message::error(
                code.s1("pkg"),
                "No package 'pkg' within the library 'libname'"
            )]
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

        assert_eq!(library.primary_units.len(), 0);
        assert_eq!(
            messages,
            vec![Message::error(
                code.s1("ent"),
                "No entity 'ent' within the library 'libname'"
            )]
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

        assert_eq!(
            library
                .primary_unit(&code.symbol("pkg"))
                .unwrap()
                .secondary
                .len(),
            0
        );
        assert_eq!(
            messages,
            vec![Message::error(
                code.s1("rtl"),
                "architecture 'rtl' cannot be a secondary unit of package 'pkg'"
            )]
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
                .primary_unit(&code.symbol("entname"))
                .unwrap()
                .secondary
                .len(),
            0
        );
        assert_eq!(
            messages,
            vec![Message::error(
                code.s("entname", 2),
                "package body cannot be a secondary unit of entity 'entname'"
            )]
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

        assert_eq!(
            library
                .primary_unit(&code.symbol("pkg"))
                .unwrap()
                .secondary
                .len(),
            1
        );
        assert_eq!(
            messages,
            vec![Message::error(
                code.s("pkg", 3),
                "Duplicate package body of package 'pkg'"
            )]
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

        assert_eq!(library.primary_units.len(), 2);
        assert_eq!(
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
        assert_eq!(
            library
                .primary_unit(&code.symbol("pkg"))
                .unwrap()
                .secondary
                .len(),
            1
        );

        assert_eq!(
            messages,
            vec![
                Message::error(
                    code.s("pkg", 1),
                    "package body declared before package 'pkg'"
                ),
                Message::error(
                    code.s("rtl", 1),
                    "architecture 'rtl' declared before entity 'entname'"
                ),
            ]
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
        assert_eq!(
            library
                .primary_unit(&builder.symbol("pkg"))
                .unwrap()
                .secondary
                .len(),
            1
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
                .primary_unit(&code.symbol("ent"))
                .unwrap()
                .secondary
                .len(),
            1
        );
        assert_eq!(
            messages,
            vec![Message::error(
                code.s("rtl", 2),
                "Duplicate architecture 'rtl' of entity 'ent'"
            )]
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
        let mut secondary = FnvHashMap::default();

        secondary.insert(
            code.symbol("arch0"),
            DesignUnit {
                context_clause: vec![],
                unit: SecondaryUnit::Architecture(architecture0),
            },
        );
        secondary.insert(
            code.symbol("arch1"),
            DesignUnit {
                context_clause: vec![],
                unit: SecondaryUnit::Architecture(architecture1),
            },
        );

        assert_eq!(
            library.primary_unit(&code.symbol("ent")),
            Some(&PrimaryDesignUnit {
                unit: DesignUnit {
                    context_clause: vec![],
                    unit: PrimaryUnit::EntityDeclaration(entity)
                },
                secondary
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

        let mut secondary = FnvHashMap::default();
        secondary.insert(
            code.symbol("pkg"),
            DesignUnit {
                context_clause: vec![],
                unit: SecondaryUnit::PackageBody(body),
            },
        );

        assert_eq!(
            library.primary_unit(&code.symbol("pkg")),
            Some(&PrimaryDesignUnit {
                unit: DesignUnit {
                    context_clause: vec![],
                    unit: PrimaryUnit::PackageDeclaration(package)
                },
                secondary
            })
        );
    }

}
