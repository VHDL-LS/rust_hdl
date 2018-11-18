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
    ) -> Option<DesignUnit<SecondaryUnit>> {
        if !self.can_be_secondary(&secondary_unit) {
            messages.push(Message::error(
                secondary_unit.ident(),
                format!(
                    "{} cannot be a secondary unit of {}",
                    secondary_unit.unit_str(),
                    self.unit.unit_str()
                ),
            ));
            Some(secondary_unit)
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
                    Some(secondary_unit)
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
                    None
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
#[derive(PartialEq, Debug, Clone)]
pub struct Library {
    pub name: Symbol,
    pub primary_units: FnvHashMap<Symbol, PrimaryDesignUnit>,
    pub secondary_orphans: Vec<DesignUnit<SecondaryUnit>>,
}

impl Library {
    pub fn new(name: Symbol) -> Library {
        let primary_units = FnvHashMap::default();
        let secondary_orphans = Vec::new();
        Library {
            name,
            primary_units,
            secondary_orphans,
        }
    }

    pub fn add_design_file(&mut self, design_file: DesignFile, messages: &mut MessageHandler) {
        for design_unit in design_file.design_units {
            self.add_design_unit(design_unit, messages);
        }
    }

    pub fn remove_source(&mut self, source: &Source) {
        // Remove orphans from source
        let secondary_orphans = std::mem::replace(&mut self.secondary_orphans, Vec::new());
        for secondary_unit in secondary_orphans {
            if secondary_unit.source() != source {
                self.secondary_orphans.push(secondary_unit);
            }
        }

        let mut primaries_to_remove = Vec::new();
        for (primary_name, primary_unit) in self.primary_units.iter_mut() {
            if primary_unit.unit.source() == source {
                primaries_to_remove.push(primary_name.clone());
            }

            // Remove secondary units from source in primary unit
            let mut names_to_remove = Vec::new();
            for (secondary_name, secondary_unit) in primary_unit.secondary.iter() {
                if secondary_unit.source() == source {
                    names_to_remove.push(secondary_name.clone());
                }
            }

            for secondary_name in names_to_remove {
                primary_unit.secondary.remove(&secondary_name);
            }
        }

        // Remove primary units from source
        for primary_name in primaries_to_remove {
            if let Some(primary_unit) = self.primary_units.remove(&primary_name) {
                self.secondary_orphans
                    .extend(primary_unit.secondary.into_iter().map(|(_, v)| v));
            }
        }
    }

    pub fn add_design_unit(&mut self, design_unit: AnyDesignUnit, messages: &mut MessageHandler) {
        match design_unit {
            AnyDesignUnit::Primary(primary) => {
                let primary_name = primary.name().clone();

                let primary = PrimaryDesignUnit {
                    unit: primary,
                    secondary: FnvHashMap::default(),
                };

                match self.primary_units.entry(primary_name) {
                    Entry::Occupied(entry) => {
                        let old_unit: &PrimaryDesignUnit = entry.get();
                        let msg = Message::error(
                            primary.unit.ident(),
                            format!(
                                "A primary unit has already been declared with name '{}' in library '{}'",
                                entry.key(),
                                self.name
                            )).related(old_unit.unit.ident(), "Previously defined here");
                        messages.push(msg);
                    }
                    Entry::Vacant(entry) => {
                        entry.insert(primary);
                    }
                }
            }
            AnyDesignUnit::Secondary(secondary) => {
                self.secondary_orphans.push(secondary);
            }
        }
    }

    /// All orphaned secondary units must be assigned to primary units
    pub fn finalize(&mut self, messages: &mut MessageHandler) {
        let secondary_orphans = std::mem::replace(&mut self.secondary_orphans, Vec::new());

        for secondary_unit in secondary_orphans {
            if let Some(ref mut primary_unit) =
                self.primary_units.get_mut(secondary_unit.primary_name())
            {
                if let Some(secondary_unit) =
                    primary_unit.add_secondary_unit(secondary_unit, messages)
                {
                    self.secondary_orphans.push(secondary_unit);
                }
            } else {
                match secondary_unit.unit {
                    SecondaryUnit::Architecture(..) => {
                        messages.push(Message::error(
                            secondary_unit.primary_ident(),
                            format!(
                                "No entity '{}' within the library '{}'",
                                secondary_unit.primary_name(),
                                self.name
                            ),
                        ));
                    }
                    SecondaryUnit::PackageBody(..) => {
                        messages.push(Message::error(
                            secondary_unit.primary_ident(),
                            format!(
                                "No package '{}' within the library '{}'",
                                secondary_unit.primary_name(),
                                self.name
                            ),
                        ));
                    }
                }

                self.secondary_orphans.push(secondary_unit);
            }
        }
    }

    #[cfg(test)]
    fn primary_unit<'a>(&'a self, name: &Symbol) -> Option<&'a PrimaryDesignUnit> {
        self.primary_units.get(name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_util::{check_no_messages, Code, CodeBuilder};

    fn new_library_with_messages(code: &Code, name: &str) -> (Library, Vec<Message>) {
        let mut messages = Vec::new();
        let mut library = Library::new(code.symbol(name));
        library.add_design_file(code.design_file(), &mut messages);
        library.finalize(&mut messages);
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
        let mut library = Library::new(builder.symbol("libname"));
        library.add_design_file(file1.design_file(), &mut messages);
        library.add_design_file(file2.design_file(), &mut messages);
        library.finalize(&mut messages);

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

    #[test]
    fn can_remove_sources() {
        let builder = CodeBuilder::new();
        let primary_file = builder.code(
            "
package pkg is
end package;
",
        );
        let secondary_file = builder.code(
            "
package body pkg is
end package body;
",
        );

        let mut messages = Vec::new();
        let mut library = Library::new(builder.symbol("libname"));
        library.add_design_file(primary_file.design_file(), &mut messages);
        library.add_design_file(secondary_file.design_file(), &mut messages);
        library.finalize(&mut messages);
        check_no_messages(&messages);

        library.remove_source(&primary_file.source());
        library.finalize(&mut messages);

        assert_eq!(library.primary_unit(&builder.symbol("pkg")), None);

        library.add_design_file(primary_file.design_file(), &mut messages);
        library.finalize(&mut messages);

        // Should still be have a secondary unit
        assert_eq!(
            library
                .primary_unit(&builder.symbol("pkg"))
                .unwrap()
                .secondary
                .len(),
            1
        );

        library.remove_source(&secondary_file.source());
        library.finalize(&mut messages);

        // No secondary unit
        assert_eq!(
            library
                .primary_unit(&builder.symbol("pkg"))
                .unwrap()
                .secondary
                .len(),
            0
        );

        library.add_design_file(secondary_file.design_file(), &mut messages);
        library.finalize(&mut messages);

        // Should still have a secondary unit again
        assert_eq!(
            library
                .primary_unit(&builder.symbol("pkg"))
                .unwrap()
                .secondary
                .len(),
            1
        );
    }

}
