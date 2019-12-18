// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

use super::*;
use crate::analysis::library::DesignRoot;

#[test]
fn incremental_analysis_of_use_within_package() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
package pkg is
  constant const : natural := 0;
end package;
",
    );

    builder.code(
        "libname",
        "
use work.pkg.const;

package pkg2 is
end package;
",
    );

    check_incremental_analysis(builder);
}

#[test]
fn incremental_analysis_of_package_use() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
package pkg is
  constant const : natural := 0;
end package;
",
    );

    builder.code(
        "libname",
        "
use work.pkg;

package pkg2 is
end package;
",
    );

    check_incremental_analysis(builder);
}

#[test]
fn incremental_analysis_of_entity_architecture() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
entity ent is
end entity;
",
    );

    builder.code(
        "libname",
        "
architecture a of ent is
begin
end architecture;
",
    );

    check_incremental_analysis(builder);
}

#[test]
fn incremental_analysis_of_package_and_body() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
package pkg is
end package;
",
    );

    builder.code(
        "libname",
        "
package body pkg is
end package body;
",
    );

    check_incremental_analysis(builder);
}

#[test]
fn incremental_analysis_of_entity_instance() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
entity ent is
end entity;

architecture a of ent is
begin
end architecture;
",
    );

    builder.code(
        "libname",
        "
entity ent2 is
end entity;

architecture a of ent2 is
begin
  inst: entity work.ent;
end architecture;
",
    );

    check_incremental_analysis(builder);
}

#[test]
fn incremental_analysis_of_configuration_instance() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
entity ent is
end entity;

architecture a of ent is
begin
end architecture;
",
    );

    builder.code(
        "libname",
        "
configuration cfg of ent is
for rtl
end for;
end configuration;
",
    );

    builder.code(
        "libname",
        "
entity ent2 is
end entity;

architecture a of ent2 is
begin
  inst : configuration work.cfg;
end architecture;
",
    );

    check_incremental_analysis(builder);
}

#[test]
fn incremental_analysis_library_all_collision() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname1",
        "
package pkg is
end package;
",
    );

    builder.code(
        "libname2",
        "
package pkg is
  constant const : natural := 0;
end package;
",
    );

    builder.code(
        "libname3",
        "

library libname1;
use libname1.all;

library libname2;
use libname2.all;

use pkg.const;

package pkg is
end package;
",
    );

    check_incremental_analysis(builder);
}

#[test]
fn incremental_analysis_of_package_and_body_with_deferred_constant() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
package pkg is
  constant deferred : natural;
end package;
",
    );

    builder.code(
        "libname",
        "
package body pkg is
  constant deferred : natural := 0;
end package body;
",
    );

    check_incremental_analysis(builder);
}

fn check_incremental_analysis(builder: LibraryBuilder) {
    let symtab = builder.symtab();
    let codes = builder.take_code();

    // Generate all combinations of removing and adding source
    for i in 0..codes.len() {
        let mut fresh_root = DesignRoot::new(symtab.clone());
        add_standard_library(symtab.clone(), &mut fresh_root);

        for (j, (library_name, code)) in codes.iter().enumerate() {
            if i != j {
                fresh_root.add_design_file(library_name.clone(), code.design_file());
            } else {
                fresh_root.ensure_library(library_name.clone());
            }
        }

        let mut root = DesignRoot::new(symtab.clone());
        add_standard_library(symtab.clone(), &mut root);
        for (library_name, code) in codes.iter() {
            root.add_design_file(library_name.clone(), code.design_file());
        }
        let mut unnused = Vec::new();
        root.analyze(&mut unnused);

        let (library_name, code) = &codes[i];

        root.remove_source(library_name.clone(), code.source());

        let mut diagnostics = Vec::new();
        root.analyze(&mut diagnostics);

        let mut fresh_diagnostics = Vec::new();
        fresh_root.analyze(&mut fresh_diagnostics);

        // Check that diagnostics are equal to doing analysis from scratch
        check_diagnostics(diagnostics.clone(), fresh_diagnostics.clone());

        // Add back files again
        root.add_design_file(library_name.clone(), code.design_file());
        fresh_root.add_design_file(library_name.clone(), code.design_file());

        let mut diagnostics = Vec::new();
        root.analyze(&mut diagnostics);

        let mut fresh_diagnostics = Vec::new();
        fresh_root.analyze(&mut fresh_diagnostics);

        // Check that diagnostics are equal to doing analysis from scratch
        check_diagnostics(diagnostics.clone(), fresh_diagnostics.clone());

        // Ensure no problems when all files are added
        check_no_diagnostics(&diagnostics);
    }
}
