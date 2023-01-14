// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

use super::*;
use crate::analysis::DesignRoot;
use crate::analysis::EntityId;
use crate::ast::search::*;
use crate::ast::Reference;
use crate::data::SrcPos;
use fnv::FnvHashSet;
use pretty_assertions::assert_eq;

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

    check_incremental_analysis(builder, vec![]);
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

    check_incremental_analysis(builder, vec![]);
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

    check_incremental_analysis(builder, vec![]);
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

    check_incremental_analysis(builder, vec![]);
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

    check_incremental_analysis(builder, vec![]);
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

    check_incremental_analysis(builder, vec![]);
}

#[test]
fn incremental_analysis_library_all_collision() {
    let mut builder = LibraryBuilder::new();
    let lib1 = builder.code(
        "libname1",
        "
package pkg is
end package;
",
    );

    let lib2 = builder.code(
        "libname2",
        "
package pkg is
  constant const : natural := 0;
end package;
",
    );

    let code = builder.code(
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

    use super::visibility::hidden_error;
    check_incremental_analysis(
        builder,
        vec![hidden_error(
            &code,
            "pkg",
            1,
            &[
                (&code, "libname1.all", 1, false),
                (&lib1, "pkg", 1, true),
                (&code, "libname2.all", 1, false),
                (&lib2, "pkg", 1, true),
            ],
        )],
    );
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

    check_incremental_analysis(builder, vec![]);
}

fn check_incremental_analysis(builder: LibraryBuilder, expected_diagnostics: Vec<Diagnostic>) {
    let symbols = builder.symbols();
    let codes = builder.take_code();

    // Generate all combinations of removing and adding source
    for i in 0..codes.len() {
        let mut fresh_root = DesignRoot::new(symbols.clone());
        add_standard_library(symbols.clone(), &mut fresh_root);

        let mut root = DesignRoot::new(symbols.clone());
        add_standard_library(symbols.clone(), &mut root);

        for (j, (library_name, code)) in codes.iter().enumerate() {
            root.add_design_file(library_name.clone(), code.design_file());

            if i != j {
                fresh_root.add_design_file(library_name.clone(), code.design_file());
            } else {
                fresh_root.ensure_library(library_name.clone());
            }
        }

        let mut diagnostics = Vec::new();
        root.analyze(&mut diagnostics);
        check_diagnostics(diagnostics, expected_diagnostics.clone());

        let (library_name, code) = &codes[i];

        // Remove a files
        root.remove_source(library_name.clone(), code.source());
        check_analysis_equal(&mut root, &mut fresh_root);

        // Add back files again
        root.add_design_file(library_name.clone(), code.design_file());
        fresh_root.add_design_file(library_name.clone(), code.design_file());

        let diagnostics = check_analysis_equal(&mut root, &mut fresh_root);

        // Ensure expected diagnostics when all files are added
        check_diagnostics(diagnostics, expected_diagnostics.clone());
    }
}

fn check_analysis_equal(got: &mut DesignRoot, expected: &mut DesignRoot) -> Vec<Diagnostic> {
    let mut got_diagnostics = Vec::new();
    got.analyze(&mut got_diagnostics);

    let mut expected_diagnostics = Vec::new();
    expected.analyze(&mut expected_diagnostics);

    // Check that diagnostics are equal to doing analysis from scratch
    check_diagnostics(got_diagnostics.clone(), expected_diagnostics);

    // Check that all references are equal, ensures the incremental
    // analysis has cleared refereces
    let mut got_searcher = FindAnyReferences::default();
    let _ = got.search(&mut got_searcher);

    let mut expected_searcher = FindAnyReferences::default();
    let _ = expected.search(&mut expected_searcher);

    let got_refs: FnvHashSet<_> = got_searcher
        .references
        .into_iter()
        .map(|id| got.get_ent(id).decl_pos())
        .collect();
    let expected_refs: FnvHashSet<_> = expected_searcher
        .references
        .into_iter()
        .map(|id| expected.get_ent(id).decl_pos())
        .collect();
    let diff: FnvHashSet<_> = got_refs.symmetric_difference(&expected_refs).collect();
    assert_eq!(diff, FnvHashSet::default());

    got_diagnostics
}

/// Find any reference
/// Added to help ensure that there are no references to removed sources
#[derive(Default)]
struct FindAnyReferences {
    references: Vec<EntityId>,
}

impl Searcher for FindAnyReferences {
    fn search_pos_with_ref(&mut self, _: &SrcPos, reference: &mut Reference) -> SearchState {
        if let Some(id) = reference {
            self.references.push(*id);
        };
        NotFinished
    }
}
