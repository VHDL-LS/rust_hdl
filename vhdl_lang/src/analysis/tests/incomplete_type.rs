// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

// @TODO
// 5.4.2 Incomplete type declarations
// Prior to the end of the corresponding full type declaration, the only allowed use of a name that denotes a type
// declared by an incomplete type declaration is as the type mark in the subtype indication of an access type
// definition; no constraints are allowed in this subtype indication.

use super::*;
use crate::data::SrcPos;

#[test]
fn allows_incomplete_type_definition() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
package pkg is
  type rec_t;
  type rec_t is record
  end record;

  type enum_t;
  type enum_t is (alpha, beta);
end package;
",
    );

    let diagnostics = builder.analyze();
    check_no_diagnostics(&diagnostics);
}

#[test]
fn error_on_duplicate_incomplete_type_definition() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
type rec_t;
type rec_t;
type rec_t is record
end record;
end package;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, duplicates(&code, &["rec_t"]));
}

#[test]
fn error_on_missing_full_type_definition_for_incomplete() {
    let mut builder = LibraryBuilder::new();
    let code_pkg = builder.code(
        "libname",
        "
package pkg is
type rec_t;
end package;

package body pkg is
-- Must appear in the same immediate declarative region
type rec_t is record
end record;
end package body;
",
    );

    let code_ent = builder.code(
        "libname",
        "
entity ent is
end entity;

architecture rtl of ent is
type rec_t;
begin
blk : block
-- Must appear in the same immediate declarative region
type rec_t is record
end record;
begin
end block;
end architecture;
",
    );

    let code_pkg2 = builder.code(
        "libname",
        "
-- To check that no duplicate errors are made when closing the immediate and extended regions
package pkg2 is
type rec_t;
end package;

package body pkg2 is
end package body;
",
    );

    let mut expected_diagnostics = Vec::new();
    for code in [&code_pkg, &code_ent, &code_pkg2].iter() {
        expected_diagnostics.push(missing_full_error(&code.s1("rec_t")));
    }

    expected_diagnostics.push(duplicate(&code_pkg, "rec_t", 1, 2));

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, expected_diagnostics);
}

#[test]
fn incomplete_type_references_point_to_full_definition() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
  type rec_t;
  type access_t is access rec_t;
  type rec_t is record
     node: access_t;
  end record;

  procedure proc(val : rec_t);
end package;
",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);

    // Reference from incomplete goes to full
    for i in 1..=4 {
        assert_eq!(
            root.search_reference_pos(code.source(), code.s("rec_t", i).start()),
            Some(code.s("rec_t", 3).pos()),
            "{i}"
        );
    }

    let references: Vec<_> = (1..=4).map(|idx| code.s("rec_t", idx).pos()).collect();
    assert_eq!(
        root.find_all_references_pos(&code.s("rec_t", 3).pos()),
        references
    );
}

#[test]
fn error_on_missing_full_type_definition_for_incomplete_still_defines_the_type() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
  type rec_t;
  type acces_t is access rec_t;
end package;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, vec![missing_full_error(&code.s1("rec_t"))]);
}

fn missing_full_error(pos: &impl AsRef<SrcPos>) -> Diagnostic {
    let mut error = Diagnostic::error(
        pos,
        "Missing full type declaration of incomplete type 'rec_t'",
    );
    error.add_related(
        pos,
        "The full type declaration shall occur immediately within the same declarative part",
    );
    error
}
