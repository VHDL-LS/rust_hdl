// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

use super::*;

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
    check_diagnostics(diagnostics, duplication_diagnostics(&code, &["rec_t"]));
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
    for code in [code_pkg, code_ent, code_pkg2].iter() {
        expected_diagnostics.push(Diagnostic::error(
            code.s1("rec_t"),
            "Missing full type declaration of incomplete type 'rec_t'",
        ));
        expected_diagnostics.push(Diagnostic::hint(
            code.s1("rec_t"),
            "The full type declaration shall occur immediately within the same declarative part",
        ));
    }

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, expected_diagnostics);
}
