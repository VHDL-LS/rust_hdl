// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::analysis::tests::LibraryBuilder;
use crate::data::ErrorCode;
use crate::syntax::test::check_diagnostics;
use crate::Diagnostic;

#[test]
fn external_package_name_must_point_to_signal_constant_or_variable() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "\
package test_pkg is
    procedure foo(x: bit);
end package;

use work.test_pkg.all;

entity test_ent is
end entity;

architecture arch of test_ent is
begin
    << signal @work.test_pkg.foo : bit >> <= '1';
end arch;
        ",
    );

    check_diagnostics(
        builder.analyze(),
        vec![Diagnostic::mismatched_kinds(
            code.s1("@work.test_pkg.foo"),
            "External path must point to a constant, variable or signal",
        )],
    );
}
#[test]
fn external_package_name_must_match_target_type() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "\
package test_pkg is
    signal foo : natural;
end package;

use work.test_pkg.all;

entity test_ent is
end entity;

architecture arch of test_ent is
begin
    << signal @work.test_pkg.foo : bit >> <= '1';
end arch;
        ",
    );

    check_diagnostics(
        builder.analyze(),
        vec![Diagnostic::new(
            code.s1("@work.test_pkg.foo"),
            "signal 'foo' of subtype 'NATURAL' does not match type 'BIT'",
            ErrorCode::TypeMismatch,
        )],
    );
}

#[test]
fn external_package_class_must_match_target_class() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "\
package test_pkg is
    shared variable foo : bit;
end package;

use work.test_pkg.all;

entity test_ent is
end entity;

architecture arch of test_ent is
begin
    << signal @work.test_pkg.foo : bit >> <= '1';
end arch;
        ",
    );

    check_diagnostics(
        builder.analyze(),
        vec![Diagnostic::new(
            code.s1("@work.test_pkg.foo"),
            "class signal does not match shared variable",
            ErrorCode::MismatchedObjectClass,
        )],
    );
}
