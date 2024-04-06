// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

use super::*;
use vhdl_lang::data::error_codes::ErrorCode;

#[test]
fn allows_deferred_constant() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
package pkg is
constant a : natural;
end package;

package body pkg is
constant a : natural := 0;
end package body;
",
    );

    let diagnostics = builder.analyze();
    check_no_diagnostics(&diagnostics);
}

#[test]
fn forbid_deferred_constant_after_constant() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
constant a1 : natural := 0;
constant a1 : natural;
end package;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, duplicates(&code, &["a1"]));
}

#[test]
fn forbid_deferred_constant_outside_of_package_declaration() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
end package;

package body pkg is
constant a1 : natural;
constant a1 : natural := 0;
end package body;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            &code.s1("a1"),
            "Deferred constants are only allowed in package declarations (not body)",
            ErrorCode::IllegalDeferredConstant,
        )],
    );
}

#[test]
fn forbid_full_declaration_of_deferred_constant_outside_of_package_body() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
constant a1 : natural;
constant a1 : natural := 0;
end package;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            &code.s("a1", 1),
            "Deferred constant 'a1' lacks corresponding full constant declaration in package body",
            ErrorCode::MissingDeferredDeclaration
        ),Diagnostic::error(
            &code.s("a1", 2),
            "Full declaration of deferred constant is only allowed in a package body",
            ErrorCode::IllegalDeferredConstant
        )],
    );
}

#[test]
fn error_on_missing_full_constant_declaration() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg_no_body is
constant a1 : natural;
end package;

package pkg is
constant b1 : natural;
end package;

package body pkg is
end package body;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(
                &code.s1("a1"),
                "Deferred constant 'a1' lacks corresponding full constant declaration in package body",
                ErrorCode::MissingDeferredDeclaration
            ),
            Diagnostic::error(
                &code.s1("b1"),
                "Deferred constant 'b1' lacks corresponding full constant declaration in package body",
                ErrorCode::MissingDeferredDeclaration
            ),
        ],
    );
}

#[test]
fn forbid_multiple_constant_after_deferred_constant() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
constant a1 : natural;
end package;

package body pkg is
constant a1 : natural := 0;
constant a1 : natural := 0;
end package body;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, vec![duplicate(&code, "a1", 2, 3)]);
}
