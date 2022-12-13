// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2022, Olof Kraigher olof.kraigher@gmail.com

use super::*;

#[test]
fn wrong_number_of_arguments() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
function subpgm(arg: natural) return natural
is begin
end;

signal good : natural := subpgm(0);
signal bad : natural := subpgm;
        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s1("subpgm;").s1("subpgm"),
            "Wrong number of arguments in call to subprogram 'subpgm'",
        )
        .related(code.s1("subpgm"), "migth be subpgm[NATURAL return NATURAL]")],
    );
}
