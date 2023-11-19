// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com
use crate::analysis::tests::{check_diagnostics, LibraryBuilder};
use crate::Diagnostic;

#[test]
pub fn declaration_not_allowed_everywhere() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "\
entity ent is
end entity;

architecture arch of ent is

function my_func return natural is
    signal x : bit;
begin

end my_func;
begin

    my_block : block
        variable y: natural;
    begin
    end block my_block;

end architecture;
    ",
    );
    check_diagnostics(
        builder.analyze(),
        vec![
            Diagnostic::error(
                code.s1("signal x : bit;"),
                "signal declaration not allowed here",
            ),
            Diagnostic::error(
                code.s1("variable y: natural;"),
                "variable declaration not allowed here",
            ),
        ],
    )
}
