// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

use super::*;

#[test]
fn test_integer_literal_expression_typecheck() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
constant good_a : integer := 1;
constant good_b : natural := 2;

constant bad_a : boolean := 3;

subtype my_bool is boolean;
constant bad_b : my_bool := 4;

        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(
                code.s1("3"),
                "integer literal does not match type 'BOOLEAN'",
            ),
            Diagnostic::error(
                code.s1("4"),
                "integer literal does not match subtype 'my_bool'",
            ),
        ],
    );
}

#[test]
fn test_physical_literal_expression_typecheck() {
    let mut builder = LibraryBuilder::new();
    builder.in_declarative_region(
        "
constant good_a : time := ns;
constant good_b : time := 2 ps;
        ",
    );

    let diagnostics = builder.analyze();
    check_no_diagnostics(&diagnostics);
}

#[test]
fn test_integer_selected_name_expression_typecheck() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
constant ival : integer := 999;

type rec_t is record
  elem : natural;
end record;

constant rval : rec_t := (elem => 0);

constant good_a : integer := ival;
constant good_b : natural := rval.elem;

constant bad_a : boolean := ival;

subtype my_bool is boolean;
constant bad_b : my_bool := rval.elem;

        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(
                code.s("ival", 3),
                "constant 'ival' does not match type 'BOOLEAN'",
            ),
            Diagnostic::error(
                code.s("rval.elem", 2),
                "element declaration 'elem' does not match subtype 'my_bool'",
            ),
        ],
    );
}
