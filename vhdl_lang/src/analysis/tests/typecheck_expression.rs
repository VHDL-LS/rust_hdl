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

#[test]
fn test_enum_literal_expression_typecheck() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
subtype my_bool is boolean;
constant good_a : boolean := true;
constant good_b : my_bool := false;

constant bad_a : integer := true;
constant bad_b : character := false;

        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(
                code.s("true", 2),
                "'true' does not match integer type 'INTEGER'",
            ),
            Diagnostic::error(
                code.s("false", 2),
                "'false' does not match type 'CHARACTER'",
            ),
        ],
    );
}

#[test]
fn test_unique_function_typecheck() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
function fun1 return natural is
begin
    return 0;
end function;

function fun1 return boolean is
begin
    return 0;
end function;

constant good : integer := fun1;
constant bad : character := fun1;
        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s("fun1", 4),
            "'fun1' does not match type 'CHARACTER'",
        )],
    );
}

#[test]
fn test_unique_function_default_arg_typecheck() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
function fun1(arg : natural := 0) return natural is
begin
    return 0;
end function;

function fun1 return boolean is
begin
    return 0;
end function;

constant good : integer := fun1;
constant bad : character := fun1;
        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s("fun1", 4),
            "'fun1' does not match type 'CHARACTER'",
        )],
    );
}

#[test]
fn test_ambiguous_function_default_arg_typecheck() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
function fun1(arg : natural := 0) return natural is
begin
    return 0;
end function;

function fun1(arg : boolean := 0) return natural is
begin
    return 0;
end function;

constant bad: integer := fun1;
        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(code.s("fun1", 3), "ambiguous use of 'fun1'")
                .related(code.s("fun1", 1), "migth be fun1[NATURAL return NATURAL]")
                .related(code.s("fun1", 2), "migth be fun1[BOOLEAN return NATURAL]"),
        ],
    );
}
