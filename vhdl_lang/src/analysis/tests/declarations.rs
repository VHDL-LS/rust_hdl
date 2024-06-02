// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com
use crate::analysis::tests::{check_diagnostics, check_no_diagnostics, LibraryBuilder};
use crate::data::error_codes::ErrorCode;
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
            Diagnostic::new(
                code.s1("signal x : bit;"),
                "signal declaration not allowed here",
                ErrorCode::DeclarationNotAllowed,
            ),
            Diagnostic::new(
                code.s1("variable y: natural;"),
                "variable declaration not allowed here",
                ErrorCode::DeclarationNotAllowed,
            ),
        ],
    )
}

// Issue #242
#[test]
pub fn attribute_with_wrong_type() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "\
entity test is
    attribute some_attr : string;
    attribute some_attr of test : signal is \"some value\";
end entity test;
    ",
    );
    let (_, diag) = builder.get_analyzed_root();
    check_diagnostics(
        diag,
        vec![Diagnostic::new(
            code.s1("test : signal").s1("test"),
            "entity 'test' is not of class signal",
            ErrorCode::MismatchedEntityClass,
        )],
    )
}

#[test]
pub fn attribute_sees_through_aliases() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "\
entity test is
    port (
        clk: in bit
    );
    alias aliased_clk is clk;
    attribute some_attr : string;
    attribute some_attr of aliased_clk : entity is \"some value\";
end entity test;
    ",
    );
    let (_, diag) = builder.get_analyzed_root();
    check_diagnostics(
        diag,
        vec![Diagnostic::new(
            code.s1("aliased_clk : entity").s1("aliased_clk"),
            "port 'clk' : in is not of class entity",
            ErrorCode::MismatchedEntityClass,
        )],
    )
}

#[test]
pub fn disconnect_no_analyze_error() {
    let mut builder = LibraryBuilder::new();
    let _ = builder.code(
        "libname",
        "\
entity ent is
end entity;
architecture arch of ent is
  constant foo : time := 10 ns;
  disconnect all : integer after foo;
begin
end arch;
    ",
    );
    check_no_diagnostics(&builder.analyze())
}

#[test]
pub fn disconnect_expression_error_1() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "\
entity ent is
end entity;
architecture arch of ent is
  constant foo : time := 10 ns;
  disconnect all : integer after bar;
begin
end arch;
    ",
    );
    check_diagnostics(
        builder.analyze(),
        vec![Diagnostic::new(
            code.s1("bar"),
            "No declaration of 'bar'",
            ErrorCode::Unresolved,
        )],
    )
}

#[test]
pub fn disconnect_expression_error_2() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "\
entity ent is
end entity;
architecture arch of ent is
  signal foo : integer;
  disconnect all : integer after foo;
begin
end arch;
    ",
    );
    check_diagnostics(
        builder.analyze(),
        vec![Diagnostic::new(
            code.s1("after foo").s1("foo"),
            "signal 'foo' of integer type 'INTEGER' does not match physical type 'TIME'",
            ErrorCode::TypeMismatch,
        )],
    )
}

#[test]
pub fn disconnect_type_error_1() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "\
entity ent is
end entity;
architecture arch of ent is
  signal foo : integer;
  disconnect all : bar after 10 ns;
begin
end arch;
    ",
    );
    check_diagnostics(
        builder.analyze(),
        vec![
            Diagnostic::new(
                code.s1("bar"),
                "No declaration of 'bar'",
                ErrorCode::Unresolved,
            ),
            Diagnostic::new(
                code.s1("bar"),
                "No declaration of 'bar'",
                ErrorCode::Unresolved,
            ),
        ],
    )
}
