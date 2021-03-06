// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

use super::*;

#[test]
fn overloaded_name_may_not_be_assignment_target() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
end entity;

architecture a of ent is
    function foo1 return natural is
    begin
        return 0;
    end;

    type enum_t is (foo2, enum_value);
begin
  main : process
  begin
    foo1 := 1;
    foo2 := 1;
  end process;
end architecture;
",
    );

    let expected = vec![
        Diagnostic::error(code.s("foo1", 2), "not a valid assignment target"),
        Diagnostic::error(code.s("foo2", 2), "not a valid assignment target"),
    ];

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, expected);
}

#[test]
fn constant_may_not_be_assignment_target() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
end entity;

architecture a of ent is
  constant foo1 : natural := 0;
  alias foo2 is foo1;
begin
  main : process
  begin
    foo1 := 1;
    foo2 := 1;
  end process;
end architecture;
",
    );

    let expected = vec![
        Diagnostic::error(
            code.s("foo1", 3),
            "constant 'foo1' may not be the target of an assignment",
        ),
        Diagnostic::error(
            code.s("foo2", 2),
            "alias 'foo2' of constant 'foo1' may not be the target of an assignment",
        ),
    ];

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, expected);
}

#[test]
fn objects_may_be_assignment_target() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
entity ent is
end entity;

architecture a of ent is
  signal foo1 : natural := 0;
  alias foo2 is foo1;
  shared variable foo3 : natural := 0;
begin
  main : process
    variable foo4 : natural := 0;
  begin
    foo1 <= 1;
    foo2 <= 1;
    foo3 := 1;
    foo4 := 1;
  end process;
end architecture;
",
    );

    let diagnostics = builder.analyze();
    check_no_diagnostics(&diagnostics);
}

#[test]
fn interface_objects_may_be_assignment_target() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
entity ent is
end entity;

architecture a of ent is
    procedure proc1(signal foo : out natural) is
    begin
       foo <= 1;
    end;
begin
  main : process
    procedure proc2(variable foo : inout natural) is
    begin
       foo := 1;
    end;
  begin
  end process;
end architecture;
",
    );

    let diagnostics = builder.analyze();
    check_no_diagnostics(&diagnostics);
}

#[test]
fn interface_constant_may_not_be_assignment_target() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
end entity;

architecture a of ent is
begin
    main : process
        procedure proc1(constant foo1 : natural) is
        begin
            foo1 := 1;
        end;
        procedure proc2(variable foo2 : in natural) is
        begin
            foo2 := 1;
        end;        
    begin
    end process;
end architecture;
",
    );

    let expected = vec![
        Diagnostic::error(
            code.s("foo1", 2),
            "interface constant 'foo1' may not be the target of an assignment",
        ),
        Diagnostic::error(
            code.s("foo2", 2),
            "interface variable 'foo2' : in may not be the target of an assignment",
        ),
    ];

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, expected);
}

#[test]
fn checks_signal_vs_variable_assignment_target() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
end entity;

architecture a of ent is
    signal foo3 : natural;
begin
    main : process
        variable foo4 : natural;
    
        procedure proc1(signal foo1 : out natural) is
        begin
            foo1 := 1;
        end;
        procedure proc2(variable foo2 : out natural) is
        begin
            foo2 <= 1;
        end;        
    begin
        foo3 := 1;
        foo4 <= 1;
    end process;
end architecture;
",
    );

    let expected = vec![
        Diagnostic::error(
            code.s("foo1", 2),
            "interface signal 'foo1' : out may not be the target of a variable assignment",
        ),
        Diagnostic::error(
            code.s("foo2", 2),
            "interface variable 'foo2' : out may not be the target of a signal assignment",
        ),
        Diagnostic::error(
            code.s("foo3", 2),
            "signal 'foo3' may not be the target of a variable assignment",
        ),
        Diagnostic::error(
            code.s("foo4", 2),
            "variable 'foo4' may not be the target of a signal assignment",
        ),
    ];

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, expected);
}
