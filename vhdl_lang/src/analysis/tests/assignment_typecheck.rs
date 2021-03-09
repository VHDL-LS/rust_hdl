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
        Diagnostic::error(code.s("foo1", 2), "Invalid assignment target"),
        Diagnostic::error(code.s("foo2", 2), "Invalid assignment target"),
    ];

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, expected);
}

#[test]
fn attribute_name_may_not_be_assignment_target() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
end entity;

architecture a of ent is
  signal foo : boolean;
begin
  main : process
  begin
    foo'stable := 1;
  end process;
end architecture;
",
    );

    let expected = vec![Diagnostic::error(
        code.s("foo'stable", 1),
        "Invalid assignment target",
    )];

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, expected);
}

#[test]
fn subprogram_call_may_not_be_assignment_target() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
  function foo1(arg : natural) return natural;
end package;
      
package body pkg is
  function foo1(arg : natural) return natural is
  begin
    return 0;
  end function;
end package body;

entity ent is
end entity;

architecture a of ent is
    function foo2(arg : natural) return natural is
    begin
      return 0;
    end function;
begin
  main : process
  begin
    work.pkg.foo1(2) := 1;
    foo2(2) := 1;
    work.pkg.foo1(arg => 2) := 1;
    foo2(arg => 2) := 1;    
  end process;
end architecture;
",
    );

    let expected = vec![
        Diagnostic::error(code.s("work.pkg.foo1", 1), "Invalid assignment target"),
        Diagnostic::error(code.s("foo2", 2), "Invalid assignment target"),
        Diagnostic::error(
            code.s("work.pkg.foo1(arg => 2)", 1),
            "Invalid assignment target",
        ),
        Diagnostic::error(code.s("foo2(arg => 2)", 1), "Invalid assignment target"),
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
fn indexed_names_may_be_assignment_target() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
entity ent is
end entity;

architecture a of ent is

  type arr1_t is array (natural range 0 to 1) of natural;
  type arr2_t is array (natural range 0 to 1, natural range 0 to 1) of natural;
  type arr3_t is array (natural range 0 to 1) of arr1_t;

  signal foo1 : arr1_t;
  signal foo2 : arr2_t;
  signal foo3 : arr3_t;
begin
  foo1(0) <= 0;
  foo2(0, 0) <= 0;
  foo3(0, 0)(0) <= 0;
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
