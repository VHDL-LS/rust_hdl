// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

use super::*;

#[test]
fn resolves_names_in_object_decl_init_expressions() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
  constant c0 : natural := 0;
  constant c1 : natural := c0;
  constant c2 : natural := missing;
  constant c3 : natural := 1 + missing;
  constant c4 : natural := - missing;
end package;",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_diagnostics(
        diagnostics,
        (1..=3).map(|idx| missing(&code, "missing", idx)).collect(),
    );

    // Goto declaration from declaration
    assert_eq!(
        root.search_reference(code.source(), code.s("c0", 1).end()),
        Some(code.s("c0", 1).pos())
    );

    // Goto declaration from reference
    assert_eq!(
        root.search_reference(code.source(), code.s("c0", 2).end()),
        Some(code.s("c0", 1).pos())
    );
}

#[test]
fn subprogram_parameters_are_visible_in_body() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
end package;

package body pkg is
  function foo(c0 : natural) return natural is
     constant c1 : natural := c0;
     constant c2 : natural := missing;
  begin
     return c1;
  end;
end package body;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        (1..=1).map(|idx| missing(&code, "missing", idx)).collect(),
    );
}

/// Check that at least the prefix is resolved also for names which are not purely selected names
#[test]
fn resolves_names_for_prefix_of_non_selected() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
  type arr_t is array (natural range 0 to 1) of integer_vector(0 to 1);
  constant c0 : integer_vector(0 to 1) := (0,1);
  constant c1 : natural := c0(0);
  constant c2 : natural := missing(0);
  constant c3 : integer_vector(0 to 1) := c0(0 to 1);
  constant c4 : integer_vector(0 to 1) := missing(0 to 1);
  constant c5 : arr_t := (0 => (0,1), 1 => (2, 3));

  -- This was a bug at one point
  constant c6 : natural := c5(0)'length;
  constant c7 : natural := missing(0)'length;

  -- This was also a but at one point
  type rec_t is record
      field : natural;
  end record;
  type rec_arr_t is array (natural range <>) of rec_t;
  constant c8 : rec_arr_t(0 to 0) := (0 => (field => 0));
  constant c9 : natural := c8(0).field;
  constant ca : natural := missing(0).field;
  constant cb : rec_t := (field => 0);
  constant cc : natural := cb.field;
  constant cd : natural := missing.field;

end package;",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_diagnostics(
        diagnostics,
        (1..=5).map(|idx| missing(&code, "missing", idx)).collect(),
    );

    // Goto declaration from reference that is prefix of indexed/function call
    assert_eq!(
        root.search_reference(code.source(), code.s("c0", 2).end()),
        Some(code.s("c0", 1).pos())
    );

    // Goto declaration from reference that is prefix of slice
    assert_eq!(
        root.search_reference(code.source(), code.s("c0", 3).end()),
        Some(code.s("c0", 1).pos())
    );
}

#[test]
fn labels_are_visible() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
end entity;

architecture a of ent is
begin
  lab1 : process
     constant name1 : string := lab1'instance_name;
     constant dummy : string := missing'instance_name;
  begin
  end process;

  lab2 : block is
     constant name1 : string := lab1'instance_name;
     constant name2 : string := lab2'instance_name;
     constant dummy : string := missing'instance_name;
  begin
  end block;
end architecture;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        (1..=2).map(|idx| missing(&code, "missing", idx)).collect(),
    );
}
