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
fn resolves_names_in_iface_object_decl_init_expressions() {
    check_missing(
        "
package pkg is
  function foo(constant c2 : natural := missing) return natural;
end package;",
    );
}

#[test]
fn search_names_in_iface_object_decl_init_expressions() {
    check_search_reference(
        "
    package pkg is
      constant decl : natural := 0;
      function foo(constant c2 : natural := decl) return natural;
    end package;",
    );
}

#[test]
fn subprogram_parameters_are_visible_in_body() {
    check_missing(
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

    let (root, diagnostics) = builder.get_analyzed_root();
    check_diagnostics(
        diagnostics,
        (1..=2).map(|idx| missing(&code, "missing", idx)).collect(),
    );

    for i in 1..=3 {
        assert_eq!(
            root.search_reference(code.source(), code.s("lab1", i).end()),
            Some(code.s("lab1", 1).pos())
        );
    }

    for i in 1..=2 {
        assert_eq!(
            root.search_reference(code.source(), code.s("lab2", i).end()),
            Some(code.s("lab2", 1).pos())
        );
    }
}

#[test]
fn resolves_names_in_discrete_ranges() {
    check_missing(
        "
package pkg is
  type arr_t is array (natural range missing to missing) of natural;
  type arr2_t is array (missing to missing) of natural;
  type arr3_t is array (missing'range) of natural;
end package;
",
    );
}

#[test]
fn search_names_in_discrete_ranges() {
    check_search_reference(
        "
package pkg is
  constant decl : natural := 0;
  type arr_t is array (natural range decl to decl) of natural;
  type arr2_t is array (decl to decl) of natural;
  type arr3_t is array (decl'range) of natural;
end package;
",
    );
}

#[test]
fn resolves_names_in_subtype_constraints() {
    // @TODO check for missing fields in record element constraints
    check_missing(
        "
package pkg is
  subtype sub1_t is integer_vector(missing to missing);
  subtype sub2_t is integer range missing to missing;

  type rec_t is record
    field : integer_vector;
  end record;

  subtype sub3_t is rec_t(field(missing to missing));

  type uarr_t is array (natural range <>) of integer_vector;
  subtype sub4_t is uarr_t(open)(missing to missing);

end package;
",
    );
}

#[test]
fn search_names_in_subtype_constraints() {
    check_search_reference(
        "
package pkg is
  constant decl : natural := 0;
  subtype sub1_t is integer_vector(decl to decl);
  subtype sub2_t is integer range decl to decl;

  type rec_t is record
    field : integer_vector;
  end record;

  subtype sub3_t is rec_t(field(decl to decl));

  type uarr_t is array (natural range <>) of integer_vector;
  subtype sub4_t is uarr_t(open)(decl to decl);

end package;
",
    );
}

#[test]
fn resolves_names_in_inside_names() {
    check_missing(
        "
package pkg is
end package;

package body pkg is
  type arr2d_t is array (natural range 0 to 1, natural range 0 to 1) of natural;

  function fun(a, b : natural) return natural is
  begin
    return 0;
  end;

  function fun2 return natural is
     -- Function call
     constant c : natural := fun(a => missing, b => missing);
     -- Function call
     constant c2 : natural := fun(missing, missing);

     variable arr : arr2d_t;
     -- Indexed
     constant c3 : natural := arr(missing, missing);

     -- Slice
     constant vec : integer_vector(0 to 1) := (0, 1);
     constant c4 : integer_vector(0 to 1) := vec(missing to missing);

     constant c5 : natural := missing'val(0);
     constant c6 : boolean := boolean'val(missing);
  begin
  end;

end package body;
",
    );
}

#[test]
fn search_names_in_inside_names() {
    check_search_reference(
        "
package pkg is
end package;

package body pkg is
  constant decl : natural := 0;
  type arr2d_t is array (natural range 0 to 1, natural range 0 to 1) of natural;

  function fun(a, b : natural) return natural is
  begin
    return 0;
  end;

  function fun2 return natural is
     -- Function call
     constant c : natural := fun(a => decl, b => decl);
     -- Function call
     constant c2 : natural := fun(decl, decl);

     variable arr : arr2d_t;
     -- Indexed
     constant c3 : natural := arr(decl, decl);

     -- Slice
     constant vec : integer_vector(0 to 1) := (0, 1);
     constant c4 : integer_vector(0 to 1) := vec(decl to decl);

     constant c5 : string := decl'simple_name;
     constant c6 : boolean := boolean'val(decl);
  begin
  end;

end package body;
",
    );
}

#[test]
fn resolves_names_in_aggregates() {
    check_missing(
        "
package pkg is
  -- Named
  constant c0 : integer_vector(0 to 0) := (0 => missing);
  constant c1 : integer_vector(0 to 0) := (missing to missing => 0);

  -- Positional
  constant c2 : integer_vector(0 to 1) := (missing, missing);
end package;
",
    );
}

#[test]
fn search_names_in_aggregates() {
    check_search_reference(
        "
package pkg is
  constant decl : natural := 0;

  -- Named
  constant c0 : integer_vector(0 to 0) := (0 => decl);
  constant c1 : integer_vector(0 to 0) := (decl to decl => 0);

  -- Positional
  constant c2 : integer_vector(0 to 1) := (decl, decl);
end package;
",
    );
}

#[test]
fn resolves_names_in_qualified_expr() {
    check_missing(
        "
package pkg is
  -- Named
  constant c0 : missing := missing'(1 + missing);
end package;
",
    );
}

#[test]
fn search_names_in_qualified_expr() {
    check_search_reference(
        "
package pkg is
  type decl is range 0 to 1;
  -- Named
  constant c0 : decl := decl'(decl'(0));
end package;
",
    );
}

#[test]
fn resolves_names_in_allocators() {
    check_missing(
        "
package pkg is
end package;

package body pkg is
    procedure p is
       type acc_t is access integer_vector;
       -- Qualified
       variable ptr0 : acc_t := new integer_vector'(missing, missing);
       -- Subtype
       variable ptr1 : acc_t := new integer_vector(missing to missing);
    begin
    end procedure;
end package body;
",
    );
}

#[test]
fn search_names_in_allocators() {
    check_search_reference(
        "
package pkg is
end package;

package body pkg is
    constant decl : natural := 0;
    procedure p is
       type acc_t is access integer_vector;
       -- Qualified
       variable ptr0 : acc_t := new integer_vector'(decl, decl);
       -- Subtype
       variable ptr1 : acc_t := new integer_vector(decl to decl);
    begin
    end procedure;
end package body;
",
    );
}

#[test]
fn resolves_names_in_sequential_statements() {
    check_missing(
        "
package pkg is
end package;

package body pkg is
    function f2(c : natural) return natural is
    begin
      return c;
    end;

    function f return natural is
    begin
       -- Variable assignment
       missing := missing;
       missing := missing when missing else missing;

       -- Procedure call
       missing;
       missing(missing);

       -- If statement
       if missing = 1 then
         missing;
       elsif missing = 2 then
         missing;
       else
         missing;
       end if;

       -- Loops
       for i in missing to missing loop
         f2(i); -- Index is defined
         missing;
       end loop;

       loop
         missing;
         next when missing;
         exit when missing;
       end loop;

       while missing loop
         missing;
       end loop;

       -- Case
       case missing is
         when missing =>
           missing;
         when missing to missing =>
           missing;
       end case;

       report missing severity missing;
       assert missing report missing severity missing;

       -- Return
       return missing;
    end;
end package body;
",
    );
}

#[test]
fn search_names_in_sequential_statements() {
    check_search_reference(
        "
package pkg is
end package;

package body pkg is
  procedure proc(c : natural) is
  begin
  end;

  function f return natural is
    variable decl : natural := 0;
  begin
    -- Variable assignment
    decl := decl;
    decl := decl when decl = 0 else decl;

    -- Procedure call
    proc(decl);


    -- If statement
    if decl = 1 then
      proc(decl);
    elsif decl = 2 then
      proc(decl);
    else
      proc(decl);
    end if;

    -- Loops
    for i in decl to decl loop
      proc(decl);
    end loop;

    loop
      proc(decl);
      next when decl = 0;
      exit when decl = 0;
    end loop;

    while decl = 0 loop
      proc(decl);
    end loop;

    -- Case
    case decl is
      when decl =>
        proc(decl);
      when decl to decl =>
        proc(decl);
    end case;

    report natural'image(decl) severity severity_level'val(decl);
    assert decl = 0 report natural'image(decl) severity severity_level'val(decl);

    -- Return
    return decl;
  end;
end package body;
",
    );
}

#[test]
fn check_missing_in_process_statements() {
    check_missing(
        "
entity ent is
end entity;

architecture a of ent is
begin
  main : process is
  begin
    wait on missing until missing = 0 ns for missing;
    missing <= missing after missing;
  end process;
end architecture;
",
    );
}

#[test]
fn search_in_process_statements() {
    check_search_reference(
        "
entity ent is
end entity;

architecture a of ent is
  signal decl : time;
begin
  main : process is
  begin
    wait on decl until decl = 0 ns for decl;
    decl <= decl after decl;
  end process;
end architecture;
",
    );
}

#[test]
fn check_missing_in_instantiations() {
    check_missing(
        "
entity ename is
  generic (g : natural);
  port (s : natural);
end entity;

entity ent is
end entity;

architecture a of ent is
  component comp is
    generic (g : natural);
    port (s : natural);
  end component;
begin
  inst: entity work.ename
    generic map (
      g => missing)
    port map (
      s => missing);

  inst2: component comp
    generic map (
      g => missing)
    port map (
      s => missing);
end architecture;
",
    );
}

#[test]
fn check_search_in_instantiations() {
    check_search_reference(
        "
entity ename is
  generic (g : natural);
  port (s : natural);
end entity;

entity ent is
end entity;

architecture a of ent is
  component comp is
    generic (g : natural);
    port (s : natural);
  end component;

  constant decl : natural := 0;
begin
  inst: entity work.ename
    generic map (
      g => decl)
    port map (
      s => decl);

  inst2: component comp
    generic map (
      g => decl)
    port map (
      s => decl);
end architecture;
",
    );
}

#[test]
fn adds_enum_variants_implicitly() {
    check_missing(
        "
package pkg is
  type enum_t is (alpha, beta);
end package;

use work.pkg.enum_t;
package pkg2 is
  constant c : enum_t := alpha;
  constant c2 : enum_t := missing;
end package;
",
    );
}

#[test]
fn adds_file_subprograms_implicitly() {
    check_missing(
        "
use std.textio.text;

package pkg is
end package;

package body pkg is
  procedure proc is
    file f : text;
  begin
    file_open(f, \"foo.txt\");
    assert not endfile(f);
    file_close(f);
    missing;
  end procedure;
end package body;
",
    );
}

#[test]
fn package_name_visible_in_body() {
    check_missing(
        "
package pkg is
end package;

package body pkg is
  constant name : string := pkg'instance_name;
  constant name2 : string := missing'instance_name;
end package body;
",
    );
}

#[test]
fn search_package_name_in_body() {
    check_search_reference(
        "
package decl is
end package;

package body decl is
  constant name : string := decl'instance_name;
end package body;
",
    );
}

#[test]
fn entity_name_visible_in_architecture() {
    check_missing(
        "
entity ent is
end entity;

architecture a of ent is
begin
  main : process is
  begin
    report missing'instance_name;
    report ent'instance_name;
  end process;
end;
",
    );
}

#[test]
fn search_entity_name_in_entity() {
    check_search_reference(
        "
entity decl is
end entity;

architecture a of decl is
begin
  main : process is
  begin
    report decl'instance_name;
  end process;
end architecture;

",
    );
}

#[test]
fn resolves_names_in_concurrent_statements() {
    check_missing(
        "
entity ent is
end entity;

architecture a of ent is
begin
  missing <= missing;
  missing <= missing when missing else missing;
  missing(missing);
  assert missing report missing severity missing;
end architecture;
",
    );
}

#[test]
fn search_names_in_concurrent_statements() {
    check_search_reference(
        "
entity ent is
end entity;

architecture a of ent is
  procedure proc(signal sig : in natural) is
  begin
  end;

  signal decl : natural := 0;
begin
  decl <= decl;
  decl <= decl when decl = 0 else decl;
  proc(decl);
  assert decl = 0 report decl'instance_name severity severity_level'val(decl);
end architecture;
",
    );
}

#[test]
fn search_for_loop_index() {
    check_search_reference(
        "
entity ent is
end entity;

architecture a of ent is
begin
main : process is
begin
 for decl in 0 to 3 loop
     report integer'image(decl);
 end loop;
end process;
end architecture;

",
    );
}
