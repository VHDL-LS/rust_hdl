// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

use super::*;
use pretty_assertions::assert_eq;
use vhdl_lang::data::error_codes::ErrorCode;

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
        root.search_reference_pos(code.source(), code.s("c0", 1).end()),
        Some(code.s("c0", 1).pos())
    );

    // Goto declaration from reference
    assert_eq!(
        root.search_reference_pos(code.source(), code.s("c0", 2).end()),
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

  -- This was also a bug at one point
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
        root.search_reference_pos(code.source(), code.s("c0", 2).end()),
        Some(code.s("c0", 1).pos())
    );

    // Goto declaration from reference that is prefix of slice
    assert_eq!(
        root.search_reference_pos(code.source(), code.s("c0", 3).end()),
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
            root.search_reference_pos(code.source(), code.s("lab1", i).end()),
            Some(code.s("lab1", 1).pos())
        );
    }

    for i in 1..=2 {
        assert_eq!(
            root.search_reference_pos(code.source(), code.s("lab2", i).end()),
            Some(code.s("lab2", 1).pos())
        );
    }
}

#[test]
fn resolves_names_in_discrete_ranges() {
    check_missing(
        "
package pkg is
  type arr0_t is array (natural range missing to 0) of natural;
  type arr1_t is array (natural range 0 to missing) of natural;
  type arr2_t is array (missing to 0) of natural;
  type arr3_t is array (0 to missing) of natural;
  type arr4_t is array (missing'range) of natural;
end package;
",
    );
}

#[test]
fn search_names_in_discrete_ranges() {
    check_search_reference(
        "
package pkg is
  constant decl : integer_vector(0 to 1) := (others => 0);
  type arr_t is array (natural range decl(0) to decl(0)) of natural;
  type arr2_t is array (decl(0) to decl(0)) of natural;
  type arr3_t is array (decl'range) of natural;
end package;
",
    );
}

#[test]
fn resolves_names_in_subtype_constraints() {
    check_missing(
        "
package pkg is
  subtype sub1_t is integer_vector(missing to 0);
  subtype sub2_t is integer range 0 to missing;

  type rec_t is record
    field : integer_vector;
  end record;

  subtype sub3_t is rec_t(field(missing to 0));

  type uarr_t is array (natural range <>) of integer_vector;
  subtype sub4_t is uarr_t(0 to missing);

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
  subtype sub4_t is uarr_t(decl to decl);

end package;
",
    );
}

#[test]
fn search_names_in_integer_type_declaration_ranges() {
    check_search_reference(
        "
package pkg is
  constant decl : natural := 0;
  type int_t is range 0 to decl;
end package;
",
    );
}

#[test]
fn search_names_in_integer_type_declaration() {
    check_search_reference(
        "
package pkg is
  type decl is range 0 to 3;
  constant foo : decl := 0;
end package;
",
    );
}

#[test]
fn search_names_in_file_type_declaration() {
    check_search_reference(
        "
package pkg is
  subtype decl is character;
  type foo is file of decl;
end package;
",
    );
}

#[test]
fn resolves_names_inside_names() {
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
     constant c4 : integer_vector(0 to 1) := vec(missing to 0);

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
  constant c1 : integer_vector(0 to 0) := (missing to 0 => 0);

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
       variable ptr1 : acc_t := new integer_vector(0 to missing);
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
    procedure proc2(c : natural) is
    begin
      return;
    end;

    function f return natural is
    begin
       -- Variable assignment
       missing := missing;
       missing := missing when missing else missing;
       with missing select
         missing := missing when missing,
                    missing when others;

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
       for i in missing to 0 loop
         for j in 0 to missing loop
         end loop;

         proc2(i); -- Index is defined
         missing;

         exit missing;
         next missing;
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
         when 0 to missing =>
           missing;
         when missing to 0 =>
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
    with decl select
      decl := decl when decl,
              decl when others;

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
  main : process(missing) is
  begin
    wait on missing until missing = 0 ns for missing;
    missing <= missing after missing;
    missing <= force missing;
    missing <= release;
    with missing select
       missing <= missing when missing,
                  missing when others;

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
  main : process (decl) is
  begin
    wait on decl until decl = 0 ns for decl;
    decl <= decl after decl;
    decl <= force decl;
    decl <= release;
    with decl select
       decl <= decl when decl,
               decl when others;
  end process;
end architecture;
",
    );
}

#[test]
fn search_in_aggregate_target() {
    check_search_reference(
        "
entity ent is
end entity;

architecture a of ent is
  signal decl : natural;
begin
  main : process is
  begin
   (0 => decl) := (0 => 1);
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
fn package_name_visible_in_header_and_body() {
    check_code_with_no_diagnostics(
        "
package pkg is
  constant name1 : string := pkg'instance_name;
end package;

package body pkg is
  constant name2 : string := pkg'instance_name;
end package body;
",
    );
}

#[test]
fn search_package_name_in_header_and_body() {
    check_search_reference(
        "
package decl is
  constant name1 : string := decl'instance_name;
end package;

package body decl is
  constant name2 : string := decl'instance_name;
end package body;
",
    );
}

#[test]
fn unit_name_visible_in_entity_architecture() {
    check_code_with_no_diagnostics(
        "
entity ent is
begin
  p1 : process is
  begin
    report ent'instance_name;
  end process;
end entity;

architecture a of ent is
begin
  p2 : process is
  begin
    report ent'instance_name;
    report a'instance_name;
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
  with missing select
     missing <= missing when missing,
                missing when others;
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
  with decl select
     decl <= decl when decl,
             decl when others;
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

#[test]
fn search_for_generate_index_range() {
    check_search_reference(
        "
entity ent is
end entity;

architecture a of ent is
  constant decl : natural := 0;
begin
 gen: for i in decl to decl + 3 generate
 end generate;
end architecture;
",
    );
}

#[test]
fn search_for_generate_index() {
    check_search_reference(
        "
entity ent is
end entity;

architecture a of ent is
  signal foo : integer_vector(0 to 3);
begin
 gen: for decl in foo'range generate
    foo(decl) <= 0;
 end generate;
end architecture;

",
    );
}

#[test]
fn search_if_generate_conditions() {
    check_search_reference(
        "
entity ent is
end entity;

architecture a of ent is
  constant decl : natural := 0;
  signal foo : natural;
begin
 gen: if decl = 0 generate
   foo <= decl;
 else generate
   foo <= decl + 1;
 end generate;
end architecture;
",
    );
}

#[test]
fn search_generate_alternate_labels() {
    check_search_reference(
        "
entity ent is
end entity;

architecture a of ent is
begin
 gen: if decl: true generate
 end generate;
end architecture;
",
    );
}

#[test]
fn resolves_missing_name_in_alias() {
    check_missing(
        "
package pkg is
  alias a is missing[natural];
  alias b is maximum[missing];
end package;
",
    );
}

#[test]
fn search_name_in_alias() {
    check_search_reference(
        "
package pkg is
  type decl is (alpha, beta);
  function fun(arg : decl) return decl;
  procedure proc(arg : decl);
  alias a is fun[decl return decl];
  alias b is proc[decl];
  alias c is decl;
end package;
",
    );
}

#[test]
fn search_external_name() {
    check_search_reference(
        "
package pkg is
  type decl is (alpha, beta);
end package;

entity ent2 is
end entity;

use work.pkg.all;

architecture a of ent2 is
  signal foo : decl;
begin
end architecture;

entity ent is
end entity;

use work.pkg.all;

architecture a of ent is
  signal foo : decl;
begin
  inst : entity work.ent2;
  foo <= << signal inst.foo : decl >>;
end architecture;
",
    );
}

#[test]
fn block_names_are_visible() {
    check_code_with_no_diagnostics(
        "
entity ent is
  port (ent_in : integer);
end entity;

architecture a of ent is
  signal sig : integer;
begin
  blk: block (ent_in = 1) is
    generic( gen : integer := 0 );
    generic map ( gen => 1);
    port(
      prt_in : in integer := 0;
      prt_out : out integer := 0
    );
    port map (
      prt_in => ent_in + sig,
      prt_out => open
    );
  begin
    prt_out <= gen + prt_in + sig;
  end block;
end architecture;
",
    );
}

#[test]
fn error_on_signature_for_non_overloaded_alias() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
  type enum_t is (alpha, beta);
  alias alias_t is enum_t[return integer];
end package;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::new(
            code.s1("[return integer]"),
            "Alias should only have a signature for subprograms and enum literals",
            ErrorCode::IllegalSignature,
        )],
    );
}

#[test]
fn error_on_non_signature_for_overloaded_alias() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
end package;

package body pkg is
  procedure subpgm(arg: natural) is
  begin
  end;

  alias alias_t is subpgm;
end package body;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::new(
            code.s("subpgm", 2),
            "Signature required for alias of subprogram and enum literals",
            ErrorCode::SignatureRequired,
        )],
    );
}

#[test]
fn signatures_are_compared_with_base_type() {
    check_code_with_no_diagnostics(
        "
package pkg is
end package;

package body pkg is
  subtype sub_type is natural range 0 to 5;
  alias type_alias is sub_type;
  subtype sub_type2 is type_alias range 0 to 2;

  function subpgm(arg: sub_type2) return sub_type2 is
  begin
  end;

  alias alias1 is subpgm[integer return integer];
  alias alias2 is subpgm[type_alias return type_alias];
  alias alias3 is subpgm[sub_type return sub_type];
  alias alias4 is subpgm[sub_type2 return sub_type2];
end package body;
",
    );
}

#[test]
fn can_goto_declaration_of_alias_with_signature() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
end package;

package body pkg is
  function subpgm(arg: natural) return natural is
  begin
  end;

  function subpgm(arg: boolean) return boolean is
  begin
  end;

  alias alias1 is subpgm[boolean return boolean];
  alias alias2 is subpgm[integer return integer];
end package body;
",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);

    // Goto declaration from declaration
    assert_eq!(
        root.search_reference_pos(code.source(), code.s("subpgm", 3).end()),
        Some(code.s("subpgm", 2).pos())
    );

    assert_eq!(
        root.search_reference_pos(code.source(), code.s("subpgm", 4).end()),
        Some(code.s("subpgm", 1).pos())
    );
}

#[test]
fn overloaded_name_can_be_selected() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
package pkg is
end package;

package body pkg is
  type rec_t is record
    f : natural;
  end record;

  function foo return rec_t is
    variable t : rec_t;
  begin
    return t;
  end function;

  procedure bar is
    variable s : natural;
  begin
    s := foo.f;
  end; 
end package body;
",
    );

    let diagnostics = builder.analyze();
    check_no_diagnostics(&diagnostics);
}

#[test]
fn record_fields_are_resolved() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is

  type rec2_t is record
    field2 : natural;
  end record;

  type rec1_t is record
    field1 : rec2_t;
  end record;

  constant rec_val : rec1_t := (field1 => (field2 => 0));
  alias rec_alias is rec_val;
  
  -- Good
  constant a : rec2_t := rec_val.field1;
  constant b : natural := rec_val.field1.field2;
  constant c : rec2_t := rec_alias.field1;
  constant d : natural := rec_alias.field1.field2;
  
  -- Bad
  constant e : natural := rec_val.missing;
  constant f : natural := rec_val.field1.missing;
  constant g : natural := rec_alias.missing;
  constant h : natural := rec_alias.field1.missing;
end package;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::new(
                code.s("missing", 1),
                "No declaration of 'missing' within record type 'rec1_t'",
                ErrorCode::Unresolved,
            ),
            Diagnostic::new(
                code.s("missing", 2),
                "No declaration of 'missing' within record type 'rec2_t'",
                ErrorCode::Unresolved,
            ),
            Diagnostic::new(
                code.s("missing", 3),
                "No declaration of 'missing' within record type 'rec1_t'",
                ErrorCode::Unresolved,
            ),
            Diagnostic::new(
                code.s("missing", 4),
                "No declaration of 'missing' within record type 'rec2_t'",
                ErrorCode::Unresolved,
            ),
        ],
    );
}

#[test]
fn find_all_references_of_record_field() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
end entity;

architecture a of ent is
  type rec_t is record
    field : natural;
  end record;

  signal sig : rec_t;
begin
  sig.field <= 1;
end architecture;",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);

    let references = vec![code.s("field", 1).pos(), code.s("field", 2).pos()];

    assert_eq_unordered(
        &root.find_all_references_pos(&code.s1("field").pos()),
        &references,
    );
}

#[test]
fn record_subtype_can_be_selected() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
end package;

package body pkg is
  type rec_t is record
    elem : integer_vector;
  end record;
  
  subtype sub_t is rec_t(elem(0 to 1));

  constant const1 : sub_t := (elem => (0, 1));
  
  -- Ok
  constant const2 : integer := const1.elem(0);

  -- Not ok
  constant const3 : integer := const1.missing;

end package body;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::new(
            code.s("missing", 1),
            "No declaration of 'missing' within record type 'rec_t'",
            ErrorCode::Unresolved,
        )],
    );
}

#[test]
fn acccess_type_of_record_can_be_selected() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
end;

architecture a of ent is
  type rec_t is record
    elem : integer;
  end record;

  type rec_access_t is access rec_t;
begin

  main : process
    variable avar : rec_access_t := new rec_t'(elem => 0);
    variable v : integer;
  begin
     -- Ok
     v := avar.elem;
     -- Not ok
     v := avar.missing;
  end process;

end architecture;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::new(
            code.s("missing", 1),
            "No declaration of 'missing' within record type 'rec_t'",
            ErrorCode::Unresolved,
        )],
    );
}

#[test]
fn protected_type_can_be_selected() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
end;

architecture a of ent is
  type prot_t is protected
    function foo return natural;
  end protected;

  type prot_t is protected body
    function foo return natural is
    begin
      return 0;
    end;
  end protected body;

  shared variable pvar : prot_t;
begin

  main : process
      variable v : natural;
  begin
     -- Ok
     v := pvar.foo;

     -- Not ok
     v := pvar.missing;
  end process;

end architecture;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::new(
            code.s("missing", 1),
            "No declaration of 'missing' within protected type 'prot_t'",
            ErrorCode::Unresolved,
        )],
    );
}

#[test]
fn incomplete_access_type_of_record_can_be_selected() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
end;

architecture a of ent is

  type rec_t;
  type rec_access_t is access rec_t;

  type rec_t is record
    elem : natural;
    child : rec_access_t;
  end record;

begin

  main : process
      variable rvar : rec_t := (elem => 0, child => null);
      variable v : natural;
  begin
     -- Ok
     v := rvar.elem;
     v := rvar.child.elem;
     -- Not ok
     v := rvar.missing;
     v := rvar.child.missing;
  end process;

end architecture;
",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::new(
                code.s("missing", 1),
                "No declaration of 'missing' within record type 'rec_t'",
                ErrorCode::Unresolved,
            ),
            Diagnostic::new(
                code.s("missing", 2),
                "No declaration of 'missing' within record type 'rec_t'",
                ErrorCode::Unresolved,
            ),
        ],
    );
}

#[test]
fn hover_for_physical_type_units() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
type time_t is range -9223372036854775807 to 9223372036854775807
units
  small;
  big = 1000 small;
end units;

constant the_time1 : time_t := small;
constant the_time2 : time_t := 1000 small;
constant the_time3 : time_t := big;
constant the_time4 : time_t := 1000 big;
    
",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);

    let time_t = root
        .search_reference(code.source(), code.s1("time_t").start())
        .unwrap();
    assert_eq!(time_t.decl_pos().unwrap(), code.s1("time_t").pos().as_ref());
    assert_eq!(
        root.find_all_references(time_t),
        vec![
            code.s("time_t", 1).pos(),
            code.s("time_t", 2).pos(),
            code.s("time_t", 3).pos(),
            code.s("time_t", 4).pos(),
            code.s("time_t", 5).pos(),
        ]
    );

    for i in 0..3 {
        let ent = root
            .search_reference(code.source(), code.s("small", 1 + i).start())
            .unwrap();
        assert_eq!(ent.decl_pos().unwrap(), &code.s1("small").pos());
        assert_eq!(root.format_declaration(ent), Some("small".to_string()));
    }

    for i in 0..2 {
        let ent = root
            .search_reference(code.source(), code.s("big", 1 + i).start())
            .unwrap();
        assert_eq!(ent.decl_pos().unwrap(), &code.s1("big").pos());
        assert_eq!(root.format_declaration(ent), Some("1000 small".to_string()));
    }
}

#[test]
fn resolve_record_aggregate_choices() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
type rec_t is record
  field : natural;
end rec_t;

constant good : rec_t := (field => 0);
constant bad : rec_t := (missing => 0);
",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::new(
            code.s1("missing"),
            "No declaration of 'missing' within record type 'rec_t'",
            ErrorCode::Unresolved,
        )],
    );
    let field = root
        .search_reference(code.source(), code.s("field", 2).start())
        .unwrap();
    assert_eq!(field.decl_pos().unwrap(), code.s1("field").pos().as_ref());
    assert_eq!(
        root.find_all_references(field),
        vec![code.s("field", 1).pos(), code.s("field", 2).pos(),]
    );

    assert_eq!(
        root.format_declaration(field),
        Some("field : natural".to_string())
    );
}

#[test]
fn unary_operator() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
constant i0 : integer := 0;
constant good1 : integer := - i0;
        ",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    let integer = root.find_standard_symbol("INTEGER");

    let op_pos = code.s1("-");
    let minus = root
        .search_reference(code.source(), op_pos.start())
        .unwrap();
    assert_eq!(minus.decl_pos(), integer.decl_pos());

    check_no_diagnostics(&diagnostics);
}

#[test]
fn binary_operator() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
constant i0 : integer := 0;
constant good1 : integer := i0 + i0;
        ",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    let integer = root.find_standard_symbol("INTEGER");

    let op_pos = code.s1("+");
    let minus = root
        .search_reference(code.source(), op_pos.start())
        .unwrap();
    assert_eq!(minus.decl_pos(), integer.decl_pos());

    check_no_diagnostics(&diagnostics);
}

#[test]
fn attribute_happy_path() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
constant c0 : string := \"block\";
attribute ram_style : string;
signal ram : integer_vector(0 to 15);

attribute ram_style of ram : signal is c0;
        ",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);

    {
        // References in the expression
        let ref_pos = code.s1("signal is c0;").s1("c0");
        let decl = root
            .search_reference(code.source(), ref_pos.start())
            .unwrap();
        assert_eq!(code.s1("c0").pos(), decl.decl_pos().cloned().unwrap());
    }
    {
        // References to the attribute itself
        let ref_pos = code.s1("attribute ram_style of").s1("ram_style");
        let decl = root
            .search_reference(code.source(), ref_pos.start())
            .unwrap();
        assert_eq!(
            code.s1("attribute ram_style").s1("ram_style").pos(),
            decl.decl_pos().cloned().unwrap()
        );
    }

    {
        // References to the named entity
        let ref_pos = code.s1("of ram").s1("ram");
        let decl = root
            .search_reference(code.source(), ref_pos.start())
            .unwrap();
        assert_eq!(
            code.s1("signal ram").s1("ram").pos(),
            decl.decl_pos().cloned().unwrap()
        );
    }
}

#[test]
fn attribute_missing_names() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
constant c0 : string := \"block\";
attribute ram_style : string;
signal ram : integer_vector(0 to 15);

attribute missing1 of ram : signal is c0;
attribute ram_style of missing2 : signal is c0;
attribute ram_style of ram : signal is missing3;
        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::new(
                code.s1("missing1"),
                "No declaration of 'missing1'",
                ErrorCode::Unresolved,
            ),
            Diagnostic::new(
                code.s1("missing2"),
                "No declaration of 'missing2'",
                ErrorCode::Unresolved,
            ),
            Diagnostic::new(
                code.s1("missing3"),
                "No declaration of 'missing3'",
                ErrorCode::Unresolved,
            ),
        ],
    );
}

#[test]
fn attribute_spec_with_non_attribute() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
constant bad : natural := 0;
signal ram : integer_vector(0 to 15);
attribute bad of ram : signal is 0;
        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::mismatched_kinds(
            code.s1("attribute bad").s1("bad"),
            "constant 'bad' is not an attribute",
        )],
    );
}

#[test]
fn selected_function_is_resolved() {
    // This test case exists because a bug was found
    // where a the arguments of a selected name prefix
    // were not resolved because the 'myfun' name already had a referenc set
    // which cause the disambiguation not to run
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
   function myfun(arg : integer) return boolean;
end package;

entity ent is
end entity;

architecture a of ent is
   constant c0 : integer := 0;
   constant c1 : boolean := work.pkg.myfun(c0);
begin
end architecture;
        ",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);

    assert!(root
        .search_reference(code.source(), code.s1("myfun(c0)").s1("c0").start())
        .is_some());
}

#[test]
fn subpgm_references_includes_both_definition_and_declaration() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
  package pkg is
    function myfun(arg : integer) return integer;
end package;

package body pkg is
    function myfun(arg : integer) return integer is
    begin
        return 0;
    end;
end package body;

entity ent is
    
end entity;

architecture a of ent is
    constant c0 : natural := work.pkg.myfun(0);
begin
end;
",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);

    let decl = root
        .search_reference(code.source(), code.sa("work.pkg.", "myfun").start())
        .unwrap();

    assert_eq!(
        root.find_all_references(decl),
        vec![
            code.s("myfun", 1).pos(),
            code.s("myfun", 2).pos(),
            code.s("myfun", 3).pos()
        ]
    );

    assert_eq!(
        root.find_definition_of(
            root.search_reference(code.source(), code.s1("myfun").start())
                .unwrap()
        )
        .unwrap()
        .decl_pos(),
        Some(&code.s("myfun", 2).pos())
    );
}

#[test]
fn find_all_references_of_deferred_constant() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
    constant c0 : natural;
end package;


package body pkg is
  constant c0 : natural := 0;
end package body;
      ",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);

    let references = vec![code.s("c0", 1).pos(), code.s("c0", 2).pos()];

    assert_eq_unordered(
        &root.find_all_references_pos(&code.s("c0", 1).pos()),
        &references,
    );

    assert_eq_unordered(
        &root.find_all_references_pos(&code.s("c0", 2).pos()),
        &references,
    );

    assert_eq!(
        root.find_definition_of(
            root.search_reference(code.source(), code.s1("c0").start())
                .unwrap()
        )
        .unwrap()
        .decl_pos(),
        Some(&code.s("c0", 2).pos())
    );
}

#[test]
fn find_architecture_references() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent1 is
end entity;

architecture a1 of ent1 is
begin
end architecture;

architecture a2 of ent1 is
begin
end architecture;

entity ent2 is
end entity;

architecture a of ent2 is
begin
    good_inst1 : entity work.ent1(a1);
    good_inst2 : entity work.ent1(a2);
    bad_inst : entity work.ent1(a3);
end architecture;
      ",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::new(
            code.sa("work.ent1(", "a3"),
            "No architecture 'a3' for entity 'libname.ent1'",
            ErrorCode::Unresolved,
        )],
    );

    assert_eq_unordered(
        &root.find_all_references_pos(&code.s("a1", 1).pos()),
        &[code.s("a1", 1).pos(), code.s("a1", 2).pos()],
    );

    assert_eq_unordered(
        &root.find_all_references_pos(&code.s("a1", 2).pos()),
        &[code.s("a1", 1).pos(), code.s("a1", 2).pos()],
    );

    assert_eq!(
        root.find_definition_of(
            root.search_reference(code.source(), code.s("a2", 2).start())
                .unwrap()
        )
        .unwrap()
        .decl_pos(),
        Some(&code.s("a2", 1).pos())
    );
}

#[test]
fn find_end_identifier_references_of_declarations() {
    for name in [
        "ent1", "a1", "rec_t", "prot_t", "phys_t", "fun1", "proc1", "comp1", "pkg", "cfg1", "ctx1",
    ] {
        check_search_reference_with_name(
            name,
            "
entity ent1 is
end entity ent1;

architecture a1 of ent1 is

    type rec_t is record
       field: natural;
    end record rec_t;

    type prot_t is protected
    end protected prot_t;

    type prot_t is protected body
    end protected body prot_t;

    type phys_t is range 0 to 10
    units
       bangs;
       bugs = 10 bangs;
    end units phys_t;

    function fun1 return integer is
    begin
    end function fun1;

    procedure proc1 is
    begin
    end procedure proc1;

    component comp1 is
    end component comp1;
begin
end architecture a1;

package pkg is
end package pkg;

package body pkg is
end package body pkg;

configuration cfg1 of ent1 is
  for rtl(0)
  end for;
end configuration cfg1;

context ctx1 is
end context ctx1;
      ",
        );
    }
}

#[test]
fn find_end_identifier_references_of_concurrent() {
    for name in ["b1", "p1", "fg1", "ig1", "ialt1", "cg1", "cgalt1"] {
        check_search_reference_with_name(
            name,
            "
entity ent1 is
end entity ent1;

architecture a1 of ent1 is
begin
  b1: block
  begin
  end block b1;

  p1: process
  begin
  end process p1;

  fg1: for i in 0 to 10 generate
  end generate fg1;

  ig1: if true generate
  else ialt1: generate
  end ialt1;
  end generate ig1;

  cg1: case 0 generate
    when cgalt1: 0 => 
      assert false;
    end cgalt1;
  end generate cg1;

end architecture;
      ",
        );
    }
}

#[test]
fn find_end_identifier_references_of_sequential() {
    for name in ["if0", "loop0", "c0"] {
        check_search_reference_with_name(
            name,
            "
entity ent1 is
end entity ent1;

architecture a1 of ent1 is
begin
  process
  begin
    if0: if true then
    end if if0;

    loop0: for i in 0 to 1 loop
      next loop0;
      exit loop0;
    end loop loop0;

    c0: case 0 is
      when others =>
    end case c0;
  end process;
end architecture;
      ",
        );
    }
}

/// This is a regression test for github issue #229
/// The reference was not set on the subprogram name when selected
#[test]
fn sets_reference_on_selected_subprogram_call() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
entity ent is
end entity;

architecture a of ent is
  type my_record_t is record
     field1 : natural;
  end record my_record_t;

  function my_function return my_record_t is
  begin
  return (others => 0);
  end function;

  constant CONST1 : natural := my_function.field1;
begin
end architecture;
    ",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);
    assert_eq!(root.find_all_unresolved().1, vec![]);
}

#[test]
pub fn parse_selected_all() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "\
type t_str_ptr is access string;
signal str_value_ptr : t_str_ptr;
signal v_value : str_value_ptr.all'subtype;
    ",
    );
    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);
    let pos = code.s1("v_value").pos();
    let value = root
        .search_reference(&pos.source, pos.start())
        .expect("Signal has no reference");
    let typ = match value.kind() {
        AnyEntKind::Object(o) => o.subtype.type_mark(),
        _ => panic!("Expecting object"),
    };
    assert_eq!(typ.id, root.standard_types.as_ref().unwrap().string)
}

#[test]
pub fn select_package_from_other_package() {
    let mut builder = LibraryBuilder::new();
    let _code = builder.code(
        "libname",
        "package pkg1 is
    generic(i_gen: natural);

    type type1_t is array(natural range <>) of bit;
end package pkg1;

package pkg2 is
    package pkg1 is new work.pkg1 generic map (i_gen => 5);
end package pkg2;
 
package pkg3 is
    variable v1 : work.pkg2.pkg1.type1_t;
end package;",
    );
    let (_root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);
}

#[test]
pub fn select_package_from_instantiated_package() {
    let mut builder = LibraryBuilder::new();
    let _code = builder.code(
        "libname",
        "package pkg1 is
    generic(i_gen: natural);

    type type1_t is array(natural range <>) of bit;
end package pkg1;

package pkg2 is
    generic (i_gen : natural);

    package pkg1 is new work.pkg1 generic map (i_gen);
end package pkg2;

package pkg3 is
    package pkg2 is new work.pkg2 generic map (1);
    variable v1 : pkg2.pkg1.type1_t;
end package;",
    );
    let (_root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);
}

#[test]
pub fn resolve_selected_names_of_types() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "\
package foo is
    type test_t is record
        asdf: bit_vector(10 downto 0);
    end record;

    signal a: integer := test_t.asdf'high;
    signal b: bit_vector(test_t.asdf'high downto test_t.asdf'low);
    signal c: bit_vector(test_t.asdf'range);
    signal d: integer := test_t.asdf'high;
    signal e: bit_vector(test_t.asdf'range);
end package;
    ",
    );
    check_no_diagnostics(&builder.analyze())
}

#[test]
fn attribute_after_range() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        r#"
package foo is
    constant x: bit_vector(12 downto 0) := "0000000000000";
    constant y: natural := x'range'high;
end package;
    "#,
    );
    check_no_diagnostics(&builder.analyze())
}
