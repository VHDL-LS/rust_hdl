// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

use super::*;

#[test]
fn allows_unique_names() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
package pkg is
constant a : natural := 0;
constant b : natural := 0;
constant c : natural := 0;
end package;
",
    );

    let diagnostics = builder.analyze();
    check_no_diagnostics(&diagnostics);
}

#[test]
fn forbid_homographs() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
constant a1 : natural := 0;
constant a : natural := 0;
constant a1 : natural := 0;
end package;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, duplicates(&code, &["a1"]));
}

#[test]
fn forbid_homographs_in_subprogram_bodies() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
end package;

package body pkg is
procedure proc(a1, a, a1 : natural) is
constant b1 : natural := 0;
constant b : natural := 0;
constant b1 : natural := 0;

procedure nested_proc(c1, c, c1 : natural) is
  constant d1 : natural := 0;
  constant d : natural := 0;
  constant d1 : natural := 0;
begin
end;

begin
end;
end package body;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, duplicates(&code, &["a1", "b1", "c1", "d1"]));
}

#[test]
fn forbid_homographs_in_component_declarations() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
component comp is
generic (
  a1 : natural;
  a : natural;
  a1 : natural;
  c1 : natural
);
port (
  b1 : natural;
  b : natural;
  b1 : natural;
  c1 : natural
);
end component;
end package;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, duplicates(&code, &["a1", "b1", "c1"]));
}

#[test]
fn forbid_homographs_in_record_type_declarations() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
type rec_t is record
a1 : natural;
a : natural;
a1 : natural;
end record;
end package;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, duplicates(&code, &["a1"]));
}

#[test]
fn forbid_homographs_in_proteced_type_declarations() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
type prot_t is protected
procedure proc(a1, a, a1 : natural);
end protected;

type prot_t is protected body
constant b1 : natural := 0;
constant b : natural := 0;
constant b1 : natural := 0;
end protected body;
end package;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, duplicates(&code, &["a1", "b1"]));
}

#[test]
fn forbid_homographs_in_subprogram_declarations() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
procedure proc(a1, a, a1 : natural);
function fun(b1, a, b1 : natural) return natural;
end package;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, duplicates(&code, &["a1", "b1"]));
}

#[test]
fn forbid_homographs_in_subprogram_iface_list_and_body() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
end package;

package body pkg is
  procedure proc(a1, a : natural) is
     constant a1 : natural := 0;
  begin
  end;

  function fun(b1, a : natural) return natural is
     constant b1 : natural := 0;
  begin
     return 0;
  end;
end package body;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, duplicates(&code, &["a1", "b1"]));
}

#[test]
fn forbid_homographs_in_block() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
begin
blk : block
constant a1 : natural := 0;
constant a : natural := 0;
constant a1 : natural := 0;
begin
process
  constant b1 : natural := 0;
  constant b : natural := 0;
  constant b1 : natural := 0;
begin
end process;
end block;
end entity;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, duplicates(&code, &["a1", "b1"]));
}

#[test]
fn forbid_homographs_in_process() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
begin
process
constant a1 : natural := 0;
constant a : natural := 0;
constant a1 : natural := 0;
begin
end process;
end entity;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, duplicates(&code, &["a1"]));
}

#[test]
fn forbid_homographs_for_generate() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
begin
gen_for: for i in 0 to 3 generate
constant a1 : natural := 0;
constant a : natural := 0;
constant a1 : natural := 0;
begin
process
  constant b1 : natural := 0;
  constant b : natural := 0;
  constant b1 : natural := 0;
begin
end process;
end generate;
end entity;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, duplicates(&code, &["a1", "b1"]));
}

#[test]
fn forbid_homographs_if_generate() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
begin
gen_if: if true generate
constant a1 : natural := 0;
constant a : natural := 0;
constant a1 : natural := 0;
begin

prcss : process
  constant b1 : natural := 0;
  constant b : natural := 0;
  constant b1 : natural := 0;
begin
end process;

else generate
constant c1 : natural := 0;
constant c: natural := 0;
constant c1 : natural := 0;
begin
prcss : process
  constant d1 : natural := 0;
  constant d : natural := 0;
  constant d1 : natural := 0;
begin
end process;
end generate;
end entity;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, duplicates(&code, &["a1", "b1", "c1", "d1"]));
}

#[test]
fn forbid_homographs_with_for_generate_loop_var() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
begin
  gen_for: for a1 in 0 to 3 generate
    constant a1 : natural := 0;
    constant a : natural := 0;
  begin
  end generate;
end entity;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, duplicates(&code, &["a1"]));
}

#[test]
fn forbid_homographs_case_generate() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
begin
gen_case: case 0 generate
when others =>
  constant a1 : natural := 0;
  constant a : natural := 0;
  constant a1 : natural := 0;
begin
  process
    constant b1 : natural := 0;
    constant b : natural := 0;
    constant b1 : natural := 0;
  begin
  end process;
end generate;
end entity;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, duplicates(&code, &["a1", "b1"]));
}

#[test]
fn forbid_homographs_in_entity_declarations() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
generic (
a1 : natural;
a : natural;
a1 : natural
);
port (
b1 : natural;
b : natural;
b1 : natural
);
constant c1 : natural := 0;
constant c : natural := 0;
constant c1 : natural := 0;
begin

blk : block
constant d1 : natural := 0;
constant d : natural := 0;
constant d1 : natural := 0;
begin

end block;

end entity;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, duplicates(&code, &["a1", "b1", "c1", "d1"]));
}

#[test]
fn forbid_homographs_in_architecture_bodies() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
end entity;

architecture arch of ent is
constant a1 : natural := 0;
constant a : natural := 0;
constant a1 : natural := 0;
begin

blk : block
constant b1 : natural := 0;
constant b : natural := 0;
constant b1 : natural := 0;
begin
end block;

end architecture;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, duplicates(&code, &["a1", "b1"]));
}

#[test]
fn forbid_homographs_of_type_declarations() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
constant a1 : natural := 0;
type a1 is (foo, bar);
end package;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, duplicates(&code, &["a1"]));
}

#[test]
fn forbid_homographs_of_component_declarations() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
constant a1 : natural := 0;
component a1 is
port (clk : bit);
end component;
end package;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, duplicates(&code, &["a1"]));
}

#[test]
fn forbid_homographs_of_file_declarations() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
constant a1 : natural := 0;
file a1 : std.textio.text;
end package;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, duplicates(&code, &["a1"]));
}

#[test]
fn forbid_homographs_in_package_declarations() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package gpkg is
generic (foo : natural);
end package;

package pkg is
package a1 is new work.gpkg generic map (foo => 0);
package a1 is new work.gpkg generic map (foo => 0);
end package;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, duplicates(&code, &["a1"]));
}

#[test]
fn forbid_homographs_in_attribute_declarations() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
attribute a1 : string;
attribute a1 : string;
end package;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, duplicates(&code, &["a1"]));
}

#[test]
fn forbid_homographs_in_alias_declarations() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is

constant c0 : natural := 0;
constant c1 : natural := 0;

alias a1 is c0;
alias a1 is c1;

function f1 return natural;
function f2 return boolean;

-- Legal since subprograms are overloaded
alias b1 is f1[return natural];
alias b1 is f2[return boolean];
end package pkg;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, duplicates(&code, &["a1"]));
}

#[test]
fn forbid_homographs_for_overloaded_vs_non_overloaded() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
constant foo : natural := 0;
function bar return boolean;

alias a1 is foo;
alias a1 is bar[return boolean];

function b1 return natural;
constant b1 : natural := 0;
end package;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, duplicates(&code, &["a1", "b1"]));
}

#[test]
fn enum_literals_may_overload() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
package pkg is
type enum_t is (a1, b1);

-- Ok since enumerations may overload
type enum2_t is (a1, b1);
end package;
",
    );

    let diagnostics = builder.analyze();
    check_no_diagnostics(&diagnostics);
}

#[test]
fn forbid_homograph_to_enum_literals() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
type enum_t is (a1, b1);
constant a1 : natural := 0;
function b1 return natural;
end package pkg;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, duplicates(&code, &["a1"]));
}

#[test]
fn homograph_of_enum_literal_declared_by_alias() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
  type enum_t is (alpha, beta);
end package;

package pkg2 is
  alias alias_t is work.pkg.enum_t;
  constant alpha : natural := 0;
end package;
",
    );

    let diagnostics = builder.analyze();
    let error = Diagnostic::error(code.s("alpha", 2), "Duplicate declaration of 'alpha'")
        .related(code.s("alias_t", 1), "Previously defined here");
    check_diagnostics(diagnostics, vec![error]);
}

#[test]
fn forbid_homographs_in_interface_file_declarations() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
procedure proc(file a1, a, a1 : std.textio.text);
end package;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, duplicates(&code, &["a1"]));
}

#[test]
fn forbid_homographs_in_interface_type_declarations() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
generic (
type a1;
type a1
);
end entity;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, duplicates(&code, &["a1"]));
}

#[test]
fn forbid_homographs_in_interface_package_declarations() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package gpkg is
generic (const : natural);
end package;

entity ent is
generic (
package a1 is new work.gpkg generic map (const => 0);
package a1 is new work.gpkg generic map (const => 0)
);
end entity;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, duplicates(&code, &["a1"]));
}

#[test]
fn forbid_homographs_in_entity_extended_declarative_regions() {
    let mut builder = LibraryBuilder::new();
    let ent = builder.code(
        "libname",
        "
entity ent is
generic (
constant g1 : natural;
constant g2 : natural;
constant g3 : natural;
constant g4 : natural
);
port (
signal g1 : natural;
signal p1 : natural;
signal p2 : natural;
signal p3 : natural
);
constant g2 : natural := 0;
constant p1 : natural := 0;
constant e1 : natural := 0;
constant e2 : natural := 0;
end entity;",
    );

    let arch1 = builder.code(
        "libname",
        "
architecture rtl of ent is
constant g3 : natural := 0;
constant p2 : natural := 0;
constant e1 : natural := 0;
constant a1 : natural := 0;
begin
end architecture;",
    );

    let arch2 = builder.code(
        "libname",
        "
architecture rtl2 of ent is
constant a1 : natural := 0;
constant e2 : natural := 0;
begin
end architecture;
",
    );

    let diagnostics = builder.analyze();
    let mut expected = duplicates(&ent, &["g1", "g2", "p1"]);
    expected.append(&mut duplicate_in_two_files(
        &ent,
        &arch1,
        &["g3", "p2", "e1"],
    ));
    expected.append(&mut duplicate_in_two_files(&ent, &arch2, &["e2"]));
    check_diagnostics(diagnostics, expected);
}

#[test]
fn forbid_homographs_in_package_extended_declarative_regions() {
    let mut builder = LibraryBuilder::new();
    let pkg = builder.code(
        "libname",
        "
package pkg is
generic (
constant g1 : natural;
constant g2 : natural
);
constant g1 : natural := 0;
end package;",
    );

    let body = builder.code(
        "libname",
        "
package body pkg is
constant g1 : natural := 0;
constant g2 : natural := 0;
constant p1 : natural := 0;
end package body;",
    );

    let diagnostics = builder.analyze();
    let mut expected = duplicates(&pkg, &["g1"]);
    expected.append(&mut duplicate_in_two_files(&pkg, &body, &["g1", "g2"]));
    check_diagnostics(diagnostics, expected);
}

#[test]
fn forbid_homographs_of_physical_type_units() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
  type phys_t is range 0 to 10
    units
       bangs;
       bugs = 10 bangs;
    end units;

    type phys2_t is range 0 to 10
    units
       bangs;
       bugs = 10 bangs;
    end units;

   constant bangs : natural := 0;
   constant bugs : natural := 0;
end package;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            // Primary unit
            duplicate(&code, "bangs", 1, 3),
            duplicate(&code, "bangs", 1, 5),
            // Secondary units
            duplicate(&code, "bugs", 1, 2),
            duplicate(&code, "bugs", 1, 3),
            Diagnostic::error(
                code.s("10 bangs", 2).s1("bangs"),
                "Physical unit of type 'phys_t' does not match physical type 'phys2_t'",
            ),
        ],
    );
}

#[test]
fn concurrent_labels_are_homographs_of_outer_declarations() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
end entity;

architecture a of ent is
  signal lab1 : natural;
  signal lab2 : natural;
begin
  lab1 : process is
    constant lab1 : natural := 0; -- Allow shadow
  begin
  end process;

  lab2 : block is
    constant lab2 : natural := 0; -- Allow shadow
  begin
  end block;
end architecture;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            duplicate(&code, "lab1", 2, 1),
            duplicate(&code, "lab2", 2, 1),
        ],
    );
}

#[test]
fn alternate_generate_labels_are_homographs_of_inner_declarations() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
end entity;

architecture a of ent is
begin
  gen: if alt1: true generate
    constant alt1 : boolean := true;
    constant alt2 : boolean := true;
    constant alt3 : boolean := true;
  begin
  elsif alt2: false generate
    constant alt1 : boolean := true;
    constant alt2 : boolean := true;
    constant alt3 : boolean := true;
  begin
  else alt3: generate
    constant alt1 : boolean := true;
    constant alt2 : boolean := true;
    constant alt3 : boolean := true;
  begin
  end generate;
end architecture;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            duplicate(&code, "alt1", 1, 2),
            duplicate(&code, "alt2", 2, 3),
            duplicate(&code, "alt3", 3, 4),
        ],
    );
}

#[test]
fn overloaded_with_identical_signatures_are_homographs() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
  function name1 return natural;
  function name1 return natural;
end package;

package body pkg is
  function name2(arg: string) return boolean is
  begin
    return false;
  end;

  function name2(arg: string) return boolean is
  begin
    return false;
  end;

end package body;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(
                code.s("name1", 2),
                "Duplicate declaration of 'name1' with signature [return NATURAL]",
            )
            .related(code.s("name1", 1), "Previously defined here"),
            Diagnostic::error(
                code.s("name2", 2),
                "Duplicate declaration of 'name2' with signature [STRING return BOOLEAN]",
            )
            .related(code.s("name2", 1), "Previously defined here"),
        ],
    );
}

#[test]
fn overloaded_declaration_is_not_homograph_with_definition() {
    check_code_with_no_diagnostics(
        "
package pkg is
  function name1 return natural;
end package;

package body pkg is
  function name1 return natural is
  begin
  end;
end package body;
",
    );
}

#[test]
fn overloaded_alias_with_identical_signatures_are_homographs() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
end entity;

architecture a of ent is
  function f1 return natural is
  begin
    return 0;
  end function;

  function f2 return natural is
  begin
    return 0;
  end function;

  -- Not ok since f1 and f2 are different functions with the same signature
  alias homo1 is f1[return natural];
  alias homo1 is f2[return natural];
begin
end architecture;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s("homo1", 2),
            "Duplicate declaration of 'homo1' with signature [return NATURAL]",
        )
        .related(code.s("homo1", 1), "Previously defined here")],
    );
}
