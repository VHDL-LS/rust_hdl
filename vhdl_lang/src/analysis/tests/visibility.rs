// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2020, Olof Kraigher olof.kraigher@gmail.com

use super::*;

#[test]
fn secondary_units_share_root_region_and_visibility_in_extended_region() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg2 is
  constant const : natural := 0;
end package;

package pkg is
  use work.pkg2;
end package;

-- Does not work
use pkg2.const;

package body pkg is
  -- Does work, share visibility of extended region
  use pkg2.const;
end package body;
",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s("pkg2", 3),
            "No declaration of 'pkg2'",
        )],
    )
}

#[test]
fn immediate_region_takes_precedence_over_local_visibility() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package false_pkg is
  constant decl : natural := 0;
end package;

package pkg is
  constant decl : natural := 1;
  use work.false_pkg.decl;

  -- Should refer to constant in pkg
  constant ref : natural := decl;
end package pkg;
",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);

    assert_eq!(
        root.search_reference_pos(code.source(), code.s("decl", 4).start()),
        Some(code.s("decl", 2).pos())
    );
}

#[test]
fn extended_region_takes_precedence_over_local_visibility() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package false_pkg is
  constant decl : natural := 0;
end package;

package pkg is
  constant decl : natural := 1;
end package pkg;

use work.false_pkg.decl;
package body pkg is
  use work.false_pkg.decl;

  -- Should refer to constant in pkg
  constant ref : natural := decl;
end package body pkg;
",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);

    assert_eq!(
        root.search_reference_pos(code.source(), code.s("decl", 5).start()),
        Some(code.s("decl", 2).pos())
    );
}

#[test]
fn enclosing_region_takes_precedence_over_local_visibility() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
  constant decl : natural := 0;
end package;

entity tb_ent is
end entity;

architecture a of tb_ent is
  constant decl : natural := 1;
begin
  main : process
    use work.pkg.decl;
  begin
    assert decl = 1;
  end process;
end architecture;
",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);

    assert_eq!(
        root.search_reference_pos(code.source(), code.s("decl", 4).start()),
        Some(code.s("decl", 2).pos())
    );
}

#[test]
fn context_clause_makes_names_visible() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
-- Package will be used for testing
package usepkg is
  constant const : natural := 0;
  type enum_t is (alpha, beta);
end package;

context ctx is
  library libname;
  -- Test both used by name
  use libname.usepkg;
  -- .. as well as used by all
  use libname.usepkg.all;
end context;


context work.ctx;
use usepkg.const;

package pkg is
  constant c : enum_t := alpha;
end package;
        ",
    );

    let diagnostics = builder.analyze();

    check_no_diagnostics(&diagnostics);
}

#[test]
fn cannot_reference_potentially_visible_name_by_selection() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg2 is
  constant const1 : natural := 0;
  constant const2 : natural := 0;
end package;


use work.pkg2.const1;

package pkg is
  use work.pkg2.const2;
  constant const3 : natural := 0;
end package;

use work.pkg.const1;
use work.pkg.const2;
use work.pkg.const3;

entity ent is
end entity;
        ",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(
                code.s("const1", 3),
                "No declaration of 'const1' within package 'pkg'",
            ),
            Diagnostic::error(
                code.s("const2", 3),
                "No declaration of 'const2' within package 'pkg'",
            ),
        ],
    );
}

#[test]
fn duplicate_identifer_is_not_directly_visible() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg1 is
  constant name : natural := 0;
end package;

package pkg2 is
  constant name : natural := 1;
end package;

use work.pkg1.name;
use work.pkg2.name;

package user is
  constant b : natural := name;
end package;

        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![hidden_error(
            &code,
            "name",
            5,
            &[
                (&code, "work.pkg1.name", 1, false),
                (&code, "name", 1, true),
                (&code, "work.pkg2.name", 1, false),
                (&code, "name", 2, true),
            ],
        )],
    );
}

#[test]
fn duplicate_identifer_visiblity_is_traced_through_context() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "lib",
        "
package pkg1 is
  constant name : natural := 0;
end package;

package pkg2 is
  constant name : natural := 1;
end package;

context ctx is
  library lib;
  use lib.pkg1.name;
end context;

context work.ctx;
use work.pkg2.name;

package user is
  constant b : natural := name;
end package;

        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![hidden_error(
            &code,
            "name",
            5,
            &[
                (&code, "work.ctx", 1, false),
                (&code, "lib.pkg1.name", 1, false),
                (&code, "name", 1, true),
                (&code, "work.pkg2.name", 1, false),
                (&code, "name", 2, true),
            ],
        )],
    );
}

#[test]
fn duplicate_identifer_is_directly_visible_when_it_is_the_same_named_entitty() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
package pkg1 is
  constant name : natural := 0;
end package;

use work.pkg1.name;
use work.pkg1.name;

package user is
  constant b : natural := name;
end package;

        ",
    );

    let diagnostics = builder.analyze();
    check_no_diagnostics(&diagnostics);
}

#[test]
fn duplicate_identifer_of_parent_visibility_is_not_directly_visible() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg1 is
  constant name : natural := 0;
end package;

package pkg2 is
  constant name : natural := 1;
end package;

use work.pkg1.name;

package user is
  use work.pkg2.name;
  constant b : natural := name;
end package;

        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![hidden_error(
            &code,
            "name",
            5,
            &[
                (&code, "work.pkg1.name", 1, false),
                (&code, "name", 1, true),
                (&code, "work.pkg2.name", 1, false),
                (&code, "name", 2, true),
            ],
        )],
    );
}

#[test]
fn local_is_still_visible_under_duplicate_identifer() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
package pkg1 is
  constant name : natural := 0;
end package;

package pkg2 is
  constant name : natural := 1;
end package;


package user is
  use work.pkg1.name;
  use work.pkg2.name;

  constant name : natural := 0;
  constant b : natural := name;
  procedure foo(constant b : natural := name);
end package;

        ",
    );

    let diagnostics = builder.analyze();
    check_no_diagnostics(&diagnostics);
}

#[test]
fn mixed_overloaded_names_require_disambiguation() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
 function fun1 return natural;
 function fun2 return natural;
end package;

package pkg2 is
  constant fun1 : boolean := false;
  constant fun2 : natural := 0;
end package;

use work.pkg.all;
use work.pkg2.all;
package user is
  -- Requires disambiguation
  constant ident : natural := fun1;
  -- Requires disambiguation
  constant ident2 : natural := fun2;
end package;
",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            hidden_error(
                &code,
                "fun1",
                3,
                &[
                    (&code, "work.pkg.all", 1, false),
                    (&code, "fun1", 1, true),
                    (&code, "work.pkg2.all", 1, false),
                    (&code, "fun1", 2, true),
                ],
            ),
            hidden_error(
                &code,
                "fun2",
                3,
                &[
                    (&code, "work.pkg.all", 1, false),
                    (&code, "fun2", 1, true),
                    (&code, "work.pkg2.all", 1, false),
                    (&code, "fun2", 2, true),
                ],
            ),
        ],
    );
}

/// This was a bug during development
#[test]
fn explicit_library_std_is_not_duplicate_of_implicit() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
library std;
use std.standard.all;

package pkg1 is
end package;

        ",
    );

    let diagnostics = builder.analyze();
    check_no_diagnostics(&diagnostics);
}

/// This was a bug during development
#[test]
fn duplicate_explicit_library_is_not_duplicate() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
package pkg is
end package;

context ctx is
  library libname;
  use libname.pkg;
end context;

library libname;
context libname.ctx;
use libname.pkg;

package user is
end package;

        ",
    );

    let diagnostics = builder.analyze();
    check_no_diagnostics(&diagnostics);
}

pub fn hidden_error(
    code: &Code,
    name: &str,
    occ: usize,
    related: &[(&Code, &str, usize, bool)],
) -> Diagnostic {
    let mut error = Diagnostic::error(
        code.s(name, occ),
        format!("Name '{name}' is hidden by conflicting use clause"),
    );

    for (code, substr, occ, declared) in related.iter() {
        if *declared {
            error.add_related(
                code.s(substr, *occ),
                format!("Conflicting name '{name}' declared here"),
            )
        } else {
            error.add_related(
                code.s(substr, *occ),
                format!("Conflicting name '{name}' made visible here"),
            )
        }
    }

    error
}

#[test]
fn duplicate_alias_is_not_directly_visible() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg1 is
  type enum_t is (alpha, beta);
  alias alias_t is enum_t;
end package;

package pkg2 is
  type enum_t is (alpha, beta);
  alias alias_t is enum_t;
end package;

use work.pkg1.alias_t;
use work.pkg2.alias_t;

package user is
  constant b : alias_t := alpha;
end package;

        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![hidden_error(
            &code,
            "alias_t",
            5,
            &[
                (&code, "work.pkg1.alias_t", 1, false),
                (&code, "alias_t", 1, true),
                (&code, "work.pkg2.alias_t", 1, false),
                (&code, "alias_t", 2, true),
            ],
        )],
    );
}

/// It is more convenient for a language server user to goto-declaration for
/// the alias rather than going directly to the declaration without knowing about the visible alias
/// The user can always navigate from the alias to the original declaration if she desires
#[test]
fn deepest_alias_is_visible() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
  type enum_t is (alpha, beta);
end package;

package pkg2 is
  alias enum_t is work.pkg.enum_t;
end package;

package pkg3 is
  alias enum_t is work.pkg2.enum_t;
end package;

use work.pkg2.enum_t;
use work.pkg3.enum_t;
use work.pkg.enum_t;

package pkg4 is
  constant c : enum_t := alpha;
end package;
",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);

    let ref_pos = code.s1("constant c : enum_t := alpha").s1("enum_t");
    let deepest_pos = code.s1("alias enum_t is work.pkg2.enum_t").s1("enum_t");

    assert_eq!(
        root.search_reference_pos(code.source(), ref_pos.start()),
        Some(deepest_pos.pos())
    );
}

/// Using an overloaded name should not conflict with an immediate declaration if they
/// have different signatures
#[test]
fn non_conflicting_used_names_are_still_visible_in_prescence_of_immediate() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg1 is
  type enum1_t is (alpha, beta);
end package;

use work.pkg1.enum1_t;

package user is
  type enum2_t is (alpha, beta);
  constant a : enum1_t := alpha;
  constant b : enum2_t := alpha;
end package;
",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);

    let alpha1_pos = code.s("alpha", 1).pos();
    let alpha1_ref = code.s("alpha", 3).pos();
    assert_eq!(
        root.search_reference_pos(code.source(), alpha1_ref.start()),
        Some(alpha1_pos.clone())
    );

    let alpha2_pos = code.s("alpha", 2).pos();
    let alpha2_ref = code.s("alpha", 4).pos();
    assert_eq!(
        root.search_reference_pos(code.source(), alpha2_ref.start()),
        Some(alpha2_pos.clone())
    );

    assert_eq_unordered(
        &root.find_all_references_pos(&alpha1_pos),
        &[alpha1_pos, alpha1_ref],
    );
    assert_eq_unordered(
        &root.find_all_references_pos(&alpha2_pos),
        &[alpha2_pos, alpha2_ref],
    );
}

#[test]
fn nested_subprogram_shadows_outer() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
procedure theproc(arg: character) is
begin
end procedure;

function thefun(arg: integer) return natural is
    procedure theproc(arg: integer) is
    begin
        theproc('c');
    end procedure;
begin
    theproc(arg);
    return 0;
end function;
",
    );
    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);

    assert_eq!(
        root.search_reference_pos(code.source(), code.s1("theproc('c')").start()),
        Some(code.s1("theproc").pos())
    );

    assert_eq!(
        root.search_reference_pos(code.source(), code.s1("theproc(arg)").start()),
        Some(code.s("theproc", 2).pos())
    );
}

#[test]
fn labels_are_visible_in_declarative_region() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
end entity;

architecture a of ent is
  attribute debug : boolean;
  attribute debug of main : label is true;
begin
  main: process
  begin
      wait;
  end process;
end architecture;

",
    );
    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);
    let label = root
        .search_reference(code.source(), code.sa("debug of ", "main").start())
        .unwrap();
    assert_eq!(label.decl_pos(), Some(&code.sb("main", ": process").pos()));
}

#[test]
fn sequential_labels_are_visible_in_declarative_region() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
end entity;

architecture a of ent is
begin
  process
    attribute debug : boolean;
    attribute debug of main : label is true;
  begin
    main: for i in 0 to 1 loop
    end loop;
    wait;
  end process;
end architecture;
",
    );
    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);
    let label = root
        .search_reference(code.source(), code.sa("debug of ", "main").start())
        .unwrap();
    assert_eq!(label.decl_pos(), Some(&code.sb("main", ": for i ").pos()));
}
