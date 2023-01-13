// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

use super::*;

#[test]
fn check_library_clause_library_exists() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
library missing_lib;

entity ent is
end entity;
        ",
    );

    let diagnostics = builder.analyze();

    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s1("missing_lib"),
            "No such library 'missing_lib'",
        )],
    )
}

#[test]
fn library_clause_extends_into_secondary_units() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
-- Package will be used for testing
package usepkg is
  constant const : natural := 0;
end package;

-- This should be visible also in architectures
library libname;

entity ent is
end entity;

use libname.usepkg;

architecture rtl of ent is
begin
end architecture;

-- This should be visible also in package body
library libname;
use libname.usepkg;

package pkg is
end package;

use usepkg.const;

package body pkg is
end package body;
        ",
    );

    let diagnostics = builder.analyze();

    check_no_diagnostics(&diagnostics);
}

/// Check that context clause in secondary units work
#[test]
fn context_clause_in_secondary_units() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
package usepkg is
  constant const : natural := 0;
end package;

entity ent is
end entity;

library libname;

architecture rtl of ent is
  use libname.usepkg;
begin
end architecture;

package pkg is
end package;

library libname;

package body pkg is
  use libname.usepkg;
end package body;
        ",
    );

    let diagnostics = builder.analyze();

    check_no_diagnostics(&diagnostics);
}

#[test]
fn check_library_clause_library_exists_in_context_declarations() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
context ctx is
  library missing_lib;
end context;
        ",
    );

    let diagnostics = builder.analyze();

    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s1("missing_lib"),
            "No such library 'missing_lib'",
        )],
    )
}

// This test was added to fix an accidental mistake when refactoring
#[test]
fn context_clause_does_change_work_symbol_meaning() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
-- Package will be used for testing
package pkg1 is
  constant const : natural := 0;
end package;

context ctx is
  library libname;
  use libname.pkg1;
end context;
        ",
    );

    builder.code(
        "libname2",
        "
package pkg2 is
end package;

library libname;
context libname.ctx;

use work.pkg2;
  package pkg3 is
end package;
        ",
    );

    let diagnostics = builder.analyze();

    check_no_diagnostics(&diagnostics);
}

#[test]
fn work_is_not_visible_in_context_clause() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
end package;

context ctx is
  use work.pkg1;
end context;
        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, vec![missing(&code, "work", 1)]);
}

#[test]
fn library_std_is_pre_defined() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
use std.textio.all;

entity ent is
end entity;
        ",
    );

    let diagnostics = builder.analyze();
    check_no_diagnostics(&diagnostics);
}

#[test]
fn work_library_not_necessary_hint() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
library work;

entity ent is
end entity;
        ",
    );

    let diagnostics = builder.analyze();

    check_diagnostics(
        diagnostics,
        vec![Diagnostic::hint(
            code.s1("work"),
            "Library clause not necessary for current working library",
        )],
    )
}

#[test]
fn check_use_clause_for_missing_design_unit() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
end package;

package gpkg is
  generic (const : natural);
end package;

entity ent is
end entity;

architecture rtl of ent is
begin
end architecture;

configuration cfg of ent is
  for rtl
  end for;
end configuration;

package ipkg is new work.gpkg
  generic map (
    const => 1
  );

library libname;

-- Should work
use work.pkg;
use libname.pkg.all;
use libname.ent;
use libname.ipkg;
use libname.cfg;

use work.missing_pkg;
use libname.missing_pkg.all;


entity dummy is
end entity;
        ",
    );

    let diagnostics = builder.analyze();

    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(
                code.s("missing_pkg", 1),
                "No primary unit 'missing_pkg' within library 'libname'",
            ),
            Diagnostic::error(
                code.s("missing_pkg", 2),
                "No primary unit 'missing_pkg' within library 'libname'",
            ),
        ],
    )
}

#[test]
fn check_use_clause_for_missing_library_clause() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
end package;

use libname.pkg;

entity dummy is
end entity;
        ",
    );

    let diagnostics = builder.analyze();

    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s("libname", 1),
            "No declaration of 'libname'",
        )],
    )
}

#[test]
fn nested_use_clause_missing() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
  constant const : natural := 0;
end package;

library libname;

entity ent is
end entity;

architecture rtl of ent is
  use libname.pkg; -- Works
  use libname.pkg1; -- Error
begin
  process
    use pkg.const; -- Works
    use libname.pkg1; -- Error
   begin
   end process;

  blk : block
    use pkg.const; -- Works
    use libname.pkg1; -- Error
  begin
  end block;

end architecture;
        ",
    );

    let diagnostics = builder.analyze();

    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(
                code.s("pkg1", 1),
                "No primary unit 'pkg1' within library 'libname'",
            ),
            Diagnostic::error(
                code.s("pkg1", 2),
                "No primary unit 'pkg1' within library 'libname'",
            ),
            Diagnostic::error(
                code.s("pkg1", 3),
                "No primary unit 'pkg1' within library 'libname'",
            ),
        ],
    )
}

#[test]
fn check_context_reference_for_missing_context() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
context ctx is
end context;

context work.ctx;
context work.missing_ctx;

entity dummy is
end entity;
        ",
    );

    let diagnostics = builder.analyze();

    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s1("missing_ctx"),
            "No primary unit 'missing_ctx' within library 'libname'",
        )],
    )
}

#[test]
fn check_context_reference_for_non_context() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
end package;

context work.pkg;

entity dummy is
end entity;
        ",
    );

    let diagnostics = builder.analyze();

    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s("pkg", 2),
            "package 'pkg' does not denote a context declaration",
        )],
    )
}

#[test]
fn check_use_clause_and_context_clause_must_be_selected_name() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
library libname;

context libname;
use work;
use libname;

use work.pkg(0);
context work.ctx'range;

entity dummy is
end entity;
        ",
    );

    let diagnostics = builder.analyze();

    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(
                code.s("libname", 2),
                "Context reference must be a selected name",
            ),
            Diagnostic::error(code.s1("work"), "Use clause must be a selected name"),
            Diagnostic::error(code.s("libname", 3), "Use clause must be a selected name"),
            Diagnostic::error(code.s1("work.pkg(0)"), "Use clause must be a selected name"),
            Diagnostic::error(
                code.s1("work.ctx'range"),
                "Context reference must be a selected name",
            ),
        ],
    );
}

#[test]
fn check_two_stage_use_clause_for_missing_name() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
  type enum_t is (alpha, beta);
  constant const : enum_t := alpha;
end package;

use work.pkg;
use pkg.const;
use pkg.const2;

package pkg2 is
end package;
        ",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s1("const2"),
            "No declaration of 'const2' within package 'pkg'",
        )],
    );
}
#[test]
fn check_use_clause_for_missing_name_in_package() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
  type enum_t is (alpha, beta);
  constant const : enum_t := alpha;
end package;

use work.pkg.const;
use work.pkg.const2;

package pkg2 is
end package;
        ",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s1("const2"),
            "No declaration of 'const2' within package 'pkg'",
        )],
    );
}

#[test]
fn check_use_clause_for_missing_name_in_package_instance() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package gpkg is
  generic (constant gconst : natural);
  constant const : natural := 0;
end package;

package ipkg is new work.gpkg generic map (gconst => 0);

use work.ipkg.const;
use work.ipkg.const2;

-- @TODO should probably not be visible #19
-- use work.ipkg.gconst;

package pkg is
end package;
        ",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            // @TODO add use instance path in error diagnostic
            Diagnostic::error(
                code.s1("const2"),
                "No declaration of 'const2' within package instance 'ipkg'",
            ),
        ],
    );
}

#[test]
fn error_on_use_clause_with_double_all() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg1 is
  constant const1 : natural := 0;
end package;

use work.all.all;
use work.all.foo;

entity ent is
end entity;
        ",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(
                code.s("work.all", 1),
                "'.all' may not be the prefix of a selected name",
            ),
            Diagnostic::error(
                code.s("work.all", 2),
                "'.all' may not be the prefix of a selected name",
            ),
        ],
    );
}

#[test]
fn an_uninstantiated_package_may_not_be_prefix_of_selected_name() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package gpkg is
  generic (const : natural);
end package;

use work.gpkg.all;

package pkg is
  use work.gpkg.const;
end package;
        ",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(
                code.s("work.gpkg", 1),
                "Uninstantiated package 'gpkg' may not be the prefix of a selected name",
            ),
            Diagnostic::error(
                code.s("work.gpkg", 2),
                "Uninstantiated package 'gpkg' may not be the prefix of a selected name",
            ),
        ],
    );
}

#[test]
fn invalid_prefix_of_use_clause_selected_name() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
  type enum_t is (alpha, beta);
  constant const : natural := 0;
end package;

use work.pkg.enum_t.foo;
use work.pkg.const.all;

package user is
end package;
        ",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(
                code.s("work.pkg.enum_t", 1),
                "Type 'enum_t' may not be the prefix of a selected name",
            ),
            Diagnostic::error(
                code.s("work.pkg.const", 1),
                "Invalid prefix for selected name",
            ),
        ],
    );
}

#[test]
fn use_clause_with_selected_all_design_units() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
package pkg1 is
  constant const1 : natural := 0;
end package;

package pkg2 is
  constant const2 : natural := 0;
end package;
        ",
    );

    builder.code(
        "libname2",
        "
library libname;
use libname.all;
use pkg1.const1;
use pkg2.const2;

entity ent is
end entity;
        ",
    );
    let diagnostics = builder.analyze();
    check_no_diagnostics(&diagnostics);
}

#[test]
fn use_clause_with_selected_all_names() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
package pkg1 is
  type enum_t is (alpha, beta);
end package;

use work.pkg1.all;

entity ent is
end entity;

architecture rtl of ent is
  signal foo : enum_t;
begin
end architecture;
        ",
    );
    let diagnostics = builder.analyze();
    check_no_diagnostics(&diagnostics);
}

#[test]
fn use_all_in_package() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg1 is
  subtype typ is natural range 0 to 1;
end package;

use work.pkg1.all;

package pkg2 is
  constant const : typ := 0;
  constant const2 : missing := 0;
end package;

",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s1("missing"),
            "No declaration of 'missing'",
        )],
    );
}

#[test]
fn use_all_in_primary_package_instance() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package gpkg is
  generic (const : natural);
  subtype typ is natural range 0 to 1;
end package;

package ipkg is new work.gpkg generic map (const => 0);

use work.ipkg.all;

package pkg is
  constant const : typ := 0;
  constant const2 : missing := 0;
end package;

",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s1("missing"),
            "No declaration of 'missing'",
        )],
    );
}

#[test]
fn use_of_interface_package_declaration() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package gpkg1 is
  generic (const : natural);
  subtype typ is natural range 0 to 1;
end package;

package gpkg2 is
  generic (package ipkg is new work.gpkg1 generic map (const => 1));
  use ipkg.typ;
  use ipkg.missing;
end package;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s1("missing"),
            "No declaration of 'missing' within package instance 'ipkg'",
        )],
    );
}
#[test]
fn use_in_local_package_instance() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package gpkg is
  generic (const : natural);
  subtype typ is natural range 0 to 1;
end package;


package pkg is
  package ipkg is new work.gpkg generic map (const => 0);
  use ipkg.typ;

  constant const : typ := 0;
  constant const2 : missing := 0;
end package;

",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s1("missing"),
            "No declaration of 'missing'",
        )],
    );
}

#[test]
fn use_all_in_local_package_instance() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package gpkg is
  generic (const : natural);
  subtype typ is natural range 0 to 1;
end package;


package pkg is
  package ipkg is new work.gpkg generic map (const => 0);
  use ipkg.all;

  constant const : typ := 0;
  constant const2 : missing := 0;
end package;

",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s1("missing"),
            "No declaration of 'missing'",
        )],
    );
}

#[test]
fn use_with_invalid_selected_name_prefix() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg1 is
  subtype typ_t is natural range 0 to 1;
end package;

use work.pkg1.typ_t.foo;

package pkg2 is
end package;

",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s("work.pkg1.typ_t", 1),
            "Subtype 'typ_t' may not be the prefix of a selected name",
        )],
    );
}

#[test]
fn resolves_reference_to_use_of_package() {
    check_search_reference_with_name(
        "pkg",
        "
package pkg is
end package;

-- Entity context clause reference
use work.pkg.all;
entity ename1 is
end entity;

-- Architecture context clause reference
use work.pkg.all;
architecture a of ename1 is
begin
end architecture;

-- Package context clause reference
use work.pkg.all;
package pname is
end package;

-- Package body context clause reference
use work.pkg.all;
package body pkg is
end package body;

-- Configuration context clause reference
use work.pkg.all;
configuration cfg of ename1 is
for rtl
end for;
end configuration;

package genpack is
  generic (constant c : natural);
end package;

-- Package instance context clause reference
use work.pkg.all;
package ipack is new work.genpack generic map(c => 0);

context ctx is
  library libname;
  -- Context declaration context clause reference
  use libname.pkg.all;
end context;

package nested is
  -- Use clause in declarative region
  use work.pkg.all;
end package;

",
    );
}

#[test]
fn resolves_context_reference() {
    check_search_reference(
        "
package pkg is
end package;

context decl is
  library libname;
  use libname.pkg;
end context;

context work.decl;

package pkg2 is
end package;
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
fn adds_alias_enum_variants_implicitly() {
    check_missing(
        "
package pkg is
  type enum_t is (alpha, beta);
  alias alias_t is enum_t;
end package;

use work.pkg.alias_t;
package pkg2 is
  constant c : alias_t := alpha;
  constant c2 : alias_t := missing;
end package;
",
    );
}

#[test]
fn adds_alias_enum_variants_implicitly_when_using_all() {
    check_missing(
        "
package pkg is
  type enum_t is (alpha, beta);
end package;

package pkg2 is
  alias alias_t is work.pkg.enum_t;
  constant c : alias_t := alpha;
end package;

use work.pkg2.all;
package pkg3 is
  constant c : alias_t := alpha;
  constant c2 : alias_t := missing;
end package;
",
    );
}
