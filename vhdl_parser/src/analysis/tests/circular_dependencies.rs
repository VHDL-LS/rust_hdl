// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

use super::*;

#[test]
fn detects_context_circular_dependencies() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "

context ctx1 is
  context work.ctx2;
end context;

context ctx2 is
  context work.ctx1;
end context;
",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(code.s1("work.ctx1"), "Found circular dependency"),
            Diagnostic::error(code.s1("work.ctx2"), "Found circular dependency"),
        ],
    );
}

#[test]
fn detects_package_circular_dependencies() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
use work.pkg2.const;

package pkg1 is
  constant const : natural := 0;
end package;

use work.pkg1.const;

package pkg2 is
  constant const : natural := 0;
end package;",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(code.s1("work.pkg1.const"), "Found circular dependency"),
            Diagnostic::error(code.s1("work.pkg2.const"), "Found circular dependency"),
        ],
    );
}

#[test]
fn detects_package_instance_circular_dependencies() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package gpkg is
  generic (c : boolean);
  constant const : boolean := c;
end package;

use work.pkg2.const;
package pkg1 is new work.gpkg generic map(c => false);

use work.pkg1.const;
package pkg2 is new work.gpkg generic map(c => true);
",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(code.s1("work.pkg1.const"), "Found circular dependency"),
            Diagnostic::error(code.s1("work.pkg2.const"), "Found circular dependency"),
        ],
    );
}

#[test]
fn detects_package_instance_circular_dependencies_in_declarative_region() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
use work.pkg2.const;
package gpkg is
  generic (c : boolean);
  constant const : boolean := c;
end package;

package pkg2 is
   package ipkg is new work.gpkg generic map(c => false);
   constant const : boolean := false;
end package;

",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(code.s1("work.pkg2.const"), "Found circular dependency"),
            Diagnostic::error(code.s1("work.gpkg"), "Found circular dependency"),
        ],
    );
}

#[test]
fn detects_package_instance_circular_dependencies_in_interface() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
use work.pkg3.const;
package gpkg is
  generic (c : boolean);
  constant const : boolean := c;
end package;

package pkg2 is
   generic (
      package ipkg is new work.gpkg generic map(c => true)
   );
   constant const : boolean := false;
end package;

package pkg3 is new work.pkg2;
",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(code.s1("work.pkg3.const"), "Found circular dependency"),
            Diagnostic::error(code.s1("work.gpkg"), "Found circular dependency"),
            Diagnostic::error(code.s1("work.pkg2"), "Found circular dependency"),
        ],
    );
}

#[test]
fn detects_package_circular_dependencies_all() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
use work.pkg2.all;

package pkg1 is
  constant const : natural := 0;
end package;

use work.pkg1.all;

package pkg2 is
  constant const : natural := 0;
end package;",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(code.s1("work.pkg1.all"), "Found circular dependency"),
            Diagnostic::error(code.s1("work.pkg2.all"), "Found circular dependency"),
        ],
    );
}

#[test]
fn detects_package_instance_circular_dependencies_all() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package gpkg is
  generic (g : boolean);
end package;

use work.pkg2.all;

package pkg1 is
  constant const : natural := 0;
end package;

use work.pkg1.all;

package pkg2 is new work.gpkg generic map (g => true);
",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(code.s1("work.pkg1.all"), "Found circular dependency"),
            Diagnostic::error(code.s1("work.pkg2.all"), "Found circular dependency"),
        ],
    );
}

#[test]
fn detects_circular_dependencies_only_when_used() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
use work.all;

package pkg1 is
  constant const : natural := 0;
end package;

use work.pkg1.const;

package pkg2 is
  constant const : natural := 0;
end package;",
    );
    let diagnostics = builder.analyze();
    check_no_diagnostics(&diagnostics);
}
