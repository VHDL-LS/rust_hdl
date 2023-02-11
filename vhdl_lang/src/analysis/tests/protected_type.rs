// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

use super::*;

#[test]
fn error_on_missing_protected_body() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg_no_body is
type a1 is protected
end protected;
end package;

package pkg is
type b1 is protected
end protected;
end package;

package body pkg is
end package body;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(&code.s1("a1"), "Missing body for protected type 'a1'"),
            Diagnostic::error(&code.s1("b1"), "Missing body for protected type 'b1'"),
        ],
    );
}

#[test]
fn error_on_missing_protected_type_for_body() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg_no_body is
  type a1 is protected body
  end protected body;
end package;

package pkg is
end package;

package body pkg is
type b1 is protected body
end protected body;

type b1 is protected
end protected;
end package body;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(&code.s1("a1"), "No declaration of protected type 'a1'"),
            Diagnostic::error(&code.s1("b1"), "No declaration of protected type 'b1'"),
            Diagnostic::error(&code.s("b1", 2), "Missing body for protected type 'b1'"),
        ],
    );
}

#[test]
fn allows_protected_type_and_body_with_same_name() {
    check_code_with_no_diagnostics(
        "
package pkg is
  type prot_t is protected
  end protected;

  type prot_t is protected body
  end protected body;
end package;
",
    );
}

#[test]
fn allows_protected_type_and_body_in_package_header_and_body() {
    check_code_with_no_diagnostics(
        "
package pkg is
  type prot_t is protected
  end protected;
end package;

package body pkg is
  type prot_t is protected body
  end protected body;
end package body;
",
    );
}

#[test]
fn forbid_duplicate_protected_type() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
  type prot_t is protected
  end protected;

  type prot_t is protected
  end protected;

  type prot_t is protected body
  end protected body;
end package;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, duplicates(&code, &["prot_t"]));
}

#[test]
fn forbid_duplicate_protected_type_body() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
  type prot_t is protected
  end protected;

  type prot_t is protected body
  end protected body;

  type prot_t is protected body
  end protected body;
end package;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, vec![duplicate(&code, "prot_t", 2, 3)]);
}

#[test]
fn protected_type_is_visible_in_declaration() {
    check_code_with_no_diagnostics(
        "
package pkg1 is
  type prot_t is protected
   procedure proc(val : inout prot_t);
  end protected;

  type prot_t is protected body
   procedure proc(val : inout prot_t) is
   begin
   end;
  end protected body;
end package;",
    );
}

#[test]
fn forbid_incompatible_deferred_items() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is

  -- Protected type vs constant
  type a1 is protected
  end protected;
  constant a1 : natural := 0;

  -- Just to avoid missing body error
  type a1 is protected body
  end protected body;

  -- Deferred constant vs protected body
  constant b1 : natural;

  type b1 is protected body
  end protected body;

end package;

package body pkg is
  constant b1 : natural := 0;
end package body;
",
    );

    let diagnostics = builder.analyze();
    let expected = vec![
        duplicate(&code, "a1", 1, 2),
        Diagnostic::error(&code.s("b1", 2), "'b1' is not a protected type"),
    ];
    check_diagnostics(diagnostics, expected);
}

#[test]
fn protected_type_body_extends_declaration() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg1 is
  type prot_t is protected
    function fun1 return natural;
    function fun2 return natural;
  end protected;

  type prot_t is protected body
    -- Function 2 should be visible before
    function fun1 return natural is
    begin
      return fun2;
    end;

    function private return natural is
    begin
      return missing;
    end;

    function fun2 return natural is
    begin
      return 0;
    end;
  end protected body;
end package;",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, vec![missing(&code, "missing", 1)]);
}

#[test]
fn protected_type_body_reference() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg1 is
  type prot_t is protected
  end protected;

  type prot_t is protected body
  end protected body;

  shared variable var : prot_t;
end package;",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);

    assert_eq!(
        root.search_reference_pos(code.source(), code.s("prot_t", 2).start()),
        Some(code.s("prot_t", 2).pos())
    );

    assert_eq!(
        root.search_reference_pos(code.source(), code.s("prot_t", 3).start()),
        Some(code.s("prot_t", 2).pos())
    );

    let ptype = root
        .search_reference(code.source(), code.s1("prot_t").start())
        .unwrap();
    assert_eq!(
        root.find_all_references(ptype),
        vec![
            code.s("prot_t", 1).pos(),
            code.s("prot_t", 2).pos(),
            code.s("prot_t", 3).pos()
        ]
    );

    assert_eq!(
        root.find_definition_of(ptype).unwrap().decl_pos(),
        Some(&code.s("prot_t", 2).pos())
    );
}
