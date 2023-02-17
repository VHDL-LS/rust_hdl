// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

use super::*;

#[test]
fn package_name_must_be_visible_in_package_instance() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package gpkg is
generic (const : natural);
end package;

package ipkg_err is new gpkg generic map (const => 0);
package ipkg_ok is new work.gpkg generic map (const => 0);

package nested is
package ipkg_err is new gpkg generic map (const => 0);
package ipkg_ok is new work.gpkg generic map (const => 0);
end package;
        ",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(code.s("gpkg", 2), "No declaration of 'gpkg'"),
            Diagnostic::error(code.s("gpkg", 4), "No declaration of 'gpkg'"),
        ],
    );
}

#[test]
fn package_name_must_be_an_uninstantiated_package_in_package_instance() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
constant const : natural := 0;
end package;

package ipkg is new work.pkg generic map (const => 0);

package nested is
package ipkg2 is new work.pkg.const generic map (const => 0);
end package;
        ",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(
                code.s1("work.pkg"),
                "'work.pkg' is not an uninstantiated generic package",
            ),
            Diagnostic::error(
                code.s1("work.pkg.const"),
                "'work.pkg.const' is not an uninstantiated generic package",
            ),
        ],
    );
}

#[test]
fn resolves_generic_package_interface_list() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package gpkg is
  generic (type type_t);
end package;

package ipkg1 is new work.gpkg
  generic map (
    missing => integer
  );

package ipkg2 is new work.gpkg
  generic map (
    type_t => missing
  );

package ipkg3 is new work.gpkg
  generic map (
    type_t => integer
  );
",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(code.s("missing", 1), "No declaration of 'missing'"),
            Diagnostic::error(code.s("missing", 2), "No declaration of 'missing'"),
        ],
    );

    let typ = root
        .search_reference(
            code.source(),
            code.s1("type_t => integer").s1("type_t").pos().start(),
        )
        .unwrap();
    assert_eq!(typ.decl_pos(), Some(&code.s1("type_t").pos()));
    assert_eq!(root.format_declaration(typ), Some("type type_t".to_owned()));
}

#[test]
fn generic_package_interface_kind_mismatch() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package gpkg is
  generic (
    type type_t;
    constant c0 : integer
  );
end package;

package good_pkg is new work.gpkg
  generic map (
    type_t => integer,
    c0 => 0
  );

package bad_pkg1 is new work.gpkg
  generic map (
    type_t => 16#bad#,
    c0 => natural
  );

package bad_pkg2 is new work.gpkg
  generic map (
    type_t => work,
    c0 => 0
  );

",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(
                code.s("16#bad#", 1),
                "Cannot map expression to type generic",
            ),
            Diagnostic::error(
                code.s1("natural"),
                "subtype 'NATURAL' cannot be used in an expression",
            ),
            Diagnostic::error(
                code.s1("=> work").s1("work"),
                "Expected type name, got library libname",
            ),
        ],
    );
}

#[test]
fn subtype_constraints_are_supported() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
package pkg is
  type rec_t is record
      field: integer_vector;
  end record;
end package;

package gpkg is
  generic (
    type type_t
  );
end package;

package arr_pkg is new work.gpkg
  generic map (
    type_t => integer_vector(0 to 3)
  );

use work.pkg.rec_t;
package rec_pkg is new work.gpkg
  generic map (
    type_t => rec_t(field(0 to 3))
  );
",
    );

    let diagnostics = builder.analyze();
    check_no_diagnostics(&diagnostics);
}

#[test]
fn interface_subprogram() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package gpkg is
  generic (
    type type_t;
    function to_string(value : type_t) return string
  );
end package;

package pkg is
  package ipkg is new work.gpkg
    generic map (
      type_t => integer,
      to_string => to_string);
end package;

",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);

    let typ = root
        .search_reference(
            code.source(),
            code.s1("to_string => to_string")
                .s1("to_string")
                .pos()
                .start(),
        )
        .unwrap();
    assert_eq!(typ.decl_pos(), Some(&code.s1("to_string").pos()));
    assert!(root.format_declaration(typ).is_some());

    assert!(root
        .search_reference(code.source(), code.sa("to_string => ", "to_string").start())
        .is_some());
}

#[test]
fn interface_subprogram_rhs_not_a_subprogram() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package gpkg is
  generic (
    type type_t;
    function to_string(value : type_t) return string
  );
end package;

package pkg is
  package ipkg is new work.gpkg
    generic map (
      type_t => integer,
      to_string => character);
end package;

",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s1("character"),
            "Cannot map type 'CHARACTER' to subprogram generic",
        )],
    );
}

#[test]
fn interface_subprogram_rhs_does_not_match_signature() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package gpkg is
  generic (
    type type_t;
    function to_string(value : type_t) return string
  );
end package;

package pkg is
  function my_to_string(arg: real) return string;

  package ipkg is new work.gpkg
    generic map (
      type_t => integer,
      to_string => my_to_string);
end package;

",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.sa("to_string => ", "my_to_string"),
            "Cannot map 'my_to_string' to subprogram generic to_string[INTEGER return STRING]",
        )
        .related(
            code.s1("my_to_string"),
            "Does not match function my_to_string[REAL return STRING]",
        )],
    );
}

#[test]
fn interface_subprogram_operator() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package gpkg is
  generic (
    type type_t;
    function match(l, r : type_t) return boolean
  );
end package;

package pkg is
  package ipkg is new work.gpkg
    generic map (
      type_t => integer,
      match => \"=\");

  package bad_pkg is new work.gpkg
    generic map (
      type_t => integer,
      match => \"invalid\");
end package;

",
    );

    let (_, diagnostics) = builder.get_analyzed_root();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s1("\"invalid\""),
            "Invalid operator symbol",
        )],
    );
}

#[test]
fn interface_package() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package gpkg_base is
  generic (
    type type_t
  );
end package;

package gpkg is
  generic (
    package iface_pkg is new work.gpkg_base generic map (<>)
  );
end package;

package ipkg_base is new work.gpkg_base
generic map (
  type_t => integer);

package ipkg is new work.gpkg
generic map (
  iface_pkg => work.ipkg_base
);
",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);

    let typ = root
        .search_reference(
            code.source(),
            code.s1("iface_pkg => ").s1("iface_pkg").pos().start(),
        )
        .unwrap();
    assert_eq!(typ.decl_pos(), Some(&code.s1("iface_pkg").pos()));
    assert!(root.format_declaration(typ).is_some());
}

#[test]
fn generics_are_not_visible_by_selection() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package gpkg is
  generic (
    type type_t
  );

  constant also_visible : natural := 0;
end package;

package pkg is
  package ipkg is new work.gpkg
  generic map (
    type_t => integer);

  subtype sub_t is ipkg.type_t;
  constant c1 : natural := ipkg.also_visible;
end package;
  ",
    );

    let (_, diagnostics) = builder.get_analyzed_root();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s1("ipkg.type_t").s1("type_t"),
            "No declaration of 'type_t' within package instance 'ipkg'",
        )],
    );
}

#[test]
fn intantiate_items_with_correct_type() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
package gpkg is
  generic (
    type type_t;
    value: type_t
  );
  subtype sub_t is type_t;
end package;

package pkg is
  package ipkg is new work.gpkg
  generic map (
    type_t => integer,
    value => 0);

  constant c1 : ipkg.sub_t := 0;
end package;
  ",
    );

    let (_, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);
}

#[test]
fn interface_type_has_comparison_operations() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
package gpkg is
  generic (
    type type_t
  );

  procedure check(a, b : type_t);
end package;

package body gpkg is
  procedure check(a, b : type_t) is
      constant c0 : boolean := a = b;
      constant c1 : boolean := a /= b;
  begin
  end;

end package body;
  ",
    );

    let (_, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);
}

#[test]
fn hover_and_references_for_instantiated_entities() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package gpkg is
  generic (
    type type_t
  );
  subtype sub_t is type_t;
end package;

package pkg is
  package ipkg is new work.gpkg
  generic map (
    type_t => integer);

  constant c1 : ipkg.sub_t := 0;
end package;
  ",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);

    let sub_t = root
        .search_reference(
            code.source(),
            code.s1("ipkg.sub_t").s1("sub_t").pos().start(),
        )
        .unwrap();

    assert_eq!(sub_t.decl_pos(), Some(&code.s1("sub_t").pos()));

    assert_eq!(
        root.format_declaration(sub_t),
        Some("subtype sub_t is type_t;".to_owned())
    );

    assert_eq!(
        root.find_all_references(sub_t),
        vec![code.s("sub_t", 1).pos(), code.s("sub_t", 2).pos()]
    );

    // Ensure find all reference from within generic package also matches
    assert_eq!(
        root.find_all_references(
            root.search_reference(code.source(), code.s1("sub_t").pos().start())
                .unwrap()
        ),
        vec![code.s("sub_t", 1).pos(), code.s("sub_t", 2).pos()]
    );
}

#[test]
fn references_of_instantiated_do_not_include_siblings() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package gpkg is
  generic (
    type type_t
  );
  subtype sub_t is type_t;
end package;

package pkg is
  package ipkg0 is new work.gpkg
  generic map (
    type_t => integer);

  package ipkg1 is new work.gpkg
    generic map (
      type_t => natural);
  
  constant c0 : ipkg0.sub_t := 0;
  constant c1 : ipkg1.sub_t := 0;
end package;
  ",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);

    // References from parent matches all sibling
    assert_eq!(
        root.find_all_references(
            root.search_reference(code.source(), code.s1("sub_t").pos().start())
                .unwrap()
        ),
        vec![
            code.s("sub_t", 1).pos(),
            code.s("sub_t", 2).pos(),
            code.s("sub_t", 3).pos()
        ]
    );

    // Siblings only include parent
    assert_eq!(
        root.find_all_references(
            root.search_reference(code.source(), code.s("sub_t", 2).pos().start())
                .unwrap()
        ),
        vec![code.s("sub_t", 1).pos(), code.s("sub_t", 2).pos(),]
    );

    // Siblings only include parent
    assert_eq!(
        root.find_all_references(
            root.search_reference(code.source(), code.s("sub_t", 3).pos().start())
                .unwrap()
        ),
        vec![code.s("sub_t", 1).pos(), code.s("sub_t", 3).pos(),]
    );
}
