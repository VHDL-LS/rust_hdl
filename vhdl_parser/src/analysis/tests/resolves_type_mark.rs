// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

use super::*;
use pretty_assertions::assert_eq;

#[test]
fn resolves_type_mark_in_subtype_indications() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg1 is
-- Object declaration
constant const : natural := 0;
constant const2 : missing := 0;

-- File declaration
file fil : std.textio.text;
file fil2 : missing;

-- Alias declaration
alias foo : natural is const;
alias foo2 : missing is const;

-- Array type definiton
type arr_t is array (natural range <>) of natural;
type arr_t2 is array (natural range <>) of missing;

-- Access type definiton
type acc_t is access natural;
type acc_t2 is access missing;

-- Subtype definiton
subtype sub_t is natural range 0 to 1;
subtype sub_t2 is missing range 0 to 1;

-- Record definition
type rec_t is record
 f1 : natural;
 f2 : missing;
end record;

-- Interface file
procedure p1 (fil : std.textio.text);
procedure p2 (fil : missing);

-- Interface object
function f1 (const : natural) return natural;
function f2 (const : missing) return natural;
end package;",
    );

    let expected = (0..9)
        .map(|idx| Diagnostic::error(code.s("missing", 1 + idx), "No declaration of 'missing'"))
        .collect();

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, expected);
}

#[test]
fn resolves_return_type() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
function f1 (const : natural) return natural;
function f2 (const : natural) return missing;
end package;",
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
fn resolves_attribute_declaration_type_mark() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
attribute attr : string;
attribute attr2 : missing;
end package;",
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
fn search_resolved_type_mark() {
    let mut builder = LibraryBuilder::new();
    let code1 = builder.code(
        "libname",
        "
package pkg is
  type typ_t is (foo, bar);
end package;",
    );

    let code2 = builder.code(
        "libname",
        "
use work.pkg.all;

package pkg2 is
  constant c : typ_t := bar;
end package;",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);

    let decl_pos = code1.s1("typ_t").pos();

    // Cursor before symbol
    assert_eq!(
        root.search_reference(code2.source(), code2.s1(" typ_t").start()),
        None
    );

    // Cursor at beginning of symbol
    assert_eq!(
        root.search_reference(code2.source(), code2.s1("typ_t").start()),
        Some(decl_pos.clone())
    );

    // Cursor at end of symbol
    assert_eq!(
        root.search_reference(code2.source(), code2.s1("typ_t").end()),
        Some(decl_pos.clone())
    );

    // Cursor after end of symbol
    assert_eq!(
        root.search_reference(code2.source(), code2.s1("typ_t ").end()),
        None
    );
}

#[test]
fn search_reference_on_declaration_returns_declaration() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
  type typ_t is (foo, bar);
end package;",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);

    let decl_pos = code.s1("typ_t").pos();

    assert_eq!(
        root.search_reference(code.source(), decl_pos.start()),
        Some(decl_pos)
    );
}

#[test]
fn find_all_references_of_type_mark() {
    let mut builder = LibraryBuilder::new();
    let code1 = builder.code(
        "libname",
        "
package pkg is
  type typ_t is (foo, bar);
  constant c1 : typ_t := bar;
end package;",
    );

    let code2 = builder.code(
        "libname",
        "
use work.pkg.all;

package pkg2 is
  constant c2 : typ_t := bar;
  constant c3 : typ_t := bar;
end package;",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);

    let references = vec![
        code1.s("typ_t", 1).pos().clone(),
        code1.s("typ_t", 2).pos().clone(),
        code2.s("typ_t", 1).pos().clone(),
        code2.s("typ_t", 2).pos().clone(),
    ];

    assert_eq_unordered(
        &root.find_all_references(&code1.s1("typ_t").pos()),
        &references,
    );
}

#[test]
fn find_references_in_record_defintions() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
  type typ_t is (foo, bar);
  type rec_t is record
    field : typ_t;
  end record;
end package;",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);

    // Goto declaration from declaration
    assert_eq!(
        root.search_reference(code.source(), code.s("typ_t", 1).end()),
        Some(code.s("typ_t", 1).pos())
    );

    // Goto declaration from reference
    assert_eq!(
        root.search_reference(code.source(), code.s("typ_t", 2).end()),
        Some(code.s("typ_t", 1).pos())
    );

    assert_eq_unordered(
        &root.find_all_references(&code.s("typ_t", 1).pos()),
        &vec![
            code.s("typ_t", 1).pos().clone(),
            code.s("typ_t", 2).pos().clone(),
        ],
    );
}

#[test]
fn find_references_in_array_defintions() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
  -- Dummy type to use where we do not care
  type placeholder_t is (a, b);

  -- The type we want to resolve references to
  type typ_t is (foo, bar);

  -- With index subtype constraint
  type arr1_t is array (typ_t range <>) of typ_t;
  type arr2_t is array (missing_t range <>) of placeholder_t;

  type arr3_t is array (typ_t) of typ_t;
  type arr4_t is array (missing_t) of placeholder_t;

end package;",
    );

    let (root, diagnostics) = builder.get_analyzed_root();

    let num_missing = 2;
    let expected = (1..=num_missing)
        .map(|idx| Diagnostic::error(code.s("missing_t", idx), "No declaration of 'missing_t'"))
        .collect();
    check_diagnostics(diagnostics, expected);

    let num_references = 5;
    let mut references = Vec::new();

    for i in 1..=num_references {
        let refpos = code.s("typ_t", i).pos();
        assert_eq!(
            root.search_reference(code.source(), refpos.end()),
            Some(code.s("typ_t", 1).pos()),
            "i={}",
            i
        );
        references.push(refpos.clone());
    }

    assert_eq_unordered(
        &root.find_all_references(&code.s("typ_t", 1).pos()),
        &references,
    );
}

#[test]
fn search_type_mark_in_file_object_declaration() {
    check_search_reference(
        "
package pkg is
  type decl is file of character;
  file foo : decl;
end package;
",
    );
}
