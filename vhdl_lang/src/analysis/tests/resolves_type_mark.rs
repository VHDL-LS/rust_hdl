// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

use super::*;
use pretty_assertions::assert_eq;
use vhdl_lang::data::error_codes::ErrorCode;

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
        .map(|idx| {
            Diagnostic::new(
                code.s("missing", 1 + idx),
                "No declaration of 'missing'",
                ErrorCode::Unresolved,
            )
        })
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
        vec![Diagnostic::new(
            code.s1("missing"),
            "No declaration of 'missing'",
            ErrorCode::Unresolved,
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
        vec![Diagnostic::new(
            code.s1("missing"),
            "No declaration of 'missing'",
            ErrorCode::Unresolved,
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
        root.search_reference_pos(code2.source(), code2.s1(" typ_t").start()),
        None
    );

    // Cursor at beginning of symbol
    assert_eq!(
        root.search_reference_pos(code2.source(), code2.s1("typ_t").start()),
        Some(decl_pos.clone())
    );

    // Cursor at end of symbol
    assert_eq!(
        root.search_reference_pos(code2.source(), code2.s1("typ_t").end()),
        Some(decl_pos)
    );

    // Cursor after end of symbol
    assert_eq!(
        root.search_reference_pos(code2.source(), code2.s1("typ_t ").end()),
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
        root.search_reference_pos(code.source(), decl_pos.start()),
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
        code1.s("typ_t", 1).pos(),
        code1.s("typ_t", 2).pos(),
        code2.s("typ_t", 1).pos(),
        code2.s("typ_t", 2).pos(),
    ];

    assert_eq_unordered(
        &root.find_all_references_pos(&code1.s1("typ_t").pos()),
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
        root.search_reference_pos(code.source(), code.s("typ_t", 1).end()),
        Some(code.s("typ_t", 1).pos())
    );

    // Goto declaration from reference
    assert_eq!(
        root.search_reference_pos(code.source(), code.s("typ_t", 2).end()),
        Some(code.s("typ_t", 1).pos())
    );

    assert_eq_unordered(
        &root.find_all_references_pos(&code.s("typ_t", 1).pos()),
        &[code.s("typ_t", 1).pos(), code.s("typ_t", 2).pos()],
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
        .map(|idx| {
            Diagnostic::new(
                code.s("missing_t", idx),
                "No declaration of 'missing_t'",
                ErrorCode::Unresolved,
            )
        })
        .collect();
    check_diagnostics(diagnostics, expected);

    let num_references = 5;
    let mut references = Vec::new();

    for i in 1..=num_references {
        let refpos = code.s("typ_t", i).pos();
        assert_eq!(
            root.search_reference_pos(code.source(), refpos.end()),
            Some(code.s("typ_t", 1).pos()),
            "i={}",
            i
        );
        references.push(refpos.clone());
    }

    assert_eq_unordered(
        &root.find_all_references_pos(&code.s("typ_t", 1).pos()),
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

#[test]
fn error_on_type_mark_with_overloaded() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
end package;

package body pkg is
  function bad return natural is
  begin
  end function;

  constant err : bad := 0;

end package body;

",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![kind_error(
            &code,
            "bad",
            2,
            1,
            "type",
            "function bad[return NATURAL]",
        )],
    );
}

#[test]
fn error_on_type_mark_with_non_type() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
end package;

package body pkg is
  constant bad : natural := 0;
  constant err : bad := 0;
end package body;

",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![kind_error(&code, "bad", 2, 1, "type", "constant 'bad'")],
    );
}

#[test]
fn error_on_type_mark_with_alias_of_non_type() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
  constant const : natural := 0;
  alias bad is const;
  constant name : bad := 0;
end package;
",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![kind_error(
            &code,
            "bad",
            2,
            1,
            "type",
            "alias 'bad' of constant",
        )],
    );
}

#[test]
fn test_qualified_expression_must_be_a_type() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
constant bar : natural := 0; 
constant foo : natural := bar'(0);
        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![kind_error(&code, "bar", 2, 1, "type", "constant 'bar'")],
    );
}

#[test]
fn test_type_mark_with_subtype_attribute_is_ok() {
    let mut builder = LibraryBuilder::new();
    builder.in_declarative_region(
        "
signal sig0 : integer_vector(0 to 7);

type rec_t is record
    field: integer_vector(0 to 7);
end record;
signal rec : rec_t;

attribute attr : sig0'subtype;
signal sig1 : sig0'subtype;
signal sig2 : rec.field'subtype;   
",
    );
    let diagnostics = builder.analyze();
    check_no_diagnostics(&diagnostics);
}

#[test]
fn check_good_type_marks() {
    check_code_with_no_diagnostics(
        "

package gpkg is
  -- Interface type
  generic (type type_t);

  type record_t is record
    field : type_t;
  end record;
end package;

entity ent is
end entity ent;

architecture arch of ent is
  type incomplete;
  -- Incomplete type
  type access_t is access incomplete;
  type incomplete is record
    field : access_t;
  end record;

  type ptype_t is protected
  end protected;

  type ptype_t is protected body
  end protected body;

  -- Protected type
  shared variable ptype : ptype_t;

  -- Other type
  type enum_t is (alpha, beta);
  constant const : enum_t := alpha;

  -- Alias of type
  alias alias_t is enum_t;
  constant const2 : alias_t := alpha;
begin
end architecture;
",
    );
}

#[test]
fn array_element_resolution_requires_array_type() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
type rec_t is record
  f : bit;
end record;
function res(arg : bit_vector) return bit;

subtype sub1_t is (res) bit;
subtype sub2_t is (res) rec_t;
subtype sub3_t is ((res)) bit_vector;
",
    );

    check_diagnostics(
        builder.analyze(),
        vec![
            Diagnostic::mismatched_kinds(
                code.s1("(res) bit;").s1("(res)"),
                "Resolving elements requires an array type",
            )
            .related(
                code.s1("(res) bit;").s1("bit"),
                "type 'BIT' is not an array type",
            ),
            Diagnostic::mismatched_kinds(
                code.s1("(res) rec_t").s1("(res)"),
                "Resolving elements requires an array type",
            )
            .related(
                code.s1("(res) rec_t").s1("rec_t"),
                "record type 'rec_t' is not an array type",
            ),
            Diagnostic::mismatched_kinds(
                code.s1("((res)) bit_vector").s1("(res)"),
                "Resolving elements requires an array type",
            )
            .related(
                code.s1("((res)) bit_vector").s1("bit_vector"),
                "Element type 'BIT' of this type is not an array type",
            ),
        ],
    );
}

#[test]
fn record_element_resolution_requires_record_type() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
function res(arg : bit_vector) return bit;

subtype sub1_t is (f res) bit;
subtype sub2_t is (f res) bit_vector;
subtype sub3_t is ((f res)) bit_vector;
",
    );

    check_diagnostics(
        builder.analyze(),
        vec![
            Diagnostic::mismatched_kinds(
                code.s1("(f res) bit;").s1("(f res)"),
                "Resolving record fields requires a record type",
            )
            .related(
                code.s1("(f res) bit;").s1("bit"),
                "type 'BIT' is not a record type",
            ),
            Diagnostic::mismatched_kinds(
                code.s1("(f res) bit_vector").s1("(f res)"),
                "Resolving record fields requires a record type",
            )
            .related(
                code.s1("(f res) bit_vector").s1("bit_vector"),
                "array type 'BIT_VECTOR' is not a record type",
            ),
            Diagnostic::mismatched_kinds(
                code.s1("((f res)) bit_vector").s1("(f res)"),
                "Resolving record fields requires a record type",
            )
            .related(
                code.s1("((f res)) bit_vector").s1("bit_vector"),
                "Element type 'BIT' of this type is not a record type",
            ),
        ],
    );
}

#[test]
fn record_element_resolution_resolves_elements() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
type inner_t is record
  a : bit;
end record;
type outer_t is record
  i : inner_t;
  b : bit_vector;
end record;
function res(arg : bit_vector) return bit;

subtype sub_t is (i (a res), b (res)) outer_t;
",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);

    let usage = code.s1("(i (a res), b (res))");
    for (name, decl) in [
        ("i", code.s1("i : inner_t").s1("i")),
        ("a", code.s1("a : bit").s1("a")),
        ("b", code.s1("b : bit_vector").s1("b")),
    ] {
        assert_eq!(
            root.search_reference_pos(code.source(), usage.s1(name).start()),
            Some(decl.pos()),
            "{name}"
        );
    }
}

#[test]
fn record_element_resolution_requires_existing_elements() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
type inner_t is record
  a : bit;
end record;
type outer_t is record
  i : inner_t;
end record;
function res(arg : bit_vector) return bit;

subtype sub_t is (x res, i (y res)) outer_t;
",
    );

    check_diagnostics(
        builder.analyze(),
        vec![
            Diagnostic::new(
                code.s1("x res").s1("x"),
                "No declaration of 'x' within record type 'outer_t'",
                ErrorCode::Unresolved,
            ),
            Diagnostic::new(
                code.s1("y res").s1("y"),
                "No declaration of 'y' within record type 'inner_t'",
                ErrorCode::Unresolved,
            ),
        ],
    );
}

#[test]
fn record_element_resolution_continues_after_unresolved_function() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
type rec_t is record
  a : bit;
  b : bit;
end record;
function res(arg : bit_vector) return bit;

subtype sub_t is (a missing, b res) rec_t;
",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::new(
            code.s1("missing"),
            "No declaration of 'missing'",
            ErrorCode::Unresolved,
        )],
    );

    let usage = code.s1("b res");
    assert_eq!(
        root.search_reference_pos(code.source(), usage.s1("b").start()),
        Some(code.s1("b : bit").s1("b").pos())
    );
    assert_eq!(
        root.search_reference_pos(code.source(), usage.s1("res").start()),
        Some(code.s1("function res").s1("res").pos())
    );
}

#[test]
fn resolution_function_is_disambiguated_by_type() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
type bv_arr_t is array (natural range <>) of bit_vector;

function res(arg : bit_vector) return bit;
function res(arg : string) return character;
function res(arg : bv_arr_t) return bit_vector;
procedure res(arg : bit_vector);

subtype sub1_t is res bit;
subtype sub2_t is res character;
subtype sub3_t is res bit_vector;
subtype sub4_t is (res) bit_vector;
subtype sub5_t is ((res)) bv_arr_t;
",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);

    let bit_res = code.s1("function res(arg : bit_vector)").s1("res").pos();
    let character_res = code.s1("function res(arg : string)").s1("res").pos();
    let bit_vector_res = code.s1("function res(arg : bv_arr_t)").s1("res").pos();

    for (usage, decl_pos) in [
        ("res bit;", &bit_res),
        ("res character;", &character_res),
        ("res bit_vector;", &bit_vector_res),
        ("(res) bit_vector;", &bit_res),
        ("((res)) bv_arr_t;", &bit_res),
    ] {
        assert_eq!(
            root.search_reference_pos(code.source(), code.s1(usage).s1("res").start()),
            Some(decl_pos.clone()),
            "{usage}"
        );
    }
}

#[test]
fn resolution_function_candidates_are_filtered_out() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
type enum_t is (res);
type bit_matrix_t is array (natural range <>, natural range <>) of bit;

procedure res(arg0 : bit_vector);
function res generic (type T) parameter (arg1 : T) return bit;
function res(arg2 : bit_vector) return boolean;
function res(arg3, arg4 : bit_vector) return bit;
function res(arg5 : bit) return bit;
function res(arg6 : string) return bit;
function res(arg7 : bit_matrix_t) return bit;

subtype sub_t is res bit;
",
    );

    check_diagnostics(
        builder.analyze(),
        vec![Diagnostic::new(
            code.s1("res bit;").s1("res"),
            "Could not resolve resolution function 'res'",
            ErrorCode::Unresolved,
        )
        .related(
            code.s1("(res)").s1("res"),
            "candidate res[return enum_t] is not a function",
        )
        .related(
            code.s1("procedure res").s1("res"),
            "candidate procedure res[BIT_VECTOR] is not a function",
        )
        .related(
            code.s1("function res generic").s1("res"),
            "candidate function res[T return BIT] is an uninstantiated subprogram",
        )
        .related(
            code.s1("function res(arg2").s1("res"),
            "candidate function res[BIT_VECTOR return BOOLEAN] returns type 'BOOLEAN' instead of type 'BIT'",
        )
        .related(
            code.s1("function res(arg3").s1("res"),
            "candidate function res[BIT_VECTOR, BIT_VECTOR return BIT] does not have a single input parameter",
        )
        .related(
            code.s1("arg5"),
            "candidate function res[BIT return BIT] has a parameter that is not of an array type",
        )
        .related(
            code.s1("arg6"),
            "candidate function res[STRING return BIT] has a parameter whose element type 'CHARACTER' does not match type 'BIT'",
        )
        .related(
            code.s1("arg7"),
            "candidate function res[bit_matrix_t return BIT] has a parameter that is not a one-dimensional array",
        )],
    );
}

#[test]
fn ambiguous_resolution_function() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
type bv2_t is array (natural range <>) of bit;

function res(arg : bit_vector) return bit;
function res(arg : bv2_t) return bit;

subtype sub_t is res bit;
",
    );

    check_diagnostics(
        builder.analyze(),
        vec![Diagnostic::new(
            code.s1("res bit;").s1("res"),
            "Ambiguous resolution function 'res'",
            ErrorCode::AmbiguousCall,
        )
        .related(
            code.s1("function res(arg : bit_vector)").s1("res"),
            "Might be function res[BIT_VECTOR return BIT]",
        )
        .related(
            code.s1("function res(arg : bv2_t)").s1("res"),
            "Might be function res[bv2_t return BIT]",
        )],
    );
}

pub fn kind_error(
    code: &Code,
    name: &str,
    occ: usize,
    occ_decl: usize,
    expected: &str,
    got: &str,
) -> Diagnostic {
    Diagnostic::mismatched_kinds(code.s(name, occ), format!("Expected {expected}, got {got}"))
        .related(code.s(name, occ_decl), "Defined here")
}
