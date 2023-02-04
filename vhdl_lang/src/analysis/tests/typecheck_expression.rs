// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

use super::*;

#[test]
fn test_integer_literal_expression_typecheck() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
constant good_a : integer := 1;
constant good_b : natural := 2;

constant bad_a : boolean := 3;

subtype my_bool is boolean;
constant bad_b : my_bool := 4;

        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(
                code.s1("3"),
                "integer literal does not match type 'BOOLEAN'",
            ),
            Diagnostic::error(
                code.s1("4"),
                "integer literal does not match subtype 'my_bool'",
            ),
        ],
    );
}

#[test]
fn test_integer_literal_with_alias() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
alias alias_t is integer;
constant good : alias_t := 1;
constant bad : alias_t := false;
        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s1("false"),
            "'false' does not match alias 'alias_t'",
        )],
    );
}

#[test]
fn test_character_literal_expression() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
constant good : character := 'a';
constant bad : natural := 'b';
        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s1("'b'"),
            "character literal does not match subtype 'NATURAL'",
        )],
    );
}

#[test]
fn test_character_literal_expression_not_part_of_enum() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
type enum_t is ('a', 'b');
constant good : enum_t := 'a';
constant bad : enum_t := 'c';
        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s1("'c'"),
            "character literal does not match type 'enum_t'",
        )],
    );
}

#[test]
fn test_physical_literal_expression_typecheck() {
    let mut builder = LibraryBuilder::new();
    builder.in_declarative_region(
        "
constant good_a : time := ns;
constant good_b : time := 2 ps;
        ",
    );

    let diagnostics = builder.analyze();
    check_no_diagnostics(&diagnostics);
}

#[test]
fn test_string_literal_expression() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
signal good : string(1 to 3) := \"101\";
signal bad : natural := \"110\";
        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s1("\"110\""),
            "string literal does not match subtype 'NATURAL'",
        )],
    )
}

#[test]
fn test_string_literals_allowed_characters_for_array() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
type enum_t is ('a', foo);
type enum_vec_t is array (natural range <>) of enum_t;

signal good1 : bit_vector(1 to 1) := \"1\";
signal good2 : enum_vec_t(1 to 1) := \"a\";

signal bad2 : bit_vector(1 to 1) := \"2\";
signal bad3 : enum_vec_t(1 to 1) := \"b\";

type enum_vec2_t is array (natural range <>, natural range <>) of enum_t;
type enum_vec3_t is array (natural range <>) of enum_vec_t(1 to 1);
signal bad4 : enum_vec2_t(1 to 1, 1 to 1) := \"a\";
signal bad5 : enum_vec3_t(1 to 1) := \"a\";
        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(code.s1("\"2\""), "type 'BIT' does not define character '2'"),
            Diagnostic::error(
                code.s1("\"b\""),
                "type 'enum_t' does not define character 'b'",
            ),
            Diagnostic::error(
                code.s("\"a\"", 2),
                "string literal does not match array type 'enum_vec2_t'",
            ),
            Diagnostic::error(
                code.s("\"a\"", 3),
                "string literal does not match array type 'enum_vec3_t'",
            ),
        ],
    )
}

#[test]
fn test_integer_selected_name_expression_typecheck() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
constant ival : integer := 999;

type rec_t is record
  elem : natural;
end record;

constant rval : rec_t := (elem => 0);

constant good_a : integer := ival;
constant good_b : natural := rval.elem;

constant bad_a : boolean := ival;

subtype my_bool is boolean;
constant bad_b : my_bool := rval.elem;

        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(
                code.s("ival", 3),
                "constant 'ival' of integer type 'INTEGER' does not match type 'BOOLEAN'",
            ),
            Diagnostic::error(
                code.s("rval.elem", 2),
                "subtype 'NATURAL' does not match subtype 'my_bool'",
            ),
        ],
    );
}

#[test]
fn test_enum_literal_expression_typecheck() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
subtype my_bool is boolean;
constant good_a : boolean := true;
constant good_b : my_bool := false;

constant bad_a : integer := true;
constant bad_b : character := false;

        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(
                code.s("true", 2),
                "'true' does not match integer type 'INTEGER'",
            ),
            Diagnostic::error(
                code.s("false", 2),
                "'false' does not match type 'CHARACTER'",
            ),
        ],
    );
}

#[test]
fn test_unique_function_typecheck() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
function fun1 return natural is
begin
    return 0;
end function;

function fun1 return boolean is
begin
    return false;
end function;

constant good : integer := fun1;
constant bad : character := fun1;
        ",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(code.s("fun1", 4), "Could not resolve 'fun1'")
                .related(
                    code.s("fun1", 1),
                    "Does not match return type of fun1[return NATURAL]",
                )
                .related(
                    code.s("fun1", 2),
                    "Does not match return type of fun1[return BOOLEAN]",
                ),
        ],
    );

    assert_eq!(
        root.search_reference_pos(code.source(), code.s1(":= fun1").end()),
        Some(code.s1("fun1").pos())
    );
}

#[test]
fn test_unique_function_default_arg_typecheck() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
function fun1(arg : natural := 0) return natural is
begin
    return 0;
end function;

function fun1 return boolean is
begin
    return false;
end function;

constant good : integer := fun1;
constant bad : character := fun1;
        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(code.s("fun1", 4), "Could not resolve 'fun1'")
                .related(
                    code.s("fun1", 1),
                    "Does not match return type of fun1[NATURAL return NATURAL]",
                )
                .related(
                    code.s("fun1", 2),
                    "Does not match return type of fun1[return BOOLEAN]",
                ),
        ],
    );
}

#[test]
fn test_ambiguous_function_default_arg_typecheck() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
function fun1(arg : natural := 0) return natural is
begin
    return 0;
end function;

function fun1(arg : boolean := false) return natural is
begin
    return 0;
end function;

-- Do not consider this as ambiguous
function fun1(arg : character) return natural is
begin
    return 0;
end function;

constant bad: integer := fun1;
        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(code.s1(":= fun1").s1("fun1"), "Ambiguous call to 'fun1'")
                .related(code.s("fun1", 1), "Migth be fun1[NATURAL return NATURAL]")
                .related(code.s("fun1", 2), "Migth be fun1[BOOLEAN return NATURAL]"),
        ],
    );
}

#[test]
fn test_name_can_be_indexed() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
constant foo : natural := 0;
constant bar : natural := foo(0);
        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s("foo", 2),
            "constant 'foo' of subtype 'NATURAL' cannot be indexed",
        )],
    );
}

#[test]
fn test_name_can_be_sliced() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
constant foo : natural := 0;
constant bar : natural := foo(0 to 0);
        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s("foo", 2),
            "constant 'foo' of subtype 'NATURAL' cannot be sliced",
        )],
    );
}

#[test]
fn test_access_type_can_be_indexed() {
    let mut builder = LibraryBuilder::new();
    builder.in_declarative_region(
        "
type arr_t is array (0 to 1) of natural;
type access_t is access arr_t;

procedure proc is
   variable myvar : access_t;
   variable foo : natural;
begin
    foo := myvar(0);
end procedure;
        ",
    );

    let diagnostics = builder.analyze();
    check_no_diagnostics(&diagnostics);
}

#[test]
fn function_result_can_be_indexed() {
    let mut builder = LibraryBuilder::new();
    builder.in_declarative_region(
        "
function thefun(arg : natural) return integer_vector is
begin
   return (0, 1);
end;

constant good : natural := thefun(0)(0);
        ",
    );

    let diagnostics = builder.analyze();
    check_no_diagnostics(&diagnostics);
}

#[ignore = "Does not work yet"]
#[test]
fn function_result_can_be_indexed_no_arg() {
    let mut builder = LibraryBuilder::new();
    builder.in_declarative_region(
        "
function thefun return integer_vector is
begin
   return (0, 1);
end;

constant good : natural := thefun(0);
        ",
    );

    let diagnostics = builder.analyze();
    check_no_diagnostics(&diagnostics);
}

#[test]
fn test_type_conversion() {
    let mut builder = LibraryBuilder::new();
    builder.in_declarative_region(
        "
  constant foo : natural := natural(0);
        ",
    );

    let diagnostics = builder.analyze();
    check_no_diagnostics(&diagnostics);
}

#[test]
fn test_indexed_array_dimension_check() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
type arr1_t is array (0 to 1) of natural;
type arr2_t is array (0 to 1, 0 to 1) of natural;
constant foo1 : arr1_t := (0, 1);
constant foo2 : arr2_t := ((0, 1), (2, 3));

constant bar1 : natural := foo1(0);
constant bar2 : natural := foo1(0, 1);
constant bar3 : natural := foo2(0);
constant bar4 : natural := foo2(0, 1);
        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(
                code.s("foo1(0, 1)", 1),
                "Number of indexes does not match array dimension",
            )
            .related(
                code.s("arr1_t", 1),
                "Array type 'arr1_t' has 1 dimension, got 2 indexes",
            ),
            Diagnostic::error(
                code.s("foo2(0)", 1),
                "Number of indexes does not match array dimension",
            )
            .related(
                code.s("arr2_t", 1),
                "Array type 'arr2_t' has 2 dimensions, got 1 index",
            ),
        ],
    );
}

#[test]
fn test_disambiguates_indexed_name_and_function_call() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
constant foo : natural := 0;
constant bar : natural := foo(arg => 0);
        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s("foo", 2),
            "constant 'foo' cannot be called as a function",
        )],
    );
}

#[test]
fn test_typechecks_expression_for_type_mark_with_subtype_attribute() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
signal sig0 : integer_vector(0 to 7);
signal sig1 : sig0'subtype := false;
signal sig2 : sig0'subtype := (others => 0); -- ok
",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s1("false"),
            "'false' does not match array type 'INTEGER_VECTOR'",
        )],
    );
}

#[test]
fn test_typechecks_expression_for_type_mark_with_element_attribute() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
subtype ivec_subtype_t is integer_vector(0 to 7);
signal sig : integer_vector(0 to 7);

signal bad1 : sig'element := (0, 0);
signal bad2 : sig'element := 'a';

signal good1 : sig'element := 0; -- ok

subtype sub_from_object_t is sig'element;
signal good2 : sub_from_object_t := 0; -- ok

subtype sub_from_type_t is ivec_subtype_t'element;
signal good3 : sub_from_type_t := 0; -- ok

subtype bad1_t is good2'element;
subtype bad2_t is integer'element;
",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(
                code.s1("(0, 0)"),
                "composite does not match integer type 'INTEGER'",
            ),
            Diagnostic::error(
                code.s1("'a'"),
                "character literal does not match integer type 'INTEGER'",
            ),
            Diagnostic::error(
                code.s1("good2'element").s1("good2"),
                "array type expected for 'element attribute",
            ),
            Diagnostic::error(
                code.s1("integer'element").s1("integer"),
                "array type expected for 'element attribute",
            ),
        ],
    );
}

#[test]
fn qualified_expression_type_mark() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
signal good : natural := integer'(0);
signal bad1 : natural := integer'(\"hello\");
signal bad2 : natural := string'(\"hello\");
",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(
                code.s1("(\"hello\")"),
                "string literal does not match integer type 'INTEGER'",
            ),
            Diagnostic::error(
                code.s1("string'(\"hello\")"),
                "array type 'STRING' does not match subtype 'NATURAL'",
            ),
        ],
    );
}

#[test]
fn subprogram_positional_argument() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
procedure theproc(arg: character) is
begin
end procedure;

function thefun(arg: integer) return natural is
begin
    theproc(arg);
    return 0;
end function;

constant const : natural := thefun('c');
",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(
                code.s1("theproc(arg)").s1("arg"),
                "constant 'arg' of integer type 'INTEGER' does not match type 'CHARACTER'",
            ),
            Diagnostic::error(
                code.s1("thefun('c')").s1("'c'"),
                "character literal does not match integer type 'INTEGER'",
            ),
        ],
    );
}

#[test]
fn check_real_vs_integer() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
constant rgood : real := 1.2;
constant rbad : real := 3;

constant igood : integer := 4;
constant ibad : integer := 5.6;

",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(
                code.s1("3"),
                "integer literal does not match real type 'REAL'",
            ),
            Diagnostic::error(
                code.s1("5.6"),
                "real literal does not match integer type 'INTEGER'",
            ),
        ],
    );
}

#[test]
fn check_bitstring_literal() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
constant good1 : bit_vector := x\"1\";
constant bad1 : integer := x\"2\";
constant bad2 : integer_vector := x\"3\";

type enum_t is (alpha, beta);
type arr_t is array (natural range <>) of enum_t;
constant bad3 : arr_t := x\"4\";
constant bad4 : arr_t := x\"5\";

type enum0_t is ('0', alpha);
type arr0_t is array (natural range <>) of enum0_t;
constant good3 : arr0_t := x\"00\";
constant bad5 : arr0_t := x\"6\";

",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(
                code.s1("x\"2\""),
                "bit string literal does not match integer type 'INTEGER'",
            ),
            Diagnostic::error(
                code.s1("x\"3\""),
                "bit string literal does not match array type 'INTEGER_VECTOR'",
            ),
            Diagnostic::error(
                code.s1("x\"4\""),
                "element type 'enum_t' of array type 'arr_t' does not define character '0'",
            ),
            Diagnostic::error(
                code.s1("x\"4\""),
                "element type 'enum_t' of array type 'arr_t' does not define character '1'",
            ),
            Diagnostic::error(
                code.s1("x\"5\""),
                "element type 'enum_t' of array type 'arr_t' does not define character '0'",
            ),
            Diagnostic::error(
                code.s1("x\"5\""),
                "element type 'enum_t' of array type 'arr_t' does not define character '1'",
            ),
            Diagnostic::error(
                code.s1("x\"6\""),
                "element type 'enum0_t' of array type 'arr0_t' does not define character '1'",
            ),
        ],
    );
}

#[test]
fn check_null_literal() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
type ptr_t is access integer_vector;

procedure proc is
    variable good : ptr_t := null;
    variable bad : integer := null;
begin
end procedure;

",
    );
    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s("null", 2),
            "null literal does not match integer type 'INTEGER'",
        )],
    );
}

#[test]
fn typecheck_aggregate() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
type rec_t is record
    field : natural;
end record;

constant good1 : integer_vector := (0, 1, 2);
constant good2 : rec_t := (others => 0);
constant bad1 : integer := (3, 4, 5);
        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s1("(3, 4, 5)"),
            "composite does not match integer type 'INTEGER'",
        )],
    );
}

#[test]
fn record_aggregate_must_be_simple_name() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
type rec_t is record
    field : natural;
end record;

constant bad : rec_t := (field(0) => 0);
        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s1("field(0)"),
            "Record aggregate choice must be a simple name",
        )],
    );
}

#[test]
fn typecheck_aggregate_element_association_expr() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
type rec_t is record
    field : integer;
end record;

type arr2_t is array (0 to 1, 0 to 1) of natural;

constant good1 : integer_vector := (0, 1, 2);
constant good2 : arr2_t := ((0, 1), (2, 3));
constant good3 : rec_t := (field => 0);
constant bad1 : integer_vector := (3, 4, 'c');
constant bad2 : integer_vector := (others => 'd');
constant bad3 : integer_vector := (1 to 3 => 'e');
constant bad4 : rec_t := (field => 'f');

        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(
                code.s1("'c'"),
                "character literal does not match integer type 'INTEGER'",
            ),
            Diagnostic::error(
                code.s1("'d'"),
                "character literal does not match integer type 'INTEGER'",
            ),
            Diagnostic::error(
                code.s1("'e'"),
                "character literal does not match integer type 'INTEGER'",
            ),
            Diagnostic::error(
                code.s1("'f'"),
                "character literal does not match integer type 'INTEGER'",
            ),
        ],
    );
}

#[test]
fn typecheck_array_association_index() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
constant good : integer_vector := (0 | 1 => 11, 2 => 22);
constant bad1 : integer_vector := ('c' => 0);
constant bad2 : integer_vector := ('a' to 'z' => 0);
        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(
                code.s1("'c'"),
                "character literal does not match integer type 'INTEGER'",
            ),
            Diagnostic::error(
                code.s1("'a'"),
                "character literal does not match integer type 'INTEGER'",
            ),
            Diagnostic::error(
                code.s1("'z'"),
                "character literal does not match integer type 'INTEGER'",
            ),
        ],
    );
}

/// LRM 9.3.3.3 Array aggregates
#[test]
fn array_element_association_may_be_array_of_element_type() {
    let mut builder = LibraryBuilder::new();
    builder.in_declarative_region(
        "
constant good1 : integer_vector := (0 to 2 => (0, 1, 2));
constant good2 : string(1 to 6) := (\"text\", others => ' ');

        ",
    );

    let diagnostics = builder.analyze();
    check_no_diagnostics(&diagnostics);
}

#[test]
fn array_element_association_may_be_type_denoting_discrete_range() {
    let mut builder = LibraryBuilder::new();
    builder.in_declarative_region(
        "
subtype sub_t is natural range 1 to 3;
constant good1 : integer_vector := (sub_t => 0);
        ",
    );

    let diagnostics = builder.analyze();
    check_no_diagnostics(&diagnostics);
}

#[test]
fn evaluates_unary_expressions() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
constant i0 : integer := 0;
constant r0 : real := 0.0;
constant t0 : time := 0 ns;
constant good1 : integer := - 1;
constant good2 : real := - 1.0;
constant good3 : time := - 1 ns;
constant good4 : integer := - i0;
constant good5 : real := - r0;
constant good6 : time := - t0;

constant bad1 : character := - i0;
constant bad2 : character := - 'a';
        ",
    );

    let (_, diagnostics) = builder.get_analyzed_root();

    check_diagnostics(
        diagnostics,
        vec![
            // Prefer to complain on return type when operator arguments are unambiguous
            Diagnostic::error(
                code.s1("character := - i0").s1("- i0"),
                "integer type 'INTEGER' does not match type 'CHARACTER'",
            ),
            Diagnostic::error(
                code.s1("character := - 'a'").s1("-"),
                "Found no match for operator \"-\"",
            ),
        ],
    );
}

#[test]
fn evaluates_binary_expressions() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
constant i0 : integer := 0;
constant r0 : real := 0.0;
constant t0 : time := 0 ns;
constant good1 : integer := 1 + 1;
constant good2 : real := 1.0 + 1.0;
constant good3 : time := 1 ns + 1 ns;
constant good4 : integer := i0 + i0;
constant good5 : real := r0 + r0;
constant good6 : time := t0 + t0;

constant bad1 : character := i0 + i0;
constant bad2 : character := 'a' + 'b';
        ",
    );

    let (_, diagnostics) = builder.get_analyzed_root();

    check_diagnostics(
        diagnostics,
        vec![
            // Prefer to complain on return type when operator arguments are unambiguous
            Diagnostic::error(
                code.s1("character := i0 + i0").s1("i0 + i0"),
                "integer type 'INTEGER' does not match type 'CHARACTER'",
            ),
            Diagnostic::error(
                code.s1("character := 'a' + 'b'").s1("+"),
                "Found no match for operator \"+\"",
            ),
        ],
    );
}

#[test]
fn overloading_nested_ambiguous_op_has_acceptable_performance() {
    let mut builder = LibraryBuilder::new();
    builder.in_declarative_region(
        "
    constant const : integer_vector :=
      (0, 0) & 0 &
      (0, 0) & 0 &
      (0, 0) & 0 &
      (0, 0) & 0 &
      (0, 0) & 0 &
      (0, 0) & 0 &
      (0, 0) & 0 &
      (0, 0) & 0 &
      (0, 0) & 0 &
      (0, 0) & 0;",
    );
    let diagnostics = builder.analyze();
    check_no_diagnostics(&diagnostics);
}

#[test]
fn overloading_nested_ambiguous_func_has_acceptable_performance() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "lib",
        "
entity ent is
end entity;

architecture a of ent is
    function func(arg : integer_vector; arg2 : integer) return integer_vector is
    begin
      return (0, 1);
    end;

    function func(arg : integer; arg2 : integer_vector) return integer_vector is
    begin
      return (0, 1);
    end;

    function func(arg : integer_vector; arg2 : integer_vector) return integer_vector is
    begin
      return (0, 1);
    end;

    function func(arg : integer; arg2 : integer) return integer_vector is
    begin
      return (0, 1);
    end;

    constant const : integer_vector :=
      func(func(
      func(func(
      func(func(
      func(func(
      func(func(
      func(func(
      func(func(
      func(func(
      func(func(
      func(func(
      func((3, 3), 0),
      (0, 0)), 0),
      (0, 0)), 0),
      (0, 0)), 0),
      (0, 0)), 0),
      (0, 0)), 0),
      (0, 0)), 0),
      (0, 0)), 0),
      (0, 0)), 0),
      (0, 0)), 0),
      (0, 0)), 0);
begin
end architecture;",
    );
    let diagnostics = builder.analyze();
    check_no_diagnostics(&diagnostics);
}

#[test]
fn attribute_spec_typecheck() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
attribute ram_style : integer;
signal good, bad : integer_vector(0 to 15);
attribute ram_style of good : signal is 0;
attribute ram_style of bad : signal is 'c';
        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s1("'c'"),
            "character literal does not match integer type 'INTEGER'",
        )],
    );
}

#[test]
fn attribute_spec_signature() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
attribute ram_style : integer;

signal good_sig : integer_vector(0 to 15);
signal bad_sig : integer_vector(0 to 15);
attribute ram_style of good_sig : signal is 0;
attribute ram_style of bad_sig[return integer] : signal is 0;

function good_fun return natural; 
function bad_fun1 return natural; 
function bad_fun2 return natural; 

attribute ram_style of good_fun[return natural] : signal is 0;
attribute ram_style of bad_fun1 : signal is 0;
attribute ram_style of bad_fun2[return boolean] : signal is 0;

",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s1("[return integer]"),
            "Attribute specification should only have a signature for subprograms and enum literals",
        ),
        Diagnostic::error(
            code.s1("bad_fun1 : signal").s1("bad_fun1"),
            "Signature required for alias of subprogram and enum literals",
        ),
        Diagnostic::error(
            code.s1("bad_fun2[return boolean]").s1("bad_fun2"),
            "Could not find declaration of 'bad_fun2' with given signature",
        ).related(code.s1("bad_fun2"), "Found bad_fun2[return NATURAL]")],
    );
}

#[test]
fn typecheck_function_return_statement() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
function good return integer is
begin
  return 0;
end;

function bad1 return integer is
begin
  return 'c';
end;


function bad2 return integer is
begin
  return;
end;

procedure bad3 is
begin
  return 1;
end;

",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(
                code.s1("'c'"),
                "character literal does not match integer type 'INTEGER'",
            ),
            Diagnostic::error(
                code.s1("return;"),
                "Functions cannot return without a value",
            ),
            Diagnostic::error(code.s1("return 1;"), "Procedures cannot return a value"),
        ],
    );
}

#[test]
fn typecheck_report_statement() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
procedure wrapper is
begin
   report \"good\";
   report 16#bad#;
   report \"good\" severity error;
   report \"good\" severity \"bad\";
end;



",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(
                code.s1("16#bad#"),
                "integer literal does not match array type 'STRING'",
            ),
            Diagnostic::error(
                code.s1("\"bad\""),
                "string literal does not match type 'SEVERITY_LEVEL'",
            ),
        ],
    );
}

#[test]
fn typecheck_assert_statement() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
procedure wrapper is
begin
   assert true report \"good\";
   assert true report 16#bad#;
   assert true report \"good\" severity error;
   assert true report \"good\" severity \"bad\";
   assert 123;
   assert bit'('0');
end;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(
                code.s1("16#bad#"),
                "integer literal does not match array type 'STRING'",
            ),
            Diagnostic::error(
                code.s1("\"bad\""),
                "string literal does not match type 'SEVERITY_LEVEL'",
            ),
            Diagnostic::error(
                code.s1("123"),
                "type universal_integer cannot be implictly converted to type 'BOOLEAN'. Operator ?? is not defined for this type.",
            ),
        ],
    );
}

#[test]
fn ambiguous_boolean_conversion_favors_boolean() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
function myfun return boolean is
begin
  return true;
end function;

function myfun return bit is
begin
  return '0';
end function;

procedure wrapper is
begin
   assert myfun;
end;
",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);

    let decl = root
        .search_reference(code.source(), code.s1("assert myfun;").s1("myfun").start())
        .unwrap();

    // Favors boolean
    assert_eq!(
        decl.decl_pos().unwrap(),
        &code
            .s1("function myfun return boolean is")
            .s1("myfun")
            .pos(),
    );
}

#[test]
fn ambiguous_qq_conversion() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
type typ1_t is (alpha, beta);
type typ2_t is (alpha, beta);
type typ3_t is (alpha, beta);

function \"??\"(val : typ1_t) return boolean is
begin
  return true;
end function;

function \"??\"(val : typ2_t) return boolean is
begin
  return true;
end function;

procedure wrapper is
begin
   assert alpha;
end;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s1("assert alpha").s1("alpha"),
            "Ambiguous use of implicit boolean conversion ??",
        )
        .related(code.s1("typ1_t"), "Could be type 'typ1_t'")
        .related(code.s1("typ2_t"), "Could be type 'typ2_t'")],
    );
}

#[test]
fn ambiguous_qq_conversion_no_candidates() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
type typ1_t is (alpha, beta);
type typ2_t is (alpha, beta);

procedure wrapper is
begin
   assert alpha;
end;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s1("assert alpha").s1("alpha"),
            "Cannot disambiguate expression to type 'BOOLEAN'",
        )
        .related(
            code.s1("typ1_t"),
            "Implicit boolean conversion operator ?? is not defined for type 'typ1_t'",
        )
        .related(
            code.s1("typ2_t"),
            "Implicit boolean conversion operator ?? is not defined for type 'typ2_t'",
        )],
    );
}
