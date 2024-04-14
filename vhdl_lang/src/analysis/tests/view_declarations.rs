use crate::analysis::tests::{check_diagnostics, check_no_diagnostics, LibraryBuilder};
use crate::data::ErrorCode;
use crate::Diagnostic;
use crate::VHDLStandard::VHDL2019;

#[test]
pub fn view_mode_declaration_must_have_declared_subtype() {
    let mut builder = LibraryBuilder::with_standard(VHDL2019);
    let code = builder.in_declarative_region(
        "\
view my_view of undeclared is
end view;
    ",
    );
    let (_, diag) = builder.get_analyzed_root();
    check_diagnostics(
        diag,
        vec![Diagnostic::new(
            code.s1("undeclared"),
            "No declaration of 'undeclared'",
            ErrorCode::Unresolved,
        )],
    )
}

#[test]
pub fn view_mode_declaration_must_have_record_as_type() {
    let mut builder = LibraryBuilder::with_standard(VHDL2019);
    let code = builder.in_declarative_region(
        "\
type foo is (A, B);

view my_view of foo is
end view;
    ",
    );
    let (_, diag) = builder.get_analyzed_root();
    check_diagnostics(
        diag,
        vec![Diagnostic::new(
            code.s("foo", 2),
            "The type of a view must be a record type, not type 'foo'",
            ErrorCode::TypeMismatch,
        )
        .related(code.s1("foo"), "type 'foo' declared here")],
    );

    let mut builder = LibraryBuilder::with_standard(VHDL2019);
    builder.in_declarative_region(
        "\
type foo is record
  x: bit;
end record;

view my_view of foo is
  x : in;
end view;
    ",
    );
    let (_, diag) = builder.get_analyzed_root();
    check_no_diagnostics(&diag);
}

#[test]
pub fn element_in_view_that_is_not_in_record() {
    let mut builder = LibraryBuilder::with_standard(VHDL2019);
    let code = builder.in_declarative_region(
        "\
type foo is record
    bar: bit;
end record;

view my_view of foo is
    baz: in;
    bar: out;
end view;
    ",
    );
    let (_, diag) = builder.get_analyzed_root();
    check_diagnostics(
        diag,
        vec![Diagnostic::new(
            code.s1("baz"),
            "Not a part of record type 'foo'",
            ErrorCode::Unresolved,
        )],
    );
}

#[test]
pub fn view_reference_set_to_the_original_element() {
    let mut builder = LibraryBuilder::with_standard(VHDL2019);
    let code = builder.in_declarative_region(
        "\
type foo is record
    bar: bit;
end record;

view my_view of foo is
    bar: in;
end view;
    ",
    );
    let (root, diag) = builder.get_analyzed_root();
    check_no_diagnostics(&diag);
    let record_element = root
        .search_reference(code.source(), code.s1("bar: bit").s1("bar").start())
        .unwrap();
    let view_element = root
        .search_reference(code.source(), code.s1("bar: in").s1("bar").start())
        .unwrap();
    assert_eq!(record_element, view_element)
}

#[test]
pub fn diagnostic_when_elements_are_missing() {
    let mut builder = LibraryBuilder::with_standard(VHDL2019);
    let code = builder.in_declarative_region(
        "\
type foo is record
    bar: bit;
    baz: bit;
end record;

view my_view of foo is
    bar: in;
end view;
    ",
    );
    let diag = builder.analyze();
    check_diagnostics(
        diag,
        vec![Diagnostic::new(
            code.s1("my_view"),
            "Missing association of element 'baz'",
            ErrorCode::Unassociated,
        )],
    );

    let mut builder = LibraryBuilder::with_standard(VHDL2019);
    let code = builder.in_declarative_region(
        "\
type foo is record
    bar: bit;
    baz: bit;
    foobar: bit;
end record;

view my_view of foo is
    bar: in;
end view;
    ",
    );
    let diag = builder.analyze();
    check_diagnostics(
        diag,
        vec![Diagnostic::new(
            code.s1("my_view"),
            "Missing association of elements 'baz' and 'foobar'",
            ErrorCode::Unassociated,
        )],
    );
    let mut builder = LibraryBuilder::with_standard(VHDL2019);
    let code = builder.in_declarative_region(
        "\
type foo is record
    bar: bit;
    baz: bit;
    foobar: bit;
    foobaz: bit;
end record;

view my_view of foo is
    bar: in;
end view;
    ",
    );
    let diag = builder.analyze();
    check_diagnostics(
        diag,
        vec![Diagnostic::new(
            code.s1("my_view"),
            "Missing association of elements 'baz', 'foobar' and 'foobaz'",
            ErrorCode::Unassociated,
        )],
    );

    let mut builder = LibraryBuilder::with_standard(VHDL2019);
    let code = builder.in_declarative_region(
        "\
type foo is record
    a, b, c, d, e, f, g, h: bit;
end record;

view my_view of foo is
    a: in;
end view;
    ",
    );
    let diag = builder.analyze();
    check_diagnostics(
        diag,
        vec![Diagnostic::new(
            code.s1("my_view"),
            "Missing association of elements 'b', 'c', 'd' and 4 more",
            ErrorCode::Unassociated,
        )],
    );
}
