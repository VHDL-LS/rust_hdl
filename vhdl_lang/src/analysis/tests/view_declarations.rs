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
