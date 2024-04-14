use crate::analysis::tests::{check_diagnostics, LibraryBuilder};
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
