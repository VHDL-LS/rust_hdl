use crate::analysis::tests::{check_diagnostics, LibraryBuilder};
use crate::Diagnostic;

#[test]
pub fn declaration_not_allowed_everywhere() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "\
function my_func return natural is
    signal x : bit;
begin

end my_func;
    ",
    );
    check_diagnostics(
        builder.analyze(),
        vec![Diagnostic::error(
            code.s1("signal x : bit;"),
            "signal declaration not allowed here",
        )],
    )
}
