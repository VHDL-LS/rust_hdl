use crate::analysis::tests::{check_diagnostics, LibraryBuilder};
use crate::Diagnostic;

#[test]
pub fn declaration_not_allowed_everywhere() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "\
entity ent is
end entity;

architecture arch of ent is

function my_func return natural is
    signal x : bit;
begin

end my_func;
begin

    my_block : block
        variable y: natural;
    begin
    end block my_block;

end architecture;
    ",
    );
    check_diagnostics(
        builder.analyze(),
        vec![
            Diagnostic::error(
                code.s1("signal x : bit;"),
                "signal declaration not allowed here",
            ),
            Diagnostic::error(
                code.s1("variable y: natural;"),
                "variable declaration not allowed here",
            ),
        ],
    )
}
