use super::*;

#[test]
fn test_static_array_size() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
signal bad: bit_vector(2 downto 0) := \"1100\";
constant good: bit_vector(2 downto 0) := \"100\";
        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s1("\"1100\""),
            "Left hand side of expression has a size of 3 but the right hand side has a size of 4",
        )],
    );
}
