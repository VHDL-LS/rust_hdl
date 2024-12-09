use crate::analysis::tests::{check_no_diagnostics, LibraryBuilder};
use crate::data::ErrorCode;
use crate::syntax::test::check_diagnostics;
use crate::Diagnostic;

#[test]
fn allows_ranges_with_case_statements() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
entity test is
end entity;

architecture rtl of test is
  type    t_my_enum is (A1, A2, A3);
  subtype t_my_enum_range is t_my_enum range A1 to A2;
  signal  my_sig_sub : t_my_enum;
begin

  process
  begin
    case my_sig_sub is
      when t_my_enum'range => null;
      when t_my_enum_range => null;
      when t_my_enum_range'range => null;
      when A1 to A2 => null;
      when t_my_enum => null;
      when others => null;
    end case;
  end process;
end architecture;
",
    );

    let diagnostics = builder.analyze();
    check_no_diagnostics(&diagnostics);
}

#[test]
fn disallows_ranges_with_wrong_type() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity test is
end entity;

architecture rtl of test is
  type    t_my_enum is (A1, A2, A3);
  type    t_my_other_enum is (B1, B2, B3);
  subtype t_my_other_enum_range is t_my_other_enum range B1 to B2;
  signal  my_sig_sub : t_my_enum;
begin

  process
  begin
    case my_sig_sub is
      when t_my_other_enum'range => null;
      when t_my_other_enum_range => null;
      when others => null;
    end case;
  end process;
end architecture;
",
    );

    check_diagnostics(
        builder.analyze(),
        vec![
            Diagnostic::new(
                code.s1("t_my_other_enum'range"),
                "type 't_my_other_enum' does not match type 't_my_enum'",
                ErrorCode::TypeMismatch,
            ),
            Diagnostic::new(
                code.s("t_my_other_enum_range", 2),
                "subtype 't_my_other_enum_range' does not match type 't_my_enum'",
                ErrorCode::TypeMismatch,
            ),
        ],
    );
}
