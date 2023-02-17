use super::*;

#[test]
fn adds_to_string_for_standard_types() {
    check_code_with_no_diagnostics(
        "
package pkg is
    alias alias1 is to_string[integer return string];
    alias alias2 is minimum[integer, integer return integer];
    alias alias3 is maximum[integer, integer return integer];
end package;
",
    );
}

// procedure FILE_OPEN (file F: FT; External_Name: in STRING; Open_Kind: in FILE_OPEN_KIND := READ_MODE);
// procedure FILE_OPEN (Status: out FILE_OPEN_STATUS; file F: FT; External_Name: in STRING; Open_Kind: in FILE_OPEN_KIND := READ_MODE);
// procedure FILE_CLOSE (file F: FT);
// procedure READ (file F: FT; VALUE: out TM);
// procedure WRITE (file F: FT; VALUE: in TM);
// procedure FLUSH (file F: FT);
// function ENDFILE (file F: FT) return BOOLEAN
#[test]
fn adds_file_subprograms_implicitly() {
    check_code_with_no_diagnostics(
        "
package pkg is
end package;

package body pkg is
  type binary_file_t is file of character;

  procedure proc is
    file f : binary_file_t;
    variable char : character;
  begin
    file_open(f, \"foo.txt\");
    assert not endfile(f);
    write(f, 'c');
    flush(f);
    read(f, char);
    file_close(f);
  end procedure;
end package body;
",
    );
}

#[test]
fn adds_to_string_for_integer_types() {
    check_code_with_no_diagnostics(
        "
package pkg is
  type type_t is range 0 to 1;
  alias my_to_string is to_string[type_t return string];
end package;
",
    );
}

#[test]
fn adds_to_string_for_array_types() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "lib",
        "
package pkg is
  type type_t is array (natural range 0 to 1) of bit;
  alias my_to_string is to_string[type_t return string];

  type bad_t is array (natural range 0 to 1) of integer;
  alias bad_to_string is to_string[bad_t return string];
end package;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        without_releated(&diagnostics),
        vec![Diagnostic::error(
            code.sa("bad_to_string is ", "to_string"),
            "Could not find declaration of 'to_string' with given signature",
        )],
    )
}

#[test]
fn adds_to_string_for_enum_types() {
    check_code_with_no_diagnostics(
        "
package pkg is
  type enum_t is (alpha, beta);
  alias my_to_string is to_string[enum_t return string];
end package;  
  ",
    );
}

#[test]
fn no_error_for_duplicate_alias_of_implicit() {
    check_code_with_no_diagnostics(
        "
package pkg is
  type type_t is array (natural range 0 to 1) of integer;
  alias alias_t is type_t;
  -- Should result in no error for duplication definiton of for example TO_STRING
end package;
",
    );
}

#[test]
fn deallocate_is_defined_for_access_type() {
    check_code_with_no_diagnostics(
        "
package pkg is
  type arr_t is array (natural range <>) of character;
  type ptr_t is access arr_t;

  procedure theproc is
      variable theptr: ptr_t;
  begin
      deallocate(theptr);
  end procedure;
end package;
",
    );
}

#[test]
fn enum_implicit_function_is_added_on_use() {
    check_code_with_no_diagnostics(
        "
package pkg1 is
    type enum_t is (alpha, beta);
end package;

use work.pkg1.enum_t;
package pkg is
    alias my_to_string is to_string[enum_t return string];
end package;
",
    );
}

#[test]
fn find_all_references_does_not_include_implicits() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "lib",
        "
package pkg is
type enum_t is (alpha, beta);
alias my_to_string is to_string[enum_t return string];
end package;
",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);

    assert_eq_unordered(
        &root.find_all_references_pos(&code.s1("enum_t").pos()),
        &[code.s("enum_t", 1).pos(), code.s("enum_t", 2).pos()],
    );
}

#[test]
fn goto_references_for_implicit() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "lib",
        "
package pkg is
type enum_t is (alpha, beta);
alias thealias is to_string[enum_t return string];
end package;
",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);

    let to_string = code.s1("to_string");
    assert_eq!(
        root.search_reference_pos(to_string.source(), to_string.start()),
        Some(code.s1("enum_t").pos())
    );
}

#[test]
fn hover_for_implicit() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "lib",
        "
package pkg is
type enum_t is (alpha, beta);
alias thealias is to_string[enum_t return string];
end package;
",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);

    let to_string = code.s1("to_string");
    assert_eq!(
        root.format_declaration(
            root.search_reference(to_string.source(), to_string.start())
                .unwrap()
        ),
        Some(
            "\
-- function TO_STRING[enum_t return STRING]

-- Implicitly defined by:
type enum_t is (alpha, beta);
"
            .to_owned()
        )
    );
}

#[test]
fn implicit_functions_on_physical_type() {
    check_code_with_no_diagnostics(
        "
package pkg is
    type time_t is range 0 to 1
    units
      small;
      big = 1000 small;
    end units;

    constant c0 : time_t := 10 small;
    constant good1 : time_t := - c0;
    constant good2 : time_t := + c0;
    constant good3 : time_t := abs c0;
    constant good4 : time_t := c0 + c0;
    constant good5 : time_t := c0 - c0;
    constant good6 : time_t := minimum(c0, c0);
    constant good7 : time_t := maximum(c0, c0);
end package;
",
    );
}

#[test]
fn implicit_functions_on_integer_type() {
    check_code_with_no_diagnostics(
        "
package pkg is
    type type_t is range 0 to 1;

    constant c0 : type_t := 10;
    constant good1 : type_t := - c0;
    constant good2 : type_t := + c0;
    constant good3 : type_t := abs c0;
    constant good4 : type_t := c0 + c0;
    constant good5 : type_t := c0 - c0;
    constant good6 : type_t := minimum(c0, c0);
    constant good7 : type_t := maximum(c0, c0);
    constant good8 : string := to_string(c0);
end package;
",
    );
}
