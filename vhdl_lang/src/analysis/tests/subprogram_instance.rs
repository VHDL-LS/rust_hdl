use crate::analysis::tests::{check_no_diagnostics, LibraryBuilder};
use crate::syntax::test::check_diagnostics;
use crate::Diagnostic;

#[test]
pub fn cannot_instantiate_procedure_that_does_not_exist() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "\
procedure proc
    generic ( x: natural := 1 ) is
begin
end proc;

procedure proc is new foo;
    ",
    );

    let diagnostics = builder.analyze();
    assert_eq!(
        diagnostics,
        vec![Diagnostic::error(
            code.s1("foo").pos(),
            "No declaration of 'foo'"
        )]
    );
}

#[test]
pub fn instantiate_wrong_type() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "\
signal x : bit;

function proc is new x;
    ",
    );

    let diagnostics = builder.analyze();
    assert_eq!(
        diagnostics,
        vec![Diagnostic::error(
            code.s1("new x").s1("x"),
            "signal 'x' does not denote an uninstantiated subprogram"
        )]
    )
}

#[test]
pub fn ambiguous_multiple_uninstantiated_subprograms() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "\
procedure foo
    generic (type T)
    parameter (x : bit)
is begin
end foo;

procedure foo
    generic (type T)
    parameter (x : bit; y: bit)
is begin
end foo;

procedure proc is new foo;
    ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s1("new foo").s1("foo"),
            "Ambiguous instantiation of 'foo'",
        )
        .related(code.s("foo", 1), "Might be procedure foo[BIT]")
        .related(code.s("foo", 3), "Might be procedure foo[BIT, BIT]")],
    )
}

#[test]
pub fn by_signature_resolved_multiple_uninstantiated_subprograms() {
    let mut builder = LibraryBuilder::new();
    builder.in_declarative_region(
        "\
procedure foo
    generic (type T)
    parameter (x : bit)
is begin
end foo;

procedure foo
    generic (type T)
    parameter (x : bit; y: bit)
is begin
end foo;

procedure proc is new foo [bit];
procedure proc2 is new foo [bit, bit];
    ",
    );

    let diagnostics = builder.analyze();
    check_no_diagnostics(&diagnostics);
}

#[test]
pub fn complain_on_mismatching_signature() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "\
procedure foo
    generic (type T)
    parameter (x : bit)
is begin
end foo;

procedure proc is new foo [bit, bit];
    ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s1("[bit, bit]").pos(),
            "Signature does not match the the signature of procedure foo[BIT]",
        )],
    );
}

#[test]
pub fn can_instantiate_procedure_that_exists() {
    let mut builder = LibraryBuilder::new();
    builder.in_declarative_region(
        "\
procedure proc
    generic ( x: natural := 1 ) is
begin
end proc;

procedure proc is new proc;
    ",
    );

    let diagnostics = builder.analyze();
    check_no_diagnostics(&diagnostics);
}

#[test]
pub fn instantiated_kind_vs_declared_kind() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "\
procedure prok
    generic ( x: natural := 1 ) is
begin
end prok;

function func is new prok;
    ",
    );

    check_diagnostics(
        builder.analyze(),
        vec![
            Diagnostic::error(code.s1("function"), "Instantiating procedure as function")
                .related(code.s1("prok"), "procedure prok[] declared here"),
        ],
    );

    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "\
function funk
    generic ( x: natural := 1 ) return bit is
begin
end funk;

procedure proc is new funk;
    ",
    );

    check_diagnostics(
        builder.analyze(),
        vec![
            Diagnostic::error(code.s1("procedure"), "Instantiating function as procedure")
                .related(code.s1("funk"), "function funk[return BIT] declared here"),
        ],
    );

    let mut builder = LibraryBuilder::new();
    builder.in_declarative_region(
        "\
function proc generic (type T) return bit is
begin
end proc;

function proc is new proc;
    ",
    );

    check_no_diagnostics(&builder.analyze());

    let mut builder = LibraryBuilder::new();
    builder.in_declarative_region(
        "\
procedure proc generic (type T) is
begin
end proc;

procedure proc is new proc;
    ",
    );

    check_no_diagnostics(&builder.analyze())
}

#[test]
pub fn cannot_instantiate_procedure_without_header() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "\
procedure proc is
begin
end proc;

procedure proc is new proc;
    ",
    );

    check_diagnostics(
        builder.analyze(),
        vec![Diagnostic::error(
            code.s1("procedure proc is new").s("proc", 2).pos(),
            "procedure proc[] does not denote an uninstantiated subprogram",
        )],
    );
}

#[test]
pub fn cannot_call_procedure_with_header() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "\
entity ent is
end ent;

architecture arch of ent is
    procedure proc
        generic ( x: natural := 1 )
    is
    begin
    end proc;
begin
    proc;
end architecture arch;
    ",
    );

    check_diagnostics(
        builder.analyze(),
        vec![Diagnostic::error(
            code.s1("begin\n    proc;").s1("proc").pos(),
            "uninstantiated procedure proc[] cannot be called",
        )],
    )
}

#[test]
pub fn resolves_the_correct_instantiated_subprogram() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "\
entity ent is
end ent;

architecture arch of ent is
    procedure proc
        generic ( type T )
    is
    begin
    end proc;

    procedure proc is new proc generic map (T => natural);
begin
    proc;
end architecture arch;
    ",
    );

    check_no_diagnostics(&builder.analyze())
}

#[test]
pub fn resolves_its_generic_map() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "\
entity ent is
end ent;

architecture arch of ent is
    procedure foo
        generic ( type T )
        parameter (param : T)
    is
    begin
    end foo;

    procedure foo is new foo generic map (T => natural);
    procedure foo is new foo generic map (T => bit);
begin
    foo('1');
    foo(42);
end architecture arch;
    ",
    );

    let (root, diagnostics) = builder.get_analyzed_root();

    check_no_diagnostics(&diagnostics);

    assert_eq!(
        root.search_reference_pos(code.source(), code.s1("foo('1')").s1("foo").end(),),
        Some(
            code.s1("procedure foo is new foo generic map (T => bit)")
                .s1("foo")
                .pos()
        )
    );
    assert_eq!(
        root.search_reference_pos(code.source(), code.s1("foo(42)").s1("foo").end(),),
        Some(
            code.s1("procedure foo is new foo generic map (T => natural)")
                .s1("foo")
                .pos()
        )
    );
    assert_eq!(
        root.search_reference_pos(
            code.source(),
            code.s1("procedure foo is new foo generic map (T => natural)")
                .s("foo", 2)
                .end(),
        ),
        Some(code.s1("procedure foo").s1("foo").pos())
    );
    assert_eq!(
        root.search_reference_pos(
            code.source(),
            code.s1("procedure foo is new foo generic map (T => bit)")
                .s("foo", 2)
                .end(),
        ),
        Some(code.s1("procedure foo").s1("foo").pos())
    );
}

#[test]
pub fn resolve_instantiated_function() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "\
function foo
    generic (type F)
    parameter (x: F)
return F
is begin
   return x;
end foo;

function foo is new foo generic map (F => bit);

constant x : bit := foo('1');
    ",
    );

    let (root, diagnostics) = builder.get_analyzed_root();

    check_no_diagnostics(&diagnostics);
    assert_eq!(
        root.search_reference_pos(code.source(), code.s1("foo('1')").s1("foo").end(),),
        Some(
            code.s1("function foo is new foo generic map (F => bit);")
                .s1("foo")
                .pos()
        )
    );
}

// LRM 4.2.1: "An uninstantiated subprogram shall not be called,
// except as a recursive call within the body of the uninstantiated subprogram"
#[test]
#[ignore]
pub fn generic_subprogram_can_be_called_recursively() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "\
function foo
    generic (type F)
    parameter (x: F)
return F
is begin
   return foo(x);
end foo;

procedure bar
    generic (type F)
    parameter (x: F)
is begin
   bar(x);
end bar;
    ",
    );

    let (root, diagnostics) = builder.get_analyzed_root();

    check_no_diagnostics(&diagnostics);
    assert_eq!(
        root.search_reference_pos(code.source(), code.s1("foo(x)").s1("foo").end(),),
        Some(code.s1("function foo").s1("foo").pos())
    );
    assert_eq!(
        root.search_reference_pos(code.source(), code.s1("bar(x)").s1("bar").end(),),
        Some(code.s1("procedure bar").s1("bar").pos())
    );
}

// LRM 4.2.1: "an uninstantiated subprogram shall not be used as a resolution
// function or used as a conversion function in an association list."
#[test]
#[ignore]
pub fn generic_subprogram_cannot_be_used_as_resolution_function() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "\
function resolved generic (type F) parameter (x: F) return F;

subtype x is resolved bit;
    ",
    );

    check_diagnostics(
        builder.analyze(),
        vec![Diagnostic::error(
            code.s1("subtype x is resolved bit").s1("resolved"),
            "uninstantiated function resolved[F] return F cannot be used as resolution function",
        )],
    );
}

#[test]
#[ignore]
pub fn generic_subprogram_cannot_be_used_as_conversion_function() {
    let mut builder = LibraryBuilder::new();
    builder.in_declarative_region(
        "\
procedure bar (x : in bit) is
begin
end bar;

function foo generic (type F) parameter (x: bit) return bit;
    ",
    );

    let code = builder.snippet("bar(foo('1'))");

    check_diagnostics(
        builder.analyze(),
        vec![Diagnostic::error(
            code.s1("foo"),
            "uninstantiated function foo[F] return F cannot be used as conversion",
        )],
    );
}

#[test]
pub fn check_instantiated_function_errors_without_implementation() {}

// TODO: Check uninstantiated subprogram & declaration...
