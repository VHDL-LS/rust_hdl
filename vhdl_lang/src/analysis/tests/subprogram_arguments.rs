// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2022, Olof Kraigher olof.kraigher@gmail.com

use super::*;
use vhdl_lang::data::error_codes::ErrorCode;

#[test]
fn wrong_number_of_arguments() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
function subpgm(arg: natural) return natural
is begin
end;

signal good : natural := subpgm(0);
signal bad : natural := subpgm;
        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::new(
            code.s1("subpgm;").s1("subpgm"),
            "Invalid call to 'subpgm'",
            ErrorCode::InvalidCall,
        )
        .related(code.s1("subpgm"), "Missing association of parameter 'arg'")],
    );
}

#[test]
fn procedure_calls() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "lib",
        "
entity ent is
end entity;

architecture a of ent is
    function subpgm(arg: natural) return natural
    is begin
    end;

    procedure theproc(arg: natural)
    is begin
    end;

    signal thesig : integer_vector(0 to 1);
begin
    
    subpgm(0);
    theproc(0);
    thesig(0);
end architecture;
",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::new(
                code.s("subpgm", 2),
                "Invalid procedure call",
                ErrorCode::InvalidCall,
            )
            .related(
                code.s("subpgm", 1),
                "function subpgm[NATURAL return NATURAL] is not a procedure",
            ),
            Diagnostic::new(
                code.s("thesig", 2),
                "signal 'thesig' of array type 'INTEGER_VECTOR' is not a procedure",
                ErrorCode::MismatchedKinds,
            ),
        ],
    );
}

#[test]
fn resolve_overloaded_subprogram_by_return_type() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
function subpgm(arg: natural) return character
is begin
end;

function subpgm(arg: natural) return natural
is begin
end;


signal good1 : natural := subpgm(0);
signal good2 : character := subpgm(0);
        ",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);

    assert_eq!(
        root.search_reference_pos(code.source(), code.s("subpgm", 3).end()),
        Some(code.s("subpgm", 2).pos())
    );
    assert_eq!(
        root.search_reference_pos(code.source(), code.s("subpgm", 4).end()),
        Some(code.s("subpgm", 1).pos())
    );
}

#[test]
fn resolves_formals() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
function subpgm(arg1 : integer) return integer;
constant good : integer := subpgm(arg1 => 1);
constant bad : integer := subpgm(arg2 => 1);
        ",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::new(
                code.s1("arg2"),
                "No declaration of 'arg2'",
                ErrorCode::Unresolved,
            ),
            Diagnostic::new(
                code.s1("subpgm(arg2 => 1)"),
                "No association of parameter 'arg1'",
                ErrorCode::Unassociated,
            )
            .related(code.s1("arg1"), "Defined here"),
        ],
    );

    assert_eq!(
        root.search_reference_pos(code.source(), code.s("arg1", 2).end()),
        Some(code.s("arg1", 1).pos())
    );
}

#[test]
fn resolve_overloaded_subprogram_by_argument() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
function subpgm(arg: character) return natural
is begin
end;

function subpgm(arg: natural) return natural
is begin
end;


signal good1 : natural := subpgm(0);
signal good2 : natural := subpgm('c');
        ",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);

    assert_eq!(
        root.search_reference_pos(code.source(), code.s("subpgm", 3).end()),
        Some(code.s("subpgm", 2).pos())
    );
    assert_eq!(
        root.search_reference_pos(code.source(), code.s("subpgm", 4).end()),
        Some(code.s("subpgm", 1).pos())
    );
}
#[test]
fn subprogram_argument_not_associated() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
function subpgm(arg1: natural; arg2: character) return natural
is begin
end;

signal bad : natural := subpgm(0);
        ",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::new(
            code.s1("subpgm(0)"),
            "No association of parameter 'arg2'",
            ErrorCode::Unassociated,
        )
        .related(code.s1("arg2"), "Defined here")],
    );

    assert_eq!(
        root.search_reference_pos(code.source(), code.s("subpgm", 2).end()),
        Some(code.s("subpgm", 1).pos())
    );
}

#[test]
fn subprogram_extra_argument_not_associated() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
function subpgm(arg1: natural) return natural
is begin
end;

signal bad : natural := subpgm(1111, 2222);
        ",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::new(
            code.s1("2222"),
            "Unexpected extra argument",
            ErrorCode::TooManyArguments,
        )],
    );

    assert_eq!(
        root.search_reference_pos(code.source(), code.s("subpgm", 2).end()),
        Some(code.s("subpgm", 1).pos())
    );
}

#[test]
fn for_loop_indexes_no_false_positives() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
procedure theproc(arg: integer) is
begin
end procedure;

procedure theproc(arg: boolean) is
begin
end procedure;

procedure calling is
variable foo : natural;
begin
for i in 0 to 3 loop
    theproc(i);
end loop;
end procedure;
",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);

    if false {
        // @TODO uncomment when analyzing for loop type
        assert_eq!(
            root.search_reference_pos(code.source(), code.s1("theproc(i)").start()),
            Some(code.s1("theproc").pos())
        );
    }
}

#[test]
fn default_before_positional_disambiguation() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
procedure theproc(arg: integer) is
begin
end procedure;

procedure theproc(arg: integer := 0; arg2: boolean) is
begin
end procedure;

procedure calling is
begin
    theproc(0);
end procedure;
",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);

    assert_eq!(
        root.search_reference_pos(code.source(), code.s1("theproc(0)").start()),
        Some(code.s1("theproc").pos())
    );
}

#[test]
fn named_argument_before_positional() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
procedure theproc(arg1: integer; arg2 : natural) is
begin
end procedure;

procedure calling is
begin
    theproc(arg1 => 0, 0);
end procedure;
",
    );

    let (_, diagnostics) = builder.get_analyzed_root();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::new(
            code.s("arg1", 2),
            "Named arguments are not allowed before positional arguments",
            ErrorCode::NamedBeforePositional,
        )],
    );
}

#[test]
fn argument_associated_as_both_named_and_positional() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
procedure theproc(arg: integer) is
begin
end procedure;

procedure calling is
begin
    theproc(0, arg => 0);
end procedure;
",
    );

    let (_, diagnostics) = builder.get_analyzed_root();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::new(
            code.s("arg", 2),
            "parameter 'arg' has already been associated",
            ErrorCode::AlreadyAssociated,
        )
        .related(code.s1("theproc(0, ").s1("0"), "Previously associated here")],
    );
}

#[test]
fn duplicate_named_argument() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
procedure theproc(arg: integer) is
begin
end procedure;

procedure calling is
begin
    theproc(arg => 0, arg => 0);
end procedure;
",
    );

    let (_, diagnostics) = builder.get_analyzed_root();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::new(
            code.s("arg", 3),
            "parameter 'arg' has already been associated",
            ErrorCode::AlreadyAssociated,
        )
        .related(code.s("arg", 2), "Previously associated here")],
    );
}

#[test]
fn partial_named_argument_is_allowed_multiple_times() {
    let mut builder = LibraryBuilder::new();
    builder.in_declarative_region(
        "
procedure theproc(arg: bit_vector) is
begin
end procedure;

procedure calling is
begin
    theproc(arg(0) => '0', arg(1 to 2) => \"01\");
end procedure;
",
    );

    let (_, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);
}

#[test]
fn cannot_combine_partial_and_full_argument() {
    let mut builder = LibraryBuilder::new();
    let code = builder.in_declarative_region(
        "
procedure theproc(arg: bit_vector) is
begin
end procedure;

procedure calling is
begin
    theproc(arg(0) => '0', arg => \"01\");
end procedure;
",
    );

    let (_, diagnostics) = builder.get_analyzed_root();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::new(
            code.s("arg", 3),
            "parameter 'arg' has already been associated",
            ErrorCode::AlreadyAssociated,
        )
        .related(code.s1("arg(0)"), "Previously associated here")],
    );
}
