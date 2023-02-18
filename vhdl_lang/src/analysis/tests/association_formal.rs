// This Source Code Form is subject to the terms of the Mozilla Public
// Lic// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// This Source Code Form is subject to the terms of the Mozilla Public
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2022, Olof Kraigher olof.kraigher@gmail.com
use super::*;
use pretty_assertions::assert_eq;

#[test]
fn missing_port_name() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent_inst is
end entity;

architecture a of ent_inst is
begin
end architecture;

entity ent is
end entity;

architecture a of ent is
   signal sig : boolean;
begin
   ent: entity work.ent_inst
      port map (missing => sig);
end architecture;
        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s1("missing"),
            "No declaration of 'missing'",
        )],
    );
}

#[test]
fn missing_generic_name() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent_inst is
end entity;

architecture a of ent_inst is
begin
end architecture;

entity ent is
end entity;

architecture a of ent is
   signal sig : boolean;
begin
   ent: entity work.ent_inst
      generic map (missing => sig);
end architecture;
        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s1("missing"),
            "No declaration of 'missing'",
        )],
    );
}

#[test]
fn resolve_port_name() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent_inst is
    port (
        theport : in boolean 
    );
end entity;

architecture a of ent_inst is
begin
end architecture;

entity ent is
end entity;

architecture a of ent is
   signal sig : boolean;
begin
   ent: entity work.ent_inst
      port map (theport => sig);
end architecture;
        ",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);

    assert_eq!(
        root.search_reference_pos(code.source(), code.s("theport", 2).end()),
        Some(code.s1("theport").pos())
    );
}

#[test]
fn resolve_generic_name() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent_inst is
    generic (
        thegeneric : boolean 
    );
end entity;

architecture a of ent_inst is
begin
end architecture;

entity ent is
end entity;

architecture a of ent is
begin
   ent: entity work.ent_inst
      generic map (thegeneric => false);
end architecture;
        ",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);

    assert_eq!(
        root.search_reference_pos(code.source(), code.s("thegeneric", 2).end()),
        Some(code.s1("thegeneric").pos())
    );
}

#[test]
fn does_not_mixup_ports_and_generics() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent_inst is
    generic (
        thegeneric : boolean 
    );
    port (
        theport : in boolean 
    );
end entity;

architecture a of ent_inst is
begin
end architecture;

entity ent is
end entity;

architecture a of ent is
   signal sig : boolean;
begin
   ent: entity work.ent_inst
       generic map (theport => sig)
       port map (thegeneric => 0);
end architecture;
        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(code.s("theport", 2), "No declaration of 'theport'"),
            Diagnostic::error(
                code.s1("work.ent_inst"),
                "No association of port 'theport' : in",
            )
            .related(code.s1("theport"), "Defined here"),
            Diagnostic::error(code.s("thegeneric", 2), "No declaration of 'thegeneric'"),
            Diagnostic::error(
                code.s1("work.ent_inst"),
                "No association of generic 'thegeneric'",
            )
            .related(code.s1("thegeneric"), "Defined here"),
        ],
    );
}

#[test]
fn resolve_port_and_surrounding_name() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent_inst is
    port (
        theport : in integer_vector(0 to 0) 
    );
end entity;

architecture a of ent_inst is
begin
end architecture;

entity ent is
end entity;

architecture a of ent is
    constant const0 : natural := 0;
begin
   ent: entity work.ent_inst
      port map (theport(const0) => 0);
end architecture;
        ",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);

    assert_eq!(
        root.search_reference_pos(code.source(), code.s("theport", 2).end()),
        Some(code.s1("theport").pos())
    );

    assert_eq!(
        root.search_reference_pos(code.source(), code.s("const0", 2).end()),
        Some(code.s1("const0").pos())
    );
}

#[test]
fn function_conversion_of_port_name() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
    port (
        theport: out natural
    );
end entity;

architecture a of ent is
begin
end architecture;

entity ent2 is
end entity;

architecture a of ent2 is    
    function fun1(arg : natural) return natural is
    begin
        return arg;
    end function;

    signal sig : natural;
begin
    inst: entity work.ent
        port map (
        fun1(theport) => sig);
end architecture;
        ",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);

    assert_eq!(
        root.search_reference_pos(code.source(), code.s("fun1", 2).end()),
        Some(code.s1("fun1").pos())
    );

    assert_eq!(
        root.search_reference_pos(code.source(), code.s("theport", 2).end()),
        Some(code.s1("theport").pos())
    );
}

#[test]
fn type_conversion_of_port_name() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
    port (
        theport: out natural
    );
end entity;

architecture a of ent is
begin
end architecture;

entity ent2 is
end entity;

architecture a of ent2 is
    signal sig : real;
begin
    inst: entity work.ent
        port map (
        real(theport) => sig);
end architecture;
        ",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);

    assert_eq!(
        root.search_reference_pos(code.source(), code.s("theport", 2).end()),
        Some(code.s1("theport").pos())
    );
}

#[test]
fn function_conversion_of_port_name_must_be_single_argument() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
    port (
        theport: out natural
    );
end entity;

architecture a of ent is
begin
end architecture;

entity ent2 is
end entity;

architecture a of ent2 is    
    function fun1(arg : natural; arg2: natural) return natural is
    begin
        return arg;
    end function;

    signal sig : natural;
begin
    inst: entity work.ent
        port map (
        fun1(theport, 2) => sig);
end architecture;
        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s1("fun1(theport, 2)"),
            "Invalid formal conversion",
        )],
    );
}

#[test]
fn function_conversion_of_port_name_must_not_have_its_own_formal() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
    port (
        theport: out natural
    );
end entity;

architecture a of ent is
begin
end architecture;

entity ent2 is
end entity;

architecture a of ent2 is    
    function fun1(arg : natural) return natural is
    begin
        return arg;
    end function;

    signal sig : natural;
begin
    inst: entity work.ent
        port map (
        fun1(arg => theport) => sig);
end architecture;
        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s1("fun1(arg => theport)"),
            "Invalid formal conversion",
        )],
    );
}

#[test]
fn output_ports_may_be_left_open() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent2 is
port (
    signal inport: in natural;
    signal outport: out natural  );
end entity;

architecture a of ent2 is
begin
end architecture;

entity ent is
end entity;

architecture a of ent is
    signal sig : natural;
begin
    inst: entity work.ent2
        port map (
        inport => sig
        );
end architecture;
    ",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);
    // Still resolves references when missing output port
    assert!(root
        .search_reference(code.source(), code.s1("inport => sig").s1("sig").start())
        .is_some())
}
