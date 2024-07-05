// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use crate::data::error_codes::ErrorCode;
use itertools::Itertools;

use super::*;

#[test]
fn signal_attribute_must_in_the_same_declarative_part() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
end entity;

architecture a of ent is
    attribute myattr : boolean;

    signal good, bad : natural;
    attribute myattr of good : signal is true;
begin
    process
        attribute myattr of bad : signal is true;
    begin
    end process;
end architecture;
        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::new(
            code.s("bad", 2),
            "Attribute specification must be in the immediate declarative part",
            ErrorCode::MisplacedAttributeSpec,
        )],
    );
}

#[test]
fn entity_attribute_must_in_the_same_declarative_part() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity myent is
    attribute myattr : boolean;
    attribute myattr of myent : entity is true;
end entity;

architecture a of myent is
    attribute myattr of myent : entity is true;
begin
end architecture;
        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::new(
            code.s("myent", 4),
            "Attribute specification must be in the immediate declarative part",
            ErrorCode::MisplacedAttributeSpec,
        )],
    );
}

#[test]
fn interface_attribute_must_in_the_same_declarative_part() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
    port (good, bad : out boolean);
    attribute myattr : natural;
    attribute myattr of good : signal is 1337;
end entity;

architecture a of ent is
  attribute myattr of bad : signal is 1337;
begin
end architecture;
        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::new(
            code.s("bad", 2),
            "Attribute specification must be in the immediate declarative part",
            ErrorCode::MisplacedAttributeSpec,
        )],
    );
}

#[test]
fn custom_attribute_can_be_used() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
entity ent is
    attribute myattr : boolean;
    attribute myattr of ent : entity is false;
end entity;

architecture a of ent is
    constant c0 : boolean := ent'myattr;

    signal mysig : natural;
    attribute myattr of mysig : signal is false;
    constant c1 : boolean := mysig'myattr;
    
    type mytype is (alpha, beta);
    attribute myattr of mytype : type is false;
    constant c2 : boolean := mytype'myattr;
begin
end architecture;
        ",
    );

    let diagnostics = builder.analyze();
    check_no_diagnostics(&diagnostics);
}

#[test]
fn finds_references_of_custom_attributes() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
    attribute myattr : boolean;
    attribute myattr of ent : entity is false;
end entity;

architecture a of ent is
    constant c0 : boolean := ent'myattr;
begin
end architecture;
        ",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);

    assert_eq!(
        root.find_all_references_pos(&code.s1("myattr").pos()),
        vec![
            code.s1("attribute myattr : boolean"),
            code.s1("myattr of ent"),
            code.s1("ent'myattr"),
        ]
        .into_iter()
        .map(|c| c.s1("myattr").pos())
        .collect_vec()
    );
}

#[test]
fn invalid_prefix_for_custom_attribute() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
end entity;

architecture a of ent is
    attribute myattr : boolean;
    constant c1 : boolean := std'myattr;
begin
end architecture;
        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::mismatched_kinds(
            code.s1("std'myattr"),
            "library std may not be the prefix of a user defined attribute",
        )],
    );
}

#[test]
fn incorrect_entity_class() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
end entity;

architecture a of ent is
    attribute myattr : boolean;

    signal good, bad : natural;
    attribute myattr of good : signal is true;
    attribute myattr of bad : variable is true;
begin
end architecture;
        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::new(
            code.s("bad", 2),
            "signal 'bad' is not of class variable",
            ErrorCode::MismatchedEntityClass,
        )],
    );
}

#[test]
fn subtype_entity_class() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
end entity;

architecture a of ent is
    attribute myattr : boolean;

    subtype good is integer range 0 to 3;
    type bad is (alpha, beta);

    attribute myattr of good : subtype is true;
    attribute myattr of bad : subtype is true;
begin
end architecture;
        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::new(
            code.s("bad", 2),
            "type 'bad' is not of class subtype",
            ErrorCode::MismatchedEntityClass,
        )],
    );
}

#[test]
fn duplicate_attribute() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
end entity;

architecture a of ent is
    attribute myattr : boolean;
    signal mysig : natural;
    attribute myattr of mysig : signal is false;
    attribute myattr of mysig : signal is true;
begin
end architecture;
        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::new(
            code.s("mysig", 3),
            "Duplicate specification of attribute 'myattr' for signal 'mysig'",
            ErrorCode::Duplicate,
        )
        .related(code.s("mysig", 2), "Previously specified here")],
    );
}

#[test]
fn attributes_affect_aliased_object_and_not_alias_itself() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
end entity;

architecture a of ent is
    attribute myattr : boolean;
    
    signal mysig : natural;
    alias myalias is mysig;

    attribute myattr of myalias : signal is false;
    attribute myattr of mysig : signal is false;

    constant c0 : boolean := mysig'myattr;
    constant c1 : boolean := myalias'myattr;
begin
end architecture;
        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::new(
            code.s1("mysig : signal").s1("mysig"),
            "Duplicate specification of attribute 'myattr' for signal 'mysig'",
            ErrorCode::Duplicate,
        )
        .related(
            code.s1("myalias : signal").s1("myalias"),
            "Previously specified here",
        )],
    );
}
