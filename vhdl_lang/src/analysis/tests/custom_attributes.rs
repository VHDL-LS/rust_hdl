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

#[test]
fn attribute_specification_others_decorates_remaining_entities() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
entity ent is
end entity;

architecture a of ent is
    attribute myattr : string;
    signal first, second : natural;
    constant c : natural := 0;
    attribute myattr of first : signal is \"one\";
    attribute myattr of others : signal is \"other\";
    constant c1 : string := first'myattr;
    constant c2 : string := second'myattr;
begin
end architecture;
        ",
    );

    check_no_diagnostics(&builder.analyze());
}

#[test]
fn attribute_specification_all_decorates_every_entity_of_the_class() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
entity ent is
end entity;

architecture a of ent is
    attribute myattr : boolean;
    constant c1 : natural := 0;
    constant c2 : natural := 1;
    attribute myattr of all : constant is true;
    signal s3 : boolean := c1'myattr and c2'myattr;
begin
end architecture;
        ",
    );

    check_no_diagnostics(&builder.analyze());
}

#[test]
fn attribute_specification_all_and_others_only_affect_the_given_class() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
end entity;

architecture a of ent is
    attribute myattr : boolean;
    signal sig : natural;
    constant const : natural := 0;
    attribute myattr of others : signal is true;
    constant c1 : boolean := sig'myattr;
    constant c2 : boolean := const'myattr;
begin
end architecture;
        ",
    );

    check_diagnostics(
        builder.analyze(),
        vec![Diagnostic::new(
            code.s("myattr", 4),
            "Unknown attribute 'myattr",
            ErrorCode::Unresolved,
        )],
    );
}

#[test]
fn attribute_specification_all_reports_duplicate_specification() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
end entity;

architecture a of ent is
    attribute myattr : boolean;
    signal sig : natural;
    attribute myattr of sig : signal is true;
    attribute myattr of all : signal is false;
begin
end architecture;
        ",
    );

    check_diagnostics(
        builder.analyze(),
        vec![Diagnostic::new(
            code.s("myattr", 3),
            "Duplicate specification of attribute 'myattr' for signal 'sig'",
            ErrorCode::Duplicate,
        )
        .related(code.s("sig", 3), "Previously specified here")],
    );
}

#[test]
fn attribute_specification_all_rejects_later_declaration_of_the_class() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
end entity;

architecture a of ent is
    attribute myattr : boolean;
    attribute myattr of all : signal is true;
    signal z : bit;
    attribute myattr of z : signal is false;
begin
end architecture;
        ",
    );

    check_diagnostics(
        builder.analyze(),
        vec![Diagnostic::new(
            code.s1("z"),
            "signal 'z' declared after an attribute specification with 'all' or 'others' for class signal",
            ErrorCode::MisplacedAttributeSpec,
        )
        .related(code.s("myattr", 2), "Attribute specification is here")],
    );
}

#[test]
fn attribute_specification_others_rejects_later_declaration_of_the_class() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
end entity;

architecture a of ent is
    attribute myattr : boolean;
    signal x : bit;
    attribute myattr of others : signal is true;
    signal z : bit;
begin
end architecture;
        ",
    );

    check_diagnostics(
        builder.analyze(),
        vec![Diagnostic::new(
            code.s1("z"),
            "signal 'z' declared after an attribute specification with 'all' or 'others' for class signal",
            ErrorCode::MisplacedAttributeSpec,
        )
        .related(code.s("myattr", 2), "Attribute specification is here")],
    );
}

#[test]
fn attribute_specification_all_allows_later_declaration_of_other_classes() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
entity ent is
end entity;

architecture a of ent is
    attribute myattr : boolean;
    attribute myattr of all : signal is true;
    constant c : natural := 0;
    attribute myattr of c : constant is false;
begin
end architecture;
        ",
    );

    check_no_diagnostics(&builder.analyze());
}

#[test]
fn attribute_specification_all_allows_declaration_in_nested_region() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
entity ent is
end entity;

architecture a of ent is
    attribute myattr : boolean;
    attribute myattr of all : signal is true;
begin
    blk : block is
        signal z : bit;
    begin
    end block;
end architecture;
        ",
    );

    check_no_diagnostics(&builder.analyze());
}
