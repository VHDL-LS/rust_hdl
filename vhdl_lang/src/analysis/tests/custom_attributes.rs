// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

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
        vec![Diagnostic::error(
            code.s("bad", 2),
            "Attribute specification must be in the immediate declarative part",
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
    attribute myattr of myent : signal is true;
begin
end architecture;
        ",
    );

    let diagnostics = builder.analyze();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s("myent", 4),
            "Attribute specification must be in the immediate declarative part",
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
        vec![Diagnostic::error(
            code.s("bad", 2),
            "Attribute specification must be in the immediate declarative part",
        )],
    );
}
