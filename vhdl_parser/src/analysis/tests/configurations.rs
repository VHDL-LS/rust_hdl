// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

use super::*;

#[test]
fn error_on_configuration_before_entity_in_same_file() {
    let mut builder = LibraryBuilder::new();

    let code = builder.code(
        "libname",
        "
configuration cfg of ent is
for rtl
end for;
end configuration;

entity ent is
end entity;
",
    );

    check_diagnostics(
        builder.analyze(),
        vec![Diagnostic::error(
            code.s("cfg", 1),
            "Configuration 'cfg' declared before entity 'ent'",
        )],
    );
}

#[test]
fn error_on_configuration_of_missing_entity() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
configuration cfg of ent is
for rtl
end for;
end configuration;
",
    );

    check_diagnostics(
        builder.analyze(),
        vec![Diagnostic::error(
            code.s("ent", 1),
            "No declaration of 'ent'",
        )],
    );
}

#[test]
fn good_configurations() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
entity ent is
end entity;

configuration cfg_good1 of ent is
for rtl
end for;
end configuration;

configuration cfg_good2 of work.ent is
for rtl
end for;
end configuration;

library libname;
configuration cfg_good3 of libname.ent is
for rtl
end for;
end configuration;
",
    );

    check_no_diagnostics(&builder.analyze());
}

#[test]
fn error_on_configuration_of_entity_outside_of_library() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "lib2",
        "
entity ent is
end entity;",
    );
    let code = builder.code(
        "libname",
        "
library lib2;

configuration cfg of lib2.ent is
for rtl
end for;
end configuration;
",
    );

    check_diagnostics(
        builder.analyze(),
        vec![Diagnostic::error(
            code.s("lib2.ent", 1),
            "Configuration must be within the same library 'libname' as the corresponding entity",
        )],
    );
}
