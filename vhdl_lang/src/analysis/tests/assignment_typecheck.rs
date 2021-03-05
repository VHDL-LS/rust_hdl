// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

use super::*;

#[test]
fn overloaded_name_may_not_be_assignment_target() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
end entity;

architecture a of ent is
    function foo1 return natural is
    begin
        return 0;
    end;

    type enum_t is (foo2, enum_value);
begin
  main : process
  begin
    foo1 := 1;
    foo2 := 1;
  end process;
end architecture;
",
    );

    let expected = vec![
        Diagnostic::error(code.s("foo1", 2), "not a valid assignment target"),
        Diagnostic::error(code.s("foo2", 2), "not a valid assignment target"),
    ];

    let diagnostics = builder.analyze();
    check_diagnostics(diagnostics, expected);
}
