//! This Source Code Form is subject to the terms of the Mozilla Public
//! License, v. 2.0. If a copy of the MPL was not distributed with this file,
//! You can obtain one at http://mozilla.org/MPL/2.0/.
//!
//! Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use super::*;

#[test]
fn must_be_object_name() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
end package;

package body pkg is
  procedure proc(signal good : in bit) is
  begin
    wait on good;
    wait on proc;
  end;
end package body;

        ",
    );
    let (_, diagnostics) = builder.get_analyzed_root();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s1("wait on proc").s1("proc"),
            "procedure proc[BIT] is not a signal and cannot be in a sensitivity list",
        )],
    )
}

#[test]
fn must_be_signal_name() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
end package;

package body pkg is
  constant c0 : bit := '0';
  procedure proc(signal good : in bit) is
  begin
    wait on good;
    wait on c0;
  end;
end package body;

        ",
    );
    let (_, diagnostics) = builder.get_analyzed_root();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s1("wait on c0").s1("c0"),
            "constant 'c0' is not a signal and cannot be in a sensitivity list",
        )],
    )
}

#[test]
fn must_not_be_output() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
end package;

package body pkg is
  procedure proc(signal bad : out bit) is
  begin
    wait on bad;
  end;
end package body;

        ",
    );
    let (_, diagnostics) = builder.get_analyzed_root();
    check_diagnostics(
        diagnostics,
        vec![Diagnostic::error(
            code.s1("wait on bad").s1("bad"),
            "interface signal 'bad' of mode out cannot be in a sensitivity list",
        )],
    )
}

#[test]
fn may_be_output_port() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
entity ent is
  port (oport : out bit);
end entity;

architecture a of ent is
begin
  main: process (oport)
  begin
  end process main;
end architecture;

        ",
    );
    let (_, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);
}
