//! This Source Code Form is subject to the terms of the Mozilla Public
//! License, v. 2.0. If a copy of the MPL was not distributed with this file,
//! You can obtain one at http://mozilla.org/MPL/2.0/.
//!
//! Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use super::*;
use crate::ast::search::FindAllEnt;
use crate::ast::Designator;
use pretty_assertions::assert_eq;

#[test]
fn entity_architecture() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
entity ent is
  generic (
    g0 : natural
  );
  port (
    p0 : bit
  );
end entity;

architecture a of ent is 
  signal s0 : natural;
begin
  process
    variable v0 : natural;
  begin
    loop0: loop
    end loop;

    if false then
    end if;
  end process;
end architecture;
      ",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);
    assert_eq!(
        get_hierarchy(&root, "libname"),
        vec![
            "libname.ent",
            "libname.ent.g0",
            "libname.ent.p0",
            "libname.ent.a",
            "libname.ent.a.s0",
            // Process is anonymous
            "libname.ent.a..v0",
            "libname.ent.a..loop0",
        ]
    );
}

#[test]
fn package() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
package pkg is
   function fun0(arg : natural) return natural;
end package;


package body pkg is
    function fun0(arg : natural) return natural is
        variable v0 : natural;
    begin
    end function;
end package body;
      ",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);
    assert_eq!(
        get_hierarchy(&root, "libname"),
        vec![
            "libname.pkg",
            "libname.pkg.fun0",
            "libname.pkg.fun0.arg",
            "libname.pkg.fun0",
            "libname.pkg.fun0.arg",
            "libname.pkg.fun0.v0",
        ]
    );
}

#[test]
fn genric_package() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
package pkg is
   generic (
     type type_t;
     value: type_t
   );

   constant c0 : type_t := value;
   function fun0(arg: type_t) return boolean;
end package;

package body pkg is
    function fun0(arg: type_t) return boolean is
    begin
        return arg = value;
    end function;
end package body;

package ipkg is new work.pkg generic map(type_t => integer, value => 0);
      ",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);
    assert_eq!(
        get_hierarchy(&root, "libname"),
        vec![
            "libname.pkg",
            "libname.pkg.type_t",
            "libname.pkg.value",
            "libname.pkg.c0",
            "libname.pkg.fun0",
            "libname.pkg.fun0.arg",
            "libname.pkg.fun0",
            "libname.pkg.fun0.arg",
            "libname.ipkg",
        ]
    );

    let ipkg = root
        .search_reference(code.source(), code.s1("ipkg").start())
        .unwrap();

    let instances: Vec<String> =
        if let AnyEntKind::Design(Design::PackageInstance(region)) = ipkg.kind() {
            region
                .immediates()
                .into_iter()
                .map(|ent| ent.path_name())
                .collect()
        } else {
            panic!("Expected instantiated package");
        };

    assert_eq!(instances, vec!["libname.ipkg.c0", "libname.ipkg.fun0"]);
}

fn get_hierarchy(root: &DesignRoot, libname: &str) -> Vec<String> {
    let mut searcher = FindAllEnt::new(root, |ent| {
        matches!(ent.designator(), Designator::Identifier(_))
    });

    let _ = root.search_library(&root.symbol_utf8(libname), &mut searcher);
    searcher
        .result
        .into_iter()
        .map(|ent| ent.path_name())
        .collect()
}

#[test]
fn find_implementation_of_entity_vs_component() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent0 is
end entity;

architecture a of ent0 is
begin
end architecture;

entity ent1 is
end entity;

architecture a of ent1 is
  component ent0 is
  end component;
begin
  inst: ent0;
end architecture;

      ",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);

    let ent = root
        .search_reference(code.source(), code.s1("ent0").start())
        .unwrap();
    let comp = root
        .search_reference(code.source(), code.sa("component ", "ent0").start())
        .unwrap();

    assert_eq!(root.find_implementation(ent), vec![comp]);
    assert_eq!(root.find_implementation(comp), vec![ent]);
}

#[test]
fn exit_and_next_outside_of_loop() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
end entity;

architecture a of ent is
begin
  process
  begin
    exit;
    next;

    loop
        exit;
    end loop;

    loop
        next;
    end loop;
  end process;
end architecture;
      ",
    );

    let (_, diagnostics) = builder.get_analyzed_root();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(code.s1("exit;"), "Exit can only be used inside a loop"),
            Diagnostic::error(code.s1("next;"), "Next can only be used inside a loop"),
        ],
    );
}

#[test]
fn exit_and_next_label_outside_of_loop() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
        "libname",
        "
entity ent is
end entity;

architecture a of ent is
begin
  main: process
  begin
    good0: loop
        good1: loop
            exit good0;
        end loop;
    end loop;

    bad0: loop
        exit;
    end loop;

    l1: loop
        exit bad0;
    end loop;

    l0: loop
        next bad0; 
    end loop;
  end process;
end architecture;
      ",
    );

    let (_, diagnostics) = builder.get_analyzed_root();
    check_diagnostics(
        diagnostics,
        vec![
            Diagnostic::error(
                code.sa("exit ", "bad0"),
                "Cannot be used outside of loop 'bad0'",
            ),
            Diagnostic::error(
                code.sa("next ", "bad0"),
                "Cannot be used outside of loop 'bad0'",
            ),
        ],
    );
}
