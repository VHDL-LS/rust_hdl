//! This Source Code Form is subject to the terms of the Mozilla Public
//! License, v. 2.0. If a copy of the MPL was not distributed with this file,
//! You can obtain one at http://mozilla.org/MPL/2.0/.
//!
//! Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use super::*;
use crate::data::error_codes::ErrorCode;
use crate::EntHierarchy;
use crate::Source;
use pretty_assertions::assert_eq;

#[test]
fn entity_architecture() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
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
  block
    begin
    process
        variable v0 : natural;
    begin
        loop0: loop
        end loop;

        if false then
        end if;
    end process;
  end block;
end architecture;
      ",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);
    assert_eq!(
        get_hierarchy(&root, "libname", code.source()),
        vec![
            nested("ent", vec![single("g0"), single("p0"),]),
            nested(
                "a",
                vec![
                    single("s0"),
                    nested(
                        "block",
                        vec![nested(
                            "process",
                            vec![single("v0"), single("loop0"), single("if statement")]
                        )]
                    ),
                ]
            )
        ]
    );
}

#[test]
fn package() {
    let mut builder = LibraryBuilder::new();
    let code = builder.code(
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
        get_hierarchy(&root, "libname", code.source()),
        vec![
            nested("pkg", vec![nested("fun0", vec![single("arg"),]),]),
            nested(
                "pkg",
                vec![nested("fun0", vec![single("arg"), single("v0")]),]
            )
        ]
    );
}

#[test]
fn generic_package() {
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
        get_hierarchy(&root, "libname", code.source()),
        vec![
            nested(
                "pkg",
                vec![
                    single("type_t"),
                    single("value"),
                    single("c0"),
                    nested("fun0", vec![single("arg"),]),
                ]
            ),
            nested("pkg", vec![nested("fun0", vec![single("arg")]),]),
            single("ipkg"),
        ]
    );

    let ipkg = root
        .search_reference(code.source(), code.s1("ipkg").start())
        .unwrap();

    let instances: Vec<_> = if let AnyEntKind::Design(Design::PackageInstance(region)) = ipkg.kind()
    {
        let mut symbols: Vec<_> = region.immediates().collect();
        symbols.sort_by_key(|ent| ent.decl_pos());
        symbols.into_iter().map(|ent| ent.path_name()).collect()
    } else {
        panic!("Expected instantiated package");
    };

    assert_eq!(instances, vec!["libname.ipkg.c0", "libname.ipkg.fun0"]);
}

#[test]
fn public_symbols() {
    let mut builder = LibraryBuilder::new();
    builder.code(
        "libname",
        "
package pkg is
   type type_t is (alpha, beta);
   constant const0 : type_t := alpha;
   function fun0(arg: type_t) return boolean;
   function \"+\"(arg: type_t) return boolean;

   type prot_t is protected
       procedure proc0(arg: type_t);
   end protected;
end package;

package body pkg is
    type prot_t is protected body
        procedure proc0(arg: type_t) is
        begin
        end;
    end protected body;
end package body;

entity ent is
  generic (
    g0 : natural
  );
  port (
    p0 : natural
  );
end entity;

architecture a of ent is
  signal not_public : bit;
begin
  main: process
  begin
  end process;
end architecture;
      ",
    );

    let (root, diagnostics) = builder.get_analyzed_root();
    check_no_diagnostics(&diagnostics);
    let mut symbols: Vec<_> = root
        .public_symbols()
        .filter(|ent| ent.library_name() == Some(&root.symbol_utf8("libname")))
        .collect();
    symbols.sort_by_key(|ent| ent.decl_pos());

    assert_eq!(
        symbols
            .into_iter()
            .map(|ent| ent.path_name())
            .collect::<Vec<_>>(),
        vec![
            "libname",
            "libname.pkg",
            "libname.pkg.type_t",
            "libname.pkg.type_t.alpha",
            "libname.pkg.type_t.beta",
            "libname.pkg.const0",
            "libname.pkg.fun0",
            "libname.pkg.\"+\"",
            "libname.pkg.prot_t",
            "libname.pkg.prot_t.proc0",
            "libname.pkg",
            "libname.ent",
            "libname.ent.g0",
            "libname.ent.p0",
            "libname.ent.a",
        ]
    );
}

#[derive(PartialEq, Debug)]
struct NameHierarchy {
    name: String,
    children: Vec<NameHierarchy>,
}

/// For compact test data creation
fn nested(name: &str, children: Vec<NameHierarchy>) -> NameHierarchy {
    NameHierarchy {
        name: name.to_owned(),
        children,
    }
}
/// For compact test data creation
fn single(name: &str) -> NameHierarchy {
    NameHierarchy {
        name: name.to_owned(),
        children: Vec::new(),
    }
}

impl From<EntHierarchy<'_>> for NameHierarchy {
    fn from(ent: EntHierarchy<'_>) -> Self {
        NameHierarchy {
            name: if matches!(ent.ent.designator(), Designator::Anonymous(_)) {
                ent.ent.kind().describe().to_string()
            } else {
                ent.ent.designator().to_string()
            },
            children: ent.children.into_iter().map(NameHierarchy::from).collect(),
        }
    }
}

fn get_hierarchy(root: &DesignRoot, libname: &str, source: &Source) -> Vec<NameHierarchy> {
    root.document_symbols(&root.symbol_utf8(libname), source)
        .into_iter()
        .map(|item| item.0)
        .map(NameHierarchy::from)
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
    let arch = root
        .search_reference(code.source(), code.s1("a ").start())
        .unwrap();
    let comp = root
        .search_reference(code.source(), code.sa("component ", "ent0").start())
        .unwrap();

    assert_eq!(root.find_implementation(ent), vec![arch, comp]);
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
            Diagnostic::new(
                code.s1("exit;"),
                "Exit can only be used inside a loop",
                ErrorCode::ExitOutsideLoop,
            ),
            Diagnostic::new(
                code.s1("next;"),
                "Next can only be used inside a loop",
                ErrorCode::NextOutsideLoop,
            ),
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
            Diagnostic::new(
                code.sa("exit ", "bad0"),
                "Cannot be used outside of loop 'bad0'",
                ErrorCode::InvalidLoopLabel,
            ),
            Diagnostic::new(
                code.sa("next ", "bad0"),
                "Cannot be used outside of loop 'bad0'",
                ErrorCode::InvalidLoopLabel,
            ),
        ],
    );
}
