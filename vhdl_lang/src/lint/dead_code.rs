// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use crate::analysis::DesignRoot;
use crate::analysis::Library;
use crate::analysis::LockedUnit;
use crate::ast::search::Search;
use crate::ast::search::SearchState;
use crate::ast::search::Searcher;
use crate::ast::UnitId;
use crate::data::error_codes::ErrorCode;
use crate::data::DiagnosticHandler;
use crate::data::Symbol;
use crate::named_entity::{HasEntityId, Reference, Related};
use crate::syntax::TokenAccess;
use crate::AnyEntKind;
use crate::Config;
use crate::Design;
use crate::Diagnostic;
use crate::EntRef;
use crate::Overloaded;
use crate::SrcPos;
use fnv::FnvHashMap;
use fnv::FnvHashSet;
use itertools::Itertools;

struct DeadCodeSearcher<'a> {
    root: &'a DesignRoot,
    references: FnvHashSet<EntRef<'a>>,
    declarations: FnvHashSet<EntRef<'a>>,
}

impl<'a> DeadCodeSearcher<'a> {
    fn new(root: &'a DesignRoot) -> Self {
        DeadCodeSearcher {
            root,
            references: Default::default(),
            declarations: Default::default(),
        }
    }
}

impl Searcher for DeadCodeSearcher<'_> {
    fn search_pos_with_ref(
        &mut self,
        _ctx: &dyn TokenAccess,
        _: &SrcPos,
        reference: &Reference,
    ) -> SearchState {
        if let Some(id) = reference.get() {
            let ent = self.root.get_ent(id);
            self.references.insert(ent);

            if let Related::DeclaredBy(other) = ent.related {
                self.references.insert(other);
            }
        };
        SearchState::NotFinished
    }
    fn search_decl(
        &mut self,
        _ctx: &dyn TokenAccess,
        decl: crate::ast::search::FoundDeclaration<'_>,
    ) -> SearchState {
        if let Some(id) = decl.ent_id() {
            self.declarations.insert(self.root.get_ent(id));
        }
        SearchState::NotFinished
    }
}

fn search_unit(unit: &LockedUnit, searcher: &mut impl Searcher) {
    let _ = unit.unit.write().search(&unit.tokens, searcher);
}

fn is_package_header(ent: EntRef<'_>) -> bool {
    matches!(
        ent.kind(),
        AnyEntKind::Design(Design::Package(..)) | AnyEntKind::Design(Design::UninstPackage(..))
    )
}

fn is_interface(ent: EntRef<'_>) -> bool {
    matches!(
        ent.kind(),
        AnyEntKind::Object(o) if o.iface.is_some())
        || matches!(
            ent.kind(),
            AnyEntKind::Overloaded(Overloaded::InterfaceSubprogram(..))
        )
}

fn can_be_locally_unused(ent: EntRef<'_>) -> bool {
    if let Related::DeclaredBy(related) = ent.related {
        if !can_be_locally_unused(related) {
            return false;
        }
    }

    if matches!(ent.kind(), AnyEntKind::Design(_)) {
        return false;
    }

    // No labels
    if matches!(
        ent.kind(),
        AnyEntKind::Concurrent(_) | AnyEntKind::Sequential(_)
    ) {
        return false;
    }

    // No loop parameters
    if matches!(ent.kind(), AnyEntKind::LoopParameter(..)) {
        return false;
    }

    // Record elements can never be assumed to be unused, they can be created by (other => 0)
    if matches!(ent.kind(), AnyEntKind::ElementDeclaration(..)) {
        return false;
    }

    // Enum variants can never be assumed to be unused, they can be created by enum_t'val(0)
    if matches!(
        ent.kind(),
        AnyEntKind::Overloaded(Overloaded::EnumLiteral(..))
    ) {
        return false;
    }

    if let Some(parent) = ent.parent {
        // Everything in package header is public, except generics in an uninstantiated package
        if is_package_header(parent) && !is_interface(ent) {
            return false;
        }

        // Component ports are assumed to be needed
        if matches!(parent.kind(), AnyEntKind::Component(..)) {
            return false;
        }

        // Everything in protected types inside of package header
        if matches!(parent.kind(), AnyEntKind::Type(crate::Type::Protected(..))) {
            if let Some(grand_parent) = parent.parent {
                if is_package_header(grand_parent) {
                    return false;
                }
            }
        }

        // Formals inside a subprogram declaration
        if matches!(
            parent.kind(),
            AnyEntKind::Overloaded(Overloaded::SubprogramDecl(..))
        ) {
            return false;
        }
    }

    true
}

/// Find *local* unused declarations
fn find_unused_declarations<'a>(
    root: &'a DesignRoot,
    lib: &Library,
    primary_unit_name: &Symbol,
) -> FnvHashSet<EntRef<'a>> {
    let mut searcher = DeadCodeSearcher::new(root);

    if let Some(unit) = lib.primary_unit(primary_unit_name) {
        search_unit(unit, &mut searcher);
    }

    for unit in lib.secondary_units(primary_unit_name) {
        search_unit(unit, &mut searcher);
    }

    searcher
        .declarations
        .difference(&searcher.references)
        .filter(|ent| {
            if let Related::DeclaredBy(other) = ent.related {
                // If the subprogram header is found in the references
                // but not the body. The body is considered to be used and we filter it away
                !searcher.references.contains(other)
            } else {
                true
            }
        })
        .filter(|ent| can_be_locally_unused(ent))
        .copied()
        .collect()
}

/// Use a struct to keep state of units that do not need to be re-scanned
#[derive(Default)]
pub(crate) struct UnusedDeclarationsLinter {
    // library name, primary name
    diagnostics: FnvHashMap<(Symbol, Symbol), Vec<Diagnostic>>,
}

impl UnusedDeclarationsLinter {
    pub fn lint(
        &mut self,
        root: &DesignRoot,
        config: &Config,
        analyzed_units: &[UnitId],
        diagnostics: &mut dyn DiagnosticHandler,
    ) {
        // Prune diagnostics that need to be re-computed
        for unit in analyzed_units {
            let key = (unit.library_name().clone(), unit.primary_name().clone());
            self.diagnostics.remove(&key);
        }

        // Prune diagnostics for units that no longer exist
        self.diagnostics.retain(|(library_name, primary_name), _| {
            if let Some(library) = root.get_lib(library_name) {
                if library.primary_unit(primary_name).is_some() {
                    return true;
                }
            }
            false
        });

        for unit in analyzed_units {
            let key = (unit.library_name().clone(), unit.primary_name().clone());

            if let Some(library) = root.get_lib(unit.library_name()) {
                self.diagnostics.entry(key).or_insert_with(|| {
                    find_unused_declarations(root, library, unit.primary_name())
                        .into_iter()
                        .filter_map(|ent| {
                            Some(Diagnostic::new(
                                ent.decl_pos()?,
                                format!("Unused declaration of {}", ent.describe()),
                                ErrorCode::Unused,
                            ))
                        })
                        .collect_vec()
                });
            }
        }

        for ((library_name, _), unit_diagnostics) in self.diagnostics.iter() {
            if let Some(library_config) = config.get_library(&library_name.name_utf8()) {
                if !library_config.is_third_party {
                    diagnostics.append(unit_diagnostics.iter().cloned());
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analysis::tests::LibraryBuilder;
    use crate::syntax::test::check_no_diagnostics;
    use crate::syntax::test::Code;

    fn get_ent(root: &DesignRoot, code: Code) -> EntRef<'_> {
        root.search_reference(code.source(), code.start()).unwrap()
    }

    fn check_unused(got: FnvHashSet<EntRef<'_>>, expected: FnvHashSet<EntRef<'_>>) {
        fn fmt_ent(ent: EntRef<'_>) -> String {
            format!(
                "{}, line {}",
                ent.describe(),
                ent.decl_pos().unwrap().start().line
            )
        }

        let mut fail = false;
        for ent in expected.difference(&got) {
            println!("Expected {}", fmt_ent(ent));
            fail = true;
        }
        for ent in got.difference(&expected) {
            println!("Got unexpected {}", fmt_ent(ent));
            fail = true;
        }

        if fail {
            panic!("Check unused failed");
        }
    }

    #[test]
    fn unused_signal() {
        let mut builder = LibraryBuilder::new();

        let code = builder.code(
            "libname",
            "
entity ent is
end entity;

architecture a of ent is
  signal unused : boolean;
  signal used : boolean;
begin
   used <= true;
end architecture;",
        );

        let (root, diagnostics) = builder.get_analyzed_root();
        check_no_diagnostics(&diagnostics);

        let lib = root.get_lib(&root.symbol_utf8("libname")).unwrap();
        let ent = get_ent(&root, code.s1("unused"));

        check_unused(
            find_unused_declarations(&root, lib, &root.symbol_utf8("ent")),
            FnvHashSet::from_iter(vec![ent]),
        )
    }

    #[test]
    fn unused_ports() {
        let mut builder = LibraryBuilder::new();

        let code = builder.code(
            "libname",
            "
entity ent is
  port (
    used : out boolean;
    unused : out boolean
  );
end entity;

architecture a of ent is
begin
   used <= true;
end architecture;",
        );

        let (root, diagnostics) = builder.get_analyzed_root();
        check_no_diagnostics(&diagnostics);

        let lib = root.get_lib(&root.symbol_utf8("libname")).unwrap();
        let ent = get_ent(&root, code.s1("unused"));

        check_unused(
            find_unused_declarations(&root, lib, &root.symbol_utf8("ent")),
            FnvHashSet::from_iter(vec![ent]),
        )
    }

    /// Since the focus of the unused declaration lint is local declarations
    /// we have to assume that a package header declaration could be used somewhere else.
    #[test]
    fn package_headers_are_public_and_will_never_be_unused() {
        let mut builder = LibraryBuilder::new();

        let code = builder.code(
            "libname",
            "
package pkg is
  constant unknown : boolean := false;
end package;

package body pkg is
   constant unused : boolean := false;
end package body;
            ",
        );

        let (root, diagnostics) = builder.get_analyzed_root();
        check_no_diagnostics(&diagnostics);

        let lib = root.get_lib(&root.symbol_utf8("libname")).unwrap();
        let ent = get_ent(&root, code.s1("unused"));

        check_unused(
            find_unused_declarations(&root, lib, &root.symbol_utf8("pkg")),
            FnvHashSet::from_iter(vec![ent]),
        )
    }

    #[test]
    fn generic_package_headers_are_public_and_will_never_be_unused() {
        let mut builder = LibraryBuilder::new();

        let code = builder.code(
            "libname",
            "
package pkg is
  generic (arg : boolean; unused0 : natural);
  constant unknown : boolean := arg;
end package;

package body pkg is
   constant unused1 : boolean := false;
end package body;
            ",
        );

        let (root, diagnostics) = builder.get_analyzed_root();
        check_no_diagnostics(&diagnostics);

        let lib = root.get_lib(&root.symbol_utf8("libname")).unwrap();

        check_unused(
            find_unused_declarations(&root, lib, &root.symbol_utf8("pkg")),
            FnvHashSet::from_iter(vec![
                get_ent(&root, code.s1("unused0")),
                get_ent(&root, code.s1("unused1")),
            ]),
        )
    }

    // We can never assume enum variants are unused because they can be created by enum_t'val(0)
    #[test]
    fn enum_variants_are_never_unused() {
        let mut builder = LibraryBuilder::new();

        let code = builder.code(
            "libname",
            "
package pkg is
    type enum_pub_t is (v1, v2);
end package;

package body pkg is
    type enum_priv_t is (v3, v4);
end package body;
            ",
        );

        let (root, diagnostics) = builder.get_analyzed_root();
        check_no_diagnostics(&diagnostics);

        let lib = root.get_lib(&root.symbol_utf8("libname")).unwrap();

        check_unused(
            find_unused_declarations(&root, lib, &root.symbol_utf8("pkg")),
            FnvHashSet::from_iter(vec![get_ent(&root, code.s1("enum_priv_t"))]),
        )
    }

    // We can never assume record elements are unused because they can be created by (others => 0)
    #[test]
    fn record_elements_are_never_unused() {
        let mut builder = LibraryBuilder::new();

        let code = builder.code(
            "libname",
            "
package pkg is
    type rec_pub_t is record
       elem : natural;
    end record;
end package;

package body pkg is
    type rec_priv_t is record
        elem : natural;
    end record;
end package body;
            ",
        );

        let (root, diagnostics) = builder.get_analyzed_root();
        check_no_diagnostics(&diagnostics);

        let lib = root.get_lib(&root.symbol_utf8("libname")).unwrap();

        check_unused(
            find_unused_declarations(&root, lib, &root.symbol_utf8("pkg")),
            FnvHashSet::from_iter(vec![get_ent(&root, code.s1("rec_priv_t"))]),
        )
    }

    #[test]
    fn protected_type_methods_are_indirectly_public() {
        let mut builder = LibraryBuilder::new();

        builder.code(
            "libname",
            "
package pkg is
  type prot_t is protected
     procedure method;
  end protected;
end package;

package body pkg is
    type prot_t is protected body
      procedure method is
      begin
      end;
    end protected body;
end package body;
            ",
        );

        let (root, diagnostics) = builder.get_analyzed_root();
        check_no_diagnostics(&diagnostics);

        let lib = root.get_lib(&root.symbol_utf8("libname")).unwrap();

        check_unused(
            find_unused_declarations(&root, lib, &root.symbol_utf8("pkg")),
            FnvHashSet::default(),
        )
    }

    #[test]
    fn labels_are_not_unused() {
        let mut builder = LibraryBuilder::new();

        builder.code(
            "libname",
            "
entity ent2 is
end entity;

architecture a of ent2 is
begin
end architecture;

entity ent is
end entity;

architecture a of ent is
begin
   main : process
   begin
     l0: loop
     end loop;
   end process;

   inst : entity work.ent2;
end architecture;",
        );

        let (root, diagnostics) = builder.get_analyzed_root();
        check_no_diagnostics(&diagnostics);

        let lib = root.get_lib(&root.symbol_utf8("libname")).unwrap();
        check_unused(
            find_unused_declarations(&root, lib, &root.symbol_utf8("ent")),
            FnvHashSet::default(),
        )
    }

    #[test]
    fn loop_parameters_are_not_unused() {
        let mut builder = LibraryBuilder::new();

        builder.code(
            "libname",
            "
entity ent is
end entity;

architecture a of ent is
begin
   main : process
   begin
      for i in 0 to 3 loop
      end loop;
   end process;

   gen: for j in 0 to 3 generate
   end generate;

end architecture;",
        );

        let (root, diagnostics) = builder.get_analyzed_root();
        check_no_diagnostics(&diagnostics);

        let lib = root.get_lib(&root.symbol_utf8("libname")).unwrap();
        check_unused(
            find_unused_declarations(&root, lib, &root.symbol_utf8("ent")),
            FnvHashSet::default(),
        )
    }

    #[test]
    fn subprogram_declaration_arguments_are_not_unused() {
        let mut builder = LibraryBuilder::new();

        builder.code(
            "libname",
            "
package pkg is
  procedure myproc(arg : natural);
end package;

package body pkg is
    procedure myproc(arg : natural) is
    begin
       report to_string(arg);
    end;
end package body;
            ",
        );

        let (root, diagnostics) = builder.get_analyzed_root();
        check_no_diagnostics(&diagnostics);

        let lib = root.get_lib(&root.symbol_utf8("libname")).unwrap();

        check_unused(
            find_unused_declarations(&root, lib, &root.symbol_utf8("pkg")),
            FnvHashSet::default(),
        )
    }

    #[test]
    fn subprogram_use_between_declaration_and_definition() {
        let mut builder = LibraryBuilder::new();

        let code = builder.code(
            "libname",
            "
package pkg is
    procedure pub_proc;
end package;

package body pkg is

   procedure priv_proc;

   procedure pub_proc is
   begin
     priv_proc;
   end procedure;

   procedure priv_proc is
   begin
   end;

   procedure unused;
   procedure unused is
   begin
   end;

end package body;
            ",
        );

        let (root, diagnostics) = builder.get_analyzed_root();
        check_no_diagnostics(&diagnostics);

        let lib = root.get_lib(&root.symbol_utf8("libname")).unwrap();

        check_unused(
            find_unused_declarations(&root, lib, &root.symbol_utf8("pkg")),
            FnvHashSet::from_iter(vec![
                get_ent(&root, code.s("unused", 1)),
                get_ent(&root, code.s("unused", 2)),
            ]),
        )
    }

    #[test]
    fn component_interface_is_not_unused() {
        let mut builder = LibraryBuilder::new();

        builder.code(
            "libname",
            "
package pkg is
  component comp is
    generic (g : natural);
    port (p : natural);
  end component;
end package;
            ",
        );

        let (root, diagnostics) = builder.get_analyzed_root();
        check_no_diagnostics(&diagnostics);

        let lib = root.get_lib(&root.symbol_utf8("libname")).unwrap();

        check_unused(
            find_unused_declarations(&root, lib, &root.symbol_utf8("pkg")),
            FnvHashSet::default(),
        )
    }

    #[test]
    fn protected_type_methods() {
        let mut builder = LibraryBuilder::new();

        let code = builder.code(
            "libname",
            "
package pkg is
    procedure public;
end package;

package body pkg is
    type prot_t is protected
        procedure method;

        procedure unused;
    end protected;

    type prot_t is protected body
        procedure method is
        begin
        end;
        
        procedure unused is
        begin
        end;
    end protected body;

    procedure public is
       variable p : prot_t;
    begin
       p.method;
    end;
end package body;
            ",
        );

        let (root, diagnostics) = builder.get_analyzed_root();
        check_no_diagnostics(&diagnostics);

        let lib = root.get_lib(&root.symbol_utf8("libname")).unwrap();

        check_unused(
            find_unused_declarations(&root, lib, &root.symbol_utf8("pkg")),
            FnvHashSet::from_iter(vec![
                get_ent(&root, code.s("unused", 1)),
                get_ent(&root, code.s("unused", 2)),
            ]),
        )
    }

    #[test]
    fn end_label_is_not_valid_use() {
        let mut builder = LibraryBuilder::new();

        let code = builder.code(
            "libname",
            "
package pkg is
end package;

package body pkg is
    procedure unused is
    begin
    end unused;
end package body;
            ",
        );

        let (root, diagnostics) = builder.get_analyzed_root();
        check_no_diagnostics(&diagnostics);

        let lib = root.get_lib(&root.symbol_utf8("libname")).unwrap();

        check_unused(
            find_unused_declarations(&root, lib, &root.symbol_utf8("pkg")),
            FnvHashSet::from_iter(vec![get_ent(&root, code.s1("unused"))]),
        )
    }
}
