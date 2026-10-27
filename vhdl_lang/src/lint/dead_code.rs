// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use crate::analysis::DesignRoot;
use crate::analysis::Library;
use crate::analysis::LockedUnit;
use crate::ast::search::search_pos_conditional_expr;
use crate::ast::search::search_pos_name;
use crate::ast::search::search_pos_name_target;
use crate::ast::search::Search;
use crate::ast::search::SearchResult;
use crate::ast::search::SearchState;
use crate::ast::search::Searcher;
use crate::ast::ActualPart;
use crate::ast::AssociationElement;
use crate::ast::ConditionalExpression;
use crate::ast::Expression;
use crate::ast::Mode;
use crate::ast::UnitId;
use crate::data::error_codes::ErrorCode;
use crate::data::DiagnosticHandler;
use crate::data::Symbol;
use crate::named_entity::{
    HasEntityId, InterfaceMode, ObjectEnt, ObjectInterface, Reference, Related,
};
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
    /// Entities whose value is read (used in an expression, sensitivity list, etc.)
    reads: FnvHashSet<EntRef<'a>>,
    /// Entities whose value is written (assignment target)
    writes: FnvHashSet<EntRef<'a>>,
}

impl<'a> DeadCodeSearcher<'a> {
    fn new(root: &'a DesignRoot) -> Self {
        DeadCodeSearcher {
            root,
            references: Default::default(),
            declarations: Default::default(),
            reads: Default::default(),
            writes: Default::default(),
        }
    }

    fn add_reference(&mut self, ent: EntRef<'a>, is_write: bool) {
        self.references.insert(ent);
        if is_write {
            self.writes.insert(ent);
        } else {
            self.reads.insert(ent);
        }

        if let Related::DeclaredBy(other) = ent.related {
            self.references.insert(other);
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
            self.add_reference(ent, false);
        };
        SearchState::NotFinished
    }

    fn search_target_ref(
        &mut self,
        _ctx: &dyn TokenAccess,
        _: &SrcPos,
        reference: &Reference,
    ) -> SearchState {
        if let Some(id) = reference.get() {
            let ent = self.root.get_ent(id);
            self.add_reference(ent, true);
        };
        SearchState::NotFinished
    }

    fn search_association(
        &mut self,
        ctx: &dyn TokenAccess,
        assoc: &AssociationElement,
    ) -> SearchState {
        // Determine whether the formal is an out/buffer/inout port.
        // If so, the actual signal is written (driven) by the connected
        // component through that port, and we must track it as a write
        // to avoid false "never assigned" (Uninitialized) diagnostics.
        let write_kind = assoc
            .formal
            .as_ref()
            .and_then(|formal| formal.item.get_suffix_reference())
            .map(|id| self.root.get_ent(id))
            .and_then(ObjectEnt::from_any)
            .and_then(|obj| {
                if let Some(ObjectInterface::Port(mode)) = &obj.object().iface {
                    match mode {
                        InterfaceMode::Simple(Mode::Out) | InterfaceMode::Simple(Mode::Buffer) => {
                            Some(false)
                        } // write-only
                        InterfaceMode::Simple(Mode::InOut) => Some(true), // read + write
                        _ => None,
                    }
                } else {
                    None
                }
            });

        let Some(is_inout) = write_kind else {
            // Not an out/buffer/inout port — let the default search handle it.
            return SearchState::NotFinished;
        };

        // Search the formal normally (counts as a reference to the port).
        if let Some(formal) = &assoc.formal {
            let _ = search_pos_name(&formal.pos(ctx), &formal.item, self, ctx);
        }

        // Search the actual.
        if let ActualPart::Expression(expr) = &assoc.actual.item {
            if is_inout {
                // inout port: the actual is both read and written.
                let _ = search_pos_conditional_expr(ctx, &assoc.actual.pos(ctx), expr, self);
                if let ConditionalExpression::Simple(Expression::Name(name)) = expr {
                    let _ = search_pos_name_target(&assoc.actual.pos(ctx), name, self, ctx);
                }
            } else {
                // out/buffer port: the actual is written (driven by the component).
                if let ConditionalExpression::Simple(Expression::Name(name)) = expr {
                    let _ = search_pos_name_target(&assoc.actual.pos(ctx), name, self, ctx);
                } else {
                    // Fallback for non-name actuals: search normally.
                    let _ = search_pos_conditional_expr(ctx, &assoc.actual.pos(ctx), expr, self);
                }
            }
        }

        SearchState::Finished(SearchResult::NotFound)
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
        AnyEntKind::Concurrent(..) | AnyEntKind::Sequential(_)
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

/// The kind of unused / partially-used issue detected for a declaration.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum UnusedKind {
    /// The declaration is never referenced at all.
    Unused,
    /// The object is assigned (written) but its value is never read.
    NeverRead,
    /// The object is read but never assigned or initialized in its declaration.
    Uninitialized,
}

impl UnusedKind {
    fn error_code(self) -> ErrorCode {
        match self {
            UnusedKind::Unused => ErrorCode::Unused,
            UnusedKind::NeverRead => ErrorCode::NeverRead,
            UnusedKind::Uninitialized => ErrorCode::Uninitialized,
        }
    }

    fn message(self, ent: EntRef<'_>) -> String {
        let describe = ent.describe();
        match self {
            UnusedKind::Unused => format!("Unused declaration of {describe}"),
            UnusedKind::NeverRead => format!("{describe} is assigned but never read"),
            UnusedKind::Uninitialized => format!("{describe} is never assigned"),
        }
    }
}

/// Returns `true` if the entity is an object (signal, variable, port, generic)
/// that should be checked for read/write analysis.
///
/// Constants are excluded (they always have a value).
/// Files are excluded.
/// Objects that are part of a public interface (package header, component)
/// are excluded – except for entity ports, which are checked for the
/// `Uninitialized` case (an output port that is never driven is a likely bug).
fn can_be_read_write_checked(ent: EntRef<'_>) -> bool {
    let Some(obj) = ObjectEnt::from_any(ent) else {
        return false;
    };

    // Constants always have a value, only the `Unused` check applies.
    if obj.is_constant() {
        return false;
    }

    if let Some(parent) = ent.parent {
        // Everything in a package header is public (except interface objects
        // of an uninstantiated package, which are still checked for `Unused`
        // but not for read/write since generics cannot be written).
        if is_package_header(parent) && !is_interface(ent) {
            return false;
        }

        // Component ports are assumed to be needed.
        if matches!(parent.kind(), AnyEntKind::Component(..)) {
            return false;
        }

        // Formals inside a subprogram declaration are part of the public
        // interface of that subprogram.
        if matches!(
            parent.kind(),
            AnyEntKind::Overloaded(Overloaded::SubprogramDecl(..))
        ) {
            return false;
        }
    }

    true
}

/// Classify the read/write usage of an object entity.
///
/// Returns `Some(kind)` if the object has a read/write issue, `None` otherwise.
/// The entity must already be known to be referenced (otherwise it would be
/// a plain `Unused` case).
fn classify_object_usage(
    ent: EntRef<'_>,
    reads: &FnvHashSet<EntRef<'_>>,
    writes: &FnvHashSet<EntRef<'_>>,
) -> Option<UnusedKind> {
    let obj = ObjectEnt::from_any(ent)?;
    let is_read = reads.contains(&ent);
    let is_written = writes.contains(&ent);
    let has_default = obj.object().has_default;

    match &obj.object().iface {
        Some(crate::named_entity::ObjectInterface::Port(mode)) => {
            // Ports: only check `Uninitialized` for output-like ports.
            // An `out`/`buffer` port that is never assigned drives an
            // uninitialized value. An `inout` port that is read but never
            // written may read an uninitialized value.
            // `in` ports are driven externally and are not checked here.
            let check_uninitialized = match mode {
                InterfaceMode::Simple(Mode::Out) | InterfaceMode::Simple(Mode::Buffer) => {
                    !is_written && !has_default
                }
                InterfaceMode::Simple(Mode::InOut) => is_read && !is_written && !has_default,
                _ => false,
            };
            if check_uninitialized {
                return Some(UnusedKind::Uninitialized);
            }
        }
        Some(crate::named_entity::ObjectInterface::Generic) => {
            // Generics are assigned externally at instantiation.
            // Only the `Unused` check applies (handled elsewhere).
        }
        Some(crate::named_entity::ObjectInterface::Parameter(_)) => {
            // Subprogram parameters: out/inout parameters that are never
            // written could be a bug, but this is more nuanced.
            // Skip for now.
        }
        None => {
            // Local object (signal, variable, shared variable).
            if is_written && !is_read {
                return Some(UnusedKind::NeverRead);
            }
            if is_read && !is_written && !has_default {
                return Some(UnusedKind::Uninitialized);
            }
        }
    }

    None
}

/// An unused or partially-used declaration together with its classification.
struct UnusedDeclaration<'a> {
    ent: EntRef<'a>,
    kind: UnusedKind,
}

/// Find *local* unused and partially-used declarations.
fn find_unused_issues<'a>(
    root: &'a DesignRoot,
    lib: &Library,
    primary_unit_name: &Symbol,
) -> Vec<UnusedDeclaration<'a>> {
    let mut searcher = DeadCodeSearcher::new(root);

    if let Some(unit) = lib.primary_unit(primary_unit_name) {
        search_unit(unit, &mut searcher);
    }

    for unit in lib.secondary_units(primary_unit_name) {
        search_unit(unit, &mut searcher);
    }

    let mut results = Vec::new();
    // Track which entities have already been flagged to avoid duplicates.
    let mut flagged: FnvHashSet<EntRef<'a>> = FnvHashSet::default();

    // 1. Output ports (out/buffer) that are never assigned and have no default
    //    → Uninitialized. This is checked first because it is more specific
    //    than a plain "unused" and indicates a likely bug (the output is X).
    //    Inout ports that are read but never written are also checked.
    for &ent in &searcher.declarations {
        if let Some(obj) = ObjectEnt::from_any(ent) {
            if let Some(crate::named_entity::ObjectInterface::Port(mode)) = &obj.object().iface {
                let check_uninitialized = match mode {
                    InterfaceMode::Simple(Mode::Out) | InterfaceMode::Simple(Mode::Buffer) => {
                        !searcher.writes.contains(&ent) && !obj.object().has_default
                    }
                    InterfaceMode::Simple(Mode::InOut) => {
                        searcher.reads.contains(&ent)
                            && !searcher.writes.contains(&ent)
                            && !obj.object().has_default
                    }
                    _ => false,
                };
                if check_uninitialized && can_be_read_write_checked(ent) {
                    results.push(UnusedDeclaration {
                        ent,
                        kind: UnusedKind::Uninitialized,
                    });
                    flagged.insert(ent);
                }
            }
        }
    }

    // 2. Never referenced at all → Unused
    for ent in searcher.declarations.difference(&searcher.references) {
        let ent = *ent;
        if flagged.contains(&ent) {
            continue;
        }
        let passes_filter = if let Related::DeclaredBy(other) = ent.related {
            // If the subprogram header is found in the references
            // but not the body. The body is considered to be used and we filter it away
            !searcher.references.contains(&other)
        } else {
            true
        };
        if passes_filter && can_be_locally_unused(ent) {
            results.push(UnusedDeclaration {
                ent,
                kind: UnusedKind::Unused,
            });
            flagged.insert(ent);
        }
    }

    // 3. Referenced local objects with read/write issues → NeverRead / Uninitialized
    for &ent in &searcher.declarations {
        if flagged.contains(&ent) {
            continue;
        }
        if searcher.references.contains(&ent) && can_be_read_write_checked(ent) {
            if let Some(kind) = classify_object_usage(ent, &searcher.reads, &searcher.writes) {
                results.push(UnusedDeclaration { ent, kind });
            }
        }
    }

    results
}

/// Find *local* unused declarations (never referenced at all).
#[cfg(test)]
fn find_unused_declarations<'a>(
    root: &'a DesignRoot,
    lib: &Library,
    primary_unit_name: &Symbol,
) -> FnvHashSet<EntRef<'a>> {
    find_unused_issues(root, lib, primary_unit_name)
        .into_iter()
        .filter(|issue| issue.kind == UnusedKind::Unused)
        .map(|issue| issue.ent)
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
                    find_unused_issues(root, library, unit.primary_name())
                        .into_iter()
                        .filter_map(|issue| {
                            Some(Diagnostic::new(
                                issue.ent.decl_pos()?,
                                issue.kind.message(issue.ent),
                                issue.kind.error_code(),
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

        // Use `in` ports here: an `out` port that is never written would be
        // classified as `Uninitialized` (see `out_port_never_assigned` test),
        // while an `in` port that is never referenced is simply `Unused`.
        let code = builder.code(
            "libname",
            "
entity ent is
  port (
    used : in boolean;
    unused : in boolean;
    o : out boolean
  );
end entity;

architecture a of ent is
begin
   o <= used;
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

    /// Helper: check that `find_unused_issues` returns exactly the expected
    /// (entity, kind) pairs.
    fn check_issues(got: &[UnusedDeclaration<'_>], expected: &[(EntRef<'_>, UnusedKind)]) {
        fn fmt_issue(ent: EntRef<'_>, kind: UnusedKind) -> String {
            format!(
                "{:?} of {}, line {}",
                kind,
                ent.describe(),
                ent.decl_pos().unwrap().start().line
            )
        }

        let mut fail = false;
        for (ent, kind) in expected {
            if !got
                .iter()
                .any(|issue| issue.ent == *ent && issue.kind == *kind)
            {
                println!("Expected {}", fmt_issue(*ent, *kind));
                fail = true;
            }
        }
        for issue in got {
            if !expected
                .iter()
                .any(|(ent, kind)| *ent == issue.ent && *kind == issue.kind)
            {
                println!("Got unexpected {}", fmt_issue(issue.ent, issue.kind));
                fail = true;
            }
        }

        if fail {
            panic!("Check issues failed");
        }
    }

    fn find_issues<'a>(
        root: &'a DesignRoot,
        lib: &Library,
        primary_name: &str,
    ) -> Vec<UnusedDeclaration<'a>> {
        find_unused_issues(root, lib, &root.symbol_utf8(primary_name))
    }

    /// Signal that is assigned but never read → NeverRead
    #[test]
    fn signal_assigned_but_never_read() {
        let mut builder = LibraryBuilder::new();

        let code = builder.code(
            "libname",
            "
entity ent is
  port (
    o : out boolean
  );
end entity;

architecture a of ent is
  signal only_written : boolean;
  signal properly_used : boolean;
begin
   only_written <= true;
   properly_used <= true;
   o <= properly_used;
end architecture;",
        );

        let (root, diagnostics) = builder.get_analyzed_root();
        check_no_diagnostics(&diagnostics);

        let lib = root.get_lib(&root.symbol_utf8("libname")).unwrap();
        let issues = find_issues(&root, lib, "ent");

        // only_written is assigned but never read → NeverRead
        // properly_used is both assigned and read → no issue
        check_issues(
            &issues,
            &[(
                get_ent(&root, code.s1("only_written")),
                UnusedKind::NeverRead,
            )],
        );
    }

    /// Signal that is read but never assigned (no init) → Uninitialized
    #[test]
    fn signal_read_but_never_assigned() {
        let mut builder = LibraryBuilder::new();

        let code = builder.code(
            "libname",
            "
entity ent is
  port (
    o : out boolean
  );
end entity;

architecture a of ent is
  signal only_read : boolean;
begin
   o <= only_read;
end architecture;",
        );

        let (root, diagnostics) = builder.get_analyzed_root();
        check_no_diagnostics(&diagnostics);

        let lib = root.get_lib(&root.symbol_utf8("libname")).unwrap();
        let issues = find_issues(&root, lib, "ent");

        check_issues(
            &issues,
            &[(
                get_ent(&root, code.s1("only_read")),
                UnusedKind::Uninitialized,
            )],
        );
    }

    /// Signal that is read but never assigned, but has an initial value → no diagnostic
    #[test]
    fn signal_read_but_never_assigned_with_init() {
        let mut builder = LibraryBuilder::new();

        let code = builder.code(
            "libname",
            "
entity ent is
  port (
    o : out boolean
  );
end entity;

architecture a of ent is
  signal initialized : boolean := true;
begin
   o <= initialized;
end architecture;",
        );

        let (root, diagnostics) = builder.get_analyzed_root();
        check_no_diagnostics(&diagnostics);

        let lib = root.get_lib(&root.symbol_utf8("libname")).unwrap();
        let issues = find_issues(&root, lib, "ent");

        // initialized has a default value, so it's not Uninitialized
        check_issues(&issues, &[]);
    }

    /// Out port that is never assigned and never referenced → Unused (existing behavior)
    /// An out port that is read (VHDL-2008) but never written → Uninitialized
    #[test]
    fn out_port_never_assigned() {
        let mut builder = LibraryBuilder::new();

        let code = builder.code(
            "libname",
            "
entity ent is
  port (
    o_read_but_never_written : out boolean;
    o_used                    : out boolean
  );
end entity;

architecture a of ent is
  signal internal : boolean;
begin
   o_used <= true;
   -- Read the out port (VHDL-2008) but never write to it
   internal <= o_read_but_never_written;
end architecture;",
        );

        let (root, diagnostics) = builder.get_analyzed_root();
        check_no_diagnostics(&diagnostics);

        let lib = root.get_lib(&root.symbol_utf8("libname")).unwrap();
        let issues = find_issues(&root, lib, "ent");

        // o_read_but_never_written is read but never written, no default → Uninitialized
        // internal is written but never read → NeverRead
        check_issues(
            &issues,
            &[
                (
                    get_ent(&root, code.s1("o_read_but_never_written")),
                    UnusedKind::Uninitialized,
                ),
                (get_ent(&root, code.s1("internal")), UnusedKind::NeverRead),
            ],
        );
    }

    /// In port that is never read → Unused (existing behavior)
    #[test]
    fn in_port_never_read() {
        let mut builder = LibraryBuilder::new();

        let code = builder.code(
            "libname",
            "
entity ent is
  port (
    i_unused : in boolean;
    i_used   : in boolean;
    o        : out boolean
  );
end entity;

architecture a of ent is
begin
   o <= i_used;
end architecture;",
        );

        let (root, diagnostics) = builder.get_analyzed_root();
        check_no_diagnostics(&diagnostics);

        let lib = root.get_lib(&root.symbol_utf8("libname")).unwrap();
        let issues = find_issues(&root, lib, "ent");

        // i_unused is never referenced → Unused
        // i_used is read → no issue
        check_issues(
            &issues,
            &[(get_ent(&root, code.s1("i_unused")), UnusedKind::Unused)],
        );
    }

    /// Variable that is assigned but never read → NeverRead
    #[test]
    fn variable_assigned_but_never_read() {
        let mut builder = LibraryBuilder::new();

        let code = builder.code(
            "libname",
            "
entity ent is
end entity;

architecture a of ent is
begin
   main : process is
      variable only_written : boolean;
   begin
      only_written := true;
      wait;
   end process;
end architecture;",
        );

        let (root, diagnostics) = builder.get_analyzed_root();
        check_no_diagnostics(&diagnostics);

        let lib = root.get_lib(&root.symbol_utf8("libname")).unwrap();
        let issues = find_issues(&root, lib, "ent");

        check_issues(
            &issues,
            &[(
                get_ent(&root, code.s1("only_written")),
                UnusedKind::NeverRead,
            )],
        );
    }

    /// Variable that is read but never assigned (no init) → Uninitialized
    #[test]
    fn variable_read_but_never_assigned() {
        let mut builder = LibraryBuilder::new();

        let code = builder.code(
            "libname",
            "
entity ent is
  port (
    o : out boolean
  );
end entity;

architecture a of ent is
begin
   main : process is
      variable only_read : boolean;
   begin
      o <= only_read;
      wait;
   end process;
end architecture;",
        );

        let (root, diagnostics) = builder.get_analyzed_root();
        check_no_diagnostics(&diagnostics);

        let lib = root.get_lib(&root.symbol_utf8("libname")).unwrap();
        let issues = find_issues(&root, lib, "ent");

        check_issues(
            &issues,
            &[(
                get_ent(&root, code.s1("only_read")),
                UnusedKind::Uninitialized,
            )],
        );
    }

    /// Test the exact example from GitHub issue #411
    #[test]
    fn issue_411_example() {
        let mut builder = LibraryBuilder::new();
        builder.add_std_logic_1164();

        let code = builder.code(
            "libname",
            "
library ieee;
use ieee.std_logic_1164.all;

entity test is
   generic(
      g_UNUSED   : boolean := true
   );
   port(
      i_unused   : in std_logic;
      o_unused   : out std_logic;
      o_used     : out std_logic
   );
end entity test;

architecture example of test is

signal never_used      : std_logic;
signal only_read       : std_logic;
signal only_written    : std_logic;
signal properly_used   : std_logic;

begin

only_written    <= '1';
properly_used   <= '0';

process (properly_used, only_read) is
begin

   o_used   <= properly_used and only_read;

end process;

end architecture example;",
        );

        let (root, diagnostics) = builder.get_analyzed_root();
        check_no_diagnostics(&diagnostics);

        let lib = root.get_lib(&root.symbol_utf8("libname")).unwrap();
        let issues = find_issues(&root, lib, "test");

        check_issues(
            &issues,
            &[
                // g_UNUSED: never referenced → Unused
                (get_ent(&root, code.s1("g_UNUSED")), UnusedKind::Unused),
                // i_unused: never referenced → Unused
                (get_ent(&root, code.s1("i_unused")), UnusedKind::Unused),
                // o_unused: out port never assigned → Uninitialized
                (
                    get_ent(&root, code.s1("o_unused")),
                    UnusedKind::Uninitialized,
                ),
                // never_used: never referenced → Unused
                (get_ent(&root, code.s1("never_used")), UnusedKind::Unused),
                // only_read: read but never assigned → Uninitialized
                (
                    get_ent(&root, code.s1("only_read")),
                    UnusedKind::Uninitialized,
                ),
                // only_written: assigned but never read → NeverRead
                (
                    get_ent(&root, code.s1("only_written")),
                    UnusedKind::NeverRead,
                ),
                // properly_used: assigned and read → no issue
            ],
        );
    }

    /// Properly used signal → no diagnostic
    #[test]
    fn properly_used_signal_no_issue() {
        let mut builder = LibraryBuilder::new();

        let code = builder.code(
            "libname",
            "
entity ent is
  port (
    i : in boolean;
    o : out boolean
  );
end entity;

architecture a of ent is
  signal internal : boolean;
begin
   internal <= i;
   o <= internal;
end architecture;",
        );

        let (root, diagnostics) = builder.get_analyzed_root();
        check_no_diagnostics(&diagnostics);

        let lib = root.get_lib(&root.symbol_utf8("libname")).unwrap();
        let issues = find_issues(&root, lib, "ent");

        // internal is both read and written → no issue
        check_issues(&issues, &[]);
    }

    /// Signal assigned in a selected signal assignment → still counts as write
    #[test]
    fn selected_signal_assignment_counts_as_write() {
        let mut builder = LibraryBuilder::new();

        let code = builder.code(
            "libname",
            "
entity ent is
  port (
    i : in boolean;
    o : out boolean
  );
end entity;

architecture a of ent is
  signal written_never_read : boolean;
  signal read_and_written : boolean;
begin
   with i select written_never_read <=
      true when true,
      false when others;

   read_and_written <= i;
   o <= read_and_written;
end architecture;",
        );

        let (root, diagnostics) = builder.get_analyzed_root();
        check_no_diagnostics(&diagnostics);

        let lib = root.get_lib(&root.symbol_utf8("libname")).unwrap();
        let issues = find_issues(&root, lib, "ent");

        check_issues(
            &issues,
            &[(
                get_ent(&root, code.s1("written_never_read")),
                UnusedKind::NeverRead,
            )],
        );
    }

    /// Signal assigned with conditional signal assignment → still counts as write
    #[test]
    fn conditional_signal_assignment_counts_as_write() {
        let mut builder = LibraryBuilder::new();

        let code = builder.code(
            "libname",
            "
entity ent is
  port (
    i : in boolean;
    o : out boolean
  );
end entity;

architecture a of ent is
  signal written_never_read : boolean;
begin
   written_never_read <= true when i else false;
   o <= i;
end architecture;",
        );

        let (root, diagnostics) = builder.get_analyzed_root();
        check_no_diagnostics(&diagnostics);

        let lib = root.get_lib(&root.symbol_utf8("libname")).unwrap();
        let issues = find_issues(&root, lib, "ent");

        check_issues(
            &issues,
            &[(
                get_ent(&root, code.s1("written_never_read")),
                UnusedKind::NeverRead,
            )],
        );
    }

    /// Inout port that is read but never written → Uninitialized
    #[test]
    fn inout_port_read_but_never_written() {
        let mut builder = LibraryBuilder::new();

        let code = builder.code(
            "libname",
            "
entity ent is
  port (
    io : inout boolean;
    o  : out boolean
  );
end entity;

architecture a of ent is
begin
   -- Read the inout port but never write to it
   o <= io;
end architecture;",
        );

        let (root, diagnostics) = builder.get_analyzed_root();
        check_no_diagnostics(&diagnostics);

        let lib = root.get_lib(&root.symbol_utf8("libname")).unwrap();
        let issues = find_issues(&root, lib, "ent");

        // io is read but never written, no default → Uninitialized
        check_issues(
            &issues,
            &[(get_ent(&root, code.s1("io")), UnusedKind::Uninitialized)],
        );
    }

    /// Out port with default expression, read but never written → not Uninitialized
    #[test]
    fn out_port_with_default_is_not_uninitialized() {
        let mut builder = LibraryBuilder::new();

        let code = builder.code(
            "libname",
            "
entity ent is
  port (
    o : out boolean := false
  );
end entity;

architecture a of ent is
  signal internal : boolean;
begin
   -- Read the out port (VHDL-2008) but never write to it.
   -- Since it has a default expression, it is not Uninitialized.
   internal <= o;
end architecture;",
        );

        let (root, diagnostics) = builder.get_analyzed_root();
        check_no_diagnostics(&diagnostics);

        let lib = root.get_lib(&root.symbol_utf8("libname")).unwrap();
        let issues = find_issues(&root, lib, "ent");

        // o is read but never written, but has a default → not Uninitialized
        // internal is written but never read → NeverRead
        check_issues(
            &issues,
            &[(get_ent(&root, code.s1("internal")), UnusedKind::NeverRead)],
        );
    }

    /// Signal driven by a component's out port via port map → not Uninitialized
    /// (regression test: the actual of an out port association is a write)
    #[test]
    fn signal_driven_by_component_out_port() {
        let mut builder = LibraryBuilder::new();

        let code = builder.code(
            "libname",
            "
entity sub is
  port (
    clk40 : out boolean
  );
end entity;

entity ent is
end entity;

architecture a of ent is
  signal driven : boolean;
begin
  inst : entity work.sub
    port map (
      clk40 => driven
    );
end architecture;",
        );

        let (root, diagnostics) = builder.get_analyzed_root();
        check_no_diagnostics(&diagnostics);

        let lib = root.get_lib(&root.symbol_utf8("libname")).unwrap();
        let issues = find_issues(&root, lib, "ent");

        // driven is written by the component through the out port → not Uninitialized.
        // driven is never read → NeverRead (the component drives it but nobody reads it).
        check_issues(
            &issues,
            &[(get_ent(&root, code.s1("driven")), UnusedKind::NeverRead)],
        );
    }

    /// Signal driven by a component's inout port via port map → not Uninitialized
    #[test]
    fn signal_connected_to_component_inout_port() {
        let mut builder = LibraryBuilder::new();

        let code = builder.code(
            "libname",
            "
entity sub is
  port (
    io : inout boolean
  );
end entity;

entity ent is
  port (
    o : out boolean
  );
end entity;

architecture a of ent is
  signal data : boolean;
begin
  inst : entity work.sub
    port map (
      io => data
    );
  -- Read the signal so it's not NeverRead
  o <= data;
end architecture;",
        );

        let (root, diagnostics) = builder.get_analyzed_root();
        check_no_diagnostics(&diagnostics);

        let lib = root.get_lib(&root.symbol_utf8("libname")).unwrap();
        let issues = find_issues(&root, lib, "ent");

        // data is both read and written (inout port) → no issue
        check_issues(&issues, &[]);
    }

    /// Signal connected to a component's in port → read, not written
    #[test]
    fn signal_connected_to_component_in_port() {
        let mut builder = LibraryBuilder::new();

        let code = builder.code(
            "libname",
            "
entity sub is
  port (
    i : in boolean
  );
end entity;

entity ent is
  port (
    o : out boolean
  );
end entity;

architecture a of ent is
  signal data : boolean := true;
begin
  inst : entity work.sub
    port map (
      i => data
    );
  o <= data;
end architecture;",
        );

        let (root, diagnostics) = builder.get_analyzed_root();
        check_no_diagnostics(&diagnostics);

        let lib = root.get_lib(&root.symbol_utf8("libname")).unwrap();
        let issues = find_issues(&root, lib, "ent");

        // data is read (in port + o <= data) and has a default → no issue
        check_issues(&issues, &[]);
    }

    /// Issue #411 example: diagnostics via the linter produce correct error codes
    #[test]
    fn issue_411_linter_diagnostics() {
        let mut builder = LibraryBuilder::new();
        builder.add_std_logic_1164();

        let code = builder.code(
            "libname",
            "
library ieee;
use ieee.std_logic_1164.all;

entity test is
   generic(
      g_UNUSED   : boolean := true
   );
   port(
      i_unused   : in std_logic;
      o_unused   : out std_logic;
      o_used     : out std_logic
   );
end entity test;

architecture example of test is

signal never_used      : std_logic;
signal only_read       : std_logic;
signal only_written    : std_logic;
signal properly_used   : std_logic;

begin

only_written    <= '1';
properly_used   <= '0';

process (properly_used, only_read) is
begin

   o_used   <= properly_used and only_read;

end process;

end architecture example;",
        );

        let (root, diagnostics) = builder.get_analyzed_root();
        check_no_diagnostics(&diagnostics);

        let lib = root.get_lib(&root.symbol_utf8("libname")).unwrap();
        let issues = find_issues(&root, lib, "test");

        // Collect diagnostics with error codes
        let mut diags: Vec<(EntRef<'_>, ErrorCode)> = issues
            .iter()
            .map(|issue| (issue.ent, issue.kind.error_code()))
            .collect();
        diags.sort_by_key(|(ent, _)| ent.decl_pos().unwrap().start().line);

        // Verify the error codes match expectations
        let expected_codes: Vec<(&str, ErrorCode)> = vec![
            ("g_UNUSED", ErrorCode::Unused),
            ("i_unused", ErrorCode::Unused),
            ("o_unused", ErrorCode::Uninitialized),
            ("never_used", ErrorCode::Unused),
            ("only_read", ErrorCode::Uninitialized),
            ("only_written", ErrorCode::NeverRead),
        ];

        for (name, expected_code) in &expected_codes {
            let ent = get_ent(&root, code.s1(name));
            assert!(
                diags
                    .iter()
                    .any(|(e, code)| *e == ent && code == expected_code),
                "Expected {:?} for '{}' but got {:?}",
                expected_code,
                name,
                diags.iter().find(|(e, _)| *e == ent).map(|(_, c)| c)
            );
        }
    }
}
