// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2024, Olof Kraigher olof.kraigher@gmail.com
use crate::analysis::DesignRoot;
use crate::completion::region::completion_items_from_region;
use crate::data::Symbol;
use crate::syntax::Kind::All;
use crate::{named_entity, CompletionItem, EntRef, HasEntityId};
use std::iter::once;

/// Produces completions for a selected name, i.e.,
/// `foo.`
/// The provided `ent` is the entity directly before the dot, i.e.,
/// `foo` in the example above.
pub(crate) fn completions_for_selected_name<'b>(
    root: &'b DesignRoot,
    ent: EntRef<'b>,
) -> Vec<CompletionItem<'b>> {
    use crate::named_entity::AnyEntKind::*;
    match ent.kind() {
        Object(object) => completions_for_type(root, object.subtype.type_mark().kind()),
        Design(design) => completions_for_design(root, design),
        ElementDeclaration(subtyp) => completions_for_type(root, subtyp.type_mark.kind()),
        Library => ent
            .library_name()
            .map(|sym| list_primaries_for_lib(root, sym))
            .unwrap_or_default(),
        _ => vec![],
    }
}

/// Returns completions applicable when calling `foo.` where `foo` is amn object of some type.
fn completions_for_type<'a>(
    root: &'a DesignRoot,
    typ: &'a named_entity::Type<'a>,
) -> Vec<CompletionItem<'a>> {
    use crate::named_entity::Type::*;
    match typ {
        Record(record_region) => record_region
            .iter()
            .map(|item| CompletionItem::Simple(item.ent))
            .collect(),
        Alias(type_ent) => completions_for_type(root, type_ent.kind()),
        Access(subtype) => {
            let mut completions = completions_for_type(root, subtype.type_mark().kind());
            completions.push(CompletionItem::Keyword(All));
            completions
        }
        Protected(region, _) => completion_items_from_region(root, region).collect(),
        _ => vec![],
    }
}

/// Returns completions applicable when calling `foo.` where `foo` is some design
/// (i.e. entity or package).
fn completions_for_design<'a>(
    root: &'a DesignRoot,
    design: &'a crate::Design<'a>,
) -> Vec<CompletionItem<'a>> {
    use crate::named_entity::Design::*;
    match design {
        Package(_, region) | PackageInstance(region) | InterfacePackageInstance(region) => {
            completion_items_from_region(root, region)
                .chain(once(CompletionItem::Keyword(All)))
                .collect()
        }
        _ => vec![],
    }
}

/// List the name of all primary units for a given library.
/// If the library is non-resolvable, list an empty vector
fn list_primaries_for_lib<'a>(root: &'a DesignRoot, lib: &Symbol) -> Vec<CompletionItem<'a>> {
    let Some(lib) = root.get_lib(lib) else {
        return vec![];
    };
    lib.primary_units()
        .filter_map(|it| it.unit.get().and_then(|unit| unit.ent_id()))
        .map(|id| CompletionItem::Simple(root.get_ent(id)))
        .collect()
}

#[cfg(test)]
mod tests {
    use crate::analysis::tests::{assert_eq_unordered, LibraryBuilder};
    use crate::ast::Designator;
    use crate::syntax::Kind::All;
    use crate::{list_completion_options, CompletionItem};

    #[test]
    pub fn completes_selected_names() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libA",
            "\
package foo is
    type my_record is record
        abc: bit;
        def: bit;
    end record;

    constant y: my_record := ('1', '1');
    constant z: bit := y.
end foo;
        ",
        );

        let (root, _) = builder.get_analyzed_root();
        let cursor = code.s1("y.").end();
        let options = list_completion_options(&root, code.source(), cursor);

        let ent1 = root
            .search_reference(code.source(), code.s1("abc").start())
            .unwrap();

        let ent2 = root
            .search_reference(code.source(), code.s1("def").start())
            .unwrap();

        assert_eq_unordered(
            &options,
            &[CompletionItem::Simple(ent1), CompletionItem::Simple(ent2)],
        )
    }

    #[test]
    pub fn completing_primaries() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "\
use std.

-- Need this so that the 'use std.' is associated to a context and can be analyzed correctly
package x is
end package x;
",
        );
        let (root, _) = builder.get_analyzed_root();
        let cursor = code.s1("use std.").end();
        let options = list_completion_options(&root, code.source(), cursor);
        assert_eq_unordered(
            &options,
            &[
                CompletionItem::Simple(root.find_textio_pkg()),
                CompletionItem::Simple(root.find_standard_pkg()),
                CompletionItem::Simple(root.find_env_pkg()),
            ],
        );

        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "\
use std.t

-- Need this so that the 'use std.' is associated to a context and can be analyzed correctly
package x is
end package x;
",
        );
        let (root, _) = builder.get_analyzed_root();
        let cursor = code.s1("use std.t").end();
        let options = list_completion_options(&root, code.source(), cursor);
        // Note that the filtering only happens at client side
        assert_eq_unordered(
            &options,
            &[
                CompletionItem::Simple(root.find_textio_pkg()),
                CompletionItem::Simple(root.find_standard_pkg()),
                CompletionItem::Simple(root.find_env_pkg()),
            ],
        );
    }

    #[test]
    pub fn completing_declarations() {
        let mut input = LibraryBuilder::new();
        let code = input.code(
            "libname",
            "\
use std.env.

-- Need this so that the 'use std.env.' is associated to a context and can be analyzed correctly
package x is
end package x;
",
        );
        let (root, _) = input.get_analyzed_root();
        let cursor = code.s1("use std.env.").end();
        let options = list_completion_options(&root, code.source(), cursor);

        assert_eq_unordered(
            &options,
            &[
                CompletionItem::Overloaded(Designator::Identifier(root.symbol_utf8("stop")), 2),
                CompletionItem::Overloaded(Designator::Identifier(root.symbol_utf8("finish")), 2),
                CompletionItem::Simple(root.find_env_symbol("resolution_limit")),
                CompletionItem::Keyword(All),
            ],
        );
    }

    #[test]
    pub fn completing_nested_records() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libA",
            "\
package foo is
    type t_subrec is record
        elem_a: bit;
        elem_b: bit;
    end record;

    type my_record is record
        abc: t_subrec;
    end record;

    signal y: my_record;
    signal z: bit := y.abc.
end foo;
        ",
        );

        let (root, _) = builder.get_analyzed_root();
        let cursor = code.s1("abc.").end();
        let options = list_completion_options(&root, code.source(), cursor);

        let ent1 = root
            .search_reference(code.source(), code.s1("elem_a").start())
            .unwrap();

        let ent2 = root
            .search_reference(code.source(), code.s1("elem_b").start())
            .unwrap();

        assert_eq_unordered(
            &options,
            &[CompletionItem::Simple(ent1), CompletionItem::Simple(ent2)],
        )
    }

    // It is currently unclear, whether the pack of this is a parser or analyzer limitation
    #[test]
    #[ignore]
    pub fn completing_external_primary_unit_names() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libA",
            "\
package foo is
    signal y: my_record;
    signal z: bit := << signal @libA.
end foo;
        ",
        );

        let (root, _) = builder.get_analyzed_root();
        let cursor = code.s1("@libA.").end();
        let options = list_completion_options(&root, code.source(), cursor);

        let ent1 = root
            .search_reference(code.source(), code.s1("foo").start())
            .unwrap();

        assert_eq_unordered(&options, &[CompletionItem::Simple(ent1)])
    }
}
