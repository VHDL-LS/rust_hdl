// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use ast::*;
use message::{error, MessageHandler};

extern crate fnv;
use self::fnv::FnvHashMap;
use std::collections::hash_map::Entry;

// @TODO move to re-usable place
fn ident<'a>(decl: &'a Declaration) -> Option<&'a Ident> {
    match decl {
        // @TODO Ignored for now
        Declaration::Alias(AliasDeclaration { .. }) => None,
        Declaration::Object(ObjectDeclaration { ref ident, .. }) => Some(ident),
        Declaration::File(FileDeclaration { ref ident, .. }) => Some(ident),
        Declaration::Component(ComponentDeclaration { ref ident, .. }) => Some(ident),
        // @TODO Ignored for now
        Declaration::Attribute(..) => None,
        // @TODO Ignored for now
        Declaration::SubprogramBody(..) => None,
        // @TODO Ignored for now
        Declaration::SubprogramDeclaration(..) => None,
        // @TODO Ignored for now
        Declaration::Use(..) => None,
        // @TODO Ignored for now
        Declaration::Package(..) => None,
        Declaration::Configuration(..) => None,
        Declaration::Type(TypeDeclaration {
            def: TypeDefinition::ProtectedBody(..),
            ..
        }) => None,
        Declaration::Type(TypeDeclaration {
            def: TypeDefinition::Incomplete,
            ..
        }) => None,
        Declaration::Type(TypeDeclaration { ref ident, .. }) => Some(ident),
    }
}

/// Check that no homographs are defined in the declarative region
fn check_declarative_part_unique_ident(
    declarations: &Vec<Declaration>,
    messages: &mut MessageHandler,
) {
    let mut idents = FnvHashMap::default();
    for decl in declarations.iter() {
        if let Some(ref ident) = ident(decl) {
            match idents.entry(&ident.item) {
                Entry::Occupied(entry) => {
                    let msg = error(
                        ident,
                        &format!("Duplicate declaration of '{}'", ident.item.name()),
                    ).related(entry.get(), "Previously defined here");
                    messages.push(msg)
                }
                Entry::Vacant(entry) => {
                    entry.insert(&ident.pos);
                }
            }
        }
    }
}

fn check_package_declaration(package: &PackageDeclaration, messages: &mut MessageHandler) {
    check_declarative_part_unique_ident(&package.decl, messages);
}

fn check_architecture_body(architecture: &ArchitectureBody, messages: &mut MessageHandler) {
    check_declarative_part_unique_ident(&architecture.decl, messages);
}

fn check_package_body(package: &PackageBody, messages: &mut MessageHandler) {
    check_declarative_part_unique_ident(&package.decl, messages);
}

fn check_entity_declaration(entity: &EntityDeclaration, messages: &mut MessageHandler) {
    check_declarative_part_unique_ident(&entity.decl, messages);
}

pub fn check_design_unit(design_unit: &DesignUnit, messages: &mut MessageHandler) {
    match &design_unit.library_unit {
        LibraryUnit::PackageDeclaration(package) => check_package_declaration(package, messages),
        LibraryUnit::Architecture(architecture) => check_architecture_body(architecture, messages),
        LibraryUnit::PackageBody(package) => check_package_body(package, messages),
        LibraryUnit::EntityDeclaration(entity) => check_entity_declaration(entity, messages),
        // @TODO others
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_util::{check_no_messages, Code};

    #[test]
    fn allows_unique_names() {
        let code = Code::new(
            "
constant a : natural;
constant b : natural;
constant c : natural;
",
        );

        let mut messages = Vec::new();
        check_declarative_part_unique_ident(&code.declarative_part(), &mut messages);
        check_no_messages(&messages);
    }

    #[test]
    fn forbid_homographs() {
        let code = Code::new(
            "
constant a1 : natural;
constant b : natural;
constant a1 : natural;
",
        );

        let mut messages = Vec::new();
        check_declarative_part_unique_ident(&code.declarative_part(), &mut messages);
        assert_eq!(
            messages,
            vec![
                error(code.s("a1", 2), "Duplicate declaration of 'a1'")
                    .related(code.s1("a1"), "Previously defined here")
            ]
        )
    }

    #[test]
    fn allows_protected_type_and_body_with_same_name() {
        let code = Code::new(
            "
type prot_t is protected
end protected;

type prot_t is protected body
end protected body;
",
        );

        let mut messages = Vec::new();
        check_declarative_part_unique_ident(&code.declarative_part(), &mut messages);
        check_no_messages(&messages);
    }

    #[test]
    fn allows_incomplete_type_definition() {
        let code = Code::new(
            "
type rec_t;
type rec_t is record
end record;
",
        );

        let mut messages = Vec::new();
        check_declarative_part_unique_ident(&code.declarative_part(), &mut messages);
        check_no_messages(&messages);
    }

}
