//! This Source Code Form is subject to the terms of the Mozilla Public
//! License, v. 2.0. If a copy of the MPL was not distributed with this file,
//! You can obtain one at http://mozilla.org/MPL/2.0/.
//!
//! Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use std::ops::Deref;

use super::*;
use crate::ast::Designator;
use crate::ast::HasDesignator;
use crate::ast::WithRef;
use crate::named_entity::visibility::Visibility;
use crate::Diagnostic;

#[derive(Clone)]
pub enum Design<'a> {
    Entity(Visibility<'a>, Region<'a>),
    /// A VHDL architecture.
    /// The linked `DesignEnt` is the entity that belongs to the architecture.
    Architecture(Visibility<'a>, Region<'a>, DesignEnt<'a>),
    Configuration,
    Package(Visibility<'a>, Region<'a>),
    PackageBody(Visibility<'a>, Region<'a>),
    UninstPackage(Visibility<'a>, Region<'a>),
    /// An instantiated Package, i.e.,
    /// ```vhdl
    /// package foo is new bar generic map (...);
    /// ```
    PackageInstance(Region<'a>),
    /// An instantiated package that is part of some generic interface, i.e.,
    /// ```vhdl
    /// generic (
    ///     package foo is new bar generic map (<>)
    /// )
    /// ```
    InterfacePackageInstance(Region<'a>),
    Context(Region<'a>),
}

impl Design<'_> {
    pub fn describe(&self) -> &'static str {
        use Design::*;
        match self {
            Entity(..) => "entity",
            Architecture(..) => "architecture",
            Configuration => "configuration",
            Package(..) => "package",
            PackageBody(..) => "package body",
            UninstPackage(..) => "uninstantiated package",
            PackageInstance(_) | InterfacePackageInstance(_) => "package instance",
            Context(..) => "context",
        }
    }
}

// A named entity that is known to be a type
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct DesignEnt<'a>(pub EntRef<'a>);

impl<'a> DesignEnt<'a> {
    pub fn from_any(ent: &'a AnyEnt<'a>) -> Option<DesignEnt<'a>> {
        if matches!(ent.kind(), AnyEntKind::Design(..)) {
            Some(DesignEnt(ent))
        } else {
            None
        }
    }

    pub fn kind(&self) -> &'a Design<'a> {
        if let AnyEntKind::Design(typ) = self.0.kind() {
            typ
        } else {
            unreachable!("Must be a design");
        }
    }

    pub fn selected(
        &self,
        ctx: &dyn TokenAccess,
        prefix_pos: TokenSpan,
        suffix: &WithToken<WithRef<Designator>>,
    ) -> Result<NamedEntities<'a>, Diagnostic> {
        match self.kind() {
            Design::Package(_, ref region)
            | Design::PackageInstance(ref region)
            | Design::InterfacePackageInstance(ref region) => {
                if let Some(decl) = region.lookup_immediate(suffix.designator()) {
                    Ok(decl.clone())
                } else {
                    Err(Diagnostic::no_declaration_within(
                        self,
                        suffix.pos(ctx),
                        &suffix.item.item,
                    ))
                }
            }
            _ => Err(Diagnostic::invalid_selected_name_prefix(
                self,
                &prefix_pos.pos(ctx),
            )),
        }
    }
}

impl<'a> From<DesignEnt<'a>> for EntRef<'a> {
    fn from(ent: DesignEnt<'a>) -> Self {
        ent.0
    }
}

impl<'a> Deref for DesignEnt<'a> {
    type Target = AnyEnt<'a>;
    fn deref(&self) -> EntRef<'a> {
        self.0
    }
}
