//! This Source Code Form is subject to the terms of the Mozilla Public
//! License, v. 2.0. If a copy of the MPL was not distributed with this file,
//! You can obtain one at http://mozilla.org/MPL/2.0/.
//!
//! Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use std::ops::Deref;

use super::AnyEnt;
use super::AnyEntKind;
use super::EntRef;
use crate::analysis::region::NamedEntities;
use crate::analysis::region::Region;
use crate::analysis::visibility::Visibility;
use crate::ast::Designator;
use crate::ast::HasDesignator;
use crate::ast::WithRef;
use crate::data::WithPos;
use crate::Diagnostic;
use crate::SrcPos;

pub enum Design<'a> {
    Entity(Visibility<'a>, Region<'a>),
    Architecture(DesignEnt<'a>),
    Configuration,
    Package(Visibility<'a>, Region<'a>),
    PackageBody,
    UninstPackage(Visibility<'a>, Region<'a>),
    PackageInstance(Region<'a>),
    Context(Region<'a>),
}

impl<'a> Design<'a> {
    pub fn describe(&self) -> &'static str {
        use Design::*;
        match self {
            Entity(..) => "entity",
            Architecture(..) => "architecture",
            Configuration => "configuration",
            Package(..) => "package",
            PackageBody => "package body",
            UninstPackage(..) => "uninstantiated package",
            PackageInstance(..) => "package instance",
            Context(..) => "context",
        }
    }
}

// A named entity that is known to be a type
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct DesignEnt<'a>(pub EntRef<'a>);

impl<'a> DesignEnt<'a> {
    pub fn from_any(ent: &'a AnyEnt) -> Option<DesignEnt<'a>> {
        if matches!(ent.kind(), AnyEntKind::Design(..)) {
            Some(DesignEnt(ent))
        } else {
            None
        }
    }

    pub fn kind(&self) -> &Design<'a> {
        if let AnyEntKind::Design(typ) = self.0.kind() {
            typ
        } else {
            unreachable!("Must be a design");
        }
    }

    pub fn selected(
        &self,
        prefix_pos: &SrcPos,
        suffix: &WithPos<WithRef<Designator>>,
    ) -> Result<NamedEntities<'a>, Diagnostic> {
        match self.kind() {
            Design::Package(_, ref region) | Design::PackageInstance(ref region) => {
                if let Some(decl) = region.lookup_immediate(suffix.designator()) {
                    Ok(decl.clone())
                } else {
                    Err(Diagnostic::no_declaration_within(
                        self,
                        &suffix.pos,
                        &suffix.item.item,
                    ))
                }
            }
            _ => Err(Diagnostic::invalid_selected_name_prefix(self, prefix_pos)),
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
