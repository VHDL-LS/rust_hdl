// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2020, Olof Kraigher olof.kraigher@gmail.com

use super::*;

#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
pub enum PrimaryKind {
    Entity,
    Configuration,
    Package,
    PackageInstance,
    Context,
}

#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
pub enum SecondaryKind {
    Architecture,
    PackageBody,
}

#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
pub enum AnyKind {
    Primary(PrimaryKind),
    Secondary(SecondaryKind),
}

/// Stores a design unit's name and, for secondary units,
/// the name of its associated primary unit.
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum UnitKey {
    Primary(Symbol),
    Secondary(Symbol, Symbol),
}

/// Identifies a design unit.
///
/// Additionally, a `UnitId` specifies a unit's name, kind, library,
/// and, for secondary units, its associated primary unit.
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct UnitId {
    library_name: Symbol,
    kind: AnyKind,
    key: UnitKey,
}

impl UnitId {
    pub fn primary(library_name: &Symbol, kind: PrimaryKind, name: &Symbol) -> UnitId {
        UnitId {
            library_name: library_name.clone(),
            kind: AnyKind::Primary(kind),
            key: UnitKey::Primary(name.clone()),
        }
    }

    pub fn secondary(
        library_name: &Symbol,
        kind: SecondaryKind,
        primary_name: &Symbol,
        name: &Symbol,
    ) -> UnitId {
        UnitId {
            library_name: library_name.clone(),
            kind: AnyKind::Secondary(kind),
            key: UnitKey::Secondary(primary_name.clone(), name.clone()),
        }
    }

    pub fn package(library_name: &Symbol, name: &Symbol) -> UnitId {
        Self::primary(library_name, PrimaryKind::Package, name)
    }

    pub fn library_name(&self) -> &Symbol {
        &self.library_name
    }

    /// For a secondary unit, returns the name of the associated primary unit;
    /// for a primary unit, returns its own name.
    pub fn primary_name(&self) -> &Symbol {
        match self.key {
            UnitKey::Primary(ref name) => name,
            UnitKey::Secondary(ref name, _) => name,
        }
    }

    pub fn secondary_name(&self) -> Option<&Symbol> {
        match self.key {
            UnitKey::Primary(_) => None,
            UnitKey::Secondary(_, ref name) => Some(name),
        }
    }
}

pub trait HasUnitId {
    fn unit_id(&self) -> &UnitId;
    fn kind(&self) -> AnyKind {
        self.unit_id().kind
    }
    fn key(&self) -> &UnitKey {
        &self.unit_id().key
    }

    fn secondary_kind(&self) -> Option<SecondaryKind> {
        match self.unit_id().kind {
            AnyKind::Secondary(kind) => Some(kind),
            AnyKind::Primary(..) => None,
        }
    }

    fn describe(&self) -> String {
        match self.key() {
            UnitKey::Primary(name) => format!("{} '{}'", self.kind().describe(), name),
            UnitKey::Secondary(primary_name, name) => match self.secondary_kind().unwrap() {
                SecondaryKind::Architecture => format!(
                    "{} '{}' of '{}'",
                    self.kind().describe(),
                    name,
                    primary_name
                ),
                SecondaryKind::PackageBody => format!("{} '{}'", self.kind().describe(), name),
            },
        }
    }
}

impl HasUnitId for UnitId {
    fn unit_id(&self) -> &UnitId {
        self
    }
}

macro_rules! delegate_primary {
    ($primary:expr, $unit:ident, $block:expr) => {
        match $primary {
            AnyPrimaryUnit::Entity($unit) => $block,
            AnyPrimaryUnit::Package($unit) => $block,
            AnyPrimaryUnit::PackageInstance($unit) => $block,
            AnyPrimaryUnit::Context($unit) => $block,
            AnyPrimaryUnit::Configuration($unit) => $block,
        }
    };
}

macro_rules! delegate_secondary {
    ($primary:expr, $unit:ident, $block:expr) => {
        match $primary {
            AnySecondaryUnit::Architecture($unit) => $block,
            AnySecondaryUnit::PackageBody($unit) => $block,
        }
    };
}

macro_rules! delegate_any {
    ($primary:expr, $unit:ident, $block:expr) => {
        match $primary {
            AnyDesignUnit::Primary($unit) => $block,
            AnyDesignUnit::Secondary($unit) => $block,
        }
    };
}

impl AnyKind {
    pub fn describe(&self) -> &str {
        match self {
            AnyKind::Primary(kind) => kind.describe(),
            AnyKind::Secondary(kind) => kind.describe(),
        }
    }
}

impl PrimaryKind {
    pub fn kind_of(unit: &AnyPrimaryUnit) -> PrimaryKind {
        match unit {
            AnyPrimaryUnit::Entity(..) => PrimaryKind::Entity,
            AnyPrimaryUnit::Configuration(..) => PrimaryKind::Configuration,
            AnyPrimaryUnit::Package(..) => PrimaryKind::Package,
            AnyPrimaryUnit::PackageInstance(..) => PrimaryKind::PackageInstance,
            AnyPrimaryUnit::Context(..) => PrimaryKind::Context,
        }
    }

    pub fn describe(&self) -> &str {
        match self {
            PrimaryKind::Entity => "entity",
            PrimaryKind::Configuration => "configuration",
            PrimaryKind::Package => "package",
            PrimaryKind::PackageInstance => "package instance",
            PrimaryKind::Context => "context",
        }
    }
}

impl SecondaryKind {
    pub fn kind_of(unit: &AnySecondaryUnit) -> SecondaryKind {
        match unit {
            AnySecondaryUnit::Architecture(..) => SecondaryKind::Architecture,
            AnySecondaryUnit::PackageBody(..) => SecondaryKind::PackageBody,
        }
    }

    pub fn describe(&self) -> &str {
        match self {
            SecondaryKind::Architecture => "architecture",
            SecondaryKind::PackageBody => "package body",
        }
    }
}

impl AnyDesignUnit {
    pub fn as_primary_mut(&mut self) -> Option<&mut AnyPrimaryUnit> {
        if let AnyDesignUnit::Primary(unit) = self {
            Some(unit)
        } else {
            None
        }
    }
}

/// Upper case first letter
pub fn capitalize(string: &str) -> String {
    let mut result = String::with_capacity(string.len());
    let mut chars = string.chars();
    if let Some(chr) = chars.next() {
        result.push(chr.to_ascii_uppercase());
    }
    for chr in chars {
        result.push(chr);
    }
    result
}
