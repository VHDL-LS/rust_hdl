// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

//! Traits concerning identiers and designators

use super::*;
use crate::source::{SrcPos, WithPos};
use crate::symbol_table::Symbol;

pub trait HasIdent {
    fn ident(&self) -> &Ident;
    fn name(&self) -> &Symbol {
        &self.ident().item
    }
    fn pos(&self) -> &SrcPos {
        &self.ident().pos
    }
}

impl HasIdent for Ident {
    fn ident(&self) -> &Ident {
        &self
    }
}

impl HasIdent for EntityDeclaration {
    fn ident(&self) -> &Ident {
        &self.ident
    }
}

impl HasIdent for PackageDeclaration {
    fn ident(&self) -> &Ident {
        &self.ident
    }
}

impl HasIdent for PackageBody {
    fn ident(&self) -> &Ident {
        &self.ident
    }
}

impl HasIdent for ArchitectureBody {
    fn ident(&self) -> &Ident {
        &self.ident
    }
}

impl HasIdent for PackageInstantiation {
    fn ident(&self) -> &Ident {
        &self.ident
    }
}

impl HasIdent for ContextDeclaration {
    fn ident(&self) -> &Ident {
        &self.ident
    }
}

impl HasIdent for ConfigurationDeclaration {
    fn ident(&self) -> &Ident {
        &self.ident
    }
}

impl HasIdent for PrimaryUnit {
    fn ident(&self) -> &Ident {
        match self {
            PrimaryUnit::EntityDeclaration(ref unit) => &unit.unit.ident,
            PrimaryUnit::Configuration(ref unit) => &unit.unit.ident,
            PrimaryUnit::PackageDeclaration(ref unit) => &unit.unit.ident,
            PrimaryUnit::PackageInstance(ref unit) => &unit.unit.ident,
            PrimaryUnit::ContextDeclaration(ref unit) => &unit.ident,
        }
    }
}

impl<T: HasIdent> HasIdent for DesignUnit<T> {
    fn ident(&self) -> &Ident {
        self.unit.ident()
    }
}

impl<'a, T: HasIdent> From<&'a T> for WithPos<Designator> {
    fn from(other: &'a T) -> WithPos<Designator> {
        other.ident().to_owned().map_into(Designator::Identifier)
    }
}

impl From<EnumerationLiteral> for Designator {
    fn from(other: EnumerationLiteral) -> Designator {
        match other {
            EnumerationLiteral::Identifier(ident) => Designator::Identifier(ident),
            EnumerationLiteral::Character(byte) => Designator::Character(byte),
        }
    }
}

impl From<Symbol> for Designator {
    fn from(other: Symbol) -> Designator {
        Designator::Identifier(other)
    }
}

impl<'a> From<&'a Symbol> for Designator {
    fn from(other: &'a Symbol) -> Designator {
        other.clone().into()
    }
}

impl std::fmt::Display for Designator {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Designator::Identifier(ref sym) => write!(f, "{}", sym),
            Designator::OperatorSymbol(ref latin1) => write!(f, "\"{}\"", latin1),
            Designator::Character(byte) => write!(f, "'{}'", *byte as char),
        }
    }
}

impl SubprogramDesignator {
    pub fn into_designator(self) -> Designator {
        match self {
            SubprogramDesignator::Identifier(ident) => Designator::Identifier(ident),
            SubprogramDesignator::OperatorSymbol(ident) => Designator::OperatorSymbol(ident),
        }
    }
}

impl SubprogramDeclaration {
    pub fn designator(&self) -> WithPos<Designator> {
        match self {
            SubprogramDeclaration::Function(ref function) => function
                .designator
                .clone()
                .map_into(|des| des.into_designator()),
            SubprogramDeclaration::Procedure(ref procedure) => procedure
                .designator
                .clone()
                .map_into(|des| des.into_designator()),
        }
    }
}

impl EnumerationLiteral {
    pub fn into_designator(self) -> Designator {
        match self {
            EnumerationLiteral::Identifier(ident) => Designator::Identifier(ident),
            EnumerationLiteral::Character(byte) => Designator::Character(byte),
        }
    }
}
