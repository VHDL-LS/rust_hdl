// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

//! Name conversions
use super::*;
use crate::diagnostic::{Diagnostic, ParseResult};
use crate::source::{HasSrcPos, SrcPos, WithPos};
use crate::symbol_table::Symbol;

impl From<WithPos<SelectedName>> for WithPos<Name> {
    fn from(selected_name: WithPos<SelectedName>) -> WithPos<Name> {
        match selected_name.item {
            SelectedName::Designator(designator) => {
                WithPos::from(Name::Designator(designator), selected_name.pos)
            }

            SelectedName::Selected(prefix, suffix) => {
                let prefix: WithPos<SelectedName> = *prefix;
                WithPos::from(
                    Name::Selected(Box::new(prefix.into()), suffix),
                    selected_name.pos,
                )
            }
        }
    }
}

pub fn to_simple_name(name: WithPos<Name>) -> ParseResult<Ident> {
    match name.item {
        Name::Designator(WithRef {
            item: Designator::Identifier(ident),
            ..
        }) => Ok(WithPos {
            item: ident,
            pos: name.pos,
        }),
        _ => Err(Diagnostic::error(&name, "Expected simple name")),
    }
}

pub trait HasDesignator {
    fn designator(&self) -> &Designator;
}

impl<T: HasDesignator> HasDesignator for WithPos<T> {
    fn designator(&self) -> &Designator {
        self.item.designator()
    }
}

impl HasDesignator for Designator {
    fn designator(&self) -> &Designator {
        self
    }
}

impl<T: HasDesignator> HasDesignator for WithRef<T> {
    fn designator(&self) -> &Designator {
        self.item.designator()
    }
}

impl Designator {
    pub fn into_ref(self) -> WithRef<Designator> {
        WithRef::new(self)
    }
}

impl Ident {
    pub fn into_ref(self) -> WithRef<Ident> {
        WithRef::new(self)
    }
}

impl WithPos<Designator> {
    pub fn into_ref(self) -> WithPos<WithRef<Designator>> {
        self.map_into(|name| name.into_ref())
    }
}

pub trait HasIdent {
    fn ident(&self) -> &Ident;
    fn name(&self) -> &Symbol {
        &self.ident().item
    }
}

impl<T: HasIdent> HasSrcPos for T {
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
        &self.ident.item
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

impl HasIdent for AnyPrimaryUnit {
    fn ident(&self) -> &Ident {
        match self {
            AnyPrimaryUnit::EntityDeclaration(ref unit) => unit.ident(),
            AnyPrimaryUnit::Configuration(ref unit) => unit.ident(),
            AnyPrimaryUnit::PackageDeclaration(ref unit) => unit.ident(),
            AnyPrimaryUnit::PackageInstance(ref unit) => unit.ident(),
            AnyPrimaryUnit::ContextDeclaration(ref unit) => unit.ident(),
        }
    }
}

impl HasIdent for AnySecondaryUnit {
    fn ident(&self) -> &Ident {
        match self {
            AnySecondaryUnit::PackageBody(ref unit) => unit.ident(),
            AnySecondaryUnit::Architecture(ref unit) => unit.ident(),
        }
    }
}

impl HasIdent for AnyDesignUnit {
    fn ident(&self) -> &Ident {
        match self {
            AnyDesignUnit::Primary(ref unit) => unit.ident(),
            AnyDesignUnit::Secondary(ref unit) => unit.ident(),
        }
    }
}

impl<'a, T: HasIdent> From<&'a T> for WithPos<Designator> {
    fn from(other: &'a T) -> WithPos<Designator> {
        other.ident().to_owned().map_into(Designator::Identifier)
    }
}

/// Primary identifier in secondary units
pub trait HasPrimaryIdent {
    fn primary_ident(&self) -> &Ident;
    fn primary_name(&self) -> &Symbol {
        &self.primary_ident().item
    }
    /// The position of the primary name in the secondary unit declaration
    fn primary_pos(&self) -> &SrcPos {
        &self.primary_ident().pos
    }
}

impl HasPrimaryIdent for ArchitectureBody {
    fn primary_ident(&self) -> &Ident {
        &self.entity_name.item
    }
}

impl HasPrimaryIdent for PackageBody {
    fn primary_ident(&self) -> &Ident {
        &self.ident.item
    }
}

impl HasPrimaryIdent for AnySecondaryUnit {
    fn primary_ident(&self) -> &Ident {
        match self {
            AnySecondaryUnit::Architecture(unit) => unit.primary_ident(),
            AnySecondaryUnit::PackageBody(unit) => unit.primary_ident(),
        }
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

impl From<WithPos<Symbol>> for WithPos<Designator> {
    fn from(other: WithPos<Symbol>) -> WithPos<Designator> {
        other.map_into(|sym| sym.into())
    }
}

impl<'a> From<&'a Symbol> for Designator {
    fn from(other: &'a Symbol) -> Designator {
        other.clone().into()
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
    pub fn pos(&self) -> &SrcPos {
        match self {
            SubprogramDeclaration::Function(ref function) => &function.designator.pos,
            SubprogramDeclaration::Procedure(ref procedure) => &procedure.designator.pos,
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
