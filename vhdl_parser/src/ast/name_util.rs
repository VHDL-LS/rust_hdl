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

impl<T: HasPrimaryIdent> HasPrimaryIdent for DesignUnit<T> {
    fn primary_ident(&self) -> &Ident {
        self.unit.primary_ident()
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

// Enum to handle SelectedName and Name by the same code
pub enum SelectedNameRef<'a, T: AsSelectedNameRef<'a, T>> {
    Selected(&'a WithPos<T>, &'a WithPos<WithRef<Designator>>),
    Designator(&'a WithRef<Designator>),
    SelectedAll(&'a WithPos<T>),
    Prefix(&'a WithPos<T>),
    Other,
}

// Trait to handle SelectedName and Name by the same code
pub trait AsSelectedNameRef<'a, T: AsSelectedNameRef<'a, T>> {
    fn as_selected_name_ref(&'a self) -> SelectedNameRef<'a, T>;
}

impl<'a> AsSelectedNameRef<'a, SelectedName> for SelectedName {
    fn as_selected_name_ref(&'a self) -> SelectedNameRef<'a, SelectedName> {
        match self {
            SelectedName::Selected(ref prefix, ref suffix) => {
                SelectedNameRef::Selected(prefix, suffix)
            }
            SelectedName::Designator(ref designator) => SelectedNameRef::Designator(designator),
        }
    }
}

impl<'a> AsSelectedNameRef<'a, Name> for Name {
    fn as_selected_name_ref(&'a self) -> SelectedNameRef<'a, Name> {
        match self {
            Name::Selected(ref prefix, ref suffix) => SelectedNameRef::Selected(prefix, suffix),
            Name::SelectedAll(ref prefix) => SelectedNameRef::SelectedAll(prefix),
            Name::Designator(ref designator) => SelectedNameRef::Designator(designator),
            Name::Indexed(ref prefix, _) => SelectedNameRef::Prefix(prefix),
            Name::Slice(ref prefix, _) => SelectedNameRef::Prefix(prefix),
            Name::Attribute(ref attr) => SelectedNameRef::Prefix(&attr.name),
            Name::FunctionCall(ref fcall) => SelectedNameRef::Prefix(&fcall.name),
            Name::External(_) => SelectedNameRef::Other,
        }
    }
}

// Enum to handle SelectedName and Name by the same code
pub enum SelectedNameRefMut<'a, T: AsSelectedNameRefMut<'a, T>> {
    Selected(&'a mut WithPos<T>, &'a mut WithPos<WithRef<Designator>>),
    Designator(&'a mut WithRef<Designator>),
    SelectedAll(&'a mut WithPos<T>),
    Prefix(&'a mut WithPos<T>),
    Other,
}

// Trait to handle SelectedName and Name by the same code
pub trait AsSelectedNameRefMut<'a, T: AsSelectedNameRefMut<'a, T>> {
    fn as_selected_name_ref_mut(&'a mut self) -> SelectedNameRefMut<'a, T>;
}

impl<'a> AsSelectedNameRefMut<'a, SelectedName> for SelectedName {
    fn as_selected_name_ref_mut(&'a mut self) -> SelectedNameRefMut<'a, SelectedName> {
        match self {
            SelectedName::Selected(ref mut prefix, ref mut suffix) => {
                SelectedNameRefMut::Selected(prefix, suffix)
            }
            SelectedName::Designator(ref mut designator) => {
                SelectedNameRefMut::Designator(designator)
            }
        }
    }
}

impl<'a> AsSelectedNameRefMut<'a, Name> for Name {
    fn as_selected_name_ref_mut(&'a mut self) -> SelectedNameRefMut<'a, Name> {
        match self {
            Name::Selected(ref mut prefix, ref mut suffix) => {
                SelectedNameRefMut::Selected(prefix, suffix)
            }
            Name::SelectedAll(ref mut prefix) => SelectedNameRefMut::SelectedAll(prefix),
            Name::Designator(ref mut designator) => SelectedNameRefMut::Designator(designator),
            Name::Indexed(ref mut prefix, _) => SelectedNameRefMut::Prefix(prefix),
            Name::Slice(ref mut prefix, _) => SelectedNameRefMut::Prefix(prefix),
            Name::Attribute(ref mut attr) => SelectedNameRefMut::Prefix(&mut attr.name),
            Name::FunctionCall(ref mut fcall) => SelectedNameRefMut::Prefix(&mut fcall.name),
            Name::External(_) => SelectedNameRefMut::Other,
        }
    }
}
