// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

//! Name conversions
use super::*;
use crate::data::*;

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

impl WithPos<SelectedName> {
    pub fn suffix_pos(&self) -> &SrcPos {
        match self.item {
            SelectedName::Designator(..) => &self.pos,
            SelectedName::Selected(_, ref suffix) => &suffix.pos,
        }
    }
}

impl WithPos<Name> {
    pub fn suffix_pos(&self) -> &SrcPos {
        match self.item {
            Name::Designator(..) => &self.pos,
            Name::Selected(_, ref suffix) => &suffix.pos,
            // @TODO add pos of .all?
            Name::SelectedAll(ref prefix) => &prefix.pos,
            Name::FunctionCall(ref fcall) => fcall.name.suffix_pos(),
            Name::Indexed(ref prefix, ..) => prefix.suffix_pos(),
            Name::Slice(ref prefix, ..) => prefix.suffix_pos(),
            Name::Attribute(ref attr, ..) => attr.name.suffix_pos(),
            Name::External(..) => &self.pos,
        }
    }
}

pub fn to_simple_name(name: WithPos<Name>) -> DiagnosticResult<Ident> {
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
        self
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
            AnyPrimaryUnit::Entity(ref unit) => unit.ident(),
            AnyPrimaryUnit::Configuration(ref unit) => unit.ident(),
            AnyPrimaryUnit::Package(ref unit) => unit.ident(),
            AnyPrimaryUnit::PackageInstance(ref unit) => unit.ident(),
            AnyPrimaryUnit::Context(ref unit) => unit.ident(),
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

impl Designator {
    pub fn as_identifier(&self) -> Option<&Symbol> {
        if let Designator::Identifier(sym) = self {
            Some(sym)
        } else {
            None
        }
    }

    pub fn expect_identifier(&self) -> &Symbol {
        self.as_identifier().unwrap()
    }
}

impl Name {
    pub fn suffix_reference_mut(&mut self) -> Option<&mut Reference> {
        match self {
            Name::Designator(suffix) => Some(&mut suffix.reference),
            Name::Selected(_, suffix) => Some(&mut suffix.item.reference),
            _ => None,
        }
    }
}

impl FunctionCall {
    // During parsing function calls and indexed names are ambiguous
    // Thus we convert function calls to indexed names during the analysis stage
    pub fn to_indexed(&self) -> Option<Name> {
        let FunctionCall {
            ref name,
            ref parameters,
        } = self;

        assoc_elems_to_indexes(parameters)
            .map(|indexes| Name::Indexed(Box::new(name.clone()), indexes))
    }
}

fn assoc_elem_to_index(assoc_elem: &AssociationElement) -> Option<WithPos<Expression>> {
    if assoc_elem.formal.is_some() {
        return None;
    }

    match assoc_elem.actual.item {
        ActualPart::Open => None,
        ActualPart::Expression(ref expr) => {
            Some(WithPos::new(expr.clone(), assoc_elem.actual.pos.clone()))
        }
    }
}

fn assoc_elems_to_indexes(assoc_elems: &[AssociationElement]) -> Option<Vec<WithPos<Expression>>> {
    let mut result: Vec<WithPos<Expression>> = Vec::with_capacity(assoc_elems.len());

    for elem in assoc_elems.iter() {
        if let Some(expr) = assoc_elem_to_index(elem) {
            result.push(expr);
        } else {
            return None;
        }
    }

    Some(result)
}
