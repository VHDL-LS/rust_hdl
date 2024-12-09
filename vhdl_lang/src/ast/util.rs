// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

//! Name conversions
use super::*;
use crate::data::error_codes::ErrorCode;
use crate::data::*;
use crate::named_entity::{Concurrent, Sequential};
use crate::TokenSpan;

impl WithTokenSpan<Name> {
    pub fn suffix_pos(&self) -> TokenSpan {
        match self.item {
            Name::Designator(..) => self.span,
            Name::Selected(_, ref suffix) => suffix.token.into(),
            // @TODO add pos of .all?
            Name::SelectedAll(ref prefix) => prefix.span,
            Name::CallOrIndexed(ref fcall) => fcall.name.span,
            Name::Slice(ref prefix, ..) => prefix.span,
            Name::Attribute(ref attr, ..) => attr.name.span,
            Name::External(..) => self.span,
        }
    }
}

pub fn to_simple_name(ctx: &dyn TokenAccess, name: WithTokenSpan<Name>) -> DiagnosticResult<Ident> {
    match name.item {
        Name::Designator(WithRef {
            item: Designator::Identifier(ident),
            ..
        }) => Ok(WithToken {
            item: ident,
            token: name.span.start_token,
        }),
        _ => Err(Diagnostic::new(
            name.span.pos(ctx),
            "Expected simple name",
            ErrorCode::SyntaxError,
        )),
    }
}

pub fn as_simple_name_mut(name: &mut Name) -> Option<&mut WithRef<Designator>> {
    match name {
        Name::Designator(
            des @ WithRef {
                item: Designator::Identifier(_),
                ..
            },
        ) => Some(des),
        _ => None,
    }
}

pub fn as_name_mut(expr: &mut Expression) -> Option<&mut Name> {
    match expr {
        Expression::Name(name) => Some(name.as_mut()),
        _ => None,
    }
}

pub trait HasDesignator {
    fn designator(&self) -> &Designator;
}

impl<T: HasDesignator> HasDesignator for WithTokenSpan<T> {
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

impl HasIdent for WithRef<Ident> {
    fn ident(&self) -> &Ident {
        &self.item
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

impl WithTokenSpan<Designator> {
    pub fn into_ref(self) -> WithTokenSpan<WithRef<Designator>> {
        self.map_into(|name| name.into_ref())
    }
}

impl WithToken<Designator> {
    pub fn into_ref(self) -> WithToken<WithRef<Designator>> {
        self.map_into(|name| name.into_ref())
    }
}

pub trait HasIdent {
    fn ident(&self) -> &Ident;
    fn name(&self) -> &Symbol {
        &self.ident().item
    }

    fn ident_pos<'a>(&'a self, ctx: &'a dyn TokenAccess) -> &'a SrcPos {
        self.ident().pos(ctx)
    }
}

impl HasIdent for Ident {
    fn ident(&self) -> &Ident {
        self
    }
}

impl<T: HasIdent> HasIdent for WithDecl<T> {
    fn ident(&self) -> &Ident {
        self.tree.ident()
    }
}

impl HasIdent for EntityDeclaration {
    fn ident(&self) -> &Ident {
        self.ident.ident()
    }
}

impl HasIdent for PackageDeclaration {
    fn ident(&self) -> &Ident {
        self.ident.ident()
    }
}

impl HasIdent for PackageBody {
    fn ident(&self) -> &Ident {
        &self.ident.tree
    }
}

impl HasIdent for ArchitectureBody {
    fn ident(&self) -> &Ident {
        self.ident.ident()
    }
}

impl HasIdent for PackageInstantiation {
    fn ident(&self) -> &Ident {
        self.ident.ident()
    }
}

impl HasIdent for ContextDeclaration {
    fn ident(&self) -> &Ident {
        self.ident.ident()
    }
}

impl HasIdent for ConfigurationDeclaration {
    fn ident(&self) -> &Ident {
        self.ident.ident()
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

impl<'a, T: HasIdent> From<&'a T> for WithToken<Designator> {
    fn from(other: &'a T) -> WithToken<Designator> {
        other.ident().to_owned().map_into(Designator::Identifier)
    }
}

/// Primary identifier in secondary units
pub trait HasPrimaryIdent {
    fn primary_ident(&self) -> &Ident;
    fn primary_name(&self) -> &Symbol {
        &self.primary_ident().item
    }
}

impl HasPrimaryIdent for ArchitectureBody {
    fn primary_ident(&self) -> &Ident {
        &self.entity_name.item
    }
}

impl HasPrimaryIdent for PackageBody {
    fn primary_ident(&self) -> &Ident {
        &self.ident.tree
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

impl From<WithTokenSpan<Symbol>> for WithTokenSpan<Designator> {
    fn from(other: WithTokenSpan<Symbol>) -> WithTokenSpan<Designator> {
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

impl SubprogramSpecification {
    pub fn token(&self) -> TokenId {
        match self {
            SubprogramSpecification::Function(ref function) => function.designator.tree.token,
            SubprogramSpecification::Procedure(ref procedure) => procedure.designator.tree.token,
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

    pub fn describe(&self) -> String {
        match self {
            Designator::Character(chr) => format!("'{chr}'"),
            Designator::Identifier(ident) => format!("'{ident}'"),
            Designator::OperatorSymbol(op) => format!("operator \"{op}\""),
            Designator::Anonymous(_) => "<anonymous>".to_owned(),
        }
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

    // Get an already set suffix reference such as when an ambiguous overloaded call has already been resolved
    pub fn get_suffix_reference(&self) -> Option<EntityId> {
        match self {
            Name::Designator(suffix) => suffix.reference.get(),
            Name::Selected(_, suffix) => suffix.item.reference.get(),
            _ => None,
        }
    }

    pub fn prefix(&self) -> Option<&Designator> {
        match self {
            Self::Attribute(attr) => attr.name.item.prefix(),
            Self::Designator(d) => Some(d.designator()),
            Self::External(..) => None,
            Self::CallOrIndexed(fcall) => fcall.name.item.prefix(),
            Self::SelectedAll(name) => name.item.prefix(),
            Self::Selected(name, ..) => name.item.prefix(),
            Self::Slice(name, ..) => name.item.prefix(),
        }
    }

    /// Returns true if the name is purely a selected name
    /// Example: a.b.c
    pub fn is_selected_name(&self) -> bool {
        match self {
            Name::Designator(_) => true,
            Name::Selected(prefix, _) => prefix.item.is_selected_name(),
            _ => false,
        }
    }
}

impl CallOrIndexed {
    // During parsing function calls and indexed names are ambiguous
    // Thus we convert function calls to indexed names during the analysis stage
    pub fn as_indexed(&mut self) -> Option<IndexedName<'_>> {
        if !self.could_be_indexed_name() {
            return None;
        }

        let CallOrIndexed {
            ref mut name,
            ref mut parameters,
        } = self;

        let mut indexes: Vec<Index<'_>> = Vec::with_capacity(parameters.items.len());

        for elem in parameters.items.iter_mut() {
            if let ActualPart::Expression(ref mut expr) = &mut elem.actual.item {
                indexes.push(Index {
                    pos: elem.actual.span,
                    expr,
                });
            }
        }

        Some(IndexedName { name, indexes })
    }

    pub fn could_be_indexed_name(&self) -> bool {
        self.parameters
            .items
            .iter()
            .all(|assoc| assoc.formal.is_none() && !matches!(assoc.actual.item, ActualPart::Open))
    }
}

pub struct IndexedName<'a> {
    pub name: &'a mut WithTokenSpan<Name>,
    pub indexes: Vec<Index<'a>>,
}

pub struct Index<'a> {
    pub pos: TokenSpan,
    pub expr: &'a mut Expression,
}

impl AttributeName {
    pub fn as_range(&self) -> Option<RangeAttribute> {
        if let AttributeDesignator::Range(r) = self.attr.item {
            Some(r)
        } else {
            None
        }
    }

    pub fn as_type(&self) -> Option<TypeAttribute> {
        if self.signature.is_none() && self.expr.is_none() {
            if let AttributeDesignator::Type(t) = self.attr.item {
                Some(t)
            } else {
                None
            }
        } else {
            None
        }
    }
}

impl RangeConstraint {
    pub fn span(&self) -> TokenSpan {
        self.left_expr.span.combine(self.right_expr.span)
    }
}

impl crate::ast::Range {
    pub fn span(&self) -> TokenSpan {
        use crate::ast::Range::*;
        match self {
            Range(constraint) => constraint.span(),
            Attribute(attr) => attr.name.span.end_with(attr.attr.token),
        }
    }
}

impl DiscreteRange {
    pub fn span(&self) -> TokenSpan {
        match self {
            DiscreteRange::Discrete(type_mark, _) => type_mark.span,
            DiscreteRange::Range(range) => range.span(),
        }
    }
}

impl SubprogramSpecification {
    pub fn subpgm_designator(&self) -> &WithToken<SubprogramDesignator> {
        match self {
            SubprogramSpecification::Procedure(s) => &s.designator.tree,
            SubprogramSpecification::Function(s) => &s.designator.tree,
        }
    }

    pub fn reference_mut(&mut self) -> &mut Reference {
        match self {
            SubprogramSpecification::Function(ref mut function) => &mut function.designator.decl,
            SubprogramSpecification::Procedure(ref mut procedure) => &mut procedure.designator.decl,
        }
    }
}

impl SubprogramDeclaration {
    pub fn subpgm_designator(&self) -> &WithToken<SubprogramDesignator> {
        self.specification.subpgm_designator()
    }

    pub fn reference_mut(&mut self) -> &mut Reference {
        self.specification.reference_mut()
    }
}

impl ConcurrentStatement {
    pub fn label_typ(&self) -> Option<Concurrent> {
        use ConcurrentStatement::*;
        match self {
            ProcedureCall(_) => None,
            Block(_) => Some(Concurrent::Block),
            Process(_) => Some(Concurrent::Process),
            Assert(_) => None,
            Assignment(_) => None,
            Instance(_) => Some(Concurrent::Instance),
            ForGenerate(_) | IfGenerate(_) | CaseGenerate(_) => Some(Concurrent::Generate),
        }
    }

    pub fn end_label_pos(&self) -> Option<&SrcPos> {
        use ConcurrentStatement::*;

        match self {
            ProcedureCall(_) => None,
            Block(value) => value.end_label_pos.as_ref(),
            Process(value) => value.end_label_pos.as_ref(),
            Assert(_) => None,
            Assignment(_) => None,
            Instance(_) => None,
            ForGenerate(value) => value.end_label_pos.as_ref(),
            IfGenerate(value) => value.end_label_pos.as_ref(),
            CaseGenerate(value) => value.end_label_pos.as_ref(),
        }
    }

    pub fn can_have_label(&self) -> bool {
        self.label_typ().is_some()
    }
}

impl SequentialStatement {
    pub fn label_typ(&self) -> Option<Sequential> {
        use SequentialStatement::*;
        match self {
            Wait(_) => None,
            Assert(_) => None,
            Report(_) => None,
            VariableAssignment(_) => None,
            SignalAssignment(_) => None,
            SignalForceAssignment(_) => None,
            SignalReleaseAssignment(_) => None,
            ProcedureCall(_) => None,
            If(_) => Some(Sequential::If),
            Case(_) => Some(Sequential::Case),
            Loop(_) => Some(Sequential::Loop),
            Next(_) => None,
            Exit(_) => None,
            Return(_) => None,
            Null => None,
        }
    }

    pub fn end_label_pos(&self) -> Option<&SrcPos> {
        use SequentialStatement::*;
        match self {
            Wait(_) => None,
            Assert(_) => None,
            Report(_) => None,
            VariableAssignment(_) => None,
            SignalAssignment(_) => None,
            SignalForceAssignment(_) => None,
            SignalReleaseAssignment(_) => None,
            ProcedureCall(_) => None,
            If(value) => value.end_label_pos.as_ref(),
            Case(value) => value.end_label_pos.as_ref(),
            Loop(value) => value.end_label_pos.as_ref(),
            Next(_) => None,
            Exit(_) => None,
            Return(_) => None,
            Null => None,
        }
    }

    pub fn can_have_label(&self) -> bool {
        self.label_typ().is_some()
    }
}
