// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2022, Olof Kraigher olof.kraigher@gmail.com

use fnv::FnvHashSet;

use super::analyze::*;
use super::expression::ExpressionType;
use super::named_entity::*;
use super::overloaded::Disambiguated;
use super::overloaded::DisambiguatedType;
use super::overloaded::SubprogramKind;
use super::region::*;
use crate::ast::*;
use crate::data::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ObjectBase<'a> {
    Object(ObjectEnt<'a>),
    ObjectAlias(ObjectEnt<'a>, EntRef<'a>),
    DeferredConstant(EntRef<'a>),
    ExternalName(ExternalObjectClass),
}

impl<'a> ObjectBase<'a> {
    pub fn mode(&self) -> Option<Mode> {
        match self {
            ObjectBase::Object(object) => object.mode(),
            ObjectBase::ObjectAlias(object, _) => object.mode(),
            ObjectBase::DeferredConstant(..) => None,
            ObjectBase::ExternalName(_) => None,
        }
    }

    pub fn class(&self) -> ObjectClass {
        match self {
            ObjectBase::Object(object) => object.class(),
            ObjectBase::ObjectAlias(object, _) => object.class(),
            ObjectBase::DeferredConstant(..) => ObjectClass::Constant,
            ObjectBase::ExternalName(class) => (*class).into(),
        }
    }

    // Use whenever the class and mode is relevant to the error
    pub fn describe_class(&self) -> String {
        if let Some(mode) = self.mode() {
            if self.class() == ObjectClass::Constant {
                format!("interface {}", self.describe())
            } else {
                format!("interface {} of mode {}", self.describe(), mode)
            }
        } else {
            self.describe()
        }
    }

    pub fn describe(&self) -> String {
        match self {
            ObjectBase::DeferredConstant(ent) => {
                format!("deferred constant '{}'", ent.designator())
            }
            ObjectBase::ExternalName(..) => "external name".to_owned(),
            ObjectBase::Object(obj) => obj.describe_name(),
            ObjectBase::ObjectAlias(_, alias) => {
                format!("alias '{}' of {}", alias.designator(), self.class())
            }
        }
    }

    pub fn is_port(&self) -> bool {
        match self {
            ObjectBase::Object(obj) => obj.kind().is_port(),
            ObjectBase::ObjectAlias(obj, _) => obj.kind().is_port(),
            ObjectBase::DeferredConstant(_) => false,
            ObjectBase::ExternalName(_) => false,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct ObjectName<'a> {
    pub base: ObjectBase<'a>,
    pub type_mark: Option<TypeEnt<'a>>,
}

impl<'a> ObjectName<'a> {
    pub fn type_mark(&self) -> TypeEnt<'a> {
        if let Some(type_mark) = self.type_mark {
            type_mark
        } else if let ObjectBase::Object(obj) = self.base {
            obj.type_mark()
        } else {
            unreachable!("No type mark implies object base")
        }
    }

    fn with_suffix(self, type_mark: TypeEnt<'a>) -> Self {
        ObjectName {
            base: self.base,
            type_mark: Some(type_mark),
        }
    }

    /// Use in error messages that focus on the type rather than class/mode
    pub fn describe_type(&self) -> String {
        if let Some(type_mark) = self.type_mark {
            type_mark.describe()
        } else {
            format!(
                "{} of {}",
                self.base.describe(),
                self.type_mark().describe()
            )
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ResolvedName<'a> {
    Library(Symbol),
    Design(DesignEnt<'a>),
    Type(TypeEnt<'a>),
    Overloaded(WithPos<Designator>, OverloadedName<'a>),
    ObjectName(ObjectName<'a>),
    /// The result of a function call and any subsequent selections thereof
    Expression(DisambiguatedType<'a>),
    // Something that cannot be further selected
    Final(EntRef<'a>),
}

impl<'a> ResolvedName<'a> {
    /// The name was selected out of a design unit
    fn from_design_not_overloaded(ent: &'a AnyEnt) -> Result<Self, String> {
        let name = match ent.kind() {
            AnyEntKind::Object(_) => ResolvedName::ObjectName(ObjectName {
                base: ObjectBase::Object(ObjectEnt::from_any(ent).unwrap()),
                type_mark: None,
            }),
            AnyEntKind::ObjectAlias {
                base_object,
                type_mark,
            } => ResolvedName::ObjectName(ObjectName {
                base: ObjectBase::ObjectAlias(*base_object, ent),
                type_mark: Some(type_mark.to_owned()),
            }),
            AnyEntKind::ExternalAlias { class, type_mark } => {
                ResolvedName::ObjectName(ObjectName {
                    base: ObjectBase::ExternalName(*class),
                    type_mark: Some(*type_mark),
                })
            }
            AnyEntKind::DeferredConstant(subtype) => ResolvedName::ObjectName(ObjectName {
                base: ObjectBase::DeferredConstant(ent),
                type_mark: Some(subtype.type_mark()),
            }),
            AnyEntKind::Type(_) => ResolvedName::Type(TypeEnt::from_any(ent).unwrap()),
            AnyEntKind::Overloaded(_) => {
                return Err(
                    "Internal error. Unreachable as overloaded is handled outside".to_owned(),
                )
            }
            AnyEntKind::File(_)
            | AnyEntKind::InterfaceFile(_)
            | AnyEntKind::Component(_)
            | AnyEntKind::PhysicalLiteral(_) => ResolvedName::Final(ent),
            AnyEntKind::Design(_)
            | AnyEntKind::Library
            | AnyEntKind::Attribute(_)
            | AnyEntKind::ElementDeclaration(_)
            | AnyEntKind::Concurrent(_)
            | AnyEntKind::Sequential(_)
            | AnyEntKind::LoopParameter(_) => {
                return Err(format!(
                    "{} cannot be selected from design unit",
                    ent.kind().describe()
                ))
            }
        };

        Ok(name)
    }

    /// The name was looked up from the current scope
    fn from_scope_not_overloaded(ent: &'a AnyEnt) -> Result<Self, String> {
        let name = match ent.kind() {
            AnyEntKind::Object(_) => ResolvedName::ObjectName(ObjectName {
                base: ObjectBase::Object(ObjectEnt::from_any(ent).unwrap()),
                type_mark: None,
            }),
            AnyEntKind::ObjectAlias {
                base_object,
                type_mark,
            } => ResolvedName::ObjectName(ObjectName {
                base: ObjectBase::ObjectAlias(*base_object, ent),
                type_mark: Some(type_mark.to_owned()),
            }),
            AnyEntKind::ExternalAlias { class, type_mark } => {
                ResolvedName::ObjectName(ObjectName {
                    base: ObjectBase::ExternalName(*class),
                    type_mark: Some(*type_mark),
                })
            }
            AnyEntKind::DeferredConstant(subtype) => ResolvedName::ObjectName(ObjectName {
                base: ObjectBase::DeferredConstant(ent),
                type_mark: Some(subtype.type_mark()),
            }),
            AnyEntKind::Type(_) => ResolvedName::Type(TypeEnt::from_any(ent).unwrap()),
            AnyEntKind::Design(_) => ResolvedName::Design(DesignEnt::from_any(ent).unwrap()),
            AnyEntKind::Library => {
                ResolvedName::Library(ent.designator().as_identifier().cloned().unwrap())
            }
            AnyEntKind::Overloaded(_) => {
                return Err(
                    "Internal error. Unreachable as overloded is handled outside this function"
                        .to_string(),
                )
            }
            AnyEntKind::File(_)
            | AnyEntKind::InterfaceFile(_)
            | AnyEntKind::Component(_)
            | AnyEntKind::Concurrent(_)
            | AnyEntKind::Sequential(_)
            | AnyEntKind::LoopParameter(_)
            | AnyEntKind::PhysicalLiteral(_) => ResolvedName::Final(ent),
            AnyEntKind::Attribute(_) | AnyEntKind::ElementDeclaration(_) => {
                return Err(format!(
                    "{} should never be looked up from the current scope",
                    ent.kind().describe()
                ))
            }
        };

        Ok(name)
    }

    /// A description that includes the type of the name
    /// This is used in contexts where the type is relevant to the error
    pub fn describe_type(&self) -> String {
        match self {
            ResolvedName::ObjectName(oname) => oname.describe_type(),
            ResolvedName::Expression(DisambiguatedType::Unambiguous(typ)) => {
                format!("Expression of {}", typ.describe())
            }
            _ => self.describe(),
        }
    }

    /// A description that does not include the name of the type
    /// This is used in contexts where the type is not relevant
    /// Such as when assigning to a constant
    pub fn describe(&self) -> String {
        match self {
            ResolvedName::Library(sym) => format!("library {sym}"),
            ResolvedName::Design(ent) => ent.describe(),
            ResolvedName::Type(ent) => ent.describe(),
            ResolvedName::Overloaded(des, name) => {
                if let Some(ent) = name.as_unique() {
                    ent.describe()
                } else {
                    format!("Overloaded name {des}")
                }
            }
            ResolvedName::ObjectName(oname) => oname.base.describe(),
            ResolvedName::Final(ent) => ent.describe(),
            ResolvedName::Expression(DisambiguatedType::Unambiguous(_)) => "Expression".to_owned(),
            ResolvedName::Expression(_) => "Ambiguous expression".to_owned(),
        }
    }

    fn type_mark(&self) -> Option<TypeEnt<'a>> {
        match self {
            ResolvedName::Type(typ) => Some(*typ),
            ResolvedName::ObjectName(oname) => Some(oname.type_mark()),
            ResolvedName::Expression(DisambiguatedType::Unambiguous(typ)) => Some(*typ),
            _ => None,
        }
    }

    pub(crate) fn as_type_of_attr_prefix(
        &self,
        prefix_pos: &SrcPos,
        attr: &AttributeSuffix,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<TypeEnt<'a>> {
        if let Some(typ) = self.type_mark() {
            Ok(typ)
        } else {
            diagnostics.push(Diagnostic::cannot_be_prefix_of_attribute(
                prefix_pos, self, attr,
            ));
            Err(EvalError::Unknown)
        }
    }

    fn as_type_of_signal_attr_prefix(
        &self,
        prefix_pos: &SrcPos,
        attr: &AttributeSuffix,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<TypeEnt<'a>> {
        if let ResolvedName::ObjectName(oname) = self {
            if matches!(oname.base.class(), ObjectClass::Signal) {
                return Ok(oname.type_mark());
            }
        }

        diagnostics.error(
            prefix_pos,
            format!(
                "Expected signal prefix for '{} attribute, got {}",
                attr.attr,
                self.describe()
            ),
        );
        Err(EvalError::Unknown)
    }
}
#[derive(Debug)]
pub struct AttributeSuffix<'a> {
    pub signature: &'a mut Option<WithPos<crate::ast::Signature>>,
    pub attr: &'a mut WithPos<AttributeDesignator>,
    pub expr: &'a mut Option<Box<WithPos<Expression>>>,
}

#[derive(Debug)]
enum Suffix<'a> {
    Selected(&'a mut WithPos<WithRef<Designator>>),
    All,
    Slice(&'a mut DiscreteRange),
    Attribute(AttributeSuffix<'a>),
    CallOrIndexed(&'a mut [AssociationElement]),
}

enum SplitName<'a> {
    Designator(&'a mut WithRef<Designator>),
    External(&'a mut ExternalName),
    Suffix(&'a mut WithPos<Name>, Suffix<'a>),
}

impl<'a> SplitName<'a> {
    fn from_name(name: &'a mut Name) -> SplitName<'a> {
        match name {
            Name::Designator(d) => SplitName::Designator(d),
            Name::External(e) => SplitName::External(e),
            Name::Selected(prefix, suffix) => {
                SplitName::Suffix(prefix.as_mut(), Suffix::Selected(suffix))
            }
            Name::SelectedAll(ref mut prefix) => SplitName::Suffix(prefix.as_mut(), Suffix::All),
            Name::Slice(ref mut prefix, range) => {
                SplitName::Suffix(prefix.as_mut(), Suffix::Slice(range))
            }
            Name::Attribute(ref mut attr) => SplitName::Suffix(
                &mut attr.name,
                Suffix::Attribute(AttributeSuffix {
                    signature: &mut attr.signature,
                    attr: &mut attr.attr,
                    expr: &mut attr.expr,
                }),
            ),
            Name::CallOrIndexed(ref mut fcall) => SplitName::Suffix(
                &mut fcall.name,
                Suffix::CallOrIndexed(&mut fcall.parameters),
            ),
        }
    }
}

enum TypeOrMethod<'a> {
    Type(TypeEnt<'a>),
    Method(WithPos<Designator>, OverloadedName<'a>),
}

fn could_be_indexed_name(assocs: &[AssociationElement]) -> bool {
    assocs
        .iter()
        .all(|assoc| assoc.formal.is_none() && !matches!(assoc.actual.item, ActualPart::Open))
}

pub fn as_type_conversion(assocs: &mut [AssociationElement]) -> Option<(&SrcPos, &mut Expression)> {
    if assocs.len() == 1 && could_be_indexed_name(assocs) {
        if let ActualPart::Expression(ref mut expr) = assocs[0].actual.item {
            return Some((&assocs[0].actual.pos, expr));
        }
    }
    None
}

impl<'a> AnalyzeContext<'a> {
    fn name_to_type(
        &self,
        pos: &SrcPos,
        // Reference to set if overloaded name was disambiguated
        reference: Option<&mut Reference>,
        name: ResolvedName<'a>,
    ) -> Result<Option<DisambiguatedType<'a>>, Diagnostic> {
        match name {
            ResolvedName::Library(_) | ResolvedName::Design(_) | ResolvedName::Type(_) => {
                Err(Diagnostic::error(
                    pos,
                    format!("{} cannot be used in an expression", name.describe_type()),
                ))
            }
            ResolvedName::Final(ent) => match ent.actual_kind() {
                AnyEntKind::LoopParameter(typ) => {
                    Ok(typ.map(|typ| DisambiguatedType::Unambiguous(typ.into())))
                }
                AnyEntKind::PhysicalLiteral(typ) => Ok(Some(DisambiguatedType::Unambiguous(*typ))),
                AnyEntKind::File(subtype) => {
                    Ok(Some(DisambiguatedType::Unambiguous(subtype.type_mark())))
                }
                AnyEntKind::InterfaceFile(typ) => Ok(Some(DisambiguatedType::Unambiguous(*typ))),
                _ => Err(Diagnostic::error(
                    pos,
                    format!("{} cannot be used in an expression", name.describe_type()),
                )),
            },
            ResolvedName::Overloaded(des, overloaded) => {
                if let Some(disamb) = self.disambiguate_no_actuals(&des, None, &overloaded)? {
                    if let Disambiguated::Unambiguous(ref ent) = disamb {
                        if let Some(reference) = reference {
                            *reference = Some(ent.id());
                        }
                    }
                    Ok(Some(disamb.into_type()))
                } else {
                    Ok(None)
                }
            }
            ResolvedName::ObjectName(oname) => {
                Ok(Some(DisambiguatedType::Unambiguous(oname.type_mark())))
            }
            ResolvedName::Expression(expr_type) => Ok(Some(expr_type)),
        }
    }

    fn name_to_unambiguous_type(
        &self,
        pos: &SrcPos,
        name: &ResolvedName<'a>,
        ttyp: TypeEnt<'a>,
        // Optional reference to set when disambiguating overloaded
        suffix_ref: Option<&mut Reference>,
    ) -> Result<Option<TypeEnt<'a>>, Diagnostic> {
        match name {
            ResolvedName::Library(_) | ResolvedName::Design(_) | ResolvedName::Type(_) => {
                Err(Diagnostic::error(
                    pos,
                    format!("{} cannot be used in an expression", name.describe_type()),
                ))
            }
            ResolvedName::Final(ent) => match ent.actual_kind() {
                AnyEntKind::LoopParameter(typ) => Ok(typ.map(|typ| typ.into())),
                AnyEntKind::PhysicalLiteral(typ) => Ok(Some(*typ)),
                AnyEntKind::File(subtype) => Ok(Some(subtype.type_mark())),
                AnyEntKind::InterfaceFile(typ) => Ok(Some(*typ)),
                _ => Err(Diagnostic::error(
                    pos,
                    format!("{} cannot be used in an expression", name.describe_type()),
                )),
            },
            ResolvedName::Overloaded(des, overloaded) => {
                if let Some(disamb) = self.disambiguate_no_actuals(des, Some(ttyp), overloaded)? {
                    match disamb {
                        Disambiguated::Unambiguous(ent) => {
                            if let Some(reference) = suffix_ref {
                                *reference = Some(ent.id());
                            }
                            Ok(Some(ent.return_type().unwrap()))
                        }
                        Disambiguated::Ambiguous(overloaded) => {
                            Err(Diagnostic::ambiguous_call(des, overloaded))
                        }
                    }
                } else {
                    Ok(None)
                }
            }
            ResolvedName::ObjectName(oname) => Ok(Some(oname.type_mark())),
            ResolvedName::Expression(DisambiguatedType::Unambiguous(typ)) => Ok(Some(*typ)),
            ResolvedName::Expression(DisambiguatedType::Ambiguous(_)) => {
                // Error reported elsewhere
                Ok(None)
            }
        }
    }

    /// An array type may be sliced with a type name
    /// For the parser this looks like a call or indexed name
    /// Example:
    /// subtype sub_t is natural range 0 to 1;
    /// arr(sub_t) := (others => 0);
    fn assoc_as_discrete_range_type(
        &self,
        scope: &Scope<'a>,
        assocs: &mut [AssociationElement],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> AnalysisResult<Option<TypeEnt<'a>>> {
        if !could_be_indexed_name(assocs) {
            return Ok(None);
        }

        if let [ref mut assoc] = assocs {
            if let ActualPart::Expression(expr) = &mut assoc.actual.item {
                return self.expr_as_discrete_range_type(
                    scope,
                    &assoc.actual.pos,
                    expr,
                    diagnostics,
                );
            }
        }
        Ok(None)
    }

    pub fn expr_as_discrete_range_type(
        &self,
        scope: &Scope<'a>,
        expr_pos: &SrcPos,
        expr: &mut Expression,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> AnalysisResult<Option<TypeEnt<'a>>> {
        if let Expression::Name(name) = expr {
            if let Name::Attribute(ref mut attr) = name.as_mut() {
                if attr.as_range().is_some() {
                    return if let Some(typ) =
                        as_fatal(self.range_attribute_type(scope, attr.as_mut(), diagnostics))?
                    {
                        Ok(Some(typ.into()))
                    } else {
                        Ok(None)
                    };
                }
            }

            if !name.is_selected_name() {
                // Early exit
                return Ok(None);
            }

            let resolved = as_fatal(self.name_resolve(scope, expr_pos, name, diagnostics))?;

            if let Some(ResolvedName::Type(typ)) = resolved {
                return if matches!(typ.base_type().kind(), Type::Enum { .. } | Type::Integer) {
                    Ok(Some(typ))
                } else {
                    Err(Diagnostic::error(
                        expr_pos,
                        format!("{} cannot be used as a discrete range", typ.describe()),
                    )
                    .into())
                };
            }
        }

        Ok(None)
    }

    // Apply suffix when prefix is known to have a type
    // The prefix may be an object or a function return value
    fn resolve_typed_suffix(
        &self,
        scope: &Scope<'a>,
        prefix_pos: &SrcPos,
        name_pos: &SrcPos,
        prefix_typ: TypeEnt<'a>,
        suffix: &mut Suffix,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> AnalysisResult<Option<TypeOrMethod<'a>>> {
        match suffix {
            Suffix::Selected(suffix) => {
                Ok(Some(match prefix_typ.selected(prefix_pos, suffix)? {
                    TypedSelection::RecordElement(elem) => {
                        suffix.set_unique_reference(&elem);
                        TypeOrMethod::Type(elem.type_mark())
                    }
                    TypedSelection::ProtectedMethod(name) => TypeOrMethod::Method(
                        WithPos::new(suffix.item.item.clone(), suffix.pos.clone()),
                        name,
                    ),
                }))
            }
            Suffix::All => Ok(prefix_typ.accessed_type().map(TypeOrMethod::Type)),
            Suffix::Slice(drange) => Ok(if let Some(typ) = prefix_typ.sliced_as() {
                if let Type::Array { indexes, .. } = typ.kind() {
                    if let [idx_typ] = indexes.as_slice() {
                        if let Some(idx_typ) = *idx_typ {
                            self.drange_with_ttyp(scope, idx_typ.into(), drange, diagnostics)?;
                        } else {
                            self.drange_unknown_type(scope, drange, diagnostics)?;
                        }
                    } else {
                        diagnostics.error(
                            name_pos,
                            format!(
                                "Cannot slice {}-dimensional {}",
                                indexes.len(),
                                typ.describe()
                            ),
                        )
                    }
                }
                Some(TypeOrMethod::Type(typ))
            } else {
                None
            }),
            // @TODO attribute is handled elesewhere
            Suffix::Attribute(_) => Ok(None),
            // @TODO Prefix must non-overloaded
            Suffix::CallOrIndexed(assocs) => {
                if let Some(typ) = prefix_typ.sliced_as() {
                    if self
                        .assoc_as_discrete_range_type(scope, assocs, diagnostics)?
                        .is_some()
                    {
                        return Ok(Some(TypeOrMethod::Type(typ)));
                    }
                }

                if could_be_indexed_name(assocs) {
                    if let Some((elem_type, indexes)) = prefix_typ.array_type() {
                        for (idx, AssociationElement { actual, .. }) in
                            assocs.iter_mut().enumerate()
                        {
                            if let ActualPart::Expression(ref mut expr) = actual.item {
                                if let Some(ttyp) = indexes.get(idx) {
                                    if let Some(ttyp) = *ttyp {
                                        self.expr_pos_with_ttyp(
                                            scope,
                                            ttyp.into(),
                                            &actual.pos,
                                            expr,
                                            diagnostics,
                                        )?;
                                    } else {
                                        self.expr_pos_unknown_ttyp(
                                            scope,
                                            &actual.pos,
                                            expr,
                                            diagnostics,
                                        )?;
                                    }
                                }
                            }
                        }

                        let num_indexes = indexes.len();
                        if assocs.len() != num_indexes {
                            Err(Diagnostic::dimension_mismatch(
                                name_pos,
                                prefix_typ,
                                assocs.len(),
                                num_indexes,
                            )
                            .into())
                        } else {
                            Ok(Some(TypeOrMethod::Type(elem_type)))
                        }
                    } else {
                        Ok(None)
                    }
                } else {
                    Ok(None)
                }
            }
        }
    }

    // Resolve an index used in an array attribute such as arr_t'left(0) to an index type
    pub(crate) fn array_index_expression_in_attribute(
        &self,
        indexes: &[Option<BaseType<'a>>],
        mut expr: Option<&mut WithPos<Expression>>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<BaseType<'a>> {
        let idx = if let Some(expr) = expr.as_mut() {
            if let Expression::Literal(Literal::AbstractLiteral(AbstractLiteral::Integer(idx))) =
                expr.item
            {
                idx as usize
            } else {
                diagnostics.error(&expr.pos, "Expected an integer literal");
                return Err(EvalError::Unknown);
            }
        } else {
            1
        };

        if let Some(idx_typ) = indexes.get(idx - 1) {
            if let Some(idx_typ) = idx_typ {
                Ok(*idx_typ)
            } else {
                // Array index was not analyzed
                Err(EvalError::Unknown)
            }
        } else {
            if let Some(expr) = expr {
                let ndims = indexes.len();
                let dimensions = plural("dimension", "dimensions", ndims);
                diagnostics.error(&expr.pos, format!("Index {idx} out of range for array with {ndims} {dimensions}, expected 1 to {ndims}"));
            }
            Err(EvalError::Unknown)
        }
    }

    pub fn attribute_suffix(
        &self,
        name_pos: &SrcPos,
        prefix_pos: &SrcPos,
        scope: &Scope<'a>,
        prefix: &ResolvedName<'a>,
        attr: &mut AttributeSuffix,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<BaseType<'a>> {
        match attr.attr.item {
            AttributeDesignator::Left
            | AttributeDesignator::Right
            | AttributeDesignator::High
            | AttributeDesignator::Low => {
                let typ = prefix.as_type_of_attr_prefix(prefix_pos, attr, diagnostics)?;

                if let Some((_, indexes)) = typ.array_type() {
                    self.array_index_expression_in_attribute(
                        indexes,
                        attr.expr.as_mut().map(|expr| expr.as_mut()),
                        diagnostics,
                    )
                } else if typ.is_scalar() {
                    check_no_attr_argument(attr, diagnostics);
                    Ok(typ.into())
                } else {
                    diagnostics.push(Diagnostic::cannot_be_prefix_of_attribute(
                        name_pos, prefix, attr,
                    ));
                    Err(EvalError::Unknown)
                }
            }
            AttributeDesignator::Ascending | AttributeDesignator::Descending => {
                let typ = prefix.as_type_of_attr_prefix(prefix_pos, attr, diagnostics)?;

                if typ.array_type().is_some() {
                    Ok(self.boolean().base())
                } else if typ.is_scalar() {
                    check_no_attr_argument(attr, diagnostics);
                    Ok(self.boolean().base())
                } else {
                    diagnostics.push(Diagnostic::cannot_be_prefix_of_attribute(
                        name_pos, prefix, attr,
                    ));
                    Err(EvalError::Unknown)
                }
            }
            AttributeDesignator::Image => {
                let typ = prefix.as_type_of_attr_prefix(prefix_pos, attr, diagnostics)?;

                if let Some(ref mut expr) = check_single_argument(name_pos, attr, diagnostics) {
                    self.expr_with_ttyp(scope, typ, expr, diagnostics)?;
                }

                if typ.is_scalar() {
                    Ok(self.string().base())
                } else {
                    diagnostics.push(Diagnostic::cannot_be_prefix_of_attribute(
                        name_pos, prefix, attr,
                    ));
                    Err(EvalError::Unknown)
                }
            }
            AttributeDesignator::Value => {
                let typ = prefix.as_type_of_attr_prefix(prefix_pos, attr, diagnostics)?;

                if let Some(ref mut expr) = check_single_argument(name_pos, attr, diagnostics) {
                    self.expr_with_ttyp(scope, self.string(), expr, diagnostics)?;
                }

                if typ.is_scalar() {
                    Ok(typ.base())
                } else {
                    diagnostics.push(Diagnostic::cannot_be_prefix_of_attribute(
                        name_pos, prefix, attr,
                    ));
                    Err(EvalError::Unknown)
                }
            }
            AttributeDesignator::Pos => {
                let typ = prefix.as_type_of_attr_prefix(prefix_pos, attr, diagnostics)?;

                if typ.base().is_discrete() {
                    if let Some(ref mut expr) = check_single_argument(name_pos, attr, diagnostics) {
                        self.expr_with_ttyp(scope, typ, expr, diagnostics)?;
                    }
                    Ok(self.universal_integer())
                } else {
                    diagnostics.push(Diagnostic::cannot_be_prefix_of_attribute(
                        name_pos, prefix, attr,
                    ));
                    Err(EvalError::Unknown)
                }
            }
            AttributeDesignator::Val => {
                let typ = prefix.as_type_of_attr_prefix(prefix_pos, attr, diagnostics)?;

                if typ.base().is_discrete() {
                    if let Some(ref mut expr) = check_single_argument(name_pos, attr, diagnostics) {
                        self.integer_expr(scope, expr, diagnostics)?;
                    }
                    Ok(typ.base())
                } else {
                    diagnostics.push(Diagnostic::cannot_be_prefix_of_attribute(
                        name_pos, prefix, attr,
                    ));
                    Err(EvalError::Unknown)
                }
            }
            AttributeDesignator::Succ
            | AttributeDesignator::Pred
            | AttributeDesignator::LeftOf
            | AttributeDesignator::RightOf => {
                let typ = prefix.as_type_of_attr_prefix(prefix_pos, attr, diagnostics)?;

                if typ.base().is_discrete() {
                    if let Some(ref mut expr) = check_single_argument(name_pos, attr, diagnostics) {
                        self.expr_with_ttyp(scope, typ, expr, diagnostics)?;
                    }
                    Ok(typ.base())
                } else {
                    diagnostics.push(Diagnostic::cannot_be_prefix_of_attribute(
                        name_pos, prefix, attr,
                    ));
                    Err(EvalError::Unknown)
                }
            }
            AttributeDesignator::Length => {
                let typ = prefix.as_type_of_attr_prefix(prefix_pos, attr, diagnostics)?;

                if typ.array_type().is_some() {
                    Ok(self.universal_integer())
                } else {
                    diagnostics.push(Diagnostic::cannot_be_prefix_of_attribute(
                        name_pos, prefix, attr,
                    ));
                    Err(EvalError::Unknown)
                }
            }
            AttributeDesignator::SimpleName
            | AttributeDesignator::InstanceName
            | AttributeDesignator::PathName => {
                check_no_attr_argument(attr, diagnostics);
                Ok(self.string().base())
            }

            AttributeDesignator::Signal(sattr) => {
                let typ = prefix.as_type_of_signal_attr_prefix(prefix_pos, attr, diagnostics)?;
                let expr = attr.expr.as_mut().map(|expr| expr.as_mut());
                match sattr {
                    SignalAttribute::Delayed => {
                        if let Some(expr) = attr.expr {
                            self.expr_with_ttyp(scope, self.time(), expr, diagnostics)?;
                        }
                        Ok(typ.base())
                    }
                    SignalAttribute::Stable | SignalAttribute::Quiet => {
                        if let Some(expr) = expr {
                            self.expr_with_ttyp(scope, self.time(), expr, diagnostics)?;
                        }
                        Ok(self.boolean().base())
                    }
                    SignalAttribute::Transaction => {
                        check_no_sattr_argument(sattr, expr, diagnostics);
                        Ok(self.bit().base())
                    }
                    SignalAttribute::Event => {
                        check_no_sattr_argument(sattr, expr, diagnostics);
                        Ok(self.boolean().base())
                    }
                    SignalAttribute::Active => {
                        check_no_sattr_argument(sattr, expr, diagnostics);
                        Ok(self.boolean().base())
                    }
                    SignalAttribute::LastEvent => {
                        check_no_sattr_argument(sattr, expr, diagnostics);
                        Ok(self.time().base())
                    }
                    SignalAttribute::LastActive => {
                        check_no_sattr_argument(sattr, expr, diagnostics);
                        Ok(self.time().base())
                    }
                    SignalAttribute::LastValue => {
                        check_no_sattr_argument(sattr, expr, diagnostics);
                        Ok(typ.base())
                    }
                    SignalAttribute::Driving => {
                        check_no_sattr_argument(sattr, expr, diagnostics);
                        Ok(self.boolean().base())
                    }
                    SignalAttribute::DrivingValue => {
                        check_no_sattr_argument(sattr, expr, diagnostics);
                        Ok(typ.base())
                    }
                }
            }

            AttributeDesignator::Ident(_) => {
                diagnostics.error(
                    &attr.attr.pos,
                    format!("Unknown attribute '{}", attr.attr.item),
                );
                Err(EvalError::Unknown)
            }
            AttributeDesignator::Range(_) => {
                diagnostics.error(name_pos, "Range cannot be used as an expression");
                Err(EvalError::Unknown)
            }
            AttributeDesignator::Type(_) => {
                diagnostics.error(name_pos, "Type cannot be used as an expression");
                Err(EvalError::Unknown)
            }
        }
    }

    pub fn name_resolve(
        &self,
        scope: &Scope<'a>,
        name_pos: &SrcPos,
        name: &mut Name,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<ResolvedName<'a>> {
        self.name_resolve_with_suffixes(scope, name_pos, name, None, false, diagnostics)
    }

    fn name_resolve_with_suffixes(
        &self,
        scope: &Scope<'a>,
        name_pos: &SrcPos,
        name: &mut Name,
        ttyp: Option<TypeEnt<'a>>,
        has_suffix: bool,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<ResolvedName<'a>> {
        let mut suffix;
        let prefix;
        let mut resolved = match SplitName::from_name(name) {
            SplitName::Designator(designator) => {
                let name =
                    catch_diagnostic(scope.lookup(name_pos, designator.designator()), diagnostics)?;
                return Ok(match name {
                    NamedEntities::Single(ent) => {
                        designator.set_unique_reference(ent);

                        catch_diagnostic(
                            ResolvedName::from_scope_not_overloaded(ent)
                                .map_err(|e| Diagnostic::error(name_pos, e)),
                            diagnostics,
                        )?
                    }
                    NamedEntities::Overloaded(overloaded) => ResolvedName::Overloaded(
                        WithPos::new(designator.item.clone(), name_pos.clone()),
                        overloaded,
                    ),
                });
            }
            SplitName::External(ename) => {
                let ExternalName { subtype, class, .. } = ename;
                let subtype = catch_analysis_err(
                    self.resolve_subtype_indication(scope, subtype, diagnostics),
                    diagnostics,
                )?;
                return Ok(ResolvedName::ObjectName(ObjectName {
                    base: ObjectBase::ExternalName(*class),
                    type_mark: Some(subtype.type_mark().to_owned()),
                }));
            }
            SplitName::Suffix(p, s) => {
                let resolved = self.name_resolve_with_suffixes(
                    scope,
                    &p.pos,
                    &mut p.item,
                    None,
                    true,
                    diagnostics,
                )?;
                prefix = p;
                suffix = s;
                resolved
            }
        };

        // Any other suffix must collapse overloaded
        if !matches!(suffix, Suffix::CallOrIndexed(_)) {
            if let ResolvedName::Overloaded(ref des, ref overloaded) = resolved {
                let disambiguated = catch_diagnostic(
                    self.disambiguate_no_actuals(
                        des,
                        {
                            // @TODO must be disambiguated with suffixes
                            None
                        },
                        overloaded,
                    ),
                    diagnostics,
                )?;

                if let Some(disambiguated) = disambiguated {
                    match disambiguated {
                        Disambiguated::Ambiguous(ents) => {
                            if let Some(types) = ambiguous_functions_to_types(&ents) {
                                if has_suffix || ttyp.is_some() {
                                    diagnostics.push(Diagnostic::ambiguous_call(des, ents));
                                }
                                resolved =
                                    ResolvedName::Expression(DisambiguatedType::Ambiguous(types));
                            } else {
                                diagnostics.error(
                                    &prefix.pos,
                                    "Procedure calls are not valid in names and expressions",
                                );
                                return Err(EvalError::Unknown);
                            }
                        }
                        Disambiguated::Unambiguous(ent) => {
                            if let Some(typ) = ent.return_type() {
                                resolved =
                                    ResolvedName::Expression(DisambiguatedType::Unambiguous(typ));
                            } else {
                                diagnostics.error(
                                    &prefix.pos,
                                    "Procedure calls are not valid in names and expressions",
                                );
                                return Err(EvalError::Unknown);
                            }
                        }
                    }
                }
            }
        }

        // Attributes for non-types not handled yet
        if let Suffix::Attribute(ref mut attr) = suffix {
            let typ =
                self.attribute_suffix(name_pos, &prefix.pos, scope, &resolved, attr, diagnostics)?;
            return Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                typ.into(),
            )));
        }

        match resolved {
            ResolvedName::Overloaded(ref des, ref overloaded) => {
                if let Suffix::CallOrIndexed(ref mut assocs) = suffix {
                    // @TODO could be overloaded with no arguments that is indexed

                    // @TODO lookup already set reference to get O(N) instead of O(N^2) when disambiguating deeply nested ambiguous calls
                    if let Some(id) = prefix.item.get_suffix_reference() {
                        if let Some(ent) = OverloadedEnt::from_any(self.arena.get(id)) {
                            return Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                                ent.return_type().unwrap(),
                            )));
                        }
                    }

                    match as_fatal(self.disambiguate(
                        scope,
                        name_pos,
                        des,
                        assocs,
                        SubprogramKind::Function(if has_suffix {
                            // @TODO disambiguate based on suffixes
                            None
                        } else {
                            ttyp
                        }),
                        overloaded.entities().collect(),
                        diagnostics,
                    ))? {
                        Some(Disambiguated::Ambiguous(ents)) => {
                            if let Some(types) = ambiguous_functions_to_types(&ents) {
                                if has_suffix || ttyp.is_some() {
                                    diagnostics.push(Diagnostic::ambiguous_call(des, ents));
                                }

                                resolved =
                                    ResolvedName::Expression(DisambiguatedType::Ambiguous(types));
                            } else {
                                diagnostics.error(
                                    &prefix.pos,
                                    "Procedure calls are not valid in names and expressions",
                                );
                                return Err(EvalError::Unknown);
                            }
                        }
                        Some(Disambiguated::Unambiguous(ent)) => {
                            prefix.set_unique_reference(&ent);
                            if let Some(return_type) = ent.return_type() {
                                resolved = ResolvedName::Expression(
                                    DisambiguatedType::Unambiguous(return_type),
                                );
                            } else {
                                diagnostics.error(
                                    &prefix.pos,
                                    "Procedure calls are not valid in names and expressions",
                                );
                                return Err(EvalError::Unknown);
                            }
                        }
                        None => {
                            return Err(EvalError::Unknown);
                        }
                    }
                } else {
                    diagnostics.push(Diagnostic::unreachable(
                        name_pos,
                        "CallOrIndexed should already be handled",
                    ));
                    return Err(EvalError::Unknown);
                }
            }
            ResolvedName::ObjectName(oname) => {
                match catch_analysis_err(
                    self.resolve_typed_suffix(
                        scope,
                        &prefix.pos,
                        name_pos,
                        oname.type_mark(),
                        &mut suffix,
                        diagnostics,
                    ),
                    diagnostics,
                )? {
                    Some(TypeOrMethod::Type(typ)) => {
                        resolved = ResolvedName::ObjectName(oname.with_suffix(typ));
                    }
                    Some(TypeOrMethod::Method(des, name)) => {
                        resolved = ResolvedName::Overloaded(des, name);
                    }
                    None => {
                        diagnostics.push(Diagnostic::cannot_be_prefix(
                            &prefix.pos,
                            resolved,
                            suffix,
                        ));
                        return Err(EvalError::Unknown);
                    }
                }
            }
            ResolvedName::Expression(ref typ) => match typ {
                DisambiguatedType::Unambiguous(typ) => {
                    match catch_analysis_err(
                        self.resolve_typed_suffix(
                            scope,
                            &prefix.pos,
                            name_pos,
                            *typ,
                            &mut suffix,
                            diagnostics,
                        ),
                        diagnostics,
                    )? {
                        Some(TypeOrMethod::Type(typ)) => {
                            resolved =
                                ResolvedName::Expression(DisambiguatedType::Unambiguous(typ));
                        }
                        Some(TypeOrMethod::Method(des, name)) => {
                            resolved = ResolvedName::Overloaded(des, name);
                        }
                        None => {
                            diagnostics.push(Diagnostic::cannot_be_prefix(
                                &prefix.pos,
                                resolved,
                                suffix,
                            ));
                            return Err(EvalError::Unknown);
                        }
                    }
                }
                DisambiguatedType::Ambiguous(_) => {
                    // @TODO ambiguous error
                    return Err(EvalError::Unknown);
                }
            },

            ResolvedName::Library(ref library_name) => {
                if let Suffix::Selected(ref mut designator) = suffix {
                    resolved = ResolvedName::Design(
                        catch_analysis_err(
                            self.lookup_in_library(
                                library_name,
                                &designator.pos,
                                &designator.item.item,
                            ),
                            diagnostics,
                        )
                        .map(|design| {
                            designator
                                .item
                                .reference
                                .set_unique_reference(design.into());
                            design
                        })?,
                    );
                } else {
                    diagnostics.push(Diagnostic::cannot_be_prefix(name_pos, resolved, suffix));
                    return Err(EvalError::Unknown);
                }
            }
            ResolvedName::Design(ref ent) => {
                if let Suffix::Selected(ref mut designator) = suffix {
                    let name =
                        catch_diagnostic(ent.selected(&prefix.pos, designator), diagnostics)?;
                    resolved = match name {
                        NamedEntities::Single(named_entity) => {
                            designator.set_reference(&name);

                            catch_diagnostic(
                                ResolvedName::from_design_not_overloaded(named_entity)
                                    .map_err(|e| Diagnostic::error(&designator.pos, e)),
                                diagnostics,
                            )?
                        }
                        NamedEntities::Overloaded(overloaded) => {
                            // Could be used for an alias of a subprogram
                            ResolvedName::Overloaded(
                                WithPos::new(designator.item.item.clone(), designator.pos.clone()),
                                overloaded,
                            )
                        }
                    }
                } else {
                    diagnostics.push(Diagnostic::cannot_be_prefix(name_pos, resolved, suffix));
                    return Err(EvalError::Unknown);
                }
            }
            ResolvedName::Type(typ) => {
                if let Suffix::CallOrIndexed(ref mut assocs) = suffix {
                    if let Some((expr_pos, expr)) = as_type_conversion(assocs) {
                        self.check_type_conversion(scope, typ, expr_pos, expr, diagnostics)?;
                        return Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                            typ,
                        )));
                    }
                }

                diagnostics.push(Diagnostic::cannot_be_prefix(name_pos, resolved, suffix));
                return Err(EvalError::Unknown);
            }
            ResolvedName::Final(_) => {
                diagnostics.push(Diagnostic::cannot_be_prefix(name_pos, resolved, suffix));
                return Err(EvalError::Unknown);
            }
        }

        Ok(resolved)
    }
    // Helper function:
    // Resolve a name that must be some kind of object selection, index or slice
    // Such names occur as assignment targets and aliases
    // Takes an error message as an argument to be re-usable
    pub fn resolve_object_name(
        &self,
        scope: &Scope<'a>,
        name_pos: &SrcPos,
        name: &mut Name,
        err_msg: &'static str,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<ObjectName<'a>> {
        let resolved = self.name_resolve(scope, name_pos, name, diagnostics)?;
        match resolved {
            ResolvedName::ObjectName(oname) => Ok(oname),
            ResolvedName::Library(_)
            | ResolvedName::Design(_)
            | ResolvedName::Type(_)
            | ResolvedName::Overloaded { .. }
            | ResolvedName::Expression(_)
            | ResolvedName::Final(_) => {
                diagnostics.error(name_pos, format!("{} {}", resolved.describe(), err_msg));
                Err(EvalError::Unknown)
            }
        }
    }

    pub fn type_name(
        &self,
        scope: &Scope<'a>,
        name_pos: &SrcPos,
        name: &mut Name,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<TypeEnt<'a>> {
        let resolved = self.name_resolve(scope, name_pos, name, diagnostics)?;
        match resolved {
            ResolvedName::Type(typ) => Ok(typ),
            ResolvedName::Library(_)
            | ResolvedName::Design(_)
            | ResolvedName::ObjectName(_)
            | ResolvedName::Overloaded { .. }
            | ResolvedName::Expression(_)
            | ResolvedName::Final(_) => {
                diagnostics.error(
                    name_pos,
                    format!("Expected type name, got {}", resolved.describe()),
                );
                Err(EvalError::Unknown)
            }
        }
    }

    pub fn check_type_conversion(
        &self,
        scope: &Scope<'a>,
        typ: TypeEnt<'a>,
        pos: &SrcPos,
        expr: &mut Expression,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        if let Some(types) = as_fatal(self.expr_pos_type(scope, pos, expr, diagnostics))? {
            match types {
                ExpressionType::Unambiguous(ctyp) => {
                    if !typ.base().is_closely_related(ctyp.base()) {
                        diagnostics.error(
                            pos,
                            format!(
                                "{} cannot be converted to {}",
                                ctyp.describe(),
                                typ.describe()
                            ),
                        )
                    }
                }
                ExpressionType::String
                | ExpressionType::Ambiguous(_)
                | ExpressionType::Null
                | ExpressionType::Aggregate => diagnostics.error(
                    pos,
                    format!(
                        "{} cannot be the argument of type conversion",
                        types.describe()
                    ),
                ),
            }
        }
        Ok(())
    }

    /// Analyze a name that is part of an expression that could be ambiguous
    pub fn expression_name_types(
        &self,
        scope: &Scope<'a>,
        expr_pos: &SrcPos,
        name: &mut Name,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<DisambiguatedType<'a>> {
        let resolved =
            self.name_resolve_with_suffixes(scope, expr_pos, name, None, false, diagnostics)?;
        match self.name_to_type(expr_pos, name.suffix_reference_mut(), resolved) {
            Ok(Some(typ)) => Ok(typ),
            Ok(None) => Err(EvalError::Unknown),
            Err(diag) => {
                diagnostics.push(diag);
                Err(EvalError::Unknown)
            }
        }
    }

    /// Analyze a name that is part of an expression that must be unambiguous
    pub fn expression_name_with_ttyp(
        &self,
        scope: &Scope<'a>,
        expr_pos: &SrcPos,
        name: &mut Name,
        ttyp: TypeEnt<'a>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        if let Some(resolved) = as_fatal(self.name_resolve_with_suffixes(
            scope,
            expr_pos,
            name,
            Some(ttyp),
            false,
            diagnostics,
        ))? {
            // @TODO target_type already used above, functions could probably be simplified
            match self.name_to_unambiguous_type(
                expr_pos,
                &resolved,
                ttyp,
                name.suffix_reference_mut(),
            ) {
                Ok(Some(type_mark)) => {
                    if !self.can_be_target_type(type_mark, ttyp.base()) {
                        diagnostics.push(Diagnostic::type_mismatch(
                            expr_pos,
                            &resolved.describe_type(),
                            ttyp,
                        ));
                    }
                }
                Ok(None) => {}
                Err(diag) => {
                    diagnostics.push(diag);
                }
            }
        }
        Ok(())
    }

    /// Analyze an indexed name where the prefix entity is already known
    /// Returns the type of the array element
    pub fn analyze_indexed_name(
        &self,
        scope: &Scope<'a>,
        name_pos: &SrcPos,
        suffix_pos: &SrcPos,
        type_mark: TypeEnt<'a>,
        indexes: &mut [Index],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> AnalysisResult<TypeEnt<'a>> {
        let base_type = type_mark.base_type();

        let base_type = if let Type::Access(ref subtype, ..) = base_type.kind() {
            subtype.base_type()
        } else {
            base_type
        };

        if let Type::Array {
            indexes: ref index_types,
            elem_type,
            ..
        } = base_type.kind()
        {
            if indexes.len() != index_types.len() {
                diagnostics.push(Diagnostic::dimension_mismatch(
                    name_pos,
                    base_type,
                    indexes.len(),
                    index_types.len(),
                ))
            }

            for index in indexes.iter_mut() {
                self.expr_pos_unknown_ttyp(scope, index.pos, index.expr, diagnostics)?;
            }

            Ok(*elem_type)
        } else {
            Err(Diagnostic::error(
                suffix_pos,
                format!("{} cannot be indexed", type_mark.describe()),
            )
            .into())
        }
    }

    pub fn lookup_selected(
        &self,
        prefix_pos: &SrcPos,
        prefix: EntRef<'a>,
        suffix: &mut WithPos<WithRef<Designator>>,
    ) -> AnalysisResult<NamedEntities<'a>> {
        match prefix.actual_kind() {
            AnyEntKind::Library => {
                let library_name = prefix.designator().expect_identifier();
                let named_entity =
                    self.lookup_in_library(library_name, &suffix.pos, &suffix.item.item)?;
                suffix
                    .item
                    .reference
                    .set_unique_reference(named_entity.into());
                Ok(NamedEntities::new(named_entity.into()))
            }
            AnyEntKind::Object(ref object) => Ok(object
                .subtype
                .type_mark()
                .selected(prefix_pos, suffix)?
                .into_any()),
            AnyEntKind::ObjectAlias { ref type_mark, .. } => {
                Ok(type_mark.selected(prefix_pos, suffix)?.into_any())
            }
            AnyEntKind::ExternalAlias { ref type_mark, .. } => {
                Ok(type_mark.selected(prefix_pos, suffix)?.into_any())
            }
            AnyEntKind::ElementDeclaration(ref subtype) => {
                Ok(subtype.type_mark().selected(prefix_pos, suffix)?.into_any())
            }
            AnyEntKind::Design(_) => {
                let design = DesignEnt::from_any(prefix).ok_or_else(|| {
                    Diagnostic::error(
                        &suffix.pos,
                        format!(
                            "Internal error when expecting design unit, got {}",
                            prefix.describe()
                        ),
                    )
                })?;

                let named = design.selected(prefix_pos, suffix)?;
                Ok(named)
            }

            _ => Err(Diagnostic::invalid_selected_name_prefix(prefix, prefix_pos).into()),
        }
    }

    pub fn resolve_selected_name(
        &self,
        scope: &Scope<'a>,
        name: &mut WithPos<SelectedName>,
    ) -> AnalysisResult<NamedEntities<'a>> {
        match name.item {
            SelectedName::Selected(ref mut prefix, ref mut suffix) => {
                let prefix_ent = self
                    .resolve_selected_name(scope, prefix)?
                    .into_non_overloaded();
                if let Ok(prefix_ent) = prefix_ent {
                    let visible = self.lookup_selected(&prefix.pos, prefix_ent, suffix)?;
                    suffix.set_reference(&visible);
                    return Ok(visible);
                };

                Err(AnalysisError::NotFatal(Diagnostic::error(
                    &prefix.pos,
                    "Invalid prefix for selected name",
                )))
            }
            SelectedName::Designator(ref mut designator) => {
                let visible = scope.lookup(&name.pos, designator.designator())?;
                designator.set_reference(&visible);
                Ok(visible)
            }
        }
    }
}

fn plural(singular: &'static str, plural: &'static str, count: usize) -> &'static str {
    if count == 1 {
        singular
    } else {
        plural
    }
}

impl Diagnostic {
    fn cannot_be_prefix(prefix_pos: &SrcPos, resolved: ResolvedName, suffix: Suffix) -> Diagnostic {
        let suffix_desc = match suffix {
            Suffix::Selected(_) => "selected",
            Suffix::All => "accessed with .all",
            Suffix::Slice(_) => "sliced",
            Suffix::Attribute(_) => "the prefix of an attribute",
            Suffix::CallOrIndexed(ref assoc) => {
                if could_be_indexed_name(assoc) {
                    "indexed"
                } else {
                    "called as a function"
                }
            }
        };

        let name_desc = if matches!(suffix, Suffix::CallOrIndexed(ref assoc) if !could_be_indexed_name(assoc) )
        {
            // When something cannot be called as a function the type is not relevant
            resolved.describe()
        } else {
            resolved.describe_type()
        };

        Diagnostic::error(prefix_pos, format!("{name_desc} cannot be {suffix_desc}"))
    }

    fn cannot_be_prefix_of_attribute(
        prefix_pos: &SrcPos,
        resolved: &ResolvedName,
        attr: &AttributeSuffix,
    ) -> Diagnostic {
        Diagnostic::error(
            prefix_pos,
            format!(
                "{} cannot be the the prefix of '{} attribute",
                resolved.describe_type(),
                attr.attr
            ),
        )
    }

    fn dimension_mismatch(
        pos: &SrcPos,
        base_type: TypeEnt,
        got: usize,
        expected: usize,
    ) -> Diagnostic {
        let mut diag = Diagnostic::error(pos, "Number of indexes does not match array dimension");

        if let Some(decl_pos) = base_type.decl_pos() {
            diag.add_related(
                decl_pos,
                capitalize(&format!(
                    "{} has {} {}, got {} {}",
                    base_type.describe(),
                    expected,
                    plural("dimension", "dimensions", expected),
                    got,
                    plural("index", "indexes", got),
                )),
            );
        }

        diag
    }

    /// An internal logic error that we want to show to the user to get bug reports
    fn unreachable(pos: &SrcPos, expected: &str) -> Diagnostic {
        Diagnostic::warning(pos, format!("Internal error, unreachable code {expected}"))
    }

    pub fn ambiguous_call<'a>(
        call_name: &WithPos<Designator>,
        candidates: impl IntoIterator<Item = OverloadedEnt<'a>>,
    ) -> Diagnostic {
        let mut diag = Diagnostic::error(
            &call_name.pos,
            format!("Ambiguous call to {}", call_name.item.describe()),
        );
        diag.add_subprogram_candidates("Migth be", candidates);
        diag
    }
}

fn check_no_attr_argument(suffix: &AttributeSuffix, diagnostics: &mut dyn DiagnosticHandler) {
    if let Some(ref expr) = suffix.expr {
        diagnostics.error(
            &expr.pos,
            format!("'{} attribute does not take an argument", suffix.attr),
        )
    }
}

fn check_no_sattr_argument(
    attr: SignalAttribute,
    expr: Option<&mut WithPos<Expression>>,
    diagnostics: &mut dyn DiagnosticHandler,
) {
    if let Some(ref expr) = expr {
        diagnostics.error(
            &expr.pos,
            format!("'{attr} attribute does not take an argument"),
        )
    }
}

fn check_single_argument<'a>(
    pos: &SrcPos,
    suffix: &'a mut AttributeSuffix,
    diagnostics: &mut dyn DiagnosticHandler,
) -> Option<&'a mut WithPos<Expression>> {
    if let Some(ref mut expr) = suffix.expr {
        Some(expr)
    } else {
        diagnostics.error(
            pos,
            format!("'{} attribute requires a single argument", suffix.attr),
        );
        None
    }
}

fn ambiguous_functions_to_types<'a>(
    overloaded: &[OverloadedEnt<'a>],
) -> Option<FnvHashSet<BaseType<'a>>> {
    let types: FnvHashSet<_> = overloaded
        .iter()
        .filter_map(|ent| ent.return_type())
        .map(|typ| typ.base())
        .collect();

    if !types.is_empty() {
        Some(types)
    } else {
        None
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use assert_matches::assert_matches;

    use crate::analysis::tests::TestSetup;
    use crate::syntax::test::check_diagnostics;
    use crate::syntax::test::Code;

    impl<'a> TestSetup<'a> {
        fn name_resolve(
            &'a self,
            code: &Code,
            ttyp: Option<TypeEnt<'a>>,
            diagnostics: &mut dyn DiagnosticHandler,
        ) -> EvalResult<ResolvedName<'a>> {
            let mut name = code.name();
            self.ctx().name_resolve_with_suffixes(
                &self.scope,
                &name.pos,
                &mut name.item,
                ttyp,
                false,
                diagnostics,
            )
        }

        fn expression_name_with_ttyp(
            &'a self,
            code: &Code,
            ttyp: TypeEnt<'a>,
            diagnostics: &mut dyn DiagnosticHandler,
        ) {
            let mut name = code.name();
            self.ctx()
                .expression_name_with_ttyp(
                    &self.scope,
                    &name.pos,
                    &mut name.item,
                    ttyp,
                    diagnostics,
                )
                .unwrap()
        }

        fn expression_name_types(
            &'a self,
            code: &Code,
            diagnostics: &mut dyn DiagnosticHandler,
        ) -> Option<DisambiguatedType<'a>> {
            let mut name = code.name();
            as_fatal(self.ctx().expression_name_types(
                &self.scope,
                &name.pos,
                &mut name.item,
                diagnostics,
            ))
            .unwrap()
        }
    }

    #[test]
    fn object_name() {
        let test = TestSetup::new();
        test.declarative_part("constant c0 : natural := 0;");
        assert_matches!(
            test.name_resolve(&test.snippet("c0"), None, &mut NoDiagnostics),
            Ok(ResolvedName::ObjectName(_))
        );
    }

    #[test]
    fn selected_object_name() {
        let test = TestSetup::new();
        test.declarative_part(
            "
type rec_t is record
  field : natural;
end record;
constant c0 : rec_t := (others => 0);
",
        );

        assert_matches!(test.name_resolve(&test.snippet("c0.field"), None, &mut NoDiagnostics), 
            Ok(ResolvedName::ObjectName(oname)) if oname.type_mark() == test.lookup_type("natural"));
    }

    #[test]
    fn access_all() {
        let test = TestSetup::new();
        test.declarative_part(
            "
type ptr_t is access integer_vector;
variable vptr : ptr_t;
",
        );
        let resolved = test.name_resolve(&test.snippet("vptr.all"), None, &mut NoDiagnostics);
        assert_matches!(resolved, Ok(ResolvedName::ObjectName(oname)) if oname.type_mark() == test.lookup_type("integer_vector"));
    }

    #[test]
    fn indexed_name() {
        let test = TestSetup::new();
        test.declarative_part(
            "
variable c0 : integer_vector(0 to 1);
",
        );
        let resolved = test.name_resolve(&test.snippet("c0(0)"), None, &mut NoDiagnostics);
        assert_matches!(resolved, Ok(ResolvedName::ObjectName(oname)) if oname.type_mark() == test.lookup_type("integer"));
    }

    #[test]
    fn indexed_name_type() {
        let test = TestSetup::new();
        test.declarative_part(
            "
variable c0 : integer_vector(0 to 1);
",
        );
        let code = test.snippet("c0('a')");
        let mut diagnostics = Vec::new();
        let resolved = test.name_resolve(&code, None, &mut diagnostics);
        assert_matches!(resolved, Ok(ResolvedName::ObjectName(oname)) if oname.type_mark() == test.lookup_type("integer"));
        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code.s1("'a'"),
                "character literal does not match integer type 'INTEGER'",
            )],
        )
    }

    #[test]
    fn indexed_name_cannot_be_call() {
        let test = TestSetup::new();
        test.declarative_part(
            "
variable c0 : integer_vector(0 to 1);
",
        );
        let code = test.snippet("c0(open)");
        let mut diagnostics = Vec::new();
        assert_eq!(
            test.name_resolve(&test.snippet("c0(open)"), None, &mut diagnostics),
            Err(EvalError::Unknown)
        );

        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                &code.s1("c0"),
                "variable 'c0' cannot be called as a function",
            )],
        );
    }

    #[test]
    fn overloaded_name() {
        let test = TestSetup::new();
        assert_matches!(
            test.name_resolve(&test.snippet("true"), None, &mut NoDiagnostics),
            Ok(ResolvedName::Overloaded { .. })
        );
    }

    #[test]
    fn call_result() {
        let test = TestSetup::new();
        test.declarative_part(
            "
function fun(arg: natural) return integer;
        ",
        );
        assert_eq!(
            test.name_resolve(&test.snippet("fun(0)"), None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.lookup_type("integer")
            ))),
        );
    }

    #[test]
    fn disambiguates_call_with_arguments_by_return_type() {
        let test = TestSetup::new();
        test.declarative_part(
            "
function fun(arg: natural) return integer;
function fun(arg: natural) return character;
        ",
        );
        test.expression_name_with_ttyp(
            &test.snippet("fun(0)"),
            test.lookup_type("integer"),
            &mut NoDiagnostics,
        );
    }

    #[test]
    fn overloaded_name_can_be_selected() {
        let test = TestSetup::new();
        test.declarative_part(
            "
type rec_t is record
    fld : natural;
end record;

function foo return rec_t;
",
        );
        test.expression_name_with_ttyp(
            &test.snippet("foo.fld"),
            test.lookup_type("natural"),
            &mut NoDiagnostics,
        );
    }

    #[test]
    fn procedure_cannot_be_used() {
        let test = TestSetup::new();
        test.declarative_part(
            "
procedure proc(arg: natural);
        ",
        );
        let mut diagnostics = Vec::new();
        let code = test.snippet("proc(0)");
        assert_eq!(
            test.name_resolve(&code, None, &mut diagnostics),
            Err(EvalError::Unknown)
        );
        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code.s1("proc"),
                "Procedure calls are not valid in names and expressions",
            )],
        );
    }

    #[test]
    fn file_can_be_expression() {
        let test = TestSetup::new();
        test.declarative_part(
            "
type file_t is file of character;
file myfile : file_t;
",
        );
        let code = test.snippet("myfile");
        assert_eq!(
            test.expression_name_types(&code, &mut NoDiagnostics),
            Some(DisambiguatedType::Unambiguous(test.lookup_type("file_t"))),
        )
    }

    #[test]
    fn disambiguates_by_target_type() {
        let test = TestSetup::new();
        test.declarative_part(
            "
type enum1_t is (alpha, beta);
type enum2_t is (alpha, beta);
",
        );
        let code = test.snippet("alpha");
        test.expression_name_with_ttyp(&code, test.lookup_type("enum2_t"), &mut NoDiagnostics);
    }

    #[test]
    fn fcall_result_can_be_sliced() {
        let test = TestSetup::new();
        test.declarative_part(
            "
function myfun(arg : integer) return string;
",
        );
        let code = test.snippet("myfun(0)(0 to 1)");
        assert_eq!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.lookup_type("string")
            )))
        );
    }

    #[test]
    fn fcall_without_actuals_can_be_sliced() {
        let test = TestSetup::new();
        test.declarative_part(
            "
function myfun return string;
",
        );
        let code = test.snippet("myfun(0 to 1)");
        assert_eq!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.lookup_type("string")
            )))
        );
    }

    #[test]
    fn disambiguates_with_target_type() {
        let test = TestSetup::new();
        test.declarative_part(
            "
type enum1_t is (alpha, beta);
type enum2_t is (alpha, beta);
",
        );
        let code = test.snippet("alpha");
        test.expression_name_with_ttyp(&code, test.lookup_type("enum1_t"), &mut NoDiagnostics);
    }

    #[test]
    fn slice_access_type() {
        let test = TestSetup::new();
        test.declarative_part(
            "
type ptr_t is access integer_vector;
variable vptr : ptr_t; 
",
        );
        let code = test.snippet("vptr(0 to 1)");
        assert_matches!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::ObjectName(oname)) if oname.type_mark() == test.lookup_type("integer_vector")
        );
    }

    #[test]
    fn index_access_type() {
        let test = TestSetup::new();
        test.declarative_part(
            "
type ptr_t is access integer_vector;
variable vptr : ptr_t; 
",
        );
        let code = test.snippet("vptr(0)");
        assert_matches!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::ObjectName(oname)) if oname.type_mark() == test.lookup_type("integer")
        );
    }

    #[test]
    fn slice_with_integer_discrete_range() {
        let test = TestSetup::new();
        test.declarative_part(
            "
subtype sub_t is integer range 0 to 3;
variable c0 : integer_vector(0 to 6);
",
        );
        let code = test.snippet("c0(sub_t)");
        assert_matches!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::ObjectName(oname)) if oname.type_mark() == test.lookup_type("integer_vector")
        );
    }

    #[test]
    fn slice_with_enum_discrete_range() {
        let test = TestSetup::new();
        test.declarative_part(
            "
type enum_t is (a, b, c);
type arr_t is array (enum_t) of character;
subtype sub_t is enum_t range a to b;
variable c0 : arr_t(a to c);
",
        );
        let code = test.snippet("c0(sub_t)");
        assert_matches!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::ObjectName(oname)) if oname.type_mark() == test.lookup_type("arr_t")
        );
    }

    #[test]
    fn slice_with_bad_type() {
        let test = TestSetup::new();
        test.declarative_part(
            "
variable c0 : integer_vector(0 to 6);
",
        );
        let code = test.snippet("c0(real)");
        let mut diagnostics = Vec::new();
        assert_eq!(
            test.name_resolve(&code, None, &mut diagnostics),
            Err(EvalError::Unknown)
        );
        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code.s1("real"),
                "real type 'REAL' cannot be used as a discrete range",
            )],
        )
    }

    #[test]
    fn cannot_slice_multi_dimensional_array() {
        let test = TestSetup::new();
        test.declarative_part(
            "
type arr_t is array (natural range 0 to 1, natural range 0 to 1) of character;
variable c0 : arr_t;
",
        );
        let code = test.snippet("c0(0 to 1)");
        let mut diagnostics = Vec::new();
        let _ = test.name_resolve(&code, None, &mut diagnostics);
        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code.s1("c0(0 to 1)"),
                "Cannot slice 2-dimensional array type 'arr_t'",
            )],
        )
    }

    #[test]
    fn scalar_type_attribute() {
        let test = TestSetup::new();
        let code = test.snippet("integer'left");
        assert_eq!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.lookup_type("integer")
            )))
        );
        let code = test.snippet("integer'right");
        assert_eq!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.lookup_type("integer")
            )))
        );
        let code = test.snippet("integer'high");
        assert_eq!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.lookup_type("integer")
            )))
        );
        let code = test.snippet("integer'low");
        assert_eq!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.lookup_type("integer")
            )))
        );
    }

    #[test]
    fn array_type_attribute() {
        let test = TestSetup::new();
        test.declarative_part(
            "
type arr_t is array (integer range 0 to 3) of integer;        
        ",
        );
        let code = test.snippet("arr_t'left");
        assert_eq!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.lookup_type("integer")
            )))
        );
    }

    #[test]
    fn array_2d_type_attribute() {
        let test = TestSetup::new();
        test.declarative_part(
            "
type arr_t is array (integer range 0 to 3, character range 'a' to 'c') of integer;        
        ",
        );
        let code = test.snippet("arr_t'left(1)");
        assert_eq!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.lookup_type("integer")
            )))
        );

        let code = test.snippet("arr_t'left(2)");
        assert_eq!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.lookup_type("character")
            )))
        );

        let code = test.snippet("arr_t'left(3)");
        let mut diagnostics = Vec::new();
        assert_eq!(
            test.name_resolve(&code, None, &mut diagnostics),
            Err(EvalError::Unknown)
        );
        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code.s1("3"),
                "Index 3 out of range for array with 2 dimensions, expected 1 to 2",
            )],
        );

        let code = test.snippet("arr_t'left(1+1)");
        let mut diagnostics = Vec::new();
        assert_eq!(
            test.name_resolve(&code, None, &mut diagnostics),
            Err(EvalError::Unknown)
        );
        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code.s1("1+1"),
                "Expected an integer literal",
            )],
        )
    }

    #[test]
    fn length_attribute_of_array_type() {
        let test = TestSetup::new();

        test.declarative_part(
            "
type arr_t is array (integer range 0 to 3) of integer;        
        ",
        );

        let code = test.snippet("arr_t'length");
        assert_eq!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.ctx().universal_integer().into()
            )))
        );
    }

    #[test]
    fn length_attribute_of_array_object() {
        let test = TestSetup::new();

        test.declarative_part(
            "
type arr_t is array (integer range 0 to 3) of integer;        
constant c0 : arr_t := (others => 0);
        ",
        );

        let code = test.snippet("c0'length");
        assert_eq!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.ctx().universal_integer().into()
            )))
        );
    }

    #[test]
    fn ascending_descending() {
        let test = TestSetup::new();

        test.declarative_part(
            "
type arr_t is array (integer range 0 to 3) of integer;        
constant c0 : arr_t := (others => 0);
        ",
        );

        let code = test.snippet("c0'ascending");
        assert_eq!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.ctx().boolean()
            )))
        );

        let code = test.snippet("arr_t'descending");
        assert_eq!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.ctx().boolean()
            )))
        );
    }

    #[test]
    fn image() {
        let test = TestSetup::new();

        let code = test.snippet("natural'image(0)");
        assert_eq!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.ctx().string()
            )))
        );

        let code = test.snippet("natural'image");
        let mut diagnostics = Vec::new();
        assert_eq!(
            test.name_resolve(&code, None, &mut diagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.ctx().string()
            )))
        );
        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code.pos(),
                "'image attribute requires a single argument",
            )],
        )
    }

    #[test]
    fn attribute_no_arg() {
        let test = TestSetup::new();

        let code = test.snippet("integer'low(0)");
        let mut diagnostics = Vec::new();
        assert_eq!(
            test.name_resolve(&code, None, &mut diagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.ctx().integer()
            )))
        );
        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code.s1("0"),
                "'low attribute does not take an argument",
            )],
        )
    }

    #[test]
    fn value() {
        let test = TestSetup::new();

        let code = test.snippet("integer'value(\"0\")");
        assert_eq!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.ctx().integer()
            )))
        );
    }

    #[test]
    fn discrete_attributes() {
        let test = TestSetup::new();

        let code = test.snippet("character'pos('a')");
        assert_eq!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.ctx().universal_integer().into()
            )))
        );

        let code = test.snippet("character'val(0)");
        assert_eq!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.ctx().character()
            )))
        );

        let code = test.snippet("character'val('a')");
        let mut diagnostics = Vec::new();
        assert_eq!(
            test.name_resolve(&code, None, &mut diagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.ctx().character()
            )))
        );
        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code.s1("'a'"),
                "Expected integer type, got type 'CHARACTER'",
            )],
        );

        let code = test.snippet("character'succ('a')");
        assert_eq!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.ctx().character()
            )))
        );

        let code = test.snippet("character'pred('a')");
        assert_eq!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.ctx().character()
            )))
        );

        let code = test.snippet("character'leftof('a')");
        assert_eq!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.ctx().character()
            )))
        );

        let code = test.snippet("character'rightof('a')");
        assert_eq!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.ctx().character()
            )))
        );
    }

    #[test]
    fn signal_attributes_on_non_signal() {
        let test = TestSetup::new();
        test.declarative_part(
            "
variable thevar : integer;
        ",
        );
        let code = test.snippet("thevar'delayed(0 ns)");
        let mut diagnostics = Vec::new();
        assert_eq!(
            test.name_resolve(&code, None, &mut diagnostics),
            Err(EvalError::Unknown)
        );
        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code.s1("thevar"),
                "Expected signal prefix for 'delayed attribute, got variable 'thevar'",
            )],
        )
    }

    #[test]
    fn signal_attributes() {
        let test = TestSetup::new();
        test.declarative_part(
            "
signal thesig : integer; 
        ",
        );

        let code = test.snippet("thesig'delayed(0 ns)");
        assert_eq!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.ctx().integer()
            )))
        );

        let code = test.snippet("thesig'delayed");
        assert_eq!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.ctx().integer()
            )))
        );

        let code = test.snippet("thesig'stable(0 ns)");
        assert_eq!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.ctx().boolean()
            )))
        );

        let code = test.snippet("thesig'quiet(0 ns)");
        assert_eq!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.ctx().boolean()
            )))
        );

        let code = test.snippet("thesig'transaction");
        assert_eq!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.ctx().bit()
            )))
        );

        let code = test.snippet("thesig'event");
        assert_eq!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.ctx().boolean()
            )))
        );

        let code = test.snippet("thesig'active");
        assert_eq!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.ctx().boolean()
            )))
        );

        let code = test.snippet("thesig'last_event");
        assert_eq!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.ctx().time()
            )))
        );

        let code = test.snippet("thesig'last_active");
        assert_eq!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.ctx().time()
            )))
        );

        let code = test.snippet("thesig'last_value");
        assert_eq!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.ctx().integer()
            )))
        );

        let code = test.snippet("thesig'driving");
        assert_eq!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.ctx().boolean()
            )))
        );

        let code = test.snippet("thesig'driving_value");
        assert_eq!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.ctx().integer()
            )))
        );
    }

    #[test]
    fn missing_attribute() {
        let test = TestSetup::new();
        test.declarative_part(
            "
variable thevar : integer;
        ",
        );
        let code = test.snippet("thevar'missing");
        let mut diagnostics = Vec::new();
        assert_eq!(
            test.name_resolve(&code, None, &mut diagnostics),
            Err(EvalError::Unknown)
        );
        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code.s1("missing"),
                "Unknown attribute 'missing",
            )],
        )
    }

    #[test]
    fn range_attribute() {
        let test = TestSetup::new();
        test.declarative_part(
            "
variable thevar : integer_vector(0 to 1);
        ",
        );
        let code = test.snippet("thevar'range");
        let mut diagnostics = Vec::new();
        assert_eq!(
            test.name_resolve(&code, None, &mut diagnostics),
            Err(EvalError::Unknown)
        );
        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code,
                "Range cannot be used as an expression",
            )],
        )
    }

    #[test]
    fn subtype_attribute() {
        let test = TestSetup::new();
        test.declarative_part(
            "
variable thevar : integer_vector(0 to 1);
        ",
        );
        let code = test.snippet("thevar'subtype");
        let mut diagnostics = Vec::new();
        assert_eq!(
            test.name_resolve(&code, None, &mut diagnostics),
            Err(EvalError::Unknown)
        );
        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code,
                "Type cannot be used as an expression",
            )],
        )
    }

    #[test]
    fn name_attributes() {
        let test = TestSetup::new();
        test.declarative_part(
            "
signal thesig : integer; 
        ",
        );

        let code = test.snippet("thesig'simple_name");
        assert_eq!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.ctx().string()
            )))
        );
        let code = test.snippet("thesig'instance_name");
        assert_eq!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.ctx().string()
            )))
        );
        let code = test.snippet("thesig'path_name");
        assert_eq!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.ctx().string()
            )))
        );
    }

    #[test]
    fn integer_type_conversion() {
        let test = TestSetup::new();
        let code = test.snippet("integer(1.0)");
        assert_eq!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.ctx().integer()
            )))
        );

        let code = test.snippet("real(1)");
        assert_eq!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.ctx().real()
            )))
        );
    }

    #[test]
    fn integer_type_conversion_not_closely_related() {
        let test = TestSetup::new();
        let code = test.snippet("integer('a')");
        let mut diagnostics = Vec::new();
        assert_eq!(
            test.name_resolve(&code, None, &mut diagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.ctx().integer()
            )))
        );
        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code.s1("'a'"),
                "type 'CHARACTER' cannot be converted to integer type 'INTEGER'",
            )],
        );

        let code = test.snippet("real(false)");
        let mut diagnostics = Vec::new();
        assert_eq!(
            test.name_resolve(&code, None, &mut diagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.ctx().real()
            )))
        );

        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code.s1("false"),
                "type 'BOOLEAN' cannot be converted to real type 'REAL'",
            )],
        );
    }

    #[test]
    fn array_type_conversion() {
        let test = TestSetup::new();
        test.declarative_part(
            "
type character_vector is array (natural range 0 to 1) of character;
        ",
        );
        let code = test.snippet("character_vector(string'(\"01\"))");
        assert_eq!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.lookup_type("character_vector")
            )))
        );
    }

    #[test]
    fn array_type_conversion_not_closely_related() {
        let test = TestSetup::new();
        test.declarative_part(
            "
type character_vector_2d is array (natural range 0 to 1, natural range 0 to 2) of character;
        ",
        );

        // Dimensionality mismatch
        let code = test.snippet("character_vector_2d(string'(\"01\"))");
        let mut diagnostics = Vec::new();
        assert_eq!(
            test.name_resolve(&code, None, &mut diagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.lookup_type("character_vector_2d")
            )))
        );
        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code.s1("string'(\"01\")"),
                "array type 'STRING' cannot be converted to array type 'character_vector_2d'",
            )],
        );

        // Element type mismatch
        let code = test.snippet("integer_vector(string'(\"01\"))");
        let mut diagnostics = Vec::new();
        assert_eq!(
            test.name_resolve(&code, None, &mut diagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.lookup_type("integer_vector")
            )))
        );
        check_diagnostics(
            diagnostics,
            vec![Diagnostic::error(
                code.s1("string'(\"01\")"),
                "array type 'STRING' cannot be converted to array type 'INTEGER_VECTOR'",
            )],
        );
    }

    #[test]
    fn array_type_conversion_of_closely_related_elements() {
        let test = TestSetup::new();
        test.declarative_part(
            "
type character_vector1 is array (natural range 0 to 1) of character;
type character_vector2 is array (natural range 0 to 1) of character;
type character_matrix1 is array (natural range 0 to 1) of character_vector1;
type character_matrix2 is array (natural range 0 to 1) of character_vector2;

constant c0 : character_matrix1 := (others => (others => 'a'));
            ",
        );
        let code = test.snippet("character_matrix2(c0)");
        assert_eq!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.lookup_type("character_matrix2")
            )))
        );
    }

    #[test]
    fn identical_type_conversion() {
        let test = TestSetup::new();
        test.declarative_part(
            "
type enum_t is (alpha, beta);
        ",
        );
        let code = test.snippet("enum_t(alpha)");
        assert_eq!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Unambiguous(
                test.lookup_type("enum_t")
            )))
        );
    }

    #[test]
    fn ambiguous_function() {
        let test = TestSetup::new();
        test.declarative_part(
            "
        function myfun(arg: integer) return integer;
        function myfun(arg: integer) return real;
        ",
        );
        let code = test.snippet("myfun(0)");
        assert_eq!(
            test.name_resolve(&code, None, &mut NoDiagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Ambiguous(
                vec![test.ctx().real().base(), test.ctx().integer().base()]
                    .into_iter()
                    .collect()
            )))
        );
    }

    #[test]
    fn ambiguous_function_with_ttyp() {
        let test = TestSetup::new();
        let decl = test.declarative_part(
            "
        function f1 return integer;
        function f1 return character;
        function myfun(arg: integer) return integer;
        function myfun(arg: character) return integer;
        ",
        );
        let code = test.snippet("myfun(f1)");
        let mut diagnostics = Vec::new();
        assert_eq!(
            test.name_resolve(&code, Some(test.ctx().integer()), &mut diagnostics),
            Ok(ResolvedName::Expression(DisambiguatedType::Ambiguous(
                vec![test.ctx().integer().base()].into_iter().collect()
            )))
        );
        check_diagnostics(
            diagnostics,
            vec![
                Diagnostic::error(code.s1("myfun"), "Ambiguous call to 'myfun'")
                    .related(
                        decl.s("myfun", 1),
                        "Migth be function myfun[INTEGER return INTEGER]",
                    )
                    .related(
                        decl.s("myfun", 2),
                        "Migth be function myfun[CHARACTER return INTEGER]",
                    ),
            ],
        )
    }

    #[test]
    fn ambiguous_function_with_suffix() {
        let test = TestSetup::new();
        let decl = test.declarative_part(
            "
        type rec1_t is record
            elem1: natural;
        end record;

        type rec2_t is record
            elem1: natural;
        end record;

        function myfun(arg: integer) return rec1_t;
        function myfun(arg: integer) return rec2_t;
        ",
        );
        let code = test.snippet("myfun(0).elem1");
        let mut diagnostics = Vec::new();
        assert_eq!(
            test.name_resolve(&code, None, &mut diagnostics),
            Err(EvalError::Unknown)
        );
        check_diagnostics(
            diagnostics,
            vec![
                Diagnostic::error(code.s1("myfun"), "Ambiguous call to 'myfun'")
                    .related(
                        decl.s("myfun", 1),
                        "Migth be function myfun[INTEGER return rec1_t]",
                    )
                    .related(
                        decl.s("myfun", 2),
                        "Migth be function myfun[INTEGER return rec2_t]",
                    ),
            ],
        )
    }
}
