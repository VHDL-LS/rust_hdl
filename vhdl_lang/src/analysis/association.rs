// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use crate::analysis::names::ObjectName;
use fnv::FnvHashMap;
use vhdl_lang::TokenSpan;

use super::analyze::*;
use super::names::ResolvedName;
use super::scope::*;
use crate::ast::*;
use crate::data::error_codes::ErrorCode;
use crate::data::*;
use crate::named_entity::*;

#[derive(Copy, Clone)]
pub(crate) struct ResolvedFormal<'a> {
    /// The index in the formal parameter list
    idx: usize,

    /// The underlying interface object
    pub(crate) iface: InterfaceEnt<'a>,

    // Has the formal been selected, indexed or sliced?
    /// Example:
    /// port map(foo.field => 0)
    /// port map(foo(0) => 0)
    is_partial: bool,

    /// Has the formal been converted by a function?
    /// Example:
    /// port map(to_slv(foo) => sig)
    is_converted: bool,

    /// The type of the potentially partial or converted formal
    type_mark: Option<TypeEnt<'a>>,
}

impl<'a> ResolvedFormal<'a> {
    fn new_basic(idx: usize, iface: InterfaceEnt<'a>) -> Self {
        Self {
            idx,
            iface,
            is_partial: false,
            is_converted: false,
            type_mark: iface.type_mark(),
        }
    }

    fn convert(&self, into_type: TypeEnt<'a>) -> Self {
        Self {
            idx: self.idx,
            iface: self.iface,
            is_partial: self.is_partial,
            is_converted: true,
            type_mark: Some(into_type),
        }
    }

    fn partial_with_typ(self, suffix_type: TypeEnt<'a>) -> Option<Self> {
        if !self.is_converted {
            Some(Self {
                idx: self.idx,
                iface: self.iface,
                is_partial: true,
                is_converted: self.is_converted,
                type_mark: Some(suffix_type),
            })
        } else {
            // Converted formals may not be further selected
            None
        }
    }

    fn partial(self) -> Self {
        Self {
            is_partial: true,
            ..self
        }
    }

    pub fn require_type_mark(&self) -> Result<TypeEnt<'a>, Diagnostic> {
        match self.type_mark {
            None => Err(Diagnostic::invalid_formal(
                self.iface
                    .decl_pos()
                    .expect("Interface ent must have a declaration position"),
            )),
            Some(type_mark) => Ok(type_mark),
        }
    }
}

impl<'a> AnalyzeContext<'a, '_> {
    fn resolve_formal(
        &self,
        formal_region: &FormalRegion<'a>,
        scope: &Scope<'a>,
        name_pos: TokenSpan,
        name: &mut Name,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<ResolvedFormal<'a>> {
        match name {
            Name::Selected(prefix, suffix) => {
                let resolved_prefix = self.resolve_formal(
                    formal_region,
                    scope,
                    prefix.span,
                    &mut prefix.item,
                    diagnostics,
                )?;
                let suffix_ent = resolved_prefix
                    .require_type_mark()
                    .into_eval_result(diagnostics)?
                    .selected(self.ctx, prefix.span, suffix)
                    .into_eval_result(diagnostics)?;

                if let TypedSelection::RecordElement(elem) = suffix_ent {
                    suffix.set_unique_reference(elem.into());
                    if let Some(resolved_formal) =
                        resolved_prefix.partial_with_typ(elem.type_mark())
                    {
                        Ok(resolved_formal)
                    } else {
                        bail!(
                            diagnostics,
                            Diagnostic::invalid_formal(name_pos.pos(self.ctx))
                        );
                    }
                } else {
                    bail!(
                        diagnostics,
                        Diagnostic::invalid_formal(name_pos.pos(self.ctx))
                    );
                }
            }

            Name::SelectedAll(_) => {
                bail!(
                    diagnostics,
                    Diagnostic::invalid_formal(name_pos.pos(self.ctx))
                );
            }
            Name::Designator(designator) => {
                let (idx, ent) = formal_region
                    .lookup(&name_pos.pos(self.ctx), designator.item.designator())
                    .into_eval_result(diagnostics)?;
                designator.set_unique_reference(ent.inner());
                Ok(ResolvedFormal::new_basic(idx, ent))
            }
            Name::Slice(ref mut prefix, ref mut drange) => {
                let resolved_prefix = self.resolve_formal(
                    formal_region,
                    scope,
                    prefix.span,
                    &mut prefix.item,
                    diagnostics,
                )?;

                if resolved_prefix.is_converted {
                    // Converted formals may not be further selected
                    bail!(
                        diagnostics,
                        Diagnostic::invalid_formal(name_pos.pos(self.ctx))
                    );
                }

                self.drange_unknown_type(scope, drange.as_mut(), diagnostics)?;
                Ok(resolved_prefix.partial())
            }
            Name::Attribute(..) => {
                bail!(
                    diagnostics,
                    Diagnostic::invalid_formal(name_pos.pos(self.ctx))
                );
            }
            Name::CallOrIndexed(ref mut fcall) => {
                let prefix = if let Some(prefix) = fcall.name.item.prefix() {
                    prefix
                } else {
                    bail!(
                        diagnostics,
                        Diagnostic::invalid_formal(name_pos.pos(self.ctx))
                    );
                };

                if formal_region
                    .lookup(&name_pos.pos(self.ctx), prefix.designator())
                    .is_err()
                {
                    // The prefix of the name was not found in the formal region
                    // it must be a type conversion or a single parameter function call

                    let (pos, resolved_formal) = if let Some((inner_pos, inner_name)) =
                        to_formal_conversion_argument(&mut fcall.parameters.items)
                    {
                        (
                            inner_pos,
                            self.resolve_formal(
                                formal_region,
                                scope,
                                inner_pos,
                                inner_name,
                                diagnostics,
                            )?,
                        )
                    } else {
                        bail!(
                            diagnostics,
                            Diagnostic::invalid_formal_conversion(name_pos.pos(self.ctx))
                        );
                    };

                    let converted_typ = match as_fatal(self.name_resolve(
                        scope,
                        fcall.name.span,
                        &mut fcall.name.item,
                        diagnostics,
                    ))? {
                        Some(ResolvedName::Type(typ)) => {
                            let ctyp = resolved_formal
                                .require_type_mark()
                                .into_eval_result(diagnostics)?
                                .base();
                            if !typ.base().is_closely_related(ctyp) {
                                bail!(
                                    diagnostics,
                                    Diagnostic::invalid_type_conversion(
                                        pos.pos(self.ctx),
                                        ctyp,
                                        typ
                                    )
                                );
                            }
                            typ
                        }
                        Some(ResolvedName::Overloaded(des, overloaded)) => {
                            let resolved_type = resolved_formal
                                .require_type_mark()
                                .into_eval_result(diagnostics)?;
                            let mut candidates = Vec::with_capacity(overloaded.len());

                            for ent in overloaded.entities() {
                                if ent.is_function()
                                    && ent
                                        .signature()
                                        .can_be_called_with_single_parameter(resolved_type)
                                {
                                    candidates.push(ent);
                                }
                            }

                            if candidates.len() > 1 {
                                // Ambiguous call
                                bail!(
                                    diagnostics,
                                    Diagnostic::ambiguous_call(self.ctx, &des, candidates)
                                );
                            } else if let Some(ent) = candidates.pop() {
                                fcall.name.set_unique_reference(&ent);
                                ent.return_type().unwrap()
                            } else {
                                // No match
                                bail!(
                                    diagnostics,
                                    Diagnostic::new(
                                        fcall.name.pos(self.ctx),
                                        format!(
                                            "No function '{}' accepting {}",
                                            fcall.name,
                                            resolved_type.describe()
                                        ),
                                        ErrorCode::Unresolved,
                                    )
                                );
                            }
                        }
                        _ => {
                            bail!(
                                diagnostics,
                                Diagnostic::invalid_formal_conversion(name_pos.pos(self.ctx))
                            );
                        }
                    };

                    Ok(resolved_formal.convert(converted_typ))
                } else if let Some(mut indexed_name) = fcall.as_indexed() {
                    let resolved_prefix = self.resolve_formal(
                        formal_region,
                        scope,
                        indexed_name.name.span,
                        &mut indexed_name.name.item,
                        diagnostics,
                    )?;
                    let resolved_type = resolved_prefix
                        .require_type_mark()
                        .into_eval_result(diagnostics)?;

                    let new_typ = self.analyze_indexed_name(
                        scope,
                        name_pos,
                        indexed_name.name.suffix_pos(),
                        resolved_type,
                        &mut indexed_name.indexes,
                        diagnostics,
                    )?;

                    if let Some(resolved_formal) = resolved_prefix.partial_with_typ(new_typ) {
                        Ok(resolved_formal)
                    } else {
                        bail!(
                            diagnostics,
                            Diagnostic::invalid_formal(name_pos.pos(self.ctx))
                        );
                    }
                } else {
                    bail!(
                        diagnostics,
                        Diagnostic::invalid_formal(name_pos.pos(self.ctx))
                    );
                }
            }
            Name::External(..) => {
                bail!(
                    diagnostics,
                    Diagnostic::invalid_formal(name_pos.pos(self.ctx))
                );
            }
        }
    }

    /// Verifies that named arguments follow positional arguments.
    ///
    /// ## Compliant example
    /// ```vhdl
    /// foo(0, arg1 => 0);
    /// ```
    ///
    /// ## Non-Compliant example
    /// ```vhdl
    /// foo(arg1 => 0, 0);
    /// ```
    pub fn check_positional_before_named(
        &self,
        elems: &[AssociationElement],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult {
        let mut is_positional = false;
        let mut fail = false;
        for AssociationElement { formal, .. } in elems.iter().rev() {
            if let Some(formal) = formal {
                if is_positional {
                    fail = true;

                    diagnostics.add(
                        formal.pos(self.ctx),
                        "Named arguments are not allowed before positional arguments",
                        ErrorCode::NamedBeforePositional,
                    );
                }
            } else {
                is_positional = true;
            }
        }

        if fail {
            Err(EvalError::Unknown)
        } else {
            Ok(())
        }
    }

    /// Combines formal elements (the ones that were declared at a function,
    /// procedure, entity, package, e.t.c.) with the actual elements (the ones
    /// that are present at the call / instantiation site).
    /// The result is a vector of tuples where the first refers to the token span
    /// of the actual and the second to the resolved formal.
    /// If there is an extraneous argument, the resolved formal will be `None`.
    pub fn combine_formal_with_actuals<'e>(
        &self,
        formal_region: &FormalRegion<'a>,
        scope: &Scope<'a>,
        elems: &'e mut [AssociationElement],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<Vec<(TokenSpan, Option<ResolvedFormal<'a>>)>> {
        self.check_positional_before_named(elems, diagnostics)?;

        // Formal region index => actual position, resolved formal
        let mut result = Vec::default();

        for (actual_idx, AssociationElement { formal, actual }) in elems.iter_mut().enumerate() {
            if let Some(ref mut formal) = formal {
                // Named argument
                let resolved_formal = as_fatal(self.resolve_formal(
                    formal_region,
                    scope,
                    formal.span,
                    &mut formal.item,
                    diagnostics,
                ))?;

                result.push((formal.span, resolved_formal));
            } else if let Some(formal) = formal_region.nth(actual_idx) {
                // positional argument
                // Actual index is same as formal index for positional argument
                let formal = ResolvedFormal::new_basic(actual_idx, formal);
                result.push((actual.span, Some(formal)));
            } else {
                diagnostics.add(
                    actual.pos(self.ctx),
                    "Unexpected extra argument",
                    ErrorCode::TooManyArguments,
                );
                result.push((actual.span, None));
            };
        }
        Ok(result)
    }

    /// Check for missing and duplicate associations.
    pub fn check_missing_and_duplicates(
        &self,
        error_pos: &SrcPos, // The position of the instance/call-site
        resolved_pairs: Vec<(TokenSpan, Option<ResolvedFormal<'a>>)>,
        formal_region: &FormalRegion<'a>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<Vec<ResolvedFormal<'a>>> {
        let mut is_error = false;
        let mut result = Vec::default();

        let mut associated: FnvHashMap<usize, (TokenSpan, ResolvedFormal<'_>)> = Default::default();
        for (actual_pos, resolved_formal) in resolved_pairs {
            match resolved_formal {
                Some(resolved_formal) => {
                    if let Some((prev_pos, prev_formal)) = associated.get(&resolved_formal.idx) {
                        if !(resolved_formal.is_partial && prev_formal.is_partial) {
                            let mut diag = Diagnostic::new(
                                actual_pos.pos(self.ctx),
                                format!(
                                    "{} has already been associated",
                                    resolved_formal.iface.describe(),
                                ),
                                ErrorCode::AlreadyAssociated,
                            );

                            diag.add_related(prev_pos.pos(self.ctx), "Previously associated here");
                            is_error = true;
                            diagnostics.push(diag);
                        }
                    }
                    result.push(resolved_formal);
                    associated.insert(resolved_formal.idx, (actual_pos, resolved_formal));
                }
                None => {
                    is_error = true;
                }
            }
        }

        for (idx, formal) in formal_region.iter().enumerate() {
            if !(associated.contains_key(&idx)
                // Default may be unconnected
                || formal.has_default()
                // Output ports are allowed to be unconnected
                || (formal_region.typ == InterfaceType::Port && formal.is_out_or_inout_signal()))
            {
                let diagnostic = Diagnostic::new(
                    error_pos,
                    format!("No association of {}", formal.describe()),
                    ErrorCode::Unassociated,
                )
                .opt_related(formal.decl_pos(), "Defined here");

                is_error = true;
                diagnostics.push(diagnostic);
            }
        }

        if !is_error {
            Ok(result)
        } else {
            Err(EvalError::Unknown)
        }
    }

    pub fn resolve_association_formals<'e>(
        &self,
        error_pos: &SrcPos, // The position of the instance/call-site
        formal_region: &FormalRegion<'a>,
        scope: &Scope<'a>,
        elems: &'e mut [AssociationElement],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<Vec<ResolvedFormal<'a>>> {
        let resolved_pairs =
            self.combine_formal_with_actuals(formal_region, scope, elems, diagnostics)?;
        self.check_missing_and_duplicates(error_pos, resolved_pairs, formal_region, diagnostics)
    }

    pub fn check_association<'e>(
        &self,
        error_pos: &SrcPos, // The position of the instance/call-site
        formal_region: &FormalRegion<'a>,
        scope: &Scope<'a>,
        elems: &'e mut [AssociationElement],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<Vec<ResolvedFormal<'a>>> {
        let resolved_pairs =
            self.combine_formal_with_actuals(formal_region, scope, elems, diagnostics)?;

        for (resolved_formal, actual) in resolved_pairs
            .iter()
            .map(|(_, resolved_formal)| resolved_formal)
            .zip(elems.iter_mut().map(|assoc| &mut assoc.actual))
        {
            match &mut actual.item {
                ActualPart::Expression(expr) => {
                    let Some(resolved_formal) = resolved_formal else {
                        self.expr_pos_unknown_ttyp(scope, actual.span, expr, diagnostics)?;
                        continue;
                    };
                    if formal_region.typ == InterfaceType::Parameter {
                        self.check_parameter_interface(
                            resolved_formal,
                            expr,
                            scope,
                            actual.span,
                            diagnostics,
                        )?;
                    }
                    if let Some(type_mark) = resolved_formal.type_mark {
                        self.expr_pos_with_ttyp(scope, type_mark, actual.span, expr, diagnostics)?;
                    }
                }
                ActualPart::Open => {}
            }
        }
        self.check_missing_and_duplicates(error_pos, resolved_pairs, formal_region, diagnostics)
    }

    pub fn generic_map(
        &self,
        scope: &Scope<'a>,
        error_pos: &SrcPos, // The position of the instance/call-site
        generics: FormalRegion<'a>,
        generic_map: &mut [AssociationElement],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<FnvHashMap<EntityId, TypeEnt<'a>>> {
        let resolved_pairs =
            self.combine_formal_with_actuals(&generics, scope, generic_map, diagnostics)?;
        let mut mapping = FnvHashMap::default();

        for (resolved_formal, actual) in resolved_pairs
            .iter()
            .map(|(_, resolved_formal)| resolved_formal)
            .zip(generic_map.iter_mut().map(|assoc| &mut assoc.actual))
        {
            match &mut actual.item {
                ActualPart::Expression(expr) => {
                    let Some(formal) = resolved_formal else {
                        self.expr_pos_unknown_ttyp(scope, actual.span, expr, &mut NullDiagnostics)?;
                        continue;
                    };
                    match &formal.iface {
                        InterfaceEnt::Type(uninst_typ) => {
                            let typ = if let Expression::Name(name) = expr {
                                match name.as_mut() {
                                    // Could be an array constraint such as integer_vector(0 to 3)
                                    // @TODO we ignore the suffix for now
                                    Name::Slice(prefix, drange) => {
                                        let typ = self.type_name(
                                            scope,
                                            prefix.span,
                                            &mut prefix.item,
                                            diagnostics,
                                        )?;
                                        if let Type::Array { indexes, .. } = typ.base().kind() {
                                            if let Some(Some(idx_typ)) = indexes.first() {
                                                self.drange_with_ttyp(
                                                    scope,
                                                    (*idx_typ).into(),
                                                    drange,
                                                    diagnostics,
                                                )?;
                                            }
                                        } else {
                                            diagnostics.add(
                                                actual.pos(self.ctx),
                                                format!(
                                                    "Array constraint cannot be used for {}",
                                                    typ.describe()
                                                ),
                                                ErrorCode::TypeMismatch,
                                            );
                                        }
                                        typ
                                    }
                                    // Could be a record constraint such as rec_t(field(0 to 3))
                                    // @TODO we ignore the suffix for now
                                    Name::CallOrIndexed(call) if call.could_be_indexed_name() => {
                                        self.type_name(
                                            scope,
                                            call.name.span,
                                            &mut call.name.item,
                                            diagnostics,
                                        )?
                                    }
                                    _ => self.type_name(scope, actual.span, name, diagnostics)?,
                                }
                            } else {
                                diagnostics.add(
                                    actual.pos(self.ctx),
                                    "Cannot map expression to type generic",
                                    ErrorCode::MismatchedKinds,
                                );
                                continue;
                            };

                            mapping.insert(uninst_typ.id(), typ);
                        }
                        InterfaceEnt::Object(obj) if obj.is_constant() => self.expr_pos_with_ttyp(
                            scope,
                            self.map_type_ent(&mapping, obj.type_mark(), scope),
                            actual.span,
                            expr,
                            diagnostics,
                        )?,
                        InterfaceEnt::Subprogram(target) => match expr {
                            Expression::Name(name) => {
                                let resolved =
                                    self.name_resolve(scope, actual.span, name, diagnostics)?;
                                if let ResolvedName::Overloaded(des, overloaded) = resolved {
                                    let signature = target.subprogram_key().map(|base_type| {
                                        mapping
                                            .get(&base_type.id())
                                            .map(|ent| ent.base())
                                            .unwrap_or(base_type)
                                    });
                                    if let Some(ent) = overloaded.get(&signature) {
                                        name.set_unique_reference(&ent);
                                    } else {
                                        let mut diag = Diagnostic::mismatched_kinds(
                                            actual.pos(self.ctx),
                                            format!(
                                                "Cannot map '{des}' to subprogram generic {}{}",
                                                target.designator(),
                                                signature.key().describe()
                                            ),
                                        );

                                        diag.add_subprogram_candidates(
                                            "Does not match",
                                            overloaded.entities(),
                                        );

                                        diagnostics.push(diag)
                                    }
                                } else {
                                    diagnostics.add(
                                        actual.pos(self.ctx),
                                        format!(
                                            "Cannot map {} to subprogram generic",
                                            resolved.describe()
                                        ),
                                        ErrorCode::MismatchedKinds,
                                    )
                                }
                            }
                            Expression::Literal(Literal::String(string)) => {
                                if Operator::from_latin1(string.clone()).is_none() {
                                    diagnostics.add(
                                        actual.pos(self.ctx),
                                        "Invalid operator symbol",
                                        ErrorCode::InvalidOperatorSymbol,
                                    );
                                }
                            }
                            _ => diagnostics.add(
                                actual.pos(self.ctx),
                                "Cannot map expression to subprogram generic",
                                ErrorCode::MismatchedKinds,
                            ),
                        },
                        InterfaceEnt::Package(_) => match expr {
                            Expression::Name(name) => {
                                self.name_resolve(scope, actual.span, name, diagnostics)?;
                            }
                            _ => diagnostics.add(
                                actual.pos(self.ctx),
                                "Cannot map expression to package generic",
                                ErrorCode::MismatchedKinds,
                            ),
                        },
                        _ => diagnostics.push(Diagnostic::invalid_formal(
                            formal
                                .iface
                                .decl_pos()
                                .expect("Formal without declaration position"),
                        )),
                    }
                }
                ActualPart::Open => {
                    // @TODO
                }
            }
        }
        self.check_missing_and_duplicates(error_pos, resolved_pairs, &generics, diagnostics)?;
        Ok(mapping)
    }

    // LRM 4.2.2.1: In a subprogram, the interface mode must match the mode of the actual designator
    // when the interface mode is signal, variable or file. Furthermore, they must be a single name.
    fn check_parameter_interface(
        &self,
        resolved_formal: &ResolvedFormal<'a>,
        expr: &mut Expression,
        scope: &Scope<'a>,
        actual_pos: TokenSpan,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        match resolved_formal.iface {
            InterfaceEnt::Object(o) => match o.class() {
                ObjectClass::Signal => {
                    let Some(name) =
                        as_fatal(self.expression_as_name(expr, scope, actual_pos, diagnostics))?
                    else {
                        diagnostics.add(
                            actual_pos.pos(self.ctx),
                            "Expression must be a name denoting a signal",
                            ErrorCode::InterfaceModeMismatch,
                        );
                        return Ok(());
                    };
                    if !matches!(name, ResolvedName::ObjectName(
                                                            ObjectName { base, .. },
                                                        ) if base.class() == ObjectClass::Signal)
                    {
                        diagnostics.add(
                            actual_pos.pos(self.ctx),
                            "Name must denote a signal name",
                            ErrorCode::InterfaceModeMismatch,
                        );
                    }
                }
                ObjectClass::Constant => {}
                ObjectClass::Variable | ObjectClass::SharedVariable => {
                    let Some(name) =
                        as_fatal(self.expression_as_name(expr, scope, actual_pos, diagnostics))?
                    else {
                        diagnostics.add(
                            actual_pos.pos(self.ctx),
                            "Expression must be a name denoting a variable or shared variable",
                            ErrorCode::InterfaceModeMismatch,
                        );
                        return Ok(());
                    };
                    if !matches!(name, ResolvedName::ObjectName(
                                                        ObjectName { base, .. },
                                                    ) if base.class() == ObjectClass::Variable || base.class() == ObjectClass::SharedVariable)
                    {
                        diagnostics.add(
                            actual_pos.pos(self.ctx),
                            "Name must denote a variable name",
                            ErrorCode::InterfaceModeMismatch,
                        );
                    }
                }
            },
            InterfaceEnt::File(_) => {
                let Some(name) =
                    as_fatal(self.expression_as_name(expr, scope, actual_pos, diagnostics))?
                else {
                    diagnostics.add(
                        actual_pos.pos(self.ctx),
                        "Expression must be a name denoting a file",
                        ErrorCode::InterfaceModeMismatch,
                    );
                    return Ok(());
                };
                if !matches!(name, ResolvedName::Final(ent) if matches!(
                    ent.kind(),
                    AnyEntKind::File(_) | AnyEntKind::InterfaceFile(_)
                )) {
                    diagnostics.add(
                        actual_pos.pos(self.ctx),
                        "Name must denote a file name",
                        ErrorCode::InterfaceModeMismatch,
                    );
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn expression_as_name(
        &self,
        expr: &mut Expression,
        scope: &Scope<'a>,
        span: TokenSpan,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<ResolvedName<'_>> {
        match expr {
            Expression::Name(name) => {
                let resolved = self.name_resolve(scope, span, name, diagnostics)?;
                Ok(resolved)
            }
            _ => Err(EvalError::Unknown),
        }
    }
}

fn to_formal_conversion_argument(
    parameters: &mut [AssociationElement],
) -> Option<(TokenSpan, &mut Box<Name>)> {
    if let &mut [AssociationElement {
        ref formal,
        ref mut actual,
    }] = parameters
    {
        if formal.is_some() {
            return None;
        } else if let ActualPart::Expression(Expression::Name(ref mut actual_name)) = actual.item {
            return Some((actual.span, actual_name));
        }
    }
    None
}

impl Diagnostic {
    pub fn invalid_formal(pos: impl AsRef<SrcPos>) -> Diagnostic {
        Diagnostic::new(pos, "Invalid formal", ErrorCode::InvalidFormal)
    }

    pub fn invalid_formal_conversion(pos: impl AsRef<SrcPos>) -> Diagnostic {
        Diagnostic::new(
            pos,
            "Invalid formal conversion",
            ErrorCode::InvalidFormalConversion,
        )
    }

    pub fn invalid_type_conversion(
        pos: impl AsRef<SrcPos>,
        from: BaseType<'_>,
        to: TypeEnt<'_>,
    ) -> Diagnostic {
        Diagnostic::new(
            pos,
            format!(
                "{} cannot be converted to {}",
                from.describe(),
                to.describe()
            ),
            ErrorCode::TypeMismatch,
        )
    }
}
