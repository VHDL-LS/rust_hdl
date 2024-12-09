// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use crate::analysis::names::ObjectName;
use fnv::FnvHashMap;
use itertools::Itertools;
use vhdl_lang::TokenSpan;

use super::analyze::*;
use super::names::ResolvedName;
use super::scope::*;
use crate::ast::*;
use crate::data::error_codes::ErrorCode;
use crate::data::*;
use crate::named_entity::*;

#[derive(Copy, Clone)]
struct ResolvedFormal<'a> {
    /// The index in the formal parameter list
    idx: usize,

    /// The underlying interface object
    iface: InterfaceEnt<'a>,

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
    type_mark: TypeEnt<'a>,
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
            type_mark: into_type,
        }
    }

    fn partial_with_typ(&self, suffix_type: TypeEnt<'a>) -> Option<Self> {
        if !self.is_converted {
            Some(Self {
                idx: self.idx,
                iface: self.iface,
                is_partial: true,
                is_converted: self.is_converted,
                type_mark: suffix_type,
            })
        } else {
            // Converted formals may not be further selected
            None
        }
    }

    fn partial(&self) -> Self {
        Self {
            is_partial: true,
            ..*self
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
                    .type_mark
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
                            let ctyp = resolved_formal.type_mark.base();
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
                            let mut candidates = Vec::with_capacity(overloaded.len());

                            for ent in overloaded.entities() {
                                if ent.is_function()
                                    && ent.signature().can_be_called_with_single_parameter(
                                        resolved_formal.type_mark,
                                    )
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
                                            resolved_formal.type_mark.describe()
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

                    let new_typ = self.analyze_indexed_name(
                        scope,
                        name_pos,
                        indexed_name.name.suffix_pos(),
                        resolved_prefix.type_mark,
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

    fn combine_formal_with_actuals<'e>(
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

    fn check_missing_and_duplicates(
        &self,
        error_pos: &SrcPos, // The position of the instance/call-site
        resolved_pairs: &[(TokenSpan, Option<ResolvedFormal<'a>>)],
        formal_region: &FormalRegion<'a>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<Vec<TypeEnt<'a>>> {
        let mut is_error = false;
        let mut result = Vec::default();

        let mut associated: FnvHashMap<usize, (TokenSpan, ResolvedFormal<'_>)> = Default::default();
        for (actual_pos, resolved_formal) in resolved_pairs.iter() {
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
                    result.push(resolved_formal.type_mark);
                    associated.insert(resolved_formal.idx, (*actual_pos, *resolved_formal));
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
    ) -> EvalResult<Vec<TypeEnt<'a>>> {
        let resolved_pairs =
            self.combine_formal_with_actuals(formal_region, scope, elems, diagnostics)?;
        self.check_missing_and_duplicates(error_pos, &resolved_pairs, formal_region, diagnostics)
    }

    pub fn check_association<'e>(
        &self,
        error_pos: &SrcPos, // The position of the instance/call-site
        formal_region: &FormalRegion<'a>,
        scope: &Scope<'a>,
        elems: &'e mut [AssociationElement],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        let resolved_pairs =
            as_fatal(self.combine_formal_with_actuals(formal_region, scope, elems, diagnostics))?;

        if let Some(resolved_pairs) = resolved_pairs {
            as_fatal(self.check_missing_and_duplicates(
                error_pos,
                &resolved_pairs,
                formal_region,
                diagnostics,
            ))?;

            let resolved_formals = resolved_pairs
                .into_iter()
                .map(|(_, resolved_formal)| resolved_formal)
                .collect_vec();

            for (resolved_formal, actual) in resolved_formals
                .iter()
                .zip(elems.iter_mut().map(|assoc| &mut assoc.actual))
            {
                match &mut actual.item {
                    ActualPart::Expression(expr) => {
                        if let Some(resolved_formal) = resolved_formal {
                            if formal_region.typ == InterfaceType::Parameter {
                                self.check_parameter_interface(
                                    resolved_formal,
                                    expr,
                                    scope,
                                    actual.span,
                                    diagnostics,
                                )?;
                            }
                            self.expr_pos_with_ttyp(
                                scope,
                                resolved_formal.type_mark,
                                actual.span,
                                expr,
                                diagnostics,
                            )?;
                        } else {
                            self.expr_pos_unknown_ttyp(scope, actual.span, expr, diagnostics)?;
                        }
                    }
                    ActualPart::Open => {}
                }
            }
        }
        Ok(())
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
        match resolved_formal.iface.interface_class() {
            InterfaceClass::Signal => {
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
            InterfaceClass::Variable => {
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
            InterfaceClass::File => {
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
