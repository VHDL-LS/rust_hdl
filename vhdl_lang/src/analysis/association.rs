// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use crate::analysis::names::ObjectName;
use fnv::FnvHashMap;
use itertools::Itertools;

use super::analyze::*;
use super::names::ResolvedName;
use super::scope::*;
use crate::ast::*;
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

impl<'a> AnalyzeContext<'a> {
    fn resolve_formal(
        &self,
        formal_region: &FormalRegion<'a>,
        scope: &Scope<'a>,
        name_pos: &SrcPos,
        name: &mut Name,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> AnalysisResult<ResolvedFormal<'a>> {
        match name {
            Name::Selected(prefix, suffix) => {
                let resolved_prefix = self.resolve_formal(
                    formal_region,
                    scope,
                    &prefix.pos,
                    &mut prefix.item,
                    diagnostics,
                )?;

                let suffix_ent = resolved_prefix.type_mark.selected(&prefix.pos, suffix)?;
                if let TypedSelection::RecordElement(elem) = suffix_ent {
                    suffix.set_unique_reference(elem.into());
                    if let Some(resolved_formal) =
                        resolved_prefix.partial_with_typ(elem.type_mark())
                    {
                        Ok(resolved_formal)
                    } else {
                        Err(Diagnostic::error(name_pos, "Invalid formal").into())
                    }
                } else {
                    Err(Diagnostic::error(name_pos, "Invalid formal").into())
                }
            }

            Name::SelectedAll(_) => Err(Diagnostic::error(name_pos, "Invalid formal").into()),
            Name::Designator(designator) => {
                let (idx, ent) = formal_region.lookup(name_pos, designator.designator())?;
                designator.set_unique_reference(ent.inner());
                Ok(ResolvedFormal::new_basic(idx, ent))
            }
            Name::Slice(ref mut prefix, ref mut drange) => {
                let resolved_prefix = self.resolve_formal(
                    formal_region,
                    scope,
                    &prefix.pos,
                    &mut prefix.item,
                    diagnostics,
                )?;

                if resolved_prefix.is_converted {
                    // Converted formals may not be further selected
                    return Err(Diagnostic::error(name_pos, "Invalid formal").into());
                }

                self.drange_unknown_type(scope, drange.as_mut(), diagnostics)?;
                Ok(resolved_prefix.partial())
            }
            Name::Attribute(..) => Err(Diagnostic::error(name_pos, "Invalid formal").into()),
            Name::CallOrIndexed(ref mut fcall) => {
                let prefix = if let Some(prefix) = fcall.name.item.prefix() {
                    prefix
                } else {
                    return Err(Diagnostic::error(name_pos, "Invalid formal").into());
                };

                if formal_region.lookup(name_pos, prefix.designator()).is_err() {
                    // The prefix of the name was not found in the formal region
                    // it must be a type conversion or a single parameter function call

                    let (pos, resolved_formal) = if let Some((inner_pos, inner_name)) =
                        to_formal_conversion_argument(&mut fcall.parameters)
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
                        return Err(Diagnostic::error(name_pos, "Invalid formal conversion").into());
                    };

                    let converted_typ = match as_fatal(self.name_resolve(
                        scope,
                        &fcall.name.pos,
                        &mut fcall.name.item,
                        diagnostics,
                    ))? {
                        Some(ResolvedName::Type(typ)) => {
                            let ctyp = resolved_formal.type_mark.base();
                            if !typ.base().is_closely_related(ctyp) {
                                return Err(Diagnostic::error(
                                    pos,
                                    format!(
                                        "{} cannot be converted to {}",
                                        ctyp.describe(),
                                        typ.describe()
                                    ),
                                )
                                .into());
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
                                let mut diagnostic = Diagnostic::error(
                                    &fcall.name.pos,
                                    format!("Ambiguous call to function '{des}'"),
                                );

                                diagnostic.add_subprogram_candidates("might be", candidates);

                                return Err(diagnostic.into());
                            } else if let Some(ent) = candidates.pop() {
                                fcall.name.set_unique_reference(&ent);
                                ent.return_type().unwrap()
                            } else {
                                // No match
                                return Err(Diagnostic::error(
                                    &fcall.name.pos,
                                    format!(
                                        "No function '{}' accepting {}",
                                        fcall.name,
                                        resolved_formal.type_mark.describe()
                                    ),
                                )
                                .into());
                            }
                        }
                        _ => {
                            return Err(
                                Diagnostic::error(name_pos, "Invalid formal conversion").into()
                            );
                        }
                    };

                    Ok(resolved_formal.convert(converted_typ))
                } else if let Some(mut indexed_name) = fcall.as_indexed() {
                    let resolved_prefix = self.resolve_formal(
                        formal_region,
                        scope,
                        &indexed_name.name.pos,
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
                        Err(Diagnostic::error(name_pos, "Invalid formal").into())
                    }
                } else {
                    Err(Diagnostic::error(name_pos, "Invalid formal").into())
                }
            }
            Name::External(..) => Err(Diagnostic::error(name_pos, "Invalid formal").into()),
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

                    diagnostics.push(Diagnostic::error(
                        formal,
                        "Named arguments are not allowed before positional arguments",
                    ));
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
    ) -> EvalResult<Vec<(&'e SrcPos, Result<ResolvedFormal<'a>, Diagnostic>)>> {
        self.check_positional_before_named(elems, diagnostics)?;

        // Formal region index => actual position, resolved formal
        let mut result: Vec<(&SrcPos, Result<ResolvedFormal, Diagnostic>)> = Vec::default();

        for (actual_idx, AssociationElement { formal, actual }) in elems.iter_mut().enumerate() {
            if let Some(ref mut formal) = formal {
                // Named argument
                let resolved_formal = match self.resolve_formal(
                    formal_region,
                    scope,
                    &formal.pos,
                    &mut formal.item,
                    diagnostics,
                ) {
                    Err(err) => Err(err.into_non_fatal()?),
                    Ok(resolved_formal) => Ok(resolved_formal),
                };

                result.push((&formal.pos, resolved_formal));
            } else if let Some(formal) = formal_region.nth(actual_idx) {
                // Actual index is same as formal index for positional argument
                let formal = ResolvedFormal::new_basic(actual_idx, formal);
                result.push((&actual.pos, Ok(formal)));
            } else {
                result.push((
                    &actual.pos,
                    Err(Diagnostic::error(&actual.pos, "Unexpected extra argument")),
                ));
            };
        }
        Ok(result)
    }

    fn check_missing_and_duplicates<'e>(
        &self,
        error_pos: &SrcPos, // The position of the instance/call-site
        resolved_pairs: &[(&'e SrcPos, Result<ResolvedFormal<'a>, Diagnostic>)],
        formal_region: &FormalRegion<'a>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<Vec<TypeEnt<'a>>> {
        let mut is_error = false;
        let mut result = Vec::default();

        let mut associated: FnvHashMap<usize, (&SrcPos, ResolvedFormal)> = Default::default();
        for (actual_pos, resolved_formal) in resolved_pairs.iter() {
            match resolved_formal {
                Ok(resolved_formal) => {
                    if let Some((prev_pos, prev_formal)) = associated.get(&resolved_formal.idx) {
                        if !(resolved_formal.is_partial && prev_formal.is_partial) {
                            let mut diag = Diagnostic::error(
                                actual_pos,
                                format!(
                                    "{} has already been associated",
                                    resolved_formal.iface.describe()
                                ),
                            );

                            diag.add_related(prev_pos, "Previously associated here");
                            is_error = true;
                            diagnostics.push(diag);
                        }
                    }
                    result.push(resolved_formal.type_mark);
                    associated.insert(resolved_formal.idx, (actual_pos, *resolved_formal));
                }
                Err(diagnostic) => {
                    is_error = true;
                    diagnostics.push(diagnostic.clone());
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
                let mut diagnostic = Diagnostic::error(
                    error_pos,
                    format!("No association of {}", formal.describe()),
                );

                if let Some(decl_pos) = formal.decl_pos() {
                    diagnostic.add_related(decl_pos, "Defined here");
                }

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
                        if let Ok(resolved_formal) = resolved_formal {
                            if formal_region.typ == InterfaceType::Parameter {
                                self.check_parameter_interface(
                                    resolved_formal,
                                    expr,
                                    scope,
                                    &actual.pos,
                                    diagnostics,
                                )?;
                            }
                            self.expr_pos_with_ttyp(
                                scope,
                                resolved_formal.type_mark,
                                &actual.pos,
                                expr,
                                diagnostics,
                            )?;
                        } else {
                            self.expr_pos_unknown_ttyp(scope, &actual.pos, expr, diagnostics)?;
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
        actual_pos: &SrcPos,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        match resolved_formal.iface.interface_class() {
            InterfaceClass::Signal => {
                let Some(name) =
                    as_fatal(self.expression_as_name(expr, scope, actual_pos, diagnostics))?
                else {
                    diagnostics.error(actual_pos, "Expression must be a name denoting a signal");
                    return Ok(());
                };
                if !matches!(name, ResolvedName::ObjectName(
                                                        ObjectName { base, .. },
                                                    ) if base.class() == ObjectClass::Signal)
                {
                    diagnostics.error(actual_pos, "Name must denote a signal name");
                }
            }
            InterfaceClass::Variable => {
                let Some(name) =
                    as_fatal(self.expression_as_name(expr, scope, actual_pos, diagnostics))?
                else {
                    diagnostics.error(
                        actual_pos,
                        "Expression must be a name denoting a variable or shared variable",
                    );
                    return Ok(());
                };
                if !matches!(name, ResolvedName::ObjectName(
                                                        ObjectName { base, .. },
                                                    ) if base.class() == ObjectClass::Variable || base.class() == ObjectClass::SharedVariable)
                {
                    diagnostics.error(actual_pos, "Name must denote a variable name");
                }
            }
            InterfaceClass::File => {
                let Some(name) =
                    as_fatal(self.expression_as_name(expr, scope, actual_pos, diagnostics))?
                else {
                    diagnostics.error(actual_pos, "Expression must be a name denoting a file");
                    return Ok(());
                };
                if !matches!(name, ResolvedName::Final(ent) if matches!(
                    ent.kind(),
                    AnyEntKind::File(_)
                )) {
                    diagnostics.error(actual_pos, "Name must denote a file name");
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
        pos: &SrcPos,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<ResolvedName> {
        match expr {
            Expression::Name(name) => {
                let resolved = self.name_resolve(scope, pos, name, diagnostics)?;
                Ok(resolved)
            }
            _ => Err(EvalError::Unknown),
        }
    }
}

fn to_formal_conversion_argument(
    parameters: &mut [AssociationElement],
) -> Option<(&SrcPos, &mut Box<Name>)> {
    if let &mut [AssociationElement {
        ref formal,
        ref mut actual,
    }] = parameters
    {
        if formal.is_some() {
            return None;
        } else if let ActualPart::Expression(Expression::Name(ref mut actual_name)) = actual.item {
            return Some((&actual.pos, actual_name));
        }
    }
    None
}
