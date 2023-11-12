#![allow(clippy::only_used_in_recursion)]

use fnv::FnvHashMap;
use fnv::FnvHashSet;

// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com
use super::analyze::*;
use super::names::ResolvedName;
use super::scope::*;
use crate::ast::*;
use crate::data::*;
use crate::named_entity::*;

#[derive(Copy, Clone)]
struct ResolvedFormal<'a> {
    /// Then index in the formal parameter list
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

    pub fn resolve_association_formals<'e>(
        &self,
        error_pos: &SrcPos, // The position of the instance/call-site
        formal_region: &FormalRegion<'a>,
        scope: &Scope<'a>,
        elems: &'e mut [AssociationElement],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<Vec<TypeEnt<'a>>> {
        self.check_positional_before_named(elems, diagnostics)?;

        // Formal region index => actual position, resolved formal
        let mut result: Vec<(&SrcPos, ResolvedFormal)> = Vec::default();

        let mut missing = false;
        let mut extra_associations: Vec<SrcPos> = Default::default();

        for (actual_idx, AssociationElement { formal, actual }) in elems.iter_mut().enumerate() {
            if let Some(ref mut formal) = formal {
                // Named argument
                match self.resolve_formal(
                    formal_region,
                    scope,
                    &formal.pos,
                    &mut formal.item,
                    diagnostics,
                ) {
                    Err(err) => {
                        missing = true;
                        diagnostics.push(err.into_non_fatal()?);
                    }
                    Ok(resolved_formal) => {
                        result.push((&formal.pos, resolved_formal));
                    }
                }
            } else if let Some(formal) = formal_region.nth(actual_idx) {
                // Actual index is same as formal index for positional argument
                let formal = ResolvedFormal::new_basic(actual_idx, formal);
                result.push((&actual.pos, formal));
            } else {
                extra_associations.push(actual.pos.clone());
            };
        }

        let mut not_associated = Vec::new();
        let mut has_duplicates = false;

        {
            let associated_indexes =
                FnvHashSet::from_iter(result.iter().map(|(_, formal)| formal.idx));
            for (idx, formal) in formal_region.iter().enumerate() {
                if !(associated_indexes.contains(&idx)
                // Default may be unconnected
                || formal.has_default()
                // Output ports are allowed to be unconnected
                || (formal_region.typ == InterfaceType::Port && formal.is_out_or_inout_signal()))
                {
                    not_associated.push(idx);
                }
            }
        }

        {
            let mut seen: FnvHashMap<usize, (&SrcPos, ResolvedFormal)> = Default::default();
            for (actual_pos, resolved_formal) in result.iter() {
                if let Some((prev_pos, prev_formal)) = seen.get(&resolved_formal.idx) {
                    if !(resolved_formal.is_partial && prev_formal.is_partial) {
                        let mut diag = Diagnostic::error(
                            actual_pos,
                            format!(
                                "{} has already been associated",
                                resolved_formal.iface.describe()
                            ),
                        );

                        diag.add_related(prev_pos, "Previously associated here");
                        diagnostics.push(diag);
                        has_duplicates = true;
                    }
                }
                seen.insert(resolved_formal.idx, (*actual_pos, *resolved_formal));
            }
        }

        if not_associated.is_empty() && extra_associations.is_empty() && !missing && !has_duplicates
        {
            Ok(result
                .into_iter()
                .map(|(_, formal)| formal.type_mark)
                .collect())
        } else {
            // Only complain if nothing else is wrong
            for idx in not_associated {
                if let Some(formal) = formal_region.nth(idx) {
                    let mut diagnostic = Diagnostic::error(
                        error_pos,
                        format!("No association of {}", formal.describe()),
                    );

                    if let Some(decl_pos) = formal.decl_pos() {
                        diagnostic.add_related(decl_pos, "Defined here");
                    }

                    diagnostics.push(diagnostic);
                }
            }
            for pos in extra_associations.into_iter() {
                diagnostics.error(pos, "Unexpected extra argument")
            }

            Err(EvalError::Unknown)
        }
    }

    pub fn analyze_assoc_elems_with_formal_region(
        &self,
        error_pos: &SrcPos, // The position of the instance/call-site
        formal_region: &FormalRegion<'a>,
        scope: &Scope<'a>,
        elems: &mut [AssociationElement],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        if let Some(formals) = as_fatal(self.resolve_association_formals(
            error_pos,
            formal_region,
            scope,
            elems,
            diagnostics,
        ))? {
            for (formal_type, actual) in formals
                .iter()
                .zip(elems.iter_mut().map(|assoc| &mut assoc.actual))
            {
                match &mut actual.item {
                    ActualPart::Expression(expr) => {
                        self.expr_pos_with_ttyp(
                            scope,
                            *formal_type,
                            &actual.pos,
                            expr,
                            diagnostics,
                        )?;
                    }
                    ActualPart::Open => {}
                }
            }
        }
        Ok(())
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
