#![allow(clippy::only_used_in_recursion)]

use fnv::FnvHashSet;

// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com
use super::analyze::*;
use super::formal_region::FormalRegion;
use super::formal_region::InterfaceEnt;
use super::named_entity::*;
use super::names::ResolvedName;
use super::region::*;
use crate::ast::*;
use crate::data::*;

#[derive(Copy, Clone)]
pub enum ResolvedFormal<'a> {
    // A basic formal
    // port map(foo => 0)
    Basic(usize, InterfaceEnt<'a>),

    /// A formal that is either selected such as a record field of array index
    /// Example:
    /// port map(foo.field => 0)
    /// port map(foo(0) => 0)
    Selected(usize, InterfaceEnt<'a>, TypeEnt<'a>),

    /// A formal that has been converted by a function
    /// Could also be a converted selected formal
    /// Example:
    /// port map(to_slv(foo) => sig)
    Converted(usize, InterfaceEnt<'a>, TypeEnt<'a>),
}

impl<'a> ResolvedFormal<'a> {
    pub fn type_mark(&self) -> TypeEnt<'a> {
        match self {
            ResolvedFormal::Basic(_, ent) => ent.type_mark(),
            ResolvedFormal::Selected(_, _, typ) => *typ,
            ResolvedFormal::Converted(_, _, typ) => *typ,
        }
    }

    fn select(self, suffix_type: TypeEnt<'a>) -> Option<Self> {
        match self {
            ResolvedFormal::Basic(idx, ent) => {
                Some(ResolvedFormal::Selected(idx, ent, suffix_type))
            }
            ResolvedFormal::Selected(idx, ent, _) => {
                Some(ResolvedFormal::Selected(idx, ent, suffix_type))
            }

            // Converted formals may not be further selected
            ResolvedFormal::Converted(..) => None,
        }
    }

    // The position of the formal in the formal region
    fn idx(&self) -> usize {
        *match self {
            ResolvedFormal::Basic(idx, _) => idx,
            ResolvedFormal::Selected(idx, _, _) => idx,
            ResolvedFormal::Converted(idx, _, _) => idx,
        }
    }
}

impl<'a> AnalyzeContext<'a> {
    pub fn resolve_formal(
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

                let suffix_ent = resolved_prefix.type_mark().selected(&prefix.pos, suffix)?;
                if let TypedSelection::RecordElement(elem) = suffix_ent {
                    suffix.set_unique_reference(elem.into());
                    if let Some(resolved_formal) = resolved_prefix.select(elem.type_mark()) {
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
                Ok(ResolvedFormal::Basic(idx, ent))
            }
            Name::Slice(ref mut prefix, ref mut drange) => {
                let resolved_prefix = self.resolve_formal(
                    formal_region,
                    scope,
                    &prefix.pos,
                    &mut prefix.item,
                    diagnostics,
                )?;

                if let ResolvedFormal::Converted(..) = resolved_prefix {
                    // Converted formals may not be further selected
                    return Err(Diagnostic::error(name_pos, "Invalid formal").into());
                }

                self.drange_unknown_type(scope, drange.as_mut(), diagnostics)?;
                Ok(resolved_prefix)
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

                    let (idx, pos, formal_ent) = if let Some((pos, designator)) =
                        to_formal_conversion_argument(&mut fcall.parameters)
                    {
                        let (idx, ent) = formal_region.lookup(name_pos, designator.designator())?;
                        designator.set_unique_reference(ent.inner());
                        (idx, pos, ent)
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
                            let ctyp = formal_ent.type_mark().base();
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
                                    && ent
                                        .signature()
                                        .can_be_called_with_single_parameter(formal_ent.type_mark())
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

                                diagnostic.add_subprogram_candidates("migth be", candidates);

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
                                        formal_ent.type_mark().describe()
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

                    Ok(ResolvedFormal::Converted(idx, formal_ent, converted_typ))
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
                        resolved_prefix.type_mark(),
                        &mut indexed_name.indexes,
                        diagnostics,
                    )?;

                    if let Some(resolved_formal) = resolved_prefix.select(new_typ) {
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

    pub fn resolve_association_formals<'e>(
        &self,
        error_pos: &SrcPos, // The position of the instance/call-site
        formal_region: &FormalRegion<'a>,
        scope: &Scope<'a>,
        elems: &'e mut [AssociationElement],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<Vec<ResolvedFormal<'a>>> {
        let mut result: Vec<ResolvedFormal> = Default::default();

        let mut missing = false;
        let mut associated_indexes: FnvHashSet<usize> = Default::default();
        let mut extra_associations: Vec<SrcPos> = Default::default();

        for (idx, AssociationElement { formal, actual }) in elems.iter_mut().enumerate() {
            if let Some(ref mut formal) = formal {
                // Call by name using formal
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
                    Ok(formal) => {
                        associated_indexes.insert(formal.idx());
                        result.push(formal);
                    }
                }
            } else if let Some(formal) = formal_region.nth(idx) {
                associated_indexes.insert(idx);
                result.push(ResolvedFormal::Basic(idx, formal));
            } else {
                extra_associations.push(actual.pos.clone());
            };
        }

        let mut not_associated = Vec::new();
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

        if not_associated.is_empty() && extra_associations.is_empty() && !missing {
            Ok(result)
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
            for (formal, actual) in formals
                .iter()
                .zip(elems.iter_mut().map(|assoc| &mut assoc.actual))
            {
                match &mut actual.item {
                    ActualPart::Expression(expr) => {
                        self.expr_pos_with_ttyp(
                            scope,
                            formal.type_mark(),
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
) -> Option<(&SrcPos, &mut WithRef<Designator>)> {
    if let &mut [AssociationElement {
        ref formal,
        ref mut actual,
    }] = parameters
    {
        if formal.is_some() {
            return None;
        } else if let ActualPart::Expression(Expression::Name(ref mut actual_name)) = actual.item {
            if let Name::Designator(designator) = actual_name.as_mut() {
                return Some((&actual.pos, designator));
            }
        }
    }
    None
}
