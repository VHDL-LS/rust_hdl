// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com
use super::names::*;
use super::*;
use crate::ast::*;
use crate::data::*;
use crate::named_entity::{Signature, *};
use crate::{ast, HasTokenSpan};
use analyze::*;
use itertools::Itertools;

impl<'a> AnalyzeContext<'a> {
    fn subprogram_header(
        &self,
        scope: &Scope<'a>,
        parent: EntRef<'a>,
        header: &mut SubprogramHeader,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult<Region<'a>> {
        let mut region = Region::default();
        for decl in header.generic_list.iter_mut() {
            match self.analyze_interface_declaration(scope, parent, decl, diagnostics) {
                Ok(ent) => {
                    region.add(ent, diagnostics);
                    scope.add(ent, diagnostics);
                }
                Err(err) => {
                    err.add_to(diagnostics)?;
                }
            }
        }
        self.analyze_map_aspect(scope, &mut header.map_aspect, diagnostics)?;
        Ok(region)
    }

    pub(crate) fn subprogram_specification(
        &self,
        scope: &Scope<'a>,
        parent: EntRef<'a>,
        subprogram: &mut SubprogramSpecification,
        to_kind: impl Fn(Signature<'a>) -> Overloaded<'a>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> AnalysisResult<(Scope<'a>, OverloadedEnt<'a>)> {
        let subpgm_region = scope.nested();
        let ent = self.arena.explicit(
            subprogram
                .subpgm_designator()
                .item
                .clone()
                .into_designator(),
            parent,
            AnyEntKind::Overloaded(to_kind(Signature::new(FormalRegion::new_params(), None))),
            Some(&subprogram.subpgm_designator().pos),
        );

        let (signature, generic_map) = match subprogram {
            SubprogramSpecification::Function(fun) => {
                let generic_map = if let Some(header) = &mut fun.header {
                    Some(self.subprogram_header(&subpgm_region, ent, header, diagnostics)?)
                } else {
                    None
                };
                let params = self.analyze_parameter_list(
                    &subpgm_region,
                    ent,
                    &mut fun.parameter_list,
                    diagnostics,
                );
                let return_type = self.resolve_type_mark(&subpgm_region, &mut fun.return_type);
                (Signature::new(params?, Some(return_type?)), generic_map)
            }
            SubprogramSpecification::Procedure(procedure) => {
                let generic_map = if let Some(header) = &mut procedure.header {
                    Some(self.subprogram_header(&subpgm_region, ent, header, diagnostics)?)
                } else {
                    None
                };
                let params = self.analyze_parameter_list(
                    &subpgm_region,
                    ent,
                    &mut procedure.parameter_list,
                    diagnostics,
                );
                (Signature::new(params?, None), generic_map)
            }
        };

        let mut kind = to_kind(signature);
        if let Some(map) = generic_map {
            match kind {
                Overloaded::SubprogramDecl(signature) => {
                    kind = Overloaded::UninstSubprogramDecl(signature, map)
                }
                Overloaded::Subprogram(signature) => {
                    kind = Overloaded::UninstSubprogram(signature, map)
                }
                _ => unreachable!(),
            }
        }

        match kind {
            Overloaded::Subprogram(_) => {
                let declared_by =
                    self.find_subpgm_specification(scope, subprogram, kind.signature());

                if let Some(declared_by) = declared_by {
                    unsafe {
                        ent.set_declared_by(declared_by.into());
                    }
                }
            }
            Overloaded::UninstSubprogram(_, _) => {
                let declared_by =
                    self.find_uninst_subpgm_specification(scope, subprogram, kind.signature());

                if let Some(declared_by) = declared_by {
                    unsafe {
                        ent.set_declared_by(declared_by.into());
                    }
                }
            }
            _ => {}
        }

        unsafe {
            ent.set_kind(AnyEntKind::Overloaded(kind));
        }
        subprogram.set_decl_id(ent.id());
        Ok((subpgm_region, OverloadedEnt::from_any(ent).unwrap()))
    }

    pub fn resolve_signature(
        &self,
        scope: &Scope<'a>,
        signature: &mut WithPos<ast::Signature>,
    ) -> AnalysisResult<SignatureKey> {
        let (args, return_type) = match &mut signature.item {
            ast::Signature::Function(ref mut args, ref mut ret) => {
                let args: Vec<_> = args
                    .iter_mut()
                    .map(|arg| self.resolve_type_mark(scope, arg))
                    .collect();
                let return_type = self.resolve_type_mark(scope, ret);
                (args, Some(return_type))
            }
            ast::Signature::Procedure(args) => {
                let args: Vec<_> = args
                    .iter_mut()
                    .map(|arg| self.resolve_type_mark(scope, arg))
                    .collect();
                (args, None)
            }
        };

        let mut params = Vec::with_capacity(args.len());
        for arg in args {
            params.push(arg?.base());
        }

        if let Some(return_type) = return_type {
            Ok(SignatureKey::new(
                params,
                Some(return_type?.base_type().base()),
            ))
        } else {
            Ok(SignatureKey::new(params, None))
        }
    }

    /// Analyze a generic subprogram instance, i.e.,
    /// ```vhdl
    /// procedure my_proc is new my_proc generic map (T => std_logic);
    /// ```
    ///
    /// # Arguments
    ///
    /// * `scope` - The scope that this instance was declared in
    /// * `inst_subprogram_ent` - A reference to the instantiated subprogram entity.
    ///     Used to set the parent reference of the signature
    /// * `uninst_name` - The [ResolvedName] of the uninstantiated subprogram
    /// * `instance` - A reference to the AST element of the subprogram instantiation
    /// * `diagnostics` - The diagnostics handler
    ///
    /// # Returns
    /// The signature after applying the optional map aspect of the uninstantiated subprogram
    pub(crate) fn generic_subprogram_instance(
        &self,
        scope: &Scope<'a>,
        inst_subprogram_ent: &EntRef<'a>,
        uninst_name: &ResolvedName<'a>,
        instance: &mut SubprogramInstantiation,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> AnalysisResult<Signature> {
        let uninstantiated_subprogram =
            self.resolve_uninstantiated_subprogram(scope, uninst_name, instance)?;
        self.check_instantiated_subprogram_kind_matches_declared(
            &uninstantiated_subprogram,
            instance,
            diagnostics,
        );
        instance
            .subprogram_name
            .item
            .set_unique_reference(&uninstantiated_subprogram);
        let region = match uninstantiated_subprogram.kind() {
            Overloaded::UninstSubprogramDecl(_, region) => region,
            Overloaded::UninstSubprogram(_, region) => region,
            _ => unreachable!(),
        };

        match as_fatal(self.generic_instance(
            inst_subprogram_ent,
            scope,
            &instance.ident.tree.pos,
            region,
            &mut instance.generic_map,
            diagnostics,
        ))? {
            None => Ok(uninstantiated_subprogram.signature().clone()),
            Some((_, mapping)) => {
                match self.map_signature(
                    Some(inst_subprogram_ent),
                    &mapping,
                    uninstantiated_subprogram.signature(),
                ) {
                    Ok(signature) => Ok(signature),
                    Err(err) => {
                        let mut diag = Diagnostic::error(&instance.ident.tree.pos, err);
                        if let Some(pos) = uninstantiated_subprogram.decl_pos() {
                            diag.add_related(pos, "When instantiating this declaration");
                        }
                        Err(AnalysisError::NotFatal(diag))
                    }
                }
            }
        }
    }

    /// Given a `ResolvedName` and the subprogram instantiation,
    /// find the uninstantiated subprogram that the resolved name references.
    /// Return that resolved subprogram, if it exists, else return an `Err`
    fn resolve_uninstantiated_subprogram(
        &self,
        scope: &Scope<'a>,
        name: &ResolvedName<'a>,
        instantiation: &mut SubprogramInstantiation,
    ) -> AnalysisResult<OverloadedEnt<'a>> {
        let signature_key = match &mut instantiation.signature {
            None => None,
            Some(ref mut signature) => Some((
                self.resolve_signature(scope, signature)?,
                signature.pos.clone(),
            )),
        };
        let overloaded_ent = match name {
            ResolvedName::Overloaded(_, overloaded) => {
                let choices = overloaded
                    .entities()
                    .filter(|ent| ent.is_uninst_subprogram())
                    .collect_vec();
                if choices.is_empty() {
                    Err(AnalysisError::NotFatal(Diagnostic::error(
                        &instantiation.ident.tree.pos,
                        format!(
                            "{} does not denote an uninstantiated subprogram",
                            name.describe()
                        ),
                    )))
                } else if choices.len() == 1 {
                    // There is only one possible candidate
                    let ent = choices[0];
                    // If the instantiated program has a signature, check that it matches
                    // that of the uninstantiated subprogram
                    if let Some((key, pos)) = signature_key {
                        match overloaded.get(&SubprogramKey::Uninstantiated(key)) {
                            None => Err(AnalysisError::NotFatal(Diagnostic::error(
                                pos.clone(),
                                format!(
                                    "Signature does not match the the signature of {}",
                                    ent.describe()
                                ),
                            ))),
                            Some(_) => Ok(ent),
                        }
                    } else {
                        Ok(ent)
                    }
                } else if let Some((key, _)) = signature_key {
                    // There are multiple candidates
                    // but there is a signature that we can try to resolve
                    if let Some(resolved_ent) =
                        overloaded.get(&SubprogramKey::Uninstantiated(key.clone()))
                    {
                        Ok(resolved_ent)
                    } else {
                        Err(AnalysisError::NotFatal(Diagnostic::error(
                            &instantiation.subprogram_name.pos,
                            format!(
                                "No uninstantiated subprogram exists with signature {}",
                                key.describe()
                            ),
                        )))
                    }
                } else {
                    // There are multiple candidates
                    // and there is no signature to resolve
                    let mut err = Diagnostic::error(
                        &instantiation.subprogram_name.pos,
                        format!("Ambiguous instantiation of '{}'", overloaded.designator()),
                    );
                    for ent in choices {
                        if let Some(pos) = &ent.decl_pos {
                            err.add_related(pos.clone(), format!("Might be {}", ent.describe()))
                        }
                    }
                    Err(AnalysisError::NotFatal(err))
                }
            }
            _ => Err(AnalysisError::NotFatal(Diagnostic::error(
                &instantiation.subprogram_name.pos,
                format!(
                    "{} does not denote an uninstantiated subprogram",
                    name.describe()
                ),
            ))),
        }?;
        if overloaded_ent.is_uninst_subprogram() {
            Ok(overloaded_ent)
        } else {
            Err(AnalysisError::NotFatal(Diagnostic::error(
                &instantiation.subprogram_name.pos,
                format!("{} cannot be instantiated", overloaded_ent.describe()),
            )))
        }
    }

    /// Checks that an instantiated subprogram kind matches the declared subprogram.
    /// For instance, when a subprogram was instantiated using
    /// ```vhdl
    /// function my_func is new proc;
    /// ```
    /// where proc is
    /// ```vhdl
    /// procedure proc is
    /// ...
    /// ```
    ///
    /// This function will push an appropriate diagnostic.
    fn check_instantiated_subprogram_kind_matches_declared(
        &self,
        ent: &OverloadedEnt,
        instance: &SubprogramInstantiation,
        diagnostics: &mut dyn DiagnosticHandler,
    ) {
        let err_msg = if ent.is_function() && instance.kind != SubprogramKind::Function {
            Some("Instantiating function as procedure")
        } else if ent.is_procedure() && instance.kind != SubprogramKind::Procedure {
            Some("Instantiating procedure as function")
        } else {
            None
        };
        if let Some(msg) = err_msg {
            let mut err = Diagnostic::error(self.ctx.get_pos(instance.get_start_token()), msg);
            if let Some(pos) = ent.decl_pos() {
                err.add_related(pos, format!("{} declared here", ent.describe()));
            }
            diagnostics.push(err)
        }
    }

    fn find_subpgm_specification(
        &self,
        scope: &Scope<'a>,
        decl: &SubprogramSpecification,
        signature: &Signature,
    ) -> Option<OverloadedEnt<'a>> {
        let des = decl.subpgm_designator().item.clone().into_designator();

        if let Some(NamedEntities::Overloaded(overloaded)) = scope.lookup_immediate(&des) {
            let ent = overloaded.get(&SubprogramKey::Normal(signature.key()))?;

            if ent.is_subprogram_decl() {
                return Some(ent);
            }
        }
        None
    }

    fn find_uninst_subpgm_specification(
        &self,
        scope: &Scope<'a>,
        decl: &SubprogramSpecification,
        signature: &Signature,
    ) -> Option<OverloadedEnt<'a>> {
        let des = decl.subpgm_designator().item.clone().into_designator();

        if let Some(NamedEntities::Overloaded(overloaded)) = scope.lookup_immediate(&des) {
            // Note: This does not work in common circumstances with a generic type parameter
            // since the parameters of the declared subprogram and the subprogram with body
            // point to two different type-ID's. For example:
            // function foo generic (type F);
            //                            ^-- F has EntityId X
            // function foo generic (type F) return F is ... end function foo;
            //                            ^-- F has EntityId Y
            // A future improvement must take this fact into account.
            let ent = overloaded.get(&SubprogramKey::Uninstantiated(signature.key()))?;

            if ent.is_uninst_subprogram_decl() {
                return Some(ent);
            }
        }
        None
    }
}
