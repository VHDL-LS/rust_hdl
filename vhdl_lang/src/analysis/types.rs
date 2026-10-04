// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use std::fmt::Display;

use super::*;
use crate::analysis::names::ResolvedName;
use crate::ast::token_range::WithToken;
use crate::ast::*;
use crate::data::error_codes::ErrorCode;
use crate::data::*;
use crate::named_entity::{Signature, *};
use crate::HasTokenSpan;
use analyze::*;

impl<'a> AnalyzeContext<'a, '_> {
    pub fn resolve_subtype_indication(
        &self,
        scope: &Scope<'a>,
        subtype_indication: &mut SubtypeIndication,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<Subtype<'a>> {
        let SubtypeIndication {
            resolution,
            type_mark,
            constraint,
        } = subtype_indication;

        let type_name = self.type_name(scope, type_mark.span, &mut type_mark.item, diagnostics)?;

        if let Some(constraint) = constraint {
            self.analyze_subtype_constraint(
                scope,
                &type_mark.pos(self.ctx),
                type_name.base(),
                &mut constraint.item,
                diagnostics,
            )?;
        }

        if let Some(resolution) = resolution {
            as_fatal(self.analyze_resolution_indication(
                scope,
                resolution,
                type_name,
                &type_mark.pos(self.ctx),
                false,
                diagnostics,
            ))?;
        }

        Ok(Subtype::new(type_name))
    }

    /// Analyzes `resolution` as applying to `typ`.
    ///
    /// `type_mark_pos` is the position of the type mark in the subtype indication.
    /// `is_element` is true when `typ` is not the type mark itself, but an
    /// element type reached through an enclosing element resolution.
    pub fn analyze_resolution_indication(
        &self,
        scope: &Scope<'a>,
        resolution: &mut ResolutionIndication,
        typ: TypeEnt<'a>,
        type_mark_pos: &SrcPos,
        is_element: bool,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult {
        let type_is_not = |kind: &str| {
            if is_element {
                format!(
                    "Element type '{}' of this type is not {kind}",
                    typ.designator()
                )
            } else {
                format!("{} is not {kind}", typ.describe())
            }
        };
        match resolution {
            ResolutionIndication::FunctionName(func) => {
                let name = self.name_resolve(scope, func.span(), &mut func.item, diagnostics)?;
                match name {
                    ResolvedName::Overloaded(des, name) => {
                        let resolved_fn = self.disambiguate_resolution_function(
                            &des,
                            &name,
                            typ.base(),
                            diagnostics,
                        )?;
                        func.item.set_unique_reference(resolved_fn.ent);
                    }
                    _ => {
                        diagnostics.push(Diagnostic::mismatched_kinds(
                            func.pos(self.ctx),
                            format!(
                                "Resolution indication must be a function, not {}",
                                name.describe()
                            ),
                        ));
                    }
                }
            }
            ResolutionIndication::Element(element) => match &mut element.item {
                ElementResolution::Array(array_resolution) => match typ.base().kind() {
                    Type::Array { elem_type, .. } => {
                        self.analyze_resolution_indication(
                            scope,
                            array_resolution,
                            *elem_type,
                            type_mark_pos,
                            true,
                            diagnostics,
                        )?;
                    }
                    _ => diagnostics.push(
                        Diagnostic::mismatched_kinds(
                            element.span.pos(self.ctx),
                            "Resolving elements requires an array type",
                        )
                        .related(type_mark_pos, type_is_not("an array type")),
                    ),
                },
                ElementResolution::Record(record_resolutions) => match typ.base().kind() {
                    Type::Record(region) => {
                        for RecordElementResolution { ident, resolution } in
                            record_resolutions.iter_mut()
                        {
                            let des = Designator::Identifier(ident.item.item.clone());
                            if let Some(elem) = region.lookup(&des) {
                                ident.set_unique_reference(&elem);
                                as_fatal(self.analyze_resolution_indication(
                                    scope,
                                    resolution,
                                    elem.type_mark(),
                                    type_mark_pos,
                                    true,
                                    diagnostics,
                                ))?;
                            } else {
                                diagnostics.push(Diagnostic::no_declaration_within(
                                    &typ.base(),
                                    ident.item.pos(self.ctx),
                                    &des,
                                ));
                            }
                        }
                    }
                    _ => diagnostics.push(
                        Diagnostic::mismatched_kinds(
                            element.span.pos(self.ctx),
                            "Resolving record fields requires a record type",
                        )
                        .related(type_mark_pos, type_is_not("a record type")),
                    ),
                },
            },
        }
        Ok(())
    }

    /// Disambiguates `candidates` into a single `OverloadedEnt`, or
    /// returns an error if this is not possible.
    ///
    /// Currently does not check / verify that
    /// - the function must be pure
    /// - the parameter must be `constant`
    /// - the parameter's array type must be unconstrained
    fn disambiguate_resolution_function(
        &self,
        name: &WithToken<Designator>,
        candidates: &OverloadedName<'a>,
        typ: BaseType<'a>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<OverloadedEnt<'a>> {
        enum FilteredOutReason<'a> {
            IsUninstantiated,
            NotAFunction,
            NotASingleInputParameter,
            ReturnTypeDoesNotMatch {
                actual: TypeEnt<'a>,
                expected: TypeEnt<'a>,
            },
            ResolutionParameterIsNotArray,
            ResolutionParameterElementTypeMismatch {
                actual: TypeEnt<'a>,
                expected: TypeEnt<'a>,
            },
            IndexNot1D,
        }

        impl Display for FilteredOutReason<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    FilteredOutReason::IsUninstantiated => {
                        write!(f, "is an uninstantiated subprogram")
                    }
                    FilteredOutReason::NotAFunction => {
                        write!(f, "is not a function")
                    }
                    FilteredOutReason::NotASingleInputParameter => {
                        write!(f, "does not have a single input parameter")
                    }
                    FilteredOutReason::ReturnTypeDoesNotMatch { actual, expected } => {
                        write!(
                            f,
                            "returns {} instead of {}",
                            actual.describe(),
                            expected.describe()
                        )
                    }
                    FilteredOutReason::ResolutionParameterIsNotArray => {
                        write!(f, "has a parameter that is not of an array type")
                    }
                    FilteredOutReason::ResolutionParameterElementTypeMismatch {
                        actual,
                        expected,
                    } => {
                        write!(
                            f,
                            "has a parameter whose element {} does not match {}",
                            actual.describe(),
                            expected.describe()
                        )
                    }
                    FilteredOutReason::IndexNot1D => {
                        write!(f, "has a parameter that is not a one-dimensional array")
                    }
                }
            }
        }

        struct FilteredOut<'a> {
            ent: OverloadedEnt<'a>,
            reason: FilteredOutReason<'a>,
            decl_pos: Option<SrcPos>,
        }

        let mut filtered_out = Vec::new();
        let mut applicable = Vec::new();

        for entity in candidates.entities() {
            let mut filter_out = |reason, decl_pos: Option<&SrcPos>| {
                filtered_out.push(FilteredOut {
                    ent: entity,
                    reason,
                    decl_pos: decl_pos.cloned(),
                })
            };
            match entity.kind() {
                Overloaded::Subprogram(signature)
                | Overloaded::SubprogramDecl(signature)
                | Overloaded::InterfaceSubprogram(signature) => {
                    // Is a procedure
                    let Some(return_type) = signature.return_type else {
                        filter_out(FilteredOutReason::NotAFunction, entity.decl_pos());
                        continue;
                    };
                    if return_type.base_type() != typ.into() {
                        filter_out(
                            FilteredOutReason::ReturnTypeDoesNotMatch {
                                actual: return_type.base_type(),
                                expected: typ.into(),
                            },
                            entity.decl_pos(),
                        );
                        continue;
                    }
                    let mut formals = signature.formals.iter();
                    if formals.len() != 1 {
                        filter_out(
                            FilteredOutReason::NotASingleInputParameter,
                            entity.decl_pos(),
                        );
                        continue;
                    }
                    let parameter = formals.next().unwrap();
                    match parameter.type_mark().base_type().kind() {
                        Type::Array { elem_type, indexes } => {
                            if elem_type.base_type() != typ.into() {
                                filter_out(
                                    FilteredOutReason::ResolutionParameterElementTypeMismatch {
                                        actual: elem_type.base_type(),
                                        expected: typ.into(),
                                    },
                                    parameter.decl_pos(),
                                );
                                continue;
                            }
                            if indexes.len() != 1 {
                                filter_out(FilteredOutReason::IndexNot1D, parameter.decl_pos());
                                continue;
                            }
                            applicable.push(entity);
                        }
                        _ => {
                            filter_out(
                                FilteredOutReason::ResolutionParameterIsNotArray,
                                parameter.decl_pos(),
                            );
                        }
                    }
                }
                Overloaded::UninstSubprogram(..) | Overloaded::UninstSubprogramDecl(..) => {
                    filter_out(FilteredOutReason::IsUninstantiated, entity.decl_pos());
                }
                Overloaded::EnumLiteral(_) => {
                    filter_out(FilteredOutReason::NotAFunction, entity.decl_pos());
                }
                Overloaded::Alias(_) => unreachable!("Alias should be resolved by 'entity.kind()'"),
            }
        }

        match applicable.as_slice() {
            [] => {
                let mut diag = Diagnostic::new(
                    name.pos(self.ctx),
                    format!(
                        "Could not resolve resolution function '{}'",
                        name.item.designator()
                    ),
                    ErrorCode::Unresolved,
                );
                filtered_out.sort_by(|x, y| x.decl_pos.cmp(&y.decl_pos));
                for FilteredOut {
                    ent,
                    reason,
                    decl_pos,
                } in filtered_out
                {
                    if let Some(decl_pos) = decl_pos {
                        diag.add_related(
                            decl_pos,
                            format!("candidate {} {reason}", ent.describe()),
                        );
                    }
                }
                diagnostics.push(diag);
                Err(EvalError::Unknown)
            }
            [single] => Ok(*single),
            _ => {
                let mut diag = Diagnostic::new(
                    name.pos(self.ctx),
                    format!("Ambiguous resolution function '{}'", name.item.designator()),
                    ErrorCode::AmbiguousCall,
                );
                diag.add_subprogram_candidates("Might be", applicable);
                diagnostics.push(diag);
                Err(EvalError::Unknown)
            }
        }
    }

    pub(crate) fn analyze_type_declaration(
        &self,
        scope: &Scope<'a>,
        parent: EntRef<'a>,
        type_decl: &mut TypeDeclaration,
        // Is the full type declaration of an incomplete type
        // Overwrite id when defining full type
        overwrite_id: Option<EntityId>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        let src_span = type_decl.span();
        match type_decl.def {
            TypeDefinition::Enumeration(ref mut enumeration) => {
                let enum_type = TypeEnt::define_with_opt_id(
                    self.ctx,
                    self.arena,
                    overwrite_id,
                    &mut type_decl.ident,
                    parent,
                    None,
                    Type::Enum(
                        enumeration
                            .iter()
                            .map(|literal| literal.tree.item.clone().into_designator())
                            .collect(),
                    ),
                    src_span,
                    self.source(),
                );

                let signature = Signature::new(ParameterRegion::default(), Some(enum_type));

                for literal in enumeration.iter_mut() {
                    let literal_ent = self.arena.explicit(
                        literal.tree.item.clone().into_designator(),
                        enum_type.into(),
                        AnyEntKind::Overloaded(Overloaded::EnumLiteral(signature.clone())),
                        Some(literal.pos(self.ctx)),
                        src_span,
                        Some(self.source()),
                    );
                    literal.decl.set(literal_ent.id());

                    unsafe {
                        self.arena.add_implicit(enum_type.id(), literal_ent);
                    }

                    scope.add(literal_ent, diagnostics);
                }

                scope.add(enum_type.into(), diagnostics);

                for ent in self.enum_implicits(enum_type, self.has_matching_op(enum_type)) {
                    unsafe {
                        self.arena.add_implicit(enum_type.id(), ent);
                    }

                    scope.add(ent, diagnostics);
                }
            }
            TypeDefinition::ProtectedBody(ref mut body) => {
                match scope.lookup_immediate(&type_decl.ident.tree.item.clone().into()) {
                    Some(visible) => {
                        let is_ok = match visible.clone().into_non_overloaded() {
                            Ok(ent) => {
                                if let AnyEntKind::Type(Type::Protected(ptype_region, is_body)) =
                                    ent.kind()
                                {
                                    if *is_body {
                                        if let Some(prev_pos) = ent.decl_pos() {
                                            diagnostics.push(Diagnostic::duplicate_error(
                                                &type_decl.ident.tree,
                                                type_decl.ident.tree.pos(self.ctx),
                                                Some(prev_pos),
                                            ))
                                        }
                                    } else {
                                        let ptype_body: EntRef<'_> = TypeEnt::define_with_opt_id(
                                            self.ctx,
                                            self.arena,
                                            overwrite_id,
                                            &mut type_decl.ident,
                                            parent,
                                            Some(ent),
                                            Type::Protected(Region::default(), true),
                                            src_span,
                                            self.source(),
                                        )
                                        .into();

                                        let region = Scope::extend(ptype_region, Some(scope));
                                        self.analyze_declarative_part(
                                            &region,
                                            ptype_body,
                                            &mut body.decl,
                                            diagnostics,
                                        )?;

                                        let kind = Type::Protected(region.into_region(), true);
                                        unsafe {
                                            ptype_body.set_kind(AnyEntKind::Type(kind));
                                        }

                                        scope.add(ptype_body, diagnostics);
                                    }

                                    true
                                } else {
                                    false
                                }
                            }
                            _ => false,
                        };

                        if !is_ok {
                            diagnostics.add(
                                type_decl.ident.pos(self.ctx),
                                format!("'{}' is not a protected type", type_decl.ident),
                                ErrorCode::TypeMismatch,
                            );
                        }
                    }
                    None => {
                        diagnostics.add(
                            type_decl.ident.pos(self.ctx),
                            format!("No declaration of protected type '{}'", type_decl.ident),
                            ErrorCode::Unresolved,
                        );
                    }
                };
            }
            TypeDefinition::Protected(ref mut prot_decl) => {
                // Protected type name is visible inside its declarative region
                // This will be overwritten later when the protected type region is finished
                let ptype: EntRef<'_> = TypeEnt::define_with_opt_id(
                    self.ctx,
                    self.arena,
                    overwrite_id,
                    &mut type_decl.ident,
                    parent,
                    None,
                    Type::Protected(Region::default(), false),
                    src_span,
                    self.source(),
                )
                .into();

                scope.add(ptype, diagnostics);

                let region = scope.nested();
                for item in prot_decl.items.iter_mut() {
                    match item {
                        ProtectedTypeDeclarativeItem::Subprogram(ref mut subprogram) => {
                            match as_fatal(self.subprogram_specification(
                                scope,
                                ptype,
                                &mut subprogram.specification,
                                subprogram.span,
                                Overloaded::SubprogramDecl,
                                diagnostics,
                            ))? {
                                Some((_, ent)) => {
                                    region.add(ent, diagnostics);
                                }
                                None => {
                                    return Ok(());
                                }
                            }
                        }
                    }
                }

                // This is safe since we are in a single thread and no other reference can exist yes
                // Also the region is stored inside an Arc which cannot move
                {
                    let AnyEntKind::Type(Type::Protected(region_ptr, _)) = ptype.kind() else {
                        unreachable!();
                    };

                    #[allow(invalid_reference_casting)]
                    let region_ptr = unsafe {
                        let region_ptr = region_ptr as *const Region<'_>;
                        let region_ptr = region_ptr as *mut Region<'_>;
                        &mut *region_ptr as &mut Region<'_>
                    };
                    *region_ptr = region.into_region();
                }
            }
            TypeDefinition::Record(ref mut element_decls) => {
                let type_ent = TypeEnt::define_with_opt_id(
                    self.ctx,
                    self.arena,
                    overwrite_id,
                    &mut type_decl.ident,
                    parent,
                    None,
                    Type::Record(RecordRegion::default()),
                    src_span,
                    self.source(),
                );

                let mut elems = RecordRegion::default();
                let mut region = Region::default();
                for elem_decl in element_decls.iter_mut() {
                    let subtype =
                        self.resolve_subtype_indication(scope, &mut elem_decl.subtype, diagnostics);
                    if let Some(subtype) = as_fatal(subtype)? {
                        for ident in &mut elem_decl.idents {
                            let elem = self.define(
                                ident,
                                type_ent.into(),
                                AnyEntKind::ElementDeclaration(subtype),
                                elem_decl.span,
                            );
                            region.add(elem, diagnostics);
                            elems.add(elem);
                        }
                    }
                }
                region.close(diagnostics);

                unsafe {
                    let kind = AnyEntKind::Type(Type::Record(elems));
                    type_ent.set_kind(kind)
                }

                scope.add(type_ent.into(), diagnostics);

                for ent in self.record_implicits(type_ent) {
                    unsafe {
                        self.arena.add_implicit(type_ent.id(), ent);
                    }
                    scope.add(ent, diagnostics);
                }
            }
            TypeDefinition::Access(ref mut subtype_indication) => {
                let subtype =
                    self.resolve_subtype_indication(scope, subtype_indication, diagnostics);
                if let Some(subtype) = as_fatal(subtype)? {
                    let type_ent = TypeEnt::define_with_opt_id(
                        self.ctx,
                        self.arena,
                        overwrite_id,
                        &mut type_decl.ident,
                        parent,
                        None,
                        Type::Access(subtype),
                        src_span,
                        self.source(),
                    );

                    scope.add(type_ent.into(), diagnostics);

                    for ent in self.access_implicits(type_ent) {
                        unsafe {
                            self.arena.add_implicit(type_ent.id(), ent);
                        }
                        scope.add(ent, diagnostics);
                    }
                }
            }
            TypeDefinition::Array(ref mut array_indexes, _, ref mut subtype_indication) => {
                let mut indexes: Vec<Option<BaseType<'_>>> =
                    Vec::with_capacity(array_indexes.len());
                for index in array_indexes.iter_mut() {
                    indexes.push(as_fatal(self.analyze_array_index(
                        scope,
                        index,
                        diagnostics,
                    ))?);
                }

                let elem_type = match as_fatal(self.resolve_subtype_indication(
                    scope,
                    subtype_indication,
                    diagnostics,
                ))? {
                    Some(subtype) => subtype.type_mark(),
                    None => return Ok(()),
                };

                let is_1d = indexes.len() == 1;
                let array_ent = TypeEnt::define_with_opt_id(
                    self.ctx,
                    self.arena,
                    overwrite_id,
                    &mut type_decl.ident,
                    parent,
                    None,
                    Type::Array { indexes, elem_type },
                    src_span,
                    self.source(),
                );

                scope.add(array_ent.into(), diagnostics);

                for ent in self.array_implicits(array_ent, is_1d && self.has_matching_op(elem_type))
                {
                    unsafe {
                        self.arena.add_implicit(array_ent.id(), ent);
                    }
                    scope.add(ent, diagnostics);
                }
            }
            TypeDefinition::Subtype(ref mut subtype_indication) => {
                if let Some(subtype) = as_fatal(self.resolve_subtype_indication(
                    scope,
                    subtype_indication,
                    diagnostics,
                ))? {
                    let type_ent = TypeEnt::define_with_opt_id(
                        self.ctx,
                        self.arena,
                        overwrite_id,
                        &mut type_decl.ident,
                        parent,
                        None,
                        Type::Subtype(subtype),
                        src_span,
                        self.source(),
                    );
                    scope.add(type_ent.into(), diagnostics);
                }
            }
            TypeDefinition::Physical(ref mut physical) => {
                self.range_with_ttyp(
                    scope,
                    self.universal_integer().into(),
                    &mut physical.range,
                    diagnostics,
                )?;

                let phys_type = TypeEnt::define_with_opt_id(
                    self.ctx,
                    self.arena,
                    overwrite_id,
                    &mut type_decl.ident,
                    parent,
                    None,
                    Type::Physical,
                    src_span,
                    self.source(),
                );
                scope.add(phys_type.into(), diagnostics);

                let primary = self.define(
                    &mut physical.primary_unit,
                    parent,
                    AnyEntKind::PhysicalLiteral(phys_type),
                    src_span,
                );

                unsafe {
                    self.arena.add_implicit(phys_type.id(), primary);
                }
                scope.add(primary, diagnostics);

                for (secondary_unit_name, value) in physical.secondary_units.iter_mut() {
                    match self.resolve_physical_unit(scope, &mut value.item.unit) {
                        Ok(secondary_unit_type) => {
                            if secondary_unit_type.base_type() != phys_type {
                                diagnostics.add(
                                    value.item.unit.item.pos(self.ctx),
                                    format!(
                                        "Physical unit of type '{}' does not match {}",
                                        secondary_unit_type.designator(),
                                        phys_type.describe()
                                    ),
                                    ErrorCode::TypeMismatch,
                                )
                            }
                        }
                        Err(err) => diagnostics.push(err),
                    }

                    let secondary_unit = self.define(
                        secondary_unit_name,
                        parent,
                        AnyEntKind::PhysicalLiteral(phys_type),
                        src_span,
                    );
                    unsafe {
                        self.arena.add_implicit(phys_type.id(), secondary_unit);
                    }
                    scope.add(secondary_unit, diagnostics)
                }

                for ent in self.physical_implicits(phys_type) {
                    unsafe {
                        self.arena.add_implicit(phys_type.id(), ent);
                    }
                    scope.add(ent, diagnostics);
                }
            }
            TypeDefinition::Incomplete(..) => {
                unreachable!("Handled elsewhere");
            }

            TypeDefinition::Numeric(ref mut range) => {
                self.range_unknown_typ(scope, range, diagnostics)?;

                let universal_type = if let Some(range_typ) =
                    as_fatal(self.range_type(scope, range, diagnostics))?
                {
                    if range_typ.is_any_integer() {
                        UniversalType::Integer
                    } else if range_typ.is_any_real() {
                        UniversalType::Real
                    } else {
                        diagnostics.add(
                            range.span().pos(self.ctx),
                            "Expected real or integer range",
                            ErrorCode::TypeMismatch,
                        );
                        return Ok(());
                    }
                } else {
                    return Ok(());
                };

                let type_ent = TypeEnt::define_with_opt_id(
                    self.ctx,
                    self.arena,
                    overwrite_id,
                    &mut type_decl.ident,
                    parent,
                    None,
                    match universal_type {
                        UniversalType::Integer => Type::Integer,
                        UniversalType::Real => Type::Real,
                    },
                    src_span,
                    self.source(),
                );
                scope.add(type_ent.into(), diagnostics);

                for ent in self.numeric_implicits(universal_type, type_ent) {
                    unsafe {
                        self.arena.add_implicit(type_ent.id(), ent);
                    }
                    scope.add(ent, diagnostics);
                }
            }

            TypeDefinition::File(ref mut type_mark) => {
                let file_type = TypeEnt::define_with_opt_id(
                    self.ctx,
                    self.arena,
                    overwrite_id,
                    &mut type_decl.ident,
                    parent,
                    None,
                    Type::File,
                    src_span,
                    self.source(),
                );

                if let Some(type_mark) = as_fatal(self.type_name(
                    scope,
                    type_mark.span,
                    &mut type_mark.item,
                    diagnostics,
                ))? {
                    for ent in self.create_implicit_file_type_subprograms(file_type, type_mark) {
                        unsafe {
                            self.arena.add_implicit(file_type.id(), ent);
                        }
                        scope.add(ent, diagnostics);
                    }
                }

                scope.add(file_type.into(), diagnostics);
            }
        }

        Ok(())
    }

    /// The matching operators such as ?= are defined for 1d arrays of bit and std_ulogic element type
    fn has_matching_op(&self, typ: TypeEnt<'a>) -> bool {
        if self.is_std_logic_1164 {
            // Within the std_logic_1164 we do not have efficient access to the types
            typ.designator() == &Designator::Identifier(self.root.symbol_utf8("std_ulogic"))
        } else {
            if let Some(ref standard_types) = self.root.standard_types {
                if typ.id() == standard_types.bit {
                    return true;
                }
            }

            if let Some(id) = self.root.std_ulogic {
                if typ.id() == id {
                    return true;
                }
            }

            false
        }
    }

    fn analyze_subtype_constraint(
        &self,
        scope: &Scope<'a>,
        pos: &SrcPos, // The position of the root type mark
        base_type: BaseType<'a>,
        constraint: &mut SubtypeConstraint,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        match constraint {
            SubtypeConstraint::Array(ref mut dranges, ref mut constraint) => {
                if let Type::Array { indexes, elem_type } = base_type.kind() {
                    for (idx, drange) in dranges.iter_mut().enumerate() {
                        if let Some(index_typ) = indexes.get(idx) {
                            if let Some(index_typ) = index_typ {
                                self.drange_with_ttyp(
                                    scope,
                                    (*index_typ).into(),
                                    &mut drange.item,
                                    diagnostics,
                                )?;
                            } else {
                                self.drange_unknown_type(scope, &mut drange.item, diagnostics)?;
                            }
                        } else {
                            diagnostics.add(
                                drange.span().pos(self.ctx),
                                format!("Got extra index constraint for {}", base_type.describe()),
                                ErrorCode::TooManyConstraints,
                            );
                        }
                    }

                    // empty dranges means (open)
                    if dranges.len() < indexes.len() && !dranges.is_empty() {
                        diagnostics.add(
                            pos,
                            format!(
                                "Too few index constraints for {}. Got {} but expected {}",
                                base_type.describe(),
                                dranges.len(),
                                indexes.len()
                            ),
                            ErrorCode::TooFewConstraints,
                        );
                    }

                    if let Some(constraint) = constraint {
                        self.analyze_subtype_constraint(
                            scope,
                            &constraint.span.pos(self.ctx),
                            elem_type.base(),
                            &mut constraint.item,
                            diagnostics,
                        )?;
                    }
                } else {
                    diagnostics.add(
                        pos,
                        format!(
                            "Array constraint cannot be used for {}",
                            base_type.describe()
                        ),
                        ErrorCode::IllegalConstraint,
                    );
                }
            }
            SubtypeConstraint::Range(ref mut range) => {
                if base_type.is_scalar() {
                    self.range_with_ttyp(scope, base_type.into(), range, diagnostics)?;
                } else {
                    diagnostics.add(
                        pos,
                        format!(
                            "Scalar constraint cannot be used for {}",
                            base_type.describe()
                        ),
                        ErrorCode::IllegalConstraint,
                    );
                }
            }
            SubtypeConstraint::Record(ref mut constraints) => {
                if let Type::Record(region) = base_type.kind() {
                    for constraint in constraints.iter_mut() {
                        let ElementConstraint { ident, constraint } = constraint;
                        let des = Designator::Identifier(ident.item.clone());
                        if let Some(elem) = region.lookup(&des) {
                            self.analyze_subtype_constraint(
                                scope,
                                &constraint.pos(self.ctx),
                                elem.type_mark().base(),
                                &mut constraint.item,
                                diagnostics,
                            )?;
                        } else {
                            diagnostics.push(Diagnostic::no_declaration_within(
                                &base_type,
                                ident.pos(self.ctx),
                                &des,
                            ))
                        }
                    }
                } else {
                    diagnostics.add(
                        pos,
                        format!(
                            "Record constraint cannot be used for {}",
                            base_type.describe()
                        ),
                        ErrorCode::IllegalConstraint,
                    );
                }
            }
        }
        Ok(())
    }

    pub fn analyze_subtype_indication(
        &self,
        scope: &Scope<'a>,
        subtype_indication: &mut SubtypeIndication,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        as_fatal(self.resolve_subtype_indication(scope, subtype_indication, diagnostics))
            .map(|_| ())
    }
}
