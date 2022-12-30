#![allow(clippy::only_used_in_recursion)]
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com
use super::analyze::*;
use super::region::*;
use super::target::AssignmentType;
use crate::ast::Range;
use crate::ast::*;
use crate::data::*;
use std::sync::Arc;

impl<'a> AnalyzeContext<'a> {
    pub fn lookup_selected(
        &self,
        prefix_pos: &SrcPos,
        prefix: &NamedEntity,
        suffix: &WithPos<WithRef<Designator>>,
    ) -> AnalysisResult<NamedEntities> {
        match prefix.actual_kind() {
            NamedEntityKind::Library => {
                let library_name = prefix.designator().expect_identifier();
                let named_entity =
                    self.lookup_in_library(library_name, &suffix.pos, suffix.designator())?;

                Ok(NamedEntities::new(named_entity))
            }

            NamedEntityKind::UninstPackage(..) => Err(AnalysisError::NotFatal(
                invalid_selected_name_prefix(prefix, prefix_pos),
            )),
            NamedEntityKind::Object(ref object) => {
                self.lookup_type_selected(prefix_pos, object.subtype.type_mark(), suffix)
            }
            NamedEntityKind::ObjectAlias { ref type_mark, .. } => {
                self.lookup_type_selected(prefix_pos, type_mark, suffix)
            }
            NamedEntityKind::ExternalAlias { ref type_mark, .. } => {
                self.lookup_type_selected(prefix_pos, type_mark, suffix)
            }
            NamedEntityKind::ElementDeclaration(ref subtype) => {
                self.lookup_type_selected(prefix_pos, subtype.type_mark(), suffix)
            }
            NamedEntityKind::Package(ref region)
            | NamedEntityKind::PackageInstance(ref region)
            | NamedEntityKind::LocalPackageInstance(ref region) => {
                if let Some(decl) = region.lookup_selected(suffix.designator()) {
                    Ok(decl.clone())
                } else {
                    Err(no_declaration_within(prefix, suffix).into())
                }
            }
            _ => Err(invalid_selected_name_prefix(prefix, prefix_pos).into()),
        }
    }

    /// Lookup a selected name when the prefix has type
    pub fn lookup_type_selected(
        &self,
        prefix_pos: &SrcPos,
        prefix_type: &TypeEnt,
        suffix: &WithPos<WithRef<Designator>>,
    ) -> AnalysisResult<NamedEntities> {
        match prefix_type.flatten_alias().kind() {
            Type::Record(ref region) => {
                if let Some(decl) = region.lookup_selected(suffix.designator()) {
                    Ok(decl.clone())
                } else {
                    Err(no_declaration_within(prefix_type, suffix).into())
                }
            }
            Type::Protected(region) => {
                if let Some(decl) = region.lookup_selected(suffix.designator()) {
                    Ok(decl.clone())
                } else {
                    Err(no_declaration_within(prefix_type, suffix).into())
                }
            }
            Type::Incomplete(full_type_ref) => {
                if let Some(full_type) = full_type_ref
                    .load()
                    .upgrade()
                    .and_then(|e| TypeEnt::from_any(e).ok())
                {
                    self.lookup_type_selected(prefix_pos, &full_type, suffix)
                } else {
                    Err(Diagnostic::error(
                        prefix_pos,
                        "Internal error when referencing full type of incomplete type",
                    )
                    .into())
                }
            }
            Type::Subtype(subtype) => {
                self.lookup_type_selected(prefix_pos, subtype.type_mark(), suffix)
            }
            Type::Access(subtype, ..) => {
                self.lookup_type_selected(prefix_pos, subtype.type_mark(), suffix)
            }
            _ => Err(invalid_selected_name_prefix(prefix_type, prefix_pos).into()),
        }
    }

    pub fn resolve_selected_name(
        &self,
        region: &Region<'_>,
        name: &mut WithPos<SelectedName>,
    ) -> AnalysisResult<NamedEntities> {
        match name.item {
            SelectedName::Selected(ref mut prefix, ref mut suffix) => {
                suffix.clear_reference();

                let prefix_ent = self
                    .resolve_selected_name(region, prefix)?
                    .into_non_overloaded();
                if let Ok(prefix_ent) = prefix_ent {
                    let visible = self.lookup_selected(&prefix.pos, &prefix_ent, suffix)?;
                    suffix.set_reference(&visible);
                    return Ok(visible);
                };

                Err(AnalysisError::NotFatal(Diagnostic::error(
                    &prefix.pos,
                    "Invalid prefix for selected name",
                )))
            }
            SelectedName::Designator(ref mut designator) => {
                designator.clear_reference();
                let visible = region.lookup_within(&name.pos, designator.designator())?;
                designator.set_reference(&visible);
                Ok(visible)
            }
        }
    }

    pub fn resolve_name(
        &self,
        region: &Region<'_>,
        name_pos: &SrcPos,
        name: &mut Name,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult<Option<NamedEntities>> {
        match name {
            Name::Selected(prefix, suffix) => {
                suffix.clear_reference();

                match self.resolve_name(region, &prefix.pos, &mut prefix.item, diagnostics)? {
                    Some(NamedEntities::Single(ref named_entity)) => {
                        match self.lookup_selected(&prefix.pos, named_entity, suffix) {
                            Ok(visible) => {
                                suffix.set_reference(&visible);
                                Ok(Some(visible))
                            }
                            Err(err) => {
                                err.add_to(diagnostics)?;
                                Ok(None)
                            }
                        }
                    }
                    Some(NamedEntities::Overloaded(..)) => Ok(None),
                    None => Ok(None),
                }
            }

            Name::SelectedAll(prefix) => {
                self.resolve_name(region, &prefix.pos, &mut prefix.item, diagnostics)?;

                Ok(None)
            }
            Name::Designator(designator) => {
                designator.clear_reference();
                match region.lookup_within(name_pos, designator.designator()) {
                    Ok(visible) => {
                        designator.set_reference(&visible);
                        Ok(Some(visible))
                    }
                    Err(diagnostic) => {
                        diagnostics.push(diagnostic);
                        Ok(None)
                    }
                }
            }
            Name::Indexed(ref mut prefix, ref mut exprs) => {
                self.resolve_name(region, &prefix.pos, &mut prefix.item, diagnostics)?;
                for expr in exprs.iter_mut() {
                    self.analyze_expression(region, expr, diagnostics)?;
                }
                Ok(None)
            }

            Name::Slice(ref mut prefix, ref mut drange) => {
                self.resolve_name(region, &prefix.pos, &mut prefix.item, diagnostics)?;
                self.analyze_discrete_range(region, drange.as_mut(), diagnostics)?;
                Ok(None)
            }
            Name::Attribute(ref mut attr) => {
                self.analyze_attribute_name(region, attr, diagnostics)?;
                Ok(None)
            }
            Name::FunctionCall(..) => {
                self.analyze_ambiguous_function_call(region, name_pos, name, diagnostics)?;
                Ok(None)
            }
            Name::External(ref mut ename) => {
                let ExternalName { subtype, .. } = ename.as_mut();
                self.analyze_subtype_indication(region, subtype, diagnostics)?;
                Ok(None)
            }
        }
    }

    pub fn resolve_non_overloaded_with_kind(
        &self,
        named_entities: NamedEntities,
        pos: &SrcPos,
        kind_ok: &impl Fn(&NamedEntityKind) -> bool,
        expected: &str,
    ) -> AnalysisResult<Arc<NamedEntity>> {
        let ent = self.resolve_non_overloaded(named_entities, pos, expected)?;
        if kind_ok(ent.actual_kind()) {
            Ok(ent)
        } else {
            Err(AnalysisError::NotFatal(ent.kind_error(pos, expected)))
        }
    }

    pub fn resolve_non_overloaded(
        &self,
        named_entities: NamedEntities,
        pos: &SrcPos,
        expected: &str,
    ) -> AnalysisResult<Arc<NamedEntity>> {
        named_entities.into_non_overloaded().map_err(|overloaded| {
            let mut error =
                Diagnostic::error(pos, format!("Expected {}, got overloaded name", expected));
            for ent in overloaded.entities() {
                if let Some(decl_pos) = ent.decl_pos() {
                    error.add_related(decl_pos, "Defined here");
                }
            }
            AnalysisError::NotFatal(error)
        })
    }

    pub fn resolve_type_mark_name(
        &self,
        region: &Region<'_>,
        type_mark: &mut WithPos<SelectedName>,
    ) -> AnalysisResult<TypeEnt> {
        let entities = self.resolve_selected_name(region, type_mark)?;

        let pos = type_mark.suffix_pos();
        let expected = "type";
        let ent = self.resolve_non_overloaded(entities, pos, expected)?;
        TypeEnt::from_any(ent).map_err(|ent| AnalysisError::NotFatal(ent.kind_error(pos, expected)))
    }

    pub fn resolve_type_mark(
        &self,
        region: &Region<'_>,
        type_mark: &mut WithPos<TypeMark>,
    ) -> AnalysisResult<TypeEnt> {
        if !type_mark.item.subtype {
            self.resolve_type_mark_name(region, &mut type_mark.item.name)
        } else {
            let entities = self.resolve_selected_name(region, &mut type_mark.item.name)?;

            let pos = type_mark.item.name.suffix_pos();
            let expected = "object or alias";
            let named_entity = self.resolve_non_overloaded(entities, pos, expected)?;

            match named_entity.kind() {
                NamedEntityKind::Object(obj) => Ok(obj.subtype.type_mark().to_owned()),
                NamedEntityKind::ObjectAlias { type_mark, .. } => Ok(type_mark.clone()),
                NamedEntityKind::ElementDeclaration(subtype) => Ok(subtype.type_mark().to_owned()),
                _ => Err(AnalysisError::NotFatal(
                    named_entity.kind_error(pos, expected),
                )),
            }
        }
    }

    fn analyze_attribute_name(
        &self,
        region: &Region<'_>,
        attr: &mut AttributeName,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        // @TODO more, attr must be checked inside the region of attributes of prefix
        let AttributeName {
            name,
            signature,
            expr,
            ..
        } = attr;

        self.resolve_name(region, &name.pos, &mut name.item, diagnostics)?;

        if let Some(ref mut signature) = signature {
            if let Err(err) = self.resolve_signature(region, signature) {
                err.add_to(diagnostics)?;
            }
        }
        if let Some(ref mut expr) = expr {
            self.analyze_expression(region, expr, diagnostics)?;
        }
        Ok(())
    }

    pub fn analyze_range(
        &self,
        region: &Region<'_>,
        range: &mut Range,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        match range {
            Range::Range(ref mut constraint) => {
                self.analyze_expression(region, &mut constraint.left_expr, diagnostics)?;
                self.analyze_expression(region, &mut constraint.right_expr, diagnostics)?;
            }
            Range::Attribute(ref mut attr) => {
                self.analyze_attribute_name(region, attr, diagnostics)?
            }
        }
        Ok(())
    }

    pub fn analyze_discrete_range(
        &self,
        region: &Region<'_>,
        drange: &mut DiscreteRange,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        match drange {
            DiscreteRange::Discrete(ref mut type_mark, ref mut range) => {
                if let Err(err) = self.resolve_type_mark_name(region, type_mark) {
                    err.add_to(diagnostics)?;
                }
                if let Some(ref mut range) = range {
                    self.analyze_range(region, range, diagnostics)?;
                }
            }
            DiscreteRange::Range(ref mut range) => {
                self.analyze_range(region, range, diagnostics)?;
            }
        }
        Ok(())
    }

    pub fn analyze_choices(
        &self,
        region: &Region<'_>,
        choices: &mut [Choice],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        for choice in choices.iter_mut() {
            match choice {
                Choice::Expression(ref mut expr) => {
                    self.analyze_expression(region, expr, diagnostics)?;
                }
                Choice::DiscreteRange(ref mut drange) => {
                    self.analyze_discrete_range(region, drange, diagnostics)?;
                }
                Choice::Others => {}
            }
        }
        Ok(())
    }

    pub fn analyze_expression(
        &self,
        region: &Region<'_>,
        expr: &mut WithPos<Expression>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        self.analyze_expression_pos(region, &expr.pos, &mut expr.item, diagnostics)
    }

    pub fn analyze_waveform(
        &self,
        region: &Region<'_>,
        wavf: &mut Waveform,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        match wavf {
            Waveform::Elements(ref mut elems) => {
                for elem in elems.iter_mut() {
                    let WaveformElement { value, after } = elem;
                    self.analyze_expression(region, value, diagnostics)?;
                    if let Some(expr) = after {
                        self.analyze_expression(region, expr, diagnostics)?;
                    }
                }
            }
            Waveform::Unaffected => {}
        }
        Ok(())
    }

    pub fn analyze_assoc_elems(
        &self,
        region: &Region<'_>,
        elems: &mut [AssociationElement],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        for AssociationElement { actual, .. } in elems.iter_mut() {
            match actual.item {
                ActualPart::Expression(ref mut expr) => {
                    self.analyze_expression_pos(region, &actual.pos, expr, diagnostics)?;
                }
                ActualPart::Open => {}
            }
        }
        Ok(())
    }

    pub fn resolve_formal_name(
        &self,
        formal_region: &Region<'_>,
        region: &Region<'_>,
        name_pos: &SrcPos,
        name: &mut Name,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult<Option<NamedEntities>> {
        match name {
            Name::Selected(prefix, suffix) => {
                suffix.clear_reference();

                match self.resolve_formal_name(
                    formal_region,
                    region,
                    &prefix.pos,
                    &mut prefix.item,
                    diagnostics,
                )? {
                    Some(NamedEntities::Single(ref named_entity)) => {
                        match self.lookup_selected(&prefix.pos, named_entity, suffix) {
                            Ok(visible) => {
                                suffix.set_reference(&visible);
                                Ok(Some(visible))
                            }
                            Err(err) => {
                                err.add_to(diagnostics)?;
                                Ok(None)
                            }
                        }
                    }
                    Some(NamedEntities::Overloaded(..)) => Ok(None),
                    None => Ok(None),
                }
            }

            Name::SelectedAll(_) => {
                diagnostics.error(name_pos, "Invalid formal");
                Ok(None)
            }
            Name::Designator(designator) => {
                designator.clear_reference();
                match formal_region.lookup_within(name_pos, designator.designator()) {
                    Ok(visible) => {
                        designator.set_reference(&visible);
                        Ok(Some(visible))
                    }
                    Err(diagnostic) => {
                        diagnostics.push(diagnostic);
                        Ok(None)
                    }
                }
            }
            Name::Indexed(ref mut prefix, ref mut exprs) => {
                self.resolve_formal_name(
                    formal_region,
                    region,
                    &prefix.pos,
                    &mut prefix.item,
                    diagnostics,
                )?;
                for expr in exprs.iter_mut() {
                    self.analyze_expression(region, expr, diagnostics)?;
                }
                Ok(None)
            }

            Name::Slice(ref mut prefix, ref mut drange) => {
                self.resolve_formal_name(
                    formal_region,
                    region,
                    &prefix.pos,
                    &mut prefix.item,
                    diagnostics,
                )?;
                self.analyze_discrete_range(region, drange.as_mut(), diagnostics)?;
                Ok(None)
            }
            Name::Attribute(..) => {
                diagnostics.error(name_pos, "Invalid formal");
                Ok(None)
            }
            Name::FunctionCall(ref mut fcall) => {
                let prefix = if let Some(prefix) = fcall.name.item.prefix() {
                    prefix
                } else {
                    diagnostics.error(name_pos, "Invalid formal");
                    return Ok(None);
                };

                if formal_region
                    .lookup_within(name_pos, prefix.designator())
                    .is_err()
                {
                    // The prefix of the name was not found in the formal region
                    // it must be a type conversion or a single parameter function call
                    self.resolve_name(region, &fcall.name.pos, &mut fcall.name.item, diagnostics)?;
                    // @TODO check is type cast or function call

                    if let &mut [AssociationElement {
                        ref formal,
                        ref mut actual,
                    }] = &mut fcall.parameters[..]
                    {
                        if formal.is_some() {
                            diagnostics.error(name_pos, "Invalid formal conversion");
                        } else if let ActualPart::Expression(Expression::Name(
                            ref mut actual_name,
                        )) = actual.item
                        {
                            self.resolve_formal_name(
                                formal_region,
                                region,
                                &actual.pos,
                                actual_name.as_mut(),
                                diagnostics,
                            )?;
                        }
                    } else {
                        diagnostics.error(name_pos, "Invalid formal conversion");
                    }
                } else {
                    match self.resolve_formal_name(
                        formal_region,
                        region,
                        &fcall.name.pos,
                        &mut fcall.name.item,
                        diagnostics,
                    )? {
                        Some(NamedEntities::Single(ent)) => {
                            if ent.actual_kind().is_type() {
                                // A type conversion
                                // @TODO Ignore for now
                                self.analyze_assoc_elems(
                                    region,
                                    &mut fcall.parameters,
                                    diagnostics,
                                )?;
                            } else if let Some(indexed_name) = fcall.to_indexed() {
                                *name = indexed_name;

                                if let Name::Indexed(ref mut prefix, ref mut indexes) = name {
                                    if let Some(type_mark) = type_mark_of_sliced_or_indexed(&ent) {
                                        if let Err(err) = self.analyze_indexed_name(
                                            region,
                                            name_pos,
                                            prefix.suffix_pos(),
                                            type_mark,
                                            indexes,
                                            diagnostics,
                                        ) {
                                            err.add_to(diagnostics)?;
                                        }
                                    } else {
                                        diagnostics.error(
                                            prefix.suffix_pos(),
                                            format!("{} cannot be indexed", ent.describe()),
                                        )
                                    }
                                }
                            } else {
                                diagnostics.push(Diagnostic::error(
                                    &fcall.name.pos,
                                    format!(
                                        "{} cannot be the prefix of a function call",
                                        ent.describe()
                                    ),
                                ));

                                self.analyze_assoc_elems(
                                    region,
                                    &mut fcall.parameters,
                                    diagnostics,
                                )?;
                            }
                        }
                        Some(NamedEntities::Overloaded(..)) => {
                            // @TODO check function arguments
                            self.analyze_assoc_elems(region, &mut fcall.parameters, diagnostics)?;
                        }
                        None => {
                            self.analyze_assoc_elems(region, &mut fcall.parameters, diagnostics)?;
                        }
                    };
                }

                Ok(None)
            }
            Name::External(..) => {
                diagnostics.error(name_pos, "Invalid formal");
                Ok(None)
            }
        }
    }

    pub fn analyze_assoc_elems_with_formal_region(
        &self,
        formal_region: &Region<'_>,
        region: &Region<'_>,
        elems: &mut [AssociationElement],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        for AssociationElement { formal, actual } in elems.iter_mut() {
            if let Some(ref mut formal) = formal {
                self.resolve_formal_name(
                    formal_region,
                    region,
                    &formal.pos,
                    &mut formal.item,
                    diagnostics,
                )?;
            }
            match actual.item {
                ActualPart::Expression(ref mut expr) => {
                    self.analyze_expression_pos(region, &actual.pos, expr, diagnostics)?;
                }
                ActualPart::Open => {}
            }
        }
        Ok(())
    }

    pub fn analyze_procedure_call(
        &self,
        region: &Region<'_>,
        fcall: &mut FunctionCall,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        let FunctionCall { name, parameters } = fcall;

        if let Some(entities) = self.resolve_name(region, &name.pos, &mut name.item, diagnostics)? {
            match entities {
                NamedEntities::Single(ent) => {
                    let mut diagnostic = Diagnostic::error(&name.pos, "Invalid procedure call");
                    if let Some(decl_pos) = ent.decl_pos() {
                        diagnostic.add_related(
                            decl_pos,
                            format!("{} is not a procedure", ent.describe()),
                        );
                    }
                    diagnostics.push(diagnostic);
                }
                NamedEntities::Overloaded(names) => {
                    let mut found = false;

                    for ent in names.entities() {
                        // @TODO add wrapper for entities with known signatures
                        if let Some(sig) = ent.signature() {
                            if sig.return_type().is_none() {
                                found = true;
                                break;
                            }
                        }
                    }

                    if !found {
                        let mut diagnostic = Diagnostic::error(&name.pos, "Invalid procedure call");
                        for ent in names.sorted_entities() {
                            if let Some(decl_pos) = ent.decl_pos() {
                                diagnostic.add_related(
                                    decl_pos,
                                    format!("{} is not a procedure", ent.describe()),
                                );
                            }
                        }
                        diagnostics.push(diagnostic);
                    }
                }
            };
        }

        self.analyze_assoc_elems(region, parameters, diagnostics)
    }

    pub fn analyze_aggregate(
        &self,
        region: &Region<'_>,
        assocs: &mut [ElementAssociation],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        for assoc in assocs.iter_mut() {
            match assoc {
                ElementAssociation::Named(ref mut choices, ref mut expr) => {
                    for choice in choices.iter_mut() {
                        match choice {
                            Choice::Expression(..) => {
                                // @TODO could be record field so we cannot do more now
                            }
                            Choice::DiscreteRange(ref mut drange) => {
                                self.analyze_discrete_range(region, drange, diagnostics)?;
                            }
                            Choice::Others => {}
                        }
                    }
                    self.analyze_expression(region, expr, diagnostics)?;
                }
                ElementAssociation::Positional(ref mut expr) => {
                    self.analyze_expression(region, expr, diagnostics)?;
                }
            }
        }
        Ok(())
    }

    fn analyze_qualified_expression(
        &self,
        region: &Region<'_>,
        qexpr: &mut QualifiedExpression,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult<Option<TypeEnt>> {
        let QualifiedExpression { type_mark, expr } = qexpr;

        match self.resolve_type_mark(region, type_mark) {
            Ok(target_type) => {
                self.analyze_expression_with_target_type(region, &target_type, expr, diagnostics)?;
                Ok(Some(target_type))
            }
            Err(e) => {
                self.analyze_expression(region, expr, diagnostics)?;
                e.add_to(diagnostics)?;
                Ok(None)
            }
        }
    }

    fn analyze_expression_pos(
        &self,
        region: &Region<'_>,
        pos: &SrcPos,
        expr: &mut Expression,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        match expr {
            Expression::Binary(_, ref mut left, ref mut right) => {
                self.analyze_expression(region, left, diagnostics)?;
                self.analyze_expression(region, right, diagnostics)
            }
            Expression::Unary(_, ref mut inner) => {
                self.analyze_expression(region, inner, diagnostics)
            }
            Expression::Name(ref mut name) => {
                self.resolve_name(region, pos, name, diagnostics)?;
                Ok(())
            }
            Expression::Aggregate(ref mut assocs) => {
                self.analyze_aggregate(region, assocs, diagnostics)
            }
            Expression::Qualified(ref mut qexpr) => {
                self.analyze_qualified_expression(region, qexpr, diagnostics)?;
                Ok(())
            }

            Expression::New(ref mut alloc) => match alloc.item {
                Allocator::Qualified(ref mut qexpr) => {
                    self.analyze_qualified_expression(region, qexpr, diagnostics)?;
                    Ok(())
                }
                Allocator::Subtype(ref mut subtype) => {
                    self.analyze_subtype_indication(region, subtype, diagnostics)
                }
            },
            Expression::Literal(_) => Ok(()),
        }
    }

    /// Analyze an indexed name where the prefix entity is already known
    /// Returns the type of the array element
    pub fn analyze_indexed_name(
        &self,
        region: &Region<'_>,
        name_pos: &SrcPos,
        suffix_pos: &SrcPos,
        type_mark: &TypeEnt,
        indexes: &mut [WithPos<Expression>],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> AnalysisResult<TypeEnt> {
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
                diagnostics.push(dimension_mismatch(
                    name_pos,
                    base_type,
                    indexes.len(),
                    index_types.len(),
                ))
            }

            for index in indexes.iter_mut() {
                self.analyze_expression(region, index, diagnostics)?;
            }

            Ok(elem_type.clone())
        } else {
            Err(Diagnostic::error(
                suffix_pos,
                format!("{} cannot be indexed", type_mark.describe()),
            )
            .into())
        }
    }

    pub fn analyze_sliced_name(
        &self,
        suffix_pos: &SrcPos,
        type_mark: &TypeEnt,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        let base_type = type_mark.base_type();

        let base_type = if let Type::Access(ref subtype, ..) = base_type.kind() {
            subtype.base_type()
        } else {
            base_type
        };

        if let Type::Array { .. } = base_type.kind() {
        } else {
            diagnostics.error(
                suffix_pos,
                format!("{} cannot be sliced", type_mark.describe()),
            );
        }

        Ok(())
    }

    /// Function call cannot be distinguished from indexed names when parsing
    /// Use the named entity kind to disambiguate
    pub fn analyze_ambiguous_function_call(
        &self,
        region: &Region<'_>,
        name_pos: &SrcPos,
        name: &mut Name,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        match name {
            Name::FunctionCall(ref mut fcall) => {
                match self.resolve_name(
                    region,
                    &fcall.name.pos,
                    &mut fcall.name.item,
                    diagnostics,
                )? {
                    Some(NamedEntities::Single(ent)) => {
                        if ent.actual_kind().is_type() {
                            // A type conversion
                            // @TODO Ignore for now
                            self.analyze_assoc_elems(region, &mut fcall.parameters, diagnostics)?;
                        } else if let Some(indexed_name) = fcall.to_indexed() {
                            *name = indexed_name;

                            if let Name::Indexed(ref mut prefix, ref mut indexes) = name {
                                if let Some(type_mark) = type_mark_of_sliced_or_indexed(&ent) {
                                    if let Err(err) = self.analyze_indexed_name(
                                        region,
                                        name_pos,
                                        prefix.suffix_pos(),
                                        type_mark,
                                        indexes,
                                        diagnostics,
                                    ) {
                                        err.add_to(diagnostics)?;
                                    }
                                } else {
                                    diagnostics.error(
                                        prefix.suffix_pos(),
                                        format!("{} cannot be indexed", ent.describe()),
                                    )
                                }
                            }
                        } else {
                            diagnostics.push(Diagnostic::error(
                                &fcall.name.pos,
                                format!(
                                    "{} cannot be the prefix of a function call",
                                    ent.describe()
                                ),
                            ));

                            self.analyze_assoc_elems(region, &mut fcall.parameters, diagnostics)?;
                        }
                    }
                    Some(NamedEntities::Overloaded(..)) => {
                        // @TODO check function arguments
                        self.analyze_assoc_elems(region, &mut fcall.parameters, diagnostics)?;
                    }
                    None => {
                        self.analyze_assoc_elems(region, &mut fcall.parameters, diagnostics)?;
                    }
                };
            }
            _ => {
                debug_assert!(false);
            }
        }
        Ok(())
    }

    pub fn analyze_name_with_target_type(
        &self,
        region: &Region<'_>,
        target_type: &TypeEnt,
        name_pos: &SrcPos,
        name: &mut Name,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        match name {
            Name::Designator(designator) => {
                designator.clear_reference();

                match region.lookup_within(name_pos, designator.designator()) {
                    Ok(entities) => {
                        // If the name is unique it is more helpful to get a reference
                        // Even if the type has a mismatch
                        designator.set_reference(&entities);

                        match entities {
                            NamedEntities::Single(ent) => {
                                designator.set_unique_reference(&ent);
                                ent.resolve(target_type, name_pos, diagnostics);
                            }
                            NamedEntities::Overloaded(overloaded) => {
                                overloaded.resolve(target_type, name_pos, designator, diagnostics)
                            }
                        }
                    }

                    Err(diagnostic) => {
                        diagnostics.push(diagnostic);
                    }
                }
            }
            Name::Selected(prefix, designator) => {
                designator.clear_reference();

                if let Some(NamedEntities::Single(ref named_entity)) =
                    self.resolve_name(region, &prefix.pos, &mut prefix.item, diagnostics)?
                {
                    match self.lookup_selected(&prefix.pos, named_entity, designator) {
                        Ok(entities) => {
                            // If the name is unique it is more helpful to get a reference
                            // Even if the type has a mismatch
                            designator.set_reference(&entities);
                            match entities {
                                NamedEntities::Single(ent) => {
                                    designator.set_unique_reference(&ent);
                                    ent.resolve(target_type, &designator.pos, diagnostics);
                                }
                                NamedEntities::Overloaded(overloaded) => overloaded.resolve(
                                    target_type,
                                    &designator.pos,
                                    &mut designator.item,
                                    diagnostics,
                                ),
                            }
                        }
                        Err(err) => {
                            err.add_to(diagnostics)?;
                        }
                    }
                }
            }
            Name::FunctionCall(..) => {
                self.analyze_ambiguous_function_call(region, name_pos, name, diagnostics)?;
            }

            Name::Slice(ref mut prefix, ref mut drange) => {
                if let Some(NamedEntities::Single(ref named_entity)) =
                    self.resolve_name(region, &prefix.pos, &mut prefix.item, diagnostics)?
                {
                    if let Some(type_mark) = type_mark_of_sliced_or_indexed(named_entity) {
                        self.analyze_sliced_name(prefix.suffix_pos(), type_mark, diagnostics)?;
                    } else {
                        diagnostics.error(
                            prefix.suffix_pos(),
                            format!("{} cannot be sliced", named_entity.describe()),
                        )
                    }
                }

                self.analyze_discrete_range(region, drange.as_mut(), diagnostics)?;
            }
            _ => {
                self.resolve_name(region, name_pos, name, diagnostics)?;
            }
        }

        Ok(())
    }

    pub fn analyze_literal_with_target_type(
        &self,
        target_type: &TypeEnt,
        pos: &SrcPos,
        literal: &Literal,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        let target_base = target_type.base_type();

        match literal {
            Literal::AbstractLiteral(AbstractLiteral::Integer(_)) => {
                if !matches!(target_base.kind(), Type::Integer(..)) {
                    diagnostics.push(Diagnostic::error(
                        pos,
                        format!("integer literal does not match {}", target_type.describe()),
                    ));
                }
            }
            Literal::Character(char) => match target_base.kind() {
                Type::Enum(literals) => {
                    if !literals.contains_key(&Designator::Character(*char)) {
                        diagnostics.push(Diagnostic::error(
                            pos,
                            format!(
                                "character literal does not match {}",
                                target_type.describe()
                            ),
                        ));
                    }
                }
                _ => {
                    diagnostics.push(Diagnostic::error(
                        pos,
                        format!(
                            "character literal does not match {}",
                            target_type.describe()
                        ),
                    ));
                }
            },
            Literal::String(string_lit) => match target_base.kind() {
                Type::Array {
                    indexes, elem_type, ..
                } => {
                    if indexes.len() == 1 {
                        match elem_type.base_type().kind() {
                            Type::Enum(literals) => {
                                for chr in string_lit.chars() {
                                    let chr = Designator::Character(*chr);
                                    if !literals.contains_key(&chr) {
                                        diagnostics.push(Diagnostic::error(
                                            pos,
                                            format!(
                                                "{} does not define character {}",
                                                elem_type.describe(),
                                                chr
                                            ),
                                        ))
                                    }
                                }
                            }
                            _ => {
                                // String literal can only be matched with single dimensional array
                                // With elements of enumeration type such as character
                                diagnostics.push(Diagnostic::error(
                                    pos,
                                    format!(
                                        "string literal does not match {}",
                                        target_type.describe()
                                    ),
                                ))
                            }
                        }
                    } else {
                        // String literal can only be matched with single dimensional array
                        diagnostics.push(Diagnostic::error(
                            pos,
                            format!("string literal does not match {}", target_type.describe()),
                        ))
                    }
                }
                _ => {
                    diagnostics.push(Diagnostic::error(
                        pos,
                        format!("string literal does not match {}", target_type.describe()),
                    ));
                }
            },

            _ => {}
        }

        Ok(())
    }

    pub fn analyze_expression_with_target_type(
        &self,
        region: &Region<'_>,
        target_type: &TypeEnt,
        expr: &mut WithPos<Expression>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        match expr.item {
            Expression::Literal(ref lit) => {
                self.analyze_literal_with_target_type(target_type, &expr.pos, lit, diagnostics)
            }
            Expression::Name(ref mut name) => self.analyze_name_with_target_type(
                region,
                target_type,
                &expr.pos,
                name,
                diagnostics,
            ),
            Expression::Qualified(ref mut qexpr) => {
                if let Some(type_mark) =
                    self.analyze_qualified_expression(region, qexpr, diagnostics)?
                {
                    if target_type.base_type() != type_mark.base_type() {
                        diagnostics.push(type_mismatch(&expr.pos, &type_mark, target_type));
                    }
                }
                Ok(())
            }
            _ => self.analyze_expression(region, expr, diagnostics),
        }
    }

    // @TODO maybe make generic function for expression/waveform.
    // wait until type checking to see if it makes sense
    pub fn analyze_expr_assignment(
        &self,
        region: &Region<'_>,
        target: &mut WithPos<Target>,
        assignment_type: AssignmentType,
        rhs: &mut AssignmentRightHand<WithPos<Expression>>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        match rhs {
            AssignmentRightHand::Simple(expr) => {
                self.analyze_target(region, target, assignment_type, diagnostics)?;
                self.analyze_expression(region, expr, diagnostics)?;
            }
            AssignmentRightHand::Conditional(conditionals) => {
                let Conditionals {
                    conditionals,
                    else_item,
                } = conditionals;
                self.analyze_target(region, target, assignment_type, diagnostics)?;
                for conditional in conditionals {
                    let Conditional { condition, item } = conditional;
                    self.analyze_expression(region, item, diagnostics)?;
                    self.analyze_expression(region, condition, diagnostics)?;
                }
                if let Some(expr) = else_item {
                    self.analyze_expression(region, expr, diagnostics)?;
                }
            }
            AssignmentRightHand::Selected(selection) => {
                let Selection {
                    expression,
                    alternatives,
                } = selection;
                self.analyze_expression(region, expression, diagnostics)?;
                // target is located after expression
                self.analyze_target(region, target, assignment_type, diagnostics)?;
                for Alternative { choices, item } in alternatives.iter_mut() {
                    self.analyze_expression(region, item, diagnostics)?;
                    self.analyze_choices(region, choices, diagnostics)?;
                }
            }
        }
        Ok(())
    }

    pub fn analyze_waveform_assignment(
        &self,
        region: &Region<'_>,
        target: &mut WithPos<Target>,
        assignment_type: AssignmentType,
        rhs: &mut AssignmentRightHand<Waveform>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult {
        match rhs {
            AssignmentRightHand::Simple(wavf) => {
                self.analyze_target(region, target, assignment_type, diagnostics)?;
                self.analyze_waveform(region, wavf, diagnostics)?;
            }
            AssignmentRightHand::Conditional(conditionals) => {
                let Conditionals {
                    conditionals,
                    else_item,
                } = conditionals;
                self.analyze_target(region, target, assignment_type, diagnostics)?;
                for conditional in conditionals {
                    let Conditional { condition, item } = conditional;
                    self.analyze_waveform(region, item, diagnostics)?;
                    self.analyze_expression(region, condition, diagnostics)?;
                }
                if let Some(wavf) = else_item {
                    self.analyze_waveform(region, wavf, diagnostics)?;
                }
            }
            AssignmentRightHand::Selected(selection) => {
                let Selection {
                    expression,
                    alternatives,
                } = selection;
                self.analyze_expression(region, expression, diagnostics)?;
                // target is located after expression
                self.analyze_target(region, target, assignment_type, diagnostics)?;
                for Alternative { choices, item } in alternatives.iter_mut() {
                    self.analyze_waveform(region, item, diagnostics)?;
                    self.analyze_choices(region, choices, diagnostics)?;
                }
            }
        }
        Ok(())
    }
}

fn type_mark_of_sliced_or_indexed(ent: &Arc<NamedEntity>) -> Option<&TypeEnt> {
    Some(match ent.kind() {
        NamedEntityKind::NonObjectAlias(ref alias) => {
            return type_mark_of_sliced_or_indexed(alias);
        }
        NamedEntityKind::Object(ref ent) => ent.subtype.type_mark(),
        NamedEntityKind::DeferredConstant(ref subtype) => subtype.type_mark(),
        NamedEntityKind::ElementDeclaration(ref subtype) => subtype.type_mark(),
        NamedEntityKind::ObjectAlias { type_mark, .. } => type_mark,
        _ => {
            return None;
        }
    })
}

enum OverloadedMatches<'a> {
    Unique(&'a Arc<NamedEntity>),
    Ambiguous(Vec<&'a Arc<NamedEntity>>),
    WrongArgumentCount(Vec<&'a Arc<NamedEntity>>),
    None,
}

impl OverloadedName {
    pub fn resolve(
        &self,
        target_type: &TypeEnt,
        pos: &SrcPos,
        designator: &mut WithRef<Designator>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) {
        match self.match_target_type(target_type) {
            OverloadedMatches::Unique(ent) => {
                designator.set_unique_reference(ent);
            }
            OverloadedMatches::Ambiguous(candidates) => {
                diagnostics.push(ambiguous_overloaded_type(
                    pos,
                    designator,
                    candidates.as_ref(),
                ));
            }
            OverloadedMatches::WrongArgumentCount(candidates) => {
                let mut diagnostic = Diagnostic::error(
                    pos,
                    format!(
                        "Wrong number of arguments in call to subprogram '{}'",
                        designator
                    ),
                );
                diagnostic.add_subprogram_candidates("migth be", &candidates);
                diagnostics.push(diagnostic);
            }
            OverloadedMatches::None => {
                let mut diagnostic = Diagnostic::error(
                    pos,
                    format!("'{}' does not match {}", designator, target_type.describe()),
                );
                if self.as_unique().is_none() {
                    diagnostic.add_subprogram_candidates("does not match", &self.sorted_entities());
                }
                diagnostics.push(diagnostic);
            }
        }
    }

    /// Match the type of overloaded name with the target type
    /// Returns an unique match if found
    ///  ... or return a vector of ambiguous candidates
    //   ... or an empty array if no match
    /// An overloaded name is a simple or selected name without any arguments
    /// such as an enumeration literal or function call without any arguments
    fn match_target_type<'e>(&'e self, target_type: &TypeEnt) -> OverloadedMatches<'e> {
        let target_base = target_type.base_type();
        let mut candidates = Vec::new();
        let mut wrong_args = Vec::new();

        for ent in self.entities() {
            if let Some(signature) = ent.signature() {
                if signature.match_return_type(target_base) {
                    if signature.can_be_called_without_parameters() {
                        candidates.push(ent);
                    } else {
                        wrong_args.push(ent);
                    }
                }
            }
        }

        if candidates.len() == 1 {
            OverloadedMatches::Unique(candidates[0])
        } else if !candidates.is_empty() {
            candidates.sort_by_key(|ent| ent.decl_pos());
            OverloadedMatches::Ambiguous(candidates)
        } else if !wrong_args.is_empty() {
            wrong_args.sort_by_key(|ent| ent.decl_pos());
            OverloadedMatches::WrongArgumentCount(wrong_args)
        } else {
            OverloadedMatches::None
        }
    }
}

fn ambiguous_overloaded_type(
    pos: &SrcPos,
    name: impl std::fmt::Display,
    candidates: &[&Arc<NamedEntity>],
) -> Diagnostic {
    let mut diagnostic = Diagnostic::error(pos, format!("ambiguous use of '{}'", name));
    diagnostic.add_subprogram_candidates("migth be", candidates);
    diagnostic
}

impl Diagnostic {
    fn add_subprogram_candidates(&mut self, prefix: &str, candidates: &[&Arc<NamedEntity>]) {
        for ent in candidates {
            if let Some(signature) = ent.signature() {
                if let Some(decl_pos) = ent.decl_pos() {
                    self.add_related(
                        decl_pos,
                        format!("{} {}{}", prefix, ent.designator(), signature.describe()),
                    )
                }
            }
        }
    }
}

impl NamedEntity {
    pub fn kind_error(&self, pos: &SrcPos, expected: &str) -> Diagnostic {
        let mut error = Diagnostic::error(
            pos,
            format!("Expected {}, got {}", expected, self.describe()),
        );
        if let Some(decl_pos) = self.decl_pos() {
            error.add_related(decl_pos, "Defined here");
        }
        error
    }

    fn resolve(
        &self,
        target_type: &TypeEnt,
        pos: &SrcPos,
        diagnostics: &mut dyn DiagnosticHandler,
    ) {
        if !self.match_with_target_type(target_type) {
            diagnostics.push(type_mismatch(pos, self, target_type));
        }
    }

    /// Match a named entity with a target type
    /// Returns a diagnostic in case of mismatch
    fn match_with_target_type(&self, target_type: &TypeEnt) -> bool {
        let typ = match self.actual_kind() {
            NamedEntityKind::ObjectAlias { ref type_mark, .. } => type_mark.base_type(),
            NamedEntityKind::Object(ref ent) => ent.subtype.base_type(),
            NamedEntityKind::DeferredConstant(ref subtype) => subtype.base_type(),
            NamedEntityKind::ElementDeclaration(ref subtype) => subtype.base_type(),
            NamedEntityKind::PhysicalLiteral(ref base_type) => base_type,

            // Ignore now to avoid false positives
            NamedEntityKind::LoopParameter => return true,
            _ => {
                return false;
            }
        };

        typ == target_type.base_type()
    }
}

fn type_mismatch(pos: &SrcPos, ent: &NamedEntity, expected_type: &NamedEntity) -> Diagnostic {
    Diagnostic::error(
        pos,
        format!(
            "{} does not match {}",
            ent.describe(),
            expected_type.describe()
        ),
    )
}

pub fn invalid_selected_name_prefix(named_entity: &NamedEntity, prefix: &SrcPos) -> Diagnostic {
    Diagnostic::error(
        prefix,
        capitalize(&format!(
            "{} may not be the prefix of a selected name",
            named_entity.describe(),
        )),
    )
}

pub fn no_declaration_within(
    named_entity: &NamedEntity,
    suffix: &WithPos<WithRef<Designator>>,
) -> Diagnostic {
    Diagnostic::error(
        suffix.as_ref(),
        format!(
            "No declaration of '{}' within {}",
            suffix.item,
            named_entity.describe(),
        ),
    )
}

fn plural(singular: &'static str, plural: &'static str, count: usize) -> &'static str {
    if count == 1 {
        singular
    } else {
        plural
    }
}

fn dimension_mismatch(
    pos: &SrcPos,
    base_type: &TypeEnt,
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
