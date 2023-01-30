//! This Source Code Form is subject to the terms of the Mozilla Public
//! License, v. 2.0. If a copy of the MPL was not distributed with this file,
//! You can obtain one at http://mozilla.org/MPL/2.0/.
//!
//! Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use fnv::FnvHashSet;

use super::analyze::*;
use super::formal_region::RecordRegion;
use super::named_entity::*;
use super::overloaded::Disambiguated;
use super::overloaded::DisambiguatedType;
use super::region::*;
use crate::ast::*;
use crate::data::*;

#[derive(Debug, PartialEq, Eq)]
pub enum ExpressionType<'a> {
    Unambiguous(TypeEnt<'a>),
    Ambiguous(FnvHashSet<BaseType<'a>>),
    String,
    Null,
    Aggregate,
}

impl<'a> ExpressionType<'a> {
    fn match_type(&self, other: BaseType<'a>) -> bool {
        match self {
            ExpressionType::Unambiguous(typ) => typ.base() == other,
            ExpressionType::Ambiguous(types) => types.contains(&other),
            ExpressionType::String => other.is_compatible_with_string_literal(),
            ExpressionType::Null => other.is_access(),
            ExpressionType::Aggregate => other.is_composite(),
        }
    }
}

impl<'a> From<DisambiguatedType<'a>> for ExpressionType<'a> {
    fn from(value: DisambiguatedType<'a>) -> Self {
        match value {
            DisambiguatedType::Unambiguous(typ) => ExpressionType::Unambiguous(typ),
            DisambiguatedType::Ambiguous(types) => ExpressionType::Ambiguous(types),
        }
    }
}

pub(crate) struct TypeMatcher<'c, 'a> {
    // Allow implicit type conversion from universal real/integer to other integer types
    implicit_type_conversion: bool,
    context: &'c AnalyzeContext<'a>,
}

impl<'c, 'a> TypeMatcher<'c, 'a> {
    // Returns true if the expression types is possible given the target type
    pub fn is_possible(&self, types: &ExpressionType<'a>, ttyp: BaseType<'a>) -> bool {
        if types.match_type(ttyp) {
            true
        } else if self.implicit_type_conversion {
            match ttyp.kind() {
                Type::Integer => types.match_type(self.context.universal_integer()),
                Type::Real => types.match_type(self.context.universal_real()),
                _ => false,
            }
        } else {
            false
        }
    }

    pub fn can_be_target_type(&self, typ: TypeEnt<'a>, ttyp: BaseType<'a>) -> bool {
        self.is_possible(&ExpressionType::Unambiguous(typ), ttyp)
    }

    pub fn disambiguate_op_by_arguments(
        &self,
        candidates: &mut Vec<OverloadedEnt<'a>>,
        operand_types: &[ExpressionType<'a>],
    ) {
        candidates.retain(|ent| {
            operand_types
                .iter()
                .enumerate()
                .all(|(idx, expr_type)| self.is_possible(expr_type, ent.nth_base(idx).unwrap()))
        })
    }

    pub fn disambiguate_op_by_return_type(
        &self,
        candidates: &mut Vec<OverloadedEnt<'a>>,
        ttyp: Option<TypeEnt<'a>>, // Optional target type constraint
    ) {
        let tbase = ttyp.map(|ttyp| ttyp.base());
        candidates.retain(|ent| {
            if let Some(tbase) = tbase {
                self.can_be_target_type(ent.return_type().unwrap(), tbase)
            } else {
                true
            }
        });
    }
}

impl<'a> AnalyzeContext<'a> {
    pub fn matcher_no_implicit(&self) -> TypeMatcher<'_, 'a> {
        TypeMatcher {
            implicit_type_conversion: false,
            context: self,
        }
    }

    pub fn matcher(&self) -> TypeMatcher<'_, 'a> {
        TypeMatcher {
            implicit_type_conversion: true,
            context: self,
        }
    }

    // Returns true if the expression types is possible given the target type
    pub fn is_possible(&self, types: &ExpressionType<'a>, ttyp: BaseType<'a>) -> bool {
        self.matcher().is_possible(types, ttyp)
    }

    pub fn can_be_target_type(&self, typ: TypeEnt<'a>, ttyp: BaseType<'a>) -> bool {
        self.matcher().can_be_target_type(typ, ttyp)
    }

    pub fn analyze_expression(
        &self,
        scope: &Scope<'a>,
        expr: &mut WithPos<Expression>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        self.analyze_expression_pos(scope, &expr.pos, &mut expr.item, diagnostics)
    }

    pub fn lookup_operator(
        &self,
        scope: &Scope<'a>,
        op_pos: &SrcPos,
        op: Operator,
        arity: usize,
    ) -> AnalysisResult<Vec<OverloadedEnt<'a>>> {
        let designator = Designator::OperatorSymbol(op);
        match scope.lookup(op_pos, &designator)? {
            NamedEntities::Single(ent) => {
                // Should never happen but better know if it does
                Err(Diagnostic::error(
                    op_pos,
                    format!(
                        "Operator symbol cannot denote non-overloaded symbol {}",
                        ent.describe(),
                    ),
                )
                .into())
            }
            NamedEntities::Overloaded(overloaded) => {
                // Candidates that match arity of operator
                let op_candidates: Vec<_> = overloaded
                    .entities()
                    .filter(|ent| ent.formals().len() == arity && ent.return_type().is_some())
                    .collect();

                if op_candidates.is_empty() {
                    Err(Diagnostic::error(
                        op_pos,
                        format!("Found no match for {}", designator.describe()),
                    )
                    .into())
                } else {
                    Ok(op_candidates)
                }
            }
        }
    }

    pub fn operand_types(
        &self,
        scope: &Scope<'a>,
        operands: &mut [&mut WithPos<Expression>],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult<Option<Vec<ExpressionType<'a>>>> {
        let mut operand_types = Vec::with_capacity(operands.len());

        let mut expr_diagnostics = Vec::new();
        for expr in operands.iter_mut() {
            if let Some(types) = self.expr_type(scope, expr, &mut expr_diagnostics)? {
                operand_types.push(types);
            } else {
                // bail if any operator argument is unknown
                diagnostics.append(expr_diagnostics);
                return Ok(None);
            }
        }

        Ok(Some(operand_types))
    }

    pub fn check_op(
        &self,
        scope: &Scope<'a>,
        op: &mut WithPos<WithRef<Operator>>,
        overloaded: OverloadedEnt<'a>,
        exprs: &mut [&mut WithPos<Expression>],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        op.set_unique_reference(&overloaded);
        for (idx, expr) in exprs.iter_mut().enumerate() {
            let target_type = overloaded.formals().nth(idx).unwrap().type_mark();
            self.expr_with_ttyp(scope, target_type, &expr.pos, &mut expr.item, diagnostics)?;
        }
        Ok(())
    }

    pub fn disambiguate_op(
        &self,
        scope: &Scope<'a>,
        ttyp: Option<TypeEnt<'a>>, // Optional target type constraint
        op: &mut WithPos<WithRef<Operator>>,
        overloaded: Vec<OverloadedEnt<'a>>,
        exprs: &mut [&mut WithPos<Expression>],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult<Option<Disambiguated<'a>>> {
        // @TODO lookup already set reference to get O(N) instead of O(N^2) when disambiguating deeply nested ambiguous operators
        if let Some(reference) = op.item.reference {
            if let Ok(ent) = OverloadedEnt::from_any(self.arena.get(reference)) {
                return Ok(Some(Disambiguated::Unambiguous(ent)));
            }
        }

        let designator = Designator::OperatorSymbol(op.item.item);
        let operand_types = if let Some(types) = self.operand_types(scope, exprs, diagnostics)? {
            types
        } else {
            return Ok(None);
        };

        let mut candidates = overloaded.clone();

        if candidates.len() > 1 {
            self.matcher()
                .disambiguate_op_by_arguments(&mut candidates, &operand_types);

            if candidates.len() > 1 && ttyp.is_some() {
                self.matcher()
                    .disambiguate_op_by_return_type(&mut candidates, ttyp);

                if candidates.len() > 1 {
                    self.matcher_no_implicit()
                        .disambiguate_op_by_arguments(&mut candidates, &operand_types);

                    if candidates.len() > 1 {
                        self.matcher_no_implicit()
                            .disambiguate_op_by_return_type(&mut candidates, ttyp);
                    }
                }
            }
        }

        if candidates.is_empty() {
            diagnostics.error(
                &op.pos,
                format!("Found no match for {}", designator.describe()),
            );

            Ok(None)
        } else if candidates.len() == 1 {
            let ent = candidates[0];
            self.check_op(scope, op, ent, exprs, diagnostics)?;
            Ok(Some(Disambiguated::Unambiguous(ent)))
        } else {
            Ok(Some(Disambiguated::Ambiguous(candidates)))
        }
    }

    pub fn operator_type(
        &self,
        scope: &Scope<'a>,
        op: &mut WithPos<WithRef<Operator>>,
        exprs: &mut [&mut WithPos<Expression>],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult<Option<ExpressionType<'a>>> {
        if !can_handle(op.item.item) {
            return Ok(None);
        }

        let op_candidates = match self.lookup_operator(scope, &op.pos, op.item.item, exprs.len()) {
            Ok(candidates) => candidates,
            Err(err) => {
                diagnostics.push(err.into_non_fatal()?);
                return Ok(None);
            }
        };

        match self.disambiguate_op(scope, None, op, op_candidates, exprs, diagnostics)? {
            Some(Disambiguated::Unambiguous(overloaded)) => Ok(Some(ExpressionType::Unambiguous(
                overloaded.return_type().unwrap(),
            ))),
            Some(Disambiguated::Ambiguous(overloaded)) => Ok(Some(ExpressionType::Ambiguous(
                overloaded
                    .into_iter()
                    .map(|o| o.return_type().unwrap().base())
                    .collect(),
            ))),
            None => Ok(None),
        }
    }

    pub fn expr_type(
        &self,
        scope: &Scope<'a>,
        expr: &mut WithPos<Expression>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult<Option<ExpressionType<'a>>> {
        self.expr_pos_type(scope, &expr.pos, &mut expr.item, diagnostics)
    }

    pub fn expr_pos_type(
        &self,
        scope: &Scope<'a>,
        expr_pos: &SrcPos,
        expr: &mut Expression,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult<Option<ExpressionType<'a>>> {
        match expr {
            Expression::Binary(ref mut op, ref mut left, ref mut right) => {
                self.operator_type(scope, op, &mut [left.as_mut(), right.as_mut()], diagnostics)
            }
            Expression::Unary(ref mut op, ref mut inner) => {
                self.operator_type(scope, op, &mut [inner.as_mut()], diagnostics)
            }
            Expression::Name(ref mut name) => self
                .expression_name_types(scope, expr_pos, name.as_mut(), diagnostics)
                .map(|value| value.map(ExpressionType::from)),
            Expression::Aggregate(_) => Ok(Some(ExpressionType::Aggregate)),
            Expression::Qualified(ref mut qexpr) => {
                let typ = self.analyze_qualified_expression(scope, qexpr, diagnostics)?;
                Ok(typ.map(ExpressionType::Unambiguous))
            }
            Expression::New(ref mut alloc) => match &mut alloc.item {
                Allocator::Qualified(ref mut qexpr) => {
                    let typ = self.analyze_qualified_expression(scope, qexpr, diagnostics)?;
                    Ok(typ.map(ExpressionType::Unambiguous))
                }
                Allocator::Subtype(ref mut subtype) => {
                    match self.resolve_subtype_indication(scope, subtype, diagnostics) {
                        Ok(typ) => Ok(Some(ExpressionType::Unambiguous(typ.type_mark()))),
                        Err(err) => {
                            diagnostics.push(err.into_non_fatal()?);
                            Ok(None)
                        }
                    }
                }
            },
            Expression::Literal(ref mut literal) => match literal {
                Literal::Physical(PhysicalLiteral { ref mut unit, .. }) => {
                    match self.resolve_physical_unit(scope, unit) {
                        Ok(typ) => Ok(Some(ExpressionType::Unambiguous(typ))),
                        Err(err) => {
                            diagnostics.push(err);
                            Ok(None)
                        }
                    }
                }
                Literal::String(_) => Ok(Some(ExpressionType::String)),
                Literal::BitString(_) => Ok(Some(ExpressionType::String)),
                Literal::Character(chr) => {
                    match scope.lookup(expr_pos, &Designator::Character(*chr)) {
                        Ok(NamedEntities::Single(ent)) => {
                            // Should never happen but better know if it does
                            diagnostics.error(
                                expr_pos,
                                format!(
                                    "Character literal cannot denote non-overloaded symbol {}",
                                    ent.describe(),
                                ),
                            );
                            Ok(None)
                        }
                        Ok(NamedEntities::Overloaded(overloaded)) => {
                            if overloaded.len() == 1 {
                                let ent = overloaded.first();
                                if let Some(return_type) = ent.return_type() {
                                    Ok(Some(ExpressionType::Unambiguous(return_type)))
                                } else {
                                    diagnostics.error(
                                        expr_pos,
                                        format!(
                                            "Character literal cannot denote procedure symbol {}",
                                            ent.describe(),
                                        ),
                                    );
                                    Ok(None)
                                }
                            } else {
                                Ok(Some(ExpressionType::Ambiguous(
                                    overloaded
                                        .entities()
                                        .flat_map(|e| e.return_type())
                                        .map(BaseType::from)
                                        .collect(),
                                )))
                            }
                        }
                        Err(e) => {
                            diagnostics.push(e);
                            Ok(None)
                        }
                    }
                }
                Literal::AbstractLiteral(AbstractLiteral::Integer(_)) => Ok(Some(
                    ExpressionType::Unambiguous(self.universal_integer().into()),
                )),
                Literal::AbstractLiteral(AbstractLiteral::Real(_)) => Ok(Some(
                    ExpressionType::Unambiguous(self.universal_real().into()),
                )),
                Literal::Null => Ok(Some(ExpressionType::Null)),
            },
        }
    }

    pub fn analyze_expression_pos(
        &self,
        scope: &Scope<'a>,
        pos: &SrcPos,
        expr: &mut Expression,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        self.expr_pos_type(scope, pos, expr, diagnostics)?;
        Ok(())
    }

    fn analyze_qualified_expression(
        &self,
        scope: &Scope<'a>,
        qexpr: &mut QualifiedExpression,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult<Option<TypeEnt<'a>>> {
        let QualifiedExpression { type_mark, expr } = qexpr;

        match self.resolve_type_mark(scope, type_mark) {
            Ok(target_type) => {
                self.expr_with_ttyp(scope, target_type, &expr.pos, &mut expr.item, diagnostics)?;
                Ok(Some(target_type))
            }
            Err(e) => {
                self.analyze_expression(scope, expr, diagnostics)?;
                e.add_to(diagnostics)?;
                Ok(None)
            }
        }
    }

    pub fn analyze_allocation(
        &self,
        scope: &Scope<'a>,
        alloc: &mut WithPos<Allocator>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        match &mut alloc.item {
            Allocator::Qualified(ref mut qexpr) => {
                self.analyze_qualified_expression(scope, qexpr, diagnostics)?;
            }
            Allocator::Subtype(ref mut subtype) => {
                self.analyze_subtype_indication(scope, subtype, diagnostics)?;
            }
        }
        Ok(())
    }

    /// Returns true if the name actually matches the target type
    /// None if it was uncertain
    pub fn expr_with_ttyp(
        &self,
        scope: &Scope<'a>,
        target_type: TypeEnt<'a>,
        expr_pos: &SrcPos,
        expr: &mut Expression,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        let target_base = target_type.base_type();
        match expr {
            Expression::Literal(ref mut lit) => self.analyze_literal_with_target_type(
                scope,
                target_type,
                expr_pos,
                lit,
                diagnostics,
            )?,
            Expression::Name(ref mut name) => self.expression_name_with_ttyp(
                scope,
                expr_pos,
                name.as_mut(),
                target_type,
                diagnostics,
            )?,
            Expression::Qualified(ref mut qexpr) => {
                if let Some(type_mark) =
                    self.analyze_qualified_expression(scope, qexpr, diagnostics)?
                {
                    if target_base != type_mark.base_type() {
                        diagnostics.push(Diagnostic::type_mismatch(
                            expr_pos,
                            &type_mark.describe(),
                            target_type,
                        ));
                    }
                }
            }
            Expression::Binary(ref mut op, ref mut left, ref mut right) => {
                if can_handle(op.item.item) {
                    let op_candidates = match self.lookup_operator(scope, &op.pos, op.item.item, 2)
                    {
                        Ok(candidates) => candidates,
                        Err(err) => {
                            diagnostics.push(err.into_non_fatal()?);
                            return Ok(());
                        }
                    };

                    match self.disambiguate_op(
                        scope,
                        Some(target_type),
                        op,
                        op_candidates,
                        &mut [left.as_mut(), right.as_mut()],
                        diagnostics,
                    )? {
                        Some(Disambiguated::Unambiguous(overloaded)) => {
                            let op_type = overloaded.return_type().unwrap();

                            if !self.can_be_target_type(op_type, target_type.base()) {
                                diagnostics.push(Diagnostic::type_mismatch(
                                    expr_pos,
                                    &op_type.describe(),
                                    target_type,
                                ));
                            }
                        }
                        Some(Disambiguated::Ambiguous(candidates)) => {
                            diagnostics.push(Diagnostic::ambiguous_op(
                                &op.pos,
                                op.item.item,
                                candidates,
                            ));
                        }
                        None => {}
                    }
                } else {
                    self.analyze_expression(scope, left, diagnostics)?;
                    self.analyze_expression(scope, right, diagnostics)?;
                }
            }
            Expression::Unary(ref mut op, ref mut expr) => {
                let op_candidates = match self.lookup_operator(scope, &op.pos, op.item.item, 1) {
                    Ok(candidates) => candidates,
                    Err(err) => {
                        diagnostics.push(err.into_non_fatal()?);
                        return Ok(());
                    }
                };

                match self.disambiguate_op(
                    scope,
                    Some(target_type),
                    op,
                    op_candidates,
                    &mut [expr.as_mut()],
                    diagnostics,
                )? {
                    Some(Disambiguated::Unambiguous(overloaded)) => {
                        let op_type = overloaded.return_type().unwrap();

                        if !self.can_be_target_type(op_type, target_type.base()) {
                            diagnostics.push(Diagnostic::type_mismatch(
                                expr_pos,
                                &op_type.describe(),
                                target_type,
                            ));
                        }
                    }
                    Some(Disambiguated::Ambiguous(candidates)) => {
                        diagnostics.push(Diagnostic::ambiguous_op(
                            &op.pos,
                            op.item.item,
                            candidates,
                        ));
                    }
                    None => {}
                }
            }
            Expression::Aggregate(assocs) => match target_base.kind() {
                Type::Array {
                    elem_type, indexes, ..
                } => {
                    if let [index_type] = indexes.as_slice() {
                        for assoc in assocs.iter_mut() {
                            self.analyze_1d_array_assoc_elem(
                                scope,
                                target_base,
                                *index_type,
                                *elem_type,
                                assoc,
                                diagnostics,
                            )?;
                        }
                    } else {
                        // @TODO multi dimensional array
                        self.analyze_aggregate(scope, assocs, diagnostics)?;
                    }
                }
                Type::Record(record_scope) => {
                    self.analyze_record_aggregate(
                        scope,
                        target_base,
                        record_scope,
                        assocs,
                        diagnostics,
                    )?;
                }
                _ => {
                    self.analyze_aggregate(scope, assocs, diagnostics)?;

                    diagnostics.error(
                        expr_pos,
                        format!("composite does not match {}", target_type.describe()),
                    );
                }
            },
            Expression::New(ref mut alloc) => {
                self.analyze_allocation(scope, alloc, diagnostics)?;
            }
        }

        Ok(())
    }

    pub fn analyze_aggregate(
        &self,
        scope: &Scope<'a>,
        assocs: &mut [ElementAssociation],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        for assoc in assocs.iter_mut() {
            match assoc {
                ElementAssociation::Named(ref mut choices, ref mut expr) => {
                    for choice in choices.iter_mut() {
                        match choice {
                            Choice::Expression(..) => {
                                // @TODO could be record field so we cannot do more now
                            }
                            Choice::DiscreteRange(ref mut drange) => {
                                self.analyze_discrete_range(scope, drange, diagnostics)?;
                            }
                            Choice::Others => {}
                        }
                    }
                    self.analyze_expression(scope, expr, diagnostics)?;
                }
                ElementAssociation::Positional(ref mut expr) => {
                    self.analyze_expression(scope, expr, diagnostics)?;
                }
            }
        }
        Ok(())
    }

    pub fn analyze_record_aggregate(
        &self,
        scope: &Scope<'a>,
        record_type: TypeEnt<'a>,
        elems: &RecordRegion<'a>,
        assocs: &mut [ElementAssociation],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        for assoc in assocs.iter_mut() {
            match assoc {
                ElementAssociation::Named(ref mut choices, ref mut actual_expr) => {
                    let elem = if let [choice] = choices.as_mut_slice() {
                        match choice {
                            Choice::Expression(choice_expr) => {
                                if let Some(simple_name) =
                                    as_name_mut(&mut choice_expr.item).and_then(as_simple_name_mut)
                                {
                                    if let Some(elem) = elems.lookup(&simple_name.item) {
                                        simple_name.set_unique_reference(&elem);
                                        Some(elem)
                                    } else {
                                        diagnostics.push(Diagnostic::no_declaration_within(
                                            &record_type,
                                            &choice_expr.pos,
                                            &simple_name.item,
                                        ));
                                        None
                                    }
                                } else {
                                    diagnostics.error(
                                        &choice_expr.pos,
                                        "Record aggregate choice must be a simple name",
                                    );
                                    None
                                }
                            }
                            Choice::DiscreteRange(_decl) => {
                                // @TODO not allowed for enum
                                None
                            }
                            Choice::Others => {
                                // @TODO handle specially
                                None
                            }
                        }
                    } else {
                        // @TODO not allowed for num
                        // Record aggregate can only have a single choice
                        None
                    };

                    if let Some(elem) = elem {
                        self.expr_with_ttyp(
                            scope,
                            elem.type_mark(),
                            &actual_expr.pos,
                            &mut actual_expr.item,
                            diagnostics,
                        )?;
                    } else {
                        self.analyze_expression(scope, actual_expr, diagnostics)?;
                    }
                }
                ElementAssociation::Positional(ref mut expr) => {
                    self.analyze_expression(scope, expr, diagnostics)?;
                }
            }
        }
        Ok(())
    }

    pub fn analyze_1d_array_assoc_elem(
        &self,
        scope: &Scope<'a>,
        array_type: TypeEnt<'a>,
        index_type: Option<TypeEnt<'a>>,
        elem_type: TypeEnt<'a>,
        assoc: &mut ElementAssociation,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        let mut can_be_array = true;

        let expr = match assoc {
            ElementAssociation::Named(ref mut choices, ref mut expr) => {
                for choice in choices.iter_mut() {
                    match choice {
                        Choice::Expression(index_expr) => {
                            match self.expr_as_discrete_range_type(
                                scope,
                                &index_expr.pos,
                                &mut index_expr.item,
                                diagnostics,
                            ) {
                                Ok(Some(_)) => {
                                    // @TODO check type matches index type
                                    can_be_array = true;
                                }
                                Ok(None) => {
                                    if let Some(index_type) = index_type {
                                        self.expr_with_ttyp(
                                            scope,
                                            index_type,
                                            &index_expr.pos,
                                            &mut index_expr.item,
                                            diagnostics,
                                        )?;
                                    }
                                    can_be_array = false;
                                }
                                Err(err) => {
                                    diagnostics.push(err.into_non_fatal()?);
                                    return Ok(());
                                }
                            }
                        }
                        Choice::DiscreteRange(ref mut drange) => {
                            if let Some(index_type) = index_type {
                                self.analyze_discrete_range_with_target_type(
                                    scope,
                                    index_type,
                                    drange,
                                    diagnostics,
                                )?;
                            } else {
                                self.analyze_discrete_range(scope, drange, diagnostics)?;
                            }
                        }
                        Choice::Others => {
                            // @TODO choice must be alone so cannot appear here
                            can_be_array = false;
                        }
                    }
                }
                expr
            }
            ElementAssociation::Positional(ref mut expr) => expr,
        };

        if can_be_array {
            // If the choice is only a range or positional the expression can be an array
            let types = if let Some(types) = self.expr_type(scope, expr, diagnostics)? {
                types
            } else {
                return Ok(());
            };

            let is_array = self.is_possible(&types, array_type.base());
            let is_elem = self.is_possible(&types, elem_type.base());

            if is_elem || !is_array {
                // Prefer element type in presence of ambiguity
                self.expr_with_ttyp(scope, elem_type, &expr.pos, &mut expr.item, diagnostics)?;
            } else if is_array {
                self.expr_with_ttyp(scope, array_type, &expr.pos, &mut expr.item, diagnostics)?;
            }
        } else {
            self.expr_with_ttyp(scope, elem_type, &expr.pos, &mut expr.item, diagnostics)?;
        }

        Ok(())
    }
}

// @TODO skip operators we do not handle yet
fn can_handle(op: Operator) -> bool {
    !matches!(
        op,
        Operator::QueEQ
            | Operator::QueNE
            | Operator::QueGT
            | Operator::QueGTE
            | Operator::QueLT
            | Operator::QueLTE
            | Operator::QueQue
    )
}

impl Diagnostic {
    fn ambiguous_op<'a>(
        pos: &SrcPos,
        op: Operator,
        candidates: impl IntoIterator<Item = OverloadedEnt<'a>>,
    ) -> Diagnostic {
        let mut diag = Diagnostic::error(
            pos,
            format!(
                "ambiguous use of {}",
                Designator::OperatorSymbol(op).describe()
            ),
        );
        diag.add_subprogram_candidates("migth be", candidates);
        diag
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::analysis::tests::NoDiagnostics;
    use crate::analysis::tests::TestSetup;
    use crate::data::DiagnosticHandler;
    use crate::syntax::test::check_diagnostics;
    use crate::syntax::test::without_releated;
    use crate::syntax::test::Code;

    impl<'a> TestSetup<'a> {
        fn expr_type(
            &'a self,
            code: &Code,
            diagnostics: &mut dyn DiagnosticHandler,
        ) -> Option<ExpressionType<'a>> {
            let mut expr = code.expr();
            self.ctx()
                .expr_type(&self.scope, &mut expr, diagnostics)
                .unwrap()
        }

        fn expr_with_ttyp(
            &'a self,
            code: &Code,
            ttyp: TypeEnt<'a>,
            diagnostics: &mut dyn DiagnosticHandler,
        ) {
            let mut expr = code.expr();
            self.ctx()
                .expr_with_ttyp(&self.scope, ttyp, &expr.pos, &mut expr.item, diagnostics)
                .unwrap()
        }
    }

    #[test]
    fn null_literal_expr_type() {
        let test = TestSetup::new();
        assert_eq!(
            test.expr_type(&test.snippet("null"), &mut NoDiagnostics),
            Some(ExpressionType::Null)
        );
    }

    #[test]
    fn string_literal_expr_type() {
        let test = TestSetup::new();
        assert_eq!(
            test.expr_type(&test.snippet("\"hello\""), &mut NoDiagnostics),
            Some(ExpressionType::String)
        );

        assert_eq!(
            test.expr_type(&test.snippet("x\"hello\""), &mut NoDiagnostics),
            Some(ExpressionType::String)
        );
    }

    #[test]
    fn character_literal_expr_type() {
        let test = TestSetup::new();
        assert_eq!(
            test.expr_type(&test.snippet("'a'"), &mut NoDiagnostics),
            Some(ExpressionType::Unambiguous(test.lookup_type("character")))
        );
    }

    #[test]
    fn character_literal_ambiguous_expr_type() {
        let test = TestSetup::new();
        test.declarative_part("type enum_t is ('a', 'b');");
        assert_eq!(
            test.expr_type(&test.snippet("'a'"), &mut NoDiagnostics),
            Some(ExpressionType::Ambiguous(
                [
                    test.lookup_type("character").base(),
                    test.lookup_type("enum_t").base()
                ]
                .into_iter()
                .collect()
            ))
        );
    }

    #[test]
    fn universal_integer_expr_type() {
        let test = TestSetup::new();
        assert_eq!(
            test.expr_type(&test.snippet("0"), &mut NoDiagnostics),
            Some(ExpressionType::Unambiguous(
                test.ctx().universal_integer().into()
            ))
        );
    }

    #[test]
    fn universal_real_expr_type() {
        let test = TestSetup::new();
        assert_eq!(
            test.expr_type(&test.snippet("0.0"), &mut NoDiagnostics),
            Some(ExpressionType::Unambiguous(
                test.ctx().universal_real().into()
            ))
        );
    }

    #[test]
    fn physical_literal_type() {
        let test = TestSetup::new();
        assert_eq!(
            test.expr_type(&test.snippet("1 ns"), &mut NoDiagnostics),
            Some(ExpressionType::Unambiguous(test.lookup_type("time")))
        );
    }

    #[test]
    fn qualified_allocator() {
        let test = TestSetup::new();
        test.declarative_part("type ptr_t is access integer_vector;");

        assert_eq!(
            test.expr_type(
                &test.snippet("new integer_vector'(0, 1)"),
                &mut NoDiagnostics
            ),
            Some(ExpressionType::Unambiguous(
                test.lookup_type("integer_vector")
            ))
        );

        // @TODO check type inside subtype indication
        assert_eq!(
            test.expr_type(
                &test.snippet("new integer_vector(0 to 1)"),
                &mut NoDiagnostics
            ),
            Some(ExpressionType::Unambiguous(
                test.lookup_type("integer_vector")
            ))
        );
    }

    #[test]
    fn binary_expression_types() {
        let test = TestSetup::new();

        let code = test.snippet("true and false");
        assert_eq!(
            test.expr_type(&code, &mut NoDiagnostics),
            Some(ExpressionType::Unambiguous(test.lookup_type("boolean")))
        );
    }

    #[test]
    fn binary_expression_typecheck_error() {
        let test = TestSetup::new();

        let code = test.snippet("0 and 1");
        let mut diagnostics = Vec::new();

        assert_eq!(test.expr_type(&code, &mut diagnostics), None);

        check_diagnostics(
            without_releated(&diagnostics),
            vec![Diagnostic::error(
                code.s1("and"),
                "Found no match for operator \"and\"",
            )],
        );
    }

    #[test]
    fn binary_expression_missing_names() {
        let test = TestSetup::new();

        let code = test.snippet("0 + missing");
        let mut diagnostics = Vec::new();

        assert_eq!(test.expr_type(&code, &mut diagnostics), None);

        check_diagnostics(
            without_releated(&diagnostics),
            vec![Diagnostic::error(
                code.s1("missing"),
                "No declaration of 'missing'",
            )],
        );
    }

    #[test]
    fn expression_ambiguous_name() {
        let test = TestSetup::new();
        test.declarative_part("type enum1_t is (alpha, beta);");
        test.declarative_part("type enum2_t is (alpha, beta);");

        let code = test.snippet("alpha");

        assert_eq!(
            test.expr_type(&code, &mut NoDiagnostics),
            Some(ExpressionType::Ambiguous(
                [
                    test.lookup_type("enum1_t").base(),
                    test.lookup_type("enum2_t").base()
                ]
                .into_iter()
                .collect()
            ))
        );
    }

    #[test]
    fn ambiguous_operator() {
        let test = TestSetup::new();
        test.declarative_part(
            "
function \"-\"(arg : bit_vector) return real;
function \"-\"(arg : string) return integer;
        ",
        );

        let code = test.snippet("- \"01\"");

        assert_eq!(
            test.expr_type(&code, &mut NoDiagnostics),
            Some(ExpressionType::Ambiguous(
                [
                    test.lookup_type("integer").base(),
                    test.lookup_type("real").base(),
                ]
                .into_iter()
                .collect()
            ))
        );
    }

    #[test]
    fn ambiguous_argument_when_return_types_are_all_the_same() {
        let test = TestSetup::new();
        let decls = test.declarative_part(
            "
        
function \"-\"(arg : bit_vector) return integer;
function \"-\"(arg : string) return integer;
        ",
        );

        let code = test.snippet("- \"01\"");
        let mut diagnostics = Vec::new();
        test.expr_with_ttyp(&code, test.lookup_type("INTEGER"), &mut diagnostics);
        check_diagnostics(
            diagnostics,
            vec![
                Diagnostic::error(code.s1("-"), "ambiguous use of operator \"-\"")
                    .related(
                        decls.s("\"-\"", 1),
                        "migth be \"-\"[BIT_VECTOR return INTEGER]",
                    )
                    .related(decls.s("\"-\"", 2), "migth be \"-\"[STRING return INTEGER]"),
            ],
        );
    }

    #[test]
    fn string_cannot_match_multi_dimensional_array() {
        let test = TestSetup::new();
        test.declarative_part(
            "
type arr_t is array (natural range <>) of string(1 to 3);
constant c0 : arr_t(0 to 0) := (0 => \"abc\");
        ",
        );

        let code = test.snippet("\"123\" & c0");
        assert_eq!(
            test.expr_type(&code, &mut NoDiagnostics),
            Some(ExpressionType::Unambiguous(test.lookup_type("arr_t")))
        );
    }

    #[test]
    fn aggregate_can_match_record() {
        let test = TestSetup::new();
        test.declarative_part(
            "
type rec_t is record
  f0: natural;
  f1: natural;
end record;
constant c0 : rec_t := (0, 1);
        ",
        );

        let code = test.snippet("c0 = (0, 1)");
        assert_eq!(
            test.expr_type(&code, &mut NoDiagnostics),
            Some(ExpressionType::Unambiguous(test.lookup_type("boolean")))
        );
    }
}
