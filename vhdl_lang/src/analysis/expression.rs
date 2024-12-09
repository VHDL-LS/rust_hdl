//! This Source Code Form is subject to the terms of the Mozilla Public
//! License, v. 2.0. If a copy of the MPL was not distributed with this file,
//! You can obtain one at http://mozilla.org/MPL/2.0/.
//!
//! Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use fnv::FnvHashMap;
use fnv::FnvHashSet;
use vhdl_lang::TokenAccess;

use super::analyze::*;
use super::overloaded::Disambiguated;
use super::overloaded::DisambiguatedType;
use super::overloaded::ResolvedCall;
use super::scope::*;
use crate::ast::token_range::{WithToken, WithTokenSpan};
use crate::ast::*;
use crate::data::error_codes::ErrorCode;
use crate::data::*;
use crate::named_entity::*;
use crate::{TokenId, TokenSpan};

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

    pub(crate) fn describe(&self) -> String {
        match self {
            ExpressionType::Unambiguous(typ) => format!("expression with {}", typ.describe()),
            ExpressionType::Ambiguous(_) => "ambiguous expression".to_owned(),
            ExpressionType::String => "string literal".to_owned(),
            ExpressionType::Null => "null literal".to_owned(),
            ExpressionType::Aggregate => "aggregate expression".to_owned(),
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

pub(super) struct TypeMatcher<'c, 'a, 't> {
    // Allow implicit type conversion from universal real/integer to other integer types
    implicit_type_conversion: bool,

    // Allow implicit type conversion from abstract types to universal real/integer
    implicit_type_conversion_from_universal: bool,
    context: &'c AnalyzeContext<'a, 't>,
}

impl<'a> TypeMatcher<'_, 'a, '_> {
    // Returns true if the expression types is possible given the target type
    pub fn is_possible(&self, types: &ExpressionType<'a>, ttyp: BaseType<'a>) -> bool {
        if types.match_type(ttyp) {
            true
        } else if self.implicit_type_conversion {
            match ttyp.kind() {
                Type::Integer => types.match_type(self.context.universal_integer()),
                Type::Real => types.match_type(self.context.universal_real()),
                Type::Universal(UniversalType::Integer)
                    if self.implicit_type_conversion_from_universal =>
                {
                    match types {
                        ExpressionType::Unambiguous(typ) => typ.base().is_any_integer(),
                        ExpressionType::Ambiguous(types) => {
                            types.iter().any(|typ| typ.is_any_integer())
                        }
                        _ => false,
                    }
                }
                Type::Universal(UniversalType::Real)
                    if self.implicit_type_conversion_from_universal =>
                {
                    match types {
                        ExpressionType::Unambiguous(typ) => typ.base().is_any_real(),
                        ExpressionType::Ambiguous(types) => {
                            types.iter().any(|typ| typ.is_any_real())
                        }
                        _ => false,
                    }
                }
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

    pub fn disambiguate_by_assoc_types(
        &self,
        actual_types: &[Option<ExpressionType<'a>>],
        candidates: &mut Vec<ResolvedCall<'a>>,
    ) {
        candidates.retain(|resolved| {
            actual_types.iter().enumerate().all(|(idx, actual_type)| {
                if let Some(actual_type) = actual_type {
                    self.is_possible(actual_type, resolved.formals[idx].base())
                } else {
                    true
                }
            })
        })
    }

    pub fn disambiguate_op_by_return_type(
        &self,
        candidates: &mut Vec<impl AsRef<OverloadedEnt<'a>>>,
        ttyp: Option<TypeEnt<'a>>, // Optional target type constraint
    ) {
        let tbase = ttyp.map(|ttyp| ttyp.base());
        candidates.retain(|ent| {
            if let Some(tbase) = tbase {
                self.can_be_target_type(ent.as_ref().return_type().unwrap(), tbase)
            } else {
                true
            }
        });
    }
}

impl<'a, 't> AnalyzeContext<'a, 't> {
    pub fn strict_matcher(&self) -> TypeMatcher<'_, 'a, 't> {
        TypeMatcher {
            implicit_type_conversion: false,
            implicit_type_conversion_from_universal: false,
            context: self,
        }
    }

    pub fn any_matcher(&self) -> TypeMatcher<'_, 'a, 't> {
        TypeMatcher {
            implicit_type_conversion: true,
            implicit_type_conversion_from_universal: true,
            context: self,
        }
    }

    pub fn implicit_matcher(&self) -> TypeMatcher<'_, 'a, 't> {
        TypeMatcher {
            implicit_type_conversion: true,
            implicit_type_conversion_from_universal: false,
            context: self,
        }
    }

    // Returns true if the expression types is possible given the target type
    pub fn is_possible(&self, types: &ExpressionType<'a>, ttyp: BaseType<'a>) -> bool {
        self.any_matcher().is_possible(types, ttyp)
    }

    pub fn common_type(&self, typ1: BaseType<'a>, typ2: BaseType<'a>) -> Option<BaseType<'a>> {
        if typ1.id() == typ2.id() {
            Some(typ1)
        } else if typ1.is_universal_of(typ2) {
            Some(typ2)
        } else if typ2.is_universal_of(typ1) {
            Some(typ1)
        } else {
            None
        }
    }

    pub fn common_types(
        &self,
        types: FnvHashSet<BaseType<'a>>,
        typ: BaseType<'a>,
    ) -> FnvHashSet<BaseType<'a>> {
        types
            .into_iter()
            .filter_map(|t| self.common_type(t, typ))
            .collect()
    }

    pub fn can_be_target_type(&self, typ: TypeEnt<'a>, ttyp: BaseType<'a>) -> bool {
        self.any_matcher().can_be_target_type(typ, ttyp)
    }

    pub fn check_type_mismatch(
        &self,
        typ: TypeEnt<'a>,
        ttyp: TypeEnt<'a>,
        pos: TokenSpan,
        diagnostics: &mut dyn DiagnosticHandler,
    ) {
        if !self.can_be_target_type(typ, ttyp.base()) {
            diagnostics.push(Diagnostic::type_mismatch(
                &pos.pos(self.ctx),
                &typ.describe(),
                ttyp,
            ));
        }
    }

    pub fn expr_unknown_ttyp(
        &self,
        scope: &Scope<'a>,
        expr: &mut WithTokenSpan<Expression>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        self.expr_pos_unknown_ttyp(scope, expr.span, &mut expr.item, diagnostics)
    }

    pub fn expr_unambiguous_type(
        &self,
        scope: &Scope<'a>,
        expr: &mut WithTokenSpan<Expression>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<TypeEnt<'a>> {
        match self.expr_type(scope, expr, diagnostics)? {
            ExpressionType::Unambiguous(typ) => Ok(typ),
            ExpressionType::Ambiguous(_)
            | ExpressionType::String
            | ExpressionType::Null
            | ExpressionType::Aggregate => {
                diagnostics.add(
                    expr.pos(self.ctx),
                    "Ambiguous expression. You can use a qualified expression type'(expr) to disambiguate.",
                    ErrorCode::AmbiguousExpression,
                );
                Err(EvalError::Unknown)
            }
        }
    }

    pub fn lookup_operator(
        &self,
        diagnostics: &mut dyn DiagnosticHandler,
        scope: &Scope<'a>,
        op_pos: TokenId,
        op: Operator,
        arity: usize,
    ) -> EvalResult<Vec<OverloadedEnt<'a>>> {
        let designator = Designator::OperatorSymbol(op);
        match scope
            .lookup(&designator)
            .map_err(|err| err.into_diagnostic(self.ctx, op_pos))
            .into_eval_result(diagnostics)?
        {
            NamedEntities::Single(ent) => {
                // Should never happen but better know if it does
                bail!(
                    diagnostics,
                    Diagnostic::internal(
                        op_pos.pos(self.ctx),
                        format!(
                            "Operator symbol cannot denote non-overloaded symbol {}",
                            ent.describe(),
                        ),
                    )
                );
            }
            NamedEntities::Overloaded(overloaded) => {
                // Candidates that match arity of operator
                let op_candidates: Vec<_> = overloaded
                    .entities()
                    .filter(|ent| ent.formals().len() == arity && ent.return_type().is_some())
                    .collect();

                if op_candidates.is_empty() {
                    bail!(
                        diagnostics,
                        Diagnostic::new(
                            self.ctx.get_pos(op_pos),
                            format!("Found no match for {}", designator.describe()),
                            ErrorCode::Unresolved,
                        )
                    );
                } else {
                    Ok(op_candidates)
                }
            }
        }
    }

    pub fn operand_types(
        &self,
        scope: &Scope<'a>,
        operands: &mut [&mut WithTokenSpan<Expression>],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<Vec<ExpressionType<'a>>> {
        let mut operand_types = Vec::with_capacity(operands.len());
        for expr in operands.iter_mut() {
            operand_types.push(self.expr_type(scope, expr, diagnostics)?);
        }
        Ok(operand_types)
    }

    pub fn check_op(
        &self,
        scope: &Scope<'a>,
        op: &mut WithToken<WithRef<Operator>>,
        overloaded: OverloadedEnt<'a>,
        exprs: &mut [&mut WithTokenSpan<Expression>],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        op.set_unique_reference(&overloaded);
        for (idx, expr) in exprs.iter_mut().enumerate() {
            let target_type = overloaded.formals().nth(idx).unwrap().type_mark();
            self.expr_pos_with_ttyp(scope, target_type, expr.span, &mut expr.item, diagnostics)?;
        }
        Ok(())
    }

    pub fn disambiguate_op(
        &self,
        scope: &Scope<'a>,
        ttyp: Option<TypeEnt<'a>>, // Optional target type constraint
        op: &mut WithToken<WithRef<Operator>>,
        overloaded: Vec<OverloadedEnt<'a>>,
        exprs: &mut [&mut WithTokenSpan<Expression>],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<Disambiguated<'a>> {
        // @TODO lookup already set reference to get O(N) instead of O(N^2) when disambiguating deeply nested ambiguous operators
        if let Some(reference) = op.item.reference.get() {
            if let Some(ent) = OverloadedEnt::from_any(self.arena.get(reference)) {
                return Ok(Disambiguated::Unambiguous(ent));
            }
        }

        let designator = Designator::OperatorSymbol(op.item.item);
        let operand_types = self.operand_types(scope, exprs, diagnostics)?;

        let mut candidates = overloaded.clone();

        if candidates.len() > 1 {
            self.implicit_matcher()
                .disambiguate_op_by_arguments(&mut candidates, &operand_types);
        }

        if candidates.len() > 1 && ttyp.is_some() {
            self.implicit_matcher()
                .disambiguate_op_by_return_type(&mut candidates, ttyp);
        }

        // Try to further disambiguate by removing implicit universal casts
        if candidates.len() > 1 {
            let return_types: FnvHashSet<_> = candidates
                .iter()
                .map(|c| c.return_type().unwrap().base())
                .collect();

            if return_types.len() == 1 {
                self.strict_matcher()
                    .disambiguate_op_by_arguments(&mut candidates, &operand_types);
            }
        }

        if candidates.len() > 1 {
            if ttyp.is_some() {
                self.strict_matcher()
                    .disambiguate_op_by_return_type(&mut candidates, ttyp);
            } else {
                let return_types: FnvHashSet<_> = candidates
                    .iter()
                    .map(|c| c.return_type().unwrap().base())
                    .collect();

                // Remove INTEGER if universal integer is a candidate
                candidates.retain(|cand| {
                    if let Some(univ) = self.as_universal(cand.return_type().unwrap().base()) {
                        !return_types.contains(&univ)
                    } else {
                        true
                    }
                })
            }
        }

        if candidates.is_empty() {
            // Try to disambiguate to a single candidate if return type ruled out all candidates
            // But it is unambiguous without implicit argument cast
            let mut cands = overloaded.clone();
            self.strict_matcher()
                .disambiguate_op_by_arguments(&mut cands, &operand_types);

            if cands.len() == 1 {
                candidates = cands;
            }
        }

        if candidates.is_empty() {
            diagnostics.add(
                self.ctx.get_pos(op.token),
                format!("Found no match for {}", designator.describe()),
                ErrorCode::Unresolved,
            );

            Err(EvalError::Unknown)
        } else if candidates.len() == 1 {
            let ent = candidates[0];
            self.check_op(scope, op, ent, exprs, diagnostics)?;
            Ok(Disambiguated::Unambiguous(ent))
        } else {
            Ok(Disambiguated::Ambiguous(candidates))
        }
    }

    fn as_universal(&self, typ: BaseType<'a>) -> Option<BaseType<'a>> {
        match typ.kind() {
            Type::Integer => Some(self.universal_integer()),
            Type::Real => Some(self.universal_real()),
            _ => None,
        }
    }

    pub fn operator_type(
        &self,
        scope: &Scope<'a>,
        op: &mut WithToken<WithRef<Operator>>,
        exprs: &mut [&mut WithTokenSpan<Expression>],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<ExpressionType<'a>> {
        let op_candidates =
            self.lookup_operator(diagnostics, scope, op.token, op.item.item, exprs.len())?;

        match self.disambiguate_op(scope, None, op, op_candidates, exprs, diagnostics)? {
            Disambiguated::Unambiguous(overloaded) => Ok(ExpressionType::Unambiguous(
                overloaded.return_type().unwrap(),
            )),
            Disambiguated::Ambiguous(overloaded) => Ok(ExpressionType::Ambiguous(
                overloaded
                    .into_iter()
                    .map(|o| o.return_type().unwrap().base())
                    .collect(),
            )),
        }
    }

    pub fn expr_type(
        &self,
        scope: &Scope<'a>,
        expr: &mut WithTokenSpan<Expression>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<ExpressionType<'a>> {
        self.expr_pos_type(scope, expr.span, &mut expr.item, diagnostics)
    }

    pub fn expr_pos_type(
        &self,
        scope: &Scope<'a>,
        span: TokenSpan,
        expr: &mut Expression,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<ExpressionType<'a>> {
        match expr {
            Expression::Binary(ref mut op, ref mut left, ref mut right) => {
                self.operator_type(scope, op, &mut [left.as_mut(), right.as_mut()], diagnostics)
            }
            Expression::Unary(ref mut op, ref mut inner) => {
                self.operator_type(scope, op, &mut [inner.as_mut()], diagnostics)
            }
            Expression::Name(ref mut name) => self
                .expression_name_types(scope, span, name.as_mut(), diagnostics)
                .map(ExpressionType::from),
            Expression::Aggregate(_) => Ok(ExpressionType::Aggregate),
            Expression::Qualified(ref mut qexpr) => {
                let typ = self.analyze_qualified_expression(scope, qexpr, diagnostics)?;
                Ok(ExpressionType::Unambiguous(typ))
            }
            Expression::New(ref mut alloc) => match &mut alloc.item {
                Allocator::Qualified(ref mut qexpr) => {
                    let typ = self.analyze_qualified_expression(scope, qexpr, diagnostics)?;
                    Ok(ExpressionType::Unambiguous(typ))
                }
                Allocator::Subtype(ref mut subtype) => self
                    .resolve_subtype_indication(scope, subtype, diagnostics)
                    .map(|typ| ExpressionType::Unambiguous(typ.type_mark())),
            },
            Expression::Parenthesized(expr) => {
                self.expr_pos_type(scope, expr.span, &mut expr.item, diagnostics)
            }
            Expression::Literal(ref mut literal) => match literal {
                Literal::Physical(PhysicalLiteral { ref mut unit, .. }) => {
                    match self.resolve_physical_unit(scope, unit) {
                        Ok(typ) => Ok(ExpressionType::Unambiguous(typ)),
                        Err(err) => {
                            diagnostics.push(err);
                            Err(EvalError::Unknown)
                        }
                    }
                }
                Literal::String(_) => Ok(ExpressionType::String),
                Literal::BitString(_) => Ok(ExpressionType::String),
                Literal::Character(chr) => {
                    match scope.lookup(&Designator::Character(*chr)) {
                        Ok(NamedEntities::Single(ent)) => {
                            // Should never happen but better know if it does
                            diagnostics.add(
                                span.pos(self.ctx),
                                format!(
                                    "Character literal cannot denote non-overloaded symbol {}",
                                    ent.describe(),
                                ),
                                ErrorCode::Internal,
                            );
                            Err(EvalError::Unknown)
                        }
                        Ok(NamedEntities::Overloaded(overloaded)) => {
                            if overloaded.len() == 1 {
                                let ent = overloaded.first();
                                if let Some(return_type) = ent.return_type() {
                                    Ok(ExpressionType::Unambiguous(return_type))
                                } else {
                                    diagnostics.add(
                                        span.pos(self.ctx),
                                        format!(
                                            "Character literal cannot denote procedure symbol {}",
                                            ent.describe(),
                                        ),
                                        ErrorCode::MismatchedKinds,
                                    );
                                    Err(EvalError::Unknown)
                                }
                            } else {
                                Ok(ExpressionType::Ambiguous(
                                    overloaded
                                        .entities()
                                        .flat_map(|e| e.return_type())
                                        .map(BaseType::from)
                                        .collect(),
                                ))
                            }
                        }
                        Err(e) => {
                            diagnostics.push(e.into_diagnostic(self.ctx, span));
                            Err(EvalError::Unknown)
                        }
                    }
                }
                Literal::AbstractLiteral(AbstractLiteral::Integer(_)) => {
                    Ok(ExpressionType::Unambiguous(self.universal_integer().into()))
                }
                Literal::AbstractLiteral(AbstractLiteral::Real(_)) => {
                    Ok(ExpressionType::Unambiguous(self.universal_real().into()))
                }
                Literal::Null => Ok(ExpressionType::Null),
            },
        }
    }

    // Fallback for analyzing an expression without a known target type
    pub fn expr_pos_unknown_ttyp(
        &self,
        scope: &Scope<'a>,
        span: TokenSpan,
        expr: &mut Expression,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        as_fatal(self.expr_pos_type(scope, span, expr, diagnostics))?;
        Ok(())
    }

    fn analyze_qualified_expression(
        &self,
        scope: &Scope<'a>,
        qexpr: &mut QualifiedExpression,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<TypeEnt<'a>> {
        let QualifiedExpression { type_mark, expr } = qexpr;

        match as_fatal(self.type_name(scope, type_mark.span, &mut type_mark.item, diagnostics))? {
            Some(target_type) => {
                self.expr_pos_with_ttyp(
                    scope,
                    target_type,
                    expr.span,
                    &mut expr.item,
                    diagnostics,
                )?;
                Ok(target_type)
            }
            None => {
                self.expr_unknown_ttyp(scope, expr, diagnostics)?;
                Err(EvalError::Unknown)
            }
        }
    }

    pub fn analyze_allocation(
        &self,
        scope: &Scope<'a>,
        alloc: &mut WithTokenSpan<Allocator>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        match &mut alloc.item {
            Allocator::Qualified(ref mut qexpr) => {
                as_fatal(self.analyze_qualified_expression(scope, qexpr, diagnostics))?;
            }
            Allocator::Subtype(ref mut subtype) => {
                self.analyze_subtype_indication(scope, subtype, diagnostics)?;
            }
        }
        Ok(())
    }

    pub fn expr_with_ttyp(
        &self,
        scope: &Scope<'a>,
        target_type: TypeEnt<'a>,
        expr: &mut WithTokenSpan<Expression>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        self.expr_pos_with_ttyp(scope, target_type, expr.span, &mut expr.item, diagnostics)
    }

    fn implicit_bool_types(&self, scope: &Scope<'a>) -> FnvHashSet<BaseType<'a>> {
        if let Ok(NamedEntities::Overloaded(overloaded)) =
            scope.lookup(&Designator::OperatorSymbol(Operator::QueQue))
        {
            overloaded
                .entities()
                .filter_map(|ent| ent.formals().nth(0).map(|typ| typ.type_mark().base()))
                .collect()
        } else {
            FnvHashSet::default()
        }
    }

    /// An expression that is either boolean or implicitly boolean via ?? operator
    pub fn boolean_expr(
        &self,
        scope: &Scope<'a>,
        expr: &mut WithTokenSpan<Expression>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        if let Some(types) = as_fatal(self.expr_type(scope, expr, diagnostics))? {
            match types {
                ExpressionType::Unambiguous(typ) => {
                    if typ.base() != self.boolean().base() {
                        let implicit_bools = self.implicit_bool_types(scope);
                        if !implicit_bools.contains(&typ.base()) {
                            diagnostics.add(
                                expr.pos(self.ctx),
                                format!(
                                    "{} cannot be implicitly converted to {}. Operator ?? is not defined for this type.",
                                    typ.describe(),
                                    self.boolean().describe()
                                ),
                                ErrorCode::NoImplicitConversion,
                            );
                        }
                    }
                }
                ExpressionType::Ambiguous(types) => {
                    if types.contains(&self.boolean().base()) {
                        self.expr_with_ttyp(scope, self.boolean(), expr, diagnostics)?;
                    } else {
                        let implicit_bool_types: FnvHashSet<_> = self
                            .implicit_bool_types(scope)
                            .intersection(&types)
                            .cloned()
                            .collect();

                        match implicit_bool_types.len().cmp(&1) {
                            std::cmp::Ordering::Equal => {
                                let typ: TypeEnt<'_> = types.into_iter().next().unwrap().into();
                                self.expr_with_ttyp(scope, typ, expr, diagnostics)?;
                            }
                            std::cmp::Ordering::Greater => {
                                let mut diag = Diagnostic::new(
                                    expr.pos(self.ctx),
                                    "Ambiguous use of implicit boolean conversion ??",
                                    ErrorCode::AmbiguousCall,
                                );
                                diag.add_type_candidates("Could be", implicit_bool_types);
                                diagnostics.push(diag);
                            }

                            std::cmp::Ordering::Less => {
                                let mut diag = Diagnostic::new(
                                    expr.pos(self.ctx),
                                    format!(
                                        "Cannot disambiguate expression to {}",
                                        self.boolean().describe()
                                    ),
                                    ErrorCode::AmbiguousExpression,
                                );
                                diag.add_type_candidates(
                                    "Implicit boolean conversion operator ?? is not defined for",
                                    types,
                                );
                                diagnostics.push(diag);
                            }
                        }
                    }
                }
                ExpressionType::String | ExpressionType::Null | ExpressionType::Aggregate => {
                    self.expr_with_ttyp(scope, self.boolean(), expr, diagnostics)?;
                }
            }
        }

        Ok(())
    }

    /// Returns true if the name actually matches the target type
    /// None if it was uncertain
    pub fn expr_pos_with_ttyp(
        &self,
        scope: &Scope<'a>,
        target_type: TypeEnt<'a>,
        span: TokenSpan,
        expr: &mut Expression,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        let target_base = target_type.base_type();
        match expr {
            Expression::Literal(ref mut lit) => {
                self.analyze_literal_with_target_type(scope, target_type, span, lit, diagnostics)?
            }
            Expression::Name(ref mut name) => self.expression_name_with_ttyp(
                scope,
                span,
                name.as_mut(),
                target_type,
                diagnostics,
            )?,
            Expression::Qualified(ref mut qexpr) => {
                if let Some(type_mark) =
                    as_fatal(self.analyze_qualified_expression(scope, qexpr, diagnostics))?
                {
                    self.check_type_mismatch(type_mark, target_type, span, diagnostics);
                }
            }
            Expression::Binary(ref mut op, ref mut left, ref mut right) => {
                let op_candidates = match as_fatal(self.lookup_operator(
                    diagnostics,
                    scope,
                    op.token,
                    op.item.item,
                    2,
                ))? {
                    Some(candidates) => candidates,
                    None => return Ok(()),
                };

                match as_fatal(self.disambiguate_op(
                    scope,
                    Some(target_type),
                    op,
                    op_candidates,
                    &mut [left.as_mut(), right.as_mut()],
                    diagnostics,
                ))? {
                    Some(Disambiguated::Unambiguous(overloaded)) => {
                        let op_type = overloaded.return_type().unwrap();
                        self.check_type_mismatch(op_type, target_type, span, diagnostics);
                    }
                    Some(Disambiguated::Ambiguous(candidates)) => {
                        diagnostics.push(Diagnostic::ambiguous_op(
                            op.pos(self.ctx),
                            op.item.item,
                            candidates,
                        ));
                    }
                    None => {}
                }
            }
            Expression::Unary(ref mut op, ref mut expr) => {
                let op_candidates = match as_fatal(self.lookup_operator(
                    diagnostics,
                    scope,
                    op.token,
                    op.item.item,
                    1,
                ))? {
                    Some(candidates) => candidates,
                    None => {
                        return Ok(());
                    }
                };

                match as_fatal(self.disambiguate_op(
                    scope,
                    Some(target_type),
                    op,
                    op_candidates,
                    &mut [expr.as_mut()],
                    diagnostics,
                ))? {
                    Some(Disambiguated::Unambiguous(overloaded)) => {
                        let op_type = overloaded.return_type().unwrap();
                        self.check_type_mismatch(op_type, target_type, span, diagnostics);
                    }
                    Some(Disambiguated::Ambiguous(candidates)) => {
                        diagnostics.push(Diagnostic::ambiguous_op(
                            op.pos(self.ctx),
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
                    for assoc in assocs.iter_mut() {
                        as_fatal(self.array_assoc_elem(
                            scope,
                            target_base,
                            indexes,
                            *elem_type,
                            &mut assoc.item,
                            diagnostics,
                        ))?;
                    }
                }
                Type::Record(record_scope) => {
                    self.analyze_record_aggregate(
                        scope,
                        target_base,
                        record_scope,
                        span,
                        assocs,
                        diagnostics,
                    )?;
                }
                _ => {
                    self.analyze_aggregate(scope, assocs, diagnostics)?;

                    diagnostics.add(
                        span.pos(self.ctx),
                        format!("composite does not match {}", target_type.describe()),
                        ErrorCode::TypeMismatch,
                    );
                }
            },
            Expression::New(ref mut alloc) => {
                self.analyze_allocation(scope, alloc, diagnostics)?;
            }
            Expression::Parenthesized(ref mut expr) => {
                self.expr_pos_with_ttyp(
                    scope,
                    target_type,
                    expr.span,
                    &mut expr.item,
                    diagnostics,
                )?;
            }
        }

        Ok(())
    }

    pub fn analyze_aggregate(
        &self,
        scope: &Scope<'a>,
        assocs: &mut [WithTokenSpan<ElementAssociation>],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        for assoc in assocs.iter_mut() {
            match &mut assoc.item {
                ElementAssociation::Named(ref mut choices, ref mut expr) => {
                    for choice in choices.iter_mut() {
                        match choice.item {
                            Choice::Expression(..) => {
                                // @TODO could be record element so we cannot do more now
                            }
                            Choice::DiscreteRange(ref mut drange) => {
                                self.drange_unknown_type(scope, drange, diagnostics)?;
                            }
                            Choice::Others => {}
                        }
                    }
                    self.expr_unknown_ttyp(scope, expr, diagnostics)?;
                }
                ElementAssociation::Positional(ref mut expr) => {
                    self.expr_unknown_ttyp(scope, expr, diagnostics)?;
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
        span: TokenSpan,
        assocs: &mut [WithTokenSpan<ElementAssociation>],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        let mut associated = RecordAssociations::default();
        let mut is_ok_so_far = true;

        for (idx, assoc) in assocs.iter_mut().enumerate() {
            match &mut assoc.item {
                ElementAssociation::Named(ref mut choices, ref mut actual_expr) => {
                    let typ = if choices.len() == 1 {
                        let choice = choices.first_mut().unwrap();
                        let choice_span = choice.span;
                        match &mut choice.item {
                            Choice::Expression(choice_expr) => {
                                if let Some(simple_name) =
                                    as_name_mut(choice_expr).and_then(as_simple_name_mut)
                                {
                                    if let Some(elem) = elems.lookup(&simple_name.item) {
                                        simple_name.set_unique_reference(&elem);
                                        associated.associate(
                                            self.ctx,
                                            &elem,
                                            choice.span,
                                            diagnostics,
                                        );
                                        Some(elem.type_mark().base())
                                    } else {
                                        is_ok_so_far = false;
                                        diagnostics.push(Diagnostic::no_declaration_within(
                                            &record_type,
                                            &choice_span.pos(self.ctx),
                                            &simple_name.item,
                                        ));
                                        None
                                    }
                                } else {
                                    is_ok_so_far = false;
                                    diagnostics.add(
                                        choice.pos(self.ctx),
                                        "Record aggregate choice must be a simple name",
                                        ErrorCode::MismatchedKinds,
                                    );
                                    None
                                }
                            }
                            Choice::DiscreteRange(_) => {
                                is_ok_so_far = false;
                                diagnostics.add(
                                    choice.pos(self.ctx),
                                    "Record aggregate choice must be a simple name",
                                    ErrorCode::MismatchedKinds,
                                );
                                None
                            }
                            Choice::Others => {
                                // @TODO empty others
                                let remaining_types: FnvHashSet<BaseType<'_>> = elems
                                    .iter()
                                    .filter_map(|elem| {
                                        if !associated.is_associated(&elem) {
                                            Some(elem.type_mark().base())
                                        } else {
                                            None
                                        }
                                    })
                                    .collect();

                                if remaining_types.len() > 1 {
                                    let mut diag = Diagnostic::new(choice.pos(self.ctx), format!("Other elements of record '{}' are not of the same type", record_type.designator()), ErrorCode::TypeMismatch);
                                    for elem in elems.iter() {
                                        if !associated.is_associated(&elem) {
                                            if let Some(decl_pos) = elem.decl_pos() {
                                                diag.add_related(
                                                    decl_pos,
                                                    format!(
                                                        "Element '{}' has {}",
                                                        elem.designator(),
                                                        elem.type_mark().describe()
                                                    ),
                                                );
                                            }
                                        }
                                    }
                                    diagnostics.push(diag);
                                } else if remaining_types.is_empty() {
                                    diagnostics.push(
                                        Diagnostic::new(
                                            choice.pos(self.ctx),
                                            format!(
                                                "All elements of record '{}' are already associated",
                                                record_type.designator()
                                            ),
                                            ErrorCode::AlreadyAssociated,
                                        )
                                            .opt_related(
                                                record_type.decl_pos(),
                                                format!(
                                                    "Record '{}' defined here",
                                                    record_type.designator()
                                                ),
                                            ),
                                    )
                                }

                                for elem in elems.iter() {
                                    if !associated.is_associated(&elem) {
                                        associated.associate(
                                            self.ctx,
                                            &elem,
                                            choice.span,
                                            diagnostics,
                                        );
                                    }
                                }

                                if remaining_types.len() == 1 {
                                    remaining_types.into_iter().next()
                                } else {
                                    None
                                }
                            }
                        }
                    } else {
                        if let (Some(first), Some(last)) = (choices.first(), choices.last()) {
                            is_ok_so_far = false;
                            let pos = first.span.combine(last.span);
                            diagnostics.add(
                                pos.pos(self.ctx),
                                "Record aggregate choice must be a simple name",
                                ErrorCode::MismatchedKinds,
                            );
                        }
                        None
                    };

                    if let Some(typ) = typ {
                        self.expr_pos_with_ttyp(
                            scope,
                            typ.into(),
                            actual_expr.span,
                            &mut actual_expr.item,
                            diagnostics,
                        )?;
                    } else {
                        self.expr_unknown_ttyp(scope, actual_expr, diagnostics)?;
                    }
                }
                ElementAssociation::Positional(ref mut expr) => {
                    if let Some(elem) = elems.nth(idx) {
                        self.expr_with_ttyp(scope, elem.type_mark(), expr, diagnostics)?;
                        associated.associate(self.ctx, elem, expr.span, diagnostics);
                    } else {
                        self.expr_unknown_ttyp(scope, expr, diagnostics)?;

                        diagnostics.push(
                            Diagnostic::new(
                                expr.pos(self.ctx),
                                format!(
                                    "Unexpected positional association for record '{}'",
                                    record_type.designator()
                                ),
                                ErrorCode::TooManyArguments,
                            )
                            .opt_related(
                                record_type.decl_pos(),
                                format!("Record '{}' defined here", record_type.designator()),
                            ),
                        )
                    }
                }
            }
        }

        if is_ok_so_far {
            // Do not complain about these when there are worse problems
            for elem in elems.iter() {
                if !associated.is_associated(&elem) {
                    diagnostics.push(
                        Diagnostic::new(
                            span.pos(self.ctx),
                            format!(
                                "Missing association of record element '{}'",
                                elem.designator()
                            ),
                            ErrorCode::Unassociated,
                        )
                        .opt_related(
                            elem.decl_pos(),
                            format!("Record element '{}' defined here", elem.designator()),
                        ),
                    )
                }
            }
        }

        Ok(())
    }

    pub fn array_assoc_elem(
        &self,
        scope: &Scope<'a>,
        array_type: TypeEnt<'a>,
        index_types: &[Option<BaseType<'a>>],
        elem_type: TypeEnt<'a>,
        assoc: &mut ElementAssociation,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult {
        let index_type = index_types.first().and_then(|x| *x);
        let mut can_be_array = true;

        let expr = match assoc {
            ElementAssociation::Named(ref mut choices, ref mut expr) => {
                for choice in choices.iter_mut() {
                    match &mut choice.item {
                        Choice::Expression(index_expr) => {
                            match self.expr_as_discrete_range_type(
                                scope,
                                choice.span,
                                index_expr,
                                diagnostics,
                            )? {
                                Some(typ) => {
                                    if let Some(index_type) = index_type {
                                        if !self.can_be_target_type(typ, index_type) {
                                            diagnostics.push(Diagnostic::type_mismatch(
                                                &choice.pos(self.ctx),
                                                &typ.describe(),
                                                index_type.into(),
                                            ));
                                        }
                                    }

                                    can_be_array = true;
                                }
                                None => {
                                    if let Some(index_type) = index_type {
                                        self.expr_pos_with_ttyp(
                                            scope,
                                            index_type.into(),
                                            choice.span,
                                            index_expr,
                                            diagnostics,
                                        )?;
                                    }
                                    can_be_array = false;
                                }
                            }
                        }
                        Choice::DiscreteRange(ref mut drange) => {
                            if let Some(index_type) = index_type {
                                self.drange_with_ttyp(
                                    scope,
                                    index_type.into(),
                                    drange,
                                    diagnostics,
                                )?;
                            } else {
                                self.drange_unknown_type(scope, drange, diagnostics)?;
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

        if index_types.len() > 1 {
            if let Expression::Aggregate(ref mut inner) = expr.item {
                for assoc in inner.iter_mut() {
                    as_fatal(self.array_assoc_elem(
                        scope,
                        array_type,
                        &index_types[1..],
                        elem_type,
                        &mut assoc.item,
                        diagnostics,
                    ))?;
                }
            } else {
                diagnostics.add(
                    expr.pos(self.ctx),
                    format!(
                        "Expected sub-aggregate for target {}",
                        array_type.describe()
                    ),
                    ErrorCode::ExpectedSubAggregate,
                );
            }
        } else if can_be_array {
            // If the choice is only a range or positional the expression can be an array
            let types = self.expr_type(scope, expr, diagnostics)?;
            let is_array = self.is_possible(&types, array_type.base());
            let is_elem = self.is_possible(&types, elem_type.base());

            if is_elem || !is_array {
                // Prefer element type in presence of ambiguity
                self.expr_pos_with_ttyp(scope, elem_type, expr.span, &mut expr.item, diagnostics)?;
            } else if is_array {
                self.expr_pos_with_ttyp(scope, array_type, expr.span, &mut expr.item, diagnostics)?;
            }
        } else {
            self.expr_pos_with_ttyp(scope, elem_type, expr.span, &mut expr.item, diagnostics)?;
        }

        Ok(())
    }
}

impl Diagnostic {
    fn ambiguous_op<'a>(
        pos: &SrcPos,
        op: Operator,
        candidates: impl IntoIterator<Item = OverloadedEnt<'a>>,
    ) -> Diagnostic {
        let mut diag = Diagnostic::new(
            pos,
            format!(
                "ambiguous use of {}",
                Designator::OperatorSymbol(op).describe()
            ),
            ErrorCode::AmbiguousCall,
        );
        diag.add_subprogram_candidates("might be", candidates);
        diag
    }
}

#[derive(Default)]
struct RecordAssociations(FnvHashMap<EntityId, TokenSpan>);

impl RecordAssociations {
    fn associate(
        &mut self,
        ctx: &dyn TokenAccess,
        elem: &RecordElement<'_>,
        pos: TokenSpan,
        diagnostics: &mut dyn DiagnosticHandler,
    ) {
        if let Some(prev_pos) = self.0.insert(elem.id(), pos) {
            diagnostics.push(
                Diagnostic::new(
                    pos.pos(ctx),
                    format!(
                        "Record element '{}' has already been associated",
                        elem.designator()
                    ),
                    ErrorCode::AlreadyAssociated,
                )
                .related(prev_pos.pos(ctx), "Previously associated here"),
            );
        }
    }

    fn is_associated(&self, elem: &RecordElement<'_>) -> bool {
        self.0.contains_key(&elem.id())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::analysis::tests::TestSetup;
    use crate::data::DiagnosticHandler;
    use crate::syntax::test::check_diagnostics;
    use crate::syntax::test::without_related;
    use crate::syntax::test::Code;

    impl<'a> TestSetup<'a> {
        fn expr_type(
            &'a self,
            code: &Code,
            diagnostics: &mut dyn DiagnosticHandler,
        ) -> Option<ExpressionType<'a>> {
            let mut expr = code.expr();
            as_fatal(
                self.ctx(&code.tokenize())
                    .expr_type(&self.scope, &mut expr, diagnostics),
            )
            .unwrap()
        }

        fn expr_with_ttyp(
            &'a self,
            code: &Code,
            ttyp: TypeEnt<'a>,
            diagnostics: &mut dyn DiagnosticHandler,
        ) {
            let mut expr = code.expr();
            self.ctx(&code.tokenize())
                .expr_pos_with_ttyp(&self.scope, ttyp, expr.span, &mut expr.item, diagnostics)
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
                test.ctx(&Vec::new()).universal_integer().into()
            ))
        );
    }

    #[test]
    fn universal_real_expr_type() {
        let test = TestSetup::new();
        assert_eq!(
            test.expr_type(&test.snippet("0.0"), &mut NoDiagnostics),
            Some(ExpressionType::Unambiguous(
                test.ctx(&Vec::new()).universal_real().into()
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
            without_related(&diagnostics),
            vec![Diagnostic::new(
                code.s1("and"),
                "Found no match for operator \"and\"",
                ErrorCode::Unresolved,
            )],
        );
    }

    #[test]
    fn type_attributes_cannot_be_used_as_an_expression() {
        let test = TestSetup::new();
        test.declarative_part("variable x : integer;");
        let code = test.snippet("x'subtype");

        let mut diagnostics = Vec::new();
        assert_eq!(test.expr_type(&code, &mut diagnostics), None);

        check_diagnostics(
            diagnostics,
            vec![Diagnostic::new(
                code.s1("x'subtype"),
                "integer type 'INTEGER' cannot be used in an expression",
                ErrorCode::MismatchedKinds,
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
            without_related(&diagnostics),
            vec![Diagnostic::new(
                code.s1("missing"),
                "No declaration of 'missing'",
                ErrorCode::Unresolved,
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
            vec![Diagnostic::new(
                code.s1("-"),
                "ambiguous use of operator \"-\"",
                ErrorCode::AmbiguousCall,
            )
            .related(
                decls.s("\"-\"", 1),
                "might be operator \"-\"[BIT_VECTOR return INTEGER]",
            )
            .related(
                decls.s("\"-\"", 2),
                "might be operator \"-\"[STRING return INTEGER]",
            )],
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

    #[test]
    fn does_not_remove_universal_candidates_when_return_types_differ() {
        let test = TestSetup::new();
        test.declarative_part(
            "
function \"+\"(a : integer; b : character) return character;
function \"+\"(a : integer; b : character) return integer;
        ",
        );

        let code = test.snippet("0 + 'c'");
        assert_eq!(
            test.expr_type(&code, &mut NoDiagnostics),
            Some(ExpressionType::Ambiguous(
                vec![
                    test.lookup_type("integer").base(),
                    test.lookup_type("character").base()
                ]
                .into_iter()
                .collect()
            ))
        );
    }

    #[test]
    fn universal_expression_is_not_ambiguous() {
        let test = TestSetup::new();
        let code = test.snippet("-1");
        assert_eq!(
            test.expr_type(&code, &mut NoDiagnostics),
            Some(ExpressionType::Unambiguous(
                test.ctx(&code.tokenize()).universal_integer().into()
            ))
        );
    }

    #[test]
    fn universal_integer_target_type_accepts_integer() {
        let test = TestSetup::new();
        test.declarative_part(
            "
function no_arg return boolean;
function no_arg return integer;
function with_arg(arg : natural) return boolean;
function with_arg(arg : natural) return integer;

        ",
        );

        let code = test.snippet("no_arg");
        test.expr_with_ttyp(
            &code,
            test.ctx(&code.tokenize()).universal_integer().into(),
            &mut NoDiagnostics,
        );
        let code = test.snippet("with_arg(0)");
        test.expr_with_ttyp(
            &code,
            test.ctx(&code.tokenize()).universal_integer().into(),
            &mut NoDiagnostics,
        );
    }
}
