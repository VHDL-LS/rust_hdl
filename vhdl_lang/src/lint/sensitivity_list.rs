// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2024, Lukas Scheller lukasscheller@icloud.com
//! lints to check for issues in the sensitivity lists of process statements.
//! The checks include the following:
//! - Checking for superfluous signals in the sensitivity lists of combinational processes
//! - Checking for missing signals in the sensitivity lists of combinational processes
//!
//! The [SensitivityListLinter] struct is the main entry point to the linting procedure
//! and is used to cache already checked processes. This avoids checking every process on every
//! analysis step.
//!
//! Whether a process is considered combinational is based on simple heuristics that are roughly
//! equivalent to the following:
//! - If the process contains a single `if` statement containing a `rising_edge(clk)` or `clk'event`
//!   signal, the process is considered sequential
//! - The same is true, if the aforementioned condition applies to an `elsif` branch
//! - otherwise, the process is considered combinational.

use crate::analysis::{DesignRoot, LockedUnit};
use crate::ast::search::{
    DeclarationItem, FoundDeclaration, Search, SearchResult, SearchState, Searcher,
};
use crate::ast::token_range::WithTokenSpan;
use crate::ast::{
    ActualPart, Allocator, AssignmentRightHand, AttributeDesignator, AttributeName,
    ConcurrentStatement, Conditionals, Designator, DiscreteRange, ElementAssociation, Expression,
    HasUnitId, IterationScheme, LabeledConcurrentStatement, Name, ProcessStatement, Range,
    SensitivityList, SequentialStatement, SignalAttribute, UnitId, UnitKey, Waveform, WithRef,
};
use crate::data::{DiagnosticHandler, ErrorCode, Symbol};
use crate::{Config, Diagnostic, EntityId, HasTokenSpan, SrcPos, TokenAccess, TokenSpan};
use fnv::FnvHashMap;
use itertools::Itertools;
use std::collections::hash_map::Entry;

/// Linter that checks the contents of sensitivity lists to verify that they match
/// the contents of the process statement.
///
/// This is a struct, as opposed to a function, so that units that were already analyzed don't need
/// re-analysis.
#[derive(Default)]
pub(crate) struct SensitivityListLinter {
    // library name, secondary key
    diagnostics: FnvHashMap<(Symbol, UnitKey), Vec<Diagnostic>>,
}

impl SensitivityListLinter {
    /// Lint all units referenced by `analyzed_units`
    pub fn lint(
        &mut self,
        root: &DesignRoot,
        config: &Config,
        analyzed_units: &[UnitId],
        diagnostics: &mut dyn DiagnosticHandler,
    ) {
        // Prune diagnostics that need to be re-computed
        for unit in analyzed_units {
            let key = (unit.library_name().clone(), unit.key().clone());
            self.diagnostics.remove(&key);
        }

        // Prune diagnostics for units that no longer exist
        self.diagnostics.retain(|(library_name, unit_key), _| {
            if let Some(library) = root.get_lib(library_name) {
                if library.get_unit(unit_key).is_some() {
                    return true;
                }
            }
            false
        });

        for unit in analyzed_units {
            if let Some(library) = root.get_lib(unit.library_name()) {
                match self
                    .diagnostics
                    .entry((unit.library_name().clone(), unit.key().clone()))
                {
                    Entry::Occupied(_) => {}
                    Entry::Vacant(vacant_entry) => {
                        if let Some(unit) = library.get_unit(&vacant_entry.key().1) {
                            vacant_entry.insert(analyze_unit(root, unit));
                        }
                    }
                }
            }
        }

        for ((library_name, _), unit_diagnostics) in self.diagnostics.iter() {
            if let Some(library_config) = config.get_library(&library_name.name_utf8()) {
                if !library_config.is_third_party {
                    diagnostics.append(unit_diagnostics.iter().cloned());
                }
            }
        }
    }
}

/// Analyze a single unit for issues in the sensitivity list.
fn analyze_unit(root: &DesignRoot, unit: &LockedUnit) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();
    let mut searcher = ProcessSearcher::new(|process, ctx| {
        diagnostics.append(&mut lint_sensitivity_list(root, ctx, process))
    });
    let _ = unit
        .unit
        .expect_analyzed()
        .search(&unit.tokens, &mut searcher);
    diagnostics
}

/// Analyzes a process statement to check whether the sensitivity list contains any
/// superfluous signals or any signals that are read in the process, but are not present in the
/// sensitivity list.
fn lint_sensitivity_list(
    root: &DesignRoot,
    ctx: &dyn TokenAccess,
    process: &ProcessStatement,
) -> Vec<Diagnostic> {
    // Process without sensitivity list => don't lint
    let Some(sensitivity_list) = &process.sensitivity_list else {
        return vec![];
    };
    // Process with all as sensitivity list => don't link
    let SensitivityList::Names(names) = &sensitivity_list.item else {
        return vec![];
    };
    // Sequential processes => don't lint
    // here, one could also add a lint to check for the clock and possibly reset signal.
    if get_likely_process_category(root, process) == ProcessCategory::Sequential {
        return vec![];
    }

    let signals_in_sensitivity_list: FnvHashMap<_, _> = names
        .iter()
        .flat_map(|name| {
            name.item
                .get_suffix_reference_disregard_index()
                .map(|reference| (reference, name.span))
        })
        .collect();
    let mut searcher = SensitivityListChecker {
        root,
        sensitivity_list: signals_in_sensitivity_list.clone(),
        superfluous_entities: signals_in_sensitivity_list,
        found_entities: FnvHashMap::default(),
    };
    let _ = process.statements.search(ctx, &mut searcher);

    let mut missing_signals = searcher.found_entities.into_iter().collect_vec();
    let mut diagnostics = Vec::new();
    if !missing_signals.is_empty() {
        missing_signals.sort_by(|(_, pos1), (_, pos2)| pos1.cmp(pos2));
        let diagnostic_pos = process.span.start_token.get_pos(ctx);
        let mut diag = Diagnostic::new(
            diagnostic_pos,
            format!(
                "{} {} {} not read in the sensitivity list",
                pluralize(missing_signals.len(), "The signal", "Signals"),
                missing_signals
                    .iter()
                    .map(|(sig, _)| format!("'{}'", root.get_ent(*sig).designator))
                    .join(", "),
                pluralize(missing_signals.len(), "is", "are"),
            ),
            ErrorCode::MissingInSensitivityList,
        );
        for (signal, pos) in missing_signals {
            diag.add_related(
                pos,
                format!(
                    "signal '{}' first read here",
                    root.get_ent(signal).designator
                ),
            )
        }
        diagnostics.push(diag);
    }

    for (_, pos) in searcher.superfluous_entities {
        diagnostics.push(Diagnostic::new(
            pos.get_pos(ctx),
            "Signal is never read in the process",
            ErrorCode::SuperfluousInSensitivityList,
        ));
    }
    diagnostics
}

/// The ProcessSearcher searches for processes and calls the provided callback
/// for each process found.
struct ProcessSearcher<S>
where
    S: FnMut(&ProcessStatement, &dyn TokenAccess),
{
    callback: S,
}

impl<S> ProcessSearcher<S>
where
    S: FnMut(&ProcessStatement, &dyn TokenAccess),
{
    /// Constructs a new searcher with the provided callback
    fn new(func: S) -> ProcessSearcher<S> {
        ProcessSearcher { callback: func }
    }
}

impl<S> Searcher for ProcessSearcher<S>
where
    S: FnMut(&ProcessStatement, &dyn TokenAccess),
{
    fn search_decl(&mut self, ctx: &dyn TokenAccess, decl: FoundDeclaration<'_>) -> SearchState {
        // prevent searching package bodies
        if matches!(decl.ast, DeclarationItem::PackageBody(_)) {
            return SearchState::Finished(SearchResult::NotFound);
        }
        if let DeclarationItem::ConcurrentStatement(LabeledConcurrentStatement {
            label: _,
            statement:
                WithTokenSpan {
                    span: _,
                    item: ConcurrentStatement::Process(process),
                },
        }) = &decl.ast
        {
            (self.callback)(process, ctx)
        }
        SearchState::NotFinished
    }
}

fn pluralize<'a>(len: usize, singular: &'a str, plural: &'a str) -> &'a str {
    if len > 1 {
        plural
    } else {
        singular
    }
}

struct SensitivityListChecker<'a> {
    root: &'a DesignRoot,
    /// The sensitivity list present at the generate statement
    sensitivity_list: FnvHashMap<EntityId, TokenSpan>,
    /// Additional entities that are in the Sensitivity list, but are never read in the process.
    superfluous_entities: FnvHashMap<EntityId, TokenSpan>,
    /// A set of named entities that form the sensitivity list.
    /// Additionally, the token span of the first occurrence is also provided.
    found_entities: FnvHashMap<EntityId, SrcPos>,
}

impl SensitivityListChecker<'_> {
    fn analyze_expression(&mut self, expr: &Expression, span: TokenSpan, ctx: &dyn TokenAccess) {
        match expr {
            Expression::Binary(_, lhs, rhs) => {
                self.analyze_expression(&lhs.item, lhs.span, ctx);
                self.analyze_expression(&rhs.item, rhs.span, ctx);
            }
            Expression::Unary(_, expr) => {
                self.analyze_expression(&expr.item, expr.span, ctx);
            }
            Expression::Aggregate(associations) => {
                for assoc in associations {
                    match &assoc.item {
                        ElementAssociation::Positional(expr) => {
                            self.analyze_expression(&expr.item, expr.span, ctx)
                        }
                        ElementAssociation::Named(_, expr) => {
                            self.analyze_expression(&expr.item, expr.span, ctx)
                        }
                    }
                }
            }
            Expression::Qualified(qual_expr) => {
                self.analyze_expression(&qual_expr.expr.item, qual_expr.expr.span, ctx)
            }
            Expression::Name(name) => self.analyze_name(name, span, ctx, false),
            Expression::New(allocator) => match &allocator.item {
                Allocator::Qualified(qual_expr) => {
                    self.analyze_expression(&qual_expr.expr.item, qual_expr.expr.span, ctx)
                }
                Allocator::Subtype(_) => {}
            },
            Expression::Parenthesized(expr) => self.analyze_expression(&expr.item, expr.span, ctx),
            Expression::Literal(_) => {}
        }
    }

    fn analyze_name(
        &mut self,
        name: &Name,
        pos: TokenSpan,
        ctx: &dyn TokenAccess,
        remove_only: bool,
    ) {
        use Name::*;
        match name {
            Designator(designator) => {
                self.analyze_designator(designator, pos, ctx, remove_only);
            }
            Selected(prefix, suffix) => {
                self.analyze_name(&prefix.item, prefix.span, ctx, true);
                self.analyze_designator(&suffix.item, pos, ctx, remove_only);
            }
            SelectedAll(prefix) => self.analyze_name(&prefix.item, prefix.span, ctx, remove_only),
            Slice(prefix, _) => self.analyze_name(&prefix.item, prefix.span, ctx, remove_only),
            Attribute(attr) => self.analyze_attribute_name(attr, ctx),
            CallOrIndexed(coi) => {
                self.analyze_name(&coi.name.item, coi.name.span, ctx, remove_only);
                for item in &coi.parameters.items {
                    match &item.actual.item {
                        ActualPart::Expression(expr) => {
                            self.analyze_expression(expr, item.actual.span, ctx)
                        }
                        ActualPart::Open => {}
                    }
                }
            }
            External(_) => {
                // TODO: External names aren't analyzed atm
            }
        }
    }

    fn analyze_designator(
        &mut self,
        designator: &WithRef<Designator>,
        pos: TokenSpan,
        ctx: &dyn TokenAccess,
        remove_only: bool,
    ) {
        if let Some(reference) = designator.reference.get() {
            self.superfluous_entities.remove(&reference);
            let ent = self.root.get_ent(reference);
            if ent.is_signal() && !self.sensitivity_list.contains_key(&reference) && !remove_only {
                self.found_entities
                    .entry(reference)
                    .or_insert_with(|| pos.get_pos(ctx));
            }
        }
    }

    fn analyze_attribute_name(&mut self, attr: &AttributeName, ctx: &dyn TokenAccess) {
        use AttributeDesignator::*;
        match attr.attr.item {
            Ident(_) | Image => {}
            // These likely do not belong inside a sensitivity list.
            // However, true analysis can only be performed when it is known whether the name is
            // static or not.
            _ => return,
        }
        self.analyze_name(&attr.name.item, attr.name.span, ctx, false);
        if let Some(expr) = &attr.expr {
            self.analyze_expression(&expr.item, expr.span, ctx)
        }
    }

    fn analyze_waveform(&mut self, waveform: &Waveform, ctx: &dyn TokenAccess) {
        match waveform {
            Waveform::Elements(elements) => {
                for element in elements {
                    self.analyze_expression(&element.value.item, element.value.span, ctx);
                }
            }
            Waveform::Unaffected(_) => {}
        }
    }

    fn analyze_signal_rhs(&mut self, rhs: &AssignmentRightHand<Waveform>, ctx: &dyn TokenAccess) {
        match rhs {
            AssignmentRightHand::Simple(waveform) => self.analyze_waveform(waveform, ctx),
            AssignmentRightHand::Conditional(conditionals) => {
                for conditional in &conditionals.conditionals {
                    self.analyze_waveform(&conditional.item, ctx);
                    self.analyze_expression(
                        &conditional.condition.item,
                        conditional.condition.span,
                        ctx,
                    );
                }
                if let Some((else_item, _)) = &conditionals.else_item {
                    self.analyze_waveform(else_item, ctx);
                }
            }
            AssignmentRightHand::Selected(selection) => {
                self.analyze_expression(&selection.expression.item, selection.expression.span, ctx);
                for alternative in &selection.alternatives {
                    self.analyze_waveform(&alternative.item, ctx)
                }
            }
        }
    }

    fn analyze_conditionals(
        &mut self,
        conditionals: &Conditionals<WithTokenSpan<Expression>>,
        ctx: &dyn TokenAccess,
    ) {
        for conditional in &conditionals.conditionals {
            self.analyze_expression(&conditional.item.item, conditional.item.span, ctx);
            self.analyze_expression(&conditional.condition.item, conditional.condition.span, ctx);
        }
        if let Some((else_item, _)) = &conditionals.else_item {
            self.analyze_expression(&else_item.item, else_item.span, ctx);
        }
    }

    fn analyze_discrete_range(&mut self, range: &DiscreteRange, ctx: &dyn TokenAccess) {
        match range {
            DiscreteRange::Discrete(name, range) => {
                self.analyze_name(&name.item, name.span, ctx, false);
                if let Some(range) = range {
                    self.analyze_range(range, ctx)
                }
            }
            DiscreteRange::Range(range) => self.analyze_range(range, ctx),
        }
    }

    fn analyze_range(&mut self, range: &Range, ctx: &dyn TokenAccess) {
        match range {
            Range::Range(range_constraint) => {
                self.analyze_expression(
                    &range_constraint.left_expr.item,
                    range_constraint.left_expr.span,
                    ctx,
                );
                self.analyze_expression(
                    &range_constraint.right_expr.item,
                    range_constraint.right_expr.span,
                    ctx,
                );
            }
            Range::Attribute(attribute) => self.analyze_attribute_name(attribute, ctx),
        }
    }

    fn analyze_variable_rhs(
        &mut self,
        rhs: &AssignmentRightHand<WithTokenSpan<Expression>>,
        ctx: &dyn TokenAccess,
    ) {
        match rhs {
            AssignmentRightHand::Simple(waveform) => {
                self.analyze_expression(&waveform.item, waveform.span, ctx)
            }
            AssignmentRightHand::Conditional(conditionals) => {
                self.analyze_conditionals(conditionals, ctx)
            }
            AssignmentRightHand::Selected(selection) => {
                self.analyze_expression(&selection.expression.item, selection.expression.span, ctx);
                for alternative in &selection.alternatives {
                    self.analyze_expression(&alternative.item.item, alternative.item.span, ctx)
                }
            }
        }
    }
}

impl Searcher for SensitivityListChecker<'_> {
    fn search_decl(&mut self, ctx: &dyn TokenAccess, decl: FoundDeclaration<'_>) -> SearchState {
        use SequentialStatement::*;
        if let DeclarationItem::SequentialStatement(stmt) = &decl.ast {
            match &stmt.statement.item {
                Assert(stmt) => {
                    self.analyze_expression(&stmt.condition.item, stmt.condition.span, ctx)
                }
                Report(stmt) => self.analyze_expression(&stmt.report.item, stmt.report.span, ctx),
                Next(stmt) => {
                    if let Some(expr) = &stmt.condition {
                        self.analyze_expression(&expr.item, expr.span, ctx)
                    }
                }
                Exit(stmt) => {
                    if let Some(expr) = &stmt.condition {
                        self.analyze_expression(&expr.item, expr.span, ctx)
                    }
                }
                Return(stmt) => {
                    if let Some(expr) = &stmt.expression {
                        self.analyze_expression(&expr.item, expr.span, ctx)
                    }
                }
                VariableAssignment(stmt) => self.analyze_variable_rhs(&stmt.rhs, ctx),
                SignalAssignment(stmt) => self.analyze_signal_rhs(&stmt.rhs, ctx),
                SignalForceAssignment(stmt) => self.analyze_variable_rhs(&stmt.rhs, ctx),
                If(if_stmt) => {
                    // Conditional statements are automatically visited
                    for conditional in &if_stmt.conds.conditionals {
                        self.analyze_expression(
                            &conditional.condition.item,
                            conditional.condition.span,
                            ctx,
                        );
                    }
                }
                Case(case_stmt) => {
                    self.analyze_expression(
                        &case_stmt.expression.item,
                        case_stmt.expression.span,
                        ctx,
                    );
                }
                Loop(loop_stmt) => match &loop_stmt.iteration_scheme {
                    Some(IterationScheme::For(_, range)) => self.analyze_discrete_range(range, ctx),
                    Some(IterationScheme::While(expr)) => {
                        self.analyze_expression(&expr.item, expr.span, ctx)
                    }
                    None => {}
                },
                ProcedureCall(call_or_indexed) => {
                    for item in &call_or_indexed.item.parameters.items {
                        match &item.actual.item {
                            ActualPart::Expression(expr) => {
                                self.analyze_expression(expr, call_or_indexed.span, ctx)
                            }
                            ActualPart::Open => {}
                        }
                    }
                }
                Wait(_) | Null | SignalReleaseAssignment(_) => {}
            }
        }
        SearchState::NotFinished
    }
}

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
enum ProcessCategory {
    Combinational,
    Sequential,
}

/// Returns the likely process category (i.e., either sequential or combinational).
/// This is only a heuristic that works correctly for common cases like
/// ```vhdl
/// process x(clk) is
/// begin
///     if rising_edge(clk) then
///         -- ...
///     end if;
/// end process;
/// ```
/// but won't work for some more exotic, mixed processes.
fn get_likely_process_category(root: &DesignRoot, process: &ProcessStatement) -> ProcessCategory {
    // using iter().any(...) guards against edge cases like the following:
    // ```
    // process(clkA, clkB) is
    // begin
    //     if rising_edge(clkA) then
    //     end if;
    //
    //     some_assignment <= xy;
    // end process;
    let is_likely_clocked = process.statements.iter().any(|stmt| {
        match &stmt.statement.item {
            SequentialStatement::If(if_stmt) => {
                // this is always guaranteed to be present
                let first_conditional = &if_stmt.conds.conditionals[0];
                is_likely_clocked(root, &first_conditional.condition.item)
                    || (if_stmt.conds.conditionals.len() == 2
                        && is_likely_clocked(root, &if_stmt.conds.conditionals[1].condition.item))
            }
            _ => false,
        }
    });
    if is_likely_clocked {
        ProcessCategory::Sequential
    } else {
        ProcessCategory::Combinational
    }
}

/// Returns whether an expression is likely clocked depending on whether that expression contains
/// the `'event` attribute or the `rising_edge(...)` resp. `falling_edge(...)` signals.
/// This is a heuristic and does not trigger for special cases such as `true or rising_edge(clk)`
/// which would not be clocked.
fn is_likely_clocked(root: &DesignRoot, expression: &Expression) -> bool {
    match expression {
        Expression::Binary(_, lhs, rhs) => {
            is_likely_clocked(root, &lhs.item) || is_likely_clocked(root, &rhs.item)
        }
        Expression::Unary(_, expr) => is_likely_clocked(root, &expr.item),
        Expression::Aggregate(_) => false,
        Expression::Qualified(_) => false,
        Expression::Name(name) => match name.as_ref() {
            Name::Attribute(attribute) => {
                attribute.attr.item == AttributeDesignator::Signal(SignalAttribute::Event)
            }
            Name::CallOrIndexed(coi) => {
                if let Some(reference) = coi.name.item.get_suffix_reference() {
                    let ent = root.get_ent(reference);
                    if let Some(library_name) = ent.library_name() {
                        if (library_name.name_utf8().to_lowercase() == "ieee"
                            || library_name.name_utf8().to_lowercase() == "std")
                            && (ent.designator.to_string().to_lowercase() == "rising_edge"
                                || ent.designator.to_string().to_lowercase() == "falling_edge")
                        {
                            return true;
                        }
                    }
                }
                false
            }
            _ => false,
        },
        Expression::Literal(_) => false,
        Expression::New(_) => false,
        Expression::Parenthesized(expr) => is_likely_clocked(root, &expr.item),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analysis::tests::{check_no_diagnostics, LibraryBuilder};
    use crate::syntax::test::check_diagnostics;
    use std::cell::Cell;

    #[test]
    fn extract_sensitivity_list_no_items() {
        let mut builder = LibraryBuilder::new();

        builder.code(
            "libname",
            "
entity ent is
end entity;

architecture a of ent is
begin
   process is
   begin
   end process;
end architecture;",
        );

        let (root, diagnostics) = builder.get_analyzed_root();
        check_no_diagnostics(&diagnostics);

        let num_of_searches = Cell::new(0);
        let mut searcher = ProcessSearcher::new(|proc, ctx| {
            num_of_searches.set(num_of_searches.get() + 1);
            let diag = lint_sensitivity_list(&root, ctx, proc);
            assert_eq!(diag, Vec::default());
        });
        let _ = root.search(&mut searcher);
        assert_eq!(num_of_searches.get(), 1)
    }

    #[test]
    fn extract_sensitivity_list_multiple_items() {
        let mut builder = LibraryBuilder::new();

        let code = builder.code(
            "libname",
            "
entity ent is
    port (
        sig_a : in bit
   );
end entity;

architecture a of ent is
    -- all of these signals should be considered as being 'read' in the sensitivity list
    signal sig_b, sig_c, sig_d, sig_e, sig_f, sig_g, sig_h, sig_i, sig_j, sig_k, sig_l : bit;
    -- all of these signals are fine and present in the sensitivity list
    signal sig_0 : bit;
    signal sig_1 : bit_vector(1 downto 0);
    type t_rec is record
        foo : bit;
    end record;
    signal sig_2 : t_rec;
    signal sig_3, sig_4, sig_5, sig_6 : bit; 
    -- superfluous signal
    signal additional : bit;
    signal void : bit;

    function func(din: bit) return bit is
    begin
    end func;
begin
   process(sig_0, sig_1(0), sig_2, sig_3, sig_4, sig_5, sig_6, additional) is
       variable void_var : bit;
   begin
       if (sig_5 = '1') then
           void <= sig_6;
       end if;
       void <= sig_3 or sig_4;
       void <= sig_3;
       void <= sig_2.foo;
       void <= sig_0;
       void <= sig_1(0);
       void <= sig_a;
       void <= sig_b;
       void <= sig_b;
       void_var := sig_c;
       assert sig_d = '1';
       report bit'image(sig_e);
       void <= force sig_f;
       if sig_g = '1' then
           void <= sig_h;
       end if;
       case sig_i is
           when '0' => void <= sig_j;
           when others => void <= sig_k;
       end case;
       void <= func(sig_l);
   end process;
end architecture;",
        );

        let (root, diagnostics) = builder.get_analyzed_root();
        check_no_diagnostics(&diagnostics);

        let expected_signals = [
            "sig_a", "sig_b", "sig_c", "sig_d", "sig_e", "sig_f", "sig_g", "sig_h", "sig_i",
            "sig_j", "sig_k", "sig_l",
        ]
        .map(|value| (value, code.s(value, 2).pos()));

        let num_of_searches = Cell::new(0);
        let mut searcher = ProcessSearcher::new(|proc, ctx| {
            num_of_searches.set(num_of_searches.get() + 1);
            let res = lint_sensitivity_list(&root, ctx, proc);
            let mut expected_missing_diag = Diagnostic::new(
                code.s1("process").pos(), 
                "Signals 'sig_a', 'sig_b', 'sig_c', 'sig_d', 'sig_e', 'sig_f', 'sig_g', 'sig_h', 'sig_i', 'sig_j', 'sig_k', 'sig_l' are not read in the sensitivity list",
                ErrorCode::MissingInSensitivityList
            );
            for (name, pos) in &expected_signals {
                expected_missing_diag.add_related(pos, format!("signal '{name}' first read here"));
            }
            let expected_superfluous_diag = Diagnostic::new(
                code.s("additional", 2),
                "Signal is never read in the process",
                ErrorCode::SuperfluousInSensitivityList,
            );
            check_diagnostics(res, vec![expected_missing_diag, expected_superfluous_diag]);
        });
        let _ = root.search(&mut searcher);
        assert_eq!(num_of_searches.get(), 1)
    }

    #[test]
    fn check_likely_process_category() {
        let mut builder = LibraryBuilder::new();
        builder.add_std_logic_1164();

        builder.code(
            "libname",
            "
library ieee;
use ieee.std_logic_1164.all;

entity ent is
end entity;

architecture a of ent is
    signal sig_a, sig_b : bit;
    signal sig_c_sl : std_logic;
begin
    a_comb : process(sig_a) is
    begin
        if sig_a = '1' then
        end if;
    end process a_comb;

    b_comb : process(sig_a) is
    begin
        sig_a <= '1';
    end process b_comb;

    c_comb : process(sig_a, sig_b) is
    begin
        sig_a <= '1';
        sig_b <= '0';
    end process c_comb;

    a_seq : process(sig_a) is
    begin
        if rising_edge(sig_a) then
        end if;
    end process a_seq;

    b_seq : process(sig_a, sig_b) is
    begin
        if sig_a = '1' then
        elsif rising_edge(sig_b) then
        end if;
    end process b_seq;

    c_seq : process(sig_a) is
    begin
        if sig_a'event and sig_a = '1' then
        end if;
    end process c_seq;

    d_seq : process(sig_a, sig_b) is
    begin
        if sig_b = '1' then
        elsif sig_a'event and sig_a = '0' then
        end if;
    end process d_seq;

    e_seq : process(sig_a) is
    begin
        if rising_edge(sig_a) then
        end if;
    end process e_seq;

    f_seq : process(sig_a, sig_b) is
    begin
        if rising_edge(sig_a) then
        end if;

        if rising_edge(sig_b) then
        end if;
    end process f_seq;
end architecture;",
        );

        let (root, diagnostics) = builder.get_analyzed_root();
        check_no_diagnostics(&diagnostics);

        let idx = Cell::new(0);
        let mut searcher = ProcessSearcher::new(|proc, _| {
            if idx.get() <= 2 {
                assert_eq!(
                    get_likely_process_category(&root, proc),
                    ProcessCategory::Combinational,
                    "Process #{} is sequential but should be combinatorial",
                    idx.get()
                );
            } else {
                assert_eq!(
                    get_likely_process_category(&root, proc),
                    ProcessCategory::Sequential,
                    "Process #{} is combinatorial but should be sequential",
                    idx.get()
                );
            }
            idx.set(idx.get() + 1);
        });
        let _ = root.search(&mut searcher);
        assert_eq!(idx.get(), 9);
    }
}
