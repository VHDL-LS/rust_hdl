// This Source Code Form is subject to the terms of the Mozilla Public
// Lic// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// This Source Code Form is subject to the terms of the Mozilla Public
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

// These fields are better explicit than .. since we are forced to consider if new fields should be searched
#![allow(clippy::unneeded_field_pattern)]

use super::*;
use crate::analysis::DesignRoot;
use crate::analysis::EntRef;
pub use crate::analysis::HasEntityId;
use crate::analysis::Related;

#[must_use]
pub enum SearchResult {
    Found,
    NotFound,
}

#[must_use]
pub enum SearchState {
    Finished(SearchResult),
    NotFinished,
}

pub use SearchResult::*;
pub use SearchState::*;

impl SearchState {
    fn or_not_found(self) -> SearchResult {
        match self {
            Finished(result) => result,
            NotFinished => NotFound,
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum FoundDeclaration<'a> {
    Object(&'a mut ObjectDeclaration),
    ElementDeclaration(&'a mut ElementDeclaration),
    EnumerationLiteral(&'a mut Ident, &'a mut WithDecl<WithPos<EnumerationLiteral>>),
    InterfaceObject(&'a mut InterfaceObjectDeclaration),
    InterfaceFile(&'a mut InterfaceFileDeclaration),
    File(&'a mut FileDeclaration),
    Type(&'a mut TypeDeclaration),
    InterfaceType(&'a mut WithDecl<Ident>),
    InterfacePackage(&'a mut InterfacePackageDeclaration),
    PhysicalTypePrimary(&'a mut WithDecl<Ident>),
    PhysicalTypeSecondary(&'a mut WithDecl<Ident>, &'a mut PhysicalLiteral),
    Component(&'a mut ComponentDeclaration),
    Attribute(&'a mut AttributeDeclaration),
    Alias(&'a mut AliasDeclaration),
    Function(&'a mut FunctionSpecification),
    Procedure(&'a mut ProcedureSpecification),
    Package(&'a mut PackageDeclaration),
    PackageBody(&'a mut PackageBody),
    PackageInstance(&'a mut PackageInstantiation),
    Configuration(&'a mut ConfigurationDeclaration),
    Entity(&'a mut EntityDeclaration),
    Architecture(&'a mut ArchitectureBody),
    Context(&'a mut ContextDeclaration),
    ForIndex(&'a mut WithDecl<Ident>, &'a mut DiscreteRange),
    ForGenerateIndex(Option<&'a Ident>, &'a mut ForGenerateStatement),
    GenerateBody(&'a mut WithDecl<Ident>),
    ConcurrentStatement(&'a Ident, &'a mut Reference),
    SequentialStatement(&'a Ident, &'a mut Reference),
}

pub trait Searcher {
    /// Search an position that has a reference to a declaration
    fn search_pos_with_ref(&mut self, _pos: &SrcPos, _ref: &mut Reference) -> SearchState {
        NotFinished
    }

    /// Search a designator that has a reference to a declaration
    fn search_designator_ref(
        &mut self,
        pos: &mut SrcPos,
        designator: &mut WithRef<Designator>,
    ) -> SearchState {
        self.search_pos_with_ref(pos, &mut designator.reference)
    }

    /// Search an identifier that has a reference to a declaration
    fn search_ident_ref(&mut self, ident: &mut WithRef<Ident>) -> SearchState {
        self.search_pos_with_ref(&ident.item.pos, &mut ident.reference)
    }

    /// Search a declaration of a named entity
    fn search_decl(&mut self, _decl: FoundDeclaration) -> SearchState {
        NotFinished
    }

    fn search_with_pos(&mut self, _pos: &SrcPos) -> SearchState {
        NotFinished
    }
    fn search_source(&mut self, _source: &Source) -> SearchState {
        NotFinished
    }
}

pub trait Search {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult;
}

#[macro_export]
macro_rules! return_if_found {
    ($result:expr) => {
        if let Found = $result {
            return Found;
        };
    };
}

#[macro_export]
macro_rules! return_if_finished {
    ($result:expr) => {
        if let Finished(result) = $result {
            return result;
        };
    };
}

impl<T: Search> Search for Vec<T> {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        for decl in self.iter_mut() {
            return_if_found!(decl.search(searcher));
        }
        NotFound
    }
}

impl<T: Search> Search for Option<T> {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        for decl in self.iter_mut() {
            return_if_found!(decl.search(searcher));
        }
        NotFound
    }
}

fn search_conditionals<T: Search>(
    conditionals: &mut Conditionals<T>,
    item_before_cond: bool,
    searcher: &mut impl Searcher,
) -> SearchResult {
    let Conditionals {
        conditionals,
        else_item,
    } = conditionals;
    for conditional in conditionals {
        let Conditional { condition, item } = conditional;
        if item_before_cond {
            // If
            return_if_found!(item.search(searcher));
            return_if_found!(condition.search(searcher));
        } else {
            // When
            return_if_found!(condition.search(searcher));
            return_if_found!(item.search(searcher));
        }
    }
    if let Some(expr) = else_item {
        return_if_found!(expr.search(searcher));
    }
    NotFound
}

fn search_alternatives<T: Search>(
    alternatives: &mut [Alternative<T>],
    item_before_choice: bool,
    searcher: &mut impl Searcher,
) -> SearchResult {
    for alternative in alternatives.iter_mut() {
        let Alternative { choices, item } = alternative;
        if item_before_choice {
            return_if_found!(item.search(searcher));
            return_if_found!(choices.search(searcher));
        } else {
            return_if_found!(choices.search(searcher));
            return_if_found!(item.search(searcher));
        }
    }
    NotFound
}

fn search_selection<T: Search>(
    selection: &mut Selection<T>,
    item_before_cond: bool,
    searcher: &mut impl Searcher,
) -> SearchResult {
    let Selection {
        expression,
        alternatives,
    } = selection;
    return_if_found!(expression.search(searcher));
    return_if_found!(search_alternatives(
        alternatives,
        item_before_cond,
        searcher
    ));
    NotFound
}

fn search_assignment<T: Search>(
    target: &mut WithPos<Target>,
    rhs: &mut AssignmentRightHand<T>,
    searcher: &mut impl Searcher,
) -> SearchResult {
    match rhs {
        AssignmentRightHand::Simple(item) => {
            return_if_found!(target.search(searcher));
            item.search(searcher)
        }
        AssignmentRightHand::Conditional(conditionals) => {
            return_if_found!(target.search(searcher));
            search_conditionals(conditionals, true, searcher)
        }
        AssignmentRightHand::Selected(selection) => {
            let Selection {
                expression,
                alternatives,
            } = selection;
            // expression comes before target
            return_if_found!(expression.search(searcher));
            return_if_found!(target.search(searcher));
            search_alternatives(alternatives, true, searcher)
        }
    }
}

impl Search for WithPos<Choice> {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        return_if_finished!(searcher.search_with_pos(&self.pos));

        match self.item {
            Choice::DiscreteRange(ref mut drange) => {
                return_if_found!(drange.search(searcher));
            }
            Choice::Expression(ref mut expr) => {
                return_if_found!(search_pos_expr(&mut self.pos, expr, searcher));
            }
            Choice::Others => {}
        }
        NotFound
    }
}

impl Search for WithPos<Target> {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        match self.item {
            Target::Name(ref mut name) => search_pos_name(&mut self.pos, name, searcher),
            Target::Aggregate(ref mut assocs) => assocs.search(searcher),
        }
    }
}

impl Search for LabeledSequentialStatement {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        if let Some(ref ident) = self.label.tree {
            return_if_found!(searcher
                .search_decl(FoundDeclaration::SequentialStatement(
                    ident,
                    &mut self.label.decl
                ))
                .or_not_found());
        }
        match self.statement.item {
            SequentialStatement::Return(ref mut ret) => {
                let ReturnStatement { ref mut expression } = ret;
                return_if_found!(expression.search(searcher));
            }
            SequentialStatement::ProcedureCall(ref mut pcall) => {
                return_if_finished!(searcher.search_with_pos(&pcall.pos));
                return_if_found!(pcall.item.search(searcher));
            }
            SequentialStatement::If(ref mut ifstmt) => {
                return_if_found!(search_conditionals(&mut ifstmt.conds, false, searcher));
            }
            SequentialStatement::Wait(ref mut wait_stmt) => {
                let WaitStatement {
                    sensitivity_clause,
                    condition_clause,
                    timeout_clause,
                } = wait_stmt;
                return_if_found!(sensitivity_clause.search(searcher));
                return_if_found!(condition_clause.search(searcher));
                return_if_found!(timeout_clause.search(searcher));
            }
            SequentialStatement::Assert(ref mut assert_stmt) => {
                let AssertStatement {
                    condition,
                    report,
                    severity,
                } = assert_stmt;
                return_if_found!(condition.search(searcher));
                return_if_found!(report.search(searcher));
                return_if_found!(severity.search(searcher));
            }
            SequentialStatement::Report(ref mut report_stmt) => {
                let ReportStatement { report, severity } = report_stmt;
                return_if_found!(report.search(searcher));
                return_if_found!(severity.search(searcher));
            }
            SequentialStatement::Exit(ref mut exit_stmt) => {
                let ExitStatement {
                    condition,
                    loop_label,
                } = exit_stmt;
                if let Some(loop_label) = loop_label {
                    return_if_found!(searcher
                        .search_pos_with_ref(&loop_label.item.pos, &mut loop_label.reference)
                        .or_not_found());
                }
                return_if_found!(condition.search(searcher));
            }
            SequentialStatement::Next(ref mut next_stmt) => {
                let NextStatement {
                    condition,
                    loop_label,
                } = next_stmt;
                if let Some(loop_label) = loop_label {
                    return_if_found!(searcher
                        .search_pos_with_ref(&loop_label.item.pos, &mut loop_label.reference)
                        .or_not_found());
                }
                return_if_found!(condition.search(searcher));
            }
            SequentialStatement::Case(ref mut case_stmt) => {
                return_if_found!(case_stmt.search(searcher));
            }
            SequentialStatement::Loop(ref mut loop_stmt) => {
                let LoopStatement {
                    iteration_scheme,
                    statements,
                    end_label_pos: _,
                } = loop_stmt;
                match iteration_scheme {
                    Some(IterationScheme::For(ref mut index, ref mut drange)) => {
                        return_if_found!(searcher
                            .search_decl(FoundDeclaration::ForIndex(index, drange))
                            .or_not_found());
                        return_if_found!(drange.search(searcher));
                        return_if_found!(statements.search(searcher));
                    }
                    Some(IterationScheme::While(ref mut expr)) => {
                        return_if_found!(expr.search(searcher));
                        return_if_found!(statements.search(searcher));
                    }
                    None => {
                        return_if_found!(statements.search(searcher));
                    }
                }
            }
            SequentialStatement::SignalAssignment(ref mut assign) => {
                // @TODO more
                let SignalAssignment { target, rhs, .. } = assign;
                return_if_found!(search_assignment(target, rhs, searcher));
            }
            SequentialStatement::VariableAssignment(ref mut assign) => {
                let VariableAssignment { target, rhs } = assign;
                return_if_found!(search_assignment(target, rhs, searcher));
            }
            SequentialStatement::SignalForceAssignment(ref mut assign) => {
                let SignalForceAssignment {
                    target,
                    force_mode: _,
                    rhs,
                } = assign;
                return_if_found!(search_assignment(target, rhs, searcher));
            }
            SequentialStatement::SignalReleaseAssignment(ref mut assign) => {
                let SignalReleaseAssignment {
                    target,
                    force_mode: _,
                } = assign;
                return_if_found!(target.search(searcher));
            }
            SequentialStatement::Null => {}
        }

        if let Some(end_label_pos) = self.statement.item.end_label_pos() {
            return_if_found!(searcher
                .search_pos_with_ref(end_label_pos, &mut self.label.decl)
                .or_not_found());
        }

        NotFound
    }
}

impl Search for GenerateBody {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        let GenerateBody {
            alternative_label,
            decl,
            statements,
            end_label_pos,
        } = self;
        if let Some(ref mut label) = alternative_label {
            return_if_found!(searcher
                .search_decl(FoundDeclaration::GenerateBody(label))
                .or_not_found());
        }
        return_if_found!(decl.search(searcher));
        return_if_found!(statements.search(searcher));

        if let Some(ref mut label) = alternative_label {
            if let Some(end_label_pos) = end_label_pos {
                return_if_found!(searcher
                    .search_pos_with_ref(end_label_pos, &mut label.decl)
                    .or_not_found());
            }
        }

        NotFound
    }
}

impl Search for InstantiationStatement {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        match self.unit {
            InstantiatedUnit::Entity(ref mut ent_name, ref mut architecture_name) => {
                return_if_found!(ent_name.search(searcher));
                if let Some(ref mut architecture_name) = architecture_name {
                    return_if_found!(searcher
                        .search_pos_with_ref(
                            &architecture_name.item.pos,
                            &mut architecture_name.reference
                        )
                        .or_not_found());
                }
            }
            InstantiatedUnit::Component(ref mut component_name) => {
                return_if_found!(component_name.search(searcher));
            }
            InstantiatedUnit::Configuration(ref mut config_name) => {
                return_if_found!(config_name.search(searcher));
            }
        };
        return_if_found!(self.generic_map.search(searcher));
        return_if_found!(self.port_map.search(searcher));

        NotFound
    }
}

impl Search for SensitivityList {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        match self {
            SensitivityList::Names(names) => names.search(searcher),
            SensitivityList::All => NotFound,
        }
    }
}

impl Search for LabeledConcurrentStatement {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        if let Some(ref ident) = self.label.tree {
            return_if_found!(searcher
                .search_decl(FoundDeclaration::ConcurrentStatement(
                    ident,
                    &mut self.label.decl
                ))
                .or_not_found());
        }
        match self.statement.item {
            ConcurrentStatement::Block(ref mut block) => {
                // @TODO guard condition
                return_if_found!(block.decl.search(searcher));
                return_if_found!(block.statements.search(searcher));
            }
            ConcurrentStatement::Process(ref mut process) => {
                let ProcessStatement {
                    postponed: _,
                    sensitivity_list,
                    decl,
                    statements,
                    end_label_pos: _,
                } = process;
                return_if_found!(sensitivity_list.search(searcher));
                return_if_found!(decl.search(searcher));
                return_if_found!(statements.search(searcher));
            }
            ConcurrentStatement::ForGenerate(ref mut gen) => {
                return_if_found!(searcher
                    .search_decl(FoundDeclaration::ForGenerateIndex(
                        self.label.tree.as_ref(),
                        gen
                    ))
                    .or_not_found());
                let ForGenerateStatement {
                    index_name: _,
                    discrete_range,
                    body,
                    end_label_pos: _,
                } = gen;
                return_if_found!(discrete_range.search(searcher));
                return_if_found!(body.search(searcher));
            }
            ConcurrentStatement::IfGenerate(ref mut gen) => {
                return_if_found!(search_conditionals(&mut gen.conds, false, searcher));
            }
            ConcurrentStatement::CaseGenerate(ref mut gen) => {
                return_if_found!(search_selection(&mut gen.sels, false, searcher));
            }
            ConcurrentStatement::Instance(ref mut inst) => {
                return_if_found!(inst.search(searcher));
            }
            ConcurrentStatement::Assignment(ref mut assign) => {
                let ConcurrentSignalAssignment { target, rhs, .. } = assign;
                return_if_found!(search_assignment(target, rhs, searcher));
            }
            ConcurrentStatement::ProcedureCall(ref mut pcall) => {
                let ConcurrentProcedureCall {
                    postponed: _postponed,
                    call,
                } = pcall;
                return_if_finished!(searcher.search_with_pos(&call.pos));
                return_if_found!(call.item.search(searcher));
            }
            ConcurrentStatement::Assert(ref mut assert) => {
                let ConcurrentAssertStatement {
                    postponed: _postponed,
                    statement:
                        AssertStatement {
                            condition,
                            report,
                            severity,
                        },
                } = assert;
                return_if_found!(condition.search(searcher));
                return_if_found!(report.search(searcher));
                return_if_found!(severity.search(searcher));
            }
        };

        if let Some(end_label_pos) = self.statement.item.end_label_pos() {
            return_if_found!(searcher
                .search_pos_with_ref(end_label_pos, &mut self.label.decl)
                .or_not_found());
        }

        NotFound
    }
}

impl Search for WithPos<WithRef<Designator>> {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        return_if_finished!(searcher.search_with_pos(&self.pos));
        searcher
            .search_designator_ref(&mut self.pos, &mut self.item)
            .or_not_found()
    }
}

impl Search for WithPos<SelectedName> {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        return_if_finished!(searcher.search_with_pos(&self.pos));
        match self.item {
            SelectedName::Selected(ref mut prefix, ref mut designator) => {
                return_if_found!(prefix.search(searcher));
                return_if_found!(designator.search(searcher));
                NotFound
            }
            SelectedName::Designator(ref mut designator) => searcher
                .search_designator_ref(&mut self.pos, designator)
                .or_not_found(),
        }
    }
}

fn search_pos_name(
    pos: &mut SrcPos,
    name: &mut Name,
    searcher: &mut impl Searcher,
) -> SearchResult {
    match name {
        Name::Selected(ref mut prefix, ref mut designator) => {
            return_if_found!(prefix.search(searcher));
            return_if_found!(designator.search(searcher));
            NotFound
        }
        Name::SelectedAll(ref mut prefix) => {
            return_if_found!(prefix.search(searcher));
            NotFound
        }
        Name::Designator(ref mut designator) => searcher
            .search_designator_ref(pos, designator)
            .or_not_found(),
        Name::Slice(ref mut prefix, ref mut dranges) => {
            return_if_found!(prefix.search(searcher));
            return_if_found!(dranges.search(searcher));
            NotFound
        }
        Name::CallOrIndexed(ref mut fcall) => fcall.search(searcher),
        Name::Attribute(ref mut attr) => {
            // @TODO more
            let AttributeName { name, expr, .. } = attr.as_mut();
            return_if_found!(name.search(searcher));
            if let Some(expr) = expr {
                return_if_found!(expr.search(searcher));
            }
            NotFound
        }
        Name::External(ref mut ename) => {
            let ExternalName { subtype, .. } = ename.as_mut();
            return_if_found!(subtype.search(searcher));
            NotFound
        }
    }
}

impl Search for WithPos<Name> {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        return_if_finished!(searcher.search_with_pos(&self.pos));
        search_pos_name(&mut self.pos, &mut self.item, searcher)
    }
}

impl Search for ElementConstraint {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        // @TODO more
        let ElementConstraint { constraint, .. } = self;
        constraint.search(searcher)
    }
}

impl Search for WithPos<ElementConstraint> {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        return_if_finished!(searcher.search_with_pos(&self.pos));
        self.item.search(searcher)
    }
}

impl Search for WithPos<SubtypeConstraint> {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        return_if_finished!(searcher.search_with_pos(&self.pos));
        match self.item {
            SubtypeConstraint::Array(ref mut dranges, ref mut constraint) => {
                return_if_found!(dranges.search(searcher));
                if let Some(ref mut constraint) = constraint {
                    return_if_found!(constraint.search(searcher));
                }
            }
            SubtypeConstraint::Range(ref mut range) => {
                return_if_found!(range.search(searcher));
            }
            SubtypeConstraint::Record(ref mut constraints) => {
                return_if_found!(constraints.search(searcher));
            }
        }
        NotFound
    }
}

impl Search for SubtypeIndication {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        // @TODO more
        let SubtypeIndication {
            type_mark,
            constraint,
            ..
        } = self;
        return_if_found!(type_mark.search(searcher));
        return_if_found!(constraint.search(searcher));
        NotFound
    }
}

impl Search for WithPos<TypeMark> {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        return_if_finished!(searcher.search_with_pos(&self.pos));
        self.item.name.search(searcher)
    }
}

impl Search for RangeConstraint {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        let RangeConstraint {
            direction: _,
            left_expr,
            right_expr,
        } = self;
        return_if_found!(left_expr.search(searcher));
        return_if_found!(right_expr.search(searcher));
        NotFound
    }
}

impl Search for AttributeName {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        // @TODO more
        let AttributeName { name, .. } = self;
        name.search(searcher)
    }
}

impl Search for Range {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        match self {
            Range::Range(constraint) => {
                return_if_found!(constraint.search(searcher));
            }
            Range::Attribute(attr) => {
                return_if_found!(attr.search(searcher));
            }
        }
        NotFound
    }
}

impl Search for DiscreteRange {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        match self {
            DiscreteRange::Discrete(ref mut type_mark, ref mut constraint) => {
                return_if_found!(type_mark.search(searcher));
                constraint.search(searcher)
            }
            DiscreteRange::Range(ref mut constraint) => constraint.search(searcher),
        }
    }
}

impl Search for TypeDeclaration {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        return_if_found!(searcher
            .search_decl(FoundDeclaration::Type(self))
            .or_not_found());

        match self.def {
            TypeDefinition::ProtectedBody(ref mut body) => {
                return_if_found!(body.decl.search(searcher));
            }
            TypeDefinition::Protected(ref mut prot_decl) => {
                for item in prot_decl.items.iter_mut() {
                    match item {
                        ProtectedTypeDeclarativeItem::Subprogram(ref mut subprogram) => {
                            return_if_found!(subprogram.search(searcher));
                        }
                    }
                }
            }
            TypeDefinition::Record(ref mut element_decls) => {
                for elem in element_decls {
                    return_if_found!(searcher
                        .search_decl(FoundDeclaration::ElementDeclaration(elem))
                        .or_not_found());
                    return_if_found!(elem.subtype.search(searcher));
                }
            }
            TypeDefinition::Access(ref mut subtype_indication) => {
                return_if_found!(subtype_indication.search(searcher));
            }
            TypeDefinition::Array(ref mut indexes, ref mut subtype_indication) => {
                for index in indexes.iter_mut() {
                    match index {
                        ArrayIndex::IndexSubtypeDefintion(ref mut type_mark) => {
                            return_if_found!(type_mark.search(searcher));
                        }
                        ArrayIndex::Discrete(ref mut drange) => {
                            return_if_found!(drange.search(searcher));
                        }
                    }
                }
                return_if_found!(subtype_indication.search(searcher));
            }
            TypeDefinition::Subtype(ref mut subtype_indication) => {
                return_if_found!(subtype_indication.search(searcher));
            }
            TypeDefinition::Numeric(ref mut range) => {
                return_if_found!(range.search(searcher));
            }
            TypeDefinition::File(ref mut type_mark) => {
                return_if_found!(type_mark.search(searcher));
            }
            TypeDefinition::Incomplete(ref mut reference) => {
                // Incomplete type should reference full declaration
                return_if_found!(searcher
                    .search_pos_with_ref(self.ident.pos(), reference)
                    .or_not_found());
            }
            TypeDefinition::Enumeration(ref mut literals) => {
                for literal in literals {
                    return_if_found!(searcher
                        .search_decl(FoundDeclaration::EnumerationLiteral(
                            &mut self.ident.tree,
                            literal
                        ))
                        .or_not_found());
                }
            }
            TypeDefinition::Physical(ref mut physical) => {
                let PhysicalTypeDeclaration {
                    range,
                    primary_unit,
                    secondary_units,
                } = physical;
                return_if_found!(range.search(searcher));
                return_if_found!(searcher
                    .search_decl(FoundDeclaration::PhysicalTypePrimary(primary_unit))
                    .or_not_found());
                for (ident, literal) in secondary_units.iter_mut() {
                    return_if_found!(searcher
                        .search_decl(FoundDeclaration::PhysicalTypeSecondary(ident, literal))
                        .or_not_found());
                    return_if_found!(searcher.search_ident_ref(&mut literal.unit).or_not_found());
                }
            }
        }
        NotFound
    }
}

fn search_pos_expr(
    pos: &mut SrcPos,
    expr: &mut Expression,
    searcher: &mut impl Searcher,
) -> SearchResult {
    return_if_finished!(searcher.search_with_pos(pos));
    match expr {
        Expression::Binary(ref mut op, ref mut left, ref mut right) => {
            return_if_found!(searcher
                .search_pos_with_ref(&op.pos, &mut op.item.reference)
                .or_not_found());
            return_if_found!(left.search(searcher));
            right.search(searcher)
        }
        Expression::Unary(ref mut op, ref mut expr) => {
            return_if_found!(searcher
                .search_pos_with_ref(&op.pos, &mut op.item.reference)
                .or_not_found());
            expr.search(searcher)
        }
        Expression::Name(ref mut name) => search_pos_name(pos, name, searcher),
        Expression::Aggregate(ref mut assocs) => assocs.search(searcher),
        Expression::Qualified(ref mut qexpr) => qexpr.search(searcher),
        Expression::New(ref mut alloc) => {
            return_if_finished!(searcher.search_with_pos(&alloc.pos));
            match alloc.item {
                Allocator::Qualified(ref mut qexpr) => qexpr.search(searcher),
                Allocator::Subtype(ref mut subtype) => subtype.search(searcher),
            }
        }
        Expression::Literal(literal) => match literal {
            Literal::Physical(PhysicalLiteral { unit, .. }) => {
                searcher.search_ident_ref(unit).or_not_found()
            }
            _ => NotFound,
        },
    }
}

impl Search for ElementAssociation {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        match self {
            ElementAssociation::Named(ref mut choices, ref mut expr) => {
                return_if_found!(choices.search(searcher));
                return_if_found!(expr.search(searcher));
            }
            ElementAssociation::Positional(ref mut expr) => {
                return_if_found!(expr.search(searcher));
            }
        }
        NotFound
    }
}

impl Search for QualifiedExpression {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        let QualifiedExpression { type_mark, expr } = self;
        return_if_found!(type_mark.search(searcher));
        return_if_found!(expr.search(searcher));
        NotFound
    }
}

impl Search for AssociationElement {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        let AssociationElement { formal, actual } = self;
        if let Some(formal) = formal {
            return_if_found!(search_pos_name(&mut formal.pos, &mut formal.item, searcher));
        }

        match actual.item {
            ActualPart::Expression(ref mut expr) => {
                return_if_found!(search_pos_expr(&mut actual.pos, expr, searcher));
            }
            ActualPart::Open => {}
        }
        NotFound
    }
}

impl Search for CallOrIndexed {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        let CallOrIndexed { name, parameters } = self;
        return_if_found!(name.search(searcher));
        return_if_found!(parameters.search(searcher));
        NotFound
    }
}

impl Search for Waveform {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        match self {
            Waveform::Elements(ref mut elems) => {
                for elem in elems.iter_mut() {
                    let WaveformElement { value, after } = elem;
                    return_if_found!(value.search(searcher));
                    return_if_found!(after.search(searcher));
                }
            }
            Waveform::Unaffected => {}
        }
        NotFound
    }
}

impl Search for WithPos<Expression> {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        search_pos_expr(&mut self.pos, &mut self.item, searcher)
    }
}

impl Search for ObjectDeclaration {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        return_if_found!(searcher
            .search_decl(FoundDeclaration::Object(self))
            .or_not_found());
        return_if_found!(self.subtype_indication.search(searcher));
        if let Some(ref mut expr) = self.expression {
            expr.search(searcher)
        } else {
            NotFound
        }
    }
}
impl Search for Signature {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        match self {
            Signature::Function(args, ret) => {
                return_if_found!(args.search(searcher));
                return_if_found!(ret.search(searcher));
            }
            Signature::Procedure(args) => {
                return_if_found!(args.search(searcher));
            }
        }
        NotFound
    }
}

impl Search for Declaration {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        match self {
            Declaration::Object(object) => {
                return_if_found!(object.search(searcher));
            }
            Declaration::Type(typ) => {
                return_if_found!(typ.search(searcher));
            }
            Declaration::SubprogramBody(body) => {
                return_if_found!(body.specification.search(searcher));
                return_if_found!(body.declarations.search(searcher));
                return_if_found!(body.statements.search(searcher));
                if let Some(ref end_ident_pos) = body.end_ident_pos {
                    return_if_found!(searcher
                        .search_pos_with_ref(end_ident_pos, body.specification.reference_mut())
                        .or_not_found());
                }
            }
            Declaration::SubprogramDeclaration(decl) => {
                return_if_found!(decl.search(searcher));
            }
            Declaration::Attribute(Attribute::Declaration(decl)) => {
                return_if_found!(searcher
                    .search_decl(FoundDeclaration::Attribute(decl))
                    .or_not_found());
                return_if_found!(decl.type_mark.search(searcher));
            }
            Declaration::Attribute(Attribute::Specification(AttributeSpecification {
                ident,
                entity_name,
                entity_class: _,
                expr,
            })) => {
                return_if_found!(searcher.search_ident_ref(ident).or_not_found());
                if let EntityName::Name(EntityTag {
                    designator,
                    signature,
                }) = entity_name
                {
                    return_if_found!(searcher
                        .search_pos_with_ref(&designator.pos, &mut designator.item.reference)
                        .or_not_found());
                    if let Some(signature) = signature {
                        return_if_found!(signature.item.search(searcher));
                    }
                }

                return_if_found!(expr.search(searcher));
            }
            Declaration::Alias(alias) => {
                return_if_found!(searcher
                    .search_decl(FoundDeclaration::Alias(alias))
                    .or_not_found());
                let AliasDeclaration {
                    designator: _,
                    subtype_indication,
                    name,
                    signature,
                } = alias;
                return_if_found!(subtype_indication.search(searcher));
                return_if_found!(name.search(searcher));
                if let Some(signature) = signature {
                    return_if_found!(signature.item.search(searcher));
                }
            }
            Declaration::Use(use_clause) => {
                return_if_found!(searcher.search_with_pos(&use_clause.pos).or_not_found());
                return_if_found!(use_clause.item.name_list.search(searcher));
            }
            Declaration::Component(component) => {
                return_if_found!(searcher
                    .search_decl(FoundDeclaration::Component(component))
                    .or_not_found());
                let ComponentDeclaration {
                    ident: _,
                    generic_list,
                    port_list,
                    end_ident_pos: _,
                } = component;
                return_if_found!(generic_list.search(searcher));
                return_if_found!(port_list.search(searcher));
            }

            Declaration::File(file) => {
                return_if_found!(searcher
                    .search_decl(FoundDeclaration::File(file))
                    .or_not_found());
                let FileDeclaration {
                    ident: _,
                    subtype_indication,
                    open_info,
                    file_name,
                } = file;
                return_if_found!(subtype_indication.search(searcher));
                return_if_found!(open_info.search(searcher));
                return_if_found!(file_name.search(searcher));
            }

            Declaration::Package(ref mut package_instance) => {
                return_if_found!(package_instance.search(searcher));
            }

            Declaration::Configuration(_) => {
                // @TODO
            }
        }
        NotFound
    }
}

impl Search for InterfaceDeclaration {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        match self {
            InterfaceDeclaration::Object(ref mut decl) => {
                return_if_found!(searcher
                    .search_decl(FoundDeclaration::InterfaceObject(decl))
                    .or_not_found());
                return_if_found!(decl.subtype_indication.search(searcher));
                return_if_found!(decl.expression.search(searcher));
            }
            InterfaceDeclaration::Subprogram(ref mut decl, ref mut subpgm_default) => {
                match decl {
                    SubprogramDeclaration::Function(f) => {
                        return_if_found!(searcher
                            .search_decl(FoundDeclaration::Function(f))
                            .or_not_found());
                    }
                    SubprogramDeclaration::Procedure(p) => {
                        return_if_found!(searcher
                            .search_decl(FoundDeclaration::Procedure(p))
                            .or_not_found());
                    }
                }

                if let Some(subpgm_default) = subpgm_default {
                    match subpgm_default {
                        SubprogramDefault::Name(selected_name) => {
                            return_if_found!(selected_name.search(searcher));
                        }
                        SubprogramDefault::Box => {}
                    }
                }
            }
            InterfaceDeclaration::Type(decl) => {
                return_if_found!(searcher
                    .search_decl(FoundDeclaration::InterfaceType(decl))
                    .or_not_found());
            }
            InterfaceDeclaration::Package(package_instance) => {
                return_if_found!(searcher
                    .search_decl(FoundDeclaration::InterfacePackage(package_instance))
                    .or_not_found());
                return_if_found!(package_instance.package_name.search(searcher));
                match package_instance.generic_map {
                    InterfacePackageGenericMapAspect::Map(ref mut generic_map) => {
                        return_if_found!(generic_map.search(searcher));
                    }
                    InterfacePackageGenericMapAspect::Box => {}
                    InterfacePackageGenericMapAspect::Default => {}
                }
            }
            InterfaceDeclaration::File(decl) => {
                return_if_found!(searcher
                    .search_decl(FoundDeclaration::InterfaceFile(decl))
                    .or_not_found());
            }
        };
        NotFound
    }
}

impl Search for SubprogramDeclaration {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        match self {
            SubprogramDeclaration::Function(ref mut decl) => {
                return_if_found!(decl.search(searcher));
            }
            SubprogramDeclaration::Procedure(ref mut decl) => {
                return_if_found!(decl.search(searcher));
            }
        }
        NotFound
    }
}

impl Search for ProcedureSpecification {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        return_if_found!(searcher
            .search_decl(FoundDeclaration::Procedure(self))
            .or_not_found());
        self.parameter_list.search(searcher)
    }
}

impl Search for FunctionSpecification {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        return_if_found!(searcher
            .search_decl(FoundDeclaration::Function(self))
            .or_not_found());
        return_if_found!(self.parameter_list.search(searcher));
        self.return_type.search(searcher)
    }
}

impl Search for LibraryClause {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        for name in self.name_list.iter_mut() {
            return_if_found!(searcher
                .search_pos_with_ref(&name.item.pos, &mut name.reference)
                .or_not_found());
        }
        NotFound
    }
}

impl Search for WithPos<ContextItem> {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        return_if_finished!(searcher.search_with_pos(&self.pos));
        match self.item {
            ContextItem::Use(ref mut use_clause) => {
                return_if_found!(use_clause.name_list.search(searcher));
            }
            ContextItem::Library(ref mut library_clause) => {
                return_if_found!(library_clause.search(searcher));
            }
            ContextItem::Context(ref mut context_clause) => {
                return_if_found!(context_clause.name_list.search(searcher));
            }
        }
        NotFound
    }
}

impl Search for AnyDesignUnit {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        delegate_any!(self, unit, unit.search(searcher))
    }
}

impl Search for AnyPrimaryUnit {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        delegate_primary!(self, unit, unit.search(searcher))
    }
}

impl Search for AnySecondaryUnit {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        delegate_secondary!(self, unit, unit.search(searcher))
    }
}

impl Search for EntityDeclaration {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        return_if_finished!(searcher.search_source(self.source()));
        return_if_found!(self.context_clause.search(searcher));
        return_if_found!(searcher
            .search_decl(FoundDeclaration::Entity(self))
            .or_not_found());
        return_if_found!(self.generic_clause.search(searcher));
        return_if_found!(self.port_clause.search(searcher));
        return_if_found!(self.decl.search(searcher));
        self.statements.search(searcher)
    }
}

impl Search for ArchitectureBody {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        return_if_finished!(searcher.search_source(self.source()));
        return_if_found!(self.context_clause.search(searcher));
        return_if_found!(searcher
            .search_ident_ref(&mut self.entity_name)
            .or_not_found());
        return_if_found!(searcher
            .search_decl(FoundDeclaration::Architecture(self))
            .or_not_found());
        return_if_found!(self.decl.search(searcher));
        self.statements.search(searcher)
    }
}

impl Search for PackageDeclaration {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        return_if_finished!(searcher.search_source(self.source()));
        return_if_found!(self.context_clause.search(searcher));
        return_if_found!(searcher
            .search_decl(FoundDeclaration::Package(self))
            .or_not_found());
        return_if_found!(self.generic_clause.search(searcher));
        self.decl.search(searcher)
    }
}

impl Search for PackageBody {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        return_if_finished!(searcher.search_source(self.source()));
        return_if_found!(self.context_clause.search(searcher));
        return_if_found!(searcher
            .search_decl(FoundDeclaration::PackageBody(self))
            .or_not_found());
        self.decl.search(searcher)
    }
}

impl Search for PackageInstantiation {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        return_if_finished!(searcher.search_source(self.source()));
        return_if_found!(self.context_clause.search(searcher));
        return_if_found!(searcher
            .search_decl(FoundDeclaration::PackageInstance(self))
            .or_not_found());
        return_if_found!(self.generic_map.search(searcher));
        self.package_name.search(searcher)
    }
}

impl Search for ConfigurationDeclaration {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        return_if_finished!(searcher.search_source(self.source()));
        return_if_found!(self.context_clause.search(searcher));
        return_if_found!(searcher
            .search_decl(FoundDeclaration::Configuration(self))
            .or_not_found());
        self.entity_name.search(searcher)
    }
}

impl Search for ContextDeclaration {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        return_if_finished!(searcher.search_source(self.source()));
        return_if_found!(searcher
            .search_decl(FoundDeclaration::Context(self))
            .or_not_found());
        self.items.search(searcher)
    }
}

impl Search for CaseStatement {
    fn search(&mut self, searcher: &mut impl Searcher) -> SearchResult {
        let CaseStatement {
            is_matching: _,
            expression,
            alternatives,
            end_label_pos: _,
        } = self;
        return_if_found!(expression.search(searcher));
        return_if_found!(search_alternatives(alternatives, false, searcher));
        NotFound
    }
}

// Search for reference to declaration/definition at cursor
pub struct ItemAtCursor {
    source: Source,
    cursor: Position,
    pub result: Option<(SrcPos, EntityId)>,
}

impl ItemAtCursor {
    pub fn new(source: &Source, cursor: Position) -> ItemAtCursor {
        ItemAtCursor {
            source: source.clone(),
            cursor,
            result: None,
        }
    }

    fn is_inside(&self, pos: &SrcPos) -> bool {
        // cursor is the gap between character cursor and cursor + 1
        // Thus cursor will match character cursor and cursor + 1
        pos.start() <= self.cursor && self.cursor <= pos.end()
    }

    fn search_decl_pos(&mut self, pos: &SrcPos, decl: &FoundDeclaration) -> SearchState {
        if self.is_inside(pos) {
            if let Some(id) = decl.ent_id() {
                self.result = Some((pos.clone(), id));
                Finished(Found)
            } else {
                Finished(NotFound)
            }
        } else {
            NotFinished
        }
    }
}

impl Searcher for ItemAtCursor {
    fn search_with_pos(&mut self, pos: &SrcPos) -> SearchState {
        // cursor is the gap between character cursor and cursor + 1
        // Thus cursor will match character cursor and cursor + 1
        if self.is_inside(pos) {
            NotFinished
        } else {
            Finished(NotFound)
        }
    }

    fn search_decl(&mut self, decl: FoundDeclaration) -> SearchState {
        let pos = decl.pos();
        if let Finished(res) = self.search_decl_pos(pos, &decl) {
            return Finished(res);
        }

        if let Some(end_pos) = decl.end_ident_pos() {
            self.search_decl_pos(end_pos, &decl)
        } else {
            NotFinished
        }
    }

    fn search_pos_with_ref(&mut self, pos: &SrcPos, reference: &mut Reference) -> SearchState {
        if self.is_inside(pos) {
            if let Some(id) = reference {
                self.result = Some((pos.clone(), *id));
                Finished(Found)
            } else {
                Finished(NotFound)
            }
        } else {
            NotFinished
        }
    }

    // Assume source is searched first to filter out design units in other files
    fn search_source(&mut self, source: &Source) -> SearchState {
        if source == &mut self.source {
            NotFinished
        } else {
            Finished(NotFound)
        }
    }
}

// Search for reference to declaration/definition at cursor
pub struct FindEnt<'a, T: Fn(EntRef<'a>) -> bool> {
    root: &'a DesignRoot,
    cond: T,
    pub result: Option<EntRef<'a>>,
}

impl<'a, T: Fn(EntRef<'a>) -> bool> FindEnt<'a, T> {
    pub fn new(root: &'a DesignRoot, cond: T) -> FindEnt<'a, T> {
        FindEnt {
            root,
            cond,
            result: None,
        }
    }
}

impl<'a, T: Fn(EntRef<'a>) -> bool> Searcher for FindEnt<'a, T> {
    fn search_decl(&mut self, decl: FoundDeclaration) -> SearchState {
        if let Some(id) = decl.ent_id() {
            let ent = self.root.get_ent(id);
            if (self.cond)(ent) {
                self.result = Some(ent);
                return SearchState::Finished(SearchResult::Found);
            }
        }

        SearchState::NotFinished
    }
}

pub struct FindAllEnt<'a, T: FnMut(EntRef<'a>) -> bool> {
    root: &'a DesignRoot,
    cond: T,
    pub result: Vec<EntRef<'a>>,
}

impl<'a, T: FnMut(EntRef<'a>) -> bool> FindAllEnt<'a, T> {
    pub fn new(root: &'a DesignRoot, cond: T) -> FindAllEnt<'a, T> {
        FindAllEnt {
            root,
            cond,
            result: Vec::default(),
        }
    }
}

impl<'a, T: FnMut(EntRef<'a>) -> bool> Searcher for FindAllEnt<'a, T> {
    fn search_decl(&mut self, decl: FoundDeclaration) -> SearchState {
        if let Some(id) = decl.ent_id() {
            let ent = self.root.get_ent(id);
            if (self.cond)(ent) {
                self.result.push(ent);
            }
        }

        SearchState::NotFinished
    }
}

// Search for a declaration/definition and format it
pub struct FormatDeclaration<'a> {
    ent: EntRef<'a>,
    pub result: Option<String>,
}

impl<'a> FormatDeclaration<'a> {
    pub fn new(ent: EntRef<'a>) -> FormatDeclaration<'a> {
        FormatDeclaration { ent, result: None }
    }
}

impl<'a> Searcher for FormatDeclaration<'a> {
    fn search_decl(&mut self, decl: FoundDeclaration) -> SearchState {
        let id = if let Some(id) = decl.ent_id() {
            id
        } else {
            return NotFinished;
        };

        if is_implicit_of(self.ent, id) {
            // Implicit
            self.result = Some(format!(
                "-- {}\n\n-- Implicitly defined by:\n{}\n",
                self.ent.describe(),
                decl,
            ));
            return Finished(Found);
        } else if self.ent.id() == id {
            // Explicit
            self.result = Some(decl.to_string());
            return Finished(Found);
        }
        NotFinished
    }
}

// Search for all references to declaration/definition
pub struct FindAllReferences<'a> {
    root: &'a DesignRoot,
    ent: EntRef<'a>,
    pub references: Vec<SrcPos>,
}

fn is_instance_of(ent: EntRef, other: EntRef) -> bool {
    if let Related::InstanceOf(ent) = ent.related {
        if ent.id() == other.id() {
            return true;
        }

        if is_instance_of(ent, other) {
            return true;
        }
    }

    false
}

fn is_declared_by(ent: EntRef, other: EntRef) -> bool {
    if let Related::DeclaredBy(ent) = ent.related {
        if ent.id() == other.id() {
            return true;
        }
    }

    false
}

fn is_implicit_of(ent: EntRef, id: EntityId) -> bool {
    match ent.related {
        Related::ImplicitOf(ent) => ent.id() == id,
        Related::InstanceOf(ent) => is_implicit_of(ent, id),
        Related::None => false,
        Related::DeclaredBy(_) => false,
    }
}

fn is_reference(ent: EntRef, other: EntRef) -> bool {
    if ent.id() == other.id() {
        return true;
    }

    if is_instance_of(ent, other) || is_instance_of(other, ent) {
        return true;
    }

    if is_declared_by(ent, other) || is_declared_by(other, ent) {
        return true;
    }

    false
}
impl<'a> FindAllReferences<'a> {
    pub fn new(root: &'a DesignRoot, ent: EntRef<'a>) -> FindAllReferences<'a> {
        FindAllReferences {
            root,
            ent,
            references: Vec::new(),
        }
    }
}

impl<'a> Searcher for FindAllReferences<'a> {
    fn search_decl(&mut self, decl: FoundDeclaration) -> SearchState {
        if let Some(id) = decl.ent_id() {
            let other = self.root.get_ent(id);

            if is_reference(self.ent, other) {
                self.references.push(decl.pos().clone());
                if let Some(pos) = decl.end_ident_pos() {
                    self.references.push(pos.clone());
                }
            }
        }
        NotFinished
    }

    fn search_pos_with_ref(&mut self, pos: &SrcPos, reference: &mut Reference) -> SearchState {
        if let Some(id) = reference.as_ref() {
            let other = self.root.get_ent(*id);
            if is_reference(self.ent, other) {
                self.references.push(pos.clone());
            }
        };
        NotFinished
    }
}

impl<'a> FoundDeclaration<'a> {
    fn end_ident_pos(&self) -> Option<&SrcPos> {
        match self {
            FoundDeclaration::InterfaceObject(_) => None,
            FoundDeclaration::ForIndex(..) => None,
            FoundDeclaration::ForGenerateIndex(..) => None,
            FoundDeclaration::Function(..) => None,
            FoundDeclaration::Procedure(..) => None,
            FoundDeclaration::Object(..) => None,
            FoundDeclaration::ElementDeclaration(..) => None,
            FoundDeclaration::EnumerationLiteral(..) => None,
            FoundDeclaration::File(..) => None,
            FoundDeclaration::Type(value) => value.end_ident_pos.as_ref(),
            FoundDeclaration::InterfaceType(..) => None,
            FoundDeclaration::InterfacePackage(..) => None,
            FoundDeclaration::InterfaceFile(..) => None,
            FoundDeclaration::PhysicalTypePrimary(..) => None,
            FoundDeclaration::PhysicalTypeSecondary(..) => None,
            FoundDeclaration::Component(value) => value.end_ident_pos.as_ref(),
            FoundDeclaration::Attribute(..) => None,
            FoundDeclaration::Alias(..) => None,
            FoundDeclaration::Package(value) => value.end_ident_pos.as_ref(),
            FoundDeclaration::PackageBody(value) => value.end_ident_pos.as_ref(),
            FoundDeclaration::PackageInstance(..) => None,
            FoundDeclaration::Configuration(value) => value.end_ident_pos.as_ref(),
            FoundDeclaration::Entity(value) => value.end_ident_pos.as_ref(),
            FoundDeclaration::Architecture(value) => value.end_ident_pos.as_ref(),
            FoundDeclaration::Context(value) => value.end_ident_pos.as_ref(),
            FoundDeclaration::GenerateBody(..) => None,
            FoundDeclaration::ConcurrentStatement(..) => None,
            FoundDeclaration::SequentialStatement(..) => None,
        }
    }
}

impl<'a> HasEntityId for FoundDeclaration<'a> {
    fn ent_id(&self) -> Option<EntityId> {
        match self {
            FoundDeclaration::InterfaceObject(value) => value.ident.decl,
            FoundDeclaration::ForIndex(ident, _) => ident.decl,
            FoundDeclaration::ForGenerateIndex(_, value) => value.index_name.decl,
            FoundDeclaration::Function(value) => value.designator.decl,
            FoundDeclaration::Procedure(value) => value.designator.decl,
            FoundDeclaration::Object(value) => value.ident.decl,
            FoundDeclaration::ElementDeclaration(elem) => elem.ident.decl,
            FoundDeclaration::EnumerationLiteral(_, elem) => elem.decl,
            FoundDeclaration::File(value) => value.ident.decl,
            FoundDeclaration::Type(value) => value.ident.decl,
            FoundDeclaration::InterfaceType(value) => value.decl,
            FoundDeclaration::InterfacePackage(value) => value.ident.decl,
            FoundDeclaration::InterfaceFile(value) => value.ident.decl,
            FoundDeclaration::PhysicalTypePrimary(value) => value.decl,
            FoundDeclaration::PhysicalTypeSecondary(value, _) => value.decl,
            FoundDeclaration::Component(value) => value.ident.decl,
            FoundDeclaration::Attribute(value) => value.ident.decl,
            FoundDeclaration::Alias(value) => value.designator.decl,
            FoundDeclaration::Package(value) => value.ident.decl,
            FoundDeclaration::PackageBody(value) => value.ident.decl,
            FoundDeclaration::PackageInstance(value) => value.ident.decl,
            FoundDeclaration::Configuration(value) => value.ident.decl,
            FoundDeclaration::Entity(value) => value.ident.decl,
            FoundDeclaration::Architecture(value) => value.ident.decl,
            FoundDeclaration::Context(value) => value.ident.decl,
            FoundDeclaration::GenerateBody(value) => value.decl,
            FoundDeclaration::ConcurrentStatement(_, value) => **value,
            FoundDeclaration::SequentialStatement(_, value) => **value,
        }
    }
}

impl<'a> HasSrcPos for FoundDeclaration<'a> {
    fn pos(&self) -> &SrcPos {
        match self {
            FoundDeclaration::InterfaceObject(value) => value.ident.pos(),
            FoundDeclaration::ForIndex(ident, _) => ident.pos(),
            FoundDeclaration::ForGenerateIndex(_, value) => value.index_name.pos(),
            FoundDeclaration::Function(value) => &value.designator.tree.pos,
            FoundDeclaration::Procedure(value) => &value.designator.tree.pos,
            FoundDeclaration::Object(value) => value.ident.pos(),
            FoundDeclaration::ElementDeclaration(elem) => elem.ident.pos(),
            FoundDeclaration::EnumerationLiteral(_, elem) => &elem.tree.pos,
            FoundDeclaration::File(value) => value.ident.pos(),
            FoundDeclaration::Type(value) => value.ident.pos(),
            FoundDeclaration::InterfaceType(value) => value.pos(),
            FoundDeclaration::InterfacePackage(value) => value.ident.pos(),
            FoundDeclaration::InterfaceFile(value) => value.ident.pos(),
            FoundDeclaration::PhysicalTypePrimary(value) => value.pos(),
            FoundDeclaration::PhysicalTypeSecondary(value, _) => value.as_ref(),
            FoundDeclaration::Component(value) => value.ident.pos(),
            FoundDeclaration::Alias(value) => &value.designator.tree.pos,
            FoundDeclaration::Attribute(value) => value.ident.pos(),
            FoundDeclaration::Package(value) => value.ident.pos(),
            FoundDeclaration::PackageBody(value) => value.ident.pos(),
            FoundDeclaration::PackageInstance(value) => value.ident.pos(),
            FoundDeclaration::Configuration(value) => value.ident.pos(),
            FoundDeclaration::Entity(value) => value.ident.pos(),
            FoundDeclaration::Architecture(value) => value.ident.pos(),
            FoundDeclaration::Context(value) => value.ident.pos(),
            FoundDeclaration::GenerateBody(value) => value.pos(),
            FoundDeclaration::ConcurrentStatement(value, _) => value.pos(),
            FoundDeclaration::SequentialStatement(value, _) => value.pos(),
        }
    }
}

impl std::fmt::Display for FoundDeclaration<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FoundDeclaration::InterfaceObject(ref value) => match value.list_type {
                InterfaceType::Port => write!(f, "port {value};"),
                InterfaceType::Generic => write!(f, "generic {value};"),
                InterfaceType::Parameter => write!(f, "{value};"),
            },
            FoundDeclaration::ForIndex(ref ident, ref drange) => {
                write!(f, "for {ident} in {drange} loop")
            }
            FoundDeclaration::ForGenerateIndex(ref ident, ref value) => match ident {
                Some(ident) => write!(f, "{ident}: {value}"),
                None => write!(f, "{value}"),
            },
            FoundDeclaration::Function(ref value) => {
                write!(f, "{value};")
            }
            FoundDeclaration::Procedure(ref value) => {
                write!(f, "{value};")
            }
            FoundDeclaration::Object(ref value) => {
                write!(f, "{value}")
            }
            FoundDeclaration::ElementDeclaration(elem) => {
                write!(f, "{elem}")
            }
            FoundDeclaration::EnumerationLiteral(ident, elem) => {
                write!(f, "{elem} : {ident}")
            }
            FoundDeclaration::File(ref value) => {
                write!(f, "{value}")
            }
            FoundDeclaration::PhysicalTypePrimary(ref value) => {
                write!(f, "{value}")
            }
            FoundDeclaration::PhysicalTypeSecondary(_, ref literal) => {
                write!(f, "{literal}")
            }
            FoundDeclaration::Type(ref value) => {
                write!(f, "{value}")
            }
            FoundDeclaration::InterfaceType(ref value) => {
                write!(f, "type {value}")
            }
            FoundDeclaration::InterfacePackage(value) => {
                write!(f, "{value}")
            }
            FoundDeclaration::InterfaceFile(value) => {
                write!(f, "{value}")
            }
            FoundDeclaration::Component(ref value) => {
                write!(f, "{value}")
            }
            FoundDeclaration::Alias(ref value) => {
                write!(f, "{value}")
            }
            FoundDeclaration::Attribute(ref value) => {
                write!(f, "{value}")
            }
            FoundDeclaration::Package(ref value) => {
                write!(f, "{value}")
            }
            FoundDeclaration::PackageBody(ref value) => {
                // Will never be shown has hover will goto the declaration
                write!(f, "package body {}", value.name())
            }
            FoundDeclaration::PackageInstance(ref value) => {
                write!(f, "{value}")
            }
            FoundDeclaration::Configuration(ref value) => {
                write!(f, "{value}")
            }
            FoundDeclaration::Entity(ref value) => {
                write!(f, "{value}")
            }
            FoundDeclaration::Architecture(value) => {
                write!(f, "architecture {} of {}", value.ident(), value.entity_name)
            }
            FoundDeclaration::Context(ref value) => {
                write!(f, "{value}")
            }
            FoundDeclaration::GenerateBody(value) => {
                write!(f, "{value}")
            }
            FoundDeclaration::ConcurrentStatement(value, _) => {
                write!(f, "{value}")
            }
            FoundDeclaration::SequentialStatement(value, _) => {
                write!(f, "{value}")
            }
        }
    }
}

#[derive(Default)]
pub struct FindAllUnresolved {
    pub count: usize,
    pub unresolved: Vec<SrcPos>,
}

impl Searcher for FindAllUnresolved {
    fn search_pos_with_ref(&mut self, pos: &SrcPos, reference: &mut Reference) -> SearchState {
        self.count += 1;
        if reference.is_none() {
            self.unresolved.push(pos.clone());
        }
        NotFinished
    }
}

pub fn clear_references(tree: &mut impl Search) {
    struct ReferenceClearer;

    impl Searcher for ReferenceClearer {
        fn search_pos_with_ref(&mut self, _pos: &SrcPos, reference: &mut Reference) -> SearchState {
            *reference = None;
            NotFinished
        }
    }

    let mut searcher = ReferenceClearer;
    let _ = tree.search(&mut searcher);
}

#[cfg(test)]
pub fn check_no_unresolved(tree: &mut impl Search) {
    #[derive(Default)]
    struct CheckNoUnresolved;

    impl Searcher for CheckNoUnresolved {
        fn search_pos_with_ref(&mut self, _pos: &SrcPos, reference: &mut Reference) -> SearchState {
            assert!(reference.is_some());
            NotFinished
        }
    }

    let mut searcher = CheckNoUnresolved;
    let _ = tree.search(&mut searcher);
}
