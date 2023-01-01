// This Source Code Form is subject to the terms of the Mozilla Public
// Lic// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// This Source Code Form is subject to the terms of the Mozilla Public
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

// These fields are better explicit than .. since we are forced to consider if new fields should be searched
#![allow(clippy::unneeded_field_pattern)]

use super::*;
pub use crate::analysis::HasNamedEntity;

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
    fn or_else(self, nested_fun: impl FnOnce() -> SearchResult) -> SearchResult {
        match self {
            Finished(result) => result,
            NotFinished => nested_fun(),
        }
    }

    fn or_not_found(self) -> SearchResult {
        self.or_else(|| NotFound)
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum FoundDeclaration<'a> {
    Object(&'a ObjectDeclaration),
    ElementDeclaration(&'a ElementDeclaration),
    EnumerationLiteral(&'a Ident, &'a WithDecl<WithPos<EnumerationLiteral>>),
    InterfaceObject(&'a InterfaceObjectDeclaration),
    File(&'a FileDeclaration),
    Type(&'a TypeDeclaration),
    Component(&'a ComponentDeclaration),
    //Attribute(&'a AttributeDeclaration),
    Alias(&'a AliasDeclaration),
    Function(&'a FunctionSpecification),
    Procedure(&'a ProcedureSpecification),
    Library(&'a Ident),
    Package(&'a PackageDeclaration),
    PackageInstance(&'a PackageInstantiation),
    Configuration(&'a ConfigurationDeclaration),
    Entity(&'a EntityDeclaration),
    Context(&'a ContextDeclaration),
    ForIndex(&'a WithDecl<Ident>, &'a DiscreteRange),
    ForGenerateIndex(Option<&'a Ident>, &'a ForGenerateStatement),
    ConcurrentStatement(&'a WithDecl<Ident>),
    GenerateBody(&'a WithDecl<Ident>),
    SequentialStatement(&'a WithDecl<Ident>),
}

pub trait Searcher {
    /// Search an position that has a reference to a declaration
    fn search_pos_with_ref(&mut self, _pos: &SrcPos, _ref: &Reference) -> SearchState {
        NotFinished
    }

    /// Search a designator that has a reference to a declaration
    fn search_designator_ref(
        &mut self,
        pos: &SrcPos,
        designator: &WithRef<Designator>,
    ) -> SearchState {
        self.search_pos_with_ref(pos, &designator.reference)
    }

    /// Search an identifier that has a reference to a declaration
    fn search_ident_ref(&mut self, ident: &WithRef<Ident>) -> SearchState {
        self.search_pos_with_ref(&ident.item.pos, &ident.reference)
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
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult;
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
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        for decl in self.iter() {
            return_if_found!(decl.search(searcher));
        }
        NotFound
    }
}

impl<T: Search> Search for Option<T> {
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        for decl in self.iter() {
            return_if_found!(decl.search(searcher));
        }
        NotFound
    }
}

fn search_conditionals<T: Search>(
    conditionals: &Conditionals<T>,
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
    alternatives: &[Alternative<T>],
    item_before_choice: bool,
    searcher: &mut impl Searcher,
) -> SearchResult {
    for alternative in alternatives.iter() {
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
    selection: &Selection<T>,
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
    target: &WithPos<Target>,
    rhs: &AssignmentRightHand<T>,
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

impl Search for Choice {
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        match self {
            Choice::DiscreteRange(ref drange) => {
                return_if_found!(drange.search(searcher));
            }
            Choice::Expression(ref expr) => {
                return_if_found!(expr.search(searcher));
            }
            Choice::Others => {}
        }
        NotFound
    }
}

impl Search for WithPos<Target> {
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        match self.item {
            Target::Name(ref name) => search_pos_name(&self.pos, name, searcher),
            Target::Aggregate(ref assocs) => assocs.search(searcher),
        }
    }
}

impl Search for LabeledSequentialStatement {
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        if let Some(ref label) = self.label {
            return_if_found!(searcher
                .search_decl(FoundDeclaration::SequentialStatement(label))
                .or_not_found());
        }
        match self.statement {
            SequentialStatement::Return(ReturnStatement { ref expression }) => {
                return_if_found!(expression.search(searcher));
            }
            SequentialStatement::ProcedureCall(ref pcall) => {
                return_if_found!(pcall.search(searcher));
            }
            SequentialStatement::If(ref ifstmt) => {
                return_if_found!(search_conditionals(ifstmt, false, searcher));
            }
            SequentialStatement::Wait(ref wait_stmt) => {
                let WaitStatement {
                    sensitivity_clause,
                    condition_clause,
                    timeout_clause,
                } = wait_stmt;
                return_if_found!(sensitivity_clause.search(searcher));
                return_if_found!(condition_clause.search(searcher));
                return_if_found!(timeout_clause.search(searcher));
            }
            SequentialStatement::Assert(ref assert_stmt) => {
                let AssertStatement {
                    condition,
                    report,
                    severity,
                } = assert_stmt;
                return_if_found!(condition.search(searcher));
                return_if_found!(report.search(searcher));
                return_if_found!(severity.search(searcher));
            }
            SequentialStatement::Report(ref report_stmt) => {
                let ReportStatement { report, severity } = report_stmt;
                return_if_found!(report.search(searcher));
                return_if_found!(severity.search(searcher));
            }
            SequentialStatement::Exit(ref exit_stmt) => {
                // @TODO loop label
                let ExitStatement { condition, .. } = exit_stmt;
                return_if_found!(condition.search(searcher));
            }
            SequentialStatement::Next(ref next_stmt) => {
                // @TODO loop label
                let NextStatement { condition, .. } = next_stmt;
                return_if_found!(condition.search(searcher));
            }
            SequentialStatement::Case(ref case_stmt) => {
                return_if_found!(case_stmt.search(searcher));
            }
            SequentialStatement::Loop(ref loop_stmt) => {
                let LoopStatement {
                    iteration_scheme,
                    statements,
                } = loop_stmt;
                match iteration_scheme {
                    Some(IterationScheme::For(ref index, ref drange)) => {
                        return_if_found!(searcher
                            .search_decl(FoundDeclaration::ForIndex(index, drange))
                            .or_not_found());
                        return_if_found!(drange.search(searcher));
                        return_if_found!(statements.search(searcher));
                    }
                    Some(IterationScheme::While(ref expr)) => {
                        return_if_found!(expr.search(searcher));
                        return_if_found!(statements.search(searcher));
                    }
                    None => {
                        return_if_found!(statements.search(searcher));
                    }
                }
            }
            SequentialStatement::SignalAssignment(ref assign) => {
                // @TODO more
                let SignalAssignment { target, rhs, .. } = assign;
                return_if_found!(search_assignment(target, rhs, searcher));
            }
            SequentialStatement::VariableAssignment(ref assign) => {
                let VariableAssignment { target, rhs } = assign;
                return_if_found!(search_assignment(target, rhs, searcher));
            }
            SequentialStatement::SignalForceAssignment(ref assign) => {
                let SignalForceAssignment {
                    target,
                    force_mode: _,
                    rhs,
                } = assign;
                return_if_found!(search_assignment(target, rhs, searcher));
            }
            SequentialStatement::SignalReleaseAssignment(ref assign) => {
                let SignalReleaseAssignment {
                    target,
                    force_mode: _,
                } = assign;
                return_if_found!(target.search(searcher));
            }
            SequentialStatement::Null => {}
        }
        NotFound
    }
}

impl Search for GenerateBody {
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        let GenerateBody {
            alternative_label,
            decl,
            statements,
        } = self;
        if let Some(ref label) = alternative_label {
            return_if_found!(searcher
                .search_decl(FoundDeclaration::GenerateBody(label))
                .or_not_found());
        }
        return_if_found!(decl.search(searcher));
        statements.search(searcher)
    }
}

impl Search for InstantiationStatement {
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        match self.unit {
            InstantiatedUnit::Entity(ref ent_name, _) => {
                return_if_found!(ent_name.search(searcher));
            }
            InstantiatedUnit::Component(ref component_name) => {
                return_if_found!(component_name.search(searcher));
            }
            InstantiatedUnit::Configuration(ref config_name) => {
                return_if_found!(config_name.search(searcher));
            }
        };
        return_if_found!(self.generic_map.search(searcher));
        return_if_found!(self.port_map.search(searcher));

        NotFound
    }
}

impl Search for SensitivityList {
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        match self {
            SensitivityList::Names(names) => names.search(searcher),
            SensitivityList::All => NotFound,
        }
    }
}

impl Search for LabeledConcurrentStatement {
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        if let Some(ref label) = self.label {
            return_if_found!(searcher
                .search_decl(FoundDeclaration::ConcurrentStatement(label))
                .or_not_found());
        }
        match self.statement {
            ConcurrentStatement::Block(ref block) => {
                // @TODO guard condition
                return_if_found!(block.decl.search(searcher));
                block.statements.search(searcher)
            }
            ConcurrentStatement::Process(ref process) => {
                let ProcessStatement {
                    postponed: _,
                    sensitivity_list,
                    decl,
                    statements,
                } = process;
                return_if_found!(sensitivity_list.search(searcher));
                return_if_found!(decl.search(searcher));
                statements.search(searcher)
            }
            ConcurrentStatement::ForGenerate(ref gen) => {
                let ForGenerateStatement {
                    index_name: _,
                    discrete_range,
                    body,
                } = gen;
                return_if_found!(searcher
                    .search_decl(FoundDeclaration::ForGenerateIndex(
                        self.label.as_ref().map(|l| &l.tree),
                        gen
                    ))
                    .or_not_found());
                return_if_found!(discrete_range.search(searcher));
                body.search(searcher)
            }
            ConcurrentStatement::IfGenerate(ref gen) => search_conditionals(gen, false, searcher),
            ConcurrentStatement::CaseGenerate(ref gen) => search_selection(gen, false, searcher),
            ConcurrentStatement::Instance(ref inst) => inst.search(searcher),
            ConcurrentStatement::Assignment(ref assign) => {
                let ConcurrentSignalAssignment { target, rhs, .. } = assign;
                return_if_found!(search_assignment(target, rhs, searcher));
                NotFound
            }
            ConcurrentStatement::ProcedureCall(ref pcall) => {
                let ConcurrentProcedureCall {
                    postponed: _postponed,
                    call,
                } = pcall;
                return_if_found!(call.search(searcher));
                NotFound
            }
            ConcurrentStatement::Assert(ref assert) => {
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
                NotFound
            }
        }
    }
}

impl Search for WithPos<WithRef<Designator>> {
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        return_if_finished!(searcher.search_with_pos(&self.pos));
        searcher
            .search_designator_ref(&self.pos, &self.item)
            .or_not_found()
    }
}

impl Search for WithPos<SelectedName> {
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        return_if_finished!(searcher.search_with_pos(&self.pos));
        match self.item {
            SelectedName::Selected(ref prefix, ref designator) => {
                return_if_found!(prefix.search(searcher));
                return_if_found!(designator.search(searcher));
                NotFound
            }
            SelectedName::Designator(ref designator) => searcher
                .search_designator_ref(&self.pos, designator)
                .or_not_found(),
        }
    }
}

fn search_pos_name(pos: &SrcPos, name: &Name, searcher: &mut impl Searcher) -> SearchResult {
    match name {
        Name::Selected(ref prefix, ref designator) => {
            return_if_found!(prefix.search(searcher));
            return_if_found!(designator.search(searcher));
            NotFound
        }
        Name::SelectedAll(ref prefix) => {
            return_if_found!(prefix.search(searcher));
            NotFound
        }
        Name::Designator(ref designator) => searcher
            .search_designator_ref(pos, designator)
            .or_not_found(),
        Name::Indexed(ref prefix, ref indexes) => {
            return_if_found!(prefix.search(searcher));
            return_if_found!(indexes.search(searcher));
            NotFound
        }
        Name::Slice(ref prefix, ref dranges) => {
            return_if_found!(prefix.search(searcher));
            return_if_found!(dranges.search(searcher));
            NotFound
        }
        Name::FunctionCall(ref fcall) => fcall.search(searcher),
        Name::Attribute(ref attr) => {
            // @TODO more
            let AttributeName { name, expr, .. } = attr.as_ref();
            return_if_found!(name.search(searcher));
            if let Some(expr) = expr {
                return_if_found!(expr.search(searcher));
            }
            NotFound
        }
        Name::External(ref ename) => {
            let ExternalName { subtype, .. } = ename.as_ref();
            return_if_found!(subtype.search(searcher));
            NotFound
        }
    }
}

impl Search for WithPos<Name> {
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        return_if_finished!(searcher.search_with_pos(&self.pos));
        search_pos_name(&self.pos, &self.item, searcher)
    }
}

impl Search for ElementConstraint {
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        // @TODO more
        let ElementConstraint { constraint, .. } = self;
        constraint.search(searcher)
    }
}

impl Search for WithPos<ElementConstraint> {
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        return_if_finished!(searcher.search_with_pos(&self.pos));
        self.item.search(searcher)
    }
}

impl Search for WithPos<SubtypeConstraint> {
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        return_if_finished!(searcher.search_with_pos(&self.pos));
        match self.item {
            SubtypeConstraint::Array(ref dranges, ref constraint) => {
                return_if_found!(dranges.search(searcher));
                if let Some(ref constraint) = constraint {
                    return_if_found!(constraint.search(searcher));
                }
            }
            SubtypeConstraint::Range(ref range) => {
                return_if_found!(range.search(searcher));
            }
            SubtypeConstraint::Record(ref constraints) => {
                return_if_found!(constraints.search(searcher));
            }
        }
        NotFound
    }
}

impl Search for SubtypeIndication {
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
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
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        return_if_finished!(searcher.search_with_pos(&self.pos));
        self.item.name.search(searcher)
    }
}

impl Search for RangeConstraint {
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
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
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        // @TODO more
        let AttributeName { name, .. } = self;
        name.search(searcher)
    }
}

impl Search for Range {
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
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
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        match self {
            DiscreteRange::Discrete(ref type_mark, ref constraint) => {
                return_if_found!(type_mark.search(searcher));
                constraint.search(searcher)
            }
            DiscreteRange::Range(ref constraint) => constraint.search(searcher),
        }
    }
}

impl Search for TypeDeclaration {
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        match self.def {
            TypeDefinition::ProtectedBody(ref body) => {
                // Protected type body is not considered a declaration
                return_if_found!(searcher
                    .search_pos_with_ref(self.ident.pos(), &body.type_reference)
                    .or_not_found());
                return_if_found!(body.decl.search(searcher));
            }
            TypeDefinition::Protected(ref prot_decl) => {
                return_if_found!(searcher
                    .search_decl(FoundDeclaration::Type(self))
                    .or_not_found());
                for item in prot_decl.items.iter() {
                    match item {
                        ProtectedTypeDeclarativeItem::Subprogram(ref subprogram) => {
                            return_if_found!(subprogram.search(searcher));
                        }
                    }
                }
            }
            TypeDefinition::Record(ref element_decls) => {
                return_if_found!(searcher
                    .search_decl(FoundDeclaration::Type(self))
                    .or_not_found());
                for elem in element_decls {
                    return_if_found!(searcher
                        .search_decl(FoundDeclaration::ElementDeclaration(elem))
                        .or_not_found());
                    return_if_found!(elem.subtype.search(searcher));
                }
            }
            TypeDefinition::Access(ref subtype_indication) => {
                return_if_found!(searcher
                    .search_decl(FoundDeclaration::Type(self))
                    .or_not_found());
                return_if_found!(subtype_indication.search(searcher));
            }
            TypeDefinition::Array(ref indexes, ref subtype_indication) => {
                return_if_found!(searcher
                    .search_decl(FoundDeclaration::Type(self))
                    .or_not_found());
                for index in indexes.iter() {
                    match index {
                        ArrayIndex::IndexSubtypeDefintion(ref type_mark) => {
                            return_if_found!(type_mark.search(searcher));
                        }
                        ArrayIndex::Discrete(ref drange) => {
                            return_if_found!(drange.search(searcher));
                        }
                    }
                }
                return_if_found!(subtype_indication.search(searcher));
            }
            TypeDefinition::Subtype(ref subtype_indication) => {
                return_if_found!(searcher
                    .search_decl(FoundDeclaration::Type(self))
                    .or_not_found());
                return_if_found!(subtype_indication.search(searcher));
            }
            TypeDefinition::Integer(ref range) => {
                return_if_found!(searcher
                    .search_decl(FoundDeclaration::Type(self))
                    .or_not_found());
                return_if_found!(range.search(searcher));
            }
            TypeDefinition::File(ref type_mark) => {
                return_if_found!(searcher
                    .search_decl(FoundDeclaration::Type(self))
                    .or_not_found());
                return_if_found!(type_mark.search(searcher));
            }
            TypeDefinition::Incomplete(ref reference) => {
                // Incomplete type should reference full declaration
                return_if_found!(searcher
                    .search_pos_with_ref(self.ident.pos(), reference)
                    .or_not_found());
            }
            TypeDefinition::Enumeration(ref literals) => {
                return_if_found!(searcher
                    .search_decl(FoundDeclaration::Type(self))
                    .or_not_found());

                for literal in literals {
                    return_if_found!(searcher
                        .search_decl(FoundDeclaration::EnumerationLiteral(
                            &self.ident.tree,
                            literal
                        ))
                        .or_not_found());
                }
            }
            // @TODO others
            _ => {
                return_if_found!(searcher
                    .search_decl(FoundDeclaration::Type(self))
                    .or_not_found());
            }
        }
        NotFound
    }
}

fn search_pos_expr(pos: &SrcPos, expr: &Expression, searcher: &mut impl Searcher) -> SearchResult {
    return_if_finished!(searcher.search_with_pos(pos));
    match expr {
        Expression::Binary(_, ref left, ref right) => {
            return_if_found!(left.search(searcher));
            right.search(searcher)
        }
        Expression::Unary(_, ref expr) => expr.search(searcher),
        Expression::Name(ref name) => search_pos_name(pos, name, searcher),
        Expression::Aggregate(ref assocs) => assocs.search(searcher),
        Expression::Qualified(ref qexpr) => qexpr.search(searcher),
        Expression::New(ref alloc) => {
            return_if_finished!(searcher.search_with_pos(&alloc.pos));
            match alloc.item {
                Allocator::Qualified(ref qexpr) => qexpr.search(searcher),
                Allocator::Subtype(ref subtype) => subtype.search(searcher),
            }
        }
        Expression::Literal(_) => NotFound,
    }
}

impl Search for ElementAssociation {
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        match self {
            ElementAssociation::Named(ref choices, ref expr) => {
                return_if_found!(choices.search(searcher));
                return_if_found!(expr.search(searcher));
            }
            ElementAssociation::Positional(ref expr) => {
                return_if_found!(expr.search(searcher));
            }
        }
        NotFound
    }
}

impl Search for QualifiedExpression {
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        let QualifiedExpression { type_mark, expr } = self;
        return_if_found!(type_mark.search(searcher));
        return_if_found!(expr.search(searcher));
        NotFound
    }
}

impl Search for AssociationElement {
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        let AssociationElement { formal, actual } = self;
        if let Some(formal) = formal {
            return_if_found!(search_pos_name(&formal.pos, &formal.item, searcher));
        }

        match actual.item {
            ActualPart::Expression(ref expr) => {
                return_if_found!(search_pos_expr(&actual.pos, expr, searcher));
            }
            ActualPart::Open => {}
        }
        NotFound
    }
}

impl Search for FunctionCall {
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        let FunctionCall { name, parameters } = self;
        return_if_found!(name.search(searcher));
        return_if_found!(parameters.search(searcher));
        NotFound
    }
}

impl Search for Waveform {
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        match self {
            Waveform::Elements(ref elems) => {
                for elem in elems.iter() {
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
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        search_pos_expr(&self.pos, &self.item, searcher)
    }
}

impl Search for ObjectDeclaration {
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        return_if_found!(searcher
            .search_decl(FoundDeclaration::Object(self))
            .or_not_found());
        return_if_found!(self.subtype_indication.search(searcher));
        if let Some(ref expr) = self.expression {
            expr.search(searcher)
        } else {
            NotFound
        }
    }
}
impl Search for Signature {
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
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
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
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
            }
            Declaration::SubprogramDeclaration(decl) => {
                return_if_found!(decl.search(searcher));
            }
            Declaration::Attribute(Attribute::Declaration(decl)) => {
                return_if_found!(decl.type_mark.search(searcher));
            }
            Declaration::Alias(alias) => {
                let AliasDeclaration {
                    designator: _,
                    subtype_indication,
                    name,
                    signature,
                } = alias;
                return_if_found!(searcher
                    .search_decl(FoundDeclaration::Alias(alias))
                    .or_not_found());
                return_if_found!(subtype_indication.search(searcher));
                return_if_found!(name.search(searcher));
                if let Some(signature) = signature {
                    return_if_found!(signature.item.search(searcher));
                }
            }
            Declaration::Use(use_clause) => {
                return_if_found!(searcher
                    .search_with_pos(&use_clause.pos)
                    .or_else(|| use_clause.item.name_list.search(searcher)));
            }
            Declaration::Component(component) => {
                let ComponentDeclaration {
                    ident: _,
                    generic_list,
                    port_list,
                } = component;
                return_if_found!(searcher
                    .search_decl(FoundDeclaration::Component(component))
                    .or_not_found());
                return_if_found!(generic_list.search(searcher));
                return_if_found!(port_list.search(searcher));
            }

            Declaration::File(file) => {
                let FileDeclaration {
                    ident: _,
                    subtype_indication,
                    open_info,
                    file_name,
                } = file;
                return_if_found!(searcher
                    .search_decl(FoundDeclaration::File(file))
                    .or_not_found());
                return_if_found!(subtype_indication.search(searcher));
                return_if_found!(open_info.search(searcher));
                return_if_found!(file_name.search(searcher));
            }

            // @TODO more
            _ => {}
        }
        NotFound
    }
}

impl Search for InterfaceDeclaration {
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        match self {
            InterfaceDeclaration::Object(ref decl) => {
                return_if_found!(searcher
                    .search_decl(FoundDeclaration::InterfaceObject(decl))
                    .or_not_found());
                return_if_found!(decl.subtype_indication.search(searcher));
                return_if_found!(decl.expression.search(searcher));
            }
            InterfaceDeclaration::Subprogram(ref decl, _) => {
                return_if_found!(decl.search(searcher));
            }
            _ => {}
        };
        NotFound
    }
}

impl Search for SubprogramDeclaration {
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        match self {
            SubprogramDeclaration::Function(ref decl) => {
                return_if_found!(decl.search(searcher));
            }
            SubprogramDeclaration::Procedure(ref decl) => {
                return_if_found!(decl.search(searcher));
            }
        }
        NotFound
    }
}

impl Search for ProcedureSpecification {
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        return_if_found!(searcher
            .search_decl(FoundDeclaration::Procedure(self))
            .or_not_found());
        self.parameter_list.search(searcher)
    }
}

impl Search for FunctionSpecification {
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        return_if_found!(searcher
            .search_decl(FoundDeclaration::Function(self))
            .or_not_found());
        return_if_found!(self.parameter_list.search(searcher));
        self.return_type.search(searcher)
    }
}

impl Search for LibraryClause {
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        for name in self.name_list.iter() {
            return_if_found!(searcher
                .search_decl(FoundDeclaration::Library(name))
                .or_not_found());
        }
        NotFound
    }
}

impl Search for WithPos<ContextItem> {
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        return_if_finished!(searcher.search_with_pos(&self.pos));
        match self.item {
            ContextItem::Use(ref use_clause) => {
                return_if_found!(use_clause.name_list.search(searcher));
            }
            ContextItem::Library(ref library_clause) => {
                return_if_found!(library_clause.search(searcher));
            }
            ContextItem::Context(ref context_clause) => {
                return_if_found!(context_clause.name_list.search(searcher));
            }
        }
        NotFound
    }
}

impl Search for AnyDesignUnit {
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        delegate_any!(self, unit, unit.search(searcher))
    }
}

impl Search for AnyPrimaryUnit {
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        delegate_primary!(self, unit, unit.search(searcher))
    }
}

impl Search for AnySecondaryUnit {
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        delegate_secondary!(self, unit, unit.search(searcher))
    }
}

impl Search for EntityDeclaration {
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
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
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        return_if_finished!(searcher.search_source(self.source()));
        return_if_found!(self.context_clause.search(searcher));
        return_if_found!(searcher.search_ident_ref(&self.entity_name).or_not_found());
        return_if_found!(self.decl.search(searcher));
        self.statements.search(searcher)
    }
}

impl Search for PackageDeclaration {
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
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
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        return_if_finished!(searcher.search_source(self.source()));
        return_if_found!(self.context_clause.search(searcher));
        return_if_found!(searcher.search_ident_ref(&self.ident).or_not_found());
        self.decl.search(searcher)
    }
}

impl Search for PackageInstantiation {
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        return_if_finished!(searcher.search_source(self.source()));
        return_if_found!(self.context_clause.search(searcher));
        return_if_found!(searcher
            .search_decl(FoundDeclaration::PackageInstance(self))
            .or_not_found());
        self.package_name.search(searcher)
    }
}

impl Search for ConfigurationDeclaration {
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        return_if_finished!(searcher.search_source(self.source()));
        return_if_found!(self.context_clause.search(searcher));
        return_if_found!(searcher
            .search_decl(FoundDeclaration::Configuration(self))
            .or_not_found());
        self.entity_name.search(searcher)
    }
}

impl Search for ContextDeclaration {
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        return_if_finished!(searcher.search_source(self.source()));
        return_if_found!(searcher
            .search_decl(FoundDeclaration::Context(self))
            .or_not_found());
        self.items.search(searcher)
    }
}

impl Search for CaseStatement {
    fn search(&self, searcher: &mut impl Searcher) -> SearchResult {
        let CaseStatement {
            is_matching: _,
            expression,
            alternatives,
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
    result: Option<Arc<NamedEntity>>,
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

    pub fn search(
        searchable: &impl Search,
        source: &Source,
        cursor: Position,
    ) -> Option<Arc<NamedEntity>> {
        let mut searcher = Self::new(source, cursor);
        let _ = searchable.search(&mut searcher);
        searcher.result
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
        if self.is_inside(pos) {
            if let Some(ent) = decl.named_entity() {
                self.result = Some(ent.clone());
                Finished(Found)
            } else {
                Finished(NotFound)
            }
        } else {
            NotFinished
        }
    }

    fn search_pos_with_ref(&mut self, pos: &SrcPos, reference: &Reference) -> SearchState {
        if self.is_inside(pos) {
            if let Some(ent) = reference {
                self.result = Some(ent.clone());
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
        if source == &self.source {
            NotFinished
        } else {
            Finished(NotFound)
        }
    }
}

// Search for a declaration/definition and format it
pub struct FormatDeclaration {
    ent: Arc<NamedEntity>,
    result: Option<String>,
}

impl FormatDeclaration {
    pub fn new(ent: Arc<NamedEntity>) -> FormatDeclaration {
        FormatDeclaration { ent, result: None }
    }

    pub fn search(searchable: &impl Search, ent: Arc<NamedEntity>) -> Option<String> {
        let mut searcher = Self::new(ent);
        let _ = searchable.search(&mut searcher);
        searcher.result
    }
}

impl Searcher for FormatDeclaration {
    fn search_decl(&mut self, decl: FoundDeclaration) -> SearchState {
        let ent = if let Some(ent) = decl.named_entity() {
            ent
        } else {
            return NotFinished;
        };

        if let Some(ref implicit_of) = self.ent.implicit_of {
            // Implicit
            if implicit_of.id() == ent.id() {
                self.result = Some(format!(
                    "-- {}\n\n-- Implicitly defined by:\n{}\n",
                    self.ent.describe(),
                    decl,
                ));
                return Finished(Found);
            }
        } else if self.ent.id() == ent.id() {
            // Explicit
            self.result = Some(decl.to_string());
            return Finished(Found);
        }
        NotFinished
    }
}

// Search for all references to declaration/definition
pub struct FindAllReferences {
    ent: Arc<NamedEntity>,
    references: Vec<SrcPos>,
}

impl FindAllReferences {
    pub fn new(ent: Arc<NamedEntity>) -> FindAllReferences {
        FindAllReferences {
            ent,
            references: Vec::new(),
        }
    }

    pub fn search(searchable: &impl Search, ent: Arc<NamedEntity>) -> Vec<SrcPos> {
        let mut searcher = Self::new(ent);
        let _ = searchable.search(&mut searcher);
        searcher.references
    }
}

impl Searcher for FindAllReferences {
    fn search_decl(&mut self, decl: FoundDeclaration) -> SearchState {
        if let Some(ent) = decl.named_entity() {
            if self.ent.id() == ent.id() {
                self.references.push(decl.pos().clone());
            }
        }
        NotFinished
    }

    fn search_pos_with_ref(&mut self, pos: &SrcPos, reference: &Reference) -> SearchState {
        if let Some(ent) = reference.as_ref() {
            if self.ent.id() == ent.id() {
                self.references.push(pos.clone());
            }
        };
        NotFinished
    }
}

impl<'a> HasNamedEntity for FoundDeclaration<'a> {
    fn named_entity(&self) -> Option<&Arc<NamedEntity>> {
        match self {
            FoundDeclaration::InterfaceObject(value) => value.ident.decl.as_ref(),
            FoundDeclaration::ForIndex(ident, _) => ident.decl.as_ref(),
            FoundDeclaration::ForGenerateIndex(_, value) => value.index_name.decl.as_ref(),
            FoundDeclaration::Library(_) => None,
            FoundDeclaration::Function(value) => value.designator.decl.as_ref(),
            FoundDeclaration::Procedure(value) => value.designator.decl.as_ref(),
            FoundDeclaration::Object(value) => value.ident.decl.as_ref(),
            FoundDeclaration::ElementDeclaration(elem) => elem.ident.decl.as_ref(),
            FoundDeclaration::EnumerationLiteral(_, elem) => elem.decl.as_ref(),
            FoundDeclaration::File(value) => value.ident.decl.as_ref(),
            FoundDeclaration::Type(value) => value.ident.decl.as_ref(),
            FoundDeclaration::Component(value) => value.ident.decl.as_ref(),
            FoundDeclaration::Alias(value) => value.designator.decl.as_ref(),
            FoundDeclaration::Package(value) => value.ident.decl.as_ref(),
            FoundDeclaration::PackageInstance(value) => value.ident.decl.as_ref(),
            FoundDeclaration::Configuration(value) => value.ident.decl.as_ref(),
            FoundDeclaration::Entity(value) => value.ident.decl.as_ref(),
            FoundDeclaration::Context(value) => value.ident.decl.as_ref(),
            FoundDeclaration::GenerateBody(value) => value.decl.as_ref(),
            FoundDeclaration::ConcurrentStatement(value) => value.decl.as_ref(),
            FoundDeclaration::SequentialStatement(value) => value.decl.as_ref(),
        }
    }
}

impl<'a> HasSrcPos for FoundDeclaration<'a> {
    fn pos(&self) -> &SrcPos {
        match self {
            FoundDeclaration::InterfaceObject(value) => value.ident.pos(),
            FoundDeclaration::ForIndex(ident, _) => ident.pos(),
            FoundDeclaration::ForGenerateIndex(_, value) => value.index_name.pos(),
            FoundDeclaration::Library(value) => value.pos(),
            FoundDeclaration::Function(value) => &value.designator.tree.pos,
            FoundDeclaration::Procedure(value) => &value.designator.tree.pos,
            FoundDeclaration::Object(value) => value.ident.pos(),
            FoundDeclaration::ElementDeclaration(elem) => elem.ident.pos(),
            FoundDeclaration::EnumerationLiteral(_, elem) => &elem.tree.pos,
            FoundDeclaration::File(value) => value.ident.pos(),
            FoundDeclaration::Type(value) => value.ident.pos(),
            FoundDeclaration::Component(value) => value.ident.pos(),
            FoundDeclaration::Alias(value) => &value.designator.tree.pos,
            FoundDeclaration::Package(value) => value.ident.pos(),
            FoundDeclaration::PackageInstance(value) => value.ident.pos(),
            FoundDeclaration::Configuration(value) => value.ident.pos(),
            FoundDeclaration::Entity(value) => value.ident.pos(),
            FoundDeclaration::Context(value) => value.ident.pos(),
            FoundDeclaration::GenerateBody(value) => value.pos(),
            FoundDeclaration::ConcurrentStatement(value) => value.pos(),
            FoundDeclaration::SequentialStatement(value) => value.pos(),
        }
    }
}

impl std::fmt::Display for FoundDeclaration<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FoundDeclaration::InterfaceObject(ref value) => match value.list_type {
                InterfaceListType::Port => write!(f, "port {};", value),
                InterfaceListType::Generic => write!(f, "generic {};", value),
                InterfaceListType::Parameter => write!(f, "{};", value),
            },
            FoundDeclaration::ForIndex(ref ident, ref drange) => {
                write!(f, "for {} in {} loop", ident, drange)
            }
            FoundDeclaration::ForGenerateIndex(ref ident, ref value) => match ident {
                Some(ident) => write!(f, "{}: {}", ident, value),
                None => write!(f, "{}", value),
            },
            FoundDeclaration::Library(ref value) => {
                write!(f, "library {};", value)
            }
            FoundDeclaration::Function(ref value) => {
                write!(f, "{};", value)
            }
            FoundDeclaration::Procedure(ref value) => {
                write!(f, "{};", value)
            }
            FoundDeclaration::Object(ref value) => {
                write!(f, "{}", value)
            }
            FoundDeclaration::ElementDeclaration(elem) => {
                write!(f, "{}", elem)
            }
            FoundDeclaration::EnumerationLiteral(ident, elem) => {
                write!(f, "{} : {}", elem, ident)
            }
            FoundDeclaration::File(ref value) => {
                write!(f, "{}", value)
            }
            FoundDeclaration::Type(ref value) => {
                write!(f, "{}", value)
            }
            FoundDeclaration::Component(ref value) => {
                write!(f, "{}", value)
            }
            FoundDeclaration::Alias(ref value) => {
                write!(f, "{}", value)
            }
            FoundDeclaration::Package(ref value) => {
                write!(f, "{}", value)
            }
            FoundDeclaration::PackageInstance(ref value) => {
                write!(f, "{}", value)
            }
            FoundDeclaration::Configuration(ref value) => {
                write!(f, "{}", value)
            }
            FoundDeclaration::Entity(ref value) => {
                write!(f, "{}", value)
            }
            FoundDeclaration::Context(ref value) => {
                write!(f, "{}", value)
            }
            FoundDeclaration::GenerateBody(value) => {
                write!(f, "{}", value)
            }
            FoundDeclaration::ConcurrentStatement(value) => {
                write!(f, "{}", value)
            }
            FoundDeclaration::SequentialStatement(value) => {
                write!(f, "{}", value)
            }
        }
    }
}
