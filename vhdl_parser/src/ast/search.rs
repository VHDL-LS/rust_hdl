// This Source Code Form is subject to the terms of the Mozilla Public
// Lic// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// This Source Code Form is subject to the terms of the Mozilla Public
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

use super::*;

#[must_use]
pub enum SearchResult<T> {
    Found(T),
    NotFound,
}

impl<T> Into<Option<T>> for SearchResult<T> {
    fn into(self) -> Option<T> {
        match self {
            Found(value) => Some(value),
            NotFound => None,
        }
    }
}

#[must_use]
pub enum SearchState<T> {
    Finished(SearchResult<T>),
    NotFinished,
}

pub use SearchResult::*;
pub use SearchState::*;

impl<T> SearchState<T> {
    fn or_else(self, nested_fun: impl FnOnce() -> SearchResult<T>) -> SearchResult<T> {
        match self {
            Finished(result) => result,
            NotFinished => nested_fun(),
        }
    }

    fn or_not_found(self) -> SearchResult<T> {
        self.or_else(|| NotFound)
    }
}

pub trait Searcher<T> {
    fn search_declaration(&mut self, _decl: &Declaration) -> SearchState<T> {
        NotFinished
    }
    fn search_interface_declaration(&mut self, _decl: &InterfaceDeclaration) -> SearchState<T> {
        NotFinished
    }
    /// Search an position that has a reference to a declaration
    fn search_pos_with_ref<U>(&mut self, _pos: &SrcPos, _ref: &WithRef<U>) -> SearchState<T> {
        NotFinished
    }

    /// Search a designator that has a reference to a declaration
    fn search_designator_ref(
        &mut self,
        pos: &SrcPos,
        designator: &WithRef<Designator>,
    ) -> SearchState<T> {
        self.search_pos_with_ref(pos, designator)
    }

    /// Search an identifier that has a reference to a declaration
    fn search_ident_ref(&mut self, ident: &WithRef<Ident>) -> SearchState<T> {
        self.search_pos_with_ref(&ident.item.pos, ident)
    }

    /// Search the position of a declaration of a named entity
    fn search_decl_pos(&mut self, _pos: &SrcPos) -> SearchState<T> {
        NotFinished
    }

    fn search_with_pos(&mut self, _pos: &SrcPos) -> SearchState<T> {
        NotFinished
    }
    fn search_source(&mut self, _source: &Source) -> SearchState<T> {
        NotFinished
    }
}

pub trait Search<T> {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T>;
}

#[macro_export]
macro_rules! return_if {
    ($result:expr) => {
        match $result {
            result @ Found(_) => {
                return result;
            }
            _ => {}
        };
    };
}

impl<T, V: Search<T>> Search<T> for Vec<V> {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        for decl in self.iter() {
            return_if!(decl.search(searcher));
        }
        NotFound
    }
}

impl<T, V: Search<T>> Search<T> for Option<V> {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        for decl in self.iter() {
            return_if!(decl.search(searcher));
        }
        NotFound
    }
}

fn search_conditionals<T, U: Search<T>>(
    conditionals: &Conditionals<U>,
    item_before_cond: bool,
    searcher: &mut impl Searcher<T>,
) -> SearchResult<T> {
    let Conditionals {
        conditionals,
        else_item,
    } = conditionals;
    for conditional in conditionals {
        let Conditional { condition, item } = conditional;
        if item_before_cond {
            // If
            return_if!(item.search(searcher));
            return_if!(condition.search(searcher));
        } else {
            // When
            return_if!(condition.search(searcher));
            return_if!(item.search(searcher));
        }
    }
    if let Some(expr) = else_item {
        return_if!(expr.search(searcher));
    }
    NotFound
}

fn search_alternatives<T, U: Search<T>>(
    alternatives: &Vec<Alternative<U>>,
    item_before_choice: bool,
    searcher: &mut impl Searcher<T>,
) -> SearchResult<T> {
    for alternative in alternatives.iter() {
        let Alternative { choices, item } = alternative;
        if item_before_choice {
            return_if!(item.search(searcher));
            return_if!(choices.search(searcher));
        } else {
            return_if!(choices.search(searcher));
            return_if!(item.search(searcher));
        }
    }
    NotFound
}

fn search_selection<T, U: Search<T>>(
    selection: &Selection<U>,
    item_before_cond: bool,
    searcher: &mut impl Searcher<T>,
) -> SearchResult<T> {
    let Selection {
        expression,
        alternatives,
    } = selection;
    return_if!(expression.search(searcher));
    return_if!(search_alternatives(
        alternatives,
        item_before_cond,
        searcher
    ));
    NotFound
}

fn search_assignment<T, U: Search<T>>(
    target: &WithPos<Target>,
    rhs: &AssignmentRightHand<U>,
    searcher: &mut impl Searcher<T>,
) -> SearchResult<T> {
    match rhs {
        AssignmentRightHand::Simple(item) => {
            return_if!(target.search(searcher));
            item.search(searcher)
        }
        AssignmentRightHand::Conditional(conditionals) => {
            return_if!(target.search(searcher));
            search_conditionals(conditionals, true, searcher)
        }
        AssignmentRightHand::Selected(selection) => {
            let Selection {
                expression,
                alternatives,
            } = selection;
            // expression comes before target
            return_if!(expression.search(searcher));
            return_if!(target.search(searcher));
            search_alternatives(alternatives, true, searcher)
        }
    }
}

impl<T> Search<T> for Choice {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        match self {
            Choice::DiscreteRange(ref drange) => {
                return_if!(drange.search(searcher));
            }
            Choice::Expression(ref expr) => {
                return_if!(expr.search(searcher));
            }
            Choice::Others => {}
        }
        NotFound
    }
}

impl<T> Search<T> for WithPos<Target> {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        match self.item {
            Target::Name(ref name) => {
                return_if!(search_pos_name(&self.pos, name, searcher));
            }
            Target::Aggregate(..) => {
                // @TODO
            }
        }
        NotFound
    }
}

impl<T> Search<T> for LabeledSequentialStatement {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        if let Some(ref label) = self.label {
            return_if!(searcher.search_decl_pos(label.pos()).or_not_found());
        }
        match self.statement {
            SequentialStatement::Return(ReturnStatement { ref expression }) => {
                return_if!(expression.search(searcher));
            }
            SequentialStatement::ProcedureCall(ref pcall) => {
                return_if!(pcall.search(searcher));
            }
            SequentialStatement::If(ref ifstmt) => {
                return_if!(search_conditionals(ifstmt, false, searcher));
            }
            SequentialStatement::Wait(ref wait_stmt) => {
                let WaitStatement {
                    sensitivity_clause,
                    condition_clause,
                    timeout_clause,
                } = wait_stmt;
                return_if!(sensitivity_clause.search(searcher));
                return_if!(condition_clause.search(searcher));
                return_if!(timeout_clause.search(searcher));
            }
            SequentialStatement::Assert(ref assert_stmt) => {
                let AssertStatement {
                    condition,
                    report,
                    severity,
                } = assert_stmt;
                return_if!(condition.search(searcher));
                return_if!(report.search(searcher));
                return_if!(severity.search(searcher));
            }
            SequentialStatement::Report(ref report_stmt) => {
                let ReportStatement { report, severity } = report_stmt;
                return_if!(report.search(searcher));
                return_if!(severity.search(searcher));
            }
            SequentialStatement::Exit(ref exit_stmt) => {
                // @TODO loop label
                let ExitStatement { condition, .. } = exit_stmt;
                return_if!(condition.search(searcher));
            }
            SequentialStatement::Next(ref next_stmt) => {
                // @TODO loop label
                let NextStatement { condition, .. } = next_stmt;
                return_if!(condition.search(searcher));
            }
            SequentialStatement::Case(ref case_stmt) => {
                return_if!(search_selection(case_stmt, false, searcher));
            }
            SequentialStatement::Loop(ref loop_stmt) => {
                let LoopStatement {
                    iteration_scheme,
                    statements,
                } = loop_stmt;
                match iteration_scheme {
                    Some(IterationScheme::For(ref index, ref drange)) => {
                        return_if!(searcher.search_decl_pos(index.pos()).or_not_found());
                        return_if!(drange.search(searcher));
                        return_if!(statements.search(searcher));
                    }
                    Some(IterationScheme::While(ref expr)) => {
                        return_if!(expr.search(searcher));
                        return_if!(statements.search(searcher));
                    }
                    None => {
                        return_if!(statements.search(searcher));
                    }
                }
            }
            SequentialStatement::SignalAssignment(ref assign) => {
                // @TODO more
                let SignalAssignment { target, rhs, .. } = assign;
                return_if!(search_assignment(target, rhs, searcher));
            }
            SequentialStatement::VariableAssignment(ref assign) => {
                let VariableAssignment { target, rhs } = assign;
                return_if!(search_assignment(target, rhs, searcher));
            }
            SequentialStatement::SignalForceAssignment(ref assign) => {
                let SignalForceAssignment {
                    target,
                    force_mode: _,
                    rhs,
                } = assign;
                return_if!(search_assignment(target, rhs, searcher));
            }
            SequentialStatement::SignalReleaseAssignment(ref assign) => {
                let SignalReleaseAssignment {
                    target,
                    force_mode: _,
                } = assign;
                return_if!(target.search(searcher));
            }
            SequentialStatement::Null => {}
        }
        NotFound
    }
}

impl<T> Search<T> for GenerateBody {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        let GenerateBody {
            alternative_label,
            decl,
            statements,
        } = self;
        if let Some(ref label) = alternative_label {
            return_if!(searcher.search_decl_pos(label.pos()).or_not_found());
        }
        return_if!(decl.search(searcher));
        statements.search(searcher)
    }
}

impl<T> Search<T> for InstantiationStatement {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        match self.unit {
            InstantiatedUnit::Entity(ref ent_name, _) => {
                return_if!(ent_name.search(searcher));
            }
            InstantiatedUnit::Component(ref component_name) => {
                return_if!(component_name.search(searcher));
            }
            InstantiatedUnit::Configuration(ref config_name) => {
                return_if!(config_name.search(searcher));
            }
        };
        return_if!(self.generic_map.search(searcher));
        return_if!(self.port_map.search(searcher));

        NotFound
    }
}

impl<T> Search<T> for SensitivityList {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        match self {
            SensitivityList::Names(names) => names.search(searcher),
            SensitivityList::All => NotFound,
        }
    }
}

impl<T> Search<T> for LabeledConcurrentStatement {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        if let Some(ref label) = self.label {
            return_if!(searcher.search_decl_pos(label.pos()).or_not_found());
        }
        match self.statement {
            ConcurrentStatement::Block(ref block) => {
                // @TODO guard condition
                return_if!(block.decl.search(searcher));
                block.statements.search(searcher)
            }
            ConcurrentStatement::Process(ref process) => {
                let ProcessStatement {
                    postponed: _,
                    sensitivity_list,
                    decl,
                    statements,
                } = process;
                return_if!(sensitivity_list.search(searcher));
                return_if!(decl.search(searcher));
                statements.search(searcher)
            }
            ConcurrentStatement::ForGenerate(ref gen) => {
                let ForGenerateStatement {
                    index_name,
                    discrete_range,
                    body,
                } = gen;
                return_if!(searcher.search_decl_pos(index_name.pos()).or_not_found());
                return_if!(discrete_range.search(searcher));
                body.search(searcher)
            }
            ConcurrentStatement::IfGenerate(ref gen) => search_conditionals(gen, false, searcher),
            ConcurrentStatement::CaseGenerate(ref gen) => search_selection(gen, false, searcher),
            ConcurrentStatement::Instance(ref inst) => inst.search(searcher),
            ConcurrentStatement::Assignment(ref assign) => {
                let ConcurrentSignalAssignment { target, rhs, .. } = assign;
                return_if!(search_assignment(target, rhs, searcher));
                NotFound
            }
            ConcurrentStatement::ProcedureCall(ref pcall) => {
                let ConcurrentProcedureCall {
                    postponed: _postponed,
                    call,
                } = pcall;
                return_if!(call.search(searcher));
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
                return_if!(condition.search(searcher));
                return_if!(report.search(searcher));
                return_if!(severity.search(searcher));
                NotFound
            }
        }
    }
}

impl<T> Search<T> for WithPos<WithRef<Designator>> {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        searcher.search_with_pos(&self.pos).or_else(|| {
            searcher
                .search_designator_ref(&self.pos, &self.item)
                .or_not_found()
        })
    }
}

impl<T> Search<T> for WithPos<SelectedName> {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        searcher
            .search_with_pos(&self.pos)
            .or_else(|| match self.item {
                SelectedName::Selected(ref prefix, ref designator) => {
                    return_if!(prefix.search(searcher));
                    return_if!(designator.search(searcher));
                    NotFound
                }
                SelectedName::Designator(ref designator) => searcher
                    .search_designator_ref(&self.pos, designator)
                    .or_not_found(),
            })
    }
}

fn search_pos_name<T>(
    pos: &SrcPos,
    name: &Name,
    searcher: &mut impl Searcher<T>,
) -> SearchResult<T> {
    match name {
        Name::Selected(ref prefix, ref designator) => {
            return_if!(prefix.search(searcher));
            return_if!(designator.search(searcher));
            NotFound
        }
        Name::SelectedAll(ref prefix) => {
            return_if!(prefix.search(searcher));
            NotFound
        }
        Name::Designator(ref designator) => searcher
            .search_designator_ref(&pos, designator)
            .or_not_found(),
        Name::Indexed(ref prefix, ..) => {
            return_if!(prefix.search(searcher));
            NotFound
        }
        Name::Slice(ref prefix, ref dranges) => {
            return_if!(prefix.search(searcher));
            return_if!(dranges.search(searcher));
            NotFound
        }
        Name::FunctionCall(ref fcall) => fcall.search(searcher),
        Name::Attribute(ref attr) => {
            // @TODO more
            let AttributeName { name, expr, .. } = attr.as_ref();
            return_if!(name.search(searcher));
            if let Some(expr) = expr {
                return_if!(expr.search(searcher));
            }
            NotFound
        }

        // @TODO more
        _ => NotFound,
    }
}

impl<T> Search<T> for WithPos<Name> {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        return_if!(searcher.search_with_pos(&self.pos).or_not_found());
        search_pos_name(&self.pos, &self.item, searcher)
    }
}

impl<T> Search<T> for ElementConstraint {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        // @TODO more
        let ElementConstraint { constraint, .. } = self;
        constraint.search(searcher)
    }
}

impl<T> Search<T> for WithPos<ElementConstraint> {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        return_if!(searcher.search_with_pos(&self.pos).or_not_found());
        self.item.search(searcher)
    }
}

impl<T> Search<T> for WithPos<SubtypeConstraint> {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        return_if!(searcher.search_with_pos(&self.pos).or_not_found());

        match self.item {
            SubtypeConstraint::Array(ref dranges, ref constraint) => {
                return_if!(dranges.search(searcher));
                if let Some(ref constraint) = constraint {
                    return_if!(constraint.search(searcher));
                }
            }
            SubtypeConstraint::Range(ref range) => {
                return_if!(range.search(searcher));
            }
            SubtypeConstraint::Record(ref constraints) => {
                return_if!(constraints.search(searcher));
            }
        }
        NotFound
    }
}

impl<T> Search<T> for SubtypeIndication {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        // @TODO more
        let SubtypeIndication {
            type_mark,
            constraint,
            ..
        } = self;
        return_if!(type_mark.search(searcher));
        return_if!(constraint.search(searcher));
        NotFound
    }
}

impl<T> Search<T> for RangeConstraint {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        let RangeConstraint {
            direction: _,
            left_expr,
            right_expr,
        } = self;
        return_if!(left_expr.search(searcher));
        return_if!(right_expr.search(searcher));
        NotFound
    }
}

impl<T> Search<T> for AttributeName {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        // @TODO more
        let AttributeName { name, .. } = self;
        name.search(searcher)
    }
}

impl<T> Search<T> for Range {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        match self {
            Range::Range(constraint) => {
                return_if!(constraint.search(searcher));
            }
            Range::Attribute(attr) => {
                return_if!(attr.search(searcher));
            }
        }
        NotFound
    }
}

impl<T> Search<T> for DiscreteRange {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        match self {
            DiscreteRange::Discrete(ref type_mark, ref constraint) => {
                return_if!(type_mark.search(searcher));
                constraint.search(searcher)
            }
            DiscreteRange::Range(ref constraint) => constraint.search(searcher),
        }
    }
}

impl<T> Search<T> for TypeDeclaration {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        return_if!(searcher.search_decl_pos(self.ident.pos()).or_not_found());

        match self.def {
            TypeDefinition::ProtectedBody(ref body) => {
                return_if!(body.decl.search(searcher));
            }
            TypeDefinition::Protected(ref prot_decl) => {
                for item in prot_decl.items.iter() {
                    match item {
                        ProtectedTypeDeclarativeItem::Subprogram(ref subprogram) => {
                            return_if!(subprogram.search(searcher));
                        }
                    }
                }
            }
            TypeDefinition::Record(ref element_decls) => {
                for elem in element_decls {
                    return_if!(elem.subtype.search(searcher));
                }
            }
            TypeDefinition::Access(ref subtype_indication) => {
                return_if!(subtype_indication.search(searcher));
            }
            TypeDefinition::Array(ref indexes, ref subtype_indication) => {
                for index in indexes.iter() {
                    match index {
                        ArrayIndex::IndexSubtypeDefintion(ref type_mark) => {
                            return_if!(type_mark.search(searcher));
                        }
                        ArrayIndex::Discrete(ref drange) => {
                            return_if!(drange.search(searcher));
                        }
                    }
                }
                return_if!(subtype_indication.search(searcher));
            }
            TypeDefinition::Subtype(ref subtype_indication) => {
                return_if!(subtype_indication.search(searcher));
            }

            _ => {}
        }
        NotFound
    }
}

fn search_pos_expr<T>(
    pos: &SrcPos,
    expr: &Expression,
    searcher: &mut impl Searcher<T>,
) -> SearchResult<T> {
    return_if!(searcher.search_with_pos(pos).or_not_found());
    match expr {
        Expression::Binary(_, ref left, ref right) => {
            return_if!(left.search(searcher));
            right.search(searcher)
        }
        Expression::Unary(_, ref expr) => expr.search(searcher),
        Expression::Name(ref name) => search_pos_name(pos, &name, searcher),
        Expression::Aggregate(ref assocs) => {
            for assoc in assocs.iter() {
                match assoc {
                    ElementAssociation::Named(ref choices, ref expr) => {
                        return_if!(choices.search(searcher));
                        return_if!(expr.search(searcher));
                    }
                    ElementAssociation::Positional(ref expr) => {
                        return_if!(expr.search(searcher));
                    }
                }
            }
            NotFound
        }
        Expression::Qualified(ref qexpr) => qexpr.search(searcher),
        Expression::New(ref alloc) => {
            return_if!(searcher.search_with_pos(&alloc.pos).or_not_found());
            match alloc.item {
                Allocator::Qualified(ref qexpr) => qexpr.search(searcher),
                Allocator::Subtype(ref subtype) => subtype.search(searcher),
            }
        }
        Expression::Literal(_) => NotFound,
    }
}

impl<T> Search<T> for QualifiedExpression {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        let QualifiedExpression { name, expr } = self;
        return_if!(name.search(searcher));
        return_if!(expr.search(searcher));
        NotFound
    }
}

impl<T> Search<T> for AssociationElement {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        // @TODO more formal
        let AssociationElement { actual, .. } = self;
        match actual.item {
            ActualPart::Expression(ref expr) => {
                return_if!(search_pos_expr(&actual.pos, expr, searcher));
            }
            ActualPart::Open => {}
        }
        NotFound
    }
}

impl<T> Search<T> for FunctionCall {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        let FunctionCall { name, parameters } = self;
        return_if!(name.search(searcher));
        return_if!(parameters.search(searcher));
        NotFound
    }
}

impl<T> Search<T> for Waveform {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        match self {
            Waveform::Elements(ref elems) => {
                for elem in elems.iter() {
                    let WaveformElement { value, after } = elem;
                    return_if!(value.search(searcher));
                    return_if!(after.search(searcher));
                }
            }
            Waveform::Unaffected => {}
        }
        NotFound
    }
}

impl<T> Search<T> for WithPos<Expression> {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        search_pos_expr(&self.pos, &self.item, searcher)
    }
}

impl<T> Search<T> for ObjectDeclaration {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        return_if!(searcher.search_decl_pos(self.ident.pos()).or_not_found());
        return_if!(self.subtype_indication.search(searcher));
        if let Some(ref expr) = self.expression {
            expr.search(searcher)
        } else {
            NotFound
        }
    }
}
impl<T> Search<T> for Signature {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        match self {
            Signature::Function(args, ret) => {
                return_if!(args.search(searcher));
                return_if!(ret.search(searcher));
            }
            Signature::Procedure(args) => {
                return_if!(args.search(searcher));
            }
        }
        NotFound
    }
}

impl<T> Search<T> for Declaration {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        searcher.search_declaration(self).or_else(|| {
            match self {
                Declaration::Object(object) => {
                    return_if!(object.search(searcher));
                }
                Declaration::Type(typ) => return_if!(typ.search(searcher)),
                Declaration::SubprogramBody(body) => {
                    return_if!(body.specification.search(searcher));
                    return_if!(body.declarations.search(searcher));
                    return_if!(body.statements.search(searcher));
                }
                Declaration::SubprogramDeclaration(decl) => {
                    return_if!(decl.search(searcher));
                }
                Declaration::Attribute(Attribute::Declaration(decl)) => {
                    return_if!(decl.type_mark.search(searcher));
                }
                Declaration::Alias(alias) => {
                    let AliasDeclaration {
                        designator,
                        subtype_indication,
                        name,
                        signature,
                    } = alias;
                    return_if!(searcher.search_decl_pos(&designator.pos).or_not_found());
                    return_if!(subtype_indication.search(searcher));
                    return_if!(name.search(searcher));
                    return_if!(signature.search(searcher));
                }
                Declaration::Use(use_clause) => return_if!(searcher
                    .search_with_pos(&use_clause.pos)
                    .or_else(|| use_clause.item.name_list.search(searcher))),
                Declaration::Component(component) => {
                    let ComponentDeclaration {
                        ident,
                        generic_list,
                        port_list,
                    } = component;
                    return_if!(searcher.search_decl_pos(ident.pos()).or_not_found());
                    return_if!(generic_list.search(searcher));
                    return_if!(port_list.search(searcher));
                }

                // @TODO more
                _ => {}
            }
            NotFound
        })
    }
}

impl<T> Search<T> for InterfaceDeclaration {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        searcher.search_interface_declaration(self).or_else(|| {
            match self {
                InterfaceDeclaration::Object(ref decl) => {
                    return_if!(searcher.search_decl_pos(decl.ident.pos()).or_not_found());
                    return_if!(decl.subtype_indication.search(searcher));
                    return_if!(decl.expression.search(searcher));
                }
                InterfaceDeclaration::Subprogram(ref decl, _) => {
                    return_if!(decl.search(searcher));
                }
                _ => {}
            };
            NotFound
        })
    }
}

impl<T> Search<T> for SubprogramDeclaration {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        match self {
            SubprogramDeclaration::Function(ref decl) => return_if!(decl.search(searcher)),
            SubprogramDeclaration::Procedure(ref decl) => return_if!(decl.search(searcher)),
        }
        NotFound
    }
}

impl<T> Search<T> for ProcedureSpecification {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        return_if!(searcher
            .search_decl_pos(&self.designator.pos)
            .or_not_found());
        self.parameter_list.search(searcher)
    }
}

impl<T> Search<T> for FunctionSpecification {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        return_if!(searcher
            .search_decl_pos(&self.designator.pos)
            .or_not_found());
        return_if!(self.parameter_list.search(searcher));
        self.return_type.search(searcher)
    }
}

impl<T> Search<T> for LibraryClause {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        for name in self.name_list.iter() {
            return_if!(searcher.search_decl_pos(name.pos()).or_not_found());
        }
        NotFound
    }
}

impl<T> Search<T> for WithPos<ContextItem> {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        searcher.search_with_pos(&self.pos).or_else(|| {
            match self.item {
                ContextItem::Use(ref use_clause) => {
                    return_if!(use_clause.name_list.search(searcher))
                }
                ContextItem::Library(ref library_clause) => {
                    return_if!(library_clause.search(searcher))
                }
                ContextItem::Context(ref context_clause) => {
                    return_if!(context_clause.name_list.search(searcher))
                }
            }
            NotFound
        })
    }
}

impl<T> Search<T> for EntityUnit {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        searcher.search_source(self.source()).or_else(|| {
            return_if!(self.context_clause.search(searcher));
            return_if!(searcher.search_decl_pos(self.ident().pos()).or_not_found());
            return_if!(self.unit.generic_clause.search(searcher));
            return_if!(self.unit.port_clause.search(searcher));
            return_if!(self.unit.decl.search(searcher));
            self.unit.statements.search(searcher)
        })
    }
}

impl<T> Search<T> for ArchitectureUnit {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        searcher.search_source(self.source()).or_else(|| {
            return_if!(self.context_clause.search(searcher));
            return_if!(searcher
                .search_ident_ref(&self.unit.entity_name)
                .or_not_found());
            return_if!(self.unit.decl.search(searcher));
            self.unit.statements.search(searcher)
        })
    }
}

impl<T> Search<T> for PackageUnit {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        searcher.search_source(self.source()).or_else(|| {
            return_if!(self.context_clause.search(searcher));
            return_if!(searcher.search_decl_pos(self.ident().pos()).or_not_found());
            return_if!(self.unit.generic_clause.search(searcher));
            self.unit.decl.search(searcher)
        })
    }
}

impl<T> Search<T> for PackageBodyUnit {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        searcher.search_source(self.source()).or_else(|| {
            return_if!(searcher.search_ident_ref(&self.unit.ident).or_not_found());
            return_if!(self.context_clause.search(searcher));
            self.unit.decl.search(searcher)
        })
    }
}

impl<T> Search<T> for PackageInstanceUnit {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        searcher.search_source(self.source()).or_else(|| {
            return_if!(self.context_clause.search(searcher));
            return_if!(searcher.search_decl_pos(self.ident().pos()).or_not_found());
            self.unit.package_name.search(searcher)
        })
    }
}

impl<T> Search<T> for ConfigurationUnit {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        searcher.search_source(self.source()).or_else(|| {
            return_if!(searcher.search_decl_pos(self.ident().pos()).or_not_found());
            return_if!(self.context_clause.search(searcher));
            self.unit.entity_name.search(searcher)
        })
    }
}

impl<T> Search<T> for ContextDeclaration {
    fn search(&self, searcher: &mut impl Searcher<T>) -> SearchResult<T> {
        searcher.search_source(self.source()).or_else(|| {
            return_if!(searcher.search_decl_pos(self.ident().pos()).or_not_found());
            self.items.search(searcher)
        })
    }
}

// Search for reference to declaration/definition at cursor
pub struct ItemAtCursor {
    source: Source,
    cursor: Position,
}

impl ItemAtCursor {
    pub fn new(source: &Source, cursor: Position) -> ItemAtCursor {
        ItemAtCursor {
            source: source.clone(),
            cursor,
        }
    }

    fn is_inside(&self, pos: &SrcPos) -> bool {
        // cursor is the gap between character cursor and cursor + 1
        // Thus cursor will match character cursor and cursor + 1
        pos.start() <= self.cursor && self.cursor <= pos.end()
    }
}

impl Searcher<SrcPos> for ItemAtCursor {
    fn search_with_pos(&mut self, pos: &SrcPos) -> SearchState<SrcPos> {
        // cursor is the gap between character cursor and cursor + 1
        // Thus cursor will match character cursor and cursor + 1
        if self.is_inside(pos) {
            NotFinished
        } else {
            Finished(NotFound)
        }
    }

    fn search_decl_pos(&mut self, pos: &SrcPos) -> SearchState<SrcPos> {
        if self.is_inside(pos) {
            Finished(Found(pos.clone()))
        } else {
            NotFinished
        }
    }

    fn search_pos_with_ref<U>(
        &mut self,
        pos: &SrcPos,
        with_ref: &WithRef<U>,
    ) -> SearchState<SrcPos> {
        if !self.is_inside(pos) {
            Finished(NotFound)
        } else if let Some(ref reference) = with_ref.reference {
            Finished(Found(reference.clone()))
        } else {
            Finished(NotFound)
        }
    }

    // Assume source is searched first to filter out design units in other files
    fn search_source(&mut self, source: &Source) -> SearchState<SrcPos> {
        if source == &self.source {
            NotFinished
        } else {
            Finished(NotFound)
        }
    }
}

// Search for all reference to declaration/defintion
pub struct FindAllReferences {
    decl_pos: SrcPos,
    references: Vec<SrcPos>,
}

impl FindAllReferences {
    pub fn new(decl_pos: &SrcPos) -> FindAllReferences {
        FindAllReferences {
            decl_pos: decl_pos.clone(),
            references: Vec::new(),
        }
    }

    pub fn search(mut self, searchable: &impl Search<()>) -> Vec<SrcPos> {
        let _unnused = searchable.search(&mut self);
        self.references
    }
}

impl Searcher<()> for FindAllReferences {
    fn search_decl_pos(&mut self, decl_pos: &SrcPos) -> SearchState<()> {
        if decl_pos == &self.decl_pos {
            self.references.push(decl_pos.clone());
        }
        NotFinished
    }

    fn search_pos_with_ref<U>(&mut self, pos: &SrcPos, with_ref: &WithRef<U>) -> SearchState<()> {
        if let Some(ref reference) = with_ref.reference {
            if reference == &self.decl_pos {
                self.references.push(pos.clone());
            }
        };
        NotFinished
    }

    fn search_ident_ref(&mut self, ident: &WithRef<Ident>) -> SearchState<()> {
        if let Some(ref reference) = ident.reference {
            if reference == &self.decl_pos {
                self.references.push(ident.item.pos().clone());
            }
        };
        NotFinished
    }
}
