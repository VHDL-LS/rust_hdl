// This Source Code Form is subject to the terms of the Mozilla Public
// Lic// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// This Source Code Form is subject to the terms of the Mozilla Public
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

use super::*;
use crate::analysis::DesignRoot;
use crate::named_entity::{EntRef, HasEntityId, Reference};
use crate::syntax::{HasTokenSpan, TokenAccess};

#[must_use]
#[derive(PartialEq, Debug)]
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
pub enum DeclarationItem<'a> {
    Object(&'a ObjectDeclaration),
    ElementDeclaration(&'a ElementDeclaration),
    EnumerationLiteral(&'a Ident, &'a WithDecl<WithToken<EnumerationLiteral>>),
    InterfaceObject(&'a InterfaceObjectDeclaration),
    InterfaceFile(&'a InterfaceFileDeclaration),
    File(&'a FileDeclaration),
    Type(&'a TypeDeclaration),
    InterfaceType(&'a WithDecl<Ident>),
    InterfacePackage(&'a InterfacePackageDeclaration),
    PhysicalTypePrimary(&'a WithDecl<Ident>),
    PhysicalTypeSecondary(&'a WithDecl<Ident>, &'a PhysicalLiteral),
    Component(&'a ComponentDeclaration),
    Attribute(&'a AttributeDeclaration),
    Alias(&'a AliasDeclaration),
    SubprogramDecl(&'a SubprogramSpecification),
    Subprogram(&'a SubprogramBody),
    SubprogramInstantiation(&'a SubprogramInstantiation),
    Package(&'a PackageDeclaration),
    PackageBody(&'a PackageBody),
    PackageInstance(&'a PackageInstantiation),
    Configuration(&'a ConfigurationDeclaration),
    Entity(&'a EntityDeclaration),
    Architecture(&'a ArchitectureBody),
    Context(&'a ContextDeclaration),
    ForIndex(&'a WithDecl<Ident>, &'a DiscreteRange),
    ForGenerateIndex(Option<&'a Ident>, &'a ForGenerateStatement),
    GenerateBody(&'a WithDecl<Ident>),
    ConcurrentStatement(&'a LabeledConcurrentStatement),
    SequentialStatement(&'a LabeledSequentialStatement),
    View(&'a ModeViewDeclaration),
}

pub struct FoundDeclaration<'a> {
    pub reference: &'a Reference,
    pub ast: DeclarationItem<'a>,
}

impl<'a> FoundDeclaration<'a> {
    pub fn new(reference: &'a Reference, ast: DeclarationItem<'a>) -> FoundDeclaration<'a> {
        FoundDeclaration { reference, ast }
    }
}

pub trait Searcher {
    /// Search an position that has a reference to a declaration
    fn search_pos_with_ref(
        &mut self,
        _ctx: &dyn TokenAccess,
        _pos: &SrcPos,
        _ref: &Reference,
    ) -> SearchState {
        NotFinished
    }

    /// Search a designator that has a reference to a declaration
    fn search_designator_ref(
        &mut self,
        ctx: &dyn TokenAccess,
        pos: &SrcPos,
        designator: &WithRef<Designator>,
    ) -> SearchState {
        self.search_pos_with_ref(ctx, pos, &designator.reference)
    }

    /// Search an identifier that has a reference to a declaration
    fn search_ident_ref(&mut self, ctx: &dyn TokenAccess, ident: &WithRef<Ident>) -> SearchState {
        self.search_pos_with_ref(ctx, ident.item.pos(ctx), &ident.reference)
    }

    /// Search a declaration of a named entity
    fn search_decl(&mut self, _ctx: &dyn TokenAccess, _decl: FoundDeclaration<'_>) -> SearchState {
        NotFinished
    }

    fn search_with_pos(&mut self, _ctx: &dyn TokenAccess, _pos: &SrcPos) -> SearchState {
        NotFinished
    }
}

pub trait Search {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult;
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
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        for decl in self.iter() {
            return_if_found!(decl.search(ctx, searcher));
        }
        NotFound
    }
}

impl<T: Search> Search for Option<T> {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        for decl in self.iter() {
            return_if_found!(decl.search(ctx, searcher));
        }
        NotFound
    }
}

fn search_conditionals<T: Search>(
    conditionals: &Conditionals<T>,
    item_before_cond: bool,
    searcher: &mut impl Searcher,
    ctx: &dyn TokenAccess,
) -> SearchResult {
    let Conditionals {
        conditionals,
        else_item,
    } = conditionals;
    for conditional in conditionals {
        let Conditional { condition, item } = conditional;
        if item_before_cond {
            // If
            return_if_found!(item.search(ctx, searcher));
            return_if_found!(condition.search(ctx, searcher));
        } else {
            // When
            return_if_found!(condition.search(ctx, searcher));
            return_if_found!(item.search(ctx, searcher));
        }
    }
    if let Some((expr, _)) = else_item {
        return_if_found!(expr.search(ctx, searcher));
    }
    NotFound
}

fn search_alternatives<T: Search>(
    alternatives: &[Alternative<T>],
    item_before_choice: bool,
    searcher: &mut impl Searcher,
    ctx: &dyn TokenAccess,
) -> SearchResult {
    for alternative in alternatives.iter() {
        let Alternative {
            choices,
            item,
            span: _,
        } = &alternative;
        if item_before_choice {
            return_if_found!(item.search(ctx, searcher));
            return_if_found!(choices.search(ctx, searcher));
        } else {
            return_if_found!(choices.search(ctx, searcher));
            return_if_found!(item.search(ctx, searcher));
        }
    }
    NotFound
}

fn search_selection<T: Search>(
    selection: &Selection<T>,
    item_before_cond: bool,
    searcher: &mut impl Searcher,
    ctx: &dyn TokenAccess,
) -> SearchResult {
    let Selection {
        expression,
        alternatives,
    } = selection;
    return_if_found!(expression.search(ctx, searcher));
    return_if_found!(search_alternatives(
        alternatives,
        item_before_cond,
        searcher,
        ctx,
    ));

    NotFound
}

fn search_assignment<T: Search>(
    target: &WithTokenSpan<Target>,
    rhs: &AssignmentRightHand<T>,
    searcher: &mut impl Searcher,
    ctx: &dyn TokenAccess,
) -> SearchResult {
    match rhs {
        AssignmentRightHand::Simple(item) => {
            return_if_found!(target.search(ctx, searcher));
            item.search(ctx, searcher)
        }
        AssignmentRightHand::Conditional(conditionals) => {
            return_if_found!(target.search(ctx, searcher));
            search_conditionals(conditionals, true, searcher, ctx)
        }
        AssignmentRightHand::Selected(selection) => {
            let Selection {
                expression,
                alternatives,
            } = selection;
            // expression comes before target
            return_if_found!(expression.search(ctx, searcher));
            return_if_found!(target.search(ctx, searcher));
            search_alternatives(alternatives, true, searcher, ctx)
        }
    }
}

impl Search for WithTokenSpan<Choice> {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        return_if_finished!(searcher.search_with_pos(ctx, &self.pos(ctx)));

        match self.item {
            Choice::DiscreteRange(ref drange) => {
                return_if_found!(drange.search(ctx, searcher));
            }
            Choice::Expression(ref expr) => {
                return_if_found!(search_pos_expr(ctx, &self.pos(ctx), expr, searcher));
            }
            Choice::Others => {}
        }
        NotFound
    }
}

impl Search for WithTokenSpan<ElementAssociation> {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        self.item.search(ctx, searcher)
    }
}

impl Search for WithTokenSpan<Target> {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        match self.item {
            Target::Name(ref name) => search_pos_name(&self.pos(ctx), name, searcher, ctx),
            Target::Aggregate(ref assocs) => assocs.search(ctx, searcher),
        }
    }
}

impl<T: Search> Search for SeparatedList<T> {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        for item in self.items.iter() {
            return_if_found!(item.search(ctx, searcher));
        }
        NotFound
    }
}

impl Search for LabeledSequentialStatement {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        return_if_found!(searcher
            .search_decl(
                ctx,
                FoundDeclaration::new(&self.label.decl, DeclarationItem::SequentialStatement(self))
            )
            .or_not_found());
        match self.statement.item {
            SequentialStatement::Return(ref ret) => {
                let ReturnStatement { ref expression } = ret;
                return_if_found!(expression.search(ctx, searcher));
            }
            SequentialStatement::ProcedureCall(ref pcall) => {
                return_if_finished!(searcher.search_with_pos(ctx, &pcall.pos(ctx)));
                return_if_found!(pcall.item.search(ctx, searcher));
            }
            SequentialStatement::If(ref ifstmt) => {
                return_if_found!(search_conditionals(&ifstmt.conds, false, searcher, ctx));
            }
            SequentialStatement::Wait(ref wait_stmt) => {
                let WaitStatement {
                    sensitivity_clause,
                    condition_clause,
                    timeout_clause,
                } = wait_stmt;
                return_if_found!(sensitivity_clause.search(ctx, searcher));
                return_if_found!(condition_clause.search(ctx, searcher));
                return_if_found!(timeout_clause.search(ctx, searcher));
            }
            SequentialStatement::Assert(ref assert_stmt) => {
                let AssertStatement {
                    condition,
                    report,
                    severity,
                } = assert_stmt;
                return_if_found!(condition.search(ctx, searcher));
                return_if_found!(report.search(ctx, searcher));
                return_if_found!(severity.search(ctx, searcher));
            }
            SequentialStatement::Report(ref report_stmt) => {
                let ReportStatement { report, severity } = report_stmt;
                return_if_found!(report.search(ctx, searcher));
                return_if_found!(severity.search(ctx, searcher));
            }
            SequentialStatement::Exit(ref exit_stmt) => {
                let ExitStatement {
                    condition,
                    loop_label,
                } = exit_stmt;
                if let Some(loop_label) = loop_label {
                    return_if_found!(searcher
                        .search_pos_with_ref(ctx, loop_label.item.pos(ctx), &loop_label.reference)
                        .or_not_found());
                }
                return_if_found!(condition.search(ctx, searcher));
            }
            SequentialStatement::Next(ref next_stmt) => {
                let NextStatement {
                    condition,
                    loop_label,
                } = next_stmt;
                if let Some(loop_label) = loop_label {
                    return_if_found!(searcher
                        .search_pos_with_ref(ctx, loop_label.item.pos(ctx), &loop_label.reference)
                        .or_not_found());
                }
                return_if_found!(condition.search(ctx, searcher));
            }
            SequentialStatement::Case(ref case_stmt) => {
                return_if_found!(case_stmt.search(ctx, searcher));
            }
            SequentialStatement::Loop(ref loop_stmt) => {
                let LoopStatement {
                    iteration_scheme,
                    statements,
                    ..
                } = loop_stmt;
                match iteration_scheme {
                    Some(IterationScheme::For(ref index, ref drange)) => {
                        return_if_found!(searcher
                            .search_decl(
                                ctx,
                                FoundDeclaration::new(
                                    &index.decl,
                                    DeclarationItem::ForIndex(index, drange)
                                )
                            )
                            .or_not_found());
                        return_if_found!(drange.search(ctx, searcher));
                        return_if_found!(statements.search(ctx, searcher));
                    }
                    Some(IterationScheme::While(ref expr)) => {
                        return_if_found!(expr.search(ctx, searcher));
                        return_if_found!(statements.search(ctx, searcher));
                    }
                    None => {
                        return_if_found!(statements.search(ctx, searcher));
                    }
                }
            }
            SequentialStatement::SignalAssignment(ref assign) => {
                // @TODO more
                let SignalAssignment { target, rhs, .. } = assign;
                return_if_found!(search_assignment(target, rhs, searcher, ctx));
            }
            SequentialStatement::VariableAssignment(ref assign) => {
                let VariableAssignment { target, rhs } = assign;
                return_if_found!(search_assignment(target, rhs, searcher, ctx));
            }
            SequentialStatement::SignalForceAssignment(ref assign) => {
                let SignalForceAssignment {
                    target,
                    force_mode: _,
                    rhs,
                } = assign;
                return_if_found!(search_assignment(target, rhs, searcher, ctx));
            }
            SequentialStatement::SignalReleaseAssignment(ref assign) => {
                let SignalReleaseAssignment {
                    target,
                    force_mode: _,
                    span: _,
                } = assign;
                return_if_found!(target.search(ctx, searcher));
            }
            SequentialStatement::Null => {}
        }

        if let Some(end_label_pos) = self.statement.item.end_label_pos() {
            return_if_found!(searcher
                .search_pos_with_ref(ctx, end_label_pos, &self.label.decl)
                .or_not_found());
        }

        NotFound
    }
}

impl Search for GenerateBody {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        let GenerateBody {
            alternative_label,
            decl,
            statements,
            end_label: end_label_pos,
            ..
        } = self;
        if let Some(ref label) = alternative_label {
            return_if_found!(searcher
                .search_decl(
                    ctx,
                    FoundDeclaration::new(&label.decl, DeclarationItem::GenerateBody(label))
                )
                .or_not_found());
        }
        if let Some((decl, _)) = decl {
            return_if_found!(decl.search(ctx, searcher));
        }
        return_if_found!(statements.search(ctx, searcher));

        if let Some(ref label) = alternative_label {
            if let Some(end_label_pos) = end_label_pos {
                return_if_found!(searcher
                    .search_pos_with_ref(ctx, ctx.get_pos(*end_label_pos), &label.decl)
                    .or_not_found());
            }
        }

        NotFound
    }
}

impl Search for InstantiationStatement {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        match self.unit {
            InstantiatedUnit::Entity(ref ent_name, ref architecture_name) => {
                return_if_found!(ent_name.search(ctx, searcher));
                if let Some(ref architecture_name) = architecture_name {
                    return_if_found!(searcher
                        .search_pos_with_ref(
                            ctx,
                            architecture_name.item.pos(ctx),
                            &architecture_name.reference
                        )
                        .or_not_found());
                }
            }
            InstantiatedUnit::Component(ref component_name) => {
                return_if_found!(component_name.search(ctx, searcher));
            }
            InstantiatedUnit::Configuration(ref config_name) => {
                return_if_found!(config_name.search(ctx, searcher));
            }
        };
        return_if_found!(self.generic_map.search(ctx, searcher));
        return_if_found!(self.port_map.search(ctx, searcher));

        NotFound
    }
}

impl Search for SensitivityList {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        match self {
            SensitivityList::Names(names) => names.search(ctx, searcher),
            SensitivityList::All => NotFound,
        }
    }
}

impl Search for LabeledConcurrentStatement {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        return_if_found!(searcher
            .search_decl(
                ctx,
                FoundDeclaration::new(&self.label.decl, DeclarationItem::ConcurrentStatement(self))
            )
            .or_not_found());
        match self.statement.item {
            ConcurrentStatement::Block(ref block) => {
                // @TODO guard condition
                return_if_found!(block.decl.search(ctx, searcher));
                return_if_found!(block.statements.search(ctx, searcher));
            }
            ConcurrentStatement::Process(ref process) => {
                let ProcessStatement {
                    postponed: _,
                    sensitivity_list,
                    decl,
                    statements,
                    end_label_pos: _,
                    ..
                } = process;
                if let Some(sensitivity_list) = sensitivity_list {
                    return_if_found!(sensitivity_list.item.search(ctx, searcher));
                }
                return_if_found!(decl.search(ctx, searcher));
                return_if_found!(statements.search(ctx, searcher));
            }
            ConcurrentStatement::ForGenerate(ref gen) => {
                return_if_found!(searcher
                    .search_decl(
                        ctx,
                        FoundDeclaration::new(
                            &gen.index_name.decl,
                            DeclarationItem::ForGenerateIndex(self.label.tree.as_ref(), gen)
                        )
                    )
                    .or_not_found());
                let ForGenerateStatement {
                    index_name: _,
                    discrete_range,
                    body,
                    end_label_pos: _,
                    ..
                } = gen;
                return_if_found!(discrete_range.search(ctx, searcher));
                return_if_found!(body.search(ctx, searcher));
            }
            ConcurrentStatement::IfGenerate(ref gen) => {
                return_if_found!(search_conditionals(&gen.conds, false, searcher, ctx));
            }
            ConcurrentStatement::CaseGenerate(ref gen) => {
                return_if_found!(search_selection(&gen.sels, false, searcher, ctx));
            }
            ConcurrentStatement::Instance(ref inst) => {
                return_if_found!(inst.search(ctx, searcher));
            }
            ConcurrentStatement::Assignment(ref assign) => {
                let ConcurrentSignalAssignment { assignment, .. } = assign;
                return_if_found!(search_assignment(
                    &assignment.target,
                    &assignment.rhs,
                    searcher,
                    ctx
                ));
            }
            ConcurrentStatement::ProcedureCall(ref pcall) => {
                let ConcurrentProcedureCall {
                    postponed: _postponed,
                    call,
                    ..
                } = pcall;
                return_if_finished!(searcher.search_with_pos(ctx, &call.pos(ctx)));
                return_if_found!(call.item.search(ctx, searcher));
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
                return_if_found!(condition.search(ctx, searcher));
                return_if_found!(report.search(ctx, searcher));
                return_if_found!(severity.search(ctx, searcher));
            }
        };

        if let Some(end_label_pos) = self.statement.item.end_label_pos() {
            return_if_found!(searcher
                .search_pos_with_ref(ctx, end_label_pos, &self.label.decl)
                .or_not_found());
        }

        NotFound
    }
}

impl Search for WithToken<WithRef<Designator>> {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        return_if_finished!(searcher.search_with_pos(ctx, self.pos(ctx)));
        searcher
            .search_designator_ref(ctx, self.pos(ctx), &self.item)
            .or_not_found()
    }
}

fn search_pos_name(
    pos: &SrcPos,
    name: &Name,
    searcher: &mut impl Searcher,
    ctx: &dyn TokenAccess,
) -> SearchResult {
    match name {
        Name::Selected(ref prefix, ref designator) => {
            return_if_found!(prefix.search(ctx, searcher));
            return_if_found!(designator.search(ctx, searcher));
            NotFound
        }
        Name::SelectedAll(ref prefix) => {
            return_if_found!(prefix.search(ctx, searcher));
            NotFound
        }
        Name::Designator(ref designator) => searcher
            .search_designator_ref(ctx, pos, designator)
            .or_not_found(),
        Name::Slice(ref prefix, ref dranges) => {
            return_if_found!(prefix.search(ctx, searcher));
            return_if_found!(dranges.search(ctx, searcher));
            NotFound
        }
        Name::CallOrIndexed(ref fcall) => fcall.search(ctx, searcher),
        Name::Attribute(ref attr) => {
            // @TODO more
            let AttributeName {
                name, expr, attr, ..
            } = attr.as_ref();
            return_if_found!(name.search(ctx, searcher));
            if let AttributeDesignator::Ident(ref user_attr) = attr.item {
                return_if_finished!(searcher.search_pos_with_ref(
                    ctx,
                    attr.pos(ctx),
                    &user_attr.reference
                ));
            }
            if let Some(expr) = expr {
                return_if_found!(expr.search(ctx, searcher));
            }
            NotFound
        }
        Name::External(ref ename) => {
            let ExternalName { subtype, .. } = ename.as_ref();
            return_if_found!(subtype.search(ctx, searcher));
            NotFound
        }
    }
}

impl Search for WithTokenSpan<Name> {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        return_if_finished!(searcher.search_with_pos(ctx, &self.pos(ctx)));
        search_pos_name(&self.pos(ctx), &self.item, searcher, ctx)
    }
}

impl Search for ElementConstraint {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        // @TODO more
        let ElementConstraint { constraint, .. } = self;
        constraint.search(ctx, searcher)
    }
}

impl Search for WithTokenSpan<ElementConstraint> {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        return_if_finished!(searcher.search_with_pos(ctx, &self.pos(ctx)));
        self.item.search(ctx, searcher)
    }
}

impl Search for WithTokenSpan<Declaration> {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        self.item.search(ctx, searcher)
    }
}

impl Search for WithTokenSpan<SubtypeConstraint> {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        return_if_finished!(searcher.search_with_pos(ctx, &self.pos(ctx)));
        match self.item {
            SubtypeConstraint::Array(ref dranges, ref constraint) => {
                for drange in dranges {
                    return_if_found!(&drange.item.search(ctx, searcher));
                }
                if let Some(ref constraint) = constraint {
                    return_if_found!(constraint.search(ctx, searcher));
                }
            }
            SubtypeConstraint::Range(ref range) => {
                return_if_found!(range.search(ctx, searcher));
            }
            SubtypeConstraint::Record(ref constraints) => {
                return_if_found!(constraints.search(ctx, searcher));
            }
        }
        NotFound
    }
}

impl Search for SubtypeIndication {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        // @TODO more
        let SubtypeIndication {
            type_mark,
            constraint,
            ..
        } = self;
        return_if_found!(type_mark.search(ctx, searcher));
        return_if_found!(constraint.search(ctx, searcher));
        NotFound
    }
}

impl Search for RangeConstraint {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        let RangeConstraint {
            direction: _,
            left_expr,
            right_expr,
        } = self;
        return_if_found!(left_expr.search(ctx, searcher));
        return_if_found!(right_expr.search(ctx, searcher));
        NotFound
    }
}

impl Search for AttributeName {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        // @TODO more
        let AttributeName { name, .. } = self;
        name.search(ctx, searcher)
    }
}

impl Search for Range {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        match self {
            Range::Range(constraint) => {
                return_if_found!(constraint.search(ctx, searcher));
            }
            Range::Attribute(attr) => {
                return_if_found!(attr.search(ctx, searcher));
            }
        }
        NotFound
    }
}

impl Search for DiscreteRange {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        match self {
            DiscreteRange::Discrete(ref type_mark, ref constraint) => {
                return_if_found!(type_mark.search(ctx, searcher));
                constraint.search(ctx, searcher)
            }
            DiscreteRange::Range(ref constraint) => constraint.search(ctx, searcher),
        }
    }
}

impl Search for TypeDeclaration {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        return_if_found!(searcher
            .search_decl(
                ctx,
                FoundDeclaration::new(&self.ident.decl, DeclarationItem::Type(self))
            )
            .or_not_found());

        match &self.def {
            TypeDefinition::ProtectedBody(ref body) => {
                return_if_found!(body.decl.search(ctx, searcher));
            }
            TypeDefinition::Protected(ref prot_decl) => {
                for item in prot_decl.items.iter() {
                    match item {
                        ProtectedTypeDeclarativeItem::Subprogram(ref subprogram) => {
                            return_if_found!(subprogram.search(ctx, searcher));
                        }
                    }
                }
            }
            TypeDefinition::Record(ref element_decls) => {
                for elem in element_decls {
                    for ident in &elem.idents {
                        return_if_found!(searcher
                            .search_decl(
                                ctx,
                                FoundDeclaration::new(
                                    &ident.decl,
                                    DeclarationItem::ElementDeclaration(elem)
                                )
                            )
                            .or_not_found());
                    }
                    return_if_found!(elem.subtype.search(ctx, searcher));
                }
            }
            TypeDefinition::Access(ref subtype_indication) => {
                return_if_found!(subtype_indication.search(ctx, searcher));
            }
            TypeDefinition::Array(ref indexes, _, ref subtype_indication) => {
                for index in indexes.iter() {
                    match index {
                        ArrayIndex::IndexSubtypeDefintion(ref type_mark) => {
                            return_if_found!(type_mark.search(ctx, searcher));
                        }
                        ArrayIndex::Discrete(ref drange) => {
                            return_if_found!(drange.item.search(ctx, searcher));
                        }
                    }
                }
                return_if_found!(subtype_indication.search(ctx, searcher));
            }
            TypeDefinition::Subtype(ref subtype_indication) => {
                return_if_found!(subtype_indication.search(ctx, searcher));
            }
            TypeDefinition::Numeric(ref range) => {
                return_if_found!(range.search(ctx, searcher));
            }
            TypeDefinition::File(ref type_mark) => {
                return_if_found!(type_mark.search(ctx, searcher));
            }
            TypeDefinition::Incomplete(ref reference) => {
                // Incomplete type should reference full declaration
                return_if_found!(searcher
                    .search_pos_with_ref(ctx, self.ident.pos(ctx), reference)
                    .or_not_found());
            }
            TypeDefinition::Enumeration(ref literals) => {
                for literal in literals {
                    return_if_found!(searcher
                        .search_decl(
                            ctx,
                            FoundDeclaration::new(
                                &literal.decl,
                                DeclarationItem::EnumerationLiteral(&self.ident.tree, literal)
                            )
                        )
                        .or_not_found());
                }
            }
            TypeDefinition::Physical(ref physical) => {
                let PhysicalTypeDeclaration {
                    range,
                    units_token: _,
                    primary_unit,
                    secondary_units,
                } = physical;
                return_if_found!(range.search(ctx, searcher));
                return_if_found!(searcher
                    .search_decl(
                        ctx,
                        FoundDeclaration::new(
                            &primary_unit.decl,
                            DeclarationItem::PhysicalTypePrimary(primary_unit)
                        )
                    )
                    .or_not_found());
                for (ident, literal) in secondary_units.iter() {
                    return_if_found!(searcher
                        .search_decl(
                            ctx,
                            FoundDeclaration::new(
                                &ident.decl,
                                DeclarationItem::PhysicalTypeSecondary(ident, &literal.item)
                            )
                        )
                        .or_not_found());
                    return_if_found!(searcher
                        .search_ident_ref(ctx, &literal.item.unit)
                        .or_not_found());
                }
            }
        }
        NotFound
    }
}

fn search_pos_expr(
    ctx: &dyn TokenAccess,
    pos: &SrcPos,
    expr: &Expression,
    searcher: &mut impl Searcher,
) -> SearchResult {
    return_if_finished!(searcher.search_with_pos(ctx, pos));
    match expr {
        Expression::Binary(ref op, ref left, ref right) => {
            return_if_found!(searcher
                .search_pos_with_ref(ctx, op.pos(ctx), &op.item.reference)
                .or_not_found());
            return_if_found!(left.search(ctx, searcher));
            right.search(ctx, searcher)
        }
        Expression::Unary(ref op, ref expr) => {
            return_if_found!(searcher
                .search_pos_with_ref(ctx, op.pos(ctx), &op.item.reference)
                .or_not_found());
            expr.search(ctx, searcher)
        }
        Expression::Name(ref name) => search_pos_name(pos, name, searcher, ctx),
        Expression::Aggregate(ref assocs) => assocs.search(ctx, searcher),
        Expression::Qualified(ref qexpr) => qexpr.search(ctx, searcher),
        Expression::New(ref alloc) => {
            return_if_finished!(searcher.search_with_pos(ctx, &alloc.pos(ctx)));
            match alloc.item {
                Allocator::Qualified(ref qexpr) => qexpr.search(ctx, searcher),
                Allocator::Subtype(ref subtype) => subtype.search(ctx, searcher),
            }
        }
        Expression::Literal(literal) => match literal {
            Literal::Physical(PhysicalLiteral { unit, .. }) => {
                searcher.search_ident_ref(ctx, unit).or_not_found()
            }
            _ => NotFound,
        },
        Expression::Parenthesized(expr) => {
            search_pos_expr(ctx, &expr.span.pos(ctx), &expr.item, searcher)
        }
    }
}

impl Search for ElementAssociation {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        match self {
            ElementAssociation::Named(ref choices, ref expr) => {
                return_if_found!(choices.search(ctx, searcher));
                return_if_found!(expr.search(ctx, searcher));
            }
            ElementAssociation::Positional(ref expr) => {
                return_if_found!(expr.search(ctx, searcher));
            }
        }
        NotFound
    }
}

impl Search for QualifiedExpression {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        let QualifiedExpression { type_mark, expr } = self;
        return_if_found!(type_mark.search(ctx, searcher));
        return_if_found!(expr.search(ctx, searcher));
        NotFound
    }
}

impl Search for AssociationElement {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        let AssociationElement { formal, actual } = self;
        if let Some(formal) = formal {
            return_if_found!(search_pos_name(
                &formal.pos(ctx),
                &formal.item,
                searcher,
                ctx
            ));
        }

        match actual.item {
            ActualPart::Expression(ref expr) => {
                return_if_found!(search_pos_expr(ctx, &actual.pos(ctx), expr, searcher));
            }
            ActualPart::Open => {}
        }
        NotFound
    }
}

impl Search for CallOrIndexed {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        let CallOrIndexed { name, parameters } = self;
        return_if_found!(name.search(ctx, searcher));
        return_if_found!(parameters.search(ctx, searcher));
        NotFound
    }
}

impl Search for Waveform {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        match self {
            Waveform::Elements(ref elems) => {
                for elem in elems.iter() {
                    let WaveformElement { value, after } = elem;
                    return_if_found!(value.search(ctx, searcher));
                    return_if_found!(after.search(ctx, searcher));
                }
            }
            Waveform::Unaffected(_) => {}
        }
        NotFound
    }
}

impl Search for WithTokenSpan<Expression> {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        search_pos_expr(ctx, &self.span.pos(ctx), &self.item, searcher)
    }
}

impl Search for ObjectDeclaration {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        for ident in &self.idents {
            return_if_found!(searcher
                .search_decl(
                    ctx,
                    FoundDeclaration::new(&ident.decl, DeclarationItem::Object(self))
                )
                .or_not_found());
        }
        return_if_found!(self.subtype_indication.search(ctx, searcher));
        if let Some(ref expr) = self.expression {
            expr.search(ctx, searcher)
        } else {
            NotFound
        }
    }
}

impl Search for Signature {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        match self {
            Signature::Function(args, ret) => {
                return_if_found!(args.search(ctx, searcher));
                return_if_found!(ret.search(ctx, searcher));
            }
            Signature::Procedure(args) => {
                return_if_found!(args.search(ctx, searcher));
            }
        }
        NotFound
    }
}

impl Search for InterfaceList {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        self.items.search(ctx, searcher)
    }
}

impl Search for Declaration {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        match self {
            Declaration::Object(object) => {
                return_if_found!(object.search(ctx, searcher));
            }
            Declaration::Type(typ) => {
                return_if_found!(typ.search(ctx, searcher));
            }
            Declaration::SubprogramBody(body) => {
                return_if_found!(searcher
                    .search_decl(
                        ctx,
                        FoundDeclaration::new(
                            body.specification.ent_id_ref(),
                            DeclarationItem::Subprogram(body)
                        )
                    )
                    .or_not_found());
                return_if_found!(search_subpgm_inner(&body.specification, ctx, searcher));
                return_if_found!(body.declarations.search(ctx, searcher));
                return_if_found!(body.statements.search(ctx, searcher));
            }
            Declaration::SubprogramDeclaration(decl) => {
                return_if_found!(decl.search(ctx, searcher));
            }
            Declaration::SubprogramInstantiation(decl) => {
                return_if_found!(decl.search(ctx, searcher));
            }
            Declaration::Attribute(Attribute::Declaration(decl)) => {
                return_if_found!(searcher
                    .search_decl(
                        ctx,
                        FoundDeclaration::new(&decl.ident.decl, DeclarationItem::Attribute(decl))
                    )
                    .or_not_found());
                return_if_found!(decl.type_mark.search(ctx, searcher));
            }
            Declaration::Attribute(Attribute::Specification(AttributeSpecification {
                ident,
                entity_name,
                expr,
                ..
            })) => {
                return_if_found!(searcher.search_ident_ref(ctx, ident).or_not_found());
                if let EntityName::Name(EntityTag {
                    designator,
                    signature,
                }) = entity_name
                {
                    return_if_found!(searcher
                        .search_pos_with_ref(ctx, designator.pos(ctx), &designator.item.reference)
                        .or_not_found());
                    if let Some(signature) = signature {
                        return_if_found!(signature.item.search(ctx, searcher));
                    }
                }

                return_if_found!(expr.search(ctx, searcher));
            }
            Declaration::Alias(alias) => {
                return_if_found!(searcher
                    .search_decl(
                        ctx,
                        FoundDeclaration::new(
                            &alias.designator.decl,
                            DeclarationItem::Alias(alias)
                        )
                    )
                    .or_not_found());
                let AliasDeclaration {
                    subtype_indication,
                    name,
                    signature,
                    ..
                } = alias;
                return_if_found!(subtype_indication.search(ctx, searcher));
                return_if_found!(name.search(ctx, searcher));
                if let Some(signature) = signature {
                    return_if_found!(signature.item.search(ctx, searcher));
                }
            }
            Declaration::Use(use_clause) => {
                return_if_found!(searcher
                    .search_with_pos(ctx, &use_clause.get_pos(ctx))
                    .or_not_found());
                return_if_found!(use_clause.name_list.search(ctx, searcher));
            }
            Declaration::Component(component) => {
                return_if_found!(searcher
                    .search_decl(
                        ctx,
                        FoundDeclaration::new(
                            &component.ident.decl,
                            DeclarationItem::Component(component)
                        )
                    )
                    .or_not_found());
                let ComponentDeclaration {
                    generic_list,
                    port_list,
                    ..
                } = component;
                if let Some(generic_list) = generic_list {
                    return_if_found!(generic_list.search(ctx, searcher));
                }
                if let Some(port_list) = port_list {
                    return_if_found!(port_list.search(ctx, searcher));
                }
            }

            Declaration::File(file) => {
                for ident in &file.idents {
                    return_if_found!(searcher
                        .search_decl(
                            ctx,
                            FoundDeclaration::new(&ident.decl, DeclarationItem::File(file))
                        )
                        .or_not_found());
                }
                let FileDeclaration {
                    idents: _,
                    colon_token: _,
                    subtype_indication,
                    open_info,
                    file_name,
                } = file;
                return_if_found!(subtype_indication.search(ctx, searcher));
                if let Some((_, open_info)) = open_info {
                    return_if_found!(open_info.search(ctx, searcher));
                }
                if let Some((_, file_name)) = file_name {
                    return_if_found!(file_name.search(ctx, searcher));
                }
            }

            Declaration::Package(ref package_instance) => {
                return_if_found!(package_instance.search(ctx, searcher));
            }

            Declaration::Configuration(_) => {
                // @TODO
            }
            Declaration::View(view) => {
                return_if_found!(searcher
                    .search_decl(
                        ctx,
                        FoundDeclaration::new(&view.ident.decl, DeclarationItem::View(view))
                    )
                    .or_not_found());
                let ModeViewDeclaration { typ, elements, .. } = view;
                return_if_found!(typ.search(ctx, searcher));
                return_if_found!(elements.search(ctx, searcher));
            }
        }
        NotFound
    }
}

impl Search for ModeViewElement {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        for name in self.names.iter() {
            return_if_found!(searcher
                .search_pos_with_ref(ctx, name.pos(ctx), &name.decl)
                .or_not_found());
        }
        NotFound
    }
}

impl Search for ModeIndication {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        match self {
            ModeIndication::Simple(simple) => simple.search(ctx, searcher),
            ModeIndication::View(view) => view.search(ctx, searcher),
        }
    }
}

impl Search for SimpleModeIndication {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        return_if_found!(self.subtype_indication.search(ctx, searcher));
        return_if_found!(self.expression.search(ctx, searcher));
        NotFound
    }
}

impl Search for ModeViewIndication {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        return_if_found!(self.name.search(ctx, searcher));
        if let Some((_, subtype)) = &self.subtype_indication {
            return_if_found!(subtype.search(ctx, searcher));
        }
        NotFound
    }
}

impl Search for InterfaceDeclaration {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        match self {
            InterfaceDeclaration::Object(ref decl) => {
                for ident in &decl.idents {
                    return_if_found!(searcher
                        .search_decl(
                            ctx,
                            FoundDeclaration::new(
                                &ident.decl,
                                DeclarationItem::InterfaceObject(decl)
                            )
                        )
                        .or_not_found());
                }
                return_if_found!(decl.mode.search(ctx, searcher));
            }
            InterfaceDeclaration::Subprogram(ref subprogram_decl) => {
                return_if_found!(searcher
                    .search_decl(
                        ctx,
                        FoundDeclaration::new(
                            subprogram_decl.specification.ent_id_ref(),
                            DeclarationItem::SubprogramDecl(&subprogram_decl.specification)
                        )
                    )
                    .or_not_found());

                if let Some(subpgm_default) = &subprogram_decl.default {
                    match subpgm_default {
                        SubprogramDefault::Name(selected_name) => {
                            return_if_found!(selected_name.search(ctx, searcher));
                        }
                        SubprogramDefault::Box => {}
                    }
                }
            }
            InterfaceDeclaration::Type(decl) => {
                return_if_found!(searcher
                    .search_decl(
                        ctx,
                        FoundDeclaration::new(&decl.decl, DeclarationItem::InterfaceType(decl))
                    )
                    .or_not_found());
            }
            InterfaceDeclaration::Package(package_instance) => {
                return_if_found!(searcher
                    .search_decl(
                        ctx,
                        FoundDeclaration::new(
                            &package_instance.ident.decl,
                            DeclarationItem::InterfacePackage(package_instance)
                        )
                    )
                    .or_not_found());
                return_if_found!(package_instance.package_name.search(ctx, searcher));
                match package_instance.generic_map.item {
                    InterfacePackageGenericMapAspect::Map(ref generic_map) => {
                        return_if_found!(generic_map.search(ctx, searcher));
                    }
                    InterfacePackageGenericMapAspect::Box => {}
                    InterfacePackageGenericMapAspect::Default => {}
                }
            }
            InterfaceDeclaration::File(decl) => {
                for ident in &decl.idents {
                    return_if_found!(searcher
                        .search_decl(
                            ctx,
                            FoundDeclaration::new(
                                &ident.decl,
                                DeclarationItem::InterfaceFile(decl)
                            )
                        )
                        .or_not_found());
                }
            }
        };
        NotFound
    }
}

impl Search for SubprogramDeclaration {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        self.specification.search(ctx, searcher)
    }
}

impl Search for SubprogramSpecification {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        return_if_found!(searcher
            .search_decl(
                ctx,
                FoundDeclaration::new(self.ent_id_ref(), DeclarationItem::SubprogramDecl(self))
            )
            .or_not_found());
        search_subpgm_inner(self, ctx, searcher)
    }
}

fn search_subpgm_inner(
    subgpm: &SubprogramSpecification,
    ctx: &dyn TokenAccess,
    searcher: &mut impl Searcher,
) -> SearchResult {
    match subgpm {
        SubprogramSpecification::Function(ref decl) => {
            return_if_found!(decl.header.search(ctx, searcher));
            return_if_found!(decl.parameter_list.search(ctx, searcher));
            decl.return_type.search(ctx, searcher)
        }
        SubprogramSpecification::Procedure(ref decl) => {
            return_if_found!(decl.header.search(ctx, searcher));
            decl.parameter_list.search(ctx, searcher)
        }
    }
}

impl Search for SubprogramHeader {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        return_if_found!(self.generic_list.search(ctx, searcher));
        self.map_aspect.search(ctx, searcher)
    }
}

impl Search for LibraryClause {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        for name in self.name_list.iter() {
            return_if_found!(searcher
                .search_pos_with_ref(ctx, name.item.pos(ctx), &name.reference)
                .or_not_found());
        }
        NotFound
    }
}

impl Search for ContextItem {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        return_if_finished!(searcher.search_with_pos(ctx, &self.get_pos(ctx)));
        match self {
            ContextItem::Use(ref use_clause) => {
                return_if_found!(use_clause.name_list.search(ctx, searcher));
            }
            ContextItem::Library(ref library_clause) => {
                return_if_found!(library_clause.search(ctx, searcher));
            }
            ContextItem::Context(ref context_clause) => {
                return_if_found!(context_clause.name_list.search(ctx, searcher));
            }
        }
        NotFound
    }
}

impl Search for AnyDesignUnit {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        delegate_any!(self, unit, unit.search(ctx, searcher))
    }
}

impl Search for AnyPrimaryUnit {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        delegate_primary!(self, unit, unit.search(ctx, searcher))
    }
}

impl Search for AnySecondaryUnit {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        delegate_secondary!(self, unit, unit.search(ctx, searcher))
    }
}

impl Search for EntityDeclaration {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        return_if_found!(self.context_clause.search(ctx, searcher));
        return_if_found!(searcher
            .search_decl(
                ctx,
                FoundDeclaration::new(&self.ident.decl, DeclarationItem::Entity(self))
            )
            .or_not_found());
        if let Some(clause) = &self.generic_clause {
            return_if_found!(clause.search(ctx, searcher));
        }
        if let Some(clause) = &self.port_clause {
            return_if_found!(clause.search(ctx, searcher));
        }
        return_if_found!(self.decl.search(ctx, searcher));
        self.statements.search(ctx, searcher)
    }
}

impl Search for ArchitectureBody {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        return_if_found!(self.context_clause.search(ctx, searcher));
        return_if_found!(searcher
            .search_ident_ref(ctx, &self.entity_name)
            .or_not_found());
        return_if_found!(searcher
            .search_decl(
                ctx,
                FoundDeclaration::new(&self.ident.decl, DeclarationItem::Architecture(self))
            )
            .or_not_found());
        return_if_found!(self.decl.search(ctx, searcher));
        self.statements.search(ctx, searcher)
    }
}

impl Search for PackageDeclaration {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        return_if_found!(self.context_clause.search(ctx, searcher));
        return_if_found!(searcher
            .search_decl(
                ctx,
                FoundDeclaration::new(&self.ident.decl, DeclarationItem::Package(self))
            )
            .or_not_found());
        return_if_found!(self.generic_clause.search(ctx, searcher));
        self.decl.search(ctx, searcher)
    }
}

impl Search for PackageBody {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        return_if_found!(self.context_clause.search(ctx, searcher));
        return_if_found!(searcher
            .search_decl(
                ctx,
                FoundDeclaration::new(&self.ident.decl, DeclarationItem::PackageBody(self))
            )
            .or_not_found());
        self.decl.search(ctx, searcher)
    }
}

impl Search for PackageInstantiation {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        return_if_found!(self.context_clause.search(ctx, searcher));
        return_if_found!(searcher
            .search_decl(
                ctx,
                FoundDeclaration::new(&self.ident.decl, DeclarationItem::PackageInstance(self))
            )
            .or_not_found());
        return_if_found!(self.generic_map.search(ctx, searcher));
        self.package_name.search(ctx, searcher)
    }
}

impl Search for ConfigurationDeclaration {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        return_if_found!(self.context_clause.search(ctx, searcher));
        return_if_found!(searcher
            .search_decl(
                ctx,
                FoundDeclaration::new(&self.ident.decl, DeclarationItem::Configuration(self))
            )
            .or_not_found());
        self.entity_name.search(ctx, searcher)
    }
}

impl Search for ContextDeclaration {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        return_if_found!(searcher
            .search_decl(
                ctx,
                FoundDeclaration::new(&self.ident.decl, DeclarationItem::Context(self))
            )
            .or_not_found());
        self.items.search(ctx, searcher)
    }
}

impl Search for CaseStatement {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        let CaseStatement {
            expression,
            alternatives,
            ..
        } = self;
        return_if_found!(expression.search(ctx, searcher));
        return_if_found!(search_alternatives(alternatives, false, searcher, ctx));
        NotFound
    }
}

impl Search for MapAspect {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        self.list.search(ctx, searcher)
    }
}

impl Search for SubprogramInstantiation {
    fn search(&self, ctx: &dyn TokenAccess, searcher: &mut impl Searcher) -> SearchResult {
        return_if_found!(searcher
            .search_decl(
                ctx,
                FoundDeclaration::new(
                    &self.ident.decl,
                    DeclarationItem::SubprogramInstantiation(self)
                )
            )
            .or_not_found());
        return_if_found!(self.subprogram_name.search(ctx, searcher));
        if let Some(signature) = &self.signature {
            return_if_found!(signature.item.search(ctx, searcher));
        }
        self.generic_map.search(ctx, searcher)
    }
}

// Search for reference to declaration/definition at cursor
pub struct ItemAtCursor<'a> {
    root: &'a DesignRoot,
    cursor: Position,
    pub result: Option<(SrcPos, EntRef<'a>)>,
}

impl<'a> ItemAtCursor<'a> {
    pub fn new(root: &'a DesignRoot, cursor: Position) -> Self {
        ItemAtCursor {
            root,
            cursor,
            result: None,
        }
    }

    fn is_inside(&self, pos: &SrcPos) -> bool {
        // cursor is the gap between character cursor and cursor + 1
        // Thus cursor will match character cursor and cursor + 1
        pos.start() <= self.cursor && self.cursor <= pos.end()
    }

    fn search_decl_pos(&mut self, pos: &SrcPos, ent: EntRef<'a>) -> SearchState {
        if self.is_inside(pos) {
            self.result = Some((pos.clone(), ent));
            Finished(Found)
        } else {
            NotFinished
        }
    }
}

impl Searcher for ItemAtCursor<'_> {
    fn search_with_pos(&mut self, _ctx: &dyn TokenAccess, pos: &SrcPos) -> SearchState {
        // cursor is the gap between character cursor and cursor + 1
        // Thus cursor will match character cursor and cursor + 1
        if self.is_inside(pos) {
            NotFinished
        } else {
            Finished(NotFound)
        }
    }

    fn search_decl(&mut self, ctx: &dyn TokenAccess, decl: FoundDeclaration<'_>) -> SearchState {
        if let Some(id) = decl.ent_id() {
            let ent = self.root.get_ent(id);

            if let Some(decl_pos) = ent.decl_pos() {
                if let Finished(res) = self.search_decl_pos(decl_pos, ent) {
                    return Finished(res);
                }
            }

            if let Some(end_pos) = decl.end_ident_pos() {
                return self.search_decl_pos(ctx.get_pos(end_pos), ent);
            }
        }
        NotFinished
    }

    fn search_pos_with_ref(
        &mut self,
        _ctx: &dyn TokenAccess,
        pos: &SrcPos,
        reference: &Reference,
    ) -> SearchState {
        if self.is_inside(pos) {
            if let Some(id) = reference.get() {
                self.result = Some((pos.clone(), self.root.get_ent(id)));
                Finished(Found)
            } else {
                Finished(NotFound)
            }
        } else {
            NotFinished
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
    fn search_decl(&mut self, _ctx: &dyn TokenAccess, decl: FoundDeclaration<'_>) -> SearchState {
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
    fn search_decl(&mut self, _ctx: &dyn TokenAccess, decl: FoundDeclaration<'_>) -> SearchState {
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

impl Searcher for FormatDeclaration<'_> {
    fn search_decl(&mut self, _ctx: &dyn TokenAccess, decl: FoundDeclaration<'_>) -> SearchState {
        let id = if let Some(id) = decl.ent_id() {
            id
        } else {
            return NotFinished;
        };

        if self.ent.is_implicit_of(id) {
            // Implicit
            self.result = Some(format!(
                "-- {}\n\n-- Implicitly defined by:\n{}\n",
                self.ent.describe(),
                decl.ast,
            ));
            return Finished(Found);
        } else if self.ent.id() == id {
            // Explicit
            self.result = Some(decl.ast.to_string());
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

pub fn is_reference(ent: EntRef<'_>, other: EntRef<'_>) -> bool {
    if ent.id() == other.id() {
        return true;
    }

    if ent.is_instance_of(other) || other.is_instance_of(ent) {
        return true;
    }

    if ent.is_declared_by(other) || other.is_declared_by(ent) {
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

impl Searcher for FindAllReferences<'_> {
    fn search_decl(&mut self, ctx: &dyn TokenAccess, decl: FoundDeclaration<'_>) -> SearchState {
        if let Some(id) = decl.ent_id() {
            let other = self.root.get_ent(id);

            if is_reference(self.ent, other) {
                if let Some(decl_pos) = other.decl_pos() {
                    self.references.push(decl_pos.clone());
                }
                if let Some(pos) = decl.end_ident_pos() {
                    self.references.push(ctx.get_pos(pos).clone());
                }
            }
        }
        NotFinished
    }

    fn search_pos_with_ref(
        &mut self,
        _ctx: &dyn TokenAccess,
        pos: &SrcPos,
        reference: &Reference,
    ) -> SearchState {
        if let Some(id) = reference.get() {
            let other = self.root.get_ent(id);
            if is_reference(self.ent, other) {
                self.references.push(pos.clone());
            }
        };
        NotFinished
    }
}

impl FoundDeclaration<'_> {
    fn end_ident_pos(&self) -> Option<TokenId> {
        match &self.ast {
            DeclarationItem::InterfaceObject(_) => None,
            DeclarationItem::ForIndex(..) => None,
            DeclarationItem::ForGenerateIndex(..) => None,
            DeclarationItem::Subprogram(value) => value.end_ident_pos,
            DeclarationItem::SubprogramDecl(..) => None,
            DeclarationItem::Object(..) => None,
            DeclarationItem::ElementDeclaration(..) => None,
            DeclarationItem::EnumerationLiteral(..) => None,
            DeclarationItem::File(..) => None,
            DeclarationItem::Type(value) => value.end_ident_pos,
            DeclarationItem::InterfaceType(..) => None,
            DeclarationItem::InterfacePackage(..) => None,
            DeclarationItem::InterfaceFile(..) => None,
            DeclarationItem::PhysicalTypePrimary(..) => None,
            DeclarationItem::PhysicalTypeSecondary(..) => None,
            DeclarationItem::Component(value) => value.end_ident_pos,
            DeclarationItem::Attribute(..) => None,
            DeclarationItem::Alias(..) => None,
            DeclarationItem::Package(value) => value.end_ident_pos,
            DeclarationItem::PackageBody(value) => value.end_ident_pos,
            DeclarationItem::PackageInstance(..) => None,
            DeclarationItem::Configuration(value) => value.end_ident_pos,
            DeclarationItem::Entity(value) => value.end_ident_pos,
            DeclarationItem::Architecture(value) => value.end_ident_pos,
            DeclarationItem::Context(value) => value.end_ident_pos,
            DeclarationItem::GenerateBody(..) => None,
            DeclarationItem::ConcurrentStatement(..) => None,
            DeclarationItem::SequentialStatement(..) => None,
            DeclarationItem::SubprogramInstantiation(_) => None,
            DeclarationItem::View(view) => view.end_ident_pos,
        }
    }

    fn ent_id_ref(&self) -> &Reference {
        self.reference
    }
}

impl SubprogramSpecification {
    fn ent_id_ref(&self) -> &Reference {
        match self {
            SubprogramSpecification::Procedure(proc) => &proc.designator.decl,
            SubprogramSpecification::Function(func) => &func.designator.decl,
        }
    }
}

impl HasEntityId for FoundDeclaration<'_> {
    fn ent_id(&self) -> Option<EntityId> {
        self.ent_id_ref().get()
    }
}

impl std::fmt::Display for DeclarationItem<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DeclarationItem::InterfaceObject(ref value) => match value.list_type {
                InterfaceType::Port => write!(f, "port {value};"),
                InterfaceType::Generic => write!(f, "generic {value};"),
                InterfaceType::Parameter => write!(f, "{value};"),
            },
            DeclarationItem::ForIndex(ref ident, ref drange) => {
                write!(f, "for {ident} in {drange} loop")
            }
            DeclarationItem::ForGenerateIndex(ref ident, ref value) => match ident {
                Some(ident) => write!(f, "{ident}: {value}"),
                None => write!(f, "{value}"),
            },
            DeclarationItem::Subprogram(value) => {
                write!(f, "{};", value.specification)
            }
            DeclarationItem::SubprogramDecl(ref value) => {
                write!(f, "{value}")
            }
            DeclarationItem::SubprogramInstantiation(ref value) => {
                write!(f, "{value};")
            }
            DeclarationItem::Object(ref value) => {
                write!(f, "{value}")
            }
            DeclarationItem::ElementDeclaration(elem) => {
                write!(f, "{elem}")
            }
            DeclarationItem::EnumerationLiteral(ident, elem) => {
                write!(f, "{elem} : {ident}")
            }
            DeclarationItem::File(ref value) => {
                write!(f, "{value}")
            }
            DeclarationItem::PhysicalTypePrimary(ref value) => {
                write!(f, "{value}")
            }
            DeclarationItem::PhysicalTypeSecondary(_, ref literal) => {
                write!(f, "{literal}")
            }
            DeclarationItem::Type(ref value) => {
                write!(f, "{value}")
            }
            DeclarationItem::InterfaceType(ref value) => {
                write!(f, "type {value}")
            }
            DeclarationItem::InterfacePackage(value) => {
                write!(f, "{value}")
            }
            DeclarationItem::InterfaceFile(value) => {
                write!(f, "{value}")
            }
            DeclarationItem::Component(ref value) => {
                write!(f, "{value}")
            }
            DeclarationItem::Alias(ref value) => {
                write!(f, "{value}")
            }
            DeclarationItem::Attribute(ref value) => {
                write!(f, "{value}")
            }
            DeclarationItem::Package(ref value) => {
                write!(f, "{value}")
            }
            DeclarationItem::PackageBody(value) => {
                // Will never be shown has hover will goto the declaration
                write!(f, "package body {}", value.name())
            }
            DeclarationItem::PackageInstance(ref value) => {
                write!(f, "{value}")
            }
            DeclarationItem::Configuration(ref value) => {
                write!(f, "{value}")
            }
            DeclarationItem::Entity(ref value) => {
                write!(f, "{value}")
            }
            DeclarationItem::Architecture(value) => {
                write!(f, "architecture {} of {}", value.ident(), value.entity_name)
            }
            DeclarationItem::Context(ref value) => {
                write!(f, "{value}")
            }
            DeclarationItem::GenerateBody(value) => {
                write!(f, "{value}")
            }
            DeclarationItem::ConcurrentStatement(value) => {
                if let Some(ref label) = value.label.tree {
                    write!(f, "{label}")
                } else {
                    write!(f, "<anonymous statement>")
                }
            }
            DeclarationItem::SequentialStatement(value) => {
                if let Some(ref label) = value.label.tree {
                    write!(f, "{label}")
                } else {
                    write!(f, "<anonymous statement>")
                }
            }
            DeclarationItem::View(value) => write!(f, "view {} of {}", value.ident, value.typ),
        }
    }
}

#[derive(Default)]
pub struct FindAllUnresolved {
    pub count: usize,
    pub unresolved: Vec<SrcPos>,
}

impl Searcher for FindAllUnresolved {
    fn search_pos_with_ref(
        &mut self,
        _ctx: &dyn TokenAccess,
        pos: &SrcPos,
        reference: &Reference,
    ) -> SearchState {
        self.count += 1;
        if reference.is_undefined() {
            self.unresolved.push(pos.clone());
        }
        NotFinished
    }
}

pub fn clear_references(tree: &mut impl Search, ctx: &dyn TokenAccess) {
    struct ReferenceClearer;

    impl Searcher for ReferenceClearer {
        fn search_pos_with_ref(
            &mut self,
            _ctx: &dyn TokenAccess,
            _pos: &SrcPos,
            reference: &Reference,
        ) -> SearchState {
            reference.clear();
            NotFinished
        }

        fn search_decl(
            &mut self,
            _ctx: &dyn TokenAccess,
            decl: FoundDeclaration<'_>,
        ) -> SearchState {
            let reference = decl.ent_id_ref();
            reference.clear();
            NotFinished
        }
    }

    let mut searcher = ReferenceClearer;
    let _ = tree.search(ctx, &mut searcher);
}

#[cfg(test)]
#[allow(clippy::ptr_arg)]
pub fn check_no_unresolved(tree: &mut impl Search, tokens: &Vec<Token>) {
    #[derive(Default)]
    struct CheckNoUnresolved;

    impl Searcher for CheckNoUnresolved {
        fn search_pos_with_ref(
            &mut self,
            _ctx: &dyn TokenAccess,
            _pos: &SrcPos,
            reference: &Reference,
        ) -> SearchState {
            assert!(reference.is_defined());
            NotFinished
        }
    }

    let mut searcher = CheckNoUnresolved;
    let _ = tree.search(tokens, &mut searcher);
}
