// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use crate::analysis::AnalysisData;
use crate::analysis::DesignRoot;
use crate::analysis::ReadGuard;
use crate::ast::AnyDesignUnit;
use crate::ast::AnyPrimaryUnit;
use crate::ast::AnySecondaryUnit;
use crate::ast::BlockStatement;
use crate::ast::CaseGenerateStatement;
use crate::ast::CaseStatement;
use crate::ast::ConcurrentStatement;
use crate::ast::Declaration;
use crate::ast::ForGenerateStatement;
use crate::ast::GenerateBody;
use crate::ast::IfGenerateStatement;
use crate::ast::IfStatement;
use crate::ast::LabeledConcurrentStatement;
use crate::ast::LabeledSequentialStatement;
use crate::ast::LoopStatement;
use crate::ast::ProcessStatement;
use crate::ast::ProtectedTypeBody;
use crate::ast::SequentialStatement;
use crate::ast::SubprogramBody;
use crate::ast::TypeDefinition;
use crate::data::Symbol;

pub enum Node<'a> {
    Declaration(&'a Declaration),
    Sequential(&'a LabeledSequentialStatement),
    Concurrent(&'a LabeledConcurrentStatement),
}

pub trait HasNodes<'a> {
    fn children(&'a self) -> Box<dyn Iterator<Item = Node<'a>> + 'a>;
}

pub struct Root<'a> {
    root: &'a DesignRoot,
}

pub struct Library<'a> {
    library: &'a crate::analysis::Library,
}

pub struct Primary<'a> {
    library: &'a crate::analysis::Library,
    unit: ReadGuard<'a, AnyDesignUnit, AnalysisData>,
}

pub struct Secondary<'a> {
    unit: ReadGuard<'a, AnyDesignUnit, AnalysisData>,
}

impl<'a> Root<'a> {
    pub fn get_library(&self, name: &Symbol) -> Option<Library<'a>> {
        Some(Library {
            library: self.root.get_lib(name)?,
        })
    }
}

impl<'a> Library<'a> {
    pub fn primary_units(&self) -> impl Iterator<Item = Primary<'a>> {
        self.library.primary_units().map(|unit| Primary {
            library: self.library,
            unit: unit.unit.expect_analyzed(),
        })
    }

    pub fn get_primary_unit(&self, name: &Symbol) -> Option<Primary<'a>> {
        self.library.primary_unit(name).map(|unit| Primary {
            library: self.library,
            unit: unit.unit.expect_analyzed(),
        })
    }
}

impl<'a> Primary<'a> {
    pub fn get(&self) -> &AnyPrimaryUnit {
        match self.unit.data() {
            AnyDesignUnit::Primary(p) => p,
            AnyDesignUnit::Secondary(_) => unreachable!("Must be primary unit"),
        }
    }

    pub fn secondary_units(&'a self) -> impl Iterator<Item = Secondary<'a>> {
        use crate::ast::HasIdent;
        self.library
            .secondary_units(self.unit.data().name())
            .map(|unit| Secondary {
                unit: unit.unit.expect_analyzed(),
            })
    }
}

impl<'a> Secondary<'a> {
    pub fn get(&self) -> &AnySecondaryUnit {
        match self.unit.data() {
            AnyDesignUnit::Primary(_) => unreachable!("Must be secondary"),
            AnyDesignUnit::Secondary(s) => s,
        }
    }
}

impl<'a> HasNodes<'a> for Primary<'a> {
    fn children(&'a self) -> Box<dyn Iterator<Item = Node<'a>> + 'a> {
        match self.unit.data() {
            AnyDesignUnit::Primary(primary) => match primary {
                AnyPrimaryUnit::Entity(entity) => {
                    let declarations = entity.decl.iter().map(Node::Declaration);
                    let statements = entity.statements.iter().map(Node::Concurrent);

                    Box::new(declarations.chain(statements))
                }
                AnyPrimaryUnit::Package(package) => {
                    Box::new(package.decl.iter().map(Node::Declaration))
                }
                AnyPrimaryUnit::Configuration(_)
                | AnyPrimaryUnit::PackageInstance(_)
                | AnyPrimaryUnit::Context(_) => no_children(),
            },
            AnyDesignUnit::Secondary(_) => unreachable!("Expected primary unit"),
        }
    }
}

impl<'a> HasNodes<'a> for Node<'a> {
    fn children(&'a self) -> Box<dyn Iterator<Item = Node<'a>> + 'a> {
        match self {
            Node::Declaration(decl) => decl.children(),
            Node::Sequential(sequential) => sequential.statement.item.children(),
            Node::Concurrent(concurrent) => concurrent.statement.item.children(),
        }
    }
}

impl<'a> HasNodes<'a> for Declaration {
    fn children(&'a self) -> Box<dyn Iterator<Item = Node<'a>> + 'a> {
        match self {
            Declaration::Object(_) => no_children(),
            Declaration::File(_) => no_children(),
            Declaration::Type(typ) => typ.def.children(),
            Declaration::Component(_) => no_children(),
            Declaration::Attribute(_) => no_children(),
            Declaration::Alias(_) => no_children(),
            Declaration::SubprogramDeclaration(_) => no_children(),
            Declaration::SubprogramInstantiation(_) => no_children(),
            Declaration::SubprogramBody(body) => body.children(),
            Declaration::Use(_) => no_children(),
            Declaration::Package(_) => no_children(),
            Declaration::Configuration(_) => no_children(),
        }
    }
}

impl<'a> HasNodes<'a> for SequentialStatement {
    fn children(&'a self) -> Box<dyn Iterator<Item = Node<'a>> + 'a> {
        match self {
            SequentialStatement::Wait(_) => no_children(),
            SequentialStatement::Assert(_) => no_children(),
            SequentialStatement::Report(_) => no_children(),
            SequentialStatement::VariableAssignment(_) => no_children(),
            SequentialStatement::SignalAssignment(_) => no_children(),
            SequentialStatement::SignalForceAssignment(_) => no_children(),
            SequentialStatement::SignalReleaseAssignment(_) => no_children(),
            SequentialStatement::ProcedureCall(_) => no_children(),
            SequentialStatement::If(stmt) => stmt.children(),
            SequentialStatement::Case(stmt) => stmt.children(),
            SequentialStatement::Loop(stmt) => stmt.children(),
            SequentialStatement::Next(_) => no_children(),
            SequentialStatement::Exit(_) => no_children(),
            SequentialStatement::Return(_) => no_children(),
            SequentialStatement::Null => no_children(),
        }
    }
}

impl<'a> HasNodes<'a> for IfStatement {
    fn children(&'a self) -> Box<dyn Iterator<Item = Node<'a>> + 'a> {
        let IfStatement {
            conds,
            end_label_pos: _,
        } = self;

        Box::new(
            conds
                .conditionals
                .iter()
                .flat_map(|branch| branch.item.iter().map(Node::Sequential))
                .chain(
                    conds
                        .else_item
                        .iter()
                        .flat_map(|else_branch| else_branch.iter().map(Node::Sequential)),
                ),
        )
    }
}

impl<'a> HasNodes<'a> for CaseStatement {
    fn children(&'a self) -> Box<dyn Iterator<Item = Node<'a>> + 'a> {
        let CaseStatement {
            is_matching: _,
            expression: _,
            alternatives,
            end_label_pos: _,
        } = self;

        Box::new(
            alternatives
                .iter()
                .flat_map(|choice| choice.item.iter().map(Node::Sequential)),
        )
    }
}

impl<'a> HasNodes<'a> for LoopStatement {
    fn children(&'a self) -> Box<dyn Iterator<Item = Node<'a>> + 'a> {
        let LoopStatement {
            iteration_scheme: _,
            statements,
            end_label_pos: _,
        } = self;

        Box::new(statements.iter().map(Node::Sequential))
    }
}

impl<'a> HasNodes<'a> for ConcurrentStatement {
    fn children(&'a self) -> Box<dyn Iterator<Item = Node<'a>> + 'a> {
        match self {
            ConcurrentStatement::ProcedureCall(_) => no_children(),
            ConcurrentStatement::Block(block) => block.children(),
            ConcurrentStatement::Process(process) => process.children(),
            ConcurrentStatement::Assert(_) => no_children(),
            ConcurrentStatement::Assignment(_) => no_children(),
            ConcurrentStatement::Instance(_) => no_children(),
            ConcurrentStatement::ForGenerate(gen) => gen.children(),
            ConcurrentStatement::IfGenerate(gen) => gen.children(),
            ConcurrentStatement::CaseGenerate(gen) => gen.children(),
        }
    }
}

impl<'a> HasNodes<'a> for BlockStatement {
    fn children(&'a self) -> Box<dyn Iterator<Item = Node<'a>> + 'a> {
        let BlockStatement {
            guard_condition: _,
            header: _,
            decl,
            statements,
            end_label_pos: _,
        } = self;
        Box::new(
            decl.iter()
                .map(Node::Declaration)
                .chain(statements.iter().map(Node::Concurrent)),
        )
    }
}

impl<'a> HasNodes<'a> for ProcessStatement {
    fn children(&'a self) -> Box<dyn Iterator<Item = Node<'a>> + 'a> {
        let ProcessStatement {
            postponed: _,
            sensitivity_list: _,
            decl,
            statements,
            end_label_pos: _,
        } = self;
        Box::new(
            decl.iter()
                .map(Node::Declaration)
                .chain(statements.iter().map(Node::Sequential)),
        )
    }
}

impl<'a> HasNodes<'a> for ForGenerateStatement {
    fn children(&'a self) -> Box<dyn Iterator<Item = Node<'a>> + 'a> {
        let ForGenerateStatement {
            index_name: _,
            discrete_range: _,
            body,
            end_label_pos: _,
        } = self;
        body.children()
    }
}

impl<'a> HasNodes<'a> for IfGenerateStatement {
    fn children(&'a self) -> Box<dyn Iterator<Item = Node<'a>> + 'a> {
        let IfGenerateStatement {
            conds,
            end_label_pos: _,
        } = self;
        Box::new(
            conds
                .conditionals
                .iter()
                .flat_map(|branch| branch.item.children())
                .chain(
                    conds
                        .else_item
                        .iter()
                        .flat_map(|else_branch| else_branch.children()),
                ),
        )
    }
}

impl<'a> HasNodes<'a> for CaseGenerateStatement {
    fn children(&'a self) -> Box<dyn Iterator<Item = Node<'a>> + 'a> {
        let CaseGenerateStatement {
            sels,
            end_label_pos: _,
        } = self;
        Box::new(
            sels.alternatives
                .iter()
                .flat_map(|choice| choice.item.children()),
        )
    }
}

impl<'a> HasNodes<'a> for GenerateBody {
    fn children(&'a self) -> Box<dyn Iterator<Item = Node<'a>> + 'a> {
        let GenerateBody {
            alternative_label: _,
            decl,
            statements,
            end_label_pos: _,
        } = self;
        Box::new(
            decl.iter()
                .flat_map(|decl| decl.iter().map(Node::Declaration))
                .chain(statements.iter().map(Node::Concurrent)),
        )
    }
}

impl<'a> HasNodes<'a> for SubprogramBody {
    fn children(&'a self) -> Box<dyn Iterator<Item = Node<'a>> + 'a> {
        let SubprogramBody {
            specification: _,
            declarations,
            statements,
            end_ident_pos: _,
            span: _,
        } = self;
        Box::new(
            declarations
                .iter()
                .map(Node::Declaration)
                .chain(statements.iter().map(Node::Sequential)),
        )
    }
}

impl<'a> HasNodes<'a> for TypeDefinition {
    fn children(&'a self) -> Box<dyn Iterator<Item = Node<'a>> + 'a> {
        match self {
            TypeDefinition::Enumeration(_) => no_children(),
            TypeDefinition::Numeric(_) => no_children(),
            TypeDefinition::Physical(_) => no_children(),
            TypeDefinition::Array(_, _) => no_children(),
            TypeDefinition::Record(_) => no_children(),
            TypeDefinition::Access(_) => no_children(),
            TypeDefinition::Incomplete(_) => no_children(),
            TypeDefinition::File(_) => no_children(),
            TypeDefinition::Protected(_) => no_children(),
            TypeDefinition::ProtectedBody(body) => body.children(),
            TypeDefinition::Subtype(_) => no_children(),
        }
    }
}

impl<'a> HasNodes<'a> for ProtectedTypeBody {
    fn children(&'a self) -> Box<dyn Iterator<Item = Node<'a>> + 'a> {
        let ProtectedTypeBody { decl } = self;
        Box::new(decl.iter().map(Node::Declaration))
    }
}

fn no_children<'a>() -> Box<dyn Iterator<Item = Node<'a>> + 'a> {
    Box::new(std::iter::empty())
}
