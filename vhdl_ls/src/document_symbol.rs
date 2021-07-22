// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2021, Olof Kraigher olof.kraigher@gmail.com

use lsp_types::{DocumentSymbol, DocumentSymbolResponse, SymbolKind, Url};
use vhdl_lang::ast::*;
use vhdl_lang::Latin1String;
use vhdl_lang::{Source, SrcPos, VHDLParser, WithPos};

pub fn nested_document_symbol_response_from_file(uri: &Url) -> Option<DocumentSymbolResponse> {
    match uri.to_file_path() {
        Ok(path) => {
            let mut diagnostics = vec![];
            match VHDLParser::default().parse_design_file(&path, &mut diagnostics) {
                Ok((_, design_file)) => Some(nested_document_symbol_response(&design_file)),
                Err(_) => None,
            }
        }
        _ => None,
    }
}

pub fn nested_document_symbol_response_from_source(source: &Source) -> DocumentSymbolResponse {
    let mut diagnostics = vec![];
    let design_file = VHDLParser::default().parse_design_source(&source, &mut diagnostics);
    nested_document_symbol_response(&design_file)
}

pub fn nested_document_symbol_response(design_file: &DesignFile) -> DocumentSymbolResponse {
    let mut response = vec![];
    for design_unit in design_file.design_units.iter() {
        response.push(design_unit.document_symbol());
    }
    DocumentSymbolResponse::from(response)
}

pub trait HasDocumentSymbol {
    fn document_symbol(&self) -> DocumentSymbol;
}

impl HasDocumentSymbol for AnyDesignUnit {
    fn document_symbol(&self) -> DocumentSymbol {
        match self {
            AnyDesignUnit::Primary(ref unit) => unit.document_symbol(),
            AnyDesignUnit::Secondary(ref unit) => unit.document_symbol(),
        }
    }
}

impl HasDocumentSymbol for AnyPrimaryUnit {
    fn document_symbol(&self) -> DocumentSymbol {
        match self {
            AnyPrimaryUnit::Entity(ref unit) => unit.document_symbol(),
            AnyPrimaryUnit::Configuration(ref unit) => unit.document_symbol(),
            AnyPrimaryUnit::Package(ref unit) => unit.document_symbol(),
            AnyPrimaryUnit::PackageInstance(ref unit) => unit.document_symbol(),
            AnyPrimaryUnit::Context(ref unit) => unit.document_symbol(),
        }
    }
}

impl HasDocumentSymbol for EntityDeclaration {
    fn document_symbol(&self) -> DocumentSymbol {
        let mut children = vec![];
        push_context_clause(&self.context_clause, &mut children);
        push_optional_interface_list(
            self.generic_clause.as_ref(),
            InterfaceListType::Generic,
            &mut children,
        );
        push_optional_interface_list(
            self.port_clause.as_ref(),
            InterfaceListType::Port,
            &mut children,
        );
        push_declarations(&self.decl, &mut children);
        if let Some(ref statements) = self.statements {
            push_concurrent_statements(statements, &mut children);
        }
        let mut range = to_lsp_range(&self.range);
        if !self.context_clause.is_empty() {
            range.start = to_lsp_pos(self.context_clause[0].pos.start());
        }
        DocumentSymbol {
            name: self.ident.item.name_utf8(),
            detail: Some(String::from("entity")),
            kind: SymbolKind::Interface,
            deprecated: None,
            range,
            selection_range: to_lsp_range(&self.ident.pos),
            children: none_if_empty(children),
        }
    }
}

impl HasDocumentSymbol for ConfigurationDeclaration {
    fn document_symbol(&self) -> DocumentSymbol {
        let mut children = vec![];
        push_context_clause(&self.context_clause, &mut children);
        let mut range = to_lsp_range(&self.range);
        if !self.context_clause.is_empty() {
            range.start = to_lsp_pos(self.context_clause[0].pos.start());
        }
        DocumentSymbol {
            name: self.ident.item.name_utf8(),
            detail: Some(String::from("configuration")),
            kind: SymbolKind::Constructor,
            deprecated: None,
            range,
            selection_range: to_lsp_range(&self.ident.pos),
            children: none_if_empty(children),
        }
    }
}

impl HasDocumentSymbol for PackageDeclaration {
    fn document_symbol(&self) -> DocumentSymbol {
        let mut children = vec![];
        push_context_clause(&self.context_clause, &mut children);
        push_optional_interface_list(
            self.generic_clause.as_ref(),
            InterfaceListType::Generic,
            &mut children,
        );
        push_declarations(&self.decl, &mut children);
        let mut range = to_lsp_range(&self.range);
        if !self.context_clause.is_empty() {
            range.start = to_lsp_pos(self.context_clause[0].pos.start());
        }
        DocumentSymbol {
            name: self.ident.item.name_utf8(),
            detail: Some(String::from("package")),
            kind: SymbolKind::Package,
            deprecated: None,
            range,
            selection_range: to_lsp_range(&self.ident.pos),
            children: none_if_empty(children),
        }
    }
}

impl HasDocumentSymbol for PackageInstantiation {
    fn document_symbol(&self) -> DocumentSymbol {
        let mut children = vec![];
        push_context_clause(&self.context_clause, &mut children);
        let mut range = to_lsp_range(&self.range);
        if !self.context_clause.is_empty() {
            range.start = to_lsp_pos(self.context_clause[0].pos.start());
        }
        DocumentSymbol {
            name: self.ident.item.name_utf8(),
            detail: Some(String::from("package instance")),
            kind: SymbolKind::Package,
            deprecated: None,
            range,
            selection_range: to_lsp_range(&self.ident.pos),
            children: none_if_empty(children),
        }
    }
}

impl HasDocumentSymbol for ContextDeclaration {
    fn document_symbol(&self) -> DocumentSymbol {
        let mut children = vec![];
        push_context_clause(&self.items, &mut children);
        DocumentSymbol {
            name: self.ident.item.name_utf8(),
            detail: Some(String::from("context")),
            kind: SymbolKind::Namespace,
            deprecated: None,
            range: to_lsp_range(&self.range),
            selection_range: to_lsp_range(&self.ident.pos),
            children: none_if_empty(children),
        }
    }
}

impl HasDocumentSymbol for AnySecondaryUnit {
    fn document_symbol(&self) -> DocumentSymbol {
        match self {
            AnySecondaryUnit::PackageBody(ref unit) => unit.document_symbol(),
            AnySecondaryUnit::Architecture(ref unit) => unit.document_symbol(),
        }
    }
}

impl HasDocumentSymbol for PackageBody {
    fn document_symbol(&self) -> DocumentSymbol {
        let mut children = vec![];
        push_context_clause(&self.context_clause, &mut children);
        push_declarations(&self.decl, &mut children);
        let mut range = to_lsp_range(&self.range);
        if !self.context_clause.is_empty() {
            range.start = to_lsp_pos(self.context_clause[0].pos.start());
        }
        DocumentSymbol {
            name: self.ident.item.item.name_utf8(),
            detail: Some(String::from("package body")),
            kind: SymbolKind::Package,
            deprecated: None,
            range,
            selection_range: to_lsp_range(&self.ident.item.pos),
            children: none_if_empty(children),
        }
    }
}

impl HasDocumentSymbol for ArchitectureBody {
    fn document_symbol(&self) -> DocumentSymbol {
        let mut children = vec![];
        let mut range = to_lsp_range(&self.range);
        if !self.context_clause.is_empty() {
            range.start = to_lsp_pos(self.context_clause[0].pos.start());
        }
        push_context_clause(&self.context_clause, &mut children);
        push_declarations(&self.decl, &mut children);
        push_concurrent_statements(&self.statements, &mut children);
        DocumentSymbol {
            name: self.ident.item.name_utf8(),
            detail: Some(format!(
                "architecture of {}",
                self.entity_name.item.item.name().to_string()
            )),
            kind: SymbolKind::Class,
            deprecated: None,
            range,
            selection_range: to_lsp_range(&self.ident.pos),
            children: none_if_empty(children),
        }
    }
}

fn push_interface_list(
    list: &WithPos<Vec<InterfaceDeclaration>>,
    list_type: InterfaceListType,
    symbols: &mut Vec<DocumentSymbol>,
) {
    if !list.item.is_empty() {
        let (name, kind) = match list_type {
            InterfaceListType::Port => (String::from("ports"), SymbolKind::Interface),
            InterfaceListType::Generic => (String::from("generics"), SymbolKind::Constant),
            InterfaceListType::Parameter => (String::from("parameters"), SymbolKind::Interface),
        };
        symbols.push(DocumentSymbol {
            name,
            detail: None,
            kind,
            deprecated: None,
            range: to_lsp_range(&list.pos),
            selection_range: lsp_types::Range {
                start: to_lsp_pos(list.pos.start()),
                end: to_lsp_pos(list.pos.start()),
            },
            children: none_if_empty(list.item.iter().map(|x| x.document_symbol()).collect()),
        });
    }
}

fn push_optional_interface_list(
    list: Option<&WithPos<Vec<InterfaceDeclaration>>>,
    list_type: InterfaceListType,
    symbols: &mut Vec<DocumentSymbol>,
) {
    if let Some(list) = list {
        push_interface_list(list, list_type, symbols);
    }
}

fn push_declarations(list: &WithPos<Vec<Declaration>>, symbols: &mut Vec<DocumentSymbol>) {
    if !list.item.is_empty() {
        let range = to_lsp_range(&list.pos);
        symbols.push(DocumentSymbol {
            name: String::from("declarations"),
            detail: None,
            kind: SymbolKind::Field,
            deprecated: None,
            range,
            selection_range: lsp_types::Range {
                start: range.start,
                end: range.start,
            },
            children: Some(
                list.item
                    .iter()
                    .map(|decl| decl.document_symbol())
                    .collect(),
            ),
        });
    }
}

fn push_concurrent_statements(
    statements: &WithPos<Vec<LabeledConcurrentStatement>>,
    symbols: &mut Vec<DocumentSymbol>,
) {
    let range = to_lsp_range(&statements.pos);
    symbols.push(DocumentSymbol {
        name: String::from("statements"),
        detail: None,
        kind: SymbolKind::Field,
        deprecated: None,
        range,
        selection_range: lsp_types::Range {
            start: range.start,
            end: range.start,
        },
        children: Some(
            statements
                .item
                .iter()
                .map(|stmt| stmt.document_symbol())
                .collect(),
        ),
    });
}

impl HasDocumentSymbol for InterfaceDeclaration {
    fn document_symbol(&self) -> DocumentSymbol {
        match self {
            InterfaceDeclaration::Object(ref object) => object.document_symbol(),
            InterfaceDeclaration::File(ref file) => file.document_symbol(),
            InterfaceDeclaration::Type(ref ident) => DocumentSymbol {
                name: ident.item.name_utf8(),
                detail: Some(String::from("Type")),
                kind: SymbolKind::TypeParameter,
                deprecated: None,
                range: to_lsp_range(&ident.pos),
                selection_range: to_lsp_range(&ident.pos),
                children: None,
            },
            InterfaceDeclaration::Subprogram(ref subprogram_declaration, _) => {
                subprogram_declaration.document_symbol()
            }
            InterfaceDeclaration::Package(ref package) => package.document_symbol(),
        }
    }
}

impl HasDocumentSymbol for InterfaceObjectDeclaration {
    fn document_symbol(&self) -> DocumentSymbol {
        let mode = if self.class == ObjectClass::Constant {
            String::from("")
        } else {
            mode_to_string(self.mode)
        };
        DocumentSymbol {
            name: self.ident.item.name_utf8(),
            detail: Some(format!(
                "{} {}",
                mode,
                subtype_indication_designator_to_string(&self.subtype_indication)
            )),
            kind: symbol_kind_from_object_class(self.class),
            deprecated: None,
            range: to_lsp_range(&self.ident.pos),
            selection_range: to_lsp_range(&self.ident.pos),
            children: None,
        }
    }
}

impl HasDocumentSymbol for InterfaceFileDeclaration {
    fn document_symbol(&self) -> DocumentSymbol {
        DocumentSymbol {
            name: self.ident.item.name_utf8(),
            detail: Some(subtype_indication_designator_to_string(
                &self.subtype_indication,
            )),
            kind: SymbolKind::File,
            deprecated: None,
            range: to_lsp_range(&self.ident.pos),
            selection_range: to_lsp_range(&self.ident.pos),
            children: None,
        }
    }
}

impl HasDocumentSymbol for SubprogramDeclaration {
    fn document_symbol(&self) -> DocumentSymbol {
        match self {
            SubprogramDeclaration::Procedure(procedure) => DocumentSymbol {
                name: match procedure.designator.item {
                    SubprogramDesignator::Identifier(ref symbol) => symbol.name_utf8(),
                    SubprogramDesignator::OperatorSymbol(ref latin1string) => {
                        format!("\"{}\"", latin1string.to_string())
                    }
                },
                detail: Some(String::from("procedure")),
                kind: SymbolKind::Method,
                deprecated: None,
                range: to_lsp_range(&procedure.designator.pos),
                selection_range: to_lsp_range(&procedure.designator.pos),
                children: None,
            },
            SubprogramDeclaration::Function(function) => DocumentSymbol {
                name: match function.designator.item {
                    SubprogramDesignator::Identifier(ref symbol) => symbol.name_utf8(),
                    SubprogramDesignator::OperatorSymbol(ref latin1string) => {
                        format!("\"{}\"", latin1string.to_string())
                    }
                },
                detail: Some(String::from("function")),
                kind: SymbolKind::Function,
                deprecated: None,
                range: to_lsp_range(&function.designator.pos),
                selection_range: to_lsp_range(&function.designator.pos),
                children: None,
            },
        }
    }
}

impl HasDocumentSymbol for WithPos<UseClause> {
    fn document_symbol(&self) -> DocumentSymbol {
        if self.item.name_list.len() == 1 {
            DocumentSymbol {
                name: name_to_string(&self.item.name_list[0].item),
                detail: Some(String::from("use")),
                kind: SymbolKind::Namespace,
                deprecated: None,
                range: to_lsp_range(&self.pos),
                selection_range: to_lsp_range(&self.item.name_list[0].pos),
                children: None,
            }
        } else {
            DocumentSymbol {
                name: String::from("use"),
                detail: None,
                kind: SymbolKind::Namespace,
                deprecated: None,
                range: to_lsp_range(&self.pos),
                selection_range: lsp_types::Range {
                    start: to_lsp_pos(self.pos.start()),
                    end: to_lsp_pos(self.pos.start()),
                },
                children: none_if_empty(
                    self.item
                        .name_list
                        .iter()
                        .map(|x| DocumentSymbol {
                            name: name_to_string(&x.item),
                            detail: Some(String::from("use")),
                            kind: SymbolKind::Namespace,
                            deprecated: None,
                            range: to_lsp_range(&x.pos),
                            selection_range: to_lsp_range(&x.pos),
                            children: None,
                        })
                        .collect(),
                ),
            }
        }
    }
}

impl HasDocumentSymbol for InterfacePackageDeclaration {
    fn document_symbol(&self) -> DocumentSymbol {
        DocumentSymbol {
            name: self.ident.item.name_utf8(),
            detail: Some(String::from("Package")),
            kind: SymbolKind::Package,
            deprecated: None,
            range: to_lsp_range(&self.ident.pos),
            selection_range: to_lsp_range(&self.ident.pos),
            children: None,
        }
    }
}

impl HasDocumentSymbol for Declaration {
    fn document_symbol(&self) -> DocumentSymbol {
        match self {
            Declaration::Object(object) => object.document_symbol(),
            Declaration::File(file) => file.document_symbol(),
            Declaration::Type(type_decl) => type_decl.document_symbol(),
            Declaration::Component(component) => component.document_symbol(),
            Declaration::Attribute(attribute) => attribute.document_symbol(),
            Declaration::Alias(alias) => alias.document_symbol(),
            Declaration::SubprogramDeclaration(subprogram) => subprogram.document_symbol(),
            Declaration::SubprogramBody(subprogram) => subprogram.specification.document_symbol(),
            Declaration::Use(use_decl) => use_decl.document_symbol(),
            Declaration::Package(package) => package.document_symbol(),
            Declaration::Configuration(configuration) => configuration.document_symbol(),
        }
    }
}

impl HasDocumentSymbol for ObjectDeclaration {
    fn document_symbol(&self) -> DocumentSymbol {
        DocumentSymbol {
            name: self.ident.item.name_utf8(),
            detail: None,
            kind: symbol_kind_from_object_class(self.class),
            deprecated: None,
            range: to_lsp_range(&self.ident.pos),
            selection_range: to_lsp_range(&self.ident.pos),
            children: None,
        }
    }
}

impl HasDocumentSymbol for FileDeclaration {
    fn document_symbol(&self) -> DocumentSymbol {
        DocumentSymbol {
            name: self.ident.item.name_utf8(),
            detail: None,
            kind: SymbolKind::File,
            deprecated: None,
            range: to_lsp_range(&self.ident.pos),
            selection_range: to_lsp_range(&self.ident.pos),
            children: None,
        }
    }
}

impl HasDocumentSymbol for TypeDeclaration {
    fn document_symbol(&self) -> DocumentSymbol {
        DocumentSymbol {
            name: self.ident.item.name_utf8(),
            detail: Some(String::from("type")),
            kind: symbol_kind_from_type_definition(&self.def),
            deprecated: None,
            range: to_lsp_range(&self.ident.pos),
            selection_range: to_lsp_range(&self.ident.pos),
            children: None,
        }
    }
}

impl HasDocumentSymbol for ComponentDeclaration {
    fn document_symbol(&self) -> DocumentSymbol {
        DocumentSymbol {
            name: self.ident.item.name_utf8(),
            detail: Some(String::from("component")),
            kind: SymbolKind::Interface,
            deprecated: None,
            range: to_lsp_range(&self.ident.pos),
            selection_range: to_lsp_range(&self.ident.pos),
            children: None,
        }
    }
}

impl HasDocumentSymbol for Attribute {
    fn document_symbol(&self) -> DocumentSymbol {
        match self {
            Attribute::Specification(spec) => DocumentSymbol {
                name: spec.ident.item.name_utf8(),
                detail: Some(String::from("attribute")),
                kind: SymbolKind::Property,
                deprecated: None,
                range: to_lsp_range(&spec.ident.pos),
                selection_range: to_lsp_range(&spec.ident.pos),
                children: None,
            },
            Attribute::Declaration(decl) => DocumentSymbol {
                name: decl.ident.item.name_utf8(),
                detail: Some(String::from("attribute")),
                kind: SymbolKind::Property,
                deprecated: None,
                range: to_lsp_range(&decl.ident.pos),
                selection_range: to_lsp_range(&decl.ident.pos),
                children: None,
            },
        }
    }
}

impl HasDocumentSymbol for AliasDeclaration {
    fn document_symbol(&self) -> DocumentSymbol {
        DocumentSymbol {
            name: designator_name_to_string(&self.designator.item),
            detail: Some(String::from("alias")),
            kind: SymbolKind::TypeParameter,
            deprecated: None,
            range: to_lsp_range(&self.designator.pos),
            selection_range: to_lsp_range(&self.designator.pos),
            children: None,
        }
    }
}

impl HasDocumentSymbol for ConfigurationSpecification {
    fn document_symbol(&self) -> DocumentSymbol {
        DocumentSymbol {
            name: String::from("configuration"),
            detail: None,
            kind: SymbolKind::Constructor,
            deprecated: None,
            range: to_lsp_range(&self.spec.component_name.pos),
            selection_range: to_lsp_range(&self.spec.component_name.pos),
            children: None,
        }
    }
}

impl HasDocumentSymbol for LabeledConcurrentStatement {
    fn document_symbol(&self) -> DocumentSymbol {
        let mut symbol = self.statement.document_symbol();
        if let Some(ref label) = self.label {
            symbol.name = label.item.name_utf8();
            symbol.range = lsp_types::Range::new(to_lsp_pos(label.pos.start()), symbol.range.end);
            symbol.selection_range = to_lsp_range(&label.pos);
        }
        symbol
    }
}

impl HasDocumentSymbol for ConcurrentStatement {
    fn document_symbol(&self) -> DocumentSymbol {
        match &self {
            ConcurrentStatement::ProcedureCall(stmt) => stmt.document_symbol(),
            ConcurrentStatement::Block(stmt) => stmt.document_symbol(),
            ConcurrentStatement::Process(stmt) => stmt.document_symbol(),
            ConcurrentStatement::Assert(stmt) => stmt.document_symbol(),
            ConcurrentStatement::Assignment(stmt) => stmt.document_symbol(),
            ConcurrentStatement::Instance(stmt) => stmt.document_symbol(),
            ConcurrentStatement::ForGenerate(stmt) => stmt.document_symbol(),
            ConcurrentStatement::IfGenerate(stmt) => stmt.document_symbol(),
            ConcurrentStatement::CaseGenerate(stmt) => stmt.document_symbol(),
        }
    }
}

impl HasDocumentSymbol for ConcurrentProcedureCall {
    fn document_symbol(&self) -> DocumentSymbol {
        DocumentSymbol {
            name: name_to_string(&self.call.name.item),
            detail: Some(String::from("procedure call")),
            kind: SymbolKind::Method,
            deprecated: None,
            range: to_lsp_range(&self.call.name.pos),
            selection_range: to_lsp_range(&self.call.name.pos),
            children: None,
        }
    }
}

// TODO: Add children from generics, ports, declarations and statements
impl HasDocumentSymbol for BlockStatement {
    fn document_symbol(&self) -> DocumentSymbol {
        let block_start = to_lsp_pos(self.range.start());
        DocumentSymbol {
            name: String::from(" "),
            detail: Some(String::from("block")),
            kind: SymbolKind::Module,
            deprecated: None,
            range: to_lsp_range(&self.range),
            selection_range: lsp_types::Range {
                start: block_start,
                end: block_start,
            },
            children: None,
        }
    }
}

impl HasDocumentSymbol for ProcessStatement {
    fn document_symbol(&self) -> DocumentSymbol {
        let start_pos = to_lsp_pos(self.range.start());
        DocumentSymbol {
            name: String::from(" "),
            detail: Some(String::from("process")),
            kind: SymbolKind::Event,
            deprecated: None,
            range: to_lsp_range(&self.range),
            selection_range: lsp_types::Range {
                start: start_pos,
                end: start_pos,
            },
            children: None,
        }
    }
}

impl HasDocumentSymbol for ConcurrentAssertStatement {
    fn document_symbol(&self) -> DocumentSymbol {
        DocumentSymbol {
            name: String::from(" "),
            detail: Some(String::from("assertion")),
            kind: SymbolKind::Field,
            deprecated: None,
            range: to_lsp_range(&self.statement.condition.pos),
            selection_range: to_lsp_range(&self.statement.condition.pos),
            children: None,
        }
    }
}

impl HasDocumentSymbol for ConcurrentSignalAssignment {
    fn document_symbol(&self) -> DocumentSymbol {
        let name = match &self.target.item {
            Target::Name(name) => name.to_string(),
            Target::Aggregate(aggregate) => aggregate
                .iter()
                .map(|x| match x {
                    ElementAssociation::Positional(pos) => pos.item.to_string(),
                    ElementAssociation::Named(_, pos) => pos.item.to_string(),
                })
                .collect::<Vec<String>>()
                .join(", "),
        };
        DocumentSymbol {
            name,
            detail: Some(String::from("assignment")),
            kind: SymbolKind::Field,
            deprecated: None,
            range: to_lsp_range(&self.target.pos),
            selection_range: to_lsp_range(&self.target.pos),
            children: None,
        }
    }
}

impl HasDocumentSymbol for InstantiationStatement {
    fn document_symbol(&self) -> DocumentSymbol {
        let name = String::from("instantiation");
        match &self.unit {
            InstantiatedUnit::Component(component_name) => DocumentSymbol {
                name,
                detail: Some(format!("{} : component", component_name.item)),
                kind: SymbolKind::Object,
                deprecated: None,
                range: to_lsp_range(&component_name.pos),
                selection_range: to_lsp_range(&component_name.pos),
                children: None,
            },
            InstantiatedUnit::Entity(entity_name, architecture_name) => DocumentSymbol {
                name,
                detail: Some(format!(
                    "{}{} : entity",
                    entity_name.item,
                    architecture_name
                        .as_ref()
                        .map_or(String::from(""), |arch| format!("({})", arch))
                )),
                kind: SymbolKind::Object,
                deprecated: None,
                range: to_lsp_range(&entity_name.pos),
                selection_range: to_lsp_range(&entity_name.pos),
                children: None,
            },
            InstantiatedUnit::Configuration(configuration_name) => DocumentSymbol {
                name,
                detail: Some(format!("{} : configuration", configuration_name.item)),
                kind: SymbolKind::Object,
                deprecated: None,
                range: to_lsp_range(&configuration_name.pos),
                selection_range: to_lsp_range(&configuration_name.pos),
                children: None,
            },
        }
    }
}

impl HasDocumentSymbol for ForGenerateStatement {
    fn document_symbol(&self) -> DocumentSymbol {
        DocumentSymbol {
            name: String::from(" "),
            detail: Some(String::from("for generate")),
            kind: SymbolKind::Field,
            deprecated: None,
            range: to_lsp_range(&self.index_name.pos),
            selection_range: to_lsp_range(&self.index_name.pos),
            children: None,
        }
    }
}

impl HasDocumentSymbol for IfGenerateStatement {
    fn document_symbol(&self) -> DocumentSymbol {
        let range = if let Some(cond) = self.conditionals.first() {
            to_lsp_range(&cond.condition.pos)
        } else {
            lsp_types::Range {
                start: lsp_types::Position {
                    line: 0,
                    character: 0,
                },
                end: lsp_types::Position {
                    line: 0,
                    character: 0,
                },
            }
        };
        DocumentSymbol {
            name: String::from(" "),
            detail: Some(String::from("if generate")),
            kind: SymbolKind::Field,
            deprecated: None,
            range,
            selection_range: range,
            children: None,
        }
    }
}

impl HasDocumentSymbol for CaseGenerateStatement {
    fn document_symbol(&self) -> DocumentSymbol {
        let range = to_lsp_range(&self.expression.pos);
        DocumentSymbol {
            name: String::from(" "),
            detail: Some(String::from("case generate")),
            kind: SymbolKind::Field,
            deprecated: None,
            range,
            selection_range: range,
            children: None,
        }
    }
}

fn symbol_kind_from_type_definition(type_definition: &TypeDefinition) -> SymbolKind {
    match type_definition {
        TypeDefinition::Enumeration(_) => SymbolKind::Enum,
        TypeDefinition::Integer(_) => SymbolKind::TypeParameter,
        TypeDefinition::Physical(_) => SymbolKind::TypeParameter,
        TypeDefinition::Array(_, _) => SymbolKind::Array,
        TypeDefinition::Record(_) => SymbolKind::Struct,
        TypeDefinition::Access(_) => SymbolKind::Key,
        TypeDefinition::Incomplete(_) => SymbolKind::TypeParameter,
        TypeDefinition::File(_) => SymbolKind::File,
        TypeDefinition::Protected(_) => SymbolKind::Object,
        TypeDefinition::ProtectedBody(_) => SymbolKind::Object,
        TypeDefinition::Subtype(_) => SymbolKind::TypeParameter,
    }
}

fn name_to_string(name: &Name) -> String {
    match name {
        Name::Designator(designator) => designator_name_to_string(&designator.item),
        Name::Selected(name, designator) => format!(
            "{}.{}",
            name_to_string(&name.item),
            designator_name_to_string(&designator.item.item)
        ),
        Name::SelectedAll(name) => format!("{}.{}", name_to_string(&name.item), "all"),
        _ => String::from(" "),
    }
}

fn designator_name_to_string(designator: &Designator) -> String {
    match designator {
        Designator::Identifier(symbol) => symbol.name_utf8(),
        Designator::OperatorSymbol(symbol) => format!("\"{}\"", symbol.to_string()),
        Designator::Character(character) => {
            format!("'{}'", Latin1String::new(&[character.to_owned()]))
        }
    }
}

fn mode_to_string(mode: Mode) -> String {
    String::from(match mode {
        Mode::In => "in",
        Mode::Out => "out",
        Mode::InOut => "inout",
        Mode::Buffer => "buffer",
        Mode::Linkage => "linkage",
    })
}

fn subtype_indication_designator_to_string(subtype_indication: &SubtypeIndication) -> String {
    match &subtype_indication.type_mark.item {
        SelectedName::Designator(name) => designator_name_to_string(&name.item),
        SelectedName::Selected(_, name) => designator_name_to_string(&name.item.item),
    }
}

fn to_lsp_pos(position: vhdl_lang::Position) -> lsp_types::Position {
    lsp_types::Position {
        line: position.line as u64,
        character: position.character as u64,
    }
}

fn to_lsp_range(src_pos: &SrcPos) -> lsp_types::Range {
    let range = src_pos.range();
    lsp_types::Range {
        start: to_lsp_pos(range.start),
        end: to_lsp_pos(range.end),
    }
}

fn symbol_kind_from_object_class(object_class: ObjectClass) -> SymbolKind {
    match object_class {
        ObjectClass::Signal => SymbolKind::Field,
        ObjectClass::Constant => SymbolKind::Constant,
        ObjectClass::Variable => SymbolKind::Variable,
        ObjectClass::SharedVariable => SymbolKind::Variable,
    }
}

fn none_if_empty<T>(vec: Vec<T>) -> Option<Vec<T>> {
    if vec.is_empty() {
        None
    } else {
        Some(vec)
    }
}

fn push_context_clause(context_clause: &[WithPos<ContextItem>], symbols: &mut Vec<DocumentSymbol>) {
    if let Some(ref first) = context_clause.first() {
        let range = first
            .pos
            .clone()
            .combine_into(context_clause.last().unwrap());
        let mut children = vec![];
        for context_item in context_clause.iter() {
            match &context_item.item {
                ContextItem::Use(use_clause) => {
                    for name in use_clause.name_list.iter() {
                        children.push(DocumentSymbol {
                            name: name_to_string(&name.item),
                            detail: Some(String::from("use")),
                            kind: SymbolKind::Namespace,
                            deprecated: None,
                            range: to_lsp_range(&name.pos),
                            selection_range: to_lsp_range(&name.pos),
                            children: None,
                        });
                    }
                }
                ContextItem::Library(library_clause) => {
                    for name in library_clause.name_list.iter() {
                        children.push(DocumentSymbol {
                            name: name.item.name_utf8(),
                            detail: Some(String::from("library")),
                            kind: SymbolKind::Namespace,
                            deprecated: None,
                            range: to_lsp_range(&name.pos),
                            selection_range: to_lsp_range(&name.pos),
                            children: None,
                        });
                    }
                }
                ContextItem::Context(context_reference) => {
                    for name in context_reference.name_list.iter() {
                        children.push(DocumentSymbol {
                            name: name_to_string(&name.item),
                            detail: Some(String::from("context")),
                            kind: SymbolKind::Namespace,
                            deprecated: None,
                            range: to_lsp_range(&name.pos),
                            selection_range: to_lsp_range(&name.pos),
                            children: None,
                        });
                    }
                }
            };
        }
        symbols.push(DocumentSymbol {
            name: String::from("context"),
            detail: None,
            kind: SymbolKind::Namespace,
            deprecated: None,
            range: to_lsp_range(&range),
            selection_range: to_lsp_range(&first.pos),
            children: none_if_empty(children),
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lsp_types;
    use pretty_assertions::assert_eq;
    use std::convert::TryInto;
    use std::io::Write;
    use std::path::Path;
    use tempfile::NamedTempFile;
    use vhdl_lang::Source;

    fn parse_str(code: &str) -> DesignFile {
        let mut diagnostics = vec![];
        let source = Source::inline(Path::new("mockpath"), code);
        let parser = VHDLParser::default();
        let design_file = parser.parse_design_source(&source, &mut diagnostics);
        for err in diagnostics.iter() {
            println!("{}", err.show());
        }
        if diagnostics.len() > 0 {
            panic!("Found errors");
        }
        design_file
    }

    fn write_source_file(code: &str) -> (Url, NamedTempFile) {
        let mut file = NamedTempFile::new().unwrap();
        file.write_all(code.as_bytes()).unwrap();
        (
            Url::from_file_path(file.path().canonicalize().unwrap()).unwrap(),
            file,
        )
    }

    fn range(code: &str, start: &str, end: &str) -> lsp_types::Range {
        find_range(code, start, 1, end, true)
    }

    fn range_between(code: &str, start: &str, end: &str) -> lsp_types::Range {
        find_range(code, start, 1, end, false)
    }

    fn find_range(
        code: &str,
        start: &str,
        start_occurance: usize,
        end: &str,
        inclusive: bool,
    ) -> lsp_types::Range {
        let mut start_line = 0;
        let mut start_column = 0;
        let mut found_start = false;
        let mut end_line = 0;
        let mut end_column = 0;
        let mut line_number = 0;
        let mut occurance = 0;
        for line in code.lines() {
            if !found_start {
                if let Some(pos) = line.find(start) {
                    occurance += 1;
                    if occurance == start_occurance {
                        start_column = pos;
                        if !inclusive {
                            start_column = start_column + start.len();
                        }
                        start_line = line_number;
                        found_start = true;
                    }
                }
            }
            if found_start {
                if let Some(pos) = line.find(end) {
                    end_column = pos;
                    if inclusive {
                        end_column = end_column + end.len();
                    }
                    end_line = line_number;
                    break;
                }
            }
            line_number += 1;
        }

        lsp_types::Range {
            start: lsp_types::Position {
                line: start_line.try_into().unwrap(),
                character: start_column.try_into().unwrap(),
            },
            end: lsp_types::Position {
                line: end_line.try_into().unwrap(),
                character: end_column.try_into().unwrap(),
            },
        }
    }

    fn range1(code: &str, start: &str) -> lsp_types::Range {
        range(code, start, start)
    }

    fn ieee_context(code: &str) -> DocumentSymbol {
        DocumentSymbol {
            name: String::from("context"),
            detail: None,
            kind: SymbolKind::Namespace,
            deprecated: None,
            range: range(code, "library ieee", "ieee;"),
            selection_range: range(code, "library ieee", "ieee;"),
            children: Some(vec![DocumentSymbol {
                name: String::from("ieee"),
                detail: Some(String::from("library")),
                kind: SymbolKind::Namespace,
                deprecated: None,
                range: range1(code, "ieee"),
                selection_range: range1(code, "ieee"),
                children: None,
            }]),
        }
    }

    fn simple_generic(code: &str) -> DocumentSymbol {
        DocumentSymbol {
            name: String::from("generics"),
            detail: None,
            kind: SymbolKind::Constant,
            deprecated: None,
            range: range(code, "generic(", ");"),
            selection_range: lsp_types::Range {
                start: range1(code, "generic").start,
                end: range1(code, "generic").start,
            },
            children: Some(vec![DocumentSymbol {
                name: String::from("g1"),
                detail: Some(String::from(" integer")),
                kind: SymbolKind::Constant,
                deprecated: None,
                range: range1(code, "g1"),
                selection_range: range1(code, "g1"),
                children: None,
            }]),
        }
    }

    fn simple_port(code: &str) -> DocumentSymbol {
        DocumentSymbol {
            name: String::from("ports"),
            detail: None,
            kind: SymbolKind::Interface,
            deprecated: None,
            range: range(code, "port(", ");"),
            selection_range: lsp_types::Range {
                start: range1(code, "port").start,
                end: range1(code, "port").start,
            },
            children: Some(vec![DocumentSymbol {
                name: String::from("p1"),
                detail: Some(String::from("in integer")),
                kind: SymbolKind::Field,
                deprecated: None,
                range: range1(code, "p1"),
                selection_range: range1(code, "p1"),
                children: None,
            }]),
        }
    }

    fn simple_declaration(
        code: &str,
        start: &str,
        start_occurance: usize,
        begin_is_end: bool,
    ) -> DocumentSymbol {
        let end = if begin_is_end { "begin" } else { "end" };
        let range = find_range(code, start, start_occurance, end, false);
        DocumentSymbol {
            name: String::from("declarations"),
            detail: None,
            kind: SymbolKind::Field,
            deprecated: None,
            range,
            selection_range: lsp_types::Range {
                start: range.start,
                end: range.start,
            },
            children: Some(vec![DocumentSymbol {
                name: String::from("decl1"),
                detail: None,
                kind: SymbolKind::Field,
                deprecated: None,
                range: range1(code, "decl1"),
                selection_range: range1(code, "decl1"),
                children: None,
            }]),
        }
    }

    fn simple_statement(code: &str, starts_with_begin: bool) -> DocumentSymbol {
        let start = if starts_with_begin { "begin" } else { "end" };
        let stmt_range = range_between(code, start, "end");
        DocumentSymbol {
            name: String::from("statements"),
            detail: None,
            kind: SymbolKind::Field,
            deprecated: None,
            range: stmt_range,
            selection_range: lsp_types::Range {
                start: stmt_range.start,
                end: stmt_range.start,
            },
            children: Some(vec![DocumentSymbol {
                name: String::from("stmt1"),
                detail: Some(String::from("assignment")),
                kind: SymbolKind::Field,
                deprecated: None,
                range: range(code, "stmt1", "decl1"),
                selection_range: range1(code, "stmt1"),
                children: None,
            }]),
        }
    }

    #[test]
    fn entity_declaration() {
        let code = "
library ieee;

entity ent1 is
    generic(
        g1 : integer := 3
    );
    port(
        p1 : integer
    );
    signal decl1 : integer;
begin
    stmt1: decl1 <= 1;
end;
";
        let design_file = parse_str(code);
        let unit = design_file.design_units.first().unwrap();
        assert_eq!(
            unit.document_symbol(),
            DocumentSymbol {
                name: String::from("ent1"),
                detail: Some(String::from("entity")),
                kind: SymbolKind::Interface,
                deprecated: None,
                range: range(code, "library", "end;"),
                selection_range: range1(code, "ent1"),
                children: Some(vec![
                    ieee_context(code),
                    simple_generic(code),
                    simple_port(code),
                    simple_declaration(code, ");", 2, true),
                    simple_statement(code, true),
                ]),
            }
        );
    }

    #[test]
    fn configuration_declaration() {
        let code = "
library ieee;
configuration cfg of entity_name is
    for rtl(0)
    end for;
end;
";
        let design_file = parse_str(code);
        let unit = design_file.design_units.first().unwrap();
        assert_eq!(
            unit.document_symbol(),
            DocumentSymbol {
                name: String::from("cfg"),
                detail: Some(String::from("configuration")),
                kind: SymbolKind::Constructor,
                deprecated: None,
                range: range(code, "library", "end;"),
                selection_range: range1(code, "cfg"),
                children: Some(vec![ieee_context(code)]),
            }
        );
    }

    #[test]
    fn package_declaration() {
        let code = "
library ieee;
package pkg is
    generic(
        g1 : integer := 3
    );
    signal decl1 : integer;
end;
";
        let design_file = parse_str(code);
        let unit = design_file.design_units.first().unwrap();
        assert_eq!(
            unit.document_symbol(),
            DocumentSymbol {
                name: String::from("pkg"),
                detail: Some(String::from("package")),
                kind: SymbolKind::Package,
                deprecated: None,
                range: range(code, "library", "end;"),
                selection_range: range1(code, "pkg"),
                children: Some(vec![
                    ieee_context(code),
                    simple_generic(code),
                    simple_declaration(code, ");", 1, false),
                ]),
            }
        );
    }

    #[test]
    fn package_instantiation() {
        let code = "
library ieee;
package pkg_inst is new work.pkg
    generic map(
        gen => 1
    );
";
        let design_file = parse_str(code);
        let unit = design_file.design_units.first().unwrap();
        assert_eq!(
            unit.document_symbol(),
            DocumentSymbol {
                name: String::from("pkg_inst"),
                detail: Some(String::from("package instance")),
                kind: SymbolKind::Package,
                deprecated: None,
                range: range(code, "library", ");"),
                selection_range: range1(code, "pkg_inst"),
                children: Some(vec![ieee_context(code)]),
            }
        );
    }

    #[test]
    fn context_declaration() {
        let code = "
context ctx is
    library ieee;
end;
";
        let design_file = parse_str(code);
        let unit = design_file.design_units.first().unwrap();
        assert_eq!(
            unit.document_symbol(),
            DocumentSymbol {
                name: String::from("ctx"),
                detail: Some(String::from("context")),
                kind: SymbolKind::Namespace,
                deprecated: None,
                range: range(code, "context", "end;"),
                selection_range: range1(code, "ctx"),
                children: Some(vec![ieee_context(code)]),
            }
        );
    }

    #[test]
    fn package_body() {
        let code = "
library ieee;
package body pkg is
    signal decl1 : integer;
end;
";
        let design_file = parse_str(code);
        let unit = design_file.design_units.first().unwrap();
        assert_eq!(
            unit.document_symbol(),
            DocumentSymbol {
                name: String::from("pkg"),
                detail: Some(String::from("package body")),
                kind: SymbolKind::Package,
                deprecated: None,
                range: range(code, "library", "end;"),
                selection_range: range1(code, "pkg"),
                children: Some(vec![
                    ieee_context(code),
                    simple_declaration(code, "is", 1, false),
                ]),
            }
        );
    }

    #[test]
    fn architecture_body() {
        let code = "
library ieee;
architecture rtl of ent is
    
    signal decl1 : integer;
    
begin

    stmt1: decl1 <= 1;

end architecture;
";
        let design_file = parse_str(code);
        let unit = design_file.design_units.first().unwrap();
        assert_eq!(
            unit.document_symbol(),
            DocumentSymbol {
                name: String::from("rtl"),
                detail: Some(String::from("architecture of ent")),
                kind: SymbolKind::Class,
                deprecated: None,
                range: range(code, "library", "end architecture;"),
                selection_range: range1(code, "rtl"),
                children: Some(vec![
                    ieee_context(code),
                    simple_declaration(code, "is", 1, true),
                    simple_statement(code, true)
                ]),
            }
        );
    }

    fn declaration(
        code: &str,
        name: &str,
        detail: Option<&str>,
        kind: SymbolKind,
    ) -> DocumentSymbol {
        DocumentSymbol {
            name: String::from(name),
            detail: detail.map(|detail| String::from(detail)),
            kind,
            deprecated: None,
            range: range1(code, name),
            selection_range: range1(code, name),
            children: None,
        }
    }

    fn declaration2(
        name: &str,
        detail: Option<&str>,
        kind: SymbolKind,
        range: lsp_types::Range,
        selection_range: lsp_types::Range,
    ) -> DocumentSymbol {
        DocumentSymbol {
            name: String::from(name),
            detail: detail.map(|detail| String::from(detail)),
            kind,
            deprecated: None,
            range,
            selection_range,
            children: None,
        }
    }

    fn get_declarations(code: &str) -> Vec<DocumentSymbol> {
        let design_file = parse_str(code);
        let document_symbol = design_file.design_units.first().unwrap().document_symbol();
        let mut declarations = None;
        for child in document_symbol.children.unwrap().iter() {
            if child.name == "declarations" {
                declarations = Some(child.children.clone().unwrap());
                break;
            }
        }
        declarations.unwrap()
    }

    #[test]
    fn object_declaration() {
        let code = "
package pkg is
    signal          o_signal          : integer;
    constant        o_constant        : integer := 0;
    variable        o_variable        : integer;
    shared variable o_shared_variable : integer;
end;
";
        assert_eq!(
            get_declarations(code),
            vec![
                declaration(code, "o_signal", None, SymbolKind::Field),
                declaration(code, "o_constant", None, SymbolKind::Constant),
                declaration(code, "o_variable", None, SymbolKind::Variable),
                declaration(code, "o_shared_variable", None, SymbolKind::Variable),
            ]
        )
    }

    #[test]
    fn file_declaration() {
        let code = "
package pkg is
    file o_file : integer;
end;
";
        assert_eq!(
            get_declarations(code),
            vec![declaration(code, "o_file", None, SymbolKind::File),]
        )
    }

    #[test]
    fn type_declaration() {
        let code = "
package pkg is
    type t_enum is (a, b, c);
    type t_integer is range 0 to 1;
    type t_physical is range 0 to 1e4 units
        a;
        b = 1000 a;
    end units t_physical;
    type t_array is array (natural range <>) of integer;
    type t_record is record
        a : integer;
    end record t_record;
    type t_access is access integer;
    type t_incomp;
    type t_file is file of integer;
    type t_protected is protected
    end protected t_protected;
    type t_protected is protected body
    end protected body t_protected;
    subtype t_subtype is integer range 1 to 2;
end;
";
        assert_eq!(
            get_declarations(code),
            vec![
                declaration(code, "t_enum", Some("type"), SymbolKind::Enum),
                declaration(code, "t_integer", Some("type"), SymbolKind::TypeParameter),
                declaration(code, "t_physical", Some("type"), SymbolKind::TypeParameter),
                declaration(code, "t_array", Some("type"), SymbolKind::Array),
                declaration(code, "t_record", Some("type"), SymbolKind::Struct),
                declaration(code, "t_access", Some("type"), SymbolKind::Key),
                declaration(code, "t_incomp", Some("type"), SymbolKind::TypeParameter),
                declaration(code, "t_file", Some("type"), SymbolKind::File),
                declaration(code, "t_protected", Some("type"), SymbolKind::Object),
                {
                    let mut protected_body =
                        declaration(code, "t_protected", Some("type"), SymbolKind::Object);
                    protected_body.range.start.line += 2;
                    protected_body.range.end.line += 2;
                    protected_body.selection_range.start.line += 2;
                    protected_body.selection_range.end.line += 2;
                    protected_body
                },
                declaration(code, "t_subtype", Some("type"), SymbolKind::TypeParameter),
            ]
        )
    }

    #[test]
    fn component_declaration() {
        let code = "
package pkg is
    component m_component is end component;
end;
";
        assert_eq!(
            get_declarations(code),
            vec![declaration(
                code,
                "m_component",
                Some("component"),
                SymbolKind::Interface,
            ),]
        )
    }

    #[test]
    fn attribute() {
        let code = "
package pkg is
    attribute m_attribute : integer;
    attribute m_attribute of m_component : component is 0;
end;
";
        assert_eq!(
            get_declarations(code),
            vec![
                declaration(code, "m_attribute", Some("attribute"), SymbolKind::Property),
                {
                    let mut attr =
                        declaration(code, "m_attribute", Some("attribute"), SymbolKind::Property);
                    attr.range.start.line += 1;
                    attr.range.end.line += 1;
                    attr.selection_range.start.line += 1;
                    attr.selection_range.end.line += 1;
                    attr
                }
            ]
        )
    }

    #[test]
    fn alias_declaration() {
        let code = "
package pkg is
    alias m_alias is o_signal;
end;
";
        assert_eq!(
            get_declarations(code),
            vec![declaration(
                code,
                "m_alias",
                Some("alias"),
                SymbolKind::TypeParameter
            ),]
        )
    }

    #[test]
    fn subprogram_declaration() {
        let code = "
package pkg is
    function m_function return integer;
    procedure m_procedure;
end;
";
        assert_eq!(
            get_declarations(code),
            vec![
                declaration(code, "m_function", Some("function"), SymbolKind::Function),
                declaration(code, "m_procedure", Some("procedure"), SymbolKind::Method),
            ]
        )
    }

    #[test]
    fn subprogram_body() {
        let code = "
package body pkg is
    function m_function return integer is begin end;
    procedure m_procedure is begin end;
end;
";
        assert_eq!(
            get_declarations(code),
            vec![
                declaration(code, "m_function", Some("function"), SymbolKind::Function),
                declaration(code, "m_procedure", Some("procedure"), SymbolKind::Method),
            ]
        )
    }

    #[test]
    fn use_declaration() {
        let code = "
package pkg is
    use work.usepkg.all;
    use use_a,use_b,      use_c;
end;
";
        let mut multi_use = declaration2(
            "use",
            None,
            SymbolKind::Namespace,
            range(code, "use use_a", "c;"),
            lsp_types::Range {
                start: range(code, "use use_a", "c;").start,
                end: range(code, "use use_a", "c;").start,
            },
        );
        multi_use.children = Some({
            let mut children = vec![];
            for child in ["use_a", "use_b", "use_c"] {
                let use_range = range(&code, child, child);
                children.push(declaration2(
                    child,
                    Some("use"),
                    SymbolKind::Namespace,
                    use_range,
                    use_range,
                ));
            }
            children
        });
        assert_eq!(
            get_declarations(code),
            vec![
                declaration2(
                    "work.usepkg.all",
                    Some("use"),
                    SymbolKind::Namespace,
                    range(code, "use work.usepkg", ";"),
                    range1(code, "work.usepkg.all")
                ),
                multi_use
            ]
        )
    }

    #[test]
    fn package_instantiation_declaration() {
        let code = "
architecture rtl of ent is
    package m_package is new gen_package;
begin
end;
";
        assert_eq!(
            get_declarations(code),
            vec![declaration2(
                "m_package",
                Some("package instance"),
                SymbolKind::Package,
                range(code, "package", ";"),
                range1(code, "m_package")
            )]
        )
    }

    #[test]
    fn configuration_specification() {
        let code = "
architecture rtl of ent is
    for all : compname use entity work.ent(sim);
begin
end;
";
        assert_eq!(
            get_declarations(code),
            vec![{
                let mut conf = declaration(code, "compname", None, SymbolKind::Constructor);
                conf.name = String::from("configuration");
                conf
            }]
        )
    }

    fn statement(code: &str, name: &str, detail: Option<&str>, kind: SymbolKind) -> DocumentSymbol {
        DocumentSymbol {
            name: String::from(name),
            detail: detail.map(|detail| String::from(detail)),
            kind,
            deprecated: None,
            range: range1(code, name),
            selection_range: range1(code, name),
            children: None,
        }
    }

    fn statement2(
        name: &str,
        detail: Option<&str>,
        kind: SymbolKind,
        range: lsp_types::Range,
        selection_range: lsp_types::Range,
    ) -> DocumentSymbol {
        DocumentSymbol {
            name: String::from(name),
            detail: detail.map(|detail| String::from(detail)),
            kind,
            deprecated: None,
            range,
            selection_range,
            children: None,
        }
    }

    fn get_statements(code: &str) -> Vec<DocumentSymbol> {
        let design_file = parse_str(code);
        let document_symbol = design_file.design_units.first().unwrap().document_symbol();
        let mut statements = None;
        for child in document_symbol.children.unwrap().iter() {
            if child.name == "statements" {
                statements = Some(child.children.clone().unwrap());
                break;
            }
        }
        statements.unwrap()
    }

    #[test]
    fn concurrent_procedure_call() {
        let code = "
architecture rtl of ent is
begin
    procedure_call(par);
    lbl: procedure_call(par);
end;
";
        assert_eq!(
            get_statements(code),
            vec![
                statement(
                    code,
                    "procedure_call",
                    Some("procedure"),
                    SymbolKind::Method
                ),
                statement2(
                    "lbl",
                    Some("procedure_call"),
                    SymbolKind::Method,
                    range(code, "lbl", "procedure_call"),
                    range1(code, "lbl"),
                )
            ]
        )
    }

    #[test]
    fn block_statment() {
        let code = "
architecture rtl of ent is
begin
    block_lbl: block is
    begin
    end block;
end;
";
        assert_eq!(
            get_statements(code),
            vec![statement2(
                "block_lbl",
                Some("block"),
                SymbolKind::Module,
                range(code, "block_lbl", ";"),
                range1(code, "block_lbl"),
            )]
        )
    }

    #[test]
    fn process_statment() {
        let code = "
architecture rtl of ent is
begin
    process is begin end process;
    lbl: process is begin end process;
end;
";
        assert_eq!(
            get_statements(code),
            vec![
                statement2(
                    "process",
                    None,
                    SymbolKind::Event,
                    range(code, "process", "end process;"),
                    lsp_types::Range {
                        start: range1(code, "process").start,
                        end: range1(code, "process").start,
                    },
                ),
                statement2(
                    "lbl",
                    Some("process"),
                    SymbolKind::Event,
                    range(code, "lbl", "end process;"),
                    range1(code, "lbl"),
                )
            ]
        )
    }

    #[test]
    fn concurrent_assert_statment() {
        let code = "
architecture rtl of ent is
begin
    assert true;
    lbl: assert false;
end;
";
        assert_eq!(
            get_statements(code),
            vec![
                statement2(
                    " ",
                    Some("assertion"),
                    SymbolKind::Field,
                    range1(code, "true"),
                    range1(code, "true"),
                ),
                statement2(
                    "lbl",
                    Some("assertion"),
                    SymbolKind::Field,
                    range(code, "lbl", "false"),
                    range1(code, "lbl"),
                )
            ]
        )
    }

    #[test]
    fn concurrent_signal_assignment() {
        let code = "
architecture rtl of ent is
begin
    sig <= 1;
    lbl: sig <= 2;
    (sig1,sig2) <= asd;
    lbl2: (sig1,sig2) <= asd;
end;
";
        assert_eq!(
            get_statements(code),
            vec![
                statement2(
                    "sig",
                    Some("assignment"),
                    SymbolKind::Field,
                    range1(code, "sig"),
                    range1(code, "sig"),
                ),
                statement2(
                    "lbl",
                    Some("assignment"),
                    SymbolKind::Field,
                    range(code, "lbl", "sig"),
                    range1(code, "lbl"),
                ),
                statement2(
                    "sig1, sig2",
                    Some("assignment"),
                    SymbolKind::Field,
                    range1(code, "(sig1,sig2)"),
                    range1(code, "(sig1,sig2)"),
                ),
                statement2(
                    "lbl2",
                    Some("assignment"),
                    SymbolKind::Field,
                    range(code, "lbl2", "(sig1,sig2)"),
                    range1(code, "lbl2"),
                ),
            ]
        )
    }
    #[test]
    fn instantiation_statement() {
        let code = "
architecture rtl of ent is
begin
    ent_i: entity work.ent_name(rtl);
    cmp_i: component comp_name;
    cnf_i: configuration work.cnf_name;
end;
";
        assert_eq!(
            get_statements(code),
            vec![
                statement2(
                    "ent_i",
                    Some("work.ent_name(rtl) : entity"),
                    SymbolKind::Object,
                    range(code, "ent_i", "work.ent_name"),
                    range1(code, "ent_i"),
                ),
                statement2(
                    "cmp_i",
                    Some("comp_name : component"),
                    SymbolKind::Object,
                    range(code, "cmp_i", "comp_name"),
                    range1(code, "cmp_i"),
                ),
                statement2(
                    "cnf_i",
                    Some("work.cnf_name : configuration"),
                    SymbolKind::Object,
                    range(code, "cnf_i", "work.cnf_name"),
                    range1(code, "cnf_i"),
                ),
            ]
        )
    }

    #[test]
    fn for_generate_statement() {
        let code = "
architecture rtl of ent is
begin
    lbl: for index_name in 0 to 1 generate
    begin
    end generate;
end;
";
        assert_eq!(
            get_statements(code),
            vec![statement2(
                "lbl",
                Some("generate"),
                SymbolKind::Field,
                range(code, "lbl", "index_name"),
                range1(code, "lbl"),
            )]
        )
    }

    #[test]
    fn if_generate_statement() {
        let code = "
architecture rtl of ent is
begin
    lbl: if true generate
    begin
    end generate;
end;
";
        assert_eq!(
            get_statements(code),
            vec![statement2(
                "lbl",
                Some("generate"),
                SymbolKind::Field,
                range(code, "lbl", "true"),
                range1(code, "lbl"),
            )]
        )
    }

    #[test]
    fn case_generate_statement() {
        let code = "
architecture rtl of ent is
begin
    lbl: case expr generate
        when others =>
    end generate;
end;
";
        assert_eq!(
            get_statements(code),
            vec![statement2(
                "lbl",
                Some("case generate"),
                SymbolKind::Field,
                range(code, "lbl", "expr"),
                range1(code, "lbl"),
            )]
        )
    }

    #[test]
    fn test_nested_document_symbol_response_from_file() {
        let code = "
library ieee;

entity ent1 is
end entity;

";
        let (source_url, _file) = write_source_file(code);
        assert_eq!(
            nested_document_symbol_response_from_file(&source_url).unwrap(),
            DocumentSymbolResponse::from(vec![DocumentSymbol {
                name: String::from("ent1"),
                detail: Some(String::from("entity")),
                kind: SymbolKind::Interface,
                deprecated: None,
                range: range(code, "library", "end entity;"),
                selection_range: range1(code, "ent1"),
                children: Some(vec![ieee_context(code)]),
            }])
        );
    }
}
