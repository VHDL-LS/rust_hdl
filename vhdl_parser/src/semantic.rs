// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use ast::*;
use latin_1::Latin1String;
use library::DesignRoot;
use message::{Message, MessageHandler};
use source::WithPos;
use symbol_table::{Symbol, SymbolTable};

extern crate fnv;
use self::fnv::FnvHashMap;
use std::collections::hash_map::Entry;
use std::sync::Arc;

#[derive(Clone)]
enum DeclarativeAst<'a> {
    Declaration(&'a Declaration),
    Element(&'a ElementDeclaration),
    Enum(&'a WithPos<EnumerationLiteral>),
    Interface(&'a InterfaceDeclaration),
}

impl<'a> DeclarativeAst<'a> {
    fn is_deferred_constant(&self) -> bool {
        match self {
            DeclarativeAst::Declaration(Declaration::Object(ObjectDeclaration {
                ref class,
                ref expression,
                ..
            })) => *class == ObjectClass::Constant && expression.is_none(),
            _ => false,
        }
    }

    fn is_non_deferred_constant(&self) -> bool {
        match self {
            DeclarativeAst::Declaration(Declaration::Object(ObjectDeclaration {
                ref class,
                ref expression,
                ..
            })) => *class == ObjectClass::Constant && expression.is_some(),
            _ => false,
        }
    }

    fn is_protected_type(&self) -> bool {
        match self {
            DeclarativeAst::Declaration(Declaration::Type(TypeDeclaration {
                def: TypeDefinition::Protected { .. },
                ..
            })) => true,
            _ => false,
        }
    }

    fn is_protected_type_body(&self) -> bool {
        match self {
            DeclarativeAst::Declaration(Declaration::Type(TypeDeclaration {
                def: TypeDefinition::ProtectedBody { .. },
                ..
            })) => true,
            _ => false,
        }
    }

    fn is_incomplete_type(&self) -> bool {
        match self {
            DeclarativeAst::Declaration(Declaration::Type(TypeDeclaration {
                def: TypeDefinition::Incomplete,
                ..
            })) => true,
            _ => false,
        }
    }

    fn is_type_declaration(&self) -> bool {
        match self {
            DeclarativeAst::Declaration(Declaration::Type(..)) => true,
            _ => false,
        }
    }
}

#[derive(Clone)]
struct DeclarativeItem<'a> {
    designator: WithPos<Designator>,
    ast: DeclarativeAst<'a>,
    may_overload: bool,
}

impl<'a> DeclarativeItem<'a> {
    fn new(
        designator: impl Into<WithPos<Designator>>,
        ast: DeclarativeAst<'a>,
    ) -> DeclarativeItem<'a> {
        DeclarativeItem {
            designator: designator.into(),
            ast,
            may_overload: false,
        }
    }
    fn from_ident(ident: &Ident, ast: DeclarativeAst<'a>) -> DeclarativeItem<'a> {
        DeclarativeItem::new(ident.to_owned().map_into(Designator::Identifier), ast)
    }

    fn with_overload(mut self, value: bool) -> DeclarativeItem<'a> {
        self.may_overload = value;
        self
    }

    fn is_deferred_of(&self, other: &Self) -> bool {
        if self.ast.is_deferred_constant() && other.ast.is_non_deferred_constant() {
            true
        } else if self.ast.is_protected_type() && other.ast.is_protected_type_body() {
            true
        } else if self.ast.is_incomplete_type()
            && other.ast.is_type_declaration()
            && !other.ast.is_incomplete_type()
        {
            true
        } else {
            false
        }
    }
}

#[derive(Copy, Clone, PartialEq)]
enum RegionKind {
    PackageDeclaration,
    PackageBody,
    Other,
}

#[derive(Clone)]
struct DeclarativeRegion<'a> {
    decls: FnvHashMap<Designator, DeclarativeItem<'a>>,
    kind: RegionKind,
}

impl<'a> DeclarativeRegion<'a> {
    fn new() -> DeclarativeRegion<'a> {
        DeclarativeRegion {
            decls: FnvHashMap::default(),
            kind: RegionKind::Other,
        }
    }

    fn in_package_declaration(mut self) -> DeclarativeRegion<'a> {
        self.kind = RegionKind::PackageDeclaration;
        self
    }

    fn in_body(&self) -> DeclarativeRegion<'a> {
        let mut region = self.clone();
        region.kind = match region.kind {
            RegionKind::PackageDeclaration => RegionKind::PackageBody,
            _ => RegionKind::Other,
        };
        region
    }

    fn close_immediate(&self, messages: &mut MessageHandler) {
        for decl in self.decls.values() {
            if decl.ast.is_incomplete_type() {
                messages.push(Message::error(
                    &decl.designator,
                    "Missing full type declaration of incomplete type 'rec_t'",
                ));
                messages.push(
                    Message::hint(
                        &decl.designator,
                        "The full type declaration shall occur immediately within the same declarative part",
                    ));
            }
        }
    }

    fn close_extended(&self, messages: &mut MessageHandler) {
        for decl in self.decls.values() {
            if decl.ast.is_deferred_constant() {
                messages.push(
                    Message::error(&decl.designator,
                                   format!("Deferred constant '{}' lacks corresponding full constant declaration in package body", &decl.designator.item)));
            } else if decl.ast.is_protected_type() {
                messages.push(Message::error(
                    &decl.designator,
                    format!(
                        "Missing body for protected type '{}'",
                        &decl.designator.item
                    ),
                ));
            }
        }
    }

    fn close_both(&self, messages: &mut MessageHandler) {
        self.close_immediate(messages);
        self.close_extended(messages);
    }

    fn add(&mut self, decl: DeclarativeItem<'a>, messages: &mut MessageHandler) {
        if self.kind != RegionKind::PackageDeclaration && decl.ast.is_deferred_constant() {
            messages.push(Message::error(
                &decl.designator,
                "Deferred constants are only allowed in package declarations (not body)",
            ));
        }

        match self.decls.entry(decl.designator.item.clone()) {
            Entry::Occupied(mut entry) => {
                let old_decl = entry.get_mut();

                if !decl.may_overload || !old_decl.may_overload {
                    if old_decl.is_deferred_of(&decl) {
                        if self.kind != RegionKind::PackageBody
                            && decl.ast.is_non_deferred_constant()
                        {
                            messages.push(Message::error(
                                &decl.designator,
                                "Full declaration of deferred constant is only allowed in a package body"));
                        }

                        std::mem::replace(old_decl, decl);
                    } else {
                        let msg = Message::error(
                            &decl.designator,
                            format!("Duplicate declaration of '{}'", decl.designator.item),
                        ).related(&old_decl.designator, "Previously defined here");
                        messages.push(msg)
                    }
                }
            }
            Entry::Vacant(entry) => {
                if decl.ast.is_protected_type_body() {
                    messages.push(Message::error(
                        &decl.designator,
                        format!(
                            "No declaration of protected type '{}'",
                            &decl.designator.item
                        ),
                    ));
                } else {
                    entry.insert(decl);
                }
            }
        }
    }

    fn add_interface_list(
        &mut self,
        declarations: &'a [InterfaceDeclaration],
        messages: &mut MessageHandler,
    ) {
        for decl in declarations.iter() {
            for item in decl.declarative_items() {
                self.add(item, messages);
            }
        }
    }

    fn add_declarative_part(
        &mut self,
        declarations: &'a [Declaration],
        messages: &mut MessageHandler,
    ) {
        for decl in declarations.iter() {
            for item in decl.declarative_items() {
                self.add(item, messages);
            }
        }
    }

    fn add_element_declarations(
        &mut self,
        declarations: &'a [ElementDeclaration],
        messages: &mut MessageHandler,
    ) {
        for decl in declarations.iter() {
            self.add(
                DeclarativeItem::from_ident(&decl.ident, DeclarativeAst::Element(decl)),
                messages,
            );
        }
    }
}

impl SubprogramDesignator {
    fn to_designator(self) -> Designator {
        match self {
            SubprogramDesignator::Identifier(ident) => Designator::Identifier(ident),
            SubprogramDesignator::OperatorSymbol(ident) => Designator::OperatorSymbol(ident),
        }
    }
}

impl SubprogramDeclaration {
    fn designator(&self) -> WithPos<Designator> {
        match self {
            SubprogramDeclaration::Function(ref function) => function
                .designator
                .clone()
                .map_into(|des| des.to_designator()),
            SubprogramDeclaration::Procedure(ref procedure) => procedure
                .designator
                .clone()
                .map_into(|des| des.to_designator()),
        }
    }
}

impl EnumerationLiteral {
    fn to_designator(self) -> Designator {
        match self {
            EnumerationLiteral::Identifier(ident) => Designator::Identifier(ident),
            EnumerationLiteral::Character(byte) => Designator::Character(byte),
        }
    }
}

impl Declaration {
    fn declarative_items(&self) -> Vec<DeclarativeItem> {
        match self {
            Declaration::Alias(alias) => vec![
                DeclarativeItem::new(alias.designator.clone(), DeclarativeAst::Declaration(self))
                    .with_overload(alias.signature.is_some()),
            ],
            Declaration::Object(ObjectDeclaration { ref ident, .. }) => {
                vec![DeclarativeItem::from_ident(
                    ident,
                    DeclarativeAst::Declaration(self),
                )]
            }
            Declaration::File(FileDeclaration { ref ident, .. }) => {
                vec![DeclarativeItem::from_ident(
                    ident,
                    DeclarativeAst::Declaration(self),
                )]
            }
            Declaration::Component(ComponentDeclaration { ref ident, .. }) => {
                vec![DeclarativeItem::from_ident(
                    ident,
                    DeclarativeAst::Declaration(self),
                )]
            }
            Declaration::Attribute(ref attr) => match attr {
                Attribute::Declaration(AttributeDeclaration { ref ident, .. }) => {
                    vec![DeclarativeItem::from_ident(
                        ident,
                        DeclarativeAst::Declaration(self),
                    )]
                }
                // @TODO Ignored for now
                Attribute::Specification(..) => vec![],
            },
            Declaration::SubprogramBody(body) => vec![
                DeclarativeItem::new(
                    body.specification.designator(),
                    DeclarativeAst::Declaration(self),
                ).with_overload(true),
            ],
            Declaration::SubprogramDeclaration(decl) => vec![
                DeclarativeItem::new(decl.designator(), DeclarativeAst::Declaration(self))
                    .with_overload(true),
            ],
            // @TODO Ignored for now
            Declaration::Use(..) => vec![],
            Declaration::Package(ref package) => vec![DeclarativeItem::from_ident(
                &package.ident,
                DeclarativeAst::Declaration(self),
            )],
            Declaration::Configuration(..) => vec![],
            Declaration::Type(TypeDeclaration {
                ref ident,
                def: TypeDefinition::Enumeration(ref enumeration),
            }) => {
                let mut items = vec![DeclarativeItem::from_ident(
                    ident,
                    DeclarativeAst::Declaration(self),
                )];
                for literal in enumeration.iter() {
                    items.push(
                        DeclarativeItem::new(
                            literal.clone().map_into(|lit| lit.to_designator()),
                            DeclarativeAst::Enum(literal),
                        ).with_overload(true),
                    )
                }
                items
            }
            Declaration::Type(TypeDeclaration { ref ident, .. }) => {
                vec![DeclarativeItem::from_ident(
                    ident,
                    DeclarativeAst::Declaration(self),
                )]
            }
        }
    }
}

impl InterfaceDeclaration {
    fn declarative_items(&self) -> Vec<DeclarativeItem> {
        match self {
            InterfaceDeclaration::File(InterfaceFileDeclaration { ref ident, .. }) => {
                vec![DeclarativeItem::from_ident(
                    ident,
                    DeclarativeAst::Interface(self),
                )]
            }
            InterfaceDeclaration::Object(InterfaceObjectDeclaration { ref ident, .. }) => {
                vec![DeclarativeItem::from_ident(
                    ident,
                    DeclarativeAst::Interface(self),
                )]
            }
            InterfaceDeclaration::Type(ref ident) => vec![DeclarativeItem::from_ident(
                ident,
                DeclarativeAst::Interface(self),
            )],
            InterfaceDeclaration::Subprogram(decl, ..) => vec![
                DeclarativeItem::new(decl.designator(), DeclarativeAst::Interface(self))
                    .with_overload(true),
            ],
            InterfaceDeclaration::Package(ref package) => vec![DeclarativeItem::from_ident(
                &package.ident,
                DeclarativeAst::Interface(self),
            )],
        }
    }
}

impl std::fmt::Display for Designator {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Designator::Identifier(ref sym) => write!(f, "{}", sym),
            Designator::OperatorSymbol(ref latin1) => write!(f, "\"{}\"", latin1),
            Designator::Character(byte) => write!(f, "'{}'", *byte as char),
        }
    }
}

/// Check that no homographs are defined in the element declarations
fn check_element_declaration_unique_ident(
    declarations: &[ElementDeclaration],
    messages: &mut MessageHandler,
) {
    let mut region = DeclarativeRegion::new();
    region.add_element_declarations(declarations, messages);
    region.close_both(messages);
}

/// Check that no homographs are defined in the interface list
fn check_interface_list_unique_ident(
    declarations: &[InterfaceDeclaration],
    messages: &mut MessageHandler,
) {
    let mut region = DeclarativeRegion::new();
    region.add_interface_list(declarations, messages);
    region.close_both(messages);
}

impl SubprogramDeclaration {
    fn interface_list<'a>(&'a self) -> &[InterfaceDeclaration] {
        match self {
            SubprogramDeclaration::Function(fun) => &fun.parameter_list,
            SubprogramDeclaration::Procedure(proc) => &proc.parameter_list,
        }
    }
}
fn check_declarative_part_unique_ident(
    declarations: &[Declaration],
    messages: &mut MessageHandler,
) {
    let mut region = DeclarativeRegion::new();
    region.add_declarative_part(declarations, messages);
    check_declarative_part_unique_ident_inner(declarations, messages);
    region.close_both(messages);
}

/// Check that no homographs are defined in the declarative region
fn check_declarative_part_unique_ident_inner(
    declarations: &[Declaration],
    messages: &mut MessageHandler,
) {
    for decl in declarations.iter() {
        match decl {
            Declaration::Component(ref component) => {
                check_interface_list_unique_ident(&component.generic_list, messages);
                check_interface_list_unique_ident(&component.port_list, messages);
            }
            Declaration::SubprogramBody(ref body) => {
                check_interface_list_unique_ident(body.specification.interface_list(), messages);
                check_declarative_part_unique_ident(&body.declarations, messages);
            }
            Declaration::SubprogramDeclaration(decl) => {
                check_interface_list_unique_ident(decl.interface_list(), messages);
            }
            Declaration::Type(type_decl) => match type_decl.def {
                TypeDefinition::ProtectedBody(ref body) => {
                    check_declarative_part_unique_ident(&body.decl, messages);
                }
                TypeDefinition::Protected(ref prot_decl) => {
                    for item in prot_decl.items.iter() {
                        match item {
                            ProtectedTypeDeclarativeItem::Subprogram(subprogram) => {
                                check_interface_list_unique_ident(
                                    subprogram.interface_list(),
                                    messages,
                                );
                            }
                        }
                    }
                }
                TypeDefinition::Record(ref decls) => {
                    check_element_declaration_unique_ident(decls, messages);
                }
                _ => {}
            },
            _ => {}
        }
    }
}

fn check_generate_body(body: &GenerateBody, messages: &mut MessageHandler) {
    if let Some(ref decl) = body.decl {
        check_declarative_part_unique_ident(&decl, messages);
    }
    check_concurrent_part(&body.statements, messages);
}

fn check_concurrent_statement(
    statement: &LabeledConcurrentStatement,
    messages: &mut MessageHandler,
) {
    match statement.statement {
        ConcurrentStatement::Block(ref block) => {
            check_declarative_part_unique_ident(&block.decl, messages);
            check_concurrent_part(&block.statements, messages);
        }
        ConcurrentStatement::Process(ref process) => {
            check_declarative_part_unique_ident(&process.decl, messages);
        }
        ConcurrentStatement::ForGenerate(ref gen) => {
            check_generate_body(&gen.body, messages);
        }
        ConcurrentStatement::IfGenerate(ref gen) => {
            for conditional in gen.conditionals.iter() {
                check_generate_body(&conditional.item, messages);
            }
            if let Some(ref else_item) = gen.else_item {
                check_generate_body(else_item, messages);
            }
        }
        ConcurrentStatement::CaseGenerate(ref gen) => {
            for alternative in gen.alternatives.iter() {
                check_generate_body(&alternative.item, messages);
            }
        }
        _ => {}
    }
}

fn check_concurrent_part(statements: &[LabeledConcurrentStatement], messages: &mut MessageHandler) {
    for statement in statements.iter() {
        check_concurrent_statement(statement, messages);
    }
}

fn check_package_declaration<'a>(
    package: &'a PackageDeclaration,
    messages: &mut MessageHandler,
) -> DeclarativeRegion<'a> {
    let mut region = DeclarativeRegion::new().in_package_declaration();
    if let Some(ref list) = package.generic_clause {
        region.add_interface_list(list, messages);
    }
    region.add_declarative_part(&package.decl, messages);
    check_declarative_part_unique_ident_inner(&package.decl, messages);
    region
}

fn check_architecture_body<'a>(
    entity_region: &mut DeclarativeRegion<'a>,
    architecture: &'a ArchitectureBody,
    messages: &mut MessageHandler,
) {
    entity_region.add_declarative_part(&architecture.decl, messages);
    check_declarative_part_unique_ident_inner(&architecture.decl, messages);
    check_concurrent_part(&architecture.statements, messages);
}

fn check_package_body<'a>(
    package_region: &mut DeclarativeRegion<'a>,
    package: &'a PackageBody,
    messages: &mut MessageHandler,
) {
    package_region.add_declarative_part(&package.decl, messages);
    check_declarative_part_unique_ident_inner(&package.decl, messages);
}

fn check_entity_declaration<'a>(
    entity: &'a EntityDeclaration,
    messages: &mut MessageHandler,
) -> DeclarativeRegion<'a> {
    let mut region = DeclarativeRegion::new();
    if let Some(ref list) = entity.generic_clause {
        region.add_interface_list(list, messages);
    }
    if let Some(ref list) = entity.port_clause {
        region.add_interface_list(list, messages);
    }
    region.add_declarative_part(&entity.decl, messages);
    check_concurrent_part(&entity.statements, messages);

    region
}

pub struct Analyzer {
    work_sym: Symbol,
    std_sym: Symbol,
}

impl Analyzer {
    pub fn new(symtab: Arc<SymbolTable>) -> Analyzer {
        Analyzer {
            work_sym: symtab.insert(&Latin1String::new(b"work")),
            std_sym: symtab.insert(&Latin1String::new(b"std")),
        }
    }

    fn check_context_clause(
        &self,
        root: &DesignRoot,
        context_clause: &Vec<WithPos<ContextItem>>,
        messages: &mut MessageHandler,
    ) {
        for context_item in context_clause.iter() {
            match context_item.item {
                ContextItem::Library(LibraryClause { ref name_list }) => {
                    for library_name in name_list.iter() {
                        if self.std_sym == library_name.item {
                            // std is pre-defined
                        } else if self.work_sym == library_name.item {
                            messages.push(Message::hint(
                                &library_name,
                                format!("Library clause not necessary for current working library"),
                            ))
                        } else if !root.has_library(&library_name.item) {
                            messages.push(Message::error(
                                &library_name,
                                format!("No such library '{}'", library_name.item),
                            ))
                        }
                    }
                }
                _ => {}
            }
        }
    }

    pub fn analyze(&self, root: &DesignRoot, messages: &mut MessageHandler) {
        for library in root.iter_libraries() {
            for entity in library.entities() {
                self.check_context_clause(root, &entity.entity.context_clause, messages);
                let mut region = check_entity_declaration(&entity.entity.unit, messages);
                region.close_immediate(messages);
                for architecture in entity.architectures.values() {
                    let mut region = region.in_body();
                    self.check_context_clause(root, &architecture.context_clause, messages);
                    check_architecture_body(&mut region, &architecture.unit, messages);
                    region.close_extended(messages);
                }
            }

            for package in library.packages() {
                self.check_context_clause(root, &package.package.context_clause, messages);
                let mut region = check_package_declaration(&package.package.unit, messages);
                if let Some(ref body) = package.body {
                    region.close_immediate(messages);
                    let mut region = region.in_body();
                    self.check_context_clause(root, &body.context_clause, messages);
                    check_package_body(&mut region, &body.unit, messages);
                    region.close_extended(messages);
                } else {
                    region.close_both(messages);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use library::Library;
    use message::Message;
    use test_util::{check_messages, check_no_messages, Code, CodeBuilder};

    fn expected_message(code: &Code, name: &str, occ1: usize, occ2: usize) -> Message {
        Message::error(
            code.s(&name, occ2),
            format!("Duplicate declaration of '{}'", &name),
        ).related(code.s(&name, occ1), "Previously defined here")
    }

    fn expected_messages(code: &Code, names: &[&str]) -> Vec<Message> {
        let mut messages = Vec::new();
        for name in names {
            messages.push(expected_message(code, name, 1, 2));
        }
        messages
    }

    fn expected_messages_multi(code1: &Code, code2: &Code, names: &[&str]) -> Vec<Message> {
        let mut messages = Vec::new();
        for name in names {
            messages.push(
                Message::error(
                    code2.s1(&name),
                    format!("Duplicate declaration of '{}'", &name),
                ).related(code1.s1(&name), "Previously defined here"),
            )
        }
        messages
    }

    #[test]
    fn allows_unique_names() {
        let mut builder = LibraryBuilder::new();
        builder.code(
            "libname",
            "
package pkg is
  constant a : natural := 0;
  constant b : natural := 0;
  constant c : natural := 0;
end package;
",
        );

        let messages = builder.analyze();
        check_no_messages(&messages);
    }

    #[test]
    fn allows_deferred_constant() {
        let mut builder = LibraryBuilder::new();
        builder.code(
            "libname",
            "
package pkg is
  constant a : natural;
end package;

package body pkg is
  constant a : natural := 0;
end package body;
",
        );

        let messages = builder.analyze();
        check_no_messages(&messages);
    }

    #[test]
    fn forbid_deferred_constant_after_constant() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  constant a1 : natural := 0;
  constant a1 : natural;
end package;
",
        );

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1"]));
    }

    #[test]
    fn forbid_deferred_constant_outside_of_package_declaration() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
end package;

package body pkg is
  constant a1 : natural;
  constant a1 : natural := 0;
end package body;
",
        );

        let messages = builder.analyze();
        check_messages(
            messages,
            vec![Message::error(
                &code.s1("a1"),
                "Deferred constants are only allowed in package declarations (not body)",
            )],
        );
    }

    #[test]
    fn forbid_full_declaration_of_deferred_constant_outside_of_package_body() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  constant a1 : natural;
  constant a1 : natural := 0;
end package;
",
        );

        let messages = builder.analyze();
        check_messages(
            messages,
            vec![Message::error(
                &code.s("a1", 2),
                "Full declaration of deferred constant is only allowed in a package body",
            )],
        );
    }

    #[test]
    fn error_on_missing_full_constant_declaration() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg_no_body is
  constant a1 : natural;
end package;

package pkg is
  constant b1 : natural;
end package;

package body pkg is
end package body;
",
        );

        let messages = builder.analyze();
        check_messages(
            messages,
            vec![
                Message::error(
                    &code.s1("a1"),
                    "Deferred constant 'a1' lacks corresponding full constant declaration in package body",
                ),
                Message::error(
                    &code.s1("b1"),
                    "Deferred constant 'b1' lacks corresponding full constant declaration in package body",
                ),
            ],
        );
    }

    #[test]
    fn error_on_missing_protected_body() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg_no_body is
  type a1 is protected
  end protected;
end package;

package pkg is
  type b1 is protected
  end protected;
end package;

package body pkg is
end package body;
",
        );

        let messages = builder.analyze();
        check_messages(
            messages,
            vec![
                Message::error(&code.s1("a1"), "Missing body for protected type 'a1'"),
                Message::error(&code.s1("b1"), "Missing body for protected type 'b1'"),
            ],
        );
    }

    #[test]
    fn error_on_missing_protected_type_for_body() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg_no_body is
  type a1 is protected body
  end protected body;
end package;

package pkg is
end package;

package body pkg is
  type b1 is protected body
  end protected body;

  type b1 is protected
  end protected;
end package body;
",
        );

        let messages = builder.analyze();
        check_messages(
            messages,
            vec![
                Message::error(&code.s1("a1"), "No declaration of protected type 'a1'"),
                Message::error(&code.s1("b1"), "No declaration of protected type 'b1'"),
                Message::error(&code.s("b1", 2), "Missing body for protected type 'b1'"),
            ],
        );
    }

    #[test]
    fn forbid_multiple_constant_after_deferred_constant() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  constant a1 : natural;
end package;

package body pkg is
  constant a1 : natural := 0;
  constant a1 : natural := 0;
end package body;
",
        );

        let messages = builder.analyze();
        check_messages(messages, vec![expected_message(&code, "a1", 2, 3)]);
    }

    #[test]
    fn forbid_homographs() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  constant a1 : natural := 0;
  constant a : natural := 0;
  constant a1 : natural := 0;
end package;
",
        );

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1"]));
    }

    #[test]
    fn allows_protected_type_and_body_with_same_name() {
        let mut builder = LibraryBuilder::new();
        builder.code(
            "libname",
            "
package pkg is
  type prot_t is protected
  end protected;

  type prot_t is protected body
  end protected body;
end package;
",
        );

        let messages = builder.analyze();
        check_no_messages(&messages);
    }

    #[test]
    fn forbid_duplicate_protected_type() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  type prot_t is protected
  end protected;

  type prot_t is protected
  end protected;

  type prot_t is protected body
  end protected body;
end package;
",
        );

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["prot_t"]));
    }

    #[test]
    fn forbid_duplicate_protected_type_body() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  type prot_t is protected
  end protected;

  type prot_t is protected body
  end protected body;

  type prot_t is protected body
  end protected body;
end package;
",
        );

        let messages = builder.analyze();
        check_messages(messages, vec![expected_message(&code, "prot_t", 2, 3)]);
    }

    #[test]
    fn forbid_incompatible_deferred_items() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is

  -- Protected type vs constant
  type a1 is protected
  end protected;
  constant a1 : natural := 0;

  -- Just to avoid missing body error
  type a1 is protected body
  end protected body;

  -- Deferred constant vs protected body
  constant b1 : natural;
  type b1 is protected body
  end protected body;

end package;

package body pkg is
  constant b1 : natural := 0;
end package body;
",
        );

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1", "b1"]));
    }

    #[test]
    fn allows_incomplete_type_definition() {
        let mut builder = LibraryBuilder::new();
        builder.code(
            "libname",
            "
package pkg is
  type rec_t;
  type rec_t is record
  end record;
end package;
",
        );

        let messages = builder.analyze();
        check_no_messages(&messages);
    }

    #[test]
    fn error_on_duplicate_incomplete_type_definition() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  type rec_t;
  type rec_t;
  type rec_t is record
  end record;
end package;
",
        );

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["rec_t"]));
    }

    #[test]
    fn error_on_missing_full_type_definition_for_incomplete() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  type rec_t;
end package;

package body pkg is
  -- Must appear in the same immediate declarative region
  type rec_t is record
  end record;
end package body;
",
        );

        let messages = builder.analyze();
        check_messages(
            messages,
            vec![
                Message::error(
                    code.s1("rec_t"),
                    "Missing full type declaration of incomplete type 'rec_t'",
                ),
                Message::hint(
                    code.s1("rec_t"),
                    "The full type declaration shall occur immediately within the same declarative part",
                ),
            ],
        );
    }

    #[test]
    fn forbid_homographs_in_subprogram_bodies() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
end package;

package body pkg is
  procedure proc(a1, a, a1 : natural) is
    constant b1 : natural := 0;
    constant b : natural := 0;
    constant b1 : natural := 0;

    procedure nested_proc(c1, c, c1 : natural) is
      constant d1 : natural := 0;
      constant d : natural := 0;
      constant d1 : natural := 0;
    begin
    end;

  begin
  end;
end package body;
",
        );

        let messages = builder.analyze();
        check_messages(
            messages,
            expected_messages(&code, &["a1", "b1", "c1", "d1"]),
        );
    }

    #[test]
    fn forbid_homographs_in_component_declarations() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  component comp is
    generic (
      a1 : natural;
      a : natural;
      a1 : natural
    );
    port (
      b1 : natural;
      b : natural;
      b1 : natural
    );
  end component;
end package;
",
        );

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1", "b1"]));
    }

    #[test]
    fn forbid_homographs_in_record_type_declarations() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  type rec_t is record
    a1 : natural;
    a : natural;
    a1 : natural;
  end record;
end package;
",
        );

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1"]));
    }

    #[test]
    fn forbid_homographs_in_proteced_type_declarations() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  type prot_t is protected
    procedure proc(a1, a, a1 : natural);
  end protected;

  type prot_t is protected body
    constant b1 : natural := 0;
    constant b : natural := 0;
    constant b1 : natural := 0;
  end protected body;
end package;
",
        );

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1", "b1"]));
    }

    #[test]
    fn forbid_homographs_in_subprogram_declarations() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  procedure proc(a1, a, a1 : natural);
  function fun(b1, a, b1 : natural) return natural;
end package;
",
        );

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1", "b1"]));
    }

    #[test]
    fn forbid_homographs_in_block() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
entity ent is
begin
  blk : block
    constant a1 : natural := 0;
    constant a : natural := 0;
    constant a1 : natural := 0;
  begin
    process
      constant b1 : natural := 0;
      constant b : natural := 0;
      constant b1 : natural := 0;
    begin
    end process;
  end block;
end entity;
",
        );

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1", "b1"]));
    }

    #[test]
    fn forbid_homographs_in_process() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
entity ent is
begin
  process
    constant a1 : natural := 0;
    constant a : natural := 0;
    constant a1 : natural := 0;
  begin
  end process;
end entity;
",
        );

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1"]));
    }

    #[test]
    fn forbid_homographs_for_generate() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
entity ent is
begin
  gen_for: for i in 0 to 3 generate
    constant a1 : natural := 0;
    constant a : natural := 0;
    constant a1 : natural := 0;
  begin
    process
      constant b1 : natural := 0;
      constant b : natural := 0;
      constant b1 : natural := 0;
    begin
    end process;
  end generate;
end entity;
",
        );

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1", "b1"]));
    }

    #[test]
    fn forbid_homographs_if_generate() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
entity ent is
begin
  gen_if: if true generate
    constant a1 : natural := 0;
    constant a : natural := 0;
    constant a1 : natural := 0;
  begin

    prcss : process
      constant b1 : natural := 0;
      constant b : natural := 0;
      constant b1 : natural := 0;
    begin
    end process;

  else generate
    constant c1 : natural := 0;
    constant c: natural := 0;
    constant c1 : natural := 0;
  begin
    prcss : process
      constant d1 : natural := 0;
      constant d : natural := 0;
      constant d1 : natural := 0;
    begin
    end process;
  end generate;
end entity;
",
        );

        let messages = builder.analyze();
        check_messages(
            messages,
            expected_messages(&code, &["a1", "b1", "c1", "d1"]),
        );
    }

    #[test]
    fn forbid_homographs_case_generate() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
entity ent is
begin
  gen_case: case 0 generate
    when others =>
      constant a1 : natural := 0;
      constant a : natural := 0;
      constant a1 : natural := 0;
    begin
      process
        constant b1 : natural := 0;
        constant b : natural := 0;
        constant b1 : natural := 0;
      begin
      end process;
  end generate;
end entity;
",
        );

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1", "b1"]));
    }

    #[test]
    fn forbid_homographs_in_entity_declarations() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
entity ent is
  generic (
    a1 : natural;
    a : natural;
    a1 : natural
  );
  port (
    b1 : natural;
    b : natural;
    b1 : natural
  );
  constant c1 : natural := 0;
  constant c : natural := 0;
  constant c1 : natural := 0;
begin

  blk : block
    constant d1 : natural := 0;
    constant d : natural := 0;
    constant d1 : natural := 0;
  begin

  end block;

end entity;
",
        );

        let messages = builder.analyze();
        check_messages(
            messages,
            expected_messages(&code, &["a1", "b1", "c1", "d1"]),
        );
    }

    #[test]
    fn forbid_homographs_in_architecture_bodies() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
entity ent is
end entity;

architecture arch of ent is
  constant a1 : natural := 0;
  constant a : natural := 0;
  constant a1 : natural := 0;
begin

  blk : block
    constant b1 : natural := 0;
    constant b : natural := 0;
    constant b1 : natural := 0;
  begin
  end block;

end architecture;
",
        );

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1", "b1"]));
    }

    #[test]
    fn forbid_homographs_of_type_declarations() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  constant a1 : natural := 0;
  type a1 is (foo, bar);
end package;
",
        );

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1"]));
    }

    #[test]
    fn forbid_homographs_of_component_declarations() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  constant a1 : natural := 0;
  component a1 is
    port (clk : bit);
  end component;
end package;
",
        );

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1"]));
    }

    #[test]
    fn forbid_homographs_of_file_declarations() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  constant a1 : natural := 0;
  file a1 : text;
end package;
",
        );

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1"]));
    }

    #[test]
    fn forbid_homographs_in_package_declarations() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  package a1 is new pkg generic map (foo => bar);
  package a1 is new pkg generic map (foo => bar);
end package;
",
        );

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1"]));
    }

    #[test]
    fn forbid_homographs_in_attribute_declarations() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  attribute a1 : string;
  attribute a1 : string;
end package;
",
        );

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1"]));
    }

    #[test]
    fn forbid_homographs_in_alias_declarations() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  alias a1 is foo;
  alias a1 is bar;

  -- Legal since subprograms are overloaded
  alias b1 is foo[return natural];
  alias b1 is bar[return boolean];
end package pkg;
",
        );

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1"]));
    }

    #[test]
    fn forbid_homographs_for_overloaded_vs_non_overloaded() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  alias a1 is foo;
  alias a1 is bar[return boolean];

  function b1 return natural;
  constant b1 : natural := 0;
end package;
",
        );

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1", "b1"]));
    }

    #[test]
    fn enum_literals_may_overload() {
        let mut builder = LibraryBuilder::new();
        builder.code(
            "libname",
            "
package pkg is
  type enum_t is (a1, b1);

  -- Ok since enumerations may overload
  type enum2_t is (a1, b1);
end package;
",
        );

        let messages = builder.analyze();
        check_no_messages(&messages);
    }

    #[test]
    fn forbid_homograph_to_enum_literals() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  type enum_t is (a1, b1);
  constant a1 : natural := 0;
  function b1 return natural;
end package pkg;
",
        );

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1"]));
    }

    #[test]
    fn forbid_homographs_in_interface_file_declarations() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  procedure proc(file a1, a, a1 : text);
end package;
",
        );

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1"]));
    }

    #[test]
    fn forbid_homographs_in_interface_type_declarations() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
entity ent is
  generic (
    type a1;
    type a1
  );
end entity;
",
        );

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1"]));
    }

    #[test]
    fn forbid_homographs_in_interface_package_declarations() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
entity ent is
  generic (
    package a1 is new pkg generic map (<>);
    package a1 is new pkg generic map (<>)
  );
end entity;
",
        );

        let messages = builder.analyze();
        check_messages(messages, expected_messages(&code, &["a1"]));
    }

    #[test]
    fn forbid_homographs_in_entity_extended_declarative_regions() {
        let mut builder = LibraryBuilder::new();
        let ent = builder.code(
            "libname",
            "
entity ent is
  generic (
    constant g1 : natural;
    constant g2 : natural;
    constant g3 : natural;
    constant g4 : natural
  );
  port (
    signal g1 : natural;
    signal p1 : natural;
    signal p2 : natural;
    signal p3 : natural
  );
  constant g2 : natural := 0;
  constant p1 : natural := 0;
  constant e1 : natural := 0;
  constant e2 : natural := 0;
end entity;",
        );

        let arch1 = builder.code(
            "libname",
            "
architecture rtl of ent is
  constant g3 : natural := 0;
  constant p2 : natural := 0;
  constant e1 : natural := 0;
  constant a1 : natural := 0;
begin
end architecture;",
        );

        let arch2 = builder.code(
            "libname",
            "
architecture rtl2 of ent is
  constant a1 : natural := 0;
  constant e2 : natural := 0;
begin
end architecture;
",
        );

        let messages = builder.analyze();
        let mut expected = expected_messages(&ent, &["g1", "g2", "p1"]);
        expected.append(&mut expected_messages_multi(
            &ent,
            &arch1,
            &["g3", "p2", "e1"],
        ));
        expected.append(&mut expected_messages_multi(&ent, &arch2, &["e2"]));
        check_messages(messages, expected);
    }

    #[test]
    fn forbid_homographs_in_package_extended_declarative_regions() {
        let mut builder = LibraryBuilder::new();
        let pkg = builder.code(
            "libname",
            "
package pkg is
  generic (
    constant g1 : natural;
    constant g2 : natural
  );
  constant g1 : natural := 0;
end package;",
        );

        let body = builder.code(
            "libname",
            "
package body pkg is
  constant g1 : natural := 0;
  constant g2 : natural := 0;
  constant p1 : natural := 0;
end package body;",
        );

        let messages = builder.analyze();
        let mut expected = expected_messages(&pkg, &["g1"]);
        expected.append(&mut expected_messages_multi(&pkg, &body, &["g1", "g2"]));
        check_messages(messages, expected);
    }

    #[test]
    fn check_library_clause_library_exists() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
library missing_lib;

entity ent is
end entity;
            ",
        );

        let messages = builder.analyze();

        check_messages(
            messages,
            vec![Message::error(
                code.s1("missing_lib"),
                "No such library 'missing_lib'",
            )],
        )
    }

    #[test]
    fn library_std_is_pre_defined() {
        let mut builder = LibraryBuilder::new();
        builder.code(
            "libname",
            "
library std;

entity ent is
end entity;
            ",
        );

        let messages = builder.analyze();
        check_no_messages(&messages);
    }

    #[test]
    fn work_library_not_necessary_hint() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
library work;

entity ent is
end entity;
            ",
        );

        let messages = builder.analyze();

        check_messages(
            messages,
            vec![Message::hint(
                code.s1("work"),
                "Library clause not necessary for current working library",
            )],
        )
    }

    struct LibraryBuilder {
        code_builder: CodeBuilder,
        libraries: FnvHashMap<Symbol, Vec<Code>>,
    }

    impl LibraryBuilder {
        fn new() -> LibraryBuilder {
            LibraryBuilder {
                code_builder: CodeBuilder::new(),
                libraries: FnvHashMap::default(),
            }
        }
        fn code(&mut self, library_name: &str, code: &str) -> Code {
            let code = self.code_builder.code(code);
            let library_name = self.code_builder.symbol(library_name);
            match self.libraries.entry(library_name) {
                Entry::Occupied(mut entry) => {
                    entry.get_mut().push(code.clone());
                }
                Entry::Vacant(entry) => {
                    entry.insert(vec![code.clone()]);
                }
            }
            code
        }

        fn analyze(&self) -> Vec<Message> {
            let mut root = DesignRoot::new();
            let mut messages = Vec::new();

            for (library_name, codes) in self.libraries.iter() {
                let design_files = codes.iter().map(|code| code.design_file()).collect();
                let library = Library::new(library_name.clone(), design_files, &mut messages);
                root.add_library(library);
            }

            Analyzer::new(self.code_builder.symtab.clone()).analyze(&root, &mut messages);

            messages
        }
    }
}
