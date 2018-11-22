// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use ast::*;
use latin_1::Latin1String;
use library::DesignRoot;
use message::{Message, MessageHandler};
use source::{SrcPos, WithPos};
use symbol_table::{Symbol, SymbolTable};

extern crate fnv;
use self::fnv::FnvHashMap;
use std::collections::hash_map::Entry;
use std::sync::Arc;

impl Declaration {
    fn ident(&self) -> Option<Ident> {
        match self {
            // @TODO Ignored for now
            Declaration::Alias(alias) => match alias.designator {
                AliasDesignator::Identifier(ref ident) => {
                    if alias.signature.is_none() {
                        Some(ident.to_owned())
                    } else {
                        None
                    }
                }
                _ => None,
            },
            Declaration::Object(ObjectDeclaration { ref ident, .. }) => Some(ident.to_owned()),
            Declaration::File(FileDeclaration { ref ident, .. }) => Some(ident.to_owned()),
            Declaration::Component(ComponentDeclaration { ref ident, .. }) => {
                Some(ident.to_owned())
            }
            Declaration::Attribute(ref attr) => match attr {
                Attribute::Declaration(AttributeDeclaration { ref ident, .. }) => {
                    Some(ident.to_owned())
                }
                // @TODO Ignored for now
                Attribute::Specification(..) => None,
            },
            // @TODO Ignored for now
            Declaration::SubprogramBody(..) => None,
            // @TODO Ignored for now
            Declaration::SubprogramDeclaration(..) => None,
            // @TODO Ignored for now
            Declaration::Use(..) => None,
            Declaration::Package(ref package) => Some(package.ident.clone()),
            Declaration::Configuration(..) => None,
            Declaration::Type(TypeDeclaration {
                def: TypeDefinition::ProtectedBody(..),
                ..
            }) => None,
            Declaration::Type(TypeDeclaration {
                def: TypeDefinition::Incomplete,
                ..
            }) => None,
            Declaration::Type(TypeDeclaration { ref ident, .. }) => Some(ident.to_owned()),
        }
    }
}

impl InterfaceDeclaration {
    fn ident(&self) -> Option<Ident> {
        match self {
            InterfaceDeclaration::File(InterfaceFileDeclaration { ref ident, .. }) => {
                Some(ident.to_owned())
            }
            InterfaceDeclaration::Object(InterfaceObjectDeclaration { ref ident, .. }) => {
                Some(ident.to_owned())
            }
            InterfaceDeclaration::Type(ref ident) => Some(ident.to_owned()),
            // @TODO ignore for now
            InterfaceDeclaration::Subprogram(..) => None,
            InterfaceDeclaration::Package(ref package) => Some(package.ident.clone()),
        }
    }
}
fn check_unique<'a>(
    idents: &mut FnvHashMap<Symbol, SrcPos>,
    ident: Ident,
    messages: &mut MessageHandler,
) {
    match idents.entry(ident.item.clone()) {
        Entry::Occupied(entry) => {
            let msg = Message::error(
                &ident,
                format!("Duplicate declaration of '{}'", ident.item.name()),
            ).related(entry.get(), "Previously defined here");
            messages.push(msg)
        }
        Entry::Vacant(entry) => {
            entry.insert(ident.pos);
        }
    }
}

/// Check that no homographs are defined in the element declarations
fn check_element_declaration_unique_ident(
    declarations: &[ElementDeclaration],
    messages: &mut MessageHandler,
) {
    let mut idents = FnvHashMap::default();
    for decl in declarations.iter() {
        check_unique(&mut idents, decl.ident.clone(), messages);
    }
}

/// Check that no homographs are defined in the interface list
fn check_interface_list_unique_ident(
    declarations: &[InterfaceDeclaration],
    messages: &mut MessageHandler,
) {
    let mut idents = FnvHashMap::default();
    for decl in declarations.iter() {
        if let Some(ident) = decl.ident() {
            check_unique(&mut idents, ident, messages);
        }
    }
}

impl SubprogramDeclaration {
    fn interface_list<'a>(&'a self) -> &[InterfaceDeclaration] {
        match self {
            SubprogramDeclaration::Function(fun) => &fun.parameter_list,
            SubprogramDeclaration::Procedure(proc) => &proc.parameter_list,
        }
    }
}

/// Check that no homographs are defined in the declarative region
fn check_declarative_part_unique_ident(
    declarations: &[Declaration],
    messages: &mut MessageHandler,
) {
    let mut idents = FnvHashMap::default();
    for decl in declarations.iter() {
        if let Some(ident) = decl.ident() {
            check_unique(&mut idents, ident, messages);
        }

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

fn check_package_declaration(package: &PackageDeclaration, messages: &mut MessageHandler) {
    check_declarative_part_unique_ident(&package.decl, messages);
}

fn check_architecture_body(architecture: &ArchitectureBody, messages: &mut MessageHandler) {
    check_declarative_part_unique_ident(&architecture.decl, messages);
    check_concurrent_part(&architecture.statements, messages);
}

fn check_package_body(package: &PackageBody, messages: &mut MessageHandler) {
    check_declarative_part_unique_ident(&package.decl, messages);
}

fn check_entity_declaration(entity: &EntityDeclaration, messages: &mut MessageHandler) {
    if let Some(ref list) = entity.generic_clause {
        check_interface_list_unique_ident(list, messages);
    }
    if let Some(ref list) = entity.port_clause {
        check_interface_list_unique_ident(list, messages);
    }
    check_declarative_part_unique_ident(&entity.decl, messages);
    check_concurrent_part(&entity.statements, messages);
}

fn check_primary_design_unit(design_unit: &DesignUnit<PrimaryUnit>, messages: &mut MessageHandler) {
    match &design_unit {
        DesignUnit {
            unit: PrimaryUnit::PackageDeclaration(package),
            ..
        } => check_package_declaration(package, messages),
        DesignUnit {
            unit: PrimaryUnit::EntityDeclaration(entity),
            ..
        } => check_entity_declaration(entity, messages),
        // @TODO others
        _ => {}
    }
}

fn check_secondary_design_unit(
    design_unit: &DesignUnit<SecondaryUnit>,
    messages: &mut MessageHandler,
) {
    match &design_unit {
        DesignUnit {
            unit: SecondaryUnit::Architecture(architecture),
            ..
        } => check_architecture_body(architecture, messages),
        DesignUnit {
            unit: SecondaryUnit::PackageBody(package),
            ..
        } => check_package_body(package, messages),
    }
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
            for primary_unit in library.iter_primary_units() {
                self.check_context_clause(root, &primary_unit.unit.context_clause, messages);
                check_primary_design_unit(&primary_unit.unit, messages);
                for secondary_unit in primary_unit.iter_secondary_units() {
                    self.check_context_clause(root, &secondary_unit.context_clause, messages);
                    check_secondary_design_unit(secondary_unit, messages);
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
    use test_util::{check_no_messages, Code, CodeBuilder};

    fn expected_messages(code: &Code, names: &[&str]) -> Vec<Message> {
        let mut messages = Vec::new();
        for name in names {
            messages.push(
                Message::error(
                    code.s(&name, 2),
                    format!("Duplicate declaration of '{}'", &name),
                ).related(code.s1(&name), "Previously defined here"),
            )
        }
        messages
    }

    #[test]
    fn allows_unique_names() {
        let code = Code::new(
            "
constant a : natural;
constant b : natural;
constant c : natural;
",
        );

        let mut messages = Vec::new();
        check_declarative_part_unique_ident(&code.declarative_part(), &mut messages);
        check_no_messages(&messages);
    }

    #[test]
    fn forbid_homographs() {
        let code = Code::new(
            "
constant a1 : natural;
constant a : natural;
constant a1 : natural;
",
        );

        let mut messages = Vec::new();
        check_declarative_part_unique_ident(&code.declarative_part(), &mut messages);
        assert_eq!(messages, expected_messages(&code, &["a1"]));
    }

    #[test]
    fn allows_protected_type_and_body_with_same_name() {
        let code = Code::new(
            "
type prot_t is protected
end protected;

type prot_t is protected body
end protected body;
",
        );

        let mut messages = Vec::new();
        check_declarative_part_unique_ident(&code.declarative_part(), &mut messages);
        check_no_messages(&messages);
    }

    #[test]
    fn allows_incomplete_type_definition() {
        let code = Code::new(
            "
type rec_t;
type rec_t is record
end record;
",
        );

        let mut messages = Vec::new();
        check_declarative_part_unique_ident(&code.declarative_part(), &mut messages);
        check_no_messages(&messages);
    }

    #[test]
    fn forbid_homographs_in_subprogram_bodies() {
        let code = Code::new(
            "
procedure proc(a1, a, a1 : natural) is
  constant b1 : natural;
  constant b : natural;
  constant b1 : natural;

  procedure nested_proc(c1, c, c1 : natural) is
    constant d1 : natural;
    constant d : natural;
    constant d1 : natural;
  begin
  end;

begin
end;
",
        );

        let mut messages = Vec::new();
        check_declarative_part_unique_ident(&code.declarative_part(), &mut messages);
        assert_eq!(
            messages,
            expected_messages(&code, &["a1", "b1", "c1", "d1"])
        );
    }

    #[test]
    fn forbid_homographs_in_component_declarations() {
        let code = Code::new(
            "
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
",
        );

        let mut messages = Vec::new();
        check_declarative_part_unique_ident(&code.declarative_part(), &mut messages);
        assert_eq!(messages, expected_messages(&code, &["a1", "b1"]));
    }

    #[test]
    fn forbid_homographs_in_record_type_declarations() {
        let code = Code::new(
            "
type rec_t is record
  a1 : natural;
  a : natural;
  a1 : natural;
end record;
",
        );

        let mut messages = Vec::new();
        check_declarative_part_unique_ident(&code.declarative_part(), &mut messages);
        assert_eq!(messages, expected_messages(&code, &["a1"]));
    }

    #[test]
    fn forbid_homographs_in_proteced_type_declarations() {
        let code = Code::new(
            "
type prot_t is protected
  procedure proc(a1, a, a1 : natural);
end protected;

type prot_t is protected body
  constant b1 : natural;
  constant b : natural;
  constant b1 : natural;
end protected body;
",
        );

        let mut messages = Vec::new();
        check_declarative_part_unique_ident(&code.declarative_part(), &mut messages);
        assert_eq!(messages, expected_messages(&code, &["a1", "b1"]));
    }

    #[test]
    fn forbid_homographs_in_subprogram_declarations() {
        let code = Code::new(
            "
procedure proc(a1, a, a1 : natural);
function fun(b1, a, b1 : natural) return natural;
",
        );

        let mut messages = Vec::new();
        check_declarative_part_unique_ident(&code.declarative_part(), &mut messages);
        assert_eq!(messages, expected_messages(&code, &["a1", "b1"]));
    }

    #[test]
    fn forbid_homographs_in_block() {
        let code = Code::new(
            "
blk : block
  constant a1 : natural;
  constant a : natural;
  constant a1 : natural;
begin
  process
    constant b1 : natural;
    constant b : natural;
    constant b1 : natural;
  begin
  end process;
end block;
",
        );

        let mut messages = Vec::new();
        check_concurrent_statement(&code.concurrent_statement(), &mut messages);
        assert_eq!(messages, expected_messages(&code, &["a1", "b1"]));
    }

    #[test]
    fn forbid_homographs_in_process() {
        let code = Code::new(
            "
process
  constant a1 : natural;
  constant a : natural;
  constant a1 : natural;
begin
end process;
",
        );

        let mut messages = Vec::new();
        check_concurrent_statement(&code.concurrent_statement(), &mut messages);
        assert_eq!(messages, expected_messages(&code, &["a1"]));
    }

    #[test]
    fn forbid_homographs_for_generate() {
        let code = Code::new(
            "
gen_for: for i in 0 to 3 generate
  constant a1 : natural;
  constant a : natural;
  constant a1 : natural;
begin
  process
    constant b1 : natural;
    constant b : natural;
    constant b1 : natural;
  begin
  end process;
end generate;
",
        );

        let mut messages = Vec::new();
        check_concurrent_statement(&code.concurrent_statement(), &mut messages);
        assert_eq!(messages, expected_messages(&code, &["a1", "b1"]));
    }

    #[test]
    fn forbid_homographs_if_generate() {
        let code = Code::new(
            "
gen_if: if true generate
  constant a1 : natural;
  constant a : natural;
  constant a1 : natural;
begin

  prcss : process
    constant b1 : natural;
    constant b : natural;
    constant b1 : natural;
  begin
  end process;

else generate
  constant c1 : natural;
  constant c: natural;
  constant c1 : natural;
begin
  prcss : process
    constant d1 : natural;
    constant d : natural;
    constant d1 : natural;
  begin
  end process;
end generate;
",
        );

        let mut messages = Vec::new();
        check_concurrent_statement(&code.concurrent_statement(), &mut messages);
        assert_eq!(
            messages,
            expected_messages(&code, &["a1", "b1", "c1", "d1"])
        );
    }

    #[test]
    fn forbid_homographs_case_generate() {
        let code = Code::new(
            "
gen_case: case 0 generate
  when others =>
    constant a1 : natural;
    constant a : natural;
    constant a1 : natural;
  begin
    process
      constant b1 : natural;
      constant b : natural;
      constant b1 : natural;
    begin
    end process;
end generate;
",
        );

        let mut messages = Vec::new();
        check_concurrent_statement(&code.concurrent_statement(), &mut messages);
        assert_eq!(messages, expected_messages(&code, &["a1", "b1"]));
    }

    #[test]
    fn forbid_homographs_in_entity_declarations() {
        let code = Code::new(
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
  constant c1 : natural;
  constant c : natural;
  constant c1 : natural;
begin

  blk : block
    constant d1 : natural;
    constant d : natural;
    constant d1 : natural;
  begin

  end block;

end entity;
",
        );

        let mut messages = Vec::new();
        check_entity_declaration(&code.entity(), &mut messages);
        assert_eq!(
            messages,
            expected_messages(&code, &["a1", "b1", "c1", "d1"])
        );
    }

    #[test]
    fn forbid_homographs_in_architecture_bodies() {
        let code = Code::new(
            "
architecture arch of ent is
  constant a1 : natural;
  constant a : natural;
  constant a1 : natural;
begin

  blk : block
    constant b1 : natural;
    constant b : natural;
    constant b1 : natural;
  begin
  end block;

end architecture;
",
        );

        let mut messages = Vec::new();
        check_architecture_body(&code.architecture(), &mut messages);
        assert_eq!(messages, expected_messages(&code, &["a1", "b1"]));
    }

    #[test]
    fn forbid_homographs_of_type_declarations() {
        let code = Code::new(
            "
constant a1 : natural := 0;
type a1 is (foo, bar);
",
        );

        let mut messages = Vec::new();
        check_declarative_part_unique_ident(&code.declarative_part(), &mut messages);
        assert_eq!(messages, expected_messages(&code, &["a1"]));
    }

    #[test]
    fn forbid_homographs_of_component_declarations() {
        let code = Code::new(
            "
constant a1 : natural := 0;
component a1 is
  port (clk : bit);
end component;
",
        );

        let mut messages = Vec::new();
        check_declarative_part_unique_ident(&code.declarative_part(), &mut messages);
        assert_eq!(messages, expected_messages(&code, &["a1"]));
    }

    #[test]
    fn forbid_homographs_of_file_declarations() {
        let code = Code::new(
            "
constant a1 : natural := 0;
file a1 : text;
",
        );

        let mut messages = Vec::new();
        check_declarative_part_unique_ident(&code.declarative_part(), &mut messages);
        assert_eq!(messages, expected_messages(&code, &["a1"]));
    }

    #[test]
    fn forbid_homographs_in_package_declarations() {
        let code = Code::new(
            "
package a1 is new pkg generic map (foo => bar);
package a1 is new pkg generic map (foo => bar);
",
        );

        let mut messages = Vec::new();
        check_declarative_part_unique_ident(&code.declarative_part(), &mut messages);
        assert_eq!(messages, expected_messages(&code, &["a1"]));
    }

    #[test]
    fn forbid_homographs_in_attribute_declarations() {
        let code = Code::new(
            "
attribute a1 : string;
attribute a1 : string;
",
        );

        let mut messages = Vec::new();
        check_declarative_part_unique_ident(&code.declarative_part(), &mut messages);
        assert_eq!(messages, expected_messages(&code, &["a1"]));
    }

    #[test]
    fn forbid_homographs_in_alias_declarations() {
        let code = Code::new(
            "
alias a1 is foo;
alias a1 is bar;

-- Legal since subprograms are overloaded
alias b1 is foo[return natural];
alias b1 is bar[return boolean];
",
        );

        let mut messages = Vec::new();
        check_declarative_part_unique_ident(&code.declarative_part(), &mut messages);
        assert_eq!(messages, expected_messages(&code, &["a1"]));
    }

    #[test]
    fn forbid_homographs_in_interface_file_declarations() {
        let code = Code::new(
            "
procedure proc(file a1, a, a1 : text);
",
        );

        let mut messages = Vec::new();
        check_declarative_part_unique_ident(&code.declarative_part(), &mut messages);
        assert_eq!(messages, expected_messages(&code, &["a1"]));
    }

    #[test]
    fn forbid_homographs_in_interface_type_declarations() {
        let code = Code::new(
            "
entity ent is
  generic (
    type a1;
    type a1
  );
end entity;
",
        );

        let mut messages = Vec::new();
        check_entity_declaration(&code.entity(), &mut messages);
        assert_eq!(messages, expected_messages(&code, &["a1"]));
    }

    #[test]
    fn forbid_homographs_in_interface_package_declarations() {
        let code = Code::new(
            "
entity ent is
  generic (
    package a1 is new pkg generic map (<>);
    package a1 is new pkg generic map (<>)
  );
end entity;
",
        );

        let mut messages = Vec::new();
        check_entity_declaration(&code.entity(), &mut messages);
        assert_eq!(messages, expected_messages(&code, &["a1"]));
    }

    #[test]
    fn check_library_clause_library_exists() {
        let mut builder = LibraryBuilder::new();
        let code = builder.add_code(
            "libname",
            "
library missing_lib;

entity ent is
end entity;
            ",
        );

        let messages = builder.analyze();

        assert_eq!(
            messages,
            vec![Message::error(
                code.s1("missing_lib"),
                "No such library 'missing_lib'"
            )]
        )
    }

    #[test]
    fn library_std_is_pre_defined() {
        let mut builder = LibraryBuilder::new();
        builder.add_code(
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
        let code = builder.add_code(
            "libname",
            "
library work;

entity ent is
end entity;
            ",
        );

        let messages = builder.analyze();

        assert_eq!(
            messages,
            vec![Message::hint(
                code.s1("work"),
                "Library clause not necessary for current working library"
            )]
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
        fn add_code(&mut self, library_name: &str, code: &str) -> Code {
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
