// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use ast::{has_ident::HasIdent, *};
use declarative_region::{AnyDeclaration, DeclarativeRegion, VisibleDeclaration};
use latin_1::Latin1String;
use library::{DesignRoot, Library};
use message::{Message, MessageHandler};
use source::{SrcPos, WithPos};
use symbol_table::{Symbol, SymbolTable};

use std::sync::Arc;

/// Check that no homographs are defined in the element declarations
fn check_element_declaration_unique_ident(
    declarations: &[ElementDeclaration],
    messages: &mut MessageHandler,
) {
    let mut region = DeclarativeRegion::new(None);
    region.add_element_declarations(declarations, messages);
    region.close_both(messages);
}

/// Check that no homographs are defined in the interface list
fn check_interface_list_unique_ident(
    declarations: &[InterfaceDeclaration],
    messages: &mut MessageHandler,
) {
    let mut region = DeclarativeRegion::new(None);
    region.add_interface_list(declarations, messages);
    region.close_both(messages);
}

impl SubprogramDeclaration {
    fn interface_list(&self) -> &[InterfaceDeclaration] {
        match self {
            SubprogramDeclaration::Function(fun) => &fun.parameter_list,
            SubprogramDeclaration::Procedure(proc) => &proc.parameter_list,
        }
    }
}

pub struct Analyzer<'a> {
    work_sym: Symbol,
    std_sym: Symbol,
    root: &'a DesignRoot<'a>,
}

impl<'r, 'a: 'r> Analyzer<'a> {
    pub fn new(root: &'a DesignRoot<'a>, symtab: &Arc<SymbolTable>) -> Analyzer<'a> {
        Analyzer {
            work_sym: symtab.insert(&Latin1String::new(b"work")),
            std_sym: symtab.insert(&Latin1String::new(b"std")),
            root,
        }
    }

    /// Returns the VisibleDeclaration or None if it was not a selected name
    /// Returns error message if a name was not declared
    /// @TODO We only lookup selected names since other names such as slice and index require typechecking
    /// @TODO return borrowed data and own VisibleDeclarations inside the Library
    pub fn lookup_selected_name(
        &self,
        region: &DeclarativeRegion<'r, 'a>,
        name: &WithPos<Name>,
    ) -> Result<Option<VisibleDeclaration<'a>>, Message> {
        match name.item {
            Name::Selected(ref prefix, ref suffix) => {
                let visible_decl = self.lookup_selected_name(region, prefix)?;
                if let Some(visible_decl) = visible_decl {
                    match visible_decl.decl {
                        AnyDeclaration::Library(ref library) => {
                            let ident = {
                                match suffix.item {
                                    Designator::Identifier(ref ident) => ident,
                                    _ => {
                                        // Only identifiers can denote primary units
                                        return Err(Message::error(
                                            suffix.as_ref(),
                                            format!(
                                                "No primary unit '{}' within '{}'",
                                                suffix.item, &library.name
                                            ),
                                        ));
                                    }
                                }
                            };

                            if let Some(package) = library.package(ident) {
                                let decl = VisibleDeclaration {
                                    designator: Designator::Identifier(
                                        package.package.unit.ident.item.clone(),
                                    ),
                                    decl: AnyDeclaration::Package(package),
                                    decl_pos: Some(package.package.unit.ident.pos.clone()),
                                    may_overload: false,
                                };

                                Ok(Some(decl))
                            } else if let Some(context) = library.context(ident) {
                                let decl = VisibleDeclaration {
                                    designator: Designator::Identifier(context.ident.item.clone()),
                                    decl: AnyDeclaration::Context(context),
                                    decl_pos: Some(context.ident.pos.clone()),
                                    may_overload: false,
                                };

                                Ok(Some(decl))
                            } else if let Some(entity) = library.entity(ident) {
                                let decl = VisibleDeclaration {
                                    designator: Designator::Identifier(
                                        entity.entity.unit.ident.item.clone(),
                                    ),
                                    decl: AnyDeclaration::Entity(entity),
                                    decl_pos: Some(entity.entity.unit.ident.pos.clone()),
                                    may_overload: false,
                                };

                                Ok(Some(decl))
                            } else if let Some(configuration) = library.configuration(ident) {
                                let decl = VisibleDeclaration {
                                    designator: Designator::Identifier(
                                        configuration.ident().item.clone(),
                                    ),
                                    decl: AnyDeclaration::Configuration(configuration),
                                    decl_pos: Some(configuration.ident().pos.clone()),
                                    may_overload: false,
                                };

                                Ok(Some(decl))
                            } else if let Some(instance) = library.package_instance(ident) {
                                let decl = VisibleDeclaration {
                                    designator: Designator::Identifier(
                                        instance.ident().item.clone(),
                                    ),
                                    decl: AnyDeclaration::PackageInstance(instance),
                                    decl_pos: Some(instance.ident().pos.clone()),
                                    may_overload: false,
                                };

                                Ok(Some(decl))
                            } else {
                                Err(Message::error(
                                    suffix.as_ref(),
                                    format!(
                                        "No primary unit '{}' within '{}'",
                                        suffix.item, &library.name
                                    ),
                                ))
                            }
                        }

                        AnyDeclaration::Package(ref package) => {
                            if let Some(region) = package.declarative_region.borrow().as_ref() {
                                if let Some(visible_decl) = region.lookup(&suffix.item) {
                                    Ok(Some(visible_decl.clone()))
                                } else {
                                    Err(Message::error(
                                        suffix.as_ref(),
                                        format!(
                                            "No declaration of '{}' within package '{}'",
                                            suffix.item,
                                            &package.package.name()
                                        ),
                                    ))
                                }
                            } else {
                                // @TODO analyse unit now if it has not already been analyzed
                                // but avoid circular dependencies
                                // @TODO just return the prefix for now to not raise error
                                Ok(Some(visible_decl.clone()))
                            }
                        }

                        // @TODO ignore other declarations for now, just return the prefix to not raise error
                        _ => Ok(Some(visible_decl)),
                    }
                } else {
                    Ok(None)
                }
            }

            // @TODO return Vec for .all
            Name::SelectedAll(ref prefix) => self.lookup_selected_name(region, prefix),
            Name::Designator(ref designator) => {
                if let Some(visible_item) = region.lookup(&designator) {
                    Ok(Some(visible_item.clone()))
                } else {
                    Err(Message::error(
                        &name.pos,
                        format!("No declaration of '{}'", designator),
                    ))
                }
            }
            _ => {
                // Not a selected name
                // @TODO at least lookup prefix for now
                Ok(None)
            }
        }
    }

    fn analyze_declaration(
        &self,
        region: &mut DeclarativeRegion<'r, 'a>,
        decl: &'a Declaration,
        messages: &mut MessageHandler,
    ) {
        match decl {
            Declaration::Alias(alias) => region.add(
                VisibleDeclaration::new(
                    alias.designator.clone(),
                    AnyDeclaration::Declaration(decl),
                ).with_overload(alias.signature.is_some()),
                messages,
            ),
            Declaration::Object(ObjectDeclaration { ref ident, .. }) => {
                region.add(
                    VisibleDeclaration::new(ident, AnyDeclaration::Declaration(decl)),
                    messages,
                );
            }
            Declaration::File(FileDeclaration { ref ident, .. }) => region.add(
                VisibleDeclaration::new(ident, AnyDeclaration::Declaration(decl)),
                messages,
            ),
            Declaration::Component(ref component) => {
                region.add(
                    VisibleDeclaration::new(&component.ident, AnyDeclaration::Declaration(decl)),
                    messages,
                );
                check_interface_list_unique_ident(&component.generic_list, messages);
                check_interface_list_unique_ident(&component.port_list, messages);
            }
            Declaration::Attribute(ref attr) => match attr {
                Attribute::Declaration(AttributeDeclaration { ref ident, .. }) => {
                    region.add(
                        VisibleDeclaration::new(ident, AnyDeclaration::Declaration(decl)),
                        messages,
                    );
                }
                // @TODO Ignored for now
                Attribute::Specification(..) => {}
            },
            Declaration::SubprogramBody(body) => {
                region.add(
                    VisibleDeclaration::new(
                        body.specification.designator(),
                        AnyDeclaration::Declaration(decl),
                    ).with_overload(true),
                    messages,
                );
                check_interface_list_unique_ident(body.specification.interface_list(), messages);
                // @TODO parent
                let mut region = DeclarativeRegion::new(None);
                self.analyze_declarative_part(&mut region, &body.declarations, messages);
            }
            Declaration::SubprogramDeclaration(subdecl) => {
                region.add(
                    VisibleDeclaration::new(
                        subdecl.designator(),
                        AnyDeclaration::Declaration(decl),
                    ).with_overload(true),
                    messages,
                );
                check_interface_list_unique_ident(subdecl.interface_list(), messages);
            }

            // @TODO Ignored for now
            Declaration::Use(ref use_clause) => {
                self.analyze_use_clause(region, &use_clause.item, &use_clause.pos, messages);
            }
            Declaration::Package(ref package) => region.add(
                VisibleDeclaration::new(&package.ident, AnyDeclaration::Declaration(decl)),
                messages,
            ),
            Declaration::Configuration(..) => {}
            Declaration::Type(TypeDeclaration {
                ref ident,
                def: TypeDefinition::Enumeration(ref enumeration),
            }) => {
                region.add(
                    VisibleDeclaration::new(ident, AnyDeclaration::Declaration(decl)),
                    messages,
                );
                for literal in enumeration.iter() {
                    region.add(
                        VisibleDeclaration::new(
                            literal.clone().map_into(|lit| lit.into_designator()),
                            AnyDeclaration::Enum(literal),
                        ).with_overload(true),
                        messages,
                    )
                }
            }
            Declaration::Type(ref type_decl) => {
                region.add(
                    VisibleDeclaration::new(&type_decl.ident, AnyDeclaration::Declaration(decl)),
                    messages,
                );

                match type_decl.def {
                    TypeDefinition::ProtectedBody(ref body) => {
                        // @TODO parent
                        let mut region = DeclarativeRegion::new(None);
                        self.analyze_declarative_part(&mut region, &body.decl, messages);
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
                }
            }
        }
    }

    fn analyze_declarative_part(
        &self,
        region: &mut DeclarativeRegion<'r, 'a>,
        declarations: &'a [Declaration],
        messages: &mut MessageHandler,
    ) {
        for decl in declarations.iter() {
            self.analyze_declaration(region, decl, messages);
        }
    }

    fn analyze_use_clause(
        &self,
        region: &mut DeclarativeRegion<'r, 'a>,
        use_clause: &UseClause,
        use_pos: &SrcPos,
        messages: &mut MessageHandler,
    ) {
        for name in use_clause.name_list.iter() {
            match name.item {
                Name::Selected(..) => {}
                Name::SelectedAll(..) => {}
                _ => {
                    messages.push(Message::error(
                        &use_pos,
                        "Use clause must be a selected name",
                    ));
                    continue;
                }
            }

            match self.lookup_selected_name(&region, &name) {
                Ok(Some(visible_decl)) => {
                    // @TODO handle others
                    if let AnyDeclaration::Package(ref package) = visible_decl.decl {
                        region.make_package_visible(package.package.name(), package);
                    }
                }
                Ok(None) => {
                    messages.push(Message::error(
                        &use_pos,
                        "Use clase must be a selected name",
                    ));
                }
                Err(msg) => {
                    messages.push(msg);
                }
            }
        }
    }

    fn analyze_context_clause(
        &self,
        region: &mut DeclarativeRegion<'r, 'a>,
        context_clause: &[WithPos<ContextItem>],
        messages: &mut MessageHandler,
    ) {
        for context_item in context_clause.iter() {
            match context_item.item {
                ContextItem::Library(LibraryClause { ref name_list }) => {
                    for library_name in name_list.iter() {
                        if self.work_sym == library_name.item {
                            messages.push(Message::hint(
                                &library_name,
                                "Library clause not necessary for current working library",
                            ))
                        } else if let Some(library) = self.root.get_library(&library_name.item) {
                            region.make_library_visible(&library.name, library);
                        } else {
                            messages.push(Message::error(
                                &library_name,
                                format!("No such library '{}'", library_name.item),
                            ));
                        }
                    }
                }
                ContextItem::Use(ref use_clause) => {
                    self.analyze_use_clause(region, use_clause, &context_item.pos, messages);
                }
                ContextItem::Context(ContextReference { ref name_list }) => {
                    for name in name_list {
                        match name.item {
                            Name::Selected(..) => {}
                            _ => {
                                messages.push(Message::error(
                                    &context_item,
                                    "Context reference must be a selected name",
                                ));
                                continue;
                            }
                        }

                        match self.lookup_selected_name(&region, &name) {
                            Ok(Some(visible_decl)) => {
                                match visible_decl.decl {
                                    // OK
                                    AnyDeclaration::Context(ref context) => {
                                        // Error will be given when
                                        // analyzing the context
                                        // clause specifically and
                                        // shall not be duplicated
                                        // here
                                        let mut ignore_messages = Vec::new();
                                        self.analyze_context_clause(
                                            region,
                                            &context.items,
                                            &mut ignore_messages,
                                        );
                                    }
                                    _ => {
                                        // @TODO maybe lookup should return the source position of the suffix
                                        if let Name::Selected(_, ref suffix) = name.item {
                                            messages.push(Message::error(
                                                &suffix,
                                                format!(
                                                    "'{}' does not denote a context declaration",
                                                    &suffix.item
                                                ),
                                            ));
                                        }
                                    }
                                }
                            }
                            Ok(None) => {
                                messages.push(Message::error(
                                    &context_item,
                                    "Context reference must be a selected name",
                                ));
                            }
                            Err(msg) => {
                                messages.push(msg);
                            }
                        }
                    }
                }
            }
        }
    }

    fn analyze_generate_body(
        &self,
        parent: &DeclarativeRegion<'r, 'a>,
        body: &'a GenerateBody,
        messages: &mut MessageHandler,
    ) {
        let mut region = DeclarativeRegion::new(Some(parent));

        if let Some(ref decl) = body.decl {
            self.analyze_declarative_part(&mut region, &decl, messages);
        }
        self.analyze_concurrent_part(&region, &body.statements, messages);
    }

    fn analyze_concurrent_statement(
        &self,
        parent: &DeclarativeRegion<'r, 'a>,
        statement: &'a LabeledConcurrentStatement,
        messages: &mut MessageHandler,
    ) {
        match statement.statement {
            ConcurrentStatement::Block(ref block) => {
                // @TODO parent
                let mut region = DeclarativeRegion::new(Some(parent));
                self.analyze_declarative_part(&mut region, &block.decl, messages);
                self.analyze_concurrent_part(&region, &block.statements, messages);
            }
            ConcurrentStatement::Process(ref process) => {
                // @TODO parent
                let mut region = DeclarativeRegion::new(Some(parent));
                self.analyze_declarative_part(&mut region, &process.decl, messages);
            }
            ConcurrentStatement::ForGenerate(ref gen) => {
                self.analyze_generate_body(parent, &gen.body, messages);
            }
            ConcurrentStatement::IfGenerate(ref gen) => {
                for conditional in gen.conditionals.iter() {
                    self.analyze_generate_body(parent, &conditional.item, messages);
                }
                if let Some(ref else_item) = gen.else_item {
                    self.analyze_generate_body(parent, else_item, messages);
                }
            }
            ConcurrentStatement::CaseGenerate(ref gen) => {
                for alternative in gen.alternatives.iter() {
                    self.analyze_generate_body(parent, &alternative.item, messages);
                }
            }
            _ => {}
        }
    }

    fn analyze_concurrent_part(
        &self,
        parent: &DeclarativeRegion<'r, 'a>,
        statements: &'a [LabeledConcurrentStatement],
        messages: &mut MessageHandler,
    ) {
        for statement in statements.iter() {
            self.analyze_concurrent_statement(parent, statement, messages);
        }
    }

    fn analyze_package_declaration(
        &self,
        package: &'a PackageDeclaration,
        messages: &mut MessageHandler,
    ) -> DeclarativeRegion<'r, 'a> {
        let mut region = DeclarativeRegion::new(None).in_package_declaration();
        if let Some(ref list) = package.generic_clause {
            region.add_interface_list(list, messages);
        }
        self.analyze_declarative_part(&mut region, &package.decl, messages);
        region
    }

    fn analyze_architecture_body(
        &self,
        entity_region: &mut DeclarativeRegion<'r, 'a>,
        architecture: &'a ArchitectureBody,
        messages: &mut MessageHandler,
    ) {
        self.analyze_declarative_part(entity_region, &architecture.decl, messages);
        self.analyze_concurrent_part(entity_region, &architecture.statements, messages);
    }

    fn analyze_package_body(
        &self,
        package_region: &mut DeclarativeRegion<'r, 'a>,
        package: &'a PackageBody,
        messages: &mut MessageHandler,
    ) {
        self.analyze_declarative_part(package_region, &package.decl, messages);
    }

    fn analyze_entity_declaration(
        &self,
        region: &mut DeclarativeRegion<'r, 'a>,
        entity: &'a EntityDeclaration,
        messages: &mut MessageHandler,
    ) {
        if let Some(ref list) = entity.generic_clause {
            region.add_interface_list(list, messages);
        }
        if let Some(ref list) = entity.port_clause {
            region.add_interface_list(list, messages);
        }
        self.analyze_declarative_part(region, &entity.decl, messages);
        self.analyze_concurrent_part(region, &entity.statements, messages);
    }

    /// Create a new root region for a design unit, making the
    /// standard library and working library visible
    pub fn new_root_region(&self, work: &'a Library<'a>) -> DeclarativeRegion<'r, 'a> {
        let mut region = DeclarativeRegion::new(None);
        region.make_library_visible(&self.work_sym, work);

        // @TODO maybe add warning if standard library is missing
        if let Some(library) = self.root.get_library(&self.std_sym) {
            region.make_library_visible(&self.std_sym, library);
        }
        region
    }

    pub fn analyze(&self, messages: &mut MessageHandler) {
        for library in self.root.iter_libraries() {
            for package in library.packages() {
                let mut root_region = self.new_root_region(library);
                self.analyze_context_clause(
                    &mut root_region,
                    &package.package.context_clause,
                    messages,
                );
                let mut region = self.analyze_package_declaration(&package.package.unit, messages);

                // @TODO may panic
                // @TODO avoid duplicate analysis
                package.declarative_region.replace(Some(region.clone()));

                if let Some(ref body) = package.body {
                    region.close_immediate(messages);
                    self.analyze_context_clause(&mut root_region, &body.context_clause, messages);
                    let mut region = region.in_body();
                    self.analyze_package_body(&mut region, &body.unit, messages);
                    region.close_both(messages);
                } else {
                    region.close_both(messages);
                }
            }
        }

        for library in self.root.iter_libraries() {
            for package_instance in library.package_instances() {
                let mut root_region = self.new_root_region(library);
                self.analyze_context_clause(
                    &mut root_region,
                    &package_instance.context_clause,
                    messages,
                );
            }

            for context in library.contexts() {
                let mut root_region = self.new_root_region(library);
                self.analyze_context_clause(&mut root_region, &context.items, messages);
            }

            for entity in library.entities() {
                let mut root_region = self.new_root_region(library);
                self.analyze_context_clause(
                    &mut root_region,
                    &entity.entity.context_clause,
                    messages,
                );
                let mut region = DeclarativeRegion::new(Some(&root_region));
                self.analyze_entity_declaration(&mut region, &entity.entity.unit, messages);
                region.close_immediate(messages);
                for architecture in entity.architectures.values() {
                    let mut root_region = region.clone();
                    self.analyze_context_clause(
                        &mut root_region,
                        &architecture.context_clause,
                        messages,
                    );
                    let mut region = region.in_body();
                    self.analyze_architecture_body(&mut region, &architecture.unit, messages);
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
        let code_pkg = builder.code(
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

        let code_ent = builder.code(
            "libname",
            "
entity ent is
end entity;

architecture rtl of ent is
  type rec_t;
begin
  blk : block
    -- Must appear in the same immediate declarative region
    type rec_t is record
    end record;
  begin
  end block;
end architecture;
",
        );

        let code_pkg2 = builder.code(
            "libname",
            "
-- To check that no duplicate errors are made when closing the immediate and extended regions
package pkg2 is
  type rec_t;
end package;

package body pkg2 is
end package body;
",
        );

        let mut expected_messages = Vec::new();
        for code in [code_pkg, code_ent, code_pkg2].iter() {
            expected_messages.push(Message::error(
                code.s1("rec_t"),
                "Missing full type declaration of incomplete type 'rec_t'",
            ));
            expected_messages.push(
                Message::hint(
                    code.s1("rec_t"),
                    "The full type declaration shall occur immediately within the same declarative part",
                ));
        }

        let messages = builder.analyze();
        check_messages(messages, expected_messages);
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
    fn library_clause_extends_into_secondary_units() {
        let mut builder = LibraryBuilder::new();
        builder.code(
            "libname",
            "
-- Package will be used for testing
package usepkg is
  constant const : natural := 0;
end package;

-- This should be visible also in architectures
library libname;

entity ent is
end entity;

use libname.usepkg;

architecture rtl of ent is
begin
end architecture;

-- This should be visible also in package body
library libname;
use libname.usepkg;

package pkg is
end package;

use usepkg.const;

package body pkg is
end package body;
            ",
        );

        let messages = builder.analyze();

        check_no_messages(&messages);
    }

    #[test]
    fn check_library_clause_library_exists_in_context_declarations() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
context ctx is
  library missing_lib;
end context;
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
    fn context_clause_makes_names_visible() {
        let mut builder = LibraryBuilder::new();
        builder.code(
            "libname",
            "
-- Package will be used for testing
package usepkg is
  constant const : natural := 0;
end package;

context ctx is
  library libname;
  use libname.usepkg;
end context;


context work.ctx;
use usepkg.const;

package pkg is
end package;
            ",
        );

        let messages = builder.analyze();

        check_no_messages(&messages);
    }

    #[test]
    fn library_std_is_pre_defined() {
        let mut builder = LibraryBuilder::new();
        builder.code(
            "std",
            "
package standard is
end package;

package textio is
end package;

package env is
end package;
",
        );

        builder.code(
            "libname",
            "
use std.textio.all;

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

    use std::collections::{hash_map::Entry, HashMap};
    struct LibraryBuilder {
        code_builder: CodeBuilder,
        libraries: HashMap<Symbol, Vec<Code>>,
    }

    impl LibraryBuilder {
        fn new() -> LibraryBuilder {
            LibraryBuilder {
                code_builder: CodeBuilder::new(),
                libraries: HashMap::default(),
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
                let library = Library::new(
                    library_name.clone(),
                    &self.code_builder.symbol("work"),
                    design_files,
                    &mut messages,
                );
                root.add_library(library);
            }

            Analyzer::new(&root, &self.code_builder.symtab.clone()).analyze(&mut messages);

            messages
        }
    }

    #[test]
    fn check_use_clause_for_missing_design_unit() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
end package;

package gpkg is
  generic (const : natural);
end package;

entity ent is
end entity;

architecture rtl of ent is
begin
end architecture;

configuration cfg of ent is
  for rtl
  end for;
end configuration;

package ipkg is new work.gpkg
  generic map (
    const => 1
  );

library libname;

-- Should work
use work.pkg;
use libname.pkg.all;
use libname.ent;
use libname.ipkg;
use libname.cfg;

use work.missing_pkg;
use libname.missing_pkg.all;


entity dummy is
end entity;
            ",
        );

        let messages = builder.analyze();

        check_messages(
            messages,
            vec![
                Message::error(
                    code.s("missing_pkg", 1),
                    "No primary unit 'missing_pkg' within 'libname'",
                ),
                Message::error(
                    code.s("missing_pkg", 2),
                    "No primary unit 'missing_pkg' within 'libname'",
                ),
            ],
        )
    }

    #[test]
    fn check_use_clause_for_missing_library_clause() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
end package;

use libname.pkg;

entity dummy is
end entity;
            ",
        );

        let messages = builder.analyze();

        check_messages(
            messages,
            vec![Message::error(
                code.s("libname", 1),
                "No declaration of 'libname'",
            )],
        )
    }

    #[test]
    fn nested_use_clause_missing() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  constant const : natural := 0;
end package;

library libname;

entity ent is
end entity;

architecture rtl of ent is
  use libname.pkg; -- Works
  use libname.pkg1; -- Error
begin
  process
    use pkg.const; -- Works
    use libname.pkg1; -- Error
  begin
  end process;

  blk : block
    use pkg.const; -- Works
    use libname.pkg1; -- Error
  begin
  end block;

end architecture;
            ",
        );

        let messages = builder.analyze();

        check_messages(
            messages,
            vec![
                Message::error(code.s("pkg1", 1), "No primary unit 'pkg1' within 'libname'"),
                Message::error(code.s("pkg1", 2), "No primary unit 'pkg1' within 'libname'"),
                Message::error(code.s("pkg1", 3), "No primary unit 'pkg1' within 'libname'"),
            ],
        )
    }

    #[test]
    fn check_context_reference_for_missing_context() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
context ctx is
end context;

context work.ctx;
context work.missing_ctx;

entity dummy is
end entity;
            ",
        );

        let messages = builder.analyze();

        check_messages(
            messages,
            vec![Message::error(
                code.s1("missing_ctx"),
                "No primary unit 'missing_ctx' within 'libname'",
            )],
        )
    }

    #[test]
    fn check_context_reference_for_non_context() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
end package;

context work.pkg;

entity dummy is
end entity;
            ",
        );

        let messages = builder.analyze();

        check_messages(
            messages,
            vec![Message::error(
                code.s("pkg", 2),
                "'pkg' does not denote a context declaration",
            )],
        )
    }

    #[test]
    fn check_use_clause_and_context_clause_must_be_selected_name() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
library libname;

context libname;
use work;
use libname;

use work.pkg(0);
context work.ctx'range;

entity dummy is
end entity;
            ",
        );

        let messages = builder.analyze();

        check_messages(
            messages,
            vec![
                Message::error(
                    code.s1("context libname;"),
                    "Context reference must be a selected name",
                ),
                Message::error(code.s1("use work;"), "Use clause must be a selected name"),
                Message::error(
                    code.s1("use libname;"),
                    "Use clause must be a selected name",
                ),
                Message::error(
                    code.s1("use work.pkg(0);"),
                    "Use clause must be a selected name",
                ),
                Message::error(
                    code.s1("context work.ctx'range;"),
                    "Context reference must be a selected name",
                ),
            ],
        );
    }

    #[test]
    fn check_two_stage_use_clause_for_missing_name() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  type enum_t is (alpha, beta);
  constant const : enum_t := alpha;
end package;

use work.pkg;
use pkg.const;
use pkg.const2;

package pkg2 is
end package;
            ",
        );
        let messages = builder.analyze();
        check_messages(
            messages,
            vec![Message::error(
                code.s1("const2"),
                "No declaration of 'const2' within package 'pkg'",
            )],
        );
    }
    #[test]
    fn check_use_clause_for_missing_name() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg is
  type enum_t is (alpha, beta);
  constant const : enum_t := alpha;
end package;

use work.pkg.const;
use work.pkg.const2;

package pkg2 is
end package;
            ",
        );
        let messages = builder.analyze();
        check_messages(
            messages,
            vec![Message::error(
                code.s1("const2"),
                "No declaration of 'const2' within package 'pkg'",
            )],
        );
    }

    #[test]
    fn use_clause_cannot_reference_potentially_visible_name() {
        let mut builder = LibraryBuilder::new();
        let code = builder.code(
            "libname",
            "
package pkg2 is
  type enum_t is (alpha, beta);
  constant const1 : enum_t := alpha;
end package;

use work.pkg2.const1;

package pkg is
  constant const2 : enum_t := alpha;
end package;

use work.pkg.const1;
use work.pkg.const2;

entity ent is
end entity;
            ",
        );
        let messages = builder.analyze();
        check_messages(
            messages,
            vec![Message::error(
                code.s("const1", 3),
                "No declaration of 'const1' within package 'pkg'",
            )],
        );
    }
}
