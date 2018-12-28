// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use crate::ast::{DesignFile, 
                 AnyDesignUnit, DesignUnit,
                 PrimaryUnit, EntityDeclaration,
                 SecondaryUnit};
use crate::message::{Message};

// The Listener trait defines functions called by the Notifier when the related AST node is entered or
// exited while traversing the AST. An implementation of the Listener trait is used by registering a
// mutable reference with the Notifier and then calling listen() from the listen trait on the AST entry
// point (e.g. a DesignFile, AnyDesignUnit etc) which implements the Listen trait. Calling listen will
// start a traversal of AST and the Notifier will call the enter_ and exit_ events of the registered 
// Listeners. The Listener trait has empty default implementations for all events so that only the events
// required by the Listener need to be implemented.

#[allow(unused_variables)] // Default implementations for events are empty and do not use any parameters
trait Listener {
    
    // ===================================
    // Results
    // -----------------------------------
    fn messages(&self) -> &Vec<Message>;

    // ===================================
    // Design events
    // -----------------------------------
    
    // Design File
    fn enter_design_file(&mut self, design_file: &mut DesignFile){}
    fn exit_design_file(&mut self, design_file: &DesignFile){}

    // Design Units
    fn enter_any_design_unit(&mut self, any_design_unit: &mut AnyDesignUnit){}
    fn exit_any_design_unit(&mut self, any_design_unit: &AnyDesignUnit){}
    fn enter_primary_unit(&mut self, primary_unit: &mut PrimaryUnit){}
    fn exit_primary_unit(&mut self, primary_unit: &PrimaryUnit){}
    fn enter_entity_declaration(&mut self, entity_declaration: &mut DesignUnit<EntityDeclaration>){}
    fn exit_entity_declaration(&mut self, entity_declaration: &DesignUnit<EntityDeclaration>){}

    fn enter_secondary_unit(&mut self, secondary_unit: &mut SecondaryUnit){}
    fn exit_secondary_unit(&mut self, secondary_unit: &SecondaryUnit){}

}


// The notifier calls AST events for all registered listeners
struct Notifier<'a> {
    messages  : Vec<Message>,
    listeners : Vec<Box<&'a mut Listener>>,
}
impl<'a> Notifier<'a> {
    pub fn new() -> Self {
        Notifier {
            messages : Vec::new(),
            listeners: Vec::new()
        }
    }

    pub fn register<T: Listener + 'a>(&mut self, listener: &'a mut T) {
        self.listeners.push(Box::new(listener));
    }

}
impl<'a> Listener for Notifier<'a> {

    fn messages(&self) -> &Vec<Message> {
        &self.messages
    }

    // Design File
    fn enter_design_file(&mut self, design_file: &mut DesignFile) {
        for listener in &mut self.listeners {
            listener.enter_design_file(design_file);
        }
    }
    fn exit_design_file(&mut self, design_file: &DesignFile) {
        for listener in &mut self.listeners {
            listener.exit_design_file(&design_file);
        }
    }

    // Design Units
    fn enter_any_design_unit(&mut self, any_design_unit: &mut AnyDesignUnit) {
        for listener in &mut self.listeners {
            listener.enter_any_design_unit(any_design_unit);
        }
    }
    fn exit_any_design_unit(&mut self, any_design_unit: &AnyDesignUnit) {
        for listener in &mut self.listeners {
            listener.exit_any_design_unit(&any_design_unit);
        }
    }
    fn enter_primary_unit(&mut self, primary_unit: &mut PrimaryUnit){
        for listener in &mut self.listeners {   
            listener.enter_primary_unit(primary_unit);
        }
    }
    fn exit_primary_unit(&mut self, primary_unit: &PrimaryUnit){
        for listener in &mut self.listeners {   
            listener.exit_primary_unit(primary_unit);
        }
    }
    fn enter_entity_declaration(&mut self, entity_declaration: &mut DesignUnit<EntityDeclaration>){
        for listener in &mut self.listeners {   
            listener.enter_entity_declaration(entity_declaration);
        }
    }
    fn exit_entity_declaration(&mut self, entity_declaration: &DesignUnit<EntityDeclaration>){
        for listener in &mut self.listeners {   
            listener.exit_entity_declaration(entity_declaration);
        }
    }


    fn enter_secondary_unit(&mut self, secondary_unit: &mut SecondaryUnit){
        for listener in &mut self.listeners {   
            listener.enter_secondary_unit(secondary_unit);
        }
    }
    fn exit_secondary_unit(&mut self, secondary_unit: &SecondaryUnit){
        for listener in &mut self.listeners {   
            listener.exit_secondary_unit(secondary_unit);
        }
    }

}


trait Listen {
    // The listen method shall:
    // - Call the notifier.enter_<AST node> method upon entry
    // - Call the listen method of all children
    // - Call the notifier.exit_<AST node> method upon exit
    fn listen(&mut self, notifier : &mut Notifier);
}


impl Listen for DesignFile {
    fn listen(&mut self, notifier : &mut Notifier){
        notifier.enter_design_file(self);
        for design_unit in &mut self.design_units {
            design_unit.listen(notifier);
        }
        notifier.exit_design_file(&self);
    }
}

impl Listen for AnyDesignUnit {
    fn listen(&mut self, notifier : &mut Notifier){
        notifier.enter_any_design_unit(self);
        match self {
            AnyDesignUnit::Primary(primary) => primary.listen(notifier),
            AnyDesignUnit::Secondary(secondary) => secondary.listen(notifier),
        }
        notifier.exit_any_design_unit(&self);
    }
}

impl Listen for PrimaryUnit {
    fn listen(&mut self, notifier : &mut Notifier){
        notifier.enter_primary_unit(self);
        match self{
            PrimaryUnit::EntityDeclaration(entity) => entity.listen(notifier),
            PrimaryUnit::Configuration(_configuration) => println!("CONF"),
            PrimaryUnit::PackageDeclaration(_package_declaration) => println!("PCKDEC"),
            PrimaryUnit::PackageInstance(_package_instance) => println!("PCKINST"),
            PrimaryUnit::ContextDeclaration(_context_declaration) => println!("CONTDEC"),
        }
        notifier.exit_primary_unit(self);
    }
}

impl Listen for DesignUnit<EntityDeclaration> {
    fn listen(&mut self, notifier : &mut Notifier){
        notifier.enter_entity_declaration(self);
        notifier.exit_entity_declaration(self);
    }
}

impl Listen for SecondaryUnit {
    fn listen(&mut self, notifier : &mut Notifier){
        notifier.enter_secondary_unit(self);
        notifier.exit_secondary_unit(self);
    }
}


// =====================================================================
// TESTS
// ---------------------------------------------------------------------
#[cfg(test)]
mod tests {
    use super::*;
    use crate::design_unit::parse_design_file;
    use crate::test_util::{check_no_messages, Code};

    fn parse_str(code: &str) -> (Code, DesignFile, Vec<Message>) {
        let code = Code::new(code);
        let mut messages = vec![];
        let design_file = code.with_stream(|stream| parse_design_file(stream, &mut messages));
        (code, design_file, messages)
    }

    fn parse_ok(code: &str) -> (Code, DesignFile) {
        let (code, design_file, messages) = parse_str(code);
        check_no_messages(&messages);
        (code, design_file)
    }

    struct TestListenerResult {
        entered_design_files : u32,
        exited_design_files : u32,
    }

    struct TestListener {
        messages : Vec<Message>,
        result : TestListenerResult,
    }

    impl TestListener {
        fn result(&self) -> &TestListenerResult {
            &self.result
        }
    }

    #[allow(unused_variables)]
    impl Listener for TestListener {

        fn messages(&self) -> &Vec<Message> {
            &self.messages
        }

        // Design File
        fn enter_design_file(&mut self, design_file: &mut DesignFile) {
            println!("Enter DesignFile");
            self.result.entered_design_files += 1;
        }
        fn exit_design_file(&mut self, design_file: &DesignFile) {
            println!("Exit DesignFile");
            self.result.exited_design_files += 1;
        }

        // Design Units
        fn enter_any_design_unit(&mut self, any_design_unit: &mut AnyDesignUnit){
            println!("Enter AnyDesignUnit");
        }
        fn exit_any_design_unit(&mut self, any_design_unit: &AnyDesignUnit){
            println!("Exit AnyDesignUnit");
        }
        fn enter_primary_unit(&mut self, primary_unit: &mut PrimaryUnit){
            println!("Enter PrimaryUnit");
        }
        fn exit_primary_unit(&mut self, primary_unit: &PrimaryUnit){
            println!("Exit PrimaryUnit");
        }
        fn enter_entity_declaration(&mut self, entity_declaration: &mut DesignUnit<EntityDeclaration>){
            println!("Enter EntityDeclaration {}", entity_declaration.unit.ident.item.name());
        }
        fn exit_entity_declaration(&mut self, entity_declaration: &DesignUnit<EntityDeclaration>){
            println!("Exit EntityDeclaration {}", entity_declaration.unit.ident.item.name());
        }


        fn enter_secondary_unit(&mut self, secondary_unit: &mut SecondaryUnit){
            println!("Enter SecondaryUnit");
        }
        fn exit_secondary_unit(&mut self, secondary_unit: &SecondaryUnit){
            println!("Exit SecondaryUnit");
        }


    }

    #[test]
    fn listen_to_design_file() {
        let mut notifier = Notifier::new();
        let mut listener = TestListener{
            messages : Vec::new(),
            result : TestListenerResult {
                entered_design_files: 0,
                exited_design_files: 0,
            },
        };
        
        notifier.register(&mut listener);
        
        let (_code, mut design_file) = parse_ok(
            "
entity myent is
end entity;

entity myent2 is
end entity myent2;

architecture rtl of myent4 is
begin
end;

",
        );
        design_file.listen(&mut notifier);
        
        let result = listener.result();
        println!("Found {} design files", result.entered_design_files.to_string());
    }

}