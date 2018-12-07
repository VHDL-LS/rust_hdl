// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use ast::*;
use concurrent_statement::parse_labeled_concurrent_statement;
use configuration::parse_configuration_declaration;
use context::{parse_context, DeclarationOrReference};
use context::{parse_library_clause, parse_use_clause};
use declarative_part::{parse_declarative_part_leave_end_token, parse_package_instantiation};
use design_unit::{
    parse_architecture_body, parse_design_file, parse_entity_declaration, parse_package_body,
    parse_package_declaration,
};
use expression::{parse_aggregate, parse_choices, parse_expression};
use interface_declaration::{parse_generic, parse_parameter, parse_port};
use latin_1::Latin1String;
use message::{Message, MessageHandler, ParseResult};
use names::{parse_association_list, parse_name, parse_selected_name};
use range::{parse_discrete_range, parse_range};
use sequential_statement::parse_sequential_statement;
use source::{Source, SrcPos, WithPos};
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::Debug;
use std::sync::Arc;
use subprogram::{parse_signature, parse_subprogram_declaration_no_semi};
use subtype_indication::parse_subtype_indication;
use symbol_table::{Symbol, SymbolTable};
use tokenizer::Tokenizer;
use tokenstream::TokenStream;
use waveform::parse_waveform;

pub struct CodeBuilder {
    pub symtab: Arc<SymbolTable>,
}

impl CodeBuilder {
    pub fn new() -> CodeBuilder {
        CodeBuilder {
            symtab: Arc::new(SymbolTable::new()),
        }
    }

    pub fn code_from_source(&self, source: Source) -> Code {
        let pos = source.entire_pos();
        let code = Code {
            source,
            symtab: self.symtab.clone(),
            pos,
        };

        // Ensure symbol table is populated
        code.with_stream(|stream| {
            while stream.pop()?.is_some() {}
            Ok(())
        });

        code
    }

    pub fn code(&self, code: &str) -> Code {
        self.code_from_source(Source::from_str(code))
    }

    pub fn symbol(&self, name: &str) -> Symbol {
        self.symtab.insert_utf8(name)
    }
}

#[derive(Clone)]
pub struct Code {
    source: Source,
    pub symtab: Arc<SymbolTable>,
    pos: SrcPos,
}

impl Code {
    pub fn new(code: &str) -> Code {
        CodeBuilder::new().code(code)
    }

    /// Create new Code from n:th occurence of substr
    pub fn s(&self, substr: &str, occurence: usize) -> Code {
        Code {
            source: self.source.clone(),
            symtab: self.symtab.clone(),
            pos: self.pos().substr_pos(&self.source, substr, occurence),
        }
    }

    /// Create new Code from first n:th occurence of substr
    pub fn s1(&self, substr: &str) -> Code {
        self.s(substr, 1)
    }

    /// Create new code between two substring matches
    pub fn between(&self, start: &str, end: &str) -> Code {
        let start = self.pos.substr_pos(&self.source, start, 1);
        let trailing = self.source.pos(
            start.start,
            self.pos.length - (start.start - self.pos.start),
        );
        let end = trailing.substr_pos(&self.source, end, 1);
        let length = (end.start + end.length) - start.start;

        Code {
            source: self.source.clone(),
            symtab: self.symtab.clone(),
            pos: self.source.pos(start.start, length),
        }
    }

    pub fn pos(self: &Self) -> SrcPos {
        self.pos.clone()
    }

    /// Helper method to run lower level parsing function at specific substring
    pub fn parse<F, R>(&self, parse_fun: F) -> R
    where
        F: FnOnce(&mut TokenStream) -> R,
    {
        let latin1 = self.source.contents().unwrap();
        let latin1 = Latin1String::new(&latin1.bytes[..self.pos.start + self.pos.length]);
        let tokenizer = Tokenizer::new(self.symtab.clone(), self.source.clone(), Arc::new(latin1));
        let mut stream = TokenStream::new(tokenizer);
        forward(&mut stream, &self.pos);
        parse_fun(&mut stream)
    }

    /// Expect Ok() value
    pub fn parse_ok<F, R>(&self, parse_fun: F) -> R
    where
        F: FnOnce(&mut TokenStream) -> ParseResult<R>,
    {
        match self.parse(parse_fun) {
            Ok(res) => res,
            Err(msg) => {
                panic!("{}", msg.show());
            }
        }
    }

    pub fn with_partial_stream<F, R>(&self, parse_fun: F) -> R
    where
        F: FnOnce(&mut TokenStream) -> R,
    {
        let tokenizer = Tokenizer::new(
            self.symtab.clone(),
            self.source.clone(),
            self.source.contents().unwrap(),
        );
        let mut stream = TokenStream::new(tokenizer);
        parse_fun(&mut stream)
    }

    pub fn with_stream<F, R>(&self, parse_fun: F) -> R
    where
        R: Debug,
        F: FnOnce(&mut TokenStream) -> ParseResult<R>,
    {
        let parse_fun_eof = |stream: &mut TokenStream| {
            let result = parse_fun(stream);
            match result {
                Err(err) => {
                    println!("{:#?}", err);
                    println!("{}", err.show());
                    panic!("Got Err()");
                }
                Ok(result) => {
                    if let Some(token) = stream.peek().unwrap() {
                        println!("result = {:#?}", result);
                        panic!("Expected EOF got {:?}", token);
                    }
                    result
                }
            }
        };

        self.with_partial_stream(parse_fun_eof)
    }

    pub fn with_stream_err<F, R>(&self, parse_fun: F) -> Message
    where
        R: Debug,
        F: FnOnce(&mut TokenStream) -> ParseResult<R>,
    {
        let parse_fun_eof = |stream: &mut TokenStream| {
            let result = parse_fun(stream);
            match result {
                Err(err) => {
                    if let Some(token) = stream.peek().unwrap() {
                        println!("err = {:#?}", err);
                        panic!("Expected EOF got {:?}", token);
                    }
                    err
                }
                Ok(result) => {
                    panic!("Expected error got {:?}", result);
                }
            }
        };

        self.with_partial_stream(parse_fun_eof)
    }

    pub fn with_partial_stream_messages<F, R>(&self, parse_fun: F) -> (R, Vec<Message>)
    where
        R: Debug,
        F: FnOnce(&mut TokenStream, &mut MessageHandler) -> R,
    {
        let mut messages = Vec::new();
        let result =
            self.with_partial_stream(|stream: &mut TokenStream| parse_fun(stream, &mut messages));
        (result, messages)
    }

    pub fn with_stream_messages<F, R>(&self, parse_fun: F) -> (R, Vec<Message>)
    where
        R: Debug,
        F: FnOnce(&mut TokenStream, &mut MessageHandler) -> ParseResult<R>,
    {
        let mut messages = Vec::new();
        let result = self.with_stream(|stream: &mut TokenStream| parse_fun(stream, &mut messages));
        (result, messages)
    }

    pub fn with_stream_no_messages<F, R>(&self, parse_fun: F) -> R
    where
        R: Debug,
        F: FnOnce(&mut TokenStream, &mut MessageHandler) -> ParseResult<R>,
    {
        let (result, messages) = self.with_stream_messages(parse_fun);
        check_no_messages(&messages);
        result
    }

    pub fn declarative_part(&self) -> Vec<Declaration> {
        let mut messages = Vec::new();
        let res =
            self.parse_ok(|stream| parse_declarative_part_leave_end_token(stream, &mut messages));
        check_no_messages(&messages);
        res
    }
    /// Helper to create a identifier at first occurence of name
    pub fn ident(&self) -> Ident {
        self.parse_ok(|stream: &mut TokenStream| stream.expect_ident())
    }

    /// Helper to create a identifier at first occurence of name
    pub fn operator_symbol(&self) -> WithPos<Latin1String> {
        self.parse_ok(|stream: &mut TokenStream| stream.expect()?.expect_string())
    }

    pub fn character(&self) -> WithPos<u8> {
        self.parse_ok(|stream: &mut TokenStream| stream.expect()?.expect_character())
    }

    /// Helper method to create expression from first occurence of substr
    /// Can be used to test all but expression parsing
    pub fn expr(&self) -> WithPos<Expression> {
        self.parse_ok(parse_expression)
    }

    pub fn name(&self) -> WithPos<Name> {
        self.parse_ok(parse_name)
    }

    pub fn selected_name(&self) -> SelectedName {
        self.parse_ok(parse_selected_name)
    }

    pub fn signature(&self) -> Signature {
        self.parse_ok(parse_signature)
    }

    /// Return symbol from symbol table
    pub fn symbol(&self, name: &str) -> Symbol {
        self.symtab.insert_utf8(name)
    }

    pub fn subtype_indication(&self) -> SubtypeIndication {
        self.parse_ok(parse_subtype_indication)
    }

    pub fn port(&self) -> InterfaceDeclaration {
        self.parse_ok(parse_port)
    }

    pub fn generic(&self) -> InterfaceDeclaration {
        self.parse_ok(parse_generic)
    }

    pub fn parameter(&self) -> InterfaceDeclaration {
        self.parse_ok(parse_parameter)
    }

    pub fn function_call(&self) -> FunctionCall {
        let name = self.name();
        match name.item {
            Name::FunctionCall(call) => *call,
            _ => FunctionCall {
                name,
                parameters: vec![],
            },
        }
    }

    pub fn parse_ok_no_messages<F, R>(&self, parse_fun: F) -> R
    where
        F: FnOnce(&mut TokenStream, &mut MessageHandler) -> ParseResult<R>,
    {
        let mut messages = Vec::new();
        let res = self.parse_ok(|stream| parse_fun(stream, &mut messages));
        check_no_messages(&messages);
        res
    }

    pub fn sequential_statement(&self) -> LabeledSequentialStatement {
        self.parse_ok_no_messages(parse_sequential_statement)
    }

    pub fn concurrent_statement(&self) -> LabeledConcurrentStatement {
        self.parse_ok_no_messages(parse_labeled_concurrent_statement)
    }

    pub fn association_list(&self) -> Vec<AssociationElement> {
        self.parse_ok(parse_association_list)
    }

    pub fn waveform(&self) -> Waveform {
        self.parse_ok(parse_waveform)
    }

    pub fn aggregate(&self) -> WithPos<Vec<ElementAssociation>> {
        self.parse_ok(|stream| parse_aggregate(stream))
    }

    pub fn range(&self) -> Range {
        self.parse_ok(parse_range).item
    }

    pub fn discrete_range(&self) -> DiscreteRange {
        self.parse_ok(parse_discrete_range)
    }

    pub fn choices(&self) -> Vec<Choice> {
        self.parse_ok(parse_choices)
    }

    pub fn use_clause(&self) -> WithPos<UseClause> {
        self.parse_ok(parse_use_clause)
    }

    pub fn library_clause(&self) -> WithPos<LibraryClause> {
        self.parse_ok(parse_library_clause)
    }

    pub fn entity(&self) -> EntityDeclaration {
        self.parse_ok_no_messages(parse_entity_declaration)
    }

    pub fn package(&self) -> PackageDeclaration {
        self.parse_ok_no_messages(parse_package_declaration)
    }

    pub fn package_body(&self) -> PackageBody {
        self.parse_ok_no_messages(parse_package_body)
    }

    pub fn design_file(&self) -> DesignFile {
        self.parse_ok_no_messages(parse_design_file)
    }

    pub fn package_instance(&self) -> PackageInstantiation {
        self.parse_ok(parse_package_instantiation)
    }

    pub fn architecture(&self) -> ArchitectureBody {
        self.parse_ok_no_messages(parse_architecture_body)
    }

    pub fn configuration(&self) -> ConfigurationDeclaration {
        self.parse_ok_no_messages(parse_configuration_declaration)
    }

    pub fn context(&self) -> ContextDeclaration {
        match self.parse_ok_no_messages(parse_context) {
            DeclarationOrReference::Declaration(context) => context,
            reference => {
                panic!("Expected context declaration, got {:?}", reference);
            }
        }
    }

    pub fn subprogram_decl(&self) -> SubprogramDeclaration {
        self.parse_ok_no_messages(parse_subprogram_declaration_no_semi)
    }

    pub fn attribute_name(&self) -> AttributeName {
        match self.parse_ok(parse_name).item {
            Name::Attribute(attr) => *attr,
            name => panic!("Expected attribute got {:?}", name),
        }
    }
}

/// Fast forward tokenstream until position
fn forward(stream: &mut TokenStream, pos: &SrcPos) {
    loop {
        let token = stream.peek_expect().unwrap();
        if token.pos.start >= pos.start {
            break;
        }
        stream.move_after(&token);
    }
}

/// Check that no errors where found
pub fn check_no_messages(messages: &Vec<Message>) {
    for err in messages.iter() {
        println!("{}", err.show());
    }
    if messages.len() > 0 {
        panic!("Found errors");
    }
}

/// Create map from message -> count
fn messages_to_map(messages: Vec<Message>) -> HashMap<Message, usize> {
    let mut map = HashMap::new();
    for msg in messages {
        match map.entry(msg) {
            Entry::Occupied(mut entry) => {
                let count = *entry.get() + 1;
                entry.insert(count);
            }
            Entry::Vacant(mut entry) => {
                entry.insert(1);
            }
        }
    }
    map
}

/// Check messages are equal without considering order
pub fn check_messages(got: Vec<Message>, expected: Vec<Message>) {
    let mut expected = messages_to_map(expected);
    let mut got = messages_to_map(got);

    let mut found_errors = false;

    for (msg, count) in expected.drain() {
        match got.remove(&msg) {
            Some(got_count) => {
                if count != got_count {
                    found_errors = true;
                    println!("-------------------------------------------------------");
                    println!(
                        "Got right message but wrong count {}, expected {}",
                        got_count, count
                    );
                    println!("-------------------------------------------------------");
                    println!("{}", msg.show());
                }
            }
            None => {
                found_errors = true;
                println!("-------------------------------------------------------");
                println!("Got no message, expected {}", count);
                println!("-------------------------------------------------------");
                println!("{}", msg.show());
            }
        }
    }

    for (msg, _) in got.drain() {
        found_errors = true;
        println!("-------------------------------------------------------");
        println!("Got unexpected message");
        println!("-------------------------------------------------------");
        println!("{}", msg.show());
    }

    if found_errors {
        panic!("Found message mismatch");
    }
}

impl AsRef<SrcPos> for Code {
    fn as_ref(&self) -> &SrcPos {
        &self.pos
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_messages_ok() {
        let code = Code::new("foo bar");
        check_messages(
            vec![Message::error(code.s1("foo"), "hello")],
            vec![Message::error(code.s1("foo"), "hello")],
        )
    }

    #[test]
    fn check_messages_ok_out_of_order() {
        let code = Code::new("foo bar");
        check_messages(
            vec![
                Message::error(code.s1("foo"), "hello"),
                Message::error(code.s1("bar"), "msg"),
            ],
            vec![
                Message::error(code.s1("bar"), "msg"),
                Message::error(code.s1("foo"), "hello"),
            ],
        )
    }

    #[test]
    #[should_panic]
    fn check_messages_not_ok_mismatch() {
        let code = Code::new("foo bar");
        check_messages(
            vec![Message::error(code.s1("bar"), "msg")],
            vec![Message::error(code.s1("foo"), "hello")],
        )
    }

    #[test]
    #[should_panic]
    fn check_messages_not_ok_count_mismatch() {
        let code = Code::new("foo bar");
        check_messages(
            vec![
                Message::error(code.s1("bar"), "msg"),
                Message::error(code.s1("bar"), "msg"),
            ],
            vec![Message::error(code.s1("bar"), "msg")],
        )
    }

    #[test]
    #[should_panic]
    fn check_messages_not_ok_missing() {
        let code = Code::new("foo bar");
        check_messages(
            vec![Message::error(code.s1("bar"), "msg")],
            vec![
                Message::error(code.s1("bar"), "msg"),
                Message::error(code.s1("bar"), "missing"),
            ],
        )
    }

    #[test]
    #[should_panic]
    fn check_messages_not_ok_unexpected() {
        let code = Code::new("foo bar");
        check_messages(
            vec![
                Message::error(code.s1("bar"), "msg"),
                Message::error(code.s1("bar"), "unexpected"),
            ],
            vec![Message::error(code.s1("bar"), "msg")],
        )
    }
}
