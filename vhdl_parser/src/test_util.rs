// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use crate::ast::*;
use crate::concurrent_statement::parse_labeled_concurrent_statement;
use crate::configuration::parse_configuration_declaration;
use crate::context::{parse_context, DeclarationOrReference};
use crate::context::{parse_library_clause, parse_use_clause};
use crate::declarative_part::{
    parse_declarative_part_leave_end_token, parse_package_instantiation,
};
use crate::design_unit::{
    parse_architecture_body, parse_design_file, parse_entity_declaration, parse_package_body,
    parse_package_declaration,
};
use crate::diagnostic::{Diagnostic, DiagnosticHandler, ParseResult};
use crate::expression::{parse_aggregate, parse_choices, parse_expression};
use crate::interface_declaration::{parse_generic, parse_parameter, parse_port};
use crate::latin_1::Latin1String;
use crate::names::{parse_association_list, parse_designator, parse_name, parse_selected_name};
use crate::range::{parse_discrete_range, parse_range};
use crate::sequential_statement::parse_sequential_statement;
use crate::source::{Source, SrcPos, WithPos};
use crate::subprogram::{parse_signature, parse_subprogram_declaration_no_semi};
use crate::subtype_indication::parse_subtype_indication;
use crate::symbol_table::{Symbol, SymbolTable};
use crate::tokenizer::Tokenizer;
use crate::tokenstream::TokenStream;
use crate::waveform::parse_waveform;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::Debug;
use std::sync::Arc;

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
            Err(diagnostic) => {
                panic!("{}", diagnostic.show());
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

    pub fn with_stream_err<F, R>(&self, parse_fun: F) -> Diagnostic
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

    pub fn with_partial_stream_diagnostics<F, R>(&self, parse_fun: F) -> (R, Vec<Diagnostic>)
    where
        R: Debug,
        F: FnOnce(&mut TokenStream, &mut dyn DiagnosticHandler) -> R,
    {
        let mut diagnostics = Vec::new();
        let result = self
            .with_partial_stream(|stream: &mut TokenStream| parse_fun(stream, &mut diagnostics));
        (result, diagnostics)
    }

    pub fn with_stream_diagnostics<F, R>(&self, parse_fun: F) -> (R, Vec<Diagnostic>)
    where
        R: Debug,
        F: FnOnce(&mut TokenStream, &mut dyn DiagnosticHandler) -> ParseResult<R>,
    {
        let mut diagnostics = Vec::new();
        let result =
            self.with_stream(|stream: &mut TokenStream| parse_fun(stream, &mut diagnostics));
        (result, diagnostics)
    }

    pub fn with_stream_no_diagnostics<F, R>(&self, parse_fun: F) -> R
    where
        R: Debug,
        F: FnOnce(&mut TokenStream, &mut dyn DiagnosticHandler) -> ParseResult<R>,
    {
        let (result, diagnostics) = self.with_stream_diagnostics(parse_fun);
        check_no_diagnostics(&diagnostics);
        result
    }

    pub fn declarative_part(&self) -> Vec<Declaration> {
        let mut diagnostics = Vec::new();
        let res = self
            .parse_ok(|stream| parse_declarative_part_leave_end_token(stream, &mut diagnostics));
        check_no_diagnostics(&diagnostics);
        res
    }
    /// Helper to create a identifier at first occurence of name
    pub fn ident(&self) -> Ident {
        self.parse_ok(|stream: &mut TokenStream| stream.expect_ident())
    }

    pub fn designator(&self) -> WithPos<Designator> {
        self.parse_ok(parse_designator)
    }

    pub fn designator_ref(&self) -> WithPos<WithRef<Designator>> {
        self.parse_ok(parse_designator).into_ref()
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

    pub fn selected_name(&self) -> WithPos<SelectedName> {
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

    pub fn parse_ok_no_diagnostics<F, R>(&self, parse_fun: F) -> R
    where
        F: FnOnce(&mut TokenStream, &mut dyn DiagnosticHandler) -> ParseResult<R>,
    {
        let mut diagnostics = Vec::new();
        let res = self.parse_ok(|stream| parse_fun(stream, &mut diagnostics));
        check_no_diagnostics(&diagnostics);
        res
    }

    pub fn sequential_statement(&self) -> LabeledSequentialStatement {
        self.parse_ok_no_diagnostics(parse_sequential_statement)
    }

    pub fn concurrent_statement(&self) -> LabeledConcurrentStatement {
        self.parse_ok_no_diagnostics(parse_labeled_concurrent_statement)
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
        self.parse_ok_no_diagnostics(parse_entity_declaration)
    }

    pub fn package(&self) -> PackageDeclaration {
        self.parse_ok_no_diagnostics(parse_package_declaration)
    }

    pub fn package_body(&self) -> PackageBody {
        self.parse_ok_no_diagnostics(parse_package_body)
    }

    pub fn design_file(&self) -> DesignFile {
        self.parse_ok_no_diagnostics(parse_design_file)
    }

    pub fn package_instance(&self) -> PackageInstantiation {
        self.parse_ok(parse_package_instantiation)
    }

    pub fn architecture(&self) -> ArchitectureBody {
        self.parse_ok_no_diagnostics(parse_architecture_body)
    }

    pub fn configuration(&self) -> ConfigurationDeclaration {
        self.parse_ok_no_diagnostics(parse_configuration_declaration)
    }

    pub fn context(&self) -> ContextDeclaration {
        match self.parse_ok_no_diagnostics(parse_context) {
            DeclarationOrReference::Declaration(context) => context,
            reference => {
                panic!("Expected context declaration, got {:?}", reference);
            }
        }
    }

    pub fn subprogram_decl(&self) -> SubprogramDeclaration {
        self.parse_ok_no_diagnostics(parse_subprogram_declaration_no_semi)
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
pub fn check_no_diagnostics(diagnostics: &Vec<Diagnostic>) {
    for err in diagnostics.iter() {
        println!("{}", err.show());
    }
    if diagnostics.len() > 0 {
        panic!("Found errors");
    }
}

/// Create map from diagnostic -> count
fn diagnostics_to_map(diagnostics: Vec<Diagnostic>) -> HashMap<Diagnostic, usize> {
    let mut map = HashMap::new();
    for diagnostic in diagnostics {
        match map.entry(diagnostic) {
            Entry::Occupied(mut entry) => {
                let count = *entry.get() + 1;
                entry.insert(count);
            }
            Entry::Vacant(entry) => {
                entry.insert(1);
            }
        }
    }
    map
}

/// Check diagnostics are equal without considering order
pub fn check_diagnostics(got: Vec<Diagnostic>, expected: Vec<Diagnostic>) {
    let mut expected = diagnostics_to_map(expected);
    let mut got = diagnostics_to_map(got);

    let mut found_errors = false;

    for (diagnostic, count) in expected.drain() {
        match got.remove(&diagnostic) {
            Some(got_count) => {
                if count != got_count {
                    found_errors = true;
                    println!("-------------------------------------------------------");
                    println!(
                        "Got right diagnostic but wrong count {}, expected {}",
                        got_count, count
                    );
                    println!("-------------------------------------------------------");
                    println!("{}", diagnostic.show());
                }
            }
            None => {
                found_errors = true;
                println!("-------------------------------------------------------");
                println!("Got no diagnostic, expected {}", count);
                println!("-------------------------------------------------------");
                println!("{}", diagnostic.show());
            }
        }
    }

    for (diagnostic, _) in got.drain() {
        found_errors = true;
        println!("-------------------------------------------------------");
        println!("Got unexpected diagnostic");
        println!("-------------------------------------------------------");
        println!("{}", diagnostic.show());
    }

    if found_errors {
        panic!("Found diagnostic mismatch");
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
    fn check_diagnostics_ok() {
        let code = Code::new("foo bar");
        check_diagnostics(
            vec![Diagnostic::error(code.s1("foo"), "hello")],
            vec![Diagnostic::error(code.s1("foo"), "hello")],
        )
    }

    #[test]
    fn check_diagnostics_ok_out_of_order() {
        let code = Code::new("foo bar");
        check_diagnostics(
            vec![
                Diagnostic::error(code.s1("foo"), "hello"),
                Diagnostic::error(code.s1("bar"), "msg"),
            ],
            vec![
                Diagnostic::error(code.s1("bar"), "msg"),
                Diagnostic::error(code.s1("foo"), "hello"),
            ],
        )
    }

    #[test]
    #[should_panic]
    fn check_diagnostics_not_ok_mismatch() {
        let code = Code::new("foo bar");
        check_diagnostics(
            vec![Diagnostic::error(code.s1("bar"), "msg")],
            vec![Diagnostic::error(code.s1("foo"), "hello")],
        )
    }

    #[test]
    #[should_panic]
    fn check_diagnostics_not_ok_count_mismatch() {
        let code = Code::new("foo bar");
        check_diagnostics(
            vec![
                Diagnostic::error(code.s1("bar"), "msg"),
                Diagnostic::error(code.s1("bar"), "msg"),
            ],
            vec![Diagnostic::error(code.s1("bar"), "msg")],
        )
    }

    #[test]
    #[should_panic]
    fn check_diagnostics_not_ok_missing() {
        let code = Code::new("foo bar");
        check_diagnostics(
            vec![Diagnostic::error(code.s1("bar"), "msg")],
            vec![
                Diagnostic::error(code.s1("bar"), "msg"),
                Diagnostic::error(code.s1("bar"), "missing"),
            ],
        )
    }

    #[test]
    #[should_panic]
    fn check_diagnostics_not_ok_unexpected() {
        let code = Code::new("foo bar");
        check_diagnostics(
            vec![
                Diagnostic::error(code.s1("bar"), "msg"),
                Diagnostic::error(code.s1("bar"), "unexpected"),
            ],
            vec![Diagnostic::error(code.s1("bar"), "msg")],
        )
    }
}
