// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use ast::{
    AssociationElement, AttributeName, Choice, Declaration, DiscreteRange, ElementAssociation,
    Expression, FunctionCall, Ident, InterfaceDeclaration, LabeledConcurrentStatement,
    LabeledSequentialStatement, Name, Range, SelectedName, Signature, SubprogramDeclaration,
    SubtypeIndication, UseClause, Waveform,
};
use concurrent_statement::parse_labeled_concurrent_statement;
use context::parse_use_clause;
use declarative_part::parse_declarative_part;
use expression::{parse_aggregate, parse_choices, parse_expression};
use interface_declaration::{parse_generic, parse_parameter, parse_port};
use latin_1::Latin1String;
use message::{Message, MessageHandler, ParseResult};
use names::{parse_association_list, parse_name, parse_selected_name};
use range::{parse_discrete_range, parse_range};
use sequential_statement::parse_sequential_statement;
use source::{Source, SrcPos, WithPos};
use std::fmt::Debug;
use std::sync::Arc;
use subprogram::{parse_signature, parse_subprogram_declaration_no_semi};
use subtype_indication::parse_subtype_indication;
use symbol_table::{Symbol, SymbolTable};
use tokenizer::Tokenizer;
use tokenstream::TokenStream;
use waveform::parse_waveform;

/// Utility to create expected values for parse results
pub struct TestUtil {
    source: Source,
    symtab: Arc<SymbolTable>,
}

impl TestUtil {
    /// Helper method to run lower level parsing function at specific substring
    pub fn parse<F, R>(&self, parse_fun: F, substr: &str, occurence: usize) -> R
    where
        F: FnOnce(&mut TokenStream) -> R,
    {
        let pos = self.substr_pos(substr, occurence);

        let latin1 = self.source.contents().unwrap();
        let latin1 = Latin1String::new(&latin1.bytes[..pos.start + pos.length]);
        let symtab = Arc::new(SymbolTable::new());
        let tokenizer = Tokenizer::new(symtab, self.source.clone(), Arc::new(latin1));

        let mut stream = TokenStream::new(tokenizer);
        forward(&mut stream, &pos);
        parse_fun(&mut stream)
    }

    /// Expect Ok() value
    pub fn parse_ok<F, R>(&self, parse_fun: F, substr: &str, occurence: usize) -> R
    where
        F: FnOnce(&mut TokenStream) -> ParseResult<R>,
    {
        self.parse(parse_fun, substr, occurence).unwrap()
    }

    /// Use first substring and expect Ok()
    pub fn parse_first_ok<F, R>(&self, parse_fun: F, substr: &str) -> R
    where
        F: FnOnce(&mut TokenStream) -> ParseResult<R>,
    {
        self.parse_ok(parse_fun, substr, 1)
    }
    /// Helper to create a identifier at first occurence of name
    pub fn ident(&self, name: &str) -> Ident {
        self.parse_first_ok(|stream: &mut TokenStream| stream.expect_ident(), name)
    }

    /// Helper to create a identifier at first occurence of name
    pub fn selected_name(&self, name: &str) -> SelectedName {
        self.parse_first_ok(parse_selected_name, name)
    }

    /// Helper method to create expression from first occurence of substr
    /// Can be used to test all but expression parsing
    pub fn expr(&self, substr: &str) -> WithPos<Expression> {
        self.parse_first_ok(parse_expression, substr)
    }

    pub fn waveform(&self, substr: &str) -> Waveform {
        self.parse_first_ok(parse_waveform, substr)
    }

    pub fn aggregate(&self, substr: &str) -> WithPos<Vec<ElementAssociation>> {
        self.parse_first_ok(|stream| parse_aggregate(stream), substr)
    }

    pub fn function_call(&self, substr: &str) -> FunctionCall {
        let name = self.name(substr);
        match name.item {
            Name::FunctionCall(call) => *call,
            _ => FunctionCall {
                name: name,
                parameters: vec![],
            },
        }
    }

    pub fn name(&self, substr: &str) -> WithPos<Name> {
        self.parse_first_ok(parse_name, substr)
    }

    pub fn association_list(&self, substr: &str) -> Vec<AssociationElement> {
        self.parse_first_ok(parse_association_list, substr)
    }

    pub fn attribute_name(&self, substr: &str) -> AttributeName {
        match self.parse_first_ok(parse_name, substr).item {
            Name::Attribute(attr) => *attr,
            name => panic!("Expected attribute got {:?}", name),
        }
    }

    pub fn choices(&self, substr: &str) -> Vec<Choice> {
        self.parse_first_ok(parse_choices, substr)
    }

    /// Helper method to create subtype indication from first occurence of substr
    /// Can be used to test all but subtype parsing
    pub fn subtype_indication(&self, substr: &str) -> SubtypeIndication {
        self.parse_first_ok(parse_subtype_indication, substr)
    }

    pub fn parameter(&self, substr: &str) -> InterfaceDeclaration {
        self.parse_first_ok(parse_parameter, substr)
    }

    pub fn generic(&self, substr: &str) -> InterfaceDeclaration {
        self.parse_first_ok(parse_generic, substr)
    }

    pub fn port(&self, substr: &str) -> InterfaceDeclaration {
        self.parse_first_ok(parse_port, substr)
    }

    pub fn subprogram_decl(&self, substr: &str) -> SubprogramDeclaration {
        let mut messages = Vec::new();
        let res = self.parse_first_ok(
            |stream| parse_subprogram_declaration_no_semi(stream, &mut messages),
            substr,
        );
        check_no_messages(&messages);
        res
    }

    pub fn declarative_part(&self, substr: &str) -> Vec<Declaration> {
        let mut messages = Vec::new();
        let res = self.parse_first_ok(
            |stream| parse_declarative_part(stream, &mut messages, false),
            substr,
        );
        check_no_messages(&messages);
        res
    }

    pub fn sequential_statement(&self, substr: &str) -> LabeledSequentialStatement {
        let mut messages = Vec::new();
        let res = self.parse_first_ok(
            |stream| parse_sequential_statement(stream, &mut messages),
            substr,
        );
        check_no_messages(&messages);
        res
    }

    pub fn concurrent_statement(&self, substr: &str) -> LabeledConcurrentStatement {
        let mut messages = Vec::new();
        let res = self.parse_first_ok(
            |stream| parse_labeled_concurrent_statement(stream, &mut messages),
            substr,
        );
        check_no_messages(&messages);
        res
    }

    pub fn use_clause(&self, substr: &str) -> UseClause {
        self.parse_first_ok(parse_use_clause, substr)
    }

    pub fn signature(&self, substr: &str) -> Signature {
        self.parse_first_ok(parse_signature, substr)
    }

    /// Helper method to create range constraint from first occurence of substr
    /// Can be used to test all but range constraint parsing
    pub fn range(&self, substr: &str) -> Range {
        self.parse_first_ok(parse_range, substr)
    }

    pub fn discrete_range(&self, substr: &str) -> DiscreteRange {
        self.parse_first_ok(parse_discrete_range, substr)
    }

    /// Helper method to create a source position from a substring
    pub fn substr_pos(self: &Self, substr: &str, occurence: usize) -> SrcPos {
        self.source.substr_pos(substr, occurence)
    }

    /// First occurence of substring position
    pub fn first_substr_pos(self: &Self, substr: &str) -> SrcPos {
        self.source.substr_pos(substr, 1)
    }

    /// Position covers entire contents
    pub fn entire_pos(self: &Self) -> SrcPos {
        self.source.entire_pos()
    }

    /// Return symbol from symbol table
    pub fn symbol(&self, name: &str) -> Symbol {
        self.symtab.lookup_utf8(name).unwrap()
    }
}

/// Fast forward tokenstream until position
fn forward(stream: &mut TokenStream, pos: &SrcPos) {
    loop {
        let token = stream.peek_expect().unwrap();
        if token.pos.start == pos.start {
            break;
        }
        stream.pop().unwrap();
    }
}

/// Helper method to parse using function
pub fn with_partial_stream<F, R>(parse_fun: F, code: &str) -> (TestUtil, R)
where
    F: FnOnce(&mut TokenStream) -> R,
{
    let source = Source::from_str(code).unwrap();
    let symtab = Arc::new(SymbolTable::new());
    let result = {
        let tokenizer = Tokenizer::new(symtab.clone(), source.clone(), source.contents().unwrap());
        let mut stream = TokenStream::new(tokenizer);
        parse_fun(&mut stream)
    };
    (TestUtil { source, symtab }, result)
}

/// Helper method to parse the stream and assert that the entire stream is parsed and that the result is ok
pub fn with_stream<F, R>(parse_fun: F, code: &str) -> (TestUtil, R)
where
    R: Debug,
    F: FnOnce(&mut TokenStream) -> ParseResult<R>,
{
    let parse_fun_eof = |stream: &mut TokenStream| {
        let result = parse_fun(stream);
        match result {
            Err(err) => {
                println!("{:#?}", err);
                println!("{}", err.pretty_string());
                panic!("Got Err()");
            }
            Ok(result) => {
                if let Some(token) = stream.peek().unwrap() {
                    println!("result = {:#?}", result);
                    panic!("Expected EOF got {:?}", token);
                }
                return result;
            }
        }
    };

    with_partial_stream(parse_fun_eof, code)
}

pub fn with_stream_messages<F, R>(parse_fun: F, code: &str) -> (TestUtil, R, Vec<Message>)
where
    R: Debug,
    F: FnOnce(&mut TokenStream, &mut MessageHandler) -> ParseResult<R>,
{
    let mut messages = Vec::new();
    let (util, result) = with_stream(
        |stream: &mut TokenStream| parse_fun(stream, &mut messages),
        code,
    );
    (util, result, messages)
}

pub fn with_stream_no_messages<F, R>(parse_fun: F, code: &str) -> (TestUtil, R)
where
    R: Debug,
    F: FnOnce(&mut TokenStream, &mut MessageHandler) -> ParseResult<R>,
{
    let (util, result, messages) = with_stream_messages(parse_fun, code);
    check_no_messages(&messages);
    (util, result)
}

/// Check that no errors where found
pub fn check_no_messages(messages: &Vec<Message>) {
    for err in messages.iter() {
        println!("{}", err.pretty_string());
    }
    if messages.len() > 0 {
        panic!("Found errors");
    }
}
