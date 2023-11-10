// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use itertools::Itertools;

use super::alias_declaration::parse_alias_declaration;
use super::common::ParseResult;
use super::component_declaration::parse_component_declaration;
use super::concurrent_statement::parse_labeled_concurrent_statement;
use super::context::{parse_library_clause, parse_use_clause};
use super::declarative_part::parse_declarative_part;
use super::design_unit::{
    parse_architecture_body, parse_design_file, parse_entity_declaration, parse_package_declaration,
};
use super::expression::{parse_aggregate, parse_choices, parse_expression};
use super::interface_declaration::{parse_generic, parse_parameter, parse_port};
use super::names::{
    parse_association_list, parse_designator, parse_name, parse_selected_name, parse_type_mark,
};
use super::object_declaration::{parse_file_declaration, parse_object_declaration};
use super::range::{parse_discrete_range, parse_range};
use super::separated_list::{parse_ident_list, parse_name_list};
use super::sequential_statement::parse_sequential_statement;
use super::subprogram::{
    parse_signature, parse_subprogram_declaration, parse_subprogram_specification,
};
use super::subtype_indication::parse_subtype_indication;
use super::tokens::{Comment, Kind, Symbols, Token, TokenStream, Tokenizer, Value};
use super::type_declaration::parse_type_declaration;
use super::waveform::parse_waveform;
use crate::ast;
use crate::ast::*;
use crate::data::Range;
use crate::data::*;
use crate::syntax::concurrent_statement::parse_map_aspect;
use crate::syntax::context::{parse_context, DeclarationOrReference};
use crate::syntax::names::parse_association_element;
use crate::syntax::subprogram::{parse_optional_subprogram_header, parse_subprogram_instantiation};
use crate::syntax::{kind_str, TokenAccess, TokenId, TokenSpan};
use std::collections::hash_map::DefaultHasher;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hasher;
use std::sync::Arc;

pub struct CodeBuilder {
    pub symbols: Arc<Symbols>,
}

impl AnyDesignUnit {
    pub fn expect_entity(&self) -> &EntityDeclaration {
        match self {
            AnyDesignUnit::Primary(AnyPrimaryUnit::Entity(ent)) => ent,
            _ => panic!("Expected entity"),
        }
    }
}

impl ContextItem {
    pub fn expect_library_clause(&self) -> &LibraryClause {
        match self {
            ContextItem::Library(lib) => lib,
            _ => panic!("Expected library clause"),
        }
    }

    pub fn expect_context_reference(&self) -> &ContextReference {
        match self {
            ContextItem::Context(ctx) => ctx,
            _ => panic!("Expected context clause"),
        }
    }
}

impl CodeBuilder {
    pub fn new() -> CodeBuilder {
        CodeBuilder {
            symbols: Arc::new(Symbols::default()),
        }
    }

    pub fn code_from_source(&self, source: Source) -> Code {
        let contents = source.contents();

        let pos = SrcPos::new(source.clone(), contents.range());

        let code = Code {
            symbols: self.symbols.clone(),
            pos,
        };

        // Ensure symbol table is populated
        code.tokenize_result();
        code
    }

    pub fn code_with_file_name(&self, file_name: &Path, code: &str) -> Code {
        self.code_from_source(Source::inline(file_name, code))
    }

    pub fn code(&self, code: &str) -> Code {
        let mut hasher = DefaultHasher::new();
        hasher.write(code.as_bytes());
        let file_name: PathBuf = format!("<unknown file with hash {}>", hasher.finish()).into();
        self.code_with_file_name(&file_name, code)
    }

    pub fn symbol(&self, name: &str) -> Symbol {
        self.symbols.symtab().insert_utf8(name)
    }
}

impl<T> SeparatedList<T> {
    pub fn single(item: T) -> SeparatedList<T> {
        SeparatedList {
            items: vec![item],
            tokens: vec![],
        }
    }
}

#[derive(Clone)]
pub struct Code {
    pub symbols: Arc<Symbols>,
    pos: SrcPos,
}

impl Code {
    pub fn new(code: &str) -> Code {
        CodeBuilder::new().code(code)
    }

    pub fn new_with_file_name(file_name: &Path, code: &str) -> Code {
        CodeBuilder::new().code_with_file_name(file_name, code)
    }

    fn in_range(&self, range: Range) -> Code {
        Code {
            symbols: self.symbols.clone(),
            pos: SrcPos::new(self.pos.source.clone(), range),
        }
    }

    fn pos_to_end(&self, start: Position) -> Code {
        Code {
            symbols: self.symbols.clone(),
            pos: SrcPos::new(
                self.pos.source.clone(),
                Range {
                    start,
                    end: self.pos.end(),
                },
            ),
        }
    }

    fn start_to_pos(&self, end: Position) -> Code {
        Code {
            symbols: self.symbols.clone(),
            pos: SrcPos::new(
                self.pos.source.clone(),
                Range {
                    start: self.pos.start(),
                    end,
                },
            ),
        }
    }

    pub fn s_to_end(&self, substr: &str, occurence: usize) -> Code {
        let substr_match_range =
            substr_range(&self.pos.source, self.pos.range(), substr, occurence);
        self.pos_to_end(substr_match_range.start)
    }

    pub fn s_from_start(&self, substr: &str, occurence: usize) -> Code {
        let substr_match_range =
            substr_range(&self.pos.source, self.pos.range(), substr, occurence);
        self.start_to_pos(substr_match_range.end)
    }

    pub fn s1_to_end(&self, substr: &str) -> Code {
        self.s_to_end(substr, 1)
    }

    pub fn s1_from_start(&self, substr: &str) -> Code {
        self.s_from_start(substr, 1)
    }

    /// Create new Code from n:th occurence of substr
    pub fn s(&self, substr: &str, occurence: usize) -> Code {
        self.in_range(substr_range(
            &self.pos.source,
            self.pos.range(),
            substr,
            occurence,
        ))
    }

    /// Create new Code from first n:th occurence of substr
    pub fn s1(&self, substr: &str) -> Code {
        self.s(substr, 1)
    }

    // Creates a code from the first occurence for before + substr at the
    // position of substr
    pub fn sa(&self, before: &str, substr: &str) -> Code {
        let mut range = self.s1(&format!("{before}{substr}")).pos().range;
        for _ in 0..before.len() {
            range.start = range.start.next_char();
        }

        self.in_range(range)
    }

    // Creates a code from the first occurence for before + substr at the
    // position of substr
    pub fn sb(&self, substr: &str, after: &str) -> Code {
        self.s1(&format!("{substr}{after}")).s1(substr)
    }

    pub fn pos(&self) -> SrcPos {
        self.pos.clone()
    }

    pub fn pos_after(&self, prefix: &str) -> Code {
        let end = self.s1(prefix).pos().end();
        self.in_range(Range {
            start: end,
            end: self.eof_pos().end().prev_char(),
        })
    }

    // Position after code
    pub fn eof_pos(&self) -> SrcPos {
        SrcPos::new(
            self.source().clone(),
            Range::new(self.end(), self.end().next_char()),
        )
    }

    pub fn start(&self) -> Position {
        self.pos.start()
    }

    pub fn end(&self) -> Position {
        self.pos.end()
    }

    pub fn source(&self) -> &Source {
        &self.pos.source
    }

    fn tokenize_result_raw(&self) -> (Vec<Result<Token, Diagnostic>>, Vec<Comment>, usize) {
        let mut tokens = Vec::new();
        let final_comments: Vec<Comment>;
        let mut dropped_tokens: usize = 0;
        {
            let contents = self.pos.source.contents();
            let source = Source::from_contents(
                self.pos.file_name(),
                contents.crop(Range::new(Position::default(), self.pos.end())),
            );
            let contents = source.contents();
            let reader = ContentReader::new(&contents);
            let mut tokenizer = Tokenizer::new(&self.symbols, &source, reader);
            loop {
                let token = tokenizer.pop();

                match token {
                    Ok(None) => break,
                    Ok(Some(token)) => {
                        if token.pos.start() >= self.pos.start() {
                            tokens.push(Ok(token));
                        } else {
                            dropped_tokens += 1;
                        }
                    }
                    Err(err) => tokens.push(Err(err)),
                }
            }
            match tokenizer.get_final_comments() {
                Some(comments) => final_comments = comments,
                None => panic!("Tokenizer failed to check for final comments."),
            }
        }
        (tokens, final_comments, dropped_tokens)
    }

    /// Helper method to test tokenization functions
    pub fn tokenize_result(&self) -> (Vec<Result<Token, Diagnostic>>, Vec<Comment>) {
        let (tokens, final_comments, _) = self.tokenize_result_raw();
        (tokens, final_comments)
    }

    /// Tokenize and check that there are no errors, ignore final comments
    pub fn tokenize(&self) -> Vec<Token> {
        let tokens = self.tokenize_result().0;
        tokens.into_iter().map(|tok| tok.unwrap()).collect()
    }

    pub fn token_span(&self) -> TokenSpan {
        let (tokens, _, dropped_tokens) = self.tokenize_result_raw();
        let start_token = TokenId::new(dropped_tokens);
        let end_token = TokenId::new(dropped_tokens + tokens.len() - 1);
        TokenSpan::new(start_token, end_token)
    }

    pub fn token(&self) -> TokenId {
        let contents = self.pos.source.contents();
        let source = Source::from_contents(
            self.pos.file_name(),
            contents.crop(Range::new(Position::default(), self.pos.end())),
        );
        let contents = source.contents();
        let reader = ContentReader::new(&contents);
        let tokenizer = Tokenizer::new(&self.symbols, &source, reader);
        let stream = TokenStream::new(tokenizer, &mut NoDiagnostics);
        forward(&stream, self.pos.start());
        stream.peek().expect("No token found");
        stream.get_current_token_id()
    }

    /// Helper method to run lower level parsing function at specific substring
    pub fn parse<F, R>(&self, parse_fun: F) -> R
    where
        F: FnOnce(&TokenStream) -> R,
    {
        let contents = self.pos.source.contents();
        let source = Source::from_contents(
            self.pos.file_name(),
            contents.crop(Range::new(Position::default(), self.pos.end())),
        );
        let contents = source.contents();
        let reader = ContentReader::new(&contents);
        let tokenizer = Tokenizer::new(&self.symbols, &source, reader);
        let mut stream = TokenStream::new(tokenizer, &mut NoDiagnostics);
        forward(&stream, self.pos.start());
        parse_fun(&mut stream)
    }

    /// Expect Ok() value
    pub fn parse_ok<F, R>(&self, parse_fun: F) -> R
    where
        F: FnOnce(&TokenStream) -> ParseResult<R>,
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
        F: FnOnce(&TokenStream) -> R,
    {
        let contents = self.pos.source.contents();
        let reader = ContentReader::new(&contents);
        let tokenizer = Tokenizer::new(&self.symbols, &self.pos.source, reader);
        let mut stream = TokenStream::new(tokenizer, &mut NoDiagnostics);
        parse_fun(&mut stream)
    }

    pub fn with_stream<F, R>(&self, parse_fun: F) -> R
    where
        R: Debug,
        F: FnOnce(&TokenStream) -> ParseResult<R>,
    {
        let parse_fun_eof = |stream: &TokenStream| {
            let result = parse_fun(stream);
            match result {
                Err(err) => {
                    println!("{err:#?}");
                    println!("{}", err.show());
                    panic!("Got Err()");
                }
                Ok(result) => {
                    if let Some(token) = stream.peek() {
                        println!("result = {result:#?}");
                        panic!("Expected EOF got {token:?}");
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
        F: FnOnce(&TokenStream) -> ParseResult<R>,
    {
        let parse_fun_eof = |stream: &TokenStream| {
            let result = parse_fun(stream);
            match result {
                Err(err) => {
                    if let Some(token) = stream.peek() {
                        println!("err = {err:#?}");
                        panic!("Expected EOF got {token:?}");
                    }
                    err
                }
                Ok(result) => {
                    panic!("Expected error got {result:?}");
                }
            }
        };

        self.with_partial_stream(parse_fun_eof)
    }

    pub fn with_partial_stream_diagnostics<F, R>(&self, parse_fun: F) -> (R, Vec<Diagnostic>)
    where
        R: Debug,
        F: FnOnce(&TokenStream, &mut dyn DiagnosticHandler) -> R,
    {
        let mut diagnostics = Vec::new();
        let result =
            self.with_partial_stream(|stream: &TokenStream| parse_fun(stream, &mut diagnostics));
        (result, diagnostics)
    }

    pub fn with_stream_diagnostics<F, R>(&self, parse_fun: F) -> (R, Vec<Diagnostic>)
    where
        R: Debug,
        F: FnOnce(&TokenStream, &mut dyn DiagnosticHandler) -> ParseResult<R>,
    {
        let mut diagnostics = Vec::new();
        let result = self.with_stream(|stream: &TokenStream| parse_fun(stream, &mut diagnostics));
        (result, diagnostics)
    }

    pub fn with_stream_no_diagnostics<F, R>(&self, parse_fun: F) -> R
    where
        R: Debug,
        F: FnOnce(&TokenStream, &mut dyn DiagnosticHandler) -> ParseResult<R>,
    {
        let (result, diagnostics) = self.with_stream_diagnostics(parse_fun);
        check_no_diagnostics(&diagnostics);
        result
    }

    pub fn declarative_part(&self) -> Vec<Declaration> {
        let mut diagnostics = Vec::new();
        let res = self.parse_ok(|stream| parse_declarative_part(stream, &mut diagnostics));
        check_no_diagnostics(&diagnostics);
        res
    }
    /// Helper to create a identifier at first occurence of name
    pub fn ident(&self) -> Ident {
        self.parse_ok(|stream: &TokenStream| stream.expect_ident())
    }

    pub fn decl_ident(&self) -> WithDecl<Ident> {
        WithDecl::new(self.parse_ok(|stream: &TokenStream| stream.expect_ident()))
    }

    pub fn designator(&self) -> WithPos<Designator> {
        self.parse_ok(parse_designator)
    }

    pub fn decl_designator(&self) -> WithDecl<WithPos<Designator>> {
        WithDecl::new(self.parse_ok(parse_designator))
    }
    pub fn ref_designator(&self) -> WithPos<WithRef<Designator>> {
        self.parse_ok(parse_designator).map_into(WithRef::new)
    }

    pub fn character(&self) -> WithPos<u8> {
        self.parse_ok(|stream: &TokenStream| {
            let id = stream.expect_kind(Kind::Character)?;
            stream.get_token(id).to_character_value()
        })
    }

    /// Helper method to create expression from first occurence of substr
    /// Can be used to test all but expression parsing
    pub fn expr(&self) -> WithPos<Expression> {
        self.parse_ok(parse_expression)
    }

    pub fn name(&self) -> WithPos<Name> {
        self.parse_ok(parse_name)
    }

    pub fn name_list(&self) -> SeparatedList<WithPos<Name>> {
        self.parse_ok_no_diagnostics(parse_name_list)
    }

    pub fn ident_list(&self) -> SeparatedList<WithRef<Ident>> {
        self.parse_ok_no_diagnostics(parse_ident_list)
    }

    pub fn selected_name(&self) -> WithPos<SelectedName> {
        self.parse_ok(parse_selected_name)
    }

    pub fn type_mark(&self) -> WithPos<TypeMark> {
        self.parse_ok(parse_type_mark)
    }

    pub fn signature(&self) -> WithPos<Signature> {
        self.parse_ok(parse_signature)
    }

    /// Return symbol from symbol table
    pub fn symbol(&self, name: &str) -> Symbol {
        self.symbols.symtab().insert_utf8(name)
    }

    pub fn type_decl(&self) -> TypeDeclaration {
        self.with_stream_no_diagnostics(parse_type_declaration)
    }

    pub fn object_decl(&self) -> ObjectDeclaration {
        self.parse_ok(parse_object_declaration).remove(0)
    }

    pub fn file_decl(&self) -> FileDeclaration {
        self.parse_ok(parse_file_declaration).remove(0)
    }

    pub fn alias_decl(&self) -> AliasDeclaration {
        self.parse_ok(parse_alias_declaration)
    }

    pub fn component_decl(&self) -> ComponentDeclaration {
        self.with_stream_no_diagnostics(parse_component_declaration)
    }

    pub fn entity_decl(&self) -> EntityDeclaration {
        self.with_stream_no_diagnostics(parse_entity_declaration)
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

    pub fn function_call(&self) -> WithPos<CallOrIndexed> {
        let name = self.name();
        match name.item {
            Name::CallOrIndexed(call) => WithPos::new(*call, name.pos),
            _ => {
                let pos = name.pos.clone();
                WithPos::new(
                    CallOrIndexed {
                        name,
                        parameters: vec![],
                    },
                    pos,
                )
            }
        }
    }

    pub fn parse_ok_no_diagnostics<F, R>(&self, parse_fun: F) -> R
    where
        F: FnOnce(&TokenStream, &mut dyn DiagnosticHandler) -> ParseResult<R>,
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

    pub fn association_list(&self) -> SeparatedList<AssociationElement> {
        self.parse_ok_no_diagnostics(parse_association_list).0
    }

    pub fn port_map_aspect(&self) -> MapAspect {
        self.parse_ok_no_diagnostics(|stream, diagnsotics| {
            parse_map_aspect(stream, Kind::Port, diagnsotics)
        })
        .expect("Expecting port map aspect")
    }

    pub fn generic_map_aspect(&self) -> MapAspect {
        self.parse_ok_no_diagnostics(|stream, diagnostics| {
            parse_map_aspect(stream, Kind::Generic, diagnostics)
        })
        .expect("Expecting generic map aspect")
    }

    pub fn waveform(&self) -> Waveform {
        self.parse_ok(parse_waveform)
    }

    pub fn aggregate(&self) -> WithPos<Vec<ElementAssociation>> {
        self.parse_ok(parse_aggregate)
    }

    pub fn range(&self) -> ast::Range {
        self.parse_ok(parse_range).item
    }

    pub fn discrete_range(&self) -> DiscreteRange {
        self.parse_ok(parse_discrete_range)
    }

    pub fn choices(&self) -> Vec<WithPos<Choice>> {
        self.parse_ok(parse_choices)
    }

    pub fn use_clause(&self) -> UseClause {
        self.parse_ok_no_diagnostics(parse_use_clause)
    }

    pub fn library_clause(&self) -> LibraryClause {
        self.parse_ok_no_diagnostics(parse_library_clause)
    }

    pub fn context_declaration(&self) -> ContextDeclaration {
        match self.parse_ok_no_diagnostics(parse_context) {
            DeclarationOrReference::Declaration(decl) => decl,
            DeclarationOrReference::Reference(_) => panic!("Expecting Context Declaration"),
        }
    }

    pub fn package_declaration(&self) -> PackageDeclaration {
        self.parse_ok_no_diagnostics(parse_package_declaration)
    }

    pub fn design_file(&self) -> DesignFile {
        self.parse_ok_no_diagnostics(parse_design_file)
    }

    pub fn architecture_body(&self) -> ArchitectureBody {
        self.parse_ok_no_diagnostics(parse_architecture_body)
    }

    pub fn subprogram_specification(&self) -> SubprogramSpecification {
        self.parse_ok_no_diagnostics(parse_subprogram_specification)
    }

    pub fn subprogram_decl(&self) -> SubprogramDeclaration {
        self.parse_ok_no_diagnostics(parse_subprogram_declaration)
    }

    pub fn subprogram_instantiation(&self) -> SubprogramInstantiation {
        self.parse_ok_no_diagnostics(parse_subprogram_instantiation)
    }

    pub fn subprogram_header(&self) -> Option<SubprogramHeader> {
        self.parse_ok_no_diagnostics(parse_optional_subprogram_header)
    }

    pub fn attribute_name(&self) -> AttributeName {
        match self.parse_ok(parse_name).item {
            Name::Attribute(attr) => *attr,
            name => panic!("Expected attribute got {name:?}"),
        }
    }

    pub fn association_element(&self) -> AssociationElement {
        self.parse_ok(parse_association_element)
    }
}

fn substr_range(source: &Source, range: Range, substr: &str, occurence: usize) -> Range {
    let contents = source.contents();
    let mut reader = ContentReader::new(&contents);
    let mut count = occurence;

    reader.seek_pos(range.start);

    while reader.pos() < range.end {
        if reader.matches(substr) {
            count -= 1;
            if count == 0 {
                let start = reader.pos();
                for _ in substr.chars() {
                    reader.skip();
                }
                if reader.pos() <= range.end {
                    return Range::new(start, reader.pos());
                }
            }
        }

        reader.skip();
    }

    panic!("Could not find occurence {occurence} of substring {substr:?}");
}

/// Fast forward tokenstream until position
fn forward(stream: &TokenStream, start: Position) {
    // short-circuit when start is zero.
    // Also prevents the case where the token stream is empty
    if start.line == 0 && start.character == 0 {
        return;
    }
    loop {
        let token = stream.peek_expect().unwrap();
        if token.pos.start() >= start {
            break;
        }
        stream.skip();
    }
}

/// Check that no errors where found
pub fn check_no_diagnostics(diagnostics: &[Diagnostic]) {
    for err in diagnostics.iter() {
        println!("{}", err.show());
    }
    if !diagnostics.is_empty() {
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

// Drop releated info when we do not want to test for it
pub fn without_releated(diagnostics: &[Diagnostic]) -> Vec<Diagnostic> {
    let mut diagnostics = diagnostics.to_vec();
    for diagnostic in diagnostics.iter_mut() {
        diagnostic.related.clear();
    }
    diagnostics
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
                    println!("Got right diagnostic but wrong count {got_count}, expected {count}");
                    println!("-------------------------------------------------------");
                    println!("{}", diagnostic.show());
                }
            }
            None => {
                found_errors = true;
                println!("-------------------------------------------------------");
                println!("Got no diagnostic, expected {count}");
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

fn compare_unordered<T: PartialEq + Debug>(got: &[T], expected: &[T]) -> bool {
    if got.len() != expected.len() {
        return false;
    }
    for exp in expected.iter() {
        if !got.contains(exp) {
            return false;
        }
    }
    true
}

pub fn assert_eq_unordered<T: PartialEq + Debug>(got: &[T], expected: &[T]) {
    if !compare_unordered(got, expected) {
        panic!(
            "\ngot(len={}): {:?}\nexp(len={}): {:?}",
            got.len(),
            got,
            expected.len(),
            expected
        );
    }
}

impl AsRef<SrcPos> for Code {
    fn as_ref(&self) -> &SrcPos {
        &self.pos
    }
}

fn value_to_string(value: &Value) -> String {
    match value {
        Value::Identifier(ident) => ident.name_utf8(),
        Value::String(s) => String::from_utf8(s.chars().copied().collect_vec()).unwrap(),
        Value::BitString(_) => {
            panic!("value_to_string is currently not supported for BitString literals!")
        }
        Value::AbstractLiteral(lit) => match lit {
            AbstractLiteral::Integer(i) => i.to_string(),
            AbstractLiteral::Real(f) => f.to_string(),
        },
        Value::Character(char) => format!("'{}'", String::from_utf8(vec![*char]).unwrap()),
        Value::Text(text) => String::from_utf8(text.chars().copied().collect_vec()).unwrap(),
        Value::NoValue => "".into(),
    }
}

pub fn token_to_string(token: &Token) -> String {
    match token.kind {
        Kind::Identifier
        | Kind::AbstractLiteral
        | Kind::StringLiteral
        | Kind::BitString
        | Kind::Character
        | Kind::Text => value_to_string(&token.value),
        _ => kind_str(token.kind).into(),
    }
}

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
