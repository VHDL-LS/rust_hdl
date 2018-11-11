// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

pub mod ast;
#[macro_use]
mod tokenizer;
mod alias_declaration;
mod attributes;
mod common;
mod component_declaration;
mod concurrent_statement;
mod configuration;
mod context;
mod declarative_part;
mod design_unit;
mod expression;
mod interface_declaration;
mod latin_1;
pub mod message;
mod names;
mod object_declaration;
mod range;
pub mod semantic;
mod sequential_statement;
pub mod source;
mod subprogram;
mod subtype_indication;
mod symbol_table;
mod tokenstream;
mod type_declaration;
mod waveform;

#[cfg(test)]
mod test_util;

use ast::DesignFile;
use design_unit::parse_design_file;
use message::{Message, MessageHandler};
use source::Source;
use std::convert::From;
use std::io;
use std::sync::Arc;
use symbol_table::SymbolTable;
use tokenizer::Tokenizer;
use tokenstream::TokenStream;

pub enum ParserError {
    Message(Message),
    IOError(io::Error),
}
pub struct VHDLParser {
    symtab: Arc<SymbolTable>,
}

pub type ParserResult = Result<DesignFile, ParserError>;

impl From<io::Error> for ParserError {
    fn from(err: io::Error) -> ParserError {
        ParserError::IOError(err)
    }
}

impl From<Message> for ParserError {
    fn from(msg: Message) -> ParserError {
        ParserError::Message(msg)
    }
}

impl VHDLParser {
    pub fn new() -> VHDLParser {
        VHDLParser {
            symtab: Arc::new(SymbolTable::new()),
        }
    }

    pub fn parse_design_source(
        &self,
        source: &Source,
        messages: &mut MessageHandler,
    ) -> ParserResult {
        let code = source.contents()?;
        let tokenizer = Tokenizer::new(self.symtab.clone(), source.clone(), code);
        let mut stream = TokenStream::new(tokenizer);
        Ok(parse_design_file(&mut stream, messages)?)
    }

    pub fn parse_design_file(
        &self,
        file_name: &str,
        messages: &mut MessageHandler,
    ) -> ParserResult {
        let source = Source::from_file(&file_name);
        Ok(self.parse_design_source(&source, messages)?)
    }
}
