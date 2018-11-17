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
mod library;
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
use std::sync::mpsc::{sync_channel, Receiver, SyncSender};
use std::sync::{Arc, Mutex};
use std::thread::spawn;
use symbol_table::SymbolTable;
use tokenizer::Tokenizer;
use tokenstream::TokenStream;

extern crate fnv;
use self::fnv::FnvHashMap;

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

fn worker(
    parser: Arc<VHDLParser>,
    input: Arc<Mutex<Receiver<Option<(usize, String)>>>>,
    output: SyncSender<(usize, ParallelResult)>,
) {
    loop {
        let item = input.lock().unwrap().recv().unwrap();
        match item {
            Some((idx, file_name)) => {
                let mut messages = Vec::new();
                let result = parser.parse_design_file(&file_name, &mut messages);
                output.send((idx, (file_name, messages, result))).unwrap();
            }
            None => {
                break;
            }
        }
    }
}

type ParallelResult = (String, Vec<Message>, ParserResult);
pub struct ParallelParser {
    result_receiver: Receiver<(usize, ParallelResult)>,
    idx: usize,
    num_files: usize,
    result_cache: FnvHashMap<usize, ParallelResult>,
}

impl ParallelParser {
    pub fn new(file_names: Vec<String>, num_threads: usize) -> ParallelParser {
        let parser = Arc::new(VHDLParser::new());

        let (work_sender, work_receiver) = sync_channel(2 * num_threads);
        let work_receiver = Arc::new(Mutex::new(work_receiver));
        let (result_sender, result_receiver) = sync_channel(2 * num_threads);

        for _ in 0..num_threads {
            let parser = parser.clone();
            let result_sender = result_sender.clone();
            let work_receiver = work_receiver.clone();
            spawn(move || worker(parser, work_receiver, result_sender));
        }
        let num_files = file_names.len();
        spawn(move || {
            for (idx, file_name) in file_names.iter().enumerate() {
                work_sender.send(Some((idx, file_name.clone()))).unwrap();
            }
            for _ in 0..num_threads {
                work_sender.send(None).unwrap();
            }
        });

        let result_cache = FnvHashMap::default();

        ParallelParser {
            result_receiver,
            idx: 0,
            num_files,
            result_cache,
        }
    }
}

impl Iterator for ParallelParser {
    type Item = ParallelResult;

    fn next(&mut self) -> Option<ParallelResult> {
        if self.idx >= self.num_files {
            return None;
        }

        let value = match self.result_cache.remove(&self.idx) {
            Some(value) => value,
            None => loop {
                let (idx, value) = self.result_receiver.recv().unwrap();
                if idx == self.idx {
                    break value;
                } else {
                    self.result_cache.insert(idx, value);
                }
            },
        };

        self.idx += 1;
        Some(value)
    }
}
