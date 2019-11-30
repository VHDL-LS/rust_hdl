// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use crate::ast::DesignFile;
use crate::design_unit::parse_design_file;
use crate::diagnostic::{Diagnostic, DiagnosticHandler};
use crate::latin_1::Latin1String;
use crate::source::Source;
use crate::symbol_table::Symbol;
use crate::symbol_table::SymbolTable;
use crate::tokenizer::Tokenizer;
use crate::tokenstream::TokenStream;
use std::convert::From;
use std::io;
use std::sync::mpsc::{sync_channel, Receiver, SyncSender};
use std::sync::{Arc, Mutex};
use std::thread::spawn;

use self::fnv::FnvHashMap;
use fnv;

#[derive(Debug)]
pub enum ParserError {
    Diagnostic(Diagnostic),
    IOError(io::Error),
}

#[derive(Clone)]
pub struct VHDLParser {
    pub symtab: Arc<SymbolTable>,
}

pub type ParserResult = Result<DesignFile, ParserError>;

impl From<io::Error> for ParserError {
    fn from(err: io::Error) -> ParserError {
        ParserError::IOError(err)
    }
}

impl From<Diagnostic> for ParserError {
    fn from(diagnostic: Diagnostic) -> ParserError {
        ParserError::Diagnostic(diagnostic)
    }
}

impl VHDLParser {
    pub fn new() -> VHDLParser {
        VHDLParser {
            symtab: Arc::new(SymbolTable::new()),
        }
    }

    pub fn symbol(&self, name: &Latin1String) -> Symbol {
        self.symtab.insert(name)
    }

    pub fn parse_design_source(
        &self,
        source: &Source,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> ParserResult {
        let code = source.contents()?;
        let tokenizer = Tokenizer::new(self.symtab.clone(), source.clone(), code);
        let mut stream = TokenStream::new(tokenizer);
        Ok(parse_design_file(&mut stream, diagnostics)?)
    }

    pub fn parse_design_file(
        &self,
        file_name: &str,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> ParserResult {
        let source = Source::from_file(file_name);
        Ok(self.parse_design_source(&source, diagnostics)?)
    }

    pub fn parse_design_files<T>(
        &self,
        files_to_parse: Vec<T>,
        num_threads: usize,
    ) -> impl Iterator<Item = (T, Vec<Diagnostic>, ParserResult)>
    where
        T: FileToParse + Send + 'static,
    {
        ParallelParser::new(self, files_to_parse, num_threads)
    }
}

impl Default for VHDLParser {
    fn default() -> Self {
        Self::new()
    }
}

pub trait FileToParse {
    fn file_name(&self) -> &str;
}

impl FileToParse for String {
    fn file_name(&self) -> &str {
        self.as_ref()
    }
}

type ParallelResult<T> = (T, Vec<Diagnostic>, ParserResult);

struct ParallelParser<T> {
    result_receiver: Receiver<(usize, ParallelResult<Box<T>>)>,
    idx: usize,
    num_files: usize,
    result_cache: FnvHashMap<usize, ParallelResult<Box<T>>>,
}

type WorkerInput<T> = Receiver<Option<(usize, Box<T>)>>;

impl<T: Send + FileToParse + 'static> ParallelParser<T> {
    fn worker(
        parser: &VHDLParser,
        input: &Arc<Mutex<WorkerInput<T>>>,
        output: &SyncSender<(usize, ParallelResult<Box<T>>)>,
    ) {
        loop {
            let item = input.lock().unwrap().recv().unwrap();
            match item {
                Some((idx, file_to_parse)) => {
                    let mut diagnostics = Vec::new();
                    let result =
                        parser.parse_design_file(file_to_parse.file_name(), &mut diagnostics);
                    output
                        .send((idx, (file_to_parse, diagnostics, result)))
                        .unwrap();
                }
                None => {
                    break;
                }
            }
        }
    }

    fn new(parser: &VHDLParser, files_to_parse: Vec<T>, num_threads: usize) -> ParallelParser<T> {
        let (work_sender, work_receiver) = sync_channel(2 * num_threads);
        let work_receiver = Arc::new(Mutex::new(work_receiver));
        let (result_sender, result_receiver) = sync_channel(2 * num_threads);

        for _ in 0..num_threads {
            let parser = parser.clone();
            let result_sender = result_sender.clone();
            let work_receiver = work_receiver.clone();
            spawn(move || Self::worker(&parser, &work_receiver, &result_sender));
        }
        let num_files = files_to_parse.len();
        spawn(move || {
            for (idx, file_to_parse) in files_to_parse.into_iter().enumerate() {
                work_sender
                    .send(Some((idx, Box::new(file_to_parse))))
                    .unwrap();
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

impl<T> Iterator for ParallelParser<T> {
    type Item = ParallelResult<T>;

    fn next(&mut self) -> Option<ParallelResult<T>> {
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
        let (file_to_parse, diagnostics, result) = value;
        Some((*file_to_parse, diagnostics, result))
    }
}
