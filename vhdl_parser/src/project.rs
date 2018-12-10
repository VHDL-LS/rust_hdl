// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use self::fnv::FnvHashMap;
use crate::analysis::{Analyzer, DesignRoot, Library};
use crate::ast::DesignFile;
use crate::config::Config;
use crate::latin_1::Latin1String;
use crate::message::Message;
use crate::parser::{FileToParse, ParserError, VHDLParser};
use crate::source::Source;
use crate::symbol_table::Symbol;
use fnv;
use std::collections::hash_map::Entry;
use std::io;

pub struct Project {
    parser: VHDLParser,
    files: FnvHashMap<String, SourceFile>,
}

pub struct FileError {
    file_name: String,
    error: io::Error,
}

impl std::fmt::Display for FileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Error in {} ({})", self.file_name, self.error)
    }
}

impl Project {
    pub fn new() -> Project {
        Project {
            parser: VHDLParser::new(),
            files: FnvHashMap::default(),
        }
    }

    pub fn from_config(
        config: &Config,
        num_threads: usize,
        errors: &mut Vec<FileError>,
    ) -> Project {
        let mut project = Project::new();
        let mut files_to_parse: FnvHashMap<&str, LibraryFileToParse> = FnvHashMap::default();

        for library in config.iter_libraries() {
            let library_name =
                Latin1String::from_utf8(library.name()).expect("Library name not latin-1 encoded");
            let library_name = project.parser.symbol(&library_name);

            for file_name in library.file_names() {
                match files_to_parse.entry(file_name) {
                    Entry::Occupied(mut entry) => {
                        entry.get_mut().library_names.push(library_name.clone());
                    }
                    Entry::Vacant(entry) => {
                        let file_to_parse = LibraryFileToParse {
                            library_names: vec![library_name.clone()],
                            file_name: file_name.clone(),
                        };

                        entry.insert(file_to_parse);
                    }
                }
            }
        }

        let files_to_parse = files_to_parse.drain().map(|(_, v)| v).collect();

        for (file_to_parse, mut parser_messages, design_file) in project
            .parser
            .parse_design_files(files_to_parse, num_threads)
        {
            let design_file = match design_file {
                Ok(design_file) => Some(design_file),
                Err(ParserError::Message(msg)) => {
                    parser_messages.push(msg);
                    None
                }
                Err(ParserError::IOError(err)) => {
                    errors.push(FileError {
                        file_name: file_to_parse.file_name,
                        error: err,
                    });
                    continue;
                }
            };

            project.files.insert(
                file_to_parse.file_name,
                SourceFile {
                    library_names: file_to_parse.library_names,
                    parser_messages,
                    design_file,
                },
            );
        }

        project
    }

    pub fn update_source(&mut self, file_name: &str, source: &Source) -> io::Result<()> {
        let mut source_file = {
            if let Some(source_file) = self.files.remove(file_name) {
                source_file
            } else {
                SourceFile {
                    library_names: vec![],
                    parser_messages: vec![],
                    design_file: None,
                }
            }
        };
        source_file.design_file = None;
        source_file.parser_messages.clear();

        let design_file = self
            .parser
            .parse_design_source(source, &mut source_file.parser_messages);

        let result = match design_file {
            Ok(design_file) => {
                source_file.design_file = Some(design_file);
                Ok(())
            }
            Err(ParserError::Message(msg)) => {
                source_file.parser_messages.push(msg);
                Ok(())
            }
            Err(ParserError::IOError(err)) => {
                // @TODO convert to soft error and push to messages
                Err(err)
            }
        };

        self.files.insert(file_name.to_owned(), source_file);

        result
    }

    pub fn analyse(&mut self) -> Vec<Message> {
        // @TODO clones all design unit and re-create all libraries
        // Investigate *correct* methonds to do this incrementally
        let mut library_to_design_file: FnvHashMap<Symbol, Vec<DesignFile>> = FnvHashMap::default();
        let mut messages = Vec::new();
        let mut root = DesignRoot::new();

        for source_file in self.files.values() {
            for library_name in &source_file.library_names {
                if let Some(ref design_file) = source_file.design_file {
                    match library_to_design_file.entry(library_name.clone()) {
                        Entry::Occupied(mut entry) => {
                            entry.get_mut().push(design_file.clone());
                        }
                        Entry::Vacant(entry) => {
                            entry.insert(vec![design_file.clone()]);
                        }
                    }
                }
            }

            for message in source_file.parser_messages.iter().cloned() {
                messages.push(message);
            }
        }

        let work_sym = self.parser.symbol(&Latin1String::new(b"work"));
        for (library_name, design_files) in library_to_design_file.drain() {
            root.add_library(Library::new(
                library_name,
                &work_sym,
                design_files,
                &mut messages,
            ));
        }

        Analyzer::new(&root, &self.parser.symtab.clone()).analyze(&mut messages);
        messages
    }
}

impl Default for Project {
    fn default() -> Self {
        Self::new()
    }
}

struct LibraryFileToParse {
    library_names: Vec<Symbol>,
    file_name: String,
}

impl FileToParse for LibraryFileToParse {
    fn file_name(&self) -> &str {
        &self.file_name
    }
}

struct SourceFile {
    library_names: Vec<Symbol>,
    design_file: Option<DesignFile>,
    parser_messages: Vec<Message>,
}
