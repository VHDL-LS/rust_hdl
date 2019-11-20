// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use self::fnv::FnvHashMap;
use crate::analysis::{Analyzer, DesignRoot, Library};
use crate::ast::DesignFile;
use crate::config::Config;
use crate::diagnostic::Diagnostic;
use crate::latin_1::Latin1String;
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

        for (file_to_parse, mut parser_diagnostics, design_file) in project
            .parser
            .parse_design_files(files_to_parse, num_threads)
        {
            let design_file = match design_file {
                Ok(design_file) => Some(design_file),
                Err(ParserError::Diagnostic(diagnostic)) => {
                    parser_diagnostics.push(diagnostic);
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
                    parser_diagnostics,
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
                    parser_diagnostics: vec![],
                    design_file: None,
                }
            }
        };
        source_file.design_file = None;
        source_file.parser_diagnostics.clear();

        let design_file = self
            .parser
            .parse_design_source(source, &mut source_file.parser_diagnostics);

        let result = match design_file {
            Ok(design_file) => {
                source_file.design_file = Some(design_file);
                Ok(())
            }
            Err(ParserError::Diagnostic(diagnostic)) => {
                source_file.parser_diagnostics.push(diagnostic);
                Ok(())
            }
            Err(ParserError::IOError(err)) => {
                // @TODO convert to soft error and push to diagnostics
                Err(err)
            }
        };

        self.files.insert(file_name.to_owned(), source_file);

        result
    }

    pub fn analyse(&mut self) -> Vec<Diagnostic> {
        // @TODO clones all design unit and re-create all libraries
        // Investigate *correct* methonds to do this incrementally
        let mut library_to_design_file: FnvHashMap<Symbol, Vec<DesignFile>> = FnvHashMap::default();
        let mut diagnostics = Vec::new();
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

            for diagnostic in source_file.parser_diagnostics.iter().cloned() {
                diagnostics.push(diagnostic);
            }
        }

        let work_sym = self.parser.symbol(&Latin1String::new(b"work"));
        for (library_name, design_files) in library_to_design_file.drain() {
            root.add_library(Library::new(
                library_name,
                &work_sym,
                design_files,
                &mut diagnostics,
            ));
        }

        Analyzer::new(&root, &self.parser.symtab.clone()).analyze(&mut diagnostics);
        diagnostics
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
    parser_diagnostics: Vec<Diagnostic>,
}
