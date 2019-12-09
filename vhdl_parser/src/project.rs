// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use self::fnv::FnvHashMap;
use crate::analysis::{Analyzer, DesignRoot};
use crate::ast::DesignFile;
use crate::config::Config;
use crate::diagnostic::Diagnostic;
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
    root: DesignRoot,
    files: FnvHashMap<String, SourceFile>,
    empty_libraries: Vec<Symbol>,
}

impl Project {
    pub fn new() -> Project {
        Project {
            parser: VHDLParser::new(),
            root: DesignRoot::new(),
            files: FnvHashMap::default(),
            empty_libraries: Vec::new(),
        }
    }

    pub fn from_config(
        config: &Config,
        num_threads: usize,
        messages: &mut Vec<Message>,
    ) -> Project {
        let mut project = Project::new();
        let mut files_to_parse: FnvHashMap<String, LibraryFileToParse> = FnvHashMap::default();

        for library in config.iter_libraries() {
            let library_name =
                Latin1String::from_utf8(library.name()).expect("Library name not latin-1 encoded");
            let library_name = project.parser.symbol(&library_name);

            let mut empty_library = true;
            for file_name in library.file_names(messages) {
                empty_library = false;

                match files_to_parse.entry(file_name.clone()) {
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

            if empty_library {
                project.empty_libraries.push(library_name)
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
                    messages.push(Message::file_error(
                        err.to_string(),
                        file_to_parse.file_name,
                    ));
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

    pub fn update_source(&mut self, source: &Source) -> io::Result<()> {
        let mut source_file = {
            if let Some(source_file) = self.files.remove(source.file_name()) {
                for library_name in source_file.library_names.iter() {
                    self.root
                        .ensure_library(library_name.clone())
                        .remove_source(source);
                }
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

        self.files
            .insert(source.file_name().to_owned(), source_file);

        result
    }

    pub fn analyse(&mut self) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();

        for source_file in self.files.values_mut() {
            let design_file = source_file.take_design_file();
            // Avoid cloning design files for single library
            let mut design_files = multiply(design_file, source_file.library_names.len());

            for library_name in source_file.library_names.iter() {
                let library = self.root.ensure_library(library_name.clone());
                if let Some(design_file) = design_files.pop().unwrap() {
                    library.add_design_file(design_file);
                }
            }

            for diagnostic in source_file.parser_diagnostics.iter().cloned() {
                diagnostics.push(diagnostic);
            }
        }

        for library_name in self.empty_libraries.iter() {
            self.root.ensure_library(library_name.clone());
        }

        for library in self.root.iter_libraries_mut() {
            library.refresh(&mut diagnostics);
        }

        Analyzer::new(&mut self.root, &self.parser.symtab.clone()).analyze(&mut diagnostics);
        diagnostics
    }
}

/// Multiply clonable value by cloning
/// Avoid clone for n=1
fn multiply<T: Clone>(value: T, n: usize) -> Vec<T> {
    if n == 0 {
        vec![]
    } else if n == 1 {
        vec![value]
    } else {
        let mut res = Vec::with_capacity(n);
        for _ in 0..n - 1 {
            res.push(value.clone());
        }
        res.push(value);
        res
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

impl SourceFile {
    fn take_design_file(&mut self) -> Option<DesignFile> {
        std::mem::replace(&mut self.design_file, None)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::check_no_diagnostics;

    /// Test that an empty library is created
    /// Thus test case was added when fixing a bug
    /// Where a library with no files was never added
    #[test]
    fn test_empty_library_is_defined() {
        let root = tempfile::tempdir().unwrap();
        let vhdl_file_path = root.path().join("file.vhd");
        std::fs::write(
            &vhdl_file_path,
            "
library missing;

entity ent is
end entity;
        ",
        )
        .unwrap();

        let config_str = "
[libraries]
missing.files = []
lib.files = ['file.vhd']
        ";

        let config = Config::from_str(config_str, root.path()).unwrap();
        let mut messages = Vec::new();
        let mut project = Project::from_config(&config, 1, &mut messages);
        assert_eq!(messages, vec![]);
        check_no_diagnostics(&project.analyse());
    }

    /// Test that the same file can be added to several libraries
    #[test]
    fn test_same_file_in_multiple_libraries() {
        let root = tempfile::tempdir().unwrap();
        let vhdl_file_path1 = root.path().join("file.vhd");
        std::fs::write(
            &vhdl_file_path1,
            "
package pkg is
end package;
        ",
        )
        .unwrap();

        let vhdl_file_path2 = root.path().join("use_file.vhd");
        std::fs::write(
            &vhdl_file_path2,
            "
library lib1;
use lib1.pkg.all;

package use_pkg1 is
end package;

library lib2;
use lib2.pkg.all;

package use_pkg2 is
end package;
        ",
        )
        .unwrap();

        let config_str = "
[libraries]
lib1.files = ['file.vhd']
lib2.files = ['file.vhd']
use_lib.files = ['use_file.vhd']
        ";

        let config = Config::from_str(config_str, root.path()).unwrap();
        let mut messages = Vec::new();
        let mut project = Project::from_config(&config, 1, &mut messages);
        assert_eq!(messages, vec![]);
        check_no_diagnostics(&project.analyse());
    }

    fn update(project: &mut Project, source: &Source, contents: &str) {
        std::fs::write(&std::path::Path::new(source.file_name()), contents).unwrap();
        project.update_source(source).unwrap();
    }

    /// Test that the same file can be added to several libraries
    #[test]
    fn test_re_analyze_after_update() {
        let root = tempfile::tempdir().unwrap();
        let path1 = root.path().join("file1.vhd");
        let source1 = Source::from_file(path1.to_str().unwrap());

        let path2 = root.path().join("file2.vhd");
        let source2 = Source::from_file(path2.to_str().unwrap());

        std::fs::write(
            &path1,
            "
package pkg is
end package;
        ",
        )
        .unwrap();

        std::fs::write(
            &path2,
            "
library lib1;
use lib1.pkg.all;

package pkg is
end package;
        ",
        )
        .unwrap();

        let config_str = "
[libraries]
lib1.files = ['file1.vhd']
lib2.files = ['file2.vhd']
        ";

        let config = Config::from_str(config_str, root.path()).unwrap();
        let mut messages = Vec::new();
        let mut project = Project::from_config(&config, 1, &mut messages);
        assert_eq!(messages, vec![]);
        check_no_diagnostics(&project.analyse());

        // Add syntax error
        update(
            &mut project,
            &source1,
            "
package is
        ",
        );
        let diagnostics = project.analyse();
        assert_eq!(diagnostics.len(), 2);
        // Syntax error comes first
        assert_eq!(diagnostics[0].pos.source, source1);
        assert_eq!(diagnostics[1].pos.source, source2);

        // Make it good again
        update(
            &mut project,
            &source1,
            "
package pkg is
end package;
        ",
        );
        check_no_diagnostics(&project.analyse());

        // Add analysis error
        update(
            &mut project,
            &source2,
            "
package pkg is
end package;

package pkg is
end package;
        ",
        );
        let diagnostics = project.analyse();
        assert_eq!(diagnostics.len(), 1);
        assert_eq!(diagnostics[0].pos.source, source2);

        // Make it good again
        update(
            &mut project,
            &source2,
            "
package pkg is
end package;
        ",
        );
        check_no_diagnostics(&project.analyse());
    }
}
