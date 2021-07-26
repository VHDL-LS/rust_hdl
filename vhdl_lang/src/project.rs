// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use crate::analysis::DesignRoot;
use crate::ast::DesignFile;
use crate::config::Config;
use crate::data::*;
use crate::syntax::VHDLParser;
use fnv::{FnvHashMap, FnvHashSet};
use std::collections::hash_map::Entry;
use std::path::{Path, PathBuf};

pub struct Project {
    parser: VHDLParser,
    root: DesignRoot,
    files: FnvHashMap<PathBuf, SourceFile>,
    empty_libraries: FnvHashSet<Symbol>,
}

impl Project {
    pub fn new() -> Project {
        let parser = VHDLParser::default();
        Project {
            root: DesignRoot::new(parser.symbols.clone()),
            files: FnvHashMap::default(),
            empty_libraries: FnvHashSet::default(),
            parser,
        }
    }

    /// Create instance from given configuration.
    /// Files referred by configuration are parsed into corresponding libraries.
    pub fn from_config(config: &Config, messages: &mut dyn MessageHandler) -> Project {
        let mut project = Project::new();

        let files = project.load_files_from_config(config, messages);
        project.parse_and_add_files(files, messages);

        project
    }

    /// Replace active project configuration.
    /// The design state is reset, new files are added and parsed. Existing source files will be
    /// kept and parsed from in-memory source (required for incremental document updates).
    pub fn update_config(&mut self, config: &Config, messages: &mut dyn MessageHandler) {
        self.parser = VHDLParser::default();
        self.root = DesignRoot::new(self.parser.symbols.clone());

        // Reset library associations for known files,
        // all project files are added to the corresponding libraries later on.
        self.files
            .values_mut()
            .for_each(|source_file| source_file.library_names.clear());

        // Files might already be part of self.files, these have to be parsed
        // from in-memory source. New files can be parsed as usual.
        let (known_files, new_files) = self
            .load_files_from_config(config, messages)
            .into_iter()
            .partition(|(file_name, _library_names)| self.files.contains_key(file_name));

        for (file_name, library_names) in known_files {
            if let Some(source_file) = self.files.get_mut(&file_name) {
                source_file.parser_diagnostics.clear();
                source_file.library_names = library_names;
                source_file.design_file = self
                    .parser
                    .parse_design_source(&source_file.source, &mut source_file.parser_diagnostics);
            }
        }

        self.parse_and_add_files(new_files, messages);
    }

    fn load_files_from_config(
        &mut self,
        config: &Config,
        messages: &mut dyn MessageHandler,
    ) -> FnvHashMap<PathBuf, FnvHashSet<Symbol>> {
        let mut files: FnvHashMap<PathBuf, FnvHashSet<Symbol>> = FnvHashMap::default();
        self.empty_libraries.clear();

        for library in config.iter_libraries() {
            let library_name =
                Latin1String::from_utf8(library.name()).expect("Library name not latin-1 encoded");
            let library_name = self.parser.symbol(&library_name);

            let mut empty_library = true;
            for file_name in library.file_names(messages) {
                empty_library = false;

                match files.entry(file_name.clone()) {
                    Entry::Occupied(mut entry) => {
                        entry.get_mut().insert(library_name.clone());
                    }
                    Entry::Vacant(entry) => {
                        let mut set = FnvHashSet::default();
                        set.insert(library_name.clone());
                        entry.insert(set);
                    }
                }
            }

            if empty_library {
                self.empty_libraries.insert(library_name);
            }
        }
        files
    }

    fn parse_and_add_files(
        &mut self,
        files_to_parse: FnvHashMap<PathBuf, FnvHashSet<Symbol>>,
        messages: &mut dyn MessageHandler,
    ) {
        use rayon::prelude::*;

        let parsed: Vec<_> = files_to_parse
            .into_par_iter()
            .map_init(
                || &self.parser,
                |parser, (file_name, library_names)| {
                    let mut diagnostics = Vec::new();
                    let result = parser.parse_design_file(&file_name, &mut diagnostics);
                    (file_name, library_names, diagnostics, result)
                },
            )
            .collect();

        for (file_name, library_names, parser_diagnostics, result) in parsed.into_iter() {
            let (source, design_file) = match result {
                Ok(result) => result,
                Err(err) => {
                    messages.push(Message::file_error(err.to_string(), &file_name));
                    continue;
                }
            };

            self.files.insert(
                source.file_name().to_owned(),
                SourceFile {
                    library_names,
                    source,
                    design_file,
                    parser_diagnostics,
                },
            );
        }
    }

    pub fn get_source(&self, file_name: &Path) -> Option<Source> {
        self.files.get(file_name).map(|file| file.source.clone())
    }

    pub fn update_source(&mut self, source: &Source) {
        let mut source_file = {
            if let Some(mut source_file) = self.files.remove(source.file_name()) {
                // File is already part of the project
                for library_name in source_file.library_names.iter() {
                    self.root.remove_source(library_name.clone(), source);
                }
                source_file.source = source.clone();
                source_file
            } else {
                // File is not part of the project
                // @TODO use config wildcards to map to library

                // Add unmapped files to an anonymous library work
                // To still get some semantic analysis for unmapped files
                let mut library_names = FnvHashSet::default();
                library_names.insert(self.root.symbol_utf8("work"));

                SourceFile {
                    source: source.clone(),
                    library_names,
                    parser_diagnostics: vec![],
                    design_file: DesignFile::default(),
                }
            }
        };
        source_file.parser_diagnostics.clear();
        source_file.design_file = self
            .parser
            .parse_design_source(source, &mut source_file.parser_diagnostics);
        self.files
            .insert(source.file_name().to_owned(), source_file);
    }

    pub fn analyse(&mut self) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();

        for source_file in self.files.values_mut() {
            let design_file = source_file.take_design_file();
            // Avoid cloning design files for single library
            let mut design_files = multiply(design_file, source_file.library_names.len());

            for library_name in source_file.library_names.iter() {
                let design_file = design_files.pop().unwrap();
                self.root.add_design_file(library_name.clone(), design_file);
            }

            for diagnostic in source_file.parser_diagnostics.iter().cloned() {
                diagnostics.push(diagnostic);
            }
        }

        for library_name in self.empty_libraries.iter() {
            self.root.ensure_library(library_name.clone());
        }

        self.root.analyze(&mut diagnostics);
        diagnostics
    }

    /// Search for reference at position
    /// Character offset on a line in a document (zero-based). Assuming that the line is
    /// represented as a string, the `character` value represents the gap between the
    /// `character` and `character + 1`.
    ///
    /// If the character value is greater than the line length it defaults back to the
    /// line length.
    pub fn search_reference(&self, source: &Source, cursor: Position) -> Option<SrcPos> {
        self.root.search_reference(source, cursor)
    }

    /// Search for the declaration at decl_pos and format it
    pub fn format_declaration(&self, decl_pos: &SrcPos) -> Option<String> {
        self.root.format_declaration(decl_pos)
    }

    /// Search for all references to the declaration at decl_pos
    pub fn find_all_references(&self, decl_pos: &SrcPos) -> Vec<SrcPos> {
        self.root.find_all_references(decl_pos)
    }

    pub fn files(&self) -> impl Iterator<Item = &SourceFile> {
        self.files.values()
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

pub struct SourceFile {
    library_names: FnvHashSet<Symbol>,
    source: Source,
    design_file: DesignFile,
    parser_diagnostics: Vec<Diagnostic>,
}

impl SourceFile {
    fn take_design_file(&mut self) -> DesignFile {
        std::mem::take(&mut self.design_file)
    }

    pub fn num_lines(&self) -> usize {
        self.source.contents().num_lines()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::test::check_no_diagnostics;

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
        let mut project = Project::from_config(&config, &mut messages);
        assert_eq!(messages, vec![]);
        check_no_diagnostics(&project.analyse());
    }

    #[test]
    fn unmapped_libraries_are_analyzed() {
        let mut messages = Vec::new();
        let mut project = Project::from_config(&Config::default(), &mut messages);
        assert_eq!(messages, vec![]);
        let diagnostics = project.analyse();
        check_no_diagnostics(&diagnostics);

        let root = tempfile::tempdir().unwrap();
        let vhdl_file_path = root.path().join("file.vhd");
        std::fs::write(
            &vhdl_file_path,
            "
entity ent is
end ent;

architecture rtl of ent is
begin
end architecture;

architecture rtl of ent is
begin
end architecture;
",
        )
        .unwrap();
        let source = Source::from_latin1_file(&vhdl_file_path).unwrap();

        project.update_source(&source);
        let diagnostics = project.analyse();
        assert_eq!(diagnostics.len(), 1);
        let diag = diagnostics.first().unwrap();
        assert_eq!(diag.message, "Duplicate architecture 'rtl' of entity 'ent'")
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
        let mut project = Project::from_config(&config, &mut messages);
        assert_eq!(messages, vec![]);
        check_no_diagnostics(&project.analyse());
    }

    fn update(project: &mut Project, source: &mut Source, contents: &str) {
        std::fs::write(&std::path::Path::new(source.file_name()), contents).unwrap();
        *source = Source::from_latin1_file(source.file_name()).unwrap();
        project.update_source(source);
    }

    /// Test that the same file can be added to several libraries
    #[test]
    fn test_re_analyze_after_update() {
        let tempdir = tempfile::tempdir().unwrap();
        let root = dunce::canonicalize(tempdir.path()).unwrap();

        let path1 = root.join("file1.vhd");
        let path2 = root.join("file2.vhd");
        std::fs::write(
            &path1,
            "
package pkg is
end package;
        ",
        )
        .unwrap();
        let mut source1 = Source::from_latin1_file(&path1).unwrap();

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
        let mut source2 = Source::from_latin1_file(&path2).unwrap();

        let config_str = "
[libraries]
lib1.files = ['file1.vhd']
lib2.files = ['file2.vhd']
        ";

        let config = Config::from_str(config_str, &root).unwrap();
        let mut messages = Vec::new();
        let mut project = Project::from_config(&config, &mut messages);
        assert_eq!(messages, vec![]);
        check_no_diagnostics(&project.analyse());

        // Add syntax error
        update(
            &mut project,
            &mut source1,
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
            &mut source1,
            "
package pkg is
end package;
        ",
        );
        check_no_diagnostics(&project.analyse());

        // Add analysis error
        update(
            &mut project,
            &mut source2,
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
            &mut source2,
            "
package pkg is
end package;
        ",
        );
        check_no_diagnostics(&project.analyse());
    }

    /// Test that the configuration can be updated
    #[test]
    fn test_config_update() {
        let tempdir = tempfile::tempdir().unwrap();
        let root = dunce::canonicalize(tempdir.path()).unwrap();

        let path1 = root.join("file1.vhd");
        let path2 = root.join("file2.vhd");
        std::fs::write(
            &path1,
            "
library unkown;
use unkown.pkg.all;

package pkg is
end package;
        ",
        )
        .unwrap();
        let source1 = Source::from_latin1_file(&path1).unwrap();

        std::fs::write(
            &path2,
            "
library unkown;
use unkown.pkg.all;

package pkg is
end package;
        ",
        )
        .unwrap();
        let source2 = Source::from_latin1_file(&path2).unwrap();

        let config_str1 = "
[libraries]
lib.files = ['file1.vhd']
        ";
        let config1 = Config::from_str(config_str1, &root).unwrap();

        let config_str2 = "
[libraries]
lib.files = ['file2.vhd']
        ";
        let config2 = Config::from_str(config_str2, &root).unwrap();

        let mut messages = Vec::new();
        let mut project = Project::from_config(&config1, &mut messages);
        assert_eq!(messages, vec![]);

        // Invalid library should only be reported in source1
        let diagnostics = project.analyse();
        assert_eq!(diagnostics.len(), 2);
        assert_eq!(diagnostics[0].pos.source, source1); // No such library
        assert_eq!(diagnostics[1].pos.source, source1); // No declaration

        // Change configuration file
        project.update_config(&config2, &mut messages);
        assert_eq!(messages, vec![]);

        // Invalid library should only be reported in source2
        let diagnostics = project.analyse();
        assert_eq!(diagnostics.len(), 2);
        assert_eq!(diagnostics[0].pos.source, source2); // No such library
        assert_eq!(diagnostics[1].pos.source, source2); // No declaration
    }
}
