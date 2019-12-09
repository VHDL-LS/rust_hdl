// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

use crate::analysis::library::DesignRoot;
use crate::analysis::semantic::Analyzer;
use crate::diagnostic::Diagnostic;
use crate::latin_1::Latin1String;
use crate::source::Source;
use crate::symbol_table::Symbol;
use crate::test_util::{Code, CodeBuilder};
use std::collections::{hash_map::Entry, HashMap};
use std::sync::Arc;

pub struct LibraryBuilder {
    code_builder: CodeBuilder,
    libraries: HashMap<Symbol, Vec<Code>>,
}

impl LibraryBuilder {
    fn new_no_std() -> LibraryBuilder {
        LibraryBuilder {
            code_builder: CodeBuilder::new(),
            libraries: HashMap::default(),
        }
    }

    pub fn new() -> LibraryBuilder {
        let mut library = LibraryBuilder::new_no_std();
        library.code_from_source(
            "std",
            Source::inline(
                "standard.vhd",
                Arc::new(Latin1String::new(include_bytes!(
                    "../../../../example_project/vhdl_libraries/2008/std/standard.vhd"
                ))),
            ),
        );
        library.code_from_source(
            "std",
            Source::inline(
                "textio.vhd",
                Arc::new(Latin1String::new(include_bytes!(
                    "../../../../example_project/vhdl_libraries/2008/std/textio.vhd"
                ))),
            ),
        );
        library.code_from_source(
            "std",
            Source::inline(
                "env.vhd",
                Arc::new(Latin1String::new(include_bytes!(
                    "../../../../example_project/vhdl_libraries/2008/std/env.vhd"
                ))),
            ),
        );
        library
    }

    fn add_code(&mut self, library_name: &str, code: Code) {
        let library_name = self.code_builder.symbol(library_name);
        match self.libraries.entry(library_name) {
            Entry::Occupied(mut entry) => {
                entry.get_mut().push(code.clone());
            }
            Entry::Vacant(entry) => {
                entry.insert(vec![code.clone()]);
            }
        }
    }

    pub fn code(&mut self, library_name: &str, code: &str) -> Code {
        let code = self.code_builder.code(code);
        self.add_code(library_name, code.clone());
        code
    }

    fn code_from_source(&mut self, library_name: &str, source: Source) -> Code {
        let code = self.code_builder.code_from_source(source);
        self.add_code(library_name, code.clone());
        code
    }

    pub fn get_analyzed_root(&self) -> (DesignRoot, Vec<Diagnostic>) {
        let mut root = DesignRoot::new();
        let mut diagnostics = Vec::new();

        for (library_name, codes) in self.libraries.iter() {
            let library = root.ensure_library(library_name.clone());
            for code in codes {
                library.add_design_file(code.design_file());
            }
            library.refresh(&mut diagnostics);
        }

        Analyzer::new(&root, &self.code_builder.symtab.clone()).analyze(&mut diagnostics);

        (root, diagnostics)
    }

    pub fn analyze(&self) -> Vec<Diagnostic> {
        self.get_analyzed_root().1
    }
}

pub fn duplication_diagnostic(code: &Code, name: &str, occ1: usize, occ2: usize) -> Diagnostic {
    Diagnostic::error(
        code.s(&name, occ2),
        format!("Duplicate declaration of '{}'", &name),
    )
    .related(code.s(&name, occ1), "Previously defined here")
}

pub fn duplication_diagnostics(code: &Code, names: &[&str]) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();
    for name in names {
        diagnostics.push(duplication_diagnostic(code, name, 1, 2));
    }
    diagnostics
}

pub fn duplication_diagnostics_two_file(
    code1: &Code,
    code2: &Code,
    names: &[&str],
) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();
    for name in names {
        diagnostics.push(
            Diagnostic::error(
                code2.s1(&name),
                format!("Duplicate declaration of '{}'", &name),
            )
            .related(code1.s1(&name), "Previously defined here"),
        )
    }
    diagnostics
}
