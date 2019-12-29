// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use crate::ast::DesignFile;
use crate::contents::ContentReader;
use crate::design_unit::parse_design_file;
use crate::diagnostic::DiagnosticHandler;
use crate::latin_1::Latin1String;
use crate::source::Source;
use crate::symbol_table::Symbol;
use crate::symbol_table::SymbolTable;
use crate::tokenizer::Tokenizer;
use crate::tokenstream::TokenStream;
use std::io;
use std::sync::Arc;

#[derive(Clone)]
pub struct VHDLParser {
    pub symtab: Arc<SymbolTable>,
}

pub type ParserResult = Result<(Source, DesignFile), io::Error>;

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
    ) -> DesignFile {
        let contents = source.contents();
        let tokenizer = Tokenizer::new(self.symtab.clone(), &source, ContentReader::new(&contents));
        let mut stream = TokenStream::new(tokenizer);

        match parse_design_file(&mut stream, diagnostics) {
            Ok(design_file) => design_file,
            Err(diagnostic) => {
                diagnostics.push(diagnostic);
                DesignFile::default()
            }
        }
    }

    pub fn parse_design_file(
        &self,
        file_name: &str,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> ParserResult {
        let source = Source::from_latin1_file(file_name)?;
        let design_file = self.parse_design_source(&source, diagnostics);
        Ok((source, design_file))
    }
}

impl Default for VHDLParser {
    fn default() -> Self {
        Self::new()
    }
}
