// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::tokens::{Symbols, TokenStream, Tokenizer};
use crate::ast::DesignFile;
use crate::data::*;
use crate::standard::VHDLStandard;
use crate::syntax::design_unit::parse_design_file;
use std::io;
use std::sync::Arc;

pub struct VHDLParser {
    pub symbols: Arc<Symbols>,
    pub standard: VHDLStandard,
}

pub(crate) struct ParsingContext<'a> {
    pub stream: &'a TokenStream<'a>,
    pub diagnostics: &'a mut dyn DiagnosticHandler,
    pub standard: VHDLStandard,
}

pub type ParserResult = Result<(Source, DesignFile), io::Error>;

impl VHDLParser {
    pub fn new(vhdl_standard: VHDLStandard) -> VHDLParser {
        VHDLParser {
            symbols: Arc::new(Symbols::from_standard(vhdl_standard)),
            standard: vhdl_standard,
        }
    }

    pub fn symbol(&self, name: &Latin1String) -> Symbol {
        self.symbols.symtab().insert(name)
    }

    pub fn parse_design_source(
        &self,
        source: &Source,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> DesignFile {
        let contents = source.contents();
        let tokenizer = Tokenizer::new(&self.symbols, source, ContentReader::new(&contents));
        let stream = TokenStream::new(tokenizer, diagnostics);

        let mut ctx = ParsingContext {
            stream: &stream,
            diagnostics,
            standard: self.standard,
        };

        match parse_design_file(&mut ctx) {
            Ok(design_file) => design_file,
            Err(diagnostic) => {
                diagnostics.push(diagnostic);
                DesignFile::default()
            }
        }
    }

    pub fn parse_design_file(
        &self,
        file_name: &Path,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> ParserResult {
        let source = Source::from_latin1_file(file_name)?;
        let design_file = self.parse_design_source(&source, diagnostics);
        Ok((source, design_file))
    }
}
