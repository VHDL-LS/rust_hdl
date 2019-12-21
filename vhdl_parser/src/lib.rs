// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

#[macro_use]
pub mod ast;
#[macro_use]
mod tokenizer;
mod alias_declaration;
mod analysis;
mod attributes;
mod common;
mod component_declaration;
mod concurrent_statement;
mod config;
mod configuration;
mod contents;
mod context;
mod declarative_part;
mod design_unit;
mod diagnostic;
mod expression;
mod interface_declaration;
mod latin_1;
mod message;
mod names;
mod object_declaration;
mod parser;
mod project;
mod range;
mod sequential_statement;
mod source;
mod subprogram;
mod subtype_indication;
mod symbol_table;
mod tokenstream;
mod type_declaration;
mod waveform;

#[cfg(test)]
mod test_util;

pub use crate::config::Config;
pub use crate::diagnostic::{Diagnostic, Severity};
pub use crate::latin_1::Utf8ToLatin1Error;
pub use crate::message::{Message, MessageType};
pub use crate::parser::{ParserError, ParserResult, VHDLParser};
pub use crate::project::Project;
pub use crate::source::{Position, Range, Source, SrcPos};
