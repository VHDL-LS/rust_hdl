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
mod config;
mod configuration;
mod context;
mod declarative_part;
mod declarative_region;
mod design_unit;
mod expression;
mod interface_declaration;
mod latin_1;
mod library;
mod message;
mod names;
mod object_declaration;
mod parser;
mod project;
mod range;
mod semantic;
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
pub use crate::message::{Message, Severity};
pub use crate::parser::{ParserError, ParserResult, VHDLParser};
pub use crate::project::Project;
pub use crate::source::{Source, SrcPos};
