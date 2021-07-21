// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

#[macro_use]
pub mod ast;
#[macro_use]
mod analysis;
mod config;
mod data;
mod project;
mod syntax;

pub use crate::config::Config;
pub use crate::data::{
    Diagnostic, Latin1String, Message, MessageHandler, MessagePrinter, MessageType, Position,
    Range, Severity, Source, SrcPos, WithPos,
};

pub use crate::project::{Project, SourceFile};
pub use crate::syntax::{ParserResult, VHDLParser};
