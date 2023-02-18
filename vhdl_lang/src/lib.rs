// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com
#![allow(clippy::upper_case_acronyms)]
// False positives with unconditional loops
// allow for now
#![allow(clippy::vec_init_then_push)]

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
    Diagnostic, Latin1String, Message, MessageHandler, MessagePrinter, MessageType,
    NullDiagnostics, NullMessages, Position, Range, Severity, Source, SrcPos,
};

pub use crate::analysis::{
    AnyEnt, AnyEntKind, Concurrent, Design, EntHierarchy, EntRef, EntityId, Object, Overloaded,
    Type,
};
pub use crate::project::{Project, SourceFile};
pub use crate::syntax::{ParserResult, VHDLParser};
