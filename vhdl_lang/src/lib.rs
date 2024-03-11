// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com
#![allow(clippy::upper_case_acronyms)]

#[macro_use]
extern crate vhdl_lang_macros;
extern crate self as vhdl_lang;

#[macro_use]
pub mod ast;
#[macro_use]
mod analysis;
mod api;
mod config;
mod data;
mod lint;
mod named_entity;
mod project;
mod syntax;

mod completion;

pub use crate::config::Config;
pub use crate::data::{
    Diagnostic, Latin1String, Message, MessageHandler, MessagePrinter, MessageType,
    NullDiagnostics, NullMessages, Position, Range, Severity, Source, SrcPos,
};

pub use crate::analysis::EntHierarchy;
pub use crate::named_entity::{
    AnyEnt, AnyEntKind, Concurrent, Design, EntRef, EntityId, HasEntityId, InterfaceEnt, Object,
    Overloaded, Reference, Related, Sequential, Type,
};

pub use crate::project::{Project, SourceFile};
pub use crate::syntax::{
    kind_str, HasTokenSpan, ParserResult, Token, TokenAccess, TokenId, TokenSpan, VHDLParser,
};

pub use completion::{list_completion_options, CompletionItem};
