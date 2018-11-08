// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use source::SrcPos;
use std::convert::AsRef;

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Severity {
    Warning,
    Error,
}

#[must_use]
#[derive(PartialEq, Debug, Clone)]
pub struct Message {
    pub pos: SrcPos,
    pub message: String,
    pub severity: Severity,
}

impl Message {
    pub fn when(self, message: &str) -> Message {
        Message {
            message: format!("{}, when {}", &self.message, &message),
            pos: self.pos,
            severity: self.severity,
        }
    }

    pub fn pretty_string(self: &Self) -> String {
        let (lineno, pretty_str) = self.pos.lineno_and_pretty_string();
        let file_name = self.pos.source.file_name().unwrap_or("<unknown file>");
        let severity = match self.severity {
            Severity::Error => &"error",
            Severity::Warning => &"warning",
        };
        format!(
            "{}:{}: {}: {}\n{}",
            file_name, lineno, severity, self.message, pretty_str
        )
    }
}

pub fn message<T: AsRef<SrcPos>>(item: T, msg: &str, severity: Severity) -> Message {
    Message {
        pos: item.as_ref().clone(),
        message: msg.to_string(),
        severity: severity,
    }
}

pub fn error(item: impl AsRef<SrcPos>, msg: &str) -> Message {
    message(item, msg, Severity::Error)
}

pub fn warning(item: impl AsRef<SrcPos>, msg: &str) -> Message {
    message(item, msg, Severity::Warning)
}

pub trait MessageHandler {
    fn push(self: &mut Self, err: Message);
}

pub fn push_result<T>(messages: &mut MessageHandler, msg: Result<T, Message>) {
    if let Err(msg) = msg {
        messages.push(msg);
    }
}

pub fn push_some(messages: &mut MessageHandler, msg: Option<Message>) {
    if let Some(msg) = msg {
        messages.push(msg);
    }
}

impl MessageHandler for Vec<Message> {
    fn push(self: &mut Self, msg: Message) {
        self.push(msg)
    }
}

pub type ParseResult<T> = Result<T, Message>;
