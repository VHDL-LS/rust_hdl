// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use source::SrcPos;
use std::convert::{AsRef, Into};

#[derive(PartialEq, Debug, Clone, Copy, Eq, Hash)]
pub enum Severity {
    Hint,
    Info,
    Warning,
    Error,
}

#[must_use]
#[derive(PartialEq, Debug, Clone, Eq, Hash)]
pub struct Message {
    pub pos: SrcPos,
    pub message: String,
    pub severity: Severity,
    pub related: Vec<(SrcPos, String)>,
}

impl Message {
    pub fn new(item: impl AsRef<SrcPos>, msg: impl Into<String>, severity: Severity) -> Message {
        Message {
            pos: item.as_ref().clone(),
            message: msg.into(),
            severity,
            related: vec![],
        }
    }

    pub fn error(item: impl AsRef<SrcPos>, msg: impl Into<String>) -> Message {
        Self::new(item, msg, Severity::Error)
    }

    pub fn warning(item: impl AsRef<SrcPos>, msg: impl Into<String>) -> Message {
        Self::new(item, msg, Severity::Warning)
    }

    pub fn hint(item: impl AsRef<SrcPos>, msg: impl Into<String>) -> Message {
        Self::new(item, msg, Severity::Hint)
    }

    pub fn info(item: impl AsRef<SrcPos>, msg: impl Into<String>) -> Message {
        Self::new(item, msg, Severity::Info)
    }

    pub fn when(self, message: impl AsRef<str>) -> Message {
        Message {
            message: format!("{}, when {}", &self.message, message.as_ref()),
            pos: self.pos,
            severity: self.severity,
            related: vec![],
        }
    }

    pub fn related(self, item: impl AsRef<SrcPos>, message: impl Into<String>) -> Message {
        let mut msg = self;
        msg.add_related(item, message);
        msg
    }

    pub fn add_related(&mut self, item: impl AsRef<SrcPos>, message: impl Into<String>) {
        self.related
            .push((item.as_ref().to_owned(), message.into()));
    }

    pub fn drain_related(&mut self) -> Vec<Message> {
        let mut messages = Vec::with_capacity(self.related.len());
        let related = std::mem::replace(&mut self.related, Vec::new());
        for (pos, msg) in related {
            messages.push(Message::new(
                pos,
                format!("related: {}", msg),
                Severity::Hint,
            ));
        }
        messages
    }

    pub fn show(&self) -> String {
        let mut result = String::new();
        for (pos, message) in self.related.iter() {
            result.push_str(&pos.show(&format!("related: {}", message)));
            result.push('\n');
        }
        let severity = match self.severity {
            Severity::Error => &"error",
            Severity::Warning => &"warning",
            Severity::Info => &"info",
            Severity::Hint => &"hint",
        };
        result.push_str(&self.pos.show(&format!("{}: {}", severity, self.message)));
        result
    }
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

#[cfg(test)]
mod tests {
    use super::*;
    use test_util::Code;

    #[test]
    fn show_warning() {
        let code = Code::new("hello\nworld\nline\n");
        assert_eq!(
            Message::warning(code.s1("world"), "Greetings").show(),
            "\
warning: Greetings
  --> {unknown file}:2
   |
1  |  hello
2 --> world
   |  ~~~~~
3  |  line
"
        );
    }

    #[test]
    fn show_error() {
        let code = Code::new("hello\nworld\nline\n");
        assert_eq!(
            Message::error(code.s1("world"), "Greetings").show(),
            "\
error: Greetings
  --> {unknown file}:2
   |
1  |  hello
2 --> world
   |  ~~~~~
3  |  line
"
        );
    }

    #[test]
    fn show_related() {
        let code = Code::new("hello\nworld\nline\n");

        let err =
            Message::error(code.s1("line"), "Greetings").related(code.s1("hello"), "From here");

        assert_eq!(
            err.show(),
            "\
related: From here
  --> {unknown file}:1
   |
1 --> hello
   |  ~~~~~
2  |  world
3  |  line

error: Greetings
  --> {unknown file}:3
   |
1  |  hello
2  |  world
3 --> line
   |  ~~~~
"
        );
    }

}
