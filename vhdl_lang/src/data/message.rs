// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

use std::path::Path;

#[derive(Debug, PartialEq, Eq)]
pub enum MessageType {
    Error,
    Warning,
    Info,
    Log,
}

#[must_use]
#[derive(Debug, PartialEq, Eq)]
pub struct Message {
    pub message_type: MessageType,
    pub message: String,
}

impl Message {
    pub fn log(message: impl Into<String>) -> Message {
        Message {
            message_type: MessageType::Log,
            message: message.into(),
        }
    }

    pub fn info(message: impl Into<String>) -> Message {
        Message {
            message_type: MessageType::Info,
            message: message.into(),
        }
    }

    pub fn warning(message: impl Into<String>) -> Message {
        Message {
            message_type: MessageType::Warning,
            message: message.into(),
        }
    }

    pub fn error(message: impl Into<String>) -> Message {
        Message {
            message_type: MessageType::Error,
            message: message.into(),
        }
    }

    pub fn file_error(message: impl Into<String>, file_name: &Path) -> Message {
        Message {
            message_type: MessageType::Error,
            message: format!(
                "{} (In file {})",
                message.into(),
                file_name.to_string_lossy()
            ),
        }
    }
}

impl std::fmt::Display for Message {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.message_type.as_ref(), self.message)
    }
}

impl AsRef<str> for MessageType {
    fn as_ref(&self) -> &str {
        match self {
            Self::Error => "error",
            Self::Warning => "warning",
            Self::Info => "info",
            Self::Log => "log",
        }
    }
}

pub trait MessageHandler {
    fn push(&mut self, message: Message);
}

impl MessageHandler for Vec<Message> {
    fn push(&mut self, message: Message) {
        self.push(message)
    }
}

#[derive(Default)]
pub struct MessagePrinter {}

impl MessageHandler for MessagePrinter {
    fn push(&mut self, message: Message) {
        println!("{message}");
    }
}

#[derive(Default)]
pub struct NullMessages;

impl MessageHandler for NullMessages {
    fn push(&mut self, _message: Message) {
        // Ignore
    }
}
