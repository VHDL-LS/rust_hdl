// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

#[derive(Debug, PartialEq)]
pub enum MessageType {
    Error,
    Warning,
    Info,
    Log,
}

#[must_use]
#[derive(Debug, PartialEq)]
pub struct Message {
    pub message_type: MessageType,
    pub message: String,
}

impl Message {
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

    pub fn file_error(message: impl Into<String>, file_name: impl Into<String>) -> Message {
        Message {
            message_type: MessageType::Error,
            message: format!("{} (In file {})", message.into(), file_name.into()),
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
