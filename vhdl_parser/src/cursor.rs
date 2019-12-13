// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

use super::latin_1::Latin1String;
use super::source::{Position, Range};
use std::sync::Arc;

#[derive(PartialEq, Clone, Copy, Debug)]
pub struct BytePos {
    idx: usize,
    line: u64,
    character: u64,
}

impl BytePos {
    pub fn new() -> BytePos {
        BytePos {
            idx: 0,
            line: 0,
            character: 0,
        }
    }
    /// Return the range from self to end
    pub fn range_to(self, end: BytePos) -> Range {
        Range {
            start: Position {
                byte_offset: self.idx,
            },
            end: Position {
                byte_offset: end.idx,
            },
        }
    }

    /// Advance cursor updating line character and byte offset
    /// \r is skipped
    /// @TODO maybe skip \r when reading the file instead
    fn pop(&mut self, bytes: &[u8]) -> Option<u8> {
        if let Some(&byte) = bytes.get(self.idx) {
            self.idx += 1;

            if byte == b'\n' {
                self.line += 1;
                self.character = 0;
            } else if byte == b'\r' {
                self.line += 1;
                self.character = 0;

                // Skip \r
                let byte = self.pop(bytes);

                // Reset line so that \r\n is not counted as two lines
                if byte == Some(b'\n') {
                    self.line -= 1;
                }

                return byte;
            } else {
                self.character += 1;
            }

            Some(byte)
        } else {
            None
        }
    }

    fn peek(&self, bytes: &[u8], offset: usize) -> Option<u8> {
        if let Some(&byte) = bytes.get(self.idx + offset) {
            if byte == b'\r' {
                self.peek(bytes, offset + 1)
            } else {
                Some(byte)
            }
        } else {
            None
        }
    }

    /// Return byte position at the (line, character) position
    #[cfg(test)]
    pub fn from_position(bytes: &[u8], position: Position) -> BytePos {
        let mut byte_pos = BytePos::new();

        // Advance byte position until it matches end range
        // Thus we can get byte offset at this (line, character) position
        while byte_pos.to_position() < position {
            byte_pos.pop(bytes);
        }
        assert_eq!(byte_pos.to_position(), position);

        byte_pos
    }

    /// Return byte position at the end
    #[cfg(test)]
    pub fn end_pos(bytes: &[u8]) -> BytePos {
        BytePos::at_byte_offset(bytes, bytes.len())
    }

    /// Return byte position at the end
    #[cfg(test)]
    pub fn at_byte_offset(bytes: &[u8], offset: usize) -> BytePos {
        let mut byte_pos = BytePos::new();
        while byte_pos.byte_offset() < offset {
            byte_pos.pop(bytes);
        }
        assert_eq!(byte_pos.byte_offset(), offset);
        byte_pos
    }

    pub fn to_position(self) -> Position {
        Position {
            byte_offset: self.idx,
        }
    }

    pub fn byte_offset(&self) -> usize {
        self.idx
    }
}

#[derive(Clone)]
pub struct ByteCursor {
    code: Arc<Latin1String>,
    pos: BytePos,
}

impl ByteCursor {
    pub fn new(code: Arc<Latin1String>) -> ByteCursor {
        ByteCursor {
            code,
            pos: BytePos::new(),
        }
    }

    pub fn pop(&mut self) -> Option<u8> {
        self.pos.pop(&self.code.bytes)
    }

    pub fn slice(&self, start: BytePos, end: BytePos) -> &[u8] {
        &self.code.bytes[start.byte_offset()..end.byte_offset()]
    }

    pub fn skip_if(&mut self, value: u8) -> bool {
        if self.peek(0) == Some(value) {
            self.pop();
            true
        } else {
            false
        }
    }

    pub fn set_pos(&mut self, pos: BytePos) {
        self.pos = pos;
    }

    pub fn pos(&self) -> BytePos {
        self.pos
    }

    pub fn peek(&mut self, offset: usize) -> Option<u8> {
        self.pos.peek(&self.code.bytes, offset)
    }
}
