// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

use super::latin_1::Latin1String;
use super::source::{Position, Range};
use std::fs::File;
use std::io;
use std::io::prelude::Read;

pub struct Contents {
    lines: Vec<Latin1String>,
}

impl Contents {
    pub fn from_latin1_file(file_name: &str) -> io::Result<Contents> {
        let mut file = File::open(file_name)?;
        let mut bytes = Vec::new();
        file.read_to_end(&mut bytes)?;
        Ok(Contents::from_latin1(&Latin1String::from_vec(bytes)))
    }

    pub fn from_latin1(code: &Latin1String) -> Contents {
        let mut lines = Vec::new();

        let mut i = 0;
        let mut start = 0;
        while i < code.bytes.len() {
            let byte = code.bytes[i];

            if byte == b'\n' {
                i += 1;
                lines.push(Latin1String::new(&code.bytes[start..i]));
                start = i;
            } else if byte == b'\r' {
                i += 1;
                let mut line = Latin1String::new(&code.bytes[start..i]);
                line.bytes[i - start - 1] = b'\n';
                lines.push(line);

                if code.bytes.get(i) == Some(&b'\n') {
                    i += 1;
                }

                start = i;
            } else {
                i += 1;
            }
        }

        if start < code.bytes.len() {
            lines.push(Latin1String::new(&code.bytes[start..]));
        }

        Contents { lines: lines }
    }

    pub fn start(&self) -> Position {
        Position {
            line: 0,
            character: 0,
        }
    }

    fn end(&self) -> Position {
        let line = self.lines.len().saturating_sub(1) as u64;
        let character = self.lines.last().map(|line| line.len()).unwrap_or(0) as u64;
        Position { line, character }
    }

    pub fn range(&self) -> Range {
        Range::new(self.start(), self.end())
    }

    pub fn slice(&self, start: Position, end: Position) -> &[u8] {
        &self.lines[start.line as usize].bytes[start.character as usize..end.character as usize]
    }

    #[cfg(test)]
    pub fn crop(&self, range: Range) -> Contents {
        let Range { start, end } = range;
        let mut lines = self.lines[start.line as usize..(end.line + 1) as usize].to_owned();
        let last_idx = lines.len() - 1;
        let ref mut line = lines[last_idx];
        line.bytes = line.bytes[..end.character as usize].to_owned();
        let ref mut line = lines[0];
        line.bytes = line.bytes[start.character as usize..].to_owned();
        Contents { lines: lines }
    }

    fn get(&self, pos: &Position) -> Option<u8> {
        if let Some(line) = self.lines.get(pos.line as usize) {
            if let Some(byte) = line.bytes.get(pos.character as usize) {
                Some(*byte)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn get_line(&self, lineno: usize) -> Option<&Latin1String> {
        self.lines.get(lineno)
    }

    pub fn reader<'a>(&'a self) -> ContentReader<'a> {
        ContentReader::new(self)
    }
}

pub struct ContentReader<'a> {
    contents: &'a Contents,
    pos: Position,
}

impl<'a> ContentReader<'a> {
    pub fn new(contents: &'a Contents) -> ContentReader<'a> {
        ContentReader {
            contents,
            pos: Position::new(),
        }
    }

    pub fn slice(&self, start: Position, end: Position) -> &[u8] {
        self.contents.slice(start, end)
    }

    fn get(&self) -> Option<u8> {
        self.contents.get(&self.pos)
    }

    fn advance(&mut self, offset: usize) {
        let mut offset = offset as u64;

        let line_len = {
            if let Some(line) = self.contents.get_line(self.pos.line as usize) {
                line.bytes.len() as u64
            } else {
                return;
            }
        };

        if self.pos.character + offset >= line_len {
            if self.pos.line + 1 < self.contents.lines.len() as u64 {
                offset -= line_len - self.pos.character;
                self.pos.character = 0;
                self.pos.line += 1;
                self.advance(offset as usize);
            } else {
                // EOF
                self.pos = self.contents.end();
            }
        } else {
            self.pos.character += offset;
        }
    }

    pub fn pop(&mut self) -> Option<u8> {
        let byte = self.get();
        self.advance(1);
        byte
    }

    #[cfg(test)]
    pub fn matches(&mut self, substr: &Latin1String) -> bool {
        for (i, exp) in substr.bytes.iter().enumerate() {
            if let Some(byte) = self.peek(i) {
                if byte != *exp {
                    return false;
                }
            } else {
                return false;
            }
        }
        true
    }

    pub fn skip_if(&mut self, value: u8) -> bool {
        if self.peek(0) == Some(value) {
            self.pop();
            true
        } else {
            false
        }
    }

    pub fn set_pos(&mut self, pos: Position) {
        self.pos = pos;
    }

    pub fn pos(&self) -> Position {
        self.pos
    }

    pub fn peek(&mut self, offset: usize) -> Option<u8> {
        let pos = self.pos;
        self.advance(offset);
        let byte = self.get();
        self.pos = pos;
        byte
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn new(code: &str) -> Contents {
        Contents::from_latin1(&Latin1String::from_utf8(code).unwrap())
    }

    #[test]
    fn pop_single_line() {
        let contents = new("hi");
        let mut reader = contents.reader();
        assert_eq!(reader.pop(), Some(b'h'));
        assert_eq!(reader.pop(), Some(b'i'));
        assert_eq!(reader.pop(), None);
    }

    #[test]
    fn pop_multi_line_no_newline_at_end() {
        let contents = new("h\ni");
        let mut reader = contents.reader();
        assert_eq!(reader.pop(), Some(b'h'));
        assert_eq!(reader.pop(), Some(b'\n'));
        assert_eq!(reader.pop(), Some(b'i'));
        assert_eq!(reader.pop(), None);
    }

    #[test]
    fn pop_multi_line() {
        let contents = new("h\ni\n");
        let mut reader = contents.reader();
        assert_eq!(reader.pop(), Some(b'h'));
        assert_eq!(reader.pop(), Some(b'\n'));
        assert_eq!(reader.pop(), Some(b'i'));
        assert_eq!(reader.pop(), Some(b'\n'));
        assert_eq!(reader.pop(), None);
    }

    #[test]
    fn empty_lines() {
        let contents = new("\n\n\n");
        let mut reader = contents.reader();
        assert_eq!(reader.pop(), Some(b'\n'));
        assert_eq!(reader.pop(), Some(b'\n'));
        assert_eq!(reader.pop(), Some(b'\n'));
    }

    #[test]
    fn peek() {
        let contents = new("hi");
        let mut reader = contents.reader();
        assert_eq!(reader.peek(0), Some(b'h'));
        assert_eq!(reader.peek(1), Some(b'i'));
        assert_eq!(reader.peek(3), None);
    }

    #[test]
    fn matches() {
        let contents = new("abc");
        let mut reader = contents.reader();
        assert!(reader.matches(&Latin1String::from_utf8("abc").unwrap()));
        assert!(!reader.matches(&Latin1String::from_utf8("bc").unwrap()));
        reader.pop();
        assert!(reader.matches(&Latin1String::from_utf8("bc").unwrap()));
    }
}
