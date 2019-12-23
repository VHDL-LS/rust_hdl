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
        Contents {
            lines: split_lines(code),
        }
    }

    pub fn start(&self) -> Position {
        Position {
            line: 0,
            character: 0,
        }
    }

    fn end(&self) -> Position {
        let line = self.num_lines().saturating_sub(1) as u64;
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

    pub fn num_lines(&self) -> usize {
        self.lines.len()
    }

    pub fn get_line(&self, lineno: usize) -> Option<&Latin1String> {
        self.lines.get(lineno)
    }

    pub fn change(&mut self, range: &Range, content: &Latin1String) {
        if self.lines.len() == 0 {
            self.lines = split_lines(content);
            return;
        }

        let Range { start, end } = range;

        let start_char = start.character as usize;
        let end_char = end.character as usize;
        let start_line = start.line as usize;
        let end_line = end.line as usize;

        let head_len = start_char;
        let last_len = self.lines.get(end_line).map(|line| line.len()).unwrap_or(0);
        let tail_len = last_len.saturating_sub(end_char);
        let merged_len = head_len + content.len() + tail_len;
        let mut merged_content = Vec::with_capacity(merged_len);

        if let Some(line) = self.lines.get(start_line) {
            merged_content.extend_from_slice(&line.bytes[..start_char]);
        }
        merged_content.extend_from_slice(&content.bytes);
        if let Some(line) = self.lines.get(end_line) {
            merged_content.extend_from_slice(&line.bytes[end_char..]);
        }

        self.lines
            .splice(
                start_line..=end_line,
                split_lines(&Latin1String::from_vec(merged_content)).into_iter(),
            )
            .count();
    }
}

fn split_lines(code: &Latin1String) -> Vec<Latin1String> {
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
    lines
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub struct ReaderState {
    pos: Position,
}

impl ReaderState {
    pub fn pos(&self) -> Position {
        self.pos
    }
}

#[derive(Clone)]
pub struct ContentReader<'a> {
    contents: &'a Contents,
    state: ReaderState,
}

impl<'a> ContentReader<'a> {
    pub fn new(contents: &'a Contents) -> ContentReader<'a> {
        ContentReader {
            contents,
            state: ReaderState {
                pos: Position::default(),
            },
        }
    }

    fn get(&self) -> Option<u8> {
        self.contents.get(&self.state.pos)
    }

    #[must_use]
    pub fn pop(&mut self) -> Option<u8> {
        let byte = self.get();
        self.skip();
        byte
    }

    pub fn skip(&mut self) {
        let ref mut pos = self.state.pos;

        let line_len = {
            if let Some(line) = self.contents.get_line(pos.line as usize) {
                line.bytes.len() as u64
            } else {
                return;
            }
        };

        if pos.character + 1 >= line_len {
            if pos.line + 1 < self.contents.num_lines() as u64 {
                pos.character = 0;
                pos.line += 1;
            } else {
                // EOF
                *pos = self.contents.end();
            }
        } else {
            pos.character += 1;
        }
    }

    pub fn pop_lowercase(&mut self) -> Option<u8> {
        self.pop().map(Latin1String::lowercase)
    }

    pub fn peek_lowercase(&mut self) -> Option<u8> {
        self.peek().map(Latin1String::lowercase)
    }

    #[cfg(test)]
    pub fn matches(&mut self, substr: &Latin1String) -> bool {
        let mut lookahead = self.clone();
        for exp in substr.bytes.iter() {
            if let Some(byte) = lookahead.pop() {
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
        if self.peek() == Some(value) {
            self.skip();
            true
        } else {
            false
        }
    }

    pub fn set_state(&mut self, state: ReaderState) {
        self.state = state;
    }

    pub fn set_to(&mut self, reader: &ContentReader) {
        self.state = reader.state;
    }

    pub fn pos(&self) -> Position {
        self.state.pos()
    }

    #[cfg(test)]
    pub fn seek_pos(&mut self, pos: Position) {
        self.state = ReaderState { pos };
    }

    pub fn state(&self) -> ReaderState {
        self.state
    }

    pub fn peek(&self) -> Option<u8> {
        self.get()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn new(code: &str) -> Contents {
        Contents::from_latin1(&Latin1String::from_utf8(code).unwrap())
    }

    fn reader(contents: &Contents) -> ContentReader {
        ContentReader::new(contents)
    }

    #[test]
    fn pop_single_line() {
        let contents = new("hi");
        let mut reader = reader(&contents);
        assert_eq!(reader.pop(), Some(b'h'));
        assert_eq!(reader.pop(), Some(b'i'));
        assert_eq!(reader.pop(), None);
    }

    #[test]
    fn pop_multi_line_no_newline_at_end() {
        let contents = new("h\ni");
        let mut reader = reader(&contents);
        assert_eq!(reader.pop(), Some(b'h'));
        assert_eq!(reader.pop(), Some(b'\n'));
        assert_eq!(reader.pop(), Some(b'i'));
        assert_eq!(reader.pop(), None);
    }

    #[test]
    fn pop_multi_line() {
        let contents = new("h\ni\n");
        let mut reader = reader(&contents);
        assert_eq!(reader.pop(), Some(b'h'));
        assert_eq!(reader.pop(), Some(b'\n'));
        assert_eq!(reader.pop(), Some(b'i'));
        assert_eq!(reader.pop(), Some(b'\n'));
        assert_eq!(reader.pop(), None);
    }

    #[test]
    fn empty_lines() {
        let contents = new("\n\n\n");
        let mut reader = reader(&contents);
        assert_eq!(reader.pop(), Some(b'\n'));
        assert_eq!(reader.pop(), Some(b'\n'));
        assert_eq!(reader.pop(), Some(b'\n'));
    }

    #[test]
    fn peek() {
        let contents = new("hi");
        let mut reader = reader(&contents);
        assert_eq!(reader.peek(), Some(b'h'));
        assert_eq!(reader.pop(), Some(b'h'));
        assert_eq!(reader.peek(), Some(b'i'));
        assert_eq!(reader.pop(), Some(b'i'));
        assert_eq!(reader.peek(), None);
        assert_eq!(reader.pop(), None);
    }

    #[test]
    fn matches() {
        let contents = new("abc");
        let mut reader = reader(&contents);
        assert!(reader.matches(&Latin1String::from_utf8("abc").unwrap()));
        assert!(!reader.matches(&Latin1String::from_utf8("bc").unwrap()));
        reader.skip();
        assert!(reader.matches(&Latin1String::from_utf8("bc").unwrap()));
    }

    fn flatten(contents: &Contents) -> String {
        let mut result = String::new();
        for line in contents.lines.iter() {
            result.push_str(&line.to_string());
        }
        result
    }

    fn l1(code: &str) -> Latin1String {
        Latin1String::from_utf8(code).unwrap()
    }

    #[test]
    fn change_first() {
        let mut contents = new("hello");
        assert_eq!(flatten(&contents), "hello");
        contents.change(
            &Range::new(Position::new(0, 0), Position::new(0, 1)),
            &l1("_"),
        );
        assert_eq!(flatten(&contents), "_ello");
    }

    #[test]
    fn change_last() {
        let mut contents = new("hello");
        assert_eq!(flatten(&contents), "hello");
        contents.change(
            &Range::new(Position::new(0, 4), Position::new(0, 5)),
            &l1("_"),
        );
        assert_eq!(flatten(&contents), "hell_");
    }

    #[test]
    fn change_middle() {
        let mut contents = new("hello");
        assert_eq!(flatten(&contents), "hello");
        contents.change(
            &Range::new(Position::new(0, 2), Position::new(0, 4)),
            &l1("__"),
        );
        assert_eq!(flatten(&contents), "he__o");
    }

    #[test]
    fn change_shrink() {
        let mut contents = new("hello");
        assert_eq!(flatten(&contents), "hello");
        contents.change(
            &Range::new(Position::new(0, 2), Position::new(0, 4)),
            &l1("_"),
        );
        assert_eq!(flatten(&contents), "he_o");
    }

    #[test]
    fn change_grow() {
        let mut contents = new("hello");
        assert_eq!(flatten(&contents), "hello");
        contents.change(
            &Range::new(Position::new(0, 2), Position::new(0, 4)),
            &l1("___"),
        );
        assert_eq!(flatten(&contents), "he___o");
    }

    #[test]
    fn change_multi_line() {
        let mut contents = new("hello\nworld");
        assert_eq!(flatten(&contents), "hello\nworld");
        contents.change(
            &Range::new(Position::new(0, 3), Position::new(1, 2)),
            &l1("__\n__"),
        );
        assert_eq!(flatten(&contents), "hel__\n__rld");
        assert_eq!(contents.num_lines(), 2);
        assert_eq!(contents.get_line(0).unwrap().to_string(), "hel__\n");
        assert_eq!(contents.get_line(1).unwrap().to_string(), "__rld");
    }

    #[test]
    fn change_to_less_lines() {
        let mut contents = new("hello\nworld");
        assert_eq!(flatten(&contents), "hello\nworld");
        contents.change(
            &Range::new(Position::new(0, 3), Position::new(1, 2)),
            &l1(""),
        );
        assert_eq!(flatten(&contents), "helrld");
        assert_eq!(contents.num_lines(), 1);
        assert_eq!(contents.get_line(0).unwrap().to_string(), "helrld");
    }

    #[test]
    fn change_to_more_lines() {
        let mut contents = new("hello\nworld");
        assert_eq!(flatten(&contents), "hello\nworld");
        contents.change(
            &Range::new(Position::new(0, 3), Position::new(1, 2)),
            &l1("\nmiddle\n"),
        );
        assert_eq!(flatten(&contents), "hel\nmiddle\nrld");
        assert_eq!(contents.num_lines(), 3);
        assert_eq!(contents.get_line(0).unwrap().to_string(), "hel\n");
        assert_eq!(contents.get_line(1).unwrap().to_string(), "middle\n");
        assert_eq!(contents.get_line(2).unwrap().to_string(), "rld");
    }

    #[test]
    fn change_keeps_surrounding_lines() {
        let mut contents = new("___\nhello\nworld\n...");
        assert_eq!(flatten(&contents), "___\nhello\nworld\n...");
        contents.change(
            &Range::new(Position::new(1, 3), Position::new(2, 2)),
            &l1(""),
        );
        assert_eq!(flatten(&contents), "___\nhelrld\n...");
        assert_eq!(contents.num_lines(), 3);
        assert_eq!(contents.get_line(0).unwrap().to_string(), "___\n");
        assert_eq!(contents.get_line(1).unwrap().to_string(), "helrld\n");
        assert_eq!(contents.get_line(2).unwrap().to_string(), "...");
    }

    #[test]
    fn change_empty() {
        let mut contents = new("");
        assert_eq!(flatten(&contents), "");
        contents.change(
            &Range::new(Position::new(0, 0), Position::new(0, 0)),
            &l1("H"),
        );
        assert_eq!(flatten(&contents), "H");
    }

    #[test]
    fn change_to_empty() {
        let mut contents = new("H");
        assert_eq!(flatten(&contents), "H");
        contents.change(
            &Range::new(Position::new(0, 0), Position::new(0, 1)),
            &l1(""),
        );
        assert_eq!(flatten(&contents), "");
    }
}
