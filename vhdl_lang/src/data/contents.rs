// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

use super::latin_1::{char_to_latin1, Latin1String, Utf8ToLatin1Error};
use super::source::{Position, Range};
use std::fs::File;
use std::io;
use std::io::prelude::Read;
use std::path::Path;

pub struct Contents {
    lines: Vec<String>,
}

impl Contents {
    pub fn from_latin1_file(file_name: &Path) -> io::Result<Contents> {
        let mut file = File::open(file_name)?;
        let mut bytes = Vec::new();
        file.read_to_end(&mut bytes)?;
        Ok(Contents::from_str(
            &Latin1String::from_vec(bytes).to_string(),
        ))
    }

    pub fn from_str(code: &str) -> Contents {
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

    pub fn end(&self) -> Position {
        let line = self.num_lines().saturating_sub(1) as u32;
        let character = self
            .lines
            .last()
            .map(|line| line.chars().map(|chr| chr.len_utf16()).sum())
            .unwrap_or(0) as u32;
        Position { line, character }
    }

    #[cfg(test)]
    pub fn range(&self) -> Range {
        Range::new(self.start(), self.end())
    }

    #[cfg(test)]
    pub fn crop(&self, range: Range) -> Contents {
        let mut reader = ContentReader::new(self);
        reader.seek_pos(range.start);

        let mut result = String::new();
        while reader.pos() < range.end {
            if let Some(chr) = reader.pop_char() {
                result.push(chr);
            }
        }

        Contents {
            lines: split_lines(&result),
        }
    }

    pub fn num_lines(&self) -> usize {
        self.lines.len()
    }

    pub fn get_line(&self, lineno: usize) -> Option<&str> {
        self.lines.get(lineno).map(|string| string.as_str())
    }

    pub fn change(&mut self, range: &Range, content: &str) {
        if self.lines.is_empty() {
            self.lines = split_lines(content);
            return;
        }

        let Range { start, end } = range;

        let start_char = start.character as usize;
        let end_char = end.character as usize;
        let start_line = start.line as usize;
        let end_line = end.line as usize;
        let mut merged_content = String::new();

        if let Some(line) = self.lines.get(start_line) {
            let mut i = 0;
            for chr in line.chars() {
                if i < start_char {
                    merged_content.push(chr);
                } else {
                    break;
                };
                i += chr.len_utf16();
            }
        }
        merged_content.push_str(content);

        if let Some(line) = self.lines.get(end_line) {
            let mut i = 0;
            for chr in line.chars() {
                if i >= end_char {
                    merged_content.push(chr);
                };
                i += chr.len_utf16();
            }
        }

        let end_line = std::cmp::min(self.lines.len().saturating_sub(1), end_line);
        self.lines
            .splice(
                start_line..=end_line,
                split_lines(&merged_content).into_iter(),
            )
            .count();
    }
}

/// Split code into several lines
fn split_lines(code: &str) -> Vec<String> {
    let mut lines = Vec::new();
    let bytes = code.as_bytes();

    let mut i = 0;
    let mut start = 0;
    while i < bytes.len() {
        let byte = bytes[i];

        if byte == b'\n' {
            i += 1;
            let line = bytes[start..i].to_owned();
            let line = unsafe { String::from_utf8_unchecked(line) };
            lines.push(line);
            start = i;
        } else if byte == b'\r' {
            i += 1;
            let mut line = bytes[start..i].to_owned();
            let last = line.len().saturating_sub(1);
            line[last] = b'\n';
            let line = unsafe { String::from_utf8_unchecked(line) };
            lines.push(line);

            if bytes.get(i) == Some(&b'\n') {
                i += 1;
            }

            start = i;
        } else {
            i += 1;
        }
    }

    if start < bytes.len() {
        let bytes = bytes[start..].to_owned();
        let line = unsafe { String::from_utf8_unchecked(bytes) };
        lines.push(line);
    }
    lines
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub struct ReaderState {
    pos: Position,
    idx: usize, // Byte offset in line
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
                idx: 0,
            },
        }
    }

    #[must_use]
    pub fn get_char(&self) -> Option<char> {
        if let Some(line) = self.contents.get_line(self.state.pos.line as usize) {
            let bytes = line.as_bytes();
            let idx = self.state.idx;
            if idx < bytes.len() {
                let slice = unsafe { std::str::from_utf8_unchecked(&bytes[idx..]) };
                slice.chars().next()
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn pop(&mut self) -> Result<Option<u8>, Utf8ToLatin1Error> {
        if let Some(chr) = self.get_char() {
            if let Some(latin1) = char_to_latin1(chr) {
                self.skip_char(chr);
                Ok(Some(latin1))
            } else {
                let pos = self.pos();
                self.skip_char(chr);
                Err(Utf8ToLatin1Error { pos, value: chr })
            }
        } else {
            Ok(None)
        }
    }

    pub fn get(&self) -> Result<Option<u8>, Utf8ToLatin1Error> {
        if let Some(chr) = self.get_char() {
            if let Some(latin1) = char_to_latin1(chr) {
                Ok(Some(latin1))
            } else {
                Err(Utf8ToLatin1Error {
                    pos: self.pos(),
                    value: chr,
                })
            }
        } else {
            Ok(None)
        }
    }

    #[must_use]
    pub fn pop_char(&mut self) -> Option<char> {
        let chr = self.get_char()?;
        self.skip_char(chr);
        Some(chr)
    }

    fn skip_char(&mut self, chr: char) {
        self.state.pos.move_after_char(chr);
        if self.state.pos.character == 0 {
            self.state.idx = 0;
        } else {
            self.state.idx += chr.len_utf8();
        }
    }

    pub fn skip(&mut self) {
        let _ = self.pop_char();
    }

    pub fn pop_lowercase(&mut self) -> Result<Option<u8>, Utf8ToLatin1Error> {
        Ok(self.pop()?.map(Latin1String::lowercase))
    }

    pub fn peek_lowercase(&mut self) -> Result<Option<u8>, Utf8ToLatin1Error> {
        Ok(self.peek()?.map(Latin1String::lowercase))
    }

    #[cfg(test)]
    pub fn matches(&mut self, substr: &str) -> bool {
        let mut lookahead = self.clone();
        for exp in substr.chars() {
            if let Some(chr) = lookahead.pop_char() {
                if chr != exp {
                    return false;
                }
            } else {
                return false;
            }
        }
        true
    }

    pub fn skip_if(&mut self, value: u8) -> Result<bool, Utf8ToLatin1Error> {
        if self.peek()? == Some(value) {
            self.skip();
            Ok(true)
        } else {
            Ok(false)
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
        self.state = ReaderState {
            pos: Position {
                line: pos.line,
                character: 0,
            },
            idx: 0,
        };
        while self.pos() < pos {
            self.skip();
        }
        assert_eq!(self.pos(), pos);
    }

    pub fn state(&self) -> ReaderState {
        self.state
    }

    pub fn peek(&self) -> Result<Option<u8>, Utf8ToLatin1Error> {
        self.get()
    }

    pub fn peek_char(&self) -> Option<char> {
        self.get_char()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn new(code: &str) -> Contents {
        Contents::from_str(code)
    }

    fn reader(contents: &Contents) -> ContentReader {
        ContentReader::new(contents)
    }

    #[test]
    fn pop_latin1_ok() {
        let contents = new("hi");
        let mut reader = reader(&contents);
        assert_eq!(reader.pop(), Ok(Some(b'h')));
        assert_eq!(reader.pop(), Ok(Some(b'i')));
        assert_eq!(reader.pop(), Ok(None));
    }

    #[test]
    fn pop_latin1_err() {
        let contents = new("h€i");
        let mut reader = reader(&contents);
        assert_eq!(reader.pop(), Ok(Some(b'h')));
        assert_eq!(
            reader.pop(),
            Err(Utf8ToLatin1Error {
                pos: Position::new(0, 1),
                value: '€'
            })
        );
        assert_eq!(reader.pop(), Ok(Some(b'i')));
        assert_eq!(reader.pop(), Ok(None));
    }

    #[test]
    fn pop_single_line() {
        let contents = new("hi");
        let mut reader = reader(&contents);
        assert_eq!(reader.pop_char(), Some('h'));
        assert_eq!(reader.pop_char(), Some('i'));
        assert_eq!(reader.pop_char(), None);
    }

    #[test]
    fn pop_char() {
        let contents = new("hå");
        let mut reader = reader(&contents);
        assert_eq!(reader.pop_char(), Some('h'));
        assert_eq!(reader.pop_char(), Some('å'));
        assert_eq!(reader.pop_char(), None);
    }

    #[test]
    fn pop_multi_line_no_newline_at_end() {
        let contents = new("h\ni");
        let mut reader = reader(&contents);
        assert_eq!(reader.pop_char(), Some('h'));
        assert_eq!(reader.pop_char(), Some('\n'));
        assert_eq!(reader.pop_char(), Some('i'));
        assert_eq!(reader.pop_char(), None);
    }

    #[test]
    fn pop_multi_line() {
        let contents = new("h\ni\n");
        let mut reader = reader(&contents);
        assert_eq!(reader.pop_char(), Some('h'));
        assert_eq!(reader.pop_char(), Some('\n'));
        assert_eq!(reader.pop_char(), Some('i'));
        assert_eq!(reader.pop_char(), Some('\n'));
        assert_eq!(reader.pop_char(), None);
    }

    #[test]
    fn empty_lines() {
        let contents = new("\n\n\n");
        let mut reader = reader(&contents);
        assert_eq!(reader.pop_char(), Some('\n'));
        assert_eq!(reader.pop_char(), Some('\n'));
        assert_eq!(reader.pop_char(), Some('\n'));
    }

    #[test]
    fn peek() {
        let contents = new("hi");
        let mut reader = reader(&contents);
        assert_eq!(reader.peek_char(), Some('h'));
        assert_eq!(reader.pop_char(), Some('h'));
        assert_eq!(reader.peek_char(), Some('i'));
        assert_eq!(reader.pop_char(), Some('i'));
        assert_eq!(reader.peek_char(), None);
        assert_eq!(reader.pop_char(), None);
    }

    #[test]
    fn cr_is_removed() {
        let contents = new("1\r2\r\n");
        let mut reader = reader(&contents);
        assert_eq!(reader.pop_char(), Some('1'));
        assert_eq!(reader.pop_char(), Some('\n'));
        assert_eq!(reader.pop_char(), Some('2'));
        assert_eq!(reader.pop_char(), Some('\n'));
        assert_eq!(reader.pop_char(), None);
    }

    #[test]
    fn matches() {
        let contents = new("abc");
        let mut reader = reader(&contents);
        assert!(reader.matches("abc"));
        assert!(!reader.matches("bc"));
        reader.skip();
        assert!(reader.matches("bc"));
    }

    #[test]
    fn character_is_utf16_len() {
        // Bomb emojii requires 2 utf-16 codes
        let bomb = '\u{1F4A3}';
        let contents = new(&format!("aä{bomb}"));
        assert_eq!(contents.end(), Position::new(0, 4));
        let mut reader = reader(&contents);
        assert_eq!(reader.pop_char(), Some('a'));
        assert_eq!(reader.pos(), Position::new(0, 1));
        assert_eq!(reader.pop_char(), Some('ä'));
        assert_eq!(reader.pos(), Position::new(0, 2));
        assert_eq!(reader.pop_char(), Some(bomb));
        assert_eq!(reader.pos(), Position::new(0, 4));
    }

    fn flatten(contents: &Contents) -> String {
        let mut result = String::new();
        for line in contents.lines.iter() {
            result.push_str(line);
        }
        result
    }

    #[test]
    fn change_first() {
        let mut contents = new("hello");
        assert_eq!(flatten(&contents), "hello");
        contents.change(&Range::new(Position::new(0, 0), Position::new(0, 1)), "_");
        assert_eq!(flatten(&contents), "_ello");
    }

    #[test]
    fn change_last() {
        let mut contents = new("hello");
        assert_eq!(flatten(&contents), "hello");
        contents.change(&Range::new(Position::new(0, 4), Position::new(0, 5)), "_");
        assert_eq!(flatten(&contents), "hell_");
    }

    #[test]
    fn change_middle() {
        let mut contents = new("hello");
        assert_eq!(flatten(&contents), "hello");
        contents.change(&Range::new(Position::new(0, 2), Position::new(0, 4)), "__");
        assert_eq!(flatten(&contents), "he__o");
    }

    #[test]
    fn change_shrink() {
        let mut contents = new("hello");
        assert_eq!(flatten(&contents), "hello");
        contents.change(&Range::new(Position::new(0, 2), Position::new(0, 4)), "_");
        assert_eq!(flatten(&contents), "he_o");
    }

    #[test]
    fn change_grow() {
        let mut contents = new("hello");
        assert_eq!(flatten(&contents), "hello");
        contents.change(&Range::new(Position::new(0, 2), Position::new(0, 4)), "___");
        assert_eq!(flatten(&contents), "he___o");
    }

    #[test]
    fn change_multi_line() {
        let mut contents = new("hello\nworld");
        assert_eq!(flatten(&contents), "hello\nworld");
        contents.change(
            &Range::new(Position::new(0, 3), Position::new(1, 2)),
            "__\n__",
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
        contents.change(&Range::new(Position::new(0, 3), Position::new(1, 2)), "");
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
            "\nmiddle\n",
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
        contents.change(&Range::new(Position::new(1, 3), Position::new(2, 2)), "");
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
        contents.change(&Range::new(Position::new(0, 0), Position::new(0, 0)), "H");
        assert_eq!(flatten(&contents), "H");
    }

    #[test]
    fn change_to_empty() {
        let mut contents = new("H");
        assert_eq!(flatten(&contents), "H");
        contents.change(&Range::new(Position::new(0, 0), Position::new(0, 1)), "");
        assert_eq!(flatten(&contents), "");
    }

    #[test]
    fn change_add_missing_newline() {
        // LSP client will use end line outside of text
        let mut contents = new("a");
        assert_eq!(flatten(&contents), "a");
        contents.change(&Range::new(Position::new(0, 1), Position::new(1, 0)), "\n");
        assert_eq!(flatten(&contents), "a\n");
        assert_eq!(contents.num_lines(), 1);
        assert_eq!(contents.get_line(0).unwrap().to_string(), "a\n");
    }
}
