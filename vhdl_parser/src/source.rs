// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

extern crate pad;
use latin_1::Latin1String;
use message::{error, ParseResult};
use std::cmp::{max, min};
use std::collections::VecDeque;
use std::convert::AsRef;
use std::fmt;
use std::fmt::Write;
use std::fs::File;
use std::io::prelude::Read;
use std::io::{BufRead, Error};
use std::sync::Arc;

#[derive(PartialEq, Clone)]
pub enum Source {
    FileName(Arc<String>),
    Contents(Arc<Latin1String>),
}

impl fmt::Debug for Source {
    /// Custom implementation to avoid large Contents strings
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Source::FileName(ref file_name) => {
                write!(f, "Source::FileName({:?})", file_name.as_str())
            }
            Source::Contents(_) => write!(f, "Source::Contents(...)"),
        }
    }
}

impl Source {
    pub fn from_str(contents: &str) -> Result<Source, String> {
        Ok(Source::Contents(Arc::new(Latin1String::from_utf8(
            contents,
        )?)))
    }
    pub fn from_file(file_name: &str) -> Source {
        Source::FileName(Arc::new(file_name.to_string()))
    }

    pub fn contents(self: &Self) -> Result<Arc<Latin1String>, Error> {
        match self {
            Source::FileName(ref file_name) => {
                let mut file = File::open(file_name.as_ref())?;
                let mut bytes = Vec::new();
                file.read_to_end(&mut bytes)?;

                Ok(Arc::new(Latin1String::from_vec(bytes)))
            }
            Source::Contents(ref contents) => Ok(contents.clone()),
        }
    }

    pub fn utf8_contents(&self) -> Result<Arc<String>, Error> {
        let contents = self.contents()?;
        Ok(Arc::new(contents.to_string()))
    }

    pub fn file_name(self: &Self) -> Option<&str> {
        match self {
            Source::FileName(ref file_name) => Some(file_name.as_str()),
            _ => None,
        }
    }

    pub fn pos(self: &Self, start: usize, length: usize) -> SrcPos {
        SrcPos {
            source: self.clone(),
            start,
            length,
        }
    }

    /// Helper method to create a source position from a substring
    #[cfg(test)]
    pub fn substr_pos(self: &Self, substr: &str, occurence: usize) -> SrcPos {
        let substr = Latin1String::from_utf8_unchecked(substr);
        let contents = self.contents().unwrap();
        let mut count = occurence;
        for i in 0..(contents.len() - substr.len() + 1) {
            if &contents.bytes[i..i + substr.len()] == substr.bytes.as_slice() {
                count -= 1;
                if count == 0 {
                    return self.pos(i, substr.len());
                }
            }
        }
        panic!(
            "Could not find occurence {} of substring {:?} in {:?}",
            occurence, substr, contents
        );
    }

    /// First occurence
    #[cfg(test)]
    pub fn first_substr_pos(self: &Self, substr: &str) -> SrcPos {
        self.substr_pos(substr, 1)
    }

    /// Position covers entire contents
    #[cfg(test)]
    pub fn entire_pos(self: &Self) -> SrcPos {
        let contents = self.contents().unwrap().to_string();
        self.pos(0, contents.len())
    }
}

/// Lexical position in a file
#[derive(PartialEq, Clone, Debug)]
pub struct SrcPos {
    /// The source
    pub source: Source,
    /// The start character position
    pub start: usize,
    /// The length of the token in characters
    pub length: usize,
}

#[derive(PartialEq, Clone, Debug)]
pub struct WithPos<T> {
    pub item: T,
    pub pos: SrcPos,
}

impl<T> WithPos<T> {
    pub fn new(item: T, pos: impl AsRef<SrcPos>) -> WithPos<T> {
        WithPos {
            item,
            pos: pos.as_ref().clone(),
        }
    }

    pub fn map_into<F, U>(self, f: F) -> WithPos<U>
    where
        F: FnOnce(T) -> U,
    {
        WithPos {
            item: f(self.item),
            pos: self.pos,
        }
    }
    pub fn try_map_into<F, U>(self, f: F) -> ParseResult<WithPos<U>>
    where
        F: FnOnce(T) -> Result<U, String>,
    {
        match f(self.item) {
            Ok(item) => Ok(WithPos {
                item: item,
                pos: self.pos,
            }),
            Err(msg) => Err(error(&self.pos, &msg)),
        }
    }

    pub fn combine_pos_with(self, other: impl AsRef<SrcPos>) -> Self {
        WithPos {
            item: self.item,
            pos: self.pos.combine(other.as_ref()),
        }
    }
}

impl<T> AsRef<SrcPos> for WithPos<T> {
    fn as_ref(&self) -> &SrcPos {
        &self.pos
    }
}

impl AsRef<SrcPos> for SrcPos {
    fn as_ref(&self) -> &SrcPos {
        self
    }
}

impl SrcPos {
    fn get_line_context(
        self: &Self,
        context_lines: usize,
        reader: &mut BufRead,
    ) -> (usize, VecDeque<(usize, usize, Latin1String)>) {
        let mut first_lineno = None;
        let mut lines = VecDeque::new();
        let mut offset: usize = 0;
        let mut lineno = 1;
        let mut buf = String::new();
        let mut early_eof = false;

        while let Ok(bytes_read) = reader.read_line(&mut buf) {
            let line = Latin1String::from_utf8(&buf).unwrap();

            if bytes_read == 0 {
                early_eof = true;
                break;
            }

            match first_lineno {
                Some(first_lineno) => {
                    if lineno > first_lineno + context_lines {
                        break;
                    }
                }
                None => {
                    if self.overlaps(offset, line.len()) {
                        first_lineno = Some(lineno);
                    } else if lines.len() >= context_lines {
                        lines.pop_front();
                    }
                }
            };

            lines.push_back((lineno, offset, line.clone()));
            offset += line.len();
            lineno += 1;
            buf.clear();
        }

        if early_eof && self.start + self.length > offset {
            if lines.len() > 0 {
                let last_idx = lines.len() - 1;
                let (_, ref offset, ref mut line) = lines.get_mut(last_idx).unwrap();
                let line_len = self.start + self.length - offset;
                for _ in line.len()..line_len {
                    line.bytes.push(b' ');
                }
            } else {
                let line_len = self.start + self.length - offset;
                let mut line = Latin1String::from_vec(Vec::with_capacity(line_len));
                for _ in 0..line_len {
                    line.bytes.push(b' ');
                }
                lines.push_back((lineno, offset, line));
            }
        }

        return (first_lineno.unwrap_or(lineno - 1), lines);
    }

    fn push_replicate(line: &mut String, chr: char, times: usize) {
        for _ in 0..times {
            line.push(chr);
        }
    }

    fn visual_width(chr: char) -> usize {
        if chr == '\t' {
            4
        } else {
            1
        }
    }

    /// Write ~~~ to underline symbol
    fn underline(self: &Self, prefix_len: usize, offset: usize, line: &str, into: &mut String) {
        let start = min(self.start, offset);
        // non-inclusive end
        let end = min(offset + line.len(), self.start + self.length);

        const NEWLINE_SIZE: usize = 1;
        into.reserve(prefix_len + end - start + NEWLINE_SIZE);

        // Prefix
        for _ in 0..prefix_len {
            into.push(' ');
        }

        // Padding before underline
        for (i, chr) in line.chars().enumerate() {
            let idx = offset + i;
            if idx < self.start {
                Self::push_replicate(into, ' ', Self::visual_width(chr));
            } else if idx < end {
                Self::push_replicate(into, '~', Self::visual_width(chr));
            } else {
                break;
            }
        }

        // Newline
        into.push_str("\n");
    }

    /// Check is line at offset overlaps source position
    fn overlaps(self: &Self, offset: usize, line_len: usize) -> bool {
        offset + line_len >= self.start + 1 && offset < self.start + self.length
    }

    fn pretty_string_from_reader(self: &Self, reader: &mut BufRead) -> (usize, String) {
        const LINE_CONTEXT: usize = 2;
        let (first_lineno, lines) = self.get_line_context(LINE_CONTEXT, reader);

        use self::pad::{Alignment, PadStr};

        let last_lineno = {
            match lines.get(lines.len() - 1) {
                Some((ref lineno, _, _)) => lineno.clone(),
                _ => 1,
            }
        };

        let max_len = format!("{}", last_lineno).len();

        let mut result = String::new();

        for (lineno, offset, ref line) in lines {
            let line = line.to_string();
            let line = line.trim_matches('\n');
            let lineno_str = lineno
                .to_string()
                .pad_to_width_with_alignment(max_len, Alignment::Right);

            write!(result, "{}: ", lineno_str);
            for chr in line.trim_right().chars() {
                if chr == '\t' {
                    Self::push_replicate(&mut result, ' ', Self::visual_width(chr));
                } else {
                    result.push(chr);
                }
            }
            result.push('\n');

            if self.overlaps(offset, line.len()) {
                self.underline(2 + max_len, offset, line, &mut result);
            }
        }

        return (first_lineno, result);
    }

    pub fn lineno_and_pretty_string(self: &Self) -> (usize, String) {
        match self.source {
            Source::FileName(ref file_name) => {
                let mut file = File::open(file_name.to_string()).unwrap();
                let mut bytes = Vec::new();
                file.read_to_end(&mut bytes).unwrap();
                let latin1 = Latin1String::from_vec(bytes);
                self.pretty_string_from_reader(&mut latin1.to_string().as_bytes())
            }
            Source::Contents(ref contents) => {
                let utf8_contents = contents.to_string();
                self.pretty_string_from_reader(&mut utf8_contents.as_bytes())
            }
        }
    }

    /// Create a string for pretty printing
    pub fn pretty_string(self: &Self) -> String {
        let (_, pretty_string) = self.lineno_and_pretty_string();
        pretty_string
    }

    /// Combines two lexical positions into a larger legical position overlapping both
    /// The file name is assumed to be the same
    pub fn combine(self: &Self, other: &Self) -> Self {
        debug_assert!(self.source == other.source, "Assumes sources are equal");

        let start = min(self.start, other.start);
        let end = max(self.start + self.length, other.start + other.length);

        SrcPos {
            source: self.source.clone(),
            start,
            length: end - start,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate tempfile;
    use message::{error, warning};

    #[test]
    fn srcpos_combine() {
        let source = Source::from_str("hello world").unwrap();

        assert_eq!(
            source.pos(0, 2).combine(&source.pos(2, 2)),
            source.pos(0, 4)
        );

        assert_eq!(
            source.pos(0, 2).combine(&source.pos(4, 2)),
            source.pos(0, 6)
        );

        assert_eq!(
            source.pos(4, 2).combine(&source.pos(0, 2)),
            source.pos(0, 6)
        );
    }

    fn with_source_from_file<F, R>(contents: &str, fun: F) -> R
    where
        F: Fn(Source) -> R,
    {
        use std::io::Write;
        let mut file = tempfile::NamedTempFile::new().unwrap();
        let file_name = file.path().to_str().unwrap().to_string();
        file.write(&Latin1String::from_utf8_unchecked(contents).bytes)
            .unwrap();
        fun(Source::from_file(&file_name))
    }

    #[test]
    fn pretty_string_pos_from_filename() {
        with_source_from_file("hello\nworld\n", |source: Source| {
            assert_eq!(
                source.first_substr_pos("hello").pretty_string(),
                "\
1: hello
   ~~~~~
2: world
"
            )
        });
    }

    #[test]
    fn pretty_string_pos_last_line_without_newline() {
        let source = Source::from_str("hello world").unwrap();
        let pos = source.first_substr_pos("hello");
        assert_eq!(
            pos.pretty_string(),
            "\
1: hello world
   ~~~~~
"
        );
    }

    #[test]
    fn pretty_string_pos_with_indent() {
        let source = Source::from_str("    hello world").unwrap();
        let pos = source.first_substr_pos("hello");
        assert_eq!(
            pos.pretty_string(),
            "\
1:     hello world
       ~~~~~
"
        );
    }

    #[test]
    fn pretty_string_eof() {
        let source = Source::from_str("h").unwrap();
        let pos = source.pos(1, 1);
        assert_eq!(
            pos.pretty_string(),
            "\
1: h
    ~
",
        );
    }

    #[test]
    fn pretty_string_eof_empty() {
        let source = Source::from_str("").unwrap();
        let pos = source.pos(0, 1);
        assert_eq!(pos.pretty_string(), "1: \n   ~\n",);
    }

    #[test]
    fn pretty_string_with_context() {
        let source = Source::from_str("hello\nworld").unwrap();
        let pos = source.first_substr_pos("hello");
        assert_eq!(
            pos.pretty_string(),
            "\
1: hello
   ~~~~~
2: world
",
        );
    }

    #[test]
    fn pretty_string_with_tabs() {
        let source = Source::from_str("\thello\t").unwrap();
        let pos = source.first_substr_pos("hello\t");
        assert_eq!(
            pos.pretty_string(),
            "\
1:     hello
       ~~~~~~~~~
",
        );
    }

    #[test]
    fn pretty_string_non_ascii() {
        let source = Source::from_str("åäö\nåäö\n__å_ä_ö__").unwrap();
        let pos = source.first_substr_pos("å_ä_ö");
        assert_eq!(pos.length, 5);
        assert_eq!(
            pos.pretty_string(),
            "\
1: åäö
2: åäö
3: __å_ä_ö__
     ~~~~~
",
        );
    }

    #[test]
    fn pretty_string_non_ascii_from_file() {
        with_source_from_file("åäö\nåäö\n__å_ä_ö__", |source: Source| {
            let pos = source.first_substr_pos("å_ä_ö");
            assert_eq!(pos.length, 5);
            assert_eq!(
                pos.pretty_string(),
                "\
1: åäö
2: åäö
3: __å_ä_ö__
     ~~~~~
",
            );
        });
    }

    #[test]
    fn pretty_string_with_full_context() {
        let source = Source::from_str(
            "\
line1
line2
line3
line4
line5
line6
line7
line8
line9
line10
line11
line12
line13",
        ).unwrap();
        let pos = source.first_substr_pos("line10");
        assert_eq!(
            pos.pretty_string(),
            " \
 8: line8
 9: line9
10: line10
    ~~~~~~
11: line11
12: line12
",
        );
    }

    #[test]
    fn message_pretty_string_from_filename() {
        with_source_from_file("hello\nworld\nline\n", |source: Source| {
            assert_eq!(
                error(&source.first_substr_pos("world"), "Greetings").pretty_string(),
                format!(
                    "\
{}:2: error: Greetings
1: hello
2: world
   ~~~~~
3: line
",
                    source.file_name().unwrap()
                )
            )
        });
    }

    #[test]
    fn message_pretty_string_contents() {
        let source = Source::from_str("hello\nworld\nline\n").unwrap();
        assert_eq!(
            error(&source.first_substr_pos("world"), "Greetings").pretty_string(),
            "\
<unknown file>:2: error: Greetings
1: hello
2: world
   ~~~~~
3: line
"
        );
    }

    #[test]
    fn message_warning_pretty_string() {
        let source = Source::from_str("hello\nworld\nline\n").unwrap();
        assert_eq!(
            warning(&source.first_substr_pos("world"), "Greetings").pretty_string(),
            "\
<unknown file>:2: warning: Greetings
1: hello
2: world
   ~~~~~
3: line
"
        );
    }
}
