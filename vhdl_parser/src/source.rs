// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

extern crate pad;
use crate::latin_1::Latin1String;
use crate::message::{Message, ParseResult};
use std::cmp::{max, min};
use std::collections::VecDeque;
use std::convert::AsRef;
use std::fmt;
use std::fmt::Write;
use std::fs::File;
use std::io;
use std::io::prelude::Read;
use std::io::BufRead;
use std::sync::Arc;

trait SourceDataProvider {
    fn contents(&self) -> io::Result<Arc<Latin1String>>;
}

struct SourceFile {
    file_name: String,
}

struct InlineSource {
    contents: Arc<Latin1String>,
}

impl SourceDataProvider for SourceFile {
    fn contents(&self) -> io::Result<Arc<Latin1String>> {
        let mut file = File::open(&self.file_name)?;
        let mut bytes = Vec::new();
        file.read_to_end(&mut bytes)?;
        Ok(Arc::new(Latin1String::from_vec(bytes)))
    }
}

impl SourceDataProvider for InlineSource {
    fn contents(&self) -> io::Result<Arc<Latin1String>> {
        Ok(self.contents.clone())
    }
}

struct UniqueSource {
    file_name: String,
    data_provider: Box<dyn SourceDataProvider + Sync + Send>,
}

impl fmt::Debug for UniqueSource {
    /// Custom implementation to avoid large contents strings
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Source {{file_name: {:?}}}", self.file_name.as_str())
    }
}

impl UniqueSource {
    fn inline(file_name: impl Into<String>, contents: Arc<Latin1String>) -> Self {
        Self {
            file_name: file_name.into(),
            data_provider: Box::new(InlineSource { contents }),
        }
    }

    fn from_file(file_name: impl Into<String>) -> Self {
        let file_name = file_name.into();
        let data_provider = Box::new(SourceFile {
            file_name: file_name.to_string(),
        });
        Self {
            file_name,
            data_provider,
        }
    }

    fn contents(&self) -> io::Result<Arc<Latin1String>> {
        self.data_provider.contents()
    }

    fn file_name(&self) -> &str {
        self.file_name.as_ref()
    }
}

#[derive(Debug, Clone)]
pub struct Source {
    source: Arc<UniqueSource>,
}

impl PartialEq for Source {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.source, &other.source)
    }
}

impl Eq for Source {}

impl std::hash::Hash for Source {
    fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
        Arc::into_raw(self.source.clone()).hash(hasher);
    }
}

impl Source {
    pub fn inline(file_name: impl Into<String>, contents: Arc<Latin1String>) -> Source {
        Source {
            source: Arc::new(UniqueSource::inline(file_name, contents)),
        }
    }

    pub fn from_file(file_name: impl Into<String>) -> Source {
        Source {
            source: Arc::new(UniqueSource::from_file(file_name)),
        }
    }

    pub fn inline_utf8(file_name: impl Into<String>, contents: &str) -> io::Result<Self> {
        let latin1 = Latin1String::from_utf8(contents)
            .map_err(|msg| io::Error::new(io::ErrorKind::Other, msg))?;
        Ok(Self::inline(file_name, Arc::new(latin1)))
    }

    #[cfg(test)]
    pub fn from_str(contents: &str) -> Self {
        Self::inline_utf8("{unknown file}", contents).unwrap()
    }

    pub fn contents(&self) -> io::Result<Arc<Latin1String>> {
        self.source.contents()
    }

    pub fn file_name(&self) -> &str {
        self.source.file_name()
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
        self.entire_pos().substr_pos(&self, substr, occurence)
    }

    /// First occurence
    #[cfg(test)]
    pub fn first_substr_pos(self: &Self, substr: &str) -> SrcPos {
        self.substr_pos(substr, 1)
    }

    /// Position covers entire contents
    #[cfg(test)]
    pub fn entire_pos(self: &Self) -> SrcPos {
        let length = self.contents().unwrap().bytes.len();
        self.pos(0, length)
    }
}

/// Lexical position in a file
#[derive(PartialEq, Clone, Debug, Eq, Hash)]
pub struct SrcPos {
    /// The source
    pub source: Source,
    /// The start character position
    pub start: usize,
    /// The length of the token in characters
    pub length: usize,
}

impl SrcPos {
    #[cfg(test)]
    pub fn substr_pos(&self, source: &Source, substr: &str, occurence: usize) -> SrcPos {
        let substr = Latin1String::from_utf8_unchecked(substr);
        let contents = source.contents().unwrap();
        let mut count = occurence;

        if self.length < substr.len() {
            let code = Latin1String::new(&contents.bytes[self.start..self.start + self.length]);
            panic!(
                "Substring {:?} is longer than code {:?}",
                substr,
                &code.to_string()
            )
        }

        let start = self.start;
        let end = self.start + self.length - substr.len() + 1;
        for i in start..end {
            if &contents.bytes[i..i + substr.len()] == substr.bytes.as_slice() {
                count -= 1;
                if count == 0 {
                    return self.source.pos(i, substr.len());
                }
            }
        }
        panic!(
            "Could not find occurence {} of substring {:?} in {:?}",
            occurence, substr, contents
        );
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct WithPos<T> {
    pub item: T,
    pub pos: SrcPos,
}

impl<T> WithPos<T> {
    // Avoid clone in production code
    #[cfg(test)]
    pub fn new(item: T, pos: impl AsRef<SrcPos>) -> WithPos<T> {
        WithPos {
            item,
            pos: pos.as_ref().clone(),
        }
    }

    pub fn from(item: T, pos: impl Into<SrcPos>) -> WithPos<T> {
        WithPos {
            item,
            pos: pos.into(),
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
                item,
                pos: self.pos,
            }),
            Err(msg) => Err(Message::error(&self.pos, msg)),
        }
    }

    pub fn combine_pos_with(self, other: &AsRef<SrcPos>) -> Self {
        WithPos {
            item: self.item,
            pos: self.pos.combine_into(other.as_ref()),
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

impl<T> Into<SrcPos> for WithPos<T> {
    fn into(self) -> SrcPos {
        self.pos
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
            if !lines.is_empty() {
                let last_idx = lines.len() - 1;
                let (_, ref offset, ref mut line) = &mut lines[last_idx];
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

        (first_lineno.unwrap_or(lineno - 1), lines)
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
    fn underline(self: &Self, lineno_len: usize, offset: usize, line: &str, into: &mut String) {
        let start = min(self.start, offset);
        // non-inclusive end
        let end = min(offset + line.len(), self.start + self.length);

        const NEWLINE_SIZE: usize = 1;
        into.reserve(5 + lineno_len + end - start + NEWLINE_SIZE);

        // Prefix
        for _ in 0..lineno_len {
            into.push(' ');
        }
        into.push_str("  |  ");

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

    fn code_context_from_reader(self: &Self, reader: &mut BufRead) -> (usize, usize, String) {
        const LINE_CONTEXT: usize = 2;
        let (first_lineno, lines) = self.get_line_context(LINE_CONTEXT, reader);

        use self::pad::{Alignment, PadStr};

        let last_lineno = {
            match lines.get(lines.len() - 1) {
                Some((ref lineno, _, _)) => *lineno,
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
            let overlaps = self.overlaps(offset, line.len());

            if overlaps {
                write!(result, "{} --> ", lineno_str).unwrap();
            } else {
                write!(result, "{}  |  ", lineno_str).unwrap();
            }

            for chr in line.trim_right().chars() {
                if chr == '\t' {
                    Self::push_replicate(&mut result, ' ', Self::visual_width(chr));
                } else {
                    result.push(chr);
                }
            }
            result.push('\n');

            if overlaps {
                self.underline(max_len, offset, line, &mut result);
            }
        }

        (first_lineno, max_len, result)
    }

    fn lineno_and_code_context(&self) -> (usize, usize, String) {
        // @TODO handle errors
        let latin1 = self.source.contents().unwrap();
        self.code_context_from_reader(&mut latin1.to_string().as_bytes())
    }

    /// Create a string for pretty printing
    pub fn code_context(self: &Self) -> String {
        let (_, _, code_context) = self.lineno_and_code_context();
        code_context
    }

    pub fn show(&self, message: &str) -> String {
        let (lineno, lineno_len, pretty_str) = self.lineno_and_code_context();
        let file_name = self.source.file_name();
        let mut result = String::new();
        writeln!(result, "{}", &message).unwrap();
        for _ in 0..lineno_len {
            result.push(' ');
        }
        writeln!(result, " --> {}:{}", file_name, lineno).unwrap();
        for _ in 0..lineno_len {
            result.push(' ');
        }
        writeln!(result, "  |").unwrap();
        result.push_str(&pretty_str);
        result
    }

    /// Combines two lexical positions into a larger legical position overlapping both
    /// The file name is assumed to be the same
    pub fn combine_into(self, other: &AsRef<Self>) -> Self {
        let other = other.as_ref();
        debug_assert!(self.source == other.source, "Assumes sources are equal");

        let start = min(self.start, other.start);
        let end = max(self.start + self.length, other.start + other.length);

        SrcPos {
            source: self.source,
            start,
            length: end - start,
        }
    }

    pub fn combine(&self, other: &AsRef<Self>) -> Self {
        self.clone().combine_into(other)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate tempfile;

    #[test]
    fn srcpos_combine() {
        let source = Source::from_str("hello world");

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
        fun(Source::from_file(file_name))
    }

    #[test]
    fn code_context_pos_from_filename() {
        with_source_from_file("hello\nworld\n", |source: Source| {
            assert_eq!(
                source.first_substr_pos("hello").code_context(),
                "\
1 --> hello
   |  ~~~~~
2  |  world
"
            )
        });
    }

    #[test]
    fn code_context_pos_last_line_without_newline() {
        let source = Source::from_str("hello world");
        let pos = source.first_substr_pos("hello");
        assert_eq!(
            pos.code_context(),
            "\
1 --> hello world
   |  ~~~~~
"
        );
    }

    #[test]
    fn code_context_pos_with_indent() {
        let source = Source::from_str("    hello world");
        let pos = source.first_substr_pos("hello");
        assert_eq!(
            pos.code_context(),
            "\
1 -->     hello world
   |      ~~~~~
"
        );
    }

    #[test]
    fn code_context_eof() {
        let source = Source::from_str("h");
        let pos = source.pos(1, 1);
        assert_eq!(
            pos.code_context(),
            "\
1 --> h
   |   ~
",
        );
    }

    #[test]
    fn code_context_eof_empty() {
        let source = Source::from_str("");
        let pos = source.pos(0, 1);
        assert_eq!(pos.code_context(), "1 --> \n   |  ~\n",);
    }

    #[test]
    fn code_context_with_context() {
        let source = Source::from_str("hello\nworld");
        let pos = source.first_substr_pos("hello");
        assert_eq!(
            pos.code_context(),
            "\
1 --> hello
   |  ~~~~~
2  |  world
",
        );
    }

    #[test]
    fn code_context_with_tabs() {
        let source = Source::from_str("\thello\t");
        let pos = source.first_substr_pos("hello\t");
        assert_eq!(
            pos.code_context(),
            "\
1 -->     hello
   |      ~~~~~~~~~
",
        );
    }

    #[test]
    fn code_context_non_ascii() {
        let source = Source::from_str("åäö\nåäö\n__å_ä_ö__");
        let pos = source.first_substr_pos("å_ä_ö");
        assert_eq!(pos.length, 5);
        assert_eq!(
            pos.code_context(),
            "\
1  |  åäö
2  |  åäö
3 --> __å_ä_ö__
   |    ~~~~~
",
        );
    }

    #[test]
    fn code_context_non_ascii_from_file() {
        with_source_from_file("åäö\nåäö\n__å_ä_ö__", |source: Source| {
            let pos = source.first_substr_pos("å_ä_ö");
            assert_eq!(pos.length, 5);
            assert_eq!(
                pos.code_context(),
                "\
1  |  åäö
2  |  åäö
3 --> __å_ä_ö__
   |    ~~~~~
",
            );
        });
    }

    #[test]
    fn code_context_with_full_context() {
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
        );
        let pos = source.first_substr_pos("line10");
        assert_eq!(
            pos.code_context(),
            " \
 8  |  line8
 9  |  line9
10 --> line10
    |  ~~~~~~
11  |  line11
12  |  line12
",
        );
    }

    #[test]
    fn show_from_filename() {
        with_source_from_file("hello\nworld\nline\n", |source: Source| {
            assert_eq!(
                source.first_substr_pos("world").show("Greetings"),
                format!(
                    "\
Greetings
  --> {}:2
   |
1  |  hello
2 --> world
   |  ~~~~~
3  |  line
",
                    source.file_name()
                )
            )
        });
    }

    #[test]
    fn show_contents() {
        let source = Source::from_str("hello\nworld\nline\n");
        assert_eq!(
            &source.first_substr_pos("world").show("Greetings"),
            "\
Greetings
  --> {unknown file}:2
   |
1  |  hello
2 --> world
   |  ~~~~~
3  |  line
"
        );
    }
}
