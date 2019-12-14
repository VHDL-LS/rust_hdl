// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use crate::diagnostic::{Diagnostic, ParseResult};
use crate::latin_1::{Latin1String, Utf8ToLatin1Error};
use pad;
use std::cmp::{max, min};
use std::collections::hash_map::DefaultHasher;
use std::collections::VecDeque;
use std::convert::AsRef;
use std::fmt;
use std::fmt::Write;
use std::fs::File;
use std::hash::{Hash, Hasher};
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

struct FileId {
    name: String,
    hash: u64, // Hash of name
}

impl FileId {
    fn new(name: impl Into<String>) -> FileId {
        let name = name.into();
        let hash = hash(&name);
        Self { name, hash }
    }
}

impl PartialEq for FileId {
    fn eq(&self, other: &Self) -> bool {
        // Use file name hash to speedup comparison
        if self.hash == other.hash {
            self.name == other.name
        } else {
            false
        }
    }
}

fn hash(value: &str) -> u64 {
    let mut hasher = DefaultHasher::new();
    hasher.write(value.as_bytes());
    hasher.finish()
}

struct UniqueSource {
    file_id: FileId,
    data_provider: Box<dyn SourceDataProvider + Sync + Send>,
}

impl fmt::Debug for UniqueSource {
    /// Custom implementation to avoid large contents strings
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Source {{file_name: {:?}}}", self.file_name())
    }
}

impl UniqueSource {
    fn inline(file_name: impl Into<String>, contents: Arc<Latin1String>) -> Self {
        Self {
            file_id: FileId::new(file_name),
            data_provider: Box::new(InlineSource { contents }),
        }
    }

    fn from_file(file_name: impl Into<String>) -> Self {
        let data_provider = Box::new(SourceFile {
            file_name: file_name.into(),
        });
        Self {
            file_id: FileId::new(data_provider.file_name.clone()),
            data_provider,
        }
    }

    fn contents(&self) -> io::Result<Arc<Latin1String>> {
        self.data_provider.contents()
    }

    fn file_name(&self) -> &str {
        self.file_id.name.as_ref()
    }
}

#[derive(Debug, Clone)]
pub struct Source {
    source: Arc<UniqueSource>,
}

impl PartialEq for Source {
    fn eq(&self, other: &Self) -> bool {
        self.source.file_id == other.source.file_id
    }
}

impl Eq for Source {}

impl Hash for Source {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
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

    pub fn inline_utf8(
        file_name: impl Into<String>,
        contents: &str,
    ) -> Result<Self, Utf8ToLatin1Error> {
        let latin1 = Latin1String::from_utf8(contents)?;
        Ok(Self::inline(file_name, Arc::new(latin1)))
    }

    pub fn contents(&self) -> io::Result<Arc<Latin1String>> {
        self.source.contents()
    }

    pub fn file_name(&self) -> &str {
        self.source.file_name()
    }

    pub fn pos(&self, start: Position, end: Position) -> SrcPos {
        SrcPos {
            source: self.clone(),
            range: Range { start, end },
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash, Debug)]
pub struct Position {
    pub line: u64,
    pub character: u64,
}

impl Position {
    pub fn next_char(&self) -> Position {
        Position {
            line: self.line,
            character: self.character + 1,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Hash, Debug)]
pub struct Range {
    pub start: Position,
    pub end: Position,
}

impl Range {
    pub fn new(start: Position, end: Position) -> Range {
        Range { start, end }
    }
}

/// Lexical position in a file.
#[derive(PartialEq, Clone, Debug, Eq, Hash)]
pub struct SrcPos {
    /// The source
    pub source: Source,
    range: Range,
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
            Err(msg) => Err(Diagnostic::error(&self.pos, msg)),
        }
    }

    pub fn combine_pos_with(self, other: &dyn AsRef<SrcPos>) -> Self {
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
    const LINE_CONTEXT: u64 = 2;

    pub fn new(source: Source, range: Range) -> SrcPos {
        SrcPos { source, range }
    }

    fn get_line_context(
        &self,
        context_lines: u64,
        reader: &mut dyn BufRead,
    ) -> VecDeque<(u64, Latin1String)> {
        let mut lines = VecDeque::new();
        let mut buf = String::new();
        let mut lineno = 0;

        while let Ok(bytes_read) = reader.read_line(&mut buf) {
            if bytes_read == 0 {
                break;
            }

            let line = Latin1String::from_utf8(&buf).unwrap();

            if lineno <= (self.range.end.line + context_lines) {
                if (lineno + context_lines) >= self.range.start.line {
                    lines.push_back((lineno, line.clone()));
                }
            } else {
                break;
            }

            lineno += 1;
            buf.clear();
        }
        if lines.is_empty() {
            lines.push_back((lineno, Latin1String::empty()));
        }
        lines
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
    fn underline(&self, lineno_len: usize, lineno: u64, line: &str, into: &mut String) {
        const NEWLINE_SIZE: usize = 1;
        into.reserve("  |  ".len() + lineno_len + line.len() + NEWLINE_SIZE);

        // Prefix
        for _ in 0..lineno_len {
            into.push(' ');
        }
        into.push_str("  |  ");

        let mut pos = Position {
            line: lineno,
            character: 0,
        };

        // Padding before underline
        for chr in line.chars() {
            if pos < self.range.start {
                Self::push_replicate(into, ' ', Self::visual_width(chr));
            } else if pos < self.range.end {
                Self::push_replicate(into, '~', Self::visual_width(chr));
            } else {
                break;
            }
            pos.character += 1;
        }

        if lineno == self.range.end.line {
            while pos < self.range.end {
                into.push('~');
                pos.character += 1;
            }
        }

        // Newline
        into.push_str("\n");
    }

    fn code_context_from_reader(
        &self,
        reader: &mut dyn BufRead,
        context_lines: u64,
    ) -> (usize, String) {
        let lines = self.get_line_context(context_lines, reader);
        use self::pad::{Alignment, PadStr};
        // +1 since lines are shown with 1-index
        let lineno_len = (self.range.start.line + context_lines + 1)
            .to_string()
            .len();

        let mut result = String::new();

        for (lineno, line) in lines.iter() {
            let line = line.to_string();
            let line = line.trim_matches('\n');
            let lineno_str = (lineno + 1)
                .to_string()
                .pad_to_width_with_alignment(lineno_len, Alignment::Right);
            let overlaps = self.range.start.line <= *lineno && *lineno <= self.range.end.line;

            if overlaps {
                write!(result, "{} --> ", lineno_str).unwrap();
            } else {
                write!(result, "{}  |  ", lineno_str).unwrap();
            }

            for chr in line.trim_end().chars() {
                if chr == '\t' {
                    Self::push_replicate(&mut result, ' ', Self::visual_width(chr));
                } else {
                    result.push(chr);
                }
            }
            result.push('\n');

            if overlaps {
                self.underline(lineno_len, *lineno, line, &mut result);
            }
        }

        (lineno_len, result)
    }

    /// Create a string for pretty printing
    pub fn code_context(&self) -> String {
        self.lineno_len_and_code_context().1
    }

    fn lineno_len_and_code_context(&self) -> (usize, String) {
        let latin1 = self.source.contents().unwrap();
        self.code_context_from_reader(&mut latin1.to_string().as_bytes(), Self::LINE_CONTEXT)
    }

    pub fn show(&self, message: &str) -> String {
        let (lineno_len, pretty_str) = self.lineno_len_and_code_context();
        let file_name = self.source.file_name();
        let mut result = String::new();

        let lineno = self.range.start.line;
        writeln!(result, "{}", &message).unwrap();
        for _ in 0..lineno_len {
            result.push(' ');
        }
        writeln!(result, " --> {}:{}", file_name, lineno + 1).unwrap();
        for _ in 0..lineno_len {
            result.push(' ');
        }
        writeln!(result, "  |").unwrap();
        result.push_str(&pretty_str);
        result
    }

    /// Combines two lexical positions into a larger legical position overlapping both
    /// The file name is assumed to be the same
    pub fn combine_into(self, other: &dyn AsRef<Self>) -> Self {
        let other = other.as_ref();
        debug_assert!(self.source == other.source, "Assumes sources are equal");

        let start = min(self.range.start, other.range.start);
        let end = max(self.range.end, other.range.end);

        SrcPos {
            source: self.source,
            range: Range { start, end },
        }
    }

    pub fn start(&self) -> Position {
        self.range.start
    }

    pub fn end(&self) -> Position {
        self.range.end
    }

    pub fn range(&self) -> Range {
        self.range
    }

    pub fn combine(&self, other: &dyn AsRef<Self>) -> Self {
        self.clone().combine_into(other)
    }
}

pub trait HasSource {
    fn source(&self) -> &Source;
}

impl HasSource for Source {
    fn source(&self) -> &Source {
        &self
    }
}

pub trait HasSrcPos {
    fn pos(&self) -> &SrcPos;
}

impl HasSrcPos for SrcPos {
    fn pos(&self) -> &SrcPos {
        &self
    }
}

impl<T: HasSrcPos> HasSource for T {
    fn source(&self) -> &Source {
        &self.pos().source
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::{Code, CodeBuilder};
    use pretty_assertions::assert_eq;
    use tempfile;

    #[test]
    fn srcpos_combine() {
        let code = Code::new("hello world");

        assert_eq!(
            code.s1("hello").pos().combine(&code.s1("world").pos()),
            code.pos()
        );

        assert_eq!(code.s1("h").pos().combine(&code.s1("d").pos()), code.pos());

        assert_eq!(code.s1("d").pos().combine(&code.s1("h").pos()), code.pos());
    }

    fn with_code_from_file<F, R>(contents: &str, fun: F) -> R
    where
        F: Fn(Code) -> R,
    {
        use std::io::Write;
        let mut file = tempfile::NamedTempFile::new().unwrap();
        let file_name = file.path().to_str().unwrap().to_string();
        file.write(&Latin1String::from_utf8_unchecked(contents).bytes)
            .unwrap();
        fun(CodeBuilder::new().code_from_source(Source::from_file(file_name)))
    }

    #[test]
    fn code_context_pos_from_filename() {
        with_code_from_file("hello\nworld\n", |code: Code| {
            assert_eq!(
                code.s1("hello").pos().code_context(),
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
        let code = Code::new("hello world");
        let pos = code.s1("hello").pos();
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
        let code = Code::new("    hello world");
        let pos = code.s1("hello").pos();
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
        let code = Code::new("h");
        assert_eq!(
            code.eof_pos().code_context(),
            "\
1 --> h
   |   ~
",
        );
    }

    #[test]
    fn code_context_eof_empty() {
        let code = Code::new("");
        assert_eq!(code.eof_pos().code_context(), "1 --> \n   |  ~\n",);
    }

    #[test]
    fn code_context_with_context() {
        let code = Code::new("hello\nworld");
        let pos = code.s1("hello").pos();
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
        let code = Code::new("\thello\t");
        let pos = code.s1("hello\t").pos();
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
        let code = Code::new("åäö\nåäö\n__å_ä_ö__");
        let substr = code.s1("å_ä_ö");
        assert_eq!(substr.length(), 5);
        assert_eq!(
            substr.pos().code_context(),
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
        with_code_from_file("åäö\nåäö\n__å_ä_ö__", |code: Code| {
            let substr = code.s1("å_ä_ö");
            assert_eq!(substr.length(), 5);
            assert_eq!(
                substr.pos().code_context(),
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
        let code = Code::new(
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
        let pos = code.s1("line10").pos();
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
        with_code_from_file("hello\nworld\nline\n", |code: Code| {
            assert_eq!(
                code.s1("world").pos().show("Greetings"),
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
                    code.source().file_name()
                )
            )
        });
    }

    #[test]
    fn show_contents() {
        let code = Code::new("hello\nworld\nline\n");
        assert_eq!(
            code.s1("world").pos().show("Greetings"),
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
                code.source().file_name()
            )
        );
    }
}
