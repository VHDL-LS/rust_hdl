// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2026, Lukas Scheller lukasscheller@icloud.com

// TODO: This whole mod must be revisited when thinking about LSP.

use std::{collections::HashMap, str::Utf8Error};

use crate::{
    fmt::encoding::{Encoder, Utf8Encoder},
    latin_1::Latin1Str,
    syntax::node::SyntaxNode,
    tokens::TriviaPiece,
};

pub trait CharInfo {
    fn is_newline(&self) -> bool;

    fn len_bytes(&self) -> usize;
}

impl CharInfo for char {
    fn is_newline(&self) -> bool {
        self == &'\n'
    }

    fn len_bytes(&self) -> usize {
        self.len_utf8()
    }
}

impl CharInfo for u8 {
    fn is_newline(&self) -> bool {
        self == &b'\n'
    }

    fn len_bytes(&self) -> usize {
        1
    }
}

pub trait CharIter {
    type Char: CharInfo;

    fn enumerate_chars(&self) -> impl Iterator<Item = (usize, Self::Char)>;
}

impl<T> CharIter for T
where
    T: AsRef<str>,
{
    type Char = char;

    fn enumerate_chars(&self) -> impl Iterator<Item = (usize, Self::Char)> {
        self.as_ref().char_indices()
    }
}

impl CharIter for &Latin1Str {
    type Char = u8;

    fn enumerate_chars(&self) -> impl Iterator<Item = (usize, Self::Char)> {
        self.as_bytes().iter().copied().enumerate()
    }
}

pub struct SourceLoc {
    /// 1-based line offset
    pub line: usize,
    /// 1-based column offset
    pub column: usize,
}

struct WideChar {
    start: usize,
    len: usize,
}

pub struct SourceLocConverter {
    /// Byte offsets of the line start of each line
    line_starts: Vec<usize>,
    /// Lines with wide chars (chars that are longer than one byte)
    wide_char_lines: HashMap<usize, Vec<WideChar>>,
    total_len: usize,
}

impl SourceLocConverter {
    pub fn new<E>(root: &SyntaxNode) -> Result<SourceLocConverter, E::Err>
    where
        E: Encoder,
        for<'a> E::Str<'a>: CharIter,
    {
        let mut line_starts = vec![0usize];
        let mut wide_char_lines = HashMap::new();
        let mut cursor = 0usize;

        let mut tok = root.first_token();
        while let Some(t) = tok {
            for piece in t.leading_trivia() {
                record_piece::<E>(piece, cursor, &mut line_starts, &mut wide_char_lines)?;
                cursor += piece.byte_len();
            }
            cursor += t.text().len();
            tok = t.next_token();
        }

        Ok(SourceLocConverter {
            line_starts,
            total_len: cursor,
            wide_char_lines,
        })
    }

    pub fn location(&self, offset: usize) -> SourceLoc {
        let offset = offset.min(self.total_len);
        let i = match self.line_starts.binary_search(&offset) {
            Ok(i) => i,
            Err(i) => i - 1,
        };
        let line_start = self.line_starts[i];

        // Char column = bytes from line_start to offset, adjusted for any
        // wide characters in that prefix. Lines with no wide chars miss the
        // map lookup entirely and fall through with no adjustment.
        let mut effective_offset = offset;
        let mut wide_adjustment = 0usize;
        if let Some(wides) = self.wide_char_lines.get(&i) {
            for wide in wides {
                if wide.start >= offset {
                    break;
                }
                let wide_end = wide.start + wide.len;
                if wide_end <= offset {
                    wide_adjustment += wide.len - 1;
                } else {
                    // offset falls inside a multi-byte character; clamp to
                    // the character's start, matching LSP convention.
                    effective_offset = wide.start;
                    break;
                }
            }
        }

        SourceLoc {
            line: i + 1,
            column: (effective_offset - line_start) - wide_adjustment + 1,
        }
    }

    pub fn line_count(&self) -> usize {
        self.line_starts.len()
    }
}

impl SourceLocConverter {
    pub fn new_utf8(root: &SyntaxNode) -> Result<SourceLocConverter, Utf8Error> {
        SourceLocConverter::new::<Utf8Encoder>(root)
    }
}

fn record_piece<E>(
    piece: &TriviaPiece,
    cursor: usize,
    line_starts: &mut Vec<usize>,
    wide_char_lines: &mut HashMap<usize, Vec<WideChar>>,
) -> Result<(), E::Err>
where
    E: Encoder,
    for<'a> E::Str<'a>: CharIter,
{
    match piece {
        TriviaPiece::LineFeeds(n)
        | TriviaPiece::CarriageReturns(n)
        | TriviaPiece::FormFeeds(n)
        | TriviaPiece::VerticalTabs(n) => {
            for i in 0..*n {
                line_starts.push(cursor + i + 1);
            }
        }
        TriviaPiece::CarriageReturnLineFeeds(n) => {
            for i in 0..*n {
                line_starts.push(cursor + (i + 1) * 2);
            }
        }
        // Note: In theory, `LineComment`s don't need the special newline handling, it's just included here for simplicity.
        // If performance ever becomes a bottleneck, this can be split.
        TriviaPiece::BlockComment(c) | TriviaPiece::LineComment(c) => {
            let str = c.encode::<E>()?;
            let body_start = cursor + 2; // both LineComment ("--") and BlockComment ("/*") have a 2-byte prefix
            let mut line = line_starts.len() - 1;
            for (char_index, ch) in str.enumerate_chars() {
                let char_width = ch.len_bytes();
                let abs = body_start + char_index;
                if ch.is_newline() {
                    line_starts.push(abs + char_width);
                    line += 1;
                }
                if char_width != 1 {
                    wide_char_lines.entry(line).or_default().push(WideChar {
                        start: abs,
                        len: char_width,
                    });
                }
            }
        }
        // TODO: Need to deal with Unexpected(Vec<u8>)?
        _ => {}
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{parser::parse, syntax::AstNode};

    fn converter(source: &str) -> SourceLocConverter {
        let (design, _) = parse(source);
        SourceLocConverter::new_utf8(&design.raw()).expect("valid utf-8")
    }

    #[test]
    fn first_line_first_column() {
        let c = converter("entity foo is end foo;");
        let loc = c.location(0);
        assert_eq!((loc.line, loc.column), (1, 1));
    }

    #[test]
    fn column_advances_by_byte_in_ascii() {
        let c = converter("entity foo is end foo;");
        let loc = c.location(7);
        assert_eq!((loc.line, loc.column), (1, 8));
    }

    #[test]
    fn second_line() {
        let c = converter("entity foo is\nend foo;");
        let loc = c.location(14);
        assert_eq!((loc.line, loc.column), (2, 1));
    }

    #[test]
    fn column_after_block_comment_with_wide_char() {
        // /*é*/  — `é` is U+00E9, 2 bytes in UTF-8.
        // Bytes: 0:'/' 1:'*' 2:0xC3 3:0xA9 4:'*' 5:'/' 6:'x'
        // Offset 6 is the start of 'x'. Characters seen: '/', '*', 'é', '*', '/' = 5 chars
        // plus we're now on the 6th = column 6 (1-based).
        let c = converter("/*é*/x");
        let loc = c.location(6);
        assert_eq!((loc.line, loc.column), (1, 6));
    }

    #[test]
    fn column_inside_multibyte_char_clamps() {
        // Offset 3 lands inside the 2-byte `é`. LSP convention: clamp to char start.
        let c = converter("/*é*/x");
        let loc = c.location(3);
        // Position of 'é' is char 3 (after '/', '*'): column 3.
        assert_eq!((loc.line, loc.column), (1, 3));
    }

    #[test]
    fn newline_inside_block_comment_advances_line() {
        // /*\n*/x — `\n` is at byte 2; new line starts at byte 3; '*' at 3, '/' at 4, 'x' at 5.
        let c = converter("/*\n*/x");
        let loc = c.location(5);
        assert_eq!((loc.line, loc.column), (2, 3));
    }
}
