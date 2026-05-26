use std::collections::HashMap;

use crate::{
    fmt::encoding::Encoder,
    latin_1::Latin1Str,
    syntax::node::SyntaxNode,
    text::{char_encoding::CharEncoding, char_iter::CharIter},
    tokens::TriviaPiece,
};

/// A zero-based `(line, column)` position in source text.
///
/// The column is expressed in the target encoding's code units, not bytes.
/// For example, with [`super::char_encoding::Utf16`] as the target, `col` counts
/// UTF-16 code units — matching what LSP clients expect.
pub struct SourceLoc {
    /// Zero-based line number.
    pub line: usize,
    /// Zero-based column in target-encoding code units.
    pub col: usize,
}

struct WideChar {
    // Byte offset relative to line start
    offset: usize,
    // how many bytes this char occupies in source
    byte_len: usize,
    // how many units in the target encoding
    target_len: usize,
}

impl WideChar {
    fn end(&self) -> usize {
        self.offset + self.byte_len
    }
}

/// Translates byte offsets in a syntax tree to [`SourceLoc`] positions.
///
/// Built once from a [`SyntaxNode`] root, it indexes line boundaries and
/// characters whose byte width differs from their target-encoding width.
/// Lookups are O(log lines + wide chars on that line).
///
/// # Type parameters (on [`Self::new`])
///
/// - `E`: [`Encoder`] — the source encoding used to interpret comment bytes
///   (e.g., [`Utf8Encoder`](crate::fmt::encoding::Utf8Encoder) or
///   [`Latin1Encoder`](crate::fmt::encoding::Latin1Encoder)).
/// - `C`: [`CharEncoding`] — the target encoding for column measurement
///   (e.g., [`Utf16`](super::char_encoding::Utf16) for LSP).
///
/// # Example
///
/// ```ignore
/// use vhdl_syntax::text::source_loc::SourceLocConverter;
/// use vhdl_syntax::text::char_encoding::Utf16;
/// use vhdl_syntax::fmt::encoding::Utf8Encoder;
///
/// let converter = SourceLocConverter::new::<Utf8Encoder, Utf16>(&root).unwrap();
/// let loc = converter.source_loc(42);
/// // loc.line and loc.col are zero-based, with col in UTF-16 code units.
/// ```
pub struct SourceLocConverter {
    line_starts: Vec<usize>,
    text_len: usize,
    wide_char_lines: HashMap<usize, Vec<WideChar>>,
}

impl SourceLocConverter {
    /// Build the index by walking all tokens and trivia in the tree.
    ///
    /// `E` determines how comment bytes are decoded into characters.
    /// `C` determines how each character's column width is measured.
    /// Returns `Err` if a comment body cannot be decoded under `E`.
    pub fn new<E: Encoder, C: CharEncoding>(root: &SyntaxNode) -> Result<SourceLocConverter, E::Err>
    where
        for<'a> E::Str<'a>: CharIter,
    {
        let mut line_starts = vec![0usize];
        let mut wide_char_lines = HashMap::new();
        let mut cursor = 0usize;

        let mut tok = root.first_token();
        while let Some(t) = tok {
            for piece in t.leading_trivia() {
                record_piece::<E, C>(piece, cursor, &mut line_starts, &mut wide_char_lines)?;
                cursor += piece.byte_len();
            }
            // Text could contain newlines for unterminated inputs or characters that are multiple byte-offsets in the target encoding.
            // Record it as well
            record_text::<C>(t.text(), cursor, &mut line_starts, &mut wide_char_lines);
            cursor += t.text().len();
            tok = t.next_token();
        }

        Ok(SourceLocConverter {
            line_starts,
            text_len: cursor,
            wide_char_lines,
        })
    }

    /// Convert a byte offset into a [`SourceLoc`].
    ///
    /// Offsets beyond the end of the text are clamped. An offset that falls
    /// inside a multi-byte character is snapped to the character's start.
    pub fn source_loc(&self, offset: usize) -> SourceLoc {
        let offset = offset.min(self.text_len);

        let i = match self.line_starts.binary_search(&offset) {
            Ok(i) => i,
            Err(i) => i - 1,
        };
        let line_start = self.line_starts[i];
        let byte_col = offset - line_start;

        let mut target_col: usize = 0;
        let mut prev_end: usize = 0;

        if let Some(wides) = self.wide_char_lines.get(&i) {
            for wide in wides {
                if wide.offset >= byte_col {
                    break;
                }
                // non-wide bytes between previous wide char and this one: 1:1 mapping
                target_col += wide.offset - prev_end;

                if wide.end() <= byte_col {
                    target_col += wide.target_len;
                    prev_end = wide.end();
                } else {
                    // offset falls inside a multi-byte character; clamp to
                    // the character's start, matching LSP convention.
                    return SourceLoc {
                        line: i,
                        col: target_col,
                    };
                }
            }
        }

        // remaining non-wide bytes after the last wide char
        target_col += byte_col - prev_end;

        SourceLoc {
            line: i,
            col: target_col,
        }
    }
}

fn record_text<C: CharEncoding>(
    text: &Latin1Str,
    cursor: usize,
    line_starts: &mut Vec<usize>,
    wide_char_lines: &mut HashMap<usize, Vec<WideChar>>,
) {
    record_str::<C>(text, line_starts, wide_char_lines, cursor, text.len());
}

fn record_piece<E: Encoder, C: CharEncoding>(
    piece: &TriviaPiece,
    cursor: usize,
    line_starts: &mut Vec<usize>,
    wide_char_lines: &mut HashMap<usize, Vec<WideChar>>,
) -> Result<(), E::Err>
where
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
            record_str::<C>(
                c.encode::<E>()?,
                line_starts,
                wide_char_lines,
                cursor + 2,
                c.byte_len(),
            );
        }
        _ => {}
    }
    Ok(())
}

fn record_str<C: CharEncoding>(
    str: impl CharIter,
    line_starts: &mut Vec<usize>,
    wide_char_lines: &mut HashMap<usize, Vec<WideChar>>,
    cursor: usize,
    byte_len: usize,
) {
    let mut line = line_starts.len() - 1;
    let mut itr = str.iter_chars_indices().peekable();
    while let Some((pos, ch)) = itr.next() {
        if ch == '\n' {
            line_starts.push(cursor + pos + 1);
            line += 1;
            continue;
        }
        let next_pos = itr.peek().map(|(p, _)| *p).unwrap_or(byte_len);
        let byte_width = next_pos - pos;
        let char_width = C::char_len(ch);
        if byte_width != char_width {
            let abs_offset = cursor + pos;
            let line_start = line_starts[line];
            wide_char_lines.entry(line).or_default().push(WideChar {
                offset: abs_offset - line_start,
                byte_len: byte_width,
                target_len: char_width,
            });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fmt::encoding::{Latin1Encoder, Utf8Encoder};
    use crate::parser::parse;
    use crate::text::char_encoding::{Utf16, Utf32, Utf8};

    fn converter<E: Encoder, C: CharEncoding>(input: &str) -> SourceLocConverter
    where
        for<'a> E::Str<'a>: CharIter,
        E::Err: std::fmt::Debug,
    {
        let (design_file, _) = parse(input);
        SourceLocConverter::new::<E, C>(&design_file.0).unwrap()
    }

    fn utf8_utf16(input: &str) -> SourceLocConverter {
        converter::<Utf8Encoder, Utf16>(input)
    }

    fn loc(conv: &SourceLocConverter, offset: usize) -> (usize, usize) {
        let l = conv.source_loc(offset);
        (l.line, l.col)
    }

    // ASCII-only (all encodings agree)

    #[test]
    fn single_line_ascii() {
        let conv = utf8_utf16("entity e is end;");
        assert_eq!(loc(&conv, 0), (0, 0));
        assert_eq!(loc(&conv, 7), (0, 7));
    }

    #[test]
    fn multi_line_ascii() {
        // "entity e is\nend;\n"
        let input = "entity e is\nend;\n";
        let conv = utf8_utf16(input);
        assert_eq!(loc(&conv, 0), (0, 0));
        // offset 11 = last char of first line ('s')
        assert_eq!(loc(&conv, 11), (0, 11));
        // offset 12 = first char of second line ('e' in "end")
        assert_eq!(loc(&conv, 12), (1, 0));
        assert_eq!(loc(&conv, 15), (1, 3));
    }

    #[test]
    fn crlf_line_endings() {
        let input = "entity e is\r\nend;\r\n";
        let conv = utf8_utf16(input);
        // After \r\n (2 bytes), second line starts at offset 13
        assert_eq!(loc(&conv, 13), (1, 0));
        assert_eq!(loc(&conv, 16), (1, 3));
    }

    // Line comment with non-ASCII (UTF-8 source, UTF-16 target)

    #[test]
    fn line_comment_with_umlaut() {
        // ä is 2 bytes in UTF-8, 1 unit in UTF-16
        // Layout: "--" (2) + " " (1) + "ä" (2) + "\n" (1) = 6 bytes on line 0
        let input = "-- ä\nentity e is end;";
        let conv = utf8_utf16(input);

        // byte 2 = space after "--", col 2
        assert_eq!(loc(&conv, 2), (0, 2));
        // byte 3 = start of ä, col 3
        assert_eq!(loc(&conv, 3), (0, 3));
        // byte 4 = inside ä (second UTF-8 byte), clamps to ä start = col 3
        assert_eq!(loc(&conv, 4), (0, 3));
        // "entity" starts at byte 6 (after \n)
        assert_eq!(loc(&conv, 6), (1, 0));
    }

    #[test]
    fn line_comment_with_emoji() {
        // 💣 is 4 bytes in UTF-8, 2 units in UTF-16
        let input = "-- 💣\nentity e is end;";
        let conv = utf8_utf16(input);
        // "-- " = 3 bytes, "💣" = 4 bytes, "\n" = 1 byte => line 1 starts at offset 8
        assert_eq!(loc(&conv, 8), (1, 0));
        // byte 3 = start of 💣, UTF-16 col = 3
        assert_eq!(loc(&conv, 3), (0, 3));
        // byte 5 = inside 💣 (4-byte char), should clamp to start = col 3
        assert_eq!(loc(&conv, 5), (0, 3));
    }

    // Block comment spanning multiple lines

    #[test]
    fn block_comment_multiline() {
        let input = "/* line1\nline2 */\nentity e is end;";
        let conv = utf8_utf16(input);
        // "/*" = 2, " line1\n" = 7 bytes => line 1 starts at offset 9
        assert_eq!(loc(&conv, 9), (1, 0));
        // "line2 */" = 8, "\n" = 1 => line 2 starts at offset 18
        assert_eq!(loc(&conv, 18), (2, 0));
    }

    // Offset clamping

    #[test]
    fn offset_beyond_end_is_clamped() {
        let input = "entity e is end;";
        let conv = utf8_utf16(input);
        let clamped = conv.source_loc(9999);
        let end = conv.source_loc(input.len());
        assert_eq!(clamped.line, end.line);
        assert_eq!(clamped.col, end.col);
    }

    #[test]
    fn offset_zero() {
        let input = "entity e is end;";
        let conv = utf8_utf16(input);
        assert_eq!(loc(&conv, 0), (0, 0));
    }

    // UTF-8 source, UTF-32 target (code point columns)

    #[test]
    fn utf32_target_counts_code_points() {
        let input = "-- 💣x\nentity e is end;";
        let conv = converter::<Utf8Encoder, Utf32>(input);
        // 💣 is 1 code point, x is 1 code point
        // byte 3 = start of 💣, UTF-32 col = 3
        assert_eq!(loc(&conv, 3), (0, 3));
        // byte 7 = 'x' (after 4-byte 💣), UTF-32 col = 4
        assert_eq!(loc(&conv, 7), (0, 4));
    }

    // UTF-8 source, UTF-8 target (byte columns, no wide chars)

    #[test]
    fn utf8_target_matches_byte_offset() {
        let input = "-- ä💣\nentity e is end;";
        let conv = converter::<Utf8Encoder, Utf8>(input);
        // With UTF-8 target, col == byte offset within line since source is also UTF-8
        assert_eq!(loc(&conv, 3), (0, 3)); // start of ä
        assert_eq!(loc(&conv, 5), (0, 5)); // start of 💣
        assert_eq!(loc(&conv, 9), (0, 9)); // past 💣
    }

    // Latin-1 source, UTF-16 target

    #[test]
    fn latin1_source_utf16_target() {
        // All ASCII, so Latin-1 and UTF-16 agree: every byte is 1 column unit.
        let input = "-- hello\nentity e is end;";
        let conv = converter::<Latin1Encoder, Utf16>(input);
        assert_eq!(loc(&conv, 0), (0, 0));
        assert_eq!(loc(&conv, 9), (1, 0));
    }

    // Multiple wide chars on the same line

    #[test]
    fn multiple_wide_chars_same_line() {
        // Two umlauts: each 2 bytes UTF-8, 1 unit UTF-16
        let input = "-- äö\nentity e is end;";
        let conv = utf8_utf16(input);
        // byte 3 = ä (2 bytes), byte 5 = ö (2 bytes)
        // UTF-16 col of ä = 3, of ö = 4
        assert_eq!(loc(&conv, 3), (0, 3));
        assert_eq!(loc(&conv, 5), (0, 4));
        // byte 7 = \n, so line 1 starts at byte 8
        assert_eq!(loc(&conv, 8), (1, 0));
    }

    // Empty input

    #[test]
    fn empty_input() {
        let conv = utf8_utf16("");
        assert_eq!(loc(&conv, 0), (0, 0));
    }
}
