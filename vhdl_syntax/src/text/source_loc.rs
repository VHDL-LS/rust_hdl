use std::{collections::BTreeMap, convert::Infallible, ops::Range};

use crate::{
    fmt::encoding::{BytePreservingEncoder, Encoder, LossyEncoder, Replacement},
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SourceLoc {
    /// Zero-based line number.
    pub line: usize,
    /// Zero-based column in target-encoding code units.
    pub col: usize,
}

/// A byte-offset expressed in some encoding.
/// This is the return type of conversion methods of [SourceLocConverter].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct EncodedOffset(usize);

impl EncodedOffset {
    pub(crate) fn new(inner: usize) -> EncodedOffset {
        EncodedOffset(inner)
    }

    pub fn raw(&self) -> usize {
        self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct EncodedSpan {
    start: usize,
    end: usize,
}

impl EncodedSpan {
    pub(crate) fn new(start: usize, end: usize) -> EncodedSpan {
        EncodedSpan { start, end }
    }

    pub fn start(&self) -> EncodedOffset {
        EncodedOffset::new(self.start)
    }

    pub fn end(&self) -> EncodedOffset {
        EncodedOffset::new(self.end)
    }
}

#[derive(Debug)]
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
    wide_char_lines: BTreeMap<usize, Vec<WideChar>>,
}

impl SourceLocConverter {
    fn build<C: CharEncoding, Err>(
        root: &SyntaxNode,
        mut handle_piece: impl FnMut(
            &TriviaPiece,
            usize,
            &mut Vec<usize>,
            &mut BTreeMap<usize, Vec<WideChar>>,
        ) -> Result<(), Err>,
    ) -> Result<SourceLocConverter, Err> {
        let mut line_starts = vec![0usize];
        let mut wide_char_lines = BTreeMap::new();
        let mut cursor = 0usize;

        let mut tok = root.first_token();
        while let Some(t) = tok {
            for piece in t.leading_trivia() {
                handle_piece(piece, cursor, &mut line_starts, &mut wide_char_lines)?;
                cursor += piece.byte_len();
            }
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

    /// Build the index by walking all tokens and trivia in the tree.
    ///
    /// `E` determines how comment bytes are decoded into characters.
    /// `C` determines how each character's column width is measured.
    /// Returns `Err` if a comment body cannot be decoded under `E`.
    pub fn new<E: BytePreservingEncoder, C: CharEncoding>(
        root: &SyntaxNode,
    ) -> Result<SourceLocConverter, E::Err>
    where
        for<'a> E::Str<'a>: CharIter,
    {
        Self::build::<C, _>(root, |piece, cursor, line_starts, wide_char_lines| {
            record_piece::<E, C>(piece, cursor, line_starts, wide_char_lines)
        })
    }

    pub fn new_lossy<E: LossyEncoder, C: CharEncoding>(root: &SyntaxNode) -> SourceLocConverter
    where
        for<'a> E::Str<'a>: CharIter,
    {
        Self::build::<C, Infallible>(root, |piece, cursor, line_starts, wide_char_lines| {
            record_piece_lossy::<E, C>(piece, cursor, line_starts, wide_char_lines);
            Ok(())
        })
        .unwrap()
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

    /// Convert a compiler-issued byte-offset to a byte-offset under the given character encoding.
    /// For example, a Latin-1 "ä" character occupies one byte but it occupies two bytes
    /// using UTF-8. This function correctly returns the beginning of the UTF-8 offset
    /// (if the encoder was constructed using a UTF-8 char encoding)
    pub fn convert_byte_offset(&self, offset: usize) -> EncodedOffset {
        let mut delta = 0usize;
        for (line, wide_chars) in &self.wide_char_lines {
            let line_start = self.line_starts[*line];
            for wide in wide_chars {
                if wide.offset + line_start >= offset {
                    break;
                }
                delta += wide.target_len - wide.byte_len;
            }
        }
        EncodedOffset::new(offset + delta)
    }

    pub fn convert_byte_span(&self, span: &Range<usize>) -> EncodedSpan {
        EncodedSpan::new(
            self.convert_byte_offset(span.start).raw(),
            self.convert_byte_offset(span.end).raw(),
        )
    }

    /// Returns the byte offset of the line at index.
    pub fn line_start(&self, index: usize) -> EncodedOffset {
        EncodedOffset::new(self.line_starts[index])
    }
}

fn record_text<C: CharEncoding>(
    text: &Latin1Str,
    cursor: usize,
    line_starts: &mut Vec<usize>,
    wide_char_lines: &mut BTreeMap<usize, Vec<WideChar>>,
) {
    record_str::<C>(text, &[], line_starts, wide_char_lines, cursor);
}

fn record_piece<E: Encoder, C: CharEncoding>(
    piece: &TriviaPiece,
    cursor: usize,
    line_starts: &mut Vec<usize>,
    wide_char_lines: &mut BTreeMap<usize, Vec<WideChar>>,
) -> Result<(), E::Err>
where
    for<'a> E::Str<'a>: CharIter,
{
    match piece {
        // Note: In theory, `LineComment`s don't need the special newline handling, it's just included here for simplicity.
        // If performance ever becomes a bottleneck, this can be split.
        TriviaPiece::BlockComment(c) | TriviaPiece::LineComment(c) => {
            record_str::<C>(
                c.encode::<E>()?,
                &[],
                line_starts,
                wide_char_lines,
                cursor + 2,
            );
        }
        other => {
            record_non_comment_trivia::<C>(other, cursor, line_starts, wide_char_lines);
        }
    }
    Ok(())
}

fn record_piece_lossy<E: LossyEncoder, C: CharEncoding>(
    piece: &TriviaPiece,
    cursor: usize,
    line_starts: &mut Vec<usize>,
    wide_char_lines: &mut BTreeMap<usize, Vec<WideChar>>,
) where
    for<'a> E::Str<'a>: CharIter,
{
    match piece {
        TriviaPiece::BlockComment(c) | TriviaPiece::LineComment(c) => {
            let (encoded, replacements) = c.encode_lossy::<E>();
            record_str::<C>(
                encoded,
                &replacements,
                line_starts,
                wide_char_lines,
                cursor + 2,
            );
        }
        other => {
            record_non_comment_trivia::<C>(other, cursor, line_starts, wide_char_lines);
        }
    }
}

fn record_non_comment_trivia<C: CharEncoding>(
    piece: &TriviaPiece,
    cursor: usize,
    line_starts: &mut Vec<usize>,
    wide_char_lines: &mut BTreeMap<usize, Vec<WideChar>>,
) {
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
        TriviaPiece::BlockComment(_) | TriviaPiece::LineComment(_) => {
            unreachable!("Handled by specific branch");
        }
        TriviaPiece::NonBreakingSpaces(n) => {
            let target_len = C::char_len('\u{00A0}');
            if target_len != 1 {
                let line = line_starts.len() - 1;
                let line_start = line_starts[line];
                for i in 0..*n {
                    let abs_offset = cursor + i;
                    wide_char_lines.entry(line).or_default().push(WideChar {
                        offset: abs_offset - line_start,
                        byte_len: 1,
                        target_len,
                    });
                }
            }
        }
        _ => {}
    }
}

fn record_str<C: CharEncoding>(
    encoded: impl CharIter,
    replacements: &[Replacement],
    line_starts: &mut Vec<usize>,
    wide_char_lines: &mut BTreeMap<usize, Vec<WideChar>>,
    cursor: usize,
) {
    let mut line = line_starts.len() - 1;
    let mut itr = encoded.iter_chars_indices().peekable();
    let byte_len = encoded.byte_count();
    let mut repl_idx = 0;
    // Accumulated difference between encoded and source positions.
    // encoded_pos = source_pos + delta
    let mut delta: usize = 0;

    while let Some((enc_pos, ch)) = itr.next() {
        let source_pos = enc_pos - delta;

        if ch == '\n' {
            line_starts.push(cursor + source_pos + 1);
            line += 1;
            continue;
        }
        if ch == '\r' {
            if itr.peek().is_some_and(|(_, ch)| *ch == '\n') {
                let _ = itr.next();
                line_starts.push(cursor + source_pos + 2);
            } else {
                line_starts.push(cursor + source_pos + 1);
            }
            line += 1;
            continue;
        }

        let enc_next = itr.peek().map(|(p, _)| *p).unwrap_or(byte_len);
        let enc_byte_width = enc_next - enc_pos;
        let target_width = C::char_len(ch);

        let at_replacement =
            repl_idx < replacements.len() && source_pos == replacements[repl_idx].source_offset;

        if at_replacement {
            let r = &replacements[repl_idx];
            let source_byte_width = r.source_bytes;
            if source_byte_width != target_width {
                let abs_offset = cursor + source_pos;
                let line_start = line_starts[line];
                wide_char_lines.entry(line).or_default().push(WideChar {
                    offset: abs_offset - line_start,
                    byte_len: source_byte_width,
                    target_len: target_width,
                });
            }
            delta += r.target_bytes - r.source_bytes;
            repl_idx += 1;
        } else {
            if enc_byte_width != 1 || enc_byte_width != target_width {
                let abs_offset = cursor + source_pos;
                let line_start = line_starts[line];
                wide_char_lines.entry(line).or_default().push(WideChar {
                    offset: abs_offset - line_start,
                    byte_len: enc_byte_width,
                    target_len: target_width,
                });
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fmt::encoding::{Latin1Encoder, LossyUtf8Encoder, Utf8Encoder};
    use crate::parser::parse;
    use crate::text::char_encoding::{Utf16, Utf32, Utf8};
    use crate::tokens::TokenStream;

    fn converter<E: BytePreservingEncoder, C: CharEncoding>(
        input: impl Into<TokenStream>,
    ) -> SourceLocConverter
    where
        for<'a> E::Str<'a>: CharIter,
        E::Err: std::fmt::Debug,
    {
        let (design_file, _) = parse(input);
        SourceLocConverter::new::<E, C>(&design_file.0).unwrap()
    }

    fn lossy_converter<E: LossyEncoder, C: CharEncoding>(
        input: impl Into<TokenStream>,
    ) -> SourceLocConverter
    where
        for<'a> E::Str<'a>: CharIter,
    {
        let (design_file, _) = parse(input);
        SourceLocConverter::new_lossy::<E, C>(&design_file.0)
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

    #[test]
    fn block_comment_multiline_carriage_return() {
        let input = "/* line1\rline2 */\rentity e is end;";
        let conv = utf8_utf16(input);
        // "/*" = 2, " line1\r" = 7 bytes => line 1 starts at offset 9
        assert_eq!(loc(&conv, 9), (1, 0));
        // "line2 */" = 8, "\r" = 1 => line 2 starts at offset 18
        assert_eq!(loc(&conv, 18), (2, 0));
    }

    #[test]
    fn block_comment_multiline_carriage_return_line_feed() {
        let input = "/* line1\r\nline2 */\r\nentity e is end;";
        let conv = utf8_utf16(input);
        // "/*" = 2, " line1\r\n" = 7 bytes => line 1 starts at offset 9
        assert_eq!(loc(&conv, 10), (1, 0));
        // "line2 */" = 8, "\r\n" = 1 => line 2 starts at offset 18
        assert_eq!(loc(&conv, 20), (2, 0));
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

    #[test]
    fn latin1_source_utf8_target_codepoint() {
        // \xE4 = Latin-1 'ä' (1 byte source, 2 bytes UTF-8 target)
        // Layout: "--" (2) + "\xE4" (1) + "\n" (1) + "entity e is end;" (16)
        let input: &[u8] = b"--\xE4\nentity e is end;";
        let conv = converter::<Latin1Encoder, Utf8>(input);
        assert_eq!(conv.convert_byte_offset(0).raw(), 0);
        assert_eq!(conv.convert_byte_offset(2).raw(), 2); // start of ä
        assert_eq!(conv.convert_byte_offset(3).raw(), 4); // \n (after ä: delta +1)
        assert_eq!(conv.convert_byte_offset(4).raw(), 5); // 'e' in entity
    }

    #[test]
    fn latin1_source_utf8_target_codepoint_twice() {
        // Two consecutive \xE4 in a line comment
        // Layout: "--" (2) + "\xE4\xE4" (2) + "\n" (1) + "entity e is end;" (16)
        let input: &[u8] = b"--\xE4\xE4\nentity e is end;";
        let conv = converter::<Latin1Encoder, Utf8>(input);
        assert_eq!(conv.convert_byte_offset(0).raw(), 0);
        assert_eq!(conv.convert_byte_offset(2).raw(), 2); // start of first ä
        assert_eq!(conv.convert_byte_offset(3).raw(), 4); // start of second ä (delta +1)
        assert_eq!(conv.convert_byte_offset(4).raw(), 6); // \n (delta +2)
        assert_eq!(conv.convert_byte_offset(5).raw(), 7); // 'e' in entity
    }

    #[test]
    fn lossy_utf8_to_utf8() {
        // Two invalid UTF-8 bytes in a block comment, each replaced with U+FFFD (3 bytes)
        // Layout: "/*" (2) + "\xE4\xE4" (2) + "*/" (2) + "entity e is end;" (16)
        let input: &[u8] = b"/*\xE4\xE4*/entity e is end;";
        let conv = lossy_converter::<LossyUtf8Encoder, Utf8>(input);
        assert_eq!(conv.convert_byte_offset(0).raw(), 0); // '/'
        assert_eq!(conv.convert_byte_offset(1).raw(), 1); // '*'
        assert_eq!(conv.convert_byte_offset(2).raw(), 2); // first \xE4 → FFFD start
        assert_eq!(conv.convert_byte_offset(3).raw(), 5); // second \xE4 → FFFD start (delta +2)
        assert_eq!(conv.convert_byte_offset(4).raw(), 8); // '*' of */ (delta +4)
        assert_eq!(conv.convert_byte_offset(5).raw(), 9); // '/' of */
        assert_eq!(conv.convert_byte_offset(6).raw(), 10); // 'e' in entity
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

    // Non-breaking spaces (U+00A0)

    #[test]
    fn non_breaking_space_latin1_to_utf8() {
        // \xA0 = non-breaking space, 1 byte in Latin-1, 2 bytes in UTF-8
        // Layout: "\xA0" (1) + "entity e is end;" (16)
        let input: &[u8] = b"\xA0entity e is end;";
        let conv = converter::<Latin1Encoder, Utf8>(input);
        assert_eq!(conv.convert_byte_offset(0).raw(), 0); // start of NBSP
        assert_eq!(conv.convert_byte_offset(1).raw(), 2); // 'e' in entity (delta +1)
    }

    #[test]
    fn non_breaking_space_latin1_to_utf16() {
        // \xA0 is 1 byte Latin-1, 1 code unit UTF-16 — no expansion
        let input: &[u8] = b"\xA0entity e is end;";
        let conv = converter::<Latin1Encoder, Utf16>(input);
        assert_eq!(loc(&conv, 0), (0, 0));
        assert_eq!(loc(&conv, 1), (0, 1));
    }

    #[test]
    fn multiple_non_breaking_spaces_latin1_to_utf8() {
        // Two consecutive NBSPs before a token
        let input: &[u8] = b"\xA0\xA0entity e is end;";
        let conv = converter::<Latin1Encoder, Utf8>(input);
        assert_eq!(conv.convert_byte_offset(0).raw(), 0); // first NBSP
        assert_eq!(conv.convert_byte_offset(1).raw(), 2); // second NBSP (delta +1)
        assert_eq!(conv.convert_byte_offset(2).raw(), 4); // 'e' in entity (delta +2)
    }
}
