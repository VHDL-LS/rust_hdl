use std::collections::HashMap;

use crate::{
    fmt::encoding::Encoder, latin_1::Latin1Str, syntax::node::SyntaxNode, text::{char_encoding::CharEncoding, char_iter::CharIter}, tokens::TriviaPiece
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
