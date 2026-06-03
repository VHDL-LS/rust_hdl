use std::{borrow::Cow, fmt::Display, ops::Range};

use annotate_snippets::{Annotation, AnnotationKind, Group, Level, OptionCow, Snippet};
use vhdl_syntax::{
    fmt::{
        encoding::{Encoder, LossyUtf8Encoder},
        write::{WriteEncoded, WriteError},
    },
    parser::error::{SyntaxErr, SyntaxErrKind},
    syntax::node::SyntaxNode,
    text::source_loc::{EncodedOffset, SourceLoc, SourceLocConverter},
};

use crate::syntax_err::{describe_unterminated, expected_message, expected_token_message, token_after};

pub struct SnippetOffset(usize);

impl SnippetOffset {
    pub fn raw(&self) -> usize {
        self.0
    }
}

pub struct SnippetSpan {
    start: usize,
    end: usize,
}

impl SnippetSpan {
    pub fn new(start: usize, end: usize) -> SnippetSpan {
        SnippetSpan { start, end }
    }

    pub fn start(&self) -> SnippetOffset {
        SnippetOffset(self.start)
    }

    pub fn end(&self) -> SnippetOffset {
        SnippetOffset(self.end)
    }
}

struct SnippetConverter<'a> {
    base: EncodedOffset,
    cache: &'a SourceLocConverter,
}

impl<'a> SnippetConverter<'a> {
    pub fn new(base: EncodedOffset, cache: &'a SourceLocConverter) -> SnippetConverter<'a> {
        SnippetConverter { base, cache }
    }

    pub fn convert_span(&self, span: &Range<usize>) -> SnippetSpan {
        let converted = self.cache.convert_byte_span(span);
        SnippetSpan::new(
            converted.start().raw() - self.base.raw(),
            converted.end().raw() - self.base.raw(),
        )
    }
}

/// Returns the snippet text covering `byte_range` (with `surplus` extra lines of
/// context on each side) along with the absolute byte offset of its first
/// character. The offset lets callers translate absolute source spans into
/// snippet-relative ones.
fn lines<'a, E: Encoder>(
    source: &SyntaxNode,
    cache: &'a SourceLocConverter,
    byte_range: &Range<usize>,
    surplus: usize,
) -> Result<(String, SnippetConverter<'a>), E::Err>
where
    for<'b> E::Str<'b>: Display,
{
    let mut full = String::new();
    match source.fmt_to::<E>(&mut full) {
        Ok(()) => {}
        Err(WriteError::Fmt(_)) => unreachable!("Writing to a string should not fail"),
        Err(WriteError::Encoding(e)) => return Err(e),
    }

    let last_byte = byte_range.end.saturating_sub(1).max(byte_range.start);
    let start_line = cache.source_loc(byte_range.start).line;
    let end_line = cache.source_loc(last_byte).line;
    let first = start_line.saturating_sub(surplus);
    let last = end_line.saturating_add(surplus);
    let base = cache.line_start(first);

    let mut result = String::new();
    for (i, line) in full.split_inclusive('\n').enumerate() {
        if i > last {
            break;
        }
        if i >= first {
            result.push_str(line);
        }
    }
    Ok((result, SnippetConverter::new(base, cache)))
}

fn annotation<'a>(
    kind: AnnotationKind,
    span: SnippetSpan,
    label: impl Into<OptionCow<'a>>,
) -> Annotation<'a> {
    kind.span(span.start().raw()..span.end().raw()).label(label)
}

fn primary_anno<'a>(span: SnippetSpan, label: impl Into<OptionCow<'a>>) -> Annotation<'a> {
    annotation(AnnotationKind::Primary, span, label)
}

fn context_anno<'a>(span: SnippetSpan, label: impl Into<OptionCow<'a>>) -> Annotation<'a> {
    annotation(AnnotationKind::Context, span, label)
}

fn make_snipet<'a, T>(snippet: impl Into<Cow<'a, str>>, source_loc: SourceLoc) -> Snippet<'a, T>
where
    T: Clone,
{
    Snippet::source(snippet).line_start(source_loc.line + 1)
}

pub fn parser_diagnostic_to_report<'a>(
    diagnostic: &SyntaxErr,
    file_name: Option<Cow<'a, str>>,
    tree: &SyntaxNode,
    cache: &SourceLocConverter,
) -> Group<'a> {
    let snippet_range = match diagnostic.err() {
        SyntaxErrKind::Expected { .. } => {
            let found = token_after(tree, diagnostic.span_raw());
            if found.kind().is_eof() {
                diagnostic.span_raw().clone()
            } else {
                diagnostic.span_raw().start..found.range().end
            }
        }
        _ => diagnostic.span_raw().clone(),
    };

    let (snippet, converter) = lines::<LossyUtf8Encoder>(tree, cache, &snippet_range, 0).unwrap();
    let snippet = make_snipet(snippet, cache.source_loc(snippet_range.start)).path(file_name);
    let span = converter.convert_span(diagnostic.span_raw());

    match diagnostic.err() {
        SyntaxErrKind::Expected { kinds } => {
            let found = token_after(tree, diagnostic.span_raw());
            let mut annotations = vec![primary_anno(
                span,
                format!("{} expected here", expected_message(kinds)),
            )];
            if !found.kind().is_eof() {
                annotations.push(context_anno(
                    converter.convert_span(&found.text_range()),
                    "unexpected token",
                ));
            }

            Level::ERROR
                .primary_title(expected_token_message(kinds, found.kind()))
                .element(snippet.annotations(annotations))
        }
        SyntaxErrKind::Unexpected { kind: _ } => Level::ERROR
            .primary_title("Unexpected input")
            .element(snippet.annotation(primary_anno(span, "This input is unexpected"))),
        SyntaxErrKind::Illegal { bytes: _ } => Level::ERROR
            .primary_title("Illegal input")
            .element(snippet.annotation(primary_anno(span, "This input is unexpected"))),
        SyntaxErrKind::Unterminated { kind } => Level::ERROR
            .primary_title(format!("Unterminated {}", describe_unterminated(kind)))
            .element(snippet.annotation(primary_anno(span, "Opening delimiter was never closed"))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use vhdl_syntax::fmt::encoding::Utf8Encoder;
    use vhdl_syntax::parser;
    use vhdl_syntax::syntax::AstNode;
    use vhdl_syntax::text::char_encoding;
    use vhdl_syntax::text::source_loc::SourceLocConverter;

    fn setup(src: &str) -> (SyntaxNode, SourceLocConverter) {
        let (design, _) = parser::parse(src);
        let node = design.raw().clone();
        let cache = SourceLocConverter::new::<Utf8Encoder, char_encoding::Utf8>(&node)
            .expect("valid utf-8");
        (node, cache)
    }

    fn run(src: &str, range: Range<usize>, surplus: usize) -> (String, usize) {
        let (node, cache) = setup(src);
        lines::<Utf8Encoder>(&node, &cache, &range, surplus).expect("utf-8")
    }

    #[test]
    fn single_line_no_surplus() {
        let src = "entity foo is end foo;";
        assert_eq!(run(src, 0..3, 0), ("entity foo is end foo;".into(), 0));
    }

    #[test]
    fn middle_line_no_surplus() {
        let src = "line1\nline2\nline3\n";
        // 'line2' starts at byte 6.
        assert_eq!(run(src, 6..11, 0), ("line2\n".into(), 6));
    }

    #[test]
    fn surplus_includes_neighbors() {
        let src = "line1\nline2\nline3\nline4\nline5\n";
        // Range on line3 (bytes 12..17), one line of context each side.
        assert_eq!(run(src, 12..17, 1), ("line2\nline3\nline4\n".into(), 6),);
    }

    #[test]
    fn surplus_clamps_at_start_and_end() {
        let src = "line1\nline2\nline3\n";
        // Range on line1 with surplus 5 — clamps to file extent.
        assert_eq!(run(src, 0..5, 5), ("line1\nline2\nline3\n".into(), 0),);
    }

    #[test]
    fn range_spanning_multiple_lines() {
        let src = "line1\nline2\nline3\nline4\n";
        // Bytes 0..17 covers lines 1..3 (inclusive end at start of line3 trailing).
        assert_eq!(run(src, 0..17, 0), ("line1\nline2\nline3\n".into(), 0),);
    }

    #[test]
    fn empty_range_picks_its_line() {
        let src = "line1\nline2\nline3\n";
        // Empty range located on line2 (byte 8 sits within "line2").
        assert_eq!(run(src, 8..8, 0), ("line2\n".into(), 6));
    }
}
