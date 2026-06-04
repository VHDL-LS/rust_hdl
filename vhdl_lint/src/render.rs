//! Rendering of [`Diagnostic`]s to a terminal

use std::{fmt, ops::Range};

use annotate_snippets::{
    Annotation, AnnotationKind, Element, Group, Level, OptionCow, Renderer, Snippet, renderer::DecorStyle
};
use vhdl_syntax::{
    fmt::{
        encoding::{Encoder, LossyUtf8Encoder},
        write::{WriteEncoded, WriteError},
    },
    syntax::node::SyntaxNode,
    text::source_loc::{EncodedOffset, SourceLocConverter},
};

use crate::diagnostic::{Diagnostic, Label, LabelKind, NoteKind, Severity, SourceId};
use crate::source::SourceProvider;

/// A terminal renderer for diagnostics, bound to a [`SourceProvider`].
///
/// Holds the rendering context once and hands out an opaque [`Displayed`] for a
/// batch of diagnostics, which renders them in a single pass.
pub struct Report<'a, P: SourceProvider> {
    provider: &'a P,
    diagnostics: &'a [Diagnostic]
}

impl<'a, P: SourceProvider> Report<'a, P> {
    pub fn new(provider: &'a P, diagnostics: &'a [Diagnostic]) -> Report<'a, P> {
        Report { provider, diagnostics }
    }
}

impl<P: SourceProvider> fmt::Display for Report<'_, P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let groups: Vec<Group> = self
            .diagnostics
            .iter()
            .filter_map(|diagnostic| diagnostic_to_group(diagnostic, self.provider))
            .collect();

        let renderer = Renderer::styled().decor_style(DecorStyle::Unicode);
        write!(f, "{}", renderer.render(&groups))
    }
}

/// Lower a single [`Diagnostic`] into an `annotate_snippets` [`Group`].
///
/// Labels are grouped by source (in first-seen order) into one snippet each, so
/// cross-file diagnostics render naturally; notes become trailing message
/// elements. Returns `None` if a referenced source cannot be resolved or the
/// diagnostic has no labels.
fn diagnostic_to_group<'a, P: SourceProvider>(
    diagnostic: &'a Diagnostic,
    provider: &'a P,
) -> Option<Group<'a>> {
    let level = match diagnostic.severity() {
        Severity::Error => Level::ERROR,
        Severity::Warning => Level::WARNING,
        Severity::Info => Level::INFO,
        Severity::Hint => Level::HELP,
    };
    let title = level.primary_title(diagnostic.message());

    let mut elements: Vec<Element<'a>> = Vec::new();

    for source_id in distinct_sources(diagnostic) {
        let file = provider.lookup(source_id)?;
        let labels: Vec<&Label> = diagnostic
            .labels()
            .iter()
            .filter(|label| label.source() == source_id)
            .collect();
        let range = union_span(labels.iter().map(|label| label.span().clone()))?;

        let (text, converter) =
            lines::<LossyUtf8Encoder>(file.tree, file.converter, &range, 0).unwrap();
        let line_start = file.converter.source_loc(range.start).line + 1;

        let snippet = Snippet::source(text)
            .line_start(line_start)
            .path(file.name)
            .annotations(labels.iter().map(|label| {
                annotation(
                    annotation_kind(label.kind()),
                    converter.convert_span(label.span()),
                    label.message(),
                )
            }));
        elements.push(snippet.into());
    }

    for note in diagnostic.notes() {
        let level = match note.kind() {
            NoteKind::Note => Level::NOTE,
            NoteKind::Help => Level::HELP,
        };
        elements.push(level.message(note.message()).into());
    }

    Some(title.elements(elements))
}

/// The distinct sources referenced by `diagnostic`'s labels, in first-seen order
/// (so the primary label's source comes first).
fn distinct_sources(diagnostic: &Diagnostic) -> Vec<SourceId> {
    let mut sources: Vec<SourceId> = Vec::new();
    for label in diagnostic.labels() {
        if !sources.contains(&label.source()) {
            sources.push(label.source());
        }
    }
    sources
}

/// Smallest range covering all `spans`, or `None` if there are none.
fn union_span(spans: impl Iterator<Item = Range<usize>>) -> Option<Range<usize>> {
    let mut spans = spans;
    let first = spans.next()?;
    let (mut start, mut end) = (first.start, first.end);
    for span in spans {
        start = start.min(span.start);
        end = end.max(span.end);
    }
    Some(start..end)
}

fn annotation_kind(kind: LabelKind) -> AnnotationKind {
    match kind {
        LabelKind::Primary => AnnotationKind::Primary,
        LabelKind::Context => AnnotationKind::Context,
    }
}

fn annotation<'a>(
    kind: AnnotationKind,
    span: SnippetSpan,
    label: impl Into<OptionCow<'a>>,
) -> Annotation<'a> {
    kind.span(span.start().raw()..span.end().raw()).label(label)
}

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

/// Maps raw source spans into offsets relative to the start of an extracted
/// snippet, in the render encoding.
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
/// context on each side) along with a converter that translates absolute source
/// spans into snippet-relative ones.
///
/// The text is currently rendered from the whole tree for simplicity; a future
/// implementation can render only the relevant portion of `source`.
fn lines<'a, E: Encoder>(
    source: &SyntaxNode,
    cache: &'a SourceLocConverter,
    byte_range: &Range<usize>,
    surplus: usize,
) -> Result<(String, SnippetConverter<'a>), E::Err>
where
    for<'b> E::Str<'b>: fmt::Display,
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
        let (text, converter) =
            lines::<Utf8Encoder>(&node, &cache, &range, surplus).expect("utf-8");
        (text, converter.base.raw())
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
