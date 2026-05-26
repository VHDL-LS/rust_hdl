use core::fmt;
use std::{borrow::Cow, fmt::Display, ops::Range};

use annotate_snippets::{AnnotationKind, Group, Level, Snippet};
use vhdl_syntax::{
    fmt::{
        encoding::{Encoder, LossyUtf8Encoder},
        write::{WriteEncoded, WriteError},
    },
    parser::diagnostics::ParserDiagnostic,
    syntax::node::SyntaxNode,
    text::source_loc::SourceLocConverter,
    tokens::{tokenizer::UnterminatedKind, TokenKind},
};

/// Returns the snippet text covering `byte_range` (with `surplus` extra lines of
/// context on each side) along with the absolute byte offset of its first
/// character. The offset lets callers translate absolute source spans into
/// snippet-relative ones.
fn lines<E: Encoder>(
    source: &SyntaxNode,
    cache: &SourceLocConverter,
    byte_range: Range<usize>,
    surplus: usize,
) -> Result<(String, usize), E::Err>
where
    for<'a> E::Str<'a>: Display,
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
    let first = start_line.saturating_sub(surplus).max(1);
    let last = end_line.saturating_add(surplus);
    let base = cache.line_start(first);

    let mut result = String::new();
    for (i, line) in full.split_inclusive('\n').enumerate() {
        let line_num = i + 1;
        if line_num > last {
            break;
        }
        if line_num >= first {
            result.push_str(line);
        }
    }
    Ok((result, base))
}

pub fn parser_diagnostic_to_report<'a>(
    diagnostic: &ParserDiagnostic,
    file_name: Option<Cow<'a, str>>,
    tree: &SyntaxNode,
    cache: &SourceLocConverter,
) -> Group<'a> {
    match diagnostic {
        ParserDiagnostic::ExpectedToken {
            expected: (insertion_pos, expected),
            found: (found_pos, found),
        } => {
            let (snippet, base) =
                lines::<LossyUtf8Encoder>(tree, cache, *insertion_pos..found_pos.end, 0).unwrap();
            let ins = *insertion_pos - base;
            let found_rel = (found_pos.start - base)..(found_pos.end - base);
            let mut annotations = vec![AnnotationKind::Primary
                .span(ins..ins)
                .label(format!("{} expected here", expected_message(expected)))];
            if !found.is_eof() {
                annotations.push(
                    AnnotationKind::Context
                        .span(found_rel)
                        .label("unexpected token"),
                );
            }

            Level::ERROR
                .primary_title(expected_token_message(expected, *found))
                .element(
                    Snippet::source(snippet)
                        .line_start(cache.source_loc(*insertion_pos).line)
                        .path(file_name)
                        .annotations(annotations),
                )
        }
        ParserDiagnostic::UnexpectedInput { span } => {
            let (snippet, base) = lines::<LossyUtf8Encoder>(tree, cache, span.clone(), 0).unwrap();
            let rel_span = (span.start - base)..(span.end - base);

            Level::ERROR.primary_title("Unexpected input").element(
                Snippet::source(snippet)
                    .line_start(cache.source_loc(span.start).line)
                    .path(file_name)
                    .annotation(
                        AnnotationKind::Primary
                            .span(rel_span)
                            .label("This input is unexpected"),
                    ),
            )
        }
        ParserDiagnostic::IllegalInput { span, text: _ } => {
            let (snippet, base) = lines::<LossyUtf8Encoder>(tree, cache, span.clone(), 0).unwrap();
            let rel_span = (span.start - base)..(span.end - base);

            Level::ERROR.primary_title("Illegal input").element(
                Snippet::source(snippet)
                    .line_start(cache.source_loc(span.start).line)
                    .path(file_name)
                    .annotation(
                        AnnotationKind::Primary
                            .span(rel_span)
                            .label("This input is unexpected"),
                    ),
            )
        }
        // TODO: For unterminated: highlight start delimiter
        ParserDiagnostic::Unterminated { span, kind } => {
            let (snippet, base) = lines::<LossyUtf8Encoder>(tree, cache, span.clone(), 0).unwrap();
            let rel_span = (span.start - base)..(span.end - base);

            Level::ERROR
                .primary_title(format!("Unterminated {}", describe_unterminated(kind)))
                .element(
                    Snippet::source(snippet)
                        .line_start(cache.source_loc(span.start).line)
                        .path(file_name)
                        .annotation(
                            AnnotationKind::Primary
                                .span(rel_span)
                                .label("Opening delimiter was never closed"),
                        ),
                )
        }
    }
}

fn describe_unterminated(kind: &UnterminatedKind) -> &'static str {
    match kind {
        UnterminatedKind::StringLiteral => "string literal",
        UnterminatedKind::BasedLiteral => "based literal",
        UnterminatedKind::ExtendedIdentifier => "extended identifier",
        UnterminatedKind::BlockComment => "comment",
    }
}

fn expected_message(expected: &[TokenKind]) -> String {
    use std::fmt::Write;
    match expected {
        [] => unreachable!("At least one expected must be present"),
        [only] => format!("{}", Expected(*only)),
        [head @ .., last] => {
            let mut out = String::new();
            for (i, tok) in head.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                write!(out, "{}", Expected(*tok)).unwrap();
            }
            write!(out, ", or {}", Expected(*last)).unwrap();
            out
        }
    }
}

fn expected_token_message(expected: &[TokenKind], found: TokenKind) -> String {
    let exp_message = expected_message(expected);

    if found == TokenKind::Eof {
        format!("Expected {exp_message}")
    } else {
        format!("Expected {exp_message}, found {}", Expected(found))
    }
}

/// Newtype wrapper to properly format token kinds when printing them to the user
struct Expected(TokenKind);

impl fmt::Display for Expected {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            TokenKind::Keyword(kw) => {
                write!(f, "`{}`", kw.canonical_text())
            }
            TokenKind::Plus => write!(f, "`+`"),
            TokenKind::Minus => write!(f, "`-`"),
            TokenKind::EQ => write!(f, "`=`"),
            TokenKind::NE => write!(f, "`/=`"),
            TokenKind::LT => write!(f, "`<`"),
            TokenKind::LTE => write!(f, "`<=`"),
            TokenKind::GT => write!(f, "`>`"),
            TokenKind::GTE => write!(f, "`>=`"),
            TokenKind::QueEQ => write!(f, "`?=`"),
            TokenKind::QueNE => write!(f, "`?/=`"),
            TokenKind::QueLT => write!(f, "`?<`"),
            TokenKind::QueLTE => write!(f, "`?<=`"),
            TokenKind::QueGT => write!(f, "`?>`"),
            TokenKind::QueGTE => write!(f, "`?>=`"),
            TokenKind::Que => write!(f, "`?`"),
            TokenKind::QueQue => write!(f, "`??`"),
            TokenKind::Times => write!(f, "`*`"),
            TokenKind::Pow => write!(f, "`**`"),
            TokenKind::Div => write!(f, "`/`"),
            TokenKind::Tick => write!(f, "`'`"),
            TokenKind::LeftPar => write!(f, "`(`"),
            TokenKind::RightPar => write!(f, "`)`"),
            TokenKind::LeftSquare => write!(f, "`[`"),
            TokenKind::RightSquare => write!(f, "`]`"),
            TokenKind::SemiColon => write!(f, "`;`"),
            TokenKind::Colon => write!(f, "`:`"),
            TokenKind::Bar => write!(f, "`|`"),
            TokenKind::Dot => write!(f, "`.`"),
            TokenKind::BOX => write!(f, "`<>`"),
            TokenKind::LtLt => write!(f, "`<<`"),
            TokenKind::GtGt => write!(f, "`>>`"),
            TokenKind::Circ => write!(f, "`^`"),
            TokenKind::CommAt => write!(f, "`@`"),
            TokenKind::Concat => write!(f, "`&`"),
            TokenKind::Comma => write!(f, "`,`"),
            TokenKind::ColonEq => write!(f, "`:=`"),
            TokenKind::RightArrow => write!(f, "`=>`"),
            TokenKind::Eof => write!(f, "end of input"),
            TokenKind::Identifier => write!(f, "identifier"),
            TokenKind::AbstractLiteral => write!(f, "abstract literal"),
            TokenKind::StringLiteral => write!(f, "string literal"),
            TokenKind::BitStringLiteral => write!(f, "bitstring literal"),
            TokenKind::CharacterLiteral => write!(f, "character literal"),
            TokenKind::ToolDirective => write!(f, "tool directive"),
            TokenKind::Unknown => {
                unreachable!("should never be called by 'expected'")
            }
        }
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
        lines::<Utf8Encoder>(&node, &cache, range, surplus).expect("utf-8")
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
