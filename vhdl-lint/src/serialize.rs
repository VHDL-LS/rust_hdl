use std::borrow::Cow;

use vhdl_syntax::text::source_loc::SourceLocConverter;

use crate::{
    diagnostic::Diagnostic,
    error_code::ErrorCode,
    fix::{Applicability, Edit, Fix},
    severity::Severity,
    FileStore,
};

#[derive(Debug, Clone, Copy, serde::Serialize)]
pub struct Position {
    line: usize,
    col: usize,
}

impl Position {
    pub fn from_offset(offset: usize, slc: &SourceLocConverter) -> Position {
        let loc = slc.source_loc(offset);
        Position {
            line: loc.line + 1,
            col: loc.col + 1,
        }
    }
}

#[derive(Debug, Clone, Copy, serde::Serialize)]
pub struct Range {
    start: Position,
    end: Position,
}

impl Range {
    pub fn from_span(span: &std::ops::Range<usize>, slc: &SourceLocConverter) -> Range {
        Range {
            start: Position::from_offset(span.start, slc),
            end: Position::from_offset(span.end, slc),
        }
    }
}

#[derive(Debug, Clone, serde::Serialize)]
pub struct RenderableEdit<'a> {
    replacement: Cow<'a, str>,
    range: Range,
}

impl<'a> RenderableEdit<'a> {
    pub fn from_edit(edit: &'a Edit, slc: &SourceLocConverter) -> RenderableEdit<'a> {
        RenderableEdit {
            range: Range::from_span(edit.span(), slc),
            replacement: edit.replacement().to_str(),
        }
    }
}

#[derive(Debug, Clone, serde::Serialize)]
pub struct RenderableFix<'a> {
    title: &'a str,
    edits: Box<[RenderableEdit<'a>]>,
    applicability: Applicability,
}

impl<'a> RenderableFix<'a> {
    pub fn from_fix(fix: &'a Fix, slc: &SourceLocConverter) -> RenderableFix<'a> {
        RenderableFix {
            title: fix.title(),
            edits: fix
                .edits()
                .iter()
                .map(|edit| RenderableEdit::from_edit(edit, slc))
                .collect(),
            applicability: fix.applicability(),
        }
    }
}

#[derive(Debug, Clone, serde::Serialize)]
pub struct RenderableDiagnostic<'a> {
    message: &'a str,
    severity: Severity,
    range: Range,
    file: Cow<'a, str>,
    code: ErrorCode,
    #[serde(skip_serializing_if = "Option::is_none")]
    fix: Option<RenderableFix<'a>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    note: Option<&'a str>,
}

impl<'a> RenderableDiagnostic<'a> {
    pub fn from_diagnostic(
        diagnostic: &'a Diagnostic,
        files: &'a FileStore,
    ) -> RenderableDiagnostic<'a> {
        let file = files.get(diagnostic.loc().file());
        RenderableDiagnostic {
            message: diagnostic.message(),
            severity: diagnostic.severity(),
            range: Range::from_span(diagnostic.loc().span(), file.source_mapping_utf32()),
            file: file.path().to_string_lossy(),
            code: *diagnostic.code(),
            fix: diagnostic
                .fix()
                .map(|fix| RenderableFix::from_fix(fix, file.source_mapping_utf32())),
            note: diagnostic.note(),
        }
    }
}

#[cfg(test)]
mod tests {
    use insta::assert_snapshot;

    use super::*;
    use crate::{
        error_code::Category, rule::registry::ActiveRules, source_loc::SourceLoc, Encoding, FileId,
        FileSettings,
    };

    fn files(source: &[u8], encoding: Encoding) -> FileStore {
        let mut files = FileStore::new();
        files.insert(
            "<inline>",
            source.to_vec(),
            FileSettings {
                encoding,
                ..FileSettings::default()
            },
        );
        files
    }

    fn diagnostic(span: std::ops::Range<usize>) -> Diagnostic {
        Diagnostic::new(
            "message",
            Severity::Warning,
            SourceLoc::new(FileId(0), span),
            ErrorCode::new(Category::Idiom, 1),
        )
    }

    fn to_json(diagnostic: &Diagnostic, files: &FileStore) -> String {
        serde_json::to_string_pretty(&RenderableDiagnostic::from_diagnostic(diagnostic, files))
            .unwrap()
    }

    /// The range of the first occurrence of `entity` in `source`, serialized.
    fn entity_range(source: &[u8], encoding: Encoding) -> String {
        let start = source
            .windows(b"entity".len())
            .position(|window| window == b"entity")
            .unwrap();
        let files = files(source, encoding);
        let file = files.get(FileId(0));
        serde_json::to_string(&Range::from_span(
            &(start..start + b"entity".len()),
            file.source_mapping_utf32(),
        ))
        .unwrap()
    }

    #[test]
    fn a_diagnostic_without_fix_or_note() {
        let files = files(b"entity e is end;", Encoding::Utf8);
        assert_snapshot!(to_json(&diagnostic(0..6), &files), @r#"
        {
          "message": "message",
          "severity": "warning",
          "range": {
            "start": {
              "line": 1,
              "col": 1
            },
            "end": {
              "line": 1,
              "col": 7
            }
          },
          "file": "<inline>",
          "code": "IDM001"
        }
        "#);
    }

    #[test]
    fn a_diagnostic_with_fix_and_note() {
        let files = files(b"entity e is end;", Encoding::Utf8);
        let mut diagnostic = diagnostic(0..6);
        diagnostic.set_note("a note");
        diagnostic.set_fix(Fix::safe(
            "shout",
            vec![Edit::new(0..6, b"ENTITY"), Edit::delete_raw(15..16)],
        ));
        assert_snapshot!(to_json(&diagnostic, &files), @r#"
        {
          "message": "message",
          "severity": "warning",
          "range": {
            "start": {
              "line": 1,
              "col": 1
            },
            "end": {
              "line": 1,
              "col": 7
            }
          },
          "file": "<inline>",
          "code": "IDM001",
          "fix": {
            "title": "shout",
            "edits": [
              {
                "replacement": "ENTITY",
                "range": {
                  "start": {
                    "line": 1,
                    "col": 1
                  },
                  "end": {
                    "line": 1,
                    "col": 7
                  }
                }
              },
              {
                "replacement": "",
                "range": {
                  "start": {
                    "line": 1,
                    "col": 16
                  },
                  "end": {
                    "line": 1,
                    "col": 17
                  }
                }
              }
            ],
            "applicability": "safe"
          },
          "note": "a note"
        }
        "#);
    }

    #[test]
    fn a_display_only_fix() {
        let files = files(b"entity e is end;", Encoding::Utf8);
        let mut diagnostic = diagnostic(0..6);
        diagnostic.set_fix(Fix::display_only("shout", vec![Edit::new(0..6, b"ENTITY")]));
        let json = serde_json::to_value(RenderableDiagnostic::from_diagnostic(&diagnostic, &files))
            .unwrap();
        assert_snapshot!(json["fix"]["applicability"], @r#""display-only""#);
    }

    #[test]
    fn a_latin1_replacement_is_serialized_as_utf8() {
        let files = files(b"entity e is end;", Encoding::Utf8);
        let mut diagnostic = diagnostic(0..6);
        diagnostic.set_fix(Fix::safe("umlaut", vec![Edit::new(0..6, b"\xE4")]));
        let json = serde_json::to_value(RenderableDiagnostic::from_diagnostic(&diagnostic, &files))
            .unwrap();
        assert_eq!(json["fix"]["edits"][0]["replacement"], "ä");
    }

    #[test]
    fn a_syntax_error_has_severity_error() {
        let files = files(b"entity e is", Encoding::Utf8);
        let diagnostics =
            crate::parse_and_analyze_file(files.get(FileId(0)), FileId(0), &ActiveRules::new([]))
                .into_diagnostics();
        let json = serde_json::to_value(RenderableDiagnostic::from_diagnostic(
            &diagnostics[0],
            &files,
        ))
        .unwrap();
        assert_eq!(json["severity"], "error");
        assert_eq!(json["code"], "SYX001");
    }

    #[test]
    fn positions_are_one_based_and_the_end_is_exclusive() {
        assert_snapshot!(
            entity_range(b"-- comment\n  entity e is end;", Encoding::Utf8),
            @r#"{"start":{"line":2,"col":3},"end":{"line":2,"col":9}}"#
        );
    }

    #[test]
    fn a_span_ending_at_a_newline_ends_on_the_next_line() {
        let files = files(b"entity e is end;\n", Encoding::Utf8);
        let file = files.get(FileId(0));
        assert_snapshot!(
            serde_json::to_string(&Range::from_span(&(16..17), file.source_mapping_utf32()))
                .unwrap(),
            @r#"{"start":{"line":1,"col":17},"end":{"line":2,"col":1}}"#
        );
    }
}
