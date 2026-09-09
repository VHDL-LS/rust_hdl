use annotate_snippets::{AnnotationKind, Group, Level, Patch, Snippet};
use vhdl_syntax::{parser::error::SyntaxErr, text::source_loc::EncodedSpan};

use crate::{
    error_code::{Category, ErrorCode},
    fix::Fix,
    severity::Severity,
    source_loc::SourceLoc,
    FileId, FileStore,
};

const SYNTAX_ERROR: ErrorCode = ErrorCode::new(Category::Syntax, 1);

#[derive(Debug)]
pub struct Diagnostic {
    message: String,
    severity: Severity,
    loc: SourceLoc,
    code: ErrorCode,
    fix: Option<Fix>,
    note: Option<String>,
}

impl Diagnostic {
    pub fn new(
        message: impl Into<String>,
        severity: Severity,
        loc: SourceLoc,
        code: ErrorCode,
    ) -> Diagnostic {
        Diagnostic {
            message: message.into(),
            severity,
            loc,
            code,
            fix: None,
            note: None,
        }
    }

    pub fn severity(&self) -> Severity {
        self.severity
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    pub fn loc(&self) -> &SourceLoc {
        &self.loc
    }

    pub fn code(&self) -> &ErrorCode {
        &self.code
    }

    pub fn fix(&self) -> Option<&Fix> {
        self.fix.as_ref()
    }

    pub fn note(&self) -> Option<&str> {
        self.note.as_deref()
    }

    pub fn set_fix(&mut self, fix: Fix) {
        self.fix = Some(fix)
    }

    pub fn set_note(&mut self, note: impl Into<String>) {
        self.note = Some(note.into())
    }

    pub fn from_syntax_err(err: SyntaxErr, source: FileId) -> Diagnostic {
        Diagnostic {
            message: err.err().to_string(),
            severity: Severity::Error,
            loc: SourceLoc::new(source, err.span().clone()),
            code: SYNTAX_ERROR,
            fix: None,
            note: None,
        }
    }

    /// Render this diagnostic
    pub fn render<'a>(&'a self, files: &'a FileStore) -> Group<'a> {
        let file = files.get(self.loc.file());
        let text = file.utf8_contents();
        let slc = file.source_mapping();
        let to_span = |span: EncodedSpan| span.start().raw()..span.end().raw();
        let mut group = self
            .severity()
            .to_level()
            .primary_title(self.message())
            .id(self.code().to_string())
            .element(
                Snippet::source(text)
                    .path(file.path().to_string_lossy())
                    .line_start(1)
                    .annotation(
                        AnnotationKind::Primary
                            .span(to_span(slc.convert_byte_span(self.loc().span()))),
                    ),
            );
        if let Some(note) = self.note() {
            group = group.element(Level::NOTE.message(note));
        }
        if let Some(fix) = self.fix() {
            let level = if fix.is_safe() {
                Level::HELP
            } else {
                Level::HELP.with_name(Some("suggestion"))
            };
            group =
                group
                    .element(level.message(fix.title()))
                    .element(
                        Snippet::source(text).patches(fix.edits().iter().map(|edit| {
                            Patch::new(
                                to_span(slc.convert_byte_span(edit.span())),
                                edit.replacement().to_str(),
                            )
                        })),
                    );
        }
        group
    }
}

pub fn render_diagnostics<'a>(
    diagnostics: &'a [Diagnostic],
    files: &'a FileStore,
) -> impl Iterator<Item = Group<'a>> {
    diagnostics.iter().map(move |diag| diag.render(files))
}

#[cfg(test)]
mod tests {
    use annotate_snippets::Renderer;
    use insta::assert_snapshot;

    use super::*;
    use crate::{fix::Edit, Encoding, FileSettings};

    const SOURCE: &str = "entity e is end;";

    fn render_with_fix(fix: Fix) -> String {
        let mut files = FileStore::new();
        files.insert(
            "<inline>",
            SOURCE.as_bytes().to_vec(),
            crate::FileSettings::default(),
        );
        let mut diagnostic = Diagnostic::new(
            "message",
            Severity::Warning,
            SourceLoc::new(FileId(0), 0..6),
            ErrorCode::new(Category::Idiom, 1),
        );
        diagnostic.set_fix(fix);
        Renderer::plain().render(&[diagnostic.render(&files)])
    }

    #[test]
    fn a_fix_that_fix_applies_is_rendered_as_help() {
        let rendered = render_with_fix(Fix::safe("do it", vec![Edit::delete_raw(0..6)]));
        assert!(rendered.contains("help: do it"), "{rendered}");
    }

    #[test]
    fn a_fix_that_fix_does_not_apply_is_rendered_as_a_suggestion() {
        let rendered = render_with_fix(Fix::display_only("do it", vec![Edit::delete_raw(0..6)]));
        assert!(rendered.contains("suggestion: do it"), "{rendered}");
        assert!(!rendered.contains("help:"), "{rendered}");
    }

    /// Renders a diagnostic on the first occurrence of `entity` in `source`,
    /// with a fix that replaces it by `ENTITY`.
    fn render_non_ascii(source: &[u8], encoding: Encoding) -> String {
        let start = source
            .windows(b"entity".len())
            .position(|window| window == b"entity")
            .unwrap();
        let span = start..start + b"entity".len();
        let mut files = FileStore::new();
        let id = files.insert(
            "<inline>",
            source.to_vec(),
            FileSettings {
                encoding,
                ..FileSettings::default()
            },
        );
        let mut diagnostic = Diagnostic::new(
            "message",
            Severity::Warning,
            SourceLoc::new(id, span.clone()),
            ErrorCode::new(Category::Idiom, 1),
        );
        diagnostic.set_fix(Fix::safe("shout", vec![Edit::new(span, b"ENTITY")]));
        Renderer::plain()
            .anonymized_line_numbers(true)
            .render(&[diagnostic.render(&files)])
    }

    #[test]
    fn a_utf8_comment_before_the_span_is_rendered_as_utf8() {
        assert_snapshot!(
            render_non_ascii("/* ä€💣 */ entity e is end;".as_bytes(), Encoding::Utf8),
            @"
        warning[IDM001]: message
          --> <inline>:1:11
           |
        LL | /* ä€💣 */ entity e is end;
           |            ^^^^^^
           |
           = help: shout
           |
        LL - /* ä€💣 */ entity e is end;
        LL + /* ä€💣 */ ENTITY e is end;
           |
        "
        );
    }

    #[test]
    fn a_latin1_comment_before_the_span_is_rendered_as_latin1() {
        assert_snapshot!(
            render_non_ascii(b"/* \xE4\xF6\xFC */ entity e is end;", Encoding::Latin1),
            @"
        warning[IDM001]: message
          --> <inline>:1:11
           |
        LL | /* äöü */ entity e is end;
           |           ^^^^^^
           |
           = help: shout
           |
        LL - /* äöü */ entity e is end;
        LL + /* äöü */ ENTITY e is end;
           |
        "
        );
    }

    #[test]
    fn an_invalid_utf8_comment_before_the_span_is_rendered_with_replacement_characters() {
        assert_snapshot!(
            render_non_ascii(b"/* \xE4\xF6\xFC */ entity e is end;", Encoding::Utf8),
            @"
        warning[IDM001]: message
          --> <inline>:1:11
           |
        LL | /* ��� */ entity e is end;
           |           ^^^^^^
           |
           = help: shout
           |
        LL - /* ��� */ entity e is end;
        LL + /* ��� */ ENTITY e is end;
           |
        "
        );
    }

    #[test]
    fn a_latin1_literal_before_the_span_is_rendered_as_latin1_in_either_encoding() {
        let source =
            b"-- \xE4\npackage p is constant c : string := \"\xE4\"; end; entity e is end;";
        assert_snapshot!(render_non_ascii(source, Encoding::Latin1), @r#"
        warning[IDM001]: message
          --> <inline>:2:47
           |
        LL | package p is constant c : string := "ä"; end; entity e is end;
           |                                               ^^^^^^
           |
           = help: shout
           |
        LL - package p is constant c : string := "ä"; end; entity e is end;
        LL + package p is constant c : string := "ä"; end; ENTITY e is end;
           |
        "#);
        assert_snapshot!(render_non_ascii(source, Encoding::Utf8), @r#"
        warning[IDM001]: message
          --> <inline>:2:47
           |
        LL | package p is constant c : string := "ä"; end; entity e is end;
           |                                               ^^^^^^
           |
           = help: shout
           |
        LL - package p is constant c : string := "ä"; end; entity e is end;
        LL + package p is constant c : string := "ä"; end; ENTITY e is end;
           |
        "#);
    }
}
