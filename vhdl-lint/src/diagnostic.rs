use std::path::PathBuf;

use annotate_snippets::{AnnotationKind, Group, Level, Patch, Snippet};
use vhdl_syntax::parser::error::SyntaxErr;

use crate::{
    error_code::{Category, ErrorCode},
    fix::Fix,
    severity::Severity,
    source_loc::SourceLoc,
    FileId,
};

const SYNTAX_ERROR: ErrorCode = ErrorCode::new(Category::Syntax, 1);

#[derive(Debug)]
pub struct Diagnostic {
    message: String,
    severity: Severity,
    loc: SourceLoc,
    code: ErrorCode,
    fix: Option<Fix>,
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

    pub fn set_fix(&mut self, fix: Fix) {
        self.fix = Some(fix)
    }

    pub fn from_syntax_err(err: SyntaxErr, source: FileId) -> Diagnostic {
        Diagnostic {
            message: err.err().to_string(),
            severity: Severity::Error,
            loc: SourceLoc::new(source, err.span().clone()),
            code: SYNTAX_ERROR,
            fix: None,
        }
    }

    /// Render this diagnostic against `files`, in which a file's position is its [`FileId`].
    pub fn render<'a>(&'a self, files: &'a [(PathBuf, Vec<u8>)]) -> Group<'a> {
        let (path, file_content) = files
            .get(self.loc.file().0 as usize)
            .expect("FileId should be valid");
        // Real encoding means
        // a) Differentiating between Comments and non-comments (which can have different encoding), and
        // b) Using a different source and target encoding (source may be Latin-1, target UTF-8) which requires bookkeeping of the offsets
        // vhdl_syntax supports this in principle, but this requires a more intrusive change (source loc converter, ...)
        if !file_content.is_ascii() {
            return Group::with_title(Level::ERROR.primary_title(format!(
                "File {} contains non-ASCII characters which are unsupported at the moment.",
                path.display()
            )));
        }
        let text = str::from_utf8(file_content).unwrap();
        let mut group = self
            .severity()
            .to_level()
            .primary_title(self.message())
            .id(self.code().to_string())
            .element(
                Snippet::source(text)
                    .path(path.to_string_lossy())
                    .line_start(1)
                    .annotation(AnnotationKind::Primary.span(self.loc().span().clone())),
            );
        if let Some(fix) = self.fix() {
            group = group.element(Level::HELP.message(fix.title())).element(
                Snippet::source(text).patches(fix.edits().iter().map(|edit| {
                    Patch::new(
                        edit.span().clone(),
                        str::from_utf8(edit.replacement()).expect("Linter only supports UTF-8"),
                    )
                })),
            );
        }
        group
    }
}

pub fn render_diagnostics<'a>(
    diagnostics: &'a [Diagnostic],
    files: &'a [(PathBuf, Vec<u8>)],
) -> impl Iterator<Item = Group<'a>> {
    diagnostics.iter().map(move |diag| diag.render(files))
}
