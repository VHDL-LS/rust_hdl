use vhdl_syntax::parser::error::Span;

use crate::FileId;

/// A location in a source file
#[derive(Debug)]
pub struct SourceLoc {
    file: FileId,
    span: Span,
}

impl SourceLoc {
    pub fn new(file: FileId, span: Span) -> SourceLoc {
        SourceLoc { file, span }
    }

    pub fn span(&self) -> &Span {
        &self.span
    }

    pub fn file(&self) -> FileId {
        self.file
    }
}
