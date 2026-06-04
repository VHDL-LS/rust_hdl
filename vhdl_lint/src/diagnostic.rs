use std::ops::Range;

/// A diagnostic emitted by the VHDL frontend.
///
/// A diagnostic holds only raw source byte spans and is
/// therefore independent of both the syntax tree it was produced from and of
/// any rendering target.
#[derive(Debug, Clone)]
pub struct Diagnostic {
    severity: Severity,
    code: DiagnosticCode,
    message: String,
    labels: Vec<Label>,
    notes: Vec<Footer>,
}

impl Diagnostic {
    /// Create a diagnostic carrying `code`'s [default severity](DiagnosticCode::default_severity).
    pub fn new(code: DiagnosticCode, message: impl Into<String>) -> Diagnostic {
        Diagnostic {
            severity: code.default_severity(),
            code,
            message: message.into(),
            labels: Vec::new(),
            notes: Vec::new(),
        }
    }

    /// Override the severity, e.g. when a lint level downgrades an error.
    pub fn with_severity(mut self, severity: Severity) -> Diagnostic {
        self.severity = severity;
        self
    }

    pub fn with_label(mut self, label: Label) -> Diagnostic {
        self.labels.push(label);
        self
    }

    pub fn with_labels(mut self, labels: impl IntoIterator<Item = Label>) -> Diagnostic {
        self.labels.extend(labels);
        self
    }

    pub fn with_note(mut self, note: Footer) -> Diagnostic {
        self.notes.push(note);
        self
    }

    pub fn severity(&self) -> Severity {
        self.severity
    }

    pub fn code(&self) -> DiagnosticCode {
        self.code
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    pub fn labels(&self) -> &[Label] {
        &self.labels
    }

    pub fn notes(&self) -> &[Footer] {
        &self.notes
    }

    /// The label that anchors the diagnostic in source, if any.
    pub fn primary_label(&self) -> Option<&Label> {
        self.labels
            .iter()
            .find(|label| label.kind() == LabelKind::Primary)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    Info,
    Hint,
}

/// A stable, matchable identifier for a class of diagnostic.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DiagnosticCode {
    /// A syntax error reported by the parser.
    Syntax,
}

impl DiagnosticCode {
    /// The severity a diagnostic with this code carries unless overridden.
    pub fn default_severity(&self) -> Severity {
        match self {
            DiagnosticCode::Syntax => Severity::Error,
        }
    }
}

/// Opaque handle to a source file
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SourceId(u64);

impl SourceId {
    pub fn new(id: u64) -> SourceId {
        SourceId(id)
    }

    pub fn raw(&self) -> u64 {
        self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LabelKind {
    /// The location the diagnostic is primarily about.
    Primary,
    /// A secondary location adding context to the primary one.
    Context,
}

/// A labelled source location
#[derive(Debug, Clone)]
pub struct Label {
    source: SourceId,
    span: Range<usize>,
    kind: LabelKind,
    message: String,
}

impl Label {
    pub fn new(
        kind: LabelKind,
        source: SourceId,
        span: Range<usize>,
        message: impl Into<String>,
    ) -> Label {
        Label {
            source,
            span,
            kind,
            message: message.into(),
        }
    }

    pub fn primary(source: SourceId, span: Range<usize>, message: impl Into<String>) -> Label {
        Label::new(LabelKind::Primary, source, span, message)
    }

    pub fn context(source: SourceId, span: Range<usize>, message: impl Into<String>) -> Label {
        Label::new(LabelKind::Context, source, span, message)
    }

    pub fn source(&self) -> SourceId {
        self.source
    }

    pub fn span(&self) -> &Range<usize> {
        &self.span
    }

    pub fn kind(&self) -> LabelKind {
        self.kind
    }

    pub fn message(&self) -> &str {
        &self.message
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NoteKind {
    /// Neutral additional information.
    Note,
    /// Advice on how to resolve the diagnostic.
    Help,
}

/// A footer message attached to a diagnostic, not tied to a source location.
#[derive(Debug, Clone)]
pub struct Footer {
    kind: NoteKind,
    message: String,
}

impl Footer {
    pub fn note(message: impl Into<String>) -> Footer {
        Footer {
            kind: NoteKind::Note,
            message: message.into(),
        }
    }

    pub fn help(message: impl Into<String>) -> Footer {
        Footer {
            kind: NoteKind::Help,
            message: message.into(),
        }
    }

    pub fn kind(&self) -> NoteKind {
        self.kind
    }

    pub fn message(&self) -> &str {
        &self.message
    }
}
