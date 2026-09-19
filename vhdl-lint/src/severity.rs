use annotate_snippets::Level;

#[derive(Copy, Clone, Debug)]
pub enum Severity {
    Warning,
    Error,
    Info,
    Note,
}

impl Severity {
    pub fn to_level(&self) -> Level<'static> {
        match self {
            Severity::Warning => Level::WARNING,
            Severity::Error => Level::ERROR,
            Severity::Info => Level::INFO,
            Severity::Note => Level::NOTE,
        }
    }
}
