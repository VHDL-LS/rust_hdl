use std::fmt;

use annotate_snippets::Level;

#[derive(Copy, Clone, Debug, serde::Serialize)]
#[serde(rename_all = "kebab-case")]
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

impl fmt::Display for Severity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Severity::Warning => "warning",
            Severity::Error => "error",
            Severity::Info => "info",
            Severity::Note => "note",
        })
    }
}
