// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::SrcPos;
use std::convert::{AsRef, Into};

#[derive(PartialEq, Debug, Clone, Copy, Eq, Hash)]
pub enum Severity {
    Hint,
    Info,
    Warning,
    Error,
}

#[must_use]
#[derive(PartialEq, Debug, Clone, Eq, Hash)]
pub struct Diagnostic {
    pub pos: SrcPos,
    pub message: String,
    pub severity: Severity,
    pub related: Vec<(SrcPos, String)>,
}

impl Diagnostic {
    pub fn new(item: impl AsRef<SrcPos>, msg: impl Into<String>, severity: Severity) -> Diagnostic {
        Diagnostic {
            pos: item.as_ref().clone(),
            message: msg.into(),
            severity,
            related: vec![],
        }
    }

    pub fn error(item: impl AsRef<SrcPos>, msg: impl Into<String>) -> Diagnostic {
        Self::new(item, msg, Severity::Error)
    }

    pub fn warning(item: impl AsRef<SrcPos>, msg: impl Into<String>) -> Diagnostic {
        Self::new(item, msg, Severity::Warning)
    }

    pub fn hint(item: impl AsRef<SrcPos>, msg: impl Into<String>) -> Diagnostic {
        Self::new(item, msg, Severity::Hint)
    }

    pub fn info(item: impl AsRef<SrcPos>, msg: impl Into<String>) -> Diagnostic {
        Self::new(item, msg, Severity::Info)
    }

    pub fn when(self, message: impl AsRef<str>) -> Diagnostic {
        Diagnostic {
            message: format!("{}, when {}", &self.message, message.as_ref()),
            pos: self.pos,
            severity: self.severity,
            related: vec![],
        }
    }

    pub fn related(self, item: impl AsRef<SrcPos>, message: impl Into<String>) -> Diagnostic {
        let mut diagnostic = self;
        diagnostic.add_related(item, message);
        diagnostic
    }

    pub fn opt_related(
        self,
        item: Option<impl AsRef<SrcPos>>,
        message: impl Into<String>,
    ) -> Diagnostic {
        let mut diagnostic = self;
        if let Some(item) = item {
            diagnostic.add_related(item, message);
        }
        diagnostic
    }

    pub fn add_related(&mut self, item: impl AsRef<SrcPos>, message: impl Into<String>) {
        self.related
            .push((item.as_ref().to_owned(), message.into()));
    }

    pub fn drain_related(&mut self) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::with_capacity(self.related.len());
        let related = std::mem::take(&mut self.related);
        for (pos, msg) in related {
            diagnostics.push(Diagnostic::new(
                pos,
                format!("related: {msg}"),
                Severity::Hint,
            ));
        }
        diagnostics
    }

    pub fn show(&self) -> String {
        let mut result = String::new();
        for (pos, message) in self.related.iter() {
            result.push_str(&pos.show(&format!("related: {message}")));
            result.push('\n');
        }
        let severity = match self.severity {
            Severity::Error => &"error",
            Severity::Warning => &"warning",
            Severity::Info => &"info",
            Severity::Hint => &"hint",
        };
        result.push_str(&self.pos.show(&format!("{}: {}", severity, self.message)));
        result
    }
}

pub type DiagnosticResult<T> = Result<T, Diagnostic>;

pub trait DiagnosticHandler {
    fn push(&mut self, diagnostic: Diagnostic);
    fn append(&mut self, diagnostics: Vec<Diagnostic>) {
        for diagnostic in diagnostics.into_iter() {
            self.push(diagnostic);
        }
    }
}

impl<'a> dyn DiagnosticHandler + 'a {
    pub fn error(&mut self, item: impl AsRef<SrcPos>, msg: impl Into<String>) {
        self.push(Diagnostic::error(item, msg));
    }

    pub fn warning(&mut self, item: impl AsRef<SrcPos>, msg: impl Into<String>) {
        self.push(Diagnostic::warning(item, msg));
    }

    pub fn hint(&mut self, item: impl AsRef<SrcPos>, msg: impl Into<String>) {
        self.push(Diagnostic::hint(item, msg));
    }

    pub fn info(&mut self, item: impl AsRef<SrcPos>, msg: impl Into<String>) {
        self.push(Diagnostic::info(item, msg));
    }

    pub fn push_result<T>(&mut self, diagnostic: Result<T, Diagnostic>) {
        if let Err(diagnostic) = diagnostic {
            self.push(diagnostic);
        }
    }

    pub fn push_some(&mut self, diagnostic: Option<Diagnostic>) {
        if let Some(diagnostic) = diagnostic {
            self.push(diagnostic);
        }
    }
}

impl DiagnosticHandler for Vec<Diagnostic> {
    fn push(&mut self, diagnostic: Diagnostic) {
        self.push(diagnostic)
    }
}

pub struct NullDiagnostics;

impl DiagnosticHandler for NullDiagnostics {
    fn push(&mut self, _diagnostic: Diagnostic) {
        // Ignore
    }
}

#[cfg(test)]
pub struct NoDiagnostics;

#[cfg(test)]
impl DiagnosticHandler for NoDiagnostics {
    fn push(&mut self, diagnostic: crate::Diagnostic) {
        panic!("{}", diagnostic.show())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::test::Code;
    use std::path::Path;

    #[test]
    fn show_warning() {
        let code = Code::new_with_file_name(Path::new("{unknown file}"), "hello\nworld\nline\n");
        assert_eq!(
            Diagnostic::warning(code.s1("world"), "Greetings").show(),
            "\
warning: Greetings
  --> {unknown file}:2
   |
1  |  hello
2 --> world
   |  ~~~~~
3  |  line
"
        );
    }

    #[test]
    fn show_error() {
        let code = Code::new_with_file_name(Path::new("{unknown file}"), "hello\nworld\nline\n");
        assert_eq!(
            Diagnostic::error(code.s1("world"), "Greetings").show(),
            "\
error: Greetings
  --> {unknown file}:2
   |
1  |  hello
2 --> world
   |  ~~~~~
3  |  line
"
        );
    }

    #[test]
    fn show_related() {
        let code = Code::new_with_file_name(Path::new("{unknown file}"), "hello\nworld\nline\n");

        let err =
            Diagnostic::error(code.s1("line"), "Greetings").related(code.s1("hello"), "From here");

        assert_eq!(
            err.show(),
            "\
related: From here
  --> {unknown file}:1
   |
1 --> hello
   |  ~~~~~
2  |  world
3  |  line

error: Greetings
  --> {unknown file}:3
   |
1  |  hello
2  |  world
3 --> line
   |  ~~~~
"
        );
    }
}
