// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::SrcPos;
use crate::data::error_codes::{ErrorCode, SeverityMap};
use std::convert::{AsRef, Into};
use strum::{EnumString, IntoStaticStr};

#[derive(PartialEq, Debug, Clone, Copy, Eq, Hash, EnumString, IntoStaticStr)]
#[strum(serialize_all = "snake_case")]
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
    pub related: Vec<(SrcPos, String)>,
    pub code: ErrorCode,
}

impl Diagnostic {
    pub fn new(item: impl AsRef<SrcPos>, msg: impl Into<String>, code: ErrorCode) -> Diagnostic {
        Diagnostic {
            pos: item.as_ref().clone(),
            message: msg.into(),
            related: vec![],
            code,
        }
    }

    pub fn when(self, message: impl AsRef<str>) -> Diagnostic {
        Diagnostic {
            message: format!("{}, when {}", &self.message, message.as_ref()),
            pos: self.pos,
            related: vec![],
            code: self.code,
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
                ErrorCode::Related,
            ));
        }
        diagnostics
    }

    pub fn show(&self, severities: &SeverityMap) -> Option<String> {
        let severity = severities[self.code]?;
        let mut result = String::new();
        for (pos, message) in self.related.iter() {
            result.push_str(&pos.show(&format!("related: {message}")));
            result.push('\n');
        }
        let severity: &str = severity.into();
        result.push_str(&self.pos.show(&format!("{}: {}", severity, self.message)));
        Some(result)
    }

    #[cfg(test)]
    pub fn show_default(&self) -> String {
        self.show(&SeverityMap::default())
            .expect("All severities should be defined in the default severity map")
    }
}

pub type DiagnosticResult<T> = Result<T, Diagnostic>;

pub trait DiagnosticHandler {
    fn push(&mut self, diagnostic: Diagnostic);
}

impl dyn DiagnosticHandler + '_ {
    pub fn add(&mut self, item: impl AsRef<SrcPos>, msg: impl Into<String>, code: ErrorCode) {
        self.push(Diagnostic::new(item, msg, code))
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

    pub fn append(&mut self, diagnostics: impl IntoIterator<Item = Diagnostic>) {
        for diagnostic in diagnostics.into_iter() {
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
    fn push(&mut self, diagnostic: Diagnostic) {
        panic!("{}", diagnostic.show_default())
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
            Diagnostic::new(code.s1("world"), "Greetings", ErrorCode::Unused).show_default(),
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
            Diagnostic::new(code.s1("world"), "Greetings", ErrorCode::SyntaxError).show_default(),
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

        let err = Diagnostic::new(code.s1("line"), "Greetings", ErrorCode::SyntaxError)
            .related(code.s1("hello"), "From here");

        assert_eq!(
            err.show_default(),
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

    #[test]
    fn do_not_show_ignored_errors() {
        let code = Code::new_with_file_name(Path::new("{unknown file}"), "hello\nworld\nline\n");
        let mut severity_map = SeverityMap::default();
        severity_map[ErrorCode::Unused] = None;

        assert_eq!(
            Diagnostic::new(code.s1("world"), "Greetings", ErrorCode::Unused).show(&severity_map),
            None
        );
    }
}
