// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

use super::region::Region;
use super::root::DependencyRecorder;
use crate::data::*;

pub struct AnalyzeContext<'a> {
    pub symtab: &'a SymbolTable,
    pub work_sym: Symbol,
    pub work_library_name: Symbol,
    pub std_sym: Symbol,
    pub standard_sym: Symbol,
    pub root: DependencyRecorder<'a>,
}

impl<'a> AnalyzeContext<'a> {
    pub fn new(
        root: DependencyRecorder<'a>,
        work_library_name: Symbol,
        symtab: &'a SymbolTable,
    ) -> AnalyzeContext<'a> {
        AnalyzeContext {
            work_sym: symtab.insert(&Latin1String::new(b"work")),
            work_library_name,
            std_sym: symtab.insert(&Latin1String::new(b"std")),
            standard_sym: symtab.insert(&Latin1String::new(b"standard")),
            symtab,
            root,
        }
    }
}

pub trait Analyze {
    fn analyze(
        &mut self,
        context: &AnalyzeContext,
        root_region: &mut Region<'_>,
        region: &mut Region<'_>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalNullResult;
}

pub enum AnalysisError {
    Fatal(CircularDependencyError),
    NotFatal(Diagnostic),
}

#[derive(Clone, Debug)]
#[must_use]
pub struct CircularDependencyError {
    reference: Option<SrcPos>,
}

impl CircularDependencyError {
    pub fn new(reference: Option<&SrcPos>) -> CircularDependencyError {
        CircularDependencyError {
            reference: reference.cloned(),
        }
    }

    pub fn push_into(self, diagnostics: &mut dyn DiagnosticHandler) {
        if let Some(pos) = self.reference {
            diagnostics.push(Diagnostic::error(
                pos,
                format!("Found circular dependency",),
            ));
        }
    }
}

pub type AnalysisResult<T> = Result<T, AnalysisError>;
pub type FatalResult<T> = Result<T, CircularDependencyError>;
pub type FatalNullResult = FatalResult<()>;

impl From<CircularDependencyError> for AnalysisError {
    fn from(err: CircularDependencyError) -> AnalysisError {
        AnalysisError::Fatal(err)
    }
}

impl From<Diagnostic> for AnalysisError {
    fn from(diagnostic: Diagnostic) -> AnalysisError {
        AnalysisError::NotFatal(diagnostic)
    }
}

impl AnalysisError {
    // Add Non-fatal error to diagnostics or return fatal error
    pub fn add_to(self, diagnostics: &mut dyn DiagnosticHandler) -> FatalNullResult {
        match self {
            AnalysisError::Fatal(err) => Err(err),
            AnalysisError::NotFatal(diag) => {
                diagnostics.push(diag);
                Ok(())
            }
        }
    }
}
