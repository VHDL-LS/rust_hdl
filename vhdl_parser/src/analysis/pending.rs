// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

///! This module contains structs to handle detecting circular dependencies
///! during analysis of packages where the dependency tree is not known
use fnv::FnvHashMap;
use std::collections::hash_map::Entry;
use std::sync::{Arc, Mutex};

use super::declarative_region::DeclarativeRegion;
use crate::diagnostic::{Diagnostic, DiagnosticHandler};
use crate::source::SrcPos;
use crate::symbol_table::Symbol;

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
struct Dependency {
    library_name: Symbol,
    primary_unit_name: Symbol,
    location: SrcPos,
}

#[derive(Clone, Default, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct AnalysisPath {
    dependencies: Vec<Dependency>,
}

impl AnalysisPath {
    pub fn new() -> AnalysisPath {
        AnalysisPath {
            dependencies: Vec::new(),
        }
    }

    pub fn push(&mut self, library_name: &Symbol, primary_unit_name: &Symbol, location: &SrcPos) {
        self.dependencies.push(Dependency {
            library_name: library_name.clone(),
            primary_unit_name: primary_unit_name.clone(),
            location: location.clone(),
        })
    }

    pub fn pop(&mut self) {
        self.dependencies
            .pop()
            .expect("Pop from empty AnalysisPath");
    }

    pub fn to_cicular_error(&self) -> CircularDependencyError {
        CircularDependencyError { path: self.clone() }
    }
}

#[derive(Clone, Debug, Default)]
#[cfg_attr(test, derive(PartialEq))]
pub struct CircularDependencyError {
    path: AnalysisPath,
}

impl CircularDependencyError {
    pub fn push_into(self, diagnostics: &mut dyn DiagnosticHandler) {
        for dependency in self.path.dependencies {
            diagnostics.push(Diagnostic::error(
                dependency.location,
                format!(
                    "Found circular dependency when referencing '{}.{}'",
                    dependency.library_name, dependency.primary_unit_name
                ),
            ));
        }
    }
}

pub type AnalysisResult = Result<Arc<AnalysisData>, CircularDependencyError>;

#[cfg_attr(test, derive(Debug))]
pub enum StartAnalysisResult {
    AlreadyAnalyzed(AnalysisResult),
    NotYetAnalyzed(AnalysisLock),
}

#[cfg_attr(test, derive(Debug))]
pub struct AnalysisLock {
    context: AnalysisContext,
    key: (Symbol, Symbol),
    has_entry_point: bool,
}

impl AnalysisLock {
    fn new(
        context: &AnalysisContext,
        key: (Symbol, Symbol),
        has_entry_point: bool,
    ) -> AnalysisLock {
        AnalysisLock {
            context: context.clone(),
            key,
            has_entry_point,
        }
    }

    pub fn end_analysis(self, data: AnalysisData) -> AnalysisResult {
        self.context.end_analysis(&self.key, data)
    }
}

impl Drop for AnalysisLock {
    fn drop(&mut self) {
        self.context.unlock(&self.key, self.has_entry_point);
    }
}

/// The analysis result of the primary unit
#[cfg_attr(test, derive(Clone, PartialEq))]
pub struct AnalysisData {
    diagnostics: Vec<Diagnostic>,
    pub region: DeclarativeRegion<'static>,
}

#[cfg(test)]
impl std::fmt::Debug for AnalysisData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "AnalysisData")
    }
}

impl AnalysisData {
    pub fn new(diagnostics: Vec<Diagnostic>, region: DeclarativeRegion<'static>) -> AnalysisData {
        AnalysisData {
            diagnostics,
            region,
        }
    }

    pub fn push_to(&self, diagnostics: &mut dyn DiagnosticHandler) {
        for diagnostic in self.diagnostics.iter().cloned() {
            diagnostics.push(diagnostic);
        }
    }
}

/// AnalysisContext which cannot be freely shared
/// Intended to be wrappend in an Rc/Arc
struct UniqueAnalysisContext {
    primary_unit_data: FnvHashMap<(Symbol, Symbol), AnalysisResult>,
    locked: FnvHashMap<(Symbol, Symbol), ()>,
    path: AnalysisPath,
}

impl UniqueAnalysisContext {
    fn new() -> UniqueAnalysisContext {
        UniqueAnalysisContext {
            primary_unit_data: FnvHashMap::default(),
            locked: FnvHashMap::default(),
            path: AnalysisPath::new(),
        }
    }

    fn start_analysis(
        &mut self,
        // The optional location where the design unit was used
        entry_point: Option<&SrcPos>,
        library_name: &Symbol,
        primary_unit_name: &Symbol,
    ) -> Option<AnalysisResult> {
        if let Some(result) = self.get_result(library_name, primary_unit_name) {
            return Some(result);
        }

        let key = (library_name.clone(), primary_unit_name.clone());

        if let Some(location) = entry_point {
            self.path.push(library_name, primary_unit_name, location);
        }

        if self.locked.insert(key.clone(), ()).is_some() {
            let path_result = Err(self.path.to_cicular_error());

            // All locked units will have circular dependencies
            for other_key in self.locked.keys() {
                let result = {
                    if *other_key != key {
                        Err(CircularDependencyError::default())
                    } else {
                        path_result.clone()
                    }
                };

                self.primary_unit_data.insert(other_key.clone(), result);
            }

            // Only provide full path error to one unit so that duplicate diagnostics are not created
            Some(path_result)
        } else {
            None
        }
    }

    fn end_analysis(&mut self, key: &(Symbol, Symbol), data: AnalysisData) -> AnalysisResult {
        match self.primary_unit_data.entry(key.clone()) {
            // Analysis already done, keep old value
            Entry::Occupied(entry) => {
                // Will always be a circular dependency error
                assert!(entry.get().is_err());
                entry.get().clone()
            }
            Entry::Vacant(entry) => {
                let result = Ok(Arc::new(data));
                entry.insert(result.clone());
                result
            }
        }
    }

    fn get_result(
        &self,
        library_name: &Symbol,
        primary_unit_name: &Symbol,
    ) -> Option<AnalysisResult> {
        self.primary_unit_data
            .get(&(library_name.clone(), primary_unit_name.clone()))
            .cloned()
    }

    fn unlock(&mut self, key: &(Symbol, Symbol), has_entry_point: bool) {
        self.locked.remove(key);
        if has_entry_point {
            self.path.pop();
        }
    }
}

#[derive(Clone)]
pub struct AnalysisContext {
    context: Arc<Mutex<UniqueAnalysisContext>>,
}

#[cfg(test)]
impl std::fmt::Debug for AnalysisContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "AnalysisContext")
    }
}

impl AnalysisContext {
    pub fn new() -> AnalysisContext {
        AnalysisContext {
            context: Arc::new(Mutex::new(UniqueAnalysisContext::new())),
        }
    }

    pub fn get_result(
        &self,
        library_name: &Symbol,
        primary_unit_name: &Symbol,
    ) -> Option<AnalysisResult> {
        self.context
            .lock()
            .unwrap()
            .get_result(library_name, primary_unit_name)
    }

    pub fn start_analysis(
        &self,
        // The optional location where the design unit was used
        entry_point: Option<&SrcPos>,
        library_name: &Symbol,
        primary_unit_name: &Symbol,
    ) -> StartAnalysisResult {
        let mut context = self.context.lock().unwrap();

        if let Some(result) = context.start_analysis(entry_point, library_name, primary_unit_name) {
            StartAnalysisResult::AlreadyAnalyzed(result)
        } else {
            StartAnalysisResult::NotYetAnalyzed(AnalysisLock::new(
                self,
                (library_name.clone(), primary_unit_name.clone()),
                entry_point.is_some(),
            ))
        }
    }

    fn end_analysis(&self, key: &(Symbol, Symbol), data: AnalysisData) -> AnalysisResult {
        self.context.lock().unwrap().end_analysis(key, data)
    }

    fn unlock(&self, key: &(Symbol, Symbol), has_entry_point: bool) {
        self.context.lock().unwrap().unlock(key, has_entry_point);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analysis::declarative_region;
    use crate::diagnostic::Diagnostic;
    use crate::test_util::CodeBuilder;
    use assert_matches::assert_matches;

    fn dummy_position() -> SrcPos {
        let builder = CodeBuilder::new();
        let code = builder.code("1");
        code.s1("1").pos()
    }

    fn dummy_analysis_data() -> AnalysisData {
        AnalysisData::new(
            vec![Diagnostic::info(dummy_position(), "Unique")],
            declarative_region::DeclarativeRegion::default(),
        )
    }

    #[test]
    fn analysis_result_is_memoized() {
        let builder = CodeBuilder::new();
        let context = AnalysisContext::new();

        let lib = builder.symbol("lib");
        let pkg = builder.symbol("pkg");

        let data = dummy_analysis_data();

        assert_matches!(context.start_analysis(None, &lib, &pkg),
        StartAnalysisResult::NotYetAnalyzed(pending) => {
            assert_matches!(pending.end_analysis(data.clone()), Ok(got_data) => {
                assert_eq!(&data, got_data.as_ref());
            });
        });

        assert_matches!(context.start_analysis(None, &lib, &pkg),
        StartAnalysisResult::AlreadyAnalyzed(Ok(got_data)) => {
            assert_eq!(&data, got_data.as_ref());
        });
    }

    #[test]
    fn circular_dependencies_are_detected() {
        let builder = CodeBuilder::new();
        let context = AnalysisContext::new();

        let code = builder.code("12");
        let entry_point1 = code.s1("1").pos();
        let entry_point2 = code.s1("2").pos();
        let lib = builder.symbol("lib");
        let pkg = builder.symbol("pkg");

        let start_result1 = context.start_analysis(Some(&entry_point1), &lib, &pkg);
        let start_result2 = context.start_analysis(Some(&entry_point2), &lib, &pkg);

        let circular_error = {
            let mut path = AnalysisPath::new();
            path.push(&lib, &pkg, &entry_point1);
            path.push(&lib, &pkg, &entry_point2);
            path.to_cicular_error()
        };

        // First result should start the analysis
        assert_matches!(start_result1, StartAnalysisResult::NotYetAnalyzed(pending) => {

            // Second start analysis caused circular dependency error
            assert_matches!(start_result2, StartAnalysisResult::AlreadyAnalyzed(Err(err)) => {
                assert_eq!(err, circular_error);
            });

            // First analysis gets circular dependency error when it ends
            let data = dummy_analysis_data();
            assert_matches!(pending.end_analysis(data), Err(err) => {
                assert_eq!(err, circular_error);
            });
        });
    }
}
