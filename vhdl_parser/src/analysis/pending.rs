// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com
use fnv::FnvHashMap;
///! This module contains structs to handle detecting circular dependencies
///! during analysis of packages where the dependency tree is not known
use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::sync::Arc;

use super::declarative_region::DeclarativeRegion;
use crate::diagnostic::{Diagnostic, DiagnosticHandler};
use crate::source::SrcPos;
use crate::symbol_table::Symbol;

#[derive(Clone, Debug)]
struct Dependency {
    library_name: Symbol,
    primary_unit_name: Symbol,
    location: SrcPos,
}

#[derive(Clone, Default, Debug)]
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

#[derive(Clone, Default, Debug)]
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

pub enum StartAnalysisResult<'a> {
    AlreadyAnalyzed(AnalysisResult),
    NotYetAnalyzed(PendingAnalysis<'a>),
}

pub struct PendingAnalysis<'a> {
    context: &'a AnalysisContext,
    key: (Symbol, Symbol),
    has_entry_point: bool,
}

impl<'a> PendingAnalysis<'a> {
    fn new(
        context: &'a AnalysisContext,
        key: (Symbol, Symbol),
        has_entry_point: bool,
    ) -> PendingAnalysis<'a> {
        PendingAnalysis {
            context,
            key,
            has_entry_point,
        }
    }

    pub fn end_analysis(self, data: AnalysisData) -> AnalysisResult {
        match self
            .context
            .primary_unit_data
            .borrow_mut()
            .entry(self.key.clone())
        {
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
}

impl Drop for PendingAnalysis<'_> {
    fn drop(&mut self) {
        self.context.locked.borrow_mut().remove(&self.key);
        if self.has_entry_point {
            self.context.path.borrow_mut().pop();
        }
    }
}

/// The analysis result of the primary unit
pub struct AnalysisData {
    diagnostics: Vec<Diagnostic>,
    pub region: DeclarativeRegion<'static>,
}

// @TODO store data in library, declarative region or in analysis context?
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

pub struct AnalysisContext {
    primary_unit_data: RefCell<FnvHashMap<(Symbol, Symbol), AnalysisResult>>,
    locked: RefCell<FnvHashMap<(Symbol, Symbol), ()>>,
    path: RefCell<AnalysisPath>,
}

impl<'a> AnalysisContext {
    pub fn new() -> AnalysisContext {
        AnalysisContext {
            primary_unit_data: RefCell::new(FnvHashMap::default()),
            locked: RefCell::new(FnvHashMap::default()),
            path: RefCell::new(AnalysisPath::new()),
        }
    }

    pub fn start_analysis(
        &'a self,
        // The optional location where the design unit was used
        entry_point: Option<&SrcPos>,
        library_name: &Symbol,
        primary_unit_name: &Symbol,
    ) -> StartAnalysisResult<'a> {
        if let Some(result) = self.get_result(library_name, primary_unit_name) {
            return StartAnalysisResult::AlreadyAnalyzed(result);
        }

        let key = (library_name.clone(), primary_unit_name.clone());

        if let Some(location) = entry_point {
            self.path
                .borrow_mut()
                .push(library_name, primary_unit_name, location);
        }

        if self.locked.borrow_mut().insert(key.clone(), ()).is_some() {
            let path_result = Err(self.path.borrow().to_cicular_error());

            // All locked units will have circular dependencies
            for other_key in self.locked.borrow().keys() {
                let result = {
                    if *other_key != key {
                        Err(CircularDependencyError::default())
                    } else {
                        path_result.clone()
                    }
                };

                self.primary_unit_data
                    .borrow_mut()
                    .insert(other_key.clone(), result);
            }

            // Only provide full path error to one unit so that duplicate diagnostics are not created
            StartAnalysisResult::AlreadyAnalyzed(path_result)
        } else {
            StartAnalysisResult::NotYetAnalyzed(PendingAnalysis::new(
                self,
                key,
                entry_point.is_some(),
            ))
        }
    }

    pub fn get_result(
        &self,
        library_name: &Symbol,
        primary_unit_name: &Symbol,
    ) -> Option<AnalysisResult> {
        self.primary_unit_data
            .borrow()
            .get(&(library_name.clone(), primary_unit_name.clone()))
            .cloned()
    }
}
