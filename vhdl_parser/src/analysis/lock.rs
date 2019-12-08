// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

///! This module contains structs to handle detecting circular dependencies
///! during analysis of packages where the dependency tree is not known
use std::sync::{RwLock, RwLockReadGuard, RwLockWriteGuard};

use crate::diagnostic::{Diagnostic, DiagnosticHandler};
use crate::source::SrcPos;

#[derive(Clone, Debug, Default)]
#[cfg_attr(test, derive(PartialEq))]
#[must_use]
pub struct CircularDependencyError {
    references: Vec<SrcPos>,
}

impl CircularDependencyError {
    /// Return self to enforce #[must_use]
    pub fn add_reference(mut self, location: &SrcPos) -> CircularDependencyError {
        self.references.push(location.clone());
        self
    }

    pub fn push_into(self, diagnostics: &mut dyn DiagnosticHandler) {
        assert!(!self.references.is_empty());
        for pos in self.references {
            diagnostics.push(Diagnostic::error(
                pos,
                format!("Found circular dependency",),
            ));
        }
    }
}

struct AnalysisState<T> {
    done: bool,
    data: T,
}

pub struct AnalysisLock<T> {
    data: RwLock<AnalysisState<T>>,
}

/// Ensure mutable access during analysis and immutable access after analysis
/// Also detect circular dependencies
///
/// Lock mechanism will not work for multi threaded analysis
/// since deadlocks can occur. Multi treaded datat types are used
/// anyway since I plan to fix this in the future
impl<T> AnalysisLock<T> {
    pub fn new(data: T) -> AnalysisLock<T> {
        AnalysisLock {
            data: RwLock::new(AnalysisState { done: false, data }),
        }
    }

    /// Get an immutable reference to the data if it is already been analyzed
    fn get(&self) -> Option<ReadGuard<T>> {
        match self.data.try_read() {
            Ok(guard) => {
                if guard.done {
                    Some(ReadGuard { guard })
                } else {
                    None
                }
            }
            Err(std::sync::TryLockError::WouldBlock) => None,
            Err(err) => panic!("{:?}", err),
        }
    }

    /// Expect non-blocking read of data
    #[cfg(test)]
    pub fn expect_read(&self) -> ReadGuard<T> {
        ReadGuard {
            guard: self.data.try_read().expect("Expect non-blocking read"),
        }
    }

    /// Get a mutable reference to the data if it has not already been analyzed
    fn get_mut(&self) -> Result<Option<WriteGuard<T>>, CircularDependencyError> {
        match self.data.try_write() {
            Ok(guard) => {
                if guard.done {
                    Ok(None)
                } else {
                    Ok(Some(WriteGuard { guard }))
                }
            }
            Err(std::sync::TryLockError::WouldBlock) => Err(CircularDependencyError::default()),
            Err(err) => panic!("{:?}", err),
        }
    }

    /// Get an immmutable reference to the data, assuming it has already been analyzed
    pub fn expect_analyzed(&self) -> ReadGuard<T> {
        let guard = self.data.read().unwrap();

        if !guard.done {
            panic!("Expected analysis to have already been done");
        }

        ReadGuard { guard }
    }

    /// Get either:
    /// - A mutable reference to the data if not analyzed
    /// - An immmutable reference to the data if already analyzed
    /// - A Circular dependency error if such is detected
    pub fn entry(&self) -> Result<AnalysisEntry<T>, CircularDependencyError> {
        if let Some(guard) = self.get() {
            Ok(AnalysisEntry::Occupied(guard))
        } else {
            match self.get_mut() {
                // Not yet analyzed and not locked
                Ok(Some(guard)) => Ok(AnalysisEntry::Vacant(guard)),
                // Just became analyzed, get a read lock
                Ok(None) => {
                    let guard = ReadGuard {
                        guard: self.data.read().unwrap(),
                    };
                    Ok(AnalysisEntry::Occupied(guard))
                }
                // Circular dependency
                Err(err) => Err(err),
            }
        }
    }
}

pub enum AnalysisEntry<'a, T> {
    Occupied(ReadGuard<'a, T>),
    Vacant(WriteGuard<'a, T>),
}

pub struct ReadGuard<'a, T> {
    guard: RwLockReadGuard<'a, AnalysisState<T>>,
}

impl<'a, T> std::ops::Deref for ReadGuard<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.guard.data
    }
}

pub struct WriteGuard<'a, T> {
    guard: RwLockWriteGuard<'a, AnalysisState<T>>,
}

impl<'a, T> std::ops::Deref for WriteGuard<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.guard.data
    }
}

impl<'a, T> std::ops::DerefMut for WriteGuard<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.guard.data
    }
}

impl<'a, T> Drop for WriteGuard<'a, T> {
    fn drop(&mut self) {
        self.guard.done = true;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn analysis_result_is_memoized() {
        let lock = AnalysisLock::new(1);

        match lock.entry() {
            Ok(AnalysisEntry::Vacant(mut entry)) => {
                *entry = 2;
            }
            _ => panic!("Expected Vacant entry"),
        };

        match lock.entry() {
            Ok(AnalysisEntry::Occupied(entry)) => {
                assert_eq!(*entry, 2);
            }
            _ => panic!("Expected Occupied entry"),
        };

        assert_eq!(*lock.get().unwrap(), 2);
    }

    #[test]
    fn circular_dependencies_are_detected() {
        let data = 1;
        let lock = AnalysisLock::new(data);

        let entry1 = lock.entry();
        let entry2 = lock.entry();

        match entry1 {
            Ok(AnalysisEntry::Vacant(mut entry)) => {
                *entry = 2;
            }
            _ => {
                panic!("Expected Vacant entry");
            }
        }

        match entry2 {
            Err(err) => {
                assert_eq!(err, CircularDependencyError::default());
            }
            _ => {
                panic!("Expected Circular dependency error");
            }
        }

        assert_eq!(*lock.get().unwrap(), 2);
    }
}
