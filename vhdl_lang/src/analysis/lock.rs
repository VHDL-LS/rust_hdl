// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

//! This module contains types to handle the analysis data in a thread-safe way,
//! in particular when the dependencies between design units are not known.

use parking_lot::{MappedRwLockWriteGuard, RwLock, RwLockReadGuard, RwLockWriteGuard};

/// Combines an item to be analyzed (typically, a design unit) with the optional results
/// of that analysis.
struct AnalysisState<T, R> {
    /// Data gathered during analysis; `None` while not yet analyzed.
    result: Option<R>,

    /// The subject of analysis; typically, this is a design unit.
    data: T,
}

/// A thread-safe r/w-lock on an item to be analyzed (`T`) and the analysis results (`R`).
///
/// Ensures mutable access during analysis and immutable access after analysis.
pub struct AnalysisLock<T, R> {
    state: RwLock<AnalysisState<T, R>>,
}

impl<T, R> AnalysisLock<T, R> {
    pub fn new(data: T) -> AnalysisLock<T, R> {
        AnalysisLock {
            state: RwLock::new(AnalysisState { result: None, data }),
        }
    }

    /// Returns an immutable reference to the data and result if it has already been analyzed.
    pub fn get(&self) -> Option<ReadGuard<'_, T, R>> {
        let guard = self.state.read();
        if guard.result.is_some() {
            Some(ReadGuard { guard })
        } else {
            None
        }
    }

    // Has been analyzed
    pub fn is_analyzed(&self) -> bool {
        self.get().is_some()
    }

    /// Returns an mutable reference to the data.
    pub fn write(&self) -> MappedRwLockWriteGuard<'_, T> {
        RwLockWriteGuard::map(self.state.write(), |data| &mut data.data)
    }

    /// Reset analysis state, analysis needs to be redone.
    pub fn reset(&self) {
        let mut guard = self.state.write();
        guard.result = None;
    }

    /// Returns an immmutable reference to the data and result.
    ///
    /// Panics if the analysis result is not available.
    pub fn expect_analyzed(&self) -> ReadGuard<'_, T, R> {
        let guard = self.state.read();

        if guard.result.is_none() {
            panic!("Expected analysis to have already been done");
        }

        ReadGuard { guard }
    }

    /// Creates a view into this lock.
    ///
    /// This view provides:
    /// - a mutable reference to the data if not analyzed
    /// - an immmutable reference to the data if already analyzed
    pub fn entry(&self) -> AnalysisEntry<'_, T, R> {
        if let Some(guard) = self.get() {
            AnalysisEntry::Occupied(guard)
        } else {
            let guard = self.state.write();

            if guard.result.is_some() {
                let guard = ReadGuard {
                    guard: RwLockWriteGuard::downgrade(guard),
                };

                AnalysisEntry::Occupied(guard)
            } else {
                let guard = WriteGuard { guard };
                AnalysisEntry::Vacant(guard)
            }
        }
    }
}

/// A view into a thread-safe r/w-lock on an [`AnalysisState`](struct.AnalysisState.html).
///
/// Instances of this type are created by
/// [`AnalysisLock::entry()`](struct.AnalysisLock.html#method.entry)
/// and allow read-access to a completed analysis data and
/// read/write-access to incomplete analysis data.
pub enum AnalysisEntry<'a, T, R> {
    Occupied(ReadGuard<'a, T, R>),
    Vacant(WriteGuard<'a, T, R>),
}

pub struct ReadGuard<'a, T, R> {
    guard: RwLockReadGuard<'a, AnalysisState<T, R>>,
}

impl<T, R> ReadGuard<'_, T, R> {
    pub fn result(&self) -> &R {
        self.guard.result.as_ref().unwrap()
    }

    pub fn data(&self) -> &T {
        &self.guard.data
    }
}

impl<T, R> std::ops::Deref for ReadGuard<'_, T, R> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.guard.data
    }
}

pub struct WriteGuard<'a, T, R> {
    guard: RwLockWriteGuard<'a, AnalysisState<T, R>>,
}

impl<'a, T, R> WriteGuard<'a, T, R> {
    pub fn finish(&mut self, result: R) {
        self.guard.result = Some(result);
    }
    pub fn downgrade(self) -> ReadGuard<'a, T, R> {
        if self.guard.result.is_none() {
            panic!("Cannot downgrade unit without result");
        }
        ReadGuard {
            guard: RwLockWriteGuard::downgrade(self.guard),
        }
    }
}

impl<T, R> std::ops::Deref for WriteGuard<'_, T, R> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.guard.data
    }
}

impl<T, R> std::ops::DerefMut for WriteGuard<'_, T, R> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.guard.data
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn analysis_result_is_memoized() {
        let lock = AnalysisLock::new(1);

        match lock.entry() {
            AnalysisEntry::Vacant(mut entry) => {
                *entry = 2;
                entry.finish(1.0);
            }
            _ => panic!("Expected Vacant entry"),
        };

        match lock.entry() {
            AnalysisEntry::Occupied(entry) => {
                assert_eq!(*entry, 2);
                assert!((*entry.result() - 1.0_f64).abs() < f64::EPSILON);
            }
            _ => panic!("Expected Occupied entry"),
        };

        assert_eq!(*lock.get().unwrap(), 2);
        assert!((*lock.get().unwrap().result() - 1.0_f64).abs() < f64::EPSILON);

        // Check that lock is reset
        lock.reset();
        assert!(lock.get().is_none());

        match lock.entry() {
            AnalysisEntry::Vacant(mut entry) => {
                *entry = 2;
                entry.finish(1.0);
            }
            _ => panic!("Expected Vacant entry"),
        };
    }
}
