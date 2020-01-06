// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

///! This module contains structs to handle detecting circular dependencies
///! during analysis of packages where the dependency tree is not known
use parking_lot::{MappedRwLockReadGuard, RwLock, RwLockReadGuard, RwLockWriteGuard};

struct AnalysisState<T, R> {
    result: Option<R>,
    data: T,
}

pub struct AnalysisLock<T, R> {
    state: RwLock<AnalysisState<T, R>>,
}

/// Ensure mutable access during analysis and immutable access after analysis
impl<T, R> AnalysisLock<T, R> {
    pub fn new(data: T) -> AnalysisLock<T, R> {
        AnalysisLock {
            state: RwLock::new(AnalysisState { result: None, data }),
        }
    }

    /// Get an immutable reference to the data if it is already been analyzed
    fn get(&self) -> Option<ReadGuard<T, R>> {
        let guard = self.state.read();
        if guard.result.is_some() {
            Some(ReadGuard { guard })
        } else {
            None
        }
    }

    pub fn read(&self) -> MappedRwLockReadGuard<'_, T> {
        RwLockReadGuard::map(self.state.read(), |data| &data.data)
    }

    /// Reset analysis state, analysis needs to be redone
    pub fn reset(&self) {
        let mut guard = self.state.write();
        guard.result = None;
    }

    /// Get an immmutable reference to the data, assuming it has already been analyzed
    pub fn expect_analyzed(&self) -> ReadGuard<T, R> {
        let guard = self.state.read();

        if guard.result.is_none() {
            panic!("Expected analysis to have already been done");
        }

        ReadGuard { guard }
    }

    /// Get either:
    /// - A mutable reference to the data if not analyzed
    /// - An immmutable reference to the data if already analyzed
    /// - A Circular dependency error if such is detected
    pub fn entry(&self) -> AnalysisEntry<T, R> {
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

pub enum AnalysisEntry<'a, T, R> {
    Occupied(ReadGuard<'a, T, R>),
    Vacant(WriteGuard<'a, T, R>),
}

pub struct ReadGuard<'a, T, R> {
    guard: RwLockReadGuard<'a, AnalysisState<T, R>>,
}

impl<'a, T, R> ReadGuard<'a, T, R> {
    pub fn result(&self) -> &R {
        self.guard.result.as_ref().unwrap()
    }
}

impl<'a, T, R> std::ops::Deref for ReadGuard<'a, T, R> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.guard.data
    }
}

pub struct WriteGuard<'a, T, R> {
    guard: RwLockWriteGuard<'a, AnalysisState<T, R>>,
}

impl<'a, T, R> WriteGuard<'a, T, R> {
    pub fn finish(mut self, result: R) -> ReadGuard<'a, T, R> {
        self.guard.result = Some(result);
        ReadGuard {
            guard: RwLockWriteGuard::downgrade(self.guard),
        }
    }
}

impl<'a, T, R> std::ops::Deref for WriteGuard<'a, T, R> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.guard.data
    }
}

impl<'a, T, R> std::ops::DerefMut for WriteGuard<'a, T, R> {
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
                assert_eq!(*entry.result(), 1.0);
            }
            _ => panic!("Expected Occupied entry"),
        };

        assert_eq!(*lock.get().unwrap(), 2);
        assert_eq!(*lock.get().unwrap().result(), 1.0);

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
