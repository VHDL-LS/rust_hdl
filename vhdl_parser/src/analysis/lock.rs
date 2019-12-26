// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

///! This module contains structs to handle detecting circular dependencies
///! during analysis of packages where the dependency tree is not known
use std::sync::{RwLock, RwLockReadGuard, RwLockWriteGuard};

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
        let guard = self.data.read().unwrap();
        if guard.done {
            Some(ReadGuard { guard })
        } else {
            None
        }
    }

    pub fn read(&self) -> ReadGuard<T> {
        ReadGuard {
            guard: self.data.read().unwrap(),
        }
    }

    /// Reset analysis state, analysis needs to be redone
    pub fn reset(&self, reset_fun: &impl Fn(&mut T)) {
        let mut guard = self.data.try_write().unwrap();
        guard.done = false;
        reset_fun(&mut guard.data);
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
    pub fn entry(&self) -> AnalysisEntry<T> {
        if let Some(guard) = self.get() {
            AnalysisEntry::Occupied(guard)
        } else {
            let guard = self.data.write().unwrap();

            if guard.done {
                // Already analyzed, convert to read lock
                // @TODO investigate parking_lot which supports atomic downgrade
                drop(guard); // Drop before taking read lock to avoid deadlock
                let guard = ReadGuard {
                    guard: self.data.read().unwrap(),
                };

                AnalysisEntry::Occupied(guard)
            } else {
                let guard = WriteGuard { guard };
                AnalysisEntry::Vacant(guard)
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
            AnalysisEntry::Vacant(mut entry) => {
                *entry = 2;
            }
            _ => panic!("Expected Vacant entry"),
        };

        match lock.entry() {
            AnalysisEntry::Occupied(entry) => {
                assert_eq!(*entry, 2);
            }
            _ => panic!("Expected Occupied entry"),
        };

        assert_eq!(*lock.get().unwrap(), 2);
    }
}
