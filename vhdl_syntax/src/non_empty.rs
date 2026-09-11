// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2026, Lukas Scheller lukasscheller@icloud.com

use std::num::NonZeroUsize;
use std::ops::Deref;

/// An Immutable non-empty collection
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct NonEmpty<T> {
    // One can potentially optimize some since len is `NonZeroUsize`;
    // using a Box<T> is for simplicity
    inner: Box<[T]>,
}

impl<T> NonEmpty<T> {
    /// Collects `iter` into a [NonEmpty], returning `None` if the iterator yields no element.
    pub fn from_iter(iter: impl IntoIterator<Item = T>) -> Option<NonEmpty<T>> {
        let boxed: Box<[T]> = iter.into_iter().collect();
        if boxed.is_empty() {
            None
        } else {
            Some(NonEmpty { inner: boxed })
        }
    }

    /// Creates a [NonEmpty] with a single element.
    pub fn single(value: T) -> NonEmpty<T> {
        NonEmpty {
            inner: Box::new([value]),
        }
    }

    /// The first element. Unlike [slice::first], this never returns `None`.
    pub fn first(&self) -> &T {
        &self.inner[0]
    }

    /// The last element. Unlike [slice::last], this never returns `None`.
    pub fn last(&self) -> &T {
        &self.inner[self.inner.len() - 1]
    }

    /// The number of elements, which is always at least one.
    pub fn len(&self) -> NonZeroUsize {
        NonZeroUsize::new(self.inner.len()).expect("invariant: a NonEmpty is never empty")
    }
}

impl<T> Deref for NonEmpty<T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T> IntoIterator for NonEmpty<T> {
    type Item = T;
    type IntoIter = std::vec::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_vec().into_iter()
    }
}

impl<'a, T> IntoIterator for &'a NonEmpty<T> {
    type Item = &'a T;
    type IntoIter = std::slice::Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.iter()
    }
}

#[cfg(test)]
mod tests {
    use super::NonEmpty;

    #[test]
    fn empty_input_yields_none() {
        assert_eq!(NonEmpty::<u8>::from_iter(std::iter::empty()), None);
    }

    #[test]
    fn first_and_last() {
        let single = NonEmpty::single(1);
        assert_eq!(single.first(), &1);
        assert_eq!(single.last(), &1);
        assert_eq!(single.len().get(), 1);

        let many = NonEmpty::from_iter([1, 2, 3]).expect("not empty");
        assert_eq!(many.first(), &1);
        assert_eq!(many.last(), &3);
        assert_eq!(many.len().get(), 3);
    }
}
