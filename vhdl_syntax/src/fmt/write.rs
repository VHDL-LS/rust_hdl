//! Traits and utilities to write formatted VHDL code using an [Encoder] of your choice.
//!
//! For most constructs in this crate, call `foo.display()` to get a [Displayed] object that
//! can be passed to `println!`, `write!` or similar formatting functions.
//!
//! **Note**: To write to a file, prefer using the `write_to` functions of the node that write binary.
// TODO: migrate write_to to its own trait mirroring this implementation.

use std::{
    convert::Infallible,
    fmt::{self},
    marker::PhantomData,
};

use crate::fmt::encoding::{Encoder, LossyUtf8Encoder};

/// Error that can happen when writing an encoded string to a [fmt::Write] sink.
/// This will either be an encoding error (generic over `E`) or a [fmt::Error]
#[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum WriteError<E> {
    Fmt(fmt::Error),
    Encoding(E),
}

impl<E> From<fmt::Error> for WriteError<E> {
    fn from(value: fmt::Error) -> Self {
        Self::Fmt(value)
    }
}

impl<E> fmt::Display for WriteError<E>
where
    E: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            WriteError::Fmt(error) => error.fmt(f),
            WriteError::Encoding(error) => error.fmt(f),
        }
    }
}

impl<E> std::error::Error for WriteError<E> where E: std::error::Error {}

/// Result type alias of a write action
pub type WriteResult<E> = std::result::Result<(), WriteError<E>>;

/// Writer for encoded strings.
///
/// Note: This is mostly an internal trait, intended for nodes and tokens to plug in.
/// The `fmt_to` function will only write ambiguous constructs (comments) using the given
/// encoder, and not transform Latin-1 to UTF-8.
pub trait WriteEncoded {
    /// Write `self` to a [fmt::Write] sink using the given encoder.
    fn fmt_to<E>(&self, writer: &mut impl fmt::Write) -> WriteResult<E::Err>
    where
        E: Encoder,
        for<'a> E::Str<'a>: fmt::Display;
}

/// Wrapper type around an element that enables writing the wrapper
/// using an [Encoder].
///
/// If the error is [Infallible], this struct can directly be used
/// to print or write to a file.
pub struct Displayed<'a, T: ?Sized, E = LossyUtf8Encoder> {
    inner: &'a T,
    _encoder: PhantomData<E>,
}

impl<T, E> fmt::Display for Displayed<'_, T, E>
where
    T: WriteEncoded,
    E: Encoder,
    for<'a> E::Str<'a>: fmt::Display,
    E::Err: Into<Infallible>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.inner.fmt_to::<E>(f) {
            Ok(()) => Ok(()),
            Err(WriteError::Fmt(e)) => Err(e),
            Err(WriteError::Encoding(e)) => match e.into() {},
        }
    }
}

/// Extension trait allowing each type that opts into [WriteEncoded]
/// to be displayed when the encoder is infallible.
///
/// The default `display` function uses the [LossyUtf8Encoder]
pub trait FormatToExt: WriteEncoded {
    fn display(&self) -> Displayed<'_, Self> {
        Displayed {
            inner: self,
            _encoder: PhantomData,
        }
    }

    fn display_as<E: Encoder>(&self) -> Displayed<'_, Self, E>
    where
        E::Err: Into<Infallible>,
    {
        Displayed {
            inner: self,
            _encoder: PhantomData,
        }
    }
}

impl<T: WriteEncoded + ?Sized> FormatToExt for T {}
