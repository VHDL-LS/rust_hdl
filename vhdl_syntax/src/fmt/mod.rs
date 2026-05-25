//! Provides facilities to format nodes and tokens to encoded strings.

use std::{convert::Infallible, fmt, marker::PhantomData};

pub mod encoding;

use encoding::{Encoder, LossyUtf8Encoder};

pub enum Error<EncodingErr> {
    Fmt(fmt::Error),
    Encoding(EncodingErr),
}

impl<E> From<fmt::Error> for Error<E> {
    fn from(value: fmt::Error) -> Self {
        Self::Fmt(value)
    }
}

pub type Result<Err> = std::result::Result<(), Error<Err>>;

pub trait FormatTo {
    fn write_encoded<E>(&self, writer: &mut impl fmt::Write) -> crate::fmt::Result<E::Err>
    where
        E: Encoder,
        for <'a> E::Str<'a>: fmt::Display;
}

pub struct Displayed<'a, T: ?Sized, E = LossyUtf8Encoder> {
    inner: &'a T,
    _encoder: PhantomData<E>,
}

pub trait FormatToExt: FormatTo {
    fn display(&self) -> Displayed<'_, Self> {
        Displayed {
            inner: self,
            _encoder: PhantomData,
        }
    }

    fn display_as<E: Encoder>(&self) -> Displayed<'_, Self, E> {
        Displayed {
            inner: self,
            _encoder: PhantomData,
        }
    }
}

impl<T: FormatTo + ?Sized> FormatToExt for T {}

impl<T, E> fmt::Display for Displayed<'_, T, E>
where
    T: FormatTo,
    E: Encoder,
    for<'a> E::Str<'a>: fmt::Display,
    E::Err: Into<Infallible>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.inner.write_encoded::<E>(f) {
            Ok(()) => Ok(()),
            Err(Error::Fmt(e)) => Err(e),
            Err(Error::Encoding(_)) => unreachable!(),
        }
    }
}
