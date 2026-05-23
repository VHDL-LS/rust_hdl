use std::{borrow::Cow, convert::Infallible, str::Utf8Error};

use crate::latin_1::Latin1Str;

pub trait Encoder {
    type Str<'a>;
    type Err;

    fn encode(bytes: &[u8]) -> Result<Self::Str<'_>, Self::Err>;
}

pub struct Utf8Encoder;

impl Encoder for Utf8Encoder {
    type Str<'a> = &'a str;
    type Err = Utf8Error;

    fn encode(bytes: &[u8]) -> Result<Self::Str<'_>, Self::Err> {
        str::from_utf8(bytes)
    }
}

pub struct Latin1Encoder;

impl Encoder for Latin1Encoder {
    type Str<'a> = &'a Latin1Str;
    type Err = Infallible;

    fn encode(bytes: &[u8]) -> Result<Self::Str<'_>, Self::Err> {
        Ok(Latin1Str::new(bytes))
    }
}

pub struct LossyUtf8Encoder;

impl Encoder for LossyUtf8Encoder {
    type Str<'a> = Cow<'a, str>;
    type Err = Infallible;

    fn encode(bytes: & [u8]) -> Result<Self::Str<'_>, Self::Err> {
        Ok(String::from_utf8_lossy(bytes))
    }
}
