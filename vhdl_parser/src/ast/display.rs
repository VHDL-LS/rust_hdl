// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

//! Implementation of Display

use super::*;
use std::fmt::{Display, Formatter, Result};

impl Display for Designator {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Designator::Identifier(ref sym) => write!(f, "{}", sym),
            Designator::OperatorSymbol(ref latin1) => write!(f, "\"{}\"", latin1),
            Designator::Character(byte) => write!(f, "'{}'", *byte as char),
        }
    }
}

impl Display for SelectedName {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            SelectedName::Selected(ref prefix, ref des) => write!(f, "{}.{}", prefix, des),
            SelectedName::Designator(ref des) => write!(f, "{}", des),
        }
    }
}

impl<T: Display> Display for WithPos<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", &self.item)
    }
}
