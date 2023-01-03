// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com
#![allow(clippy::upper_case_acronyms)]

#[macro_use]
extern crate log;

mod rpc_channel;
mod stdio_server;
mod vhdl_server;
pub use crate::stdio_server::start;
pub use crate::vhdl_server::VHDLServerSettings;
