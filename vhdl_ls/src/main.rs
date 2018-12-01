// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

extern crate vhdl_ls;

#[macro_use]
extern crate log;
extern crate env_logger;

fn main() {
    env_logger::init();
    info!("Starting language server");
    vhdl_ls::start();
}
