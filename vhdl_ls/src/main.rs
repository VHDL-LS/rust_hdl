// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use clap::Parser;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Args {}

fn main() {
    Args::parse();

    env_logger::init();
    log::info!("Starting language server");
    vhdl_ls::start();
}
