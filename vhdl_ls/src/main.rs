// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use clap::Parser;
use vhdl_ls::VHDLServerSettings;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Disable diagnostic messages, only use navigation and hover features
    #[arg(long, default_value_t = false)]
    no_lint: bool,

    /// Path to the config file for the VHDL standard libraries (i.e., IEEE std_logic_1164).
    /// If omitted, will search for these libraries in a set of standard paths
    #[arg(short = 'l', long)]
    libraries: Option<String>,

    /// Normally warning and error messages are sent to window/showMessage
    /// This will silence all window/showMessage and only use window/logMessage
    #[arg(long, default_value_t = false)]
    silent: bool,
}

fn main() {
    let args = Args::parse();

    env_logger::init();
    log::info!("Starting language server");
    vhdl_ls::start(VHDLServerSettings {
        no_lint: args.no_lint,
        silent: args.silent,
        libraries_path: args.libraries,
        ..Default::default()
    });
}
