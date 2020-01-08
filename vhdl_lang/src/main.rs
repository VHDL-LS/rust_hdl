// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

// Allowing this, since there is an open issue with this lint
// Track here: https://github.com/rust-lang/rust-clippy/issues/1981
// Track here: https://github.com/rust-lang/rust-clippy/issues/1981
#![allow(clippy::ptr_arg)]

#[macro_use]
extern crate clap;

use std::path::Path;
use std::time::SystemTime;
use vhdl_lang::{Config, Diagnostic, MessagePrinter, Project};

fn main() {
    use clap::{App, Arg};

    let matches = App::new(env!("CARGO_PKG_NAME"))
        .version(env!("CARGO_PKG_VERSION"))
        .author(env!("CARGO_PKG_AUTHORS"))
        .about(env!("CARGO_PKG_DESCRIPTION"))
        .arg(
            Arg::with_name("num-threads")
                .short("-p")
                .long("--num-threads")
                .help("The number of threads to use. By default the maximum is selected based on process cores"),
        )
        .arg(
            Arg::with_name("perf")
                .long("--perf")
                .help("Prints the number of files processed and the execution time")
                .takes_value(false),
        )
        .arg(
            Arg::with_name("config")
                .help("Config file in TOML format containing libraries and settings")
                .short("-c")
                .long("--config")
                .required(true)
                .takes_value(true),
        )
        .get_matches();

    if matches.is_present("num-threads") {
        let num_threads = value_t_or_exit!(matches.value_of("num-threads"), usize);
        rayon::ThreadPoolBuilder::new()
            .num_threads(num_threads)
            .build_global()
            .unwrap();
    }

    let show_perf = matches.is_present("perf");

    let file_name = value_t_or_exit!(matches.value_of("config"), String);
    let mut config = Config::default();
    let mut msg_printer = MessagePrinter::default();
    config.load_external_config(&mut msg_printer);
    config.append(
        &Config::read_file_path(Path::new(&file_name)).expect("Failed to read config file"),
        &mut msg_printer,
    );

    let start = SystemTime::now();
    let mut project = Project::from_config(&config, &mut msg_printer);
    let diagnostics = project.analyse();
    let duration = start.elapsed().unwrap();
    show_diagnostics(&diagnostics);

    if show_perf {
        let mut num_files = 0;
        let mut num_lines = 0;
        for source_file in project.files() {
            num_files += 1;
            num_lines += source_file.num_lines();
        }
        let duration_per_line = duration.checked_div(num_lines as u32).unwrap();

        println!(
            "Analyzed {} files with {} lines of code",
            num_files, num_lines
        );
        println!(
            "Total time to run was {} ms with an average of {} ns per line",
            duration.as_millis(),
            duration_per_line.as_nanos()
        );
    }

    // Exit without running Drop on entire allocated AST
    std::process::exit(0);
}

fn show_diagnostics(diagnostics: &[Diagnostic]) {
    for diagnostic in diagnostics {
        println!("{}", diagnostic.show());
    }
}
