// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

// Allowing this, since there is an open issue with this lint
// Track here: https://github.com/rust-lang/rust-clippy/issues/1981
// Track here: https://github.com/rust-lang/rust-clippy/issues/1981
#![allow(clippy::ptr_arg)]

use clap::Parser;
use std::path::Path;
use std::time::SystemTime;
use vhdl_lang::{Config, Diagnostic, MessagePrinter, NullMessages, Project, Severity};

/// Run vhdl analysis
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// The number of threads to use. By default the maximum is selected based on process cores
    #[arg(short = 'p', long)]
    num_threads: Option<usize>,

    /// Prints the number of files processed and the execution time
    #[arg(long, default_value_t = false)]
    perf: bool,

    /// Run repeatedly to get a reliable benchmark result
    #[arg(long, default_value_t = false)]
    bench: bool,

    /// Hide hint diagnostics
    #[arg(long, default_value_t = false)]
    no_hint: bool,

    /// Config file in TOML format containing libraries and settings
    #[arg(short, long)]
    config: String,

    /// Dump items that are not resolved into an unique reference
    /// This is used for development to test where the language server is blind
    #[arg(long)]
    dump_unresolved: bool,

    /// Count items that are not resolved into an unique reference
    /// This is used for development to test where the language server is blind
    #[arg(long)]
    count_unresolved: bool,
}

fn main() {
    let args = Args::parse();
    rayon::ThreadPoolBuilder::new()
        .num_threads(args.num_threads.unwrap_or(0))
        .build_global()
        .unwrap();

    let mut config = Config::default();
    let mut msg_printer = MessagePrinter::default();
    config.load_external_config(&mut msg_printer);
    config.append(
        &Config::read_file_path(Path::new(&args.config)).expect("Failed to read config file"),
        &mut msg_printer,
    );

    let start = SystemTime::now();

    let iterations = if args.bench {
        let iterations = 10;
        println!("Running {iterations} iterations for benchmarking");
        for _ in 0..(iterations - 1) {
            let mut project = Project::from_config(&config, &mut NullMessages);
            project.analyse();
        }
        iterations
    } else {
        1
    };

    let mut project = Project::from_config(&config, &mut msg_printer);
    let mut diagnostics = project.analyse();
    let duration = start.elapsed().unwrap() / iterations;

    if args.no_hint {
        diagnostics.retain(|diag| diag.severity != Severity::Hint);
    }

    show_diagnostics(&diagnostics);

    if args.perf || args.bench {
        let mut num_files = 0;
        let mut num_lines = 0;
        for source_file in project.files() {
            num_files += 1;
            num_lines += source_file.num_lines();
        }
        let duration_per_line = duration.checked_div(num_lines as u32).unwrap();

        println!("Analyzed {num_files} files with {num_lines} lines of code");
        println!(
            "Total time to run was {} ms with an average of {} ns per line",
            duration.as_millis(),
            duration_per_line.as_nanos()
        );
    }

    if args.dump_unresolved || args.count_unresolved {
        let (total, unresolved) = project.find_all_unresolved();

        if args.dump_unresolved {
            for pos in unresolved.iter() {
                println!("{}", pos.show("Unresolved"));
            }
        }

        if args.count_unresolved {
            println!("{} out of {} positions unresolved", unresolved.len(), total);
        }
    }

    // Exit without running Drop on entire allocated AST
    std::process::exit(0);
}

fn show_diagnostics(diagnostics: &[Diagnostic]) {
    for diagnostic in diagnostics {
        println!("{}", diagnostic.show());
    }

    if !diagnostics.is_empty() {
        println!("Found {} diagnostics", diagnostics.len());
    }
}
