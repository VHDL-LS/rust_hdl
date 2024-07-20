// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use clap::Parser;
use itertools::Itertools;
use std::iter::zip;
use std::path::{Path, PathBuf};
use vhdl_lang::{
    format_design_file, Config, Diagnostic, MessagePrinter, Project, Severity, SeverityMap, Source,
    VHDLParser, VHDLStandard,
};

#[derive(Debug, clap::Args)]
#[group(required = true, multiple = false)]
pub struct Group {
    /// Config file in TOML format containing libraries and settings
    #[arg(short, long)]
    config: Option<String>,

    /// Format the passed file and write the contents to stdout
    #[arg(short, long)]
    format: Option<String>,
}

/// Run vhdl analysis
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// The number of threads to use. By default, the maximum is selected based on process cores
    #[arg(short = 'p', long)]
    num_threads: Option<usize>,

    /// Path to the config file for the VHDL standard libraries (i.e., IEEE std_logic_1164).
    /// If omitted, will search for these libraries in a set of standard paths
    #[arg(short = 'l', long)]
    libraries: Option<String>,

    #[clap(flatten)]
    group: Group,
}

fn main() {
    let args = Args::parse();
    if let Some(config_path) = args.group.config {
        rayon::ThreadPoolBuilder::new()
            .num_threads(args.num_threads.unwrap_or(0))
            .build_global()
            .unwrap();

        let mut config = Config::default();
        let mut msg_printer = MessagePrinter::default();
        config.load_external_config(&mut msg_printer, args.libraries.clone());
        config.append(
            &Config::read_file_path(Path::new(&config_path)).expect("Failed to read config file"),
            &mut msg_printer,
        );

        let severity_map = *config.severities();
        let mut project = Project::from_config(config, &mut msg_printer);
        project.enable_unused_declaration_detection();
        let diagnostics = project.analyse();

        show_diagnostics(&diagnostics, &severity_map);

        if diagnostics
            .iter()
            .any(|diag| severity_map[diag.code].is_some_and(|severity| severity == Severity::Error))
        {
            std::process::exit(1);
        } else {
            std::process::exit(0);
        }
    } else if let Some(format) = args.group.format {
        let path = PathBuf::from(format);
        let parser = VHDLParser::new(VHDLStandard::VHDL2008);
        let mut diagnostics = Vec::new();
        let result = parser.parse_design_file(&path, &mut diagnostics);
        match result {
            Ok((_, design_file)) => {
                if !diagnostics.is_empty() {
                    show_diagnostics(&diagnostics, &SeverityMap::default());
                    std::process::exit(1);
                }
                let result = format_design_file(&design_file);
                let new_file =
                    parser.parse_design_source(&Source::inline(&path, &result), &mut diagnostics);
                if !diagnostics.is_empty() {
                    println!("Formatting failed! File was OK before, but is not after.");
                    show_diagnostics(&diagnostics, &SeverityMap::default());
                    std::process::exit(1);
                }
                println!("{result}");
                if new_file != design_file {
                    println!(
                        "Here. A: {}, B: {}",
                        new_file.design_units.len(),
                        design_file.design_units.len()
                    );
                    for (a, b) in zip(new_file.design_units, design_file.design_units) {
                        println!("A");
                        if a != b {
                            println!("Mismatch")
                        }
                    }
                }
                std::process::exit(0);
            }
            Err(err) => {
                println!("{err}");
                std::process::exit(1);
            }
        }
    }
}

fn show_diagnostics(diagnostics: &[Diagnostic], severity_map: &SeverityMap) {
    let diagnostics = diagnostics
        .iter()
        .filter_map(|diag| diag.show(severity_map))
        .collect_vec();
    for str in &diagnostics {
        println!("{str}");
    }

    if !diagnostics.is_empty() {
        println!("Found {} diagnostics", diagnostics.len());
    }
}
