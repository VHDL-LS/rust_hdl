// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use clap::Parser;
use itertools::Itertools;
use std::iter::zip;
use std::path::{Path, PathBuf};
use vhdl_lang::ast::DesignFile;
use vhdl_lang::{
    Config, Diagnostic, MessagePrinter, Project, Severity, SeverityMap, Source, VHDLFormatter,
    VHDLParser, VHDLStandard,
};

#[derive(Debug, clap::Args)]
#[group(required = true, multiple = false)]
pub struct Group {
    /// Config file in TOML format containing libraries and settings
    #[arg(short, long)]
    config: Option<String>,

    /// Format the passed file and write the contents to stdout.
    ///
    /// This is experimental and the formatting behavior will change in the future.
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
        parse_and_analyze_project(&config_path, args.num_threads, args.libraries.as_ref());
    } else if let Some(format) = args.group.format {
        format_file(format);
    }
}

fn format_file(format: String) {
    let path = PathBuf::from(format);
    let parser = VHDLParser::new(VHDLStandard::default());
    let mut diagnostics = Vec::new();
    let result = parser.parse_design_file(&path, &mut diagnostics);
    match result {
        Ok((_, design_file)) => {
            if !diagnostics.is_empty() {
                show_diagnostics(&diagnostics, &SeverityMap::default());
                std::process::exit(1);
            }
            let result = VHDLFormatter::format_design_file(&design_file);
            println!("{result}");
            check_formatted_file(&path, &parser, design_file, &result);
            std::process::exit(0);
        }
        Err(err) => {
            println!("{err}");
            std::process::exit(1);
        }
    }
}

fn check_formatted_file(path: &Path, parser: &VHDLParser, design_file: DesignFile, result: &str) {
    let mut diagnostics: Vec<Diagnostic> = Vec::new();
    let new_file = parser.parse_design_source(&Source::inline(path, result), &mut diagnostics);
    if !diagnostics.is_empty() {
        println!("Formatting failed as it resulted in a syntactically incorrect file.");
        show_diagnostics(&diagnostics, &SeverityMap::default());
        std::process::exit(1);
    }
    for ((tokens_a, _), (tokens_b, _)) in zip(new_file.design_units, design_file.design_units) {
        for (a, b) in zip(tokens_a, tokens_b) {
            if !a.equal_format(&b) {
                println!("Token mismatch");
                println!("New Token={a:#?}");
                let contents = a.pos.source.contents();
                let a_line = contents.get_line(a.pos.range.start.line as usize).unwrap();
                println!("    {a_line}");
                println!("Old Token={b:#?}");
                let b_line = result.lines().nth(b.pos.range.start.line as usize).unwrap();
                println!("    {b_line}");
                break;
            }
        }
    }
}

fn parse_and_analyze_project(
    config_path: &str,
    num_threads: Option<usize>,
    libraries: Option<&String>,
) {
    rayon::ThreadPoolBuilder::new()
        .num_threads(num_threads.unwrap_or(0))
        .build_global()
        .unwrap();

    let mut config = Config::default();
    let mut msg_printer = MessagePrinter::default();
    config.load_external_config(&mut msg_printer, libraries.cloned());
    config.append(
        &Config::read_file_path(Path::new(&config_path)).expect("Failed to read config file"),
        &mut msg_printer,
    );

    let severity_map = *config.severities();
    let mut project = Project::from_config(config, &mut msg_printer);
    project.enable_all_linters();
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
