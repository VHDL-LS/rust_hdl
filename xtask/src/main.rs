// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2025, Lukas Scheller lukasscheller@icloud.com

use clap::{Parser, Subcommand};
use generate::{check_generators, run_generators, Generator, SyntaxNodeGenerator};
use model::load_model;
use std::path::Path;
use std::process;

mod config;
mod generate;
mod model;

#[derive(Parser)]
#[command(about = "Build tools for rust_hdl")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Generate AST node code from YAML grammar definitions
    Codegen {
        /// Check that generated files are up-to-date; exit 1 if any differ
        #[arg(long)]
        check: bool,
    },
}

fn main() {
    let cli = Cli::parse();

    // CARGO_MANIFEST_DIR is the xtask/ directory at compile time
    let workspace_root = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();

    match cli.command {
        Commands::Codegen { check } => {
            let output_dir = workspace_root.join("vhdl_syntax/src/syntax/generated");
            let definitions_dir = workspace_root.join("xtask/src/syntax_definitions");
            let model = load_model(&definitions_dir);
            let generators: &[&dyn Generator] = &[&SyntaxNodeGenerator];

            if check {
                let stale = check_generators(generators, &model, &output_dir)
                    .expect("failed to check generators");
                if stale.is_empty() {
                    println!("All generated files are up-to-date.");
                } else {
                    eprintln!("The following generated files are out of date:");
                    for stem in &stale {
                        eprintln!("  {stem}.rs");
                    }
                    eprintln!("Run `cargo xtask codegen` to regenerate.");
                    process::exit(1);
                }
            } else {
                run_generators(generators, &model, &output_dir).expect("failed to run generators");
                println!("Generated files written to {}", output_dir.display());
            }
        }
    }
}
