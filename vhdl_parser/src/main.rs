// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

// Allowing this, since there is an open issue with this lint
// Track here: https://github.com/rust-lang/rust-clippy/issues/1981
// Track here: https://github.com/rust-lang/rust-clippy/issues/1981
#![cfg_attr(feature = "cargo-clippy", allow(ptr_arg))]

#[macro_use]
extern crate clap;
extern crate vhdl_parser;

use std::path::Path;

use vhdl_parser::ast::{AnyDesignUnit, PrimaryUnit, SecondaryUnit, SelectedName};
use vhdl_parser::{Config, Message, ParserError, Project, Severity, VHDLParser};

fn main() {
    use clap::{App, Arg};

    let matches = App::new("VHDL Parser")
        .version("0.2")
        .author("Olof Kraigher <olof.kraigher@gmail.com>")
        .about("VHDL Parser Demonstrator")
        .arg(
            Arg::with_name("show")
                .long("show")
                .help("Show information about design units"),
        ).arg(
            Arg::with_name("num-threads")
                .short("-p")
                .long("--num-threads")
                .default_value("4")
                .help("The number of threads to use"),
        ).arg(
            Arg::with_name("files")
                .help("The list of files to parse. The files are only parsed without any semantic analysis")
                .index(1)
                .multiple(true)
        ).arg(
            Arg::with_name("config")
                .help("Config file in TOML format containing libraries and settings")
                .short("-c")
                .long("--config")
                .takes_value(true)
                .conflicts_with("files"))
        .get_matches();

    let show = matches.is_present("show");
    let num_threads = value_t_or_exit!(matches.value_of("num-threads"), usize);
    let parser = VHDLParser::new();

    if let Some(files) = matches.values_of("files") {
        parse(
            &parser.clone(),
            files.map(|s| s.to_owned()).collect(),
            num_threads,
            show,
        )
    }

    if let Some(file_name) = matches.value_of("config") {
        let config =
            Config::read_file_path(Path::new(file_name)).expect("Failed to read config file");

        let mut errors = Vec::new();
        let mut project = Project::from_config(&config, num_threads, &mut errors);
        show_messages(&project.analyse());

        if !errors.is_empty() {
            println!("Errors when reading config {}:", file_name);
            for error in errors {
                println!("{}", error);
            }
        }
    }
}

fn to_string(selected_name: &SelectedName) -> String {
    let names: Vec<String> = selected_name
        .iter()
        .map(|ident| ident.item.name().to_string())
        .collect();
    names.join(".")
}

fn show_design_unit(design_unit: &AnyDesignUnit) {
    match design_unit {
        AnyDesignUnit::Primary(ref primary) => match primary {
            PrimaryUnit::EntityDeclaration(ref entity) => {
                let entity = &entity.unit;
                println!("entity {}", entity.ident.item.name());
                if let Some(ref list) = entity.generic_clause {
                    println!("  with {} generics", list.len())
                }
                if let Some(ref list) = entity.port_clause {
                    println!("  with {} ports", list.len())
                }
                if !entity.decl.is_empty() {
                    println!("  with {} declarations", entity.decl.len())
                }
                if !entity.statements.is_empty() {
                    println!("  with {} concurrent statements", entity.statements.len())
                }
            }
            PrimaryUnit::ContextDeclaration(ref context) => {
                println!("context {}", context.ident.item.name());
                if !context.items.is_empty() {
                    println!("  with {} items", context.items.len())
                }
            }
            PrimaryUnit::PackageDeclaration(ref package) => {
                let package = &package.unit;
                println!("package {}", package.ident.item.name());
                if let Some(ref list) = package.generic_clause {
                    println!("  with {} generics", list.len())
                }
                if !package.decl.is_empty() {
                    println!("  with {} declarations", package.decl.len())
                }
            }
            PrimaryUnit::Configuration(ref config) => {
                let config = &config.unit;
                println!(
                    "configuration {} of {}",
                    config.ident.item.name(),
                    to_string(&config.entity_name)
                );
            }
            PrimaryUnit::PackageInstance(ref inst) => {
                let inst = &inst.unit;
                println!(
                    "package instance {} of {}",
                    inst.ident.item.name(),
                    to_string(&inst.package_name)
                );
            }
        },
        AnyDesignUnit::Secondary(ref secondary) => match secondary {
            SecondaryUnit::Architecture(ref arch) => {
                let arch = &arch.unit;
                println!(
                    "architecture {} of {}",
                    arch.ident.item.name(),
                    arch.entity_name.item.name()
                );
                if !arch.decl.is_empty() {
                    println!("  with {} declarations", arch.decl.len())
                }
                if !arch.statements.is_empty() {
                    println!("  with {} concurrent statements", arch.statements.len())
                }
            }
            SecondaryUnit::PackageBody(ref package_body) => {
                let package_body = &package_body.unit;
                println!("package body {}", package_body.ident.item.name());
                if !package_body.decl.is_empty() {
                    println!("  with {} declarations", package_body.decl.len())
                }
            }
        },
    }
}

fn show_messages(messages: &[Message]) {
    for message in messages {
        println!("{}", message.show());
    }
}

fn parse(parser: &VHDLParser, file_names: Vec<String>, num_threads: usize, show: bool) {
    let mut num_errors = 0;
    let mut num_warnings = 0;

    for (file_name, mut messages, design_file) in parser.parse_design_files(file_names, num_threads)
    {
        let design_file = match design_file {
            Ok(design_file) => design_file,
            Err(ParserError::Message(msg)) => {
                println!("Error when parsing {}", file_name);
                show_messages(&messages);
                println!("{}", msg.show());
                num_errors += 1;
                continue;
            }
            Err(ParserError::IOError(err)) => {
                println!("Error when parsing {}", file_name);
                println!("{}", err);
                num_errors += 1;
                continue;
            }
        };

        if show {
            println!("\nShowing design units from {}", file_name);
            for design_unit in design_file.design_units {
                show_design_unit(&design_unit);
            }
        }

        let mut file_has_errors = false;

        for message in messages.iter() {
            match message.severity {
                Severity::Warning => {
                    num_warnings += 1;
                }
                Severity::Error => {
                    file_has_errors = true;
                }
                _ => {}
            };
        }

        show_messages(&messages);
        if file_has_errors {
            num_errors += 1;
        }
    }

    println!();
    println!("Summary:");
    if num_warnings > 0 {
        println!("Found {} warnings", num_warnings);
    } else {
        println!("Found no warnings");
    }

    if num_errors > 0 {
        println!("BAD: Found errors in {} files", num_errors);
    } else {
        println!("OK: Found no errors");
    }
}
