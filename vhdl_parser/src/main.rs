// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

#[macro_use]
extern crate clap;
extern crate vhdl_parser;

use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::path::Path;

use vhdl_parser::ast::{AnyDesignUnit, PrimaryUnit, SecondaryUnit, SelectedName};
use vhdl_parser::message::{Message, Severity};
use vhdl_parser::{Config, FileToParse, Latin1String, Library, ParserError, Symbol, VHDLParser};

extern crate fnv;
use self::fnv::FnvHashMap;

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
            parser.clone(),
            files.map(|s| s.to_owned()).collect(),
            num_threads,
            show,
        )
    }

    if let Some(file_name) = matches.value_of("config") {
        let config = read_config(Path::new(file_name)).expect("Failed to read config file");

        let mut libraries = FnvHashMap::default();
        let mut files_to_parse = Vec::new();
        for library in config.iter_libraries() {
            let library_name =
                Latin1String::from_utf8(library.name()).expect("Library name not latin-1 encoded");
            let library_name = parser.symbol(&library_name);
            libraries.insert(library_name.clone(), Vec::new());

            for file_name in library.file_names() {
                let file_to_parse = LibraryFileToParse {
                    library_name: library_name.clone(),
                    file_name: file_name.to_owned(),
                };

                files_to_parse.push(file_to_parse)
            }
        }

        for (file_to_parse, mut messages, design_file) in
            parser.parse_design_files(files_to_parse, num_threads)
        {
            let design_file = match design_file {
                Ok(design_file) => design_file,
                Err(ParserError::Message(msg)) => {
                    println!("Error when parsing {}", file_to_parse.file_name);
                    show_messages(&messages);
                    println!("{}", msg.show());
                    continue;
                }
                Err(ParserError::IOError(err)) => {
                    println!("Error when parsing {}", file_to_parse.file_name);
                    println!("{}", err);
                    continue;
                }
            };

            // @TODO check for errors
            show_messages(&messages);

            libraries
                .get_mut(&file_to_parse.library_name)
                .unwrap()
                .push(design_file);
        }

        for (library_name, design_files) in libraries.into_iter() {
            let mut messages = Vec::new();
            let mut library = Library::new(library_name);
            for design_file in design_files {
                library.add_design_file(design_file, &mut messages);
            }
            library.finalize(&mut messages);
            show_messages(&messages);
        }
    }
}

struct LibraryFileToParse {
    library_name: Symbol,
    file_name: String,
}

impl FileToParse for LibraryFileToParse {
    fn file_name(&self) -> &str {
        &self.file_name
    }
}

fn read_config(file_name: &Path) -> io::Result<Config> {
    let mut file = File::open(file_name)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    let parent = file_name.parent().unwrap();

    Config::from_str(&contents, parent).map_err(|msg| io::Error::new(io::ErrorKind::Other, msg))
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
        AnyDesignUnit::Primary(ref primary) => match primary.unit {
            PrimaryUnit::EntityDeclaration(ref entity) => {
                println!("entity {}", entity.ident.item.name());
                if let Some(ref list) = entity.generic_clause {
                    println!("  with {} generics", list.len())
                }
                if let Some(ref list) = entity.port_clause {
                    println!("  with {} ports", list.len())
                }
                if entity.decl.len() > 0 {
                    println!("  with {} declarations", entity.decl.len())
                }
                if entity.statements.len() > 0 {
                    println!("  with {} concurrent statements", entity.statements.len())
                }
            }
            PrimaryUnit::ContextDeclaration(ref context) => {
                println!("context {}", context.ident.item.name());
                if context.items.len() > 0 {
                    println!("  with {} items", context.items.len())
                }
            }
            PrimaryUnit::PackageDeclaration(ref package) => {
                println!("package {}", package.ident.item.name());
                if let Some(ref list) = package.generic_clause {
                    println!("  with {} generics", list.len())
                }
                if package.decl.len() > 0 {
                    println!("  with {} declarations", package.decl.len())
                }
            }
            PrimaryUnit::Configuration(ref config) => println!(
                "configuration {} of {}",
                config.ident.item.name(),
                to_string(&config.entity_name)
            ),
            PrimaryUnit::PackageInstance(ref inst) => {
                println!(
                    "package instance {} of {}",
                    inst.ident.item.name(),
                    to_string(&inst.package_name)
                );
            }
        },
        AnyDesignUnit::Secondary(ref secondary) => match secondary.unit {
            SecondaryUnit::Architecture(ref arch) => {
                println!(
                    "architecture {} of {}",
                    arch.ident.item.name(),
                    arch.entity_name.item.name()
                );
                if arch.decl.len() > 0 {
                    println!("  with {} declarations", arch.decl.len())
                }
                if arch.statements.len() > 0 {
                    println!("  with {} concurrent statements", arch.statements.len())
                }
            }
            SecondaryUnit::PackageBody(ref package_body) => {
                println!("package body {}", package_body.ident.item.name());
                if package_body.decl.len() > 0 {
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

fn parse(parser: VHDLParser, file_names: Vec<String>, num_threads: usize, show: bool) {
    let mut num_errors = 0;
    let mut num_warnings = 0;

    for (file_name, mut messages, design_file) in parser.parse_design_files(file_names, num_threads)
    {
        use vhdl_parser::semantic;
        let design_file = match design_file {
            Ok(design_file) => {
                for design_unit in design_file.design_units.iter() {
                    semantic::check_design_unit(design_unit, &mut messages)
                }
                design_file
            }
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
            };
        }

        show_messages(&messages);
        if file_has_errors {
            num_errors += 1;
        }
    }

    println!("");
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
