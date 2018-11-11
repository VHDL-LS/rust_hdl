// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

extern crate clap;
extern crate vhdl_parser;

use std::sync::mpsc::{sync_channel, Receiver, SyncSender};
use std::sync::{Arc, Mutex};
use std::thread::spawn;
use vhdl_parser::ast::{DesignUnit, LibraryUnit, SelectedName};
use vhdl_parser::message::{Message, Severity};
use vhdl_parser::{ParserError, ParserResult, VHDLParser};

extern crate fnv;
use self::fnv::FnvHashMap;

fn worker(
    parser: Arc<VHDLParser>,
    input: Arc<Mutex<Receiver<Option<(usize, String)>>>>,
    output: SyncSender<(usize, (String, Vec<Message>, ParserResult))>,
) {
    loop {
        let item = input.lock().unwrap().recv().unwrap();
        match item {
            Some((idx, file_name)) => {
                let mut messages = Vec::new();
                let result = parser.parse_design_file(&file_name, &mut messages);
                output.send((idx, (file_name, messages, result))).unwrap();
            }
            None => {
                break;
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

fn show_design_unit(design_unit: &DesignUnit) {
    match design_unit.library_unit {
        LibraryUnit::EntityDeclaration(ref entity) => {
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
        LibraryUnit::ContextDeclaration(ref context) => {
            println!("context {}", context.ident.item.name());
            if context.items.len() > 0 {
                println!("  with {} items", context.items.len())
            }
        }
        LibraryUnit::Architecture(ref arch) => {
            println!(
                "architecture {} of {}",
                arch.ident.item.name(),
                arch.entity_name.name()
            );
            if arch.decl.len() > 0 {
                println!("  with {} declarations", arch.decl.len())
            }
            if arch.statements.len() > 0 {
                println!("  with {} concurrent statements", arch.statements.len())
            }
        }
        LibraryUnit::PackageDeclaration(ref package) => {
            println!("package {}", package.ident.item.name());
            if let Some(ref list) = package.generic_clause {
                println!("  with {} generics", list.len())
            }
            if package.decl.len() > 0 {
                println!("  with {} declarations", package.decl.len())
            }
        }
        LibraryUnit::PackageBody(ref package_body) => {
            println!("package body {}", package_body.ident.item.name());
            if package_body.decl.len() > 0 {
                println!("  with {} declarations", package_body.decl.len())
            }
        }
        LibraryUnit::Configuration(ref config) => println!(
            "configuration {} of {}",
            config.ident.item.name(),
            to_string(&config.entity_name)
        ),
        LibraryUnit::PackageInstance(ref inst) => {
            println!(
                "package instance {} of {}",
                inst.ident.item.name(),
                to_string(&inst.package_name)
            );
        }
    }
}

fn show_messages(messages: &[Message]) {
    for message in messages {
        println!("{}", message.show());
    }
}

fn parse(file_names: Vec<String>, num_threads: usize, show: bool) {
    let parser = Arc::new(VHDLParser::new());

    let (work_sender, work_receiver) = sync_channel(2 * num_threads);
    let work_receiver = Arc::new(Mutex::new(work_receiver));
    let (result_sender, result_receiver) = sync_channel(2 * num_threads);

    for _ in 0..num_threads {
        let parser = parser.clone();
        let result_sender = result_sender.clone();
        let work_receiver = work_receiver.clone();
        spawn(move || worker(parser, work_receiver, result_sender));
    }
    let num_files = file_names.len();
    spawn(move || {
        for (idx, file_name) in file_names.iter().enumerate() {
            work_sender.send(Some((idx, file_name.clone()))).unwrap();
        }
        for _ in 0..num_threads {
            work_sender.send(None).unwrap();
        }
    });

    let mut num_errors = 0;
    let mut num_warnings = 0;

    let mut map = FnvHashMap::default();

    for idx in 0..num_files {
        let (file_name, mut messages, design_file) = {
            match map.remove(&idx) {
                Some(value) => value,
                None => loop {
                    let (i, value) = result_receiver.recv().unwrap();
                    if i == idx {
                        break value;
                    } else {
                        map.insert(i, value);
                    }
                },
            }
        };

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
                .help("The list of files to parse")
                .index(1)
                .multiple(true),
        ).get_matches();

    let show = matches.is_present("show");
    let num_threads = matches
        .value_of("num-threads")
        .unwrap()
        .parse::<usize>()
        .unwrap();
    if let Some(files) = matches.values_of("files") {
        parse(files.map(|s| s.to_owned()).collect(), num_threads, show)
    }
}
