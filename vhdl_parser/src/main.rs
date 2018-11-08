// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

extern crate vhdl_parser;
use std::env;

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
        LibraryUnit::EntityDeclaration { ref ident, .. } => {
            println!("entity {}", ident.item.name())
        }
        LibraryUnit::ContextDeclaration(ref context) => {
            println!("context {}", context.ident.item.name())
        }
        LibraryUnit::ArchitectureBody {
            ref ident,
            ref entity_name,
            ref decl,
        } => println!(
            "architecture {} of {} with {} declarations",
            ident.item.name(),
            entity_name.name(),
            decl.len()
        ),
        LibraryUnit::PackageDeclaration {
            ref ident,
            ref decl,
        } => println!(
            "package {} with {} declarations",
            ident.item.name(),
            decl.len()
        ),
        LibraryUnit::PackageBody { ref ident } => println!("package body {}", ident.item.name()),
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
        println!("{}", message.pretty_string());
    }
}

fn main() {
    let parser = Arc::new(VHDLParser::new());

    let num_workers = 4;
    let (work_sender, work_receiver) = sync_channel(2 * num_workers);
    let work_receiver = Arc::new(Mutex::new(work_receiver));
    let (result_sender, result_receiver) = sync_channel(2 * num_workers);

    for _ in 0..num_workers {
        let parser = parser.clone();
        let result_sender = result_sender.clone();
        let work_receiver = work_receiver.clone();
        spawn(move || worker(parser, work_receiver, result_sender));
    }
    spawn(move || {
        for (idx, file_name) in env::args().skip(1).enumerate() {
            work_sender.send(Some((idx, file_name.clone()))).unwrap();
        }
        for _ in 0..num_workers {
            work_sender.send(None).unwrap();
        }
    });

    let mut num_errors = 0;
    let mut num_warnings = 0;

    let mut map = FnvHashMap::default();

    for idx in 0..env::args().skip(1).len() {
        let (file_name, messages, design_file) = {
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

        let design_file = match design_file {
            Ok(design_file) => design_file,
            Err(ParserError::Message(msg)) => {
                println!("Error when parsing {}", file_name);
                show_messages(&messages);
                println!("{}", msg.pretty_string());
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

        println!("\nResults from {}", file_name);
        for design_unit in design_file.design_units {
            show_design_unit(&design_unit);
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
