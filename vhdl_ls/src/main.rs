// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

extern crate jsonrpc_core;
extern crate languageserver_types;

use jsonrpc_core::request::Notification;
use jsonrpc_core::*;
use languageserver_types::{
    Diagnostic, DiagnosticSeverity, DidChangeTextDocumentParams, InitializeResult, Position,
    PublishDiagnosticsParams, Range, ServerCapabilities, TextDocumentSyncCapability,
    TextDocumentSyncKind,
};
use std::io::prelude::*;
use std::io::{self, BufRead};

extern crate vhdl_parser;
use vhdl_parser::message::{Message, Severity};
use vhdl_parser::source::{Source, SrcPos};
use vhdl_parser::{ParserError, VHDLParser};

fn srcpos_to_range(srcpos: SrcPos) -> Range {
    let contents = srcpos.source.contents().unwrap();
    let mut start = None;
    let mut end = None;

    let mut cursor = Position {
        line: 0,
        character: 0,
    };
    for (i, byte) in contents.bytes.iter().enumerate() {
        if i == srcpos.start {
            start = Some(cursor);
        }

        if i == srcpos.start + srcpos.length {
            end = Some(cursor);
        }

        if *byte == b'\n' {
            cursor.line += 1;
            cursor.character = 0;
        } else {
            cursor.character += 1;
        };
    }

    Range {
        start: start.unwrap_or(cursor),
        end: end.unwrap_or(cursor),
    }
}

fn read_header(reader: &mut BufRead) -> u64 {
    let mut buffer = String::new();
    reader.read_line(&mut buffer).unwrap();
    let fields = buffer.trim_end().clone().split(": ").collect::<Vec<&str>>();
    if fields.get(0) != Some(&"Content-Length") {
        eprintln!("{:?}", fields);
        panic!();
    }
    let content_length = fields.get(1).unwrap().parse::<u64>().unwrap();

    let mut buffer = String::new();
    reader.read_line(&mut buffer).unwrap();
    if buffer == "\r\n" {
        return content_length;
    }

    let fields = buffer.trim_end().clone().split(": ").collect::<Vec<&str>>();
    if fields.get(0) != Some(&"Content-Type") {
        eprintln!("{:?}", fields);
        panic!();
    } else {
        eprintln!("got Content-Type: {}", fields.get(1).unwrap());
    }

    let mut buffer = String::new();
    reader.read_line(&mut buffer).unwrap();
    if buffer != "\r\n" {
        eprintln!("{:?}", buffer);
        panic!();
    }

    return content_length;
}

fn to_diagnostic(message: Message) -> Diagnostic {
    let severity = match message.severity {
        Severity::Error => DiagnosticSeverity::Error,
        Severity::Warning => DiagnosticSeverity::Warning,
    };
    Diagnostic {
        range: srcpos_to_range(message.pos),
        severity: Some(severity),
        code: None,
        source: Some("vhdl ls".to_owned()),
        message: message.message,
        related_information: None,
    }
}

fn main() -> io::Result<()> {
    let mut io: IoHandler<()> = IoHandler::default();
    io.add_method("initialize", |_params| {
        let result = InitializeResult {
            capabilities: ServerCapabilities {
                /// Defines how text documents are synced.
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::Full,
                )),

                /// The server provides hover support.
                hover_provider: None,

                /// The server provides completion support.
                completion_provider: None,

                /// The server provides signature help support.
                signature_help_provider: None,

                /// The server provides goto definition support.
                definition_provider: None,

                /// The server provides goto type definition support.
                type_definition_provider: None,

                /// the server provides goto implementation support.
                implementation_provider: None,

                /// The server provides find references support.
                references_provider: None,

                /// The server provides document highlight support.
                document_highlight_provider: None,

                /// The server provides document symbol support.
                document_symbol_provider: None,

                /// The server provides workspace symbol support.
                workspace_symbol_provider: None,

                /// The server provides code actions.
                code_action_provider: None,

                /// The server provides code lens.
                code_lens_provider: None,

                /// The server provides document formatting.
                document_formatting_provider: None,

                /// The server provides document range formatting.
                document_range_formatting_provider: None,

                /// The server provides document formatting on typing.
                document_on_type_formatting_provider: None,

                /// The server provides rename support.
                rename_provider: None,

                /// The server provides color provider support.
                color_provider: None,

                /// The server provides folding provider support.
                folding_range_provider: None,

                /// The server provides execute command support.
                execute_command_provider: None,

                /// Workspace specific server capabilities
                workspace: None,
            },
        };

        // Ok(serde_json::from_str(&serde_json::to_string(&result).unwrap()).unwrap())
        Ok(serde_json::to_value(result).unwrap())
    });
    io.add_notification("initialized", |_params| {});
    io.add_notification("textDocument/didChange", |params: jsonrpc_core::Params| {
        let params: DidChangeTextDocumentParams = params.parse().unwrap();
        eprintln!("textDocument/didChange params: {:#?}", params);

        let parser = VHDLParser::new();
        let mut messages = Vec::new();

        // @TODO return error to client
        let source = Source::from_str(&params.content_changes.get(0).unwrap().text)
            .expect("Source was not legal latin-1");
        let mut diagnostics = Vec::new();
        match parser.parse_design_source(&source, &mut messages) {
            Err(ParserError::Message(message)) => {
                eprintln!("{}", message.pretty_string());
                diagnostics.push(to_diagnostic(message));
            }
            Err(ParserError::IOError(error)) => eprintln!("{}", error),
            Ok(..) => {}
        };

        for message in messages {
            eprintln!("{}", message.pretty_string());
            diagnostics.push(to_diagnostic(message));
        }

        let publish_diagnostics = PublishDiagnosticsParams {
            uri: params.text_document.uri,
            diagnostics: diagnostics,
        };

        let publish_diagnostics_json = match serde_json::to_value(publish_diagnostics).unwrap() {
            serde_json::Value::Object(map) => map,
            map => panic!("{:?}", map),
        };

        let notification = Notification {
            jsonrpc: Some(Version::V2),
            method: "textDocument/publishDiagnostics".to_owned(),
            params: Params::Map(publish_diagnostics_json),
        };

        let serialized = serde_json::to_string(&notification).unwrap();

        eprintln!("{:?}", serialized);
        print!("Content-Length: {}\r\n", serialized.len());
        print!("\r\n");
        print!("{}", serialized);
        io::stdout().flush().ok().expect("Could not flush stdout");
    });

    // ServerBuilder::new(io).build();

    let mut stdin = io::stdin();

    loop {
        let content_length = read_header(&mut stdin.lock());
        eprintln!("content_length = {}", content_length);

        let request = {
            let handle = &mut stdin;
            let mut buffer = String::new();
            handle
                .take(content_length)
                .read_to_string(&mut buffer)
                .unwrap();
            buffer
        };
        eprintln!("{:?}", request);
        if let Some(response) = io.handle_request_sync(&request) {
            eprintln!("{:?}", response);
            print!("Content-Length: {}\r\n", response.len());
            print!("\r\n");
            print!("{}", response);
            io::stdout().flush().ok().expect("Could not flush stdout");
        }
    }
}
