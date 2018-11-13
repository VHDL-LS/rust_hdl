// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

extern crate jsonrpc_core;
extern crate languageserver_types;
extern crate serde_json;
extern crate url;

use jsonrpc_core::request::Notification;
use jsonrpc_core::{IoHandler, Params, Value};
use languageserver_types::{
    ClientCapabilities, Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity,
    DidChangeTextDocumentParams, DidOpenTextDocumentParams, InitializeResult, Location, Position,
    PublishDiagnosticsParams, Range, ServerCapabilities, TextDocumentSyncCapability,
    TextDocumentSyncKind,
};
use std::io::prelude::*;
use std::io::{self, BufRead};
use url::Url;

extern crate vhdl_parser;
use vhdl_parser::message::{Message, Severity};
use vhdl_parser::semantic;
use vhdl_parser::source::{Source, SrcPos};
use vhdl_parser::{ParserError, VHDLParser};

use std::sync::mpsc::{sync_channel, SyncSender};
use std::thread::spawn;

use std::sync::{Arc, Mutex};

fn main() {
    let (request_sender, request_receiver) = sync_channel(1);
    let (response_sender, response_receiver) = sync_channel(1);
    let mut io = IoHandler::new();

    // @TODO handle jsonrpc synchronously
    let lang_server = Arc::new(Mutex::new(LanguageServer::new(response_sender.clone())));
    let server = lang_server.clone();
    io.add_method("initialize", move |params| {
        server.lock().unwrap().initialize_request(params)
    });

    let server = lang_server.clone();
    io.add_notification("initialized", move |params| {
        server.lock().unwrap().initialized_notification(params)
    });

    let server = lang_server.clone();
    io.add_notification("textDocument/didChange", move |params| {
        server
            .lock()
            .unwrap()
            .text_document_did_change_notification(params)
    });

    let server = lang_server.clone();
    io.add_notification("textDocument/didOpen", move |params| {
        server
            .lock()
            .unwrap()
            .text_document_did_open_notification(params)
    });

    // Spawn thread to read requests from stdin
    spawn(move || {
        let stdin = io::stdin();
        loop {
            let request = read_request(&mut stdin.lock());
            request_sender.send(request).unwrap();
        }
    });

    // Spawn thread to write notificaitons to stdout
    spawn(move || {
        let mut stdout = io::stdout();
        loop {
            let response: String = response_receiver.recv().unwrap();
            send_response(&mut stdout, &response);
        }
    });

    loop {
        let request = request_receiver.recv().unwrap();
        let response = io.handle_request_sync(&request);
        if let Some(response) = response {
            response_sender.send(response).unwrap();
        }
    }
}

fn read_request(reader: &mut BufRead) -> String {
    let content_length = read_header(reader);

    let mut request = String::new();
    reader
        .take(content_length)
        .read_to_string(&mut request)
        .unwrap();
    eprintln!("DEBUG GOT REQUEST: {:?}", request);
    request
}

fn send_response(writer: &mut Write, response: &str) {
    eprintln!("DEBUG SEND RESPONSE: {:?}", response);
    write!(writer, "Content-Length: {}\r\n", response.len());
    write!(writer, "\r\n");
    write!(writer, "{}", response);
    writer.flush().ok().expect("Could not flush stdout");
}

struct LanguageServer {
    response_sender: SyncSender<String>,
    client_capabilities: Option<ClientCapabilities>,
}

impl LanguageServer {
    fn new(response_sender: SyncSender<String>) -> LanguageServer {
        LanguageServer {
            response_sender,
            client_capabilities: None,
        }
    }

    fn initialize_request(&mut self, params: Params) -> jsonrpc_core::Result<Value> {
        self.client_capabilities = params.parse().unwrap();

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

        Ok(serde_json::to_value(result).unwrap())
    }

    fn client_supports_related_information(&self) -> bool {
        let try_fun = || {
            self.client_capabilities
                .as_ref()?
                .text_document
                .as_ref()?
                .publish_diagnostics
                .as_ref()?
                .related_information
        };
        try_fun().unwrap_or(false)
    }

    fn parse_and_publish_diagnostics(&self, uri: Url, code: &str) {
        let parser = VHDLParser::new();
        let mut messages = Vec::new();

        // @TODO return error to client
        let source = Source::from_str(code).expect("Source was not legal latin-1");
        match parser.parse_design_source(&source, &mut messages) {
            Err(ParserError::Message(message)) => {
                eprintln!("{}", message.show());
                messages.push(message);
            }
            Err(ParserError::IOError(error)) => eprintln!("{}", error),
            Ok(ref design_file) => {
                for design_unit in design_file.design_units.iter() {
                    semantic::check_design_unit(design_unit, &mut messages);
                }
            }
        };

        let mut diagnostics = Vec::new();
        for message in messages {
            eprintln!("{}", message.show());
            diagnostics.extend(to_diagnostics(
                &uri,
                message,
                self.client_supports_related_information(),
            ));
        }

        let publish_diagnostics = PublishDiagnosticsParams {
            uri,
            diagnostics: diagnostics,
        };

        let publish_diagnostics_json = match serde_json::to_value(publish_diagnostics).unwrap() {
            serde_json::Value::Object(map) => map,
            map => panic!("{:?}", map),
        };

        let notification = Notification {
            jsonrpc: Some(jsonrpc_core::Version::V2),
            method: "textDocument/publishDiagnostics".to_owned(),
            params: Params::Map(publish_diagnostics_json),
        };

        self.response_sender
            .send(serde_json::to_string(&notification).unwrap())
            .unwrap();
    }

    fn initialized_notification(&mut self, _params: Params) {}

    fn text_document_did_change_notification(&self, params: Params) {
        let params: DidChangeTextDocumentParams = params.parse().unwrap();
        self.parse_and_publish_diagnostics(
            params.text_document.uri,
            &params.content_changes.get(0).unwrap().text,
        );
    }

    fn text_document_did_open_notification(&mut self, params: Params) {
        let params: DidOpenTextDocumentParams = params.parse().unwrap();
        self.parse_and_publish_diagnostics(params.text_document.uri, &params.text_document.text);
    }
}

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

fn to_diagnostics(
    uri: &Url,
    message: Message,
    supports_related_information: bool,
) -> Vec<Diagnostic> {
    let severity = match message.severity {
        Severity::Error => DiagnosticSeverity::Error,
        Severity::Warning => DiagnosticSeverity::Warning,
    };

    let mut diagnostics = Vec::new();

    let related_information = if supports_related_information {
        let mut related_information = Vec::new();
        for (pos, msg) in message.related {
            related_information.push(DiagnosticRelatedInformation {
                location: Location {
                    uri: uri.to_owned(),
                    range: srcpos_to_range(pos),
                },
                message: msg,
            })
        }
        Some(related_information)
    } else {
        for (pos, msg) in message.related {
            diagnostics.push(Diagnostic {
                range: srcpos_to_range(pos),
                severity: Some(DiagnosticSeverity::Hint),
                code: None,
                source: Some("vhdl ls".to_owned()),
                message: format!("related: {}", msg),
                related_information: None,
            });
        }
        None
    };

    diagnostics.push(Diagnostic {
        range: srcpos_to_range(message.pos),
        severity: Some(severity),
        code: None,
        source: Some("vhdl ls".to_owned()),
        message: message.message,
        related_information,
    });
    diagnostics
}
