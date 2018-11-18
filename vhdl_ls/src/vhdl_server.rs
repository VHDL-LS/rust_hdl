// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

extern crate languageserver_types;
use self::languageserver_types::*;
extern crate serde;

extern crate url;
use self::url::Url;

extern crate vhdl_parser;
use self::vhdl_parser::message::{Message, Severity};
use self::vhdl_parser::semantic;
use self::vhdl_parser::source::{Source, SrcPos};
use self::vhdl_parser::{ParserError, VHDLParser};

pub trait RpcChannel {
    fn send_notification(
        &self,
        method: impl Into<String>,
        notification: impl serde::ser::Serialize,
    );
}

pub struct VHDLServer<T: RpcChannel + Clone> {
    rpc_channel: T,
    server: Option<InitializedVHDLServer<T>>,
}

impl<T: RpcChannel + Clone> VHDLServer<T> {
    pub fn new(rpc_channel: T) -> VHDLServer<T> {
        VHDLServer {
            rpc_channel,
            server: None,
        }
    }

    pub fn initialize_request(
        &mut self,
        params: InitializeParams,
    ) -> jsonrpc_core::Result<InitializeResult> {
        let (server, result) = InitializedVHDLServer::initialize(self.rpc_channel.clone(), params)?;
        self.server = Some(server);
        Ok(result)
    }

    fn server(&self) -> &InitializedVHDLServer<T> {
        self.server.as_ref().expect("Expected initialized server")
    }

    pub fn initialized_notification(&self, params: InitializedParams) {
        self.server().initialized_notification(params);
    }

    pub fn text_document_did_change_notification(&self, params: DidChangeTextDocumentParams) {
        self.server().text_document_did_change_notification(params)
    }

    pub fn text_document_did_open_notification(&self, params: DidOpenTextDocumentParams) {
        self.server().text_document_did_open_notification(params)
    }
}

struct InitializedVHDLServer<T: RpcChannel> {
    rpc_channel: T,
    init_params: InitializeParams,
}

impl<T: RpcChannel + Clone> InitializedVHDLServer<T> {
    pub fn initialize(
        rpc_channel: T,
        init_params: InitializeParams,
    ) -> jsonrpc_core::Result<(InitializedVHDLServer<T>, InitializeResult)> {
        let server = InitializedVHDLServer {
            rpc_channel,
            init_params,
        };

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

        Ok((server, result))
    }

    fn client_supports_related_information(&self) -> bool {
        let try_fun = || {
            self.init_params
                .capabilities
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
        let source =
            Source::inline_utf8(uri.to_string(), code).expect("Source was not legal latin-1");
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

        self.rpc_channel
            .send_notification("textDocument/publishDiagnostics", publish_diagnostics);
    }

    pub fn initialized_notification(&self, _params: InitializedParams) {}

    pub fn text_document_did_change_notification(&self, params: DidChangeTextDocumentParams) {
        self.parse_and_publish_diagnostics(
            params.text_document.uri,
            &params.content_changes.get(0).unwrap().text,
        );
    }

    pub fn text_document_did_open_notification(&self, params: DidOpenTextDocumentParams) {
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::cell::RefCell;
    use std::collections::VecDeque;
    use std::rc::Rc;
    extern crate tempfile;

    #[derive(Clone)]
    enum RpcExpected {
        Notification {
            method: String,
            notification: serde_json::Value,
        },
    }

    #[derive(Clone)]
    struct RpcMock {
        expected: Rc<RefCell<VecDeque<RpcExpected>>>,
    }

    impl RpcMock {
        fn new() -> RpcMock {
            RpcMock {
                expected: Rc::new(RefCell::new(VecDeque::new())),
            }
        }

        fn expect_notification(
            &self,
            method: impl Into<String>,
            notification: impl serde::ser::Serialize,
        ) {
            self.expected
                .borrow_mut()
                .push_back(RpcExpected::Notification {
                    method: method.into(),
                    notification: serde_json::to_value(notification).unwrap(),
                });
        }
    }

    impl RpcChannel for RpcMock {
        fn send_notification(
            &self,
            method: impl Into<String>,
            notification: impl serde::ser::Serialize,
        ) {
            let notification = serde_json::to_value(notification).unwrap();
            let expected = self
                .expected
                .borrow_mut()
                .pop_front()
                .ok_or_else(|| panic!("No expected value, got {:?}", notification))
                .unwrap();

            match expected {
                RpcExpected::Notification {
                    method: exp_method,
                    notification: exp_notification,
                } => {
                    assert_eq!(method.into(), exp_method);
                    assert_eq!(notification, exp_notification);
                }
            }
        }
    }

    fn initialize_server(server: &mut VHDLServer<RpcMock>, root_uri: Url) {
        let capabilities = ClientCapabilities {
            workspace: None,
            text_document: None,
            experimental: None,
        };

        let initialize_params = InitializeParams {
            process_id: None,
            root_path: None,
            root_uri: Some(root_uri),
            initialization_options: None,
            capabilities,
            trace: None,
            workspace_folders: None,
        };

        server
            .initialize_request(initialize_params)
            .expect("Should not fail");
        server.initialized_notification(InitializedParams {});
    }

    fn temp_root_uri() -> (tempfile::TempDir, Url) {
        let tempdir = tempfile::tempdir().unwrap();
        let root_uri = Url::from_file_path(tempdir.path()).unwrap();
        (tempdir, root_uri)
    }

    #[test]
    fn initialize() {
        let mock = RpcMock::new();
        let mut server = VHDLServer::new(mock);
        let (_tempdir, root_uri) = temp_root_uri();
        initialize_server(&mut server, root_uri);
    }

    #[test]
    fn did_open_no_diagnostics() {
        let mock = RpcMock::new();
        let mut server = VHDLServer::new(mock.clone());

        let (_tempdir, root_uri) = temp_root_uri();
        initialize_server(&mut server, root_uri.clone());

        let file_url = root_uri.join("ent.vhd").unwrap();
        let code = "
entity ent is
end entity ent;
".to_owned();

        let did_open = DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: file_url.clone(),
                language_id: "vhdl".to_owned(),
                version: 0,
                text: code.to_owned(),
            },
        };

        let publish_diagnostics = PublishDiagnosticsParams {
            uri: file_url.clone(),
            diagnostics: vec![],
        };

        mock.expect_notification("textDocument/publishDiagnostics", publish_diagnostics);

        server.text_document_did_open_notification(did_open);
    }

    #[test]
    fn did_open_with_diagnostics_and_change_without() {
        let mock = RpcMock::new();
        let mut server = VHDLServer::new(mock.clone());

        let (_tempdir, root_uri) = temp_root_uri();
        initialize_server(&mut server, root_uri.clone());

        let file_url = root_uri.join("ent.vhd").unwrap();
        let code = "
entity ent is
end entity ent2;
".to_owned();

        let did_open = DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: file_url.clone(),
                language_id: "vhdl".to_owned(),
                version: 0,
                text: code.to_owned(),
            },
        };

        let publish_diagnostics = PublishDiagnosticsParams {
            uri: file_url.clone(),
            diagnostics: vec![Diagnostic {
                range: Range {
                    start: Position {
                        line: 2,
                        character: "end entity ".len() as u64,
                    },
                    end: Position {
                        line: 2,
                        character: "end entity ent2".len() as u64,
                    },
                },
                code: None,
                severity: Some(DiagnosticSeverity::Error),
                source: Some("vhdl ls".to_owned()),
                message: "End identifier mismatch, expected ent".to_owned(),
                related_information: None,
            }],
        };

        mock.expect_notification("textDocument/publishDiagnostics", publish_diagnostics);
        server.text_document_did_open_notification(did_open);

        let code = "
entity ent is
end entity ent;
".to_owned();

        let did_change = DidChangeTextDocumentParams {
            text_document: VersionedTextDocumentIdentifier {
                uri: file_url.clone(),
                version: Some(1),
            },
            content_changes: vec![TextDocumentContentChangeEvent {
                range: None,
                range_length: None,
                text: code.clone(),
            }],
        };

        let publish_diagnostics = PublishDiagnosticsParams {
            uri: file_url.clone(),
            diagnostics: vec![],
        };

        mock.expect_notification("textDocument/publishDiagnostics", publish_diagnostics);
        server.text_document_did_change_notification(did_change);
    }

}
