// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use self::languageserver_types::*;
use languageserver_types;

use self::url::Url;
use url;

use self::fnv::FnvHashMap;
use fnv;
use std::collections::hash_map::Entry;

use self::vhdl_parser::{Config, Message, Project, Severity, Source, SrcPos};
use crate::rpc_channel::RpcChannel;
use std::io;
use std::path::Path;
use vhdl_parser;

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

    /// Load the vhdl_ls.toml config file from initalizeParams.rootUri
    fn load_config(init_params: &InitializeParams) -> io::Result<Config> {
        let root_uri = init_params.root_uri.as_ref().ok_or_else(|| {
            io::Error::new(io::ErrorKind::Other, "initializeParams.rootUri not set")
        })?;

        let root_path = root_uri.to_file_path().map_err(|_| {
            io::Error::new(
                io::ErrorKind::Other,
                format!(
                    "initializeParams.rootUri {:?} not a valid file path",
                    root_uri
                ),
            )
        })?;

        let config_file = root_path.join("vhdl_ls.toml");
        let config = Config::read_file_path(&config_file)?;

        Ok(config)
    }

    pub fn initialize_request(
        &mut self,
        params: InitializeParams,
    ) -> jsonrpc_core::Result<InitializeResult> {
        let config = {
            match Self::load_config(&params) {
                Ok(config) => config,
                Err(ref err) => {
                    self.rpc_channel.window_show_message(
                        MessageType::Warning,
                        format!(
                            "Found no vhdl_ls.toml config file in the root path: {}",
                            err
                        ),
                    );
                    self.rpc_channel.window_show_message(
                        MessageType::Warning,
                        "Semantic analysis disabled, will perform syntax checking only",
                    );
                    Config::default()
                }
            }
        };

        let (server, result) = InitializedVHDLServer::new(self.rpc_channel.clone(), config, params);
        self.server = Some(server);
        Ok(result)
    }

    pub fn shutdown_server(&mut self, _params: ()) -> jsonrpc_core::Result<()> {
        self.server = None;
        Ok(())
    }

    fn mut_server(&mut self) -> &mut InitializedVHDLServer<T> {
        self.server.as_mut().expect("Expected initialized server")
    }

    pub fn exit_notification(&mut self, _params: ()) {
        match self.server {
            Some(_) => ::std::process::exit(1),
            None => ::std::process::exit(0),
        }
    }

    pub fn initialized_notification(&mut self, params: &InitializedParams) {
        self.mut_server().initialized_notification(params);
    }

    pub fn text_document_did_change_notification(&mut self, params: &DidChangeTextDocumentParams) {
        self.mut_server()
            .text_document_did_change_notification(&params)
    }

    pub fn text_document_did_open_notification(&mut self, params: &DidOpenTextDocumentParams) {
        self.mut_server()
            .text_document_did_open_notification(&params)
    }
}

struct InitializedVHDLServer<T: RpcChannel> {
    rpc_channel: T,
    init_params: InitializeParams,
    project: Project,
    files_with_notifications: FnvHashMap<Url, ()>,
}

/// Allow VHDL Server to act as an RpcChannel
impl<T: RpcChannel> RpcChannel for InitializedVHDLServer<T> {
    fn send_notification(
        &self,
        method: impl Into<String>,
        notification: impl serde::ser::Serialize,
    ) {
        self.rpc_channel.send_notification(method, notification);
    }
}

impl<T: RpcChannel + Clone> InitializedVHDLServer<T> {
    pub fn new(
        rpc_channel: T,
        config: Config,
        init_params: InitializeParams,
    ) -> (InitializedVHDLServer<T>, InitializeResult) {
        // @TODO read num_threads from config file
        let num_threads = 4;
        // @TODO send error to client
        let mut errors = Vec::new();
        let project = Project::from_config(&config, num_threads, &mut errors);

        for error in errors {
            rpc_channel.window_show_message(MessageType::Error, error.to_string());
        }

        let server = InitializedVHDLServer {
            rpc_channel,
            init_params,
            project,
            files_with_notifications: FnvHashMap::default(),
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

        (server, result)
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

    fn publish_diagnostics(&mut self) {
        let supports_related_information = self.client_supports_related_information();
        let messages = self.project.analyse();
        let messages = {
            if supports_related_information {
                messages
            } else {
                flatten_related(messages)
            }
        };

        let mut files_with_notifications =
            std::mem::replace(&mut self.files_with_notifications, FnvHashMap::default());
        for (file_uri, messages) in messages_by_uri(messages).into_iter() {
            let mut diagnostics = Vec::new();
            for message in messages {
                diagnostics.push(to_diagnostic(message));
            }

            let publish_diagnostics = PublishDiagnosticsParams {
                uri: file_uri.clone(),
                diagnostics,
            };

            self.send_notification("textDocument/publishDiagnostics", publish_diagnostics);

            self.files_with_notifications.insert(file_uri.clone(), ());
        }

        for (file_uri, _) in files_with_notifications.drain() {
            // File has no longer any diagnosics, publish empty notification to clear them
            if !self.files_with_notifications.contains_key(&file_uri) {
                let publish_diagnostics = PublishDiagnosticsParams {
                    uri: file_uri.clone(),
                    diagnostics: vec![],
                };

                self.send_notification("textDocument/publishDiagnostics", publish_diagnostics);
            }
        }
    }

    fn parse_and_publish_diagnostics(&mut self, uri: &Url, code: &str) {
        // @TODO return error to client
        let file_name = uri_to_file_name(&uri);

        // @TODO return error to client
        let source =
            Source::inline_utf8(file_name.to_string(), code).expect("Source was not legal latin-1");

        // @TODO log error to client
        self.project.update_source(&file_name, &source).unwrap();
        self.publish_diagnostics();
    }

    pub fn initialized_notification(&mut self, _params: &InitializedParams) {
        self.publish_diagnostics();
    }

    pub fn text_document_did_change_notification(&mut self, params: &DidChangeTextDocumentParams) {
        self.parse_and_publish_diagnostics(
            &params.text_document.uri,
            &params.content_changes[0].text,
        );
    }

    pub fn text_document_did_open_notification(&mut self, params: &DidOpenTextDocumentParams) {
        self.parse_and_publish_diagnostics(&params.text_document.uri, &params.text_document.text);
    }
}

fn srcpos_to_range(srcpos: &SrcPos) -> Range {
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

fn messages_by_uri(messages: Vec<Message>) -> FnvHashMap<Url, Vec<Message>> {
    let mut map: FnvHashMap<Url, Vec<Message>> = FnvHashMap::default();

    for message in messages {
        let uri = file_name_to_uri(message.pos.source.file_name());
        match map.entry(uri) {
            Entry::Occupied(mut entry) => entry.get_mut().push(message),
            Entry::Vacant(entry) => {
                let vec = vec![message];
                entry.insert(vec);
            }
        }
    }

    map
}

fn flatten_related(messages: Vec<Message>) -> Vec<Message> {
    let mut flat_messages = Vec::new();
    for mut message in messages {
        flat_messages.extend(message.drain_related());
        flat_messages.push(message);
    }
    flat_messages
}

fn file_name_to_uri(file_name: impl AsRef<str>) -> Url {
    // @TODO return error to client
    Url::from_file_path(Path::new(file_name.as_ref())).unwrap()
}

fn uri_to_file_name(uri: &Url) -> String {
    // @TODO return error to client
    uri.to_file_path().unwrap().to_str().unwrap().to_owned()
}

fn to_diagnostic(message: Message) -> Diagnostic {
    let severity = match message.severity {
        Severity::Error => DiagnosticSeverity::Error,
        Severity::Warning => DiagnosticSeverity::Warning,
        Severity::Info => DiagnosticSeverity::Information,
        Severity::Hint => DiagnosticSeverity::Hint,
    };

    let related_information = if !message.related.is_empty() {
        let mut related_information = Vec::new();
        for (pos, msg) in message.related {
            let uri = file_name_to_uri(pos.source.file_name());
            related_information.push(DiagnosticRelatedInformation {
                location: Location {
                    uri: uri.to_owned(),
                    range: srcpos_to_range(&pos),
                },
                message: msg,
            })
        }
        Some(related_information)
    } else {
        None
    };

    Diagnostic {
        range: srcpos_to_range(&message.pos),
        severity: Some(severity),
        code: None,
        source: Some("vhdl ls".to_owned()),
        message: message.message,
        related_information,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rpc_channel::test_support::*;
    use tempfile;

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
        server.initialized_notification(&InitializedParams {});
    }

    fn temp_root_uri() -> (tempfile::TempDir, Url) {
        let tempdir = tempfile::tempdir().unwrap();
        let root_uri = Url::from_file_path(tempdir.path()).unwrap();
        (tempdir, root_uri)
    }

    #[test]
    fn initialize() {
        let mock = RpcMock::new();
        let mut server = VHDLServer::new(mock.clone());
        let (_tempdir, root_uri) = temp_root_uri();
        mock.expect_notification_contains(
            "window/showMessage",
            "Found no vhdl_ls.toml config file in the root path",
        );
        mock.expect_notification_contains(
            "window/showMessage",
            "Semantic analysis disabled, will perform syntax checking only",
        );
        initialize_server(&mut server, root_uri);
    }

    #[test]
    fn did_open_no_diagnostics() {
        let mock = RpcMock::new();
        let mut server = VHDLServer::new(mock.clone());

        let (_tempdir, root_uri) = temp_root_uri();
        mock.expect_notification_contains(
            "window/showMessage",
            "Found no vhdl_ls.toml config file in the root path",
        );
        mock.expect_notification_contains(
            "window/showMessage",
            "Semantic analysis disabled, will perform syntax checking only",
        );
        initialize_server(&mut server, root_uri.clone());

        let file_url = root_uri.join("ent.vhd").unwrap();
        let code = "
entity ent is
end entity ent;
"
        .to_owned();

        let did_open = DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: file_url.clone(),
                language_id: "vhdl".to_owned(),
                version: 0,
                text: code.to_owned(),
            },
        };

        server.text_document_did_open_notification(&did_open);
    }

    #[test]
    fn did_open_with_diagnostics_and_change_without() {
        let mock = RpcMock::new();
        let mut server = VHDLServer::new(mock.clone());

        let (_tempdir, root_uri) = temp_root_uri();
        mock.expect_notification_contains(
            "window/showMessage",
            "Found no vhdl_ls.toml config file in the root path",
        );
        mock.expect_notification_contains(
            "window/showMessage",
            "Semantic analysis disabled, will perform syntax checking only",
        );
        initialize_server(&mut server, root_uri.clone());

        let file_url = root_uri.join("ent.vhd").unwrap();
        let code = "
entity ent is
end entity ent2;
"
        .to_owned();

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
        server.text_document_did_open_notification(&did_open);

        let code = "
entity ent is
end entity ent;
"
        .to_owned();

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
        server.text_document_did_change_notification(&did_change);
    }

    fn write_file(root_uri: &Url, file_name: impl AsRef<str>, contents: impl AsRef<str>) -> Url {
        let path = root_uri.to_file_path().unwrap().join(file_name.as_ref());
        std::fs::write(&path, contents.as_ref()).unwrap();
        Url::from_file_path(path).unwrap()
    }

    fn write_config(root_uri: &Url, contents: impl AsRef<str>) {
        write_file(root_uri, "vhdl_ls.toml", contents);
    }

    #[test]
    fn initialize_with_config() {
        let mock = RpcMock::new();
        let mut server = VHDLServer::new(mock.clone());
        let (_tempdir, root_uri) = temp_root_uri();
        let file_uri = write_file(
            &root_uri,
            "file.vhd",
            "\
entity ent is
end entity;

architecture rtl of ent2 is
begin
end;
",
        );

        write_config(
            &root_uri,
            "
[libraries]
lib.files = [
  'file.vhd'
]
",
        );

        let publish_diagnostics = PublishDiagnosticsParams {
            uri: file_uri.clone(),
            diagnostics: vec![Diagnostic {
                range: Range {
                    start: Position {
                        line: 3,
                        character: "architecture rtl of ".len() as u64,
                    },
                    end: Position {
                        line: 3,
                        character: "architecture rtl of ent2".len() as u64,
                    },
                },
                code: None,
                severity: Some(DiagnosticSeverity::Error),
                source: Some("vhdl ls".to_owned()),
                message: "No entity \'ent2\' within library \'lib\'".to_owned(),
                related_information: None,
            }],
        };

        mock.expect_notification("textDocument/publishDiagnostics", publish_diagnostics);

        initialize_server(&mut server, root_uri);
    }

    #[test]
    fn initialize_with_bad_config() {
        let mock = RpcMock::new();
        let mut server = VHDLServer::new(mock.clone());
        let (_tempdir, root_uri) = temp_root_uri();

        write_config(
            &root_uri,
            "
[libraries
",
        );
        mock.expect_notification_contains(
            "window/showMessage",
            "Found no vhdl_ls.toml config file in the root path",
        );
        mock.expect_notification_contains(
            "window/showMessage",
            "Semantic analysis disabled, will perform syntax checking only",
        );
        initialize_server(&mut server, root_uri);
    }

    #[test]
    fn initialize_with_config_missing_files() {
        let mock = RpcMock::new();
        let mut server = VHDLServer::new(mock.clone());
        let (_tempdir, root_uri) = temp_root_uri();

        write_config(
            &root_uri,
            "
[libraries]
lib.files = [
'missing_file.vhd',
]
",
        );

        mock.expect_notification_contains("window/showMessage", "missing_file.vhd");
        initialize_server(&mut server, root_uri);
    }
}
