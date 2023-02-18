// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

//! This module handles setting up `VHDLServer` for `stdio` communication.
//! It also contains the main event loop for handling incoming messages from the LSP client and
//! dispatching them to the appropriate server methods.

use lsp_server::{Connection, ExtractError, Request, RequestId};
use lsp_types::{notification, request, InitializeParams};
use serde_json::Value;

use std::{cell::RefCell, rc::Rc};

use crate::rpc_channel::{RpcChannel, SharedRpcChannel};
use crate::vhdl_server::VHDLServer;
use crate::vhdl_server::VHDLServerSettings;

/// Set up the IO channel for `stdio` and start the VHDL language server.
pub fn start(settings: VHDLServerSettings) {
    let (connection, io_threads) = Connection::stdio();
    let connection_rpc = Rc::new(ConnectionRpcChannel::new(connection));
    let rpc = SharedRpcChannel::new(connection_rpc.clone());
    let mut server = VHDLServer::new_settings(rpc, settings);
    connection_rpc.handle_initialization(&mut server);
    connection_rpc.main_event_loop(server);

    io_threads.join().unwrap();
}

/// Wrapper for Connection implementing RpcChannel + Clone
/// and keeping track of outgoing request IDs.
#[derive(Clone)]
struct ConnectionRpcChannel {
    connection: Rc<Connection>,
    next_outgoing_request_id: Rc<RefCell<i32>>,
}

impl RpcChannel for ConnectionRpcChannel {
    /// Send notification to the client.
    fn send_notification(&self, method: String, params: Value) {
        let notification = lsp_server::Notification { method, params };

        trace!("Sending notification: {:?}", notification);
        self.connection.sender.send(notification.into()).unwrap();
    }

    /// Send request to the client.
    fn send_request(&self, method: String, params: Value) {
        let request_id = self.next_outgoing_request_id.replace_with(|&mut id| id + 1);

        let request = Request::new(RequestId::from(request_id), method, params);
        self.connection.sender.send(request.into()).unwrap();
    }
}

impl ConnectionRpcChannel {
    fn new(connection: Connection) -> Self {
        Self {
            connection: Rc::new(connection),
            next_outgoing_request_id: Rc::new(RefCell::new(0)),
        }
    }

    /// Wait for initialize request from the client and let the server respond to it.
    fn handle_initialization(&self, server: &mut VHDLServer) {
        let (initialize_id, initialize_params) = self.connection.initialize_start().unwrap();
        let initialize_params =
            serde_json::from_value::<InitializeParams>(initialize_params).unwrap();
        let initialize_result = server.initialize_request(initialize_params);
        self.connection
            .initialize_finish(
                initialize_id,
                serde_json::to_value(initialize_result).unwrap(),
            )
            .unwrap();

        server.initialized_notification();
    }

    /// Main event loop handling incoming messages from the client.
    fn main_event_loop(&self, mut server: VHDLServer) {
        info!("Language server initialized, waiting for messages ...");
        while let Ok(message) = self.connection.receiver.recv() {
            trace!("Received message: {:?}", message);
            match message {
                lsp_server::Message::Request(request) => self.handle_request(&mut server, request),
                lsp_server::Message::Notification(notification) => {
                    self.handle_notification(&mut server, notification);
                }
                lsp_server::Message::Response(response) => {
                    self.handle_response(&mut server, response)
                }
            };
        }
    }

    /// Send responses (to requests sent by the client) back to the client.
    fn send_response(&self, response: lsp_server::Response) {
        trace!("Sending response: {:?}", response);
        self.connection.sender.send(response.into()).unwrap();
    }

    /// Handle incoming requests from the client.
    fn handle_request(&self, server: &mut VHDLServer, request: lsp_server::Request) {
        fn extract<R>(
            request: lsp_server::Request,
        ) -> Result<(lsp_server::RequestId, R::Params), lsp_server::Request>
        where
            R: request::Request,
            R::Params: serde::de::DeserializeOwned,
        {
            request.extract(R::METHOD).map_err(|e| match e {
                ExtractError::MethodMismatch(r) => r,
                err @ ExtractError::JsonError { .. } => {
                    panic!("{err:?}");
                }
            })
        }

        trace!("Handling request: {:?}", request);
        let request = match extract::<request::GotoDeclaration>(request) {
            Ok((id, params)) => {
                let result =
                    server.text_document_declaration(&params.text_document_position_params);
                self.send_response(lsp_server::Response::new_ok(id, result));
                return;
            }
            Err(request) => request,
        };
        let request = match extract::<request::GotoDefinition>(request) {
            Ok((id, params)) => {
                let result = server.text_document_definition(&params.text_document_position_params);
                self.send_response(lsp_server::Response::new_ok(id, result));
                return;
            }
            Err(request) => request,
        };
        let request = match extract::<request::GotoImplementation>(request) {
            Ok((id, params)) => {
                let result =
                    server.text_document_implementation(&params.text_document_position_params);
                self.send_response(lsp_server::Response::new_ok(id, result));
                return;
            }
            Err(request) => request,
        };
        let request = match extract::<request::Rename>(request) {
            Ok((id, params)) => {
                let result = server.rename(&params);
                self.send_response(lsp_server::Response::new_ok(id, result));
                return;
            }
            Err(request) => request,
        };
        let request = match extract::<request::PrepareRenameRequest>(request) {
            Ok((id, params)) => {
                let result = server.prepare_rename(&params);
                self.send_response(lsp_server::Response::new_ok(id, result));
                return;
            }
            Err(request) => request,
        };
        let request = match extract::<request::WorkspaceSymbolRequest>(request) {
            Ok((id, params)) => {
                let result = server.workspace_symbol(&params);
                self.send_response(lsp_server::Response::new_ok(id, result));
                return;
            }
            Err(request) => request,
        };
        let request = match extract::<request::DocumentSymbolRequest>(request) {
            Ok((id, params)) => {
                let result = server.document_symbol(&params);
                self.send_response(lsp_server::Response::new_ok(id, result));
                return;
            }
            Err(request) => request,
        };
        let request = match extract::<request::HoverRequest>(request) {
            Ok((id, params)) => {
                let result = server.text_document_hover(&params.text_document_position_params);
                self.send_response(lsp_server::Response::new_ok(id, result));
                return;
            }
            Err(request) => request,
        };
        let request = match extract::<request::References>(request) {
            Ok((id, params)) => {
                let result = server.text_document_references(&params);
                self.send_response(lsp_server::Response::new_ok(id, result));
                return;
            }
            Err(request) => request,
        };
        let request = match extract::<request::Shutdown>(request) {
            Ok((id, _params)) => {
                server.shutdown_server();
                self.send_response(lsp_server::Response::new_ok(id, ()));
                return;
            }
            Err(request) => request,
        };

        debug!("Unhandled request: {:?}", request);
        self.send_response(lsp_server::Response::new_err(
            request.id,
            lsp_server::ErrorCode::MethodNotFound as i32,
            "Unknown request".to_string(),
        ));
    }

    /// Handle incoming notifications from the client.
    fn handle_notification(&self, server: &mut VHDLServer, notification: lsp_server::Notification) {
        fn extract<N>(
            notification: lsp_server::Notification,
        ) -> Result<N::Params, lsp_server::Notification>
        where
            N: notification::Notification,
            N::Params: serde::de::DeserializeOwned,
        {
            notification.extract(N::METHOD).map_err(|e| match e {
                ExtractError::MethodMismatch(n) => n,
                err @ ExtractError::JsonError { .. } => {
                    panic!("{err:?}");
                }
            })
        }

        trace!("Handling notification: {:?}", notification);
        // textDocument/didChange
        let notification = match extract::<notification::DidChangeTextDocument>(notification) {
            Ok(params) => return server.text_document_did_change_notification(&params),
            Err(notification) => notification,
        };
        // textDocument/didOpen
        let notification = match extract::<notification::DidOpenTextDocument>(notification) {
            Ok(params) => return server.text_document_did_open_notification(&params),
            Err(notification) => notification,
        };
        // workspace.didChangeWatchedFiles
        let notification = match extract::<notification::DidChangeWatchedFiles>(notification) {
            Ok(params) => return server.workspace_did_change_watched_files(&params),
            Err(notification) => notification,
        };
        // exit
        let notification = match extract::<notification::Exit>(notification) {
            Ok(_params) => return server.exit_notification(),
            Err(notification) => notification,
        };

        if !notification.method.starts_with("$/") {
            debug!("Unhandled notification: {:?}", notification);
        }
    }

    /// Handle incoming responses (to requests sent by us) from the client.
    fn handle_response(&self, _server: &mut VHDLServer, response: lsp_server::Response) {
        trace!("Handling response: {:?}", response);
        // We currently can ignore incoming responses as the implemented
        // outgoing requests do not require confirmation by the client.
    }
}
