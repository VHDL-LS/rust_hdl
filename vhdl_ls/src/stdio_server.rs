// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

//! This module handles setting up `VHDLServer` for `stdio` communication.
//! It also contains the main event loop for handling incoming messages from the LSP client and
//! dispatching them to the appropriate server methods.

use lsp_server::Connection;
use lsp_types::{
    notification::{self, Notification},
    request, InitializeParams,
};
use std::rc::Rc;

use crate::rpc_channel::RpcChannel;
use crate::vhdl_server::VHDLServer;

/// Set up the IO channel for `stdio` and start the VHDL language server.
pub fn start() {
    let (connection, io_threads) = Connection::stdio();
    let connection = Rc::new(connection);
    let mut server = VHDLServer::new(connection.clone());

    handle_initialization(&connection, &mut server);
    main_event_loop(connection, server);
    io_threads.join().unwrap();
}

/// Wait for initialize request from the client and let the server respond to it.
fn handle_initialization<T: RpcChannel + Clone>(
    connection: &Connection,
    server: &mut VHDLServer<T>,
) {
    let (initialize_id, initialize_params) = connection.initialize_start().unwrap();
    let initialize_params = serde_json::from_value::<InitializeParams>(initialize_params).unwrap();
    let initialize_result = server.initialize_request(initialize_params);
    connection
        .initialize_finish(
            initialize_id,
            serde_json::to_value(initialize_result).unwrap(),
        )
        .unwrap();

    server.initialized_notification();
}

/// Main event loop handling incoming messages from the client.
fn main_event_loop<T: RpcChannel + Clone>(connection: Rc<Connection>, mut server: VHDLServer<T>) {
    info!("Language server initialized, waiting for messages ...");
    while let Ok(message) = connection.receiver.recv() {
        trace!("Received message: {:?}", message);
        if let lsp_server::Message::Notification(notification) = &message {
            if notification.method == notification::Exit::METHOD {
                return;
            }
        }
        match message {
            lsp_server::Message::Request(request) => {
                handle_request(&mut server, connection.as_ref(), request)
            }
            lsp_server::Message::Notification(notification) => {
                handle_notification(&mut server, notification);
            }
            lsp_server::Message::Response(response) => handle_response(&mut server, response),
        };
    }
}

/// Send responses (to requests sent by the client) back to the client.
fn send_response(connection: &Connection, response: lsp_server::Response) {
    trace!("Sending response: {:?}", response);
    connection.sender.send(response.into()).unwrap();
}

impl RpcChannel for Rc<Connection> {
    /// Send notifications to the client.
    fn send_notification(
        &self,
        method: impl Into<String>,
        notification: impl serde::ser::Serialize,
    ) {
        let notification = lsp_server::Notification {
            method: method.into(),
            params: serde_json::to_value(notification).unwrap(),
        };

        trace!("Sending notification: {:?}", notification);
        self.sender.send(notification.into()).unwrap();
    }
}

/// Handle incoming requests from the client.
fn handle_request<T: RpcChannel + Clone>(
    server: &mut VHDLServer<T>,
    connection: &Connection,
    request: lsp_server::Request,
) {
    fn extract<R>(
        request: lsp_server::Request,
    ) -> Result<(lsp_server::RequestId, R::Params), lsp_server::Request>
    where
        R: request::Request,
        R::Params: serde::de::DeserializeOwned,
    {
        request.extract(R::METHOD)
    }

    trace!("Handling request: {:?}", request);
    let request = match extract::<request::GotoDeclaration>(request) {
        Ok((id, params)) => {
            let result = server.text_document_declaration(&params);
            send_response(connection, lsp_server::Response::new_ok(id, result));
            return;
        }
        Err(request) => request,
    };
    let request = match extract::<request::GotoDefinition>(request) {
        Ok((id, params)) => {
            let result = server.text_document_definition(&params);
            send_response(connection, lsp_server::Response::new_ok(id, result));
            return;
        }
        Err(request) => request,
    };
    let request = match extract::<request::References>(request) {
        Ok((id, params)) => {
            let result = server.text_document_references(&params);
            send_response(connection, lsp_server::Response::new_ok(id, result));
            return;
        }
        Err(request) => request,
    };
    let request = match extract::<request::Shutdown>(request) {
        Ok((id, _params)) => {
            server.shutdown_server();
            send_response(connection, lsp_server::Response::new_ok(id, ()));
            return;
        }
        Err(request) => request,
    };

    debug!("Unhandled request: {:?}", request);
    send_response(
        connection,
        lsp_server::Response::new_err(
            request.id,
            lsp_server::ErrorCode::MethodNotFound as i32,
            "Unknown request".to_string(),
        ),
    );
}

/// Handle incoming notifications from the client.
fn handle_notification<T: RpcChannel + Clone>(
    server: &mut VHDLServer<T>,
    notification: lsp_server::Notification,
) {
    fn extract<N>(
        notification: lsp_server::Notification,
    ) -> Result<N::Params, lsp_server::Notification>
    where
        N: notification::Notification,
        N::Params: serde::de::DeserializeOwned,
    {
        notification.extract(N::METHOD)
    }

    trace!("Handling notification: {:?}", notification);
    let notification = match extract::<notification::DidChangeTextDocument>(notification) {
        Ok(params) => return server.text_document_did_change_notification(&params),
        Err(notification) => notification,
    };
    let notification = match extract::<notification::DidOpenTextDocument>(notification) {
        Ok(params) => return server.text_document_did_open_notification(&params),
        Err(notification) => notification,
    };
    let notification = match extract::<notification::Exit>(notification) {
        Ok(_params) => return server.exit_notification(),
        Err(notification) => notification,
    };

    if !notification.method.starts_with("$/") {
        debug!("Unhandled notification: {:?}", notification);
    }
}

/// Handle incoming responses (to requests sent by us) from the client.
fn handle_response<T: RpcChannel + Clone>(
    _server: &mut VHDLServer<T>,
    response: lsp_server::Response,
) {
    trace!("Handling response: {:?}", response);
    debug!("Unhandled response: {:?}", response);
}
